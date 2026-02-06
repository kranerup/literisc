;;; ===========================================================================
;;; ========================= C Compiler for liteRISC =========================
;;; ===========================================================================
;;; A minimal C compiler targeting liteRISC 32-bit processor
;;; Outputs S-expressions compatible with assembler.lisp

(defpackage :c-compiler
  (:use :cl :unit :lr-asm)
  (:export :compile-c
           :compile-c-file
           :compile-c-to-asm
           :run-c-program
           :run-and-verify-registers
           :test-c-compiler
           :test-c-compiler-with-output
           ;; Re-export key types/functions
           :tokenize
           :parse-program
           :generate-program
           ;; Optimization
           :fold-constants
           ;; Pretty printing and test output
           :pretty-print-asm
           :pretty-print-asm-to-string
           :save-compilation-output
           :compile-and-save))

(in-package :c-compiler)

;;; ===========================================================================
;;; Data Structures
;;; ===========================================================================

;;; Token structure
(defstruct token
  type      ; keyword, identifier, number, string, operator, punctuation
  value     ; the actual value
  line      ; source line number
  column)   ; source column

;;; AST Node structure
(defstruct ast-node
  type        ; function-def, return, binary-op, unary-op, literal, var-ref, etc.
  value       ; operator, literal value, name, etc.
  children    ; list of child nodes
  result-type ; type descriptor for expression result
  source-loc  ; (line . column) for error messages
  data)       ; extra data (frame-size for functions, etc.)

;;; Type descriptor
(defstruct type-desc
  base           ; int, char, void, struct, union
  pointer-level  ; 0 = not a pointer, 1 = *, 2 = **, etc.
  array-size     ; nil for non-arrays, integer for arrays
  size           ; 1, 2, or 4 bytes (nil defaults based on base type)
  unsigned-p     ; t for unsigned, nil for signed
  volatile-p     ; t for volatile, prevents register allocation
  enum-tag       ; nil, :anonymous, or tag-name string for enum types
  struct-tag)    ; nil, :anonymous, or tag-name string for struct types

;;; Symbol table entry
(defstruct sym-entry
  name      ; symbol name
  type      ; type-desc
  storage   ; :parameter, :local, :global
  offset    ; stack offset for locals/params, or address for globals
  scope     ; scope level (0 = global)
  function) ; function name (nil for globals)

;;; Struct member
(defstruct struct-member
  name          ; string: member name
  type          ; type-desc: member type
  offset)       ; integer: byte offset within struct

;;; Struct definition
(defstruct struct-def
  tag           ; string or nil for anonymous
  members       ; list of struct-member
  size          ; total size in bytes (with padding)
  alignment)    ; required alignment (max of member alignments)

;;; Compiler state
(defstruct compiler-state
  (tokens nil)           ; token stream
  (token-pos 0)          ; current position in token stream
  (symbols (make-hash-table :test 'equal)) ; symbol table
  (scope-level 0)        ; current scope level
  (label-counter 0)      ; for generating unique labels
  (current-function nil) ; name of function being compiled
  (local-offset 0)       ; current local variable offset
  (param-count 0)        ; number of parameters in current function
  (local-reg-count 0)    ; number of local registers used (0-4 for R6-R9)
  (address-taken (make-hash-table :test 'equal)) ; variables whose address is taken
  (code nil)             ; generated assembly code (list of s-expressions)
  (data nil)             ; data section for strings/globals
  (errors nil)           ; compilation errors
  (warnings nil)         ; compilation warnings
  (source-lines nil)     ; original source lines for annotations
  (source-annotations t) ; enable source annotations in output
  (optimize nil)         ; enable optimizations when t
  (optimize-size t)      ; optimize for code size (use runtime lib for mul/div/mod)
  (need-mul-runtime nil) ; set to t when __mul runtime is needed
  (need-div-runtime nil) ; set to t when __div runtime is needed
  (need-mod-runtime nil) ; set to t when __mod runtime is needed
  (function-table (make-hash-table :test 'equal)) ; name -> function AST node for inlining
  (dead-functions (make-hash-table :test 'equal)) ; functions to not emit (fully inlined)
  (enum-types (make-hash-table :test 'equal))    ; tag-name -> list of (name . value) constants
  (struct-types (make-hash-table :test 'equal))  ; tag-name -> struct-def
  (union-types (make-hash-table :test 'equal))   ; tag-name -> struct-def (all offsets=0)
  (typedef-types (make-hash-table :test 'equal)) ; typedef-name -> type-desc
  (peephole nil))                                ; enable peephole optimization

;;; Global compiler state
(defvar *state* nil)

;;; Current source context for annotations
(defvar *current-source-line* nil)    ; line number in source
(defvar *current-source-context* nil) ; description of what we're generating

;;; ===========================================================================
;;; Error Handling
;;; ===========================================================================

(defun compiler-error (message &rest args)
  "Record a compilation error"
  (let ((err (apply #'format nil message args)))
    (push err (compiler-state-errors *state*))
    (error "Compilation error: ~a" err)))

(defun compiler-warning (message &rest args)
  "Record a compilation warning"
  (let ((warn (apply #'format nil message args)))
    (push warn (compiler-state-warnings *state*))))

;;; ===========================================================================
;;; Symbol Table Management
;;; ===========================================================================

(defun make-symbol-key (name scope function)
  "Create a unique key for the symbol table"
  (format nil "~a@~a@~a" name scope (or function "global")))

(defun add-symbol (name type storage &optional offset)
  "Add a symbol to the symbol table"
  (let* ((func (compiler-state-current-function *state*))
         (key (make-symbol-key name (compiler-state-scope-level *state*) func))
         (entry (make-sym-entry :name name
                                :type type
                                :storage storage
                                :offset offset
                                :scope (compiler-state-scope-level *state*)
                                :function func)))
    (setf (gethash key (compiler-state-symbols *state*)) entry)
    entry))

(defun lookup-symbol (name)
  "Look up a symbol, preferring symbols from current function at highest valid scope, then globals"
  ;; First try to find in current function
  (let ((current-func (compiler-state-current-function *state*))
        (current-scope (compiler-state-scope-level *state*))
        (best-entry nil)
        (best-scope -1)
        (best-global nil)
        (best-global-scope -1))
    (maphash (lambda (key entry)
               (declare (ignore key))
               (when (string= (sym-entry-name entry) name)
                 (let ((entry-scope (sym-entry-scope entry)))
                   (cond
                     ;; Symbol from current function - prefer higher scope, but only if in valid scope
                     ((equal (sym-entry-function entry) current-func)
                      (when (and (<= entry-scope current-scope)
                                 (> entry-scope best-scope))
                        (setf best-entry entry)
                        (setf best-scope entry-scope)))
                     ;; Global symbol (no function) - scope 0 is always valid
                     ((null (sym-entry-function entry))
                      (when (and (<= entry-scope current-scope)
                                 (> entry-scope best-global-scope))
                        (setf best-global entry)
                        (setf best-global-scope entry-scope)))))))
             (compiler-state-symbols *state*))
    ;; Return current function's symbol if found, otherwise global
    (or best-entry best-global)))

(defun enter-scope ()
  "Enter a new scope level"
  (incf (compiler-state-scope-level *state*)))

(defun exit-scope ()
  "Exit the current scope"
  ;; Don't remove symbols - they're needed for code generation after parsing
  ;; Just decrement the scope level for future symbol additions
  (decf (compiler-state-scope-level *state*)))

(defun remove-symbol (name)
  "Remove a symbol from the symbol table (used for inline variable cleanup)"
  (let ((current-func (compiler-state-current-function *state*))
        (keys-to-remove nil))
    ;; Find all keys matching this name in the current function
    (maphash (lambda (key entry)
               (when (and (string= (sym-entry-name entry) name)
                          (equal (sym-entry-function entry) current-func))
                 (push key keys-to-remove)))
             (compiler-state-symbols *state*))
    ;; Remove them
    (dolist (key keys-to-remove)
      (remhash key (compiler-state-symbols *state*)))))

;;; ===========================================================================
;;; Code Generation Helpers
;;; ===========================================================================

(defun emit (&rest instruction)
  "Emit an assembly instruction"
  (push (if (= (length instruction) 1)
            (car instruction)
            instruction)
        (compiler-state-code *state*)))

(defun emit-comment (text)
  "Emit a comment/annotation in the assembly"
  (when (and *state* (compiler-state-source-annotations *state*))
    (push (list :comment text) (compiler-state-code *state*))))

(defun emit-source-line (line-num)
  "Emit a source line annotation"
  (when (and *state*
             (compiler-state-source-annotations *state*)
             (compiler-state-source-lines *state*)
             line-num
             (> line-num 0)
             (<= line-num (length (compiler-state-source-lines *state*))))
    (let ((line (nth (1- line-num) (compiler-state-source-lines *state*))))
      (emit-comment (format nil "~a: ~a" line-num (string-trim '(#\Space #\Tab) line))))))

(defmacro with-source-context ((line-num context) &body body)
  "Execute body with source context set for annotations"
  `(let ((*current-source-line* ,line-num)
         (*current-source-context* ,context))
     ,@body))

(defun emit-label (name)
  "Emit a label"
  (emit (list 'label name)))

(defun gen-label (&optional (prefix "L"))
  "Generate a unique label"
  (let ((num (incf (compiler-state-label-counter *state*))))
    (intern (format nil "~a~a" prefix num) :c-compiler)))

(defun get-generated-code ()
  "Return the generated code in correct order"
  ;; First emit any needed runtime library functions
  (emit-runtime-library)
  (let ((code (reverse (compiler-state-code *state*))))
    ;; Apply peephole optimization if enabled
    (if (compiler-state-peephole *state*)
        (peephole-optimize code)
        code)))

(defun emit-runtime-library ()
  "Emit runtime library functions if needed (for size-optimized mul/div/mod)"
  ;; Emit multiply runtime if needed
  (when (compiler-state-need-mul-runtime *state*)
    (emit-mul-runtime))
  ;; Emit divide runtime if needed
  (when (compiler-state-need-div-runtime *state*)
    (emit-div-runtime))
  ;; Emit modulo runtime if needed
  (when (compiler-state-need-mod-runtime *state*)
    (emit-mod-runtime)))

(defun emit-mul-runtime ()
  "Emit the __mul runtime function: P0 = P0 * P1"
  (emit '(:comment "======== runtime: __mul ========"))
  (emit '(:comment "P0 = P0 * P1 (shift-and-add multiply)"))
  (emit '(label __MUL))
  ;; Save callee-saved registers (leaf function, no push-srp needed)
  (emit '(push-r R3))
  ;; R0 = multiplier (P1), R1 = multiplicand (P0), R2 = result, R3 = temp
  (emit '(A=Rx P1))           ; A = multiplier
  (emit '(Rx=A R0))           ; R0 = multiplier
  (emit '(A=Rx P0))           ; A = multiplicand
  (emit '(Rx=A R1))           ; R1 = multiplicand
  (emit '(A= 0))
  (emit '(Rx=A R2))           ; R2 = result = 0
  (emit '(label __MUL_LOOP))
  ;; Check if multiplier is zero
  (emit '(A=Rx R0))
  (emit '(Rx= 0 R3))
  (emit '(A-=Rx R3))
  (emit '(jz __MUL_END))
  ;; Check LSB of multiplier
  (emit '(A=Rx R0))
  (emit '(Rx= 1 R3))
  (emit '(A&=Rx R3))
  (emit '(Rx= 0 R3))
  (emit '(A-=Rx R3))
  (emit '(jz __MUL_SKIP))
  ;; Add multiplicand to result
  (emit '(A=Rx R2))
  (emit '(A+=Rx R1))
  (emit '(Rx=A R2))
  (emit '(label __MUL_SKIP))
  ;; Shift multiplicand left
  (emit '(A=Rx R1))
  (emit '(A=A<<1))
  (emit '(Rx=A R1))
  ;; Shift multiplier right
  (emit '(A=Rx R0))
  (emit '(A=A>>1))
  (emit '(Rx=A R0))
  (emit '(j __MUL_LOOP))
  (emit '(label __MUL_END))
  (emit '(A=Rx R2))           ; A = result
  (emit '(Rx=A P0))           ; P0 = result
  ;; Restore callee-saved registers and return
  (emit '(pop-r R3))
  (emit '(A=Rx SRP))
  (emit '(j-A)))

(defun emit-div-runtime ()
  "Emit the __div runtime function: P0 = P0 / P1"
  (emit '(:comment "======== runtime: __div ========"))
  (emit '(:comment "P0 = P0 / P1 (repeated subtraction)"))
  (emit '(label __DIV))
  ;; Save callee-saved registers (leaf function, no push-srp needed)
  (emit '(push-r R3))
  ;; R0 = dividend (will become remainder), R1 = divisor, R2 = quotient, R3 = temp
  (emit '(A=Rx P0))           ; A = dividend
  (emit '(Rx=A R0))           ; R0 = dividend
  (emit '(A=Rx P1))           ; A = divisor
  (emit '(Rx=A R1))           ; R1 = divisor
  (emit '(A= 0))
  (emit '(Rx=A R2))           ; R2 = quotient = 0
  (emit '(label __DIV_LOOP))
  ;; Check if dividend >= divisor
  (emit '(A=Rx R0))           ; A = dividend
  (emit '(A-=Rx R1))          ; A = dividend - divisor
  (emit '(jlt __DIV_END))     ; if dividend < divisor, done
  ;; dividend >= divisor: update and increment quotient
  (emit '(Rx=A R0))           ; R0 = dividend - divisor
  (emit '(A=Rx R2))
  (emit '(Rx= 1 R3))
  (emit '(A+=Rx R3))
  (emit '(Rx=A R2))           ; R2 = quotient + 1
  (emit '(j __DIV_LOOP))
  (emit '(label __DIV_END))
  (emit '(A=Rx R2))           ; A = quotient
  (emit '(Rx=A P0))           ; P0 = quotient
  ;; Restore callee-saved registers and return
  (emit '(pop-r R3))
  (emit '(A=Rx SRP))
  (emit '(j-A)))

(defun emit-mod-runtime ()
  "Emit the __mod runtime function: P0 = P0 % P1"
  (emit '(:comment "======== runtime: __mod ========"))
  (emit '(:comment "P0 = P0 % P1 (repeated subtraction)"))
  (emit '(label __MOD))
  ;; Save callee-saved registers (leaf function, no push-srp needed)
  (emit '(push-r R1))
  ;; R0 = dividend (will become remainder), R1 = divisor
  (emit '(A=Rx P0))           ; A = dividend
  (emit '(Rx=A R0))           ; R0 = dividend (becomes remainder)
  (emit '(A=Rx P1))           ; A = divisor
  (emit '(Rx=A R1))           ; R1 = divisor
  (emit '(label __MOD_LOOP))
  ;; Check if dividend >= divisor
  (emit '(A=Rx R0))           ; A = dividend
  (emit '(A-=Rx R1))          ; A = dividend - divisor
  (emit '(jlt __MOD_END))     ; if dividend < divisor, done
  ;; dividend >= divisor: update dividend
  (emit '(Rx=A R0))           ; R0 = dividend - divisor
  (emit '(j __MOD_LOOP))
  (emit '(label __MOD_END))
  (emit '(A=Rx R0))           ; A = remainder
  (emit '(Rx=A P0))           ; P0 = remainder
  ;; Restore callee-saved registers and return
  (emit '(pop-r R1))
  (emit '(A=Rx SRP))
  (emit '(j-A)))

;;; ===========================================================================
;;; Type Utilities
;;; ===========================================================================

(defun make-int-type (&optional unsigned)
  (make-type-desc :base 'int :pointer-level 0 :array-size nil
                  :size 4 :unsigned-p unsigned))

(defun make-char-type (&optional unsigned)
  ;; char is signed by default (matches GCC/Clang x86 behavior)
  (make-type-desc :base 'char :pointer-level 0 :array-size nil
                  :size 1 :unsigned-p unsigned))

(defun make-void-type ()
  (make-type-desc :base 'void :pointer-level 0 :array-size nil
                  :size 0 :unsigned-p nil))

(defun make-short-type (&optional unsigned)
  (make-type-desc :base 'int :pointer-level 0 :array-size nil
                  :size 2 :unsigned-p unsigned))

(defun make-long-type (&optional unsigned)
  (make-type-desc :base 'int :pointer-level 0 :array-size nil
                  :size 4 :unsigned-p unsigned))

;; C99 fixed-width type constructors
(defun make-int8-type (&optional unsigned)
  (make-type-desc :base 'int :pointer-level 0 :array-size nil
                  :size 1 :unsigned-p unsigned))

(defun make-int16-type (&optional unsigned)
  (make-type-desc :base 'int :pointer-level 0 :array-size nil
                  :size 2 :unsigned-p unsigned))

(defun make-int32-type (&optional unsigned)
  (make-type-desc :base 'int :pointer-level 0 :array-size nil
                  :size 4 :unsigned-p unsigned))

(defun make-pointer-type (base-type)
  (make-type-desc :base (type-desc-base base-type)
                  :pointer-level (1+ (type-desc-pointer-level base-type))
                  :array-size nil
                  :size (type-desc-size base-type)
                  :unsigned-p (type-desc-unsigned-p base-type)))

(defun make-enum-type (&optional tag-name)
  "Create a type descriptor for an enum type"
  (make-type-desc :base 'int :pointer-level 0 :array-size nil
                  :size 4 :unsigned-p nil
                  :enum-tag (or tag-name :anonymous)))

(defun align-to (offset alignment)
  "Align offset up to next multiple of alignment"
  (let ((rem (mod offset alignment)))
    (if (zerop rem) offset (+ offset (- alignment rem)))))

(defun type-alignment (type)
  "Return alignment requirement for a type (1, 2, or 4)"
  (cond
    ((> (type-desc-pointer-level type) 0) 4)
    ((eq (type-desc-base type) 'struct)
     (let ((def (gethash (type-desc-struct-tag type)
                         (compiler-state-struct-types *state*))))
       (if def (struct-def-alignment def) 4)))
    ((eq (type-desc-base type) 'union)
     (let ((def (gethash (type-desc-struct-tag type)
                         (compiler-state-union-types *state*))))
       (if def (struct-def-alignment def) 4)))
    (t (min 4 (type-size type)))))

(defun make-struct-type (tag-name)
  "Create a type descriptor for a struct type"
  (let ((def (when tag-name
               (gethash tag-name (compiler-state-struct-types *state*)))))
    (make-type-desc :base 'struct :pointer-level 0 :array-size nil
                    :size (if def (struct-def-size def) 0)
                    :struct-tag (or tag-name :anonymous))))

(defun make-union-type (tag-name)
  "Create a type descriptor for a union type"
  (let ((def (when tag-name
               (gethash tag-name (compiler-state-union-types *state*)))))
    (make-type-desc :base 'union :pointer-level 0 :array-size nil
                    :size (if def (struct-def-size def) 0)
                    :struct-tag (or tag-name :anonymous))))

(defun lookup-struct-member (struct-type member-name)
  "Look up a member in a struct or union type, returns struct-member or nil"
  (let* ((tag (type-desc-struct-tag struct-type))
         (base (type-desc-base struct-type))
         ;; Check both struct and union registries
         (def (or (gethash tag (compiler-state-struct-types *state*))
                  (gethash tag (compiler-state-union-types *state*)))))
    (when def
      (find member-name (struct-def-members def)
            :key #'struct-member-name :test #'string=))))

(defun type-size (type)
  "Return the size in bytes of a type"
  (cond
    ;; Pointers are always 4 bytes
    ((> (type-desc-pointer-level type) 0) 4)
    ;; Struct types - look up size from definition
    ((eq (type-desc-base type) 'struct)
     (let ((def (gethash (type-desc-struct-tag type)
                         (compiler-state-struct-types *state*))))
       (if def (struct-def-size def) 0)))
    ;; Union types - look up size from definition
    ((eq (type-desc-base type) 'union)
     (let ((def (gethash (type-desc-struct-tag type)
                         (compiler-state-union-types *state*))))
       (if def (struct-def-size def) 0)))
    ;; Use explicit size if set
    ((type-desc-size type) (type-desc-size type))
    ;; Fall back to base type defaults
    (t (case (type-desc-base type)
         (int 4)
         (char 1)
         (void 0)
         (otherwise 4)))))

(defun is-pointer-type (type)
  (> (type-desc-pointer-level type) 0))

;; Note: copy-type-desc is auto-generated by defstruct type-desc

;;; ===========================================================================
;;; Main Entry Points
;;; ===========================================================================

(defun split-source-lines (source)
  "Split source into a list of lines"
  (let ((lines nil)
        (start 0))
    (loop for i from 0 below (length source)
          when (char= (char source i) #\Newline)
          do (progn
               (push (subseq source start i) lines)
               (setf start (1+ i))))
    ;; Handle last line without newline
    (when (< start (length source))
      (push (subseq source start) lines))
    (nreverse lines)))

(defun compile-c (source &key (verbose nil) (annotate t) (optimize nil) (optimize-size t) (peephole nil))
  "Compile C source code to assembly S-expressions.
   :optimize-size t (default) uses runtime library calls for mul/div/mod to reduce code size.
   :optimize-size nil inlines mul/div/mod loops for better performance.
   :peephole t enables peephole optimization to eliminate redundant instructions."
  (let ((*state* (make-compiler-state))
        (*current-source-line* nil)
        (*current-source-context* nil))
    ;; Store source lines for annotations
    (setf (compiler-state-source-lines *state*) (split-source-lines source))
    (setf (compiler-state-source-annotations *state*) annotate)
    (setf (compiler-state-optimize *state*) optimize)
    (setf (compiler-state-optimize-size *state*) optimize-size)
    (setf (compiler-state-peephole *state*) peephole)

    ;; Lexical analysis
    (setf (compiler-state-tokens *state*) (tokenize source))
    (when verbose
      (format t "~%Tokens:~%")
      (dolist (tok (compiler-state-tokens *state*))
        (format t "  ~a~%" tok)))

    ;; Pre-scan for address-taken variables (affects register allocation)
    (scan-address-taken-variables)

    ;; Parsing
    (let ((ast (parse-program)))
      (when verbose
        (format t "~%AST (before optimization):~%")
        (print-ast ast))

      ;; Apply optimizations
      (when optimize
        ;; Function inlining only when full optimization is enabled
        (setf ast (inline-functions ast))
        (when verbose
          (format t "~%AST (after inlining):~%")
          (print-ast ast)))

      ;; Constant folding always runs (it's always beneficial)
      (setf ast (fold-constants ast))
      (when verbose
        (format t "~%AST (after constant folding):~%")
        (print-ast ast))

      ;; Code generation
      (generate-program ast)

      ;; Return the generated assembly
      (let ((code (get-generated-code)))
        (when verbose
          (format t "~%Generated Assembly:~%")
          (dolist (instr code)
            (format t "  ~s~%" instr)))
        code))))

(defun compile-c-file (filename &key (verbose nil))
  "Compile a C file to assembly S-expressions"
  (with-open-file (stream filename :direction :input)
    (let ((source (make-string (file-length stream))))
      (read-sequence source stream)
      (compile-c source :verbose verbose))))

(defun strip-asm-comments (asm)
  "Remove comment annotations from assembly code for the assembler"
  (remove-if (lambda (instr)
               (and (listp instr) (eq (first instr) :comment)))
             asm))

(defun compile-c-to-asm (source &key (verbose nil) (optimize nil) (optimize-size t) (peephole nil))
  "Compile C source and assemble to machine code"
  (let ((asm (compile-c source :verbose verbose :annotate nil :optimize optimize :optimize-size optimize-size :peephole peephole)))
    (assemble (strip-asm-comments asm) verbose)))

(defun run-c-program (source &key (verbose nil) (max-cycles 10000) (optimize nil) (optimize-size t) (peephole nil))
  "Compile, assemble, and run a C program, returning the result"
  (let* ((mcode (compile-c-to-asm source :verbose verbose :optimize optimize :optimize-size optimize-size :peephole peephole))
         (dmem (lr-emulator:make-dmem #x10000))  ; 64KB data memory
         (emul (lr-emulator:make-emulator mcode dmem :shared-mem t :debug verbose)))
    ;; Run the program
    (lr-emulator:run-emul emul max-cycles verbose)
    ;; Return value is in P0 (R10)
    (let ((ret-val (aref (lr-emulator::processor-state-r
                           (lr-emulator:emulated-system-processor emul))
                         10)))
      (when verbose (format t "P0 = ~a~%" ret-val))
      ret-val)))

(defun run-and-verify-registers (source &key (verbose nil) (max-cycles 10000) (optimize-size t) (peephole nil))
  "Compile, assemble, run, and verify register preservation.
   Returns (values return-value violations-list).
   violations-list is nil if all callee-saved registers were properly preserved."
  (let* ((mcode (compile-c-to-asm source :verbose verbose :optimize-size optimize-size :peephole peephole))
         (dmem (lr-emulator:make-dmem #x10000))  ; 64KB data memory
         (emul (lr-emulator:make-emulator mcode dmem :shared-mem t :debug verbose)))
    ;; Run with verification
    (multiple-value-bind (reason verifier)
        (reg-verifier:run-emul-with-verification emul max-cycles verbose)
      (declare (ignore reason))
      ;; Return value is in P0 (R10)
      (let ((result (aref (lr-emulator::processor-state-r
                           (lr-emulator:emulated-system-processor emul))
                          10)))
        (values result (reg-verifier:reg-verifier-violations verifier))))))

;;; ===========================================================================
;;; Pretty Print and Test Output
;;; ===========================================================================

(defun format-asm-instruction (instr)
  "Format an assembly instruction for pretty printing"
  (cond
    ;; Comment/annotation (keyword :comment)
    ((and (listp instr) (eq (first instr) :comment))
     (format nil ";; ~a" (second instr)))
    ;; Label
    ((and (listp instr) (eq (first instr) 'label))
     (format nil "~a:" (second instr)))
    ;; Regular instruction
    ((listp instr)
     (format nil "    ~{~a~^ ~}" instr))
    ;; Symbol instruction
    (t (format nil "    ~a" instr))))

(defun pretty-print-asm (code &optional (stream t))
  "Pretty print assembly code with proper indentation"
  (dolist (instr code)
    (format stream "~a~%" (format-asm-instruction instr))))

(defun pretty-print-asm-to-string (code)
  "Pretty print assembly code to a string"
  (with-output-to-string (s)
    (pretty-print-asm code s)))

(defun save-compilation-output (source filename &key (run-result nil run-result-p) (optimize nil))
  "Save C source and annotated assembly to a file"
  (let ((asm (compile-c source :annotate t :optimize optimize)))
    (with-open-file (out filename :direction :output :if-exists :supersede)
      ;; Write header
      (format out ";;; =============================================================~%")
      (format out ";;; C Compiler Output~a~%"
              (if optimize " (Optimized)" ""))
      (format out ";;; =============================================================~%~%")

      ;; Write C source
      (format out ";;; -------------------------------------------------------------~%")
      (format out ";;; C Source Code~%")
      (format out ";;; -------------------------------------------------------------~%")
      (let ((line-num 0))
        (dolist (line (split-source-lines source))
          (incf line-num)
          (format out ";;; ~3d: ~a~%" line-num line)))
      (format out "~%")

      ;; Write run result if provided
      (when run-result-p
        (format out ";;; -------------------------------------------------------------~%")
        (format out ";;; Execution Result: ~a~%" run-result)
        (format out ";;; -------------------------------------------------------------~%~%"))

      ;; Write annotated assembly
      (format out ";;; -------------------------------------------------------------~%")
      (format out ";;; Generated Assembly~a~%"
              (if optimize " (Optimized)" " (Annotated)"))
      (format out ";;; -------------------------------------------------------------~%~%")
      (pretty-print-asm asm out))

    filename))

(defun compile-and-save (source filename &key (run t) (max-cycles 10000))
  "Compile C source, optionally run it, and save output to file"
  (if run
      (let ((result (run-c-program source :max-cycles max-cycles)))
        (save-compilation-output source filename :run-result result)
        result)
      (progn
        (save-compilation-output source filename)
        nil)))

;;; ===========================================================================
;;; Load component files
;;; ===========================================================================

;; These will be loaded after this file
;; (load "c-lexer.lisp")
;; (load "c-parser.lisp")
;; (load "c-codegen.lisp")
