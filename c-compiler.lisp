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
           :run-c-program-ex
           :run-and-verify-registers
           :test-c-compiler
           :test-c-compiler-with-output
           ;; Re-export key types/functions
           :tokenize
           :parse-program
           :generate-program
           ;; Optimization
           :fold-constants
           :unroll-loops
           ;; Pretty printing and test output
           :pretty-print-asm
           :pretty-print-asm-to-string
           :save-compilation-output
           :compile-and-save
           ;; Memory inspection helpers
           :get-label-address
           :read-memory-value
           :extract-memory-value))

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
  const-p        ; t for const, enables constant propagation
  enum-tag       ; nil, :anonymous, or tag-name string for enum types
  struct-tag     ; nil, :anonymous, or tag-name string for struct types
  struct-scope   ; scope level where struct/union was defined
  return-type)   ; for function pointers, the full return type

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
  (need-indirect-call nil) ; set to t when indirect function calls are used
  (need-mul64-runtime nil) ; set to t when __mul64 runtime is needed
  (need-div64-runtime nil) ; set to t when __div64 runtime is needed
  (need-mod64-runtime nil) ; set to t when __mod64 runtime is needed
  (function-table (make-hash-table :test 'equal)) ; name -> function AST node for inlining
  (dead-functions (make-hash-table :test 'equal)) ; functions to not emit (fully inlined)
  (enum-types (make-hash-table :test 'equal))    ; tag-name -> list of (name . value) constants
  (struct-types (make-hash-table :test 'equal))  ; tag-name -> struct-def
  (union-types (make-hash-table :test 'equal))   ; tag-name -> struct-def (all offsets=0)
  (typedef-types (make-hash-table :test 'equal)) ; typedef-name -> type-desc
  (global-defs (make-hash-table :test 'equal))   ; name -> :tentative or :definite
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

(defun add-global-symbol (name type storage &optional offset)
  "Add a symbol at global scope (scope 0, no function)"
  (let* ((key (make-symbol-key name 0 nil))
         (entry (make-sym-entry :name name
                                :type type
                                :storage storage
                                :offset offset
                                :scope 0
                                :function nil)))
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

(defun make-c-label (name)
  "Create a label symbol for a C identifier, avoiding Common Lisp reserved names"
  (let ((upper-name (string-upcase name)))
    ;; Check if the name conflicts with CL special symbols
    (if (member upper-name '("GO" "IF" "OR" "AND" "DO" "NIL" "T") :test #'string=)
        (intern (format nil "_~a" upper-name) :c-compiler)
        (intern upper-name :c-compiler))))

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
    (emit-mod-runtime))
  ;; Emit indirect call helper if needed
  (when (compiler-state-need-indirect-call *state*)
    (emit-indirect-call-runtime))
  ;; Emit 64-bit multiply runtime if needed
  (when (compiler-state-need-mul64-runtime *state*)
    (emit-mul64-runtime))
  ;; Emit 64-bit divide runtime if needed
  (when (compiler-state-need-div64-runtime *state*)
    (emit-div64-runtime))
  ;; Emit 64-bit modulo runtime if needed
  (when (compiler-state-need-mod64-runtime *state*)
    (emit-mod64-runtime)))

(defun emit-indirect-call-runtime ()
  "Emit the __indirect_call runtime helper: jumps to address in R0"
  (emit '(:comment "======== runtime: __indirect_call ========"))
  (emit '(:comment "Jump to function address in R0. SRP is preserved."))
  (emit '(label |__indirect_call|))
  (emit '(A=Rx R0))   ; load function address into A
  (emit '(j-a)))      ; jump to it - called function returns to JSR's caller

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

(defun emit-mul64-runtime ()
  "Emit the __mul64 runtime function: P0:P1 = P0:P1 * P2:P3 (64-bit multiply)
   Uses shift-and-add algorithm."
  (emit '(:comment "======== runtime: __mul64 ========"))
  (emit '(:comment "P0:P1 = P0:P1 * P2:P3 (64-bit shift-and-add)"))
  (emit '(label __MUL64))
  ;; Save R0-R8 (push-r Rn pushes R0 through Rn)
  (emit '(push-r R8))
  ;; R0:R1 = multiplicand (copy of P0:P1)
  ;; R2:R3 = multiplier (copy of P2:P3)
  ;; R4:R5 = result (starts at 0)
  ;; R6 = 0 (for comparisons), R7/R8 = temps
  (emit '(A=Rx P0))
  (emit '(Rx=A R0))           ; R0 = multiplicand low
  (emit '(A=Rx P1))
  (emit '(Rx=A R1))           ; R1 = multiplicand high
  (emit '(A=Rx P2))
  (emit '(Rx=A R2))           ; R2 = multiplier low
  (emit '(A=Rx P3))
  (emit '(Rx=A R3))           ; R3 = multiplier high
  ;; Use explicit Rx= 0 to avoid peephole optimization removing A= 0
  (emit '(Rx= 0 R4))           ; R4 = result low = 0
  (emit '(Rx= 0 R5))           ; R5 = result high = 0
  (emit '(Rx= 0 R6))           ; R6 = 0 (for comparisons)
  (emit '(label __MUL64_LOOP))
  ;; Check if multiplier (R2:R3) is zero
  (emit '(A=Rx R2))
  (emit '(A\|=Rx R3))
  (emit '(A-=Rx R6))
  (emit '(jz __MUL64_END))
  ;; Check LSB of multiplier
  (emit '(A=Rx R2))
  (emit '(Rx= 1 R7))
  (emit '(A&=Rx R7))
  (emit '(A-=Rx R6))
  (emit '(jz __MUL64_SKIP_ADD))
  ;; Add multiplicand to result (64-bit add with carry)
  (emit '(A=Rx R4))
  (emit '(A+=Rx R0))           ; result_low += multiplicand_low (sets carry)
  (emit '(Rx=A R4))
  (emit '(A=Rx R5))
  (emit '(A+=Rx+c R1))         ; result_high += multiplicand_high + carry
  (emit '(Rx=A R5))
  (emit '(label __MUL64_SKIP_ADD))
  ;; Shift multiplicand left (64-bit)
  ;; First get carry bit from low word's bit 31
  (emit '(A=Rx R0))
  (emit '(Rx= #x80000000 R7))
  (emit '(A&=Rx R7))           ; isolate bit 31
  (emit '(Rx=A R8))            ; R8 = 0 or 0x80000000
  ;; Shift low word left
  (emit '(A=Rx R0))
  (emit '(A=A<<1))
  (emit '(Rx=A R0))
  ;; Shift high word left and add carry
  (emit '(A=Rx R1))
  (emit '(A=A<<1))
  ;; Add carry if bit 31 was set (R8 != 0)
  (emit '(A=Rx R8))
  (emit '(A-=Rx R6))           ; compare R8 to 0
  (emit '(jz __MUL64_NO_CARRY))
  (emit '(A=Rx R1))
  (emit '(A=A<<1))
  (emit '(Rx= 1 R7))
  (emit '(A\|=Rx R7))          ; add carry bit
  (emit '(Rx=A R1))
  (emit '(j __MUL64_SHIFT_DONE))
  (emit '(label __MUL64_NO_CARRY))
  (emit '(A=Rx R1))
  (emit '(A=A<<1))
  (emit '(Rx=A R1))
  (emit '(label __MUL64_SHIFT_DONE))
  ;; Shift multiplier right (64-bit)
  ;; First get bit 0 from high word
  (emit '(A=Rx R3))
  (emit '(Rx= 1 R7))
  (emit '(A&=Rx R7))           ; isolate bit 0 of high
  (emit '(Rx=A R8))            ; R8 = 0 or 1
  ;; Shift high word right
  (emit '(A=Rx R3))
  (emit '(A=A>>1))
  (emit '(Rx=A R3))
  ;; Shift low word right
  (emit '(A=Rx R2))
  (emit '(A=A>>1))
  ;; Add bit from high word if it was set
  (emit '(A=Rx R8))
  (emit '(A-=Rx R6))
  (emit '(jz __MUL64_NO_HI_BIT))
  (emit '(A=Rx R2))
  (emit '(A=A>>1))
  (emit '(Rx= #x80000000 R7))
  (emit '(A\|=Rx R7))          ; set bit 31
  (emit '(Rx=A R2))
  (emit '(j __MUL64_LOOP))
  (emit '(label __MUL64_NO_HI_BIT))
  (emit '(A=Rx R2))
  (emit '(A=A>>1))
  (emit '(Rx=A R2))
  (emit '(j __MUL64_LOOP))
  (emit '(label __MUL64_END))
  ;; Copy result to P0:P1
  (emit '(A=Rx R4))
  (emit '(Rx=A P0))
  (emit '(A=Rx R5))
  (emit '(Rx=A P1))
  ;; Restore R0-R8 and return
  (emit '(pop-r R8))
  (emit '(A=Rx SRP))
  (emit '(j-A)))

(defun emit-div64-runtime ()
  "Emit the __div64 runtime function: P0:P1 = P0:P1 / P2:P3 (64-bit divide)
   Uses repeated subtraction algorithm (unsigned division).
   Note: This is O(quotient) - slow for large quotients."
  (emit '(:comment "======== runtime: __div64 ========"))
  (emit '(:comment "P0:P1 = P0:P1 / P2:P3 (64-bit repeated subtraction)"))
  (emit '(label __DIV64))
  ;; Save R0-R7 (push-r Rn pushes R0 through Rn)
  (emit '(push-r R7))
  ;; R0:R1 = dividend (becomes remainder)
  ;; R2:R3 = divisor
  ;; R4:R5 = quotient (starts at 0)
  ;; R6 = temp, R7 = 0 for comparisons
  (emit '(A=Rx P0))
  (emit '(Rx=A R0))           ; R0 = dividend low
  (emit '(A=Rx P1))
  (emit '(Rx=A R1))           ; R1 = dividend high
  (emit '(A=Rx P2))
  (emit '(Rx=A R2))           ; R2 = divisor low
  (emit '(A=Rx P3))
  (emit '(Rx=A R3))           ; R3 = divisor high
  ;; Use explicit Rx= 0 to avoid peephole optimization removing A= 0
  (emit '(Rx= 0 R4))           ; quotient low = 0
  (emit '(Rx= 0 R5))           ; quotient high = 0
  (emit '(Rx= 0 R7))           ; R7 = 0 for comparisons
  ;; Check for division by zero - if divisor is 0, return 0
  (emit '(A=Rx R2))
  (emit '(A\|=Rx R3))
  (emit '(A-=Rx R7))
  (emit '(jz __DIV64_END))
  (emit '(label __DIV64_LOOP))
  ;; Compare dividend (R0:R1) >= divisor (R2:R3)
  ;; Compare high words first
  (emit '(A=Rx R1))
  (emit '(A-=Rx R3))
  (emit '(jlt __DIV64_END))   ; dividend.high < divisor.high -> done
  (emit '(jnz __DIV64_DO_SUB)) ; dividend.high > divisor.high -> subtract
  ;; High words equal, compare low words
  (emit '(A=Rx R0))
  (emit '(A-=Rx R2))
  (emit '(jlt __DIV64_END))   ; dividend.low < divisor.low -> done
  (emit '(label __DIV64_DO_SUB))
  ;; dividend >= divisor, so subtract: R0:R1 -= R2:R3
  ;; First save original R0 to detect borrow
  (emit '(A=Rx R0))
  (emit '(Rx=A R6))           ; R6 = original low
  ;; Subtract low words
  (emit '(A=Rx R0))
  (emit '(A-=Rx R2))
  (emit '(Rx=A R0))
  ;; Subtract high words
  (emit '(A=Rx R1))
  (emit '(A-=Rx R3))
  (emit '(Rx=A R1))
  ;; Check for borrow: if new_low > old_low, we wrapped (borrow occurred)
  (emit '(A=Rx R0))           ; new low
  (emit '(A-=Rx R6))          ; new_low - old_low
  ;; jle = jlt or jz (no jle instruction available)
  (emit '(jlt __DIV64_NO_BORROW))  ; if new < old, no wrap
  (emit '(jz __DIV64_NO_BORROW))   ; if new = old, no wrap
  ;; Borrow occurred - decrement high by 1
  (emit '(A=Rx R1))
  (emit '(Rx= 1 R7))
  (emit '(A-=Rx R7))
  (emit '(Rx=A R1))
  (emit '(Rx= 0 R7))          ; restore R7 = 0
  (emit '(label __DIV64_NO_BORROW))
  ;; Increment quotient
  (emit '(A=Rx R4))
  (emit '(Rx= 1 R6))
  (emit '(A+=Rx R6))
  (emit '(Rx=A R4))
  ;; Check for overflow: if result is 0, we wrapped (0xFFFFFFFF + 1 = 0)
  (emit '(Rx= 0 R7))
  (emit '(A-=Rx R7))
  (emit '(jnz __DIV64_LOOP))  ; if not zero, no overflow, continue
  ;; Overflow - increment high quotient
  (emit '(A=Rx R5))
  (emit '(Rx= 1 R6))
  (emit '(A+=Rx R6))          ; quotient.high += 1
  (emit '(Rx=A R5))
  (emit '(j __DIV64_LOOP))
  (emit '(label __DIV64_END))
  ;; Copy quotient to P0:P1
  (emit '(A=Rx R4))
  (emit '(Rx=A P0))
  (emit '(A=Rx R5))
  (emit '(Rx=A P1))
  ;; Restore R0-R7 and return
  (emit '(pop-r R7))
  (emit '(A=Rx SRP))
  (emit '(j-A)))

(defun emit-mod64-runtime ()
  "Emit the __mod64 runtime function: P0:P1 = P0:P1 % P2:P3 (64-bit modulo)
   Uses repeated subtraction - returns remainder after division."
  (emit '(:comment "======== runtime: __mod64 ========"))
  (emit '(:comment "P0:P1 = P0:P1 % P2:P3 (64-bit modulo)"))
  (emit '(label __MOD64))
  ;; Save R0-R7 (push-r Rn pushes R0 through Rn)
  (emit '(push-r R7))
  ;; R0:R1 = dividend (becomes remainder)
  ;; R2:R3 = divisor
  ;; R6 = temp, R7 = 0 for comparisons
  (emit '(A=Rx P0))
  (emit '(Rx=A R0))
  (emit '(A=Rx P1))
  (emit '(Rx=A R1))
  (emit '(A=Rx P2))
  (emit '(Rx=A R2))
  (emit '(A=Rx P3))
  (emit '(Rx=A R3))
  ;; Use explicit Rx= 0 for consistency with other runtime functions
  (emit '(Rx= 0 R7))           ; R7 = 0 for comparisons
  ;; Check for division by zero - return dividend as-is
  (emit '(A=Rx R2))
  (emit '(A\|=Rx R3))
  (emit '(A-=Rx R7))
  (emit '(jz __MOD64_END))
  (emit '(label __MOD64_LOOP))
  ;; Compare R0:R1 >= R2:R3
  (emit '(A=Rx R1))
  (emit '(A-=Rx R3))
  (emit '(jlt __MOD64_END))
  (emit '(jnz __MOD64_DO_SUB))
  (emit '(A=Rx R0))
  (emit '(A-=Rx R2))
  (emit '(jlt __MOD64_END))
  (emit '(label __MOD64_DO_SUB))
  ;; Subtract: R0:R1 -= R2:R3
  (emit '(A=Rx R0))
  (emit '(Rx=A R6))           ; save old low
  (emit '(A=Rx R0))
  (emit '(A-=Rx R2))
  (emit '(Rx=A R0))
  (emit '(A=Rx R1))
  (emit '(A-=Rx R3))
  (emit '(Rx=A R1))
  ;; Check borrow
  (emit '(A=Rx R0))
  (emit '(A-=Rx R6))
  ;; jle = jlt or jz (no jle instruction available)
  (emit '(jlt __MOD64_NO_BORROW))
  (emit '(jz __MOD64_NO_BORROW))
  (emit '(A=Rx R1))
  (emit '(Rx= 1 R7))
  (emit '(A-=Rx R7))
  (emit '(Rx=A R1))
  (emit '(Rx= 0 R7))
  (emit '(label __MOD64_NO_BORROW))
  (emit '(j __MOD64_LOOP))
  (emit '(label __MOD64_END))
  ;; Remainder is in R0:R1
  (emit '(A=Rx R0))
  (emit '(Rx=A P0))
  (emit '(A=Rx R1))
  (emit '(Rx=A P1))
  ;; Restore R0-R7 and return
  (emit '(pop-r R7))
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

(defun make-longlong-type (&optional unsigned)
  "Create a 64-bit integer type descriptor"
  (make-type-desc :base 'longlong :pointer-level 0 :array-size nil
                  :size 8 :unsigned-p unsigned))

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
     (let* ((scope (type-desc-struct-scope type))
            (def (if scope
                     (lookup-struct-def-at-scope (type-desc-struct-tag type) scope)
                     (nth-value 0 (lookup-struct-def (type-desc-struct-tag type))))))
       (if def (struct-def-alignment def) 4)))
    ((eq (type-desc-base type) 'union)
     (let* ((scope (type-desc-struct-scope type))
            (def (if scope
                     (lookup-union-def-at-scope (type-desc-struct-tag type) scope)
                     (nth-value 0 (lookup-union-def (type-desc-struct-tag type))))))
       (if def (struct-def-alignment def) 4)))
    (t (min 4 (type-size type)))))

(defun make-struct-key (tag-name scope)
  "Create a key for struct lookup: (tag-name . scope)"
  (cons tag-name scope))

(defun register-struct-def (tag-name def)
  "Register a struct definition at the current scope"
  (let ((key (make-struct-key tag-name (compiler-state-scope-level *state*))))
    (setf (gethash key (compiler-state-struct-types *state*)) def)))

(defun lookup-struct-def (tag-name)
  "Look up a struct definition, searching from current scope down to 0"
  (when tag-name
    (loop for scope from (compiler-state-scope-level *state*) downto 0
          for key = (make-struct-key tag-name scope)
          for def = (gethash key (compiler-state-struct-types *state*))
          when def return (values def scope))))

(defun lookup-struct-def-at-scope (tag-name scope-level)
  "Look up a struct definition at a specific scope level or below"
  (when tag-name
    (loop for scope from scope-level downto 0
          for key = (make-struct-key tag-name scope)
          for def = (gethash key (compiler-state-struct-types *state*))
          when def return def)))

(defun register-union-def (tag-name def)
  "Register a union definition at the current scope"
  (let ((key (make-struct-key tag-name (compiler-state-scope-level *state*))))
    (setf (gethash key (compiler-state-union-types *state*)) def)))

(defun lookup-union-def (tag-name)
  "Look up a union definition, searching from current scope down to 0"
  (when tag-name
    (loop for scope from (compiler-state-scope-level *state*) downto 0
          for key = (make-struct-key tag-name scope)
          for def = (gethash key (compiler-state-union-types *state*))
          when def return (values def scope))))

(defun lookup-union-def-at-scope (tag-name scope-level)
  "Look up a union definition at a specific scope level or below"
  (when tag-name
    (loop for scope from scope-level downto 0
          for key = (make-struct-key tag-name scope)
          for def = (gethash key (compiler-state-union-types *state*))
          when def return def)))

(defun make-struct-type (tag-name)
  "Create a type descriptor for a struct type"
  (multiple-value-bind (def scope) (lookup-struct-def tag-name)
    (make-type-desc :base 'struct :pointer-level 0 :array-size nil
                    :size (if def (struct-def-size def) 0)
                    :struct-tag (or tag-name :anonymous)
                    :struct-scope scope)))

(defun make-union-type (tag-name)
  "Create a type descriptor for a union type"
  (multiple-value-bind (def scope) (lookup-union-def tag-name)
    (make-type-desc :base 'union :pointer-level 0 :array-size nil
                    :size (if def (struct-def-size def) 0)
                    :struct-tag (or tag-name :anonymous)
                    :struct-scope scope)))

(defun lookup-struct-member (struct-type member-name)
  "Look up a member in a struct or union type, returns struct-member or nil"
  (let* ((tag (type-desc-struct-tag struct-type))
         (scope (type-desc-struct-scope struct-type))
         ;; Use stored scope if available, otherwise search from current scope
         (def (if scope
                  (or (lookup-struct-def-at-scope tag scope)
                      (lookup-union-def-at-scope tag scope))
                  (or (nth-value 0 (lookup-struct-def tag))
                      (nth-value 0 (lookup-union-def tag))))))
    (when def
      (find member-name (struct-def-members def)
            :key #'struct-member-name :test #'string=))))

(defun type-size (type)
  "Return the size in bytes of a type"
  (cond
    ;; Arrays - multiply element count by element size (check BEFORE pointers!)
    ;; This handles arrays of pointers like int* a[2] correctly
    ;; For multi-dimensional arrays, total-array-elements computes the product
    ((type-desc-array-size type)
     (let* ((array-size (type-desc-array-size type))
            (total-elements (if (or (numberp array-size) (listp array-size))
                                (total-array-elements array-size)
                                1))
            (element-type (make-type-desc :base (type-desc-base type)
                                          :pointer-level (type-desc-pointer-level type)
                                          :size (type-desc-size type)
                                          :struct-tag (type-desc-struct-tag type)
                                          :struct-scope (type-desc-struct-scope type)))
            (elem-size (type-size element-type)))
       (* total-elements elem-size)))
    ;; Pointers are always 4 bytes (non-array pointers only reach here)
    ((> (type-desc-pointer-level type) 0) 4)
    ;; Struct types - look up size from definition
    ((eq (type-desc-base type) 'struct)
     (let* ((scope (type-desc-struct-scope type))
            (def (if scope
                     (lookup-struct-def-at-scope (type-desc-struct-tag type) scope)
                     (nth-value 0 (lookup-struct-def (type-desc-struct-tag type))))))
       (if def (struct-def-size def) 0)))
    ;; Union types - look up size from definition
    ((eq (type-desc-base type) 'union)
     (let* ((scope (type-desc-struct-scope type))
            (def (if scope
                     (lookup-union-def-at-scope (type-desc-struct-tag type) scope)
                     (nth-value 0 (lookup-union-def (type-desc-struct-tag type))))))
       (if def (struct-def-size def) 0)))
    ;; Use explicit size if set
    ((type-desc-size type) (type-desc-size type))
    ;; Fall back to base type defaults
    (t (case (type-desc-base type)
         (int 4)
         (char 1)
         (void 0)
         (longlong 8)
         (otherwise 4)))))

(defun get-outer-dimension (array-size)
  "Get outermost dimension from array-size (integer or list)"
  (if (listp array-size) (car array-size) array-size))

(defun get-remaining-dimensions (array-size)
  "Get remaining dimensions after stripping outer. Returns nil, integer, or list."
  (when (listp array-size)
    (let ((rest (cdr array-size)))
      (cond ((null rest) nil)
            ((= (length rest) 1) (car rest))
            (t rest)))))

(defun total-array-elements (array-size)
  "Calculate total element count (product of all dimensions)"
  (if (listp array-size)
      (reduce #'* array-size)
      array-size))

(defun is-pointer-type (type)
  (> (type-desc-pointer-level type) 0))

(defun is-longlong-type (type)
  "Check if type is a 64-bit long long type"
  (and type
       (type-desc-p type)
       (eq (type-desc-base type) 'longlong)
       (= (type-desc-pointer-level type) 0)))

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

      ;; Loop unrolling (after constant folding so bounds are known)
      (when optimize
        (setf ast (unroll-loops ast))
        (when verbose
          (format t "~%AST (after loop unrolling):~%")
          (print-ast ast))
        ;; Re-fold constants after unrolling (loop var substitution creates new constants)
        (setf ast (fold-constants ast))
        (when verbose
          (format t "~%AST (after re-folding):~%")
          (print-ast ast)))

      ;; Dead code elimination (removes unused variables after constant propagation)
      (setf ast (dead-code-elimination ast))
      (when verbose
        (format t "~%AST (after dead code elimination):~%")
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
;;; Memory Inspection for Test Observability
;;; ===========================================================================

(defun get-label-address (symtab label-name)
  "Look up a label's address in the symbol table.
   The symtab maps address -> label-name (uppercase)."
  (let ((upper-name (string-upcase label-name)))
    (maphash (lambda (addr name)
               (when (string= name upper-name)
                 (return-from get-label-address addr)))
             symtab)
    (error "Label ~S not found in symbol table" label-name)))

(defun read-memory-value (dmem addr size)
  "Read a value of given size (1, 2, 4, or 8 bytes) from memory"
  (case size
    (1 (lr-emulator:mem-read-byte dmem addr))
    (2 (lr-emulator:mem-read-word dmem addr))
    (4 (lr-emulator:mem-read-dword dmem addr))
    (8 ;; 64-bit: read low and high dwords, combine (little-endian)
     (let ((low (lr-emulator:mem-read-dword dmem addr))
           (high (lr-emulator:mem-read-dword dmem (+ addr 4))))
       (logior low (ash high 32))))
    (otherwise (error "Unsupported memory read size: ~a" size))))

(defun extract-memory-value (dmem symtab spec)
  "Extract memory value(s) according to spec.
   SPEC is a plist with keys:
     :label - label name (string)
     :size  - bytes per element (1, 2, 4, or 8; default 4)
     :count - number of elements to read (default 1)
     :index - starting index (default 0)
   Returns (label-name . value) for count=1, or (label-name . (values...)) for count>1."
  (let* ((label (getf spec :label))
         (size (or (getf spec :size) 4))
         (count (or (getf spec :count) 1))
         (index (or (getf spec :index) 0))
         (base-addr (get-label-address symtab label))
         (addr (+ base-addr (* index size))))
    (if (= count 1)
        ;; Single value
        (cons label (read-memory-value dmem addr size))
        ;; Multiple values
        (cons label
              (loop for i from 0 below count
                    for a = (+ addr (* i size))
                    collect (read-memory-value dmem a size))))))

(defun run-c-program-ex (source &key (verbose nil) (max-cycles 10000)
                                     (optimize nil) (optimize-size t) (peephole nil)
                                     (inspect-memory nil))
  "Compile, assemble, and run a C program. Returns (values result memory-values).

   INSPECT-MEMORY is a list of memory inspection specs:
     (:label \"name\" :size 4)           - Read 4 bytes at label
     (:label \"name\" :size 8)           - Read 8 bytes (64-bit) at label
     (:label \"name\" :count 3 :size 4)  - Read 3 dwords starting at label
     (:label \"arr\" :index 2 :size 4)   - Read arr[2] (4 bytes each)

   Returns: (values return-value alist-of-memory-values)
   where alist is ((\"name\" . value) ...) or ((\"name\" . (v0 v1 v2)) ...)"

  (let* ((asm (compile-c source :verbose verbose :annotate nil
                         :optimize optimize :optimize-size optimize-size
                         :peephole peephole))
         (symtab (make-hash-table :test 'eql))
         (mcode (assemble (strip-asm-comments asm) verbose symtab))
         (dmem (lr-emulator:make-dmem #x10000))
         (emul (lr-emulator:make-emulator mcode dmem :shared-mem t :debug verbose)))

    ;; Run the program
    (lr-emulator:run-emul emul max-cycles verbose)

    ;; Extract return value
    (let ((ret-val (aref (lr-emulator::processor-state-r
                          (lr-emulator:emulated-system-processor emul)) 10)))

      ;; Extract memory values if requested
      (let ((mem-values
              (when inspect-memory
                (loop for spec in inspect-memory
                      collect (extract-memory-value dmem symtab spec)))))

        (when verbose
          (format t "P0 = ~a~%" ret-val)
          (when mem-values
            (format t "Memory: ~S~%" mem-values)))

        (values ret-val mem-values)))))

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

(defun save-compilation-output (source filename &key (run-result nil run-result-p)
                                                      (optimize nil) (optimize-size t) (peephole nil))
  "Save C source and annotated assembly to a file"
  (let* ((opt-label (cond ((and optimize (not optimize-size)) " (-O)")
                          (optimize " (-Os)")
                          (t "")))
         (asm (compile-c source :annotate t :optimize optimize :optimize-size optimize-size :peephole peephole)))
    (with-open-file (out filename :direction :output :if-exists :supersede)
      ;; Write header
      (format out ";;; =============================================================~%")
      (format out ";;; C Compiler Output~a~%" opt-label)
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
      (format out ";;; Generated Assembly~a~%" opt-label)
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
