;;; ===========================================================================
;;; ====================== C Code Generator for liteRISC ======================
;;; ===========================================================================
;;; Generates S-expression assembly for the liteRISC assembler

(in-package :c-compiler)

;;; ===========================================================================
;;; Virtual Register Allocation
;;; ===========================================================================

;;; Register usage (after allocation):
;;; R0-R9  : Allocated contiguously from R0 upward for temps and locals
;;; P0-P3  : Parameters (R10-R13), P0 is also return value
;;; SRP    : Subroutine return pointer (R14)
;;; SP     : Stack pointer (R15)
;;; A      : Accumulator - primary computation register
;;;
;;; During code generation, we use virtual registers V0, V1, V2, ...
;;; These are mapped to physical registers R0, R1, R2, ... by the allocator.

(defparameter *param-regs* '(P0 P1 P2 P3))

;;; Virtual register counter for current function
(defvar *vreg-counter* 0)

;;; Map from local variable index to virtual register
;;; (replaces fixed R6-R9 allocation)
(defvar *local-vreg-map* nil)

;;; Track local register usage for current function (for compatibility)
(defvar *local-reg-count* 0)

;;; Track maximum physical register index after allocation (for PUSH/POP)
(defvar *max-temp-reg-index* -1)

;;; Bytes pushed by push-r for callee-saved regs (used for stack param offset)
(defvar *pushed-reg-bytes* 0)

;;; Flag to enable/disable virtual register allocation
;;; When nil, use old physical register allocation for compatibility
(defvar *use-virtual-regs* t)

;;; Old physical register lists (for fallback mode)
(defparameter *temp-regs* '(R0 R1 R2 R3 R4 R5))
(defparameter *local-regs* '(R6 R7 R8 R9))
(defvar *reg-in-use* nil)

(defun init-registers ()
  "Initialize register allocation state"
  (setf *vreg-counter* 0)
  (setf *local-vreg-map* (make-hash-table :test 'eql))
  (setf *max-temp-reg-index* -1)
  (setf *pushed-reg-bytes* 0)
  ;; Also init old-style tracking for fallback
  (setf *reg-in-use* (make-array 6 :initial-element nil)))

(defun alloc-temp-reg ()
  "Allocate a temporary register (returns virtual register V0, V1, ...)"
  (if *use-virtual-regs*
      ;; Virtual register mode: return V0, V1, V2, ...
      (prog1 (make-vreg *vreg-counter*)
        (incf *vreg-counter*))
      ;; Fallback to physical register mode
      (loop for i from 0 below 6
            when (not (aref *reg-in-use* i))
            do (progn
                 (setf (aref *reg-in-use* i) t)
                 (when (> i *max-temp-reg-index*)
                   (setf *max-temp-reg-index* i))
                 (return-from alloc-temp-reg (nth i *temp-regs*)))
            finally (compiler-error "Out of temporary registers"))))

(defun free-temp-reg (reg)
  "Free a temporary register (no-op for virtual registers)"
  (unless *use-virtual-regs*
    ;; Only needed in physical register mode
    (let ((idx (position reg *temp-regs*)))
      (when idx
        (setf (aref *reg-in-use* idx) nil)))))

(defun save-temp-regs ()
  "Return list of temp registers currently in use (for physical mode only)"
  (unless *use-virtual-regs*
    (loop for i from 0 below 6
          when (aref *reg-in-use* i)
          collect (nth i *temp-regs*))))

(defun get-local-reg (index)
  "Get register for local variable by index.
   In virtual mode: returns a virtual register (allocated lazily).
   In physical mode: returns R6-R9."
  (if *use-virtual-regs*
      ;; Virtual register mode: allocate a vreg for this local index
      (let ((existing (gethash index *local-vreg-map*)))
        (or existing
            (let ((vreg (make-vreg *vreg-counter*)))
              (incf *vreg-counter*)
              (setf (gethash index *local-vreg-map*) vreg)
              vreg)))
      ;; Physical register mode
      (nth index *local-regs*)))

;;; ===========================================================================
;;; Code Generation Helpers
;;; ===========================================================================

(defun emit-test-zero ()
  "Emit code to test if A is zero (result in flags for jz/jnz).
   Uses a temp register to hold 0, properly handling virtual registers."
  (let ((temp (alloc-temp-reg)))
    (emit `(Rx= 0 ,temp))
    (emit `(A-=Rx ,temp))
    (free-temp-reg temp)))

;;; ===========================================================================
;;; Code Generation Context
;;; ===========================================================================

(defvar *break-label* nil)      ; target for break statements
(defvar *continue-label* nil)   ; target for continue statements
(defvar *function-end-label* nil) ; label at end of current function
(defvar *frame-size* 0)         ; size of current stack frame
(defvar *is-leaf-function* t)   ; true if function makes no calls
(defvar *string-literals* nil)  ; list of (label . string) pairs
(defvar *current-param-count* 0) ; number of parameters in current function
(defvar *param-save-offset* 0)  ; offset where saved params start on stack
(defvar *skip-final-return-jump* nil) ; when t, generate-return skips the jump
(defvar *user-labels* nil)      ; hash table: user label name -> generated asm label

;;; ===========================================================================
;;; Sized Memory Access Helpers
;;; ===========================================================================

(defun emit-load-sized (type temp-reg)
  "Emit load instruction based on type size. Address in A, result in temp-reg."
  (let ((size (type-size type)))
    (case size
      (1 (emit `(Rx=M[A].b ,temp-reg)))
      (2 (emit `(Rx=M[A].w ,temp-reg)))
      (otherwise (emit `(Rx=M[A] ,temp-reg))))))

(defun emit-store-sized (type value-reg)
  "Emit store instruction based on type size. Address in A, value in value-reg."
  (let ((size (type-size type)))
    (case size
      (1 (emit `(M[A].b=Rx ,value-reg)))
      (2 (emit `(M[A].w=Rx ,value-reg)))
      (otherwise (emit `(M[A]=Rx ,value-reg))))))

(defun emit-load-sized-offset (type offset temp-reg)
  "Emit load instruction with offset based on type size. Base address in A."
  (let ((size (type-size type)))
    (case size
      (1 (emit `(Rx=M[A+n].b ,offset ,temp-reg)))
      (2 ;; No M[A+n].w instruction - compute address first
       (emit `(Rx= ,offset ,temp-reg))
       (emit `(A+=Rx ,temp-reg))
       (emit `(Rx=M[A].w ,temp-reg)))
      (otherwise (emit `(Rx=M[A+n] ,offset ,temp-reg))))))

(defun emit-store-sized-offset (type offset value-reg)
  "Emit store instruction with offset based on type size. Base address in A."
  (let ((size (type-size type)))
    (case size
      (1 (emit `(M[A+n].b=Rx ,offset ,value-reg)))
      (2 ;; No M[A+n].w instruction - compute address first
       (let ((addr-temp (alloc-temp-reg)))
         (emit `(Rx= ,offset ,addr-temp))
         (emit `(A+=Rx ,addr-temp))
         (emit `(M[A].w=Rx ,value-reg))
         (free-temp-reg addr-temp)))
      (otherwise (emit `(M[A+n]=Rx ,offset ,value-reg))))))

;;; ===========================================================================
;;; Sign Extension Helpers
;;; ===========================================================================

(defun emit-sign-extend (type)
  "Sign extend value in A if needed for signed types smaller than 32 bits"
  (when (and (not (type-desc-unsigned-p type))
             (< (type-size type) 4))
    (case (type-size type)
      (1 (emit-sign-extend-byte))
      (2 (emit-sign-extend-word)))))

(defun emit-sign-extend-byte ()
  "Sign extend byte in A (bit 7 → bits 8-31)"
  (emit '(sex-a-b)))

(defun emit-sign-extend-word ()
  "Sign extend word in A (bit 15 → bits 16-31)"
  (emit '(sex-a-w)))

(defun emit-mask-to-size (type)
  "Mask value in A to appropriate size (for storing sub-word values)"
  (let ((size (type-size type)))
    (case size
      (1 (emit `(Rx= #xFF R0))
         (emit `(A&=Rx R0)))
      (2 (emit `(Rx= #xFFFF R0))
         (emit `(A&=Rx R0))))))

(defun emit-promote-to-int (type)
  "Promote value in A to int (32-bit) for arithmetic"
  (when (< (type-size type) 4)
    (if (type-desc-unsigned-p type)
        ;; Unsigned: already zero-extended by .b/.w load
        nil
        ;; Signed: need sign extension
        (emit-sign-extend type))))

;;; ===========================================================================
;;; Main Code Generation Entry Points
;;; ===========================================================================

(defun generate-program (ast)
  "Generate code for a complete program"
  ;; Reset all codegen state
  (init-registers)
  (setf *string-literals* nil)
  (setf *break-label* nil)
  (setf *continue-label* nil)
  (setf *function-end-label* nil)
  (setf *frame-size* 0)
  (setf *is-leaf-function* t)
  (setf *current-param-count* 0)
  (setf *param-save-offset* 0)
  (setf *skip-final-return-jump* nil)
  (setf *local-reg-count* 0)
  ;; Reset scope level to global scope for struct/type lookups
  (setf (compiler-state-scope-level *state*) 0)

  ;; First pass: collect string literals
  (collect-strings ast)

  ;; Generate startup code that calls main and then halts
  (emit '(label _START))
  ;; Initialize stack pointer to a reasonable value
  (emit '(Rx= #x8000 SP))  ; 32KB stack
  ;; Call main
  (emit '(jsr MAIN))
  ;; Store return value from main (in P0) for test retrieval
  ;; Then enter infinite loop to signal completion
  (emit '(label _HALT))
  (emit '(j _HALT))

  ;; Generate string data section
  (dolist (str-pair (reverse *string-literals*))
    (emit `(label ,(car str-pair)))
    (emit `(lstring ,(cdr str-pair))))

  ;; Generate code for each function/declaration
  (dolist (child (ast-node-children ast))
    (generate-top-level child))

  ;; Generate static local data section
  ;; Format: (label init-value size) stored during parsing
  (when (compiler-state-data *state*)
    (emit-comment "======== static local variables ========")
    ;; Ensure proper alignment for data section
    (emit '(lalign-dword 0))
    (dolist (static-entry (reverse (compiler-state-data *state*)))
      (let ((label (first static-entry))
            (init-value (second static-entry))
            (size (third static-entry)))
        (emit `(label ,label))
        (case size
          (1 (emit `(abyte ,init-value)))
          (2 (emit `(aword ,init-value)))
          (otherwise (emit `(adword ,init-value))))))))

(defun generate-top-level (node)
  "Generate code for a top-level declaration"
  (case (ast-node-type node)
    (function (generate-function node))
    (decl-list (generate-global-decls node))
    (global-var (generate-global-var node))
    (enum-decl nil)  ; Enum declarations are compile-time only, no code needed
    (typedef-decl nil)  ; Typedef declarations are compile-time only, no code needed
    (struct-decl nil)  ; Struct declarations are compile-time only, no code needed
    (type-decl nil)  ; Type declarations are compile-time only, no code needed
    (func-prototype nil)  ; Function prototypes are declarations only, no code needed
    (otherwise
     (compiler-warning "Ignoring top-level ~a" (ast-node-type node)))))

;;; ===========================================================================
;;; Function Generation
;;; ===========================================================================

(defun generate-function (node)
  "Generate code for a function.
   Uses the calling convention from README.md:
   - PUSH-SRP for non-leaf functions
   - PUSH Rn to save R0..Rn (callee-saved registers)
   - POP Rn to restore
   - POP-A; J-A for non-leaf return, A=Rx SRP; J-A for leaf

   With virtual register allocation enabled (*use-virtual-regs* t):
   - Code is generated with virtual registers V0, V1, V2, ...
   - Register allocator maps them to physical registers R0, R1, R2, ...
   - This ensures contiguous allocation from R0 upward, minimizing PUSH/POP overhead"
  (let* ((name (ast-node-value node)))
    ;; Skip dead functions (fully inlined with no remaining calls)
    (when (gethash name (compiler-state-dead-functions *state*))
      (return-from generate-function nil)))

  (let* ((name (ast-node-value node))
         (params (first (ast-node-children node)))
         (body (second (ast-node-children node)))
         (func-data (ast-node-data node))
         (func-label (make-c-label name))
         (*function-end-label* (gen-label (format nil "~a_END" (string-upcase name))))
         (*is-leaf-function* (is-leaf-function body))
         (*frame-size* (or (getf func-data :frame-size) 0))
         (*current-param-count* (or (getf func-data :param-count) 0))
         (*local-reg-count* (or (getf func-data :local-reg-count) 0))
         (*user-labels* (make-hash-table :test 'equal))  ; Reset user labels for each function
         (body-ends-with-return (ends-with-return-p body)))

    ;; Set current function for symbol lookup
    (setf (compiler-state-current-function *state*) name)

    ;; Enter function scope
    (enter-scope)

    (init-registers)

    ;; Pre-compute pushed register bytes for stack param offset calculation
    ;; Only need conservative estimate when there are stack parameters (>4 params)
    ;; For <=4 params, we can use actual register usage after body generation
    (let ((has-stack-params (> *current-param-count* 4)))
      (when has-stack-params
        (let ((estimated-max-reg (if (> *local-reg-count* 0)
                                      (+ 5 *local-reg-count*)  ; R5 + locals
                                      5)))                     ; R5 (all temp regs)
          (setf *pushed-reg-bytes* (* 4 (1+ estimated-max-reg)))))

      ;; Generate body first to determine actual register usage
      ;; Save current code output, generate body, then prepend prologue
      (let ((saved-code (compiler-state-code *state*)))
        (setf (compiler-state-code *state*) nil)

        ;; Register parameters in symbol table for body generation
        (register-parameters params)

        ;; Generate body (with virtual registers if enabled)
        (if body-ends-with-return
            (generate-body-with-final-return body)
            (generate-statement body))

        ;; Epilogue label (target for return statements)
        (emit-label *function-end-label*)

        ;; Get generated body code
        (let* ((body-code-raw (nreverse (compiler-state-code *state*)))
               ;; Run register allocation if using virtual registers
               (body-code nil)
               (max-reg -1)
               (need-regenerate nil))

          (if *use-virtual-regs*
              ;; Virtual register mode: try allocator, fall back if spilling needed
              (handler-case
                  (multiple-value-bind (allocated-code allocated-max-reg spill-count)
                      (allocate-registers body-code-raw)
                    (let ((spill-size (* spill-count 4)))
                      (incf *frame-size* spill-size))
                    (setf body-code allocated-code)
                    ;; When stack params exist, force max-reg >= pre-estimated value
                    ;; so push-r saves enough regs to match the offsets baked into body code
                    (setf max-reg (if has-stack-params
                                      (let ((estimated-max-reg
                                             (if (> *local-reg-count* 0)
                                                 (+ 5 *local-reg-count*)
                                                 5)))
                                        (max allocated-max-reg estimated-max-reg))
                                      allocated-max-reg)))
                (spill-needed ()
                  ;; Fall back to regenerating without virtual registers
                  (setf need-regenerate t)))
              ;; Physical register mode: use old-style tracking
              (let ((actual-max-reg *max-temp-reg-index*)
                    (max-local-reg (if (> *local-reg-count* 0)
                                       (+ 5 *local-reg-count*)
                                       -1)))
                (setf body-code body-code-raw)
                (setf max-reg (if has-stack-params
                                  (max actual-max-reg
                                       (if (> *local-reg-count* 0)
                                           (+ 5 *local-reg-count*)
                                           5))
                                  (max actual-max-reg max-local-reg)))))

          ;; If spilling was needed, regenerate body without virtual registers
          (when need-regenerate
            (setf (compiler-state-code *state*) nil)
            (init-registers)
            (let ((*use-virtual-regs* nil))
              ;; Clear local vreg map and counter
              (setf *local-vreg-map* (make-hash-table :test 'eql))
              (setf *vreg-counter* 0)
              ;; Re-register parameters without virtual registers
              (exit-scope)
              (enter-scope)
              (register-parameters params)
              ;; Regenerate body
              (if body-ends-with-return
                  (generate-body-with-final-return body)
                  (generate-statement body))
              ;; Epilogue label
              (emit-label *function-end-label*)
              ;; Get regenerated code
              (setf body-code (nreverse (compiler-state-code *state*)))
              ;; Calculate max-reg using old-style tracking
              (let ((actual-max-reg *max-temp-reg-index*)
                    (max-local-reg (if (> *local-reg-count* 0)
                                       (+ 5 *local-reg-count*)
                                       -1)))
                (setf max-reg (if has-stack-params
                                  (max actual-max-reg
                                       (if (> *local-reg-count* 0)
                                           (+ 5 *local-reg-count*)
                                           5))
                                  (max actual-max-reg max-local-reg))))))

          ;; Restore original code and emit function
          (setf (compiler-state-code *state*) saved-code)

          ;; Update pushed-reg-bytes for stack param offset calculation
          (setf *pushed-reg-bytes* (if (>= max-reg 0)
                                       (* 4 (1+ max-reg))
                                       0))

          ;; Function header annotation
          (emit-comment (format nil "======== function ~a ========" name))
          (emit-comment (format nil "frame-size: ~a, params: ~a, max-reg: ~a, leaf: ~a, vreg: ~a, spills: ~a"
                                *frame-size* *current-param-count* max-reg
                                (if *is-leaf-function* "yes" "no")
                                (if *use-virtual-regs* "yes" "no")
                                (floor *frame-size* 4)))

          ;; Function label
          (emit `(label ,func-label))

          ;; Prologue with proper PUSH instructions
          (generate-prologue-new max-reg)

          ;; Append body code (now with physical registers)
          (dolist (instr body-code)
            (push instr (compiler-state-code *state*)))

          ;; Epilogue with proper POP instructions
          (generate-epilogue-new max-reg))))

    ;; Exit function scope
    (exit-scope)))

(defun is-leaf-function (body)
  "Check if function body contains no function calls (including implicit __MUL calls)"
  ;; First collect struct array variable names from declarations
  (let ((struct-array-vars (collect-struct-array-vars body)))
    (not (contains-call body struct-array-vars))))

(defun collect-struct-array-vars (node)
  "Collect names of local variables that are arrays of structs needing __MUL"
  (let ((result nil))
    (labels ((collect (n)
               (when (ast-node-p n)
                 (when (eq (ast-node-type n) 'var-decl)
                   (let ((var-type (ast-node-result-type n)))
                     (when (and var-type
                                (eq (type-desc-base var-type) 'struct)
                                (type-desc-array-size var-type))
                       ;; This is a struct array
                       (let* ((tag (type-desc-struct-tag var-type))
                              (struct-def (lookup-struct-def tag)))
                         (when (and struct-def
                                    (not (member (struct-def-size struct-def) '(1 2 4))))
                           (push (ast-node-value n) result))))))
                 (dolist (child (ast-node-children n))
                   (collect child)))))
      (collect node))
    result))

(defun contains-call (node struct-array-vars)
  "Check if AST node or children contain a function call.
   Also detects implicit runtime calls like __MUL/__DIV/__MOD for
   multiply/divide/modulo operations and struct array access."
  (when (and node (ast-node-p node))
    (or (eq (ast-node-type node) 'call)
        ;; Check for binary ops that call runtime functions (*, /, %)
        ;; These generate JSR __MUL/__DIV/__MOD when optimize-size is true
        (and (eq (ast-node-type node) 'binary-op)
             (member (ast-node-value node) '("*" "/" "%") :test #'string=))
        ;; Check for compound assignments that use runtime functions (*=, /=, %=)
        (and (eq (ast-node-type node) 'assign)
             (member (ast-node-value node) '("*=" "/=" "%=") :test #'string=))
        ;; Check for subscript of a known struct array
        (and (eq (ast-node-type node) 'subscript)
             (subscript-uses-struct-array node struct-array-vars))
        ;; Check for member access where base is struct array subscript
        (and (eq (ast-node-type node) 'member)
             (let ((base (first (ast-node-children node))))
               (and base
                    (eq (ast-node-type base) 'subscript)
                    (subscript-uses-struct-array base struct-array-vars))))
        (some (lambda (child) (contains-call child struct-array-vars))
              (ast-node-children node)))))

(defun subscript-uses-struct-array (subscript-node struct-array-vars)
  "Check if a subscript node accesses one of the known struct arrays
   or if the element type has a non-power-of-2 size"
  (let ((array-node (first (ast-node-children subscript-node))))
    (or
     ;; Check local struct array variables
     (and array-node
          (eq (ast-node-type array-node) 'var-ref)
          (member (ast-node-value array-node) struct-array-vars :test #'string=))
     ;; Check type of array element - if struct with non-power-of-2 size, needs __MUL
     (let ((elem-type (get-subscript-element-type subscript-node)))
       (and elem-type
            (or (eq (type-desc-base elem-type) 'struct)
                (eq (type-desc-base elem-type) 'union))
            (let* ((tag (type-desc-struct-tag elem-type))
                   (def (or (lookup-struct-def tag)
                            (lookup-union-def tag))))
              (and def
                   (not (member (struct-def-size def) '(1 2 4))))))))))

(defun ends-with-return-p (node)
  "Check if a statement or block ends with a return statement"
  (when (and node (ast-node-p node))
    (case (ast-node-type node)
      (return t)
      (block
       ;; Block ends with return if its last child is a return
       (let ((children (ast-node-children node)))
         (and children
              (ends-with-return-p (car (last children))))))
      (otherwise nil))))

(defun generate-body-with-final-return (body)
  "Generate body, skipping the jump for the final return statement"
  (if (eq (ast-node-type body) 'block)
      ;; Body is a block - enter scope and generate children
      (progn
        (enter-scope)
        (let ((children (ast-node-children body)))
          (when children
            ;; Generate all statements except the last
            (dolist (stmt (butlast children))
              (generate-statement stmt))
            ;; Generate the last statement with skip flag if it's a return
            (let ((last-stmt (car (last children))))
              (if (eq (ast-node-type last-stmt) 'return)
                  (let ((*skip-final-return-jump* t))
                    (generate-statement last-stmt))
                  (generate-statement last-stmt)))))
        (exit-scope))
      ;; Body is just a single return statement
      (let ((*skip-final-return-jump* t))
        (generate-statement body))))

(defun generate-prologue (params-node)
  "Generate function prologue"
  (let ((param-count (length (ast-node-children params-node))))
    ;; Save SRP if not a leaf function
    (unless *is-leaf-function*
      (emit '(push-srp)))

    ;; For non-leaf functions, we need to save parameters and local registers
    ;; because function calls will overwrite P0-P3 and R0-R9
    (let* ((save-param-space (if *is-leaf-function* 0
                                 (* 4 (min param-count 4))))
           ;; Also need space for local registers in non-leaf functions
           (save-local-reg-space (if *is-leaf-function* 0
                                     (* 4 *local-reg-count*))))
      ;; Allocate stack frame for locals + saved parameters + saved local regs
      (let ((total-frame (+ *frame-size* save-param-space save-local-reg-space)))
        (setf *param-save-offset* *frame-size*)  ; Params start after locals
        (when (> total-frame 0)
          (emit `(A=Rx SP))
          (emit `(Rx= ,(- total-frame) R0))
          (emit `(A+=Rx R0))
          (emit `(Rx=A SP))))

      ;; Save parameters P0-P3 to stack for non-leaf functions
      (unless *is-leaf-function*
        (loop for i from 0 below (min param-count 4)
              for offset = (+ *param-save-offset* (* i 4))
              do (progn
                   (emit `(A=Rx SP))
                   (emit `(M[A+n]=Rx ,offset ,(nth i *param-regs*)))))
        ;; Save local registers (R6-R9) that are used
        (loop for i from 0 below *local-reg-count*
              for offset = (+ *param-save-offset* save-param-space (* i 4))
              do (progn
                   (emit `(A=Rx SP))
                   (emit `(M[A+n]=Rx ,offset ,(get-local-reg i)))))))))

(defun generate-epilogue ()
  "Generate function epilogue"
  ;; Calculate total frame size (locals + saved params + saved local regs for non-leaf)
  (let* ((save-param-space (if *is-leaf-function* 0
                               (* 4 (min *current-param-count* 4))))
         (save-local-reg-space (if *is-leaf-function* 0
                                   (* 4 *local-reg-count*))))
    ;; Restore local registers (R6-R9) before deallocating frame (for non-leaf)
    (unless *is-leaf-function*
      (loop for i from 0 below *local-reg-count*
            for offset = (+ *param-save-offset* save-param-space (* i 4))
            do (let ((temp (alloc-temp-reg)))
                 (emit `(A=Rx SP))
                 (emit `(Rx=M[A+n] ,offset ,temp))
                 (emit `(A=Rx ,temp))
                 (emit `(Rx=A ,(get-local-reg i)))
                 (free-temp-reg temp))))

    (let ((total-frame (+ *frame-size* save-param-space save-local-reg-space)))
      ;; Deallocate stack frame
      (when (> total-frame 0)
        (emit `(A=Rx SP))
        (emit `(Rx= ,total-frame R0))
        (emit `(A+=Rx R0))
        (emit `(Rx=A SP)))))

  ;; Return
  (if *is-leaf-function*
      (progn
        (emit '(A=Rx SRP))
        (emit '(j-a)))
      (progn
        (emit '(pop-a))
        (emit '(j-a)))))

;;; ---------------------------------------------------------------------------
;;; New Calling Convention (uses PUSH/POP for efficiency)
;;; ---------------------------------------------------------------------------

(defun register-parameters (params-node)
  "Register function parameters in the symbol table for code generation.
   For leaf functions, params stay in P0-P3.
   For non-leaf functions, params are saved to stack and accessed from there."
  (loop for param in (ast-node-children params-node)
        for idx from 0
        when (ast-node-value param)
        do (add-symbol (ast-node-value param)
                       (ast-node-result-type param)
                       :parameter
                       idx)))

(defun compute-highest-save-reg (max-temp-idx)
  "Compute the highest register that needs to be saved with PUSH.
   Returns the register symbol (R0-R9) or nil if no saves needed."
  (let ((highest-idx max-temp-idx))
    ;; If local registers (R6-R9) are used, need to save up to the highest one
    (when (> *local-reg-count* 0)
      (setf highest-idx (max highest-idx (+ 5 *local-reg-count*))))
    ;; Return register symbol or nil
    (when (>= highest-idx 0)
      (intern (format nil "R~d" highest-idx) :c-compiler))))

(defun generate-prologue-new (max-temp-idx)
  "Generate function prologue using efficient PUSH instructions.
   Convention:
   - Non-leaf: PUSH-SRP, then PUSH Rn for callee-saved regs
   - Leaf: PUSH Rn for callee-saved regs (if any used)
   Then allocate stack frame for locals + saved params (non-leaf)."
  (let ((save-reg (when (>= max-temp-idx 0)
                    (intern (format nil "R~d" max-temp-idx) :c-compiler)))
        (param-count *current-param-count*))

    ;; Update pushed-reg-bytes to match what we're actually pushing
    (setf *pushed-reg-bytes* (if (>= max-temp-idx 0)
                                  (* 4 (1+ max-temp-idx))
                                  0))

    ;; 1. Save return address for non-leaf functions
    (unless *is-leaf-function*
      (emit '(push-srp)))

    ;; 2. Save callee-saved registers with single PUSH instruction (if any used)
    (when save-reg
      (emit `(push-r ,save-reg)))

    ;; 3. Allocate stack frame for locals + param save area (non-leaf only)
    (let* ((save-param-space (if *is-leaf-function* 0
                                 (* 4 (min param-count 4))))
           (total-frame (+ *frame-size* save-param-space)))
      (setf *param-save-offset* *frame-size*)  ; Params start after locals
      (when (> total-frame 0)
        (emit `(A=Rx SP))
        (emit `(Rx= ,(- total-frame) R0))
        (emit `(A+=Rx R0))
        (emit `(Rx=A SP)))

      ;; 4. Save parameters P0-P3 to stack for non-leaf functions
      ;; (They get clobbered by nested calls)
      (unless *is-leaf-function*
        (loop for i from 0 below (min param-count 4)
              for offset = (+ *param-save-offset* (* i 4))
              do (progn
                   (emit `(A=Rx SP))
                   (emit `(M[A+n]=Rx ,offset ,(nth i *param-regs*)))))))))

(defun generate-epilogue-new (max-temp-idx)
  "Generate function epilogue using efficient POP instructions."
  (let ((save-reg (when (>= max-temp-idx 0)
                    (intern (format nil "R~d" max-temp-idx) :c-compiler)))
        (param-count *current-param-count*))

    ;; 1. Deallocate stack frame (locals + saved params for non-leaf)
    (let* ((save-param-space (if *is-leaf-function* 0
                                 (* 4 (min param-count 4))))
           (total-frame (+ *frame-size* save-param-space)))
      (when (> total-frame 0)
        (emit `(A=Rx SP))
        (emit `(Rx= ,total-frame R0))
        (emit `(A+=Rx R0))
        (emit `(Rx=A SP))))

    ;; 2. Restore callee-saved registers with single POP instruction (if any saved)
    (when save-reg
      (emit `(pop-r ,save-reg)))

    ;; 3. Return
    (if *is-leaf-function*
        (progn
          (emit '(A=Rx SRP))
          (emit '(j-a)))
        (progn
          (emit '(pop-a))
          (emit '(j-a))))))

;;; ===========================================================================
;;; Statement Generation
;;; ===========================================================================

(defun get-source-line (node)
  "Get the source line number from an AST node"
  (when (and node (ast-node-source-loc node))
    (car (ast-node-source-loc node))))

(defun emit-source-annotation (node &optional description)
  "Emit a source annotation comment for an AST node"
  (let ((line-num (get-source-line node)))
    (when line-num
      (emit-source-line line-num))
    (when description
      (emit-comment description))))

(defun generate-statement (node)
  "Generate code for a statement"
  (when node
    ;; Emit source annotation for non-block statements
    (unless (eq (ast-node-type node) 'block)
      (emit-source-annotation node))
    (case (ast-node-type node)
      (block (generate-block node))
      (if (generate-if node))
      (while (generate-while node))
      (for (generate-for node))
      (do-while (generate-do-while node))
      (return (generate-return node))
      (break (generate-break node))
      (continue (generate-continue node))
      (expr-stmt (generate-expr-stmt node))
      (decl-list (generate-local-decls node))
      (empty nil)
      ;; Inline-related nodes
      (inline-return-jump (generate-inline-return-jump node))
      (inline-return-label (generate-inline-return-label node))
      ;; Goto and labels
      (goto (generate-goto node))
      (labeled-stmt (generate-labeled-stmt node))
      (otherwise
       (compiler-warning "Unknown statement type: ~a" (ast-node-type node))))))

(defun generate-block (node)
  "Generate code for a block of statements"
  (enter-scope)
  (dolist (stmt (ast-node-children node))
    (generate-statement stmt))
  (exit-scope))

(defun generate-if (node)
  "Generate code for an if statement"
  (let ((condition (first (ast-node-children node)))
        (then-branch (second (ast-node-children node)))
        (else-branch (third (ast-node-children node)))
        (else-label (gen-label "ELSE"))
        (end-label (gen-label "ENDIF")))

    ;; Generate condition
    (generate-expression condition)

    ;; Test condition in A
    (emit-test-zero)
    (if else-branch
        (emit `(jz ,else-label))
        (emit `(jz ,end-label)))

    ;; Then branch
    (generate-statement then-branch)

    (when else-branch
      (emit `(j ,end-label))
      (emit-label else-label)
      (generate-statement else-branch))

    (emit-label end-label)))

(defun generate-while (node)
  "Generate code for a while statement"
  (let ((condition (first (ast-node-children node)))
        (body (second (ast-node-children node)))
        (loop-label (gen-label "WHILE"))
        (end-label (gen-label "ENDWHILE"))
        (*break-label* (gen-label "ENDWHILE"))
        (*continue-label* (gen-label "WHILE")))

    ;; Use the actual labels for break/continue
    (setf *break-label* end-label)
    (setf *continue-label* loop-label)

    (emit-label loop-label)

    ;; Condition
    (generate-expression condition)
    (emit-test-zero)
    (emit `(jz ,end-label))

    ;; Body
    (generate-statement body)
    (emit `(j ,loop-label))

    (emit-label end-label)))

(defun generate-for (node)
  "Generate code for a for statement"
  ;; Enter scope to match the scope entered during parsing (for init declarations)
  (enter-scope)
  (let ((init (first (ast-node-children node)))
        (condition (second (ast-node-children node)))
        (update (third (ast-node-children node)))
        (body (fourth (ast-node-children node)))
        (loop-label (gen-label "FOR"))
        (continue-label (gen-label "FORCONT"))
        (end-label (gen-label "ENDFOR"))
        (*break-label* nil)
        (*continue-label* nil))

    (setf *break-label* end-label)
    (setf *continue-label* continue-label)

    ;; Init
    (when init
      (if (eq (ast-node-type init) 'decl-list)
          (generate-local-decls init)
          (generate-expression init)))

    (emit-label loop-label)

    ;; Condition
    (when condition
      (generate-expression condition)
      (emit-test-zero)
      (emit `(jz ,end-label)))

    ;; Body
    (generate-statement body)

    ;; Continue target (before update)
    (emit-label continue-label)

    ;; Update
    (when update
      (generate-expression update))

    (emit `(j ,loop-label))
    (emit-label end-label))
  (exit-scope))

(defun generate-do-while (node)
  "Generate code for a do-while statement"
  (let ((body (first (ast-node-children node)))
        (condition (second (ast-node-children node)))
        (loop-label (gen-label "DOWHILE"))
        (end-label (gen-label "ENDDOWHILE"))
        (*break-label* nil)
        (*continue-label* nil))

    (setf *break-label* end-label)
    (setf *continue-label* loop-label)

    (emit-label loop-label)

    ;; Body
    (generate-statement body)

    ;; Condition
    (generate-expression condition)
    (emit-test-zero)
    (emit `(jnz ,loop-label))

    (emit-label end-label)))

(defun generate-return (node)
  "Generate code for a return statement"
  (when (ast-node-children node)
    ;; Generate return expression
    (generate-expression (first (ast-node-children node)))
    ;; Move result to P0 (R10)
    (emit '(Rx=A P0)))

  ;; Jump to function epilogue (skip if this is the final return before func_end)
  (unless *skip-final-return-jump*
    (emit `(j ,*function-end-label*))))

(defun generate-break (node)
  "Generate code for a break statement"
  (declare (ignore node))
  (if *break-label*
      (emit `(j ,*break-label*))
      (compiler-error "break statement outside loop")))

(defun generate-continue (node)
  "Generate code for a continue statement"
  (declare (ignore node))
  (if *continue-label*
      (emit `(j ,*continue-label*))
      (compiler-error "continue statement outside loop")))

(defun get-or-create-user-label (name)
  "Get the assembly label for a user-defined label, creating it if needed"
  (or (gethash name *user-labels*)
      (let ((asm-label (gen-label (format nil "USR_~a" (string-upcase name)))))
        (setf (gethash name *user-labels*) asm-label)
        asm-label)))

(defun generate-goto (node)
  "Generate code for a goto statement"
  (let* ((label-name (ast-node-value node))
         (asm-label (get-or-create-user-label label-name)))
    (emit `(j ,asm-label))))

(defun generate-labeled-stmt (node)
  "Generate code for a labeled statement"
  (let* ((label-name (ast-node-value node))
         (asm-label (get-or-create-user-label label-name))
         (stmt (first (ast-node-children node))))
    ;; Emit the label
    (emit-label asm-label)
    ;; Generate the associated statement
    (when stmt
      (generate-statement stmt))))

(defun generate-expr-stmt (node)
  "Generate code for an expression statement"
  (generate-expression (first (ast-node-children node))))

;;; ===========================================================================
;;; Initializer List Code Generation
;;; ===========================================================================

(defun get-array-element-type (array-type)
  "Get element type of array. For multi-dim, preserves inner dimensions."
  (when (type-desc-array-size array-type)
    (make-type-desc :base (type-desc-base array-type)
                    :pointer-level (type-desc-pointer-level array-type)
                    :array-size (get-remaining-dimensions
                                  (type-desc-array-size array-type))
                    :size (type-desc-size array-type)
                    :unsigned-p (type-desc-unsigned-p array-type)
                    :struct-tag (type-desc-struct-tag array-type)
                    :struct-scope (type-desc-struct-scope array-type))))

(defun generate-local-init-list (init-list base-offset target-type)
  "Generate stores for each element of an init-list at computed offsets.
   base-offset is the stack offset of the array/struct base.
   target-type is the type of the variable being initialized."
  (cond
    ;; Array initialization
    ((type-desc-array-size target-type)
     (let* ((element-type (get-array-element-type target-type))
            (element-size (type-size element-type))
            (array-size (type-desc-array-size target-type))
            (init-elements (ast-node-children init-list))
            (num-inits (length init-elements))
            (offset 0))
       ;; Initialize elements from init-list
       (dolist (elem init-elements)
         (cond
           ;; Nested init-list (for arrays of structs or 2D arrays)
           ((and (ast-node-p elem)
                 (eq (ast-node-type elem) 'init-list))
            (generate-local-init-list elem (+ base-offset offset) element-type))
           ;; Regular expression
           (t
            (generate-expression elem)
            ;; Mask value to appropriate size
            (when (< element-size 4)
              (emit-mask-to-size element-type))
            ;; Store to stack at base-offset + offset
            (let ((actual-offset (+ base-offset offset *frame-size*))
                  (value-reg (alloc-temp-reg)))
              (emit `(Rx=A ,value-reg))
              (emit `(A=Rx SP))
              (emit-store-sized-offset element-type actual-offset value-reg)
              (free-temp-reg value-reg))))
         (incf offset element-size))
       ;; Zero-fill remaining elements if fewer initializers than array size
       ;; For multi-dim arrays, use outer dimension for this comparison
       (let ((outer-dim (get-outer-dimension array-size)))
         (when (< num-inits outer-dim)
           (let ((zero-reg (alloc-temp-reg)))
             (emit `(Rx= 0 ,zero-reg))
             (loop for i from num-inits below outer-dim
                   for elem-offset = (+ base-offset (* i element-size) *frame-size*)
                   do (emit `(A=Rx SP))
                      (emit-store-sized-offset element-type elem-offset zero-reg))
             (free-temp-reg zero-reg))))))

    ;; Struct initialization
    ((type-desc-struct-tag target-type)
     (let* ((struct-def (lookup-struct-def-at-scope
                          (type-desc-struct-tag target-type)
                          (or (type-desc-struct-scope target-type)
                              (compiler-state-scope-level *state*))))
            (members (when struct-def (struct-def-members struct-def)))
            (init-elements (ast-node-children init-list))
            ;; Check if we have designated initializers
            (has-designated (some (lambda (e)
                                    (and (ast-node-p e)
                                         (eq (ast-node-type e) 'designated-init)))
                                  init-elements)))
       (when struct-def
         (if has-designated
             ;; Handle designated initializers
             (let ((init-map (make-hash-table :test #'equal)))
               ;; Populate the map from designated initializers
               (dolist (elem init-elements)
                 (when (and (ast-node-p elem)
                            (eq (ast-node-type elem) 'designated-init))
                   (let ((field-name (ast-node-value elem))
                         (init-expr (first (ast-node-children elem))))
                     (setf (gethash field-name init-map) init-expr))))
               ;; Initialize each member
               (dolist (member members)
                 (let* ((member-name (struct-member-name member))
                        (member-type (struct-member-type member))
                        (member-offset (struct-member-offset member))
                        (member-size (type-size member-type))
                        (elem (gethash member-name init-map)))
                   (when elem
                     (cond
                       ;; Nested init-list
                       ((and (ast-node-p elem)
                             (eq (ast-node-type elem) 'init-list))
                        (generate-local-init-list elem
                                                  (+ base-offset member-offset)
                                                  member-type))
                       ;; Regular expression
                       (t
                        (generate-expression elem)
                        (when (< member-size 4)
                          (emit-mask-to-size member-type))
                        (let ((value-reg (alloc-temp-reg))
                              (addr-temp (alloc-temp-reg))
                              (total-offset (+ base-offset *frame-size* member-offset)))
                          (emit `(Rx=A ,value-reg))
                          (emit `(A=Rx SP))
                          (emit `(Rx= ,total-offset ,addr-temp))
                          (emit `(A+=Rx ,addr-temp))
                          (emit-store-sized member-type value-reg)
                          (free-temp-reg addr-temp)
                          (free-temp-reg value-reg))))))))
             ;; Non-designated: initialize members in order
             (loop for elem in init-elements
                   for member in members
                   do (let* ((member-type (struct-member-type member))
                             (member-offset (struct-member-offset member))
                             (member-size (type-size member-type)))
                        (cond
                          ;; Nested init-list for nested struct/array
                          ((and (ast-node-p elem)
                                (eq (ast-node-type elem) 'init-list))
                           (generate-local-init-list elem
                                                     (+ base-offset member-offset)
                                                     member-type))
                          ;; Regular expression
                          (t
                           (generate-expression elem)
                           (when (< member-size 4)
                             (emit-mask-to-size member-type))
                           ;; Store using computed address
                           (let ((value-reg (alloc-temp-reg))
                                 (addr-temp (alloc-temp-reg))
                                 (total-offset (+ base-offset *frame-size* member-offset)))
                             (emit `(Rx=A ,value-reg))
                             (emit `(A=Rx SP))
                             (emit `(Rx= ,total-offset ,addr-temp))
                             (emit `(A+=Rx ,addr-temp))
                             (emit-store-sized member-type value-reg)
                             (free-temp-reg addr-temp)
                             (free-temp-reg value-reg))))))))))

    ;; Fallback: shouldn't happen
    (t (compiler-warning "Unknown type for init-list initialization"))))

(defun generate-string-to-char-array (string-node base-offset array-size)
  "Generate stores to initialize a char array from a string literal.
   Copies each character and the null terminator."
  (let* ((str (ast-node-value string-node))
         (str-len (length str))
         (char-type (make-char-type))
         (zero-reg (alloc-temp-reg))
         (value-reg (alloc-temp-reg)))
    (emit `(Rx= 0 ,zero-reg))
    ;; Store each character
    (loop for i from 0 below str-len
          for char-code = (char-code (char str i))
          for elem-offset = (+ base-offset i *frame-size*)
          do (emit `(Rx= ,char-code ,value-reg))
             (emit `(A=Rx SP))
             (emit-store-sized-offset char-type elem-offset value-reg))
    ;; Store null terminator
    (let ((term-offset (+ base-offset str-len *frame-size*)))
      (emit `(A=Rx SP))
      (emit-store-sized-offset char-type term-offset zero-reg))
    ;; Zero-fill rest if array is larger
    (when (and array-size (> array-size (1+ str-len)))
      (loop for i from (1+ str-len) below array-size
            for elem-offset = (+ base-offset i *frame-size*)
            do (emit `(A=Rx SP))
               (emit-store-sized-offset char-type elem-offset zero-reg)))
    (free-temp-reg zero-reg)
    (free-temp-reg value-reg)))

(defun generate-local-decls (node)
  "Generate code for local variable declarations"
  (dolist (decl (ast-node-children node))
    (when (eq (ast-node-type decl) 'var-decl)
      (let ((name (ast-node-value decl))
            (init (first (ast-node-children decl)))
            (var-type (ast-node-result-type decl)))
        (when init
          (let ((sym (lookup-symbol name)))
            (when sym
              (cond
                ;; Init-list for array or struct
                ((and (ast-node-p init)
                      (eq (ast-node-type init) 'init-list)
                      (eq (sym-entry-storage sym) :local))
                 (generate-local-init-list init (sym-entry-offset sym) var-type))

                ;; String literal initializing char array
                ((and (ast-node-p init)
                      (eq (ast-node-type init) 'string-literal)
                      (type-desc-array-size var-type)
                      (eq (type-desc-base var-type) 'char)
                      (eq (sym-entry-storage sym) :local))
                 (generate-string-to-char-array init
                                                 (sym-entry-offset sym)
                                                 (type-desc-array-size var-type)))

                ;; Regular expression initializer
                (t
                 (generate-expression init)
                 ;; Mask value to appropriate size before storing
                 (when (and var-type (< (type-size var-type) 4))
                   (emit-mask-to-size var-type))
                 (case (sym-entry-storage sym)
                   (:register
                    ;; Store directly to local register (R6-R9)
                    (emit `(Rx=A ,(get-local-reg (sym-entry-offset sym)))))
                   (:local
                    ;; Store to stack with appropriate size
                    (generate-store-local (sym-entry-offset sym) var-type))))))))))))

;;; ===========================================================================
;;; Expression Generation
;;; ===========================================================================

(defun generate-expression (node)
  "Generate code for an expression, result goes in A"
  (when node
    (case (ast-node-type node)
      (literal (generate-literal node))
      (string-literal (generate-string-literal node))
      (var-ref (generate-var-ref node))
      (binary-op (generate-binary-op node))
      (unary-op (generate-unary-op node))
      (assign (generate-assignment node))
      (call (generate-call node))
      (subscript (generate-subscript node))
      (ternary (generate-ternary node))
      (cast (generate-cast node))
      (sizeof (generate-sizeof node))
      (post-op (generate-post-op node))
      (inline-expr (generate-inline-expr node))
      (member (generate-member node))
      (otherwise
       (compiler-warning "Unknown expression type: ~a" (ast-node-type node))))))

(defun generate-literal (node)
  "Generate code for a numeric literal"
  (let ((value (ast-node-value node)))
    ;; Load immediate into A
    ;; For small values, use A= instruction
    ;; For larger values, use Rx= then A=Rx with an allocated temp
    (if (and (>= value -8) (<= value 7))
        (emit `(A= ,value))
        (let ((temp (alloc-temp-reg)))
          (emit `(Rx= ,value ,temp))
          (emit `(A=Rx ,temp))
          (free-temp-reg temp)))))

(defun generate-string-literal (node)
  "Generate code for a string literal (load address)"
  (let* ((str (ast-node-value node))
         (label (car (find str *string-literals* :key #'cdr :test #'string=)))
         (temp (alloc-temp-reg)))
    (emit `(Rx= ,label ,temp))
    (emit `(A=Rx ,temp))
    (free-temp-reg temp)))

(defun generate-var-ref (node)
  "Generate code for a variable reference"
  (let* ((name (ast-node-value node))
         (sym (lookup-symbol name)))
    (unless sym
      (compiler-error "Undefined variable: ~a" name))

    (case (sym-entry-storage sym)
      (:parameter
       ;; Parameters 0-3 are in P0-P3 (leaf) or saved on stack (non-leaf)
       (let ((idx (sym-entry-offset sym)))
         (cond
           ;; Leaf function: read directly from register
           ((and *is-leaf-function* (< idx 4))
            (emit `(A=Rx ,(nth idx *param-regs*))))
           ;; Non-leaf function with register param: read from saved stack location
           ((< idx 4)
            (let ((offset (+ *param-save-offset* (* idx 4)))
                  (temp (alloc-temp-reg)))
              (emit `(A=Rx SP))
              (emit `(Rx=M[A+n] ,offset ,temp))
              (emit `(A=Rx ,temp))
              (free-temp-reg temp)))
           ;; Stack parameter (5th param and beyond)
           (t
            (let* ((save-param-space (if *is-leaf-function* 0
                                         (* 4 (min *current-param-count* 4))))
                   (temp (alloc-temp-reg)))
              ;; Stack layout from callee perspective:
              ;; [stack params]  <- pushed by caller before JSR
              ;; [saved SRP]     <- pushed by push-srp (non-leaf only)
              ;; [saved regs]    <- pushed by push-r (size = *pushed-reg-bytes*)
              ;; [locals+saved P0-P3] <- allocated by SP adjustment
              ;; SP ->
              (let ((offset (+ *frame-size* save-param-space
                               *pushed-reg-bytes*
                               (if *is-leaf-function* 0 4)  ; saved SRP
                               (* (- idx 4) 4))))
                (emit `(A=Rx SP))
                (emit `(Rx=M[A+n] ,offset ,temp))
                (emit `(A=Rx ,temp))
                (free-temp-reg temp)))))))

      (:register
       ;; Register-allocated local variable - read directly from R6-R9
       ;; For signed sub-word types, sign extend after reading
       (let ((reg-idx (sym-entry-offset sym))
             (var-type (sym-entry-type sym)))
         (emit `(A=Rx ,(get-local-reg reg-idx)))
         ;; Sign extend for signed sub-word types
         (when (and var-type (< (type-size var-type) 4))
           (emit-promote-to-int var-type))))

      (:local
       ;; For arrays, return the address (array-to-pointer decay)
       ;; For scalars, load the value from stack with appropriate size
       (let ((var-type (sym-entry-type sym))
             (offset (+ (sym-entry-offset sym) *frame-size*)))
         (if (and var-type (type-desc-array-size var-type))
             ;; Array: return address (SP + offset)
             (let ((temp (alloc-temp-reg)))
               (emit `(A=Rx SP))
               (emit `(Rx= ,offset ,temp))
               (emit `(A+=Rx ,temp))
               (free-temp-reg temp))
             ;; Scalar: load value from stack with sized load
             (let ((temp (alloc-temp-reg)))
               (emit `(A=Rx SP))
               (emit-load-sized-offset var-type offset temp)
               (emit `(A=Rx ,temp))
               (free-temp-reg temp)
               ;; Apply sign extension for signed sub-word types
               (emit-promote-to-int var-type)))))

      (:global
       ;; For arrays, return the address (array-to-pointer decay)
       ;; For scalars, load the value from global address with sized load
       (let ((label (make-c-label name))
             (var-type (sym-entry-type sym))
             (temp (alloc-temp-reg)))
         (emit `(Rx= ,label ,temp))
         (emit `(A=Rx ,temp))
         (if (and var-type (type-desc-array-size var-type))
             ;; Array: address is already in A
             (free-temp-reg temp)
             ;; Scalar: load value
             (progn
               (emit-load-sized var-type temp)
               (emit `(A=Rx ,temp))
               (free-temp-reg temp)
               ;; Apply sign extension for signed sub-word types
               (when var-type
                 (emit-promote-to-int var-type))))))

      (:enum-constant
       ;; Enum constants are compile-time values stored in the offset field
       (let ((value (sym-entry-offset sym)))
         (if (and (>= value -8) (<= value 7))
             (emit `(A= ,value))
             (let ((temp (alloc-temp-reg)))
               (emit `(Rx= ,value ,temp))
               (emit `(A=Rx ,temp))
               (free-temp-reg temp)))))

      (:function
       ;; Function reference - load function address (for function pointers)
       (let ((label (sym-entry-offset sym))  ; label is stored in offset
             (temp (alloc-temp-reg)))
         (emit `(Rx= ,label ,temp))
         (emit `(A=Rx ,temp))
         (free-temp-reg temp)))

      (:static-local
       ;; Static local - load from global label (stored in offset field)
       (let ((label (sym-entry-offset sym))
             (var-type (sym-entry-type sym))
             (temp (alloc-temp-reg)))
         (emit `(Rx= ,label ,temp))
         (emit `(A=Rx ,temp))
         (emit-load-sized var-type temp)
         (emit `(A=Rx ,temp))
         (free-temp-reg temp)
         ;; Apply sign extension for signed sub-word types
         (when var-type
           (emit-promote-to-int var-type)))))))

(defun generate-store-local (offset &optional var-type)
  "Generate code to store A to a local variable at offset with optional sized store"
  (let ((actual-offset (+ offset *frame-size*))
        (value-reg (alloc-temp-reg)))
    (emit `(Rx=A ,value-reg))           ; save value
    (emit `(A=Rx SP))                   ; get SP
    (if (and var-type (< (type-size var-type) 4))
        (emit-store-sized-offset var-type actual-offset value-reg)
        (emit `(M[A+n]=Rx ,actual-offset ,value-reg))) ; store
    (free-temp-reg value-reg)))

(defun generate-load-local (offset &optional var-type)
  "Generate code to load a local variable at offset into A"
  (declare (ignore var-type))  ; For now, always do full word load
  (let ((actual-offset (+ offset *frame-size*))
        (temp (alloc-temp-reg)))
    (emit `(A=Rx SP))
    (emit `(Rx=M[A+n] ,actual-offset ,temp))
    (emit `(A=Rx ,temp))
    (free-temp-reg temp)))

(defun generate-binary-op (node)
  "Generate code for a binary operation"
  (let ((op (ast-node-value node))
        (left (first (ast-node-children node)))
        (right (second (ast-node-children node))))

    ;; Handle short-circuit operators specially
    (cond
      ((string= op "&&") (generate-logical-and left right))
      ((string= op "||") (generate-logical-or left right))
      (t (generate-arithmetic-op op left right)))))

(defun get-expression-type (node)
  "Get the type of an expression (for pointer arithmetic)"
  (when (and node (ast-node-p node))
    (case (ast-node-type node)
      (var-ref
       (let ((sym (lookup-symbol (ast-node-value node))))
         (when sym (sym-entry-type sym))))
      (literal nil)  ; integer literal, no pointer type
      (subscript
       ;; Array subscript - return element type
       (get-subscript-element-type node))
      (member
       (let* ((member-name (ast-node-value node))
              (base-expr (first (ast-node-children node)))
              (access-type (ast-node-data node))
              (base-type (get-expression-type base-expr))
              (struct-type (if (eq access-type :pointer)
                               (get-dereferenced-type base-type) base-type))
              (member (lookup-struct-member struct-type member-name)))
         (when member (struct-member-type member))))
      (call
       ;; Function call - get return type from callee's type
       (let* ((callee (first (ast-node-children node))))
         ;; Check if callee is a direct function reference (var-ref to :function symbol)
         (if (and (eq (ast-node-type callee) 'var-ref)
                  (let ((sym (lookup-symbol (ast-node-value callee))))
                    (and sym (eq (sym-entry-storage sym) :function))))
             ;; Direct function call - return the stored return type
             (let ((sym (lookup-symbol (ast-node-value callee))))
               (sym-entry-type sym))
             ;; Indirect call through function pointer - get callee type and extract return-type
             (let ((callee-type (get-expression-type callee)))
               (when callee-type
                 (if (type-desc-return-type callee-type)
                     (type-desc-return-type callee-type)
                     ;; Fallback for non-function-pointer types
                     callee-type))))))
      (otherwise (ast-node-result-type node)))))

(defun pointer-element-size (type)
  "Get the element size for pointer arithmetic (returns nil for non-pointers)"
  (when (and type
             (type-desc-p type)
             (> (type-desc-pointer-level type) 0))
    ;; Use the size slot for element size, default to 4 for int pointers
    (or (type-desc-size type) 4)))

(defun power-of-2-p (n)
  "Check if N is a positive power of 2"
  (and (integerp n) (> n 0) (zerop (logand n (1- n)))))

(defun log2-int (n)
  "Compute floor(log2(n)) for positive integer n"
  (1- (integer-length n)))

(defun emit-multiply-by-constant (size)
  "Multiply A by a constant SIZE. Uses shifts for powers of 2, else general multiply.
   Returns t if simple (shifts only), nil if needed general multiply."
  (cond
    ((= size 1) t)  ; no-op
    ((power-of-2-p size)
     ;; Use shifts: emit log2(size) left shifts
     (dotimes (i (log2-int size))
       (emit '(A=A<<1)))
     t)
    (t
     ;; General case: use software multiply
     (let ((size-reg (alloc-temp-reg)))
       (emit `(Rx= ,size ,size-reg))
       (generate-multiply size-reg)
       (free-temp-reg size-reg))
     nil)))

(defun emit-divide-by-constant (size)
  "Divide A by a constant SIZE. Uses shifts for powers of 2, else general divide."
  (cond
    ((= size 1) nil)  ; no-op
    ((power-of-2-p size)
     ;; Use shifts: emit log2(size) right shifts
     (dotimes (i (log2-int size))
       (emit '(A=A>>1))))
    (t
     ;; General case: use software divide
     (let ((size-reg (alloc-temp-reg)))
       (emit `(Rx= ,size ,size-reg))
       (generate-divide size-reg nil)
       (free-temp-reg size-reg)))))

(defun emit-scale-for-pointer (elem-size)
  "Scale value in A by element size for pointer arithmetic"
  (emit-multiply-by-constant elem-size))

(defun emit-unscale-for-pointer (elem-size)
  "Unscale value in A by element size for pointer subtraction result"
  (emit-divide-by-constant elem-size))

(defun constant-multiply-is-simple-p (size)
  "Check if multiplying by SIZE can be done with just shifts (no function call)"
  (power-of-2-p size))

(defun expression-uses-mul-p (node)
  "Check if expression contains struct array access that will call __MUL"
  (when (ast-node-p node)
    (or (and (eq (ast-node-type node) 'subscript)
             (let ((elem-type (get-subscript-element-type node)))
               (and elem-type
                    (not (constant-multiply-is-simple-p (type-size elem-type))))))
        (and (eq (ast-node-type node) 'member)
             (let ((base (first (ast-node-children node))))
               (and base (expression-uses-mul-p base))))
        (some #'expression-uses-mul-p (ast-node-children node)))))

(defun get-var-local-register (node)
  "If node is a var-ref in a local register (R6-R9), return that register.
   Local registers are safe to use directly since they won't be clobbered
   during expression evaluation. Returns nil otherwise."
  (when (and (ast-node-p node)
             (eq (ast-node-type node) 'var-ref))
    (let* ((name (ast-node-value node))
           (sym (lookup-symbol name)))
      (when (and sym (eq (sym-entry-storage sym) :register))
        ;; offset is the register index (0-3), get-local-reg converts to R6-R9
        (get-local-reg (sym-entry-offset sym))))))

(defun generate-arithmetic-op (op left right)
  "Generate code for arithmetic/bitwise operation"
  ;; Handle shifts specially - they can optimize for constant counts
  (when (or (string= op "<<") (string= op ">>"))
    (generate-expression left)
    (let ((left-reg (alloc-temp-reg)))
      (emit `(Rx=A ,left-reg))
      (if (string= op "<<")
          (generate-shift-left left-reg right)
          (generate-shift-right left-reg right))
      (free-temp-reg left-reg)
      (return-from generate-arithmetic-op)))

  ;; Check for pointer arithmetic
  (let* ((left-type (get-expression-type left))
         (right-type (get-expression-type right))
         (left-elem-size (pointer-element-size left-type))
         (right-elem-size (pointer-element-size right-type))
         ;; If right operand uses __MUL, use a safe register for left
         (right-uses-mul (expression-uses-mul-p right))
         ;; Check if left is already in a local register (R6-R9) that we can reuse
         ;; BUT: operators > and <= write to left-reg, so can't use local reg directly
         (writes-to-left-reg (or (string= op ">") (string= op "<=")))
         (existing-left-reg (and (not writes-to-left-reg)
                                  (get-var-local-register left)))
         ;; Flag whether we need to free the left-reg later
         (need-free-left nil))

    ;; Generate left operand (skip if already in local register)
    (let ((left-reg
           (cond
             ;; Left is already in a local register - use it directly
             (existing-left-reg
              existing-left-reg)
             ;; Need to evaluate and save to register
             (t
              (generate-expression left)
              ;; In virtual register mode, just allocate a vreg - the calling
              ;; convention ensures registers are preserved across calls.
              ;; In physical mode, use a local reg for values live across calls.
              (let ((reg (if (and (not *use-virtual-regs*) right-uses-mul)
                             (get-local-reg (1+ *local-reg-count*))
                             (alloc-temp-reg))))
                (emit `(Rx=A ,reg))
                (setf need-free-left (or *use-virtual-regs* (not right-uses-mul)))
                reg)))))

      ;; Generate right operand
      (generate-expression right)

      ;; For pointer + int or pointer - int, scale the integer
      (when (and (or (string= op "+") (string= op "-"))
                 left-elem-size
                 (not right-elem-size))
        ;; Left is pointer, right is integer - scale right by element size
        (emit-scale-for-pointer left-elem-size))

      ;; For int + pointer (less common), scale the integer
      (when (and (string= op "+")
                 right-elem-size
                 (not left-elem-size))
        ;; Right is pointer, left is integer - need to swap and scale
        ;; left-reg has the integer, A has the pointer
        ;; Save pointer, scale integer, add
        (let ((ptr-reg (alloc-temp-reg)))
          (emit `(Rx=A ,ptr-reg))      ; save pointer
          (emit `(A=Rx ,left-reg))     ; A = integer
          (emit-scale-for-pointer right-elem-size)
          (emit `(A+=Rx ,ptr-reg))     ; add pointer
          (emit `(Rx=A ,left-reg))     ; put result back in convention
          (emit `(A=Rx ,left-reg))
          (free-temp-reg ptr-reg)
          (free-temp-reg left-reg)
          (return-from generate-arithmetic-op)))

      ;; Right is now in A, left is in left-reg
      ;; Perform operation
      (cond
        ((string= op "+")
         (emit `(A+=Rx ,left-reg)))

        ((string= op "-")
         ;; A = left - right: save right, load left, subtract right
         (let ((right-reg (alloc-temp-reg)))
           (emit `(Rx=A ,right-reg))    ; save right
           (emit `(A=Rx ,left-reg))     ; A = left
           (emit `(A-=Rx ,right-reg))   ; A = left - right
           (free-temp-reg right-reg)
           ;; For pointer - pointer, divide result by element size
           (when (and left-elem-size right-elem-size)
             (emit-unscale-for-pointer left-elem-size))))

      ((string= op "*")
       ;; Software multiply - result in A
       (generate-multiply left-reg))

      ((string= op "/")
       ;; Software divide
       (generate-divide left-reg nil))

      ((string= op "%")
       ;; Software modulo
       (generate-divide left-reg t))

      ((string= op "&")
       (emit `(A&=Rx ,left-reg)))

      ((string= op "|")
       (emit `(A\|=Rx ,left-reg)))

      ((string= op "^")
       (emit `(A^=Rx ,left-reg)))

      ((string= op "<<")
       (generate-shift-left left-reg right))

      ((string= op ">>")
       (generate-shift-right left-reg right))

      ;; Comparison operators
      ((string= op "==")
       (generate-comparison left-reg 'jz))

      ((string= op "!=")
       (generate-comparison left-reg 'jnz))

      ((string= op "<")
       (generate-comparison left-reg 'jlt))

      ((string= op ">")
       ;; A > B is B < A, swap operands
       ;; Currently: left-reg = A, accumulator = B
       ;; We need: left-reg = B, accumulator = A
       (let ((temp-reg (alloc-temp-reg))
             (temp-reg2 (alloc-temp-reg)))
         (emit `(Rx=A ,temp-reg))       ; temp = B (from accumulator)
         (emit `(A=Rx ,left-reg))       ; A = A (old left)
         (emit `(Rx=A ,temp-reg2))      ; temp2 = A (save it)
         (emit `(A=Rx ,temp-reg))       ; A = B
         (emit `(Rx=A ,left-reg))       ; left-reg = B (new left)
         (emit `(A=Rx ,temp-reg2))      ; A = A (new right)
         (free-temp-reg temp-reg2)
         (free-temp-reg temp-reg))
       (generate-comparison left-reg 'jlt))

      ((string= op "<=")
       ;; A <= B is !(A > B) is !(B < A), swap and use jge
       ;; Need same swap as > but use jge (jump if >=)
       (let ((temp-reg (alloc-temp-reg))
             (temp-reg2 (alloc-temp-reg)))
         (emit `(Rx=A ,temp-reg))       ; temp = B
         (emit `(A=Rx ,left-reg))       ; A = A (old left)
         (emit `(Rx=A ,temp-reg2))      ; temp2 = A
         (emit `(A=Rx ,temp-reg))       ; A = B
         (emit `(Rx=A ,left-reg))       ; left-reg = B (new left)
         (emit `(A=Rx ,temp-reg2))      ; A = A (new right)
         (free-temp-reg temp-reg2)
         (free-temp-reg temp-reg))
       (generate-comparison left-reg 'jge))

      ((string= op ">=")
       (generate-comparison left-reg 'jge))

      (t (compiler-warning "Unknown binary operator: ~a" op)))

      ;; Only free if it was an allocated temp reg
      (when need-free-left
        (free-temp-reg left-reg)))))

(defun generate-comparison (left-reg jump-type)
  "Generate code for comparison, result 0 or 1 in A"
  (let ((true-label (gen-label "CMPTRUE"))
        (end-label (gen-label "CMPEND"))
        (right-reg (alloc-temp-reg)))
    ;; Compute left - right (sets flags)
    ;; A has right, left-reg has left
    (emit `(Rx=A ,right-reg))   ; save right
    (emit `(A=Rx ,left-reg))    ; A = left
    (emit `(A-=Rx ,right-reg))  ; A = left - right, sets flags
    (free-temp-reg right-reg)

    ;; Jump based on condition
    (emit `(,jump-type ,true-label))

    ;; False path
    (emit '(A= 0))
    (emit `(j ,end-label))

    ;; True path
    (emit-label true-label)
    (emit '(A= 1))

    (emit-label end-label)))

(defun generate-logical-and (left right)
  "Generate short-circuit && operator"
  (let ((false-label (gen-label "ANDFALSE"))
        (end-label (gen-label "ANDEND")))

    ;; Evaluate left
    (generate-expression left)
    (emit-test-zero)
    (emit `(jz ,false-label))

    ;; Left was true, evaluate right
    (generate-expression right)
    (emit-test-zero)
    (emit `(jz ,false-label))

    ;; Both true
    (emit '(A= 1))
    (emit `(j ,end-label))

    ;; False
    (emit-label false-label)
    (emit '(A= 0))

    (emit-label end-label)))

(defun generate-logical-or (left right)
  "Generate short-circuit || operator"
  (let ((true-label (gen-label "ORTRUE"))
        (end-label (gen-label "OREND")))

    ;; Evaluate left
    (generate-expression left)
    (emit-test-zero)
    (emit `(jnz ,true-label))

    ;; Left was false, evaluate right
    (generate-expression right)
    (emit-test-zero)
    (emit `(jnz ,true-label))

    ;; Both false
    (emit '(A= 0))
    (emit `(j ,end-label))

    ;; True
    (emit-label true-label)
    (emit '(A= 1))

    (emit-label end-label)))

(defun generate-unary-op (node)
  "Generate code for a unary operation"
  (let ((op (ast-node-value node))
        (operand (first (ast-node-children node))))

    (cond
      ((string= op "-")
       ;; Negate: 0 - x
       (generate-expression operand)
       (let ((temp (alloc-temp-reg)))
         (emit `(Rx=A ,temp))
         (emit '(A= 0))
         (emit `(A-=Rx ,temp))
         (free-temp-reg temp)))

      ((string= op "!")
       ;; Logical not: x == 0 ? 1 : 0
       (generate-expression operand)
       (let ((zero-label (gen-label "NOTZERO"))
             (end-label (gen-label "NOTEND"))
             (temp (alloc-temp-reg)))
         (emit `(Rx= 0 ,temp))
         (emit `(A-=Rx ,temp))
         (free-temp-reg temp)
         (emit `(jz ,zero-label))
         (emit '(A= 0))
         (emit `(j ,end-label))
         (emit-label zero-label)
         (emit '(A= 1))
         (emit-label end-label)))

      ((string= op "~")
       ;; Bitwise not
       (generate-expression operand)
       (emit '(not-a)))

      ((string= op "*")
       ;; Dereference - use sized load based on pointer element type
       (generate-expression operand)
       (let ((temp (alloc-temp-reg))
             (ptr-type (get-expression-type operand)))
         ;; Determine element type from pointer
         (if (and ptr-type (> (type-desc-pointer-level ptr-type) 0))
             (let ((elem-type (make-type-desc
                               :base (type-desc-base ptr-type)
                               :pointer-level (1- (type-desc-pointer-level ptr-type))
                               :size (type-desc-size ptr-type)
                               :unsigned-p (type-desc-unsigned-p ptr-type))))
               (emit-load-sized elem-type temp)
               (emit `(A=Rx ,temp))
               (free-temp-reg temp)
               ;; Sign extend if needed
               (emit-promote-to-int elem-type))
             ;; Fallback to word load
             (progn
               (emit `(Rx=M[A] ,temp))
               (emit `(A=Rx ,temp))
               (free-temp-reg temp)))))

      ((string= op "&")
       ;; Address-of
       (generate-address operand))

      ((string= op "++")
       ;; Pre-increment
       (generate-pre-inc-dec operand 1))

      ((string= op "--")
       ;; Pre-decrement
       (generate-pre-inc-dec operand -1))

      (t (compiler-warning "Unknown unary operator: ~a" op)))))

(defun generate-pre-inc-dec (operand delta)
  "Generate code for pre-increment/decrement"
  ;; Check if operand is a pointer (for scaled arithmetic)
  (let* ((operand-type (get-expression-type operand))
         (elem-size (pointer-element-size operand-type))
         (actual-delta (if elem-size (* delta elem-size) delta)))
    ;; Load current value
    (generate-expression operand)
    ;; Add scaled delta using allocated temp register
    (let ((delta-reg (alloc-temp-reg)))
      (emit `(Rx= ,actual-delta ,delta-reg))
      (emit `(A+=Rx ,delta-reg))
      (free-temp-reg delta-reg))
    ;; Store back and keep result
    (generate-store-lvalue operand)))

(defun generate-post-op (node)
  "Generate code for post-increment/decrement"
  (let* ((op (ast-node-value node))
         (operand (first (ast-node-children node)))
         ;; Check if operand is a pointer (for scaled arithmetic)
         (operand-type (get-expression-type operand))
         (elem-size (pointer-element-size operand-type))
         (delta (if (string= op "++") 1 -1))
         (actual-delta (if elem-size (* delta elem-size) delta)))
    ;; Load current value
    (generate-expression operand)
    ;; Save original value
    (let ((save-reg (alloc-temp-reg))
          (inc-reg (alloc-temp-reg)))
      (emit `(Rx=A ,save-reg))
      ;; Compute new value using allocated temp register with scaled delta
      (emit `(Rx= ,actual-delta ,inc-reg))
      (emit `(A+=Rx ,inc-reg))
      (free-temp-reg inc-reg)
      ;; Store new value
      (generate-store-lvalue operand)
      ;; Return original value
      (emit `(A=Rx ,save-reg))
      (free-temp-reg save-reg))))

(defun generate-address (node)
  "Generate code for address-of operator"
  (case (ast-node-type node)
    (var-ref
     (let* ((name (ast-node-value node))
            (sym (lookup-symbol name)))
       (unless sym
         (compiler-error "Undefined variable: ~a" name))
       (case (sym-entry-storage sym)
         (:register
          (compiler-error "Cannot take address of register variable ~a" name))
         (:local
          (let ((offset (+ (sym-entry-offset sym) *frame-size*))
                (temp (alloc-temp-reg)))
            (emit `(A=Rx SP))
            (emit `(Rx= ,offset ,temp))
            (emit `(A+=Rx ,temp))
            (free-temp-reg temp)))
         (:global
          (let ((label (make-c-label name))
                (temp (alloc-temp-reg)))
            (emit `(Rx= ,label ,temp))
            (emit `(A=Rx ,temp))
            (free-temp-reg temp)))
         (:function
          ;; Address of function - load the function label
          (let ((label (sym-entry-offset sym))  ; offset holds the label
                (temp (alloc-temp-reg)))
            (emit `(Rx= ,label ,temp))
            (emit `(A=Rx ,temp))
            (free-temp-reg temp)))
         (:parameter
          (compiler-error "Cannot take address of register parameter")))))
    (subscript
     ;; &arr[i] = arr + i * element_size
     (generate-subscript-address node))
    (otherwise
     (compiler-error "Cannot take address of ~a" (ast-node-type node)))))

(defun generate-store-lvalue (node)
  "Generate code to store A to an lvalue with appropriate sized store"
  (case (ast-node-type node)
    (var-ref
     (let* ((name (ast-node-value node))
            (sym (lookup-symbol name)))
       (unless sym
         (compiler-error "Undefined variable: ~a" name))
       (let ((var-type (sym-entry-type sym)))
         (case (sym-entry-storage sym)
           (:register
            ;; Register-allocated local - store directly to R6-R9
            ;; For sub-word types, mask the value first
            (let ((reg-idx (sym-entry-offset sym)))
              (when (and var-type (< (type-size var-type) 4))
                (case (type-size var-type)
                  (1 (emit '(mask-a-b)))
                  (2 (emit '(mask-a-w)))))
              (emit `(Rx=A ,(get-local-reg reg-idx)))))
           (:local
            (let ((offset (+ (sym-entry-offset sym) *frame-size*))
                  (value-reg (alloc-temp-reg)))
              (emit `(Rx=A ,value-reg))
              (emit `(A=Rx SP))
              (if (and var-type (< (type-size var-type) 4))
                  (emit-store-sized-offset var-type offset value-reg)
                  (emit `(M[A+n]=Rx ,offset ,value-reg)))
              (free-temp-reg value-reg)))
           (:global
            (let ((label (make-c-label name))
                  (value-reg (alloc-temp-reg))
                  (addr-reg (alloc-temp-reg)))
              (emit `(Rx=A ,value-reg))
              (emit `(Rx= ,label ,addr-reg))
              (emit `(A=Rx ,addr-reg))
              (if (and var-type (< (type-size var-type) 4))
                  (emit-store-sized var-type value-reg)
                  (emit `(M[A]=Rx ,value-reg)))
              (free-temp-reg addr-reg)
              (free-temp-reg value-reg)))
           (:static-local
            ;; Static local - store to global label (stored in offset field)
            (let ((label (sym-entry-offset sym))
                  (value-reg (alloc-temp-reg))
                  (addr-reg (alloc-temp-reg)))
              (emit `(Rx=A ,value-reg))
              (emit `(Rx= ,label ,addr-reg))
              (emit `(A=Rx ,addr-reg))
              (if (and var-type (< (type-size var-type) 4))
                  (emit-store-sized var-type value-reg)
                  (emit `(M[A]=Rx ,value-reg)))
              (free-temp-reg addr-reg)
              (free-temp-reg value-reg)))
           (:parameter
            (let ((idx (sym-entry-offset sym)))
              (if (< idx 4)
                  (emit `(Rx=A ,(nth idx *param-regs*)))
                  (compiler-error "Cannot store to stack parameter"))))))))
    (unary-op
     ;; *ptr = value - need to determine element type from pointer
     (when (string= (ast-node-value node) "*")
       (let ((value-reg (alloc-temp-reg))
             (ptr-type (get-lvalue-type (first (ast-node-children node)))))
         (emit `(Rx=A ,value-reg))  ; save value
         (generate-expression (first (ast-node-children node)))  ; get pointer
         (if (and ptr-type (< (type-size ptr-type) 4))
             (emit-store-sized ptr-type value-reg)
             (emit `(M[A]=Rx ,value-reg)))  ; store value at pointer
         (free-temp-reg value-reg))))
    (subscript
     ;; arr[i] = value - get element type
     (let* ((elem-type (get-subscript-element-type node))
            (elem-size (if elem-type (type-size elem-type) 4))
            (needs-multiply (not (member elem-size '(1 2 4))))
            ;; In virtual mode, just use alloc-temp-reg - calling convention preserves regs.
            ;; In physical mode, use local reg for values live across __MUL calls.
            (value-reg (if (and (not *use-virtual-regs*) needs-multiply)
                           (get-local-reg (1+ *local-reg-count*))
                           (alloc-temp-reg))))
       (emit `(Rx=A ,value-reg))  ; save value
       (generate-subscript-address node)  ; get address (may call __MUL)
       (if (and elem-type (< (type-size elem-type) 4))
           (emit-store-sized elem-type value-reg)
           (emit `(M[A]=Rx ,value-reg)))  ; store
       (when (or *use-virtual-regs* (not needs-multiply))
         (free-temp-reg value-reg))))
    (member
     ;; struct.member = value or ptr->member = value
     ;; Check if base expression is a struct array subscript that needs __MUL
     (let* ((base-expr (first (ast-node-children node)))
            (needs-safe-reg (and (not *use-virtual-regs*)
                                 (eq (ast-node-type base-expr) 'subscript)
                                 (let ((elem-type (get-subscript-element-type base-expr)))
                                   (and elem-type
                                        (not (member (type-size elem-type) '(1 2 4)))))))
            ;; In virtual mode, just use alloc-temp-reg.
            ;; In physical mode, use local reg for values live across __MUL calls.
            (value-reg (if needs-safe-reg
                           (get-local-reg (1+ *local-reg-count*))
                           (alloc-temp-reg))))
       (emit `(Rx=A ,value-reg))
       (generate-member-address node)
       (let* ((member-name (ast-node-value node))
              (access-type (ast-node-data node))
              (base-type (get-expression-type base-expr))
              (struct-type (if (eq access-type :pointer)
                               (get-dereferenced-type base-type) base-type))
              (member (lookup-struct-member struct-type member-name)))
         (emit-store-sized (struct-member-type member) value-reg))
       (when (or *use-virtual-regs* (not needs-safe-reg))
         (free-temp-reg value-reg))))
    (otherwise
     (compiler-error "Cannot assign to ~a" (ast-node-type node)))))

(defun get-lvalue-type (node)
  "Get the type of an lvalue expression (for determining store size)"
  (case (ast-node-type node)
    (var-ref
     (let ((sym (lookup-symbol (ast-node-value node))))
       (when sym
         (let ((var-type (sym-entry-type sym)))
           (when (and var-type (> (type-desc-pointer-level var-type) 0))
             ;; Dereferencing a pointer - return element type
             (make-type-desc :base (type-desc-base var-type)
                             :pointer-level (1- (type-desc-pointer-level var-type))
                             :size (type-desc-size var-type)
                             :unsigned-p (type-desc-unsigned-p var-type)))))))
    (otherwise nil)))

(defun get-subscript-element-type (node)
  "Get the element type for an array subscript expression"
  (let ((array-node (first (ast-node-children node))))
    (case (ast-node-type array-node)
      (var-ref
       (let ((sym (lookup-symbol (ast-node-value array-node))))
         (when sym
           (let ((arr-type (sym-entry-type sym)))
             (when arr-type
               ;; For arrays, use get-array-element-type to preserve inner dimensions
               (if (type-desc-array-size arr-type)
                   (get-array-element-type arr-type)
                   ;; For pointers, decrement pointer level
                   (make-type-desc :base (type-desc-base arr-type)
                                   :pointer-level (max 0 (1- (type-desc-pointer-level arr-type)))
                                   :size (type-desc-size arr-type)
                                   :unsigned-p (type-desc-unsigned-p arr-type)
                                   :struct-tag (type-desc-struct-tag arr-type)
                                   :struct-scope (type-desc-struct-scope arr-type))))))))
      ;; For nested subscripts, get element type from inner subscript's result
      (subscript
       (let ((inner-elem-type (get-subscript-element-type array-node)))
         (when (and inner-elem-type (type-desc-array-size inner-elem-type))
           (get-array-element-type inner-elem-type))))
      (otherwise nil))))

(defun generate-assignment (node)
  "Generate code for assignment"
  (let ((op (ast-node-value node))
        (left (first (ast-node-children node)))
        (right (second (ast-node-children node))))

    (cond
      ((string= op "=")
       ;; Simple assignment
       (generate-expression right)
       (generate-store-lvalue left))

      (t
       ;; Compound assignment: +=, -=, etc.
       (let ((base-op (subseq op 0 (1- (length op)))))
         ;; Compute left op right
         (generate-binary-op
          (make-node 'binary-op
                     :value base-op
                     :children (list left right)))
         ;; Store result
         (generate-store-lvalue left))))))

(defun generate-call (node)
  "Generate code for a function call"
  (let* ((func-expr (first (ast-node-children node)))
         (args (rest (ast-node-children node)))
         ;; Only use direct call if callee is a var-ref to a :function symbol
         (func-label (when (eq (ast-node-type func-expr) 'var-ref)
                       (let* ((name (ast-node-value func-expr))
                              (sym (lookup-symbol name)))
                         (when (and sym (eq (sym-entry-storage sym) :function))
                           (make-c-label name)))))
         (arg-count (length args))
         (indirect-temp nil))  ; Temp register for indirect call target

    ;; For indirect calls (func-label is nil), evaluate func expression first
    ;; and save to a temp register
    (unless func-label
      (setf indirect-temp (alloc-temp-reg))
      (generate-expression func-expr)
      (emit `(Rx=A ,indirect-temp)))

    ;; Handle stack arguments FIRST (args 5+) before allocating temps for args 0-3
    ;; This maximizes available temp regs during stack arg evaluation
    ;; Note: Can't use push-r because it's a multi-register push (R0..Rn)
    (when (> arg-count 4)
      (let ((const-temp (alloc-temp-reg)))
        (emit `(Rx= -4 ,const-temp))
        (loop for i from (1- arg-count) downto 4
              for arg = (nth i args)
              do (let ((value-temp (alloc-temp-reg)))
                   (generate-expression arg)
                   (emit `(Rx=A ,value-temp))
                   ;; Decrement SP and store value
                   (emit '(A=Rx SP))
                   (emit `(A+=Rx ,const-temp))
                   (emit '(Rx=A SP))
                   (emit `(M[A]=Rx ,value-temp))
                   (free-temp-reg value-temp)))
        (free-temp-reg const-temp)))

    ;; Now evaluate args 0-3 and save to temp registers
    ;; This ensures stack-relative parameter reads work correctly
    (let ((arg-temps nil))
      (loop for i from 0 below (min arg-count 4)
            for arg = (nth i args)
            do (let ((temp (alloc-temp-reg)))
                 (generate-expression arg)
                 (emit `(Rx=A ,temp))
                 (push temp arg-temps)))
      (setf arg-temps (nreverse arg-temps))

      ;; Move evaluated args from temp regs to P0-P3
      ;; Note: R0-R9 are callee-saved, so no need to save caller's temp regs
      (loop for i from 0 below (min arg-count 4)
            for temp in arg-temps
            do (progn
                 (emit `(A=Rx ,temp))
                 (emit `(Rx=A ,(nth i *param-regs*)))))

      ;; Call function
      (if func-label
          ;; Direct call
          (emit `(jsr ,func-label))
          ;; Indirect call: move func addr to R0 and call helper
          (progn
            (setf (compiler-state-need-indirect-call *state*) t)
            (emit `(A=Rx ,indirect-temp))
            (emit '(Rx=A R0))
            (emit '(jsr |__indirect_call|))))

      ;; Clean up stack arguments
      (when (> arg-count 4)
        (let ((stack-args (* 4 (- arg-count 4))))
          (emit `(A=Rx SP))
          (emit `(Rx= ,stack-args R0))
          (emit '(A+=Rx R0))
          (emit '(Rx=A SP))))

      ;; Free the arg temps
      (dolist (temp arg-temps)
        (free-temp-reg temp))
      (when indirect-temp
        (free-temp-reg indirect-temp))

      ;; Result is in P0, move to A
      (emit '(A=Rx P0)))))

(defun generate-subscript (node)
  "Generate code for array subscript with sized load"
  (let ((elem-type (get-subscript-element-type node)))
    (generate-subscript-address node)
    ;; For multi-dimensional arrays: if element is itself an array,
    ;; don't load - just keep the computed address in A
    (unless (and elem-type (type-desc-array-size elem-type))
      ;; Load value at computed address with appropriate size
      (let ((temp (alloc-temp-reg)))
        (if (and elem-type (< (type-size elem-type) 4))
            (emit-load-sized elem-type temp)
            (emit `(Rx=M[A] ,temp)))
        (emit `(A=Rx ,temp))
        (free-temp-reg temp)
        ;; Apply sign extension for signed sub-word types
        (when elem-type
          (emit-promote-to-int elem-type))))))

(defun generate-subscript-address (node)
  "Generate code to compute address of array subscript"
  (let* ((array (first (ast-node-children node)))
         (index (second (ast-node-children node)))
         (elem-type (get-subscript-element-type node))
         (elem-size (if elem-type (type-size elem-type) 4))
         (is-simple (constant-multiply-is-simple-p elem-size)))
    ;; For non-power-of-2 sizes (e.g., structs), we need to call __MUL
    ;; or inline multiply. Both can clobber R0-R5 (inline uses temp regs).
    ;; In physical mode, use local register (R6+) to preserve base address.
    ;; In virtual mode, just use alloc-temp-reg - calling convention preserves regs.
    (if is-simple
        ;; Simple case: power-of-2 element size, use shifts
        (let ((base-reg (alloc-temp-reg)))
          ;; Get array base address
          (generate-expression array)
          (emit `(Rx=A ,base-reg))  ; save base
          ;; Get index
          (generate-expression index)
          ;; Multiply by element size using shifts
          (emit-multiply-by-constant elem-size)
          ;; Add to base
          (emit `(A+=Rx ,base-reg))
          (free-temp-reg base-reg))
        ;; Complex case: non-power-of-2, need general multiply
        (let ((base-save-reg (if *use-virtual-regs*
                                 (alloc-temp-reg)
                                 (get-local-reg *local-reg-count*))))
          ;; Step 1: Get array base address and save to safe register
          (generate-expression array)
          (emit `(Rx=A ,base-save-reg))  ; save base (safe from multiply)
          ;; Step 2: Compute index * elem_size (may call __MUL or inline, clobbers R0-R5)
          (generate-expression index)
          (emit-multiply-by-constant elem-size)
          ;; Step 3: Add base (in safe reg) to offset (in A)
          (emit `(A+=Rx ,base-save-reg))
          (when *use-virtual-regs*
            (free-temp-reg base-save-reg))))))

;;; ===========================================================================
;;; Struct Member Access
;;; ===========================================================================

(defun get-dereferenced-type (type)
  "Get the type that a pointer points to"
  (when (and type (> (type-desc-pointer-level type) 0))
    (make-type-desc :base (type-desc-base type)
                    :pointer-level (1- (type-desc-pointer-level type))
                    :size (type-desc-size type)
                    :unsigned-p (type-desc-unsigned-p type)
                    :struct-tag (type-desc-struct-tag type)
                    :struct-scope (type-desc-struct-scope type))))

(defun generate-struct-address (node)
  "Generate code to get address of a struct variable"
  (case (ast-node-type node)
    (var-ref
     (let* ((name (ast-node-value node))
            (sym (lookup-symbol name)))
       (unless sym (compiler-error "Undefined variable: ~a" name))
       (case (sym-entry-storage sym)
         (:local
          (let ((offset (+ (sym-entry-offset sym) *frame-size*))
                (temp (alloc-temp-reg)))
            (emit `(A=Rx SP))
            (emit `(Rx= ,offset ,temp))
            (emit `(A+=Rx ,temp))
            (free-temp-reg temp)))
         (:global
          (let ((label (make-c-label name)))
            (emit `(Rx= ,label R0))
            (emit `(A=Rx R0))))
         (otherwise
          (compiler-error "Cannot get address of struct in ~a storage"
                          (sym-entry-storage sym))))))
    (subscript (generate-subscript-address node))
    (member (generate-member-address node))
    (otherwise (compiler-error "Cannot get struct address of ~a" (ast-node-type node)))))

(defun generate-member-address (node)
  "Generate code to compute address of struct member"
  (let* ((member-name (ast-node-value node))
         (base-expr (first (ast-node-children node)))
         (access-type (ast-node-data node))
         (base-type (get-expression-type base-expr))
         (struct-type (if (eq access-type :pointer)
                          (get-dereferenced-type base-type) base-type))
         (member (lookup-struct-member struct-type member-name)))
    (unless member (compiler-error "Unknown struct member: ~a" member-name))
    (if (eq access-type :pointer)
        (generate-expression base-expr)
        (generate-struct-address base-expr))
    (let ((offset (struct-member-offset member)))
      (when (> offset 0)
        (let ((temp (alloc-temp-reg)))
          (emit `(Rx= ,offset ,temp))
          (emit `(A+=Rx ,temp))
          (free-temp-reg temp))))))

(defun generate-member (node)
  "Generate code for struct member access"
  (let* ((member-name (ast-node-value node))
         (base-expr (first (ast-node-children node)))
         (access-type (ast-node-data node))
         (base-type (get-expression-type base-expr))
         (struct-type (if (eq access-type :pointer)
                          (get-dereferenced-type base-type) base-type))
         (member (lookup-struct-member struct-type member-name)))
    (unless member (compiler-error "Unknown struct member: ~a" member-name))
    (generate-member-address node)
    (let ((member-type (struct-member-type member)))
      ;; If member is an array, just return its address (array decays to pointer)
      (if (type-desc-array-size member-type)
          ;; Address is already in A, nothing more to do
          nil
          ;; Non-array: load the value
          (let ((temp (alloc-temp-reg)))
            (emit-load-sized member-type temp)
            (emit `(A=Rx ,temp))
            (free-temp-reg temp)
            (emit-promote-to-int member-type))))))

(defun generate-ternary (node)
  "Generate code for ternary conditional"
  (let ((condition (first (ast-node-children node)))
        (then-expr (second (ast-node-children node)))
        (else-expr (third (ast-node-children node)))
        (else-label (gen-label "TERNELSE"))
        (end-label (gen-label "TERNEND")))

    ;; Evaluate condition
    (generate-expression condition)
    ;; Test if condition is zero (use allocated temp to avoid clobbering caller's regs)
    (let ((zero-reg (alloc-temp-reg)))
      (emit `(Rx= 0 ,zero-reg))
      (emit `(A-=Rx ,zero-reg))
      (free-temp-reg zero-reg))
    (emit `(jz ,else-label))

    ;; Then expression
    (generate-expression then-expr)
    (emit `(j ,end-label))

    ;; Else expression
    (emit-label else-label)
    (generate-expression else-expr)

    (emit-label end-label)))

(defun generate-cast (node)
  "Generate code for type cast"
  (let ((target-type (ast-node-value node))
        (expr (first (ast-node-children node))))
    ;; Generate the expression
    (generate-expression expr)
    ;; Apply any necessary conversions
    (when (and target-type (eq (type-desc-base target-type) 'char))
      (emit '(mask-a-b)))))

(defun generate-sizeof (node)
  "Generate code for sizeof"
  (let* ((operand (first (ast-node-children node)))
         (size (cond
                 ;; sizeof(type)
                 ((type-desc-p operand)
                  (type-size operand))
                 ;; sizeof(variable)
                 ((and (ast-node-p operand)
                       (eq (ast-node-type operand) 'var-ref))
                  (let ((sym (lookup-symbol (ast-node-value operand))))
                    (if (and sym (sym-entry-type sym))
                        (type-size (sym-entry-type sym))
                        4)))
                 ;; sizeof(subscript) - get element type including inner dimensions
                 ((and (ast-node-p operand)
                       (eq (ast-node-type operand) 'subscript))
                  (let ((elem-type (get-subscript-element-type operand)))
                    (if elem-type
                        (type-size elem-type)
                        4)))
                 ;; sizeof(expression) - default to 4 bytes
                 (t 4))))
    (emit `(Rx= ,size R0))
    (emit '(A=Rx R0))))

;;; ===========================================================================
;;; Inlined Function Generation
;;; ===========================================================================

(defun collect-var-decls (node)
  "Collect all var-decl nodes from an AST (for registering inline locals)"
  (when (and node (ast-node-p node))
    (case (ast-node-type node)
      (var-decl (list node))
      (otherwise
       (mapcan #'collect-var-decls (ast-node-children node))))))

(defun generate-inline-expr (node)
  "Generate code for an inlined function expression.
   The inline-expr node contains:
   - children[0]: decl-list with result var and parameter temp vars
   - children[1]: the transformed function body
   - children[2]: inline-return-label marking the exit point
   - value: the result variable name
   Result ends up in A after evaluation."
  (let* ((result-var (ast-node-value node))
         (init-decls (first (ast-node-children node)))
         (body (second (ast-node-children node)))
         (exit-label-node (third (ast-node-children node)))
         (inline-vars nil))  ; track variables we add to symbol table

    (emit-comment (format nil "-- inline begin: result in ~a --" result-var))

    ;; First, register all inline variables in the symbol table
    ;; They need to be allocated storage before we can generate code
    (dolist (decl (ast-node-children init-decls))
      (when (eq (ast-node-type decl) 'var-decl)
        (let* ((name (ast-node-value decl))
               (var-type (or (ast-node-result-type decl) (make-int-type)))
               ;; Allocate stack space for the variable
               (current-offset (compiler-state-local-offset *state*))
               (new-offset (- current-offset (type-size var-type))))
          (setf (compiler-state-local-offset *state*) new-offset)
          (add-symbol name var-type :local new-offset)
          (push name inline-vars))))

    ;; Also register any local variables declared in the body
    ;; (these were renamed during the inline transformation)
    (dolist (decl (collect-var-decls body))
      (let* ((name (ast-node-value decl))
             (var-type (or (ast-node-result-type decl) (make-int-type)))
             (current-offset (compiler-state-local-offset *state*))
             (new-offset (- current-offset (type-size var-type))))
        (unless (lookup-symbol name)  ; don't re-register if already done
          (setf (compiler-state-local-offset *state*) new-offset)
          (add-symbol name var-type :local new-offset)
          (push name inline-vars))))

    ;; Now generate initializations for the inline variables
    (dolist (decl (ast-node-children init-decls))
      (when (eq (ast-node-type decl) 'var-decl)
        (let ((name (ast-node-value decl))
              (init (first (ast-node-children decl)))
              (var-type (or (ast-node-result-type decl) (make-int-type))))
          (when init
            ;; Generate initializer
            (generate-expression init)
            ;; Mask value to appropriate size before storing
            (when (and var-type (< (type-size var-type) 4))
              (emit-mask-to-size var-type))
            ;; Store to local variable
            (let ((sym (lookup-symbol name)))
              (when sym
                (generate-store-local (sym-entry-offset sym) var-type)))))))

    ;; Generate the transformed body
    (generate-statement body)

    ;; Generate the exit label
    (generate-statement exit-label-node)

    ;; Load result into A
    (let ((sym (lookup-symbol result-var)))
      (if sym
          (let ((offset (sym-entry-offset sym))
                (var-type (sym-entry-type sym)))
            (generate-load-local offset var-type))
          (compiler-warning "Inline result var ~a not found" result-var)))

    ;; Clean up: remove inline variables from symbol table
    (dolist (name inline-vars)
      (remove-symbol name))

    (emit-comment "-- inline end --")))

(defun generate-inline-return-jump (node)
  "Generate a jump to the inline exit label (used in place of return)"
  (let ((label (ast-node-value node)))
    (emit `(j ,label))))

(defun generate-inline-return-label (node)
  "Generate the exit label for an inlined function"
  (let ((label (ast-node-value node)))
    (emit-label label)))

;;; ===========================================================================
;;; Software Multiply/Divide
;;; ===========================================================================

(defun generate-multiply (left-reg)
  "Generate software multiply: A = left-reg * A"
  (if (compiler-state-optimize-size *state*)
      ;; Call runtime library function for code size optimization
      (generate-multiply-call left-reg)
      ;; Inline the multiply loop for performance
      (generate-multiply-inline left-reg)))

(defun generate-multiply-call (left-reg)
  "Generate a call to __mul runtime function: A = left-reg * A"
  ;; Mark that we need the multiply runtime
  (setf (compiler-state-need-mul-runtime *state*) t)
  ;; Set up arguments: P0 = left operand, P1 = right operand (in A)
  (emit '(Rx=A P1))              ; P1 = multiplier (from A)
  (emit `(A=Rx ,left-reg))       ; A = multiplicand
  (emit '(Rx=A P0))              ; P0 = multiplicand
  (emit '(jsr __MUL))            ; call runtime
  (emit '(A=Rx P0)))             ; result in P0, move to A

(defun generate-multiply-inline (left-reg)
  "Generate inline software multiply: A = left-reg * A"
  ;; Simple shift-and-add multiply
  ;; Result in A, multiplicand in left-reg, multiplier in A
  ;; Allocate temp registers to avoid conflicts
  (let ((loop-label (gen-label "MULLOOP"))
        (skip-label (gen-label "MULSKIP"))
        (end-label (gen-label "MULEND"))
        (multiplier-reg (alloc-temp-reg))
        (multiplicand-reg (alloc-temp-reg))
        (result-reg (alloc-temp-reg))
        (temp-reg (alloc-temp-reg)))

    (emit `(Rx=A ,multiplier-reg))     ; multiplier-reg = multiplier (from A)
    (emit `(A=Rx ,left-reg))           ; A = multiplicand
    (emit `(Rx=A ,multiplicand-reg))   ; multiplicand-reg = multiplicand
    (emit '(A= 0))
    (emit `(Rx=A ,result-reg))         ; result-reg = 0

    (emit-label loop-label)
    ;; Check if multiplier is zero
    (emit `(A=Rx ,multiplier-reg))
    (emit `(Rx= 0 ,temp-reg))
    (emit `(A-=Rx ,temp-reg))
    (emit `(jz ,end-label))

    ;; Check LSB of multiplier
    (emit `(A=Rx ,multiplier-reg))
    (emit `(Rx= 1 ,temp-reg))
    (emit `(A&=Rx ,temp-reg))
    (emit `(Rx= 0 ,temp-reg))
    (emit `(A-=Rx ,temp-reg))
    (emit `(jz ,skip-label))

    ;; Add multiplicand to result
    (emit `(A=Rx ,result-reg))
    (emit `(A+=Rx ,multiplicand-reg))
    (emit `(Rx=A ,result-reg))

    (emit-label skip-label)
    ;; Shift multiplicand left
    (emit `(A=Rx ,multiplicand-reg))
    (emit '(A=A<<1))
    (emit `(Rx=A ,multiplicand-reg))

    ;; Shift multiplier right
    (emit `(A=Rx ,multiplier-reg))
    (emit '(A=A>>1))
    (emit `(Rx=A ,multiplier-reg))

    (emit `(j ,loop-label))

    (emit-label end-label)
    (emit `(A=Rx ,result-reg))

    ;; Free allocated registers
    (free-temp-reg temp-reg)
    (free-temp-reg result-reg)
    (free-temp-reg multiplicand-reg)
    (free-temp-reg multiplier-reg)))

(defun generate-divide (left-reg get-remainder)
  "Generate software divide: A = left-reg / A or left-reg % A"
  (if (compiler-state-optimize-size *state*)
      ;; Call runtime library function for code size optimization
      (generate-divide-call left-reg get-remainder)
      ;; Inline the divide loop for performance
      (generate-divide-inline left-reg get-remainder)))

(defun generate-divide-call (left-reg get-remainder)
  "Generate a call to __div or __mod runtime function"
  ;; Mark that we need the appropriate runtime
  (if get-remainder
      (setf (compiler-state-need-mod-runtime *state*) t)
      (setf (compiler-state-need-div-runtime *state*) t))
  ;; Set up arguments: P0 = dividend (left), P1 = divisor (in A)
  (emit '(Rx=A P1))              ; P1 = divisor (from A)
  (emit `(A=Rx ,left-reg))       ; A = dividend
  (emit '(Rx=A P0))              ; P0 = dividend
  (if get-remainder
      (emit '(jsr __MOD))        ; call modulo runtime
      (emit '(jsr __DIV)))       ; call divide runtime
  (emit '(A=Rx P0)))             ; result in P0, move to A

(defun generate-divide-inline (left-reg get-remainder)
  "Generate inline software divide: A = left-reg / A or left-reg % A"
  ;; Simple repeated subtraction (works correctly, can be optimized later)
  ;; Allocate temp registers to avoid conflicts
  (let ((loop-label (gen-label "DIVLOOP"))
        (end-label (gen-label "DIVEND"))
        (dividend-reg (alloc-temp-reg))
        (divisor-reg (alloc-temp-reg))
        (quotient-reg (alloc-temp-reg))
        (temp-reg (alloc-temp-reg)))

    ;; dividend-reg = dividend, divisor-reg = divisor, quotient-reg = quotient
    (emit `(Rx=A ,divisor-reg))        ; divisor-reg = divisor (from A)
    (emit `(A=Rx ,left-reg))           ; A = dividend
    (emit `(Rx=A ,dividend-reg))       ; dividend-reg = dividend (will become remainder)
    (emit '(A= 0))
    (emit `(Rx=A ,quotient-reg))       ; quotient-reg = 0

    (emit-label loop-label)
    ;; Check if dividend >= divisor
    (emit `(A=Rx ,dividend-reg))       ; A = current dividend
    (emit `(A-=Rx ,divisor-reg))       ; A = dividend - divisor (sets flags)
    (emit `(jlt ,end-label))           ; if dividend < divisor, done

    ;; dividend >= divisor: update dividend and increment quotient
    (emit `(Rx=A ,dividend-reg))       ; dividend-reg = dividend - divisor
    (emit `(A=Rx ,quotient-reg))
    (emit `(Rx= 1 ,temp-reg))
    (emit `(A+=Rx ,temp-reg))
    (emit `(Rx=A ,quotient-reg))       ; quotient-reg = quotient + 1

    (emit `(j ,loop-label))

    (emit-label end-label)
    (if get-remainder
        (emit `(A=Rx ,dividend-reg))   ; return remainder
        (emit `(A=Rx ,quotient-reg)))  ; return quotient

    ;; Free allocated registers
    (free-temp-reg temp-reg)
    (free-temp-reg quotient-reg)
    (free-temp-reg divisor-reg)
    (free-temp-reg dividend-reg)))

;;; Threshold for inline vs loop shift (in terms of instructions)
;;; Below this, use inline shifts. At or above, use loop (if optimize-size) or inline (if not)
(defparameter *shift-inline-threshold* 4)

(defun generate-shift-left (left-reg right-node)
  "Generate left shift: result = left-reg << right-node.
   Optimizes constant shift counts: small counts are inline,
   large counts use loops when optimizing for size."
  ;; Check if shift count is constant
  (if (and (ast-node-p right-node)
           (eq (ast-node-type right-node) 'literal)
           (integerp (ast-node-value right-node)))
      ;; Constant shift count
      (let ((count (ast-node-value right-node)))
        (cond
          ;; Zero shift - just return value
          ((= count 0)
           (emit `(A=Rx ,left-reg)))
          ;; Shift by 32 or more - result is 0
          ((>= count 32)
           (emit '(A= 0)))
          ;; Small shift - inline
          ((< count *shift-inline-threshold*)
           (emit `(A=Rx ,left-reg))
           (dotimes (i count)
             (emit '(A=A<<1))))
          ;; Large shift - use loop if optimizing for size, inline otherwise
          ((compiler-state-optimize-size *state*)
           (generate-shift-left-loop left-reg count))
          (t
           ;; Inline for speed
           (emit `(A=Rx ,left-reg))
           (dotimes (i count)
             (emit '(A=A<<1))))))
      ;; Variable shift count - must use loop
      (progn
        (generate-expression right-node)
        (generate-shift-left-loop-variable left-reg))))

(defun generate-shift-left-loop (left-reg count)
  "Generate left shift loop for constant count"
  (let ((loop-label (gen-label "SHLLOOP"))
        (end-label (gen-label "SHLEND"))
        (temp-val (alloc-temp-reg))
        (temp-count (alloc-temp-reg))
        (temp-const (alloc-temp-reg)))
    (emit `(Rx= ,count ,temp-count))       ; temp-count = count
    (emit `(A=Rx ,left-reg))               ; A = value
    (emit-label loop-label)
    (emit `(Rx=A ,temp-val))               ; save value
    (emit `(A=Rx ,temp-count))
    (emit `(Rx= 0 ,temp-const))
    (emit `(A-=Rx ,temp-const))
    (emit `(jz ,end-label))
    (emit `(A=Rx ,temp-count))
    (emit `(Rx= -1 ,temp-const))
    (emit `(A+=Rx ,temp-const))
    (emit `(Rx=A ,temp-count))
    (emit `(A=Rx ,temp-val))
    (emit '(A=A<<1))
    (emit `(j ,loop-label))
    (emit-label end-label)
    (emit `(A=Rx ,temp-val))
    (free-temp-reg temp-const)
    (free-temp-reg temp-count)
    (free-temp-reg temp-val)))

(defun generate-shift-left-loop-variable (left-reg)
  "Generate left shift loop for variable count (count already in A)"
  (let ((loop-label (gen-label "SHLLOOP"))
        (end-label (gen-label "SHLEND"))
        (temp-val (alloc-temp-reg))
        (temp-count (alloc-temp-reg))
        (temp-const (alloc-temp-reg)))
    (emit `(Rx=A ,temp-count))             ; temp-count = count
    (emit `(A=Rx ,left-reg))               ; A = value
    (emit-label loop-label)
    (emit `(Rx=A ,temp-val))               ; save value
    (emit `(A=Rx ,temp-count))
    (emit `(Rx= 0 ,temp-const))
    (emit `(A-=Rx ,temp-const))
    (emit `(jz ,end-label))
    (emit `(A=Rx ,temp-count))
    (emit `(Rx= -1 ,temp-const))
    (emit `(A+=Rx ,temp-const))
    (emit `(Rx=A ,temp-count))
    (emit `(A=Rx ,temp-val))
    (emit '(A=A<<1))
    (emit `(j ,loop-label))
    (emit-label end-label)
    (emit `(A=Rx ,temp-val))
    (free-temp-reg temp-const)
    (free-temp-reg temp-count)
    (free-temp-reg temp-val)))

(defun generate-shift-right (left-reg right-node)
  "Generate right shift: result = left-reg >> right-node.
   Optimizes constant shift counts: small counts are inline,
   large counts use loops when optimizing for size."
  ;; Check if shift count is constant
  (if (and (ast-node-p right-node)
           (eq (ast-node-type right-node) 'literal)
           (integerp (ast-node-value right-node)))
      ;; Constant shift count
      (let ((count (ast-node-value right-node)))
        (cond
          ;; Zero shift - just return value
          ((= count 0)
           (emit `(A=Rx ,left-reg)))
          ;; Shift by 32 or more - result is 0 (for unsigned) or sign-extended
          ((>= count 32)
           (emit '(A= 0)))
          ;; Small shift - inline
          ((< count *shift-inline-threshold*)
           (emit `(A=Rx ,left-reg))
           (dotimes (i count)
             (emit '(A=A>>1))))
          ;; Large shift - use loop if optimizing for size, inline otherwise
          ((compiler-state-optimize-size *state*)
           (generate-shift-right-loop left-reg count))
          (t
           ;; Inline for speed
           (emit `(A=Rx ,left-reg))
           (dotimes (i count)
             (emit '(A=A>>1))))))
      ;; Variable shift count - must use loop
      (progn
        (generate-expression right-node)
        (generate-shift-right-loop-variable left-reg))))

(defun generate-shift-right-loop (left-reg count)
  "Generate right shift loop for constant count"
  (let ((loop-label (gen-label "SHRLOOP"))
        (end-label (gen-label "SHREND"))
        (temp-val (alloc-temp-reg))
        (temp-count (alloc-temp-reg))
        (temp-const (alloc-temp-reg)))
    (emit `(Rx= ,count ,temp-count))       ; temp-count = count
    (emit `(A=Rx ,left-reg))               ; A = value
    (emit-label loop-label)
    (emit `(Rx=A ,temp-val))               ; save value
    (emit `(A=Rx ,temp-count))
    (emit `(Rx= 0 ,temp-const))
    (emit `(A-=Rx ,temp-const))
    (emit `(jz ,end-label))
    (emit `(A=Rx ,temp-count))
    (emit `(Rx= -1 ,temp-const))
    (emit `(A+=Rx ,temp-const))
    (emit `(Rx=A ,temp-count))
    (emit `(A=Rx ,temp-val))
    (emit '(A=A>>1))
    (emit `(j ,loop-label))
    (emit-label end-label)
    (emit `(A=Rx ,temp-val))
    (free-temp-reg temp-const)
    (free-temp-reg temp-count)
    (free-temp-reg temp-val)))

(defun generate-shift-right-loop-variable (left-reg)
  "Generate right shift loop for variable count (count already in A)"
  (let ((loop-label (gen-label "SHRLOOP"))
        (end-label (gen-label "SHREND"))
        (temp-val (alloc-temp-reg))
        (temp-count (alloc-temp-reg))
        (temp-const (alloc-temp-reg)))
    (emit `(Rx=A ,temp-count))             ; temp-count = count
    (emit `(A=Rx ,left-reg))               ; A = value
    (emit-label loop-label)
    (emit `(Rx=A ,temp-val))               ; save value
    (emit `(A=Rx ,temp-count))
    (emit `(Rx= 0 ,temp-const))
    (emit `(A-=Rx ,temp-const))
    (emit `(jz ,end-label))
    (emit `(A=Rx ,temp-count))
    (emit `(Rx= -1 ,temp-const))
    (emit `(A+=Rx ,temp-const))
    (emit `(Rx=A ,temp-count))
    (emit `(A=Rx ,temp-val))
    (emit '(A=A>>1))
    (emit `(j ,loop-label))
    (emit-label end-label)
    (emit `(A=Rx ,temp-val))
    (free-temp-reg temp-const)
    (free-temp-reg temp-count)
    (free-temp-reg temp-val)))

;;; ===========================================================================
;;; Global Variable Generation
;;; ===========================================================================

(defun generate-global-decls (node)
  "Generate code for global variable declarations"
  (dolist (decl (ast-node-children node))
    ;; Skip function prototypes - they don't generate code
    (unless (eq (ast-node-type decl) 'func-prototype)
      (generate-global-var decl))))

(defun generate-global-var (node)
  "Generate code/data for a global variable with optional initializer"
  (let* ((name (ast-node-value node))
         (var-type (ast-node-result-type node))
         (init-data (ast-node-data node))
         (label (make-c-label name))
         (alignment (type-alignment var-type)))
    ;; Ensure proper alignment before data label
    (when (>= alignment 4)
      (emit '(lalign-dword 0)))
    (when (= alignment 2)
      (emit '(lalign-word 0)))
    (emit `(label ,label))
    (cond
      ;; Init-list for array or struct
      ((and (ast-node-p init-data)
            (eq (ast-node-type init-data) 'init-list))
       (generate-global-init-list init-data var-type))

      ;; String literal for char array
      ((and (ast-node-p init-data)
            (eq (ast-node-type init-data) 'string-literal)
            (type-desc-array-size var-type)
            (eq (type-desc-base var-type) 'char))
       (generate-global-string-array init-data (type-desc-array-size var-type)))

      ;; Simple value
      (t
       (let ((init-value (or init-data 0))
             (size (type-size var-type)))
         ;; Check if init-value is a label reference (:label SYMBOL)
         (when (and (listp init-value) (eq (car init-value) :label))
           (setf init-value (second init-value)))  ; extract the label symbol
         ;; For arrays without initializer, emit zeroes for each element
         (if (type-desc-array-size var-type)
             (let* ((array-size (type-desc-array-size var-type))
                    (elem-size (type-desc-size var-type)))
               (dotimes (i array-size)
                 (case elem-size
                   (1 (emit `(abyte 0)))
                   (2 (emit `(aword 0)))
                   (otherwise (emit `(adword 0))))))
             ;; Non-array: emit single value
             (case size
               (1 (emit `(abyte ,init-value)))
               (2 (emit `(aword ,init-value)))
               (otherwise (emit `(adword ,init-value))))))))))

(defun generate-global-init-list (init-list target-type)
  "Generate data section directives for a global init-list."
  (cond
    ;; Array initialization
    ((type-desc-array-size target-type)
     (let* ((element-type (get-array-element-type target-type))
            (element-size (type-size element-type))
            (array-size (type-desc-array-size target-type))
            (init-elements (ast-node-children init-list))
            ;; Check if we have array designated initializers
            (has-array-designated (some (lambda (e)
                                          (and (ast-node-p e)
                                               (eq (ast-node-type e) 'array-designated-init)))
                                        init-elements)))
       (if has-array-designated
           ;; Handle array designated initializers by building a value array
           (let ((values (make-array array-size :initial-element nil))
                 (current-index 0))
             ;; Populate values array
             (dolist (elem init-elements)
               (cond
                 ;; Array designated initializer [n] = value
                 ((and (ast-node-p elem)
                       (eq (ast-node-type elem) 'array-designated-init))
                  (let* ((index-expr (first (ast-node-children elem)))
                         (init-expr (second (ast-node-children elem)))
                         (index-val (evaluate-constant-expression index-expr)))
                    (when (and index-val (< index-val array-size))
                      (setf (aref values index-val) init-expr)
                      (setf current-index (1+ index-val)))))
                 ;; Regular element
                 (t
                  (when (< current-index array-size)
                    (setf (aref values current-index) elem)
                    (incf current-index)))))
             ;; Emit data for each element
             (dotimes (i array-size)
               (let ((elem (aref values i)))
                 (cond
                   ;; Nested init-list
                   ((and elem (ast-node-p elem)
                         (eq (ast-node-type elem) 'init-list))
                    (generate-global-init-list elem element-type))
                   ;; Has a value
                   (elem
                    (let ((val (if (and (ast-node-p elem)
                                        (eq (ast-node-type elem) 'literal))
                                   (ast-node-value elem)
                                   (or (evaluate-constant-expression elem) 0))))
                      (case element-size
                        (1 (emit `(abyte ,val)))
                        (2 (emit `(aword ,val)))
                        (otherwise (emit `(adword ,val))))))
                   ;; Zero for uninitialized
                   (t
                    (case element-size
                      (1 (emit `(abyte 0)))
                      (2 (emit `(aword 0)))
                      (otherwise (emit `(adword 0)))))))))
           ;; No designated initializers - simple case
           (let ((num-inits (length init-elements)))
             ;; Emit data for each initializer element
             (dolist (elem init-elements)
               (cond
                 ;; Nested init-list (for arrays of structs or 2D arrays)
                 ((and (ast-node-p elem)
                       (eq (ast-node-type elem) 'init-list))
                  (generate-global-init-list elem element-type))
                 ;; Literal value
                 ((and (ast-node-p elem)
                       (eq (ast-node-type elem) 'literal))
                  (let ((val (ast-node-value elem)))
                    (case element-size
                      (1 (emit `(abyte ,val)))
                      (2 (emit `(aword ,val)))
                      (otherwise (emit `(adword ,val))))))
                 ;; Constant expression
                 (t
                  (let ((val (evaluate-constant-expression elem)))
                    (unless val
                      (compiler-error "Global array initializer must be a constant expression"))
                    (case element-size
                      (1 (emit `(abyte ,val)))
                      (2 (emit `(aword ,val)))
                      (otherwise (emit `(adword ,val))))))))
             ;; Zero-fill remaining elements
             (when (< num-inits array-size)
               (dotimes (i (- array-size num-inits))
                 (case element-size
                   (1 (emit `(abyte 0)))
                   (2 (emit `(aword 0)))
                   (otherwise (emit `(adword 0))))))))))

    ;; Struct initialization
    ((type-desc-struct-tag target-type)
     (let* ((struct-def (lookup-struct-def (type-desc-struct-tag target-type)))
            (members (when struct-def (struct-def-members struct-def)))
            (init-elements (ast-node-children init-list))
            (current-offset 0)
            ;; Check if we have designated initializers
            (has-designated (some (lambda (e)
                                    (and (ast-node-p e)
                                         (eq (ast-node-type e) 'designated-init)))
                                  init-elements)))
       (when struct-def
         (if has-designated
             ;; Handle designated initializers - build a map from member name to value
             (let ((init-map (make-hash-table :test #'equal)))
               ;; Populate the map from designated initializers
               (dolist (elem init-elements)
                 (when (and (ast-node-p elem)
                            (eq (ast-node-type elem) 'designated-init))
                   (let ((field-name (ast-node-value elem))
                         (init-expr (first (ast-node-children elem))))
                     (setf (gethash field-name init-map) init-expr))))
               ;; Now emit members in order
               (dolist (member members)
                 (let* ((member-name (struct-member-name member))
                        (member-type (struct-member-type member))
                        (member-offset (struct-member-offset member))
                        (member-size (type-size member-type))
                        (elem (gethash member-name init-map)))
                   ;; Emit padding if needed
                   (when (> member-offset current-offset)
                     (dotimes (i (- member-offset current-offset))
                       (emit `(abyte 0))))
                   (setf current-offset member-offset)
                   (cond
                     ;; Nested init-list for nested struct/array
                     ((and elem
                           (ast-node-p elem)
                           (eq (ast-node-type elem) 'init-list))
                      (generate-global-init-list elem member-type)
                      (incf current-offset (type-size member-type)))
                     ;; Value (or nil/0 for uninitialized)
                     (t
                      (let ((val (if elem
                                     (or (evaluate-constant-expression elem) 0)
                                     0)))
                        ;; Check if val is a label reference (:label SYMBOL)
                        (when (and (listp val) (eq (car val) :label))
                          (setf val (second val)))
                        (case member-size
                          (1 (emit `(abyte ,val)))
                          (2 (emit `(aword ,val)))
                          (otherwise (emit `(adword ,val))))
                        (incf current-offset member-size)))))))
             ;; Non-designated: iterate through members and pop elements in order
             (loop for member in members
                   for elem = (pop init-elements)
                   do (let* ((member-type (struct-member-type member))
                             (member-offset (struct-member-offset member))
                             (member-size (type-size member-type)))
                        ;; Emit padding if needed
                        (when (> member-offset current-offset)
                          (dotimes (i (- member-offset current-offset))
                            (emit `(abyte 0))))
                        (setf current-offset member-offset)
                        (cond
                          ;; Nested init-list for nested struct/array
                          ((and elem
                                (ast-node-p elem)
                                (eq (ast-node-type elem) 'init-list))
                           (generate-global-init-list elem member-type)
                           (incf current-offset (type-size member-type)))
                          ;; Value (or nil for uninitialized)
                          (t
                           (let ((val (if elem
                                          (or (evaluate-constant-expression elem) 0)
                                          0)))
                             ;; Check if val is a label reference (:label SYMBOL)
                             (when (and (listp val) (eq (car val) :label))
                               (setf val (second val)))
                             (case member-size
                               (1 (emit `(abyte ,val)))
                               (2 (emit `(aword ,val)))
                               (otherwise (emit `(adword ,val))))
                             (incf current-offset member-size)))))))
         ;; Emit trailing padding to reach struct size
         (let ((struct-size (struct-def-size struct-def)))
           (when (< current-offset struct-size)
             (dotimes (i (- struct-size current-offset))
               (emit `(abyte 0))))))))

    ;; Fallback
    (t (compiler-warning "Unknown type for global init-list"))))

(defun generate-global-string-array (string-node array-size)
  "Generate data section for a char array initialized from a string literal."
  (let* ((str (ast-node-value string-node))
         (str-len (length str)))
    ;; Emit each character
    (loop for i from 0 below str-len
          for char-code = (char-code (char str i))
          do (emit `(abyte ,char-code)))
    ;; Emit null terminator
    (emit `(abyte 0))
    ;; Zero-fill rest if array is larger
    (when (> array-size (1+ str-len))
      (dotimes (i (- array-size (1+ str-len)))
        (emit `(abyte 0))))))

;;; ===========================================================================
;;; String Literal Collection
;;; ===========================================================================

(defun collect-strings (node)
  "Collect all string literals in AST"
  (when (and node (ast-node-p node))
    (when (eq (ast-node-type node) 'string-literal)
      (let ((str (ast-node-value node)))
        (unless (find str *string-literals* :key #'cdr :test #'string=)
          (push (cons (gen-label "STR") str) *string-literals*))))
    (dolist (child (ast-node-children node))
      (collect-strings child))))
