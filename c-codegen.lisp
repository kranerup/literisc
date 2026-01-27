;;; ===========================================================================
;;; ====================== C Code Generator for liteRISC ======================
;;; ===========================================================================
;;; Generates S-expression assembly for the liteRISC assembler

(in-package :c-compiler)

;;; ===========================================================================
;;; Register Allocation
;;; ===========================================================================

;;; Register usage:
;;; R0-R5  : Temporaries / scratch (for expression evaluation)
;;; R6-R9  : Local variable registers (when register-allocated)
;;; P0-P3  : Parameters (R10-R13), P0 is also return value
;;; SRP    : Subroutine return pointer (R14)
;;; SP     : Stack pointer (R15)
;;; A      : Accumulator - primary computation register

(defparameter *temp-regs* '(R0 R1 R2 R3 R4 R5))  ; 6 regs for temps
(defparameter *local-regs* '(R6 R7 R8 R9))        ; 4 regs for locals
(defparameter *param-regs* '(P0 P1 P2 P3))

;;; Track which temp registers are in use
(defvar *reg-in-use* nil)

;;; Track local register usage for current function
(defvar *local-reg-count* 0)  ; how many of R6-R9 are used

(defun init-registers ()
  "Initialize register allocation state"
  (setf *reg-in-use* (make-array 6 :initial-element nil)))  ; Only 6 temp regs now

(defun alloc-temp-reg ()
  "Allocate a temporary register"
  (loop for i from 0 below 6
        when (not (aref *reg-in-use* i))
        do (progn
             (setf (aref *reg-in-use* i) t)
             (return-from alloc-temp-reg (nth i *temp-regs*))))
  (compiler-error "Out of temporary registers"))

(defun free-temp-reg (reg)
  "Free a temporary register"
  (let ((idx (position reg *temp-regs*)))
    (when idx
      (setf (aref *reg-in-use* idx) nil))))

(defun save-temp-regs ()
  "Return list of temp registers currently in use"
  (loop for i from 0 below 6
        when (aref *reg-in-use* i)
        collect (nth i *temp-regs*)))

(defun get-local-reg (index)
  "Get local register by index (0-3 -> R6-R9)"
  (nth index *local-regs*))

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
  ;; Check if bit 7 is set: if (A & 0x80) A |= 0xFFFFFF00
  (let ((done-label (gen-label "SEXTBDONE"))
        (save-reg (alloc-temp-reg))
        (mask-reg (alloc-temp-reg)))
    (emit `(Rx=A ,save-reg))       ; save original value
    (emit `(Rx= #x80 ,mask-reg))
    (emit `(A&=Rx ,mask-reg))      ; A = A & 0x80
    (emit `(Rx= 0 ,mask-reg))
    (emit `(A-=Rx ,mask-reg))      ; test if zero
    (emit `(jz ,done-label))       ; if bit 7 not set, done
    ;; Sign bit is set, extend with 1s
    (emit `(A=Rx ,save-reg))       ; restore original
    (emit `(Rx= #xFFFFFF00 ,mask-reg))
    (emit `(A\|=Rx ,mask-reg))     ; A |= 0xFFFFFF00
    (emit `(Rx=A ,save-reg))       ; save result
    (emit-label done-label)
    (emit `(A=Rx ,save-reg))       ; A = result
    (free-temp-reg mask-reg)
    (free-temp-reg save-reg)))

(defun emit-sign-extend-word ()
  "Sign extend word in A (bit 15 → bits 16-31)"
  ;; Check if bit 15 is set: if (A & 0x8000) A |= 0xFFFF0000
  (let ((done-label (gen-label "SEXTWDONE"))
        (save-reg (alloc-temp-reg))
        (mask-reg (alloc-temp-reg)))
    (emit `(Rx=A ,save-reg))       ; save original value
    (emit `(Rx= #x8000 ,mask-reg))
    (emit `(A&=Rx ,mask-reg))      ; A = A & 0x8000
    (emit `(Rx= 0 ,mask-reg))
    (emit `(A-=Rx ,mask-reg))      ; test if zero
    (emit `(jz ,done-label))       ; if bit 15 not set, done
    ;; Sign bit is set, extend with 1s
    (emit `(A=Rx ,save-reg))       ; restore original
    (emit `(Rx= #xFFFF0000 ,mask-reg))
    (emit `(A\|=Rx ,mask-reg))     ; A |= 0xFFFF0000
    (emit `(Rx=A ,save-reg))       ; save result
    (emit-label done-label)
    (emit `(A=Rx ,save-reg))       ; A = result
    (free-temp-reg mask-reg)
    (free-temp-reg save-reg)))

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
  (init-registers)
  (setf *string-literals* nil)

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
    (generate-top-level child)))

(defun generate-top-level (node)
  "Generate code for a top-level declaration"
  (case (ast-node-type node)
    (function (generate-function node))
    (decl-list (generate-global-decls node))
    (global-var (generate-global-var node))
    (otherwise
     (compiler-warning "Ignoring top-level ~a" (ast-node-type node)))))

;;; ===========================================================================
;;; Function Generation
;;; ===========================================================================

(defun generate-function (node)
  "Generate code for a function"
  (let* ((name (ast-node-value node))
         (params (first (ast-node-children node)))
         (body (second (ast-node-children node)))
         (func-data (ast-node-data node))
         (func-label (intern (string-upcase name) :c-compiler))
         (*function-end-label* (gen-label (format nil "~a_END" (string-upcase name))))
         (*is-leaf-function* (is-leaf-function body))
         (*frame-size* (or (getf func-data :frame-size) 0))
         (*current-param-count* (or (getf func-data :param-count) 0))
         (*local-reg-count* (or (getf func-data :local-reg-count) 0))
         (body-ends-with-return (ends-with-return-p body)))

    ;; Set current function for symbol lookup
    (setf (compiler-state-current-function *state*) name)

    (init-registers)

    ;; Function header annotation
    (emit-comment (format nil "======== function ~a ========" name))
    (emit-comment (format nil "frame-size: ~a, params: ~a, local-regs: ~a, leaf: ~a"
                          *frame-size* *current-param-count* *local-reg-count*
                          (if *is-leaf-function* "yes" "no")))

    ;; Function label
    (emit `(label ,func-label))

    ;; Prologue
    (generate-prologue params)

    ;; Body - if it ends with return, skip the final jump
    (if body-ends-with-return
        (generate-body-with-final-return body)
        (generate-statement body))

    ;; Epilogue (also target for return statements)
    (emit-label *function-end-label*)
    (generate-epilogue)))

(defun is-leaf-function (body)
  "Check if function body contains no function calls"
  (not (contains-call body)))

(defun contains-call (node)
  "Check if AST node or children contain a function call"
  (when (and node (ast-node-p node))
    (or (eq (ast-node-type node) 'call)
        (some #'contains-call (ast-node-children node)))))

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
      ;; Body is a block - generate all but last, then last with skip flag
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
      (otherwise
       (compiler-warning "Unknown statement type: ~a" (ast-node-type node))))))

(defun generate-block (node)
  "Generate code for a block of statements"
  (dolist (stmt (ast-node-children node))
    (generate-statement stmt)))

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
    (emit `(Rx= 0 R0))
    (emit `(A-=Rx R0))
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
    (emit `(Rx= 0 R0))
    (emit `(A-=Rx R0))
    (emit `(jz ,end-label))

    ;; Body
    (generate-statement body)
    (emit `(j ,loop-label))

    (emit-label end-label)))

(defun generate-for (node)
  "Generate code for a for statement"
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
      (emit `(Rx= 0 R0))
      (emit `(A-=Rx R0))
      (emit `(jz ,end-label)))

    ;; Body
    (generate-statement body)

    ;; Continue target (before update)
    (emit-label continue-label)

    ;; Update
    (when update
      (generate-expression update))

    (emit `(j ,loop-label))
    (emit-label end-label)))

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
    (emit `(Rx= 0 R0))
    (emit `(A-=Rx R0))
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

(defun generate-expr-stmt (node)
  "Generate code for an expression statement"
  (generate-expression (first (ast-node-children node))))

(defun generate-local-decls (node)
  "Generate code for local variable declarations"
  (dolist (decl (ast-node-children node))
    (when (eq (ast-node-type decl) 'var-decl)
      (let ((name (ast-node-value decl))
            (init (first (ast-node-children decl)))
            (var-type (ast-node-result-type decl)))
        (when init
          ;; Generate initializer
          (generate-expression init)
          ;; Mask value to appropriate size before storing
          (when (and var-type (< (type-size var-type) 4))
            (emit-mask-to-size var-type))
          ;; Store to local variable
          (let ((sym (lookup-symbol name)))
            (when sym
              (case (sym-entry-storage sym)
                (:register
                 ;; Store directly to local register (R6-R9)
                 (emit `(Rx=A ,(get-local-reg (sym-entry-offset sym)))))
                (:local
                 ;; Store to stack with appropriate size
                 (generate-store-local (sym-entry-offset sym) var-type))))))))))

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
                   (save-local-reg-space (if *is-leaf-function* 0
                                             (* 4 *local-reg-count*)))
                   (temp (alloc-temp-reg)))
              (let ((offset (+ *frame-size* save-param-space save-local-reg-space
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
       ;; Load from global address with sized load
       (let ((label (intern (string-upcase name) :c-compiler))
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
      (otherwise (ast-node-result-type node)))))

(defun pointer-element-size (type)
  "Get the element size for pointer arithmetic (returns nil for non-pointers)"
  (when (and type
             (type-desc-p type)
             (> (type-desc-pointer-level type) 0))
    ;; Use the size slot for element size, default to 4 for int pointers
    (or (type-desc-size type) 4)))

(defun emit-scale-for-pointer (elem-size)
  "Scale value in A by element size for pointer arithmetic"
  (case elem-size
    (1 nil)                     ; no scaling needed for char*
    (2 (emit '(A=A<<1)))        ; * 2 for short*
    (4 (emit '(A=A<<1))         ; * 4 for int*
       (emit '(A=A<<1)))
    (otherwise
     ;; General case: use software multiply
     (let ((size-reg (alloc-temp-reg)))
       (emit `(Rx= ,elem-size ,size-reg))
       (generate-multiply size-reg)
       (free-temp-reg size-reg)))))

(defun generate-arithmetic-op (op left right)
  "Generate code for arithmetic/bitwise operation"
  ;; Check for pointer arithmetic
  (let* ((left-type (get-expression-type left))
         (right-type (get-expression-type right))
         (left-elem-size (pointer-element-size left-type))
         (right-elem-size (pointer-element-size right-type)))

    ;; Generate left operand
    (generate-expression left)

    ;; Save left result
    (let ((left-reg (alloc-temp-reg)))
      (emit `(Rx=A ,left-reg))

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
           (free-temp-reg right-reg)))

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
       (generate-shift-left left-reg))

      ((string= op ">>")
       (generate-shift-right left-reg))

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

    (free-temp-reg left-reg))))

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
    (emit `(Rx= 0 R0))
    (emit `(A-=Rx R0))
    (emit `(jz ,false-label))

    ;; Left was true, evaluate right
    (generate-expression right)
    (emit `(Rx= 0 R0))
    (emit `(A-=Rx R0))
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
    (emit `(Rx= 0 R0))
    (emit `(A-=Rx R0))
    (emit `(jnz ,true-label))

    ;; Left was false, evaluate right
    (generate-expression right)
    (emit `(Rx= 0 R0))
    (emit `(A-=Rx R0))
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
       (emit '(Rx=A R0))
       (emit '(A= 0))
       (emit '(A-=Rx R0)))

      ((string= op "!")
       ;; Logical not: x == 0 ? 1 : 0
       (generate-expression operand)
       (let ((zero-label (gen-label "NOTZERO"))
             (end-label (gen-label "NOTEND")))
         (emit `(Rx= 0 R0))
         (emit `(A-=Rx R0))
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
  ;; Load current value
  (generate-expression operand)
  ;; Add delta
  (emit `(Rx= ,delta R0))
  (emit '(A+=Rx R0))
  ;; Store back and keep result
  (generate-store-lvalue operand))

(defun generate-post-op (node)
  "Generate code for post-increment/decrement"
  (let ((op (ast-node-value node))
        (operand (first (ast-node-children node))))
    ;; Load current value
    (generate-expression operand)
    ;; Save original value
    (let ((save-reg (alloc-temp-reg)))
      (emit `(Rx=A ,save-reg))
      ;; Compute new value
      (if (string= op "++")
          (emit `(Rx= 1 R0))
          (emit `(Rx= -1 R0)))
      (emit '(A+=Rx R0))
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
          (let ((label (intern (string-upcase name) :c-compiler))
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
            (let ((label (intern (string-upcase name) :c-compiler))
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
     (let ((value-reg (alloc-temp-reg))
           (elem-type (get-subscript-element-type node)))
       (emit `(Rx=A ,value-reg))  ; save value
       (generate-subscript-address node)  ; get address
       (if (and elem-type (< (type-size elem-type) 4))
           (emit-store-sized elem-type value-reg)
           (emit `(M[A]=Rx ,value-reg)))  ; store
       (free-temp-reg value-reg)))
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
               ;; Return element type (pointer level - 1 or same for array)
               (make-type-desc :base (type-desc-base arr-type)
                               :pointer-level (max 0 (1- (type-desc-pointer-level arr-type)))
                               :size (type-desc-size arr-type)
                               :unsigned-p (type-desc-unsigned-p arr-type)))))))
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
         (func-name (if (eq (ast-node-type func-expr) 'var-ref)
                        (ast-node-value func-expr)
                        nil))
         (func-label (when func-name
                       (intern (string-upcase func-name) :c-compiler)))
         (arg-count (length args)))

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

      ;; Save caller-saved registers (excluding the arg temps we just allocated)
      (let ((saved-regs (remove-if (lambda (r) (member r arg-temps))
                                   (save-temp-regs))))
        (dolist (reg saved-regs)
          (emit `(push-r ,reg)))

        ;; Move evaluated args from temp regs to P0-P3
        (loop for i from 0 below (min arg-count 4)
              for temp in arg-temps
              do (progn
                   (emit `(A=Rx ,temp))
                   (emit `(Rx=A ,(nth i *param-regs*)))))

        ;; Call function
        (emit `(jsr ,func-label))

        ;; Clean up stack arguments
        (when (> arg-count 4)
          (let ((stack-args (* 4 (- arg-count 4))))
            (emit `(A=Rx SP))
            (emit `(Rx= ,stack-args R0))
            (emit '(A+=Rx R0))
            (emit '(Rx=A SP))))

        ;; Restore caller-saved registers (reverse order)
        (dolist (reg (reverse saved-regs))
          (emit `(pop-r ,reg)))

        ;; Free the arg temps
        (dolist (temp arg-temps)
          (free-temp-reg temp))

        ;; Result is in P0, move to A
        (emit '(A=Rx P0))))))

(defun generate-subscript (node)
  "Generate code for array subscript with sized load"
  (let ((elem-type (get-subscript-element-type node)))
    (generate-subscript-address node)
    ;; Load value at computed address with appropriate size
    (let ((temp (alloc-temp-reg)))
      (if (and elem-type (< (type-size elem-type) 4))
          (emit-load-sized elem-type temp)
          (emit `(Rx=M[A] ,temp)))
      (emit `(A=Rx ,temp))
      (free-temp-reg temp)
      ;; Apply sign extension for signed sub-word types
      (when elem-type
        (emit-promote-to-int elem-type)))))

(defun generate-subscript-address (node)
  "Generate code to compute address of array subscript"
  (let* ((array (first (ast-node-children node)))
         (index (second (ast-node-children node)))
         (elem-type (get-subscript-element-type node))
         (elem-size (if elem-type (type-size elem-type) 4))
         (base-reg (alloc-temp-reg)))
    ;; Get array base address
    (generate-expression array)
    (emit `(Rx=A ,base-reg))  ; save base

    ;; Get index
    (generate-expression index)
    ;; Multiply by element size
    (case elem-size
      (1 nil)                     ; no shift needed for byte
      (2 (emit '(A=A<<1)))        ; * 2 for short
      (4 (emit '(A=A<<1))         ; * 4 for int/pointer
         (emit '(A=A<<1)))
      (otherwise
       ;; General case: use software multiply
       (let ((size-reg (alloc-temp-reg)))
         (emit `(Rx= ,elem-size ,size-reg))
         (generate-multiply size-reg)
         (free-temp-reg size-reg))))

    ;; Add to base
    (emit `(A+=Rx ,base-reg))
    (free-temp-reg base-reg)))

(defun generate-ternary (node)
  "Generate code for ternary conditional"
  (let ((condition (first (ast-node-children node)))
        (then-expr (second (ast-node-children node)))
        (else-expr (third (ast-node-children node)))
        (else-label (gen-label "TERNELSE"))
        (end-label (gen-label "TERNEND")))

    ;; Evaluate condition
    (generate-expression condition)
    (emit `(Rx= 0 R0))
    (emit `(A-=Rx R0))
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

(defun generate-shift-left (left-reg)
  "Generate left shift: A = left-reg << A"
  (let ((loop-label (gen-label "SHLLOOP"))
        (end-label (gen-label "SHLEND")))

    ;; A = shift count, left-reg = value
    (emit '(Rx=A R2))           ; R2 = count
    (emit `(A=Rx ,left-reg))    ; A = value

    (emit-label loop-label)
    ;; Check if count is zero
    (emit '(Rx=A R1))           ; save value
    (emit '(A=Rx R2))
    (emit '(Rx= 0 R0))
    (emit '(A-=Rx R0))
    (emit `(jz ,end-label))

    ;; Decrement count
    (emit '(A=Rx R2))
    (emit '(Rx= -1 R0))
    (emit '(A+=Rx R0))
    (emit '(Rx=A R2))

    ;; Shift value left
    (emit '(A=Rx R1))
    (emit '(A=A<<1))

    (emit `(j ,loop-label))

    (emit-label end-label)
    (emit '(A=Rx R1))))

(defun generate-shift-right (left-reg)
  "Generate right shift: A = left-reg >> A"
  (let ((loop-label (gen-label "SHRLOOP"))
        (end-label (gen-label "SHREND")))

    ;; A = shift count, left-reg = value
    (emit '(Rx=A R2))           ; R2 = count
    (emit `(A=Rx ,left-reg))    ; A = value

    (emit-label loop-label)
    ;; Check if count is zero
    (emit '(Rx=A R1))           ; save value
    (emit '(A=Rx R2))
    (emit '(Rx= 0 R0))
    (emit '(A-=Rx R0))
    (emit `(jz ,end-label))

    ;; Decrement count
    (emit '(A=Rx R2))
    (emit '(Rx= -1 R0))
    (emit '(A+=Rx R0))
    (emit '(Rx=A R2))

    ;; Shift value right
    (emit '(A=Rx R1))
    (emit '(A=A>>1))

    (emit `(j ,loop-label))

    (emit-label end-label)
    (emit '(A=Rx R1))))

;;; ===========================================================================
;;; Global Variable Generation
;;; ===========================================================================

(defun generate-global-decls (node)
  "Generate code for global variable declarations"
  (dolist (decl (ast-node-children node))
    (generate-global-var decl)))

(defun generate-global-var (node)
  "Generate code/data for a global variable"
  (let* ((name (ast-node-value node))
         (var-type (ast-node-result-type node))
         (label (intern (string-upcase name) :c-compiler))
         (size (type-size var-type)))
    (emit `(label ,label))
    (emit `(lalloc-bytes ,size))))

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
