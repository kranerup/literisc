;;; ===========================================================================
;;; ==================== C Optimizer for liteRISC ============================
;;; ===========================================================================
;;; Optimization passes for the C compiler AST

(in-package :c-compiler)

;;; ===========================================================================
;;; Constant Folding
;;; ===========================================================================

(defun fold-constants (node)
  "Recursively fold constant expressions in AST.
   Returns the optimized AST node (possibly a new node)."
  (when (null node)
    (return-from fold-constants nil))

  ;; If it's a type-desc (from sizeof), return as-is
  (when (type-desc-p node)
    (return-from fold-constants node))

  ;; Must be an AST node
  (unless (ast-node-p node)
    (return-from fold-constants node))

  (case (ast-node-type node)
    ;; Recursively process children for compound nodes
    (program
     (make-ast-node :type 'program
                    :children (mapcar #'fold-constants (ast-node-children node))
                    :source-loc (ast-node-source-loc node)))

    (function
     (make-ast-node :type 'function
                    :value (ast-node-value node)
                    :children (mapcar #'fold-constants (ast-node-children node))
                    :result-type (ast-node-result-type node)
                    :source-loc (ast-node-source-loc node)
                    :data (ast-node-data node)))

    (params
     (make-ast-node :type 'params
                    :children (mapcar #'fold-constants (ast-node-children node))
                    :source-loc (ast-node-source-loc node)))

    (param
     node)  ; Params don't need folding

    (block
     (make-ast-node :type 'block
                    :children (mapcar #'fold-constants (ast-node-children node))
                    :source-loc (ast-node-source-loc node)))

    (if
     (let ((folded-children (mapcar #'fold-constants (ast-node-children node))))
       (make-ast-node :type 'if
                      :children folded-children
                      :source-loc (ast-node-source-loc node))))

    (while
     (let ((folded-children (mapcar #'fold-constants (ast-node-children node))))
       (make-ast-node :type 'while
                      :children folded-children
                      :source-loc (ast-node-source-loc node))))

    (for
     (let ((folded-children (mapcar #'fold-constants (ast-node-children node))))
       (make-ast-node :type 'for
                      :children folded-children
                      :source-loc (ast-node-source-loc node))))

    (do-while
     (let ((folded-children (mapcar #'fold-constants (ast-node-children node))))
       (make-ast-node :type 'do-while
                      :children folded-children
                      :source-loc (ast-node-source-loc node))))

    (return
     (make-ast-node :type 'return
                    :children (mapcar #'fold-constants (ast-node-children node))
                    :source-loc (ast-node-source-loc node)))

    (expr-stmt
     (make-ast-node :type 'expr-stmt
                    :children (mapcar #'fold-constants (ast-node-children node))
                    :source-loc (ast-node-source-loc node)))

    (decl-list
     (make-ast-node :type 'decl-list
                    :children (mapcar #'fold-constants (ast-node-children node))
                    :source-loc (ast-node-source-loc node)))

    (var-decl
     (make-ast-node :type 'var-decl
                    :value (ast-node-value node)
                    :children (mapcar #'fold-constants (ast-node-children node))
                    :result-type (ast-node-result-type node)
                    :source-loc (ast-node-source-loc node)))

    (global-var
     node)  ; Global vars don't need folding

    ;; Expressions that can be folded
    (binary-op
     (fold-binary-op node))

    (unary-op
     (fold-unary-op node))

    (assign
     (make-ast-node :type 'assign
                    :value (ast-node-value node)
                    :children (list (fold-constants (first (ast-node-children node)))
                                    (fold-constants (second (ast-node-children node))))
                    :source-loc (ast-node-source-loc node)))

    (call
     (make-ast-node :type 'call
                    :children (mapcar #'fold-constants (ast-node-children node))
                    :source-loc (ast-node-source-loc node)))

    (subscript
     (make-ast-node :type 'subscript
                    :children (mapcar #'fold-constants (ast-node-children node))
                    :source-loc (ast-node-source-loc node)))

    (ternary
     (fold-ternary node))

    (cast
     (fold-cast node))

    (sizeof
     (fold-sizeof node))

    (post-op
     (make-ast-node :type 'post-op
                    :value (ast-node-value node)
                    :children (mapcar #'fold-constants (ast-node-children node))
                    :source-loc (ast-node-source-loc node)))

    ;; Leaf nodes - no folding needed
    (literal node)
    (string-literal node)
    (var-ref
     ;; Check if var-ref is an enum constant - fold to literal
     (let* ((name (ast-node-value node))
            (sym (lookup-symbol name)))
       (if (and sym (eq (sym-entry-storage sym) :enum-constant))
           ;; Fold enum constant to literal
           (make-constant-node (sym-entry-offset sym)
                               (make-int-type)
                               (ast-node-source-loc node))
           ;; Not an enum constant - keep as var-ref
           node)))
    (break node)
    (continue node)
    (empty node)

    ;; Default: return node unchanged
    (otherwise node)))

(defun is-constant-node (node)
  "Check if node is a constant literal"
  (and (ast-node-p node)
       (eq (ast-node-type node) 'literal)
       (integerp (ast-node-value node))))

(defun get-constant-value (node)
  "Get the constant value from a literal node"
  (ast-node-value node))

(defun make-constant-node (value &optional result-type source-loc)
  "Create a new constant literal node"
  (make-ast-node :type 'literal
                 :value (to-32bit-signed value)
                 :result-type (or result-type (make-int-type))
                 :source-loc source-loc))

(defun to-32bit-signed (n)
  "Convert a value to 32-bit signed integer (wrap on overflow)"
  (let ((masked (logand n #xFFFFFFFF)))
    (if (>= masked #x80000000)
        (- masked #x100000000)
        masked)))

(defun to-32bit-unsigned (n)
  "Convert a value to 32-bit unsigned integer"
  (logand n #xFFFFFFFF))

;;; ===========================================================================
;;; Binary Operation Folding
;;; ===========================================================================

(defun fold-binary-op (node)
  "Fold binary operation if both operands are constants"
  (let* ((op (ast-node-value node))
         (left (fold-constants (first (ast-node-children node))))
         (right (fold-constants (second (ast-node-children node)))))

    ;; Handle short-circuit operators specially
    (cond
      ;; && short-circuit: 0 && x -> 0
      ((string= op "&&")
       (cond
         ((and (is-constant-node left) (zerop (get-constant-value left)))
          (make-constant-node 0 (make-int-type) (ast-node-source-loc node)))
         ;; 1 && x -> x (converted to bool)
         ((and (is-constant-node left) (not (zerop (get-constant-value left)))
               (is-constant-node right))
          (make-constant-node (if (zerop (get-constant-value right)) 0 1)
                              (make-int-type) (ast-node-source-loc node)))
         (t
          (make-ast-node :type 'binary-op
                         :value op
                         :children (list left right)
                         :source-loc (ast-node-source-loc node)))))

      ;; || short-circuit: 1 || x -> 1
      ((string= op "||")
       (cond
         ((and (is-constant-node left) (not (zerop (get-constant-value left))))
          (make-constant-node 1 (make-int-type) (ast-node-source-loc node)))
         ;; 0 || x -> x (converted to bool)
         ((and (is-constant-node left) (zerop (get-constant-value left))
               (is-constant-node right))
          (make-constant-node (if (zerop (get-constant-value right)) 0 1)
                              (make-int-type) (ast-node-source-loc node)))
         (t
          (make-ast-node :type 'binary-op
                         :value op
                         :children (list left right)
                         :source-loc (ast-node-source-loc node)))))

      ;; Both operands are constants - evaluate at compile time
      ((and (is-constant-node left) (is-constant-node right))
       (let ((left-val (get-constant-value left))
             (right-val (get-constant-value right)))
         ;; Don't fold division/modulo by zero
         (if (and (member op '("/" "%") :test #'string=)
                  (zerop right-val))
             ;; Return unfolded node
             (make-ast-node :type 'binary-op
                            :value op
                            :children (list left right)
                            :source-loc (ast-node-source-loc node))
             ;; Evaluate the operation
             (let ((result (evaluate-binary-op op left-val right-val)))
               (make-constant-node result (make-int-type) (ast-node-source-loc node))))))

      ;; Not both constants - return with folded children
      (t
       (make-ast-node :type 'binary-op
                      :value op
                      :children (list left right)
                      :source-loc (ast-node-source-loc node))))))

(defun evaluate-binary-op (op left-val right-val)
  "Evaluate a binary operation on constant values"
  (cond
    ;; Arithmetic
    ((string= op "+")
     (to-32bit-signed (+ left-val right-val)))
    ((string= op "-")
     (to-32bit-signed (- left-val right-val)))
    ((string= op "*")
     (to-32bit-signed (* left-val right-val)))
    ((string= op "/")
     (if (zerop right-val)
         0  ; Shouldn't happen, checked above
         (to-32bit-signed (truncate left-val right-val))))
    ((string= op "%")
     (if (zerop right-val)
         0  ; Shouldn't happen, checked above
         (to-32bit-signed (rem left-val right-val))))

    ;; Bitwise (use unsigned for bitwise ops)
    ((string= op "&")
     (to-32bit-signed (logand (to-32bit-unsigned left-val)
                              (to-32bit-unsigned right-val))))
    ((string= op "|")
     (to-32bit-signed (logior (to-32bit-unsigned left-val)
                              (to-32bit-unsigned right-val))))
    ((string= op "^")
     (to-32bit-signed (logxor (to-32bit-unsigned left-val)
                              (to-32bit-unsigned right-val))))
    ((string= op "<<")
     (to-32bit-signed (ash (to-32bit-unsigned left-val)
                           (min right-val 31))))
    ((string= op ">>")
     (to-32bit-signed (ash (to-32bit-unsigned left-val)
                           (- (min right-val 31)))))

    ;; Comparison (result is 0 or 1)
    ((string= op "==")
     (if (= left-val right-val) 1 0))
    ((string= op "!=")
     (if (/= left-val right-val) 1 0))
    ((string= op "<")
     (if (< left-val right-val) 1 0))
    ((string= op ">")
     (if (> left-val right-val) 1 0))
    ((string= op "<=")
     (if (<= left-val right-val) 1 0))
    ((string= op ">=")
     (if (>= left-val right-val) 1 0))

    ;; Logical (result is 0 or 1)
    ((string= op "&&")
     (if (and (not (zerop left-val)) (not (zerop right-val))) 1 0))
    ((string= op "||")
     (if (or (not (zerop left-val)) (not (zerop right-val))) 1 0))

    (t
     (error "Unknown binary operator in constant folding: ~a" op))))

;;; ===========================================================================
;;; Unary Operation Folding
;;; ===========================================================================

(defun fold-unary-op (node)
  "Fold unary operation if operand is constant"
  (let* ((op (ast-node-value node))
         (operand (fold-constants (first (ast-node-children node)))))

    (if (is-constant-node operand)
        (let ((val (get-constant-value operand)))
          (let ((result (evaluate-unary-op op val)))
            (if result
                (make-constant-node result (make-int-type) (ast-node-source-loc node))
                ;; Some unary ops can't be folded (e.g., address-of, dereference)
                (make-ast-node :type 'unary-op
                               :value op
                               :children (list operand)
                               :source-loc (ast-node-source-loc node)))))
        ;; Not a constant - return with folded operand
        (make-ast-node :type 'unary-op
                       :value op
                       :children (list operand)
                       :source-loc (ast-node-source-loc node)))))

(defun evaluate-unary-op (op val)
  "Evaluate a unary operation on a constant value.
   Returns nil if the operation cannot be folded."
  (cond
    ;; Negation
    ((string= op "-")
     (to-32bit-signed (- val)))

    ;; Bitwise NOT
    ((string= op "~")
     (to-32bit-signed (lognot (to-32bit-unsigned val))))

    ;; Logical NOT
    ((string= op "!")
     (if (zerop val) 1 0))

    ;; These cannot be folded
    ((string= op "*") nil)   ; dereference
    ((string= op "&") nil)   ; address-of
    ((string= op "++") nil)  ; pre-increment
    ((string= op "--") nil)  ; pre-decrement

    (t nil)))

;;; ===========================================================================
;;; Ternary/Cast/Sizeof Folding
;;; ===========================================================================

(defun fold-ternary (node)
  "Fold ternary conditional if condition is constant"
  (let ((condition (fold-constants (first (ast-node-children node))))
        (then-expr (fold-constants (second (ast-node-children node))))
        (else-expr (fold-constants (third (ast-node-children node)))))

    (if (is-constant-node condition)
        ;; Condition is constant - select the appropriate branch
        (if (not (zerop (get-constant-value condition)))
            then-expr
            else-expr)
        ;; Condition not constant - return with folded children
        (make-ast-node :type 'ternary
                       :children (list condition then-expr else-expr)
                       :source-loc (ast-node-source-loc node)))))

(defun fold-cast (node)
  "Fold cast of constant value"
  (let ((target-type (ast-node-value node))
        (expr (fold-constants (first (ast-node-children node)))))

    (if (is-constant-node expr)
        ;; Casting a constant - compute result at compile time
        (let ((val (get-constant-value expr)))
          ;; Apply type-specific masking
          (cond
            ;; Cast to char (1 byte)
            ((and (type-desc-p target-type)
                  (eq (type-desc-base target-type) 'char))
             (make-constant-node (logand val #xFF) target-type (ast-node-source-loc node)))
            ;; Cast to short (2 bytes)
            ((and (type-desc-p target-type)
                  (= (type-desc-size target-type) 2))
             (make-constant-node (logand val #xFFFF) target-type (ast-node-source-loc node)))
            ;; Cast to int (4 bytes) - value unchanged (already 32-bit)
            (t
             (make-constant-node val (or target-type (make-int-type)) (ast-node-source-loc node)))))
        ;; Not a constant - return with folded child
        (make-ast-node :type 'cast
                       :value target-type
                       :children (list expr)
                       :source-loc (ast-node-source-loc node)))))

(defun fold-sizeof (node)
  "Fold sizeof to a constant.
   Only folds when we can determine the size statically (type-desc or expression with result-type).
   For var-ref without type info, leaves it for code generation to handle."
  (let* ((operand (first (ast-node-children node))))
    (cond
      ;; sizeof(type) - type-desc passed directly
      ((type-desc-p operand)
       (make-constant-node (type-size operand) (make-int-type) (ast-node-source-loc node)))
      ;; sizeof(expression) with result-type set
      ((and (ast-node-p operand)
            (ast-node-result-type operand))
       (make-constant-node (type-size (ast-node-result-type operand))
                           (make-int-type) (ast-node-source-loc node)))
      ;; Can't determine size at compile time (e.g., var-ref) - leave for codegen
      (t node))))

;;; ===========================================================================
;;; Function Inlining
;;; ===========================================================================

;;; Parameters for inlining heuristics
(defparameter *inline-stmt-threshold* 10
  "Maximum number of statements for auto-inlining")
(defparameter *inline-max-depth* 3
  "Maximum inlining depth to prevent infinite expansion")

;;; Global state for inlining pass
(defvar *inline-counter* 0 "Counter for generating unique inline suffixes")
(defvar *recursive-functions* nil "Set of recursive function names")

(defun detect-recursive-functions (ast)
  "Detect which functions are recursive (call themselves directly or indirectly)"
  (let ((call-graph (make-hash-table :test 'equal))
        (recursive (make-hash-table :test 'equal)))
    ;; Build call graph: function -> list of functions it calls
    (dolist (child (ast-node-children ast))
      (when (and (ast-node-p child) (eq (ast-node-type child) 'function))
        (let ((name (ast-node-value child)))
          (setf (gethash name call-graph) (collect-called-functions child)))))
    ;; Find recursive functions using DFS
    (maphash (lambda (name callees)
               (declare (ignore callees))
               (when (can-reach name name call-graph (make-hash-table :test 'equal))
                 (setf (gethash name recursive) t)))
             call-graph)
    recursive))

(defun collect-called-functions (node)
  "Collect all function names called within an AST node"
  (let ((called nil))
    (labels ((walk (n)
               (when (and n (ast-node-p n))
                 (when (eq (ast-node-type n) 'call)
                   (let ((func-expr (first (ast-node-children n))))
                     (when (and (ast-node-p func-expr)
                                (eq (ast-node-type func-expr) 'var-ref))
                       (pushnew (ast-node-value func-expr) called :test #'string=))))
                 (dolist (child (ast-node-children n))
                   (walk child)))))
      (walk node))
    called))

(defun can-reach (start target call-graph visited)
  "Check if 'start' can reach 'target' in the call graph (detecting cycles)"
  (when (gethash start visited)
    (return-from can-reach nil))
  (setf (gethash start visited) t)
  (let ((callees (gethash start call-graph)))
    (or (member target callees :test #'string=)
        (some (lambda (callee)
                (can-reach callee target call-graph visited))
              callees))))

(defun can-inline-function (func-name func-node depth)
  "Check if a function can be inlined"
  (when (null func-node)
    (return-from can-inline-function nil))
  (let ((data (ast-node-data func-node)))
    (cond
      ;; Don't inline if depth limit exceeded
      ((> depth *inline-max-depth*) nil)
      ;; Don't inline recursive functions
      ((gethash func-name *recursive-functions*) nil)
      ;; Inline if explicitly marked
      ((getf data :inline-hint) t)
      ;; Auto-inline small functions when optimizing
      ((and (compiler-state-optimize *state*)
            (let ((stmt-count (or (getf data :stmt-count) 999)))
              (<= stmt-count *inline-stmt-threshold*)))
       t)
      (t nil))))

(defun gen-inline-suffix ()
  "Generate a unique suffix for inlined variable names"
  (format nil "_i~a" (incf *inline-counter*)))

(defun rename-variable (name suffix)
  "Rename a variable with an inline suffix"
  (format nil "~a~a" name suffix))

(defun rename-variables-in-node (node renames suffix)
  "Recursively rename variables in an AST node"
  (when (null node)
    (return-from rename-variables-in-node nil))
  (unless (ast-node-p node)
    (return-from rename-variables-in-node node))

  (case (ast-node-type node)
    ;; Variable reference - check if needs renaming
    (var-ref
     (let* ((name (ast-node-value node))
            (new-name (gethash name renames)))
       (if new-name
           (make-ast-node :type 'var-ref
                          :value new-name
                          :result-type (ast-node-result-type node)
                          :source-loc (ast-node-source-loc node))
           node)))

    ;; Variable declaration - rename and track
    (var-decl
     (let* ((name (ast-node-value node))
            (new-name (rename-variable name suffix)))
       (setf (gethash name renames) new-name)
       (make-ast-node :type 'var-decl
                      :value new-name
                      :children (mapcar (lambda (c) (rename-variables-in-node c renames suffix))
                                        (ast-node-children node))
                      :result-type (ast-node-result-type node)
                      :source-loc (ast-node-source-loc node)
                      :data (ast-node-data node))))

    ;; Other nodes - recursively process children
    (otherwise
     (make-ast-node :type (ast-node-type node)
                    :value (ast-node-value node)
                    :children (mapcar (lambda (c) (rename-variables-in-node c renames suffix))
                                      (ast-node-children node))
                    :result-type (ast-node-result-type node)
                    :source-loc (ast-node-source-loc node)
                    :data (ast-node-data node)))))

(defun transform-returns (node result-var exit-label)
  "Transform return statements to assignments + jumps for inlining"
  (when (null node)
    (return-from transform-returns nil))
  (unless (ast-node-p node)
    (return-from transform-returns node))

  (case (ast-node-type node)
    ;; Transform return into: result = expr; goto exit_label;
    (return
     (let ((return-expr (first (ast-node-children node))))
       (if return-expr
           ;; return expr; -> { result = expr; jump to exit; }
           ;; The assign must be wrapped in expr-stmt for generate-statement to handle it
           (make-ast-node :type 'block
                          :children (list
                                     (make-ast-node :type 'expr-stmt
                                                    :children (list
                                                               (make-ast-node :type 'assign
                                                                              :value "="
                                                                              :children (list
                                                                                         (make-ast-node :type 'var-ref
                                                                                                        :value result-var)
                                                                                         (transform-returns return-expr result-var exit-label)))))
                                     (make-ast-node :type 'inline-return-jump
                                                    :value exit-label))
                          :source-loc (ast-node-source-loc node))
           ;; return; -> jump to exit;
           (make-ast-node :type 'inline-return-jump
                          :value exit-label
                          :source-loc (ast-node-source-loc node)))))

    ;; Other nodes - recursively process children
    (otherwise
     (make-ast-node :type (ast-node-type node)
                    :value (ast-node-value node)
                    :children (mapcar (lambda (c) (transform-returns c result-var exit-label))
                                      (ast-node-children node))
                    :result-type (ast-node-result-type node)
                    :source-loc (ast-node-source-loc node)
                    :data (ast-node-data node)))))

(defun inline-call (call-node func-node depth)
  "Inline a function call, returning an inline-expr node"
  (let* ((suffix (gen-inline-suffix))
         (result-var (format nil "__inline_result~a" suffix))
         (exit-label (intern (format nil "INLINE_RET~a" suffix) :c-compiler))
         (args (rest (ast-node-children call-node)))
         (params-node (first (ast-node-children func-node)))
         (body (second (ast-node-children func-node)))
         (param-nodes (ast-node-children params-node))
         (renames (make-hash-table :test 'equal))
         (init-stmts nil))

    ;; Create parameter initializations
    (loop for param in param-nodes
          for arg in args
          for i from 0
          when (ast-node-value param) ; param might have no name in declaration
          do (let* ((param-name (ast-node-value param))
                    (temp-name (format nil "__inline_p~a~a" i suffix)))
               ;; Map parameter name to temp name
               (setf (gethash param-name renames) temp-name)
               ;; Create: type temp = arg;
               (push (make-ast-node :type 'var-decl
                                    :value temp-name
                                    :children (list (inline-functions-in-node arg (1+ depth)))
                                    :result-type (ast-node-result-type param))
                     init-stmts)))

    ;; Create result variable declaration
    (let ((result-decl (make-ast-node :type 'var-decl
                                      :value result-var
                                      :result-type (or (ast-node-result-type func-node)
                                                       (make-int-type)))))
      (push result-decl init-stmts))

    ;; Rename local variables in body to avoid conflicts
    (let* ((renamed-body (rename-variables-in-node body renames suffix))
           ;; Transform returns to assignments + jumps
           (transformed-body (transform-returns renamed-body result-var exit-label))
           ;; Further inline any calls in the body
           (inlined-body (inline-functions-in-node transformed-body (1+ depth))))

      ;; Create the inline expression node
      (make-ast-node :type 'inline-expr
                     :value result-var
                     :children (list
                                ;; Initialization block
                                (make-ast-node :type 'decl-list
                                               :children (nreverse init-stmts))
                                ;; Inlined body
                                inlined-body
                                ;; Exit label
                                (make-ast-node :type 'inline-return-label
                                               :value exit-label))
                     :result-type (ast-node-result-type func-node)
                     :source-loc (ast-node-source-loc call-node)))))

(defun inline-functions-in-node (node depth)
  "Recursively inline function calls in an AST node"
  (when (null node)
    (return-from inline-functions-in-node nil))
  (unless (ast-node-p node)
    (return-from inline-functions-in-node node))

  (case (ast-node-type node)
    ;; Function call - potentially inline
    (call
     (let* ((func-expr (first (ast-node-children node)))
            (func-name (when (and (ast-node-p func-expr)
                                  (eq (ast-node-type func-expr) 'var-ref))
                         (ast-node-value func-expr)))
            (func-node (when func-name
                         (gethash func-name (compiler-state-function-table *state*)))))
       (if (and func-node (can-inline-function func-name func-node depth))
           ;; Inline the call
           (inline-call node func-node depth)
           ;; Don't inline - just process arguments
           (make-ast-node :type 'call
                          :children (mapcar (lambda (c) (inline-functions-in-node c depth))
                                            (ast-node-children node))
                          :result-type (ast-node-result-type node)
                          :source-loc (ast-node-source-loc node)))))

    ;; Other nodes - recursively process children
    (otherwise
     (make-ast-node :type (ast-node-type node)
                    :value (ast-node-value node)
                    :children (mapcar (lambda (c) (inline-functions-in-node c depth))
                                      (ast-node-children node))
                    :result-type (ast-node-result-type node)
                    :source-loc (ast-node-source-loc node)
                    :data (ast-node-data node)))))

(defun inline-functions (ast)
  "Main entry point for function inlining optimization.
   Transforms the AST by inlining eligible function calls."
  (when (null ast)
    (return-from inline-functions nil))

  ;; Reset inline counter
  (setf *inline-counter* 0)

  ;; Detect recursive functions first
  (setf *recursive-functions* (detect-recursive-functions ast))

  ;; Process each top-level declaration
  (make-ast-node :type 'program
                 :children (mapcar (lambda (child)
                                     (if (and (ast-node-p child)
                                              (eq (ast-node-type child) 'function))
                                         ;; Inline calls within function bodies
                                         (let* ((name (ast-node-value child))
                                                (body (second (ast-node-children child)))
                                                (new-body (inline-functions-in-node body 0)))
                                           (make-ast-node :type 'function
                                                          :value name
                                                          :children (list (first (ast-node-children child))
                                                                          new-body)
                                                          :result-type (ast-node-result-type child)
                                                          :source-loc (ast-node-source-loc child)
                                                          :data (ast-node-data child)))
                                         child))
                                   (ast-node-children ast))
                 :source-loc (ast-node-source-loc ast)))
