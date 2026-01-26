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
    (var-ref node)
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
  "Fold sizeof to a constant"
  (let* ((operand (first (ast-node-children node)))
         (size (cond
                 ;; sizeof(type) - type-desc passed directly
                 ((type-desc-p operand)
                  (type-size operand))
                 ;; sizeof(expression) with result-type
                 ((and (ast-node-p operand)
                       (ast-node-result-type operand))
                  (type-size (ast-node-result-type operand)))
                 ;; Default to int size
                 (t 4))))
    (make-constant-node size (make-int-type) (ast-node-source-loc node))))
