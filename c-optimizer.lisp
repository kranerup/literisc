;;; ===========================================================================
;;; ==================== C Optimizer for liteRISC ============================
;;; ===========================================================================
;;; Optimization passes for the C compiler AST

(in-package :c-compiler)

;;; ===========================================================================
;;; Global Constant Propagation
;;; ===========================================================================

(defvar *constant-globals* nil
  "Hash table mapping global variable names to their constant values.
   Used for propagating constants during fold-constants.")

(defvar *local-constants* nil
  "Hash table mapping local variable names to their constant values.
   Used for propagating local constants within a function.")

(defun collect-assignment-targets (node targets)
  "Collect all variable names that are assigned to in the AST.
   Populates the TARGETS hash table with variable names."
  (when (and node (ast-node-p node))
    (case (ast-node-type node)
      ;; Direct assignment
      (assign
       (let ((lhs (first (ast-node-children node))))
         (when (and (ast-node-p lhs) (eq (ast-node-type lhs) 'var-ref))
           (setf (gethash (ast-node-value lhs) targets) t)))
       ;; Also check children for nested assignments
       (dolist (child (ast-node-children node))
         (collect-assignment-targets child targets)))
      ;; Pre/post increment/decrement
      ((unary-op post-op)
       (when (member (ast-node-value node) '("++" "--") :test #'string=)
         (let ((operand (first (ast-node-children node))))
           (when (and (ast-node-p operand) (eq (ast-node-type operand) 'var-ref))
             (setf (gethash (ast-node-value operand) targets) t))))
       (dolist (child (ast-node-children node))
         (collect-assignment-targets child targets)))
      ;; Recurse into all other nodes
      (otherwise
       (dolist (child (ast-node-children node))
         (collect-assignment-targets child targets))))))

(defun collect-constant-globals (ast)
  "Analyze AST to find global variables that can be constant-propagated.
   Returns a hash table mapping global names to their constant values.
   A global can be propagated if:
   1. It's declared const AND has an initializer, OR
   2. It's initialized with a constant AND never assigned to"
  (let ((globals (make-hash-table :test 'equal))       ; name -> (init-value . const-p)
        (targets (make-hash-table :test 'equal))       ; assigned-to variables
        (result (make-hash-table :test 'equal)))       ; propagatable name -> value
    ;; First pass: collect global variable info
    ;; Handle both direct global-var nodes and those inside decl-list
    (labels ((process-global (node)
               (when (and (ast-node-p node) (eq (ast-node-type node) 'global-var))
                 (let* ((name (ast-node-value node))
                        (init-val (ast-node-data node))
                        (var-type (ast-node-result-type node))
                        (is-const (and var-type (type-desc-p var-type) (type-desc-const-p var-type))))
                   ;; Only collect if it has an initializer and is not an array
                   ;; Arrays cannot be constant-propagated since array names represent addresses
                   (when (and init-val (integerp init-val)
                              (not (and var-type (type-desc-array-size var-type))))
                     (setf (gethash name globals) (cons init-val is-const)))))))
      (dolist (child (ast-node-children ast))
        (when (ast-node-p child)
          (case (ast-node-type child)
            (global-var (process-global child))
            (decl-list (dolist (decl (ast-node-children child))
                         (process-global decl)))))))
    ;; Second pass: collect all assignment targets
    (dolist (child (ast-node-children ast))
      (collect-assignment-targets child targets))
    ;; Build result: globals that are const OR not assigned to
    (maphash (lambda (name val-and-const)
               (let ((init-val (car val-and-const))
                     (is-const (cdr val-and-const)))
                 (when (or is-const
                           (not (gethash name targets)))
                   (setf (gethash name result) init-val))))
             globals)
    result))

(defun collect-local-constants (body &optional param-names)
  "Analyze function body to find local variables that can be constant-propagated.
   PARAM-NAMES is a list of parameter names that should not be shadowed.
   Returns a hash table mapping local variable names to their constant values.
   A local can be propagated if:
   1. It's initialized with a constant expression, AND
   2. It's never assigned to after initialization, AND
   3. Its address is never taken (can't do &x on a propagated constant), AND
   4. It's not shadowed (no other declaration with the same name in scope), AND
   5. It doesn't shadow a parameter"
  (let ((locals (make-hash-table :test 'equal))        ; name -> init-value (after folding)
        (shadowed (make-hash-table :test 'equal))      ; names that appear multiple times
        (targets (make-hash-table :test 'equal))       ; assigned-to variables
        (result (make-hash-table :test 'equal)))       ; propagatable name -> value
    ;; Mark parameter names as already seen (locals with same name shadow them)
    (dolist (pname param-names)
      (when pname
        (setf (gethash pname locals) :parameter)))
    ;; First pass: collect assignment targets
    (collect-assignment-targets body targets)
    ;; Second pass: collect var-decls with constant initializers
    ;; Also detect shadowed names (declared more than once, or shadows parameter)
    (labels ((process-node (node)
               (when (and node (ast-node-p node))
                 (case (ast-node-type node)
                   (var-decl
                    (let* ((name (ast-node-value node))
                           (init-expr (first (ast-node-children node))))
                      ;; Check if this name was already seen (shadowing param or another local)
                      (if (gethash name locals)
                          (setf (gethash name shadowed) t)
                          ;; First occurrence - check if initializer is a constant
                          (when (and init-expr (is-constant-node init-expr))
                            (setf (gethash name locals) (get-constant-value init-expr)))))
                    ;; Also process children (initializer might have nested decls)
                    (dolist (child (ast-node-children node))
                      (process-node child)))
                   (otherwise
                    (dolist (child (ast-node-children node))
                      (process-node child)))))))
      (process-node body))
    ;; Build result: locals with constant init AND not assigned to AND not address-taken AND not shadowed
    (maphash (lambda (name init-val)
               (unless (or (eq init-val :parameter)  ; Skip parameter placeholders
                           (gethash name targets)
                           (gethash name shadowed)
                           (is-address-taken name))
                 (setf (gethash name result) init-val)))
             locals)
    result))

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
     ;; Collect constant-propagatable globals before processing
     (let ((*constant-globals* (collect-constant-globals node)))
       (make-ast-node :type 'program
                      :children (mapcar #'fold-constants (ast-node-children node))
                      :source-loc (ast-node-source-loc node))))

    (function
     ;; Iteratively fold and propagate local constants until fixed point
     (let* ((params-node (first (ast-node-children node)))
            (body-node (second (ast-node-children node)))
            (folded-params (fold-constants params-node))
            (current-body (fold-constants body-node))
            ;; Extract parameter names to avoid propagating locals that shadow them
            (param-names (mapcar (lambda (p)
                                   (when (and (ast-node-p p) (eq (ast-node-type p) 'param))
                                     (ast-node-value p)))
                                 (ast-node-children params-node)))
            (prev-const-count 0)
            (max-iterations 10))  ; Safety limit
       ;; Iterate: collect local constants, fold with them, repeat until stable
       (loop for iteration from 1 to max-iterations
             do (let* ((local-consts (collect-local-constants current-body param-names))
                       (const-count (hash-table-count local-consts)))
                  ;; Stop if no new constants found
                  (when (<= const-count prev-const-count)
                    (return))
                  (setf prev-const-count const-count)
                  ;; Fold with local constants bound
                  (let ((*local-constants* local-consts))
                    (setf current-body (fold-constants current-body)))))
       (make-ast-node :type 'function
                      :value (ast-node-value node)
                      :children (list folded-params current-body)
                      :result-type (ast-node-result-type node)
                      :source-loc (ast-node-source-loc node)
                      :data (ast-node-data node))))

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
     ;; Check if var-ref can be folded to a constant
     (let* ((name (ast-node-value node))
            (sym (lookup-symbol name)))
       (cond
         ;; Fold enum constant to literal
         ((and sym (eq (sym-entry-storage sym) :enum-constant))
          (make-constant-node (sym-entry-offset sym)
                              (make-int-type)
                              (ast-node-source-loc node)))
         ;; Fold constant global to literal
         ((and *constant-globals* (gethash name *constant-globals*))
          (make-constant-node (gethash name *constant-globals*)
                              (make-int-type)
                              (ast-node-source-loc node)))
         ;; Fold constant local to literal
         ((and *local-constants* (gethash name *local-constants*))
          (make-constant-node (gethash name *local-constants*)
                              (make-int-type)
                              (ast-node-source-loc node)))
         ;; Not a constant - keep as var-ref
         (t node))))
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

(defun substitute-constants-in-node (node const-bindings)
  "Substitute constant values for variable references in an AST node.
   const-bindings is a hash table mapping variable names to constant values."
  (when (null node)
    (return-from substitute-constants-in-node nil))
  (unless (ast-node-p node)
    (return-from substitute-constants-in-node node))

  (case (ast-node-type node)
    ;; Variable reference - check if it's a constant
    (var-ref
     (let ((const-val (gethash (ast-node-value node) const-bindings)))
       (if const-val
           ;; Replace with constant literal
           (make-constant-node const-val
                               (or (ast-node-result-type node) (make-int-type))
                               (ast-node-source-loc node))
           node)))

    ;; Recursively process children for other nodes
    (otherwise
     (make-ast-node :type (ast-node-type node)
                    :value (ast-node-value node)
                    :children (mapcar (lambda (c) (substitute-constants-in-node c const-bindings))
                                      (ast-node-children node))
                    :result-type (ast-node-result-type node)
                    :source-loc (ast-node-source-loc node)
                    :data (ast-node-data node)))))

(defun extract-simple-inline-expr (body result-var)
  "Check if body is a simple { result = expr; goto exit; } pattern.
   If so, return the expression. Otherwise return nil.
   Handles the case where the function body is a block containing
   the transformed return block."
  (when (and (ast-node-p body) (eq (ast-node-type body) 'block))
    ;; The function body is a block. Check if it contains exactly one statement
    ;; which is the transformed return (another block with assign + jump)
    (let ((children (ast-node-children body)))
      (cond
        ;; Case 1: block with one child that is the transformed return block
        ((and (= (length children) 1)
              (ast-node-p (first children))
              (eq (ast-node-type (first children)) 'block))
         (extract-simple-inline-expr (first children) result-var))
        ;; Case 2: block with expr-stmt + inline-return-jump directly
        ((= (length children) 2)
         (let ((first-stmt (first children))
               (second-stmt (second children)))
           (when (and (ast-node-p first-stmt)
                      (eq (ast-node-type first-stmt) 'expr-stmt)
                      (ast-node-p second-stmt)
                      (eq (ast-node-type second-stmt) 'inline-return-jump))
             (let ((assign (first (ast-node-children first-stmt))))
               (when (and (ast-node-p assign)
                          (eq (ast-node-type assign) 'assign)
                          (string= (ast-node-value assign) "="))
                 (let ((lhs (first (ast-node-children assign)))
                       (rhs (second (ast-node-children assign))))
                   ;; LHS should be var-ref to result-var
                   (when (and (ast-node-p lhs)
                              (eq (ast-node-type lhs) 'var-ref)
                              (string= (ast-node-value lhs) result-var))
                     ;; Return the RHS expression
                     rhs)))))))
        (t nil)))))

(defun inline-call (call-node func-node depth)
  "Inline a function call, returning an inline-expr node.
   Constant arguments are propagated directly into the body.
   Simple inlines (just returning an expression) are optimized to
   avoid stack allocation."
  (let* ((suffix (gen-inline-suffix))
         (result-var (format nil "__inline_result~a" suffix))
         (exit-label (intern (format nil "INLINE_RET~a" suffix) :c-compiler))
         (args (rest (ast-node-children call-node)))
         (params-node (first (ast-node-children func-node)))
         (body (second (ast-node-children func-node)))
         (param-nodes (ast-node-children params-node))
         (renames (make-hash-table :test 'equal))
         (const-bindings (make-hash-table :test 'equal))
         (init-stmts nil)
         (has-param-temps nil))  ; Track if we need parameter temp vars

    ;; Process parameters - constant args get propagated, var-refs get renamed,
    ;; complex expressions get temp vars
    (loop for param in param-nodes
          for arg in args
          for i from 0
          when (ast-node-value param)
          do (let ((param-name (ast-node-value param)))
               (cond
                 ;; Constant argument - propagate directly
                 ((is-constant-node arg)
                  (setf (gethash param-name const-bindings) (get-constant-value arg)))
                 ;; Simple var-ref argument - rename parameter to argument variable
                 ;; This avoids creating unnecessary temp variables for simple cases
                 ((and (ast-node-p arg)
                       (eq (ast-node-type arg) 'var-ref)
                       (ast-node-value arg))
                  (setf (gethash param-name renames) (ast-node-value arg)))
                 ;; Complex expression - create temp variable to evaluate once
                 (t
                  (let ((temp-name (format nil "__inline_p~a~a" i suffix)))
                    (setf has-param-temps t)
                    (setf (gethash param-name renames) temp-name)
                    (push (make-ast-node :type 'var-decl
                                         :value temp-name
                                         :children (list (inline-functions-in-node arg (1+ depth)))
                                         :result-type (ast-node-result-type param))
                          init-stmts))))))

    ;; Process the body:
    ;; 1. Substitute constants for constant parameters
    ;; 2. Rename remaining variables
    ;; 3. Transform returns
    ;; 4. Inline nested calls
    ;; 5. Fold constants in the result
    (let* ((const-subst-body (substitute-constants-in-node body const-bindings))
           (renamed-body (rename-variables-in-node const-subst-body renames suffix))
           (transformed-body (transform-returns renamed-body result-var exit-label))
           (inlined-body (inline-functions-in-node transformed-body (1+ depth)))
           ;; Run constant folding on the inlined body
           (folded-body (fold-constants inlined-body)))

      ;; Try to simplify trivial inline expressions
      ;; If no param temps needed and body is just { result = expr; goto exit; }
      ;; we can return just the expression without stack allocation
      (when (not has-param-temps)
        (let ((simple-expr (extract-simple-inline-expr folded-body result-var)))
          (when simple-expr
            (return-from inline-call simple-expr))))

      ;; Create result variable declaration (only for non-simple cases)
      (let ((result-decl (make-ast-node :type 'var-decl
                                        :value result-var
                                        :result-type (or (ast-node-result-type func-node)
                                                         (make-int-type)))))
        (push result-decl init-stmts))

      ;; Create the inline expression node
      (make-ast-node :type 'inline-expr
                     :value result-var
                     :children (list
                                ;; Initialization block
                                (make-ast-node :type 'decl-list
                                               :children (nreverse init-stmts))
                                ;; Inlined body (with constants folded)
                                folded-body
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

(defun collect-remaining-calls (node)
  "Collect all function names that are still called (not inlined) in the AST.
   Returns a hash table of called function names."
  (let ((called (make-hash-table :test 'equal)))
    (labels ((walk (n)
               (when (and n (ast-node-p n))
                 (when (eq (ast-node-type n) 'call)
                   ;; Found a call node - extract function name
                   (let ((func-expr (first (ast-node-children n))))
                     (when (and (ast-node-p func-expr)
                                (eq (ast-node-type func-expr) 'var-ref))
                       (setf (gethash (ast-node-value func-expr) called) t))))
                 ;; Recurse into children
                 (dolist (child (ast-node-children n))
                   (walk child)))))
      (walk node))
    called))

(defun inline-functions (ast)
  "Main entry point for function inlining optimization.
   Transforms the AST by inlining eligible function calls.
   Also marks fully-inlined functions as dead for elimination."
  (when (null ast)
    (return-from inline-functions nil))

  ;; Reset inline counter
  (setf *inline-counter* 0)

  ;; Detect recursive functions first
  (setf *recursive-functions* (detect-recursive-functions ast))

  ;; Collect all defined function names (except main)
  (let ((defined-functions (make-hash-table :test 'equal)))
    (dolist (child (ast-node-children ast))
      (when (and (ast-node-p child)
                 (eq (ast-node-type child) 'function))
        (let ((name (ast-node-value child)))
          (unless (string= name "main")
            (setf (gethash name defined-functions) t)))))

    ;; Process each top-level declaration (inline calls)
    (let ((result-ast
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
                           :source-loc (ast-node-source-loc ast))))

      ;; After inlining, find which functions are still called
      (let ((remaining-calls (collect-remaining-calls result-ast)))
        ;; Mark functions with no remaining calls as dead
        (maphash (lambda (func-name defined-p)
                   (declare (ignore defined-p))
                   (unless (gethash func-name remaining-calls)
                     (setf (gethash func-name (compiler-state-dead-functions *state*)) t)))
                 defined-functions))

      result-ast)))

;;; ===========================================================================
;;; Dead Code Elimination
;;; ===========================================================================

(defun collect-used-variables (node used)
  "Collect all variable names that are actually read in the AST.
   Populates the USED hash table with variable names."
  (when (and node (ast-node-p node))
    (case (ast-node-type node)
      ;; Variable reference - this variable is used
      (var-ref
       (setf (gethash (ast-node-value node) used) t))
      ;; Assignment - only the RHS counts as a read, LHS is a write
      (assign
       (let ((lhs (first (ast-node-children node)))
             (rhs (second (ast-node-children node))))
         ;; Don't count simple var-ref on LHS as a use
         ;; But do collect uses from subscript expressions on LHS
         (when (and (ast-node-p lhs)
                    (not (eq (ast-node-type lhs) 'var-ref)))
           (collect-used-variables lhs used))
         (collect-used-variables rhs used)))
      ;; For other nodes, recurse into children
      (otherwise
       (dolist (child (ast-node-children node))
         (collect-used-variables child used))))))

(defun has-side-effects (node)
  "Check if an expression has side effects (function calls, assignments, etc.)"
  (when (and node (ast-node-p node))
    (case (ast-node-type node)
      ;; These have side effects
      ((call assign post-op) t)
      ;; Pre-increment/decrement have side effects
      (unary-op
       (member (ast-node-value node) '("++" "--") :test #'string=))
      ;; Check children for side effects
      (otherwise
       (some #'has-side-effects (ast-node-children node))))))

(defun eliminate-dead-code (node used)
  "Remove dead variable declarations from the AST.
   USED is a hash table of variable names that are actually read."
  (when (null node)
    (return-from eliminate-dead-code nil))
  (unless (ast-node-p node)
    (return-from eliminate-dead-code node))

  (case (ast-node-type node)
    ;; For blocks and decl-lists, filter out dead declarations
    ((block decl-list)
     (let ((live-children
             (loop for child in (ast-node-children node)
                   ;; Keep non-var-decl nodes
                   when (or (not (ast-node-p child))
                            (not (eq (ast-node-type child) 'decl-list))
                            ;; For decl-list, check if var-decl inside is used
                            (let ((inner (first (ast-node-children child))))
                              (or (not (ast-node-p inner))
                                  (not (eq (ast-node-type inner) 'var-decl))
                                  ;; Keep if variable is used
                                  (gethash (ast-node-value inner) used)
                                  ;; Keep if has address taken
                                  (is-address-taken (ast-node-value inner))
                                  ;; Keep if initializer has side effects
                                  (has-side-effects (first (ast-node-children inner))))))
                   collect (eliminate-dead-code child used))))
       (make-ast-node :type (ast-node-type node)
                      :value (ast-node-value node)
                      :children live-children
                      :result-type (ast-node-result-type node)
                      :source-loc (ast-node-source-loc node))))

    ;; Recursively process other nodes
    (otherwise
     (make-ast-node :type (ast-node-type node)
                    :value (ast-node-value node)
                    :children (mapcar (lambda (c) (eliminate-dead-code c used))
                                      (ast-node-children node))
                    :result-type (ast-node-result-type node)
                    :source-loc (ast-node-source-loc node)
                    :data (ast-node-data node)))))

(defun dead-code-elimination (ast)
  "Main entry point for dead code elimination.
   Removes unused variable declarations whose initializers have no side effects."
  (when (null ast)
    (return-from dead-code-elimination nil))

  ;; Process each function separately
  (make-ast-node :type 'program
                 :children (mapcar (lambda (child)
                                     (if (and (ast-node-p child)
                                              (eq (ast-node-type child) 'function))
                                         ;; Collect used variables and eliminate dead code
                                         (let ((used (make-hash-table :test 'equal)))
                                           (collect-used-variables child used)
                                           (eliminate-dead-code child used))
                                         ;; Non-function nodes pass through
                                         child))
                                   (ast-node-children ast))
                 :source-loc (ast-node-source-loc ast)))
