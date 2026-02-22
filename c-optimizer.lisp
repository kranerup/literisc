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
                        (is-const (and var-type (type-desc-p var-type) (type-desc-const-p var-type)))
                        (is-volatile (and var-type (type-desc-p var-type) (type-desc-volatile-p var-type))))
                   ;; Only collect if it has an initializer, is not an array, and is not volatile
                   ;; Arrays cannot be constant-propagated since array names represent addresses
                   ;; Volatile variables must always be read from memory
                   (when (and init-val (integerp init-val)
                              (not is-volatile)
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
            (sym (lookup-symbol name))
            ;; Never fold volatile variables - they must always be read from memory
            (is-volatile (and sym
                              (sym-entry-type sym)
                              (type-desc-volatile-p (sym-entry-type sym)))))
       (cond
         ;; Never fold volatile variables
         (is-volatile node)
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

;;; ===========================================================================
;;; Loop Unrolling
;;; ===========================================================================

;;; Configuration parameters
(defparameter *unroll-max-iterations* 8
  "Maximum iterations for full loop unrolling")

(defparameter *unroll-max-body-stmts* 10
  "Maximum statements in loop body for unrolling (per iteration)")

(defun extract-loop-variable (init-node)
  "Extract loop variable name and initial value from init clause.
   Returns (values var-name init-value) or (values nil nil).
   Handles: decl-list > var-decl with constant initializer"
  (when (and (ast-node-p init-node)
             (eq (ast-node-type init-node) 'decl-list))
    (let ((var-decl (first (ast-node-children init-node))))
      (when (and (ast-node-p var-decl)
                 (eq (ast-node-type var-decl) 'var-decl))
        (let ((var-name (ast-node-value var-decl))
              (init-expr (first (ast-node-children var-decl))))
          (when (and var-name (is-constant-node init-expr))
            (values var-name (get-constant-value init-expr))))))))

(defun extract-loop-bound (condition-node var-name)
  "Extract loop bound and comparison operator from condition.
   Returns (values bound-value comparison-op) or (values nil nil).
   Handles: var < const, var <= const, var != const, var > const, var >= const"
  (when (and (ast-node-p condition-node)
             (eq (ast-node-type condition-node) 'binary-op))
    (let ((op (ast-node-value condition-node))
          (left (first (ast-node-children condition-node)))
          (right (second (ast-node-children condition-node))))
      (when (member op '("<" "<=" "!=" ">" ">=") :test #'string=)
        (cond
          ;; var OP const
          ((and (ast-node-p left)
                (eq (ast-node-type left) 'var-ref)
                (string= (ast-node-value left) var-name)
                (is-constant-node right))
           (values (get-constant-value right) op))
          ;; const OP var (reverse the comparison)
          ((and (is-constant-node left)
                (ast-node-p right)
                (eq (ast-node-type right) 'var-ref)
                (string= (ast-node-value right) var-name))
           (let ((reversed-op (cond ((string= op "<") ">")
                                    ((string= op "<=") ">=")
                                    ((string= op ">") "<")
                                    ((string= op ">=") "<=")
                                    (t op))))  ; != stays the same
             (values (get-constant-value left) reversed-op))))))))

(defun extract-loop-step (update-node var-name)
  "Extract step value from update expression.
   Returns step-value (positive for increment, negative for decrement) or nil.
   Handles: i++, ++i, i--, --i, i += const, i -= const"
  (when (ast-node-p update-node)
    (case (ast-node-type update-node)
      ;; Post-increment/decrement: i++, i--
      (post-op
       (let ((operand (first (ast-node-children update-node)))
             (op (ast-node-value update-node)))
         (when (and (ast-node-p operand)
                    (eq (ast-node-type operand) 'var-ref)
                    (string= (ast-node-value operand) var-name))
           (cond ((string= op "++") 1)
                 ((string= op "--") -1)))))
      ;; Pre-increment/decrement: ++i, --i
      (unary-op
       (let ((operand (first (ast-node-children update-node)))
             (op (ast-node-value update-node)))
         (when (and (ast-node-p operand)
                    (eq (ast-node-type operand) 'var-ref)
                    (string= (ast-node-value operand) var-name))
           (cond ((string= op "++") 1)
                 ((string= op "--") -1)))))
      ;; Compound assignment: i += const, i -= const
      (assign
       (let ((op (ast-node-value update-node))
             (lhs (first (ast-node-children update-node)))
             (rhs (second (ast-node-children update-node))))
         (when (and (ast-node-p lhs)
                    (eq (ast-node-type lhs) 'var-ref)
                    (string= (ast-node-value lhs) var-name)
                    (is-constant-node rhs))
           (let ((val (get-constant-value rhs)))
             (cond ((string= op "+=") val)
                   ((string= op "-=") (- val))))))))))

(defun loop-var-modified-in-body-p (body var-name)
  "Check if loop variable is assigned or has address taken in body."
  (when (ast-node-p body)
    (case (ast-node-type body)
      ;; Assignment to loop variable
      (assign
       (let ((lhs (first (ast-node-children body))))
         (or (and (ast-node-p lhs)
                  (eq (ast-node-type lhs) 'var-ref)
                  (string= (ast-node-value lhs) var-name))
             (some (lambda (c) (loop-var-modified-in-body-p c var-name))
                   (ast-node-children body)))))
      ;; Post-op on loop variable
      (post-op
       (let ((operand (first (ast-node-children body))))
         (or (and (ast-node-p operand)
                  (eq (ast-node-type operand) 'var-ref)
                  (string= (ast-node-value operand) var-name))
             (some (lambda (c) (loop-var-modified-in-body-p c var-name))
                   (ast-node-children body)))))
      ;; Pre-increment/decrement on loop variable
      (unary-op
       (let ((operand (first (ast-node-children body)))
             (op (ast-node-value body)))
         (or (and (member op '("++" "--") :test #'string=)
                  (ast-node-p operand)
                  (eq (ast-node-type operand) 'var-ref)
                  (string= (ast-node-value operand) var-name))
             ;; Address-of on loop variable
             (and (string= op "&")
                  (ast-node-p operand)
                  (eq (ast-node-type operand) 'var-ref)
                  (string= (ast-node-value operand) var-name))
             (some (lambda (c) (loop-var-modified-in-body-p c var-name))
                   (ast-node-children body)))))
      ;; Recurse into children
      (otherwise
       (some (lambda (c) (loop-var-modified-in-body-p c var-name))
             (ast-node-children body))))))

(defun body-has-control-flow-p (body)
  "Check if body contains break/continue/goto/return."
  (when (ast-node-p body)
    (case (ast-node-type body)
      ((break continue return goto) t)
      (otherwise
       (some #'body-has-control-flow-p (ast-node-children body))))))

(defun count-body-statements (body)
  "Count approximate number of statements in body."
  (if (not (ast-node-p body))
      0
      (case (ast-node-type body)
        (block
         (reduce #'+ (ast-node-children body)
                 :key #'count-body-statements :initial-value 0))
        ((expr-stmt decl-list return if while for do-while) 1)
        (otherwise
         (reduce #'+ (ast-node-children body)
                 :key #'count-body-statements :initial-value 0)))))

(defun compute-iteration-count (start end step comparison)
  "Calculate number of iterations for the loop.
   Returns iteration count or nil if cannot be determined."
  (when (and start end step (not (zerop step)))
    (cond
      ;; Ascending loop (step > 0)
      ((> step 0)
       (cond
         ((string= comparison "<")
          (if (< start end)
              (ceiling (- end start) step)
              0))
         ((string= comparison "<=")
          (if (<= start end)
              (1+ (floor (- end start) step))
              0))
         ((string= comparison "!=")
          (if (and (< start end) (zerop (mod (- end start) step)))
              (/ (- end start) step)
              nil))  ; May not terminate or complex
         (t nil)))
      ;; Descending loop (step < 0)
      ((< step 0)
       (let ((abs-step (- step)))
         (cond
           ((string= comparison ">")
            (if (> start end)
                (ceiling (- start end) abs-step)
                0))
           ((string= comparison ">=")
            (if (>= start end)
                (1+ (floor (- start end) abs-step))
                0))
           ((string= comparison "!=")
            (if (and (> start end) (zerop (mod (- start end) abs-step)))
                (/ (- start end) abs-step)
                nil))
           (t nil)))))))

(defun copy-ast (node)
  "Create a deep copy of an AST node."
  (if (not (ast-node-p node))
      node
      (make-ast-node :type (ast-node-type node)
                     :value (ast-node-value node)
                     :children (mapcar #'copy-ast (ast-node-children node))
                     :result-type (ast-node-result-type node)
                     :source-loc (ast-node-source-loc node)
                     :data (ast-node-data node))))

(defun substitute-loop-var (body var-name value)
  "Replace all references to var-name with constant value."
  (if (not (ast-node-p body))
      body
      (case (ast-node-type body)
        (var-ref
         (if (string= (ast-node-value body) var-name)
             (make-constant-node value
                                 (or (ast-node-result-type body) (make-int-type))
                                 (ast-node-source-loc body))
             body))
        (otherwise
         (make-ast-node :type (ast-node-type body)
                        :value (ast-node-value body)
                        :children (mapcar (lambda (c)
                                            (substitute-loop-var c var-name value))
                                          (ast-node-children body))
                        :result-type (ast-node-result-type body)
                        :source-loc (ast-node-source-loc body)
                        :data (ast-node-data body))))))

(defun unroll-for-loop (node)
  "Attempt to fully unroll a for loop.
   Returns the unrolled block or the original node if not unrollable."
  (let* ((children (ast-node-children node))
         (init (first children))
         (condition (second children))
         (update (third children))
         (body (fourth children)))

    ;; Extract loop parameters
    (multiple-value-bind (var-name start)
        (extract-loop-variable init)
      (unless var-name
        (return-from unroll-for-loop node))

      (multiple-value-bind (end comparison)
          (extract-loop-bound condition var-name)
        (unless end
          (return-from unroll-for-loop node))

        (let ((step (extract-loop-step update var-name)))
          (unless step
            (return-from unroll-for-loop node))

          ;; Check body constraints
          (when (loop-var-modified-in-body-p body var-name)
            (return-from unroll-for-loop node))

          (when (body-has-control-flow-p body)
            (return-from unroll-for-loop node))

          ;; Compute iteration count
          (let ((iterations (compute-iteration-count start end step comparison)))
            (unless iterations
              (return-from unroll-for-loop node))

            ;; Check size limits
            (when (> iterations *unroll-max-iterations*)
              (return-from unroll-for-loop node))

            (let ((body-stmts (count-body-statements body)))
              (when (> (* iterations body-stmts)
                       (* *unroll-max-iterations* *unroll-max-body-stmts*))
                (return-from unroll-for-loop node)))

            ;; Zero iterations - return empty block
            (when (zerop iterations)
              (return-from unroll-for-loop
                (make-ast-node :type 'block
                               :children nil
                               :source-loc (ast-node-source-loc node))))

            ;; Generate unrolled body
            (let ((unrolled-stmts
                    (loop for i from 0 below iterations
                          for val = (+ start (* i step))
                          collect (substitute-loop-var (copy-ast body) var-name val))))
              (make-ast-node :type 'block
                             :children unrolled-stmts
                             :source-loc (ast-node-source-loc node)))))))))

(defun unroll-loops-in-node (node)
  "Recursively process AST and unroll eligible for loops."
  (if (not (ast-node-p node))
      node
      (case (ast-node-type node)
        ;; For loop - attempt to unroll
        (for
         (let ((unrolled (unroll-for-loop node)))
           (if (eq unrolled node)
               ;; Not unrolled - still process children (body might have nested loops)
               (make-ast-node :type 'for
                              :children (mapcar #'unroll-loops-in-node
                                                (ast-node-children node))
                              :source-loc (ast-node-source-loc node))
               ;; Unrolled - process the result for nested loops
               (unroll-loops-in-node unrolled))))
        ;; Other nodes - recurse into children
        (otherwise
         (make-ast-node :type (ast-node-type node)
                        :value (ast-node-value node)
                        :children (mapcar #'unroll-loops-in-node
                                          (ast-node-children node))
                        :result-type (ast-node-result-type node)
                        :source-loc (ast-node-source-loc node)
                        :data (ast-node-data node))))))

(defun unroll-loops (ast)
  "Main entry point for loop unrolling optimization.
   Walks the AST and unrolls eligible for loops."
  (when (null ast)
    (return-from unroll-loops nil))
  (unroll-loops-in-node ast))

;;; ===========================================================================
;;; Common Subexpression Elimination (CSE)
;;; ===========================================================================
;;; Eliminates redundant computation of identical pure subexpressions within
;;; a function by introducing a temporary variable for the first occurrence
;;; and replacing subsequent occurrences with a reference to that variable.
;;;
;;; Algorithm (per block):
;;;   Pass 1 – Scan all statements tracking "available" expressions.
;;;            An expression becomes a candidate if seen 2+ times while
;;;            no operand variable has been written to between occurrences.
;;;            Control-flow statements conservatively clear the available set.
;;;   Pass 2 – Walk statements in order, inserting a temp-var decl just
;;;            before the first statement in which each candidate appears,
;;;            then replacing all occurrences (in that statement and later
;;;            ones) with a var-ref to the temp.
;;;
;;; Only binary-op expressions whose operands are purely literal/var-ref/
;;; eligible-binary-op (no calls, no volatile reads, no side effects) are
;;; considered.  Short-circuit operators (&& and ||) are excluded.

(defvar *cse-counter* 0
  "Counter for generating unique CSE temporary variable names.")

(defun cse-fresh-name ()
  "Generate a unique CSE temporary variable name."
  (format nil "_cse~a" (incf *cse-counter*)))

;;; --- Eligibility predicates ---

(defun cse-pure-p (node)
  "Return t if NODE is a pure expression: no side effects, no volatile reads.
   Pure nodes: literals, non-volatile var-refs, and binary-ops (non-short-
   circuit) whose operands are themselves pure."
  (when (and node (ast-node-p node))
    (case (ast-node-type node)
      (literal t)
      (var-ref
       (let ((type (ast-node-result-type node)))
         (not (and (type-desc-p type) (type-desc-volatile-p type)))))
      (binary-op
       (and (not (member (ast-node-value node) '("&&" "||") :test #'string=))
            (every #'cse-pure-p (ast-node-children node))))
      (otherwise nil))))

(defun cse-eligible-p (node)
  "Return t if NODE is a CSE-eligible expression.
   Must be a binary-op (not &&/||) with pure operands."
  (and (ast-node-p node)
       (eq (ast-node-type node) 'binary-op)
       (not (member (ast-node-value node) '("&&" "||") :test #'string=))
       (every #'cse-pure-p (ast-node-children node))))

;;; --- AST walkers ---

(defun cse-uses-var-p (node var-name)
  "Return t if NODE or any descendant is a var-ref to VAR-NAME."
  (and (ast-node-p node)
       (or (and (eq (ast-node-type node) 'var-ref)
                (equal (ast-node-value node) var-name))
           (some (lambda (c) (cse-uses-var-p c var-name))
                 (ast-node-children node)))))

(defun cse-collect-from-expr (expr result)
  "Collect all CSE-eligible subexpressions from EXPR into RESULT.
   Bottom-up order (innermost first), with duplicates (for counting)."
  (when (and expr (ast-node-p expr))
    (dolist (child (ast-node-children expr))
      (setf result (cse-collect-from-expr child result)))
    (when (cse-eligible-p expr)
      (push expr result)))
  result)

(defun cse-collect-stmt-exprs (stmt)
  "Return a list (with duplicates) of CSE-eligible subexpressions from the
   expression positions of STMT, or :all if STMT is a control-flow construct
   (signalling that the available-expression set should be cleared)."
  (when (and stmt (ast-node-p stmt))
    (case (ast-node-type stmt)
      (decl-list
       (let ((result nil))
         (dolist (child (ast-node-children stmt))
           (when (and (ast-node-p child) (eq (ast-node-type child) 'var-decl))
             (let ((init (first (ast-node-children child))))
               (when init
                 (setf result (cse-collect-from-expr init result))))))
         result))
      (expr-stmt
       (cse-collect-from-expr (first (ast-node-children stmt)) nil))
      (return
       (when (ast-node-children stmt)
         (cse-collect-from-expr (first (ast-node-children stmt)) nil)))
      ;; if / while / for / do-while / block / goto / … → conservative
      (otherwise :all))))

(defun cse-written-in-expr (node result)
  "Collect variables written by NODE (assignments and ++/--)."
  (when (and node (ast-node-p node))
    (case (ast-node-type node)
      (assign
       (let ((lhs (first (ast-node-children node))))
         (when (and (ast-node-p lhs) (eq (ast-node-type lhs) 'var-ref))
           (pushnew (ast-node-value lhs) result :test #'string=))
         (setf result (cse-written-in-expr (second (ast-node-children node)) result))))
      ((unary-op post-op)
       (when (member (ast-node-value node) '("++" "--") :test #'string=)
         (let ((op (first (ast-node-children node))))
           (when (and (ast-node-p op) (eq (ast-node-type op) 'var-ref))
             (pushnew (ast-node-value op) result :test #'string=))))
       (dolist (child (ast-node-children node))
         (setf result (cse-written-in-expr child result))))
      (otherwise
       (dolist (child (ast-node-children node))
         (setf result (cse-written-in-expr child result))))))
  result)

(defun cse-written-in-stmt (stmt)
  "Return the list of variable names written in STMT (not descending into
   nested control-flow blocks), or :all for control-flow statements."
  (when (and stmt (ast-node-p stmt))
    (case (ast-node-type stmt)
      (decl-list
       (loop for child in (ast-node-children stmt)
             when (and (ast-node-p child) (eq (ast-node-type child) 'var-decl))
             collect (ast-node-value child)))
      (expr-stmt
       (cse-written-in-expr (first (ast-node-children stmt)) nil))
      (return nil)
      (otherwise :all))))

(defun cse-expr-appears-in-p (expr node)
  "Return t if EXPR appears as a subtree anywhere within NODE."
  (when (ast-node-p node)
    (or (ast-node-equal expr node)
        (some (lambda (child) (cse-expr-appears-in-p expr child))
              (ast-node-children node)))))

(defun cse-invalidate-alist (alist written-vars)
  "Remove ALIST entries whose expression uses any variable in WRITTEN-VARS."
  (remove-if (lambda (entry)
               (some (lambda (v) (cse-uses-var-p (car entry) v))
                     written-vars))
             alist))

;;; --- Transformation ---

(defun cse-replace-in-node (node done)
  "Replace CSE-eligible expressions in NODE with var-refs from DONE alist.
   Checks the node itself first (larger expressions take priority over
   their subexpressions)."
  (when (null node) (return-from cse-replace-in-node nil))
  (unless (ast-node-p node) (return-from cse-replace-in-node node))
  (let ((entry (find node done :test #'ast-node-equal :key #'car)))
    (when entry
      (return-from cse-replace-in-node
        (make-ast-node :type 'var-ref
                       :value (cdr entry)
                       :result-type (ast-node-result-type node)
                       :source-loc (ast-node-source-loc node)))))
  (make-ast-node :type (ast-node-type node)
                 :value (ast-node-value node)
                 :children (mapcar (lambda (c) (cse-replace-in-node c done))
                                   (ast-node-children node))
                 :result-type (ast-node-result-type node)
                 :source-loc (ast-node-source-loc node)
                 :data (ast-node-data node)))

(defun cse-replace-in-stmt (stmt done)
  "Apply DONE CSE replacements to the expression positions of STMT.
   For control-flow statements, recursively applies CSE to sub-blocks
   (with a fresh analysis) rather than propagating DONE across branches."
  (when (null stmt) (return-from cse-replace-in-stmt nil))
  (unless (ast-node-p stmt) (return-from cse-replace-in-stmt stmt))
  (case (ast-node-type stmt)
    (decl-list
     (make-ast-node
      :type 'decl-list
      :children (mapcar (lambda (child)
                          (if (and (ast-node-p child)
                                   (eq (ast-node-type child) 'var-decl))
                              (let ((init (first (ast-node-children child))))
                                (if init
                                    (make-ast-node
                                     :type 'var-decl
                                     :value (ast-node-value child)
                                     :children (list (cse-replace-in-node init done))
                                     :result-type (ast-node-result-type child)
                                     :source-loc (ast-node-source-loc child)
                                     :data (ast-node-data child))
                                    child))
                              child))
                        (ast-node-children stmt))
      :source-loc (ast-node-source-loc stmt)))
    (expr-stmt
     (make-ast-node
      :type 'expr-stmt
      :children (list (cse-replace-in-node (first (ast-node-children stmt)) done))
      :source-loc (ast-node-source-loc stmt)))
    (return
     (if (ast-node-children stmt)
         (make-ast-node
          :type 'return
          :children (list (cse-replace-in-node (first (ast-node-children stmt)) done))
          :source-loc (ast-node-source-loc stmt))
         stmt))
    ;; Control-flow: recurse into sub-blocks with a fresh CSE analysis
    (otherwise (cse-node stmt))))

(defun cse-make-temp-decl (name expr)
  "Build a decl-list/var-decl node for a CSE temporary variable."
  (make-ast-node
   :type 'decl-list
   :children (list (make-ast-node
                    :type 'var-decl
                    :value name
                    :children (list expr)
                    :result-type (or (ast-node-result-type expr) (make-int-type))))))

;;; --- Block-level CSE ---

(defun cse-block (block)
  "Apply CSE within BLOCK (two-pass). Returns the transformed block."
  (let ((stmts (ast-node-children block)))
    (when (null stmts)
      (return-from cse-block block))

    ;; ---- Pass 1: identify CSE candidates ----
    ;; seen:       alist (expr . name) – eligible exprs seen exactly once so far
    ;; candidates: alist (expr . name) – exprs seen 2+ times (CSE opportunity)
    (let ((seen nil)
          (candidates nil))
      (dolist (stmt stmts)
        (let ((exprs (cse-collect-stmt-exprs stmt)))
          (if (eq exprs :all)
              (setf seen nil)          ; control flow: clear available set
              (dolist (expr exprs)
                (unless (find expr candidates :test #'ast-node-equal :key #'car)
                  (if (find expr seen :test #'ast-node-equal :key #'car)
                      ;; Seen before while still available → CSE candidate
                      (let ((entry (find expr seen :test #'ast-node-equal :key #'car)))
                        (push (cons expr (cdr entry)) candidates))
                      ;; First time → add to seen with a fresh temp name
                      (push (cons expr (cse-fresh-name)) seen))))))
        ;; Invalidate seen entries when variables are written
        (let ((written (cse-written-in-stmt stmt)))
          (unless (eq written :all)
            (setf seen (cse-invalidate-alist seen written)))))

      ;; No candidates → just recurse into sub-blocks and return
      (when (null candidates)
        (return-from cse-block
          (make-ast-node :type 'block
                         :value (ast-node-value block)
                         :children (mapcar #'cse-node stmts)
                         :result-type (ast-node-result-type block)
                         :source-loc (ast-node-source-loc block)
                         :data (ast-node-data block))))

      ;; ---- Pass 2: transform ----
      ;; not-yet: candidates whose temp-var decl has not been emitted yet
      ;; done:    candidates whose decl has been emitted (active for replacement)
      (let ((not-yet (copy-list candidates))
            (done    nil)
            (new-stmts nil))
        (dolist (stmt stmts)
          (let ((new-decls nil))
            ;; Find which not-yet candidates first appear in this statement
            (let ((newly-done nil))
              (dolist (entry not-yet)
                (when (cse-expr-appears-in-p (car entry) stmt)
                  ;; Register temp var in symbol table and note for emission
                  (let* ((expr (car entry))
                         (name (cdr entry))
                         (expr-type (or (ast-node-result-type expr) (make-int-type))))
                    (cse-alloc-temp name expr-type)
                    (push (cse-make-temp-decl name expr) new-decls)
                    (push entry done)
                    (push entry newly-done))))
              (setf not-yet (remove-if (lambda (e) (member e newly-done)) not-yet)))

            ;; Apply done replacements to this statement
            (let ((stmt* (cse-replace-in-stmt stmt done)))
              (dolist (decl (nreverse new-decls))
                (push decl new-stmts))
              (push stmt* new-stmts)))

          ;; Invalidate done and not-yet based on writes in original stmt
          (let ((written (cse-written-in-stmt stmt)))
            (if (eq written :all)
                (progn (setf not-yet nil) (setf done nil))
                (progn
                  (setf not-yet (cse-invalidate-alist not-yet written))
                  (setf done    (cse-invalidate-alist done    written))))))

        (make-ast-node :type 'block
                       :value (ast-node-value block)
                       :children (nreverse new-stmts)
                       :result-type (ast-node-result-type block)
                       :source-loc (ast-node-source-loc block)
                       :data (ast-node-data block))))))

(defun cse-node (node)
  "Recursively apply CSE to any block nodes within NODE."
  (when (null node) (return-from cse-node nil))
  (unless (ast-node-p node) (return-from cse-node node))
  (case (ast-node-type node)
    (block (cse-block node))
    (otherwise
     (make-ast-node :type (ast-node-type node)
                    :value (ast-node-value node)
                    :children (mapcar #'cse-node (ast-node-children node))
                    :result-type (ast-node-result-type node)
                    :source-loc (ast-node-source-loc node)
                    :data (ast-node-data node)))))

(defun cse-alloc-temp (name var-type)
  "Allocate a CSE temporary variable on the current function's stack.
   Extends compiler-state-local-offset and registers the symbol.
   Must be called while the function's context is active in *state*."
  (let* ((size (type-size var-type))
         (new-offset (- (compiler-state-local-offset *state*) size)))
    (setf (compiler-state-local-offset *state*) new-offset)
    (add-symbol name var-type :local new-offset)))

(defun eliminate-common-subexpressions (ast)
  "Apply CSE to all functions in the program AST.
   For each function, temporarily restores the function's stack context in
   *state* so that CSE temporary variables can be registered in the symbol
   table with correct offsets.  The function node's :frame-size is updated
   to account for any newly introduced temporaries."
  (when (null ast)
    (return-from eliminate-common-subexpressions nil))
  (make-ast-node
   :type 'program
   :children
   (mapcar
    (lambda (child)
      (if (not (and (ast-node-p child) (eq (ast-node-type child) 'function)))
          child
          (let* ((func-name  (ast-node-value child))
                 (func-data  (ast-node-data child))
                 (frame-size (or (getf func-data :frame-size) 0))
                 (params     (first  (ast-node-children child)))
                 (body       (second (ast-node-children child)))
                 ;; Save compiler state
                 (saved-func   (compiler-state-current-function *state*))
                 (saved-scope  (compiler-state-scope-level     *state*))
                 (saved-offset (compiler-state-local-offset    *state*)))
            ;; Install this function's stack context
            (setf (compiler-state-current-function *state*) func-name)
            (setf (compiler-state-scope-level      *state*) 1)
            (setf (compiler-state-local-offset     *state*) (- frame-size))
            (let ((new-body (cse-node body)))
              (let ((new-frame-size (- (compiler-state-local-offset *state*))))
                ;; Restore state
                (setf (compiler-state-current-function *state*) saved-func)
                (setf (compiler-state-scope-level      *state*) saved-scope)
                (setf (compiler-state-local-offset     *state*) saved-offset)
                ;; Return updated function node
                (make-ast-node
                 :type 'function
                 :value func-name
                 :children (list params new-body)
                 :result-type (ast-node-result-type child)
                 :source-loc (ast-node-source-loc child)
                 :data (if (= new-frame-size frame-size)
                           func-data
                           (let ((d (copy-list func-data)))
                             (setf (getf d :frame-size) new-frame-size)
                             d))))))))
    (ast-node-children ast))))

;;; ===========================================================================
;;; AST Query Utilities
;;; ===========================================================================
;;; These utilities support testing of AST-level optimizations (e.g. CSE) by
;;; allowing tests to inspect the structure of the optimized AST rather than
;;; relying on emulated program output or generated assembly.

(defun ast-node-equal (a b)
  "Return t if two AST nodes are structurally equal.
   Compares type, value, and children recursively.
   Ignores source-loc, result-type, and data fields so that hand-built
   template nodes can be matched against compiler-produced nodes."
  (and (ast-node-p a)
       (ast-node-p b)
       (eq (ast-node-type a) (ast-node-type b))
       (equal (ast-node-value a) (ast-node-value b))
       (= (length (ast-node-children a)) (length (ast-node-children b)))
       (every #'ast-node-equal (ast-node-children a) (ast-node-children b))))

(defun ast-count-if (node pred)
  "Count all AST nodes in the subtree rooted at NODE for which PRED returns true.
   NODE itself is included in the count."
  (if (or (null node) (not (ast-node-p node)))
      0
      (+ (if (funcall pred node) 1 0)
         (reduce #'+ (ast-node-children node)
                 :key (lambda (child) (ast-count-if child pred))
                 :initial-value 0))))

(defun ast-count-subtree (node template)
  "Count occurrences of TEMPLATE as a subtree anywhere within NODE.
   TEMPLATE is an ast-node; matching uses ast-node-equal (ignores
   source-loc, result-type, and data)."
  (ast-count-if node (lambda (n) (ast-node-equal n template))))

(defun ast-count-var-refs (node name)
  "Count var-ref nodes referencing NAME anywhere within NODE."
  (ast-count-if node (lambda (n)
                       (and (ast-node-p n)
                            (eq (ast-node-type n) 'var-ref)
                            (equal (ast-node-value n) name)))))

(defun ast-find-function (ast name)
  "Find the function node named NAME in the program AST.
   Returns the function node, or nil if not found."
  (when (and ast (ast-node-p ast))
    (if (and (eq (ast-node-type ast) 'function)
             (equal (ast-node-value ast) name))
        ast
        (some (lambda (child) (ast-find-function child name))
              (ast-node-children ast)))))
