;;; ===========================================================================
;;; ========================= C Parser for liteRISC ===========================
;;; ===========================================================================
;;; Recursive descent parser for C subset

(in-package :c-compiler)

;;; ===========================================================================
;;; Parser Utilities
;;; ===========================================================================

(defun current-token ()
  "Return the current token"
  (let ((pos (compiler-state-token-pos *state*))
        (tokens (compiler-state-tokens *state*)))
    (if (< pos (length tokens))
        (nth pos tokens)
        nil)))

(defun peek-token (&optional (offset 0))
  "Peek at token at current position + offset"
  (let ((pos (+ (compiler-state-token-pos *state*) offset))
        (tokens (compiler-state-tokens *state*)))
    (if (< pos (length tokens))
        (nth pos tokens)
        nil)))

(defun advance-token ()
  "Consume current token and return it"
  (let ((tok (current-token)))
    (incf (compiler-state-token-pos *state*))
    tok))

(defun at-end ()
  "Check if we've consumed all tokens"
  (null (current-token)))

(defun check-token (type &optional value)
  "Check if current token matches type and optionally value"
  (let ((tok (current-token)))
    (and tok
         (eq (token-type tok) type)
         (or (null value)
             (equal (token-value tok) value)))))

(defun expect-token (type &optional value)
  "Consume token if it matches, error otherwise"
  (if (check-token type value)
      (advance-token)
      (let ((tok (current-token)))
        (compiler-error "Expected ~a~a but got ~a at line ~a"
                        type
                        (if value (format nil " '~a'" value) "")
                        (if tok (token-value tok) "end of file")
                        (if tok (token-line tok) "?")))))

(defun match-token (type &optional value)
  "If current token matches, consume and return t, else nil"
  (when (check-token type value)
    (advance-token)
    t))

;;; ===========================================================================
;;; Number Token Helpers
;;; ===========================================================================

(defun number-token-value (tok)
  "Extract the integer value from a number token.
   Number tokens store (value . suffix) in their value field."
  (let ((val (token-value tok)))
    (if (consp val) (car val) val)))

(defun number-token-suffix (tok)
  "Extract the suffix from a number token.
   Returns nil, :u, :l, :ul, :lu, :ll, :ull, or :llu."
  (let ((val (token-value tok)))
    (if (consp val) (cdr val) nil)))

(defun suffix-to-type (suffix)
  "Convert a number suffix to the appropriate type descriptor.
   nil -> signed int
   :u -> unsigned int
   :l, :ll -> signed long (32-bit on this platform)
   :ul, :lu, :ull, :llu -> unsigned long"
  (case suffix
    ((nil) (make-int-type nil))
    ((:u) (make-int-type t))
    ((:l :ll) (make-long-type nil))
    ((:ul :lu :ull :llu) (make-long-type t))
    (otherwise (make-int-type nil))))

;;; ===========================================================================
;;; AST Construction Helpers
;;; ===========================================================================

(defun make-node (type &key value children result-type data)
  "Create an AST node"
  (let ((tok (current-token)))
    (make-ast-node :type type
                   :value value
                   :children children
                   :result-type result-type
                   :source-loc (when tok
                                 (cons (token-line tok)
                                       (token-column tok)))
                   :data data)))

(defun print-ast (node &optional (indent 0))
  "Print AST for debugging"
  (when node
    (format t "~v@T~a" indent (ast-node-type node))
    (when (ast-node-value node)
      (format t " : ~a" (ast-node-value node)))
    (when (ast-node-result-type node)
      (format t " [~a]" (type-desc-base (ast-node-result-type node))))
    (format t "~%")
    (dolist (child (ast-node-children node))
      (print-ast child (+ indent 2)))))

;;; ===========================================================================
;;; Address-Taken Analysis
;;; ===========================================================================

(defun is-operand-token (tok)
  "Check if token is an operand (can be followed by binary operator)"
  (when tok
    (or (eq (token-type tok) 'identifier)
        (eq (token-type tok) 'number)
        (and (eq (token-type tok) 'punctuation)
             (string= (token-value tok) ")")))))

(defun scan-address-taken-variables ()
  "Pre-scan tokens to find variables that have their address taken with &.
   This is needed to prevent register allocation for such variables.
   We conservatively mark any &identifier as address-taken, even if it might
   be binary AND, since the cost of missing a register allocation is low
   but the cost of incorrectly register-allocating an addressed variable is a bug."
  (let ((tokens (compiler-state-tokens *state*))
        (addr-taken (compiler-state-address-taken *state*)))
    (loop for i from 0 below (1- (length tokens))
          for tok = (nth i tokens)
          for next-tok = (nth (1+ i) tokens)
          when (and (eq (token-type tok) 'operator)
                    (string= (token-value tok) "&")
                    (eq (token-type next-tok) 'identifier))
          do (setf (gethash (token-value next-tok) addr-taken) t))))

(defun is-address-taken (name)
  "Check if a variable has its address taken"
  (gethash name (compiler-state-address-taken *state*)))

;;; ===========================================================================
;;; Constant Expression Evaluation (for enum initializers)
;;; ===========================================================================

(defun evaluate-constant-expression (expr)
  "Evaluate a constant expression at parse time. Returns the integer value or nil if not constant."
  (when (null expr)
    (return-from evaluate-constant-expression nil))
  (unless (ast-node-p expr)
    (return-from evaluate-constant-expression nil))

  (case (ast-node-type expr)
    ;; Literal number - direct value
    (literal
     (let ((val (ast-node-value expr)))
       (when (integerp val)
         val)))

    ;; Variable reference - might be an enum constant or function
    (var-ref
     (let* ((name (ast-node-value expr))
            (sym (lookup-symbol name)))
       (when sym
         (cond
           ;; Enum constant - return the value
           ((eq (sym-entry-storage sym) :enum-constant)
            (sym-entry-offset sym))
           ;; Function reference - return the label
           ((eq (sym-entry-storage sym) :function)
            (list :label (sym-entry-offset sym)))
           ;; Global variable - return the label (for function pointers initialized to globals)
           ((eq (sym-entry-storage sym) :global)
            (list :label (make-c-label name)))))))

    ;; Unary operators
    (unary-op
     (let* ((op (ast-node-value expr))
            (operand (first (ast-node-children expr))))
       ;; Address-of operator with global variable or function
       (when (string= op "&")
         (when (and (ast-node-p operand)
                    (eq (ast-node-type operand) 'var-ref))
           (let* ((name (ast-node-value operand))
                  (sym (lookup-symbol name)))
             (when sym
               (cond
                 ((eq (sym-entry-storage sym) :global)
                  (return-from evaluate-constant-expression
                    (list :label (make-c-label name))))
                 ((eq (sym-entry-storage sym) :function)
                  (return-from evaluate-constant-expression
                    (list :label (sym-entry-offset sym)))))))))
       ;; Other unary operators
       (let ((operand-val (evaluate-constant-expression operand)))
         (when (and operand-val (numberp operand-val))
           (cond
             ((string= op "-") (- operand-val))
             ((string= op "+") operand-val)
             ((string= op "~") (lognot operand-val))
             ((string= op "!") (if (zerop operand-val) 1 0))
             (t nil))))))

    ;; Binary operators
    (binary-op
     (let* ((op (ast-node-value expr))
            (left (first (ast-node-children expr)))
            (right (second (ast-node-children expr)))
            (left-val (evaluate-constant-expression left))
            (right-val (evaluate-constant-expression right)))
       (when (and left-val right-val)
         (cond
           ((string= op "+") (+ left-val right-val))
           ((string= op "-") (- left-val right-val))
           ((string= op "*") (* left-val right-val))
           ((string= op "/") (when (/= right-val 0) (truncate left-val right-val)))
           ((string= op "%") (when (/= right-val 0) (rem left-val right-val)))
           ((string= op "&") (logand left-val right-val))
           ((string= op "|") (logior left-val right-val))
           ((string= op "^") (logxor left-val right-val))
           ((string= op "<<") (ash left-val right-val))
           ((string= op ">>") (ash left-val (- right-val)))
           ((string= op "==") (if (= left-val right-val) 1 0))
           ((string= op "!=") (if (/= left-val right-val) 1 0))
           ((string= op "<") (if (< left-val right-val) 1 0))
           ((string= op ">") (if (> left-val right-val) 1 0))
           ((string= op "<=") (if (<= left-val right-val) 1 0))
           ((string= op ">=") (if (>= left-val right-val) 1 0))
           ((string= op "&&") (if (and (not (zerop left-val)) (not (zerop right-val))) 1 0))
           ((string= op "||") (if (or (not (zerop left-val)) (not (zerop right-val))) 1 0))
           (t nil)))))

    ;; Ternary conditional
    (ternary
     (let* ((condition (first (ast-node-children expr)))
            (then-expr (second (ast-node-children expr)))
            (else-expr (third (ast-node-children expr)))
            (cond-val (evaluate-constant-expression condition)))
       (when cond-val
         (if (not (zerop cond-val))
             (evaluate-constant-expression then-expr)
             (evaluate-constant-expression else-expr)))))

    ;; Cast expressions - just evaluate the operand (the cast is compile-time only for constants)
    (cast
     (let ((operand (first (ast-node-children expr))))
       (evaluate-constant-expression operand)))

    (otherwise nil)))

;;; ===========================================================================
;;; Enum Parsing
;;; ===========================================================================

(defun parse-enum-specifier ()
  "Parse an enum specifier: enum [tag] [{ enumerator-list }]"
  (expect-token 'keyword "enum")

  (let ((tag-name nil)
        (has-body nil))
    ;; Optional tag name
    (when (check-token 'identifier)
      (setf tag-name (token-value (advance-token))))

    ;; Optional body { ... }
    (when (match-token 'punctuation "{")
      (setf has-body t)
      (let ((next-value 0)
            (constants nil))
        ;; Parse enumerators
        (unless (check-token 'punctuation "}")
          (loop
            (let ((const-name (token-value (expect-token 'identifier))))
              ;; Check for explicit value assignment
              (when (match-token 'operator "=")
                (let* ((value-expr (parse-conditional-expression))
                       (value (evaluate-constant-expression value-expr)))
                  (if value
                      (setf next-value value)
                      (compiler-error "Enum initializer must be a constant expression"))))

              ;; Add enum constant to symbol table with :enum-constant storage
              ;; The offset field stores the integer value
              (add-symbol const-name (make-enum-type tag-name) :enum-constant next-value)
              (push (cons const-name next-value) constants)

              ;; Increment for next constant
              (incf next-value)

              ;; Continue or stop
              (if (match-token 'punctuation ",")
                  ;; Allow trailing comma (check if } follows)
                  (when (check-token 'punctuation "}")
                    (return))
                  (return)))))

        (expect-token 'punctuation "}")

        ;; Register named enum in the registry
        (when tag-name
          (setf (gethash tag-name (compiler-state-enum-types *state*))
                (nreverse constants)))))

    ;; Return enum type (or error if just "enum" with nothing)
    (unless (or tag-name has-body)
      (compiler-error "Expected enum tag name or body"))

    (make-enum-type tag-name)))

;;; ===========================================================================
;;; Struct Parsing
;;; ===========================================================================

(defun parse-function-pointer-declarator (return-type)
  "Parse function pointer declarator (*name)(...) or (*name)[]
   Returns (name final-type) or nil if not a function pointer pattern"
  ;; We've already confirmed we're at '(' and next is '*'
  (expect-token 'punctuation "(")
  (expect-token 'operator "*")
  (let ((name (token-value (expect-token 'identifier))))
    (expect-token 'punctuation ")")
    ;; Check for function parameters
    (when (match-token 'punctuation "(")
      ;; Skip parameter list - for now just consume until matching )
      (let ((depth 1))
        (loop while (and (> depth 0) (current-token))
              do (cond
                   ((match-token 'punctuation "(") (incf depth))
                   ((match-token 'punctuation ")") (decf depth))
                   (t (advance-token)))))
      ;; Function pointer - store full return type for use when calling
      (let ((ptr-type (make-type-desc :base 'function
                                      :pointer-level 1
                                      :size 4
                                      :return-type return-type)))
        (return-from parse-function-pointer-declarator
          (values name ptr-type))))
    ;; Not a function pointer, might be a pointer to array - for now return as pointer
    (let ((ptr-type (make-type-desc :base (type-desc-base return-type)
                                    :pointer-level (1+ (type-desc-pointer-level return-type))
                                    :struct-tag (type-desc-struct-tag return-type)
                                    :struct-scope (type-desc-struct-scope return-type))))
      (values name ptr-type))))

(defun check-function-pointer-pattern ()
  "Check if we're looking at a function pointer pattern: ( * identifier )"
  (and (check-token 'punctuation "(")
       ;; Peek ahead to check for *
       (let ((saved-pos (compiler-state-token-pos *state*)))
         (advance-token)  ; skip (
         (let ((is-ptr (check-token 'operator "*")))
           (setf (compiler-state-token-pos *state*) saved-pos)  ; restore position
           is-ptr))))

(defun parse-struct-member-declaration (current-offset)
  "Parse struct member declaration. Returns (member-list new-offset)"
  (let ((member-type (parse-type))
        (members nil)
        (offset current-offset))
    ;; Check for anonymous struct/union (no identifier after type, just semicolon)
    (if (and (or (eq (type-desc-base member-type) 'struct)
                 (eq (type-desc-base member-type) 'union))
             (check-token 'punctuation ";"))
        ;; Anonymous struct/union - inline its members with adjusted offsets
        (let* ((tag (type-desc-struct-tag member-type))
               (nested-def (or (lookup-struct-def tag)
                               (lookup-union-def tag))))
          (when nested-def
            (let ((aligned-base (align-to offset (struct-def-alignment nested-def))))
              (dolist (nested-member (struct-def-members nested-def))
                (let ((adjusted-member (make-struct-member
                                        :name (struct-member-name nested-member)
                                        :type (struct-member-type nested-member)
                                        :offset (+ aligned-base (struct-member-offset nested-member)))))
                  (push adjusted-member members)))
              (setf offset (+ aligned-base (struct-def-size nested-def)))))
          (expect-token 'punctuation ";")
          (values (nreverse members) offset))
        ;; Named member(s)
        (progn
          (loop
            (let (name final-type)
              ;; Check for function pointer pattern: int (*name)()
              (if (check-function-pointer-pattern)
                  (multiple-value-setq (name final-type)
                    (parse-function-pointer-declarator member-type))
                  ;; Normal member
                  (progn
                    (setf name (token-value (expect-token 'identifier)))
                    (setf final-type member-type)
                    ;; Check for array (supports multi-dimensional)
                    (when (check-token 'punctuation "[")
                      (let ((dimensions nil))
                        (loop while (match-token 'punctuation "[")
                              do (let ((dim (number-token-value (expect-token 'number))))
                                   (expect-token 'punctuation "]")
                                   (push dim dimensions)))
                        (when dimensions
                          (let ((array-size (if (= (length dimensions) 1)
                                                (car dimensions)
                                                (nreverse dimensions))))
                            (setf final-type (make-type-desc :base (type-desc-base member-type)
                                                             :pointer-level (type-desc-pointer-level member-type)
                                                             :array-size array-size
                                                             :struct-tag (type-desc-struct-tag member-type)
                                                             :struct-scope (type-desc-struct-scope member-type)))))))))
              (let* ((aligned-offset (align-to offset (type-alignment final-type)))
                     (member (make-struct-member :name name
                                                 :type final-type
                                                 :offset aligned-offset)))
                (push member members)
                (setf offset (+ aligned-offset (type-size final-type)))))
            (unless (match-token 'punctuation ",")
              (return)))
          (expect-token 'punctuation ";")
          (values (nreverse members) offset)))))

(defun parse-struct-specifier ()
  "Parse: struct [tag] [{ member-list }]"
  (expect-token 'keyword "struct")
  (let ((tag-name nil) (has-body nil))
    ;; Optional tag name
    (when (check-token 'identifier)
      (setf tag-name (token-value (advance-token))))
    ;; Optional body
    (when (match-token 'punctuation "{")
      (setf has-body t)
      (let ((members nil) (current-offset 0) (max-align 1))
        (loop until (check-token 'punctuation "}")
              do (multiple-value-bind (member-list next-offset)
                     (parse-struct-member-declaration current-offset)
                   (dolist (m member-list)
                     (setf max-align (max max-align
                                          (type-alignment (struct-member-type m)))))
                   (setf members (append members member-list))
                   (setf current-offset next-offset)))
        (expect-token 'punctuation "}")
        (let ((def (make-struct-def :tag tag-name
                                    :members members
                                    :size (align-to current-offset max-align)
                                    :alignment max-align)))
          (when tag-name
            (register-struct-def tag-name def))
          ;; For anonymous structs, generate a unique tag
          (unless tag-name
            (let ((anon-tag (format nil "__anon_struct_~a"
                                    (incf (compiler-state-label-counter *state*)))))
              (register-struct-def anon-tag def)
              (setf tag-name anon-tag))))))
    (unless (or tag-name has-body)
      (compiler-error "Expected struct tag name or body"))
    (make-struct-type tag-name)))

(defun parse-union-specifier ()
  "Parse: union [tag] [{ member-list }]
   Unlike struct, all union members have offset 0, and size is max of member sizes."
  (expect-token 'keyword "union")
  (let ((tag-name nil) (has-body nil))
    ;; Optional tag name
    (when (check-token 'identifier)
      (setf tag-name (token-value (advance-token))))
    ;; Optional body
    (when (match-token 'punctuation "{")
      (setf has-body t)
      (let ((members nil) (max-size 0) (max-align 1))
        (loop until (check-token 'punctuation "}")
              do (multiple-value-bind (member-list next-offset)
                     (parse-struct-member-declaration 0)  ; Always pass 0 for union
                   (declare (ignore next-offset))
                   (dolist (m member-list)
                     ;; For union, override offset to 0 and track max size
                     (setf (struct-member-offset m) 0)
                     (let ((member-type (struct-member-type m)))
                       (setf max-size (max max-size (type-size member-type)))
                       (setf max-align (max max-align (type-alignment member-type)))))
                   (setf members (append members member-list))))
        (expect-token 'punctuation "}")
        (let ((def (make-struct-def :tag tag-name
                                    :members members
                                    :size (align-to max-size max-align)
                                    :alignment max-align)))
          (when tag-name
            (register-union-def tag-name def))
          ;; For anonymous unions, generate a unique tag
          (unless tag-name
            (let ((anon-tag (format nil "__anon_union_~a"
                                    (incf (compiler-state-label-counter *state*)))))
              (register-union-def anon-tag def)
              (setf tag-name anon-tag))))))
    (unless (or tag-name has-body)
      (compiler-error "Expected union tag name or body"))
    (make-union-type tag-name)))

;;; ===========================================================================
;;; Type Parsing
;;; ===========================================================================

(defun parse-typedef-declaration ()
  "Parse: typedef existing-type new-name;"
  (expect-token 'keyword "typedef")
  (let ((base-type (parse-type-specifier))
        (ptr-level 0))
    ;; Parse pointer stars
    (loop while (match-token 'operator "*")
          do (incf ptr-level))
    ;; Build intermediate type with pointer level
    (let ((inter-type (make-type-desc
                        :base (type-desc-base base-type)
                        :pointer-level ptr-level
                        :size (type-desc-size base-type)
                        :unsigned-p (type-desc-unsigned-p base-type)
                        :enum-tag (type-desc-enum-tag base-type)
                        :struct-tag (type-desc-struct-tag base-type)
                        :struct-scope (type-desc-struct-scope base-type))))
      ;; Check for function pointer typedef: typedef type (*name)(...)
      (if (check-function-pointer-pattern)
          (multiple-value-bind (typedef-name ptr-type)
              (parse-function-pointer-declarator inter-type)
            (setf (gethash typedef-name (compiler-state-typedef-types *state*))
                  ptr-type)
            (expect-token 'punctuation ";")
            (make-node 'typedef-decl :value typedef-name :result-type ptr-type))
          ;; Normal typedef
          (let ((typedef-name (token-value (expect-token 'identifier)))
                (array-size nil))
            ;; Check for array typedef
            (when (match-token 'punctuation "[")
              (when (check-token 'number)
                (setf array-size (number-token-value (advance-token))))
              (expect-token 'punctuation "]"))
            ;; Create and register typedef
            (let ((typedef-type (make-type-desc
                                  :base (type-desc-base inter-type)
                                  :pointer-level (type-desc-pointer-level inter-type)
                                  :array-size array-size
                                  :size (type-desc-size inter-type)
                                  :unsigned-p (type-desc-unsigned-p inter-type)
                                  :enum-tag (type-desc-enum-tag inter-type)
                                  :struct-tag (type-desc-struct-tag inter-type)
                                  :struct-scope (type-desc-struct-scope inter-type))))
              (setf (gethash typedef-name (compiler-state-typedef-types *state*))
                    typedef-type)
              (expect-token 'punctuation ";")
              (make-node 'typedef-decl :value typedef-name :result-type typedef-type)))))))

(defun parse-type-specifier ()
  "Parse a type specifier with modifiers (signed/unsigned, short/long)"
  ;; Handle enum, struct, and union first
  (cond
    ((check-token 'keyword "enum")
     (parse-enum-specifier))
    ((check-token 'keyword "struct")
     (parse-struct-specifier))
    ((check-token 'keyword "union")
     (parse-union-specifier))
    ;; Handle C99 fixed-width types
    ((match-token 'keyword "int8_t")
     (make-int8-type nil))
    ((match-token 'keyword "uint8_t")
     (make-int8-type t))
    ((match-token 'keyword "int16_t")
     (make-int16-type nil))
    ((match-token 'keyword "uint16_t")
     (make-int16-type t))
    ((match-token 'keyword "int32_t")
     (make-int32-type nil))
    ((match-token 'keyword "uint32_t")
     (make-int32-type t))
    ((match-token 'keyword "void")
     (make-void-type))
    ;; Check for typedef names (identifiers in typedef-types hash table)
    ((and (check-token 'identifier)
          (gethash (token-value (current-token))
                   (compiler-state-typedef-types *state*)))
     (let* ((typedef-name (token-value (advance-token)))
            (typedef-type (gethash typedef-name
                                   (compiler-state-typedef-types *state*))))
       (copy-type-desc typedef-type)))
    (t
     ;; Parse type with possible modifiers
     (let ((is-unsigned nil)
           (is-signed nil)
           (is-short nil)
           (is-long nil)
           (has-int nil)
           (has-char nil))
       ;; Collect modifiers and base type (can appear in any order)
       (loop
         (cond
           ((match-token 'keyword "unsigned") (setf is-unsigned t))
           ((match-token 'keyword "signed") (setf is-signed t))
           ((match-token 'keyword "short") (setf is-short t))
           ((match-token 'keyword "long") (setf is-long t))
           ((match-token 'keyword "int") (setf has-int t))
           ((match-token 'keyword "char") (setf has-char t))
           (t (return))))

       ;; Determine the final type based on modifiers
       (cond
         ;; char types
         (has-char
          (make-char-type is-unsigned))

         ;; short types: "short" or "short int"
         (is-short
          (make-short-type is-unsigned))

         ;; long types: "long" or "long int"
         (is-long
          (make-long-type is-unsigned))

         ;; int types (explicit or implied by unsigned/signed alone)
         ((or has-int is-unsigned is-signed)
          (make-int-type is-unsigned))

         (t
          (compiler-error "Expected type specifier, got ~a"
                          (token-value (current-token)))))))))

(defun parse-type ()
  "Parse a full type including const/volatile qualifiers and pointer levels"
  ;; Check for const and volatile qualifiers (can appear before type)
  ;; Both can appear in either order: const volatile int, volatile const int
  (let ((is-const nil)
        (is-volatile nil))
    ;; Parse qualifiers (in any order)
    (loop while (or (check-token 'keyword "const")
                    (check-token 'keyword "volatile"))
          do (cond
               ((match-token 'keyword "const") (setf is-const t))
               ((match-token 'keyword "volatile") (setf is-volatile t))))
    (let ((base-type (parse-type-specifier))
          (ptr-level 0))
      ;; Count pointer stars
      (loop while (match-token 'operator "*")
            do (incf ptr-level))
      (make-type-desc :base (type-desc-base base-type)
                      :pointer-level (+ (type-desc-pointer-level base-type) ptr-level)
                      :array-size nil
                      :size (type-desc-size base-type)
                      :unsigned-p (type-desc-unsigned-p base-type)
                      :volatile-p is-volatile
                      :const-p is-const
                      :struct-tag (type-desc-struct-tag base-type)
                      :struct-scope (type-desc-struct-scope base-type)
                      :return-type (type-desc-return-type base-type)))))

;;; ===========================================================================
;;; Expression Parsing (Precedence Climbing)
;;; ===========================================================================

;;; Operator precedence table (higher = binds tighter)
(defparameter *binary-precedence*
  '(("||" . 1)
    ("&&" . 2)
    ("|" . 3)
    ("^" . 4)
    ("&" . 5)
    ("==" . 6) ("!=" . 6)
    ("<" . 7) (">" . 7) ("<=" . 7) (">=" . 7)
    ("<<" . 8) (">>" . 8)
    ("+" . 9) ("-" . 9)
    ("*" . 10) ("/" . 10) ("%" . 10)))

(defun get-precedence (op)
  "Get precedence of binary operator, or nil if not a binary op"
  (cdr (assoc op *binary-precedence* :test #'string=)))

(defun is-assignment-op (op)
  "Check if op is an assignment operator"
  (member op '("=" "+=" "-=" "*=" "/=" "%=" "&=" "|=" "^=" "<<=" ">>=")
          :test #'string=))

(defun parse-expression ()
  "Parse an expression (entry point)"
  (parse-assignment-expression))

(defun parse-assignment-expression ()
  "Parse assignment expression (right associative)"
  (let ((left (parse-conditional-expression)))
    (when (and (current-token)
               (eq (token-type (current-token)) 'operator)
               (is-assignment-op (token-value (current-token))))
      (let ((op (token-value (advance-token)))
            (right (parse-assignment-expression)))
        (return-from parse-assignment-expression
          (make-node 'assign
                     :value op
                     :children (list left right)))))
    left))

(defun parse-conditional-expression ()
  "Parse ternary conditional expression"
  (let ((cond-expr (parse-binary-expression 1)))
    (when (match-token 'operator "?")
      (let ((then-expr (parse-expression)))
        (expect-token 'punctuation ":")
        (let ((else-expr (parse-conditional-expression)))
          (return-from parse-conditional-expression
            (make-node 'ternary
                       :children (list cond-expr then-expr else-expr))))))
    cond-expr))

(defun parse-binary-expression (min-prec)
  "Parse binary expression using precedence climbing"
  (let ((left (parse-unary-expression)))
    (loop
      (let* ((tok (current-token))
             (op (when (and tok (eq (token-type tok) 'operator))
                   (token-value tok)))
             (prec (when op (get-precedence op))))
        (when (or (null prec) (< prec min-prec))
          (return left))
        (advance-token) ; consume operator
        (let ((right (parse-binary-expression (1+ prec))))
          (setf left (make-node 'binary-op
                                :value op
                                :children (list left right))))))))

(defun parse-unary-expression ()
  "Parse unary expression"
  (let ((tok (current-token)))
    (cond
      ;; Unary operators: - ! ~ * & ++ --
      ((and tok (eq (token-type tok) 'operator)
            (member (token-value tok) '("-" "!" "~" "*" "&" "++" "--")
                    :test #'string=))
       (let ((op (token-value (advance-token)))
             (operand (parse-unary-expression)))
         (make-node 'unary-op
                    :value op
                    :children (list operand))))

      ;; sizeof
      ((check-token 'keyword "sizeof")
       (advance-token)
       (if (match-token 'punctuation "(")
           (let ((type-node (if (check-token 'keyword)
                                (prog1 (parse-type)
                                  (expect-token 'punctuation ")"))
                                (let ((expr (parse-unary-expression)))
                                  (expect-token 'punctuation ")")
                                  expr))))
             (make-node 'sizeof :children (list type-node)))
           (make-node 'sizeof :children (list (parse-unary-expression)))))

      ;; Cast expression: (type) expr
      ((and (check-token 'punctuation "(")
            (let ((next (peek-token 1)))
              (and next
                   (or (and (eq (token-type next) 'keyword)
                            (member (token-value next)
                                    '("int" "char" "void" "unsigned" "signed" "short" "long"
                                      "int8_t" "uint8_t" "int16_t" "uint16_t" "int32_t" "uint32_t"
                                      "enum" "struct" "union" "volatile")
                                    :test #'string=))
                       ;; Typedef names are identifiers in typedef-types
                       (and (eq (token-type next) 'identifier)
                            (gethash (token-value next)
                                     (compiler-state-typedef-types *state*)))))))
       (advance-token) ; skip (
       (let ((cast-type (parse-type)))
         (expect-token 'punctuation ")")
         (let ((expr (parse-unary-expression)))
           (make-node 'cast
                      :value cast-type
                      :children (list expr)))))

      (t (parse-postfix-expression)))))

(defun parse-postfix-expression ()
  "Parse postfix expression (function calls, array access, etc.)"
  (let ((expr (parse-primary-expression)))
    (loop
      (cond
        ;; Function call
        ((match-token 'punctuation "(")
         (let ((args nil))
           (unless (check-token 'punctuation ")")
             (push (parse-assignment-expression) args)
             (loop while (match-token 'punctuation ",")
                   do (push (parse-assignment-expression) args)))
           (expect-token 'punctuation ")")
           (setf expr (make-node 'call
                                 :children (cons expr (reverse args))))))

        ;; Array subscript
        ((match-token 'punctuation "[")
         (let ((index (parse-expression)))
           (expect-token 'punctuation "]")
           (setf expr (make-node 'subscript
                                 :children (list expr index)))))

        ;; Post-increment/decrement
        ((and (current-token)
              (eq (token-type (current-token)) 'operator)
              (member (token-value (current-token)) '("++" "--") :test #'string=))
         (let ((op (token-value (advance-token))))
           (setf expr (make-node 'post-op
                                 :value op
                                 :children (list expr)))))

        ;; Member access (.)
        ((check-token 'punctuation ".")
         (advance-token)
         (let ((member-name (token-value (expect-token 'identifier))))
           (setf expr (make-node 'member :value member-name
                                 :children (list expr) :data :direct))))

        ;; Pointer member access (->)
        ((check-token 'operator "->")
         (advance-token)
         (let ((member-name (token-value (expect-token 'identifier))))
           (setf expr (make-node 'member :value member-name
                                 :children (list expr) :data :pointer))))

        (t (return expr))))))

(defun parse-primary-expression ()
  "Parse primary expression (literals, identifiers, parenthesized)"
  (let ((tok (current-token)))
    (cond
      ;; Number literal
      ((check-token 'number)
       (advance-token)
       (make-node 'literal
                  :value (number-token-value tok)
                  :result-type (suffix-to-type (number-token-suffix tok))))

      ;; String literal
      ((check-token 'string)
       (advance-token)
       (make-node 'string-literal
                  :value (token-value tok)
                  :result-type (make-pointer-type (make-char-type))))

      ;; Identifier
      ((check-token 'identifier)
       (advance-token)
       (make-node 'var-ref :value (token-value tok)))

      ;; Parenthesized expression
      ((match-token 'punctuation "(")
       (let ((expr (parse-expression)))
         (expect-token 'punctuation ")")
         expr))

      (t
       (compiler-error "Expected expression, got ~a"
                       (if tok (token-value tok) "end of file"))))))

;;; ===========================================================================
;;; Statement Parsing
;;; ===========================================================================

(defun parse-statement ()
  "Parse a statement"
  (cond
    ;; Compound statement (block)
    ((check-token 'punctuation "{")
     (parse-compound-statement))

    ;; If statement
    ((check-token 'keyword "if")
     (parse-if-statement))

    ;; While statement
    ((check-token 'keyword "while")
     (parse-while-statement))

    ;; For statement
    ((check-token 'keyword "for")
     (parse-for-statement))

    ;; Do-while statement
    ((check-token 'keyword "do")
     (parse-do-while-statement))

    ;; Return statement
    ((check-token 'keyword "return")
     (parse-return-statement))

    ;; Break statement
    ((check-token 'keyword "break")
     (advance-token)
     (expect-token 'punctuation ";")
     (make-node 'break))

    ;; Continue statement
    ((check-token 'keyword "continue")
     (advance-token)
     (expect-token 'punctuation ";")
     (make-node 'continue))

    ;; Goto statement
    ((check-token 'keyword "goto")
     (parse-goto-statement))

    ;; Labeled statement (identifier followed by colon)
    ;; Need to look ahead to distinguish from expression statement
    ((and (check-token 'identifier)
          (let ((next (peek-token 1)))
            (and next
                 (eq (token-type next) 'punctuation)
                 (string= (token-value next) ":"))))
     (parse-labeled-statement))

    ;; Empty statement
    ((match-token 'punctuation ";")
     (make-node 'empty))

    ;; Declaration or expression statement
    (t
     (if (is-type-token (current-token))
         (parse-local-declaration)
         (parse-expression-statement)))))

(defun is-type-token (tok)
  "Check if token starts a type declaration (including storage class specifiers)"
  (and tok
       (or (and (eq (token-type tok) 'keyword)
                (member (token-value tok)
                        '("int" "char" "void" "unsigned" "signed" "short" "long"
                          "int8_t" "uint8_t" "int16_t" "uint16_t" "int32_t" "uint32_t"
                          "enum" "struct" "union" "volatile" "const" "static")
                        :test #'string=))
           ;; Typedef names are identifiers
           (and (eq (token-type tok) 'identifier)
                (gethash (token-value tok)
                         (compiler-state-typedef-types *state*))))))

(defun parse-compound-statement ()
  "Parse a compound statement (block)"
  (expect-token 'punctuation "{")
  (enter-scope)
  (let ((statements nil))
    (loop until (or (at-end) (check-token 'punctuation "}"))
          do (push (parse-statement) statements))
    (expect-token 'punctuation "}")
    (exit-scope)
    (make-node 'block :children (reverse statements))))

(defun parse-if-statement ()
  "Parse an if statement"
  (expect-token 'keyword "if")
  (expect-token 'punctuation "(")
  (let ((condition (parse-expression)))
    (expect-token 'punctuation ")")
    (let ((then-branch (parse-statement))
          (else-branch nil))
      (when (match-token 'keyword "else")
        (setf else-branch (parse-statement)))
      (make-node 'if
                 :children (if else-branch
                               (list condition then-branch else-branch)
                               (list condition then-branch))))))

(defun parse-while-statement ()
  "Parse a while statement"
  (expect-token 'keyword "while")
  (expect-token 'punctuation "(")
  (let ((condition (parse-expression)))
    (expect-token 'punctuation ")")
    (let ((body (parse-statement)))
      (make-node 'while :children (list condition body)))))

(defun parse-for-statement ()
  "Parse a for statement"
  (expect-token 'keyword "for")
  (expect-token 'punctuation "(")
  (enter-scope)

  ;; Init clause
  (let ((init (cond
                ((match-token 'punctuation ";") nil)
                ((is-type-token (current-token))
                 (parse-local-declaration))
                (t (let ((expr (parse-expression)))
                     (expect-token 'punctuation ";")
                     expr)))))

    ;; Condition clause
    (let ((condition (if (check-token 'punctuation ";")
                         nil
                         (parse-expression))))
      (expect-token 'punctuation ";")

      ;; Update clause
      (let ((update (if (check-token 'punctuation ")")
                        nil
                        (parse-expression))))
        (expect-token 'punctuation ")")
        (let ((body (parse-statement)))
          (exit-scope)
          (make-node 'for :children (list init condition update body)))))))

(defun parse-do-while-statement ()
  "Parse a do-while statement"
  (expect-token 'keyword "do")
  (let ((body (parse-statement)))
    (expect-token 'keyword "while")
    (expect-token 'punctuation "(")
    (let ((condition (parse-expression)))
      (expect-token 'punctuation ")")
      (expect-token 'punctuation ";")
      (make-node 'do-while :children (list body condition)))))

(defun parse-return-statement ()
  "Parse a return statement"
  (expect-token 'keyword "return")
  (if (match-token 'punctuation ";")
      (make-node 'return)
      (let ((expr (parse-expression)))
        (expect-token 'punctuation ";")
        (make-node 'return :children (list expr)))))

(defun parse-expression-statement ()
  "Parse an expression statement"
  (let ((expr (parse-expression)))
    (expect-token 'punctuation ";")
    (make-node 'expr-stmt :children (list expr))))

(defun parse-goto-statement ()
  "Parse a goto statement: goto label;"
  (expect-token 'keyword "goto")
  (let ((label-name (token-value (expect-token 'identifier))))
    (expect-token 'punctuation ";")
    (make-node 'goto :value label-name)))

(defun parse-labeled-statement ()
  "Parse a labeled statement: label: statement"
  (let ((label-name (token-value (advance-token))))  ; consume identifier
    (expect-token 'punctuation ":")  ; consume colon
    (let ((stmt (parse-statement)))  ; parse the following statement
      (make-node 'labeled-stmt :value label-name :children (list stmt)))))

;;; ===========================================================================
;;; Initializer List Parsing
;;; ===========================================================================

(defun parse-initializer ()
  "Parse either a single expression or an initializer list.
   Also handles string literal as special case for char array initialization."
  (if (check-token 'punctuation "{")
      (parse-initializer-list)
      (parse-assignment-expression)))

(defun parse-designated-or-plain-initializer ()
  "Parse either a designated initializer (.field = expr or [n] = expr) or a plain initializer"
  (cond
    ;; Designated initializer: .field = expr
    ((match-token 'punctuation ".")
     (let* ((field-name (token-value (expect-token 'identifier))))
       (expect-token 'operator "=")
       (let ((init-expr (parse-initializer)))
         (make-node 'designated-init
                    :value field-name
                    :children (list init-expr)))))
    ;; Array designated initializer: [n] = expr
    ((match-token 'punctuation "[")
     (let ((index-expr (parse-expression)))
       (expect-token 'punctuation "]")
       (expect-token 'operator "=")
       (let ((init-expr (parse-initializer)))
         (make-node 'array-designated-init
                    :children (list index-expr init-expr)))))
    ;; Plain initializer
    (t (parse-initializer))))

(defun parse-initializer-list ()
  "Parse {expr, expr, ...} including nested lists and designated initializers.
   Returns an AST node of type 'init-list with children being the elements."
  (expect-token 'punctuation "{")
  (let ((elements nil))
    ;; Handle empty initializer list {}
    (unless (check-token 'punctuation "}")
      (push (parse-designated-or-plain-initializer) elements)
      (loop while (match-token 'punctuation ",")
            do (unless (check-token 'punctuation "}")
                 (push (parse-designated-or-plain-initializer) elements))))
    (expect-token 'punctuation "}")
    (make-node 'init-list :children (nreverse elements))))

(defun infer-array-size-from-init (init-node element-type)
  "Infer array size from initializer.
   For init-list: count top-level elements, handling array designated initializers.
   For nested init-lists (multi-dim arrays): recursively infer inner dimensions.
   For string-literal initializing char[]: use strlen+1."
  (cond
    ;; Init list - need to handle array designated initializers
    ((and (ast-node-p init-node)
          (eq (ast-node-type init-node) 'init-list))
     (let ((elements (ast-node-children init-node))
           (current-index 0)
           (max-index 0))
       ;; Walk through elements, tracking current position
       (dolist (elem elements)
         (cond
           ;; Array designated initializer: [n] = value
           ((and (ast-node-p elem)
                 (eq (ast-node-type elem) 'array-designated-init))
            (let* ((index-expr (first (ast-node-children elem)))
                   (index-val (evaluate-constant-expression index-expr)))
              (when index-val
                (setf current-index index-val)
                (setf max-index (max max-index current-index))
                (incf current-index))))
           ;; Regular element - uses current position
           (t
            (setf max-index (max max-index current-index))
            (incf current-index))))
       ;; outer-dim is max-index + 1
       (let ((outer-dim (1+ max-index)))
         ;; Check for nested init-lists (multi-dimensional arrays)
         (if (and elements
                  (ast-node-p (first elements))
                  (eq (ast-node-type (first elements)) 'init-list))
             ;; Recursively infer inner dimensions from first nested init-list
             (let ((inner (infer-array-size-from-init (first elements) element-type)))
               (if inner
                   (if (listp inner)
                       (cons outer-dim inner)
                       (list outer-dim inner))
                   outer-dim))
             outer-dim))))
    ;; String literal for char array - use strlen+1
    ((and (ast-node-p init-node)
          (eq (ast-node-type init-node) 'string-literal)
          (eq (type-desc-base element-type) 'char))
     (1+ (length (ast-node-value init-node))))
    ;; Otherwise can't infer
    (t nil)))

(defun parse-local-declaration ()
  "Parse a local variable declaration, including static locals"
  ;; Check for static keyword
  (let ((is-static (match-token 'keyword "static")))
    (let ((base-type (parse-type))
          (decls nil))
      ;; Check for type-only declaration (e.g., struct T { int x; }; or struct T;)
      (when (check-token 'punctuation ";")
        (advance-token)
        ;; Return empty declaration list for type-only definitions
        (return-from parse-local-declaration
          (make-node 'decl-list :children nil)))
      ;; Parse declarators
      (loop
        ;; Count declarator-specific pointer stars (e.g., int x, *p, **pp)
        (let ((decl-ptr-level 0))
          (loop while (match-token 'operator "*")
                do (incf decl-ptr-level))
          (let* ((name (token-value (expect-token 'identifier)))
                 (array-size nil)
                 ;; Create per-declarator type combining base type and declarator pointers
                 (var-type (make-type-desc :base (type-desc-base base-type)
                                           :pointer-level (+ (type-desc-pointer-level base-type)
                                                             decl-ptr-level)
                                           :array-size nil
                                           :size (type-desc-size base-type)
                                           :unsigned-p (type-desc-unsigned-p base-type)
                                           :volatile-p (type-desc-volatile-p base-type)
                                           :const-p (type-desc-const-p base-type)
                                           :struct-tag (type-desc-struct-tag base-type)
                                           :struct-scope (type-desc-struct-scope base-type)
                                           :return-type (type-desc-return-type base-type))))
            ;; Check for array declaration (supports multi-dimensional)
            (when (check-token 'punctuation "[")
              (let ((dimensions nil))
                (loop while (match-token 'punctuation "[")
                      do (let ((dim (if (check-token 'number)
                                        (number-token-value (advance-token))
                                        :infer)))
                           (expect-token 'punctuation "]")
                           (push dim dimensions)))
                (when dimensions
                  (setf array-size (if (= (length dimensions) 1)
                                       (car dimensions)
                                       (nreverse dimensions)))))
              (setf var-type (make-type-desc :base (type-desc-base var-type)
                                             :pointer-level (type-desc-pointer-level var-type)
                                             :array-size array-size
                                             :size (type-desc-size var-type)
                                             :unsigned-p (type-desc-unsigned-p var-type)
                                             :volatile-p (type-desc-volatile-p var-type)
                                             :const-p (type-desc-const-p var-type)
                                             :struct-tag (type-desc-struct-tag var-type)
                                             :struct-scope (type-desc-struct-scope var-type))))

          ;; Check for initializer
          (let ((init nil)
                (init-value 0))  ; Default init for static
            (when (match-token 'operator "=")
              (setf init (parse-initializer))
              ;; Infer array size if declared with [] but no size specified
              (when (and (eq array-size :infer)
                         init
                         (or (and (ast-node-p init)
                                  (eq (ast-node-type init) 'init-list))
                             (and (ast-node-p init)
                                  (eq (ast-node-type init) 'string-literal)
                                  (eq (type-desc-base var-type) 'char))))
                ;; Infer array size from initializer
                (let ((inferred-size (infer-array-size-from-init init var-type)))
                  (when inferred-size
                    (setf array-size inferred-size)
                    (setf var-type (make-type-desc :base (type-desc-base var-type)
                                                   :pointer-level (type-desc-pointer-level var-type)
                                                   :array-size array-size
                                                   :size (type-desc-size var-type)
                                                   :unsigned-p (type-desc-unsigned-p var-type)
                                                   :struct-tag (type-desc-struct-tag var-type)
                                                   :struct-scope (type-desc-struct-scope var-type))))))
              ;; For static locals, try to evaluate constant initializer
              (when is-static
                (let ((const-val (evaluate-constant-expression init)))
                  (when const-val
                    (setf init-value const-val)))))

            (if is-static
                ;; Static local - store in data section with unique label
                (let* ((func-name (or (compiler-state-current-function *state*) "global"))
                       (label-name (format nil "__static_~a_~a_~a"
                                           func-name name
                                           (incf (compiler-state-label-counter *state*))))
                       (label-sym (intern (string-upcase label-name) :c-compiler)))
                  ;; Register symbol with :static-local storage, offset stores init value
                  (add-symbol name var-type :static-local label-sym)
                  ;; Record the static data for emission (label, initial value, size)
                  (push (list label-sym init-value (type-size var-type))
                        (compiler-state-data *state*))
                  ;; For static, we don't use the init AST node - already handled
                  (push (make-node 'var-decl
                                   :value name
                                   :children nil  ; No runtime init for static
                                   :result-type var-type
                                   :data :static)
                        decls))
                ;; Non-static local - existing logic
                (progn
                  ;; Decide between register and stack allocation
                  (let ((local-reg-count (compiler-state-local-reg-count *state*)))
                    (if (and (not array-size)               ; Not an array
                             (not (eq (type-desc-base var-type) 'struct)) ; Not a struct
                             (not (eq (type-desc-base var-type) 'union))  ; Not a union
                             (not (type-desc-volatile-p var-type))        ; Not volatile
                             (< local-reg-count 4)          ; Have free local regs (R6-R9)
                             (not (is-address-taken name))) ; Address not taken
                        ;; Register allocation
                        (progn
                          (add-symbol name var-type :register local-reg-count)
                          (incf (compiler-state-local-reg-count *state*)))
                        ;; Stack allocation (arrays or when out of registers)
                        ;; type-size now handles arrays, so just use it directly
                        (let* ((size (type-size var-type))
                               (elem-size (if array-size
                                             ;; For arrays, get element size (base type without array-size)
                                             (type-size (make-type-desc :base (type-desc-base var-type)
                                                                        :pointer-level (type-desc-pointer-level var-type)
                                                                        :size (type-desc-size var-type)
                                                                        :struct-tag (type-desc-struct-tag var-type)
                                                                        :struct-scope (type-desc-struct-scope var-type)))
                                             (type-size var-type)))
                               (alignment (if (>= elem-size 4) 4
                                             (if (>= elem-size 2) 2 1)))
                               (current-offset (compiler-state-local-offset *state*))
                               (aligned-offset (- current-offset
                                                  (mod current-offset alignment)))
                               (offset (- aligned-offset size)))
                          (setf (compiler-state-local-offset *state*) offset)
                          (add-symbol name var-type :local offset))))

                  (push (make-node 'var-decl
                                   :value name
                                   :children (when init (list init))
                                   :result-type var-type)
                        decls))))))

        ;; Check for more declarators
        (unless (match-token 'punctuation ",")
          (return)))

      (expect-token 'punctuation ";")
      (make-node 'decl-list :children (reverse decls)))))

;;; ===========================================================================
;;; Top-Level Parsing
;;; ===========================================================================

(defun count-statements (node)
  "Count the number of statements in an AST node (for inlining heuristics)"
  (when (null node)
    (return-from count-statements 0))
  (unless (ast-node-p node)
    (return-from count-statements 0))
  (case (ast-node-type node)
    (block
     (reduce #'+ (ast-node-children node) :key #'count-statements :initial-value 0))
    ((if while for do-while)
     (+ 1 (reduce #'+ (ast-node-children node) :key #'count-statements :initial-value 0)))
    ((return expr-stmt decl-list break continue)
     1)
    (otherwise 0)))

(defun parse-function-prototype-params (name ret-type)
  "Parse just the parameter list of a function prototype (for multi-declarator lines)"
  ;; Register function name in global symbol table
  (add-global-symbol name ret-type :function (make-c-label name))

  (expect-token 'punctuation "(")
  (let ((params nil))
    (unless (check-token 'punctuation ")")
      (loop
        (let ((param-type (parse-type))
              (param-name nil))
          ;; Parameter name is optional in declaration
          (when (check-token 'identifier)
            (setf param-name (token-value (advance-token))))
          ;; Check for array parameter - decays to pointer
          (when (match-token 'punctuation "[")
            (unless (check-token 'punctuation "]")
              (parse-expression))  ; Ignore size
            (expect-token 'punctuation "]")
            (setf param-type (make-type-desc :base (type-desc-base param-type)
                                             :pointer-level (1+ (type-desc-pointer-level param-type))
                                             :size (type-desc-size param-type)
                                             :unsigned-p (type-desc-unsigned-p param-type)
                                             :struct-tag (type-desc-struct-tag param-type)
                                             :struct-scope (type-desc-struct-scope param-type))))
          (push (make-node 'param :value param-name :result-type param-type) params))
        (unless (match-token 'punctuation ",")
          (return))))
    (expect-token 'punctuation ")")
    (make-node 'func-prototype
               :value name
               :children (list (make-node 'params :children (nreverse params)))
               :result-type ret-type)))

(defun parse-function-definition (ret-type name &optional is-inline)
  "Parse a function definition"
  (setf (compiler-state-current-function *state*) name)
  (setf (compiler-state-local-offset *state*) 0)
  (setf (compiler-state-param-count *state*) 0)
  (setf (compiler-state-local-reg-count *state*) 0)  ; Reset local register count

  ;; Register function name in global symbol table for use as function pointer
  ;; Store the return type directly - get-expression-type uses this when function is called
  (add-global-symbol name ret-type :function (make-c-label name))

  (expect-token 'punctuation "(")
  (enter-scope)

  ;; Parse parameters
  (let ((params nil)
        (param-idx 0))
    (unless (check-token 'punctuation ")")
      (loop
        (let ((param-type (parse-type))
              (param-name nil))
          ;; Parameter name is optional in declaration
          (when (check-token 'identifier)
            (setf param-name (token-value (advance-token))))
          ;; Check for array parameter (e.g., int x[100]) - decays to pointer
          (when (match-token 'punctuation "[")
            ;; Consume optional size
            (unless (check-token 'punctuation "]")
              (parse-expression))  ; Ignore the size, it's just for documentation
            (expect-token 'punctuation "]")
            ;; Array parameters decay to pointers
            (setf param-type (make-type-desc :base (type-desc-base param-type)
                                             :pointer-level (1+ (type-desc-pointer-level param-type))
                                             :size (type-desc-size param-type)
                                             :unsigned-p (type-desc-unsigned-p param-type)
                                             :struct-tag (type-desc-struct-tag param-type)
                                             :struct-scope (type-desc-struct-scope param-type))))

          ;; Register parameter in symbol table
          ;; First 4 params go in P0-P3 (R10-R13), rest on stack
          (when param-name
            (add-symbol param-name param-type :parameter param-idx))

          (push (make-node 'param
                           :value param-name
                           :result-type param-type)
                params)
          (incf param-idx))

        (unless (match-token 'punctuation ",")
          (return))))

    (expect-token 'punctuation ")")
    (setf (compiler-state-param-count *state*) param-idx)

    ;; Check for function prototype (declaration without body)
    (when (match-token 'punctuation ";")
      (exit-scope)
      (setf (compiler-state-current-function *state*) nil)
      ;; Return a prototype node (no code generation needed)
      (return-from parse-function-definition
        (make-node 'func-prototype
                   :value name
                   :children (list (make-node 'params :children (reverse params)))
                   :result-type ret-type)))

    ;; Check for multiple declarators (e.g., int f(int a), g(int b), x;)
    (when (check-token 'punctuation ",")
      (exit-scope)
      (setf (compiler-state-current-function *state*) nil)
      ;; First prototype is done, continue parsing more declarators
      (let ((decls (list (make-node 'func-prototype
                                    :value name
                                    :children (list (make-node 'params :children (reverse params)))
                                    :result-type ret-type))))
        (loop while (match-token 'punctuation ",")
              do (let ((next-name (token-value (expect-token 'identifier))))
                   (if (check-token 'punctuation "(")
                       ;; Another function prototype
                       (let ((proto-node (parse-function-prototype-params next-name ret-type)))
                         (push proto-node decls))
                       ;; Regular variable
                       (progn
                         (add-symbol next-name ret-type :global nil)
                         ;; Handle tentative definition
                         (let ((existing-def (gethash next-name (compiler-state-global-defs *state*))))
                           (unless existing-def
                             (setf (gethash next-name (compiler-state-global-defs *state*))
                                   (list :tentative ret-type))))))))
        (expect-token 'punctuation ";")
        (return-from parse-function-definition
          (make-node 'decl-list :children (nreverse decls)))))

    ;; Parse function body
    (let ((body (parse-compound-statement)))
      (exit-scope)

      ;; Reset current function to nil (back to global scope)
      (setf (compiler-state-current-function *state*) nil)

      ;; Calculate frame size from locals allocated during parsing
      (let* ((frame-size (- (compiler-state-local-offset *state*)))
             (local-reg-count (compiler-state-local-reg-count *state*))
             (stmt-count (count-statements body))
             ;; Create function node with local register info and inline hint
             (func-node (make-node 'function
                                   :value name
                                   :children (list (make-node 'params :children (reverse params))
                                                   body)
                                   :result-type ret-type
                                   :data (list :frame-size frame-size
                                               :param-count param-idx
                                               :local-reg-count local-reg-count
                                               :inline-hint is-inline
                                               :stmt-count stmt-count))))
        ;; Register function in function table for inlining
        (setf (gethash name (compiler-state-function-table *state*)) func-node)
        func-node))))

(defun parse-global-declaration (var-type name)
  "Parse a global variable declaration with optional initializer"
  (let ((decls nil))
    ;; Add first declarator
    (let ((array-size nil)
          (init-value 0)
          (init-node nil))
      ;; Check for array declaration (supports multi-dimensional)
      (when (check-token 'punctuation "[")
        (let ((dimensions nil))
          (loop while (match-token 'punctuation "[")
                do (let ((dim (if (check-token 'number)
                                  (number-token-value (advance-token))
                                  :infer)))
                     (expect-token 'punctuation "]")
                     (push dim dimensions)))
          (when dimensions
            (setf array-size (if (= (length dimensions) 1)
                                 (car dimensions)
                                 (nreverse dimensions)))))
        ;; Create array type
        (setf var-type (make-type-desc :base (type-desc-base var-type)
                                       :pointer-level (type-desc-pointer-level var-type)
                                       :array-size array-size
                                       :size (type-desc-size var-type)
                                       :unsigned-p (type-desc-unsigned-p var-type)
                                       :struct-tag (type-desc-struct-tag var-type)
                                       :struct-scope (type-desc-struct-scope var-type))))

      ;; Check for initializer
      (when (match-token 'operator "=")
        (setf init-node (parse-initializer))
        ;; Infer array size if declared with [] but no size specified
        (when (and (eq array-size :infer)
                   init-node
                   (or (and (ast-node-p init-node)
                            (eq (ast-node-type init-node) 'init-list))
                       (and (ast-node-p init-node)
                            (eq (ast-node-type init-node) 'string-literal)
                            (eq (type-desc-base var-type) 'char))))
          (let ((inferred-size (infer-array-size-from-init init-node var-type)))
            (when inferred-size
              (setf array-size inferred-size)
              (setf var-type (make-type-desc :base (type-desc-base var-type)
                                             :pointer-level (type-desc-pointer-level var-type)
                                             :array-size array-size
                                             :size (type-desc-size var-type)
                                             :unsigned-p (type-desc-unsigned-p var-type)
                                             :struct-tag (type-desc-struct-tag var-type)
                                             :struct-scope (type-desc-struct-scope var-type))))))
        ;; For simple expressions, evaluate to constant
        ;; Skip for init-list and string-literal - those are handled specially
        (unless (and (ast-node-p init-node)
                     (member (ast-node-type init-node) '(init-list string-literal)))
          (let ((const-val (evaluate-constant-expression init-node)))
            (if const-val
                (setf init-value const-val)
                (compiler-error "Global variable initializer must be a constant expression")))))

      ;; Handle tentative definitions (C allows multiple declarations of same global)
      ;; In C, multiple declarations merge: int x; int x = 3; int x; is valid
      ;; Only emit a node if this is definite (has initializer) or first tentative
      (let* ((has-init (or init-node (not (zerop init-value))))
             (existing-def (gethash name (compiler-state-global-defs *state*)))
             (should-emit nil))
        (cond
          ;; Already definitively defined with an initializer
          ((eq existing-def :definite)
           (when has-init
             (compiler-error "Redefinition of global variable '~a'" name)))
          ;; Previously tentatively defined (stored as (:tentative type))
          ((and (listp existing-def) (eq (first existing-def) :tentative))
           (when has-init
             ;; Upgrade to definite and emit this definition
             (setf (gethash name (compiler-state-global-defs *state*)) :definite)
             (setf should-emit t)))
          ;; First time seeing this variable
          (t
           (setf (gethash name (compiler-state-global-defs *state*))
                 (if has-init :definite :tentative))
           ;; Only emit if definite; tentative will be emitted at end if not overridden
           (setf should-emit has-init)))

        (add-symbol name var-type :global nil)
        ;; Store type info for tentative definitions to emit later if needed
        ;; But don't overwrite a definite definition
        (when (and (not has-init)
                   (not (eq existing-def :definite)))
          (setf (gethash name (compiler-state-global-defs *state*))
                (list :tentative var-type)))
        (when should-emit
          (push (make-node 'global-var
                           :value name
                           :result-type var-type
                           ;; Store init-list/string-literal node in data field, or constant value
                           :data (if (and (ast-node-p init-node)
                                          (member (ast-node-type init-node)
                                                  '(init-list string-literal)))
                                     init-node
                                     init-value))
                decls))))

    ;; Parse additional declarators
    (loop while (match-token 'punctuation ",")
          do (let ((next-name (token-value (expect-token 'identifier)))
                   (next-init-value 0)
                   (next-init-node nil))
               ;; Check for initializer on additional declarators
               (when (match-token 'operator "=")
                 (setf next-init-node (parse-initializer))
                 ;; For simple expressions, evaluate to constant
                 (unless (and (ast-node-p next-init-node)
                              (eq (ast-node-type next-init-node) 'init-list))
                   (let ((const-val (evaluate-constant-expression next-init-node)))
                     (if const-val
                         (setf next-init-value const-val)
                         (compiler-error "Global variable initializer must be a constant expression")))))
               ;; Handle tentative definitions for additional declarators
               (let* ((has-init (or next-init-node (not (zerop next-init-value))))
                      (existing-def (gethash next-name (compiler-state-global-defs *state*)))
                      (should-emit nil))
                 (cond
                   ((eq existing-def :definite)
                    (when has-init
                      (compiler-error "Redefinition of global variable '~a'" next-name)))
                   ((and (listp existing-def) (eq (first existing-def) :tentative))
                    (when has-init
                      (setf (gethash next-name (compiler-state-global-defs *state*)) :definite)
                      (setf should-emit t)))
                   (t
                    (setf (gethash next-name (compiler-state-global-defs *state*))
                          (if has-init :definite :tentative))
                    (setf should-emit has-init)))

                 (add-symbol next-name var-type :global nil)
                 (when (and (not has-init)
                            (not (eq existing-def :definite)))
                   (setf (gethash next-name (compiler-state-global-defs *state*))
                         (list :tentative var-type)))
                 (when should-emit
                   (push (make-node 'global-var
                                    :value next-name
                                    :result-type var-type
                                    :data (if (and (ast-node-p next-init-node)
                                                   (eq (ast-node-type next-init-node) 'init-list))
                                              next-init-node
                                              next-init-value))
                         decls)))))

    (expect-token 'punctuation ";")
    (make-node 'decl-list :children (reverse decls))))

(defun parse-top-level ()
  "Parse a top-level declaration or function definition"
  ;; Check for typedef first
  (when (check-token 'keyword "typedef")
    (return-from parse-top-level (parse-typedef-declaration)))
  ;; Check for static keyword (no-op for functions, just consume it)
  (match-token 'keyword "static")
  ;; Check for inline keyword
  (let ((is-inline (match-token 'keyword "inline")))
    ;; Parse type and name
    (let* ((type-spec (parse-type)))

      ;; Check for standalone type declaration (no variable name)
      ;; Handles: enum E { ... }; struct T { ... }; struct T;
      (when (match-token 'punctuation ";")
        ;; Standalone type definition - type already registered during parse-type
        (return-from parse-top-level
          (cond
            ((type-desc-enum-tag type-spec)
             (make-node 'enum-decl :result-type type-spec))
            ((type-desc-struct-tag type-spec)
             (make-node 'struct-decl :result-type type-spec))
            (t (make-node 'type-decl :result-type type-spec)))))

      ;; Check for function pointer pattern: return_type (*name)(...)
      (if (check-function-pointer-pattern)
          (multiple-value-bind (name ptr-type)
              (parse-function-pointer-declarator type-spec)
            (parse-global-declaration ptr-type name))
          ;; Normal case: expect identifier for variable/function name
          (let ((name (token-value (expect-token 'identifier))))
            ;; Function definition or declaration?
            (if (check-token 'punctuation "(")
                (parse-function-definition type-spec name is-inline)
                (parse-global-declaration type-spec name)))))))

(defun parse-program ()
  "Parse a complete program"
  (let ((declarations nil))
    (loop until (at-end)
          do (push (parse-top-level) declarations))
    ;; Emit tentative definitions that were never given a definite initializer
    (maphash (lambda (name def-info)
               (when (and (listp def-info) (eq (first def-info) :tentative))
                 (let ((var-type (second def-info)))
                   (push (make-node 'global-var
                                    :value name
                                    :result-type var-type
                                    :data 0)  ; Default to zero
                         declarations))))
             (compiler-state-global-defs *state*))
    (make-node 'program :children (reverse declarations))))
