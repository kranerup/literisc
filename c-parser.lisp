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
   This is needed to prevent register allocation for such variables."
  (let ((tokens (compiler-state-tokens *state*))
        (addr-taken (compiler-state-address-taken *state*)))
    (loop for i from 0 below (1- (length tokens))
          for tok = (nth i tokens)
          for next-tok = (nth (1+ i) tokens)
          for prev-tok = (when (> i 0) (nth (1- i) tokens))
          when (and (eq (token-type tok) 'operator)
                    (string= (token-value tok) "&")
                    (eq (token-type next-tok) 'identifier)
                    ;; Not after an operand means it's unary & (address-of)
                    ;; After an operand, it's binary & (bitwise AND)
                    (not (is-operand-token prev-tok)))
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

    ;; Variable reference - might be an enum constant
    (var-ref
     (let* ((name (ast-node-value expr))
            (sym (lookup-symbol name)))
       (when (and sym (eq (sym-entry-storage sym) :enum-constant))
         (sym-entry-offset sym))))  ; offset stores the enum value

    ;; Unary operators
    (unary-op
     (let* ((op (ast-node-value expr))
            (operand (first (ast-node-children expr)))
            (operand-val (evaluate-constant-expression operand)))
       (when operand-val
         (cond
           ((string= op "-") (- operand-val))
           ((string= op "+") operand-val)
           ((string= op "~") (lognot operand-val))
           ((string= op "!") (if (zerop operand-val) 1 0))
           (t nil)))))

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
              (unless (match-token 'punctuation ",")
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

(defun parse-struct-member-declaration (current-offset)
  "Parse struct member declaration. Returns (member-list new-offset)"
  (let ((member-type (parse-type))
        (members nil)
        (offset current-offset))
    (loop
      (let* ((name (token-value (expect-token 'identifier)))
             (aligned-offset (align-to offset (type-alignment member-type)))
             (member (make-struct-member :name name
                                         :type member-type
                                         :offset aligned-offset)))
        (push member members)
        (setf offset (+ aligned-offset (type-size member-type))))
      (unless (match-token 'punctuation ",")
        (return)))
    (expect-token 'punctuation ";")
    (values (nreverse members) offset)))

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
            (setf (gethash tag-name (compiler-state-struct-types *state*)) def))
          ;; For anonymous structs, generate a unique tag
          (unless tag-name
            (let ((anon-tag (format nil "__anon_struct_~a"
                                    (incf (compiler-state-label-counter *state*)))))
              (setf (gethash anon-tag (compiler-state-struct-types *state*)) def)
              (setf tag-name anon-tag))))))
    (unless (or tag-name has-body)
      (compiler-error "Expected struct tag name or body"))
    (make-struct-type tag-name)))

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
    ;; Get typedef name
    (let ((typedef-name (token-value (expect-token 'identifier)))
          (array-size nil))
      ;; Check for array typedef
      (when (match-token 'punctuation "[")
        (when (check-token 'number)
          (setf array-size (token-value (advance-token))))
        (expect-token 'punctuation "]"))
      ;; Create and register typedef
      (let ((typedef-type (make-type-desc
                            :base (type-desc-base base-type)
                            :pointer-level ptr-level
                            :array-size array-size
                            :size (type-desc-size base-type)
                            :unsigned-p (type-desc-unsigned-p base-type)
                            :enum-tag (type-desc-enum-tag base-type)
                            :struct-tag (type-desc-struct-tag base-type))))
        (setf (gethash typedef-name (compiler-state-typedef-types *state*))
              typedef-type)
        (expect-token 'punctuation ";")
        (make-node 'typedef-decl :value typedef-name :result-type typedef-type)))))

(defun parse-type-specifier ()
  "Parse a type specifier with modifiers (signed/unsigned, short/long)"
  ;; Handle enum and struct first
  (cond
    ((check-token 'keyword "enum")
     (parse-enum-specifier))
    ((check-token 'keyword "struct")
     (parse-struct-specifier))
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
  "Parse a full type including pointer levels"
  (let ((base-type (parse-type-specifier))
        (ptr-level 0))
    ;; Count pointer stars
    (loop while (match-token 'operator "*")
          do (incf ptr-level))
    (make-type-desc :base (type-desc-base base-type)
                    :pointer-level ptr-level
                    :array-size nil
                    :size (type-desc-size base-type)
                    :unsigned-p (type-desc-unsigned-p base-type)
                    :struct-tag (type-desc-struct-tag base-type))))

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
                                      "enum" "struct")
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
                  :value (token-value tok)
                  :result-type (make-int-type)))

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

    ;; Empty statement
    ((match-token 'punctuation ";")
     (make-node 'empty))

    ;; Declaration or expression statement
    (t
     (if (is-type-token (current-token))
         (parse-local-declaration)
         (parse-expression-statement)))))

(defun is-type-token (tok)
  "Check if token starts a type declaration"
  (and tok
       (or (and (eq (token-type tok) 'keyword)
                (member (token-value tok)
                        '("int" "char" "void" "unsigned" "signed" "short" "long"
                          "int8_t" "uint8_t" "int16_t" "uint16_t" "int32_t" "uint32_t"
                          "enum" "struct")
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

(defun parse-local-declaration ()
  "Parse a local variable declaration"
  (let ((var-type (parse-type))
        (decls nil))
    ;; Parse declarators
    (loop
      (let* ((name (token-value (expect-token 'identifier)))
             (array-size nil))
        ;; Check for array declaration
        (when (match-token 'punctuation "[")
          (when (check-token 'number)
            (setf array-size (token-value (advance-token))))
          (expect-token 'punctuation "]")
          (setf var-type (make-type-desc :base (type-desc-base var-type)
                                         :pointer-level (type-desc-pointer-level var-type)
                                         :array-size array-size
                                         :size (type-desc-size var-type)
                                         :unsigned-p (type-desc-unsigned-p var-type)
                                         :struct-tag (type-desc-struct-tag var-type))))

        ;; Check for initializer
        (let ((init nil))
          (when (match-token 'operator "=")
            (setf init (parse-assignment-expression)))

          ;; Decide between register and stack allocation
          ;; Use register for scalar (non-array) variables when registers available
          ;; and the variable doesn't have its address taken
          ;; Limit to 4 local regs (R6-R9) for physical mode compatibility
          (let ((local-reg-count (compiler-state-local-reg-count *state*)))
            (if (and (not array-size)               ; Not an array
                     (not (eq (type-desc-base var-type) 'struct)) ; Not a struct
                     (< local-reg-count 4)          ; Have free local regs (R6-R9)
                     (not (is-address-taken name))) ; Address not taken
                ;; Register allocation
                (progn
                  (add-symbol name var-type :register local-reg-count)
                  (incf (compiler-state-local-reg-count *state*)))
                ;; Stack allocation (arrays or when out of registers)
                (let* ((size (if array-size
                                 (* array-size (type-size var-type))
                                 (type-size var-type)))
                       (elem-size (type-size var-type))
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
                decls)))

      ;; Check for more declarators
      (unless (match-token 'punctuation ",")
        (return)))

    (expect-token 'punctuation ";")
    (make-node 'decl-list :children (reverse decls))))

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

(defun parse-function-definition (ret-type name &optional is-inline)
  "Parse a function definition"
  (setf (compiler-state-current-function *state*) name)
  (setf (compiler-state-local-offset *state*) 0)
  (setf (compiler-state-param-count *state*) 0)
  (setf (compiler-state-local-reg-count *state*) 0)  ; Reset local register count

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

    ;; Parse function body
    (let ((body (parse-compound-statement)))
      (exit-scope)

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
  "Parse a global variable declaration"
  (let ((decls nil))
    ;; Add first declarator
    (let ((array-size nil))
      (when (match-token 'punctuation "[")
        (when (check-token 'number)
          (setf array-size (token-value (advance-token))))
        (expect-token 'punctuation "]"))

      (add-symbol name var-type :global nil)
      (push (make-node 'global-var
                       :value name
                       :result-type var-type)
            decls))

    ;; Parse additional declarators
    (loop while (match-token 'punctuation ",")
          do (let ((next-name (token-value (expect-token 'identifier))))
               (add-symbol next-name var-type :global nil)
               (push (make-node 'global-var
                                :value next-name
                                :result-type var-type)
                     decls)))

    (expect-token 'punctuation ";")
    (make-node 'decl-list :children (reverse decls))))

(defun parse-top-level ()
  "Parse a top-level declaration or function definition"
  ;; Check for typedef first
  (when (check-token 'keyword "typedef")
    (return-from parse-top-level (parse-typedef-declaration)))
  ;; Check for inline keyword
  (let ((is-inline (match-token 'keyword "inline")))
    ;; Parse type and name
    (let* ((type-spec (parse-type)))

      ;; Check for standalone enum declaration (no variable name)
      (when (match-token 'punctuation ";")
        ;; Standalone enum definition - constants already added during parse-enum-specifier
        (return-from parse-top-level
          (make-node 'enum-decl :result-type type-spec)))

      ;; Normal case: expect identifier for variable/function name
      (let ((name (token-value (expect-token 'identifier))))
        ;; Function definition or declaration?
        (if (check-token 'punctuation "(")
            (parse-function-definition type-spec name is-inline)
            (parse-global-declaration type-spec name))))))

(defun parse-program ()
  "Parse a complete program"
  (let ((declarations nil))
    (loop until (at-end)
          do (push (parse-top-level) declarations))
    (make-node 'program :children (reverse declarations))))
