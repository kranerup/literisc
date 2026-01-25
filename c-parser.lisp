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
;;; Type Parsing
;;; ===========================================================================

(defun parse-type-specifier ()
  "Parse a type specifier with modifiers (signed/unsigned, short/long)"
  ;; Handle C99 fixed-width types first
  (cond
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
                    :unsigned-p (type-desc-unsigned-p base-type))))

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
              (and next (eq (token-type next) 'keyword)
                   (member (token-value next)
                           '("int" "char" "void" "unsigned" "signed" "short" "long"
                             "int8_t" "uint8_t" "int16_t" "uint16_t" "int32_t" "uint32_t")
                           :test #'string=))))
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

        ;; Member access (. and ->)
        ((or (match-token 'operator ".")
             (match-token 'operator "->"))
         (let ((member-name (token-value (expect-token 'identifier))))
           (setf expr (make-node 'member
                                 :value member-name
                                 :children (list expr)))))

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
       (eq (token-type tok) 'keyword)
       (member (token-value tok)
               '("int" "char" "void" "unsigned" "signed" "short" "long"
                 "int8_t" "uint8_t" "int16_t" "uint16_t" "int32_t" "uint32_t")
               :test #'string=)))

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
                                         :unsigned-p (type-desc-unsigned-p var-type))))

        ;; Check for initializer
        (let ((init nil))
          (when (match-token 'operator "=")
            (setf init (parse-assignment-expression)))

          ;; Calculate stack offset with alignment
          (let* ((size (if array-size
                           (* array-size (type-size var-type))
                           (type-size var-type)))
                 ;; Align offset based on variable size (4-byte alignment for int/pointers)
                 (elem-size (type-size var-type))
                 (alignment (if (>= elem-size 4) 4
                               (if (>= elem-size 2) 2 1)))
                 (current-offset (compiler-state-local-offset *state*))
                 ;; Round down to align (negative stack growth)
                 (aligned-offset (- current-offset
                                    (mod current-offset alignment)))
                 (offset (- aligned-offset size)))
            (setf (compiler-state-local-offset *state*) offset)

            ;; Add to symbol table
            (add-symbol name var-type :local offset)

            (push (make-node 'var-decl
                             :value name
                             :children (when init (list init))
                             :result-type var-type)
                  decls))))

      ;; Check for more declarators
      (unless (match-token 'punctuation ",")
        (return)))

    (expect-token 'punctuation ";")
    (make-node 'decl-list :children (reverse decls))))

;;; ===========================================================================
;;; Top-Level Parsing
;;; ===========================================================================

(defun parse-function-definition (ret-type name)
  "Parse a function definition"
  (setf (compiler-state-current-function *state*) name)
  (setf (compiler-state-local-offset *state*) 0)
  (setf (compiler-state-param-count *state*) 0)

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
      (let ((frame-size (- (compiler-state-local-offset *state*))))
        ;; Create function node
        (make-node 'function
                   :value name
                   :children (list (make-node 'params :children (reverse params))
                                   body)
                   :result-type ret-type
                   :data (list :frame-size frame-size
                               :param-count param-idx))))))

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
  ;; Parse type and name
  (let* ((type-spec (parse-type))
         (name (token-value (expect-token 'identifier))))

    ;; Function definition or declaration?
    (if (check-token 'punctuation "(")
        (parse-function-definition type-spec name)
        (parse-global-declaration type-spec name))))

(defun parse-program ()
  "Parse a complete program"
  (let ((declarations nil))
    (loop until (at-end)
          do (push (parse-top-level) declarations))
    (make-node 'program :children (reverse declarations))))
