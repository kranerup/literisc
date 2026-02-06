;;; ===========================================================================
;;; ========================= C Lexer for liteRISC ============================
;;; ===========================================================================

(in-package :c-compiler)

;;; ===========================================================================
;;; Character Classification
;;; ===========================================================================

(defun whitespace-p (char)
  (member char '(#\Space #\Tab #\Newline #\Return)))

(defun digit-p (char)
  (and char (digit-char-p char)))

(defun hex-digit-p (char)
  (and char (or (digit-char-p char)
                (member (char-downcase char) '(#\a #\b #\c #\d #\e #\f)))))

(defun alpha-p (char)
  (and char (or (alpha-char-p char) (char= char #\_))))

(defun alphanumeric-p (char)
  (and char (or (alpha-p char) (digit-p char))))

;;; ===========================================================================
;;; Keywords
;;; ===========================================================================

(defparameter *keywords*
  '("int" "char" "void" "if" "else" "while" "for" "do"
    "return" "break" "continue" "sizeof" "struct" "union"
    "typedef" "static" "extern" "const" "volatile" "unsigned" "signed"
    "short" "long" "switch" "case" "default" "enum" "goto"
    "inline"
    ;; C99 fixed-width integer types
    "int8_t" "uint8_t" "int16_t" "uint16_t" "int32_t" "uint32_t"))

(defun keyword-p (str)
  (member str *keywords* :test #'string=))

;;; ===========================================================================
;;; Operators (multi-character)
;;; ===========================================================================

(defparameter *multi-char-operators*
  '("++" "--" "+=" "-=" "*=" "/=" "%=" "&=" "|=" "^="
    "<<" ">>" "<<=" ">>=" "==" "!=" "<=" ">=" "&&" "||"
    "->" "..."))

(defun starts-multi-char-op (char)
  "Check if char could start a multi-character operator"
  (member char '(#\+ #\- #\* #\/ #\% #\& #\| #\^ #\< #\> #\= #\! #\.)))

;;; ===========================================================================
;;; Lexer State
;;; ===========================================================================

(defstruct lexer-state
  source     ; source string
  pos        ; current position
  line       ; current line
  column)    ; current column

(defvar *lexer* nil)

(defun lexer-peek (&optional (offset 0))
  "Peek at current or future character without consuming"
  (let ((pos (+ (lexer-state-pos *lexer*) offset)))
    (if (< pos (length (lexer-state-source *lexer*)))
        (char (lexer-state-source *lexer*) pos)
        nil)))

(defun lexer-advance ()
  "Consume and return current character"
  (let ((char (lexer-peek)))
    (when char
      (incf (lexer-state-pos *lexer*))
      (if (char= char #\Newline)
          (progn
            (incf (lexer-state-line *lexer*))
            (setf (lexer-state-column *lexer*) 1))
          (incf (lexer-state-column *lexer*))))
    char))

(defun lexer-at-end ()
  (>= (lexer-state-pos *lexer*) (length (lexer-state-source *lexer*))))

(defun current-location ()
  "Return current source location as (line . column)"
  (cons (lexer-state-line *lexer*)
        (lexer-state-column *lexer*)))

;;; ===========================================================================
;;; Token Creation
;;; ===========================================================================

(defun make-tok (type value)
  (make-token :type type
              :value value
              :line (lexer-state-line *lexer*)
              :column (lexer-state-column *lexer*)))

;;; ===========================================================================
;;; Lexer Functions
;;; ===========================================================================

(defun skip-whitespace ()
  "Skip whitespace characters"
  (loop while (and (not (lexer-at-end))
                   (whitespace-p (lexer-peek)))
        do (lexer-advance)))

(defun skip-line-comment ()
  "Skip // comment to end of line"
  (lexer-advance) ; skip first /
  (lexer-advance) ; skip second /
  (loop while (and (not (lexer-at-end))
                   (not (char= (lexer-peek) #\Newline)))
        do (lexer-advance))
  (when (not (lexer-at-end))
    (lexer-advance))) ; skip newline

(defun skip-block-comment ()
  "Skip /* ... */ comment"
  (lexer-advance) ; skip /
  (lexer-advance) ; skip *
  (loop while (and (not (lexer-at-end))
                   (not (and (char= (lexer-peek) #\*)
                             (char= (lexer-peek 1) #\/))))
        do (lexer-advance))
  (when (not (lexer-at-end))
    (lexer-advance)   ; skip *
    (lexer-advance))) ; skip /

(defun skip-whitespace-and-comments ()
  "Skip all whitespace and comments"
  (loop
    (skip-whitespace)
    (cond
      ((lexer-at-end) (return))
      ((and (char= (lexer-peek) #\/)
            (char= (lexer-peek 1) #\/))
       (skip-line-comment))
      ((and (char= (lexer-peek) #\/)
            (char= (lexer-peek 1) #\*))
       (skip-block-comment))
      (t (return)))))

(defun scan-number ()
  "Scan a numeric literal"
  (let ((start-col (lexer-state-column *lexer*))
        (start-line (lexer-state-line *lexer*))
        (chars nil)
        (base 10))
    ;; Check for hex prefix
    (when (and (char= (lexer-peek) #\0)
               (or (char= (lexer-peek 1) #\x)
                   (char= (lexer-peek 1) #\X)))
      (lexer-advance) ; skip 0
      (lexer-advance) ; skip x
      (setf base 16))

    ;; Scan digits
    (if (= base 16)
        (loop while (hex-digit-p (lexer-peek))
              do (push (lexer-advance) chars))
        (loop while (digit-p (lexer-peek))
              do (push (lexer-advance) chars)))

    ;; Parse the number
    (let ((numstr (coerce (reverse chars) 'string)))
      (make-token :type 'number
                  :value (parse-integer numstr :radix base)
                  :line start-line
                  :column start-col))))

(defun scan-identifier ()
  "Scan an identifier or keyword"
  (let ((start-col (lexer-state-column *lexer*))
        (start-line (lexer-state-line *lexer*))
        (chars nil))
    ;; Scan identifier characters
    (loop while (alphanumeric-p (lexer-peek))
          do (push (lexer-advance) chars))

    (let ((name (coerce (reverse chars) 'string)))
      (make-token :type (if (keyword-p name) 'keyword 'identifier)
                  :value name
                  :line start-line
                  :column start-col))))

(defun scan-string ()
  "Scan a string literal"
  (let ((start-col (lexer-state-column *lexer*))
        (start-line (lexer-state-line *lexer*))
        (chars nil))
    (lexer-advance) ; skip opening "

    (loop while (and (not (lexer-at-end))
                     (not (char= (lexer-peek) #\")))
          do (let ((char (lexer-advance)))
               (if (char= char #\\)
                   ;; Handle escape sequences
                   (let ((next (lexer-advance)))
                     (push (case next
                             (#\n #\Newline)
                             (#\t #\Tab)
                             (#\r #\Return)
                             (#\0 #\Nul)
                             (#\\ #\\)
                             (#\" #\")
                             (otherwise next))
                           chars))
                   (push char chars))))

    (when (not (lexer-at-end))
      (lexer-advance)) ; skip closing "

    (make-token :type 'string
                :value (coerce (reverse chars) 'string)
                :line start-line
                :column start-col)))

(defun scan-char-literal ()
  "Scan a character literal"
  (let ((start-col (lexer-state-column *lexer*))
        (start-line (lexer-state-line *lexer*)))
    (lexer-advance) ; skip opening '

    (let ((char (if (char= (lexer-peek) #\\)
                    (progn
                      (lexer-advance) ; skip backslash
                      (let ((next (lexer-advance)))
                        (case next
                          (#\n #\Newline)
                          (#\t #\Tab)
                          (#\r #\Return)
                          (#\0 #\Nul)
                          (#\\ #\\)
                          (#\' #\')
                          (otherwise next))))
                    (lexer-advance))))

      (when (char= (lexer-peek) #\')
        (lexer-advance)) ; skip closing '

      (make-token :type 'number
                  :value (char-code char)
                  :line start-line
                  :column start-col))))

(defun scan-operator ()
  "Scan an operator (single or multi-character)"
  (let ((start-col (lexer-state-column *lexer*))
        (start-line (lexer-state-line *lexer*))
        (char1 (lexer-peek))
        (char2 (lexer-peek 1))
        (char3 (lexer-peek 2)))

    ;; Try to match multi-character operators (longest first)
    (let ((three-char (when (and char1 char2 char3)
                        (coerce (list char1 char2 char3) 'string)))
          (two-char (when (and char1 char2)
                      (coerce (list char1 char2) 'string))))

      (cond
        ;; Three-character operators
        ((and three-char (member three-char '("<<=" ">>=") :test #'string=))
         (lexer-advance)
         (lexer-advance)
         (lexer-advance)
         (make-token :type 'operator
                     :value three-char
                     :line start-line
                     :column start-col))

        ;; Two-character operators
        ((and two-char (member two-char *multi-char-operators* :test #'string=))
         (lexer-advance)
         (lexer-advance)
         (make-token :type 'operator
                     :value two-char
                     :line start-line
                     :column start-col))

        ;; Single-character operator
        (t
         (lexer-advance)
         (make-token :type 'operator
                     :value (string char1)
                     :line start-line
                     :column start-col))))))

(defun scan-punctuation ()
  "Scan punctuation characters"
  (let ((start-col (lexer-state-column *lexer*))
        (start-line (lexer-state-line *lexer*))
        (char (lexer-advance)))
    (make-token :type 'punctuation
                :value (string char)
                :line start-line
                :column start-col)))

(defun next-token ()
  "Scan and return the next token"
  (skip-whitespace-and-comments)

  (when (lexer-at-end)
    (return-from next-token nil))

  (let ((char (lexer-peek)))
    (cond
      ;; Numbers
      ((digit-p char)
       (scan-number))

      ;; Identifiers and keywords
      ((alpha-p char)
       (scan-identifier))

      ;; String literals
      ((char= char #\")
       (scan-string))

      ;; Character literals
      ((char= char #\')
       (scan-char-literal))

      ;; Operators (check before punctuation since some overlap)
      ;; Note: ? is included here for the ternary conditional operator
      ((member char '(#\+ #\- #\* #\/ #\% #\& #\| #\^ #\< #\> #\= #\! #\~ #\?))
       (scan-operator))

      ;; Punctuation
      ((member char '(#\( #\) #\{ #\} #\[ #\] #\; #\, #\. #\:))
       (scan-punctuation))

      ;; Unknown character
      (t
       (let ((start-col (lexer-state-column *lexer*))
             (start-line (lexer-state-line *lexer*)))
         (lexer-advance)
         (make-token :type 'unknown
                     :value (string char)
                     :line start-line
                     :column start-col))))))

;;; ===========================================================================
;;; Main Tokenize Function
;;; ===========================================================================

(defun tokenize (source)
  "Tokenize the source string and return a list of tokens"
  (let ((*lexer* (make-lexer-state :source source
                                   :pos 0
                                   :line 1
                                   :column 1)))
    (loop for token = (next-token)
          while token
          collect token)))
