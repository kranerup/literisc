(asdf:load-system :literisc)
(ql:quickload :literisc)
(use-package :lr-asm)
(use-package :lr-emulator)
(use-package :unit)
(use-package :lr-soc)

;;; The main data structure is the cons table. It consists
;;; of a vector of cons cells and a cons type vector.
;;; The cons vector holds both actual cons cells but also
;;; all other data needed such as integers and symbols.
;;; By choosing a 32 bit cons cell it can hold either
;;; a 32 bit integer or car/cdr pointers. With this approach
;;; integers can be full size without using tag bits in a 32 bit word.
;;; Cons cells can have 16 bit car/cdr pointers which limits the
;;; amount of cons cells to 65k. This is a reasonable limitation for
;;; small embedded processors which is the target for this lisp implementation.
;;; Strings used in symbols are kept in a separate string space so that
;;; their memory can be packed efficiently.
;;;
;;; cons-type: [ t:8 ]  (msb..lsb)
;;;   [ 0:4 | gc:1 | type:3 ]
;;;   type: 0 - free
;;;         1 - number
;;;         2 - symbol
;;;         3 - char
;;;         4 - cons
;;;         5 - macro
;;;         6 - function
;;;         7 - primitive
;;;
;;; cons:    [ f1:16    |    f2:16 ]
;;;          [         f:32        ]
;;;
;;;             msb    ...    lsb
;;;          [   f1     |       f2 ]
;;; byte        3  2          1  0
;;;
;;;   type   | f1 (cdr) | f2 (car)  |
;;;   -------+----------+-----------+
;;;   symbol | name-ptr | value-ptr |
;;;     name-ptr:string space ptr
;;;     value-ptr:cons cell ptr
;;;
;;;   cons   | cdr-ptr  | car-ptr   |
;;;     car/cdr-ptr: cons cell ptr

;;;          |          f           |
;;;   -------+----------------------+
;;;   number |        value         |
;;;   char   |        value         |
;;;   prim   |       code-ptr       |
;;;   
;;; Cons pointers are relative indexes in the
;;; cons table with each entry being a complete cons
;;; cell (so not byte indexes but 4-bytes per index).
;;;
;;; The symbols name-ptr is a relative pointer into the string
;;; space. Each symbol name is a zero terminated string.
;;; Symbol names are unique. Adding a symbol therefore requires checking that
;;; it doesn't exist already in which case it is reused. 
;;; Two symbol conses therefore has the same symbol name if their name-ptr
;;; values are the same.
;;;
;;; Primitives are assembler code functions. The code-ptr is
;;; the address of the machine code.
;;;
;;; It is assumed that the cons table is initialized with nil, t and error at
;;; position 0,1,2 to simplify the code.
;;; 0: sym ["nil" 0]
;;; 1: sym ["t", 0]
;;; 2: sym ["error", 0]

;;; An environment is a list of symbol-conses.
;;;
;;;  env->[*|*]--->NIL
;;;        |
;;;        v
;;;       NIL
;;;
;;;  env->[*|*]--->NIL
;;;        |
;;;        v
;;;       [*|*]--->1
;;;        |
;;;        v
;;;        A
;;;       
;;;  env->[*|*]------->[*|*]--->NIL
;;;        |            |
;;;        v            v
;;;       [*|*]--->1   [*|*]--->2
;;;        |            |
;;;        v            v
;;;        A            B
;;;

(defvar dmem nil)
(defvar dmem-allocated 0)
(setq dmem (make-dmem 4000))

(defmacro alloc-init ( sym data )
  `(progn
     (defvar ,sym nil)
     (setq ,sym dmem-allocated)
     (let* ((d ,data)
            (d-len (list-length d)))
       (set-program dmem d dmem-allocated)
       (setq dmem-allocated (+ dmem-allocated d-len)))
     (assert (< dmem-allocated (length dmem)))))

(defmacro alloc ( sym len )
  `(progn
     (defvar ,sym nil)
     (setq ,sym dmem-allocated)
     (setq dmem-allocated (+ dmem-allocated ,len))
     (assert (< dmem-allocated (length dmem)))
     ,sym))

(defun word-align (addr)
  (if (equal (logand 1 addr) 0)
      addr
      (1+ addr)))
             
(defun dword-align (addr)
  (if (equal (logand #b11 addr) 0)
      addr
      (+ addr (- 4 (logand #b11 addr)))))

;;; allocate 16-bit words, word aligned
(defmacro alloc-words (sym words)
  `(progn
     (defvar ,sym nil)
     (setq dmem-allocated (word-align dmem-allocated))
     (setq ,sym dmem-allocated)
     (setq dmem-allocated (+ dmem-allocated (* 2 ,words)))
     (assert (< dmem-allocated (length dmem)))))

;;; allocate 32-bit words, word aligned
(defmacro alloc-dwords (sym words)
  `(progn
     (defvar ,sym nil)
     (setq dmem-allocated (dword-align dmem-allocated))
     (setq ,sym dmem-allocated)
     (setq dmem-allocated (+ dmem-allocated (* 4 ,words)))
     (assert (< dmem-allocated (length dmem)))))

(defvar string-space-free 0) ; relative n-string-space
(defparameter source-region-size 256) ; allocated bytes to source buffer
(defvar cons-free nil)
(defvar n-cons-end nil)


(defun cons-cell (f1 f2)
  (logior (logand #xFFFF f2)
          (ash (logand #xFFFF f1) 16)))
(defun cons-bytes ( cell )
  (integer-to-byte-list cell cons-size))

    
(defconstant c-cons-free      0)
(defconstant c-cons-number    1)
(defconstant c-cons-symbol    2)
(defconstant c-cons-char      3)
(defconstant c-cons-cons      4)
(defconstant c-cons-macro     5)
(defconstant c-cons-func      6)
(defconstant c-cons-primitive 7)

(defconstant c-cons-gc-mask     #b00001000)
(defconstant c-cons-pin-mask    #b00010000)
(defconstant c-cons-marked-mask #b00011000)
(defconstant c-cons-type-mask   #b00000111)

(defparameter cons-type-names (make-hash-table))
(setf (gethash 0 cons-type-names) "free")
(setf (gethash 1 cons-type-names) "num")
(setf (gethash 2 cons-type-names) "sym")
(setf (gethash 3 cons-type-names) "char")
(setf (gethash 4 cons-type-names) "cons")
(setf (gethash 5 cons-type-names) "macr")
(setf (gethash 6 cons-type-names) "func")
(setf (gethash 7 cons-type-names) "prim")

(defvar n-stack-highest nil)

;;; -- sym-strings --
(defun add-symbol (dmem sym-name value-ptr &optional (verbose nil))
  (let ((str (string-to-mem sym-name))
        (str-ptr (find-in-string-space dmem sym-name))
        (cons-ptr cons-free)) ; cons index, not address
   
    (when str-ptr
      (setf str-ptr (- str-ptr n-string-space))
      (when verbose (format t "reuse string ~d~%" str-ptr)))

    (when (not str-ptr)
      (set-program dmem str (+ n-string-space string-space-free))
      (setf str-ptr string-space-free)
      (setq string-space-free (+ string-space-free (list-length str)))
      (mem-write-word dmem n-string-space-free string-space-free))

    (if verbose (format t "add-symbol str at:~x~%" (+ n-string-space string-space-free)))
    (if verbose (format t "str-ptr ~x~%" str-ptr))

    (set-program dmem (list c-cons-symbol )
                 (+ n-cons-type cons-ptr)) ; one byte per cons-type item
    (if verbose (format t "cons-type ~d at ~x~%"  c-cons-symbol (+ n-cons-type cons-ptr)))
    ;(format t "ccell ~x cbytes ~a~%"
    ;        (cons-cell str-ptr value-ptr)
    ;        (cons-bytes (cons-cell str-ptr value-ptr)))

    (set-program dmem
                 (cons-bytes 
                   (cons-cell str-ptr value-ptr))
                 (+ n-cons (* cons-size cons-ptr)))
    (if verbose (format t "cons-cell at ~x car:~x cdr:~x~%"
                        (+ n-cons (* cons-size cons-ptr))
                        str-ptr value-ptr))

    (setf cons-free (1+ cons-free))
    (setf (aref dmem n-cons-free) (logand #xff cons-free))
    (setf (aref dmem (1+ n-cons-free)) (ash cons-free -8))
    cons-ptr))

;(set-program dmem (string-to-mem "symbol2") (+ (* sym-length n-sym-strings)))

;(set-program dmem (string-to-mem "symbol next ( inside ) end") source-start)
;(set-program dmem (string-to-mem "symbol") source-start)
;(set-program dmem (string-to-mem "symbol") sym-string-start)


(defun add-num (dmem number &optional (verbose nil))
  (let ((cons-ptr cons-free)) ; cons index, not address
    (if verbose (format t "add-num ~d at:~d~%" number cons-free))

    (mem-write-byte dmem (+ n-cons-type cons-ptr) c-cons-number) ; one byte per cons-type item 

    (if verbose (format t "cons-type ~d at ~x~%"  c-cons-number (+ n-cons-type cons-ptr)))

    (mem-write-dword dmem (+ n-cons (* cons-size cons-ptr)) number)

    (setf cons-free (1+ cons-free))
    (mem-write-word dmem n-cons-free cons-free)
    cons-ptr))

(defun add-prim (dmem asm-label &optional (verbose nil))
  (let ((code-ptr (get-label asm-label *symtab*))
        (cons-ptr cons-free)) ; cons index, not address
    (if verbose (format t "add-prim ~a ~d at:~d~%" asm-label code-ptr cons-free))

    (mem-write-byte dmem (+ n-cons-type cons-ptr) c-cons-primitive) ; one byte per cons-type item 

    (if verbose (format t "cons-type ~d at ~x~%"  c-cons-primitive (+ n-cons-type cons-ptr)))

    (mem-write-dword dmem (+ n-cons (* cons-size cons-ptr)) code-ptr)

    (setf cons-free (1+ cons-free))
    (mem-write-word dmem n-cons-free cons-free)
    cons-ptr))

(defun set-sym-val (dmem symb val)
  (mem-write-word dmem (+ n-cons (* cons-size symb)) val))

(defun mem-write-dword ( dmem addr word32b )
  (assert (equal (logand addr 3) 0))
  (setf (aref dmem addr) (logand #xff word32b))
  (setf (aref dmem (1+ addr)) (ash word32b -8))
  (setf (aref dmem (+ 2 addr)) (ash word32b -16))
  (setf (aref dmem (+ 3 addr)) (ash word32b -24)))

(defun mem-write-word ( dmem addr word16b )
  (assert (equal (logand addr 1) 0))
  (setf (aref dmem addr) (logand #xff word16b))
  (setf (aref dmem (1+ addr)) (ash word16b -8)))

(defun mem-write-byte ( dmem addr word8b )
  (setf (aref dmem addr) (logand #xff word8b)))

(defun make-cons (dmem the-car the-cdr)
  (let ((cons-ptr cons-free))
    (set-program dmem
                 (cons-bytes (cons-cell the-cdr the-car))
                 (+ n-cons (* cons-size cons-ptr)))
    (mem-write-byte dmem (+ n-cons-type cons-ptr) c-cons-cons)
    (setf cons-free (1+ cons-free))
    (mem-write-word dmem n-cons-free cons-free)
    cons-ptr))

;;; prepends a sym/val to the environment and returns the new env
;(defun pair ( sym val env )
;  (cons (cons sym val) env ))
(defun pair (dmem sym val env)
  (let* ((symbol-cons (make-cons dmem sym val))
         (head (make-cons dmem symbol-cons env)))
    head))

(defun push-env (dmem symbol-cons env)
  (make-cons dmem symbol-cons env))


(defun print-string (dmem start)
  (loop for i from start 
        until (equal (aref dmem i) 0)
        do (format t "~c" (code-char (aref dmem i)))
        finally (return (1+ i))))

(defun next-string (dmem start)
  (loop for i from start 
        until (equal (aref dmem i) 0)
        finally (return (1+ i))))

(defun equal-str-mem (dmem str start)
  (loop for memidx from start
        for stridx from 0
        do (cond ( (not (equal (aref dmem memidx) (aref str stridx)))
                   (return-from equal-str-mem nil))
                   ((equal (aref str stridx) 0)
                    (return-from equal-str-mem t)))))

(defun find-in-string-space (dmem str)
  (let ((memstr (coerce (append (map 'list #'char-code str) (list 0)) 'vector)))
    (loop with i = n-string-space
          do (cond ((>= i (+ n-string-space string-space-free))
                    (return-from find-in-string-space nil))
                   ((equal-str-mem dmem memstr i)
                    (return-from find-in-string-space i))
                   (t (setf i (next-string dmem i)))))))

(defun print-string-space (dmem)
  (loop with i = n-string-space
        until (>= i (+ n-string-space string-space-free))
        do (format t "~d ~d: " (- i n-string-space) i)
        do (setf i (print-string dmem i))
        do (format t "~%")))

;; ----------------------------------------------------------------------------
(defun init-lisp ()
  (setq dmem-allocated 0)
  (loop for idx from 0 to (1- (length dmem))
        do (setf (aref dmem idx) 0))
  ;;(alloc-init n-source-start     (string-to-mem "symbol symbol2 ( inner )"))
  ;;(alloc-init n-sym-string-start (string-to-mem "symbol"))
  (format t "n-source-start: ~a~%" (alloc n-source-start source-region-size))
  (format t "n-sym-string-start: ~a~%" (alloc n-sym-string-start 32))

  (alloc-init n-print-sep (string-to-mem
                            (format nil "~C~C> " #\Return #\Linefeed)))

  (format t "n-read-sym-str: ~a~%" (alloc n-read-sym-str 24))

  (format t "n-string-space: ~a~%" (alloc n-string-space 248))
  (alloc-words n-string-space-free 1) ; index to next free byte in string space
  (setq string-space-free 0)

  (defparameter nr-cons 80)
  (defparameter cons-size 4) ; bytes

  (alloc-words n-cons-free 1) ; cons index to next free cons cell
  (mem-write-word dmem n-cons-free 0)

  (format t "n-cons: ~a~%" (alloc-dwords n-cons nr-cons))
  (setq n-cons-end dmem-allocated)
  (format t "n-cons-type: ~a~%" (alloc n-cons-type nr-cons)) ; holds type of each cons cell, one byte per cons
  (setq cons-free 0) ; points to entry nr in n-cons table

  ;; use-unread, last-read, read-ptr
  (alloc-dwords reader-state 4)
  (defparameter rs-use-unread 0)
  (defparameter rs-last-read 4)
  (defparameter rs-read-ptr 8)
  (defparameter rs-eof 12)

  (alloc-words n-global-env 1)
  
  (alloc-dwords n-stack 300)
  (setq n-stack-highest (- (logand dmem-allocated #xfffffffc) 4)))


(init-lisp)

;;; ----------------------------------------------------------------------------
;;; read-c keeps it's state in global memory variables relative
;;; reader-state base address.
;;; 
;;; R0 - reader-state base address
;;;    read/updates the following as offset from base address
;;;    - use-unread (updated)
;;;    - last-read (updated)
;;;    - read-ptr (updated)
;;; P0 - returns char
(defvar read-c nil)
(setq read-c
  '( (label l-read-c)
     (push-srp)
     (push-r R2)
     (Rx= reader-state R0)
     (A=Rx R0) ; A = R0 base-ptr
     (ld-a-rel->r rs-use-unread R1) ; R1 = M[base-ptr+use-unread-offs]
     ;; if use-unread == 0
     (mvi->a 0)
     (sub-r R1)
     (jnz l-use-unread)

     ;; seen eof?
     (A=Rx R0)
     (Rx=M[A+n] rs-eof R1)
     (A= 1)
     (A-=Rx R1)
     (jz l-at-eof)
     
     ;; read-char = M[ read-ptr ]
     (A=Rx R0) ; base-ptr
     (ld-a-rel->r rs-read-ptr R2) ; R2 (read-ptr) = M[ A(base) + read-ptr-offs ]
     (ld.b-r->a R2) ; P0 = A = M[ R2 (read-ptr) ]
     (a->r P0)

     ;; 0? end of read buffer
     (a= 0)
     (A-=Rx P0)
     (jz l-end-buf)

     ;; read-ptr += 1
     (mvi->a 1)
     (add-r R2)
     (a->r R2)

     ;; save read-ptr
     (A=Rx R0) ; base-ptr
     (st-r->a-rel rs-read-ptr R2) ; M[ A(base) + read-ptr-offs ] = R2 (read-ptr)

     ;; update last-read
     ;;   last-read = read-char
     ;; A (base-ptr)
     (label l-update-read)
     (st-r->a-rel rs-last-read P0) ; M[ A(base) + last-read-offs ] = P0 (last-read)
     ;; return
     (pop-r R2)
     (pop-a)
     (j-a)

     ;; use-unread == 1
     (label l-use-unread)
     ;; clear use-unread
     (mvi->r 0 R1) ; R1 = use-unread = 0
     (A=Rx R0) ; A = base-ptr
     (st-r->a-rel rs-use-unread R1) ; M[ A(base) + use-unread-offs ] = R1 (0)

     ;; read-char = last-read
     (ld-a-rel->r rs-last-read P0) ; P0 = M[ A(base) + last-read-offs ]
     ;; return
     (pop-r R2)
     (pop-a)
     (j-a)

     ;; we've seen the char 0 which means that we got to the end of
     ;; the read buffer (eof)
     (label l-end-buf)
     (Rx= 1 R1) ; eof=1
     (A=Rx R0)
     (M[A+n]=Rx rs-eof R1) ; M[ base+rs-eof ] = 1
     ;; we still need to return a char.
     ;; By returning space we terminate any symbol being parsed.
     (Rx= 32 P0) ; return space
     (A=Rx R0) ; A = base-ptr
     (j l-update-read)
     
     ;; after seeing eof we only read from input stream
     (label l-at-eof)
     (jsr l-getchar) ; -> P0
     (jsr l-putchar) ; echo
     (A=Rx R0) ; A = base-ptr
     (j l-update-read)
     ))

(defvar func-find-symbol nil)
(setq func-find-symbol
  '( ;; find-symbol
     ;; P0 - string-ptr, searched symbol
     ;;    - returns true if found
     ;; P1 - returns cons-ptr to symbol
     (label l-find-symbol)
     (push-srp)
     (push-r R5)

     ;; R0 = string-ptr to searched symbol
     ;; R1 = cons-rel-ptr - relative offset
     ;; R2 = cons-table-ptr - absolute address
     ;; R3 = string-space
     ;; R4 = tmp
     ;; R5 = cons-end
     (r->a P0) (a->r R0)
     (mvi->r 0 R1)
     (mvi->r n-cons R2)
     (mvi->r n-string-space R3) ; strings
     (mvi->r n-cons-end R5)

     (label l-next-cons)
     ;; R4 = n-cons + 2
     (r->a R2)
     (a->r R4)
     (mvi->a 2) ; high word (cons-f1)
     (add-r R4)
     (a->r R4)
     ;; read cdr
     (ld.w-r->a R4) ; cdr -> sym-name-ptr
     (add-r R3) ; string-space + sym-name-ptr
     (a->r P1) ; P1 = string-ptr in sym-table
     (r->a R0) (a->r P0)
     ;; P0 = string-ptr searched symbol
     (jsr l-str-equal)

     (mvi->a 0)
     (sub-r P0)
     (jnz l-found-sym)

     ;; cons-rel-ptr += 1
     (mvi->a 1)
     (add-r R1)
     (a->r R1)

     ;; cons-ptr += 4
     (mvi->a 4)
     (add-r R2)
     (a->r R2)

     (sub-r R5) ; equal end of cons table
     (jlo l-next-cons) ; A < cons-end
     ;; end
     (mvi->a 0)
     (a->r P0)
     (j l-ret-find-sym)

     (label l-found-sym) 
     (mvi->a 1)
     (a->r P0) ; P0 = 1, return flag
     (r->a R1)
     (a->r P1) ; P1 = cons-ptr, return sym-cons
    
     (label l-ret-find-sym)
     (pop-r R5)
     (pop-a)
     (j-a)
     ))

(defconstant reader-lpar 1)
(defconstant reader-rpar 2)
(defconstant reader-sym 3)
(defconstant reader-num 4)
(defconstant reader-quote 5)

;;; output:
;;;   P0 - returns read object type
;;;   P1 - number if type is reader-num
;;;   if type is reader-sym then symbol string is in n-read-sym-str
(defvar scan nil)
(setq scan
  '(
    (label f-scan)
    (push-srp)
    (push-r R4)
    ;; R1 - read-sym-ptr
    (mvi->r n-read-sym-str R1)
    ;; R3 - io-ptr
    (mvi->r -1 R3) ; ptr to I/O reg
    ;; R4 - reading-symbol
    (mvi->r 0 R4)

    (label l-rd-more)
    (jsr l-read-c)
    ;; R0 = P0 = char
    (r->a P0) (a->r R0)

    ;; --- if reading-symbol -----
    (mvi->a 0)
    (sub-r R4)
    (jz l-not-rd-sym)

    ;; ----------- reading-symbol --------------
    ;; end of symbol?
    ;; if c in ['(', ')', ' ', ''', 0]:  
    (mvi->r (char-code #\( ) R2)
    (r->a R2)
    (sub-r R0)
    (jz l-end-of-sym-unrd)

    (mvi->r (char-code #\) ) R2)
    (r->a R2)
    (sub-r R0)
    (jz l-end-of-sym-unrd)

    (mvi->r (char-code #\  ) R2)
    (r->a R2)
    (sub-r R0)
    (jz l-end-of-sym)

    (mvi->r (char-code #\' ) R2)
    (r->a R2)
    (sub-r R0)
    (jz l-end-of-sym-unrd)

    (Rx= 10 R2) ; LF
    (A=Rx R2)
    (A-=Rx R0)
    (jz l-end-of-sym)
    
    (Rx= 13 R2) ; CR
    (A=Rx R2)
    (A-=Rx R0)
    (jz l-end-of-sym)

    (mvi->a 0)
    (sub-r R0)
    (jnz l-read-sym-char)

    (label l-end-of-sym-unrd)
    ;; unread-c
    (mvi->r reader-state R2) ; reader-state base-ptr
    (r->a R2) ; A = base-ptr
    ;; last-read = char
    (st-r->a-rel rs-last-read R0) ; M[ A(base) + last-read-offs ] = R0 (char)
    (mvi->r 1 R2) ; use-unread = 1
    (st-r->a-rel rs-use-unread R2) ; M[ A(base) + use-unread-offs ] = R2 (1)

    (label l-end-of-sym)

    ;; try to convert string to number
    (mvi->r n-read-sym-str P0) ; string-ptr = start of read sym buffer
    (jsr l-str2num) ; P0=string-ptr -> P0=num-flag P1=num
    (mvi->a 0)
    (sub-r P0)
    (jz l-ret-sym)
    ;; return num
    (mvi->r reader-num P0)
    ;; P1 is already num
    (j l-reader-ret)
    
    ;; return sym
    (label l-ret-sym)
    (mvi->r reader-sym P0)
    (j l-reader-ret)
   
    ;; else ------- not reading-symbol ---------
    (label l-not-rd-sym)
    ;;  if char == '('
    (mvi->r (char-code #\( ) R2)
    (r->a R2)
    (sub-r R0)
    (jnz l-not-lpar)
    ;;   then return lpar
    (mvi->r reader-lpar P0)
    (j l-reader-ret)
    
    (label l-not-lpar)
    ;;  if char == ')'
    (mvi->r (char-code #\) ) R2)
    (r->a R2)
    (sub-r R0)
    (jnz l-not-rpar)
    ;;   then return rpar
    (mvi->r reader-rpar P0)
    (j l-reader-ret)
   
    (label l-not-rpar)
    ;;  if char == '
    (mvi->r (char-code #\' ) R2)
    (r->a R2)
    (sub-r R0)
    (jnz l-not-quote)
    ;;   then return '
    (mvi->r reader-quote P0)
    (j l-reader-ret)

    (label l-not-quote)
    ;; is it space, CR or LF then just read next char
    (mvi->r (char-code #\ ) R2)
    (r->a R2)
    (sub-r R0) ; ' ' - R0
    (jz l-rd-more) ; loop back and read next char

    (Rx= 10 R2) ; LF
    (A=Rx R2)
    (A-=Rx R0)
    (jz l-rd-more) ; loop back and read next char

    (Rx= 13 R2) ; CR
    (A=Rx R2)
    (A-=Rx R0)
    (jz l-rd-more) ; loop back and read next char

    ;; all other chars are written to sym buffer
    (label l-read-sym-char)
    (r->a R0)
    (st.b-a->r R1) ; M[read-sym-str] = char
    (mvi->a 1)
    (add-r R1) ; read-sym-str++
    (a->r R1)
    (A= 0)
    (M[Rx].b=A R1) ; zero terminate string
    ;; debug print
    ;(r->a R0)
    ;(st-a->r R3); putchar
    ;; reading-symbol = True
    (mvi->r 1 R4)
    (j l-rd-more) ; loop back and read next char

    ;; return
    (label l-reader-ret)
    (pop-r R4)
    (pop-a)
    (j-a)
    
    ))

;;; parse (input from scan):
;;;   if '('
;;;     return list() 
;;;   elif \'
;;;     return quote()
;;;   else
;;;     return atom()
;;;
;;; atom:
;;;   return symbol / number
;;;   symbol = cons-cell with sym-type
;;;   number = cons-cell with num-type
;;;   if symbol found return existing symbol
;;;   else create a new symbol
;;; list:
;;;    if ')'
;;;       return nil
;;;    else
;;        return cons( sym, list() )

(defvar func-str2num nil)
(setq func-str2num
  '( ;; --- str2num ---
     ;; params:  P0 = string-ptr
     ;; returns: P0 = true if number
     ;;          P1 = number
     (label l-str2num)
     (push-r R2)
     (mvi->r 0 P1) ; result

     (label l-num-loop)
     (ld.b-r->a P0)
     (a->r R0) ; the char
     (mvi->a 0)
     (sub-r R0)
     (jz l-num-end)
     
     (mvi->r (1+ (char-code #\9 )) R1)
     (r->a R0)
     (sub-r R1) ; C - '9'
     (jhs-b l-no-num)

     (mvi->r (char-code #\0 ) R1)
     (r->a R0)
     (sub-r R1) ; C - '0'
     (jlo-b l-no-num)

     ; digit, then convert to int
     ; but that is already done: A = C - '0'
     ; result *= 10 -> res << 3 + res + res
     (a->r R0) ; int-digit
     (r->a P1)
     (lsl-a)
     (lsl-a)
     (lsl-a)
     (add-r P1)
     (add-r P1) ; result*10
     (add-r R0) ; + int-digit
     (a->r P1)
     ;; string-ptr++
     (mvi->a 1)
     (add-r P0)
     (a->r P0)
     (j l-num-loop)

     (label l-no-num)
     (mvi->a 0)
     (a->r P0) ; P0 = false
     (j l-num-ret)
     
     (label l-num-end) 
     (mvi->a 1)
     (a->r P0) ; P0 = true

     (label l-num-ret)
     (pop-r R2)
     (r->a SRP)
     (j-a)))
     
(defvar func-parse nil)
(setq func-parse
  '( ;; --- parse ---
     ;; input:
     ;;   P0 - object type from scan()
     ;;   P1 - number if type is numeric
     ;; output:
     ;;   P0 - returns cons ptr to read objects
     (label l-parse)
     (push-srp)
     (push-r R3)

     (mvi->a reader-lpar)
     (sub-r P0)
     (jnz l-parse-quo)

     ;; '('
     (jsr l-list) ; -> P0
     (j l-parse-ret)
     
     ;; quote  -> (cons 'quote (cons (read) nil))
     (label l-parse-quo)
     (A= reader-quote)
     (A-=Rx P0)
     (jnz l-parse-nxt)

     (jsr l-read) ; -> P0
     (A= 0) (Rx=A P1) 
     (jsr l-cons) ; (cons P0=read P1) -> P0 = new cons
     (A=Rx P0)(Rx=A P1)
     (Rx= fixed-cons-quote P0)
     (jsr l-cons) ; (cons P0=quote P1=(cons (read) nil)) -> P0 = new cons
     (j l-parse-ret)
     
     ;; atom
     (label l-parse-nxt)
     ;; number
     (A= reader-num)
     (A-=Rx P0)
     (jnz l-parse-sym)

     (r->a P1) (a->r P0)
     (jsr l-box-int); P0=number -> P0=cons
     (j l-parse-ret)

     ;; symbol
     (label l-parse-sym)
     (mvi->r n-read-sym-str P0)
     (jsr l-find-symbol) ; P0=found-flag P1=symbol
     (mvi->a 1)
     (sub-r P0)
     (jnz l-parse-new-sym)
    
     (r->a P1) (a->r P0) ; P0 = symbol (from l-find-symbol)
    
     (label l-parse-ret)
     (pop-r R3)
     (pop-a)
     (j-a)

     ;; new symbol, copy to string space and box the pointer
     (label l-parse-new-sym)
     (mvi->r n-read-sym-str P0)
     (jsr l-copy-sym) ; copy string from read-sym-str to string space (P0->P0)
     (jsr l-box-sym) ; wrap the pointer in a cons (P0->P0)

     (j l-parse-ret)

     ;; -----------------
     ;; L list() {
     ;;   L x;
     ;;   return scan() == ')' ?
     ;;     nil :
     ;;     !strcmp(buf, ".") ?
     ;;       (x = read(),scan(),x) :
     ;;       (x = parse(),cons(x,list()));
     ;;    }
     ;; list:
     ;;    if ')'
     ;;       return nil
     ;;    else
     ;;       return cons( sym, list() )
     (label l-list)
     (push-srp)
     (push-r R1)

     (jsr f-scan) ; -> P0
     (mvi->a reader-rpar) ; if ')'
     (sub-r P0)
     (jnz l-list-cont)
     ;; return nil
     (mvi->r 0 P0)
     (pop-r R1)
     (pop-a)
     (j-a)

     ;; else 
     ;;   parse()
     ;;   return cons( sym, list() )
     (label l-list-cont)

     (jsr l-parse) ; P0 = scan result -> cons (sym)
     
     (r->a P0) (a->r R0) ; R0 = sym
     (jsr l-cons) ; P0 = cons-cell
     (r->a P0) (a->r R1) ; R1 = cons-cell = P0
     (r->a R0) (a->r P1) ; P1 = sym
     (jsr l-rplca) ; (rplca cons-cell sym)
     (jsr l-list)
     (r->a P0) (a->r P1) ; P1 = list-ret-val = P0
     (r->a R1) (a->r P0) ; P0 = cons-cell
     (jsr l-rplcd) ; (rplcd cons-cell list-ret-val) 
     ;; return cons-cell
     (r->a R1) (a->r P0) ; P0 = cons-cell = R1
     (pop-r R1)
     (pop-a)
     (j-a)

     ;; --- copy-sym ---------------------------
     ;; input: P0 - reader buffer
     ;; output: P0 - start of string in string space
     (label l-copy-sym)
     (push-r R4)

     (Rx= n-string-space R0)
     (Rx= n-string-space-free R1)
     (A=M[Rx].w R1)
     (Rx=A R2) ; save free index as start of string
     (Rx=A R4) ; relative pointer

     (A=Rx R2)
     (A+=Rx R0)
     (Rx=A R0) ; n-string-space + M[n-string-space-free], start of free string space, string dest

     (label l-cp-str)
     (A=M[Rx].b P0) ; src
     (M[Rx].b=A R0) ; dst
     (Rx=A R3) ; char
     (A= 0)
     (A-=Rx R3)
     (jz l-end-cp) ; = 0? end of string
    
     ;; inc pointers
     (A= 1) (A+=Rx P0) (Rx=A P0)
     (A= 1) (A+=Rx R0) (Rx=A R0)
     (A= 1) (A+=Rx R4) (Rx=A R4)
     (j l-cp-str)
    
     (label l-end-cp)
     (A= 1) (A+=Rx R4) ; we exited loop before inc pointers
     (M[Rx].w=A R1) ; update n-string-space-free with the relative pointer
     (A=Rx R2) (Rx=A P0) ; return start of string
     
     (pop-r R4)
     (A=Rx SRP)
     (j-a)
     
))

(defvar func-read nil)
(setq func-read
  '( ;; read
     ;; output: P0 - cons ptr to read result
     (label l-read)
     (push-srp)
     (jsr f-scan)
     (jsr l-parse)
     (pop-a)
     (j-a)))
     
(defvar func-rplca nil)
(setq func-rplca
  '( ;; rplca
     ;; P0 - cons ptr (reg unmodified)
     ;; P1 - value to replace car of cons cell (reg unmodified)
     (label l-rplca)
     (push-r R0)
     (mvi->r n-cons R0)
     (r->a P0)
     (lsl-a)
     (lsl-a)
     (add-r R0) ; n-cons + 4*cons-ptr is car ptr
     (st.w-r->a P1) ; write car
     (pop-r R0)
     (r->a SRP)
     (j-a)))

(defvar func-rplcd nil)
(setq func-rplcd
  '( ;; rplcd
     ;; P0 - cons ptr (reg unmodified)
     ;; P1 - value to replace cdr of cons cell (reg unmodified)
     (label l-rplcd)
     (push-r R0)
     (mvi->r n-cons R0)
     (r->a P0)
     (lsl-a)
     (lsl-a)
     (add-r R0) ; n-cons + 4*cons-ptr
     (a->r R0)
     (mvi->a 2) ; offs for cdr
     (add-r R0)
     (st.w-r->a P1) ; write cdr
     (pop-r R0)
     (r->a SRP)
     (j-a)))
     
(defvar func-cons nil)
(setq func-cons
  '( 
     ;; --- alloc box -------------------------------
     ;; input: -
     ;; output: P0 - new allocated cons index
     ;;
     ;; We always start searching for a free cons from
     ;; n-cons-free pointer and upwards. Wrapping to 0
     ;; at end of cons space. 
     (label l-box)
     (push-srp)
     (push-r R5)

     (Rx= n-cons-free R0)
     (A=M[Rx].w R0) ; A = next cons to search from
     (Rx=A P0) ; return value = cons index
     (Rx=A R4) ; original start point
     (Rx= 0 R5) ; have we garbage collected?

     (label l-retry-alloc)

     ;; increment for next allocation
     (A= 1)
     (A+=Rx P0) ; cons-free += 1
     (Rx=A R2) ; R2= next cons-free

     (Rx= nr-cons R1)
     (A-=Rx R1)
     (jnz l-no-wrap)

     (Rx= 0 R2) ; wrap to 0

     (label l-no-wrap)

     (A=Rx R2) ; next cons to try
     (A-=Rx R4) ; start cons
     (jnz l-cont) ; did we reached the start point without finding any free cons
     
     (A= 0)
     (A-=Rx R5) ; have garbage collated?
     (jnz l-out-of-mem) ; gc:ed and still didn't find free cons

     (jsr l-garbage-collect)
     (Rx= 1 R5) ; set that we have gc:ed 
    
     (j l-retry-alloc)

     (label l-out-of-mem) ; halt when we're out of memory
     (j l-out-of-mem)

     (label l-cont)
 
     (Rx= n-cons-type R1)
     (A=Rx R1)
     (A+=Rx P0)
     (Rx=M[A].b R3) ; R3 = cons-type
     ; is the cons cell free?
     (A= c-cons-free)
     (A-=Rx R3)
     (jz l-is-free)
  
     (A=Rx R2) (Rx=A P0) ; cons index
     
     (j l-retry-alloc)
     
     (label l-is-free)

     (A=Rx R2) ; next cons-free
     (M[Rx].w=A R0) ; update n-cons-free

     (pop-r R5)
     (pop-a)
     (j-a)

     
     ;; --- set box type -------------------------------
     ;; input: P0 - cons index
     ;;        P1 - type
     ;; output: --
     (label l-set-type)
     (push-r R0)
     
     (Rx= n-cons-type R0)
     (A=Rx P0)
     (A+=Rx R0) ; A = n-cons-type + cons-idx
     (M[A].b=Rx P1) ; n-cons-type[cons-idx] = P1

     (pop-r R0)
     (A=Rx SRP)
     (j-a)

     ;; --- box-cons -------------------------------
     ;; input: -
     ;; output: P0 - returns a ptr to a new allocated cons cell
     (label l-box-cons)

     (push-srp)
     (push-r R0)

     (jsr l-box) ; -> P0=box
     (A=Rx P0) (Rx=A R0) ; R0 = new box

     ;; set type
     (Rx= c-cons-cons P1)
     (jsr l-set-type)

     (A=Rx R0) (Rx=A P0) ; return new cons
     
     (pop-r R0)
     (pop-a)
     (j-a)

     ;; --- cons -------------------------------------
     ;; allocate a new cons and set car/cdr
     ;; input: P0 - value to be the car
     ;;        P1 - value to be the cdr
     ;; output: P0 - the new cons
     ;;         (P1 - the cdr)
     (label l-cons)
     (push-srp)
     (push-r R1)

     (A=Rx P0) (Rx=A R0) ; R0=car
     (A=Rx P1) (Rx=A R1) ; R1=cdr

     (jsr l-box-cons) ; -> P0

     (A=Rx R0) (Rx=a P1)
     (jsr l-rplca) ; P0=cons P1=the-car

     (A=Rx R1) (Rx=A P1)
     (jsr l-rplcd) ; P0=cons P1=the-cdr
    
     (pop-r R1)
     (pop-a)
     (j-a)
     
     ;; --- box integer -------------------------------
     ;; input: P0 - integer value
     ;; output: P0 - integer cons cell
     (label l-box-int)

     (push-srp)
     (push-r R1)

     (A=Rx P0) (Rx=A R0) ; R0 = save integer
     (jsr l-box) ; -> P0=box
     (A=Rx P0) (Rx=A R1) ; R1 = new box

     ;; set type
     (Rx= c-cons-number P1)
     (jsr l-set-type)

     ;; set value
     (A=Rx R1) (Rx=A P0) ; P0 - cons
     (A=Rx R0) (Rx=A P1) ; P1 - val (integer)
     (jsr l-setcons)
     
     (A=Rx R1) (Rx=A P0) ; return new cons
     
     (pop-r R1)
     (pop-a)
     (j-a)
     
     ;; --- box symbol string -------------------------------
     ;; input: P0 - string
     ;; output: P0 - integer cons cell
     (label l-box-sym)
     (push-srp)
     (push-r R1)

     (A=Rx P0) (Rx=A R0) ; R0 = string
     (jsr l-box) ; -> P0=box
     (A=Rx P0) (Rx=A R1) ; R1 = new box

     ;; set type
     (Rx= c-cons-symbol P1)
     (jsr l-set-type)

     ;; set value
     (A=Rx R1) (Rx=A P0) ; P0 - cons
     (A=Rx R0) (Rx=A P1) ; P1 - val (string)
     (jsr l-rplcd)
     
     (A=Rx R1) (Rx=A P0) ; return new cons
     
     (pop-r R1)
     (pop-a)
     (j-a)

     ;;---------------------------------
     ;; search for free cons cell
     (label l-find-free)
     (push-r R2)
     
     (pop-r R2)
     (A=Rx SRP)
     (j-a)
     
     ))

(defparameter func-gc
  '(
     ;; --- mark and trace reachable -------------------------------
     ;; -- input: P0 - cons to trace
     ;; --        P1 - mark method, 1 - pin, 0 - normal (must be passed unmodified)
     ;; This is a recursive depth-first search. It could consume more
     ;; stack than all the cons:es if all conses are in a long chain.
     ;; This algorithm should be rewritten to use pointer reversal.
     (label l-mark-trace)
     (push-srp)
     (push-r R3)

     (Rx= (1- nr-cons) R0) ; R0 = last index in cons space (first index is 0)

     ;; last cons index < cons index to trace -> outside of cons space, do not follow
     (A=Rx R0)
     (A-=Rx P0)
     (jlo l-trace-no-more)

     ;(Rx= n-cons-type R0)
     ;(A=Rx P0) ; cons-idx
     ;(A+=Rx R0) ; A = type-ptr = n-cons-type + cons-idx
     ;(Rx=A R3) ; R3 = type-ptr
     ;(Rx=M[A].b R2) ; R2 = type = n-cons-type[cons-idx]

     ;(Rx= c-cons-gc-mask R1) ; gc-bit
     ;(A=Rx R2) ; type
     ;(A&=Rx R1) ; type & gc-bit
     ;(A-=Rx R1)
     ;(jz l-already-done)

     ;;; mark gc:ed before recursing to avoid endless loops
     ;(Rx= c-cons-gc-mask R1) ; gc-bit
     ;(A=Rx R1) ; gc-bit
     ;(A\|=Rx R2) ; set gc-bit
     ;(M[Rx].b=A R3) ; write back to n-cons-type[idx]

     (jsr l-mark) ; P0,P1 -> P0,P1,P2(type),P3(marked)
     (A= 0)
     (A-=Rx P3) ; already marked?
     (jnz l-already-done)
     
     (Rx= c-cons-type-mask R1) ; type-mask

     (A=Rx R1) ; mask
     (A&=Rx P2) ; type w/o gc bit
     (Rx=A R0) ; R0 = type-wo-gc

     ;; cons and symbol are the only types that contains cons pointer
     (A= c-cons-cons)
     (A-=Rx R0)
     (jz l-trace-cons)

     (A= c-cons-symbol)
     (A-=Rx R0)
     (jz l-trace-symbol)

     ;; no cons pointers in this type
     (label l-trace-no-more)

     (label l-already-done)
     (pop-r R3)
     (pop-a)
     (j-a)

     ;; --- cons cell: trace car then cdr
     (label l-trace-cons)
     (A=Rx P0) (Rx=A R0) ; save P0 cons idx
     (jsr l-car)
     (jsr l-mark-trace)
     (A=Rx R0) (Rx=A P0)
     (jsr l-cdr)
     (jsr l-mark-trace)
     (j l-trace-no-more)

     ;; --- symbol cell: trace car then cdr
     (label l-trace-symbol)
     (jsr l-car)
     (jsr l-mark-trace)
     (j l-trace-no-more)

     
     ;; --- mark --------------------------------------
     ;; -- input: P0 - cons to mark
     ;; --        P1 - mark method, 1 - pin, 0 - normal  
     ;; -- output: P0 - unchanged
     ;;            P1 - unchanged
     ;;            P2 - type
     ;;            P3 - already marked -> P2 != 0 
     
     ;; Pin marking means that the node is reachable but that
     ;; it can not be moved since it is found through the stack
     ;; scan.
     ;; Normal marking means that the node is reachable and movable.
     ;; Pin marking overwrites normal mark but normal marking must not overwrite pin mark.
     ;; 
     (label l-mark)
     (push-r R3)

     (Rx= n-cons-type R0)
     (A=Rx P0) ; cons-idx
     (A+=Rx R0) ; A = type-ptr = n-cons-type + cons-idx
     (Rx=A R3) ; R3 = type-ptr
     (Rx=M[A].b R2) ; R2 = type = n-cons-type[cons-idx]

     (Rx= c-cons-marked-mask R0)
     (A=Rx R2) ; type
     (A&=Rx R0) ; & mask
     (Rx=A P3) ; return value, !=0 if marked

     ;; which type of mark?
     (A= 0)
     (A-=Rx P1) ; P1=mark-method
     (jz l-normal-mark)
     
     (label l-pin-mark)

     (Rx= c-cons-pin-mask R1) ; pin-bit
     (j l-do-mark)
     
     (label l-normal-mark)

     (Rx= c-cons-gc-mask R1) ; gc-bit

     (label l-do-mark)
     (A=Rx R1)
     (A\|=Rx R2) ; set bit
     (M[Rx].b=A R3) ; write back to n-cons-type[idx]

     (Rx=A P2) ; return type

     (pop-r R3)
     (A=Rx SRP)
     (j-a)

     ;; --- garbage collector -------------------------------
     ;; input: -
     ;; output: -
     (label l-garbage-collect)
     (push-srp)

     (push-r R13) ; save all register to the stack for later gc scanning the stack. skip SRP and SP since they can never contain cons pointers.
     (jsr l-trace-stack)
     (pop-r R13)

     (jsr l-trace-env)

     (jsr l-free)

     (pop-a)
     (j-a)

     ;; --- trace roots from env --------------------------------
     ;; --- anything reachable from the global env can not be garbage.
     (label l-trace-env)
     (push-srp)
     (push-r R0)

     (Rx= n-global-env R0)
     (A=Rx R0)
     (Rx=M[A].w P0)

     (A= 0) ; normal mark
     (Rx=A P1)
     (jsr l-mark-trace)

     (pop-r R0)
     (pop-a)
     (j-a)


     ;; --- scan the stack for values that might be cons cells. These can
     ;;     not be compacted/moved since we can not know if the value just coincide
     ;;     with a cons.
     (label l-trace-stack)
     (push-srp)
     
     (Rx= n-stack-highest R1) ; R1 = sp-idx starting at stack bottom
     (A=Rx SP) (Rx=A R0) ; R0 = current stack top
     (Rx= (1- nr-cons) R2) ; R2 = last index in cons space (first index is 0)

     (label l-stack-loop)
     (A=Rx R1) ; sp-idx
     (Rx=M[A] R3) ; stack word

     ;; last cons index < stack word
     (A=Rx R2)
     (A-=Rx R3)
     (jlo l-next-item)

     (label l-possible-cons)
     ;; TODO: here we should sanity check that cons type isn't free
     (A=Rx R3) (Rx=A P0)
     
     ;; --- cons cell: trace car then cdr
     (A= 1) ; pin mark
     (Rx=A P1)
     (jsr l-mark-trace)

     (label l-next-item)
     (A= -4)
     (A+=Rx R1) ; sp-idx-=4
     (Rx=A R1)

     (A-=Rx R0) ; stack top
     (jnz l-stack-loop)
     
     (pop-a)
     (j-a)

     ;; -------------------------------------------------------------
     ;; 
     (label l-free)
     (push-srp)
     (push-r R6)

     (Rx= 0 R0) ; cons idx 
     (Rx= (1- (+ n-cons-type nr-cons)) R1) ; address of last type
     (Rx= n-cons-type R2) ; type ptr
     (Rx= c-cons-marked-mask R3) ; the pin/gc bits
     (Rx= c-cons-pin-mask R4) ; pin bit
     (Rx= 0 P1) ; value for l-setcons call

     ;;; ----
     (label l-free-loop)
     
     (A=M[Rx].b R2) ; M[type-ptr]
     (Rx=A R6) ; curr type 
     (A&=Rx R3) ; & marked-mask - get pin/gc bits
     (Rx= 0 R5)
     (A-=Rx R5) ; pin/gc==0?
     (jz l-can-free)
     
     (label l-in-use)
     ;; must remove gc bits
     (A= c-cons-type-mask)
     (A&=Rx R6) ; type without gc bits
     (M[Rx].b=A R2) ; write back type with cleared gc state

     (j l-next-type)
    
     (label l-can-free)
     (A= c-cons-free)
     (M[Rx].b=A R2) ; write back type with cleared gc state

     (A=Rx R0) (Rx=A P0)
     (jsr l-setcons)
    
     ;--- next
     (label l-next-type)
     (A=Rx R2) ; last type
     (A-=Rx R1) ; type ptr == last type
     (jz l-free-done)
     
     ; incr R0 cons idx
     (Rx= 1 R6)
     (A=Rx R0)
     (A+=Rx R6)
     (Rx=A R0)
    
     ; incr R2 type ptr
     (A=Rx R2)
     (A+=Rx R6)
     (Rx=A R2)

     (j l-free-loop)
     
     (label l-free-done)
     (pop-r R6)
     (pop-a)
     (j-a)
     
     ))

(defvar func-div10 nil)
(setq func-div10
  '( ;; div10
     (label l-div10)
     ;; input: P0 = number
     ;; output: P0 = number//10
     ;;         P1 = number % 10
     ;; using this algorithm:
     ;; x = 0
     ;; B =   0x10000000  # 32-bit binary 10000000000000000000000000000000
     ;; b10 = 0xA0000000  # 32-bit binary 10100000000000000000000000000000
     ;; 
     ;; while b != 0:
     ;;     if b10 <= v:
     ;;         v -= b10
     ;;         x |= b
     ;;     b10 >>= 1
     ;;     b >>= 1
     ;; return x,v
     (push-r R2)
     (mvi->r #x10000000 R0) ; b
     (mvi->r #xA0000000 R1) ; b10
     (mvi->r 0 R2) ; x
     ; P0 = v
     (label l-div10-loop)
     (mvi->a 0)
     (sub-r R0)
     (jz l-div10-ret)
     (r->a P0) ; v 
     (sub-r R1) ; v - b10
     (jlo l-no-sub) ; if b10 <= v then sub -> if v >= b10 -> if v < b10 then not sub
     ; 123 <= 122 -> 122 >= 123
     ; v = v - b10 (already in A)
     (a->r P0)
     ; x |= b
     (r->a R2) ; x
     (or-r R0) ; | b
     (a->r R2) ; ->x

     (label l-no-sub)
     ; b10 >>= 1
     (r->a R1)
     (lsr-a)
     (a->r R1)
     ; b >>= 1
     (r->a R0)
     (lsr-a)
     (a->r R0)
     
     (j l-div10-loop)
     
     (label l-div10-ret)
     (r->a P0) (a->r P1) ; P1 = v (remainder)
     (r->a R2) (a->r P0) ; P0 = x (quotient)
     (pop-r R2)
     (r->a SRP)
     (j-a)))


(defparameter func-assoc
  '( ;; assoc
     (label l-assoc)
     ;; input: P0 = cons index to a symbol cons
     ;;        P1 = an env/association list, cons index
     ;; output: P0 = cons index to the value (nil if not found)
     ;;         P1 = 0 if not found
     ;; env is a list: (cons `sym-a (cons `sym-b nil))
     ;; a symbols value is embedded in the symbol cons cell
     (push-srp)
     (push-r R2)

     (A=Rx P0) (Rx=A R0)  ; R0 = symb cons
     (A=Rx P1) (Rx=A R1)  ; R1 = env

     (label l-assoc-search)
     (A= 0) ; nil
     (A-=Rx R1)
     (jz l-not-found) ; env is nil so end of env, symbol not found

     (A=Rx R1) (Rx=A P0) ; env
     (jsr l-car) ; (car env) - a symbol cons
     (A=Rx P0) (Rx=A R2) ; the symbol cons

     (Rx=A P0)
     (A=Rx R0) (Rx=A P1)
     (jsr l-eq-sym) ; -> P0

     (A= 0)
     (A-=Rx P0)  ; nil?
     (jz l-next-symb)
     
     (label l-symb-found)
     (A=Rx R2) ; the symb cons
     (Rx=A P0)
     (jsr l-car) ; P0 = the value ptr of the symb cons
     (A= 1)
     (Rx=A P1) ; #t / symbol found
     (j l-assoc-ret)
     
     (label l-next-symb)
     (A=Rx R1) (Rx=A P0) ; env
     (jsr l-cdr) ; P0 = (cdr env)
     (A=Rx P0) (Rx=A R1) ; next env
     (j l-assoc-search)
     
     (label l-not-found)
     (A= 0) ; nil
     (Rx=A P0)
     (Rx=A P1)

     (label l-assoc-ret)
     (pop-r R2)
     (pop-a)
     (j-a)


     ;;;---------------------------------------
     (label l-eq-sym)
     ;;; input P0: cons symb index 0
     ;;;       P1: cons symb index 1
     ;;; output: P0: nil or #t (cons index 0 or 1)
     ;;; 
     (push-srp)
     (push-r R0)

     (jsr l-cdr) ; cdr(sym-cons 0) = name-ptr
     (A=Rx P0) (Rx=A R0) ; -> R0
     
     (A=Rx P1) (Rx=A P0)
     (jsr l-cdr) ; cdr(sym-cons 1) = name-ptr
     (A=Rx P0)
     (A-=Rx R0)
     (jz l-name-eq)
     (A= 0) ; nil
    
     (label l-cdr-ret)
     (Rx=A P0)
     (pop-r R0)
     (pop-a)
     (j-a)

     (label l-name-eq)
     (A= 1) ; #t
     (j l-cdr-ret)
     ))

(defparameter func-eval
  '( ;; assoc
     (label l-eval)
     ;; input: P0 = cons index to an expression to be evaluated
     ;;        P1 = an env/association list, cons index
     ;; output: P0 = cons index to the value
     ;; env is a list: (cons `sym-a (cons `sym-b nil))
     ;; a symbols value is embedded in the symbol cons cell
     (push-srp)
     (push-r R2)

     (Rx= n-cons-type R0)
     (A=Rx R0)
     (A+=Rx P0) ; index in cons-type
     (Rx=M[A].b R0) ; R0=type of cons
   
     ;; number
     (A= c-cons-number)
     (A-=Rx R0)
     (jz l-eval-ret) ; number evals to itself
   
     ;; primitive
     (A= c-cons-primitive)
     (A-=Rx R0)
     (jz l-eval-ret) ; primitive evals to itself

     ;; symbol
     (A= c-cons-symbol)
     (A-=Rx R0)
     (jz l-eval-symbol) ; a symbol
     
     (A= c-cons-cons)
     (A-=Rx R0)
     (jz l-eval-cons) ; a cons

     (label l-eval-ret)
     (pop-r R2)
     (pop-a)
     (j-a)

     ;; symbol value is returned by assoc search in the env
     (label l-eval-symbol)
     (jsr l-assoc)
     (A= 0)
     (A-=Rx P1) ; 0 -> not found then return error
     (jz l-ret-err)
     (j l-eval-ret) ; return value from assoc in P0
   
     ;; apply function
     ;; function call is a list: ( func arg1 arg2 ... )
     (label l-eval-cons)
     (A=Rx P0) (Rx=A R0) ; expr before call
     (A=Rx P1) (Rx=A R1) ; env 

     (jsr l-car) ; car(P0) is the function
     (jsr l-eval) ; evaluate to get value -> P0 = func
     (A=Rx P0) (Rx=A R2) ; R2=eval(car(expr)) = func
     (A=Rx R0) (Rx=A P0)
     (jsr l-cdr) ; P0 = cdr(expr), do not evaluate since we don't know if it's a special form or no
     (A=Rx P0) (Rx=A P1) ; args
     (A=Rx R2) (Rx=A P0) ; func
     (A=Rx R1) (Rx=A P2) ; env
     (jsr l-apply) ; P0=func P1=args P2=env -> P0 = result
     (j l-eval-ret) 
 
     ;; error case
     (label l-ret-err)
     (A= 2) (Rx=A P0) ; return error symbol
     (j l-eval-ret)
     
     ))

(defparameter func-apply
  '( ;; apply
     ;; input: P0=func P1=args-list P2=env
     ;; output: P0=function result
     (label l-apply)
     (push-srp) 
     (push-r R2)

     (Rx= n-cons-type R0)
     (A=Rx R0)
     (A+=Rx P0) ; index in cons-type
     (Rx=M[A].b R0) ; R0=type of cons

     (A= c-cons-primitive)
     (A-=Rx R0)
     (jz l-apply-prim)

     (A= c-cons-func)
     (A-=Rx R0)
     (jz l-apply-func)

     (label l-apply-err)
     (A= 2) ; error
     (Rx=A P0)
    
     (label l-apply-ret)
     (pop-r R2)
     (pop-a)
     (j-a)

     ;; primitive
     ;; For primitives we don't evaluate arguments.
     (label l-apply-prim)
     (jsr l-getcons) ; P0=cons -> P0=cons-value = code-ptr

     ;; at this point P0=code-ptr P1=args-list P2=env
     ;; but prim call needs: P0=args-list P1=env

     (A=Rx P1) (Rx=A R0) ; save P1
     (A=Rx P2) (Rx=A P1) ; P1=env
     (A=Rx P0) (Rx=A P2) ; P2=code-ptr, using P2 since R0-2 are popped, P2 is not parameter to primitive, only a temp register
     (A=Rx R0) (Rx=A P0) ; P0=args-list from saved P1
     (pop-r R2)
     (A=Rx P2)
     (j-a) ; jump to code-ptr
     ;; because we jumped the primitive must "pop-a/j-a" and will
     ;; then return to caller of l-apply using the srp on the stack.

     (label l-apply-func)
     (jsr l-reduce) ; same param as l-apply
     
     (pop-r R2)
     (pop-a)
     (j-a)
     ))

(defparameter func-evlis
  '(
    ;; evlis is used to evaluate the arguments in a function call
    ;;
    ;; L evlis(L t,L e) {
    ;;   return T(t) == CONS ? cons( eval(  car(t),e),
    ;;                               evlis( cdr(t),e)) :
    ;;          T(t) == ATOM ? assoc(t,e) :
    ;;          nil;
    ;; }
     (label l-evlis)
     ;; input: P0 = cons index to a list. The car of each list item should be evaluated.
     ;;             and a new list with the evaluated results should be returned.
     ;;        P1 = an env/association list, cons index
     ;; output: P0 = cons index to a new list of evaluation results
     ;; env is a list: (cons `sym-a (cons `sym-b nil))
     ;; a symbols value is embedded in the symbol cons cell
     (push-srp)
     (push-r R4)

     (Rx= n-cons-type R0)
     (A=Rx R0)
     (A+=Rx P0) ; index in cons-type
     (Rx=M[A].b R0) ; R0=type of cons

     (A= c-cons-cons)
     (A-=Rx R0)
     (jnz l-no-cons) ; a cons
     ;; -------------------
     ;; (eval (car t))
     (A=Rx P0) (Rx=A R0) ; R0 = t
     (A=Rx P1) (Rx=A R1) ; R1 = e

     (jsr l-car) ; -> P0
     (jsr l-eval) ; -> P0
     (A=Rx P0) (Rx=A R2) ; R2 = (eval (car t))
    
     (A=Rx R0) (Rx=A P0) ; t
     (jsr l-cdr) ; P0 = (cdr t)
     (A=Rx R1) (Rx=A P1) ; e
     
     (jsr l-evlis) ; -> P0
     (A=Rx P0) (Rx=A R3) ; R3 = (evlis (cdr t))
    
     (jsr l-cons) ; -> P0 = new cons
     (A=Rx P0) (Rx=A R4) ; R4 = new cons
     (A=Rx R2) (Rx=A P1)
     (jsr l-rplca) ; P0=cons P1=val <-> (rplca cons (eval (car t)))
     (A=Rx R3) (Rx=A P1)
     (jsr l-rplcd) ; P0=cons P1=val <-> (rplcd cons (evlis (cdr t) e))
     ;; P0 = new cons
     (j l-ret-evlis)
     ;; -------------------
     (label l-no-cons)

     (jsr l-eval) ; -> P0

     (label l-ret-evlis)
     (pop-r R4)
     (pop-a)
     (j-a)
    ))

(defparameter func-bind
  '(
    ;;
    ;; bind creates an environment from a list of symbols and a list of values.
    ;; This binds the formal parameter names to the evaluated actual parameters in a function call.
    ;;
    ;; This is the C code from Lisp in 99 lines but we don't use the same env data structure.
    ;; L bind(L v,L t,L e) {
    ;;   return T(v) == NIL ? e :
    ;;          T(v) == CONS ? bind( cdr(v),
    ;;                               cdr(t),
    ;;                               pair( car(v), car(t), e)) :
    ;;          pair(v,t,e);
    ;; }
    (label l-bind)
    ;; input: P0 = cons index to a list. The car of each list item is a symbol.
    ;;        P1 = cons index to a list. The car of each list item is a value.
    ;;        P2 = an env/association list, cons index
    ;; output: P0 = cons index to a new environment list: ( ( syma . vala ) (symb . valb) ... )
    (push-srp)
    (push-r R4)

    (A= 0)
    (A-=Rx P0)
    (jz l-bind-nil)

    (Rx= n-cons-type R0)
    (A=Rx R0)
    (A+=Rx P0) ; index in cons-type
    (Rx=M[A].b R0) ; R0=type of cons

    (A= c-cons-cons)
    (A-=Rx R0)
    (jnz l-bind-no-cons) ; a cons

    ;; -----
    ;; it's a list so add the car of v and t lists to env
    ;; and recursively the rest of the lists
    (A=Rx P0) (Rx=A R0) ; R0 = v-list
    (A=Rx P1) (Rx=A R1) ; R1 = t-list

    (jsr l-car) ; (car v)
    (A=Rx P0) (Rx=A R3) ; R3 = (car v)

    (A=Rx R1) (Rx=A P0)
    (jsr l-car) ; (car t)
    (A=Rx P0) (Rx=A R4) ; R4 = (car t)

    (A=Rx R3) (Rx=A P0)
    (A=Rx R4) (Rx=A P1) ; P2 is already env
    (jsr l-pair) ; -> P0
    (A=Rx P0) (Rx=A P2) ; P2 = new env

    (A=Rx R0) (Rx=A P0) ; v-list
    (jsr l-cdr) ; P0 = (cdr v-list)
    (A=Rx P0) (Rx=A R0) ; R0 = (cdr v-list)

    (A=Rx R1) (Rx=A P0) ; t-list
    (jsr l-cdr) ; P0 = (cdr t-list)
    (A=Rx P0) (Rx=A P1) ; P1 = (cdr t-list)

    (A=Rx R0) (Rx=A P0) ; (cdr v-list)
    (jsr l-bind) ; P0 = (cdr v-list), P1 = (cdr t-list), P2 = new env -> P0 = bind result

    (j l-bind-ret)

    ;; -----
    (label l-bind-no-cons)
    ;; it's not a list so must be a single symbol, add it to env
    (jsr l-pair) ; pair has same params and return val as bind
    (j l-bind-ret)
    
    ;; -----
    (label l-bind-nil)
    ;; nil means nothing to bind, just return env unmodified
    (A=Rx P2) (Rx=A P0)

    (label l-bind-ret)
    (pop-r R4)
    (pop-a)
    (j-a)

    ;; ------------------
    ;; prepends a symbol to an env list
    ;; from lisp in 99 lines:
    ;;   pair(L v,L x,L e) { return cons( cons(v,x), e ); }
    ;; but we store symbol value in symbol cons
    (label l-pair)
    ;; input: P0 = symbol
    ;;        P1 = value
    ;;        P2 = env, an list, cons index
    ;; output: P0 = head of new env cons
    ;;         (P1 = env)
    ;;         (P2 = env)
    (push-srp)

    ;; set symbol value
    (jsr l-rplca) ; P0=symbol P1=value -> P0=symbol

    (A=Rx P2) (Rx=A P1) ; P1=env
    (jsr l-cons) ; P0 = outer-cons = (cons P0=inner-cons P1=env)
    
    (pop-a)
    (j-a)
    
    ))

(defparameter func-reduce
  '(
    ;; L reduce(L f,L t,L e) {
    ;;   return eval( cdr(car(f)),
    ;;                bind( car(car(f)),
    ;;                      evlis(t,e),
    ;;                      not(cdr(f)) ?   ; if lambda has a nil env then use input env
    ;;                        env :
    ;;                        cdr(f)));
    ;; }
    ;;
    ;; lambda definition: ( (formal-params . body) . env )
    ;; (lambda (x) (body x)) -> (cons (cons '(x) '(body x)) env)
    ;;
    ;; calling lambda f with parameter p:
    ;; (f p) -> (eval (cdr (car f))
    ;;                (bind (car (car f)) ; symbol list of formal params
    ;;                      (evlis p env) ; actual params
    ;;                      (cdr f))) ; env from lambda
    ;; In a function call then apply has already evaluated "f" which then must be
    ;; a lambda. Reduce is then called.
    (label l-reduce)
    ;; input: P0 - f, the lambda with structure: ( (formal-params . body) . env )
    ;;        P1 - t, a list with the actual params
    ;;        P2 - the env
    (push-srp)
    (push-r R6)
 
    (A=Rx P0)(Rx=A R0) ; R0=f
    (A=Rx P1)(Rx=A R1) ; R1=t
    (A=Rx P2)(Rx=A R2) ; R2=env

    ;; prepare bind params
    (jsr l-car)
    (A=Rx P0)(Rx=A R3) ; R3 = (car f) = lambda outer cons
    (jsr l-car)
    (A=Rx P0)(Rx=A R4) ; -> R4(car(car f)) = lambda formal params
 
    (A=Rx R1)(Rx=A P0) ; t - actual params
    (A=Rx R2)(Rx=A P1) ; env
    (jsr l-evlis)
    (A=Rx P0)(Rx=A R5) ; R5 = evlis result, the evaluated params
   
    (A=Rx R0)(Rx=A P0)
    (jsr l-cdr) ; P0 = (cdr f) = the lambda env
   
    (A= 0)
    (A-=Rx P0)
    (jnz l-no-env) ; lambda env is not nil

    (A=Rx R2)(Rx=A P2) ; P2 = lambda env
    (j l-bind-env)
  
    (label l-no-env)
    (A=Rx R2)(Rx=A P2) ; P2 = incoming env

    (label l-bind-env)
   
    (A=Rx R4)(Rx=A P0) ; P0 = lambda formal params
    (A=Rx R5)(Rx=A P1) ; P1 = evaluated params

    (jsr l-bind) ; bind params and create a new env to be used in the body evaluation
    (A=Rx P0)(Rx=A R6) ; R6 = env from bind

    (A=Rx R3)(Rx=A P0) ; P0 = (car f) = lambda outer cons
    (jsr l-cdr) ; P0 = (cdr (car f)) = lambda body

    (A=Rx R6)(Rx=A P1) ; P1 = env from bind
    (jsr l-eval) ; P0 = evaluated body, to be the reduce return value
    
    (pop-r R6)
    (pop-a)
    (j-a)

    
    ;; L f_lambda(L t,L e) { return closure(car(t),car(cdr(t)),e); }
    ;; (lambda (x y) (body ...))
    
    ))

;;
;; (defun f () N)

;; (defun f (x y z) N);
;;  -> symbol f with value:
;;  lambda: ( (formal-params . body) . env )
;;          ( ((x y z ) . N ) . env )
;; (defvar v N) 
;;  -> symbol v with value N

;; All primitives are called in the same way with unevaluated arguments.
;; primitives: (prim args...)
;;   input: P0=arg-list
;;          P1=env
;;
(defparameter func-primitives
  '( ;; --- not -----------------------------------
     ;; input: P0=arg-list (but only one arg allowed)
     ;;        P1=env
     ;; output: P0=result
     (label l-not)
     ;; do not push SRP, apply already did that
  
     (jsr l-car) ; P0=car(arg-list)
     (jsr l-eval) ; (P0,P1)->P0 eval:ed arg
 
     (A= 0) ; nil
     (A-=Rx P0)
     (jz l-is-nil)
     (A= 0) ; is not nil-> return nil 
     (Rx=A P0)
     (j l-not-ret)
    
     (label l-is-nil)
     (A= 1) ; is nil -> return t
     (Rx=A P0)

     (label l-not-ret)
     (pop-a) ;; apply did push SRP
     (j-a)

     ;; --- add -----------------------------------
     ;; input: P0=arg-list (#args >= 1)
     ;;        P1=env
     ;; output: P0=result
     (label l-add)
     ;; do not push SRP, apply already did that
     (push-r R2)

     (A= 0) (Rx=A R2) ; the sum 
     (A=Rx P0) (Rx=A R0) ; arg-list
     (A=Rx P1) (Rx=A R1) ; env

     (label l-sum-loop) ; enter with P0=arg-list
     (jsr l-car) ; ->P0=car(arg-list)
     (jsr l-eval) ; (P0,P1)->P0 eval:ed arg
     (jsr l-getcons) ; P0=cons -> P0=cons-value

     ;; sum
     (A=Rx P0)
     (A+=Rx R2)
     (Rx=A R2)
    
     ;; next arg
     (A=Rx R0)
     (Rx=A P0) ; arg-list
     (jsr l-cdr) ; P0 = cdr(arg-list)

     (A= 0) ; nil
     (A-=Rx P0)
     (jz l-end-args)

     (A=Rx R1) (Rx=A P1) ; restore env to P1
     (A=Rx P0) (Rx=A R0) ; save arg-list
     (j l-sum-loop)
    
     (label l-end-args)
     (A=Rx R2)
     (Rx=A P0)
     (jsr l-box-int) ; P0 -> P0
    
     (label l-end-add)
     (pop-r R2)
     (pop-a) ;; apply did push SRP
     (j-a)
     
     
     ;; --- getcons -----------------------------------
     ;; input: P0=cons-index
     ;; output: P0=cons content as a 32-bit value
     (label l-getcons)
     (push-r R0)
     (Rx= n-cons R0)
     (A=Rx P0)
     (lsl-a)
     (lsl-a)
     (A+=Rx R0)
     (Rx=M[A] P0)
     (pop-r R0)
     (A=Rx SRP)
     (j-a)

     ;; --- setcons -----------------------------------
     ;; input: P0=cons-index
     ;;        P1=cons content as a 32-bit value
     ;; output: -
     (label l-setcons)
     (push-r R0)
     (Rx= n-cons R0)
     (A=Rx P0)
     (lsl-a)
     (lsl-a)
     (A+=Rx R0)
     (M[A]=Rx P1)
     (pop-r R0)
     (A=Rx SRP)
     (j-a)

     
     ;; --- defvar ---------------------------------- 
     ;; input: P0=arg-list
     ;;        P1=env
     ;; output: P0=result
     ;;
     ;; (defvar v N) -> add symbol (car args) to global env
     ;;                 eval (car (cdr args)) to symval
     (label l-defvar)
     ;; do not push SRP, apply already did that
     (push-r R2)

     (A=Rx P0)(Rx=A R0) ; R0=arg-list
     (A=Rx P1)(Rx=A R1) ; R1=env

     (jsr l-car) ; -> P0 = symbol = (car args)
     (A=Rx P0)(Rx=A R2) ; R2=symbol

     (A=Rx R0)(Rx=A P0)
     (jsr l-cdr)
     (jsr l-car) ; P0 = N
     (A=Rx R1)(Rx=A P1) ; P1 = env
     (jsr l-eval)
     (A=Rx P0)(Rx=A P1) ; P1 =  evaled arg

     (A=Rx R2)(Rx=A P0) ; P0 = symb
     (A=Rx R1)(Rx=A P2) ; P2 = env
     (jsr l-bind) ; P0=symb P1=val P2=env
     ;; P0 = new env

     ;; write new env to the global env
     (Rx= n-global-env R0)
     (A=Rx R0)
     (M[A].w=Rx P0)

     (A=Rx R2)(Rx=A P0) ; return symbol

     (pop-r R2)
     (pop-a) ;; apply did push SRP
     (j-a)

     ;; --- lambda ---------------------------------- 
     ;; input: P0=arg-list
     ;;        P1=env
     ;; output: P0=result
     ;;
     ;; lambda definition: ( (formal-params . body) . env )
     ;; (lambda (x) (body x)) -> (cons (cons '(x) '(body x)) env)
     ;; (car args) - formal-params
     ;; (car (cdr args) - body

     (label l-lambda)
     ;; do not push SRP, apply already did that
     (push-r R2)

     (A=Rx P0)(Rx=A R0) ; R0=arg-list
     (A=Rx P1)(Rx=A R1) ; R1=env

     (jsr l-car) ; -> P0 = formal-params = (car args)
     (A=Rx P0)(Rx=A R2) ; R2=formal-params

     (A=Rx R0)(Rx=A P0)
     (jsr l-cdr)
     (jsr l-car) ; P0 = body
     (A=Rx P0)(Rx=A P1) ; P1 = body
     (A=Rx R2)(Rx=A P0) ; P0 = formal-params
     (jsr l-cons) ; P0 = inner cons

     (A=Rx R1)(Rx=A P1) ; P1 = env
     (jsr l-cons) ; P0 = outer cons - return value

     (A= c-cons-func)
     (Rx=A P1)
     (jsr l-set-type)
      
     (pop-r R2)
     (pop-a) ;; apply did push SRP
     (j-a)

     ;; --- defun ---------------------------------- 
     ;; input: P0=arg-list
     ;;        P1=env
     ;; output: P0=result
     ;; (defun f (plist) body) -> add symbol (car args) to global env
     ;;   symbol value: (lambda (plist) body)
     (label l-defun)
     ;; do not push SRP, apply already did that
     (push-r R3)

     (A=Rx P0)(Rx=A R1) ; R1=arg-list
     (A=Rx P1)(Rx=A R2) ; R2=env

     (jsr l-car) ; -> P0 = symbol = (car args)
     (A=Rx P0)(Rx=A R3) ; R3=symbol

     (A=Rx R1)(Rx=A P0)
     (jsr l-cdr) ; -> P0 = (cdr arg-list)
     ;; P1 = env
     ;; use l-lambda to create a lambda since the arg-list after symbol is the same as for lambda
     (Rx= l-defun-l-ret R0)
     (push-r R0)
     (j l-lambda)
     (label l-defun-l-ret) ; we need a return point to push so that primtive can pop-a/j-a here

     (A=Rx P0)(Rx=A R0) ; lambda / symbol value
     (A=Rx R3)(Rx=A P0) ; symbol
     (A=Rx R2)(Rx=A P2) ; env
     (A=Rx R0)(Rx=A P1) ; sym-val
     (jsr l-bind) ; P0=symb P1=val P2=env
     ;; P0 = new env

     ;; write new env to the global env
     (Rx= n-global-env R0)
     (A=Rx R0)
     (M[A].w=Rx P0)

     (A=Rx R3)(Rx=A P0) ; return symbol

     (pop-r R3)
     (pop-a) ;; apply did push SRP
     (j-a)
     ))

(defparameter func-primitives2
  '( ;; --- -----------------------------------
     ;; input: P0=arg-list (but only one arg allowed)
     ;;        P1=env
     ;; output: P0=result
     (label l-quote)
     ;; do not push SRP, apply already did that

     (jsr l-car) ; quote just returns the argument unevaluated

     (pop-a) ;; apply did push SRP
     (j-a)

    ))
;;; expected: "ssymb"
(defvar test-read-c nil)
(setq test-read-c 
  '( 
     (mvi->r n-stack-highest SP)

     ;; setup read-ptr to point to source-start
     (mvi->r n-source-start R1)
     (mvi->r reader-state R0) ; base-ptr
     (r->a R0) ; base-ptr
     (st-r->a-rel rs-read-ptr R1) ; M[ A(base) + read-ptr-offs ] = R1 (read-ptr)
     (mvi->r 0 R1) ; use-unread
     (st-r->a-rel rs-use-unread R1) ; M[ A(base) + use-unread-offs ] = R1 (0)

     (jsr l-read-c) ; P0 = char
     (jsr l-putchar) ; print P0

     (r->a R0) ; base-ptr
     (mvi->r 1 R1) ; use-unread = 1
     (st-r->a-rel rs-use-unread R1) ; M[ A(base) + use-unread-offs ] = R1 (1)

     (jsr l-read-c) ; P0 = char
     (jsr l-putchar) ; print P0

     (jsr l-read-c) ; P0 = char
     (jsr l-putchar) ; print P0

     (jsr l-read-c) ; P0 = char
     (jsr l-putchar) ; print P0

     (jsr l-read-c) ; P0 = char
     (jsr l-putchar) ; print P0

     (label end)
     (j end)))

;;; expected: symbol 123 123 symbol2 inner (but no spaces)
(defvar test-reader nil)
(setq test-reader
 '(
     (mvi->r n-stack-highest SP)

     ;; setup read-ptr to point to source-start
     (mvi->r n-source-start R1)
     (mvi->r reader-state R0) ; base-ptr
     (r->a R0) ; base-ptr
     (st-r->a-rel rs-read-ptr R1) ; M[ A(base) + read-ptr-offs ] = R1 (read-ptr)
     (mvi->r 0 R1) ; use-unread
     (st-r->a-rel rs-use-unread R1) ; M[ A(base) + use-unread-offs ] = R1 (0)

     ;; sym  "symbol"
     (jsr f-scan)
     (mvi->r n-read-sym-str P0)
     (jsr prtstr)
    
     ;; num "123"
     (jsr f-scan)
     (r->a P1) (a->r P0)
     (jsr l-prtdec)

     ;; quote
     (jsr f-scan)

     ;; num "123"
     (jsr f-scan)
     (r->a P1) (a->r P0)
     (jsr l-prtdec)

     ;; sym "symbol2"
     (jsr f-scan)
     (mvi->r n-read-sym-str P0)
     (jsr prtstr)

     ;; lpar
     (jsr f-scan)

     ;; sym "inner"
     (jsr f-scan)
     (mvi->r n-read-sym-str P0)
     (jsr prtstr)

     ;; rpar
     (jsr f-scan)

     (label end-reader)
     (j end-reader)))

;;; expected: symbol
(defvar test-find-symbol nil)
(setq test-find-symbol
 '(      
     (mvi->r n-stack-highest SP)

     (mvi->r n-source-start P0)
     (jsr l-find-symbol) ; P0=found-flag P1=symbol
     (r->a P1)(a->r P0)
     (jsr l-cdr) ; P0 = name-ptr = cdr(symbol)
     ;; name-ptr is index into string-space
     (mvi->r n-string-space R0)
     (r->a P0)
     (add-r R0)
     (a->r P0)
     (jsr prtstr)

     (label end-fs)
     (j end-fs)))

;;; test reading a symbol
;;; expect: "symbol"
(defvar test-parse-1 nil)
(setq test-parse-1
 '(      
     (mvi->r n-stack-highest SP)

     ;; --- init scan -----------
     ;; setup read-ptr to point to source-start
     (mvi->r n-source-start R1)
     (mvi->r reader-state R0) ; base-ptr
     (r->a R0) ; base-ptr
     (st-r->a-rel rs-read-ptr R1) ; M[ A(base) + read-ptr-offs ] = R1 (read-ptr)
     (mvi->r 0 R1) ; use-unread
     (st-r->a-rel rs-use-unread R1) ; M[ A(base) + use-unread-offs ] = R1 (0)

     (jsr f-scan) ; P0 = read obj type

     (jsr l-parse) ; P0=object (cons prt)
     ;; in this test we know it's a symbol
     (jsr l-cdr) ; P0 = name-ptr = cdr(symbol)
     ;; name-ptr is index into string-space
     (mvi->r n-string-space R0)
     (r->a P0)
     (add-r R0)
     (a->r P0)
     (jsr prtstr)

     (label end-fs)
     (j end-fs)))

;;; test reading a number
(defvar test-parse-num nil)
(setq test-parse-num
 '(      
     (mvi->r n-stack-highest SP)

     ;; --- init scan -----------
     ;; setup read-ptr to point to source-start
     (mvi->r n-source-start R1)
     (mvi->r reader-state R0) ; base-ptr
     (r->a R0) ; base-ptr
     (st-r->a-rel rs-read-ptr R1) ; M[ A(base) + read-ptr-offs ] = R1 (read-ptr)
     (mvi->r 0 R1) ; use-unread
     (st-r->a-rel rs-use-unread R1) ; M[ A(base) + use-unread-offs ] = R1 (0)

     (jsr f-scan) ; P0 = read obj type
     
     (jsr l-parse) ; P0= obj type from scan -> object (cons ptr)

     (jsr l-print-number)

     (label end-pn)
     (j end-pn)))

;;; test reading a list
(defvar test-parse-2 nil)
(setq test-parse-2
 '(      
     (mvi->r n-stack-highest SP)

     ;; --- init scan -----------
     ;; setup read-ptr to point to source-start
     (mvi->r n-source-start R1)
     (mvi->r reader-state R0) ; base-ptr
     (r->a R0) ; base-ptr
     (st-r->a-rel rs-read-ptr R1) ; M[ A(base) + read-ptr-offs ] = R1 (read-ptr)
     (mvi->r 0 R1) ; use-unread
     (st-r->a-rel rs-use-unread R1) ; M[ A(base) + use-unread-offs ] = R1 (0)

     (jsr f-scan) ; P0 = read obj type
     
     (jsr l-parse) ; P0= obj type from scan -> object (cons ptr)
     ;; in this test we know it's a symbol
     (jsr l-cdr) ; P0 = name-ptr = cdr(symbol)
     ;; name-ptr is index into string-space
     (mvi->r n-string-space R0)
     (r->a P0)
     (add-r R0)
     (a->r P0)
     (jsr prtstr)

     (label end-fs)
     (j end-fs)))

;;; test reading a list
(defvar test-parse-3 nil)
(setq test-parse-3
 '(      
     (mvi->r n-stack-highest SP)

     ;; --- init scan -----------
     ;; setup read-ptr to point to source-start
     (mvi->r n-source-start R1)
     (mvi->r reader-state R0) ; base-ptr
     (r->a R0) ; base-ptr
     (st-r->a-rel rs-read-ptr R1) ; M[ A(base) + read-ptr-offs ] = R1 (read-ptr)
     (mvi->r 0 R1) ; use-unread
     (st-r->a-rel rs-use-unread R1) ; M[ A(base) + use-unread-offs ] = R1 (0)

     ;(jsr f-scan) ; -> P0 = read obj type, P1 = num (if type is num)
     ;(jsr l-parse) ; P0= obj type from scan, P1 = num -> object (cons ptr)
     (jsr l-read)
     ;; [ sym1 | . ]
     ;;          |
     ;;          +->[ sym2 | . ]
     ;;                      |
     ;;                      +-> nil
     (jsr l-print)

     (label end-fs)
     (j end-fs)))

(defvar test-read nil)
(setq test-read
 '(      
     (mvi->r n-stack-highest SP)

     ;; --- init scan -----------
     ;; setup read-ptr to point to source-start
     (mvi->r n-source-start R1)
     (mvi->r reader-state R0) ; base-ptr
     (r->a R0) ; base-ptr
     (st-r->a-rel rs-read-ptr R1) ; M[ A(base) + read-ptr-offs ] = R1 (read-ptr)
     (mvi->r 0 R1) ; use-unread
     (st-r->a-rel rs-use-unread R1) ; M[ A(base) + use-unread-offs ] = R1 (0)

     (jsr l-read) ; P0 = result (cons-ptr)

     (label end-fs)
     (j end-fs)))

(defvar test-str2num nil)
(setq test-str2num
  '( 
     (mvi->r n-stack-highest SP)
     (mvi->r n-source-start P0)
     (jsr l-str2num)
     (label end-s2n)
     (j end-s2n)))

(defvar test-div10 nil)
(setq test-div10
  '( (mvi->r n-stack-highest SP)
     (mvi->r 439 P0)
     (jsr l-div10)
     (mvi->r 141 P0)
     (jsr l-div10)
     (mvi->r 4200000000 P0)
     (jsr l-div10)
     (jsr l-div10)
     (jsr l-div10)
     (jsr l-div10)
     (label end-d10)
     (j end-d10)))

(defvar test-prtdec nil)
(setq test-prtdec
      '( (mvi->r n-stack-highest SP)
         (mvi->r 439 P0)
         (jsr l-prtdec)
         (mvi->r 100001 P0)
         (jsr l-prtdec)
         (mvi->r 0 P0)
         (jsr l-prtdec)
         (label end-pd)
         (j end-pd)))


(defparameter test-print-nil
  '( (Rx= n-stack-highest SP)
     (Rx= 0 P0) ;  nil
     (jsr l-print)
     (label end-prn)
     (j end-prn)))

(defparameter test-func-assoc
 '(  
     ;; call with P0 = symbol to find
     (Rx= n-stack-highest SP)
     (Rx= n-global-env R0)
     (A=Rx R0)
     (Rx=M[A].w P1)

     (jsr l-assoc)
     (label end-fa) (j end-fa)))

(defparameter test-func-eval
 '(  
     ;; call with P0 = expression to evaluate
     (Rx= n-stack-highest SP)
     (Rx= n-global-env R0)
     (A=Rx R0)
     (Rx=M[A].w P1)

     (jsr l-eval)
     (label end-fe) (j end-fe)))

(defparameter test-re
  '( 
     (Rx= n-stack-highest SP)

     ;; --- init scan -----------
     ;; setup read-ptr to point to source-start
     (mvi->r n-source-start R1)
     (mvi->r reader-state R0) ; base-ptr
     (r->a R0) ; base-ptr
     (st-r->a-rel rs-read-ptr R1) ; M[ A(base) + read-ptr-offs ] = R1 (read-ptr)
     (mvi->r 0 R1) ; use-unread
     (st-r->a-rel rs-use-unread R1) ; M[ A(base) + use-unread-offs ] = R1 (0)

     (jsr l-read) ; P0 = result (cons-ptr)

     (Rx= n-global-env R0)
     (A=Rx R0)
     (Rx=M[A].w P1)

     (jsr l-eval)
     (label end-fe) (j end-fe)))

(defparameter test-rep
  '( ;; --- test read/eval/print -----------------------------------
     (Rx= n-stack-highest SP)

     ;; --- init scan -----------
     ;; setup read-ptr to point to source-start
     (mvi->r n-source-start R1)
     (mvi->r reader-state R0) ; base-ptr
     (r->a R0) ; base-ptr
     (st-r->a-rel rs-read-ptr R1) ; M[ A(base) + read-ptr-offs ] = R1 (read-ptr)
     (mvi->r 0 R1) ; use-unread
     (st-r->a-rel rs-use-unread R1) ; M[ A(base) + use-unread-offs ] = R1 (0)

     (jsr l-read) ; P0 = result (cons-ptr)

     (Rx= n-global-env R0)
     (A=Rx R0)
     (Rx=M[A].w P1)

     (jsr l-eval)

     (jsr l-print)
     
     (label end-fe) (j end-fe)))

(defparameter test-repl
  '( ;; --- test read/eval/print/loop -----------------------------------
     (Rx= n-stack-highest SP)

     ;; --- init scan -----------
     ;; setup read-ptr to point to source-start
     (mvi->r n-source-start R1)
     (mvi->r reader-state R0) ; base-ptr
     (r->a R0) ; base-ptr
     (st-r->a-rel rs-read-ptr R1) ; M[ A(base) + read-ptr-offs ] = R1 (read-ptr)
     (mvi->r 0 R1) ; use-unread
     (st-r->a-rel rs-use-unread R1) ; M[ A(base) + use-unread-offs ] = R1 (0)

     (label l-repl)
    
     (Rx= n-print-sep P0)
     (jsr prtstr)
     
     (jsr l-read) ; P0 = result (cons-ptr)

     (A=Rx P0) (Rx=A R2)
     (Rx= (char-code #\Return) P0)
     (jsr l-putchar) 
     (Rx= (char-code #\Linefeed) P0)
     (jsr l-putchar) 
     (A=Rx R2) (Rx=A P0)

     (Rx= n-global-env R0)
     (A=Rx R0)
     (Rx=M[A].w P1)

     (jsr l-eval)

     (jsr l-print)

     ;(jsr l-garbage-collect)
     (j l-repl)
     ))

(defparameter test-repl-noecho
  '( ;; --- test read/eval/print/loop -----------------------------------
     (Rx= n-stack-highest SP)

     ;; --- init scan -----------
     ;; setup read-ptr to point to source-start
     (mvi->r n-source-start R1)
     (mvi->r reader-state R0) ; base-ptr
     (r->a R0) ; base-ptr
     (st-r->a-rel rs-read-ptr R1) ; M[ A(base) + read-ptr-offs ] = R1 (read-ptr)
     (mvi->r 0 R1) ; use-unread
     (st-r->a-rel rs-use-unread R1) ; M[ A(base) + use-unread-offs ] = R1 (0)

     (label l-repl)
    
     (jsr l-read) ; P0 = result (cons-ptr)

     (Rx= n-global-env R0)
     (A=Rx R0)
     (Rx=M[A].w P1)

     (jsr l-eval)

     (jsr l-print)

     (j l-repl)
     ))

(defvar main nil)
(setq main 
  '( ;; --- main ---
     ;(mvi->r 0 R1)
     ;(jsr prtstr)
     
     (mvi->r n-stack-highest SP)

     ;; test str-equal
     ;;(mvi->r n-source-start P1)
     ;;(mvi->r n-sym-string-start P0)
     ;;(jsr l-str-equal)
     ;;(j end)
     
     (mvi->r n-source-start P0)
     (jsr l-find-symbol)
     ))
     
     ;; setup:
     ;; P3 - use-unread
     ;(mvi->r 0 P3)
     ;; P1 - read-ptr
     ;(mvi->r n-source-start P1)

     ;; ------- scan 1 --------------
     ;(jsr f-scan)
     ;(r->a P0) (a->r R0)

     ;; R5 - the source symbol that we are searching for
     ;;(mvi->r n-source-start R5)
     
     ;;;(mvi->r n-cons-type R1) ; R1 cons type table ptr
     ;;(mvi->r n-cons R2) ; R2 cons table ptr
     ;;(mvi->r n-string-space R3) ; strings
     ;;;(ld.b-r->a R1) ;; ignore value for now

     ;;; R2 = cons-table-ptr 
     ;;(label l-next-cons)
     ;;;; n-cons + 2
     ;;(r->a R2)
     ;;(a->r R4)
     ;;(mvi->a 2) ; high word
     ;;(add-r R4)
     ;;(a->r R4)
     ;;;; read cdr
     ;;(ld.w-r->a R4) ; cdr -> sym-name-ptr
     ;;(add-r R3) ; string-space + sym-name-ptr
     ;;(a->r P0)
     ;;(r->a R5)
     ;;(a->r P1)
     ;;(jsr l-str-equal)

     ;;(mvi->a 0)
     ;;(sub-r P0)
     ;;(jnz l-found-sym)
     ;;;(jsr prtstr)

     ;;;; cons-ptr += 4
     ;;(mvi->a 4)
     ;;(add-r R2)
     ;;(a->r R2)

     ;;(j l-next-cons)

     ;;(label l-found-sym) 
     ;;(label l-stop)
     ;;(j l-stop)
     ;;
     ;;(mvi->r n-read-sym-str R1)
     ;;(jsr prtstr)
     
     
     

(defvar func-prtstr nil)
#|
(setq func-prtstr-old
  '( ;; --- prtstr ---
     ;; r1 - ptr to zero terminated string
     (label prtstr)
     (ld.b-r->a R1)
     (mask-a-b)
     (a->r R2)
     (mvi->r -1 R3) ; ptr to I/O reg
     (mvi->a 0)
     (sub-r R2)
     (jz ret-prtstr)
     (r->a R2)
     (st-a->r R3)
     (mvi->a 1)
     (add-r R1)
     (a->r R1)
     (j prtstr)
     (label ret-prtstr)
     (r->a srp)
     (j-a)))
|#

(setq func-prtstr
  '( ;; --- prtstr ---
     ;; P0 - ptr to zero terminated string (abs address, not relative in string space)
     (label prtstr)
     (push-srp)
     (push-r R1)
     (label l-prtstr-loop)
     (ld.b-r->a P0)
     (mask-a-b)
     (a->r R0) ; R0=char
     (mvi->a 0)
     (sub-r R0)
     (jz ret-prtstr)

     (A=Rx P0) (Rx=A R1) ; save P0
     (A=Rx R0) (Rx=A P0) ; P0=char
     (jsr l-putchar)
     (A=Rx R1) (Rx=A P0) ; restore P0
     
     (mvi->a 1)
     (add-r P0)
     (a->r P0)
     (j l-prtstr-loop)
     (label ret-prtstr)
     (pop-r R1)
     (pop-a)
     (j-a)))


;;;(defparameter char-output :uart-io) ; :uart-io / :emul-io
;;;(defparameter char-input :uart-io) ; :uart-io / :emul-io
(defparameter char-output :emul-io) ; :uart-io / :emul-io
(defparameter char-input :emul-io) ; :uart-io / :emul-io

(defparameter func-putchar
  (cond ((equal char-output :emul-io)
         '( ;; --- putchar ---
            ;; params: P0 - char to print
            ;; returns: -
            (label l-putchar)
            (push-r R0)
            (mvi->r -1 R0) ; ptr to emulator char output reg
            (r->a P0)
            (M[Rx].b=A R0)
            (pop-r R0)
            (r->a srp)
            (j-a)))
        ((equal char-output :uart-io)
         '( ;; --- putchar ---
            ;; params: P0 - char to print
            ;; returns: -
            (label l-putchar)
            (push-r R1)
            (Rx= io-base-address R0)

            ;; check serial tx ready
            (label tx-ready)
            (A=Rx R0) ; R0=IO
            (Rx=M[A+n].b serial_tx_status_address R1) ; IO+2 = ser-tx-ready -> R1
            ;; ser-ready == 1?
            (A= 1)
            (A-=Rx R1)
            (jnz tx-ready)
            ;; --- ready, send char ---
            (A=Rx R0) ; IO
            (M[A+n].b=Rx serial_tx_data_address P0) ; IO+1/ser-tx-data = P0

            (pop-r R1)
            (A=Rx SRP)
            (j-a)))))


(defparameter func-getchar
  (cond ((equal char-input :emul-io)
         '(
            (label l-getchar)
            (j l-getchar)
           ))
        ((equal char-input :uart-io)
         '( ;; ---- read-ch --------------------
            ;; params:  -
            ;; returns: P0 = serial byte received
            (label l-getchar)
            (push-r R1)
            (Rx= io-base-address R0)

            (label rx-ready)
            
            (A=Rx R0) ; io-base
            (Rx=M[A+n].b serial_rx_status_address R1) ; ser-rx-ready -> R1
            ;; ser-rx-ready == 1?
            (A= 1)
            (A-=Rx R1)
            (jnz rx-ready)

            ;; get the rx data
            (A=Rx R0) ; R0=io-base
            (Rx=M[A+n].b serial_rx_data_address P0) ; ser-rx-data -> P0

            (pop-r R1)
            (A=Rx SRP)
            (j-a)
            ))))


(defvar func-str-equal nil)
(setq func-str-equal
  '( ;; str-equal
     ;; P1 - string-ptr
     ;; P0 - string-ptr / return value
     (label l-str-equal)
     (push-r R1)
     (label l-str-equal-loop)
     (ld.b-r->a P1)
     (a->r R0)
     (ld.b-r->a P0)
     (a->r R1)
     (sub-r R0)
     (jnz l-ret-uneq)
     ;; equal but are we at end of string?
     (mvi->a 0)
     (sub-r R0)
     (jnz l-inc-strs) ; not 0, inc ptrs then loop back
     ;; end of string and equal
     (mvi->r 1 P0)
     (pop-r R1)
     (r->a SRP)
     (j-a)

     (label l-ret-uneq)
     (mvi->r 0 P0)
     (pop-r R1)
     (r->a SRP)
     (j-a)
     
     (label l-inc-strs)
     (mvi->a 1)
     (add-r P1)
     (a->r P1)
     (mvi->a 1)
     (add-r P0)
     (a->r P0)
     (j l-str-equal-loop)))

(defvar func-car nil)
(setq func-car
  '( ;; car
     ;; P0 - cons ptr 
     ;;    - returns car ptr from the cons cell
     (label l-car)
     (r->a P0) ; cons-ptr = cons-index << 2 + n-cons
     (lsl-a)
     (lsl-a)
     (ld.w-a-rel->r n-cons P0) ; P0 = lowest word of cons-cell (cons.f2/car)
     (r->a SRP)
     (j-a)))

(defvar func-cdr nil)
(setq func-cdr
  '( ;; cdr
     ;; P0 - cons ptr 
     ;;    - returns cdr ptr from the cons cell
     (label l-cdr)
     (r->a P0) ; cons-ptr = cons-index << 2 + n-cons + 2
     (lsl-a)
     (lsl-a)
     (ld.w-a-rel->r (+ 2 n-cons) P0) ; P0 = highest word of cons-cell (cons.f1/cdr)
     (r->a SRP)
     (j-a)))

(defvar func-print-symbol nil)
(setq func-print-symbol
  '( ;; print-symbol
     ;; P0 = cons ptr to a symbol cons-cell (P0 destroyed)
     (label l-print-symbol)
     (push-srp)
     (push-r R0)
     (jsr l-cdr) ; P0 = name-ptr = cdr(symbol)
     ;; name-ptr is index into string-space
     (mvi->r n-string-space R0)
     (r->a P0)
     (add-r R0)
     (a->r P0)
     (jsr prtstr)
     (pop-r R0)
     (pop-a)
     (j-a)))

(defvar func-print-number nil)
(setq func-print-number
  '( ;; print-number
     ;; P0 = cons ptr to a number cons-cell (P0 destroyed)
     (label l-print-number)
     (push-srp)
     (push-r R0)
     (mvi->r n-cons R0)
     (r->a P0)
     (lsl-a)
     (lsl-a)
     (add-r R0) ; cons-addr = n-cons + cons-idx * 4
     (ld-a->r P0) ; the number
     (jsr l-prtdec)
     (pop-r R0)
     (pop-a)
     (j-a)
     
     (label l-prtdec) 
     ;; P0 = number
     (push-srp)
     (push-r R1)
     (r->a P0) (a->r R0)
     (mvi->r 10 R1)
     (r->a P0)
     (sub-r R1) ; P0 >= 10
     (jhs l-is-more-digits)
     ;; single digit
     (label l-prt-single)
     (mvi->r (char-code #\0 ) R0)
     (r->a P0)
     (add-r R0) ; '0' + num
     (a->r P0)
     (jsr l-putchar)
     (pop-r R1)
     (pop-a)
     (j-a)

     (label l-is-more-digits)
     (jsr l-div10) ; P0 -> P0=q P1=r
     (r->a P1) (a->r R1) 
     (jsr l-prtdec)
     (r->a R1) (a->r P0)
     (j l-prt-single)
     ))


(defvar func-print nil)
(setq func-print
  '( ;; print
     ;; P0 = object to be printed
     (label l-print)
     (push-srp)
     (push-r R1)
     (mvi->r n-cons-type R1)
     (r->a P0) (a->r R0)
     (add-r R1) ; n-cons-type + cons-idx
     (ld.b-a->r R1) ; R1 = type
     (mvi->a c-cons-symbol)
     (sub-r R1)
     (jnz l-not-sym)
     ;; symbol
     (jsr l-print-symbol) ; P0 is still cons-idx
     (j l-print-ret)
    
     ;;
     (label l-not-sym)
     (mvi->a c-cons-cons)
     (sub-r R1)
     (jnz l-not-cons)
     ;; cons, i.e. a list
     (jsr l-print-list)
     (j l-print-ret)
     
     (label  l-not-cons)
     (mvi->a c-cons-number)
     (sub-r R1)
     (jnz l-not-num)
     ;; number
     (jsr l-print-number)
     (j l-print-ret)
   
     (label l-not-num)
     ;; other type TBD
     
     (label l-print-ret)
     (pop-r R1)
     (pop-a)
     (j-a)))

(defvar func-print-list nil)
(setq func-print-list
  '( ;; print-list
     ;; P0 = cons-idx of list
     (label l-print-list)
     (push-srp)
     (push-r R1)
     (r->a P0) (a->r R0)
     (mvi->r (char-code #\() P0)
     (jsr l-putchar)
     (label l-pr-list-loop)
     (r->a R0) (a->r P0)
     (jsr l-car)
     (jsr l-print)
     (r->a R0) (a->r P0)
     (jsr l-cdr)
     (r->a P0) (a->r R0)
     (mvi->a 0) ;; nil?
     (sub-r R0)
     (jz l-eol)

     (mvi->r (char-code #\ ) P0)
     (jsr l-putchar)
     (j l-pr-list-loop)
    
     (label l-eol)
     (mvi->r (char-code #\)) P0)
     (jsr l-putchar)
     (pop-r R1)
     (pop-a)
     (j-a)))
     


(defvar *hello-world* nil)
;(setq *hello-world*
;      (masm main func-prtstr read-c scan))

(defun check-dmem (dmem)
  (loop for item across dmem
        for i from 0
        do (if (> item 255) (format t "Index ~D: ~A~%" i item))))

(defvar e nil)
(defvar *symtab* nil)

(defun asm-n-run ( main &optional
                        (setup nil)
                        (debug nil)
                        (no-curses nil)
                        (nr-instr 1000)
                        (pty nil))
  ;(setq *symtab* 
  ;      (if no-curses
  ;          nil
  ;          (make-hash-table)))
  (setq *symtab* (make-hash-table))
  (setq *hello-world*
        (masm *symtab*
              main func-prtstr read-c scan func-str-equal
              func-putchar func-getchar
              func-find-symbol func-cdr
              func-car func-parse func-rplca func-rplcd
              func-cons func-gc func-print-symbol func-print
              func-print-list func-str2num func-div10
              func-print-number func-read func-assoc func-eval
              func-apply func-evlis func-bind func-reduce
              func-primitives func-primitives2))
  (setf e (make-emulator *hello-world* dmem :shared-mem nil :debug debug))
  (if setup (funcall setup dmem (lr-emulator::emulated-system-processor e)))
  (if no-curses (run-emul e nr-instr)
      (if pty
          (run-with-curses-io e pty *symtab*)
          (run-with-curses e *symtab*)))
  (check-dmem dmem)
  (list dmem (lr-emulator::emulated-system-processor e)))

;(setf e (make-emulator *hello-world* dmem 0 nil))
;(run-with-curses e)

(defun run-test (test &optional (expected-print nil))
  (with-output-to-string (*standard-output*)
    (setq dmem (make-dmem 4000))
    (init-lisp))
  (check
    (let ((printed 
            (with-output-to-string (*standard-output*)
              (funcall test t))))
      (if (and expected-print (not (equal printed expected-print)))
          (progn (format t "missmatch printed:~a expected:~a" printed expected-print) nil)
          t))))


;;; expected: "ssymb"
(defun t1 ( &optional (regression nil) )
  (asm-n-run test-read-c
    #'(lambda (dmem proc)
        (set-program dmem 
                     (string-to-mem "symbol symbol2 ( inner )") 
                     n-source-start))
    nil
    regression))

(deftest run-t1 () (run-test #'t1 "ssymb"))

;;; expected: "symbol 123 123 symbol2 inner"
(defun t2 ( &optional (regression nil) )
  (asm-n-run test-reader
    #'(lambda (dmem proc)
        (set-program dmem 
                     (string-to-mem "symbol 123 ' 123x symbol2 ( inner )") 
                     n-source-start))
    nil regression 10000))

(deftest run-t2 () (run-test #'t2 "symbol123123symbol2inner"))

;;; expected: "symbol"
(defun t3 ( &optional (regression nil) )
  (asm-n-run test-find-symbol
    #'(lambda (dmem proc)
        (setq string-space-free 0)
        (add-symbol dmem "not-this" 0)
        (add-symbol dmem "symbol2" 0)
        (add-symbol dmem "symbol" 0)
        (set-program dmem 
                     (string-to-mem "symbol") 
                     n-source-start))
    nil regression 10000))

(deftest run-t3 () (run-test #'t3 "symbol"))

(defun t4 ( &optional (regression nil) )
  (asm-n-run test-parse-1
    #'(lambda (dmem proc)
        (setq string-space-free 0)
        (add-symbol dmem "not-this" 0)
        (add-symbol dmem "symbol" 0)
        (set-program dmem 
                     (string-to-mem "symbol") 
                     n-source-start))
    nil regression 10000))

(deftest run-t4 () (run-test #'t4 "symbol"))


;;; will read from pty, should input " symbol " to pty
(defparameter pty "/dev/pts/6")

(defun t13 ( &optional (regression nil) )
  (init-lisp)
  (asm-n-run test-parse-1
    #'(lambda (dmem proc)
        (setq string-space-free 0)
        (add-symbol dmem "not-this" 0)
        (add-symbol dmem "symbol" 0)
        (set-program dmem '(0 0) n-source-start)) ; source is empty
    nil regression 10000 pty))

;;; reading "(123 (sym2 sym1))"
;;; first part in source buffer "(123"
;;; the rest should be input from pty: "(sym2 sym1))"
(defun t14 ( &optional (regression nil) )
  (init-lisp)
  (asm-n-run test-parse-3
    #'(lambda (dmem proc)
        (setq string-space-free 0)
        (add-symbol dmem "nil" 0) ; nil must be symbol 0
        (add-symbol dmem "sym1" 0)
        (add-symbol dmem "sym2" 0)
        (set-program dmem 
                     (string-to-mem "(123") 
                     n-source-start))
    nil regression 10000 pty))

;;; reading an empty list == nil
;;; expect: "nil"
(defun t5 ( &optional (regression nil) )
  (asm-n-run test-parse-2
    #'(lambda (dmem proc)
        (setq string-space-free 0)
        (add-symbol dmem "nil" 0) ; nil must be symbol 0
        (add-symbol dmem "sym1" 0)
        (add-symbol dmem "sym2" 0)
        (set-program dmem 
                     (string-to-mem "()") 
                     n-source-start))
    nil regression 10000))

(deftest run-t5 () (run-test #'t5 "nil"))

;;; expected: ? list?
(defun t6 ()
  (asm-n-run test-parse-2
    #'(lambda (dmem proc)
        (setq string-space-free 0)
        (add-symbol dmem "nil" 0) ; nil must be symbol 0
        (add-symbol dmem "sym1" 0)
        (add-symbol dmem "sym2" 0)
        (set-program dmem 
                     (string-to-mem "(sym1)") 
                     n-source-start))))

;;; expected: (123 (sym2 sym1))
(defun t7 ( &optional (regression nil) )
  (asm-n-run test-parse-3
    #'(lambda (dmem proc)
        (setq string-space-free 0)
        (add-symbol dmem "nil" 0) ; nil must be symbol 0
        (add-symbol dmem "sym1" 0)
        (add-symbol dmem "sym2" 0)
        (set-program dmem 
                     (string-to-mem "(123 (sym2 sym1))") 
                     n-source-start))
    nil regression 10000))

(deftest run-t7 () (run-test #'t7 "(123 (sym2 sym1))"))

;;; expected: P0=1 P1=1234
(defun t8 ()
  (asm-n-run test-str2num
    #'(lambda (dmem proc)
        (setq string-space-free 0)
        (set-program dmem 
                     (string-to-mem "1234") 
                     n-source-start))))

;;; expected: P0=420000
(defun t9 ()
  (asm-n-run test-div10))

;;; expected: "439 100001 0"
(defun t10 ( &optional (regression nil) )
  (asm-n-run test-prtdec
             nil nil regression 10000))

(deftest run-t10 () (run-test #'t10 "4391000010"))

;;; expected: "123"
(defun t11 ( &optional (regression nil) )
  (asm-n-run test-parse-num
    #'(lambda (dmem proc)
        (setq string-space-free 0)
        (add-symbol dmem "nil" 0) ; nil must be symbol 0
        (add-symbol dmem "sym1" 0)
        (add-symbol dmem "sym2" 0)
        (set-program dmem 
                     (string-to-mem "123") 
                     n-source-start))
    nil regression 10000))

(deftest run-t11 () (run-test #'t11 "123"))


;;; expected: (123 (sym2 sym1))
(defun t12 ( &optional (regression nil) )
  (init-lisp)
  (asm-n-run test-read
    #'(lambda (dmem proc)
        (setq string-space-free 0)
        (add-symbol dmem "nil" 0) ; nil must be symbol 0
        (add-symbol dmem "sym1" 0)
        (add-symbol dmem "sym2" 0)
        (set-program dmem (string-to-mem "(123 (sym2 sym1))") n-source-start))
        ;;(set-program dmem (string-to-mem "123") n-source-start))
        ;;(set-program dmem (string-to-mem "sym1") n-source-start))
    nil regression 10000)
    (format t "res: P0=~a~%" (get-reg P0 e)))



;;; expected: "nil"
(defun t15 ( &optional (regression nil) )
  (asm-n-run test-print-nil
    #'(lambda (dmem proc)
        (default-env dmem))
    nil regression 10000))

(deftest run-t15 () (run-test #'t15 "nil"))

;;; needs to set nr-cons to 30 to test wrapping during allocation
(defparameter test-trace-env
  '( (Rx= n-stack-highest SP)
     ;; set values that can not be cons cells to simplify debug
     (Rx= 100000 R0)
     (Rx= 100000 R1)
     (Rx= 100000 R2)
     (Rx= 100000 R3)
     (Rx= 100000 R4)
     (Rx= 100000 R5)
     (Rx= 100000 R6)
     (Rx= 100000 R7)
     (Rx= 100000 R8)
     (Rx= 100000 R9)
     (Rx= 100000 R10)
     (Rx= 100000 R11)
     (Rx= 100000 R12)
  
     ;; allocate conses but don't use them so there will be
     ;; garbage to collect
     (jsr l-box)
     (jsr l-box)
     (jsr l-box)
     (jsr l-box)
     (jsr l-box)
     (jsr l-box)
     (jsr l-box)
     (jsr l-box)
     (jsr l-box)
    
     ;; create some conses that will be held in registers to see
     ;; that they are marked and not deallocated
     (Rx= 12321 P0)
     (jsr l-box-int) ; reachable
     (A=Rx P0) (Rx=A R1)
     (Rx= 12322 P0)
     (jsr l-box-int) ; reachable
     (A=Rx P0) (Rx=A R2)
     (Rx= 12323 P0)
     (jsr l-box-int) ; reachable
     (A=Rx P0) (Rx=A R3)
     (Rx= 12324 P0)
     (jsr l-box-int) ; unreachable, not stored in any reg
     (Rx= 12325 P0)
     (jsr l-box-int) ; reachable

     (jsr l-garbage-collect)
    
     ;; allocate more so that the free pointer can wrap
     (jsr l-box)
     (jsr l-box)
     (jsr l-box)
     (jsr l-box)
     (jsr l-box)
     (jsr l-box)
     (jsr l-box)
     (jsr l-box)
     (jsr l-box)

     (Rx= 12326 P0)
     (jsr l-box-int) ; reachable
     (A=Rx P0) (Rx=A R4)
     (Rx= 12327 P0)
     (jsr l-box-int) ; reachable
     (A=Rx P0) (Rx=A R5)

     (jsr l-garbage-collect)

     (label end-prn)
     (j end-prn)))

;;; test the garbage collector and allocation
(defun t16 ( &optional (regression nil) )
  (destructuring-bind (dmem proc)
    (asm-n-run test-trace-env
               #'(lambda (dmem proc)
                   (default-env dmem)
                   (mem-write-word dmem n-cons-free 0))
               nil regression 10000)))
    ;(print-conses dmem n-cons n-cons-type)
    ;(format t "SP:~a~%" (aref (lr-emulator::processor-state-r proc) SP))
    ;(print-stack dmem 
    ;             n-stack-highest
    ;             (aref (lr-emulator::processor-state-r proc) SP))))

(deftest run-t16 () (run-test #'t16 ""))




(defun dump-cons (dmem n-cons n-cons-type)
  (let ((incr 4))
    (loop with addr := n-cons and taddr := n-cons-type
          for row from 1 to 20
          do (format t "~4,'0X: ~{~2,'0X~^ ~} ~a~%"
                       addr
                       (coerce (subseq dmem addr (+ 4 addr)) 'list)
                       (aref dmem taddr))
          until (equal (aref dmem taddr) c-cons-free)
          do (setf addr (+ addr incr))
          do (setf taddr (1+ taddr)))))

(defun get-car (dmem cons-index)
  (let* ((addr (+ n-cons (* cons-index 4)))
         (car-lo (aref dmem addr))
         (car-hi (aref dmem (1+ addr)))
         (car-val (+ car-lo (ash car-hi 8))))
   car-val))

(defun get-cdr (dmem cons-index)
  (let* ((addr (+ n-cons (+ 2 (* cons-index 4))))
         (cdr-lo (aref dmem addr))
         (cdr-hi (aref dmem (1+ addr)))
         (cdr-val (+ cdr-lo (ash cdr-hi 8))))
   cdr-val)) 

(defun print-cons (dmem cons-index)
  (format t "car:~4d cdr:~4d" (get-car dmem cons-index) (get-cdr dmem cons-index)))

(defun print-symbol (dmem cons-index)
  (let ((name-ptr (get-cdr dmem cons-index))
        (val-ptr (get-car dmem cons-index)))
    (print-string dmem (+ n-string-space name-ptr))
    (format t " str:~d val:~d" name-ptr val-ptr)))

(defun print-number (dmem cons-index is-prim)
  (let* ((addr (+ n-cons (* cons-index 4)))
         (res (aref dmem addr)))
    (assert (= 0 (logand addr 3)))
    (setf res (logior res 
                      (ash (aref dmem (+ addr 1)) 8)))
    (setf res (logior res 
                      (ash (aref dmem (+ addr 2)) 16)))
    (setf res (logior res 
                      (ash (aref dmem (+ addr 3)) 24)))
    (if is-prim
      (format t "c:~a" res)
      (format t "n:~a" res))))

(defun print-gc-state (dmem cons-index)
  (let ((gcbits (logand c-cons-marked-mask (aref dmem (+ n-cons-type cons-index)))))
    (cond
      ((equal 0 gcbits) "  ")
      ((equal 0 (logand c-cons-pin-mask gcbits)) "gc")
      (t "p "))))

(defun print-cons-cell (dmem cons-index cons-addr t-addr)
  (let ((cons-type (logand 7 (aref dmem t-addr))))
    (format t "~4a: ~a ~a " cons-index 
            (print-gc-state dmem cons-index)
            (gethash cons-type cons-type-names))
    (cond ((equal c-cons-symbol cons-type) (print-symbol dmem cons-index))
          ((equal c-cons-free cons-type) t)
          ((equal c-cons-number cons-type) (print-number dmem cons-index nil))
          ((equal c-cons-primitive cons-type) (print-number dmem cons-index t))
          ((equal c-cons-func cons-type) (print-cons dmem cons-index))
          ((equal c-cons-cons cons-type) (print-cons dmem cons-index)))
    (format t "~%")))

(defun rest-free-p (dmem index)
  (every #'(lambda (item) (equal c-cons-free item))
         (subseq dmem index (+ n-cons-type nr-cons))))
  
(defun print-conses (dmem n-cons n-cons-type)
  (let ((incr 4))
    (loop with addr := n-cons and taddr := n-cons-type and index := 0
          do (print-cons-cell dmem index addr taddr)
          until (rest-free-p dmem taddr)
          do (setf addr (+ addr incr))
          do (setf index (1+ index))
          do (setf taddr (1+ taddr)))))

(defun test-print (dmem)
  (setq string-space-free 0)
  (let* ((env (push-env dmem (add-symbol dmem "nil" 0) 0))
         (numval (add-num dmem 1234)))
    (setf env (push-env dmem (add-symbol dmem "#t" 1) env))
    (setf env (push-env dmem (add-symbol dmem "not-this" 0) env))
    (setf env (push-env dmem (add-symbol dmem "symbol2" numval) env))
    (setf env (push-env dmem (add-symbol dmem "symbol" 0) env))
    (mem-write-word dmem n-global-env env)))

(defun print-stack (dmem stack-top stack-bottom)
  (loop for ptr from stack-top downto stack-bottom by 4
        do (let ((stack-val (lr-emulator::mem-read-dword dmem ptr)))
             (format t "~5d: ~8x ~d~%" ptr stack-val stack-val))))

;;; search env for a symbol and returns it's value
;;; searching for "symbol2" and returns cons index to num 1234
(defun test-assoc ( &optional (regression nil) )
  (destructuring-bind (dmem proc)
    (asm-n-run test-func-assoc
               #'(lambda (dmem proc)
                   (let* ((env (push-env dmem (add-symbol dmem "nil" 0) 0))
                          (numval (add-num dmem 1234))
                          (to-find (add-symbol dmem "symbol2" numval)))
                     (setf env (push-env dmem (add-symbol dmem "t" 1) env))
                     (setf env (push-env dmem (add-symbol dmem "not-this" 0) env))
                     (setf env (push-env dmem to-find env))
                     (setf env (push-env dmem (add-symbol dmem "symbol" 0) env))
                     (mem-write-word dmem n-global-env env)
                     (setf (aref (lr-emulator::processor-state-r proc) P0) to-find)))
               nil regression 10000)
    (when (not regression) (print-conses dmem n-cons n-cons-type))
    (let ((reg-p0 (aref (lr-emulator::processor-state-r proc) P0))
          (reg-p1 (aref (lr-emulator::processor-state-r proc) P1)))
      (when (not regression) (format t "P0:~a P1:~a~%" reg-p0 reg-p1))
      (check (and (equal reg-p0 2) (equal reg-p1 1))))))

(deftest run-assoc () (run-test #'test-assoc nil))

;;; search env for a primitive symbol and return it's value
;;; searching for "not" and returns the code-ptr to the l-not label
(defun test-assoc-prim ( &optional (regression nil) )
  (destructuring-bind (dmem proc)
    (asm-n-run test-func-assoc
               #'(lambda (dmem proc)
                   (let* ((env (push-env dmem (add-symbol dmem "nil" 0) 0))
                          (find-sym (add-symbol dmem "not" 0))
                          (to-find (add-prim dmem "l-not")))
                     (setf env (push-env dmem (add-symbol dmem "t" 1) env))
                     (setf env (push-env dmem (add-symbol dmem "not-this" 0) env))
                     (setf env (push-env dmem (add-symbol dmem "not" to-find) env))
                     (setf env (push-env dmem (add-symbol dmem "symbol" 0) env))
                     (mem-write-word dmem n-global-env env)
                     (setf (aref (lr-emulator::processor-state-r proc) P0) find-sym)))
               nil regression 10000)
    (when (not regression) (print-conses dmem n-cons n-cons-type))
    (let ((reg-p0 (aref (lr-emulator::processor-state-r proc) P0))
          (reg-p1 (aref (lr-emulator::processor-state-r proc) P1)))
      (when (not regression) (format t "P0:~a P1:~a~%" reg-p0 reg-p1))
      (check (and (equal reg-p0 3) (equal reg-p1 1))))))

(deftest run-assoc-prim () (run-test #'test-assoc-prim nil))

;;; search env for a symbol that is missing and return nil
(defun test-assoc2 ( &optional (regression nil) )
  (destructuring-bind (dmem proc)
    (asm-n-run test-func-assoc
               #'(lambda (dmem proc)
                   (init-lisp)
                   (let* ((env (push-env dmem (add-symbol dmem "nil" 0) 0))
                          (numval (add-num dmem 1234))
                          (to-find (add-symbol dmem "symbol2" numval)))
                     (setf env (push-env dmem (add-symbol dmem "t" 1) env))
                     (setf env (push-env dmem (add-symbol dmem "not-this" 0) env))
                     (setf env (push-env dmem (add-symbol dmem "symbol" 0) env))
                     (mem-write-word dmem n-global-env env)
                     (setf (aref (lr-emulator::processor-state-r proc) P0) to-find)))
               nil regression 10000)
    (when (not regression) (print-conses dmem n-cons n-cons-type))
    (let ((reg-p0 (aref (lr-emulator::processor-state-r proc) P0))
          (reg-p1 (aref (lr-emulator::processor-state-r proc) P1)))
      (when (not regression) (format t "P0:~a P1:~a~%" reg-p0 reg-p1))
      (check (and (equal reg-p0 0) (equal reg-p1 0))))))

(deftest run-assoc2 () (run-test #'test-assoc2 nil))

;;; The initial cons cells and env.
;;; Note that to add primitives the symtab must be populated.
(defconstant fixed-cons-nil   0)
(defconstant fixed-cons-t     1)
(defconstant fixed-cons-err   2)
(defconstant fixed-cons-quote 3)

(defun default-env (dmem)
  (let* ((sym-0-nil (add-symbol dmem "nil" 0)) ; fixed positioned symbol
         (sym-1-t   (add-symbol dmem "t" 1)); fixed positioned symbol
         (sym-2-err (add-symbol dmem "error" 0)); fixed positioned symbol
         (sym-3-q   (add-symbol dmem "quote" 0)); fixed positioned symbol
         (sym-3-p   (set-sym-val dmem sym-3-q (add-prim dmem "l-quote")))
         (env (push-env dmem sym-0-nil 0)))
      (setf env (push-env dmem sym-1-t env))
      (setf env (push-env dmem sym-2-err env))
      (setf env (push-env dmem sym-3-q env))
      (setf env (push-env dmem (add-symbol dmem "not" (add-prim dmem "l-not")) env))
      (setf env (push-env dmem (add-symbol dmem "+" (add-prim dmem "l-add")) env))
      (setf env (push-env dmem (add-symbol dmem "defvar" (add-prim dmem "l-defvar")) env))
      (setf env (push-env dmem (add-symbol dmem "lambda" (add-prim dmem "l-lambda")) env))
      (setf env (push-env dmem (add-symbol dmem "defun" (add-prim dmem "l-defun")) env))
      (mem-write-word dmem n-global-env env)
    env))
  
;; call eval with a num cons
;; should return the num cons
(defun test-eval-num ( &optional (regression nil) )
  (destructuring-bind (dmem proc)
    (asm-n-run test-func-eval
               #'(lambda (dmem proc)
                   (let ((env (default-env dmem))
                         (numval (add-num dmem 1234)))
                     (setf (aref (lr-emulator::processor-state-r proc) P0) numval)))
               nil regression 10000)
    (when (not regression) (print-conses dmem n-cons n-cons-type))
    (let ((reg-p0 (aref (lr-emulator::processor-state-r proc) P0)))
      (when (not regression) (format t "P0:~a~%" reg-p0))
      (check (equal reg-p0 12)))))

(deftest run-eval-num () (run-test #'test-eval-num nil))

;; call eval with a symbol with value a num cons
;; should return the num consj
(defun test-eval-symb ( &optional (regression nil) )
  (destructuring-bind (dmem proc)
    (asm-n-run test-func-eval
               #'(lambda (dmem proc)
                   (let* ((env (default-env dmem))
                          (numval (add-num dmem 1234))
                          (sym (add-symbol dmem "symbol" numval)))
                     (setf env (push-env dmem sym env))
                     (mem-write-word dmem n-global-env env)
                     (format t "env: ~d~%" env)
                     (format t "sym ~d~%" sym)
                     (setf (aref (lr-emulator::processor-state-r proc) P0) sym)))
               nil regression 10000)
    (when (not regression) (print-conses dmem n-cons n-cons-type))
    (let ((reg-p0 (aref (lr-emulator::processor-state-r proc) P0)))
      (when (not regression) (format t "P0:~a~%" reg-p0))
      (check (equal reg-p0 12)))))

(deftest run-eval-symb () (run-test #'test-eval-symb nil))

;; eval symbol with value nil
;; should return nil cons
(defun test-eval-symb-nil ( &optional (regression nil) )
  (destructuring-bind (dmem proc)
    (asm-n-run test-func-eval
               #'(lambda (dmem proc)
                   (let* ((env (default-env dmem))
                          (numval (add-num dmem 1234))
                          (sym (add-symbol dmem "symbol" numval)))
                     (setf env (push-env dmem sym env))
                     (mem-write-word dmem n-global-env env)
                     ;(format t "env: ~d~%" env)
                     ;(format t "sym ~d~%" sym)
                     (setf (aref (lr-emulator::processor-state-r proc) P0) 0))) ; nil
               nil regression 10000)
    (when (not regression) (print-conses dmem n-cons n-cons-type))
    (let ((reg-p0 (aref (lr-emulator::processor-state-r proc) P0)))
      (when (not regression) (format t "P0:~a~%" reg-p0))
      (check (equal reg-p0 0)))))

(deftest run-eval-symb-nil () (run-test #'test-eval-symb-nil nil))

;; eval the primitive symbol "not" and return the prim cons
(defun test-eval-prim-symb ( &optional (regression nil) )
  (destructuring-bind (dmem proc)
    (asm-n-run test-func-eval
               #'(lambda (dmem proc)
                   (let* ((env (default-env dmem))
                          (sym 7)) ; the not symbol in default-env
                          ;(sym (add-prim dmem "not" "l-not")))
                     (setf env (push-env dmem sym env))
                     (mem-write-word dmem n-global-env env)
                     (format t "env: ~d~%" env)
                     (format t "sym ~d~%" sym)
                     (setf (aref (lr-emulator::processor-state-r proc) P0) sym)))
               nil regression 10000)
    (when (not regression) (print-conses dmem n-cons n-cons-type))
    (let ((reg-p0 (aref (lr-emulator::processor-state-r proc) P0)))
      (when (not regression) (format t "P0:~a~%" reg-p0))
      (check (equal reg-p0 6)))))

(deftest run-eval-prim-symb () (run-test #'test-eval-prim-symb nil))

;;; (cons (symbol "not") (cons 111 nil)) i.e. (not 111)
(defun make-not-fcall (dmem)
  (let* ((true-val (add-num dmem 111))
         (arg1 (make-cons dmem true-val 0))
         (func (add-symbol dmem "not" 0))
         (head (make-cons dmem func arg1)))
    head))

;;; (not 111) -> nil
(defun test-eval-not ( &optional (regression nil) )
  (destructuring-bind (dmem proc)
    (asm-n-run test-func-eval
               #'(lambda (dmem proc)
                   (init-lisp)
                   (let* ((env (default-env dmem))
                          (fcall (make-not-fcall dmem)))
                     (mem-write-word dmem n-global-env env)
                     (format t "env: ~d~%" env)
                     (format t "fcall ~d~%" fcall)
                     (setf (aref (lr-emulator::processor-state-r proc) P0) fcall)))
               nil regression 10000)
    (print-conses dmem n-cons n-cons-type)
    (format t "P0:~a~%"
            (aref (lr-emulator::processor-state-r proc) P0))))


;;; (cons (symbol "not") (cons nil nil)) i.e. (not nil)
(defun make-not-fcall-nil (dmem)
  (let* ((nil-sym (add-symbol dmem "nil" 0))
         (arg1 (make-cons dmem nil-sym 0))
         (func (add-symbol dmem "not" 0))
         (head (make-cons dmem func arg1)))
    head))

;;; (not nil) -> t
(defun test-eval-not-nil ( &optional (regression nil) )
  (destructuring-bind (dmem proc)
    (asm-n-run test-func-eval
               #'(lambda (dmem proc)
                   (let* ((env (default-env dmem))
                          (fcall (make-not-fcall-nil dmem)))
                     (mem-write-word dmem n-global-env env)
                     (format t "env: ~d~%" env)
                     (format t "fcall ~d~%" fcall)
                     (setf (aref (lr-emulator::processor-state-r proc) P0) fcall)))
               nil regression 10000)
    (when (not regression) (print-conses dmem n-cons n-cons-type))
    (let ((reg-p0 (aref (lr-emulator::processor-state-r proc) P0)))
      (when (not regression) (format t "P0:~a~%" reg-p0))
      (check (equal reg-p0 1)))))

(deftest run-eval-not-nil () (run-test #'test-eval-not-nil nil))


;;; (cons (symbol "+") (cons 10 (cons 11 nil)))
(defun make-add-fcall (dmem)
  (let* ((true-val 0)
         (arg2 (make-cons dmem (add-num dmem 11) 0))
         (arg1 (make-cons dmem (add-num dmem 10) arg2))
         (func (add-symbol dmem "+" 0))
         (head (make-cons dmem func arg1)))
    head))

;;; (+ 10 11) -> 21
(defun test-eval-add ( &optional (regression nil) )
  (destructuring-bind (dmem proc)
    (asm-n-run test-func-eval
               #'(lambda (dmem proc)
                   (let* ((env (default-env dmem))
                          (fcall (make-add-fcall dmem)))
                     (mem-write-word dmem n-global-env env)
                     (format t "env: ~d~%" env)
                     (format t "fcall ~d~%" fcall)
                     (setf (aref (lr-emulator::processor-state-r proc) P0) fcall)))
               nil regression 10000)
    (when (not regression) (print-conses dmem n-cons n-cons-type))
    (let ((reg-p0 (aref (lr-emulator::processor-state-r proc) P0)))
      (when (not regression) (format t "P0:~a~%" reg-p0))
      (check (equal reg-p0 18))))) ; num cons 18

(deftest run-eval-add () (run-test #'test-eval-add nil))

;;; (eval (read "(+ 10 11)"))
(defun test-eval-read ( &optional (regression nil) )
  (destructuring-bind (dmem proc)
    (asm-n-run test-re
               #'(lambda (dmem proc)
                   (let* ((env (default-env dmem)))
                     (mem-write-word dmem n-global-env env)
                     (format t "env: ~d~%" env)
                     (set-program dmem (string-to-mem "(+ 10 11)") n-source-start)))
               nil regression 10000)
    (when (not regression) (print-conses dmem n-cons n-cons-type))
    (let ((reg-p0 (aref (lr-emulator::processor-state-r proc) P0)))
      (when (not regression) (format t "P0:~a~%" reg-p0))
      (check (equal reg-p0 17))))) ; num cons 17

(deftest run-eval-read () (run-test #'test-eval-read nil))

;;; (print (eval (read "(+ 10 11)")))
(defun test-eval-read-print ( &optional (regression nil) )
  (destructuring-bind (dmem proc)
    (asm-n-run test-rep
               #'(lambda (dmem proc)
                   (let* ((env (default-env dmem)))
                     (mem-write-word dmem n-global-env env)
                     (when (not regression) (format t "env: ~d~%" env))
                     (set-program dmem (string-to-mem "(+ 10 11)") n-source-start)))
               nil regression 10000)
    (when (not regression) (print-conses dmem n-cons n-cons-type))
    (let ((reg-p0 (aref (lr-emulator::processor-state-r proc) P0)))
      (when (not regression) (format t "P0:~a~%" reg-p0)))))

(deftest run-eval-read-print  () (run-test #'test-eval-read-print "21"))

(defun set-source (dmem source-string)
  (let ((l (length source-string)))
    (assert (< l source-region-size))
    (set-program dmem (string-to-mem source-string) n-source-start)))
  
(defun test-source-repl ( &optional (regression nil) )
  (destructuring-bind (dmem proc)
    (asm-n-run test-repl
               #'(lambda (dmem proc)
                   (let* ((env (default-env dmem)))
                     (mem-write-word dmem n-global-env env)
                     (when (not regression) (format t "env: ~d~%" env))
                     (set-source dmem 
                                    "(not t)(not (not (not nil)))(+ 10 11)
                                     (+ 6 5 4 3 2 1) (+ 1 1 1 1 1 1 1 1 1 1)
                                     (+ 1 2 3 4 5 6) (not (+ 1 2 3))")))
               nil regression 200000)
    (when (not regression) (print-conses dmem n-cons n-cons-type))
    (let ((reg-p0 (aref (lr-emulator::processor-state-r proc) P0)))
      (when (not regression) (format t "P0:~a~%" reg-p0)))))

(deftest run-source-repl  () (run-test #'test-source-repl "
> 
nil
> 
t
> 
21
> 
21
> 
10
> 
21
> 
nil
> "))

;;; use pty
(defun test-eval-read-print-loop ( &optional (regression nil) )
  (destructuring-bind (dmem proc)
    (asm-n-run test-repl
               #'(lambda (dmem proc)
                   (init-lisp)
                   (let* ((env (default-env dmem)))
                     (mem-write-word dmem n-global-env env)
                     (format t "env: ~d~%" env)
                     (set-program dmem '(0 0) n-source-start)))
               nil regression 10000 pty)
    (print-conses dmem n-cons n-cons-type)
    (format t "P0:~a~%"
            (aref (lr-emulator::processor-state-r proc) P0))))


;;; ------------------------------------------------------------------------
(defparameter test-func-evlis
  '( (Rx= n-stack-highest SP)
     (Rx= n-global-env R0)
     (A=Rx R0)
     (Rx=M[A].w P1)

     (jsr l-evlis)

     (label l-e-evlis) (j l-e-evlis)
     ))

;;; (cons 123 (cons 11 nil))
(defun make-num-list (dmem)
  (let* ((num2 (add-num dmem 11))
         (cons2 (make-cons dmem num2 0))
         (num1 (add-num dmem 123))
         (head (make-cons dmem num1 cons2)))
    head))

(defun test-evlis ( &optional (regression nil) )
  (destructuring-bind (dmem proc)
    (asm-n-run test-func-evlis
               #'(lambda (dmem proc)
                   (let* ((env (default-env dmem))
                          (lst (make-num-list dmem)))
                     (mem-write-word dmem n-global-env env)
                     (format t "env: ~d~%" env)
                     (format t "lst ~d~%" lst)
                     (setf (aref (lr-emulator::processor-state-r proc) P0) lst)))
               nil regression 10000)
    (when (not regression) (print-conses dmem n-cons n-cons-type))
    (let ((reg-p0 (aref (lr-emulator::processor-state-r proc) P0)))
      (when (not regression) (format t "P0:~a~%" reg-p0)))))

;;; ------------------------------------------------------------------------
(defparameter test-func-bind
  '( (Rx= n-stack-highest SP)
     (Rx= n-global-env R0)
     (A=Rx R0)
     (Rx=M[A].w P2) ; env

     (jsr l-bind)

     (label l-e-bind) (j l-e-bind)
     ))


;; (bind 'the-sym 1234 env)
(defun test-bind ( &optional (regression nil) )
  (destructuring-bind (dmem proc)
    (asm-n-run test-func-bind
               #'(lambda (dmem proc)
                   (let* ((env (default-env dmem))
                          (sym (add-symbol dmem "the-sym" 0))
                          (val (add-num dmem 1234)))
                     (mem-write-word dmem n-global-env env)
                     (format t "env: ~d~%" env)
                     (setf (aref (lr-emulator::processor-state-r proc) P0) sym)
                     (setf (aref (lr-emulator::processor-state-r proc) P1) val)))
               nil regression 10000)
    (when (not regression) (print-conses dmem n-cons n-cons-type))
    (let ((reg-p0 (aref (lr-emulator::processor-state-r proc) P0)))
      (when (not regression) (format t "P0:~a~%" reg-p0)))))

;; (bind '( sym1 sym2 ) '( 111 222 ) env)
(defun test-bind-lst ( &optional (regression nil) )
  (destructuring-bind (dmem proc)
    (asm-n-run test-func-bind
               #'(lambda (dmem proc)
                   (let* ((env (default-env dmem))
                          (sym1 (add-symbol dmem "sym1" 0))
                          (sym2 (add-symbol dmem "sym2" 0))
                          (val1 (add-num dmem 111))
                          (val2 (add-num dmem 222))
                          (cs2 (make-cons dmem sym2 0))
                          (cs1 (make-cons dmem sym1 cs2))
                          (cv2 (make-cons dmem val2 0))
                          (cv1 (make-cons dmem val1 cv2)))
                     (mem-write-word dmem n-global-env env)
                     (format t "env: ~d~%" env)
                     (format t "cs1 ~d~%" cs1)
                     (format t "cv1 ~d~%" cv1)
                     (setf (aref (lr-emulator::processor-state-r proc) P0) cs1)
                     (setf (aref (lr-emulator::processor-state-r proc) P1) cv1)))
               nil regression 10000)
    (when (not regression) (print-conses dmem n-cons n-cons-type))
    (let ((reg-p0 (aref (lr-emulator::processor-state-r proc) P0)))
      (when (not regression) (format t "P0:~a~%" reg-p0)))))

;;; ------------------------------------------------------------------------
(defparameter test-func-reduce
  '((Rx= n-stack-highest SP)
    
    (jsr l-reduce)
    (jsr l-print)

    (label l-e-red) (j l-e-red)
    ))

(defun create-lambda (dmem)
  (let* ((param (add-symbol dmem "x" 0))
         (func  (add-symbol dmem "not" 0))
         (body  (make-cons dmem func param))
         (lambda-inner (make-cons dmem param body))
         (lambda-outer (make-cons dmem lambda-inner 0))) ; env=nil
    lambda-outer))

;; ( (lambda (x) (not x) nil)
(defun test-reduce-not ( &optional (regression nil) )
  (destructuring-bind (dmem proc)
    (asm-n-run test-func-reduce
               #'(lambda (dmem proc)
                   (let* ((env (default-env dmem))
                          (param (add-symbol dmem "nil" 0))
                          (the-lambda (create-lambda dmem)))
                     (mem-write-word dmem n-global-env env)
                     (format t "env: ~d~%" env)
                     (format t "param ~d~%" param)
                     (format t "lambda ~d~%" the-lambda)
                     (setf (aref (lr-emulator::processor-state-r proc) P0) the-lambda)
                     (setf (aref (lr-emulator::processor-state-r proc) P1) param)
                     (setf (aref (lr-emulator::processor-state-r proc) P2) env)))
               nil regression 10000)
    (when (not regression) (print-conses dmem n-cons n-cons-type))
    (let ((reg-p0 (aref (lr-emulator::processor-state-r proc) P0)))
      (when (not regression) (format t "P0:~a~%" reg-p0))
      (check (equal reg-p0 1)))))

(deftest run-reduce-not  () (run-test #'test-reduce-not))
  
;;; ------------------------------------------------------------------------

;; lambda definition: ( (formal-params . body) . env )
(defun create-lambda-plus (dmem)
  (let* ((param1 (add-symbol dmem "x" 0))
         (param2 (add-symbol dmem "y" 0))
         (pcons1 (make-cons dmem param2 0))
         (pcons2 (make-cons dmem param1 pcons1))
         (func  (add-symbol dmem "+" 0))
         (body2 (make-cons dmem param2 0))
         (body1 (make-cons dmem param1 body2))
         (body  (make-cons dmem func body1))
         (lambda-inner (make-cons dmem pcons2 body))
         (lambda-outer (make-cons dmem lambda-inner 0))) ; env=nil
    lambda-outer))

;; ( (lambda (x y) (+ x y) 11 22)
(defun test-reduce-add ( &optional (regression nil) )
  (destructuring-bind (dmem proc)
    (asm-n-run test-func-reduce
               #'(lambda (dmem proc)
                   (let* ((env (default-env dmem))
                          (param1 (add-num dmem 11))
                          (param2 (add-num dmem 22))
                          (pcons1 (make-cons dmem param2 0))
                          (pcons2 (make-cons dmem param1 pcons1))
                          (the-lambda (create-lambda-plus dmem)))
                     (mem-write-word dmem n-global-env env)
                     (when (not regression)
                       (format t "env: ~d~%" env)
                       (format t "params ~d~%" pcons2)
                       (format t "lambda ~d~%" the-lambda))
                     (setf (aref (lr-emulator::processor-state-r proc) P0) the-lambda)
                     (setf (aref (lr-emulator::processor-state-r proc) P1) pcons2)
                     (setf (aref (lr-emulator::processor-state-r proc) P2) env)))
               nil regression 10000)
    (when (not regression) (print-conses dmem n-cons n-cons-type))
    (let ((reg-p0 (aref (lr-emulator::processor-state-r proc) P0)))
      (when (not regression) (format t "P0:~a~%" reg-p0)))))

(deftest run-reduce-add  () (run-test #'test-reduce-add "33"))
;;; ------------------------------------------------------------------------
(defun test-source-defvar ( &optional (regression nil) )
  (destructuring-bind (dmem proc)
    (asm-n-run test-repl-noecho
               #'(lambda (dmem proc)
                   (let* ((env (default-env dmem)))
                     (mem-write-word dmem n-global-env env)
                     (when (not regression) (format t "env: ~d~%" env))
                     (set-source dmem 
                                    "(defvar kalle 3)(+ 1 kalle)")))
               nil regression 200000)
    (when (not regression) (print-conses dmem n-cons n-cons-type))
    (let ((reg-p0 (aref (lr-emulator::processor-state-r proc) P0)))
      (when (not regression) (format t "P0:~a~%" reg-p0)))))

(deftest run-defvar  () (run-test #'test-source-defvar "kalle4"))

;;; ------------------------------------------------------------------------
(defun test-source-lambda ( &optional (regression nil) )
  (destructuring-bind (dmem proc)
    (asm-n-run test-repl-noecho
               #'(lambda (dmem proc)
                   (let* ((env (default-env dmem)))
                     (mem-write-word dmem n-global-env env)
                     (when (not regression) (format t "env: ~d~%" env))
                     (set-source dmem 
                                    "((lambda (x) (+ x x)) 3)")))
               nil regression 200000)
    (when (not regression) (print-conses dmem n-cons n-cons-type))
    (let ((reg-p0 (aref (lr-emulator::processor-state-r proc) P0)))
      (when (not regression) (format t "P0:~a~%" reg-p0)))))

(deftest run-lambda  () (run-test #'test-source-lambda "6"))
;;; ------------------------------------------------------------------------
(defun test-source-defun ( &optional (regression nil) )
  (destructuring-bind (dmem proc)
    (asm-n-run test-repl-noecho
               #'(lambda (dmem proc)
                   (let* ((env (default-env dmem)))
                     (mem-write-word dmem n-global-env env)
                     (when (not regression) (format t "env: ~d~%" env))
                     (set-source dmem 
                                    "(defvar z 3)(defun f (x) (+ x z))(f 12)")))
               nil regression 200000)
    (when (not regression) (print-conses dmem n-cons n-cons-type))
    (let ((reg-p0 (aref (lr-emulator::processor-state-r proc) P0)))
      (when (not regression) (format t "P0:~a~%" reg-p0)))))

(deftest run-defun  () (run-test #'test-source-defun "zf15"))
;;; ------------------------------------------------------------------------
(defun test-source-read-quote ( &optional (regression nil) )
  (destructuring-bind (dmem proc)
    (asm-n-run test-parse-3
               #'(lambda (dmem proc)
                   (let* ((env (default-env dmem)))
                     (mem-write-word dmem n-global-env env)
                     (when (not regression) (format t "env: ~d~%" env))
                     (set-source dmem 
                                    "(123 'sym0 '(sym2 sym1))")))
               nil regression 200000)
    (when (not regression) (print-conses dmem n-cons n-cons-type))
    (let ((reg-p0 (aref (lr-emulator::processor-state-r proc) P0)))
      (when (not regression) (format t "P0:~a~%" reg-p0)))))

(deftest run-read-quote  () (run-test #'test-source-read-quote "(123 (quote sym0) (quote (sym2 sym1)))"))
;;; ------------------------------------------------------------------------
(defun test-source-eval-quote ( &optional (regression nil) )
  (destructuring-bind (dmem proc)
    (asm-n-run test-repl-noecho
               #'(lambda (dmem proc)
                   (let* ((env (default-env dmem)))
                     (mem-write-word dmem n-global-env env)
                     (when (not regression) (format t "env: ~d~%" env))
                     (set-source dmem 
                                    "(defvar kalle '(1 2 3)) kalle")))
               nil regression 200000)
    (when (not regression) (print-conses dmem n-cons n-cons-type))
    (let ((reg-p0 (aref (lr-emulator::processor-state-r proc) P0)))
      (when (not regression) (format t "P0:~a~%" reg-p0)))))

(deftest run-eval-quote  () (run-test #'test-source-eval-quote "kalle(1 2 3)"))
;;; ------------------------------------------------------------------------

(deftest test-lisp ()
  (combine-results
    (run-t1)
    (run-t2)
    (run-t3)
    (run-t4)
    (run-t5)
    (run-t7)
    (run-t10)
    (run-t11)
    (run-t15)
    (run-t16)
    (run-assoc)
    (run-assoc-prim)
    (run-assoc2)
    (run-eval-num)
    (run-eval-symb)
    (run-eval-symb-nil)
    (run-eval-prim-symb)
    (run-eval-not-nil)
    (run-eval-add)
    (run-eval-read)
    (run-eval-read-print)
    (run-source-repl)
    (run-reduce-not)
    (run-reduce-add)
    (run-defvar)
    (run-lambda)
    (run-defun)
    (run-read-quote)
    (run-eval-quote)
    ))

;(run-emul e 200 nil)

