(ql:quickload :literisc)

;(defmacro part3 (reg)
;  `'(
;    (mvi->r 0 ,reg)))
;(masm (part1) (part2) (part3 3))

(defconstant R0  0 )
(defconstant R1  1 )
(defconstant R2  2 )
(defconstant R3  3 )
(defconstant R4  4 )
(defconstant R5  5 )
(defconstant R6  6 )
(defconstant R7  7 )
(defconstant R8  8 )
(defconstant R9  9 )
(defconstant R10 10) (defconstant P0 R10)
(defconstant R11 11) (defconstant P1 R11)
(defconstant R12 12) (defconstant P2 R12)
(defconstant R13 13) (defconstant P3 R13)

(defvar dmem nil)
(defvar dmem-allocated 0)
(setq dmem (make-dmem 1000))

(defmacro alloc-init ( sym data )
  `(progn
     (defvar ,sym nil)
     (setq ,sym dmem-allocated)
     (let* ((d ,data)
            (d-len (list-length d)))
       (set-program dmem d dmem-allocated)
       (setq dmem-allocated (+ dmem-allocated d-len)))))

(defmacro alloc ( sym len )
  `(progn
     (defvar ,sym nil)
     (setq ,sym dmem-allocated)
     (setq dmem-allocated (+ dmem-allocated ,len))))

(defvar string-space-free 0) ; relative n-string-space

(defvar cons-free nil)

;;; cons-type: [ t:8 ]  (msb..lsb)
;;;   [ 0:4 | gc:1 | type:3 ]
;;;   type: 0 - free
;;;         1 - number
;;;         2 - symbol
;;;         3 - char
;;;         4 - cons
;;;         5 - macro
;;;         6 - function
;;;         7 - special function
;;;
;;; cons: [ f1:16 | f2:16 ]
;;;       [      f:32     ]
;;;   type   |    f1    |     f2    |
;;;   -------+----------+-----------+
;;;   symbol | name-ptr | value-ptr |
;;;     name-ptr:string space ptr
;;;     value-ptr:cons cell ptr
;;;   cons   | car-ptr  | cdr-ptr   |
;;;     car/cdr-ptr: cons cell ptr
;;;          |          f           |
;;;   number |        value         |
;;;   char   |        value         |
;;;   

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
(defconstant c-cons-spec-func 7)

(defvar n-stack-highest nil)

(defun init-lisp ()
  (setq dmem-allocated 0)
  (alloc-init n-source-start     (string-to-mem "symbol"))
  (alloc-init n-sym-string-start (string-to-mem "symbol"))
  (alloc-init n-print-sep        (string-to-mem "---
"))
  (alloc n-read-sym-str 20)

  (alloc n-string-space 200)
  (setq string-space-free 0)

  (defparameter nr-cons 20)
  (defparameter cons-size 4) ; bytes
  (alloc n-cons (* nr-cons cons-size))
  (alloc n-cons-type nr-cons)
  (setq cons-free 0) ; points to entry nr in n-cons table

  (alloc n-stack (* 100 4))
  (setq n-stack-highest (- dmem-allocated 1)))


  

;;; -- sym-strings --
(defun add-symbol (sym-name value-ptr)
(let ((str (string-to-mem sym-name))
      (str-ptr nil)
      (cons-ptr cons-free))
  (set-program dmem str (+ n-string-space string-space-free))
  (setf str-ptr string-space-free)

  (set-program dmem (list c-cons-symbol )
               (+ n-cons-type cons-ptr)) ; one byte per cons-type item
  (format t "ccell ~x cbytes ~a~%"
          (cons-cell str-ptr value-ptr)
          (cons-bytes (cons-cell str-ptr value-ptr)))
          
  (set-program dmem
               (cons-bytes 
                 (cons-cell str-ptr value-ptr))
               (+ n-cons (* cons-size cons-ptr)))

  (setf cons-free (1+ cons-free))
  (setq string-space-free (+ string-space-free
                             (list-length str)))))

(add-symbol "symbol" 0)
(add-symbol "symbol2" 0)
;(set-program dmem (string-to-mem "symbol2") (+ (* sym-length n-sym-strings)))

;(set-program dmem (string-to-mem "symbol next ( inside ) end") source-start)
;(set-program dmem (string-to-mem "symbol") source-start)
;(set-program dmem (string-to-mem "symbol") sym-string-start)



;;; P3 - use-unread (updated)
;;; P2 - last-read (updated)
;;; P1 - read-ptr (updated)
;;; P0 - returns char
(defvar read-c nil)
(setq read-c
  '( (label l-read-c)
     ;; if use-unread == 0
     (mvi->a 0)
     (sub-r P3)
     (jnz l-use-unread)
     ;; read char
     ;; char = source[ read-ptr ]
     (ld.b-r->a P1)
     (a->r P0)
     ;; read-ptr =+ 1
     (mvi->a 1)
     (add-r P1)
     (a->r P1)
     ;; last-read = char
     (r->a P0) (a->r P2)
     ;; return
     (r->a srp)
     (j-a)

     (label l-use-unread)
     (mvi->a 0) ; use-unread = 0
     (a->r P3)
     (r->a P2) (a->r P0) ; char = last-read
     ;; return
     (r->a srp)
     (j-a)

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
     
     
     ;; setup:
     ;; P3 - use-unread
     (mvi->r 0 P3)
     ;; P1 - read-ptr
     (mvi->r n-source-start P1)

     ;; ------- reader 1 --------------
     ;(jsr f-reader)
     ;(r->a P0) (a->r R0)

     ;(mvi->r n-cons-type R1) ; R1 cons type table ptr
     (mvi->r n-cons R2) ; R2 cons table ptr
     (mvi->r n-string-space R3) ; strings
     ;(ld.b-r->a R1) ;; ignore value for now

     ;; n-cons + 2
     (r->a R2)
     
     (label l-next-cons)
     (a->r R4)
     (mvi->a 2) ; high word
     (add-r R4)
     (a->r R4)
     ;; read cdr
     (ld.w-r->a R4) ; cdr -> sym-name-ptr
     (add-r R3) ; string-space + sym-name-ptr
     (a->r R1)
     (jsr prtstr)

     ;; cons-ptr += 4
     (mvi->a 4)
     (add-r R4)
     (a->r R4)

     (j l-next-cons)

     
     (label stop)
     (j stop)
     
     
     (mvi->r n-read-sym-str R1)
     (jsr prtstr)
     
     
     
     ;; ------- reader 1 --------------
     (jsr f-reader)
     (r->a P0) (a->r R0)

     (mvi->r 0 R1)
     (jsr prtstr)

     (mvi->r n-read-sym-str R1)
     (jsr prtstr)

     ;; ------- reader 2 --------------
     (jsr f-reader)
     (r->a P0) (a->r R0)

     (mvi->r 0 R1)
     (jsr prtstr)

     (mvi->r n-read-sym-str R1)
     (jsr prtstr)

     ;; ------- reader 3 --------------
     (jsr f-reader)
     (r->a P0) (a->r R0)

     (mvi->r 0 R1)
     (jsr prtstr)

     (mvi->r n-read-sym-str R1)
     (jsr prtstr)

     ;; ------- reader 4 --------------
     (jsr f-reader)
     (r->a P0) (a->r R0)

     (mvi->r 0 R1)
     (jsr prtstr)

     (mvi->r n-read-sym-str R1)
     (jsr prtstr)

     ;; ------- reader 5 --------------
     (jsr f-reader)
     (r->a P0) (a->r R0)

     (mvi->r 0 R1)
     (jsr prtstr)

     (mvi->r n-read-sym-str R1)
     (jsr prtstr)

     ;; ------- reader 6 --------------
     (jsr f-reader)
     (r->a P0) (a->r R0)

     (mvi->r 0 R1)
     (jsr prtstr)

     (mvi->r n-read-sym-str R1)
     (jsr prtstr)

     (label end)
     (j end)))

(defvar func-prtstr nil)
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

(setq func-prtstr
  '( ;; --- prtstr ---
     ;; P0 - ptr to zero terminated string
     (label prtstr)
     (push-r R1)
     (ld.b-r->a P0)
     (mask-a-b)
     (a->r R0)
     (mvi->r -1 R1) ; ptr to I/O reg
     (mvi->a 0)
     (sub-r R0)
     (jz ret-prtstr)
     (r->a R0)
     (st-a->r R1)
     (mvi->a 1)
     (add-r P0)
     (a->r P0)
     (j prtstr)
     (label ret-prtstr)
     (pop-r R1)
     (r->a srp)
     (j-a)))

(defvar func-str-equal nil)
(setq func-str-equal
  '( ;; str-equal
     ;; P1 - string-ptr
     ;; P0 - string-ptr / return value
     ;; clobbers R0,R1
     (label l-str-equal)
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
     (r->a SRP)
     (j-a)

     (label l-ret-uneq)
     (mvi->r 0 P0)
     (r->a SRP)
     (j-a)
     
     (label l-inc-strs)
     (mvi->a 1)
     (add-r P1)
     (a->r P1)
     (mvi->a 1)
     (add-r P0)
     (a->r P0)
     (j l-str-equal)))

;;; P3 - use-unread (updated)
;;; P2 - last-read (updated)
;;; P1 - read-ptr (updated)
;;; P0 - returns read object
(defconstant reader-lpar 1)
(defconstant reader-rpar 2)
(defconstant reader-sym 3)

(defvar reader nil)
(setq reader
  '(
    (label f-reader)
    (push-srp)
    (push-r R4)
    ;; R1 - read-sym-ptr
    (mvi->r n-read-sym-str R1)
    ;; R3 - io-ptr
    (mvi->r -1 R3) ; ptr to I/O reg
    ;; R4 - reading-symbol
    (mvi->r 0 R4)

    (label l-rd-more)
    (jsr l-read-c) ; P1=read-ptr
    ;; R0 = char
    (r->a P0) (a->r R0)

    ;; --- if reading-symbol -----
    (mvi->a 0)
    (sub-r R4)
    (jz l-not-rd-sym)

    ;; ----------- reading-symbol --------------
    ;; end of symbol?
    ;; if c in ['(',')',' ',0]:  
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
    (mvi->a 0)
    (sub-r R0)
    (jnz l-read-sym-char)

    (label l-end-of-sym-unrd)
    ;; unread-c
    (r->a R0) (a->r P2) ; last-read = char
    (mvi->r 1 P3) ; use-unread = 1
    (label l-end-of-sym)
    ;; reading-symbol = False
    (mvi->r 0 R4)
    ;; return sym
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
    ;; is it a space then just read next char
    (mvi->r (char-code #\ ) R2)
    (r->a R2)
    (sub-r R0) ; ' ' - R0
    (jz l-rd-more) ; loop back and read next char

    ;; all other chars are written to sym buffer
    (label l-read-sym-char)
    (r->a R0)
    (st-a->r R1) ; M[read-sym-str] = char
    (mvi->a 1)
    (add-r R1) ; read-sym-str++
    (a->r R1)
    ;; debug print
    (r->a R0)
    (st-a->r R3); putchar
    ;; reading-symbol = True
    (mvi->r 1 R4)
    (j l-rd-more) ; loop back and read next char

    ;; return
    (label l-reader-ret)
    (pop-r R4)
    (pop-a)
    (j-a)
    
    ))


(defvar *hello-world* nil)
;(setq *hello-world*
;      (masm main func-prtstr read-c reader))

(defvar e nil)
                                  
;(setf e (make-emulator *hello-world* dmem 200 nil))
;(run-with-curses e)

(defun asm-n-run ()
  (setq *hello-world*
        (masm main func-prtstr read-c reader func-str-equal))
  (setf e (make-emulator *hello-world* dmem 200 nil))
  (run-with-curses e))

;(run-emul e 200 nil)
(quit)

