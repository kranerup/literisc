(ql:quickload :literisc)

;(defmacro part3 (reg)
;  `'(
;    (mvi->r 0 ,reg)))
;(masm (part1) (part2) (part3 3))

;;; calling convention:
;;; - return value in R0
;;; - parameters in R1..
;;; - A is allowed to be clobbered

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

(defconstant source-start 300)
(defconstant read-sym-str 100)
(defconstant stack-highest 400)

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
     (jne l-use-unread)
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
     (mvi->r stack-highest SP)
     ;; setup:
     ;; P3 - use-unread
     (mvi->r 0 P3)
     ;; P1 - read-ptr
     (mvi->r source-start P1)

     (jsr f-reader)
     (r->a P0) (a->r R0)
     (label end)
     (j end)))

(defvar func-prtstr nil)
(setq func-prtstr
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
    (mvi->r read-sym-str R1)
    ;; R3 - io-ptr
    (mvi->r -1 R3) ; ptr to I/O reg
    ;; R4 - reading-symbol
    (mvi->r 0 R4)

    (label l-rd-more)
    (jsr l-read-c) ; P1=read-ptr
    ;; R0 = char
    (r->a P0) (a->r R0)
    ;; if reading-symbol
    (mvi->a 0)
    (sub-r R4)
    (jz l-not-rd-sym)

    ;; end of symbol?
    ;; if c in ['(',')',' ',0]:  
    (mvi->r (char-code #\( ) R2)
    (r->a R2)
    (sub-r R0)
    (jz l-end-of-sym)
    (mvi->r (char-code #\) ) R2)
    (r->a R2)
    (sub-r R0)
    (jz l-end-of-sym)
    (mvi->r (char-code #\  ) R2)
    (r->a R2)
    (sub-r R0)
    (jz l-end-of-sym)
    (mvi->a 0)
    (sub-r R0)
    (jnz l-read-sym-char)

    (label l-end-of-sym)
    ;; unread-c
    (r->a R0) (a->r P2) ; last-read = char
    (mvi->r 1 P3) ; use-unread = 1
    ;; reading-symbol = False
    (mvi->r 0 R4)
    ;; return sym
    (mvi->r reader-sym P0)
    (j l-reader-ret)
    
    ;; else
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
    (j l-rd-more)
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
(setq *hello-world*
      (masm main func-prtstr read-c reader))

(defvar e nil)
(defvar dmem nil)
(setq dmem (make-dmem 1000))
(set-program dmem (string-to-mem "Hello World!"))
(set-program dmem (string-to-mem "symbol") source-start)
                                  
(setf e (make-emulator *hello-world* dmem 200 nil))
(run-with-curses e)

(defun asm-n-run ()
  (setq *hello-world*
        (masm main func-prtstr read-c reader))
  (setf e (make-emulator *hello-world* dmem 200 nil))
  (run-with-curses e))

;(run-emul e 200 nil)
(quit)

