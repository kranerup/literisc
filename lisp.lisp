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

;;; P1 - read-ptr (updated)
;;; P0 - returns char
(defvar read-c nil)
(setq read-c
  '( (label l-read-c)
     (ld.b-r->a P1)
     (a->r P0)
     (mvi->a 1)
     (add-r P1)
     (a->r P1)
     (r->a srp)
     (j-a)))

(defvar main nil)
(setq main 
  '( ;; --- main ---
     (mvi->r stack-highest SP)
     (mvi->r source-start R0)
     (r->a R0) (a->r P1)
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

;;; P1 - read-ptr (updated)
;;; P0 - returns read object
(defvar reader nil)
(setq reader
  '(
    (label f-reader)
    (push-srp)
    (push-r R3)
    (mvi->r read-sym-str R1)
    (mvi->r -1 R3) ; ptr to I/O reg

    (label l-rd-more)
    (jsr l-read-c) ; P1=read-ptr
    (r->a P0) (a->r R0) ; R0 = char
    (mvi->a 0)
    (sub-r R0)
    (jz l-end-input) ; if char == 0
    (mvi->r (char-code #\ ) R2)
    (r->a R2)
    (sub-r R0) ; ' ' - R0
    (jz l-rd-more) ; skip spaces
    (mvi->r (char-code #\( ) R2)
    (r->a R2)
    (sub-r R0) ; '(' - R0
    (jz l-lparen)
    (r->a R0)
    (st-a->r R1) ; M[read-sym-str] = char
    (mvi->a 1)
    (add-r R1) ; read-sym-str++
    (a->r R1)
    (r->a R0)
    (st-a->r R3); putchar
    (j l-rd-more)

    (label l-end-input)
    (mvi->r 0 P0)
    (pop-r R3)
    (pop-a)
    (j-a)
    
    (label l-lparen)
    (j l-end-input)
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

