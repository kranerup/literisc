(ql:quickload :cl-charms)
(ql:quickload :str)
(load "macro-utilities")
(load "unit")
(load "assembler")
(load "disassembler")
(load "charms-extra")
(load "emulator")

(defvar *hello-world*
      (assemble 
        '( 
           ; --- main ---
           (mvi->r 0 0)
           (jsr prtstr)
           (label end)
           (j end)
           ; --- prtstr ---
           ; r0 - ptr to zero terminated string
           (label prtstr)
           (ld.b-r->a 0)
           (mask-a-b)
           (a->r 1)
           (mvi->r -1 2) ; ptr to I/O reg
           (mvi->a 0)
           (sub-r 1)
           (jz ret-prtstr)
           (r->a 1)
           (st-a->r 2)
           (mvi->a 1)
           (add-r 0)
           (a->r 0)
           (j prtstr)
           (label ret-prtstr)
           (r->a srp)
           (j-a))))

(defvar e nil)
(defvar dmem nil)
(setq dmem (make-dmem 1000))
(set-program dmem (string-to-mem "Hello World!"))
                                  
(setf e (make-emulator *hello-world* dmem 200 t))
(run-with-curses e)
(quit)

