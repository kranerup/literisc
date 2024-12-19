(asdf:load-system :literisc)
(use-package :lr-asm)
(use-package :lr-disasm)
(in-package :cl-user)

(defvar IO_LOW (- 65536 100))
(defparameter pty "/dev/pts/17")
(format t "~%")
(let* ((symtab (make-hash-table))
       (text (assemble
           '(
             (Rx= IO_LOW R0) ; R0 = IO_LOW
             (Rx= -1 R1) ; R1 = emulator output

             (label the-loop)

             ;; wait for serial receive
             (label rx-ready)
             (A=Rx R0) ; R0=IO
             (Rx=M[A+n].b 4 R2) ; IO+4 = ser-rx-ready -> R2
             ;; ser-rx-ready == 1?
             (A= 1)
             (A-=Rx R2)
             (jnz rx-ready)
             ;; get the rx data
             (A=Rx R0) ; R0=IO
             (Rx=M[A+n].b 3 R3) ; IO+3 = ser-rx-data -> R3

             (A=Rx R1)
             (M[A].b=Rx R3) ; char to emulator output
            
             (j the-loop))
           nil
           symtab))
       (dmem (make-dmem 1000))
       (emul (make-emulator text dmem :shared-mem t :debug nil)))
       (disasm text)
       (format t "~%")
       (hexdump text)
       (run-with-curses-io emul pty symtab))
