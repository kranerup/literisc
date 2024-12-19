(asdf:load-system :literisc)
(use-package :lr-asm)
(use-package :lr-disasm)

(defvar IO_LOW (- 65536 100))
(defparameter pty "/dev/pts/17")
(format t "~%")
(let* ((symtab (make-hash-table))
       (text (assemble
           '(
             (Rx= IO_LOW R0) ; R0 = IO_LOW

             (label the-loop)
             ;;----- print the zero terminated string ---------------
             (Rx= hello-str R9) ; str = R9 = the string to print

             ;; prt loop
             (label prt)
             (A=M[Rx].b R9) ; c
             (Rx=A R8) ; R8=c
             (A= 0) ; c == 0?
             (A-=Rx R8) 
             (jz end-of-string)

             ;; check serial ready
             (label wait-ready)
             (A=Rx R0) ; R0=IO
             (Rx=M[A+n].b 2 R2) ; IO+2 = ser-ready -> R2
             ;; ser-ready == 1?
             (A= 1)
             (A-=Rx R2)
             (jnz wait-ready)
             ;; --- ready, send char ---
             (A=Rx R0) ; IO
             (M[A+n].b=Rx 1 R8) ; IO+1/ser-tx-data = R8
             ;; --- str++
             (A= 1)
             (A+=Rx R9)
             (Rx=A R9)
             (j prt)
             ;; ----
             (label end-of-string)
             (j the-loop)
             (label hello-str))
           nil
           symtab))
       (data (string-to-mem "Hello World!"))
       (mcode (concatenate 'list text data))
       (dmem (make-dmem 1000))
       (emul (make-emulator mcode dmem :shared-mem t :debug nil)))
       (disasm text)
       (format t "~%")
       (hexdump mcode)
       (run-with-curses-io emul pty symtab))
