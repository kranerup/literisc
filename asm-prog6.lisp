;(ql:quickload :str)
(asdf:load-system :literisc)
;(require :literisc)
(use-package :lr-asm)
(use-package :lr-disasm)

(defvar IO_LOW (- 65536 100))
(format t "~%")
(let* ((text (assemble
           '(
             (Rx= IO_LOW R0) ; R0 = IO_LOW

             (label the-loop)

             ;;;;;--- idle gen -------------------
             ;;;;; we need an idle period once in a while to allow uart receiver
             ;;;;; to synchronize. just continue counting gpio until enough zeros
             ;;;(label idle-loop)
             ;;;(A= 1)
             ;;;(A+=Rx R1)
             ;;;(Rx=A R1)       ; R1 = counter
             ;;;(M[Rx].b=A R0) ; counter -> GPIO
             ;;;(Rx= #x3ff R6) ; mask to get 1/65536 which should be long enough at 19200 baud and 10 MHz clock freq
             ;;;(A&=Rx R6)
             ;;;(Rx=A R8) ; debug
             ;;;(Rx= 0 R7)
             ;;;(A-=Rx R7) ; R7=0
             ;;;(jnz idle-loop) ; loop until 0

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

             ;; check serial tx ready
             (label tx-ready)
             (A=Rx R0) ; R0=IO
             (Rx=M[A+n].b 2 R4) ; IO+2 = ser-tx-ready -> R4
             ;; ser-ready == 1?
             (A= 1)
             (A-=Rx R4)
             (jnz tx-ready)
             ;; --- ready, send char ---
             (A=Rx R0) ; IO
             (M[A+n].b=Rx 1 R3) ; IO+1/ser-tx-data = ser-rx-data = R3

             (j rx-ready)

             (label hello-str))))
       (data (string-to-mem "Hello World!"))
       (mcode (concatenate 'list text data)))
       (disasm text)
       (format t "~%")
       (hexdump mcode))
