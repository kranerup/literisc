;(ql:quickload :str)
(asdf:load-system :literisc)
;(require :literisc)
(use-package :lr-asm)
(use-package :lr-disasm)

(defvar IO_LOW (- 65536 100))
(format t "~%")
(let* ((mcode
         (assemble
           '(
             (Rx= IO_LOW R0) ; R0 = IO_LOW
             (label the-loop)
             (A= 1)
             (A+=Rx R1)
             (Rx=A R1)       ; R1 = counter
             (M[Rx].b=A R0) ; counter -> GPIO
             ;; check serial ready
             (A=Rx R0) ; R0=IO
             (Rx=M[A+n].b 2 R2) ; IO+2 = ser-ready -> R2
             ;; test imem access
             (A= 0)
             (Rx=M[A+n].b 0 R9) ; read imem at 0
             ;; ser-ready == 1?
             (A= 1)
             (A-=Rx R2)
             (jnz skip-serial)
             ;; --- ready, send char ---
             (Rx= #xf R5)
             (A=Rx R5)
             (A&=Rx R1) ; R5 = counter & 0xf
             (Rx=A R5)
             (A= 0)
             (A-=Rx R5) ; R5 == 0
             (jnz skip-idle-gen)
             ;;--- idle gen
             ;; we need an idle period once in a while to allow uart receiver
             ;; to synchronize. just continue counting gpio until enough zeros
             (label idle-loop)
             (A= 1)
             (A+=Rx R1)
             (Rx=A R1)       ; R1 = counter
             (M[Rx].b=A R0) ; counter -> GPIO
             (Rx= #x3ff R6) ; mask to get 1/65536 which should be long enough at 19200 baud and 10 MHz clock freq
             (A&=Rx R6)
             (Rx=A R8) ; debug
             (Rx= 0 R7)
             (A-=Rx R7) ; R7=0
             (jnz idle-loop) ; loop until 0
             ;;-----
             (label skip-idle-gen)
             (A=Rx R5) ; A= counter& 0xf
             (Rx= #x40 R3) ; ascii 'A'
             (A+=Rx R3) ; A = (counter&0xf) + 'A'
             (Rx=A R4) ; R4 = 'A'+(counter&0x0f)
             (A=Rx R0) ; IO
             (M[A+n].b=Rx 1 R4) ; IO+1 = R4
             ;; --- ser send char done --- 
             (label skip-serial)
             (j the-loop)))))
       (disasm mcode)
       (format t "~%")
       (hexdump mcode))
