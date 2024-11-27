;(ql:quickload :str)
(asdf:load-system :literisc)
;(require :literisc)
(use-package :lr-asm)

(defvar IO_LOW (- 65536 100))
(format t "~%")
(hexdump
  (assemble
    '(
      (Rx= IO_LOW R0)
      (label the-loop)
      (A= 1)
      (A+=Rx R1)
      (Rx=A R1)
      (M[Rx].b=A R0) ; counter -> GPIO
      (A=Rx R0) ; R0=IO
      (Rx=M[A+n].b 2 R2) ; IO+2 = ser-ready -> R2
      ;; ser-ready == 1?
      (A= 1)
      (A-=Rx R2)
      (jnz skip-serial)
      ;; --- ready, send char ---
      (Rx= R3 #x40) ; ascii 'A'
      (Rx= R5 #xf)
      (A=Rx R5)
      (A&=Rx R0) ; counter & 0x0000000f
      (A+=Rx R3) ; + 'A'
      (Rx=A R4) ; R4 = 'A'+(counter&0x0f)
      (A=Rx R0) ; IO
      (M[A+n].b=Rx 1 R4) ; IO+1 = R4
      ;; --- ser send char done --- 
      (label skip-serial)
      (j the-loop))))

