(require :literisc)
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
      (M[Rx].b=A R0)
      (j the-loop))))
