;(ql:quickload :str)
(asdf:load-system :literisc)
;(require :literisc)
(use-package :lr-asm)
(use-package :lr-disasm)

(defvar IO_LOW (- 65536 100))
(defvar IMEM_HIGH 8191)
(format t "~%")
(let* ((text (assemble
           '(
             (Rx= IMEM_HIGH R1)
             (Rx= 0 R2) ; imem addr
             (Rx= 16384 R3) ; dmem addr

             (label the-loop)
             (A=M[Rx].b R3) ; read DMEM
             (A=M[Rx].b R1) ; read IMEM RAM
             (A=M[Rx].b R2) ; read ROM
             (M[Rx].b=A R1) ; write IMEM RAM

             (j the-loop)

             (label hello-str))))
       (data (string-to-mem "Hello World!"))
       (mcode (concatenate 'list text data)))
       (disasm text)
       (format t "~%")
       (hexdump mcode))
