;(ql:quickload :str)
(asdf:load-system :literisc)
;(require :literisc)
(use-package :lr-asm)
(use-package :lr-disasm)

;;; ---- memory test for ROM / IMEM / DMEM --------

(defvar imem-ram 500); just large enough to jump over rom
(defvar dmem 8192) ; start of dmem
(format t "~%")
(let* ((text (assemble
           '(
             (Rx= hello-str R0)
             (Rx= imem-ram R1)
             (Rx= dmem R2)

             (label copy-loop)
             (A=M[Rx].b R0) ; read ROM
             (Rx=A R3) ;save char
             (M[Rx].b=A R1) ; write imem-ram
             (M[Rx].b=A R2) ; write dmem
             (A= 1) (A+=Rx R0) (Rx=A R0)
             (A= 1) (A+=Rx R1) (Rx=A R1)
             (A= 1) (A+=Rx R2) (Rx=A R2)
             (A= 0)
             (A-=Rx R3)
             (jnz copy-loop) ; loop until zero termination

             (Rx= hello-str R0)
             (Rx= imem-ram R1)
             (Rx= dmem R2)

             (label cmp-loop)
             (A=M[Rx].b R0) ; read ROM
             (Rx=A R3) ;save char
             (A=M[Rx].b R1) ; read imem-ram
             (A-=Rx R3) ; compare with ROM char
             (jz imem-ok)

             (A=Rx R1)
             (Rx=A R4) ; store fail address in R4

             (label imem-ok)
             (A=M[Rx].b R2) ; read dmem
             (A-=Rx R3) ; compare with ROM char
             (jz dmem-ok)

             (A=Rx R2)
             (Rx=A R5) ; store fail address in R5

             (label dmem-ok)

             (A= 1) (A+=Rx R0) (Rx=A R0)
             (A= 1) (A+=Rx R1) (Rx=A R1)
             (A= 1) (A+=Rx R2) (Rx=A R2)
             (A= 0)
             (A-=Rx R3)
             (jnz cmp-loop) ; loop until zero termination

             (label halt)
             (j halt)

             (label hello-str))))
       (data (string-to-mem "Hello World!"))
       (mcode (concatenate 'list text data)))
       (disasm text)
       (format t "~%")
       (hexdump mcode))
