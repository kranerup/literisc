(asdf:load-system :literisc)

(use-package :lr-asm)
(use-package :lr-disasm)

(defvar program nil)

(let* ((text (assemble
        '( 
           ; --- main ---
           (Rx= 0 R0)
           (jsr prtstr)
           (label end)
           (j end)
           ; --- prtstr ---
           ; r0 - ptr to zero terminated string
           (label prtstr)
           (A=M[Rx].b R0)
           (mask-a-b)
           (Rx=A R1)
           (Rx= -1 R2) ; ptr to I/O reg
           (A= 0)
           (A-=Rx R1)
           (jz ret-prtstr)
           (A=Rx R1)
           (M[Rx]=A R2)
           (A= 1)
           (A+=Rx R0)
           (Rx=A R0)
           (j prtstr)
           (label ret-prtstr)
           (A=Rx srp)
           (j-a))))
             ;; ---------------------------------
       (data '())
       (mcode (concatenate 'list text data)))
       (disasm text)
       (format t "~%")
       (hexdump mcode)
       (setf program mcode))
