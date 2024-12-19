(asdf:load-system :literisc)
(use-package :lr-asm)
(use-package :lr-disasm)
(in-package :cl-user)

;;; ---- xmodem boot loader --------

(defparameter imem-ram 500); just large enough to jump over rom
(defparameter dmem 8192) ; start of dmem
(defparameter dmem-end (logand (+ 8192 16383) #xfffffffc))
(defparameter IO (- 65536 100))
;;; offset from IO
(defparameter io-reg-tx-data 1)
(defparameter io-reg-tx-ready 2)
(defparameter io-reg-rx-data 3)
(defparameter io-reg-rx-ready 4)

(defparameter SOH #x01)
(defparameter EOT #x04)
(defparameter ACK #x06)
(defparameter NAK #x15)
(defparameter CAN #x18)

(format t "~%")
(defvar program nil)



(defparameter *symtab* (make-hash-table))

(let* ((text (assemble
           '(
             (Rx= dmem-end SP)
             (Rx= 1 R0) ; expected block number
             (Rx= imem-ram R1) ; current buffer
             (Rx= imem-ram R2) ; last successful buffer

             ;; R6 = received block number
             ;; R7 = previous block number
             (Rx= 1 R7)

             (Rx= NAK P0)
             (jsr write-ch)

             (label trf-loop)
             (jsr read-ch)

             ;; if EOT then we're done
             (label cmp-eot)
             (Rx= EOT R3)
             (A=Rx P0)
             (A-=Rx R3)
             (jz end-of-tfr)
            
             ;; should be SOH else restart
             (label cmp-soh)
             (Rx= SOH R3)
             (A=Rx P0)
             (A-=Rx R3)
             (jnz trf-loop)

             (label rd-bnr)
             (jsr read-ch)
             (A=Rx P0)
             (Rx=A R6) ; block nr (R6)

             (label rd-ibnr)
             (jsr read-ch) ; block nr inverted
             (A=Rx P0)
             (Rx=A R8) ; inverted in R8

             (label cmp-bnr)
             (A=Rx R6)
             (A+=Rx R8) ; bnr+ibnr == 255
             (mask-a-b)
             (Rx= #xff R9)
             (A-=Rx R9) ; ==255?
             (jz block-nr-ok)

             ;; failed block nr check, send NAK and retry
             (Rx= NAK P0)
             (jsr write-ch)
             (j trf-loop)
             
             (label block-nr-ok)
             (Rx= 0 R3) ; checksum
             (Rx= 0 R4) ; byte-count

             (label read-block)
             (jsr read-ch)
             (label wr-byte)
             (A=Rx P0) ; write received byte to block
             (M[Rx].b=A R1)

             (label inc-ptr)
             (A= 1)      ; increment block pointer
             (A+=Rx R1)
             (Rx=A R1)

             (label upd-csum)
             (A=Rx R3) ; update checksum
             (A+=Rx P0) ; checksum += received-byte
             (Rx=A R3)

             (label upd-cnt)
             (A= 1)
             (A+=Rx R4)
             (Rx=A R4)  ; byte-count++

             (Rx= 128 R5) ; byte-count == 128 ?
             (A-=Rx R5)
             (jnz read-block)
             
             (label read-csum)
             (jsr read-ch) ; read checksum byte

             (label csum-check)
             (A=Rx R3)
             (mask-a-b) ; only one byte checksum
             (A-=Rx P0) ; equal?
             (jz csum-ok)

             ;; failed checksum check, restore block pointer, send NAK and retry
             (A=Rx R2) ; restore pointer
             (Rx=A R1)

             (Rx= NAK P0)
             (jsr write-ch)
             (j trf-loop)

             (label csum-ok)

             ;; validate block number
             (A=Rx R0) ; expected
             (A-=Rx R6) ; received
             (jz block-expected)

             (A=Rx R7) ; previous block number
             (A-=Rx R6) ; received
             (jz block-retransmitted) ; previous block was retransmitted
           
             ;; wrong block number
             (A=Rx R2)
             (Rx=A R1) ; only restore buffer pointer

             (Rx= NAK P0) ; tell transmitter it was not ok so we get retransmission
             (jsr write-ch)

             (j trf-loop)


             (label block-expected)
             ;; incr block nr, update buffer pointer
             (A=Rx R0)
             (Rx=A R7) ; save block nr so we can detect retransmit

             (A= 1) ; increment expected block number with byte wrap
             (A+=Rx R0)
             (mask-a-b)
             (Rx=A R0)

             (A=Rx R1)
             (Rx=A R2) ; update successful buffer pointer

             (Rx= ACK P0) ; tell transmitter it was ok
             (jsr write-ch)

             (j trf-loop)

             (label block-retransmitted)
             ;; it was the previous block retransmitted so we
             ;; need to go back and recieve the current block again
             (A=Rx R2)
             (Rx=A R1) ; only restore buffer pointer

             (Rx= ACK P0) ; tell transmitter it was ok so he doesn't retransmit again
             (jsr write-ch)

             (j trf-loop)


             ;; -----------------------
             ;; end of transfer, code has been loaded, jump to start of code
             (label end-of-tfr)
             (Rx= imem-ram R0)
             (A=Rx R0)
             (j-a)

             ;; ---- read-ch --------------------
             ;; params:  -
             ;; returns: P0 = serial byte received
             (label read-ch)
             (push-r R1)
             (Rx= IO R0)

             (label rx-ready)
             (A=Rx R0) ;
             (Rx=M[A+n].b 4 R1) ; IO+4 = ser-rx-ready -> R1
             ;; ser-rx-ready == 1?
             (A= 1)
             (A-=Rx R1)
             (jnz rx-ready)

             ;; get the rx data
             (A=Rx R0) ; R0=IO
             (Rx=M[A+n].b 3 P0) ; IO+3 = ser-rx-data -> P0

             (pop-r R1)
             (A=Rx SRP)
             (j-a)

             ;; ---- write-ch --------------------
             ;; params:  P0 = serial byte to transmit 
             ;; returns: -
             (label write-ch)
             (push-r R1)
             (Rx= IO R0)

             ;; check serial tx ready
             (label tx-ready)
             (A=Rx R0) ; R0=IO
             (Rx=M[A+n].b 2 R1) ; IO+2 = ser-tx-ready -> R1
             ;; ser-ready == 1?
             (A= 1)
             (A-=Rx R1)
             (jnz tx-ready)
             ;; --- ready, send char ---
             (A=Rx R0) ; IO
             (M[A+n].b=Rx 1 P0) ; IO+1/ser-tx-data = ser-rx-data = P0

             (pop-r R1)
             (A=Rx SRP)
             (j-a)) nil *symtab*))

             ;; ---------------------------------
       (data (string-to-mem "Hello World!"))
       (mcode (concatenate 'list text data)))
       (disasm text)
       (format t "~%")
       (hexdump mcode)
       (setf program mcode))
