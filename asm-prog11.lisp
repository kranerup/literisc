(asdf:load-system :literisc)
(use-package :lr-asm)
(use-package :lr-disasm)
(in-package :cl-user)

;;; ---- led blinker for xmodem boot load --------

(defparameter imem-ram 500); just large enough to jump over rom
(defparameter dmem 8192) ; start of dmem
(defparameter dmem-end (logand (+ 8192 16383) #xfffffffc))
(defparameter IO (- 65536 100))
;;; offset from IO
(defparameter io-gpio-out 0)
;;; gpio led bit numbers:
(defparameter io-gpio-led-0 20)
(defparameter io-gpio-led-1 21)

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
             (Rx= 0 R9) ; counter

             (label the-loop)
             (Rx= IO R0)
             (Rx= io-gpio-out R1)
             (A=Rx R0)
             (A+=Rx R1) ; A = gpio-address
             (Rx=A R0) ; R0 = gpio-address

             (A= 1)
             (A+=Rx R9) ; inc counter
             (Rx=A R9)
             
             (M[Rx]=A R0) ; counter -> gpio
            
             (j the-loop)
             ) nil *symtab*)))

             ;; ---------------------------------
       (disasm text)
       (format t "~%")
       (hexdump text)
       (output-binary-file text "asm.bin")
       (setf program text))
