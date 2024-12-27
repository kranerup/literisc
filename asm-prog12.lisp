;;; Prints hello world on the serial port. For loading
;;; with xmodem.

(asdf:load-system :literisc)
(use-package :lr-asm)
(use-package :lr-disasm)
(use-package :lr-soc)

(defparameter char-output :uart-io) ; :uart-io / :emul-io

(defparameter func-putchar
  (cond ((equal char-output :emul-io)
         '( ;; --- putchar ---
            ;; params: P0 - char to print
            ;; returns: -
            (label l-putchar)
            (push-r R0)
            (mvi->r -1 R0) ; ptr to emulator char output reg
            (r->a P0)
            (M[Rx].b=A R0)
            (pop-r R0)
            (r->a srp)
            (j-a)))
        ((equal char-output :uart-io)
         '( ;; --- putchar ---
            ;; params: P0 - char to print
            ;; returns: -
            (label l-putchar)
            (push-r R1)
            (Rx= io-base-address R0)

            ;; check serial tx ready
            (label tx-ready)
            (A=Rx R0) ; R0=IO
            (Rx=M[A+n].b serial_tx_status_address R1) ; IO+2 = ser-tx-ready -> R1
            ;; ser-ready == 1?
            (A= 1)
            (A-=Rx R1)
            (jnz tx-ready)
            ;; --- ready, send char ---
            (A=Rx R0) ; IO
            (M[A+n].b=Rx serial_tx_data_address P0) ; IO+1/ser-tx-data = P0

            (pop-r R1)
            (A=Rx SRP)
            (j-a)))))


(defparameter *symtab* nil)
(defparameter *program* nil)


(format t "~%")
(let* ((symtab (make-hash-table))
       (text (assemble
               (append
           '(
             (Rx= dmem-end SP)
             (Rx= (+ io-base-address io-gpio-out) R0)

             (label the-loop)
             
             ;;--- idle gen -------------------
             ;; we need an idle period once in a while to allow uart receiver
             ;; to synchronize. just continue counting gpio until enough zeros
             (label idle-loop)
             (A= 1)
             (A+=Rx R1)
             (Rx=A R1)       ; R1 = counter
             (M[Rx].b=A R0) ; counter -> GPIO
             ;;(Rx= #x3f R6) ; mask to get 1/65536 which should be long enough at 19200 baud and 10 MHz clock freq
             (Rx= #x3ffff R6) ; mask to get 1/65536 which should be long enough at 19200 baud and 10 MHz clock freq
             (A&=Rx R6)
             (Rx=A R8) ; debug
             (Rx= 0 R7)
             (A-=Rx R7) ; R7=0
             (jnz idle-loop) ; loop until 0

             ;;----- print the zero terminated string ---------------
             (Rx= (+ imem-ram hello-str) R9) ; str = R9 = the string to print
             ;; prt loop
             (label prt)
             (A=M[Rx].b R9) ; c
             (Rx=A R8) ; R8=c
             (A= 0) ; c == 0?
             (A-=Rx R8) 
             (jz end-of-string)

             (A=Rx R8) (Rx=A P0)
             (jsr l-putchar)
             ;; --- str++
             (A= 1)
             (A+=Rx R9)
             (Rx=A R9)
             (j prt)
             ;; ----
             (label end-of-string)
             (j the-loop))
             func-putchar
             '(
               (label hello-str)))
               nil
               symtab
               ))
       (data (string-to-mem (format nil "Hello World!~C~A" #\Return #\Linefeed)))
       (mcode (concatenate 'list text data)))
       (disasm text)
       (format t "~%")
       (hexdump mcode)
       (output-binary-file mcode "asm.bin")
       (setf *symtab* symtab)
       (setf *program* mcode))


(let* ((symtab *symtab*)
       (dmem (make-dmem 65536))
       (emul (make-emulator *program* dmem :shared-mem t :debug nil)))
       (run-with-curses emul symtab))


(defparameter pty "/dev/pts/11")
(let* ((symtab *symtab*)
       (dmem (make-dmem 65536))
       (emul (make-emulator *program* dmem :shared-mem t :debug nil)))
       (run-with-curses-io emul pty symtab))
