(asdf:load-system :literisc)
(ql:quickload :cl-charms)
(ql:quickload :str)

(use-package :lr-asm)
(use-package :lr-emulator)
(use-package :lr-disasm)

(defvar program nil)

(defvar IO_HIGH (- (expt 2 17) 1))
(defvar TICKS (+ IO_HIGH 2))
(defvar IMEM_LOW 512)
(defvar *hello-world*
  (assemble
    '(
       (Rx= (+ IO_HIGH 3) R0)
       (A=M[Rx].b R0)
       (Rx= IMEM_LOW R0)
       (A=Rx R0)
       (j-a)
       )))

(let* ((text *hello-world*)
       (data '())
       (mcode (concatenate 'list text data)))
  (disasm text)
  (format t "~%")
  (hexdump mcode)
  (setf program mcode))

(defvar e nil)
(defvar dmem nil)
(setq dmem (make-dmem (expt 2 18)))
(set-program dmem (string-to-mem "Hello World!"))

(setf e (make-emulator *hello-world* dmem :shared-mem 0 :debug t))
(run-with-curses e)
(quit)
