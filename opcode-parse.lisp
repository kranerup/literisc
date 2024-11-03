(defpackage :lr-opcodes
  (:use :cl :cl-ppcre)
  (:export :load-opcodes))
(in-package :lr-opcodes)

(defun parse-opcode-line (line)
  "Parse a single line of opcode definition into (values name value)"
  (multiple-value-bind (full matches)
      (cl-ppcre:scan-to-strings
       "^(OP[A-Z_0-9]+)\\s*=\\s*(\\d+)"
       line)
    (when full
      (values (aref matches 0)           ; name
              (parse-integer (aref matches 1)))))) ; value

(defun parse-opcode-lines (lines)
  (let ((result '()))
    (dolist (line lines)
      (multiple-value-bind (name value)
          (parse-opcode-line (string-trim " " line))
        (when name
          (push (cons name value) result))))
    result))

#|
(parse-opcode-line "OPC_A_RX      = 0  # Rx = A")

(defparameter *opcode-text*
  "OPC_A_RX      = 0  # Rx = A
OPC_RX_A      = 1  # A = Rx
something else
OPC_LD_A_OFFS = 2  # Rx = M[A+nn].l
OPC_LD_A      = 3  # Rx = M[A].l
OPC_LD_RX     = 4  # A = M[Rx].l
OPC_ST_A_OFFS = 5  # M[A+nn].l = Rx")
|#

(defun read-lines-from-file (file-path)
  (with-open-file (stream file-path)
    (loop for line = (read-line stream nil)
	  while line
	  collect line)))

(defun load-opcodes (&optional (file "rtl/literisc/cpu.py"))
  (let ((opcodes (parse-opcode-lines
                   (read-lines-from-file file))))
    (loop for (k . v) in opcodes
          do (let ((symbol (intern k)))
               (setf (symbol-value symbol) v)
               (format t "sym:~a = ~a~%" k v)
               (proclaim `(special ,symbol))))))
