(defpackage :lr-soc
  (:use :cl :cl-ppcre))
(in-package :lr-soc)

(defmacro export-parameter (name &rest rest)
  `(progn
     (export ',name)
     (defparameter ,name ,@rest)))

;;; definitions from cpu_sys.py and axi_slaves.py
(export-parameter dmem-start 8192) ; start of dmem
(export-parameter dmem-end (logand (+ 8192 16383) #xfffffffc))

;;; boot xmodem loader is positioned at 0-x and loads binary file
;;; starting at 500.
(export-parameter imem-ram 500); just large enough to jump over boot rom
;; i/o devices
(export-parameter io-base-address (- 65536 100))
(export-parameter io-hi 65535)
;;; offset from IO
(export-parameter io-gpio-out 0)
;;; gpio led bit numbers:
(export-parameter io-gpio-led-0 20)
(export-parameter io-gpio-led-1 21)

(export-parameter gpio_address 0)
(export-parameter serial_tx_data_address  1)
(export-parameter serial_tx_status_address  2)
(export-parameter serial_rx_data_address  3)
(export-parameter serial_rx_status_address  4)

;;; emulator simple character output
(export-parameter emul-print-char-address #xffffffff)
