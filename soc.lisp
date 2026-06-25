(defpackage :lr-soc
  (:use :cl :cl-ppcre))
(in-package :lr-soc)

(defmacro export-parameter (name &rest rest)
  `(progn
     (export ',name)
     (defparameter ,name ,@rest)))

;;; generated from constants.py via gen_constants.py
(load (asdf:system-relative-pathname :literisc "constants.lisp"))

(export-parameter imem-start      +imem_low+)
(export-parameter imem-end        +imem_high+)
(export-parameter dmem-start      +dmem_low+)
(export-parameter dmem-end        +dmem_high+)
(export-parameter irq-address     +irq_address+)
(export-parameter tick-address    +tick_address+)
(export-parameter interrupt-address +interrupt_address+)
(export-parameter cpu-reset-address +cpu_reset_address+)
(export-parameter io-base-address +io_low+)
(export-parameter io-hi           +io_high+)
(export-parameter conf-low        +conf_low+)
(export-parameter conf-hi         +conf_high+)

;;; offset from IO base
(export-parameter io-gpio-out              0)
(export-parameter io-gpio-led-0           20)
(export-parameter io-gpio-led-1           21)
(export-parameter gpio_address             0)
(export-parameter serial_tx_data_address   1)
(export-parameter serial_tx_status_address 2)
(export-parameter serial_rx_data_address   3)
(export-parameter serial_rx_status_address 4)

;;; emulator simple character output
(export-parameter emul-print-char-address #xffffffff)
