(defpackage :lr-soc
  (:use :cl :cl-ppcre))
(in-package :lr-soc)

(defmacro export-parameter (name &rest rest)
  `(progn
     (export ',name)
     (defparameter ,name ,@rest)))

;;; Memory map, derived from imem-depth/dmem-depth. Mirrors
;;; rtl/literisc/constants.py's compute_memory_map exactly, so the RTL and
;;; the Lisp side always agree on the address map for a given pair of sizes
;;; without needing a generated/copied snapshot file.
;;;
;;;   IMEM             : imem-low .. imem-high   (boot rom + RAM), imem-depth bytes
;;;   DMEM             : dmem-low .. dmem-high   dmem-depth bytes
;;;   PERIPHERAL IO    : io-low   .. io-high     (UART, GPIO etc), +io-depth+ bytes
;;;   Special registers: IRQ, TICK, INTERRUPT, CPU_RESET (4 bytes)
;;;   CONF bus window  : conf-low .. conf-high

(defconstant +imem-depth+ 8192)
(defconstant +dmem-depth+ 32768)
(defconstant +io-depth+ 100)
(defconstant +conf-window-size+ (- (expt 2 24) (expt 2 16)))

(defstruct memory-map
  imem-low imem-high
  dmem-low dmem-high
  io-low io-high
  cpu-reset-address interrupt-address tick-address irq-address
  conf-low conf-high)

(defun compute-memory-map (imem-depth dmem-depth)
  (assert (and (> imem-depth 0) (> dmem-depth 0)) ()
          "imem-depth and dmem-depth must be positive")
  (let* ((imem-low 0)
         (imem-high (1- imem-depth))
         (dmem-low (1+ imem-high))
         (dmem-high (1- (+ dmem-low dmem-depth)))
         (io-low (1+ dmem-high))
         (io-high (1- (+ io-low +io-depth+)))
         (cpu-reset-address (1+ io-high))
         (interrupt-address (+ io-high 2))
         (tick-address (+ io-high 3))
         (irq-address (+ io-high 4))
         ;; CONF bus window, immediately after peripheral IO
         (conf-low (+ io-high 5))
         (conf-high (1- (+ conf-low +conf-window-size+))))
    (make-memory-map
      :imem-low imem-low :imem-high imem-high
      :dmem-low dmem-low :dmem-high dmem-high
      :io-low io-low :io-high io-high
      :cpu-reset-address cpu-reset-address :interrupt-address interrupt-address
      :tick-address tick-address :irq-address irq-address
      :conf-low conf-low :conf-high conf-high)))

(export '(compute-memory-map memory-map
          memory-map-imem-low memory-map-imem-high
          memory-map-dmem-low memory-map-dmem-high
          memory-map-io-low memory-map-io-high
          memory-map-cpu-reset-address memory-map-interrupt-address
          memory-map-tick-address memory-map-irq-address
          memory-map-conf-low memory-map-conf-high
          +imem-depth+ +dmem-depth+ +io-depth+))

;;; Default map (imem-depth=8192, dmem-depth=32768), exported as the same
;;; flat parameter names used throughout the rest of the codebase.
(defparameter *default-memory-map* (compute-memory-map +imem-depth+ +dmem-depth+))

(export-parameter imem-start      (memory-map-imem-low *default-memory-map*))
(export-parameter imem-end        (memory-map-imem-high *default-memory-map*))
(export-parameter dmem-start      (memory-map-dmem-low *default-memory-map*))
(export-parameter dmem-end        (memory-map-dmem-high *default-memory-map*))
(export-parameter irq-address     (memory-map-irq-address *default-memory-map*))
(export-parameter tick-address    (memory-map-tick-address *default-memory-map*))
(export-parameter interrupt-address (memory-map-interrupt-address *default-memory-map*))
(export-parameter cpu-reset-address (memory-map-cpu-reset-address *default-memory-map*))
(export-parameter io-base-address (memory-map-io-low *default-memory-map*))
(export-parameter io-hi           (memory-map-io-high *default-memory-map*))
(export-parameter conf-low        (memory-map-conf-low *default-memory-map*))
(export-parameter conf-hi         (memory-map-conf-high *default-memory-map*))

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
