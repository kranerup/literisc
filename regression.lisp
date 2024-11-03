;;;(ql:quickload :cl-charms)
;;;(ql:quickload :str)
;;;(load "macro-utilities")
;;;(load "unit")
;;;(load "assembler")
;;;(load "disassembler")
;;;(load "charms-extra")
;;;(load "emulator")
(asdf:load-system :literisc)

(use-package :unit)

(deftest test-regression ()
  (combine-results
    (lr-asm::test-asm)
    (lr-disasm::test-disasm)
    (lr-emulator::test-instructions)))

(if (test-regression)
    (format t "======== Regression PASS ========~%")
    (format t "*********** FAIL **************~%"))
(quit)

