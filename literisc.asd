;;; CL-USER> (asdf:load-system :literisc)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :str))

(defsystem "literisc"
  :depends-on (#:cl-charms
	       #:cl-ppcre
               #:str)
  :components ((:file "charms-extra")
               (:file "macro-utilities")
               (:file "unit")
               (:file "opcode-parse")
               (:file "assembler")
               (:file "disassembler")
               (:file "emulator")))
