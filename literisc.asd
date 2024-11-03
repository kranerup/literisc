;;; CL-USER> (asdf:load-system :literisc)

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
