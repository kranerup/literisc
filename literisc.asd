(defsystem "literisc"
  :depends-on (#:cl-charms
               #:str)
  :components ((:file "charms-extra")
               (:file "macro-utilities")
               (:file "unit")
               (:file "assembler")
               (:file "disassembler")
               (:file "emulator")))
