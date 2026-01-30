;;; CL-USER> (asdf:load-system :literisc)
;(eval-when (:compile-toplevel :load-toplevel :execute)
;  (unless (find-package :str)
;    (when (find-package :asdf)
;      (when (asdf:find-system :str nil)
;    ;; before attempting to use it.
;    (if (find-package :ql nil)
;        ;; Quicklisp is present, so use it to load the system.
;        (ql:quickload :str))))

(defsystem "literisc"
  :depends-on (
    #:cl-charms
	#:cl-ppcre
    #:str
    #:cffi )
  :components ((:file "charms-extra")
               (:file "macro-utilities")
               (:file "unit")
               (:file "opcode-parse")
               (:file "assembler")
               (:file "disassembler")
               (:file "pty-lib")
               (:file "soc")
               (:file "emulator")
               (:file "reg-verifier")
               (:file "c-compiler")
               (:file "c-lexer")
               (:file "c-parser")
               (:file "c-optimizer")
               (:file "c-peephole")
               (:file "c-codegen")
               (:file "c-compiler-tests")))
