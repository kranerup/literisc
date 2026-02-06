#!/usr/bin/env -S sbcl --script
;;; Force recompile the literisc system
;;; Use this after modifying Lisp source files

(require :asdf)
(let ((script-directory (make-pathname :directory (pathname-directory *load-truename*))))
  (pushnew script-directory asdf:*central-registry* :test #'equal))

(format t "Force recompiling literisc...~%")
(handler-case
    (progn
      (asdf:compile-system :literisc :force t)
      (format t "Recompilation complete.~%"))
  (error (e)
    (format t "Compilation error: ~a~%" e)
    (sb-ext:exit :code 1)))
