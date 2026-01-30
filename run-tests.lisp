#!/usr/bin/env -S sbcl --script
(require :asdf)
(let ((script-directory (make-pathname :directory (pathname-directory *load-truename*))))
  (pushnew script-directory asdf:*central-registry* :test #'equal))
;;;(pushnew #P"/home/kenny/work/lite_risc_c/literisc/" asdf:*central-registry*)
(handler-case
  (asdf:load-system :literisc)
  (error (e) (format t "Load error: ~a~%" e) (sb-ext:exit :code 1)))

(defun find-test-symbol (name-string)
  "Finds a test symbol from its string name, checking all registered tests."
  (loop for test-sym being the hash-keys of unit:*all-tests*
        when (string-equal name-string (symbol-name test-sym))
        do (return test-sym)
        finally (return nil))) ; Return nil if not found

(let ((args (cdr sb-ext:*posix-argv*)))
  (if (and args (or (string-equal (car args) "list")
                    (string-equal (car args) "--list")))
      (progn
        (format t "Available tests:~%")
        (dolist (test (unit:list-tests))
          (format t "  ~a~%" (string-downcase (symbol-name test))))
        (sb-ext:exit :code 0))
      (if args
          (let ((test-names (mapcar (lambda (name)
                                      (let ((found-sym (find-test-symbol name)))
                                        (if found-sym
                                            found-sym
                                            ;; Fallback to C-COMPILER for legacy/simplicity if not found
                                            (intern (string-upcase name) :c-compiler))))
                                    args)))
            (format t "~%=== Running selected tests: ~a ===~%" test-names)
            (unless (unit:run-tests test-names)
              (sb-ext:exit :code 1)))
          (progn
            (format t "~%=== Running All C Compiler Tests ===~%")
            (unless (unit:run-tests)
              (sb-ext:exit :code 1))))))

(format t "~%All tests passed!~%")
(sb-ext:exit :code 0)
