(pushnew #P"/home/kenny/work/lite_risc_c/literisc/" asdf:*central-registry*)
(handler-case
  (asdf:load-system :literisc)
  (error (e) (format t "Load error: ~a~%" e) (sb-ext:exit :code 1)))
(format t "~%=== Running All C Compiler Tests ===~%")
(if (c-compiler::test-c-compiler)
    (format t "~%All tests passed!~%")
    (progn (format t "~%Some tests failed!~%") (sb-ext:exit :code 1)))
