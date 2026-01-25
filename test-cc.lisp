;;; Simple test script for the C compiler
;;; Run with: cd literisc && sbcl --load test-cc.lisp

(pushnew #P"/home/kenny/work/lite_risc_c/literisc/" asdf:*central-registry*)
(asdf:load-system :literisc)

(in-package :c-compiler)

;; Test tokenizer
(format t "~%=== Testing Tokenizer ===~%")
(let ((tokens (tokenize "int main() { return 42; }")))
  (dolist (tok tokens)
    (format t "~a~%" tok)))

;; Test parser
(format t "~%=== Testing Parser ===~%")
(let ((*state* (make-compiler-state)))
  (setf (compiler-state-tokens *state*)
        (tokenize "int main() { return 42; }"))
  (let ((ast (parse-program)))
    (print-ast ast)))

;; Test code generation
(format t "~%=== Testing Code Generation ===~%")
(let ((asm (compile-c "int main() { return 42; }" :verbose t)))
  (format t "~%Generated assembly:~%")
  (dolist (instr asm)
    (format t "  ~s~%" instr)))

;; Test full compilation and assembly
(format t "~%=== Testing Assembly ===~%")
(let* ((asm (compile-c "int main() { return 42; }"))
       (mcode (assemble asm)))
  (format t "Machine code (~a bytes):~%" (length mcode))
  (lr-asm:hexdump mcode))

;; Test execution
(format t "~%=== Testing Execution ===~%")
(let ((result (run-c-program "int main() { return 42; }")))
  (format t "Result: ~a (expected: 42)~%" result))

(format t "~%Done!~%")
