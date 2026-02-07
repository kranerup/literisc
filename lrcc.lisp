#!/usr/bin/env -S sbcl --script
;;; lrcc - liteRISC C Compiler
;;; Command-line compiler for C to liteRISC machine code

(require :asdf)
(let ((script-directory (make-pathname :directory (pathname-directory *load-truename*))))
  (pushnew script-directory asdf:*central-registry* :test #'equal))
(handler-case
    (asdf:load-system :literisc :verbose nil)
  (error (e)
    (format *error-output* "Error loading compiler: ~a~%" e)
    (sb-ext:exit :code 1)))

(in-package :c-compiler)

(defun print-usage ()
  (format t "Usage: lrcc [options] <source.c>~%~%")
  (format t "Options:~%")
  (format t "  -o <file>        Write binary output to file (hex format)~%")
  (format t "  -S               Output assembly code only (pretty printed)~%")
  (format t "  -r, --run        Run program in emulator and print return value~%")
  (format t "  -O               Optimize for speed (inlining, peephole, inline mul/div/mod)~%")
  (format t "  -Os              Optimize for size (inlining, peephole, library mul/div/mod)~%")
  (format t "  -fno-peephole    Disable peephole optimization~%")
  (format t "  -v, --verbose    Verbose output~%")
  (format t "  -h, --help       Show this help message~%")
  (format t "~%Constant folding is always enabled.~%")
  (format t "~%Examples:~%")
  (format t "  lrcc -S hello.c              # Print assembly~%")
  (format t "  lrcc -o hello.hex hello.c    # Compile to hex file~%")
  (format t "  lrcc -r hello.c              # Compile and run~%")
  (format t "  lrcc -O -r hello.c           # Full optimization for speed~%")
  (format t "  lrcc -Os -r hello.c          # Full optimization for size~%"))

(defun pretty-print-asm (asm-list)
  "Pretty print assembly code in a readable format"
  (dolist (instr asm-list)
    (cond
      ;; Comments - print as-is with indentation
      ((and (listp instr) (eq (first instr) :comment))
       (format t "        ; ~a~%" (second instr)))
      ;; Labels - print without indentation
      ((and (listp instr) (eq (first instr) 'label))
       (format t "~a:~%" (second instr)))
      ;; COMMENT pseudo-op - print as comment
      ((and (listp instr) (eq (first instr) 'comment))
       (format t "; ~a~%" (second instr)))
      ;; Regular instructions - indent
      ((listp instr)
       (format t "        ~a~%" (format-instruction instr)))
      (t
       (format t "        ~a~%" instr)))))

(defun format-instruction (instr)
  "Format an instruction for pretty printing"
  (string-downcase (format nil "~{~a~^ ~}" instr)))

(defun write-hex-file (bytes filename)
  "Write bytes to a file in hex dump format"
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (loop for i from 0 by 16 below (length bytes) do
          (format out "~8,'0x: " i)
          (loop for j from 0 below 16
                for idx = (+ i j)
                do (if (< idx (length bytes))
                       (format out "~2,'0x " (nth idx bytes))
                       (format out "   ")))
          (format out " |")
          (loop for j from 0 below 16
                for idx = (+ i j)
                do (if (< idx (length bytes))
                       (let ((b (nth idx bytes)))
                         (format out "~c" (if (and (>= b 32) (<= b 126))
                                              (code-char b)
                                              #\.)))
                       (format out " ")))
          (format out "|~%"))))

(defun write-binary-file (bytes filename)
  "Write raw bytes to a binary file"
  (with-open-file (out filename :direction :output
                                :if-exists :supersede
                                :element-type '(unsigned-byte 8))
    (dolist (b bytes)
      (write-byte b out))))

(defun main ()
  (let* ((args (cdr sb-ext:*posix-argv*))
         (output-file nil)
         (asm-only nil)
         (run-program nil)
         (optimize nil)
         (peephole nil)
         (optimize-size t)
         (verbose nil)
         (source-file nil))

    ;; Parse command line arguments
    (loop while args do
          (let ((arg (pop args)))
            (cond
              ((or (string= arg "-h") (string= arg "--help"))
               (print-usage)
               (sb-ext:exit :code 0))
              ((string= arg "-o")
               (if args
                   (setf output-file (pop args))
                   (progn
                     (format *error-output* "Error: -o requires an argument~%")
                     (sb-ext:exit :code 1))))
              ((string= arg "-S")
               (setf asm-only t))
              ((or (string= arg "-r") (string= arg "--run"))
               (setf run-program t))
              ((string= arg "-O")
               ;; Optimize for speed: inlining, peephole, inline mul/div/mod
               (setf optimize t)
               (setf peephole t)
               (setf optimize-size nil))
              ((string= arg "-Os")
               ;; Optimize for size: inlining, peephole, library mul/div/mod
               (setf optimize t)
               (setf peephole t)
               (setf optimize-size t))
              ((string= arg "-fno-peephole")
               (setf peephole nil))
              ((or (string= arg "-v") (string= arg "--verbose"))
               (setf verbose t))
              ((char= (char arg 0) #\-)
               (format *error-output* "Error: Unknown option ~a~%" arg)
               (sb-ext:exit :code 1))
              (t
               (if source-file
                   (progn
                     (format *error-output* "Error: Multiple source files not supported~%")
                     (sb-ext:exit :code 1))
                   (setf source-file arg))))))

    ;; Check for source file
    (unless source-file
      (format *error-output* "Error: No source file specified~%")
      (print-usage)
      (sb-ext:exit :code 1))

    ;; Check source file exists
    (unless (probe-file source-file)
      (format *error-output* "Error: Source file not found: ~a~%" source-file)
      (sb-ext:exit :code 1))

    ;; Read source file
    (let ((source (with-open-file (in source-file :direction :input)
                    (let ((content (make-string (file-length in))))
                      (read-sequence content in)
                      content))))

      (handler-case
          (cond
            ;; Assembly output only
            (asm-only
             (let ((asm (compile-c source :verbose verbose
                                          :annotate t
                                          :optimize optimize
                                          :optimize-size optimize-size
                                          :peephole peephole)))
               (if output-file
                   (with-open-file (*standard-output* output-file
                                                      :direction :output
                                                      :if-exists :supersede)
                     (pretty-print-asm asm))
                   (pretty-print-asm asm))))

            ;; Run in emulator
            (run-program
             (when verbose
               (format t "Compiling ~a...~%" source-file))
             (let ((result (run-c-program source :verbose verbose
                                                 :optimize optimize
                                                 :optimize-size optimize-size
                                                 :peephole peephole
                                                 :max-cycles 1000000)))
               (format t "~a~%" result)
               (sb-ext:exit :code 0)))

            ;; Compile to binary
            (t
             (when verbose
               (format t "Compiling ~a...~%" source-file))
             (let ((mcode (compile-c-to-asm source :verbose verbose
                                                   :optimize optimize
                                                   :optimize-size optimize-size
                                                   :peephole peephole)))
               (if output-file
                   (progn
                     (if (string-suffix-p output-file ".bin")
                         (write-binary-file mcode output-file)
                         (write-hex-file mcode output-file))
                     (when verbose
                       (format t "Wrote ~a bytes to ~a~%" (length mcode) output-file)))
                   (lr-asm:hexdump mcode)))))

        (error (e)
          (format *error-output* "Compilation error: ~a~%" e)
          (sb-ext:exit :code 1))))))

(defun string-suffix-p (string suffix)
  "Check if STRING ends with SUFFIX"
  (let ((slen (length string))
        (xlen (length suffix)))
    (and (>= slen xlen)
         (string= string suffix :start1 (- slen xlen)))))

(main)
