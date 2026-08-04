#!/usr/bin/env -S sbcl --script
;;; lrcc - liteRISC C Compiler
;;; Command-line compiler for C to liteRISC machine code
(require :asdf)

;; Load quicklisp if available (needed to resolve external dependencies)
(let ((ql-setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file ql-setup) (load ql-setup)))
;;
;; Register the script's own directory so ASDF finds literisc.asd
(let ((script-directory (make-pathname :directory (pathname-directory *load-truename*))))
  (pushnew script-directory asdf:*central-registry* :test #'equal))

;; Prefer ql:quickload (fetches missing dependencies automatically);
;; fall back to plain ASDF if quicklisp isn't installed.
(handler-case
    (if (find-package :quicklisp)
        (uiop:symbol-call :ql :quickload :literisc :silent t)
        (asdf:load-system :literisc :verbose nil))
  (error (e)
    (format *error-output* "Error loading compiler: ~a~%" e)
    (sb-ext:exit :code 1)))


(in-package :c-compiler)

(defun print-usage ()
  (format t "Usage: lrcc [options] <source.c>~%~%")
  (format t "Options:~%")
  (format t "  -o <file>            Write binary output to file (hex format)~%")
  (format t "  -S                   Output assembly code only (pretty printed)~%")
  (format t "  -r, --run            Run program in emulator (program output to stdout,~%")
  (format t "                       return value to stderr)~%")
  (format t "  --ui                 Run program in curses emulator UI (interactive debugger)~%")
  (format t "  --conf               Enable conf bus connection (default socket)~%")
  (format t "  --no-conf            Disable conf bus connection (default)~%")
  (format t "  --conf-socket <path> Specify conf bus socket path~%")
  (format t "                       (implies --conf, default: /tmp/coe_emulator.sock)~%")
  (format t "  --conf-mem-size <n>  When --conf is not on, back the conf address space~%")
  (format t "                       with a dedicated local memory of <n> bytes instead~%")
  (format t "                       of requiring a conf bus peer~%")
  (format t "  --imem-size <n>      Size (bytes) of program memory reserved for imem~%")
  (format t "                       (default: ~a, matching the RTL default)~%" lr-soc:+imem-depth+)
  (format t "  --dmem-size <n>      Size (bytes) of program memory reserved for dmem~%")
  (format t "                       (default: ~a, matching the RTL default)~%" lr-soc:+dmem-depth+)
  (format t "                       imem-size and dmem-size together size the emulator's~%")
  (format t "                       flat memory and the initial stack pointer~%")
  (format t "  -I <dir>             Add directory to preprocessor include search path~%")
  (format t "  -O                   Optimize for speed (inlining, peephole, inline mul/div/mod)~%")
  (format t "  -Os                  Optimize for size (inlining, peephole, library mul/div/mod)~%")
  (format t "  -fno-peephole        Disable peephole optimization~%")
  (format t "  -fkeep-unused-functions~%")
  (format t "                       Compile functions even if unreachable from main~%")
  (format t "                       (default: unused functions are eliminated)~%")
  (format t "  -v, --verbose        Verbose output~%")
  (format t "  -h, --help           Show this help message~%")
  (format t "~%Source is always preprocessed via clang -E -P before compilation.~%")
  (format t "~%Examples:~%")
  (format t "  lrcc -S hello.c                              # Print assembly~%")
  (format t "  lrcc -o hello.hex hello.c                    # Compile to hex file~%")
  (format t "  lrcc -r hello.c                              # Compile and run~%")
  (format t "  lrcc -r --conf hello.c                       # Compile and run with conf bus (default socket)~%")
  (format t "  lrcc --ui hello.c                            # Interactive curses debugger~%")
  (format t "  lrcc --ui --conf hello.c                     # With conf bus (default socket)~%")
  (format t "  lrcc --ui --conf-socket /tmp/my.sock hello.c # With conf bus (custom socket)~%")
  (format t "  lrcc -I include -Os -r hello.c               # With stdio.h, optimized for size~%")
  (format t "  lrcc -r --imem-size 4096 --dmem-size 8192 hello.c  # Compile and run with a smaller memory map~%")
  (format t "  lrcc -O -r hello.c                           # Full optimization for speed~%"))

(defun pretty-print-asm (asm-list)
  "Pretty print assembly code in a readable format"
  (dolist (instr asm-list)
    (cond
      ((and (listp instr) (eq (first instr) :comment))
       (format t "        ; ~a~%" (second instr)))
      ((and (listp instr) (eq (first instr) 'label))
       (format t "~a:~%" (second instr)))
      ((and (listp instr) (eq (first instr) 'comment))
       (format t "; ~a~%" (second instr)))
      ((listp instr)
       (format t "        ~a~%" (format-instruction instr)))
      (t
       (format t "        ~a~%" instr)))))

(defun format-instruction (instr)
  "Format an instruction for pretty printing"
  (string-downcase (format nil "~{~a~^ ~}" instr)))

(defun write-hex-file (bytes filename)
  "Write bytes to a file in hex dump format"
  (let* ((vec (coerce bytes 'vector))
         (len (length vec)))
    (with-open-file (out filename :direction :output :if-exists :supersede)
      (loop for i from 0 by 16 below len do
            (format out "~8,'0x: " i)
            (loop for j from 0 below 16
                  for idx = (+ i j)
                  do (if (< idx len)
                         (format out "~2,'0x " (aref vec idx))
                         (format out "   ")))
            (format out " |")
            (loop for j from 0 below 16
                  for idx = (+ i j)
                  do (if (< idx len)
                         (let ((b (aref vec idx)))
                           (format out "~c" (if (and (>= b 32) (<= b 126))
                                                (code-char b)
                                                #\.)))
                         (format out " ")))
            (format out "|~%")))))

(defun write-vmem-file (bytes filename)
  "Write bytes in Verilog $readmemh format: one hex byte per line,
   with an @0 origin so sparse loading also works."
  (let* ((vec (coerce bytes 'vector))
         (len (length vec)))
    (with-open-file (out filename :direction :output :if-exists :supersede)
      (format out "@00000000~%")
      (loop for i from 0 below len
            do (format out "~2,'0x~%" (aref vec i))))))

(defun write-binary-file (bytes filename)
  "Write raw bytes to a binary file"
  (with-open-file (out filename :direction :output
                                :if-exists :supersede
                                :element-type '(unsigned-byte 8))
    (dolist (b bytes)
      (write-byte b out))))

(defun cpp-preprocess (source-file include-dirs)
  "Preprocess source-file using clang -E -P with given include dirs.
   Returns preprocessed source as a string."
  (let* ((args (append (list "-E" "-P" "-x" "c")
                       (loop for d in include-dirs nconc (list "-I" d))
                       (list source-file)))
         (output (make-string-output-stream))
         (proc (sb-ext:run-program "clang" args
                                   :search t
                                   :wait t
                                   :output output
                                   :error *error-output*)))
    (unless (zerop (sb-ext:process-exit-code proc))
      (format *error-output* "Error: Preprocessing failed~%")
      (sb-ext:exit :code 1))
    (get-output-stream-string output)))

(defun main ()
  (let* ((args (cdr sb-ext:*posix-argv*))
         (output-file nil)
         (asm-only nil)
         (conf-socket nil)
         (conf-mem-size nil)
         (imem-size nil)
         (dmem-size nil)
         (run-program nil)
         (run-ui nil)
         (optimize nil)
         (peephole nil)
         (optimize-size t)
         (eliminate-dead t)
         (verbose nil)
         (include-dirs nil)
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
              ((string= arg "--ui")
               (setf run-ui t))
              ((string= arg "--conf")
               (setf conf-socket "/tmp/coe_emulator.sock"))
              ((string= arg "--no-conf")
               (setf conf-socket nil))
              ((string= arg "--conf-socket")
               (if args
                   (setf conf-socket (pop args))
                   (progn
                     (format *error-output* "Error: --conf-socket requires an argument~%")
                     (sb-ext:exit :code 1))))
              ((string= arg "--conf-mem-size")
               (if args
                   (let ((n (parse-integer (pop args) :junk-allowed t)))
                     (unless (and n (> n 0))
                       (format *error-output* "Error: --conf-mem-size requires a positive integer argument~%")
                       (sb-ext:exit :code 1))
                     (setf conf-mem-size n))
                   (progn
                     (format *error-output* "Error: --conf-mem-size requires an argument~%")
                     (sb-ext:exit :code 1))))
              ((string= arg "--imem-size")
               (if args
                   (let ((n (parse-integer (pop args) :junk-allowed t)))
                     (unless (and n (> n 0))
                       (format *error-output* "Error: --imem-size requires a positive integer argument~%")
                       (sb-ext:exit :code 1))
                     (setf imem-size n))
                   (progn
                     (format *error-output* "Error: --imem-size requires an argument~%")
                     (sb-ext:exit :code 1))))
              ((string= arg "--dmem-size")
               (if args
                   (let ((n (parse-integer (pop args) :junk-allowed t)))
                     (unless (and n (> n 0))
                       (format *error-output* "Error: --dmem-size requires a positive integer argument~%")
                       (sb-ext:exit :code 1))
                     (setf dmem-size n))
                   (progn
                     (format *error-output* "Error: --dmem-size requires an argument~%")
                     (sb-ext:exit :code 1))))
              ((string= arg "-O")
               (setf optimize t)
               (setf peephole t)
               (setf optimize-size nil))
              ((string= arg "-Os")
               (setf optimize t)
               (setf peephole t)
               (setf optimize-size t))
              ((string= arg "-fno-peephole")
               (setf peephole nil))
              ((string= arg "-fkeep-unused-functions")
               (setf eliminate-dead nil))
              ((or (string= arg "-v") (string= arg "--verbose"))
               (setf verbose t))
              ((string= arg "-I")
               (if args
                   (push (pop args) include-dirs)
                   (progn
                     (format *error-output* "Error: -I requires an argument~%")
                     (sb-ext:exit :code 1))))
              ((and (> (length arg) 2) (string= arg "-I" :end1 2))
               (push (subseq arg 2) include-dirs))
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

    ;; Preprocess with cpp
    (let ((source (cpp-preprocess source-file (nreverse include-dirs)))
          (mem-size (when (or imem-size dmem-size)
                      (+ (or imem-size lr-soc:+imem-depth+)
                         (or dmem-size lr-soc:+dmem-depth+)))))

      (handler-bind
          ((error (lambda (e)
                    (format *error-output* "Compilation error: ~a~%" e)
                    (sb-debug:print-backtrace :count 30 :stream *error-output*)
                    (sb-ext:exit :code 1))))
        (cond
          (asm-only
           (let ((asm (compile-c source :verbose verbose
                                        :annotate t
                                        :optimize optimize
                                        :optimize-size optimize-size
                                        :peephole peephole
                                        :eliminate-dead eliminate-dead
                                        :mem-size mem-size)))
             (if output-file
                 (with-open-file (*standard-output* output-file
                                                    :direction :output
                                                    :if-exists :supersede)
                   (pretty-print-asm asm))
                 (pretty-print-asm asm))))

          (run-program
           (when verbose
             (format t "Compiling ~a...~%" source-file))
           (when (and verbose conf-socket)
             (format t "Using conf socket: ~a~%" conf-socket))
           (multiple-value-bind (result instr-count)
               (run-c-program source :verbose verbose
                                     :optimize optimize
                                     :optimize-size optimize-size
                                     :peephole peephole
                                     :conf-socket conf-socket
                                     :conf-mem-size conf-mem-size
                                     :imem-size imem-size
                                     :dmem-size dmem-size
                                     :eliminate-dead eliminate-dead
                                     :max-cycles 10000000000)
             (format *error-output* "instructions executed: ~a~%" instr-count)
             (format *error-output* "~a~%" result)
             (sb-ext:exit :code (logand result 255))))

          (run-ui
           (when verbose
             (format t "Compiling ~a...~%" source-file))
           (let* ((asm (compile-c source :verbose verbose
                                         :annotate nil
                                         :optimize optimize
                                         :optimize-size optimize-size
                                         :peephole peephole
                                         :eliminate-dead eliminate-dead
                                         :mem-size mem-size))
                  (symtab (make-hash-table :test 'eql))
                  (mcode (assemble (strip-asm-comments asm) verbose symtab))
                  (dmem (lr-emulator:make-dmem (or mem-size #x1000000)))
                  (emul (lr-emulator:make-emulator mcode dmem :shared-mem t :debug verbose)))
             (if conf-socket
                 (progn
                   (when verbose
                     (format t "Using conf socket: ~a~%" conf-socket))
                   (lr-emulator:run-with-curses-conf emul conf-socket symtab))
                 (lr-emulator:run-with-curses emul symtab))))

          (t
           (when verbose
             (format t "Compiling ~a...~%" source-file))
           (let ((mcode (compile-c-to-asm source :verbose verbose
                                                 :optimize optimize
                                                 :optimize-size optimize-size
                                                 :peephole peephole
                                                 :eliminate-dead eliminate-dead
                                                 :mem-size mem-size)))
             (if output-file
                 (progn
                   (cond
                     ((string-suffix-p output-file ".bin")
                      (write-binary-file mcode output-file))
                     ((or (string-suffix-p output-file ".mem")
                          (string-suffix-p output-file ".vmem"))
                      (write-vmem-file mcode output-file))
                     (t
                      (write-hex-file mcode output-file)))
                   (when verbose
                     (format t "Wrote ~a bytes to ~a~%" (length mcode) output-file)))
                 (lr-asm:hexdump mcode)))))))))

(defun string-suffix-p (string suffix)
  "Check if STRING ends with SUFFIX"
  (let ((slen (length string))
        (xlen (length suffix)))
    (and (>= slen xlen)
         (string= string suffix :start1 (- slen xlen)))))

(main)
