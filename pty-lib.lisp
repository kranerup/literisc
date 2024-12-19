(ql:quickload :cffi)
(defpackage :pty
  (:use :cl :cffi)
  (:export #:with-raw-pty
           #:pty-read-char
           #:pty-read-char-no-hang
           #:pty-write-char
           #:pty-char-available-p
           #:with-open-pty))

;; pty.lisp - Main implementation
(in-package :pty)

;; Load the C library
(define-foreign-library libc
  (:unix (:or "libc.so.6" "libc.so"))
  (t (:default "libc")))

(use-foreign-library libc)

;; fd_set structure for select()
(defcstruct fd-set
  (fds-bits (:array :uint64 16)))  ; 1024 bits for file descriptors

;; timeval structure for select() timeout
(defcstruct timeval
  (tv-sec :long)
  (tv-usec :long))

;; C struct termios binding
(defcstruct termios
  (c-iflag :unsigned-int)
  (c-oflag :unsigned-int)
  (c-cflag :unsigned-int)
  (c-lflag :unsigned-int)
  (c-line :unsigned-char)
  (c-cc (:array :unsigned-char 32))
  (c-ispeed :unsigned-int)
  (c-ospeed :unsigned-int))

;; Constants
(defconstant +ICANON+ #o0000002)
(defconstant +ECHO+ #o0000010)
(defconstant +TCSANOW+ 0)
(defconstant +O-RDWR+ #o2)
(defconstant +O-NOCTTY+ #o400)
(defconstant +O-NONBLOCK+ #o4000)

;; CFFI function definitions
(defcfun ("memcpy" memcpy) :pointer
  (dest :pointer)
  (src :pointer)
  (n :size))

(defcfun ("open" c-open) :int
  (pathname :string)
  (flags :int))

(defcfun ("close" c-close) :int
  (fd :int))

(defcfun ("tcgetattr" c-tcgetattr) :int
  (fd :int)
  (termios-p :pointer))

(defcfun ("tcsetattr" c-tcsetattr) :int
  (fd :int)
  (optional-actions :int)
  (termios-p :pointer))

(defcfun ("read" c-read) :int
  (fd :int)
  (buf :pointer)
  (count :size))

(defcfun ("write" c-write) :int
  (fd :int)
  (buf :pointer)
  (count :size))

(defcfun ("select" c-select) :int
  (nfds :int)
  (readfds :pointer)
  (writefds :pointer)
  (exceptfds :pointer)
  (timeout :pointer))

;; Helper functions for fd_set manipulation
(defun fd-set-zero (fd-set-ptr)
  (dotimes (i 16)
    (setf (mem-aref fd-set-ptr :uint64 i) 0)))

(defun fd-set-set (fd fd-set-ptr)
  (let* ((word-index (floor fd 64))
         (bit-index (mod fd 64))
         (word (mem-aref fd-set-ptr :uint64 word-index)))
    (setf (mem-aref fd-set-ptr :uint64 word-index)
          (logior word (ash 1 bit-index)))))

;; Helper functions for termios struct manipulation
(defun get-terminal-attrs (fd)
  "Get terminal attributes, returning a foreign pointer to termios struct"
  (let ((termios (foreign-alloc '(:struct termios))))
    (c-tcgetattr fd termios)
    termios))

(defun set-terminal-attrs (fd termios-ptr)
  "Set terminal attributes from a foreign pointer to termios struct"
  (c-tcsetattr fd +TCSANOW+ termios-ptr))

(defun copy-termios (src-ptr)
  "Create a new termios struct and copy contents from src-ptr"
  (let ((dst-ptr (foreign-alloc '(:struct termios))))
    (memcpy dst-ptr src-ptr (foreign-type-size '(:struct termios)))
    dst-ptr))

;; Main interface
(defmacro with-raw-pty ((fd) &body body)
  "Execute body with the PTY in raw mode (no buffering or echo)"
  `(let* ((old-termios-ptr (get-terminal-attrs ,fd))
          (new-termios-ptr (copy-termios old-termios-ptr)))
     (unwind-protect
          (progn
            (with-foreign-slots ((c-lflag) new-termios-ptr (:struct termios))
              (setf c-lflag (logand c-lflag (lognot (logior +ICANON+ +ECHO+)))))
            (set-terminal-attrs ,fd new-termios-ptr)
            ,@body)
       (progn
         (set-terminal-attrs ,fd old-termios-ptr)
         (foreign-free new-termios-ptr)
         (foreign-free old-termios-ptr)))))

(defmacro with-open-pty ((fd-var path) &body body)
  `(let ((,fd-var (c-open ,path (logior +O-RDWR+ +O-NOCTTY+))))
     (when (< ,fd-var 0)
       (error "Failed to open PTY device ~A" ,path))
     (unwind-protect
          (progn ,@body)
       (c-close ,fd-var))))

(defun pty-char-available-p (fd)
  "Check if there's a character available to read from the PTY"
  (with-foreign-objects ((readfds '(:struct fd-set))
                        (timeout '(:struct timeval)))
    ;; Initialize fd_set
    (fd-set-zero readfds)
    (fd-set-set fd readfds)
    
    ;; Set timeout to 0 for immediate return
    (setf (foreign-slot-value timeout '(:struct timeval) 'tv-sec) 0)
    (setf (foreign-slot-value timeout '(:struct timeval) 'tv-usec) 0)
    
    ;; Call select
    (let ((result (c-select (1+ fd) readfds (null-pointer) (null-pointer) timeout)))
      (if (< result 0)
          (error "select() failed")
          (> result 0)))))

(defun pty-read-char (fd)
  "Read a single character from the PTY, blocking if necessary"
  (with-foreign-object (buf :char)
    (let ((bytes-read (c-read fd buf 1)))
      (if (= bytes-read 1)
          (code-char (mem-ref buf :char))
          nil))))

(defun pty-read-char-no-hang (fd)
  "Read a single character from the PTY if available, returning nil if none available"
  (when (pty-char-available-p fd)
    (pty-read-char fd)))

(defun pty-write-char (fd char)
  "Write a single character to the PTY"
  (with-foreign-object (buf :char)
    (setf (mem-ref buf :char) (char-code char))
    (c-write fd buf 1)))

;; Example usage:

#|
;; Blocking read:
(with-open-pty (fd "/dev/pts/17")
  (with-raw-pty (fd)
    (loop
      (let ((char (pty-read-char fd)))
        (when (char= char #\q)
          (return))
        (format t "c:~a~%" char)))))
|#
#|
;; Non-blocking read:
(with-open-pty (fd "/dev/pts/17")
  (with-raw-pty (fd)
    (loop
      (let ((char (pty-read-char-no-hang fd)))
        (when char
          (if (char= char #\q)
              (return)
              (format t "c:~a~%" char))))
        (format t "sleep~%")
        (sleep 0.3)))) ; Sleep to prevent busy-waiting
|#
