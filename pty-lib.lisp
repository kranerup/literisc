(ql:quickload :cffi)

(defpackage :pty
  (:use :cl :cffi)
  (:export #:with-raw-pty
           #:pty-read-char
           #:pty-write-char
           #:with-open-pty))

(in-package :pty)

;; Load the C library
(define-foreign-library libc
  (:unix (:or "libc.so.6" "libc.so"))
  (t (:default "libc")))

(use-foreign-library libc)

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

;; Constants from termios.h and fcntl.h
(defconstant +ICANON+ #o0000002)
(defconstant +ECHO+ #o0000010)
(defconstant +TCSANOW+ 0)
(defconstant +O-RDWR+ #o2)    ; Read/write access
(defconstant +O-NOCTTY+ #o400)  ; Don't make controlling tty

;; CFFI function definitions
(defcfun ("open" c-open) :int
  (pathname :string)
  (flags :int))

(defcfun ("close" c-close) :int
  (fd :int))

(defcfun ("memcpy" memcpy) :pointer
  (dest :pointer)
  (src :pointer)
  (n :size))

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

(defmacro with-raw-pty ((fd) &body body)
  "Execute body with the PTY in raw mode (no buffering or echo)"
  `(let* ((old-termios-ptr (get-terminal-attrs ,fd))
          (new-termios-ptr (copy-termios old-termios-ptr)))
     (unwind-protect
          (progn
            ;; Clear ICANON and ECHO flags
            (with-foreign-slots ((c-lflag) new-termios-ptr (:struct termios))
              (setf c-lflag (logand c-lflag (lognot (logior +ICANON+ +ECHO+)))))
            (set-terminal-attrs ,fd new-termios-ptr)
            ,@body)
       ;; Cleanup
       (progn
         (set-terminal-attrs ,fd old-termios-ptr)
         (foreign-free new-termios-ptr)
         (foreign-free old-termios-ptr)))))

(defun pty-read-char (fd)
  "Read a single character from the PTY without buffering"
  (with-foreign-object (buf :char)
    (let ((bytes-read (c-read fd buf 1)))
      (if (= bytes-read 1)
          (code-char (mem-ref buf :char))
          nil))))

(defun pty-write-char (fd char)
  "Write a single character to the PTY"
  (with-foreign-object (buf :char)
    (setf (mem-ref buf :char) (char-code char))
    (c-write fd buf 1)))

;; Helper function for opening PTY devices
(defmacro with-open-pty ((fd-var path) &body body)
  `(let ((,fd-var (c-open ,path (logior +O-RDWR+ +O-NOCTTY+))))
     (when (< ,fd-var 0)
       (error "Failed to open PTY device ~A" ,path))
     (unwind-protect
          (progn ,@body)
       (c-close ,fd-var))))

#|
;; Example usage:
;; Using standard input:

  (with-raw-pty (0)
  (loop
    (let ((char (pty-read-char 0)))
      (when (char= char #\q)
        (return))
      (pty-write-char 1 char)
      ))))  ; 1 is standard output

|#
#|
;; Using a specific PTY:
(with-open-pty (fd "/dev/pts/17")
  (with-raw-pty (fd)
  (loop
    (let ((char (pty-read-char fd)))
      (when (char= char #\q)
        (return))
      (pty-write-char fd char)
      ))))  ; 1 is standard output
|#

;; Using a specific PTY:
(with-open-pty (fd "/dev/pts/17")
  (with-raw-pty (fd)
  (loop
    (let ((char (pty-read-char fd)))
      (when (char= char #\q)
        (return))
      (format t "char:~a~%" char)
      ))))  ; 1 is standard output
|#
