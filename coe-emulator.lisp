(defpackage :conf-socket
  (:use :cl)
  (:export :connect-to-sim
           :read-msg
           :send-master-request
           :+msg-master-reply+
           :+msg-master-request+))

(in-package :conf-socket)

(require :sb-bsd-sockets)

(defconstant +msg-size+ 13)
(defconstant +msg-master-reply+   2)
(defconstant +msg-master-request+ 3)

(defun connect-to-sim (path &key (retry-interval 0.5) (max-retries nil))
  "Connect to the simulator's listening socket at PATH.
Retries every RETRY-INTERVAL seconds until the simulator is up.
If MAX-RETRIES is non-nil, gives up after that many attempts.
Returns a binary two-way stream."
  (loop for attempt from 0
        do (let ((socket (make-instance 'sb-bsd-sockets:local-socket
                                        :type :stream)))
             (handler-case
                 (progn
                   (sb-bsd-sockets:socket-connect socket path)
                   (format t "conf-socket: connected to ~a~%" path)
                   (return (sb-bsd-sockets:socket-make-stream
                            socket
                            :input t :output t
                            :element-type '(unsigned-byte 8)
                            :buffering :none)))
               (sb-bsd-sockets:socket-error (e)
                 (declare (ignore e))
                 (sb-bsd-sockets:socket-close socket)
                 (when (and max-retries (>= attempt max-retries))
                   (error "conf-socket: could not connect to ~a" path))
                 (format t "conf-socket: waiting for simulator at ~a...~%" path)
                 (sleep retry-interval))))))

(defun read-u32 (stream)
  (logior (read-byte stream)
          (ash (read-byte stream) 8)
          (ash (read-byte stream) 16)
          (ash (read-byte stream) 24)))

(defun write-u32 (stream val)
  (write-byte (logand val #xff) stream)
  (write-byte (logand (ash val -8)  #xff) stream)
  (write-byte (logand (ash val -16) #xff) stream)
  (write-byte (logand (ash val -24) #xff) stream))

(defun read-msg (stream)
  (let* ((type    (read-byte stream))
         (address (read-u32 stream))
         (data    (read-u32 stream))
         (we      (read-byte stream))
         (re      (read-byte stream))
         (id      (read-byte stream))
         (status  (read-byte stream)))
    (list :type type :address address :data data
          :we we :re re :id id :status status)))

(defun write-msg (stream type address data we re id status)
  (let ((bytes (list type
                     (logand address #xff) (logand (ash address -8) #xff)
                     (logand (ash address -16) #xff) (logand (ash address -24) #xff)
                     (logand data #xff) (logand (ash data -8) #xff)
                     (logand (ash data -16) #xff) (logand (ash data -24) #xff)
                     we re id status)))
    (format t "write-msg: ~{~2,'0x ~}~%" bytes))
  (write-byte type stream)
  (write-u32 stream address)
  (write-u32 stream data)
  (write-byte we stream)
  (write-byte re stream)
  (write-byte id stream)
  (write-byte status stream)
  (finish-output stream))

(defun send-master-request (stream address data we re)
  (format t "send-master-request: addr=~a data=~a we=~a re=~a~%" address data we re)
  (write-msg stream +msg-master-request+ address data we re 0 0)
  (format t "send-master-request: done~%"))

;; Example usage:
;;
;; (defun main ()
;;   (let ((stream (connect-to-sim "/tmp/coe_emulator.sock")))
;;     ;; issue a request, wait for the reply
;;     (send-master-request stream #x10000 #xdeadbeef 1 0)
;;     (let ((msg (read-msg stream)))
;;       (format t "got: ~a~%" msg))))
