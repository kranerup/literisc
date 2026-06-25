(defpackage :conf-socket
  (:use :cl)
  (:export :make-server-socket
           :accept-connection
           :read-msg
           :send-master-request
           :+msg-master-reply+
           :+msg-master-request+))
(in-package :conf-socket)

(require :sb-bsd-sockets)

(defconstant +msg-size+ 13)
(defconstant +msg-master-reply+   2)
(defconstant +msg-master-request+ 3)

(defun make-server-socket (path)
  (let ((socket (make-instance 'sb-bsd-sockets:local-socket
                                :type :stream)))
    (when (probe-file path)
      (delete-file path))
    (sb-bsd-sockets:socket-bind socket path)
    (sb-bsd-sockets:socket-listen socket 1)
    socket))

(defun accept-connection (server)
  (sb-bsd-sockets:socket-accept server))

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

;;(defun write-msg (stream type address data we re id status)
;;  (write-byte type stream)
;;  (write-u32 stream address)
;;  (write-u32 stream data)
;;  (write-byte we stream)
;;  (write-byte re stream)
;;  (write-byte id stream)
;;  (write-byte status stream)
;;  (finish-output stream))
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

;;(defun main ()
;;  (let* ((server (make-server-socket "/tmp/coe_emulator.sock"))
;;         (dummy  (format t "COE emulator waiting for connection...~%"))
;;         (client (accept-connection server))
;;         (stream (sb-bsd-sockets:socket-make-stream
;;                   client
;;                   :input t :output t
;;                   :element-type '(unsigned-byte 8)
;;                   :buffering :none)))
;;    (format t "connected~%")
;;    (loop
;;      ;; wait for master reply from C
;;      (let ((msg (read-msg stream)))
;;        (format t "got: ~a~%" msg)
;;        ;; your emulator logic here
;;        ;; (send-master-request stream address data 1 0)
;;        ))))
