(in-package :sb-bsd-sockets)

;;; Miscellaneous things, placed here until I can find a logically more
;;; coherent place to put them

;;; I don't want to provide a complete interface to unix file
;;; operations, for example, but being about to set O_NONBLOCK on a
;;; socket is a necessary operation.

;;; XXX bad (sizeof (int) ==4 ) assumptions

(defgeneric non-blocking-mode (socket)
  (:documentation "Is SOCKET in non-blocking mode?"))

#-win32
(defmethod non-blocking-mode ((socket socket))
  (let ((fd (socket-file-descriptor socket)))
    (sb-alien:with-alien ((arg integer))
                         (> (logand
                             (sockint::fcntl fd sockint::f-getfl arg)
                             sockint::o-nonblock)
                            0))))

#+win32
(defmethod non-blocking-mode ((socket socket)) 0)

(defgeneric (setf non-blocking-mode) (non-blocking-p socket)
  (:documentation "Put SOCKET in non-blocking mode - or not, according to NON-BLOCKING-P"))

#-win32
(defmethod (setf non-blocking-mode) (non-blocking-p (socket socket))
  (declare (optimize (speed 3)))
  (let* ((fd (socket-file-descriptor socket))
         (arg1 (the (signed-byte 32) (sockint::fcntl fd sockint::f-getfl 0)))
         (arg2
          (if non-blocking-p
              (logior arg1 sockint::o-nonblock)
            (logand (lognot sockint::o-nonblock) arg1))))
    (when (= (the (signed-byte 32) -1)
             (the (signed-byte 32)
               (sockint::fcntl fd sockint::f-setfl arg2)))
      (socket-error "fcntl"))
    non-blocking-p))

#+win32
(defmethod (setf non-blocking-mode) (non-blocking-p (socket socket)) 0)
;;  (sb-alien:with-alien ((mode (unsigned 32)))
;;    (if non-blocking-p (setf mode 1))
;;       (ioctlsocket socket FIONBIO mode)))

