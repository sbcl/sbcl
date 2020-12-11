(in-package :sb-bsd-sockets)

;;; Socket class and constructor

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass inet6-socket (socket)
    ((family :initform sockint::AF-INET6))
    (:documentation "Class representing TCP and UDP over IPv6 sockets.

Examples:

 (make-instance 'sb-bsd-sockets:inet6-socket :type :stream :protocol :tcp)

 (make-instance 'sb-bsd-sockets:inet6-socket :type :datagram :protocol :udp)
")))

(defun address-numbers/v6 (address)
  (loop for i from 0 below 16 by 2 collect
       (+ (* 256 (elt address i)) (elt address (1+ i)))))

(defun endpoint-string/v6 (address port)
  (assert (= (length address) 16))
  (format nil "[~A]:~A" (unparse-inet6-address address) port))

(defmethod socket-namestring ((socket inet6-socket))
  (ignore-errors
    (multiple-value-bind (address port) (socket-name socket)
      (endpoint-string/v6 address port))))

(defmethod socket-peerstring ((socket inet6-socket))
  (ignore-errors
   (multiple-value-bind (address port) (socket-peername socket)
     (endpoint-string/v6 address port))))

(defun unparse-inet6-address (address)
  (sb-alien:with-alien ((octets (array sb-alien:unsigned-char 16))
                        (storage (array sb-alien:char #.sockint::INET6-ADDRSTRLEN)))
    (dotimes (i 16)
      (setf (sb-alien:deref octets i) (elt address i)))
    (socket-error-case ("inet_ntop"
                        (sockint::inet-ntop
                         sockint::af-inet6
                         (sb-alien:cast octets (* t))
                         (sb-alien:cast storage (* sb-alien:char))
                         sockint::INET6-ADDRSTRLEN)
                        result (null result))
        result)))

(defun make-inet6-address (colon-separated-integers)
  "Return a vector of octets given a string representation of an IPv6
address COLON-SEPARATED-INTEGERS. Signal an error if the string is
malformed."
  (declare (type string colon-separated-integers))
  (sb-alien:with-alien ((octets (array sb-alien:unsigned-char 16)))
    (socket-error-case ("inet_pton"
                        (sockint::inet-pton
                         sockint::af-inet6 colon-separated-integers
                         (sb-alien:cast octets (* sb-alien:unsigned-char)))
                        result (and (/= result 0) (/= result 1)))
        (if (zerop result)
            (error "~@<~S does not designate an IPv6 address.~@:>"
                   colon-separated-integers)
            (let ((result (make-array 16 :element-type '(unsigned-byte 8))))
              (dotimes (i 16)
                (setf (elt result i) (sb-alien:deref octets i)))
              result)))))

;;; our protocol provides make-sockaddr-for, size-of-sockaddr,
;;; bits-of-sockaddr

(defmethod make-sockaddr-for ((socket inet6-socket) &optional sockaddr
                              &rest address)
  (check-type address (or null (cons sequence (cons (unsigned-byte 16)))))
  (let ((host (first address))
        (port (second address))
        (sockaddr (or sockaddr (sockint::allocate-sockaddr-in6))))
    (when (and host port)
      (assert (= (length host) 16))
      ;; port and host are represented in C as "network-endian" unsigned
      ;; integers of various lengths.  This is stupid.  The value of the
      ;; integer doesn't matter (and will change depending on your
      ;; machine's endianness); what the bind(2) call is interested in
      ;; is the pattern of bytes within that integer.

      ;; We have no truck with such dreadful type punning.  Octets to
      ;; octets, dust to dust.

      (setf (sockint::sockaddr-in6-family sockaddr)
            sockint::af-inet6
            (sb-alien:deref (sockint::sockaddr-in6-port sockaddr) 0)
            (ldb (byte 8 8) port)
            (sb-alien:deref (sockint::sockaddr-in6-port sockaddr) 1)
            (ldb (byte 8 0) port))
      (dotimes (i 4)
        (setf (sb-alien:deref (sockint::sockaddr-in6-flowinfo sockaddr) i) 0))
      (dotimes (i 16)
        (setf (sb-alien:deref (sockint::sockaddr-in6-addr sockaddr) i) (elt host i)))
      (dotimes (i 4)
        (setf (sb-alien:deref (sockint::sockaddr-in6-scope-id sockaddr) i) 0)))
    sockaddr))

(defmethod free-sockaddr-for ((socket inet6-socket) sockaddr)
  (sb-alien:free-alien sockaddr))

(defmethod size-of-sockaddr ((socket inet6-socket))
  sockint::size-of-sockaddr-in6)

(defmethod bits-of-sockaddr ((socket inet6-socket) sockaddr &optional size)
  "Returns address and port of SOCKADDR as multiple values"
  (declare (ignore size))
  (values
   (coerce (loop for i from 0 below 16
              collect (sb-alien:deref (sockint::sockaddr-in6-addr sockaddr) i))
           '(vector (unsigned-byte 8) 16))
   (+ (* 256 (sb-alien:deref (sockint::sockaddr-in6-port sockaddr) 0))
      (sb-alien:deref (sockint::sockaddr-in6-port sockaddr) 1))))
