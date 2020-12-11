(in-package :sb-bsd-sockets)

;;; Our class and constructor

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass inet-socket (socket)
    ((family :initform sockint::AF-INET))
    (:documentation "Class representing TCP and UDP over IPv4 sockets.

Examples:

 (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)

 (make-instance 'sb-bsd-sockets:inet-socket :type :datagram :protocol :udp)
")))

(defun address-numbers/v4 (address)
  (coerce address 'list))

(defun endpoint-string/v4 (address port)
  (format nil "~{~A~^.~}:~A" (address-numbers/v4 address) port))

(defmethod socket-namestring ((socket inet-socket))
  (ignore-errors
    (multiple-value-bind (address port) (socket-name socket)
      (endpoint-string/v4 address port))))

(defmethod socket-peerstring ((socket inet-socket))
  (ignore-errors
    (multiple-value-bind (address port) (socket-peername socket)
      (endpoint-string/v4 address port))))

;;; binding a socket to an address and port.  Doubt that anyone's
;;; actually using this much, to be honest.

(defun make-inet-address (dotted-quads)
  "Return a vector of octets given a string DOTTED-QUADS in the format
\"127.0.0.1\". Signals an error if the string is malformed."
  (declare (type string dotted-quads))
  (labels ((oops ()
             (error "~S is not a string designating an IP address."
                    dotted-quads))
           (check (x)
             (if (typep x '(unsigned-byte 8))
                 x
                 (oops))))
    (let* ((s1 (position #\. dotted-quads))
           (s2 (if s1 (position #\. dotted-quads :start (1+ s1)) (oops)))
           (s3 (if s2 (position #\. dotted-quads :start (1+ s2)) (oops)))
           (u0 (parse-integer dotted-quads :end s1))
           (u1 (parse-integer dotted-quads :start (1+ s1) :end s2))
           (u2 (parse-integer dotted-quads :start (1+ s2) :end s3)))
      (multiple-value-bind (u3 end) (parse-integer dotted-quads :start (1+ s3) :junk-allowed t)
        (unless (= end (length dotted-quads))
          (oops))
        (let ((vector (make-array 4 :element-type '(unsigned-byte 8))))
          (setf (aref vector 0) (check u0)
                (aref vector 1) (check u1)
                (aref vector 2) (check u2)
                (aref vector 3) (check u3))
          vector)))))

;;; our protocol provides make-sockaddr-for, size-of-sockaddr,
;;; bits-of-sockaddr

(defmethod make-sockaddr-for ((socket inet-socket) &optional sockaddr &rest address)
  (check-type address (or null (cons sequence (cons (unsigned-byte 16)))))
  (let ((host (first address))
        (port (second address))
        (sockaddr (or sockaddr (sockint::allocate-sockaddr-in))))
    (when (and host port)
      (assert (= (length host) 4))
      (let ((in-port (sockint::sockaddr-in-port sockaddr))
            (in-addr (sockint::sockaddr-in-addr sockaddr)))
        (declare (fixnum port))
        ;; port and host are represented in C as "network-endian" unsigned
        ;; integers of various lengths.  This is stupid.  The value of the
        ;; integer doesn't matter (and will change depending on your
        ;; machine's endianness); what the bind(2) call is interested in
        ;; is the pattern of bytes within that integer.

        ;; We have no truck with such dreadful type punning.  Octets to
        ;; octets, dust to dust.
        (setf (sockint::sockaddr-in-family sockaddr) sockint::af-inet)
        (setf (sb-alien:deref in-port 0) (ldb (byte 8 8) port))
        (setf (sb-alien:deref in-port 1) (ldb (byte 8 0) port))

        (setf (sb-alien:deref in-addr 0) (elt host 0))
        (setf (sb-alien:deref in-addr 1) (elt host 1))
        (setf (sb-alien:deref in-addr 2) (elt host 2))
        (setf (sb-alien:deref in-addr 3) (elt host 3))))
  sockaddr))

(defmethod free-sockaddr-for ((socket inet-socket) sockaddr)
  (sb-alien:free-alien sockaddr))

(defmethod size-of-sockaddr ((socket inet-socket))
  sockint::size-of-sockaddr-in)

(defmethod bits-of-sockaddr ((socket inet-socket) sockaddr &optional size)
  "Returns address and port of SOCKADDR as multiple values"
  (declare (type (sb-alien:alien
                  (* (sb-alien:struct sb-bsd-sockets-internal::sockaddr-in)))
                 sockaddr)
           (ignore size))
  (let ((vector (make-array 4 :element-type '(unsigned-byte 8))))
    (loop for i below 4
          do (setf (aref vector i)
                   (sb-alien:deref (sockint::sockaddr-in-addr sockaddr) i)))
    (values
     vector
     (+ (* 256 (sb-alien:deref (sockint::sockaddr-in-port sockaddr) 0))
        (sb-alien:deref (sockint::sockaddr-in-port sockaddr) 1)))))

(defun make-inet-socket (type protocol)
  "Make an INET socket."
  (make-instance 'inet-socket :type type :protocol protocol))

(declaim (sb-ext:deprecated
          :late ("SBCL" "1.2.15")
          (function make-inet-socket :replacement make-instance)))
