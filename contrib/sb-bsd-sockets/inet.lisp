(in-package :sb-bsd-sockets)

;;; Our class and constructor

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass inet-socket (socket)
    ((family :initform sockint::AF-INET))
    (:documentation "Class representing TCP and UDP sockets.

Examples:

 (make-instance 'inet-socket :type :stream :protocol :tcp)

 (make-instance 'inet-socket :type :datagram :protocol :udp)
")))

;;; XXX should we *...* this?
(defparameter inet-address-any (vector 0 0 0 0))

;;; binding a socket to an address and port.  Doubt that anyone's
;;; actually using this much, to be honest.

(defun make-inet-address (dotted-quads)
  "Return a vector of octets given a string DOTTED-QUADS in the format
\"127.0.0.1\""
  (map 'vector
       #'parse-integer
       (split dotted-quads nil '(#\.))))

;;; getprotobyname only works in the internet domain, which is why this
;;; is here
(defun get-protocol-by-name (name)      ;exported
  "Returns the network protocol number associated with the string NAME,
using getprotobyname(2) which typically looks in NIS or /etc/protocols"
  ;; for extra brownie points, could return canonical protocol name
  ;; and aliases as extra values
  (let ((ent (sockint::getprotobyname name)))
    (sockint::protoent-proto ent)))

;;; our protocol provides make-sockaddr-for, size-of-sockaddr,
;;; bits-of-sockaddr

(defmethod make-sockaddr-for ((socket inet-socket) &optional sockaddr &rest address &aux (host (first address)) (port (second address)))
  (let ((sockaddr (or sockaddr (sockint::allocate-sockaddr-in))))
    (when (and host port)
      (setf host (coerce host '(simple-array (unsigned-byte 8) (4))))
      ;; port and host are represented in C as "network-endian" unsigned
      ;; integers of various lengths.  This is stupid.  The value of the
      ;; integer doesn't matter (and will change depending on your
      ;; machine's endianness); what the bind(2) call is interested in
      ;; is the pattern of bytes within that integer.

      ;; We have no truck with such dreadful type punning.  Octets to
      ;; octets, dust to dust.
      
      (setf (sockint::sockaddr-in-family sockaddr) sockint::af-inet)
      (setf (sb-alien:deref (sockint::sockaddr-in-port sockaddr) 0) (ldb (byte 8 8) port))
      (setf (sb-alien:deref (sockint::sockaddr-in-port sockaddr) 1) (ldb (byte 8 0) port))

      (setf (sb-alien:deref (sockint::sockaddr-in-addr sockaddr) 0) (elt host 0))
      (setf (sb-alien:deref (sockint::sockaddr-in-addr sockaddr) 1) (elt host 1))
      (setf (sb-alien:deref (sockint::sockaddr-in-addr sockaddr) 2) (elt host 2))
      (setf (sb-alien:deref (sockint::sockaddr-in-addr sockaddr) 3) (elt host 3)))
    sockaddr))

(defmethod free-sockaddr-for ((socket inet-socket) sockaddr)
  (sockint::free-sockaddr-in sockaddr))

(defmethod size-of-sockaddr ((socket inet-socket))
  sockint::size-of-sockaddr-in)

(defmethod bits-of-sockaddr ((socket inet-socket) sockaddr)
  "Returns address and port of SOCKADDR as multiple values"
  (values
   (coerce (loop for i from 0 below 4
		 collect (sb-alien:deref (sockint::sockaddr-in-addr sockaddr) i))
	   '(vector (unsigned-byte 8) 4))
   (+ (* 256 (sb-alien:deref (sockint::sockaddr-in-port sockaddr) 0))
      (sb-alien:deref (sockint::sockaddr-in-port sockaddr) 1))))
   
(defun make-inet-socket (type protocol)
  "Make an INET socket.  Deprecated in favour of make-instance"
  (make-instance 'inet-socket :type type :protocol protocol))
