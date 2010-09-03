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

(defmethod socket-namestring ((socket inet-socket))
  (ignore-errors
    (multiple-value-bind (addr port) (socket-name socket)
      (format nil "~{~A~^.~}:~A" (coerce addr 'list) port))))

(defmethod socket-peerstring ((socket inet-socket))
  (ignore-errors
    (multiple-value-bind (addr port) (socket-peername socket)
      (format nil "~{~A~^.~}:~A" (coerce addr 'list) port))))

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

(define-condition unknown-protocol ()
  ((name :initarg :name
         :reader unknown-protocol-name))
  (:report (lambda (c s)
             (format s "Protocol not found: ~a" (prin1-to-string
                                                 (unknown-protocol-name c))))))

#+(and sb-thread (not os-provides-getprotoby-r))
;; Since getprotobyname is not thread-safe, we need a lock.
(sb-ext:defglobal **getprotoby-lock** (sb-thread:make-mutex :name "getprotoby lock"))

;;; getprotobyname only works in the internet domain, which is why this
;;; is here
(defun get-protocol-by-name (name)      ;exported
  "Given a protocol name, return the protocol number, the protocol name, and
a list of protocol aliases"

  ;; Brownie Points.  Hopefully there's one person out there using
  ;; RSPF sockets and SBCL who will appreciate the extra info
  (labels ((protoent-to-values (protoent)
             (values
              (sockint::protoent-proto protoent)
              (sockint::protoent-name protoent)
              (let ((index 0))
                (loop
                   for alias = (sb-alien:deref (sockint::protoent-aliases protoent) index)
                   while (not (sb-alien:null-alien alias))
                   do (incf index)
                   collect (sb-alien::c-string-to-string (sb-alien:alien-sap alias)
                                                         (sb-impl::default-external-format)
                                                         'character))))))
    #+(and sb-thread os-provides-getprotoby-r)
    (let ((buffer-length 1024)
          (max-buffer 10000))
      (declare (type fixnum buffer-length)
               (type fixnum max-buffer))
      (loop
         (sb-alien:with-alien ((result-buf (* sockint::protoent)
                                           (sb-alien:make-alien sockint::protoent))
                               (buffer (* char )
                                       (sb-alien:make-alien sb-alien:char buffer-length))
                               #-solaris
                               (result (* (* sockint::protoent))
                                       (sb-alien:make-alien (* sockint::protoent))))

           (let ((res (sockint::getprotobyname-r name
                                                 result-buf
                                                 buffer
                                                 buffer-length
                                                 #-solaris
                                                 result)))
             (if (eql res 0)
                 (progn
                   #-solaris
                   (when (sb-alien::null-alien (sb-alien:deref result 0))
                     (error 'unknown-protocol :name name))
                   (return-from get-protocol-by-name
                     (protoent-to-values result-buf)))
                 (let ((errno (sb-unix::get-errno)))
                   (if (eql errno  sockint::erange)
                       (progn
                         (incf buffer-length 1024)
                         (if (> buffer-length max-buffer)
                             (error "Exceeded max-buffer of ~d" max-buffer)))
                       (error "Unexpected errno ~d" errno))))))))
    #-(and sb-thread os-provides-getprotoby-r)
    (tagbody
       (flet ((get-it ()
                (let ((ent (sockint::getprotobyname name)))
                  (if (sb-alien::null-alien ent)
                      (go :error)
                      (return-from get-protocol-by-name (protoent-to-values ent))))))
         #+sb-thread
         (sb-thread::with-system-mutex (**getprotoby-lock**)
           (get-it))
         #-sb-thread
         (get-it))
     :error
       (error 'unknown-protocol :name name))))

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
