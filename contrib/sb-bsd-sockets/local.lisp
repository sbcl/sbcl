(in-package :sb-bsd-sockets)

(defclass local-socket (socket)
  ((family :initform sockint::af-local))
  (:documentation "Class representing local domain (AF_LOCAL) sockets,
also known as unix-domain sockets."))

(defmethod make-sockaddr-for ((socket local-socket)
			      &optional sockaddr &rest address &aux (filename (first address)))
  (let ((sockaddr (or sockaddr (sockint::allocate-sockaddr-un))))
    (setf (sockint::sockaddr-un-family sockaddr) sockint::af-local)
    (when filename
      (setf (sockint::sockaddr-un-path sockaddr) filename))
    sockaddr))

(defmethod free-sockaddr-for ((socket local-socket) sockaddr)
  (sockint::free-sockaddr-un sockaddr))

(defmethod size-of-sockaddr ((socket local-socket))
  sockint::size-of-sockaddr-un)

(defmethod bits-of-sockaddr ((socket local-socket) sockaddr)
  "Return the file name of the local socket address SOCKADDR."
  (let ((name (sockint::sockaddr-un-path sockaddr)))
    (if (zerop (length name)) nil name)))

