(in-package :sb-bsd-sockets)

#|| <h2>Local (unix) domain sockets</h2>

Local domain (AF_LOCAL) sockets are also known as Unix-domain sockets, but were
renamed by POSIX presumably on the basis that they may be
available on other systems too.  

A local socket address is a string, which is used to create a node
in the local filesystem.  This means of course that they cannot be used across
a network.

||#

(defclass local-socket (socket)
  ((family :initform sockint::af-local)))

(defmethod make-sockaddr-for ((socket local-socket) &optional sockaddr &rest address &aux (filename (first address)))
  (let ((sockaddr (or sockaddr (sockint::allocate-sockaddr-un))))
    (setf (sockint::sockaddr-un-family sockaddr) sockint::af-local)
    (when filename
      (loop for c across filename
	    ;; XXX magic constant ew ew ew.  should grovel this from
	    ;; system headers
	    for i from 0 to (min 107 (1- (length filename)))
	    do (setf (sockint::sockaddr-un-path sockaddr i) (char-code c))
	    finally
	    (setf (sockint::sockaddr-un-path sockaddr (1+ i)) 0)))
    sockaddr))

(defmethod size-of-sockaddr ((socket local-socket))
  sockint::size-of-sockaddr-un)

(defmethod bits-of-sockaddr ((socket local-socket) sockaddr)
  "Return the file name of the local socket address SOCKADDR."
  (let ((name (sb-c-call::%naturalize-c-string
	       (sb-sys:sap+ (sockint::array-data-address sockaddr)
			    sockint::offset-of-sockaddr-un-path))))
    (if (zerop (length name)) nil name)))

