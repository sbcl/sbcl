(in-package "SB-BSD-SOCKETS")

;;;; Methods, classes, functions for sockets.  Protocol-specific stuff
;;;; is deferred to inet.lisp, unix.lisp, etc

#|| <h2>SOCKETs</h2>

|#

(eval-when (:load-toplevel :compile-toplevel :execute)
(defclass socket ()
  ((file-descriptor :initarg :descriptor
		    :reader socket-file-descriptor)
   (family :initform (error "No socket family") :reader socket-family)
   (protocol :initarg :protocol :reader socket-protocol)
   (type  :initarg :type :reader socket-type)
   (stream))))
  
(defmethod print-object ((object socket) stream)
  (print-unreadable-object (object stream :type t :identity t)
                           (princ "descriptor " stream)
                           (princ (slot-value object 'file-descriptor) stream)))


(defmethod shared-initialize :after ((socket socket) slot-names
				     &key protocol type
				     &allow-other-keys)
  (let* ((proto-num
	  (cond ((and protocol (keywordp protocol))
		 (get-protocol-by-name (string-downcase (symbol-name protocol))))
		(protocol protocol)
		(t 0)))
	 (fd (or (and (slot-boundp socket 'file-descriptor)
		      (socket-file-descriptor socket))
		 (sockint::socket (socket-family socket)
				  (ecase type
				    ((:datagram) sockint::sock-dgram)
				    ((:stream) sockint::sock-stream))
				  proto-num))))
      (if (= fd -1) (socket-error "socket"))
      (setf (slot-value socket 'file-descriptor) fd
	    (slot-value socket 'protocol) proto-num
	    (slot-value socket 'type) type)
      (sb-ext:finalize socket (lambda () (sockint::close fd)))))



(defgeneric make-sockaddr-for (socket &optional sockaddr &rest address)
  (:documentation "Return a Socket Address object suitable for use with SOCKET.
When SOCKADDR is passed, it is used instead of a new object."))

;; we deliberately redesign the "bind" interface: instead of passing a
;; sockaddr_something as second arg, we pass the elements of one as
;; multiple arguments.

(defgeneric socket-bind (socket &rest address)
  (:documentation "Bind SOCKET to ADDRESS, which may vary according to
socket family.  For the INET family, pass ADDRESS and PORT as two
arguments; for FILE address family sockets, pass the filename string.
See also bind(2)"))

(defmethod socket-bind ((socket socket)
                        &rest address)
  (let ((sockaddr (apply #'make-sockaddr-for socket nil address)))
    (if (= (sb-sys:without-gcing
	    (sockint::bind (socket-file-descriptor socket)
			   (sockint::array-data-address sockaddr)
			   (size-of-sockaddr socket)))
           -1)
        (socket-error "bind"))))


(defgeneric socket-accept (socket)
  (:documentation "Perform the accept(2) call, returning a
newly-created connected socket and the peer address as multiple
values"))
  
(defmethod socket-accept ((socket socket))
  (let* ((sockaddr (make-sockaddr-for socket))
         (fd (sb-sys:without-gcing
	      (sockint::accept (socket-file-descriptor socket)
			       (sockint::array-data-address sockaddr)
			       (size-of-sockaddr socket)))))
    (apply #'values
	   (if (= fd -1)
	       (socket-error "accept") 
	       (let ((s (make-instance (class-of socket)
				       :type (socket-type socket)
				       :protocol (socket-protocol socket)
				       :descriptor fd)))
		 (sb-ext:finalize s (lambda () (sockint::close fd)))))
	   (multiple-value-list (bits-of-sockaddr socket sockaddr)))))

(defgeneric socket-connect (socket &rest address)
  (:documentation "Perform the connect(2) call to connect SOCKET to a
  remote PEER.  No useful return value."))

(defmethod socket-connect ((socket socket) &rest peer)
  (let* ((sockaddr (apply #'make-sockaddr-for socket nil peer)))
    (if (= (sb-sys:without-gcing
	    (sockint::connect (socket-file-descriptor socket)
			      (sockint::array-data-address sockaddr)
			      (size-of-sockaddr socket)))
	   -1)
	(socket-error "connect") )))

(defgeneric socket-peername (socket)
  (:documentation "Return the socket's peer; depending on the address
  family this may return multiple values"))
  
(defmethod socket-peername ((socket socket))
  (let* ((sockaddr (make-sockaddr-for socket)))
    (when (= (sb-sys:without-gcing
	      (sockint::getpeername (socket-file-descriptor socket)
				    (sockint::array-data-address sockaddr)
				    (size-of-sockaddr socket)))
	     -1)
      (socket-error "getpeername"))
    (bits-of-sockaddr socket sockaddr)))

(defgeneric socket-name (socket)
  (:documentation "Return the address (as vector of bytes) and port
  that the socket is bound to, as multiple values."))

(defmethod socket-name ((socket socket))
  (let* ((sockaddr (make-sockaddr-for socket)))
    (when (= (sb-sys:without-gcing
	      (sockint::getsockname (socket-file-descriptor socket)
				    (sockint::array-data-address sockaddr)
				    (size-of-sockaddr socket)))
	     -1)
      (socket-error "getsockname"))
    (bits-of-sockaddr socket sockaddr)))


;;; There are a whole bunch of interesting things you can do with a
;;; socket that don't really map onto "do stream io", especially in
;;; CL which has no portable concept of a "short read".  socket-receive
;;; allows us to read from an unconnected socket into a buffer, and
;;; to learn who the sender of the packet was

(defgeneric socket-receive (socket buffer length
			    &key
			    oob peek waitall element-type)
  (:documentation "Read LENGTH octets from SOCKET into BUFFER (or a freshly-consed buffer if
NIL), using recvfrom(2).  If LENGTH is NIL, the length of BUFFER is
used, so at least one of these two arguments must be non-NIL.  If
BUFFER is supplied, it had better be of an element type one octet wide.
Returns the buffer, its length, and the address of the peer
that sent it, as multiple values.  On datagram sockets, sets MSG_TRUNC
so that the actual packet length is returned even if the buffer was too
small"))
  
(defmethod socket-receive ((socket socket) buffer length
			 &key
			 oob peek waitall
			 (element-type 'character))
  (let ((flags
	 (logior (if oob sockint::MSG-OOB 0)
		 (if peek sockint::MSG-PEEK 0)
		 (if waitall sockint::MSG-WAITALL 0)
		 sockint::MSG-NOSIGNAL	;don't send us SIGPIPE
		 (if (eql (socket-type socket) :datagram)
		     sockint::msg-TRUNC 0)))
	(sockaddr (make-sockaddr-for socket)))
    (unless (or buffer length)
      (error "Must supply at least one of BUFFER or LENGTH"))
    (unless buffer
      (setf buffer (make-array length :element-type element-type)))
    (sb-alien:with-alien ((sa-len (array (sb-alien:unsigned 32) 2)))
      (setf (sb-alien:deref sa-len 0) (size-of-sockaddr socket))
      (sb-sys:without-gcing 
       (let ((len
	      (sockint::recvfrom (socket-file-descriptor socket)
				 (sockint::array-data-address buffer)
				 (or length (length buffer))
				 flags
				 (sockint::array-data-address sockaddr)
				 (sb-alien:cast sa-len (* integer)))))
	 (when (= len -1) (socket-error "recvfrom"))
	 (apply #'values buffer len (multiple-value-list
				     (bits-of-sockaddr socket sockaddr))))))))



(defgeneric socket-listen (socket backlog)
  (:documentation "Mark SOCKET as willing to accept incoming connections.  BACKLOG
defines the maximum length that the queue of pending connections may
grow to before new connection attempts are refused.  See also listen(2)"))

(defmethod socket-listen ((socket socket) backlog)
  (let ((r (sockint::listen (socket-file-descriptor socket) backlog)))
    (if (= r -1)
        (socket-error "listen"))))

(defgeneric socket-close (socket)
  (:documentation "Close SOCKET.  May throw any kind of error that write(2) would have
thrown.  If SOCKET-MAKE-STREAM has been called, calls CLOSE on that
stream instead"))

(defmethod socket-close ((socket socket))
  ;; the close(2) manual page has all kinds of warning about not
  ;; checking the return value of close, on the grounds that an
  ;; earlier write(2) might have returned successfully w/o actually
  ;; writing the stuff to disk.  It then goes on to define the only
  ;; possible error return as EBADF (fd isn't a valid open file
  ;; descriptor).  Presumably this is an oversight and we could also
  ;; get anything that write(2) would have given us.

  ;; What we do: we catch EBADF.  It should only ever happen if
  ;; (a) someone's closed the socket already (stream closing seems
  ;; to have this effect) or (b) the caller is messing around with
  ;; socket internals.  That's not supported, dude
  
  (if (slot-boundp socket 'stream)
      (close (slot-value socket 'stream))  ;; closes socket as well
    (handler-case
     (if (= (sockint::close (socket-file-descriptor socket)) -1)
         (socket-error "close"))
     (bad-file-descriptor-error (c) (declare (ignore c)) nil)
     (:no-error (c)  (declare (ignore c)) nil))))

(defgeneric socket-make-stream (socket  &rest args)
    (:documentation "Find or create a STREAM that can be used for IO
on SOCKET (which must be connected).  ARGS are passed onto
SB-SYS:MAKE-FD-STREAM."))

(defmethod socket-make-stream ((socket socket)  &rest args)
  (let ((stream
	 (and (slot-boundp socket 'stream) (slot-value socket 'stream))))
    (unless stream
      (setf stream (apply #'sb-sys:make-fd-stream
			  (socket-file-descriptor socket) args))
      (setf (slot-value socket 'stream) stream)
      (sb-ext:cancel-finalization socket))
    stream))



;;; Error handling

(define-condition socket-error (error)
  ((errno :initform nil
          :initarg :errno  
          :reader socket-error-errno) 
   (symbol :initform nil :initarg :symbol :reader socket-error-symbol)
   (syscall  :initform "outer space" :initarg :syscall :reader socket-error-syscall))
  (:report (lambda (c s)
             (let ((num (socket-error-errno c)))
               (format s "Socket error in \"~A\": ~A (~A)"
                       (socket-error-syscall c)
                       (or (socket-error-symbol c) (socket-error-errno c))
                       #+cmu (sb-unix:get-unix-error-msg num)
                       #+sbcl (sb-int:strerror num))))))

;;; watch out for slightly hacky symbol punning: we use both the value
;;; and the symbol-name of sockint::efoo

(defmacro define-socket-condition (symbol name)
  `(progn
     (define-condition ,name (socket-error)
       ((symbol :reader socket-error-symbol :initform (quote ,symbol))))
     (push (cons ,symbol (quote ,name)) *conditions-for-errno*)))

(defparameter *conditions-for-errno* nil)
;;; this needs the rest of the list adding to it, really.  They also
;;; need
;;; - conditions to be exported in the DEFPACKAGE form
;;; - symbols to be added to constants.ccon
;;; I haven't yet thought of a non-kludgey way of keeping all this in
;;; the same place
(define-socket-condition sockint::EADDRINUSE address-in-use-error)
(define-socket-condition sockint::EAGAIN interrupted-error)
(define-socket-condition sockint::EBADF bad-file-descriptor-error)
(define-socket-condition sockint::ECONNREFUSED connection-refused-error)
(define-socket-condition sockint::EINTR interrupted-error)
(define-socket-condition sockint::EINVAL invalid-argument-error)
(define-socket-condition sockint::ENOBUFS no-buffers-error)
(define-socket-condition sockint::ENOMEM out-of-memory-error)
(define-socket-condition sockint::EOPNOTSUPP operation-not-supported-error)
(define-socket-condition sockint::EPERM operation-not-permitted-error)
(define-socket-condition sockint::EPROTONOSUPPORT protocol-not-supported-error)
(define-socket-condition sockint::ESOCKTNOSUPPORT socket-type-not-supported-error)
(define-socket-condition sockint::ENETUNREACH network-unreachable-error)


(defun condition-for-errno (err)
  (or (cdr (assoc err *conditions-for-errno* :test #'eql)) 'socket-error))
      
#+cmu
(defun socket-error (where)
  ;; Peter's debian/x86 cmucl packages (and sbcl, derived from them)
  ;; use a direct syscall interface, and have to call UNIX-GET-ERRNO
  ;; to update the value that unix-errno looks at.  On other CMUCL
  ;; ports, (UNIX-GET-ERRNO) is not needed and doesn't exist
  (when (fboundp 'unix::unix-get-errno) (unix::unix-get-errno))
  (let ((condition (condition-for-errno sb-unix:unix-errno)))
    (error condition :errno sb-unix:unix-errno  :syscall where)))

#+sbcl
(defun socket-error (where)
  (let* ((errno  (sb-unix::get-errno))
         (condition (condition-for-errno errno)))
    (error condition :errno errno  :syscall where)))


(defgeneric bits-of-sockaddr (socket sockaddr)
  (:documentation "Return protocol-dependent bits of parameter
SOCKADDR, e.g. the Host/Port if SOCKET is an inet socket."))

(defgeneric size-of-sockaddr (socket)
  (:documentation "Return the size of a sockaddr object for SOCKET."))
