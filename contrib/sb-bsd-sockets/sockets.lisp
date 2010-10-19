(in-package "SB-BSD-SOCKETS")

;;;; Methods, classes, functions for sockets.  Protocol-specific stuff
;;;; is deferred to inet.lisp, unix.lisp, etc

(eval-when (:load-toplevel :compile-toplevel :execute)

(defclass socket ()
  ((file-descriptor :initarg :descriptor
                    :reader socket-file-descriptor)
   (family :initform (error "No socket family")
           :reader socket-family)
   (protocol :initarg :protocol
             :reader socket-protocol
             :documentation "Protocol used by the socket. If a
keyword, the symbol-name of the keyword will be passed to
GET-PROTOCOL-BY-NAME downcased, and the returned value used as
protocol. Other values are used as-is.")
   (type  :initarg :type
          :reader socket-type
          :documentation "Type of the socket: :STREAM or :DATAGRAM.")
   (stream))
  (:documentation "Common base class of all sockets, not meant to be
directly instantiated.")))

(defmethod print-object ((object socket) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~@[~A, ~]~@[peer: ~A, ~]fd: ~A"
            (socket-namestring object)
            (socket-peerstring object)
            (slot-value object 'file-descriptor))))

(defgeneric socket-namestring (socket))

(defmethod socket-namestring (socket)
  nil)

(defgeneric socket-peerstring (socket))

(defmethod socket-peerstring (socket)
  nil)

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
      (sb-ext:finalize socket (lambda () (sockint::close fd))
                       :dont-save t)))



(defgeneric make-sockaddr-for (socket &optional sockaddr &rest address)
  (:documentation "Return a Socket Address object suitable for use with SOCKET.
When SOCKADDR is passed, it is used instead of a new object."))

(defgeneric free-sockaddr-for (socket sockaddr)
  (:documentation "Deallocate a Socket Address object that was
created for SOCKET."))

(defmacro with-sockaddr-for ((socket sockaddr &optional sockaddr-args) &body body)
  `(let ((,sockaddr (apply #'make-sockaddr-for ,socket nil ,sockaddr-args)))
     (unwind-protect (progn ,@body)
       (free-sockaddr-for ,socket ,sockaddr))))

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
  (with-sockaddr-for (socket sockaddr address)
    (if (= (sockint::bind (socket-file-descriptor socket)
                          sockaddr
                          (size-of-sockaddr socket))
           -1)
        (socket-error "bind"))))


(defgeneric socket-accept (socket)
  (:documentation "Perform the accept(2) call, returning a
newly-created connected socket and the peer address as multiple
values"))

(defmethod socket-accept ((socket socket))
  (with-sockaddr-for (socket sockaddr)
    (let ((fd (sockint::accept (socket-file-descriptor socket)
                               sockaddr
                               (size-of-sockaddr socket))))
      (cond
        ((and (= fd -1)
              (member (sb-unix::get-errno)
                      (list sockint::EAGAIN sockint::EINTR)))
         nil)
        ((= fd -1) (socket-error "accept"))
        (t (apply #'values
                  (let ((s (make-instance (class-of socket)
                              :type (socket-type socket)
                              :protocol (socket-protocol socket)
                              :descriptor fd)))
                    (sb-ext:finalize s (lambda () (sockint::close fd))
                                     :dont-save t))
                  (multiple-value-list (bits-of-sockaddr socket sockaddr))))))))

(defgeneric socket-connect (socket &rest address)
  (:documentation "Perform the connect(2) call to connect SOCKET to a
  remote PEER.  No useful return value."))

(defmethod socket-connect ((socket socket) &rest peer)
  (with-sockaddr-for (socket sockaddr peer)
    (if (= (sockint::connect (socket-file-descriptor socket)
                             sockaddr
                             (size-of-sockaddr socket))
           -1)
        (socket-error "connect"))))

(defgeneric socket-peername (socket)
  (:documentation "Return the socket's peer; depending on the address
  family this may return multiple values"))

(defmethod socket-peername ((socket socket))
  (with-sockaddr-for (socket sockaddr)
    (when (= (sockint::getpeername (socket-file-descriptor socket)
                                    sockaddr
                                    (size-of-sockaddr socket))
             -1)
      (socket-error "getpeername"))
    (bits-of-sockaddr socket sockaddr)))

(defgeneric socket-name (socket)
  (:documentation "Return the address (as vector of bytes) and port
  that the socket is bound to, as multiple values."))

(defmethod socket-name ((socket socket))
  (with-sockaddr-for (socket sockaddr)
    (when (= (sockint::getsockname (socket-file-descriptor socket)
                                   sockaddr
                                   (size-of-sockaddr socket))
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
                            oob peek waitall dontwait element-type)
  (:documentation
   "Read LENGTH octets from SOCKET into BUFFER (or a freshly-consed
buffer if NIL), using recvfrom(2). If LENGTH is NIL, the length of
BUFFER is used, so at least one of these two arguments must be
non-NIL. If BUFFER is supplied, it had better be of an element type
one octet wide. Returns the buffer, its length, and the address of the
peer that sent it, as multiple values. On datagram sockets, sets
MSG_TRUNC so that the actual packet length is returned even if the
buffer was too small."))

(defmethod socket-receive ((socket socket) buffer length
                           &key
                           oob peek waitall dontwait
                           (element-type 'character))
  (with-sockaddr-for (socket sockaddr)
    (let ((flags
           (logior (if oob sockint::MSG-OOB 0)
                   (if peek sockint::MSG-PEEK 0)
                   (if waitall sockint::MSG-WAITALL 0)
                   (if dontwait sockint::MSG-DONTWAIT 0)
                   #+linux sockint::MSG-NOSIGNAL ;don't send us SIGPIPE
                   (if (eql (socket-type socket) :datagram)
                       sockint::msg-TRUNC 0))))
      (unless (or buffer length)
        (error "Must supply at least one of BUFFER or LENGTH"))
      (unless length
        (setf length (length buffer)))
      (when buffer (setf element-type (array-element-type buffer)))
      (unless (or (subtypep element-type 'character)
                  (subtypep element-type 'integer))
        (error "Buffer element-type must be either a character or an integer subtype."))
      (unless buffer
        (setf buffer (make-array length :element-type element-type)))
      ;; really big FIXME: This whole copy-buffer thing is broken.
      ;; doesn't support characters more than 8 bits wide, or integer
      ;; types that aren't (unsigned-byte 8).
      (let ((copy-buffer (sb-alien:make-alien (array (sb-alien:unsigned 8) 1) length)))
        (unwind-protect
            (sb-alien:with-alien ((sa-len sockint::socklen-t (size-of-sockaddr socket)))
              (let ((len
                     (sockint::recvfrom (socket-file-descriptor socket)
                                        copy-buffer
                                        length
                                        flags
                                        sockaddr
                                        (sb-alien:addr sa-len))))
                (cond
                  ((and (= len -1)
                        (member (sb-unix::get-errno)
                                (list sockint::EAGAIN sockint::EINTR)))
                   nil)
                  ((= len -1) (socket-error "recvfrom"))
                  (t (loop for i from 0 below len
                           do (setf (elt buffer i)
                                    (cond
                                      ((or (eql element-type 'character) (eql element-type 'base-char))
                                       (code-char (sb-alien:deref (sb-alien:deref copy-buffer) i)))
                                      (t (sb-alien:deref (sb-alien:deref copy-buffer) i)))))
                     (apply #'values buffer len (multiple-value-list
                                                 (bits-of-sockaddr socket sockaddr)))))))
          (sb-alien:free-alien copy-buffer))))))

(defmacro with-vector-sap ((name vector) &body body)
  `(sb-sys:with-pinned-objects (,vector)
     (let ((,name (sb-sys:vector-sap ,vector)))
       ,@body)))

(defgeneric socket-send (socket buffer length
                                &key
                                address
                                external-format
                                oob eor dontroute dontwait nosignal
                                #+linux confirm #+linux more)
  (:documentation
   "Send LENGTH octets from BUFFER into SOCKET, using sendto(2). If BUFFER
is a string, it will converted to octets according to EXTERNAL-FORMAT. If
LENGTH is NIL, the length of the octet buffer is used. The format of ADDRESS
depends on the socket type (for example for INET domain sockets it would
be a list of an IP address and a port). If no socket address is provided,
send(2) will be called instead. Returns the number of octets written."))

(defmethod socket-send ((socket socket) buffer length
                        &key
                        address
                        (external-format :default)
                        oob eor dontroute dontwait nosignal
                        #+linux confirm #+linux more)
  (let* ((flags
          (logior (if oob sockint::MSG-OOB 0)
                  (if eor sockint::MSG-EOR 0)
                  (if dontroute sockint::MSG-DONTROUTE 0)
                  (if dontwait sockint::MSG-DONTWAIT 0)
                  #-darwin (if nosignal sockint::MSG-NOSIGNAL 0)
                  #+linux (if confirm sockint::MSG-CONFIRM 0)
                  #+linux (if more sockint::MSG-MORE 0)))
         (buffer (etypecase buffer
                   (string
                    (sb-ext:string-to-octets buffer
                                             :external-format external-format
                                             :null-terminate nil))
                   ((simple-array (unsigned-byte 8))
                    buffer)
                   ((array (unsigned-byte 8))
                    (make-array (length buffer)
                                :element-type '(unsigned-byte 8)
                                :initial-contents buffer))))
         (len (with-vector-sap (buffer-sap buffer)
                (unless length
                  (setf length (length buffer)))
                (if address
                    (with-sockaddr-for (socket sockaddr address)
                      (sb-alien:with-alien ((sa-len sockint::socklen-t
                                                    (size-of-sockaddr socket)))
                        (sockint::sendto (socket-file-descriptor socket)
                                         buffer-sap
                                         length
                                         flags
                                         sockaddr
                                         sa-len)))
                    (sockint::send (socket-file-descriptor socket)
                                   buffer-sap
                                   length
                                   flags)))))
    (cond
      ((and (= len -1)
            (member (sb-unix::get-errno)
                    (list sockint::EAGAIN sockint::EINTR)))
       nil)
      ((= len -1)
       (socket-error "sendto"))
      (t len))))

(defgeneric socket-listen (socket backlog)
  (:documentation "Mark SOCKET as willing to accept incoming connections.  BACKLOG
defines the maximum length that the queue of pending connections may
grow to before new connection attempts are refused.  See also listen(2)"))

(defmethod socket-listen ((socket socket) backlog)
  (let ((r (sockint::listen (socket-file-descriptor socket) backlog)))
    (if (= r -1)
        (socket-error "listen"))))

(defgeneric socket-open-p (socket)
  (:documentation "Return true if SOCKET is open; otherwise, return false.")
  (:method ((socket t)) (error 'type-error
                               :datum socket :expected-type 'socket)))

(defmethod socket-open-p ((socket socket))
  (if (slot-boundp socket 'stream)
      (open-stream-p (slot-value socket 'stream))
      (/= -1 (socket-file-descriptor socket))))

(defgeneric socket-close (socket &key abort)
  (:documentation
   "Close SOCKET, unless it was already closed.

If SOCKET-MAKE-STREAM has been called, calls CLOSE using ABORT on that stream.
Otherwise closes the socket file descriptor using close(2)."))

(defmethod socket-close ((socket socket) &key abort)
  ;; the close(2) manual page has all kinds of warning about not
  ;; checking the return value of close, on the grounds that an
  ;; earlier write(2) might have returned successfully w/o actually
  ;; writing the stuff to disk.  It then goes on to define the only
  ;; possible error return as EBADF (fd isn't a valid open file
  ;; descriptor).  Presumably this is an oversight and we could also
  ;; get anything that write(2) would have given us.

  ;; note that if you have a socket _and_ a stream on the same fd,
  ;; the socket will avoid doing anything to close the fd in case
  ;; the stream has done it already - if so, it may have been
  ;; reassigned to some other file, and closing it would be bad
  (let ((fd (socket-file-descriptor socket)))
    (flet ((drop-it (&optional streamp)
             (setf (slot-value socket 'file-descriptor) -1)
             (if streamp
                 (slot-makunbound socket 'stream)
                 (sb-ext:cancel-finalization socket))
             t))
      (cond ((eql fd -1)
             ;; already closed
             nil)
           ((slot-boundp socket 'stream)
            (close (slot-value socket 'stream) :abort abort)
            ;; Don't do this if there was an error from CLOSE -- the stream is
            ;; still live.
            (drop-it t))
           (t
            (handler-case
                (when (minusp (sockint::close fd))
                  (socket-error "close"))
              (bad-file-descriptor-error ()
                (drop-it))
              (:no-error (r)
                (declare (ignore r))
                (drop-it))))))))

(defgeneric socket-make-stream (socket &key input output
                                       element-type external-format
                                       buffering
                                       timeout)
  (:documentation "Find or create a STREAM that can be used for IO on
SOCKET \(which must be connected\).  Specify whether the stream is for
INPUT, OUTPUT, or both \(it is an error to specify neither\).  ELEMENT-TYPE
and EXTERNAL-FORMAT are as per OPEN.  TIMEOUT specifies a read timeout
for the stream."))

(defmethod socket-make-stream ((socket socket)
                               &key input output
                               (element-type 'character)
                               (buffering :full)
                               (external-format :default)
                               timeout
                               auto-close
                               (serve-events t))
  "Default method for SOCKET objects.

An ELEMENT-TYPE of :DEFAULT will construct a bivalent stream, capable of both
binary and character IO. Acceptable values for BUFFERING are :FULL, :LINE
and :NONE. Streams will have no TIMEOUT by default. If AUTO-CLOSE is true, the
underlying OS socket is automatically closed after the stream and the socket
have been garbage collected. If SERVE-EVENTS is true, blocking IO on the
socket will dispatch to the recursive event loop -- the default is currently
true, but this liable to change.

The stream for SOCKET will be cached, and a second invocation of this method
will return the same stream. This may lead to oddities if this function is
invoked with inconsistent arguments \(e.g., one might request an input stream
and get an output stream in response\)."
  (let ((stream
         (and (slot-boundp socket 'stream) (slot-value socket 'stream))))
    (unless stream
      (setf stream (sb-sys:make-fd-stream
                    (socket-file-descriptor socket)
                    :name (format nil "socket~@[ ~A~]~@[, peer: ~A~]"
                                  (socket-namestring socket)
                                  (socket-peerstring socket))
                    :dual-channel-p t
                    :input input
                    :output output
                    :element-type element-type
                    :buffering buffering
                    :external-format external-format
                    :timeout timeout
                    :auto-close auto-close
                    :serve-events serve-events))
      (setf (slot-value socket 'stream) stream))
    (sb-ext:cancel-finalization socket)
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
                       #+sbcl (sb-int:strerror num)))))
  (:documentation "Common base class of socket related conditions."))

;;; watch out for slightly hacky symbol punning: we use both the value
;;; and the symbol-name of sockint::efoo

(defmacro define-socket-condition (symbol name)
  `(progn
     (define-condition ,name (socket-error)
       ((symbol :reader socket-error-symbol :initform (quote ,symbol))))
     (export ',name)
     (push (cons ,symbol (quote ,name)) *conditions-for-errno*)))

(defparameter *conditions-for-errno* nil)
;;; this needs the rest of the list adding to it, really.  They also
;;; need symbols to be added to constants.ccon
;;; I haven't yet thought of a non-kludgey way of keeping all this in
;;; the same place
(define-socket-condition sockint::EADDRINUSE address-in-use-error)
(define-socket-condition sockint::EAGAIN interrupted-error)
(define-socket-condition sockint::EBADF bad-file-descriptor-error)
(define-socket-condition sockint::ECONNREFUSED connection-refused-error)
(define-socket-condition sockint::ETIMEDOUT operation-timeout-error)
(define-socket-condition sockint::EINTR interrupted-error)
(define-socket-condition sockint::EINVAL invalid-argument-error)
(define-socket-condition sockint::ENOBUFS no-buffers-error)
(define-socket-condition sockint::ENOMEM out-of-memory-error)
(define-socket-condition sockint::EOPNOTSUPP operation-not-supported-error)
(define-socket-condition sockint::EPERM operation-not-permitted-error)
(define-socket-condition sockint::EPROTONOSUPPORT protocol-not-supported-error)
(define-socket-condition sockint::ESOCKTNOSUPPORT socket-type-not-supported-error)
(define-socket-condition sockint::ENETUNREACH network-unreachable-error)
(define-socket-condition sockint::ENOTCONN not-connected-error)

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
  ;; FIXME: Our Texinfo documentation extracter need at least his to spit
  ;; out the signature. Real documentation would be better...
  ""
  (let* ((errno  (sb-unix::get-errno))
         (condition (condition-for-errno errno)))
    (error condition :errno errno  :syscall where)))


(defgeneric bits-of-sockaddr (socket sockaddr)
  (:documentation "Return protocol-dependent bits of parameter
SOCKADDR, e.g. the Host/Port if SOCKET is an inet socket."))

(defgeneric size-of-sockaddr (socket)
  (:documentation "Return the size of a sockaddr object for SOCKET."))
