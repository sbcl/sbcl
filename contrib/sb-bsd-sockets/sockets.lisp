(in-package :sb-bsd-sockets)

;;;; Methods, classes, functions for sockets.  Protocol-specific stuff
;;;; is deferred to inet.lisp, unix.lisp, etc

(eval-when (:load-toplevel :compile-toplevel :execute)

;;; Winsock is different w.r.t errno
(defun socket-errno ()
  "Get socket error code, usually from errno, but see #+win32."
  #+win32 (sockint::wsa-get-last-error)
  #-win32 (sb-unix::get-errno))

(defclass socket ()
  ((file-descriptor :initarg :descriptor
                    :reader socket-file-descriptor)
   (family :initform (error "No socket family") ; subclasses supply initforms
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
   #+win32
   (non-blocking-p :type (member t nil) :initform nil)
   (stream))
  (:default-initargs
   :type (sb-int:missing-arg))
  (:documentation "Common superclass of all sockets, not meant to be
directly instantiated.")))

(defmethod print-object ((object socket) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~@[~A, ~]~@[peer: ~A, ~]fd: ~A"
            (socket-namestring object)
            (socket-peerstring object)
            (slot-value object 'file-descriptor))))

(defmethod shared-initialize :after ((socket socket) slot-names
                                     &key protocol type
                                     &allow-other-keys)
  (let* ((proto-num
          (cond ((and protocol (keywordp protocol))
                 (get-protocol-by-name protocol))
                (protocol protocol)
                (t 0)))
         (fd (or (and (slot-boundp socket 'file-descriptor)
                      (socket-file-descriptor socket))
                 (sockint::socket (socket-family socket)
                                  (ecase type
                                    ((:datagram) sockint::sock-dgram)
                                    ((:stream) sockint::sock-stream))
                                  proto-num))))
    (socket-error-case ("socket" fd)
        (progn
          (setf (slot-value socket 'file-descriptor) fd
                (slot-value socket 'protocol) proto-num
                (slot-value socket 'type) type)
          (sb-ext:finalize socket (lambda () (sockint::close fd))
                           :dont-save t)))))



(defun call-with-socket-addr (socket sockaddr-args thunk)
  (multiple-value-bind (sockaddr size)
      (apply #'make-sockaddr-for socket nil sockaddr-args)
    (unless size
      (setf size (size-of-sockaddr socket)))
    (unwind-protect (funcall thunk sockaddr size)
      (free-sockaddr-for socket sockaddr))))

(defmacro with-socket-addr ((sockaddr-var size-of-sockaddr-var
                            &optional sockaddr-args)
                            socket &body body)
  `(sb-int:dx-flet ((with-socket-addr-thunk (,sockaddr-var ,size-of-sockaddr-var)
                      ,@body))
     (call-with-socket-addr ,socket ,sockaddr-args #'with-socket-addr-thunk)))

(defmacro with-socket-fd-and-addr ((fd-var sockaddr-var size-of-sockaddr-var
                                   &optional sockaddr-args)
                                   socket &body body)
  (sb-int:once-only ((socket socket))
    `(let ((,fd-var (socket-file-descriptor ,socket)))
       (with-socket-addr (,sockaddr-var ,size-of-sockaddr-var ,sockaddr-args)
           ,socket
         ,@body))))


;; we deliberately redesign the "bind" interface: instead of passing a
;; sockaddr_something as second arg, we pass the elements of one as
;; multiple arguments.
(defmethod socket-bind ((socket socket) &rest address)
  (with-socket-fd-and-addr (fd sockaddr size address) socket
    (socket-error-case ("bind" (sockint::bind fd sockaddr size)))))



(defmethod socket-accept ((socket socket))
  (with-socket-fd-and-addr (fd sockaddr size) socket
    (socket-error-case ("accept" (sockint::accept fd sockaddr size) new-fd)
        (multiple-value-call #'values
          (let ((socket (make-instance (class-of socket)
                                       :type (socket-type socket)
                                       :protocol (socket-protocol socket)
                                       :descriptor new-fd)))
            (sb-ext:finalize socket (lambda () (sockint::close new-fd))
                             :dont-save t))
          (bits-of-sockaddr socket sockaddr))
      (:interrupted nil))))

(defmethod socket-connect ((socket socket) &rest peer)
  (with-socket-fd-and-addr (fd sockaddr size peer) socket
    (socket-error-case ("connect" (sockint::connect fd sockaddr size))
        socket)))

(defmethod socket-peername ((socket socket))
  (with-socket-fd-and-addr (fd sockaddr size) socket
    (socket-error-case ("getpeername"
                        (sockint::getpeername fd sockaddr size)
                        (result actual-size))
        (bits-of-sockaddr socket sockaddr actual-size))))

(defmethod socket-name ((socket socket))
  (with-socket-fd-and-addr (fd sockaddr size) socket
    (socket-error-case ("getsockname"
                        (sockint::getsockname fd sockaddr size)
                       (result actual-size))
        (bits-of-sockaddr socket sockaddr actual-size))))

;;; There are a whole bunch of interesting things you can do with a
;;; socket that don't really map onto "do stream io", especially in
;;; CL which has no portable concept of a "short read".  socket-receive
;;; allows us to read from an unconnected socket into a buffer, and
;;; to learn who the sender of the packet was

(defmethod socket-receive ((socket socket) buffer length
                           &key
                           oob peek waitall dontwait
                           (element-type 'character))
  (with-socket-fd-and-addr (fd sockaddr size) socket
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
             (sb-alien:with-alien ((sa-len sockint::socklen-t size))
               (socket-error-case ("recvfrom"
                                   (sockint::recvfrom fd copy-buffer length
                                                      flags sockaddr (sb-alien:addr sa-len))
                                   len)
                   (progn
                     (loop for i from 0 below (min len length)
                        do (setf (elt buffer i)
                                 (cond
                                   ((or (eql element-type 'character) (eql element-type 'base-char))
                                    (code-char (sb-alien:deref (sb-alien:deref copy-buffer) i)))
                                   (t (sb-alien:deref (sb-alien:deref copy-buffer) i)))))
                     (multiple-value-call #'values buffer len
                       (bits-of-sockaddr socket sockaddr)))
                 (:interrupted nil)))
          (sb-alien:free-alien copy-buffer))))))

(defmacro with-vector-sap ((name vector) &body body)
  `(sb-sys:with-pinned-objects (,vector)
     (let ((,name (sb-sys:vector-sap ,vector)))
       ,@body)))

(defmethod socket-send ((socket socket) buffer length
                        &key
                        address
                        (external-format :default)
                        oob eor dontroute dontwait nosignal
                        #+linux confirm #+linux more)
  (declare (ignorable nosignal))
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
                    (with-socket-fd-and-addr (fd sockaddr size address) socket
                      (sb-alien:with-alien ((sa-len sockint::socklen-t size))
                        (sockint::sendto fd buffer-sap length
                                         flags sockaddr sa-len)))
                    (sockint::send (socket-file-descriptor socket)
                                   buffer-sap length flags)))))
    (socket-error-case ("sendto" len)
        len
      (:interrupted nil))))

(defmethod socket-listen ((socket socket) backlog)
  (socket-error-case
      ("listen" (sockint::listen (socket-file-descriptor socket) backlog))))

(defmethod socket-open-p ((socket socket))
  (if (slot-boundp socket 'stream)
      (open-stream-p (slot-value socket 'stream))
      (/= -1 (socket-file-descriptor socket))))

(defmethod socket-shutdown ((socket socket) &key direction)
  (let* ((fd  (socket-file-descriptor socket))
         (how (ecase direction
                (:input sockint::SHUT_RD)
                (:output sockint::SHUT_WR)
                (:io sockint::SHUT_RDWR))))
    (socket-error-case ("shutdown" (sockint::shutdown fd how)
                                   result (minusp result)))))

(defmethod socket-make-stream ((socket socket)
                               &key input output
                               (element-type 'character)
                               (buffering :full)
                               (external-format :default)
                               timeout
                               auto-close
                               serve-events)
  "Default method for SOCKET objects.

ELEMENT-TYPE defaults to CHARACTER, to construct a bivalent stream,
capable of both binary and character IO use :DEFAULT.

Acceptable values for BUFFERING are :FULL, :LINE and :NONE, default
is :FULL, ie. output is buffered till it is explicitly flushed using
CLOSE or FINISH-OUTPUT. (FORCE-OUTPUT forces some output to be
flushed: to ensure all buffered output is flused use FINISH-OUTPUT.)

Streams have no TIMEOUT by default. If one is provided, it is the
number of seconds the system will at most wait for input to appear on
the socket stream when trying to read from it.

If AUTO-CLOSE is true, the underlying OS socket is automatically
closed after the stream and the socket have been garbage collected.
Default is false.

If SERVE-EVENTS is true, blocking IO on the socket will dispatch to
the recursive event loop. Default is false.

The stream for SOCKET will be cached, and a second invocation of this
method will return the same stream. This may lead to oddities if this
function is invoked with inconsistent arguments \(e.g., one might
request an input stream and get an output stream in response\)."
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
                    :serve-events (and serve-events #+win32 nil)))
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
                       #+win32 (sb-win32:format-system-message num)
                       #-win32 (sb-int:strerror num)))))
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
(define-socket-condition sockint::EAFNOSUPPORT address-family-not-supported)
(define-socket-condition sockint::EINPROGRESS operation-in-progress)

(defun condition-for-errno (err)
  (or (cdr (assoc err *conditions-for-errno* :test #'eql)) 'socket-error))

(defun socket-error (where &optional (errno (socket-errno)))
  "Signal an appropriate error for syscall WHERE and ERRNO.

WHERE should be a string naming the failed function.

When supplied, ERRNO should be the UNIX error number associated to the
failed call. The default behavior is to use the current value of the
errno variable."
  (error (condition-for-errno errno) :errno errno :syscall where))

;;; This wants to refer to the BAD-FILE-DESCRIPTOR-ERROR condition class.
;;; :BLOCK-COMPILE would have handled it correctly, but I don't know how
;;; to pass that flag through our contrib-building steps.
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
                (socket-error-case ("close" (sockint::close fd)
                                            result (minusp result)))
              (bad-file-descriptor-error ()
                (drop-it))
              (:no-error (r)
                (declare (ignore r))
                (drop-it))))))))
