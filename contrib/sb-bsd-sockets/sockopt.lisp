(in-package :sb-bsd-sockets)

#|
getsockopt(socket, level, int optname, void *optval, socklen_t *optlen)
setsockopt(socket, level, int optname, void *optval, socklen_t optlen)
             ^ SOL_SOCKET or a protocol number

In terms of providing a useful interface, we have to face up to the
fact that most of these take different data types - some are integers,
some are booleans, some are foreign struct instances, etc etc

 (define-socket-option lisp-name doc level number mangle-arg size mangle-return)

macro-expands to two functions that define lisp-name and (setf ,lisp-name)
and calls the functions mangle-arg and mangle-return on outgoing and incoming
data resp.

Parameters passed to the function thus defined (lisp-name)
are all passed directly into mangle-arg.  mangle-arg should return an
alien pointer  - this is passed unscathed to the foreign routine, so
wants to have type (* t).  Note that even for options that have
integer arguments, this is still a pointer to said integer.

size is the size of the buffer that the return of mangle-arg points
to, and also of the buffer that we should allocate for getsockopt
to write into.

mangle-return is called with an alien buffer and should turn it into
something that the caller will want.

Code for options that not every system has should be conditionalised:

 (if (boundp 'sockint::IP_RECVIF)
     (define-socket-option so-receive-interface nil (getprotobyname "ip")
       sockint::IP_RECVIF  ...  ))
|#

(defun unsupported-socket-option (name)
  (error 'unsupported-operator
         :format-control "Socket option ~S is not supported in this platform."
         :format-arguments (list name)))

(defmacro define-socket-option
    (lisp-name documentation
     level number buffer-type mangle-arg mangle-return mangle-setf-buffer
     &optional features info)
  (let ((find-level
         (if (numberp (eval level))
             level
             `(get-protocol-by-name ,(string-downcase (symbol-name level)))))
        (supportedp (or (null features) (sb-int:featurep features))))
    `(progn
       (export ',lisp-name)
       (defun ,lisp-name (socket)
         ,@(when documentation (list (concatenate 'string documentation " " info)))
         ,@(if supportedp
               `((sb-alien:with-alien ((size sb-alien:int)
                                       (buffer ,buffer-type))
                   (setf size (sb-alien:alien-size ,buffer-type :bytes))
                   (socket-error-case
                       ("getsockopt"
                        (sockint::getsockopt (socket-file-descriptor socket)
                                             ,find-level ,number
                                             (sb-alien:addr buffer)
                                             (sb-alien:addr size)))
                       (,mangle-return buffer size))))
               `((declare (ignore socket))
                 (unsupported-socket-option ',lisp-name))))
       (defun (setf ,lisp-name) (new-value socket)
         ,@(if supportedp
               `((sb-alien:with-alien ((buffer ,buffer-type))
                   (setf buffer ,(if mangle-arg
                                     `(,mangle-arg new-value)
                                     `new-value))
                   (socket-error-case
                       ("setsockopt"
                        (sockint::setsockopt
                         (socket-file-descriptor socket)
                         ,find-level ,number
                         (,mangle-setf-buffer buffer)
                         ,(if (eql buffer-type 'sb-alien:c-string)
                              `(length new-value)
                              `(sb-alien:alien-size ,buffer-type :bytes))))))
                 new-value)
               `((declare (ignore new-value socket))
                 (unsupported-socket-option ',lisp-name)))))))

;;; sockopts that have integer arguments

(defun foreign-int-to-integer (buffer size)
  (assert (= size (sb-alien:alien-size sb-alien:int :bytes)))
  buffer)

(defmacro define-socket-option-int (name level number &optional features (info ""))
  `(define-socket-option ,name nil ,level ,number
     sb-alien:int nil foreign-int-to-integer sb-alien:addr ,features ,info))

(define-socket-option-int
  sockopt-receive-low-water sockint::sol-socket sockint::so-rcvlowat)
(define-socket-option-int
  sockopt-send-low-water sockint::sol-socket sockint::so-sndlowat)
(define-socket-option-int sockopt-type sockint::sol-socket sockint::so-type)
(define-socket-option-int sockopt-error sockint::sol-socket sockint::so-error)

(define-socket-option-int
  sockopt-send-buffer sockint::sol-socket sockint::so-sndbuf)
(define-socket-option-int
  sockopt-receive-buffer sockint::sol-socket sockint::so-rcvbuf)
(define-socket-option-int
  sockopt-priority sockint::sol-socket sockint::so-priority :linux
  "Available only on Linux.")

(define-socket-option-int
  sockopt-tcp-keepcnt :tcp sockint::tcp-keepcnt (or :linux (and :bsd (not :openbsd)))
  "Available only on Linux, BSD (except OpenBSD).")
(define-socket-option-int
  sockopt-tcp-keepidle :tcp sockint::tcp-keepidle (or :linux (and :bsd (not (or :openbsd :darwin))))
  "Available only on Linux, BSD (except OpenBSD, Darwin).")
(define-socket-option-int
  sockopt-tcp-keepintvl :tcp sockint::tcp-keepintvl (or :linux (and :bsd (not :openbsd)))
  "Available only on Linux, BSD (except OpenBSD).")
(define-socket-option-int
  sockopt-tcp-user-timeout :tcp sockint::tcp-user-timeout :linux
  "Available only on Linux.")

;;; boolean options are integers really

(defun foreign-int-to-bool (x size)
  (if (zerop (foreign-int-to-integer x size))
      nil
      t))

(defun bool-to-foreign-int (val)
  (if val 1 0))

(defmacro define-socket-option-bool (name level c-name &optional features (info ""))
  `(define-socket-option ,name
    ,(format nil "~@<Return the value of the ~A socket option for SOCKET. ~
                 This can also be updated with SETF.~:@>"
             (symbol-name c-name))
    ,level ,c-name
    sb-alien:int bool-to-foreign-int foreign-int-to-bool sb-alien:addr
    ,features ,info))

(define-socket-option-bool
  sockopt-reuse-address sockint::sol-socket sockint::so-reuseaddr)
(define-socket-option-bool
  sockopt-keep-alive sockint::sol-socket sockint::so-keepalive)
(define-socket-option-bool
  sockopt-oob-inline sockint::sol-socket sockint::so-oobinline)
(define-socket-option-bool
  sockopt-bsd-compatible sockint::sol-socket sockint::so-bsdcompat :linux
  "Available only on Linux.")
(define-socket-option-bool
  sockopt-pass-credentials sockint::sol-socket sockint::so-passcred :linux
  "Available only on Linux.")
(define-socket-option-bool
  sockopt-debug sockint::sol-socket sockint::so-debug)
(define-socket-option-bool
  sockopt-dont-route sockint::sol-socket sockint::so-dontroute)
(define-socket-option-bool
  sockopt-broadcast sockint::sol-socket sockint::so-broadcast)

(define-socket-option-bool sockopt-tcp-nodelay :tcp sockint::tcp-nodelay)

(defun identity-1 (x &rest args)
  (declare (ignore args))
  x)

(macrolet ((cast-to-pointer (local-alien)
             `(sb-alien:deref (sb-alien:cast (sb-alien:addr ,local-alien) (* (* t))) 0)))

  (define-socket-option sockopt-bind-to-device nil sockint::sol-socket
    sockint::so-bindtodevice sb-alien:c-string nil identity-1 cast-to-pointer
    :linux "Available only on Linux"))

;;; other kinds of socket option

;;; so_peercred takes a ucre structure
;;; so_linger struct linger {
;                  int   l_onoff;    /* linger active */
;                  int   l_linger;   /* how many seconds to linger for */
;              };

#|

(sockopt-reuse-address 2)

(defun echo-server ()
  (let ((s (make-inet-socket :stream (get-protocol-by-name "tcp"))))
    (setf (sockopt-reuse-address s) t)
    (setf (sockopt-bind-to-device s) "lo")
    (socket-bind s (make-inet-address "127.0.0.1") 3459)
    (socket-listen s 5)
    (dotimes (i 10)
      (let* ((s1 (socket-accept s))
             (stream (socket-make-stream s1 :input t :output t :buffering :none)))
        (let ((line (read-line stream)))
          (format t "got one ~A ~%" line)
          (format stream "~A~%" line))
        (close stream)))))

NIL
|#
