(in-package :sb-bsd-sockets)

#||
<H2> Socket Options </h2>
<a name="sockopt"> </a>
<p> A subset of socket options are supported, using a fairly
general framework which should make it simple to add more as required 
- see sockopt.lisp for details.  The name mapping from C is fairly
straightforward: <tt>SO_RCVLOWAT</tt> becomes
<tt>sockopt-receive-low-water</tt> and <tt>(setf
sockopt-receive-low-water)</tt>.
||#

#|
getsockopt(socket, level, int optname, void *optval, socklen_t *optlen)
setsockopt(socket, level, int optname, void *optval, socklen_t optlen)
             ^ SOL_SOCKET or a protocol number

In terms of providing a useful interface, we have to face up to the
fact that most of these take different data types - some are integers,
some are booleans, some are foreign struct instances, etc etc

(define-socket-option lisp-name level number mangle-arg size mangle-return)

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
    (define-socket-option so-receive-interface (getprotobyname "ip")
      sockint::IP_RECVIF  ...  ))


|#

(defmacro define-socket-option
  (lisp-name level number mangle-arg size mangle-return)
  (let ((find-level
	 (if (numberp (eval level))
	     level
	     `(get-protocol-by-name ,(string-downcase (symbol-name level))))))
    `(progn
      (export ',lisp-name)
      (defun ,lisp-name (socket &aux (fd (socket-file-descriptor socket)))
	(let ((buf (make-array sockint::size-of-int
			       :element-type '(unsigned-byte 8)
			       :initial-element 0)))
	  (sb-sys:with-pinned-objects (buf)
	    (if (= -1 (sockint::getsockopt
		       fd ,find-level ,number (sb-grovel::array-data-address buf) ,size))
		(socket-error "getsockopt")
		(,mangle-return buf ,size)))))
      (defun (setf ,lisp-name) (new-val socket
				&aux (fd (socket-file-descriptor socket)))
	(if (= -1
	       (sb-sys:without-gcing
		(sockint::setsockopt
		 fd ,find-level ,number (funcall (function ,mangle-arg) new-val ,size)
		 ,size)))
	    (socket-error "setsockopt"))))))

;;; sockopts that have integer arguments

(defun int-to-foreign (x size)
  ;; can't use with-alien, as the variables it creates only have
  ;; dynamic scope.  can't use the passed-in size because sap-alien
  ;; is a macro and evaluates its second arg at read time
  (let* ((v (make-array size :element-type '(unsigned-byte 8)
			:initial-element 0))
	 (d (sb-grovel::array-data-address v))
	 (alien (sb-alien:sap-alien
		 d; (sb-sys:int-sap d)
		 (* (sb-alien:signed #.(* 8 sockint::size-of-int))))))
    (setf (sb-alien:deref alien 0) x)
    alien))

(defun buffer-to-int (x size)
  (declare (ignore size))
  (let ((alien (sb-alien:sap-alien
		(sb-grovel::array-data-address x)
		(* (sb-alien:signed #.(* 8 sockint::size-of-int))))))
    (sb-alien:deref alien)))

(defmacro define-socket-option-int (name level number)
  `(define-socket-option ,name ,level ,number
     int-to-foreign sockint::size-of-int buffer-to-int))

(define-socket-option-int
  sockopt-receive-low-water sockint::sol-socket sockint::so-rcvlowat)
(define-socket-option-int
  sockopt-send-low-water sockint::sol-socket sockint::so-sndlowat)
(define-socket-option-int
  sockopt-type sockint::sol-socket sockint::so-type)
(define-socket-option-int
  sockopt-send-buffer sockint::sol-socket sockint::so-sndbuf)
(define-socket-option-int
  sockopt-receive-buffer sockint::sol-socket sockint::so-rcvbuf)
(define-socket-option-int
  sockopt-priority sockint::sol-socket sockint::so-priority)

;;; boolean options are integers really

(defun bool-to-foreign (x size)
  (int-to-foreign (if x 1 0) size))

(defun buffer-to-bool (x size)
  (not (= (buffer-to-int x size) 0)))

(defmacro define-socket-option-bool (name level number)
  `(define-socket-option ,name ,level ,number
     bool-to-foreign sockint::size-of-int buffer-to-bool))

(define-socket-option-bool
  sockopt-reuse-address sockint::sol-socket sockint::so-reuseaddr)
(define-socket-option-bool
  sockopt-keep-alive sockint::sol-socket sockint::so-keepalive)
(define-socket-option-bool
  sockopt-oob-inline sockint::sol-socket sockint::so-oobinline)
(define-socket-option-bool
  sockopt-bsd-compatible sockint::sol-socket sockint::so-bsdcompat)
(define-socket-option-bool
  sockopt-pass-credentials sockint::sol-socket sockint::so-passcred)
(define-socket-option-bool
  sockopt-debug sockint::sol-socket sockint::so-debug)
(define-socket-option-bool
  sockopt-dont-route sockint::sol-socket sockint::so-dontroute)
(define-socket-option-bool
  sockopt-broadcast sockint::sol-socket sockint::so-broadcast)

(define-socket-option-bool sockopt-tcp-nodelay :tcp sockint::tcp-nodelay)

(defun string-to-foreign (string size)
  (declare (ignore size))
  (let ((data (sb-grovel::array-data-address string)))
    (sb-alien:sap-alien data (* t))))
                                                         
(defun buffer-to-string (x size)
  (declare (ignore size))
  (sb-c-call::%naturalize-c-string
   (sb-grovel::array-data-address x)))

(define-socket-option sockopt-bind-to-device sockint::sol-socket
  sockint::so-bindtodevice string-to-foreign sockint::ifnamsiz
  buffer-to-string)

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

