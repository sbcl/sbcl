(in-package :sb-bsd-sockets)
#|| <a name="name-service"><h2>Name Service</h2></a>

<p>Presently name service is implemented by calling whatever
gethostbyname(2) uses.  This may be any or all of /etc/hosts, NIS, DNS,
or something completely different.  Typically it's controlled by
/etc/nsswitch.conf

<p> Direct links to the asynchronous resolver(3) routines would be nice to have
eventually, so that we can do DNS lookups in parallel with other things
|#

(defclass host-ent ()
  ((name :initarg :name :accessor host-ent-name)
   (aliases :initarg :aliases :accessor host-ent-aliases)
   (address-type :initarg :type :accessor host-ent-address-type)
					; presently always AF_INET
   (addresses :initarg :addresses :accessor host-ent-addresses)))

(defgeneric host-ent-address (host-ent))

(defmethod host-ent-address ((host-ent host-ent))
  (car (host-ent-addresses host-ent)))

;(define-condition host-not-found-error (socket-error)) ; host unknown
;(define-condition no-address-error (socket-error)) ; valid name but no IP address
;(define-condition no-recovery-error (socket-error)) ; name server error
;(define-condition try-again-error (socket-error)) ; temporary

(defun make-host-ent (h)
  (if (sb-grovel::foreign-nullp h) (name-service-error "gethostbyname"))
  (let* ((length (sockint::hostent-length h))
	 (aliases (loop for i = 0 then (1+ i)
			for al = (sb-alien:deref (sockint::hostent-aliases h) i)
			while al
			collect al))
	 (addresses 
	  (loop for i = 0 then (1+ i)
		for ad = (sb-alien:deref (sockint::hostent-addresses h) i)
		until (sb-alien:null-alien ad)
		collect (ecase (sockint::hostent-type h)
			  (#.sockint::af-inet
			     (assert (= length 4))
			     (let ((addr (make-array 4 :element-type '(unsigned-byte 8))))
			       (loop for i from 0 below length
				     do (setf (elt addr i) (sb-alien:deref ad i)))
			       addr))
			  (#.sockint::af-local
			   (sb-alien:cast ad sb-alien:c-string))))))
    (make-instance 'host-ent
                   :name (sockint::hostent-name h)
		   :type (sockint::hostent-type h)
                   :aliases aliases
                   :addresses addresses)))

(defun get-host-by-name (host-name)
  "Returns a HOST-ENT instance for HOST-NAME or throws some kind of condition.
HOST-NAME may also be an IP address in dotted quad notation or some other
weird stuff - see gethostbyname(3) for grisly details."
  (make-host-ent (sockint::gethostbyname host-name)))

(defun get-host-by-address (address)
  "Returns a HOST-ENT instance for ADDRESS, which should be a vector of
 (integer 0 255), or throws some kind of error.  See gethostbyaddr(3) for
grisly details."
  (sockint::with-in-addr packed-addr ()
    (let ((addr-vector (coerce address 'vector)))
      (loop for i from 0 below (length addr-vector)
	    do (setf (sb-alien:deref (sockint::in-addr-addr packed-addr) i)
		     (elt addr-vector i)))
      (make-host-ent (sockint::gethostbyaddr packed-addr
					     4
					     sockint::af-inet)))))

;;; The remainder is my fault - gw

(defvar *name-service-errno* 0
  "The value of h_errno, after it's been fetched from Unix-land by calling
GET-NAME-SERVICE-ERRNO")

(defun name-service-error (where)
  (get-name-service-errno)
  ;; Comment next to NETDB_INTERNAL in netdb.h says "See errno.".
  ;; This special case treatment hasn't actually been tested yet.
  (if (= *name-service-errno* sockint::NETDB-INTERNAL)
      (socket-error where)
    (let ((condition
	   (condition-for-name-service-errno *name-service-errno*)))
      (error condition :errno *name-service-errno* :syscall where))))

(define-condition name-service-error (condition)
  ((errno :initform nil
	  :initarg :errno
	  :reader name-service-error-errno)
   (symbol :initform nil :initarg :symbol :reader name-service-error-symbol)
   (syscall :initform "an unknown location" :initarg :syscall :reader name-service-error-syscall))
  (:report (lambda (c s)
	     (let ((num (name-service-error-errno c)))
	       (format s "Name service error in \"~A\": ~A (~A)"
		       (name-service-error-syscall c)
		       (or (name-service-error-symbol c)
			   (name-service-error-errno c))
		       (get-name-service-error-message num))))))

(defmacro define-name-service-condition (symbol name)
  `(progn
     (define-condition ,name (name-service-error)
       ((symbol :reader name-service-error-symbol :initform (quote ,symbol))))
     (push (cons ,symbol (quote ,name)) *conditions-for-name-service-errno*)))

(defparameter *conditions-for-name-service-errno* nil)

(define-name-service-condition sockint::NETDB-INTERNAL netdb-internal-error)
(define-name-service-condition sockint::NETDB-SUCCESS netdb-success-error)
(define-name-service-condition sockint::HOST-NOT-FOUND host-not-found-error)
(define-name-service-condition sockint::TRY-AGAIN try-again-error)
(define-name-service-condition sockint::NO-RECOVERY no-recovery-error)
;; this is the same as the next one
;;(define-name-service-condition sockint::NO-DATA no-data-error)
(define-name-service-condition sockint::NO-ADDRESS no-address-error)

(defun condition-for-name-service-errno (err)
  (or (cdr (assoc err *conditions-for-name-service-errno* :test #'eql))
      'name-service))



(defun get-name-service-errno ()
  (setf *name-service-errno*
	(sb-alien:alien-funcall
	 (sb-alien:extern-alien "get_h_errno" (function integer)))))

#-(and cmu solaris)
(progn
  #+sbcl
  (sb-alien:define-alien-routine "hstrerror"
      sb-c-call:c-string
    (errno integer))
  #+cmu
  (alien:def-alien-routine "hstrerror"
      sb-c-call:c-string
    (errno integer))
  (defun get-name-service-error-message (num)
  (hstrerror num))
)
