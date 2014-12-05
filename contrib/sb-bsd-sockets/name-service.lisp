(in-package :sb-bsd-sockets)

(defclass host-ent ()
  ((name :initarg :name :reader host-ent-name
         :documentation "The name of the host")
   ;; Deliberately not documented, since this isn't very useful,
   ;; and the data isn't available when using getaddrinfo(). Unfortunately
   ;; it is exported.
   (aliases :initarg :aliases :reader host-ent-aliases)
   (address-type :initarg :type :reader host-ent-address-type)
   (addresses :initarg :addresses :reader host-ent-addresses
              :documentation "A list of addresses for this host."))
  (:documentation "This class represents the results of an address lookup."))

(defmethod host-ent-address ((host-ent host-ent))
  (car (host-ent-addresses host-ent)))

#-sb-bsd-sockets-addrinfo
(defun make-host-ent (h &optional errno)
  (when (sb-alien:null-alien h)
    (name-service-error "gethostbyname" errno))
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
                           ;; CLH: Work around x86-64 darwin bug here.
                           ;; The length is reported as 8, when it should be 4.
                           ;; FIXME: this is rumored to be fixed in 10.5
                           #+(and darwin x86-64)
                           (progn
                             (assert (or (= length 4) (= length 8)))
                             (naturalize-unsigned-byte-8-array ad 4))
                           #-(and darwin x86-64)
                           (progn
                             (assert (= length 4))
                             (naturalize-unsigned-byte-8-array ad length)))
                          #-win32
                          (#.sockint::af-local
                           (sb-alien:cast ad sb-alien:c-string))))))
    (make-instance 'host-ent
                   :name (sockint::hostent-name h)
                   :type (sockint::hostent-type h)
                   :aliases aliases
                   :addresses addresses)))

(declaim (inline naturalize-unsigned-byte-8-array)) ; FIXME: out-of-order
(defun naturalize-unsigned-byte-8-array (array length)
  (let ((addr (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (i length)
      (setf (elt addr i) (sb-alien:deref array i)))
    addr))

;;; Resolving

#-sb-bsd-sockets-addrinfo
(progn
  (sb-ext:defglobal **gethostby-lock**
      (sb-thread:make-mutex :name "gethostby lock"))

  (defun get-host-by-name (host-name)
    "Returns a HOST-ENT instance for HOST-NAME or signals a NAME-SERVICE-ERROR.
HOST-NAME may also be an IP address in dotted quad notation or some other
weird stuff - see gethostbyname(3) for the details."
    (sb-thread::with-system-mutex (**gethostby-lock** :allow-with-interrupts t)
      (make-host-ent (sockint::gethostbyname host-name))))

  (defun get-host-by-address (address)
    "Returns a HOST-ENT instance for ADDRESS, which should be a vector of
 (integer 0 255), or signals a NAME-SERVICE-ERROR. See gethostbyaddr(3)
 for details."
    (sb-thread::with-system-mutex (**gethostby-lock** :allow-with-interrupts t)
      (sockint::with-in-addr packed-addr ()
        (let ((addr-vector (coerce address 'vector)))
          (loop for i from 0 below (length addr-vector)
                do (setf (sb-alien:deref (sockint::in-addr-addr packed-addr) i)
                         (elt addr-vector i)))
          (make-host-ent (sockint::gethostbyaddr packed-addr
                                                 4
                                                 sockint::af-inet)))))))

#+sb-bsd-sockets-addrinfo
(defconstant ni-max-host 1025) ;; Not inside PROGN because of #.

#+sb-bsd-sockets-addrinfo
(progn
  (defun get-host-by-name (node)
    "Returns a HOST-ENT instance for NODE or signals a NAME-SERVICE-ERROR.

Another HOST-ENT instance containing zero, one or more IPv6 addresses
may be returned as a second return value.

NODE may also be an IP address in dotted quad notation or some other
weird stuff - see getaddrinfo(3) for the details."
    (declare (optimize speed))
    (sb-alien:with-alien ((info (* sockint::addrinfo)))
      (addrinfo-error-case ("getaddrinfo"
                            (sockint::getaddrinfo
                             node nil nil (sb-alien:addr info)))
          (let ((host-ent4 (make-instance 'host-ent
                                          :name node
                                          :type sockint::af-inet
                                          :aliases nil
                                          :addresses nil))
                (host-ent6 (make-instance 'host-ent
                                          :name node
                                          :type sockint::af-inet6
                                          :aliases nil
                                          :addresses nil)))
            ;; The same effective result can be multiple time
            ;; in the list, with different socktypes. Only record
            ;; each address once.
            (loop for info* = info then (sockint::addrinfo-next info*)
               until (sb-alien::null-alien info*) do
                 (cond
                   ((= (sockint::addrinfo-family info*) sockint::af-inet)
                    (let ((address (sockint::sockaddr-in-addr
                                    (sb-alien:cast
                                     (sockint::addrinfo-addr info*)
                                     (* (sb-alien:struct sockint::sockaddr-in))))))
                      (setf (slot-value host-ent4 'addresses)
                            (adjoin (naturalize-unsigned-byte-8-array address 4)
                                    (host-ent-addresses host-ent4)
                                    :test 'equalp))))
                   ((= (sockint::addrinfo-family info*) sockint::af-inet6)
                    (let ((address (sockint::sockaddr-in6-addr
                                    (sb-alien:cast
                                     (sockint::addrinfo-addr info*)
                                     (* (sb-alien:struct sockint::sockaddr-in6))))))
                      (setf (slot-value host-ent6 'addresses)
                            (adjoin (naturalize-unsigned-byte-8-array address 16)
                                    (host-ent-addresses host-ent6)
                                    :test 'equalp))))))
            (sockint::freeaddrinfo info)
            (values host-ent4 host-ent6)))))

  (defun get-host-by-address (address)
    "Returns a HOST-ENT instance for ADDRESS, which should be a vector of
\(integer 0 255) with 4 elements in case of an IPv4 address and 16
elements in case of an IPv6 address, or signals a NAME-SERVICE-ERROR.
See gethostbyaddr(3) for details."
    (declare (optimize speed)
             (vector address))
    (assert (member (length address) '(4 16) :test #'=))
    (multiple-value-bind (sockaddr sockaddr-free sockaddr-size address-family)
        (case (length address)
          (4
           (let ((sockaddr (sb-alien:make-alien sockint::sockaddr-in)))
             #+darwin (setf (sockint::sockaddr-in-len sockaddr) 16)
             (setf (sockint::sockaddr-in-family sockaddr) sockint::af-inet)
             (dotimes (i (length address))
               (setf (sb-alien:deref (sockint::sockaddr-in-addr sockaddr) i)
                     (aref address i)))
             (values sockaddr #'sockint::free-sockaddr-in
                     (sb-alien:alien-size sockint::sockaddr-in :bytes)
                     sockint::af-inet)))
          (16
           (let ((sockaddr (sb-alien:make-alien sockint::sockaddr-in6)))
             (setf (sockint::sockaddr-in6-family sockaddr) sockint::af-inet6)
             (dotimes (i (length address))
               (setf (sb-alien:deref (sockint::sockaddr-in6-addr sockaddr) i)
                     (aref address i)))
             (values sockaddr #'sockint::free-sockaddr-in6
                     (sb-alien:alien-size sockint::sockaddr-in6 :bytes)
                     sockint::af-inet6))))
      (unwind-protect
           (sb-alien:with-alien ((host-buf (array char #.ni-max-host)))
             (addrinfo-error-case ("getnameinfo"
                                   (sockint::getnameinfo
                                    (sb-alien:cast sockaddr (* t)) sockaddr-size
                                    (sb-alien:cast host-buf (* char)) ni-max-host
                                    nil 0
                                    sockint::ni-namereqd))
                 (make-instance 'host-ent
                                :name (sb-alien::c-string-to-string
                                       (sb-alien:alien-sap host-buf)
                                       (sb-impl::default-external-format)
                                       'character)
                                :type address-family
                                :aliases nil
                                :addresses (list address))))
        (funcall sockaddr-free sockaddr)))))

;;; Error handling

(defvar *name-service-errno* 0
  "The value of h_errno, after it's been fetched from Unix-land by calling
GET-NAME-SERVICE-ERRNO")

(defun name-service-error (where &optional errno)
  ;; There was a dummy docstring here for the texinfo extractor, but I
  ;; see no reason for this to be documented in the manual, and removed
  ;; it. -- JES
  (let ((*name-service-errno* (get-name-service-errno errno)))
    ;; Comment next to NETDB_INTERNAL in netdb.h says "See errno.".
    ;; This special case treatment hasn't actually been tested yet.
    (if (and #-win32 (= *name-service-errno* sockint::NETDB-INTERNAL))
        (socket-error where)
        (let ((condition
               (condition-for-name-service-errno *name-service-errno*)))
          (error condition :errno *name-service-errno* :syscall where)))))

(defun addrinfo-error (where error-code)
  (let ((condition (condition-for-name-service-error-code error-code)))
    (error condition :error-code error-code :syscall where)))

(define-condition name-service-error (error)
  ((errno :initform nil :initarg :errno :reader name-service-error-errno)
   (error-code :initform nil :initarg :error-code
               :reader name-service-error-error-code)
   (symbol :initform nil :initarg :symbol :reader name-service-error-symbol)
   (syscall :initform "an unknown location" :initarg :syscall :reader name-service-error-syscall))
  (:report (lambda (c s)
             (let* ((errno (name-service-error-errno c))
                    (error-code (name-service-error-error-code c)))
               (format s "Name service error in \"~A\": ~A (~A)"
                       (name-service-error-syscall c)
                       (or (name-service-error-symbol c)
                           errno
                           error-code)
                       (get-name-service-error-message errno error-code))))))

(defparameter *conditions-for-name-service-errno* nil)
;; getaddrinfo and getnameinfo return an error code, rather than using
;; h_errno.  While on Linux there's no overlap between their possible
;; values, this doesn't seem to be guaranteed on all systems.
(defparameter *conditions-for-name-service-error-code* nil)

;; Define a special name-service-error for variour error cases, and associate
;; them with the matching h_errno / error code.
(defmacro define-name-service-condition (errno-symbol error-code-symbol name)
  `(progn
     (define-condition ,name (name-service-error)
       ((errno-symbol :reader name-service-error-errno-symbol
                      :initform (quote ,errno-symbol))
        (error-code-symbol :reader name-service-error-error-code-symbol
                           :initform (quote ,error-code-symbol))))
     (push (cons ,errno-symbol (quote ,name))
           *conditions-for-name-service-errno*)
     #+sb-bsd-sockets-addrinfo
     (push (cons ,error-code-symbol (quote ,name))
           *conditions-for-name-service-error-code*)))

#-win32
(define-name-service-condition
    sockint::NETDB-INTERNAL
    nil ;; Doesn't map directly to any getaddrinfo error code
    netdb-internal-error)
#-win32
(define-name-service-condition
    sockint::NETDB-SUCCESS
    nil ;; Doesn't map directly to any getaddrinfo error code
    netdb-success-error)
(define-name-service-condition
    sockint::HOST-NOT-FOUND
    sockint::EAI-NONAME
    host-not-found-error)
(define-name-service-condition
    sockint::TRY-AGAIN
    sockint::EAI-AGAIN
    try-again-error)
(define-name-service-condition
    sockint::NO-RECOVERY
    sockint::EAI-FAIL
    no-recovery-error)
(define-name-service-condition
    ;; Also defined as NO-DATA, with the same value
    sockint::NO-ADDRESS
    ;; getaddrinfo() as of RFC 3493 can no longer distinguish between
    ;; host no found and address not found
    nil
    no-address-error)

(defun condition-for-name-service-errno (err)
  (or (cdr (assoc err *conditions-for-name-service-errno* :test #'eql))
      'name-service-error))

(defun condition-for-name-service-error-code (err)
  (or (cdr (assoc err *conditions-for-name-service-error-code* :test #'eql))
      'name-service-error))

(defun get-name-service-errno (&optional errno)
  (setf *name-service-errno*
        (or errno
            (sb-alien:alien-funcall
             #-win32
             (sb-alien:extern-alien "get_h_errno" (function integer))
             #+win32
             (sb-alien:extern-alien "WSAGetLastError" (function integer))))))

(defun get-name-service-error-message (errno error-code)
  #-win32
  (if errno
      (sockint::h-strerror errno)
      (sockint::gai-strerror error-code)))
