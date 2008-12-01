(in-package :sb-bsd-sockets)

(defclass host-ent ()
  ((name :initarg :name :reader host-ent-name
         :documentation "The name of the host")
   ;; Deliberately not documented, since this isn't very useful,
   ;; and the data isn't available when using getaddrinfo(). Unfortunately
   ;; it is exported.
   (aliases :initarg :aliases :reader host-ent-aliases)
   ;; presently always AF_INET. Not exported.
   (address-type :initarg :type :reader host-ent-address-type)
   (addresses :initarg :addresses :reader host-ent-addresses
              :documentation "A list of addresses for this host."))
  (:documentation "This class represents the results of an address lookup."))

(defgeneric host-ent-address (host-ent)
  (:documentation "Returns some valid address for HOST-ENT."))

(defmethod host-ent-address ((host-ent host-ent))
  (car (host-ent-addresses host-ent)))

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

(defun naturalize-unsigned-byte-8-array (array length)
  (let ((addr (make-array 4 :element-type '(unsigned-byte 8))))
    (dotimes (i length)
      (setf (elt addr i) (sb-alien:deref array i)))
    addr))

;;; Resolving

(defun get-host-by-name (host-name)
  "Returns a HOST-ENT instance for HOST-NAME or signals a NAME-SERVICE-ERROR.
HOST-NAME may also be an IP address in dotted quad notation or some other
weird stuff - see gethostbyname(3) or getaddrinfo(3) for the details."
  #+sb-bsd-sockets-addrinfo
  (get-address-info host-name)
  #-sb-bsd-sockets-addrinfo
  (make-host-ent (sockint::gethostbyname host-name)))

(defun get-host-by-address (address)
  "Returns a HOST-ENT instance for ADDRESS, which should be a vector of
 (integer 0 255), or signals a NAME-SERVICE-ERROR.  See gethostbyaddr(3)
 or gethostinfo(3) for details."
  #+sb-bsd-sockets-addrinfo
  (get-name-info address)
  #-sb-bsd-sockets-addrinfo
  (sockint::with-in-addr packed-addr ()
    (let ((addr-vector (coerce address 'vector)))
      (loop for i from 0 below (length addr-vector)
            do (setf (sb-alien:deref (sockint::in-addr-addr packed-addr) i)
                     (elt addr-vector i)))
      (make-host-ent (sockint::gethostbyaddr packed-addr
                                             4
                                             sockint::af-inet)))))

;;; Emulate the above two functions with getaddrinfo / getnameinfo

#+sb-bsd-sockets-addrinfo
(defun get-address-info (node)
  (sb-alien:with-alien ((res (* (* sockint::addrinfo)) :local
                             (sb-alien:make-alien (* sockint::addrinfo))))
    (let ((err (sockint::getaddrinfo node nil nil res)))
      (if (zerop err)
          (let ((host-ent (make-instance 'host-ent
                                         :name node
                                         :type sockint::af-inet
                                         :aliases nil
                                         :addresses nil)))
            (loop for sap = (sb-alien:deref res) then (sockint::addrinfo-next info)
                  until (sb-alien::null-alien sap)
                  for info = (sb-alien:cast sap (* sockint::addrinfo))
                  ;; Only handle AF_INET currently.
                  do (when (eq (sockint::addrinfo-family info) sockint::af-inet)
                       (let* ((sockaddr (sockint::addrinfo-addr info))
                              (address (sockint::sockaddr-in-addr sockaddr)))
                         ;; The same effective result can be multiple time
                         ;; in the list, with different socktypes. Only record
                         ;; each address once.
                         (setf (slot-value host-ent 'addresses)
                               (adjoin (naturalize-unsigned-byte-8-array address
                                                                         4)
                                       (host-ent-addresses host-ent)
                                       :test 'equalp)))))
            (sockint::free-addrinfo (sb-alien:deref res))
            host-ent)
          (addrinfo-error "getaddrinfo" err)))))

(defconstant ni-max-host 1025)

#+sb-bsd-sockets-addrinfo
(defun get-name-info (address)
  (assert (= (length address) 4))
  (sockint::with-sockaddr-in sockaddr ()
    (sb-alien:with-alien ((host-buf (array char #.ni-max-host)))
      #+darwin (setf (sockint::sockaddr-in-len sockaddr) 16)
      (setf (sockint::sockaddr-in-family sockaddr) sockint::af-inet)
      (dotimes (i 4)
        (setf (sb-alien:deref (sockint::sockaddr-in-addr sockaddr) i)
              (aref address i)))
      (let ((err (sockint::getnameinfo (sb-alien:alien-sap sockaddr)
                                       (sb-alien:alien-size sockint::sockaddr-in :bytes)
                                       (sb-alien:cast host-buf (* char)) ni-max-host
                                       nil 0
                                       sockint::ni-namereqd)))
        (if (zerop err)
            (make-instance 'host-ent
                           :name (sb-alien::c-string-to-string
                                  (sb-alien:alien-sap host-buf)
                                  (sb-impl::default-external-format)
                                  'character)
                           :type sockint::af-inet
                           :aliases nil
                           :addresses (list address))
            (addrinfo-error "getnameinfo" err))))))

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
    #-win32
    (if (= *name-service-errno* sockint::NETDB-INTERNAL)
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
