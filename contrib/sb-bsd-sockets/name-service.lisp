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

(declaim (inline naturalize-unsigned-byte-8-array))
(defun naturalize-unsigned-byte-8-array (array length)
  (let ((addr (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (i length)
      (setf (elt addr i) (sb-alien:deref array i)))
    addr))

#-sb-bsd-sockets-addrinfo
(defun make-host-ent (h &optional errno)
  (when (sb-alien:null-alien h)
    (name-service-error "gethostbyname" (get-name-service-errno)))
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

;;; Resolving

#-sb-bsd-sockets-addrinfo
(progn
  (sb-ext:defglobal **gethostby-lock**
      (sb-thread:make-mutex :name "gethostby lock"))

  (defun get-host-by-name (host-name)
    "Returns a HOST-ENT instance for HOST-NAME or signals a NAME-SERVICE-ERROR.
HOST-NAME may also be an IP address in dotted quad notation or some other
weird stuff - see gethostbyname(3) for the details."
    (sb-int:with-system-mutex (**gethostby-lock** :allow-with-interrupts t)
      (make-host-ent (sockint::gethostbyname host-name))))

  (defun get-host-by-address (address)
    "Returns a HOST-ENT instance for ADDRESS, which should be a vector of
 (integer 0 255), or signals a NAME-SERVICE-ERROR. See gethostbyaddr(3)
 for details."
    (sb-int:with-system-mutex (**gethostby-lock** :allow-with-interrupts t)
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
  (defun get-host-by-name (host-name)
    "Returns a HOST-ENT instance for HOST-NAME or signals a NAME-SERVICE-ERROR.

Another HOST-ENT instance containing zero, one or more IPv6 addresses
may be returned as a second return value.

HOST-NAME may also be an IP address in dotted quad notation or some other
weird stuff - see getaddrinfo(3) for the details."
    (declare (optimize speed))
    (sb-alien:with-alien ((info (* sockint::addrinfo)))
      (addrinfo-error-case ("getaddrinfo"
                            (sb-sys:without-interrupts
                              (sockint::getaddrinfo
                               host-name nil nil (sb-alien:addr info))))
          (let ((host-ent4 (make-instance 'host-ent
                                          :name host-name
                                          :type sockint::af-inet
                                          :aliases nil
                                          :addresses nil))
                #-win32
                (host-ent6 (make-instance 'host-ent
                                          :name host-name
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
                   #-win32
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
            (values host-ent4 #-win32 host-ent6)))))

  (defun get-host-by-address (address)
    "Returns a HOST-ENT instance for ADDRESS, which should be a vector of
\(integer 0 255) with 4 elements in case of an IPv4 address and 16
elements in case of an IPv6 address, or signals a NAME-SERVICE-ERROR.
See gethostbyaddr(3) for details."
    (declare (optimize speed))
    (multiple-value-bind (sockaddr sockaddr-size address-family)
        (etypecase address
          ((vector (unsigned-byte 8) 4)
           (let ((sockaddr (sb-alien:make-alien sockint::sockaddr-in)))
             #+darwin (setf (sockint::sockaddr-in-len sockaddr) 16)
             (setf (sockint::sockaddr-in-family sockaddr) sockint::af-inet)
             (dotimes (i (length address))
               (setf (sb-alien:deref (sockint::sockaddr-in-addr sockaddr) i)
                     (aref address i)))
             (values sockaddr
                     (sb-alien:alien-size sockint::sockaddr-in :bytes)
                     sockint::af-inet)))
          #-win32
          ((vector (unsigned-byte 8) 16)
           (let ((sockaddr (sb-alien:make-alien sockint::sockaddr-in6)))
             (setf (sockint::sockaddr-in6-family sockaddr) sockint::af-inet6)
             (dotimes (i (length address))
               (setf (sb-alien:deref (sockint::sockaddr-in6-addr sockaddr) i)
                     (aref address i)))
             (values sockaddr
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
        (sb-alien:free-alien sockaddr)))))

;;; Error handling

(defun name-service-error (where errno)
  (cond #-sb-bsd-sockets-addrinfo
        ((= errno sockint::NETDB-INTERNAL)
         ;; Comment next to NETDB_INTERNAL in netdb.h says "See errno.".
         ;; This special case treatment hasn't actually been tested yet.
         (socket-error where))
        (t
         (error (condition-for-name-service-errno errno)
                :errno errno :syscall where))))

(define-condition name-service-error (error)
  ((errno :initform nil :initarg :errno :reader name-service-error-errno)
   (symbol :initform nil :initarg :symbol :reader name-service-error-symbol)
   (syscall :initform "an unknown location" :initarg :syscall :reader name-service-error-syscall))
  (:report (lambda (c s)
             (let* ((errno (name-service-error-errno c)))
               (format s "Name service error in \"~A\": ~A (~A)"
                       (name-service-error-syscall c)
                       (or (name-service-error-symbol c)
                           errno)
                       (get-name-service-error-message errno))))))

(defparameter *conditions-for-name-service-errno* nil)

;; Define a special name-service-error for variour error cases, and associate
;; them with the matching h_errno / error code.
(macrolet ((def (errno gai-errno name)
             (declare (ignorable errno gai-errno))
             `(progn
                (define-condition ,name (name-service-error)
                  ())
                ,(let ((errno #-sb-bsd-sockets-addrinfo errno
                              #+sb-bsd-sockets-addrinfo gai-errno))
                   (when errno
                     `(push (cons ,(find-symbol errno 'sockint)
                                  (quote ,name))
                            *conditions-for-name-service-errno*))))))
  (def "NETDB-INTERNAL" nil netdb-internal-error)
  (def "NETDB-SUCCESS" nil netdb-success-error)
  (def "HOST-NOT-FOUND" "EAI-NONAME" host-not-found-error)
  (def "TRY-AGAIN" "EAI-AGAIN" try-again-error)
  (def "NO-RECOVERY" "EAI-FAIL" no-recovery-error)
  (def "NO-ADDRESS" ;; Also defined as NO-DATA, with the same value
    ;; getaddrinfo() as of RFC 3493 can no longer distinguish between
    ;; host no found and address not found
    nil
    no-address-error))

(defun condition-for-name-service-errno (err)
  (or (cdr (assoc err *conditions-for-name-service-errno*))
      'name-service-error))

#-sb-bsd-sockets-addrinfo
(defun get-name-service-errno ()
  (sb-alien:alien-funcall
   (sb-alien:extern-alien #-win3 "get_h_errno" #+win32 "WSAGetLastError"
                          (function integer))))

(defun get-name-service-error-message (errno)
  #-sb-bsd-sockets-addrinfo
  (sockint::h-strerror errno)
  #+sb-bsd-sockets-addrinfo
  (sockint::gai-strerror errno))
