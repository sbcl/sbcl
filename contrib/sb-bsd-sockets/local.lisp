(in-package :sb-bsd-sockets)


;;;; Local domain sockets

(defclass local-socket (socket)
  ((family :initform sockint::af-local))
  (:documentation
   "Class representing local domain (AF_LOCAL) sockets,
also known as unix-domain sockets."))

(defmethod socket-namestring ((socket local-socket))
  (ignore-errors (socket-name socket)))

(defmethod socket-peerstring ((socket local-socket))
  (ignore-errors (socket-peername socket)))

(defmethod make-sockaddr-for ((socket local-socket)
                              &optional sockaddr &rest address)
  (let ((filename (first address))
        (sockaddr (or sockaddr (sockint::allocate-sockaddr-un))))
    (setf (sockint::sockaddr-un-family sockaddr) sockint::af-local)
    (when filename
      (setf (sockint::sockaddr-un-path sockaddr) filename))
    (values sockaddr sockint::size-of-sockaddr-un)))

(defmethod free-sockaddr-for ((socket local-socket) sockaddr)
  (sb-alien:free-alien sockaddr))

(defmethod size-of-sockaddr ((socket local-socket))
  sockint::size-of-sockaddr-un)

(defmethod bits-of-sockaddr ((socket local-socket) sockaddr &optional size)
  "Return the file name of the local socket address SOCKADDR."
  (declare (ignore size))
  (let ((name (sockint::sockaddr-un-path sockaddr)))
    (unless (zerop (length name)) name)))


;;;; Local domain sockets in the abstract namespace

(defclass local-abstract-socket (local-socket) ()
  (:documentation
   "Class representing local domain (AF_LOCAL) sockets with addresses
in the abstract namespace."))

(defmethod make-sockaddr-for ((socket local-abstract-socket)
                              &optional sockaddr &rest address)
  (check-type address (or null
                          (cons (or (vector (unsigned-byte 8)) string pathname)
                                null)))

  (let* ((path (first address))
         (path/octets (etypecase path
                        (pathname (sb-ext:string-to-octets
                                   (sb-ext:native-namestring path)))
                        (string (sb-ext:string-to-octets path))
                        (t path))))
    (when (and path/octets
               (> (length path/octets)
                  (- sockint::size-of-sockaddr-un-abstract 3)))
      (error "~@<Path is two long for the AF_LOCAL abstract namespace: ~
              ~S.~@:>"
             path))

    (let ((sockaddr (or sockaddr (sockint::allocate-sockaddr-un-abstract))))
      (setf (sockint::sockaddr-un-abstract-family sockaddr) sockint::af-local)
      ;; First byte of the path is always 0.
      (setf (sb-alien:deref (sockint::sockaddr-un-abstract-path sockaddr) 0) 0)
      (cond
        (path/octets
         ;; We fill in the rest of the path starting at index 1.
         (loop for octet across path/octets
            for i from 1
            do (setf (sb-alien:deref
                      (sockint::sockaddr-un-abstract-path sockaddr) i)
                     octet))
         (values sockaddr (+ 2 1 (length path/octets))))
        (t
         (values sockaddr sockint::size-of-sockaddr-un-abstract))))))

(defmethod free-sockaddr-for ((socket local-abstract-socket) sockaddr)
  (sb-alien:free-alien sockaddr))

(defmethod size-of-sockaddr ((socket local-abstract-socket))
  sockint::size-of-sockaddr-un-abstract)

(defmethod bits-of-sockaddr ((socket local-abstract-socket) sockaddr
                             &optional
                             (size sockint::size-of-sockaddr-un-abstract))
  "Return the contents of the local socket address SOCKADDR."
  (when (> size 2)
    (let* ((path-length (- size 2 1))
           (path (make-array path-length
                             :element-type '(unsigned-byte 8)
                             :initial-element 0)))
      ;; Exclude the first byte (it's always null) of the address.
      (loop for i from 1 to path-length
         do (setf (aref path (1- i))
                  (sb-alien:deref
                   (sockint::sockaddr-un-abstract-path sockaddr) i)))
      path)))
