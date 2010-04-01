(in-package :sb-bsd-sockets)

(defclass local-socket (socket)
  ((family :initform sockint::af-local))
  (:documentation "Class representing local domain (AF_LOCAL) sockets,
also known as unix-domain sockets."))

(defmethod socket-namestring ((socket local-socket))
  (ignore-errors (socket-name socket)))

(defmethod socket-peerstring ((socket local-socket))
  (ignore-errors (socket-peername socket)))

(defmethod make-sockaddr-for ((socket local-socket)
                              &optional sockaddr &rest address &aux (filename (first address)))
  (let ((sockaddr (or sockaddr (sockint::allocate-sockaddr-un))))
    (setf (sockint::sockaddr-un-family sockaddr) sockint::af-local)
    (when filename
      (setf (sockint::sockaddr-un-path sockaddr) filename))
    sockaddr))

(defmethod free-sockaddr-for ((socket local-socket) sockaddr)
  (sockint::free-sockaddr-un sockaddr))

(defmethod size-of-sockaddr ((socket local-socket))
  sockint::size-of-sockaddr-un)

(defmethod bits-of-sockaddr ((socket local-socket) sockaddr)
  "Return the file name of the local socket address SOCKADDR."
  (let ((name (sockint::sockaddr-un-path sockaddr)))
    (if (zerop (length name)) nil name)))

(defclass local-abstract-socket (local-socket) ()
  (:documentation "Class representing local domain (AF_LOCAL) sockets with
addresses in the abstract namespace."))

(defmethod make-sockaddr-for ((socket local-abstract-socket)
                              &optional sockaddr &rest address
                              &aux (path (first address)))
  (let ((sockaddr (or sockaddr (sockint::allocate-sockaddr-un-abstract)))
        (len 0))
    (setf (sockint::sockaddr-un-abstract-family sockaddr) sockint::af-local)
    ;;First byte of the path is always 0.
    (setf (sb-alien:deref (sockint::sockaddr-un-abstract-path sockaddr) 0) 0)

    (when path
      (when (stringp path)
        (setf path (sb-ext:string-to-octets path)))
      (setf len (min (- sockint::size-of-sockaddr-un-abstract 3) (length path)))
      ;;We fill in the rest of the path starting at index 1.
      (loop for i from 0 below len
            do (setf (sb-alien:deref (sockint::sockaddr-un-abstract-path
                                      sockaddr)
                                     (1+ i))
                     (elt path i))))
    (values sockaddr (+ 3 len))))

(defmethod free-sockaddr-for ((socket local-abstract-socket) sockaddr)
  (sockint::free-sockaddr-un-abstract sockaddr))

(defmethod size-of-sockaddr ((socket local-abstract-socket))
  sockint::size-of-sockaddr-un-abstract)

(defmethod bits-of-sockaddr ((socket local-abstract-socket) sockaddr)
  "Return the contents of the local socket address SOCKADDR."
  (let* ((path-len (- sockint::size-of-sockaddr-un-abstract 3))
         (path (make-array `(,path-len)
                           :element-type '(unsigned-byte 8)
                           :initial-element 0)))
    ;;exclude the first byte (it's always null) of the address
    (loop for i from 1 to path-len
          do (setf (elt path (1- i))
                   (sb-alien:deref (sockint::sockaddr-un-abstract-path sockaddr)
                                   i)))
    path))

(defmethod socket-connect ((socket local-abstract-socket) &rest peer
                           &aux (path (first peer)))
  (multiple-value-bind (sockaddr addr-len)
      (make-sockaddr-for socket nil path)
    (unwind-protect
         (if (= (sockint::connect (socket-file-descriptor socket)
                                  sockaddr
                                  addr-len)
                -1)
             (socket-error "connect"))
      (free-sockaddr-for socket sockaddr))))

(defmethod socket-bind ((socket local-abstract-socket)
                        &rest address &aux (path (first address)))
  (multiple-value-bind (sockaddr addr-len)
      (make-sockaddr-for socket nil path)
    (unwind-protect
         (if (= (sockint::bind (socket-file-descriptor socket)
                               sockaddr
                               addr-len)
                -1)
             (socket-error "bind"))
      (free-sockaddr-for socket sockaddr))))
