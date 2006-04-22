;;;; win32 socket operations
;;;; these have all been done by hand since I can't seem
;;;; to get my head around the sb-grovel stuff

;;;; Winsock requires us to convert HANDLES to/from
;;;; file descriptors, so I've added an additional
;;;; package for the actual winsock alien defs, and then
;;;; in the sockint package, we implement wrappers that
;;;; handle the conversion.

;;; these are all of the basic structure alien defs
(in-package :sockint)

(sb-alien:load-shared-object "ws2_32.dll")
(sb-alien:load-shared-object "msvcrt.dll")

(sb-alien:define-alien-type nil
  (struct WSADATA
          (wVersion (unsigned 16))
          (wHighVersion (unsigned 16))
          (szDescription (array char 257))
          (szSystemStatus (array char 129))
          (iMaxSockets (unsigned 16))
          (iMaxUdpDg (unsigned 16))
          (lpVendorInfo sb-alien:c-string)))

(sb-alien:define-alien-type nil
  (struct s_un_byte
          (s_b1 (unsigned 8))
          (s_b2 (unsigned 8))
          (s_b3 (unsigned 8))
          (s_b4 (unsigned 8))))

(sb-alien:define-alien-type nil
  (struct s_un_wide
          (s_w1 (unsigned 16))
          (s_w2 (unsigned 16))))

(sb-alien:define-alien-type nil
  (union s_union
         (s_un_b (struct s_un_byte))
         (s_un_w (struct s_un_wide))
         (s_addr (unsigned 32))))

(sb-alien:define-alien-type nil
  (struct in_addr
          (s_union (union s_union))))

(sb-alien:define-alien-type nil
  (struct sockaddr_in
          (sin_family (signed 16))
          (sin_port (array (unsigned 8) 2))
          (sin_addr (array (unsigned 8) 4))
          (sin_zero (array char 8))))

(defconstant size-of-sockaddr-in 16)

(defconstant size-of-sockaddr-un 16)

(sb-alien:define-alien-type nil
  (struct sockaddr
          (sa_family (unsigned 16))
          (sa_data (array char 14))))

(sb-alien:define-alien-type nil
  (struct hostent
          (h_name sb-alien:c-string)
          (h_aliases (* sb-alien:c-string))
          (h_addrtype sb-alien:short)
          (h_length sb-alien:short)
          (h_addr_list (* (* (unsigned 8))))))

(sb-alien:define-alien-type nil
  (struct  protoent
        (pname sb-alien:c-string)
        (p_aliases (* sb-alien:c-string))
        (p_proto (signed 16))))

(sb-alien:define-alien-type socklen-t
                            (unsigned 32))


;;; these are all non-HANDLE using, so are safe to have here
(sb-alien:define-alien-routine "gethostbyaddr" (struct hostent)
                               (addr sb-alien:c-string)
                               (len int)
                               (type int))

(sb-alien:define-alien-routine "gethostbyname" (struct hostent)
                               (addr sb-alien:c-string))

(sb-alien:define-alien-routine "getservbyport" (struct servent)
                               (port int)
                               (proto sb-alien:c-string))

(sb-alien:define-alien-routine "getservbyname" (struct servent)
                               (name sb-alien:c-string)
                               (proto sb-alien:c-string))

(sb-alien:define-alien-routine "getprotobynumber" (struct protoent)
                               (number int))

(sb-alien:define-alien-routine "getprotobyname" (struct protoent)
                               (name sb-alien:c-string))

;;; these are the alien references to the
;;; winsock calls

(in-package :win32sockint)

(sb-alien:define-alien-routine "socket" int
  (af int)
  (type int)
  (protocol int))

(sb-alien:define-alien-routine ("WSASocketA" wsa-socket) int
  (af int)
  (type int)
  (protocol int)
  (lpProtocolInfo (* t))
  (g int)
  (flags int))

(sb-alien:define-alien-routine "bind" int
  (s int)
  (name (* (struct sockint::sockaddr_in)))
  (namelen int))

(sb-alien:define-alien-routine "getsockname" int
  (s int)
  (name (* (struct sockint::sockaddr_in)))
  (namelen int :in-out))

(sb-alien:define-alien-routine "listen" int
  (s int)
  (backlog int))

(sb-alien:define-alien-routine "accept" int
  (s int)
  (addr (* (struct sockint::sockaddr_in)))
  (addrlen int :in-out))

(sb-alien:define-alien-routine "recv" int
                               (s int)
                               (buf (* t))
                               (len int)
                               (flags int))

(sb-alien:define-alien-routine "recvfrom" int
                               (s int)
                               (buf (* t))
                               (len int)
                               (flags int)
                               (from (* (struct sockint::sockaddr_in)))
                               (fromlen (* sockint::socklen-t)))

(sb-alien:define-alien-routine ("closesocket" close) int
                               (s int))

(sb-alien:define-alien-routine "connect" int
                               (s int)
                               (name (* (struct sockint::sockaddr_in)))
                               (namelen int))

(sb-alien:define-alien-routine "getpeername" int
                               (s int)
                               (name (* (struct sockint::sockaddr_in)))
                               (namelen int :in-out))

(sb-alien:define-alien-routine "getsockopt" int
                               (s int)
                               (level int)
                               (optname int)
                               (optval sb-alien:c-string)
                               (optlen int :in-out))

(sb-alien:define-alien-routine ("ioctlsocket" ioctl) int
                               (s int)
                               (cmd int)
                               (argp (unsigned 32) :in-out))

(sb-alien:define-alien-routine "setsockopt" int
                               (s int)
                               (level int)
                               (optname int)
                               (optval (* t))
                               (optlen int))


;;;; we are now going back to the normal sockint
;;;; package where we will redefine all of the above
;;;; functions, converting between HANDLES and fds

(in-package :sockint)

(sb-alien:define-alien-routine ("_get_osfhandle" fd->handle) sb-alien:long
                               (fd int))

(sb-alien:define-alien-routine ("_open_osfhandle" handle->fd) int
                               (osfhandle int)
                               (flags int))

(defun socket (af type proto)
  (let* ((handle (win32sockint::wsa-socket af type proto nil 0 0))
         (fd (handle->fd handle 0)))
    fd))

(defun bind (fd &rest options)
  (let ((handle (fd->handle fd)))
    (apply #'win32sockint::bind handle options)))

(defun getsockname (fd &rest options)
  (apply #'win32sockint::getsockname (fd->handle fd) options))

(defun listen (fd &rest options)
  (apply #'win32sockint::listen (fd->handle fd) options))

(defun accept (fd &rest options)
  (handle->fd 
   (apply #'win32sockint::accept (fd->handle fd) options)
   0))

(defun recv (fd &rest options)
  (apply #'win32sockint::recv (fd->handle fd) options))

(defun recvfrom (fd &rest options)
  (apply #'win32sockint::recvfrom (fd->handle fd) options))

(defun close (fd &rest options)
  (apply #'win32sockint::close (fd->handle fd) options))

(defun connect (fd &rest options)
  (apply #'win32sockint::connect (fd->handle fd) options))

(defun getpeername (fd &rest options)
  (apply #'win32sockint::getpeername (fd->handle fd) options))

(defun getsockopt (fd &rest options)
  (apply #'win32sockint::getsockopt (fd->handle fd) options))

(defun ioctl (fd &rest options)
  (apply #'win32sockint::ioctl (fd->handle fd) options))

(defun setsockopt (fd &rest options)
  (apply #'win32sockint::setsockopt (fd->handle fd) options))

(defmacro with-in-addr (name init &rest body)
  (declare (ignore init))
  `(with-alien ((,name (struct in_addr)))
               ,@body))

(defun in-addr-addr (addr)
  (sb-alien:slot (sb-alien:slot addr 's_union) 's_addr))

(defmacro sockaddr-in-addr (addr)
  `(sb-alien:slot ,addr 'sin_addr))

(defmacro sockaddr-in-family (addr)
  `(sb-alien:slot ,addr 'sin_family))

(defmacro sockaddr-in-port (addr)
  `(sb-alien:slot ,addr 'sin_port))

(defun allocate-sockaddr-in ()
  (sb-alien:make-alien (struct sockaddr_in)))

(defun free-sockaddr-in (addr)
  (sb-alien:free-alien addr))

(defmacro protoent-proto (ent)
  `(sb-alien:slot ,ent 'p_proto))

(defmacro hostent-addresses (ent)
  `(sb-alien:slot ,ent 'h_addr_list))

(defmacro hostent-aliases (ent)
  `(sb-alien:slot ,ent 'h_aliases))

(defmacro hostent-length (ent)
   `(sb-alien:slot ,ent 'h_length))

(defmacro hostent-name (ent)
   `(sb-alien:slot ,ent 'h_name))

(defmacro hostent-type (ent)
   `(sb-alien:slot ,ent 'h_addrtype))

(sb-alien:define-alien-routine ("WSAStartup" wsa-startup) int
  (wVersionRequested (unsigned 16))
  (lpWSAData (struct WSADATA) :out))

(sb-alien:define-alien-routine ("WSAGetLastError" wsa-get-last-error) int)

(defun make-wsa-version (major minor)
  (dpb minor (byte 8 8) major))

(defun make-sockaddr (family)
  (let ((sa (make-alien (struct sockaddr))))
    (setf (slot sa 'sa_family) family)
    (dotimes (n 10)
      (setf (deref (slot sa 'sa_data) n) 0))
    sa))




;; un-addr not implemented on win32
(defun (setf sockaddr-un-family) (addr family) ())
(defun (setf sockaddr-un-path) (addr family) ())
(defun sockaddr-un-path (addr) ())
(defun free-sockaddr-un (addr) ())
(defun allocate-sockaddr-un () ())

