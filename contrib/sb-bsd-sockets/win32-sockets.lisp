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

;;;; we are now going back to the normal sockint
;;;; package where we will redefine all of the above
;;;; functions, converting between HANDLES and fds

(defun socket (af type proto)
  (let* ((handle (wsa-socket af type proto nil 0 0))
         (fd (handle->fd handle 0)))
    fd))

(defun bind (fd &rest options)
  (let ((handle (fd->handle fd)))
    (apply #'win32-bind handle options)))

(defun getsockname (fd &rest options)
  (apply #'win32-getsockname (fd->handle fd) options))

(defun listen (fd &rest options)
  (apply #'win32-listen (fd->handle fd) options))

(defun accept (fd &rest options)
  (handle->fd
   (apply #'win32-accept (fd->handle fd) options)
   0))

(defun recv (fd &rest options)
  (apply #'win32-recv (fd->handle fd) options))

(defun recvfrom (fd &rest options)
  (apply #'win32-recvfrom (fd->handle fd) options))

(defun send (fd &rest options)
  (apply #'win32-send (fd->handle fd) options))

(defun sendto (fd &rest options)
  (apply #'win32-sendto (fd->handle fd) options))

(defun close (fd &rest options)
  (apply #'win32-close (fd->handle fd) options))

(defun connect (fd &rest options)
  (apply #'win32-connect (fd->handle fd) options))

(defun getpeername (fd &rest options)
  (apply #'win32-getpeername (fd->handle fd) options))

(defun ioctl (fd &rest options)
  (apply #'win32-ioctl (fd->handle fd) options))

(defun setsockopt (fd &rest options)
  (apply #'win32-setsockopt (fd->handle fd) options))

(defun getsockopt (fd &rest options)
  (apply #'win32-getsockopt (fd->handle fd) options))

(defun make-wsa-version (major minor)
  (dpb minor (byte 8 8) major))

(defvar *wsa-startup-call* nil)

(defun call-wsa-startup ()
  (setf *wsa-startup-call* (wsa-startup (make-wsa-version 2 2))))

;;; Startup!
(call-wsa-startup)

;;; Ensure startup for saved cores as well.
(push 'call-wsa-startup sb-ext:*init-hooks*)

;; not implemented on win32
(defconstant af-local 0)
(defconstant msg-dontwait 0)
(defconstant msg-trunc 0)
(defconstant msg-eor 0)
(defconstant msg-nosignal 0)
(defconstant msg-waitall 0)
(defconstant msg-eor 0)
(defconstant size-of-sockaddr-un 0)
(defun (setf sockaddr-un-family) (addr family) ())
(defun (setf sockaddr-un-path) (addr family) ())
(defun sockaddr-un-path (addr) ())
(defun free-sockaddr-un (addr) ())
(defun allocate-sockaddr-un () ())


