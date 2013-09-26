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

(defconstant WSA_FLAG_OVERLAPPED 1)
(declaim (inline handle->fd fd->handle))

;;; For a few more releases, let's preserve old functions (now
;;; implemented as identity) for user code which might have had to peek
;;; into our internals in past versions when we hadn't been using
;;; handles yet. -- DFL, 2012
(defun handle->fd (handle flags) (declare (ignore flags)) handle)
(defun fd->handle (fd) fd)

(defun socket (af type proto)
  (wsa-socket af type proto nil 0 WSA_FLAG_OVERLAPPED))

;;; For historical reasons, the FFI functions declared in win32-constants
;;; prepend "win32-" to the symbol names.  Rather than break compatibility
;;; for users depending on those names, wrap the misnamed functions in
;;; correctly named ones...
;;;
;;; FIXME: We have a deprecation pipe these days. Remove the win32-prefixes,
;;; and make deprecation-wrappers with the prefixes.
(macrolet ((define-socket-fd-arg-routines (&rest names)
             `(progn
                (declaim (inline ,@names))
                ,@(loop for routine in names collect
                       `(defun ,routine (handle &rest options)
                          (apply #',(sb-int:symbolicate "WIN32-" routine)
                                 handle options))))))
  (define-socket-fd-arg-routines
      bind getsockname listen recv recvfrom send sendto close connect
      getpeername ioctl setsockopt getsockopt))

(defun accept (handle &rest options)
  (let ((handle (apply #'win32-accept handle options)))
    (if (= handle -1)
        -1
        handle)))

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


