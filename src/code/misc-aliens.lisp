;;;; assorted alien definitions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(declaim (inline memmove))
(define-alien-routine ("memmove" memmove) void
  (dest (* char))
  (src (* char))
  (n unsigned-int))

(define-alien-routine ("os_get_errno" get-errno) integer)
#!+sb-doc
(setf (fdocumentation 'get-errno 'function)
      "Return the value of the C library pseudo-variable named \"errno\".")

;;; Decode errno into a string.
#!-win32
(defun strerror (&optional (errno (get-errno)))
  (alien-funcall (extern-alien "strerror" (function c-string int)) errno))

#!+win32
(defun strerror (&optional (errno (sb!win32:get-last-error)))
  (sb!win32:format-system-message errno))
