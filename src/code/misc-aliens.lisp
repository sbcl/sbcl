;;;; assorted alien definitions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;; Declare each of the free space pointers (except dynamic) as an alien var
(define-alien-variable ("read_only_space_free_pointer"
                        sb-vm:*read-only-space-free-pointer*)
    system-area-pointer)
(define-alien-variable ("static_space_free_pointer" sb-vm:*static-space-free-pointer*)
  system-area-pointer)

#+darwin-jit
(define-alien-variable ("static_code_space_free_pointer" sb-vm:*static-code-space-free-pointer*)
  system-area-pointer)

(declaim (inline memmove))
(define-alien-routine ("memmove" memmove) void
  (dest (* char))
  (src (* char))
  (n unsigned-int))

(define-alien-routine ("os_get_errno" get-errno) int)
(setf (documentation 'get-errno 'function)
      "Return the value of the C library pseudo-variable named \"errno\".")

;;; Decode errno into a string.
#-win32
(defun strerror (&optional (errno (get-errno)))
  (alien-funcall (extern-alien "strerror" (function c-string int)) errno))

#+win32
(defun strerror (&optional (errno (sb-win32:get-last-error)))
  (sb-win32:format-system-message errno))
