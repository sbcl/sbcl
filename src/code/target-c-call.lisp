;;;; FIXME: This file and host-c-call.lisp are separate from the
;;;; rest of the alien source code for historical reasons: CMU CL
;;;; made a distinction between the stuff in the C-CALL package and
;;;; stuff in the ALIEN package. There's no obvious boundary
;;;; there, though, and SBCL doesn't try to make this distinction,
;;;; so it might make sense to just merge these files in with the
;;;; rest of the SB-ALIEN code.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-ALIEN")

;;;; extra types

(define-alien-type char (integer 8))
(define-alien-type short (integer 16))
(define-alien-type int (integer 32))

(define-alien-type long (integer #+win32 32
                                 #-win32 #.sb-vm:n-machine-word-bits))

(define-alien-type long-long (integer 64))

(define-alien-type unsigned-char (unsigned 8))
(define-alien-type unsigned-short (unsigned 16))
(define-alien-type unsigned-int (unsigned 32))
(define-alien-type unsigned-long (unsigned #+win32 32
                                           #-win32 #.sb-vm:n-machine-word-bits))
(define-alien-type unsigned-long-long (unsigned 64))

(define-alien-type float single-float)
(define-alien-type double double-float)

(define-alien-type utf8-string (c-string :external-format :utf8))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-alien-type-translator void ()
    (parse-alien-type '(values) (sb-kernel:make-null-lexenv))))


(defun default-c-string-external-format ()
  (or *default-c-string-external-format*
      (setf *default-c-string-external-format*
            (sb-impl::default-external-format))))

(defun %naturalize-c-string (sap)
  (declare (type system-area-pointer sap))
  ;; It can be assumed that any modern implementation of strlen() reads 4, 8, 16,
  ;; or possibly even 32 bytes at a time when searching for the '\0' terminator.
  ;; As such, we expect it to be on average much faster than a loop over SAP-REF-8.
  ;; And much to my surprise, the foreign call overhead on x86-64 is so small that
  ;; there is not a minimum threshold length below which the foreign call costs too much.
  ;; With as few as 5 characters in the string, I saw 2x speedup.
  ;; Below that, it's about the same to do a foreign call versus staying in lisp.
  ;; The limiting case of a 0 length string would be faster without the foreign call,
  ;; but pre-checking would slow down every other case.
  (let* ((length (alien-funcall
                 (extern-alien "strlen" (function size-t system-area-pointer))
                 sap))
         (result (make-string length :element-type 'base-char)))
    ;; COPY-UB8 pins the lisp string, no need to do it here
    (sb-kernel:copy-ub8-from-system-area sap 0 result 0 length)
    result))
