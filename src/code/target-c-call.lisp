;;;; This file contains some extensions to the Alien facility to
;;;; simplify importing C interfaces.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C-CALL")

;;;; extra types

(define-alien-type char (integer 8))
(define-alien-type short (integer 16))
(define-alien-type int (integer 32))
(define-alien-type long (integer #!-alpha 32 #!+alpha 64))

(define-alien-type unsigned-char (unsigned 8))
(define-alien-type unsigned-short (unsigned 16))
(define-alien-type unsigned-int (unsigned 32))
(define-alien-type unsigned-long (unsigned #!-alpha 32 #!+alpha 64))

(define-alien-type float single-float)
(define-alien-type double double-float)

(define-alien-type-translator void ()
  (parse-alien-type '(values) (sb!kernel:make-null-lexenv)))

#+nil 
(define-alien-routine strlen integer
  (s (* char)))

(defun %naturalize-c-string (sap)
  (declare (type system-area-pointer sap))
  (with-alien ((ptr (* char) sap))
    (let* ((length (alien-funcall (extern-alien "strlen"
						(function integer (* char)))
				  ptr))
	   (result (make-string length)))
      (declare (optimize (speed 3) (safety 0)))
      (sb!kernel:%byte-blt sap 0 result 0 length)
      result)))
