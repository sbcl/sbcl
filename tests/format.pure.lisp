;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(in-package :cl-user)

(with-test (:name :~[-non-integer-argument)
  (assert-error (eval '(format nil "~[~]" 1d0))
                sb-format:format-error)
  (assert-error (funcall (checked-compile
                          '(lambda (x)
                            (format nil "~[~]" x)))
                         1d0)
                sb-format:format-error))
