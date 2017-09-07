;;;; loaded by init.test.sh

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

(write-line "/loading custom sysinit")

;;; *load-pathname* and *load-truename* shouldn't be NIL
(assert (string= (pathname-name *load-pathname*) "custom-sysinit"))
(assert *load-truename*)

(defun sysinit-21 ()
  21)
