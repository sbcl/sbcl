;;;; miscellaneous side-effectful tests of CLOS and file-compiler
;;;; optimizations

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

;;; Fix due to pmai, ported from CMUCL, regarding
;;; MAKE-INSTANCES-OBSOLETE:
(defclass mio-test ()
  ((test :initarg :test)))

(defun mio-demo ()
  (let ((x (make-instance 'mio-test :test 42)))
    (incf (slot-value x 'test))))

(defun mio-test ()
  (mio-demo)
  (make-instances-obsolete 'mio-test)
  (mio-demo))

(mio-test)

;;; success
(sb-ext:quit :unix-status 104)