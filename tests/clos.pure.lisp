;;;; CLOS tests with no side effects

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

(cl:in-package :cl-user)

;;; not really a test for observable behaviour, but: make sure that
;;; all generic functions on startup have lambda lists known to the
;;; system, because some functionality (e.g. &key argument checking)
;;; depends on it.  The basic functionality is tested elsewhere, but
;;; this is to investigate the internals for possible inconsistency.
(assert (null
	 (let (collect)
	   (sb-pcl::map-all-generic-functions
	    (lambda (gf)
	      (let ((arg-info (sb-pcl::gf-arg-info gf)))
		(when (eq (sb-pcl::arg-info-lambda-list arg-info)
			  :no-lambda-list)
		  (push gf collect)))))
	   (print (nreverse collect)))))

;;; Regressing test for invalid slot specification error printing
(multiple-value-bind (value err)    
    (ignore-errors (macroexpand '(defclass foo () (frob (frob bar)))))
  (declare (ignore value))
  (assert (typep err 'simple-condition))
  (multiple-value-bind (value format-err)
      (ignore-errors (apply #'format nil 
                            (simple-condition-format-control err)
                            (simple-condition-format-arguments err)))
    (declare (ignore value))
    (assert (not format-err))))
