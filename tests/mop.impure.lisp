;;;; miscellaneous side-effectful tests of the MOP

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

;;;; Note that the MOP is not in a supported state. Package issues
;;;; (both MOP/SB-PCL and CL/SB-PCL) have yet to be resolved, and
;;;; there is likely to be missing functionality.  However, this seems
;;;; a good a way as any of ensuring that we have no regressions.

(defpackage "MOP-TEST"
  ;; eventually, we might want "MOP" as well here.
  (:use "CL"))

(in-package "MOP-TEST")

(defgeneric fn-with-odd-arg-precedence (a b c)
  (:argument-precedence-order b c a))

(assert (equal
	 (sb-pcl:generic-function-lambda-list #'fn-with-odd-arg-precedence)
	 '(a b c)))
(assert (equal
	 (sb-pcl:generic-function-argument-precedence-order #'fn-with-odd-arg-precedence)
	 '(b c a)))

#||
This is actually a test of vanilla CLOS, not the MOP; however, there isn't
a terribly easy way of testing this without it (FIXME: one would have to
construct a series of DOCUMENTATION methods, probably involving
CALL-NEXT-METHOD). However, since we're actually getting this wrong
currently, better put in a quick test in the hope that we can fix it soon:

(assert (equal
	 (sb-pcl:generic-function-argument-precedence-order #'documentation)
	 (let ((ll (sb-pcl:generic-function-lambda-list #'documentation)))
	   (list (nth ll 1) (nth ll 0)))))
||#

;;;; success
(sb-ext:quit :unix-status 104)
