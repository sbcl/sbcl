;;;; miscellaneous tests of compiling toplevel forms

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

;;; Exercise a compiler bug (by causing a call to ERROR).
;;;
;;; This bug was in sbcl-0.6.11.6.
(let ((a 1) (b 1))
  (declare (type (mod 1000) a b))
  (let ((tmp (= 10 (+ (incf a) (incf a) (incf b) (incf b)))))
    (or tmp (error "TMP not true"))))

;;; Exercise a (byte-)compiler bug by causing a call to ERROR, not
;;; because the symbol isn't defined as a variable, but because
;;;  TYPE-ERROR in SB-KERNEL::OBJECT-NOT-TYPE-ERROR-HANDLER:
;;;     0 is not of type (OR FUNCTION SB-KERNEL:FDEFN).
;;; Correct behavior is to warn at compile time because the symbol
;;; isn't declared as a variable, but to set its SYMBOL-VALUE anyway.
;;; 
;;; This bug was in sbcl-0.6.11.13.
(print (setq improperly-declared-var '(1 2)))
(assert (equal (symbol-value 'improperly-declared-var) '(1 2)))
(makunbound 'improperly-declared-var)
;;; This is a slightly different way of getting the same symptoms out
;;; of the sbcl-0.6.11.13 byte compiler bug.
(print (setq *print-level* *print-level*))
