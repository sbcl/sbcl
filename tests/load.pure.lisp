;;;; miscellaneous tests of LOOP-related stuff

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

(in-package "CL-USER")

;;; As reported by Gregory Wright sbcl-devel 2002-07-15, SBCL 0.7.5
;;; didn't honor the LOAD :IF-DOES-NOT-EXIST argument when the type of
;;; the LOADed file was specified.
(load "i-am-not" :if-does-not-exist nil)
(load "i-am-not.lisp" :if-does-not-exist nil)
(load "i-am-not.fasl" :if-does-not-exist nil)
(load "i-am-not.misc-garbage" :if-does-not-exist nil)

;;; In 1.0.17, LOAD didn't error when the file didn't exist and
;;; IF-DOES-NOT-EXIST was true.
(assert (typep (nth-value 1 (ignore-errors (load "i-am-not"))) 'file-error))
(assert (typep (nth-value 1 (ignore-errors (load "i-am-not" :if-does-not-exist t))) 'file-error))
