;;;; rudimentary tests ("smoke tests") for miscellaneous stuff which
;;;; doesn't seem to deserve specialized files at the moment

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

;;; ROOM should run without signalling an error. (bug 247)
(room)
(room t)
(room nil)

;;; DESCRIBE should run without signalling an error.
(defstruct to-be-described a b)
(describe (make-to-be-described))
(describe 12)
(describe "a string")
(describe 'symbolism)
(describe (find-package :cl))
(describe '(a list))
(describe #(a vector))

;;; COPY-SYMBOL should work without signalling an error, even if the
;;; symbol is unbound.
(copy-symbol 'foo)
(copy-symbol 'bar t)
(defvar *baz* nil)
(copy-symbol '*baz* t)

;;; SETQ should return its value.
(assert (typep (setq *baz* 1) 'integer))
(assert (typep (in-package :cl-user) 'package))

;;; PROFILE should run without obvious breakage
(defun profiled-fun ()
  (random 1d0))
(profile profiled-fun)
(loop repeat 100000 do (profiled-fun))
(report)

;;; success
(quit :unix-status 104)
