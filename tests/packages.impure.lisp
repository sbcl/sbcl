;;;; miscellaneous tests of package-related stuff

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

(make-package "FOO")
(defvar *foo* (find-package (coerce "FOO" 'base-string)))
(rename-package "FOO" (make-array 0 :element-type nil))
(assert (eq *foo* (find-package "")))
(assert (delete-package ""))

(handler-case
    (export :foo)
  (package-error (c) (princ c))
  (:no-error (&rest args) (error "(EXPORT :FOO) returned ~S" args)))

(sb-ext:quit :unix-status 104)
