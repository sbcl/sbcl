;;;; miscellaneous impure tests of SYMBOL-related stuff

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

(declaim (type (simple-array fixnum (*)) *foo*))
(with-test (:name :defvar-type-error)
  (assert (eq :ok
              (handler-case
                  (eval `(defvar *foo* (make-array 10 :element-type '(unsigned-byte 60))))
                (type-error (e)
                  (when (and (typep e 'type-error)
                             (equal '(simple-array fixnum (*))
                                    (type-error-expected-type e)))
                    ;; Check that it prints without an error.
                    (let ((string (princ-to-string e)))
                      :ok)))))))
