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

;;; If *BREAK-ON-SIGNALS* has a bogus value, don't go off in an infinite
;;; recursion.
(with-test (:name (*break-on-signals* :smoke))
  (assert
   (catch 'ok
     (handler-bind
         ((error
           (lambda (condition)
             (when (search "NOT-A-TYPE-SPECIFIER" (princ-to-string condition))
               (throw 'ok t)))))
       (let ((*break-on-signals* '#:not-a-type-specifier))
         (signal "foo"))
       nil))))
