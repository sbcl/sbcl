;;;; lambda-list parsing tests with no side-effects

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

(macrolet ((error-p (ll)
             `(progn
                (multiple-value-bind (result error) (ignore-errors (handler-bind ((error #'error))
                                                                     (eval `(lambda ,',ll 'ok))))
                  (unless (and (not result) error)
                    (error "No error from lambda ~S." ',ll)))
                (multiple-value-bind (result error) (ignore-errors (handler-bind ((error #'error))
                                                                     (eval `(lambda (x) (destructuring-bind ,',ll x 'ok)))))
                  (unless (and (not result) error)
                    (error "No error from d-b ~S." ',ll))))))
  (error-p (&aux (foo 1) &aux (bar 2)))
  (error-p (&aux (foo 1) &key bar))
  (error-p (&aux (foo 1) &optional bar))
  (error-p (&aux (foo 1) &rest bar))
  (error-p (&key foo &allow-other-keys &allow-other-keys))
  (error-p (&key foo &key bar))
  (error-p (&key foo &optional bar))
  (error-p (&key foo &rest bar))
  (error-p (&optional foo &optional bar))
  (error-p (&rest foo &rest bar))
  (error-p (&rest foo &optional bar)))
