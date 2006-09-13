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

(let ((*macroexpand-hook*
       (compile nil
                (lambda (fun form env)
                  (handler-bind ((error (lambda (c)
                                          (when (eq 'destructuring-bind (car form))
                                            (throw 'd-b-error c)))))
                    (funcall fun form env))))))
  (macrolet ((maybe-funcall (&rest args)
               ;; The evaluator will delay lambda-list checks until
               ;; the lambda is actually called.
               (if (eq sb-ext:*evaluator-mode* :interpret)
                   `(funcall ,@args)
                   `(progn ,@args)))
             (error-p (ll)
               `(progn
                  (multiple-value-bind (result error)
                      (ignore-errors (maybe-funcall (eval `(lambda ,',ll 'ok))))
                    (unless (and (not result) error)
                      (error "No error from lambda ~S." ',ll)))
                  (catch 'd-b-error
                    (maybe-funcall
                     (eval `(lambda (x) (destructuring-bind ,',ll x 'ok)))
                     nil)
                    (error "No error from d-b ~S." ',ll)))))
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
    (error-p (&rest foo &optional bar))))
