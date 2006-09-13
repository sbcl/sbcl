;;;; various tests of the interpreter

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

#-sb-eval
(sb-ext:quit :unix-status 104)

(setf sb-ext:*evaluator-mode* :interpret)

(assert (not (typep (lambda ()) 'compiled-function)))

(assert (not (compiled-function-p (lambda ()))))

(let ((seen-forms (make-hash-table :test 'equal)))
  (let ((*macroexpand-hook* (compile nil
                                     `(lambda (fun form env)
                                        (setf (gethash form ,seen-forms) t)
                                        (funcall fun form env)))))
    (let ((fun (lambda ()
                 (when t nil))))
      (assert (not (gethash '(when t nil) seen-forms)))
      (funcall fun)
      (assert (gethash '(when t nil) seen-forms)))))

