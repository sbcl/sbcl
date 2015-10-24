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

#-(or sb-eval sb-fasteval)
(sb-ext:exit :code 104)

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

;;; defstruct constructor
(let ((sb-ext:*evaluator-mode* :interpret))
  (eval '(progn
          (defstruct evaluated-struct
            (pointer nil)
            (word 0 :type sb-ext:word)
            (single 0.0 :type single-float)
            (double 0.0d0 :type double-float)
            (csingle (complex 0.0 0.0) :type (complex single-float))
            (cdouble (complex 0.0d0 0.0d0) :type (complex double-float)))
          (defvar *evaluated-struct* (make-evaluated-struct
                                      :pointer :foo
                                      :word 42
                                      :single 1.23
                                      :double 2.34d0
                                      :csingle (complex 1.0 2.0)
                                      :cdouble (complex 2.0d0 3.0d0)))
          (assert (eq :foo (evaluated-struct-pointer *evaluated-struct*)))
          (assert (eql 42 (evaluated-struct-word *evaluated-struct*)))
          (assert (eql 1.23 (evaluated-struct-single *evaluated-struct*)))
          (assert (eql 2.34d0 (evaluated-struct-double *evaluated-struct*)))
          (assert (eql #c(1.0 2.0) (evaluated-struct-csingle *evaluated-struct*)))
          (assert (eql #c(2.0d0 3.0d0) (evaluated-struct-cdouble *evaluated-struct*))))))

;;; Prior to 1.0.25, the interpreter checked for package lock
;;; violation for a local function in the fbinding form's body's
;;; lexical environment.
(let ((sb-ext:*evaluator-mode* :interpret))
  (assert
   (ignore-errors
     (eval
      '(eql
        (locally (declare (disable-package-locks
                           ;; rather than create a whole new package
                           ;; just to test this corner case, we'll
                           ;; lexically shadow something innocuous in
                           ;; the CL package.
                           cl:ed))
          (flet ((cl:ed ()
                   42))
            (declare (enable-package-locks cl:ed))
            (cl:ed)))
        42)))))

(defvar *file* #p"full-eval-temp.lisp")
(with-test (:name (:full-eval :redefinition-warnings))
  (with-open-file (stream *file* :direction :output :if-exists :supersede)
    (write '(defun function-for-redefinition () nil) :stream stream))
  (handler-bind ((warning #'error))
    (let ((sb-ext:*evaluator-mode* :interpret))
      (load *file*)
      (load *file*))
    (let ((sb-ext:*evaluator-mode* :compile))
      (load *file*))))
(delete-file *file*)

(defvar *stash*)
(defun save-it (f) (setq *stash* f) 'whatever)
(with-test (:name (let* :nested-environments))
  (let ((z 'zee) (y 'y) (x 92))
    (let* ((baz (save-it (lambda (what) (assert (equal (list what x y z)
                                                       (list what 92 'y 'zee))))))
           (mum (funcall *stash* :after-binding-baz))
           (y 'new-y)
           (z (progn (funcall *stash* :after-binding-y) 'new-z))
           (x (progn (funcall *stash* :after-binding-z) 'new-x)))
      (funcall *stash* :in-body)
      (values))))

(with-test (:name (let* :nested-environment-again))
  (let* ((foo 3)
         (foo (lambda () (typep foo 'integer))))
    (assert (funcall foo))))
