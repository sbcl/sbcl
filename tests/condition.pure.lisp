;;;; side-effect-free tests of the condition system

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

;;; Until 0.7.7.21, (MAKE-CONDITION 'FILE-ERROR :PATHNAME "FOO")
;;; wasn't printable, because the REPORT function for FILE-ERROR
;;; referred to unbound slots. This was reported and fixed by Antonio
;;; Martinez (sbcl-devel 2002-09-10).
(format t
        "~&printable now: ~A~%"
        (make-condition 'file-error :pathname "foo"))

(assert (eq
         (block nil
           (macrolet ((opaque-error (arg) `(error ,arg)))
             (handler-bind
                 ((error (lambda (c)
                           (let ((restarts (remove 'res (compute-restarts c)
                                                   :key #'restart-name
                                                   :test-not #'eql)))
                             (assert (= (length restarts) 2))
                             (invoke-restart (second restarts))))))
               (let ((foo1 (make-condition 'error))
                     (foo2 (make-condition 'error)))
                 (restart-case
                     (with-condition-restarts foo1 (list (find-restart 'res))
                       (restart-case
                           (opaque-error foo2)
                         (res () 'int1)
                         (res () 'int2)))
                   (res () 'ext))))))
         'int2))

(assert (eq
         (block nil
           (macrolet ((opaque-error (arg) `(error ,arg)))
             (let ((foo1 (make-condition 'error))
                   (foo2 (make-condition 'error)))
               (handler-bind
                   ((error (lambda (c)
                             (let ((restarts (remove 'res (compute-restarts foo1)
                                                     :key #'restart-name
                                                     :test-not #'eql)))
                               (assert (= (length restarts) 1))
                               (invoke-restart (first restarts))))))
                 (restart-case
                     (with-condition-restarts foo1 (list (find-restart 'res))
                       (restart-case
                           (opaque-error foo2)
                         (res () 'int1)
                         (res () 'int2)))
                   (res () 'ext))))))
         'ext))

(assert (eq
         'ext
         (block nil
           (let ((visible nil)
                 (c1 (make-condition 'error))
                 (c2 (make-condition 'error)))
             (handler-bind
                 ((error
                   (lambda (c)
                     (declare (ignore c))
                     (flet ((check-restarts (length)
                              (assert (= length
                                         (length (remove 'foo (compute-restarts c1)
                                                         :key #'restart-name
                                                         :test-not #'eql))))))
                       (check-restarts 1)
                       (setq visible t)
                       (check-restarts 1)
                       (invoke-restart (find-restart 'foo c1))))))
               (restart-case
                   (restart-case
                       (error c2)
                     (foo () 'in1)
                     (foo () :test (lambda (c) (declare (ignore c)) visible)
                          'in2))
                 (foo () 'ext)))))))

;;; First argument of CERROR is a format control
(assert
 (eq (block nil
       (handler-bind
           ((type-error (lambda (c) (return :failed)))
            (simple-error (lambda (c)
                            (return (if (find-restart 'continue)
                                        :passed
                                        :failed)))))
         (cerror (formatter "Continue from ~A") "bug ~A" :bug)))
     :passed))

;;; clauses in HANDLER-CASE are allowed to have declarations (and
;;; indeed, only declarations)
(assert
 (null (handler-case (error "foo") (error () (declare (optimize speed))))))

(handler-case
    (handler-bind ((warning #'muffle-warning))
      (signal 'warning))
  ;; if it's a control error, it had better be printable
  (control-error (c) (format nil "~A" c))
  ;; there had better be an error
  (:no-error (&rest args) (error "No error: ~S" args)))

(handler-case
    (funcall (lambda (x) (check-type x fixnum) x) t)
  (type-error (c)
    (assert (and (subtypep (type-error-expected-type c) 'fixnum)
                 (subtypep 'fixnum (type-error-expected-type c))))
    (assert (eq (type-error-datum c) t)))
  (:no-error (&rest rest) (error "no error: ~S" rest)))

;;; ANSI specifies TYPE-ERROR if datum and arguments of ERROR are not
;;; designators for a condition. Reported by Bruno Haible on cmucl-imp
;;; 2004-10-12.
(flet ((test (&rest args)
         (multiple-value-bind (res err)
             (ignore-errors (apply #'error args))
           (assert (not res))
           (assert (typep err 'type-error))
           (assert (not (nth-value 1 (ignore-errors
                                       (type-error-datum err)))))
           (assert (not (nth-value 1 (ignore-errors
                                       (type-error-expected-type err))))))))
  (test '#:no-such-condition)
  (test nil)
  (test t)
  (test 42)
  (test (make-instance 'standard-object)))

;;; If CERROR is given a condition, any remaining arguments are only
;;; used for the continue format control.
(let ((x 0))
  (handler-bind
      ((simple-error (lambda (c) (incf x) (continue c))))
    (cerror "Continue from ~A at ~A"
            (make-condition 'simple-error :format-control "foo"
                            :format-arguments nil)
            'cerror (get-universal-time))
    (assert (= x 1))))
