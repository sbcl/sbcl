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
