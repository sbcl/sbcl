;;;; This file is for testing UNWIND-TO-FRAME-AND-CALL, used for
;;;; implementing RESTART-FRAME and RETURN-FROM-FRAME in the debugger.

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

;;; The debugger doesn't have any native knowledge of the interpreter
(when (eq sb-ext:*evaluator-mode* :interpret)
  (sb-ext:exit :code 104))

(declaim (optimize debug))

(defun return-from-frame (frame-name &rest values)
  (let ((frame (sb-di::top-frame)))
    (loop until (equal frame-name
                       (sb-debug::frame-call frame))
          do (setf frame (sb-di::frame-down frame)))
    (assert frame)
    (assert (sb-debug::frame-has-debug-tag-p frame))
    (sb-debug::unwind-to-frame-and-call frame
                                        (lambda ()
                                          (values-list values)))))

(defun restart-frame (frame-name)
  (let ((frame (sb-di::top-frame)))
    (loop until (equal (sb-debug::frame-call frame)
                       frame-name)
          do (setf frame (sb-di::frame-down frame)))
    (assert frame)
    (assert (sb-debug::frame-has-debug-tag-p frame))
    (let* ((call-list (sb-debug::frame-call-as-list frame))
           (fun (fdefinition (car call-list))))
      (sb-debug::unwind-to-frame-and-call frame
                                          (lambda ()
                                            (apply fun (cdr call-list)))))))

(defvar *foo*)
(defvar *a*)
(defvar *b*)
(defvar *c*)


;;;; Test RESTART-FRAME

(define-condition restart-condition () ())

(defvar *count* 0)

(defun restart/special (*foo*)
  (incf *count*)
  (unless *a*
    (setf *a* t)
    (signal 'restart-condition))
  *foo*)

(defun restart/optional-special (&optional (*foo* 1))
  (incf *count*)
  (unless *a*
    (setf *a* t)
    (signal 'restart-condition))
  *foo*)

(defun restart/normal (foo)
  (incf *count*)
  (unless *a*
    (setf *a* t)
    (signal 'restart-condition))
  foo)

#+win32
(defun decline ()
  ;; these tests currently fail no matter whether threads are enabled or
  ;; not, but on threaded builds the failure mode is particularly
  ;; unfortunate.  As a workaround, opt out of running the test.
  #+sb-thread
  (error "this test fails with exception 0xc0000029 ~
          (STATUS_INVALID_UNWIND_TARGET), from which we cannot currently ~
          recover"))

(defun test-restart (name)
  #+win32 (decline)
  (setf *a* nil)
  (let ((*foo* 'x))
    (let ((*foo* 'y)
          (*count* 0))
      (handler-bind ((restart-condition (lambda (c)
                                          (declare (ignore c))
                                          (restart-frame name))))
        (assert (eql (funcall name 1) 1))
        (assert (eql *count* 2))))
    ;; Check that the binding stack was correctly unwound.
    (assert (eql *foo* 'x))))

(with-test (:name (:restart-frame :special) :fails-on :win32)
  (test-restart 'restart/special))

(with-test (:name (:restart-frame :optional-special) :fails-on :win32)
  (test-restart 'restart/optional-special))

(with-test (:name (:restart-frame :normal) :fails-on :win32)
  (test-restart 'restart/normal))


;;;; Test RETURN-FROM-FRAME with normal functions

(define-condition return-condition () ())

(defun return/special (*foo*)
  (unless *a*
    (setf *a* t)
    (signal 'return-condition))
  *foo*)

(defun return/optional-special (&optional (*foo* 1))
  (unless *a*
    (setf *a* t)
    (signal 'return-condition))
  *foo*)

(defun return/normal (foo)
  (unless *a*
    (setf *a* t)
    (signal 'return-condition))
  foo)

(defun do-signal ()
  (signal 'return-condition))

(defun return/catch (foo)
  (catch 'y
    (do-signal))
  foo)

(defun test-return (name)
  #+win32 (decline)
  (setf *a* nil)
  (let ((*foo* 'x))
    (let ((*foo* 'y))
      (handler-bind ((return-condition (lambda (c)
                                          (declare (ignore c))
                                          (return-from-frame name 1 2 3 4))))
        (assert (equal (multiple-value-list (funcall name 0))
                       (list 1 2 3 4)))))
    ;; Check that the binding stack was correctly unwound.
    (assert (eql *foo* 'x))))

(with-test (:name (:return-from-frame :special) :fails-on :win32)
  (test-return 'return/special))

(with-test (:name (:return-from-frame :optional-special) :fails-on :win32)
  (test-return 'return/optional-special))

(with-test (:name (:return-from-frame :normal) :fails-on :win32)
  (test-return 'return/normal))

(defun throw-y () (throw 'y 'y))

;; Check that *CURRENT-CATCH-BLOCK* was correctly restored.
(with-test (:name :current-catch-block-restored :fails-on :win32)
  (assert (eql (catch 'y
                 (test-return 'return/catch)
                 (throw-y))
               'y)))


;;;; Test RETURN-FROM-FRAME with local functions

(define-condition in-a () ())
(define-condition in-b () ())

(defun locals ()
  (flet ((a ()
           (signal 'in-a)
           (values 1 2))
         (b ()
           (signal 'in-b)
           1))
    (setf *a* (multiple-value-list (a)))
    (setf *b* (multiple-value-list (b)))))

(defun hairy-locals ()
  (let ((*c* :bad))
    (flet ((a (&optional *c*)
             (signal 'in-a)
             (values 1 2))
           (b (&key *c*)
             (signal 'in-b)
             1))
      ;; Ensure that A and B actually appear in the backtrace; the
      ;; compiler for some reason likes to optimize away single-use
      ;; local functions with hairy lambda-lists even on high debug
      ;; levels.
      (setf *a* (a :good))
      (setf *b* (b :*c* :good))
      ;; Do the real tests
      (setf *a* (multiple-value-list (a :good)))
      (setf *b* (multiple-value-list (b :*c* :good))))))

(defun test-locals (name)
  #+win32 (decline)
  (handler-bind ((in-a (lambda (c)
                         (declare (ignore c))
                         (return-from-frame `(flet a :in ,name) 'x 'y)))
                 (in-b (lambda (c)
                         (declare (ignore c))
                         (return-from-frame `(flet b :in ,name) 'z))))
    (funcall name))
  ;; We're intentionally not testing for returning a different amount
  ;; of values than the local functions are normally returning. It's
  ;; hard to think of practical cases where that'd be useful, but
  ;; allowing it (as in the old fully CATCH-based implementation of
  ;; UNWIND-TO-FRAME-AND-CALL) will make it harder for the compiler to
  ;; work well.
  (let ((*foo* 'x))
    (let ((*foo* 'y))
      (assert (equal *a* '(x y)))
      (assert (equal *b* '(z))))
    (assert (eql *foo* 'x))))

(with-test (:name (:return-from-frame :local-function) :fails-on :win32)
  (test-locals 'locals))

(with-test (:name (:return-from-frame :hairy-local-function) :fails-on :win32)
  (test-locals 'hairy-locals))


;;;; Test RETURN-FROM-FRAME with anonymous functions

(define-condition anon-condition () ())

(defparameter *anon-1*
  (lambda (foo)
    (signal 'anon-condition)
    foo))

(defparameter *anon-2*
  (lambda (*foo*)
    (signal 'anon-condition)
    *foo*))

(defun make-anon-3 ()
  (let ((a (lambda (foo)
             (signal 'anon-condition)
             foo)))
    (funcall a 1)
    a))

(defun make-anon-4 ()
  (let ((a (lambda (*foo*)
             (signal 'anon-condition)
             *foo*)))
    (funcall a 1)
    a))

(defparameter *anon-3* (make-anon-3))
(defparameter *anon-4* (make-anon-4))

(defun test-anon (fun var-name &optional in)
  #+win32 (decline)
  (handler-bind ((anon-condition (lambda (c)
                                   (declare (ignore c))
                                   (return-from-frame
                                    `(lambda (,var-name) ,@(when in `(:in ,in)))
                                    'x 'y))))
    (let ((*foo* 'x))
      (let ((*foo* 'y))
        (assert (equal (multiple-value-list (funcall fun 1))
                       '(x y)))
        (assert (eql *foo* 'y)))
      (assert (eql *foo* 'x)))))

(defvar *p* (namestring (if sb-c::*merge-pathnames* *load-truename* *load-pathname*)))

(with-test (:name (:return-from-frame :anonymous :toplevel) :fails-on :win32)
  (test-anon *anon-1* 'foo *p*))

(with-test (:name (:return-from-frame :anonymous :toplevel-special)
                  :fails-on :win32)
  (test-anon *anon-2* '*foo* *p*))

(with-test (:name (:return-from-frame :anonymous) :fails-on :win32)
  (test-anon *anon-3* 'foo 'make-anon-3))

(with-test (:name (:return-from-frame :anonymous :special) :fails-on :win32)
  (test-anon *anon-4* '*foo* 'make-anon-4))


;;;; Test that unwind cleanups are executed

(defvar *unwind-state* nil)
(defvar *signal* nil)

(defun unwind-1 ()
  (unwind-protect
       (when *signal*
         (signal 'return-condition))
    (push :unwind-1 *unwind-state*)))

(defun unwind-2 ()
  (unwind-protect
       (unwind-1)
    (push :unwind-2 *unwind-state*)))

(defun test-unwind (fun wanted)
  #+win32 (decline)
  (handler-bind ((return-condition (lambda (c)
                                     (declare (ignore c))
                                     (return-from-frame fun
                                                        'x 'y))))
    (dolist (*signal* (list nil t))
      (let ((*foo* 'x)
            (*unwind-state* nil))
        (let ((*foo* 'y))
          (if *signal*
              (assert (equal (multiple-value-list (funcall fun))
                             '(x y)))
              (funcall fun))
          (assert (equal *unwind-state* wanted))
          (assert (eql *foo* 'y)))
        (assert (eql *foo* 'x))))))

(with-test (:name :test-unwind-1 :fails-on :win32)
  (test-unwind 'unwind-1 '(:unwind-1)))
(with-test (:name :test-unwind-2 :fails-on :win32)
  (test-unwind 'unwind-2 '(:unwind-2 :unwind-1)))

;;; Regression in 1.0.10.47 reported by James Knight

(defun inner1 (tla)
  (zerop tla))

(declaim (inline inline-fun))
(defun inline-fun (tla)
  (or (inner1 tla)
      (inner1 tla)))

(defun foo (predicate)
  (funcall predicate 2))

(defun test ()
  (let ((blah (foo #'inline-fun)))
    (inline-fun 3)))

(with-test (:name (:debug-instrumentation :inline/xep))
  (test))

