;;;; This file is for testing the single-stepper.

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

(in-package :cl-user)

;; No stepper support on some platforms.
#-(or x86 x86-64 ppc)
(sb-ext:quit :unix-status 104)

(defun fib (x)
  (declare (optimize debug))
  (if (< x 2)
      1
      (+ (fib (1- x))
         (fib (- x 2)))))

(defvar *cerror-called* nil)

(defun fib-break (x)
  (declare (optimize debug))
  (if (< x 2)
      (progn
        (unless *cerror-called*
          (cerror "a" "b")
          (setf *cerror-called* t))
        1)
      (+ (fib-break (1- x))
         (fib-break (- x 2)))))

(defun test-step-into ()
  (let* ((results nil)
         (expected '(("(< X 2)" :unknown)
                     ("(- X 1)" :unknown)
                     ("(FIB (1- X))" (2))
                     ("(< X 2)" :unknown)
                     ("(- X 1)" :unknown)
                     ("(FIB (1- X))" (1))
                     ("(< X 2)" :unknown)
                     ("(- X 2)" :unknown)
                     ("(FIB (- X 2))" (0))
                     ("(< X 2)" :unknown)
                     ("(+ (FIB (1- X)) (FIB (- X 2)))" :unknown)
                     ("(- X 2)" :unknown)
                     ("(FIB (- X 2))" (1))
                     ("(< X 2)" :unknown)
                     ("(+ (FIB (1- X)) (FIB (- X 2)))" :unknown)))
         (*stepper-hook* (lambda (condition)
                           (typecase condition
                             (step-form-condition
                              (push (list (step-condition-form condition)
                                          (step-condition-args condition))
                                    results)
                              (invoke-restart 'step-into))))))
    (step (fib 3))
    (assert (equal expected (reverse results)))))

(defun test-step-next ()
  (let* ((results nil)
         (expected '(("(< X 2)" :unknown)
                     ("(- X 1)" :unknown)
                     ("(FIB (1- X))" (2))
                     ("(< X 2)" :unknown)
                     ("(- X 1)" :unknown)
                     ("(FIB (1- X))" (1))
                     ("(- X 2)" :unknown)
                     ("(FIB (- X 2))" (0))
                     ("(+ (FIB (1- X)) (FIB (- X 2)))" :unknown)
                     ("(- X 2)" :unknown)
                     ("(FIB (- X 2))" (1))
                     ("(+ (FIB (1- X)) (FIB (- X 2)))" :unknown)))
         (count 0)
         (*stepper-hook* (lambda (condition)
                           (typecase condition
                             (step-form-condition
                              (push (list (step-condition-form condition)
                                          (step-condition-args condition))
                                    results)
                              (if (< (incf count) 4)
                                  (invoke-restart 'step-into)
                                  (invoke-restart 'step-next)))))))
    (step (fib 3))
    (assert (equal expected (reverse results)))))

(defun test-step-out ()
  (let* ((results nil)
         (expected '(("(< X 2)" :unknown)
                     ("(- X 1)" :unknown)
                     ("(FIB (1- X))" (2))
                     ("(< X 2)" :unknown)
                     ("(- X 2)" :unknown)
                     ("(FIB (- X 2))" (1))
                     ("(< X 2)" :unknown)
                     ("(+ (FIB (1- X)) (FIB (- X 2)))" :unknown)))
         (count 0)
         (*stepper-hook* (lambda (condition)
                           (typecase condition
                             (step-form-condition
                              (push (list (step-condition-form condition)
                                          (step-condition-args condition))
                                    results)
                              (if (= (incf count) 4)
                                  (invoke-restart 'step-out)
                                  (invoke-restart 'step-into)))))))
    (step (fib 3))
    (assert (equal expected (reverse results)))))

(defun test-step-start-from-break ()
  (let* ((results nil)
         (expected '(("(- X 2)" :unknown)
                     ("(FIB-BREAK (- X 2))" (0))
                     ("(< X 2)" :unknown)
                     ("(+ (FIB-BREAK (1- X)) (FIB-BREAK (- X 2)))" :unknown)
                     ("(- X 2)" :unknown)
                     ("(FIB-BREAK (- X 2))" (1))
                     ("(< X 2)" :unknown)
                     ("(+ (FIB-BREAK (1- X)) (FIB-BREAK (- X 2)))" :unknown)))
         (count 0)
         (*stepper-hook* (lambda (condition)
                           (typecase condition
                             (step-form-condition
                              (push (list (step-condition-form condition)
                                          (step-condition-args condition))
                                    results)
                              (invoke-restart 'step-into))))))
    (setf *cerror-called* nil)
    (handler-bind ((error
                    (lambda (c)
                      (sb-impl::enable-stepping)
                      (invoke-restart 'continue))))
      (fib-break 3))
    (assert (equal expected (reverse results)))))

(defun test-step-frame ()
  (let* ((count 0)
         (*stepper-hook* (lambda (condition)
                           (typecase condition
                             (step-form-condition
                              (let* ((frame (sb-di::find-stepped-frame))
                                     (dfun (sb-di::frame-debug-fun frame))
                                     (name (sb-di::debug-fun-name dfun)))
                                (assert (equal name 'fib))
                                (incf count)))))))
    (step (fib 3))
    (assert (= count 6))))

(defun test-step-backtrace ()
  (let* ((*stepper-hook* (lambda (condition)
                           (typecase condition
                             (step-form-condition
                              (let ((*debug-io* (make-broadcast-stream)))
                                (backtrace)))))))
    (step (fib 3))))

(handler-bind ((step-condition (lambda (c)
                                 (funcall *stepper-hook* c))))
  (with-test (:name :step-into)
    (test-step-into))
  (with-test (:name :step-next)
    (test-step-next))
  (with-test (:name :step-out)
    (test-step-out))
  (with-test (:name :step-start-from-break)
    (test-step-start-from-break))
  (with-test (:name :step-frame)
    (test-step-frame))
  (with-test (:name :step-backtrace)
    (test-step-backtrace)))



