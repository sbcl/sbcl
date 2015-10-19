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

#+interpreter (sb-ext:exit :code 104)
;; No stepper support on some platforms.
#-(or x86 x86-64 ppc sparc mips arm)
(sb-ext:exit :code 104)

(defun fib (x)
  (declare (optimize debug))
  (if (< x 2)
      1
      (+ (fib (1- x))
         (fib (- x 2)))))

(defvar *cerror-called* nil)

(define-condition cerror-break (error) ())

(defun fib-break (x)
  (declare (optimize debug))
  (if (< x 2)
      (progn
        (unless *cerror-called*
          (cerror "a" 'cerror-break)
          (setf *cerror-called* t))
        1)
      (+ (fib-break (1- x))
         (fib-break (- x 2)))))

(defun in ()
  (declare (optimize debug))
  (print 1)
  (print 2)
  (print 3)
  (print 4))

(defun out ()
  (declare (optimize debug))
  (in))

(defun test-step-into ()
  (let* ((results nil)
         ;; The generic-< VOP on x86oids doesn't emit a full call
         (expected
          #-(or x86 x86-64)
           '(("(< X 2)" :unknown)
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
             ("(+ (FIB (1- X)) (FIB (- X 2)))" :unknown))
           #+(or x86 x86-64)
           '(("(- X 1)" :unknown)
             ("(FIB (1- X))" (2))
             ("(- X 1)" :unknown)
             ("(FIB (1- X))" (1))
             ("(- X 2)" :unknown)
             ("(FIB (- X 2))" (0))
             ("(+ (FIB (1- X)) (FIB (- X 2)))" :unknown)
             ("(- X 2)" :unknown)
             ("(FIB (- X 2))" (1))
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
         (expected
          #-(or x86 x86-64)
          '(("(< X 2)" :unknown)
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
            ("(+ (FIB (1- X)) (FIB (- X 2)))" :unknown))
          #+(or x86 x86-64)
          '(("(- X 1)" :unknown)
            ("(FIB (1- X))" (2))
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
         (expected
          #-(or x86 x86-64)
          '(("(< X 2)" :unknown)
            ("(- X 1)" :unknown)
            ("(FIB (1- X))" (2))
            ("(< X 2)" :unknown)
            ("(- X 2)" :unknown)
            ("(FIB (- X 2))" (1))
            ("(< X 2)" :unknown)
            ("(+ (FIB (1- X)) (FIB (- X 2)))" :unknown))
          #+(or x86 x86-64)
          '(("(- X 1)" :unknown)
            ("(FIB (1- X))" (2))
            ("(- X 1)" :unknown)
            ("(FIB (1- X))" (1))
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
                              (if (= (incf count) 4)
                                  (invoke-restart 'step-out)
                                  (invoke-restart 'step-into)))))))
    (step (fib 3))
    (assert (equal expected (reverse results)))))

(defun test-step-start-from-break ()
  (let* ((results nil)
         (expected
          #-(or x86 x86-64)
          '(("(- X 2)" :unknown)
            ("(FIB-BREAK (- X 2))" (0))
            ("(< X 2)" :unknown)
            ("(+ (FIB-BREAK (1- X)) (FIB-BREAK (- X 2)))" :unknown)
            ("(- X 2)" :unknown)
            ("(FIB-BREAK (- X 2))" (1))
            ("(< X 2)" :unknown)
            ("(+ (FIB-BREAK (1- X)) (FIB-BREAK (- X 2)))" :unknown))
          #+(or x86 x86-64)
          '(("(- X 2)" :unknown)
            ("(FIB-BREAK (- X 2))" (0))
            ("(+ (FIB-BREAK (1- X)) (FIB-BREAK (- X 2)))" :unknown)
            ("(- X 2)" :unknown)
            ("(FIB-BREAK (- X 2))" (1))
            ("(+ (FIB-BREAK (1- X)) (FIB-BREAK (- X 2)))" :unknown)))
         (*stepper-hook* (lambda (condition)
                           (typecase condition
                             (step-form-condition
                              (push (list (step-condition-form condition)
                                          (step-condition-args condition))
                                    results)
                              (invoke-restart 'step-into))))))
    (setf *cerror-called* nil)
    (handler-bind ((cerror-break
                    (lambda (c)
                      (declare (ignore c))
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
                                (incf count)
                                (invoke-restart 'step-next)))))))
    (step (fib 3))
    (assert (= count #-(or x86 x86-64) 6 #+(or x86 x86-64) 5))))

(defun test-step-backtrace ()
  (let* ((*stepper-hook* (lambda (condition)
                           (typecase condition
                             (step-form-condition
                              (let ((*debug-io* (make-broadcast-stream)))
                                (print-backtrace)))))))
    (step (fib 3))))

(defun test-step-next/2 ()
  (let* ((results nil)
         (expected '(("(IN)" ())
                     ("(PRINT 1)" (1))
                     ("(PRINT 2)" (2))
                     ("(PRINT 3)" (3))
                     ("(PRINT 4)" (4))))
         (count 0)
         (*stepper-hook* (lambda (condition)
                           (typecase condition
                             (step-form-condition
                              (push (list (step-condition-form condition)
                                          (step-condition-args condition))
                                    results)
                              (if (>= (incf count) 3)
                                  (invoke-restart 'step-into)
                                  (invoke-restart 'step-into)))))))
    (step (out))
    (assert (equal expected (reverse results)))))

(defun test-step-out/2 ()
  (let* ((results nil)
         (expected '(("(IN)" ())
                     ("(PRINT 1)" (1))
                     ("(PRINT 2)" (2))))
         (count 0)
         (*stepper-hook* (lambda (condition)
                           (typecase condition
                             (step-form-condition
                              (push (list (step-condition-form condition)
                                          (step-condition-args condition))
                                    results)
                              (if (>= (incf count) 3)
                                  (invoke-restart 'step-out)
                                  (invoke-restart 'step-into)))))))
    (step (out))
    (assert (equal expected (reverse results)))))

(with-test (:name :step-into)
  (handler-bind ((step-condition #'sb-impl::invoke-stepper))
    (test-step-into)))

(with-test (:name :step-next)
  (handler-bind ((step-condition #'sb-impl::invoke-stepper))
      (test-step-next)))

(with-test (:name :step-out)
  (handler-bind ((step-condition #'sb-impl::invoke-stepper))
    (test-step-out)))

(with-test (:name :step-start-from-break)
  (handler-bind ((step-condition #'sb-impl::invoke-stepper))
    (test-step-start-from-break)))

(with-test (:name :step-frame)
  (handler-bind ((step-condition #'sb-impl::invoke-stepper))
    (test-step-frame)))

(with-test (:name :step-backtrace)
  (handler-bind ((step-condition #'sb-impl::invoke-stepper))
    (test-step-backtrace)))

(with-test (:name :step-next/2)
  (handler-bind ((step-condition #'sb-impl::invoke-stepper))
    (test-step-next/2)))

(with-test (:name :step-out/2)
  (handler-bind ((step-condition #'sb-impl::invoke-stepper))
    (test-step-out/2)))
