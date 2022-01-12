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

;; No stepper support on some platforms.
#+(or interpreter ppc64 riscv) (invoke-restart 'run-tests::skip-file)

;; These tests should either with code in dynamic space
;; or immobile space, but they only accidentally worked
;; because the default is dynamic space.
;; Make sure they work in the non-default.
;; The issue was that when we elide the move to register
;; of the jump address, there's no register for the stepper
;; to mess with on return from the breakpoint.
#+immobile-code (setq sb-c::*compile-to-memory-space* :immobile)

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
          #-(or x86 x86-64 arm64)
          '(("(FIB 3)" (3))
            ("(< X 2)" :unknown)
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
           '(("(FIB 3)" (3))
             ("(- X 1)" :unknown)
             ("(FIB (1- X))" (2))
             ("(- X 1)" :unknown)
             ("(FIB (1- X))" (1))
             ("(- X 2)" :unknown)
             ("(FIB (- X 2))" (0))
             ("(+ (FIB (1- X)) (FIB (- X 2)))" :unknown)
             ("(- X 2)" :unknown)
             ("(FIB (- X 2))" (1))
             ("(+ (FIB (1- X)) (FIB (- X 2)))" :unknown))
           #+arm64
           '(("(FIB 3)" (3))
             ("(FIB (1- X))" (2))
             ("(FIB (1- X))" (1))
             ("(FIB (- X 2))" (0))
             ("(FIB (- X 2))" (1))))
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
           #-(or x86 x86-64 arm64)
           '(("(FIB 3)" (3))
             ("(< X 2)" :unknown)
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
           '(("(FIB 3)" (3))
             ("(- X 1)" :unknown)
             ("(FIB (1- X))" (2))
             ("(- X 1)" :unknown)
             ("(FIB (1- X))" (1))
             ("(- X 2)" :unknown)
             ("(FIB (- X 2))" (0))
             ("(+ (FIB (1- X)) (FIB (- X 2)))" :unknown)
             ("(- X 2)" :unknown)
             ("(FIB (- X 2))" (1))
             ("(+ (FIB (1- X)) (FIB (- X 2)))" :unknown))
           #+arm64
           '(("(FIB 3)" (3))
             ("(FIB (1- X))" (2))
             ("(FIB (1- X))" (1))
             ("(FIB (- X 2))" (0))
             ("(FIB (- X 2))" (1))))
         (count 0)
         (*stepper-hook* (lambda (condition)
                           (typecase condition
                             (step-form-condition
                              (push (list (step-condition-form condition)
                                          (step-condition-args condition))
                                    results)
                              (if (< (incf count)
                                     #+(or x86 x86-64) 4
                                     #-(or x86 x86-64) 5)
                                  (invoke-restart 'step-into)
                                  (invoke-restart 'step-next)))))))
    (step (fib 3))
    (assert (equal expected (reverse results)))))

(defun test-step-out ()
  (let* ((results nil)
         (expected
           #-(or x86 x86-64 arm64)
           '(("(FIB 3)" (3))
             ("(< X 2)" :unknown)
             ("(- X 1)" :unknown)
             ("(FIB (1- X))" (2))
             ("(< X 2)" :unknown)
             ("(- X 2)" :unknown)
             ("(FIB (- X 2))" (1))
             ("(< X 2)" :unknown)
             ("(+ (FIB (1- X)) (FIB (- X 2)))" :unknown))
           #+(or x86 x86-64)
           '(("(FIB 3)" (3))
             ("(- X 1)" :unknown)
             ("(FIB (1- X))" (2))
             ("(- X 1)" :unknown)
             ("(FIB (1- X))" (1))
             ("(- X 2)" :unknown)
             ("(FIB (- X 2))" (1))
             ("(+ (FIB (1- X)) (FIB (- X 2)))" :unknown))
           #+arm64
           '(("(FIB 3)" (3))
             ("(FIB (1- X))" (2))
             ("(FIB (1- X))" (1))
             ("(FIB (- X 2))" (0))
             ("(FIB (- X 2))" (1))))
         (count 0)
         (*stepper-hook* (lambda (condition)
                           (typecase condition
                             (step-form-condition
                              (push (list (step-condition-form condition)
                                          (step-condition-args condition))
                                    results)
                              (if (= (incf count) 5)
                                  (invoke-restart 'step-out)
                                  (invoke-restart 'step-into)))))))
    (step (fib 3))
    (assert (equal expected (reverse results)))))

(defun test-step-start-from-break ()
  (let* ((results nil)
         (expected
          #-(or x86 x86-64 arm64)
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
            ("(+ (FIB-BREAK (1- X)) (FIB-BREAK (- X 2)))" :unknown))
          #+arm64
          '(("(FIB-BREAK (- X 2))" (0))
            ("(FIB-BREAK (- X 2))" (1))))
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
  (declare (optimize (debug 0)))
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
    (assert (= count #+arm64 2 #-(or x86 x86-64 arm64) 6 #+(or x86 x86-64) 5))))

(defun test-step-backtrace ()
  (let* ((*stepper-hook* (lambda (condition)
                           (typecase condition
                             (step-form-condition
                              (let ((*debug-io* (make-broadcast-stream)))
                                (print-backtrace)))))))
    (step (fib 3))))

(defun test-step-next/2 ()
  (let* ((results nil)
         (expected '(("(OUT)" ())
                     ("(IN)" ())
                     ("(PRINT 1)" (1))
                     ("(PRINT 2)" (2))
                     ("(PRINT 3)" (3))
                     ("(PRINT 4)" (4))))
         (*stepper-hook* (lambda (condition)
                           (typecase condition
                             (step-form-condition
                              (push (list (step-condition-form condition)
                                          (step-condition-args condition))
                                    results)
                              (invoke-restart 'step-into))))))
    (step (out))
    (assert (equal expected (reverse results)))))

(defun test-step-out/2 ()
  (let* ((results nil)
         (expected '(("(OUT)" ())
                     ("(IN)" ())
                     ("(PRINT 1)" (1))
                     ("(PRINT 2)" (2))))
         (count 0)
         (*stepper-hook* (lambda (condition)
                           (typecase condition
                             (step-form-condition
                              (push (list (step-condition-form condition)
                                          (step-condition-args condition))
                                    results)
                              (if (>= (incf count) 4)
                                  (invoke-restart 'step-out)
                                  (invoke-restart 'step-into)))))))
    (step (out))
    (assert (equal expected (reverse results)))))

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
  (test-step-backtrace))

(with-test (:name :step-next/2)
  (test-step-next/2))

(with-test (:name :step-out/2)
  (test-step-out/2))

(with-test (:name :static-fun-step)
  (handler-bind ((step-form-condition
                   (lambda (c)
                     c
                     (invoke-restart 'step-into))))
    (assert (= (step (funcall (checked-compile
                               `(lambda (x) (declare (optimize debug)) (/ 1 x)))
                              2))
               1/2))))
