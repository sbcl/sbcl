;;;; session-related tests

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;
;;;; This software is in the public domain and is provided with
;;;; absoluely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(cl:in-package #:cl-user)

(cl:use-package '#:test-util)
(cl:use-package '#:assertoid)

(setf sb-unix::*on-dangerous-wait* :error)

;; this used to deadlock on session-lock
(with-test (:name (:no-session-deadlock))
  (make-join-thread (lambda () (sb-ext:gc))))

(with-test (:name (:debugger-no-hang-on-session-lock-if-interrupted)
                  :fails-on :win32)
  #+win32 (error "user would have to touch a key interactively to proceed")
  (sb-debug::enable-debugger)
  (let* ((main-thread sb-thread:*current-thread*)
         (interruptor-thread
          (sb-thread:make-thread
           (lambda ()
             (sleep 2)
             (sb-thread:interrupt-thread
              main-thread (lambda ()
                            (sb-thread::with-interrupts
                              (break))))
             (sleep 2)
             (sb-thread:interrupt-thread main-thread #'continue))
           :name "interruptor")))
    (sb-thread::with-session-lock (sb-thread::*session*)
      (sleep 3))
    (loop while (sb-thread:thread-alive-p interruptor-thread))))
