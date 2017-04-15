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

(defun make-quiet-io-stream (&key
                            (input (make-synonym-stream '*standard-input*))
                            (output (make-broadcast-stream)))
  (make-two-way-stream input output))

(defun get-foreground-quietly ()
  (let ((*query-io* (make-quiet-io-stream)))
    (sb-thread::get-foreground)))

;; this used to deadlock on session-lock
(with-test (:name (:no-session-deadlock))
  (make-join-thread (lambda () (sb-ext:gc))))

(with-test (:name (:debugger-no-hang-on-session-lock-if-interrupted)
                  :fails-on :win32)
  #+win32 (error "user would have to touch a key interactively to proceed")
  (sb-debug::enable-debugger)
  (let ((main-thread sb-thread:*current-thread*))
    (make-join-thread
     (lambda ()
       (sleep 2)
       (sb-thread:interrupt-thread
        main-thread (lambda ()
                      (sb-thread::with-interrupts
                        (break))))
       (sleep 2)
       (sb-thread:interrupt-thread main-thread #'continue))
     :name "interruptor"))
  ;; Somewhat verify output.
  (let* ((error-output (make-string-output-stream))
         (debug-output (make-string-output-stream)))
    (let ((*error-output* error-output)
          (*debug-io* (make-quiet-io-stream :output debug-output)))
      (sb-thread::with-session-lock (sb-thread::*session*)
        (sleep 3)))
    (let ((output (get-output-stream-string error-output)))
      (assert (search "debugger invoked" output))
      (assert (search "break" output)))
    (let ((output (get-output-stream-string debug-output)))
      (assert (search "Type HELP for debugger help" output))
      (assert (search "[CONTINUE" output))
      (assert (search "Return from BREAK" output)))))

;;; The sequence of actions in this test creates a situation in which
;;; the list of interactive threads of the current session contains a
;;; single thread.
(with-test (:name (sb-thread::get-foreground :inifite-loop :bug-1682671))
  (let ((state nil)
        (lock (sb-thread:make-mutex :name "get-foreground test lock"))
        (variable (sb-thread:make-waitqueue :name "get-foreground test waitqueue")))
    (flet ((enter-state (new-state)
             (sb-thread:with-mutex (lock)
               (setf state new-state)
               (sb-thread:condition-notify variable)))
           (wait-for-state (goal)
             (sb-thread:with-mutex (lock)
               (loop :until (eq state goal) :do
                  (sb-thread:condition-wait variable lock)))))

      (make-join-thread (lambda ()
                          (enter-state :ready)
                          (get-foreground-quietly)
                          (enter-state :done))
                        :name "get-foreground test thread 2")

      (wait-for-state :ready)
      (sb-thread:release-foreground)

      (wait-for-state :done)
      (get-foreground-quietly))))
