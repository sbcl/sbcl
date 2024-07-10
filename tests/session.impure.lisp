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

#-sb-thread (invoke-restart 'run-tests::skip-file)

(setf sb-unix::*on-dangerous-wait* :error)

(defun make-quiet-io-stream (&key
                            (input (make-synonym-stream '*standard-input*))
                            (output (make-broadcast-stream)))
  (make-two-way-stream input output))

(defun get-foreground-quietly ()
  (let ((*query-io* (make-quiet-io-stream)))
    (sb-thread:get-foreground)))

;; this used to deadlock on session-lock
(with-test (:name (:no-session-deadlock))
  (make-join-thread (lambda () (sb-ext:gc))))

(with-test (:name (:make-thread-while-holding-session-lock))
  (let ((thr1 nil)
        (thr2 nil)
        (sem1 (sb-thread:make-semaphore))
        (sem2 (sb-thread:make-semaphore)))
    (sb-thread::with-session-lock (sb-thread::*session*)
      (setq thr1 (sb-thread:make-thread
                  #'sb-thread:signal-semaphore :arguments sem1)
            thr2 (sb-thread:make-thread
                  #'sb-thread:signal-semaphore :arguments sem2))
      ;; This used to hang right here because threads could not make progress
      ;; in their lisp-side trampoline.
      (sb-thread:wait-on-semaphore sem1)
      (sb-thread:wait-on-semaphore sem2))
    (sb-thread:join-thread thr1)
    (sb-thread:join-thread thr2)))

(with-test (:name (:debugger-no-hang-on-session-lock-if-interrupted)
            :broken-on :win32)
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
(with-test (:name (sb-thread:get-foreground :inifite-loop :bug-1682671))
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

(with-test (:name (sb-thread:release-foreground :bug-1682867))
  (let ((thread (make-join-thread (lambda ())
                                  :name "release-foreground test thread")))
    (sb-thread:release-foreground thread)
    (let ((interactive-threads
           (sb-thread::with-session-lock (sb-thread::*session*)
             (copy-list (sb-thread::interactive-threads)))))
      (assert (not (member sb-thread::*current-thread*
                           interactive-threads))))))

;;; On termination, interactive (including foreground) threads remove
;;; themselves from the list of interactive threads in their
;;; session. However, this did not previously include notifying the
;;; interactive threads waitqueue, resulting in GET-FOREGROUND hanging
;;; after termination of the previous foreground thread.
(with-test (:name (sb-thread:get-foreground :hang :missing-broadcast))
  (let ((thread (make-join-thread
                 (lambda () (sleep 1))
                 :name "get-foreground hang missing-broadcast test")))
    (sb-thread:release-foreground thread)
    (get-foreground-quietly)))

;;; Releasing foreground to an already dead thread previously made the
;;; dead thread the foreground thread. At that point, all succeeding
;;; GET-FOREGROUND calls would just hang.
(with-test (:name (sb-thread:get-foreground :hang :already-dead))
  (let ((thread (sb-thread:make-thread
                 (lambda ())
                 :name "get-foreground hang already-dead test")))
    (sb-thread:join-thread thread)
    (sb-thread:release-foreground thread)
    (get-foreground-quietly)))

(with-test (:name :new-session)
  (let ((old-session sb-thread::*session*))
    (sb-thread:with-new-session ()
      (let ((new-session sb-thread::*session*))
        (assert (not (eq old-session new-session)))
        ;; this thread should not be in session-threads of the old session
        (assert (not (member sb-thread:*current-thread*
                             (sb-thread::session-threads old-session))))
        (assert (member sb-thread:*current-thread*
                        (sb-thread::session-threads new-session)))))))
