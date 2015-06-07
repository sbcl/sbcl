;;;; miscellaneous tests of thread stuff

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

(cl:defpackage #:thread-test
  (:use #:cl #:sb-thread #:sb-ext #:test-util #:assertoid))

(cl:in-package #:thread-test)

(with-test (:name atomic-update
            :skipped-on '(not :sb-thread))
  (let ((x (cons :count 0))
        (nthreads (ecase sb-vm:n-word-bits (32 100) (64 1000))))
    (mapc #'join-thread
          (loop repeat nthreads
             collect (make-thread (lambda ()
                                    (loop repeat 1000
                                       do (atomic-update (cdr x) #'1+)
                                         (sleep 0.00001))))))
    (assert (equal x `(:count ,@(* 1000 nthreads))))))

(with-test (:name mutex-owner)
  ;; Make sure basics are sane on unithreaded ports as well
  (let ((mutex (make-mutex)))
    (grab-mutex mutex)
    (assert (eq *current-thread* (mutex-value mutex)))
    (handler-bind ((warning #'error))
      (release-mutex mutex))
    (assert (not (mutex-value mutex)))))

;;; Terminating a thread that's waiting for the terminal.

#+sb-thread
(let ((thread (make-thread (lambda ()
                             (sb-thread::get-foreground)))))
  (sleep 1)
  (assert (thread-alive-p thread))
  (terminate-thread thread)
  (sleep 1)
  (assert (not (thread-alive-p thread))))

;;; Condition-wait should not be interruptible under WITHOUT-INTERRUPTS

(with-test (:name :without-interrupts+condition-wait
            :skipped-on '(not :sb-thread)
            :fails-on '(and :win32 :sb-futex))
  (let* ((lock (make-mutex))
         (queue (make-waitqueue))
         (thread (make-thread (lambda ()
                                (sb-sys:without-interrupts
                                  (with-mutex (lock)
                                    (condition-wait queue lock)))))))
    (sleep 1)
    (assert (thread-alive-p thread))
    (terminate-thread thread)
    (sleep 1)
    (assert (thread-alive-p thread))
    (condition-notify queue)
    (sleep 1)
    (assert (not (thread-alive-p thread)))))

;;; GRAB-MUTEX should not be interruptible under WITHOUT-INTERRUPTS

(with-test (:name :without-interrupts+grab-mutex :skipped-on '(not :sb-thread))
  (let* ((lock (make-mutex))
         (bar (progn (grab-mutex lock) nil))
         (thread (make-thread (lambda ()
                                (sb-sys:without-interrupts
                                    (with-mutex (lock)
                                      (setf bar t)))))))
    (sleep 1)
    (assert (thread-alive-p thread))
    (terminate-thread thread)
    (sleep 1)
    (assert (thread-alive-p thread))
    (release-mutex lock)
    (sleep 1)
    (assert (not (thread-alive-p thread)))
    (assert (eq :aborted (join-thread thread :default :aborted)))
    (assert bar)))

(with-test (:name :parallel-find-class :skipped-on '(not :sb-thread))
  (let* ((oops nil)
         (threads (loop repeat 10
                        collect (make-thread (lambda ()
                                               (handler-case
                                                   (loop repeat 10000
                                                         do (find-class (gensym) nil))
                                                 (serious-condition ()
                                                   (setf oops t))))))))
    (mapc #'join-thread threads)
    (assert (not oops))))

(with-test (:name :semaphore-multiple-waiters :skipped-on '(not :sb-thread))
  (let ((semaphore (make-semaphore :name "test sem")))
    (labels ((make-readers (n i)
               (values
                (loop for r from 0 below n
                      collect
                      (make-thread
                       (lambda ()
                         (let ((sem semaphore))
                           (dotimes (s i)
                             (wait-on-semaphore sem))))
                       :name "reader"))
                (* n i)))
             (make-writers (n readers i)
               (let ((j (* readers i)))
                 (multiple-value-bind (k rem) (truncate j n)
                   (values
                    (let ((writers
                           (loop for w from 0 below n
                                 collect
                                 (make-thread
                                  (lambda ()
                                    (let ((sem semaphore))
                                      (dotimes (s k)
                                        (signal-semaphore sem))))
                                  :name "writer"))))
                      (assert (zerop rem))
                      writers)
                    (+ rem (* n k))))))
             (test (r w n)
               (multiple-value-bind (readers x) (make-readers r n)
                 (assert (= (length readers) r))
                 (multiple-value-bind (writers y) (make-writers w r n)
                   (assert (= (length writers) w))
                   (assert (= x y))
                   (mapc #'join-thread writers)
                   (mapc #'join-thread readers)
                   (assert (zerop (semaphore-count semaphore)))
                   (values)))))
      (assert
       (eq :ok
           (handler-case
               (sb-ext:with-timeout 10
                 (test 1 1 100)
                 (test 2 2 10000)
                 (test 4 2 10000)
                 (test 4 2 10000)
                 (test 10 10 10000)
                 (test 10 1 10000)
                 :ok)
             (sb-ext:timeout ()
               :timeout)))))))

;;;; Printing waitqueues

(with-test (:name :waitqueue-circle-print :skipped-on '(not :sb-thread))
  (let* ((*print-circle* nil)
         (lock (make-mutex))
         (wq (make-waitqueue)))
    (with-recursive-lock (lock)
      (condition-notify wq))
    ;; Used to blow stack due to recursive structure.
    (assert (princ-to-string wq))))

;;;; SYMBOL-VALUE-IN-THREAD

(with-test (:name :symbol-value-in-thread.1)
  (let ((* (cons t t)))
    (assert (eq * (symbol-value-in-thread '* *current-thread*)))
    (setf (symbol-value-in-thread '* *current-thread*) 123)
    (assert (= 123 (symbol-value-in-thread '* *current-thread*)))
    (assert (= 123 *))))

(with-test (:name :symbol-value-in-thread.2 :skipped-on '(not :sb-thread))
  (let* ((parent *current-thread*)
         (semaphore (make-semaphore))
         (child (make-thread (lambda ()
                               (wait-on-semaphore semaphore)
                               (let ((old (symbol-value-in-thread 'this-is-new parent)))
                                 (setf (symbol-value-in-thread 'this-is-new parent) :from-child)
                                 old)))))
    (progv '(this-is-new) '(42)
      (signal-semaphore semaphore)
      (assert (= 42 (join-thread child)))
      (assert (eq :from-child (symbol-value 'this-is-new))))))

;;; Disabled on Darwin due to deadlocks caused by apparent OS specific deadlocks,
;;; wich _appear_ to be caused by malloc() and free() not being thread safe: an
;;; interrupted malloc in one thread can apparently block a free in another.
(with-test (:name :symbol-value-in-thread.3
            :skipped-on '(not :sb-thread))
  (let* ((parent *current-thread*)
         (semaphore (make-semaphore))
         (running t)
         (noise (make-thread (lambda ()
                               (loop while running
                                     do (setf * (make-array 1024))
                                     ;; Busy-wait a bit so we don't TOTALLY flood the
                                     ;; system with GCs: a GC occurring in the middle of
                                     ;; S-V-I-T causes it to start over -- we want that
                                     ;; to occur occasionally, but not _all_ the time.
                                        (loop repeat (random 128)
                                              do (setf ** *)))))))
    (write-string "; ")
    (dotimes (i #+(or win32 openbsd) 2000
                #-(or win32 openbsd) 15000)
      (when (zerop (mod i 200))
        (write-char #\.)
        (force-output))
      (let* ((mom-mark (cons t t))
             (kid-mark (cons t t))
             (child (make-thread (lambda ()
                                   (wait-on-semaphore semaphore)
                                   (let ((old (symbol-value-in-thread 'this-is-new parent)))
                                     (setf (symbol-value-in-thread 'this-is-new parent)
                                           (make-array 24 :initial-element kid-mark))
                                     old)))))
        (progv '(this-is-new) (list (make-array 24 :initial-element mom-mark))
          (signal-semaphore semaphore)
          (assert (eq mom-mark (aref (join-thread child) 0)))
          (assert (eq kid-mark (aref (symbol-value 'this-is-new) 0))))))
    (setf running nil)
    (join-thread noise)))

(with-test (:name :symbol-value-in-thread.4 :skipped-on '(not :sb-thread))
  (let* ((parent *current-thread*)
         (semaphore (make-semaphore))
         (child (make-thread (lambda ()
                               (wait-on-semaphore semaphore)
                               (symbol-value-in-thread 'this-is-new parent nil)))))
    (signal-semaphore semaphore)
    (assert (equal '(nil nil) (multiple-value-list (join-thread child))))))

(with-test (:name :symbol-value-in-thread.5 :skipped-on '(not :sb-thread))
  (let* ((parent *current-thread*)
         (semaphore (make-semaphore))
         (child (make-thread (lambda ()
                               (wait-on-semaphore semaphore)
                               (handler-case
                                   (symbol-value-in-thread 'this-is-new parent)
                                 (symbol-value-in-thread-error (e)
                                   (list (thread-error-thread e)
                                         (cell-error-name e)
                                         (sb-thread::symbol-value-in-thread-error-info e))))))))
    (signal-semaphore semaphore)
    (assert (equal (list *current-thread* 'this-is-new (list :read :unbound-in-thread))
                   (join-thread child)))))

(with-test (:name :symbol-value-in-thread.6 :skipped-on '(not :sb-thread))
  (let* ((parent *current-thread*)
         (semaphore (make-semaphore))
         (name (gensym))
         (child (make-thread (lambda ()
                               (wait-on-semaphore semaphore)
                               (handler-case
                                   (setf (symbol-value-in-thread name parent) t)
                                 (symbol-value-in-thread-error (e)
                                   (list (thread-error-thread e)
                                         (cell-error-name e)
                                         (sb-thread::symbol-value-in-thread-error-info e))))))))
    (signal-semaphore semaphore)
    (let ((res (join-thread child))
          (want (list *current-thread* name (list :write :no-tls-value))))
      (unless (equal res want)
        (error "wanted ~S, got ~S" want res)))))

(with-test (:name :symbol-value-in-thread.7 :skipped-on '(not :sb-thread))
  (let ((child (make-thread (lambda ())))
        (error-occurred nil))
    (join-thread child)
    (handler-case
        (symbol-value-in-thread 'this-is-new child)
      (symbol-value-in-thread-error (e)
        (setf error-occurred t)
        (assert (eq child (thread-error-thread e)))
        (assert (eq 'this-is-new (cell-error-name e)))
        (assert (equal (list :read :thread-dead)
                       (sb-thread::symbol-value-in-thread-error-info e)))))
    (assert error-occurred)))

(with-test (:name :symbol-value-in-thread.8 :skipped-on '(not :sb-thread))
  (let ((child (make-thread (lambda ())))
        (error-occurred nil))
    (join-thread child)
    (handler-case
        (setf (symbol-value-in-thread 'this-is-new child) t)
      (symbol-value-in-thread-error (e)
        (setf error-occurred t)
        (assert (eq child (thread-error-thread e)))
        (assert (eq 'this-is-new (cell-error-name e)))
        (assert (equal (list :write :thread-dead)
                       (sb-thread::symbol-value-in-thread-error-info e)))))
    (assert error-occurred)))

(with-test (:name :deadlock-detection.1 :skipped-on '(not :sb-thread))
  (loop
    repeat 1000
    do (flet ((test (ma mb sa sb)
                (lambda ()
                  (handler-case
                      (with-mutex (ma)
                        (signal-semaphore sa)
                        (wait-on-semaphore sb)
                        (with-mutex (mb)
                          :ok))
                    (thread-deadlock (e)
                      ;; (assert (plusp (length ...))) prevents
                      ;; flushing.
                      (assert (plusp (length (princ-to-string e))))
                      :deadlock)))))
         (let* ((m1 (make-mutex :name "M1"))
                (m2 (make-mutex :name "M2"))
                (s1 (make-semaphore :name "S1"))
                (s2 (make-semaphore :name "S2"))
                (t1 (make-thread (test m1 m2 s1 s2) :name "T1"))
                (t2 (make-thread (test m2 m1 s2 s1) :name "T2")))
           ;; One will deadlock, and the other will then complete normally.
           (let ((res (list (join-thread t1)
                            (join-thread t2))))
             (assert (or (equal '(:deadlock :ok) res)
                         (equal '(:ok :deadlock) res))))))))

(with-test (:name :deadlock-detection.2 :skipped-on '(not :sb-thread))
  (let* ((m1 (make-mutex :name "M1"))
         (m2 (make-mutex :name "M2"))
         (s1 (make-semaphore :name "S1"))
         (s2 (make-semaphore :name "S2"))
         (t1 (make-thread
              (lambda ()
                (with-mutex (m1)
                  (signal-semaphore s1)
                  (wait-on-semaphore s2)
                  (with-mutex (m2)
                    :ok)))
              :name "T1")))
    (prog (err)
     :retry
       (handler-bind ((thread-deadlock
                       (lambda (e)
                         (unless err
                           ;; Make sure we can print the condition
                           ;; while it's active
                           (let ((*print-circle* nil))
                             (setf err (princ-to-string e)))
                           (go :retry)))))
         (when err
           (sleep 1))
         (assert (eq :ok (with-mutex (m2)
                           (unless err
                             (signal-semaphore s2)
                             (wait-on-semaphore s1)
                             (sleep 1))
                           (with-mutex (m1)
                             :ok)))))
       (assert (stringp err)))
    (assert (eq :ok (join-thread t1)))))

(with-test (:name :deadlock-detection.3  :skipped-on '(not :sb-thread))
  (let* ((m1 (make-mutex :name "M1"))
         (m2 (make-mutex :name "M2"))
         (s1 (make-semaphore :name "S1"))
         (s2 (make-semaphore :name "S2"))
         (t1 (make-thread
              (lambda ()
                (with-mutex (m1)
                  (signal-semaphore s1)
                  (wait-on-semaphore s2)
                  (with-mutex (m2)
                    :ok)))
              :name "T1")))
    ;; Currently we don't consider it a deadlock
    ;; if there is a timeout in the chain.
    (assert (eq :deadline
                (handler-case
                    (with-mutex (m2)
                      (signal-semaphore s2)
                      (wait-on-semaphore s1)
                      (sleep 1)
                      (sb-sys:with-deadline (:seconds 0.1)
                        (with-mutex (m1)
                          :ok)))
                  (sb-sys:deadline-timeout ()
                    :deadline)
                  (thread-deadlock ()
                    :deadlock))))
    (assert (eq :ok (join-thread t1)))))

#+sb-thread
(with-test (:name :pass-arguments-to-thread)
  (assert (= 3 (join-thread (make-thread #'+ :arguments '(1 2))))))

#+sb-thread
(with-test (:name :pass-atom-to-thread)
  (assert (= 1/2 (join-thread (make-thread #'/ :arguments 2)))))

#+sb-thread
(with-test (:name :pass-nil-to-thread)
  (assert (= 1 (join-thread (make-thread #'* :arguments '())))))

#+sb-thread
(with-test (:name :pass-nothing-to-thread)
  (assert (= 1 (join-thread (make-thread #'*)))))

#+sb-thread
(with-test (:name :pass-improper-list-to-thread)
  (multiple-value-bind (value error)
      (ignore-errors (make-thread #'+ :arguments '(1 . 1)))
    (when value
      (join-thread value))
    (assert (and (null value)
                 error))))

(with-test (:name (:wait-for :basics))
  (assert (not (sb-ext:wait-for nil :timeout 0.1)))
  (assert (eql 42 (sb-ext:wait-for 42)))
  (let ((n 0))
    (assert (eql 100 (sb-ext:wait-for (when (= 100 (incf n))
                                        n))))))

(with-test (:name (:wait-for :deadline))
  (assert (eq :ok
              (sb-sys:with-deadline (:seconds 10)
                (assert (not (sb-ext:wait-for nil :timeout 0.1)))
                :ok)))
  (assert (eq :deadline
              (handler-case
                  (sb-sys:with-deadline (:seconds 0.1)
                    (sb-ext:wait-for nil :timeout 10)
                    (error "oops"))
                (sb-sys:deadline-timeout () :deadline)))))

(with-test (:name (:condition-wait :timeout :one-thread))
  (let ((mutex (make-mutex))
        (waitqueue (make-waitqueue)))
    (assert (not (with-mutex (mutex)
                   (condition-wait waitqueue mutex :timeout 0.01))))))

(with-test (:name (:condition-wait :timeout :many-threads)
            :skipped-on '(not :sb-thread))
  (let* ((mutex (make-mutex))
         (waitqueue (make-waitqueue))
         (sem (make-semaphore))
         (data nil)
         (workers
           (loop repeat 100
                 collect (make-thread
                          (lambda ()
                            (wait-on-semaphore sem)
                            (block thread
                              (with-mutex (mutex)
                                (loop until data
                                      do (or (condition-wait waitqueue mutex :timeout 0.01)
                                             (return-from thread nil)))
                                (assert (eq t (pop data)))
                                t)))))))
    (loop repeat 50
          do (with-mutex (mutex)
               (push t data)
               (condition-notify waitqueue)))
    (signal-semaphore sem 100)
    (let ((ok (count-if #'join-thread workers)))
      (unless (eql 50 ok)
        (error "Wanted 50, got ~S" ok)))))

(with-test (:name (wait-on-semaphore :timeout :one-thread))
  (let ((count 10)
        (semaphore (make-semaphore)))
    (signal-semaphore semaphore count)
    (let ((values (loop repeat 100
                     collect (wait-on-semaphore semaphore :timeout 0.001)))
          (expected (loop for i from 9 downto 0 collect i)))
      (assert (equal (remove nil values) expected)))))

(with-test (:name (wait-on-semaphore :timeout :many-threads)
            :skipped-on '(not :sb-thread))
  (let* ((count 10)
         (semaphore (make-semaphore)))
    ;; Add 10 tokens right away.
    (signal-semaphore semaphore count)
    ;; 100 threads try to decrement the semaphore by 1.
    (let ((threads
           (loop repeat 100
              collect (make-thread
                       (lambda ()
                         (sleep (random 0.02))
                         (wait-on-semaphore semaphore :timeout 0.5))))))
      ;; Add 10 more tokens while threads may already be waiting and
      ;; decrementing.
      (loop repeat (floor count 2) do (signal-semaphore semaphore 2))
      ;; 20 threads should have been able to decrement the semaphore
      ;; and obtain an updated count.
      (let ((values (mapcar #'join-thread threads)))
        ;; 20 threads should succeed waiting for the semaphore.
        (assert (= (* 2 count) (count-if-not #'null values)))
        ;; The updated semaphore count should be in [0,19] at all
        ;; times.
        (assert (every (lambda (value) (<= 0 value (1- (* 2 count))))
                       (remove nil values)))
        ;; (At least) one thread should decrease the count to 0.
        (assert (find 0 values))))))

(with-test (:name (:join-thread :timeout)
                  :skipped-on '(not :sb-thread))
  (assert-error
   (join-thread (make-join-thread (lambda () (sleep 10))) :timeout 0.01)
   join-thread-error)
  (let ((cookie (cons t t)))
    (assert (eq cookie
                (join-thread (make-join-thread (lambda () (sleep 10)))
                             :timeout 0.01
                             :default cookie)))))

(with-test (:name (wait-on-semaphore semaphore-notification :lp-1038034)
            :skipped-on '(not :sb-thread)
            :fails-on :sb-thread)
  ;; Test robustness of semaphore acquisition and notification with
  ;; asynchronous thread termination...  Which we know is currently
  ;; fragile.
  (dotimes (run 180)
    (let ((sem (make-semaphore)))
      ;; In CRITICAL, WAIT-ON-SEMAPHORE and SLEEP can be interrupted
      ;; by TERMINATE-THREAD below. But the SIGNAL-SEMAPHORE cleanup
      ;; cannot be interrupted.
      (flet ((critical (sleep)
               (let ((note (make-semaphore-notification)))
                 (sb-sys:without-interrupts
                     (unwind-protect
                          (sb-sys:with-local-interrupts
                            (wait-on-semaphore sem :notification note)
                            (sleep sleep))
                       ;; Re-increment on exit if we decremented it.
                       (when (semaphore-notification-status note)
                         (signal-semaphore sem)))))))
        ;; Create /parallel/ threads trying to acquire and then signal
        ;; the semaphore. Try to asynchronously abort T2 just as T1 is
        ;; exiting.
        (destructuring-bind (t1 t2 t3)
            (loop for i from 1
               for sleep in '(0.01 0.02 0.02)
               collect (make-thread #'critical :arguments sleep
                                    :name (format nil "T~A" i)))
          (signal-semaphore sem)
          (sleep 0.01)
          (ignore-errors
            (terminate-thread t2))
          (flet ((safe-join-thread (thread &key timeout)
                   (assert timeout)
                   (when (eq :timeout
                             (join-thread thread
                                          :timeout timeout
                                          :default :timeout))
                     (error "Hang in (join-thread ~A) ?" thread))))
            (safe-join-thread t1 :timeout 10)
            (safe-join-thread t3 :timeout 10)))))
    (when (zerop (mod run 60))
      (fresh-line)
      (write-string "; "))
    (write-char #\.)
    (force-output)))

(with-test (:name (wait-on-semaphore semaphore-notification)
            :skipped-on '(not :sb-thread))
  (let ((sem (make-semaphore))
        (ok nil)
        (n 0))
    (flet ((critical ()
             (let ((note (make-semaphore-notification)))
               (sb-sys:without-interrupts
                 (unwind-protect
                      (progn
                        (sb-sys:with-local-interrupts
                          (wait-on-semaphore sem :notification note)
                          (sleep (random 0.1)))
                        (incf n))
                   ;; Re-increment on exit if we decremented it.
                   (when (semaphore-notification-status note)
                     (signal-semaphore sem))
                   ;; KLUDGE: Prevent interrupts after this point from
                   ;; unwinding us, so that we can reason about the counts.
                   #+sb-thread
                   (sb-thread::block-deferrable-signals))))))
      (let* ((threads (loop for i from 1 upto 100
                            collect (make-join-thread #'critical :name (format nil "T~A" i))))
             (safe nil)
             (unsafe nil)
             (interruptor (make-thread (lambda ()
                                         (loop until ok)
                                         (let (x)
                                           (dolist (thread threads)
                                             (cond (x
                                                    (push thread unsafe)
                                                    (sleep (random 0.1))
                                                    (ignore-errors
                                                     (terminate-thread thread)))
                                                   (t
                                                    (push thread safe)))
                                             (setf x (not x))))))))
        (signal-semaphore sem)
        (setf ok t)
        (join-thread interruptor)
        (mapc #'join-thread safe)
        (let ((k (count-if (lambda (th)
                             (join-thread th :default nil))
                           unsafe)))
          (assert (= n (+ k (length safe))))
          (assert unsafe))))))

(with-test (:name (wait-on-semaphore :n))
  (let ((semaphore (make-semaphore :count 3)))
    (assert (= 1 (wait-on-semaphore semaphore :n 2)))
    (assert (= 1 (semaphore-count semaphore)))))

(with-test (:name (try-semaphore semaphore-notification)
            :skipped-on '(not :sb-thread))
  (let* ((sem (make-semaphore))
         (note (make-semaphore-notification)))
    (assert (eql nil (try-semaphore sem 1 note)))
    (assert (not (semaphore-notification-status note)))
    (signal-semaphore sem)
    (assert (eql 0 (try-semaphore sem 1 note)))
    (assert (semaphore-notification-status note))))

(with-test (:name (return-from-thread :normal-thread)
            :skipped-on '(not :sb-thread))
  (let ((thread (make-thread (lambda ()
                               (return-from-thread (values 1 2 3))
                               :foo))))
    (assert (equal '(1 2 3) (multiple-value-list (join-thread thread))))))

(with-test (:name (return-from-thread :main-thread))
  (assert (main-thread-p))
  (assert-error (return-from-thread t) thread-error))

(with-test (:name (abort-thread :normal-thread)
            :skipped-on '(not :sb-thread))
  (let ((thread (make-thread (lambda ()
                               (abort-thread)
                               :foo))))
    (assert (equal '(:aborted! :abort)
                   (multiple-value-list
                    (join-thread thread :default :aborted!))))))

(with-test (:name (abort-thread :main-thread))
  (assert (main-thread-p))
  (assert-error (abort-thread) thread-error))

;; SB-THREAD:MAKE-THREAD used to lock SB-THREAD:*MAKE-THREAD-LOCK*
;; before entering WITHOUT-INTERRUPTS. When a thread which was
;; executing SB-THREAD:MAKE-THREAD was interrupted with code which
;; also called SB-THREAD:MAKE-THREAD, it could happen that the first
;; thread already owned SB-THREAD:*MAKE-THREAD-LOCK* and the
;; interrupting code thus made a recursive lock attempt.
;;
;; See (:TIMER :DISPATCH-THREAD :MAKE-THREAD :BUG-1180102) in
;; timer.impure.lisp.
(with-test (:name (make-thread :interrupt-with make-thread :bug-1180102)
            :skipped-on '(not :sb-thread))
  (fresh-line)
  (write-string "; ")
  (force-output)
  (dotimes (i 100)
    (let ((threads '())
          (parent *current-thread*))
      (dotimes (i 100)
        (push (make-thread
               (lambda ()
                 (interrupt-thread
                  parent
                  (lambda () (push (make-thread (lambda ())) threads)))))
              threads)
        (push (make-thread (lambda ())) threads))
      (mapc #'join-thread threads))
    (write-char #\.)
    (force-output)))
