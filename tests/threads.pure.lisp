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

(use-package '("SB-EXT" "SB-THREAD"))

(with-test (:name atomic-update
            :skipped-on (not :sb-thread))
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
    (assert (eq *current-thread* (mutex-owner mutex)))
    (handler-bind ((warning #'error))
      (release-mutex mutex))
    (assert (not (mutex-owner mutex)))))

;;; Terminating a thread that's waiting for the terminal.

(with-test (:name (:terminate-thread :get-foreground)
                  :skipped-on (not :sb-thread)
                  :broken-on :win32)
 (let ((thread (make-thread (lambda ()
                              (sb-thread::get-foreground)))))
   (sleep 1)
   (assert (thread-alive-p thread))
   (terminate-thread thread)
   (sleep 1)
   (assert (not (thread-alive-p thread)))))

;;; Condition-wait should not be interruptible under WITHOUT-INTERRUPTS
;;; BUT: Such a claim is without much merit. Even if a wait is not "interrupted",
;;; the very definition of spurious wakeup is that return from the wait happens
;;; for ANY reason - users of condition variables must ALWAYS anticipate needing
;;; to loop over a condition-wait.

(with-test (:name :without-interrupts+condition-wait
            :skipped-on (not :sb-thread)
            :broken-on :win32)
  (let* ((lock (make-mutex))
         (queue (make-waitqueue))
         (actually-wakeup nil)
         (thread (make-thread (lambda ()
                                (sb-sys:without-interrupts
                                  (with-mutex (lock)
                                    (loop
                                     (condition-wait queue lock)
                                     (if actually-wakeup (return)))))))))
    (sleep .25)
    (assert (thread-alive-p thread))
    ;; this is the supposed "interrupt that doesn't interrupt",
    ;; but it _is_ permitted to wake the condition variable.
    (terminate-thread thread)
    (sleep .5)
    (assert (thread-alive-p thread))
    (setq actually-wakeup t)
    (sb-thread:barrier (:write))
    (condition-notify queue)
    (sleep .25)
    (assert (not (thread-alive-p thread)))))

;;; GRAB-MUTEX should not be interruptible under WITHOUT-INTERRUPTS

(with-test (:name :without-interrupts+grab-mutex
            :skipped-on (not :sb-thread)
            :broken-on :win32)
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

(with-test (:name :parallel-find-class :skipped-on (not :sb-thread))
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

(with-test (:name :semaphore-multiple-waiters :skipped-on (not :sb-thread))
  (let ((semaphore (make-semaphore :name "test sem")))
    (labels ((make-readers (n i)
               (values
                (loop for r from 0 below n
                      collect
                      (make-thread
                       (lambda ()
                         (sb-ext:with-timeout 10
                           (let ((sem semaphore))
                             (dotimes (s i)
                               (wait-on-semaphore sem)))))
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
                                    (sb-ext:with-timeout 10
                                      (let ((sem semaphore))
                                        (dotimes (s k)
                                          (signal-semaphore sem)))))
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
           (sb-ext:with-timeout 20
             (test 1 1 100)
             (test 2 2 10000)
             (test 4 2 10000)
             (test 4 2 10000)
             (test 10 10 10000)
             (test 10 1 10000)
             :ok))))))

;;;; Printing waitqueues

(with-test (:name :waitqueue-circle-print :skipped-on (not :sb-thread))
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

(with-test (:name :symbol-value-in-thread.2 :skipped-on (not :sb-thread))
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

(with-test (:name :symbol-value-in-thread.3
            :skipped-on (not :sb-thread)
            :broken-on :sb-safepoint)
  (let* ((parent *current-thread*)
         (semaphore (make-semaphore))
         (running t)
         (noise (make-thread (lambda ()
                               (loop while running
                                     do (setf * (make-array 1024))
                                     ;; Busy-wait a bit so we don't TOTALLY flood the
                                     ;; system with GCs.
                                        (loop repeat (random 128)
                                              do (setf ** *)))))))
    (dotimes (i 500)
      (let* ((mom-mark (cons t t))
             (kid-mark (cons t t))
             (child (make-thread
                     (lambda ()
                       (if (wait-on-semaphore semaphore :timeout 10)
                           (let ((old (symbol-value-in-thread 'this-is-new parent)))
                             (setf (symbol-value-in-thread 'this-is-new parent)
                                   (make-array 24 :initial-element kid-mark))
                             old)
                           :timeout)))))
        (progv '(this-is-new) (list (make-array 24 :initial-element mom-mark))
          (signal-semaphore semaphore)
          (assert (eq mom-mark (aref (join-thread child :timeout 10) 0)))
          (assert (eq kid-mark (aref (symbol-value 'this-is-new) 0))))))
    (setf running nil)
    (join-thread noise)))

(with-test (:name :symbol-value-in-thread.4 :skipped-on (not :sb-thread))
  (let* ((parent *current-thread*)
         (semaphore (make-semaphore))
         (child (make-thread (lambda ()
                               (wait-on-semaphore semaphore)
                               (symbol-value-in-thread 'this-is-new parent nil)))))
    (signal-semaphore semaphore)
    (assert (equal '(nil nil) (multiple-value-list (join-thread child))))))

(with-test (:name :symbol-value-in-thread.5 :skipped-on (not :sb-thread))
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
    (assert (equal (list *current-thread* 'this-is-new (list :read :no-tls-value))
                   (join-thread child)))))

(with-test (:name :symbol-value-in-thread.6 :skipped-on (not :sb-thread))
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

(with-test (:name :symbol-value-in-thread.7 :skipped-on (not :sb-thread))
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

(with-test (:name :symbol-value-in-thread.8 :skipped-on (not :sb-thread))
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
            :skipped-on (not :sb-thread))
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
            :skipped-on (not :sb-thread))
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

(with-test (:name (wait-on-semaphore semaphore-notification :lp-1038034)
            :skipped-on (not :sb-thread)
            :broken-on :sb-safepoint)
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
            (safe-join-thread t1 :timeout 60)
            (safe-join-thread t3 :timeout 60)))))
    (when (zerop (mod run 60))
      (fresh-line)
      (write-string "; "))
    (write-char #\.)
    (force-output)))

(with-test (:name (wait-on-semaphore :n))
  (let ((semaphore (make-semaphore :count 3)))
    (assert (= 1 (wait-on-semaphore semaphore :n 2)))
    (assert (= 1 (semaphore-count semaphore)))))

(with-test (:name (try-semaphore semaphore-notification)
            :skipped-on (not :sb-thread))
  (let* ((sem (make-semaphore))
         (note (make-semaphore-notification)))
    (assert (eql nil (try-semaphore sem 1 note)))
    (assert (not (semaphore-notification-status note)))
    (signal-semaphore sem)
    (assert (eql 0 (try-semaphore sem 1 note)))
    (assert (semaphore-notification-status note))))

(with-test (:name (return-from-thread :normal-thread)
            :skipped-on (not :sb-thread))
  (let ((thread (make-thread (lambda ()
                               (return-from-thread (values 1 2 3))
                               :foo))))
    (assert (equal '(1 2 3) (multiple-value-list (join-thread thread))))))

(with-test (:name (return-from-thread :main-thread))
  (assert (main-thread-p))
  (assert-error (return-from-thread t) thread-error))

(with-test (:name (abort-thread :normal-thread)
            :skipped-on (not :sb-thread))
  (let ((thread (make-thread (lambda ()
                               (abort-thread)
                               :foo))))
    (assert (equal '(:aborted! :abort)
                   (multiple-value-list
                    (join-thread thread :default :aborted!))))))

(with-test (:name (abort-thread :main-thread))
  (assert (main-thread-p))
  (assert-error (abort-thread) thread-error))

;;; The OSes vary in how pthread_setname works.
;;; According to https://stackoverflow.com/questions/2369738/how-to-set-the-name-of-a-thread-in-linux-pthreads
;;;   // NetBSD: name + arg work like printf(name, arg)
;;;   int pthread_setname_np(pthread_t thread, const char *name, void *arg);
;;;   // FreeBSD & OpenBSD: function name is slightly different, and has no return value
;;;   void pthread_set_name_np(pthread_t tid, const char *name);
;;;   // Mac OS X: must be set from within the thread (can't specify thread ID)
;;;   int pthread_setname_np(const char*);
;;; Only Linux is implemented for now.
(with-test (:name :os-thread-name :skipped-on (:not (and :linux :sb-thread)))
  (let ((thr
         (make-thread
          (lambda ()
            (with-open-file (stream (format nil "/proc/self/task/~d/comm"
                                            (thread-os-tid *current-thread*)))
              (read-line stream)))
          :name "testme")))
    (assert (string= (join-thread thr) "testme"))))
