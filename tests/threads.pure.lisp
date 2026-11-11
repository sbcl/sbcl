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

(with-test (:name :dont-print-array
            :skipped-on (not :sb-thread))
  (let ((thr (sb-thread:make-thread (lambda () (make-array 100)))))
    (sb-thread:join-thread thr)
    (assert (search "#<(SIMPLE-VECTOR" (write-to-string thr)))))

(with-test (:name atomic-update
            :skipped-on (not :sb-thread))
  (let ((x (cons :count 0))
        (nthreads (ecase sb-vm:n-word-bits (32 100) (64 1000))))
    (mapc #'join-thread
          (loop repeat nthreads
             collect (make-thread (lambda ()
                                    (loop repeat 1000
                                       do (atomic-update (cdr x) #'1+))))))
    (assert (equal x `(:count ,@(* 1000 nthreads))))))

(with-test (:name mutex-owner)
  ;; Make sure basics are sane on unithreaded ports as well
  (let ((mutex (make-mutex)))
    (grab-mutex mutex)
    (assert (eq *current-thread* (mutex-owner mutex)))
    (handler-bind ((warning #'error))
      (release-mutex mutex))
    (assert (not (mutex-owner mutex)))))

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
            )
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

(with-test (:name (:condition-wait :timeout :one-thread)
                  :skipped-on :gc-stress)
  (let ((mutex (make-mutex))
        (waitqueue (make-waitqueue)))
    (assert (not (with-mutex (mutex)
                   (condition-wait waitqueue mutex :timeout 0.01))))))

(with-test (:name (:condition-wait :timeout :many-threads)
            :skipped-on (or (not :sb-thread) :gc-stress))
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
            (let ((all-names
                   (loop for filename in (directory "/proc/self/task/*/comm")
                         collect (with-open-file (stream filename) (read-line stream)))))
              (setf (thread-name *current-thread*) "newname")
              (with-open-file (stream (format nil "/proc/self/task/~d/comm"
                                              (thread-os-tid *current-thread*)))
                (list (read-line stream) all-names))))
          :name "testme")))
    (let ((results (join-thread thr)))
      (assert (string= (first results) "newname"))
      (assert (find "finalizer" (second results) :test 'string=))
      (assert (find "testme" (second results) :test 'string=)))))

(with-test (:name :thread-from-tid)
  (let ((tid (sb-thread:thread-os-tid *current-thread*)))
    (when (plusp tid) ; SunOS returns 0
      (assert (eq (sb-thread::%thread-from-tid tid) *current-thread*)))))
