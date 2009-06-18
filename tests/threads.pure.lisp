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

(in-package :cl-user)

(defpackage :thread-test
  (:use :cl :sb-thread))

(in-package :thread-test)

(use-package :test-util)

(with-test (:name mutex-owner)
  ;; Make sure basics are sane on unithreaded ports as well
  (let ((mutex (make-mutex)))
    (get-mutex mutex)
    (assert (eq *current-thread* (mutex-value mutex)))
    (handler-bind ((warning #'error))
      (release-mutex mutex))
    (assert (not (mutex-value mutex)))))

(with-test (:name spinlock-owner)
  ;; Make sure basics are sane on unithreaded ports as well
  (let ((spinlock (sb-thread::make-spinlock)))
    (sb-thread::get-spinlock spinlock)
    (assert (eq *current-thread* (sb-thread::spinlock-value spinlock)))
    (handler-bind ((warning #'error))
      (sb-thread::release-spinlock spinlock))
    (assert (not (sb-thread::spinlock-value spinlock)))))

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

#+sb-thread
(with-test (:name without-interrupts+condition-wait
            :fails-on :sb-lutex)
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

;;; GET-MUTEX should not be interruptible under WITHOUT-INTERRUPTS

#+sb-thread
(with-test (:name without-interrupts+get-mutex)
  (let* ((lock (make-mutex))
         (bar (progn (get-mutex lock) nil))
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

#+sb-thread
(with-test (:name parallel-find-class)
  (let* ((oops nil)
         (threads (loop repeat 10
                        collect (make-thread (lambda ()
                                               (handler-case
                                                   (loop repeat 10000
                                                         do (find-class (gensym) nil))
                                                 (serious-condition ()
                                                   (setf oops t))))))))
    (mapcar #'sb-thread:join-thread threads)
    (assert (not oops))))

#+sb-thread
(with-test (:name :semaphore-multiple-waiters)
  (let ((semaphore (make-semaphore :name "test sem")))
    (labels ((make-readers (n i)
               (values
                (loop for r from 0 below n
                      collect
                      (sb-thread:make-thread
                       (lambda ()
                         (let ((sem semaphore))
                           (dotimes (s i)
                             (sb-thread:wait-on-semaphore sem))))
                       :name "reader"))
                (* n i)))
             (make-writers (n readers i)
               (let ((j (* readers i)))
                 (multiple-value-bind (k rem) (truncate j n)
                   (values
                    (let ((writers
                           (loop for w from 0 below n
                                 collect
                                 (sb-thread:make-thread
                                  (lambda ()
                                    (let ((sem semaphore))
                                      (dotimes (s k)
                                        (sb-thread:signal-semaphore sem))))
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
                   (mapc #'sb-thread:join-thread writers)
                   (mapc #'sb-thread:join-thread readers)
                   (assert (zerop (sb-thread:semaphore-count semaphore)))
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

;;;; SYMBOL-VALUE-IN-THREAD

(with-test (:name symbol-value-in-thread.1)
  (let ((* (cons t t)))
    (assert (eq * (symbol-value-in-thread '* *current-thread*)))
    (setf (symbol-value-in-thread '* *current-thread*) 123)
    (assert (= 123 (symbol-value-in-thread '* *current-thread*)))
    (assert (= 123 *))))

#+sb-thread
(with-test (:name symbol-value-in-thread.2)
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

#+sb-thread
(with-test (:name symbol-value-in-thread.3)
  (let* ((parent *current-thread*)
         (semaphore (make-semaphore))
         (running t)
         (noise (make-thread (lambda ()
                               (loop while running
                                     do (setf * (make-array 1024)))))))

    (loop repeat 10000
          do (let* ((mom-mark (cons t t))
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

#+sb-thread
(with-test (:name symbol-value-in-thread.4)
  (let* ((parent *current-thread*)
         (semaphore (make-semaphore))
         (child (make-thread (lambda ()
                               (wait-on-semaphore semaphore)
                               (symbol-value-in-thread 'this-is-new parent nil)))))
    (signal-semaphore semaphore)
    (assert (equal '(nil nil) (multiple-value-list (join-thread child))))))

#+sb-thread
(with-test (:name symbol-value-in-thread.5)
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
    (assert (equal (list *current-thread* 'this-is-new (list :read :unbound))
                   (join-thread child)))))

#+sb-thread
(with-test (:name symbol-value-in-thread.6)
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
          (want (list *current-thread* name (list :write :unbound))))
      (unless (equal res want)
        (error "wanted ~S, got ~S" want res)))))

#+sb-thread
(with-test (:name symbol-value-in-thread.7)
  (let ((child (make-thread (lambda ()))))
    (handler-case
        (symbol-value-in-thread 'this-is-new child)
      (symbol-value-in-thread-error (e)
        (assert (eq child (thread-error-thread e)))
        (assert (eq 'this-is-new (cell-error-name e)))
        (assert (equal (list :read :dead) (sb-thread::symbol-value-in-thread-error-info e)))))))

#+sb-thread
(with-test (:name symbol-value-in-thread.8)
  (let ((child (make-thread (lambda ()))))
    (handler-case
        (setf (symbol-value-in-thread 'this-is-new child) t)
      (symbol-value-in-thread-error (e)
        (assert (eq child (thread-error-thread e)))
        (assert (eq 'this-is-new (cell-error-name e)))
        (assert (equal (list :write :dead) (sb-thread::symbol-value-in-thread-error-info e)))))))
