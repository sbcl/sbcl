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


;;;; STRUCTURAL TESTS

(shadowing-import 'assertoid:assert-error)
(use-package "SB-THREAD")
(use-package "SB-SYS")

(setf sb-unix::*on-dangerous-wait* :error)

(with-test (:name (:threads :trivia))
  (assert (eq *current-thread*
              (find (thread-name *current-thread*) (list-all-threads)
                    :key #'thread-name :test #'equal)))

  (assert (thread-alive-p *current-thread*)))

(with-test (:name (with-mutex :basics))
  (let ((mutex (make-mutex)))
    (with-mutex (mutex)
      mutex)))

(sb-alien:define-alien-routine "check_deferrables_blocked_or_lose"
    void
  (where sb-alien:unsigned-long))
(sb-alien:define-alien-routine "check_deferrables_unblocked_or_lose"
    void
  (where sb-alien:unsigned-long))

(with-test (:name (interrupt-thread :basics :no-unwinding))
  (let ((a 0))
    (interrupt-thread *current-thread* (lambda () (setq a 1)))
    (process-all-interrupts)
    (assert (eql a 1))))

(with-test (:name (interrupt-thread :deferrables-blocked))
  (interrupt-thread *current-thread*
                    (lambda ()
                      ;; Make sure sb-ext:gc doesn't leave the
                      ;; deferrables unblocked
                      (sb-ext:gc)
                      (check-deferrables-blocked-or-lose 0)))
  (process-all-interrupts))

(with-test (:name (interrupt-thread :deferrables-unblocked))
  (interrupt-thread *current-thread*
                    (lambda ()
                      (with-interrupts
                        (check-deferrables-unblocked-or-lose 0))))
  (process-all-interrupts))

(with-test (:name (interrupt-thread :nlx))
  (catch 'xxx
    (interrupt-thread *current-thread*
                      (lambda ()
                        (check-deferrables-blocked-or-lose 0)
                        (throw 'xxx nil)))
    (process-all-interrupts))
  (check-deferrables-unblocked-or-lose 0))

#-sb-thread (invoke-restart 'run-tests::skip-file)

;;;; Now the real tests...

(with-test (:name (with-mutex :timeout)
            :broken-on :gc-stress)
  (let ((m (make-mutex)))
    (with-mutex (m)
      (assert (null (join-thread (make-thread
                                  (lambda ()
                                    (with-mutex (m :timeout 0.1)
                                      t)))))))
    (assert (join-thread (make-thread
                          (lambda ()
                            (with-mutex (m :timeout 0.1)
                              t)))))))

;;; compare-and-swap

(defmacro defincf (name accessor &rest args)
  `(defun ,name (x)
     (let* ((old (,accessor x ,@args))
            (new (1+ old)))
       (loop until (eq old (sb-ext:compare-and-swap (,accessor x ,@args) old new))
             do (setf old (,accessor x ,@args)
                      new (1+ old)))
       new)))

(defstruct cas-struct (slot 0))

(defincf incf-car car)
(defincf incf-cdr cdr)
(defincf incf-slot cas-struct-slot)
(defincf incf-symbol-value symbol-value)
(defincf incf-svref/1 svref 1)
(defincf incf-svref/0 svref 0)

(macrolet
    ((def (name init incf op)
       `(with-test (:name ,name)
          (let* ((n 200000)
                 (x ,init)
                 (run (sb-thread:make-semaphore))
                 (threadcount 10)
                 (threads
                  (loop repeat threadcount
                        collect (make-thread
                                 (lambda ()
                                   (sb-thread:wait-on-semaphore run)
                                   (loop repeat n do (,incf x)))))))
            (sb-thread:signal-semaphore run threadcount)
            (map nil #'join-thread threads)
            (assert (= (,op x) (* threadcount n)))))))

  (def (cas car) (cons 0 nil) incf-car car)
  (def (cas cdr) (cons nil 0) incf-cdr cdr)
  (def (cas :slot) (make-cas-struct) incf-slot cas-struct-slot)
  (def (cas :value)
      (let ((x '.x.))
        (set x 0)
        x)
    incf-symbol-value
    symbol-value)
  (def (cas :svref/0) (vector 0 nil) incf-svref/0 (lambda (x) (svref x 0)))
  (def (cas :svref/1) (vector nil 0) incf-svref/1 (lambda (x) (svref x 1))))

(with-test (:name (:threads :more-trivia))
  (let ((old-threads (list-all-threads))
        (thread (make-thread
                 (lambda ()
                   ;; I honestly have no idea what this is testing.
                   ;; It seems to be nothing more than an implementation change detector test.
                   (assert (sb-thread::avl-find
                            (sb-thread::thread-primitive-thread sb-thread:*current-thread*)
                            sb-thread::*all-threads*))
                   (sleep 2))))
        (new-threads (list-all-threads)))
    (assert (thread-alive-p thread))
    ;; there is no order guarantee
    ;;(assert (eq thread (first new-threads)))
    (assert (= (1+ (length old-threads)) (length new-threads)))
    (sleep 3)
    (assert (not (thread-alive-p thread)))))

(with-test (:name (join-thread :abort :default))
  (let* ((sym (gensym))
         (thread (make-thread (lambda () (abort-thread)))))
    (assert (equal (multiple-value-list
                    (join-thread thread :default sym))
                   (list sym :abort)))))

(with-test (:name (join-thread :abort :error))
  (assert-error (join-thread (make-thread (lambda () (abort-thread))))
                join-thread-error))

(with-test (:name (join-thread :timeout :default))
  (let* ((sym (gensym))
         (sem (make-semaphore))
         (thread (make-thread (lambda () (wait-on-semaphore sem)))))
    (assert (equal (multiple-value-list
                    (join-thread thread :timeout .001 :default sym))
                   (list sym :timeout)))
    (signal-semaphore sem)
    (assert (join-thread thread))))

(with-test (:name (join-thread :timeout :error))
  (let* ((sem (make-semaphore))
         (thread (make-thread (lambda () (wait-on-semaphore sem)))))
    (assert-error (join-thread thread :timeout .001) join-thread-error)
    (signal-semaphore sem)
    (assert (join-thread thread))))

(with-test (:name (join-thread :multiple-values))
  (assert (equal '(1 2 3)
                 (multiple-value-list
                  (join-thread (make-thread (lambda () (values 1 2 3))))))))

;; Used to signal a SIMPLE-ERROR about a recursive lock attempt.
(with-test (:name (join-thread :self-join))
  (assert-error (join-thread *current-thread*) join-thread-error))

;;; elementary "can we get a lock and release it again"
(with-test (:name (:mutex :basics))
  (let ((l (make-mutex :name "foo"))
        (p *current-thread*))
    (assert (eql (mutex-owner l) nil) nil "1")
    (grab-mutex l)
    (assert (eql (mutex-owner l) p) nil "3")
    (release-mutex l)
    (assert (eql (mutex-owner l) nil) nil "5")))

(with-test (:name (with-recursive-lock :basics))
  (labels ((ours-p (value)
             (eq *current-thread* value)))
    (let ((l (make-mutex :name "rec")))
      (assert (eql (mutex-owner l) nil) nil "1")
      (with-recursive-lock (l)
        (assert (ours-p (mutex-owner l)) nil "3")
        (with-recursive-lock (l)
          (assert (ours-p (mutex-owner l)) nil "4"))
        (assert (ours-p (mutex-owner l)) nil "5"))
      (assert (eql (mutex-owner l) nil) nil "6"))))

(with-test (:name (with-recursive-lock :wait-p))
  (let ((m (make-mutex)))
    (with-mutex (m)
      (assert (null (join-thread (make-thread
                                  (lambda ()
                                    (with-recursive-lock (m :wait-p nil)
                                      t)))))))
    (assert (join-thread (make-thread
                          (lambda ()
                            (with-recursive-lock (m :wait-p nil)
                              t)))))))

(with-test (:name (with-recursive-lock :wait-p :recursive))
  (let ((m (make-mutex)))
    (assert (join-thread (make-thread
                          (lambda ()
                            (with-recursive-lock (m :wait-p nil)
                              (with-recursive-lock (m :wait-p nil)
                                t))))))))

(with-test (:name (with-recursive-lock :timeout)
                  :broken-on :gc-stress)
  (let ((m (make-mutex)))
    (with-mutex (m)
      (assert (null (join-thread (make-thread
                                  (lambda ()
                                    (with-recursive-lock (m :timeout 0.1)
                                      t)))))))
    (assert (join-thread (make-thread
                          (lambda ()
                            (with-recursive-lock (m :timeout 0.1)
                              t)))))))

(with-test (:name (with-recursive-lock :timeout :recursive))
  (let ((m (make-mutex)))
    (assert (join-thread (make-thread
                          (lambda ()
                            (with-recursive-lock (m :timeout 0.1)
                              (with-recursive-lock (m :timeout 0.1)
                                t))))))))

(with-test (:name (:mutex :nesting-mutex-and-recursive-lock))
  (let ((l (make-mutex :name "a mutex")))
    (with-mutex (l)
      (with-recursive-lock (l)))))

(with-test (:name (condition-wait :basics-1)
                  :skipped-on :gc-stress)
  (let ((queue (make-waitqueue :name "queue"))
        (lock (make-mutex :name "lock"))
        (n 0))
    (labels ((in-new-thread ()
               (with-mutex (lock)
                 (assert (eql (mutex-owner lock) *current-thread*))
                 (format t "~A got mutex~%" *current-thread*)
                 ;; now drop it and sleep
                 ;; FIXME: condition-wait returning doesn't mean there was condition-notify
                 (condition-wait queue lock)
                 ;; after waking we should have the lock again
                 (assert (eql (mutex-owner lock) *current-thread*))
                 (assert (eql n 1))
                 (decf n))))
      (make-join-thread #'in-new-thread)
      (sleep 2)            ; give it  a chance to start
      ;; check the lock is free while it's asleep
      (format t "parent thread ~A~%" *current-thread*)
      (assert (eql (mutex-owner lock) nil))
      (with-mutex (lock)
        (incf n)
        (condition-notify queue))
      (sleep 1))))

(with-test (:name (condition-wait :basics-2)
                  :skipped-on :gc-stress)
  (let ((queue (make-waitqueue :name "queue"))
        (lock (make-mutex :name "lock")))
    (labels ((ours-p (value)
               (eq *current-thread* value))
             (in-new-thread ()
               (with-recursive-lock (lock)
                 (assert (ours-p (mutex-owner lock)))
                 (format t "~A got mutex~%" (mutex-owner lock))
                 ;; now drop it and sleep
                 ;; FIXME: condition-wait returning doesn't mean there was condition-notify
                 (condition-wait queue lock)
                 ;; after waking we should have the lock again
                 (format t "woken, ~A got mutex~%" (mutex-owner lock))
                 (assert (ours-p (mutex-owner lock))))))
      (make-join-thread #'in-new-thread)
      (sleep 2)            ; give it  a chance to start
      ;; check the lock is free while it's asleep
      (format t "parent thread ~A~%" *current-thread*)
      (assert (eql (mutex-owner lock) nil))
      (with-recursive-lock (lock)
        (condition-notify queue))
      (sleep 1))))

;;; GRAB-MUTEX

(with-test (:name (grab-mutex :waitp nil))
  (let ((m (make-mutex)))
    (with-mutex (m)
      (assert (null (join-thread (make-thread
                                  #'(lambda ()
                                      (grab-mutex m :waitp nil)))))))))

(with-test (:name (grab-mutex :timeout :acquisition-fail))
  (let ((m (make-mutex))
        (w (make-semaphore)))
    (with-mutex (m)
      (let ((th (make-thread
                 #'(lambda ()
                     (prog1
                         (grab-mutex m :timeout 0.1)
                       (signal-semaphore w))))))
        ;; Wait for it to -- otherwise the detect the deadlock chain
        ;; from JOIN-THREAD.
        (wait-on-semaphore w)
        (assert (null (join-thread th)))))))

(with-test (:name (grab-mutex :timeout :acquisition-success)
            :skipped-on :gc-stress)
  (let ((m (make-mutex))
        (child))
    (with-mutex (m)
      (setq child (make-thread #'(lambda () (grab-mutex m :timeout 1.0))))
      (sleep 0.2))
    (assert (eq (join-thread child) 't))))

(with-test (:name (grab-mutex :timeout+deadline :lp-1727789))
  (flet ((test (deadline)
           (let ((m (make-mutex))
                 (w (make-semaphore)))
             (with-mutex (m)
               (let ((th (make-thread #'(lambda ()
                                          (sb-sys:with-deadline (:seconds 0.0)
                                            (handler-case
                                                (grab-mutex m :timeout deadline)
                                              (sb-sys:deadline-timeout ()
                                                (signal-semaphore w)
                                                :deadline)))))))
                 (wait-on-semaphore w)
                 (assert (eq (join-thread th) :deadline)))))))
    (test 0.0)
    (test 10000000000000000000000)))

(with-test (:name (grab-mutex :waitp+deadline))
  (let ((m (make-mutex)))
    (with-mutex (m)
      (assert (eq (join-thread
                   (make-thread #'(lambda ()
                                    (sb-sys:with-deadline (:seconds 0.0)
                                      (handler-case
                                          (grab-mutex m :waitp nil)
                                        (sb-sys:deadline-timeout ()
                                          :deadline))))))
                  'nil)))))

