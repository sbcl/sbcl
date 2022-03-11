(defmacro assert-timeout ((expected-message) form)
  (let ((ok (gensym "OK")))
    `(let ((,ok ',ok))
       (unless (eq ,ok
                   (handler-case ,form
                     (timeout (condition)
                       (assert (string= ,expected-message
                                        (princ-to-string condition)))
                       ,ok)))
         (error "No timeout from form:~%  ~S" ',form)))))

(defun run-sleep (seconds)
  (sb-ext:run-program "sleep" (list (format nil "~D" seconds))
                      :search t :wait t))

(with-test (:name (sb-sys:decode-timeout :large-values :lp-1727789))
  (flet ((test (seconds)
           (assert (not (sb-sys:decode-timeout seconds)))))
    (test (expt 2 64))
    (test (1+ sb-kernel:internal-seconds-limit))
    (test (float (1+ sb-kernel:internal-seconds-limit) 1.0f0))
    (test (float (1+ sb-kernel:internal-seconds-limit) 1.0d0))))

(with-test (:name (sb-sys:with-deadline :large-values :lp-1727789))
  (flet ((test (seconds)
           (let ((sem (sb-thread:make-semaphore :count 0)))
             (assert-timeout ("A deadline was reached after 0 seconds.")
               (sb-sys:with-deadline (:seconds seconds)
                 (sb-sys:with-deadline (:seconds 0)
                   (sb-thread:wait-on-semaphore sem)))))))
    (test (1+ most-positive-fixnum))
    (test (1+ sb-kernel:internal-seconds-limit))
    (test (float (1+ sb-kernel:internal-seconds-limit) 1.0f0))
    (test (float (1+ sb-kernel:internal-seconds-limit) 1.0d0))))

(with-test (:name (:deadline sb-ext:run-program :trivial) :fails-on :win32)
  (assert-timeout ("A deadline was reached after 1 second.")
    (sb-sys:with-deadline (:seconds 1)
      (run-sleep 3))))

(with-test (:name (:deadline sb-sys:defer-deadline 1) :fails-on :win32)
  (let ((n 0))
    (assert-timeout ("A deadline was reached after 0.1 seconds.")
      (handler-bind ((sb-sys:deadline-timeout
                      (lambda (c)
                        (when (< n 2)
                          (incf n)
                          (sb-sys:defer-deadline 0.1 c)))))
        (sb-sys:with-deadline (:seconds 1)
          (run-sleep 2))))
    (assert (= n 2))))

(with-test (:name (:deadline sb-sys:defer-deadline 2) :fails-on :win32)
  (let ((n 0)
        (final nil))
    (handler-case
        (handler-bind ((sb-sys:deadline-timeout
                        (lambda (c)
                          (incf n)
                          (sb-sys:defer-deadline 0.1 c))))
          (sb-sys:with-deadline (:seconds 1)
            (run-sleep 2)))
      (sb-sys:deadline-timeout (c)
        (setf final c)))
    (assert (plusp n))
    (assert (not final))))

(with-test (:name (:deadline sb-sys:defer-deadline 3) :fails-on :win32)
  (let ((n 0))
    (assert-timeout ("A deadline was reached after 0.1 seconds.")
      (handler-bind ((sb-sys:deadline-timeout
                      (lambda (condition)
                        (declare (ignore condition))
                        (when (< n 2)
                          (incf n)
                          (invoke-restart 'sb-sys:defer-deadline)))))
        (sb-sys:with-deadline (:seconds .1)
          (run-sleep 3))))
    (assert (plusp n))))

(with-test (:name (:deadline sb-sys:cancel-deadline) :fails-on :win32)
  (let ((n 0)
        (final nil))
    (handler-case
        (handler-bind ((sb-sys:deadline-timeout
                        (lambda (c)
                          (incf n)
                          (sb-sys:cancel-deadline c))))
          (sb-sys:with-deadline (:seconds 1)
            (run-sleep 2)))
      (sb-sys:deadline-timeout (c)
        (setf final c)))
    (assert (= n 1))
    (assert (not final))))

(with-test (:name (:deadline sb-thread:grab-mutex)
                  :skipped-on (not :sb-thread))
  (assert-timeout ("A deadline was reached after 1 second.")
    (let ((lock (sb-thread:make-mutex))
          (waitp t))
      (make-join-thread (lambda ()
                          (sb-thread:grab-mutex lock)
                          (setf waitp nil)
                          (sleep 5)))
      (loop while waitp do (sleep 0.01))
      (sb-sys:with-deadline (:seconds 1)
        (sb-thread:grab-mutex lock)))))

(with-test (:name (:deadline sb-thread:wait-on-semaphore)
                  :skipped-on (not :sb-thread))
  (assert-timeout ("A deadline was reached after 1 second.")
    (let ((sem (sb-thread:make-semaphore :count 0)))
      (sb-sys:with-deadline (:seconds 1)
        (sb-thread:wait-on-semaphore sem)))))

(with-test (:name (:deadline sb-thread:join-thread)
            :skipped-on (not :sb-thread)
            :broken-on :win32)
  (assert-timeout ("A deadline was reached after 1 second.")
    (sb-sys:with-deadline (:seconds 1)
      (sb-thread:join-thread
       (make-kill-thread (lambda () (loop (sleep 1))))))))

(with-test (:name (:deadline :futex-wait-eintr)
            :skipped-on (not :sb-thread)
            :broken-on :win32)
  (let ((lock (sb-thread:make-mutex))
        (waitp t))
    (make-join-thread (lambda ()
                        (sb-thread:grab-mutex lock)
                        (setf waitp nil)
                        (sleep 5)))
    (loop while waitp do (sleep 0.01))
    (let ((thread (make-join-thread
                   (lambda ()
                     (let ((start (get-internal-real-time)))
                       (handler-case
                           (sb-sys:with-deadline (:seconds 1)
                             (sb-thread:grab-mutex lock))
                         (sb-sys:deadline-timeout (x)
                           (declare (ignore x))
                           (let ((end (get-internal-real-time)))
                             (float (/ (- end start)
                                       internal-time-units-per-second)
                                    0.0)))))))))
      (sleep 0.3)
      (sb-thread:interrupt-thread thread (lambda () 42))
      (let ((seconds-passed (sb-thread:join-thread thread)))
        (assert (< seconds-passed 1.2))))))

;;;; Sleep

(with-test (:name (sb-sys:with-deadline sleep :smoke))
  (assert-timeout ("A deadline was reached after 0.1 seconds.")
    (sb-sys:with-deadline (:seconds .1) (sleep 1)))

  (assert-no-signal
      (sb-sys:with-deadline (:seconds .2) (sleep .1))
    sb-sys:deadline-timeout))

(with-test (:name (sb-sys:with-deadline sleep :long-sleep))
  (assert-timeout ("A deadline was reached after 0.1 seconds.")
    (sb-sys:with-deadline (:seconds .1)
      (sleep (1+ sb-kernel:internal-seconds-limit)))))

(with-test (:name (sb-sys:with-deadline sleep :no-sleep))
  ;; When SLEEP is called in the context of an expired deadline, the
  ;; DEADLINE-TIMEOUT must be signaled even if there is no sleeping to
  ;; be done.
  (assert-timeout ("A deadline was reached after 0.1 seconds.")
    (sb-sys:with-deadline (:seconds .1)
      (let ((sb-impl::*deadline* nil)) (sleep .2))
      (sleep 0))))

(with-test (:name (sb-sys:with-deadline sleep sb-sys:defer-deadline))
  (let ((n 0))
    (assert-no-signal
        (handler-bind ((sb-sys:deadline-timeout
                        (lambda (condition)
                          (incf n)
                          (sb-sys:defer-deadline .1 condition))))
          (sb-sys:with-deadline (:seconds .1) (sleep .5)))
      sb-sys:deadline-timeout)
    (assert (plusp n))))

(with-test (:name (sb-sys:with-deadline sleep sb-sys:cancel-deadline))
  (assert-no-signal
      (handler-bind ((sb-sys:deadline-timeout
                      (lambda (condition)
                        (sb-sys:cancel-deadline condition))))
        (sb-sys:with-deadline (:seconds .1) (sleep 1)))
    sb-sys:deadline-timeout))

(defmacro timestamp (string &optional mutex)
  (declare (ignorable string mutex))
  #+nil
  `(let ((s ,string))
     (sb-sys:with-pinned-objects (s)
       ,(if mutex
            `(alien-funcall (extern-alien "lisp_mutex_event1" (function void system-area-pointer system-area-pointer))
                            (sb-sys:vector-sap s)
                            (sb-sys:vector-sap (or (mutex-name ,mutex) "unnamed")))
            `(alien-funcall (extern-alien "lisp_mutex_event" (function void system-area-pointer))
                            (sb-sys:vector-sap s))))))

;;; I observed (at least) four different ways this next test can flake:
;;;
;;; 1. one of the first CONDITION-WAITs at the initial deadline can be spuriously woken.
;;;    This is easy to see:
;;;      Thread A            Thread B
;;;      --------            --------
;;;      mutex.acquire
;;;      cv.wait
;;;        set wakeup token
;;;        mutex.release
;;;                           mutex.acquire
;;;                           cv.wait
;;;                             set wakeup token
;;;                             mutex.release
;;;                             sys_futex
;;;        sys_futex
;;;        (* no waiting - "wrong" token *)
;;;    So this causes the (:UNKNOWN :UNKNOWN) result for one thread,
;;;    because it never enters its deadline handler and because the customary
;;;    ("obligatory" one might say) loop around condition-wait is absent.
;;;    This is so common that it is no longer considered a failure.
;;;
;;; 2. one of the threads could be scheduled entirely before the other-
;;;    that is, it runs from start to finish before the other thread does anything.
;;;    This causes them both to conclude they acquired the mutex,
;;;    though the deadline handlers were completely nonoverlapping.
;;;
;;; 3. one of the calls to SYS_futex with a timeout can wait significantly more than
;;;    requested. This essentially reduces to case 2 but in a more convoluted way,
;;;    which I've somewhat mitigated this by scaling up the timeouts so that we're
;;;    not skating at the edge of the granularity the system is willing to give you.
;;;    This mode of flakiness is likely to happen on a cloud compute environment.
;;;
;;;  4. the deadline handler can be invoked twice in one thread because
;;;     - I don't know why.
;;;
;;; Here's an example with MUTEX_EVENTRECORDING #defined that was
;;; considered a "failure":
;;; [0.000000000] main thread: try-mutex: acquired 'Make-Thread Lock'
;;; [0.000038478] main thread: released 'Make-Thread Lock'
;;; [0.000053675] main thread: try-mutex: acquired 'Make-Thread Lock'
;;; [0.000068255] main thread: released 'Make-Thread Lock'
;;; [0.000069415] main thread: try-mutex: acquired 'gate sem mutex'
;;; [0.000069781] main thread: released 'gate sem mutex'
;;; [0.000071536] main thread: start futex wait 'thread result lock'
;;; [0.000094761] B: try-mutex: acquired 'gate sem mutex'
;;; [0.000095494] B: released 'gate sem mutex'
;;; [0.000096646] B: deadline established
;;; [0.000097026] B: try-mutex: acquired 'testmut'
;;; [0.000097931] B: released 'testmut'
;;; [0.000098251] B: start futex timedwait 'testcv' timeout 300000
;;; [0.300161614] B: back from sys_futex 'testcv'
;;; [0.300163763] B: try-mutex: acquired 'testmut'
;;; [0.300172833] B: enter deadline handler
;;; [0.300173184] B: start sleep
;;; [0.601937423] A: try-mutex: acquired 'gate sem mutex'
;;; [0.601940795] A: released 'gate sem mutex'
;;; [0.601945635] A: deadline established
;;; [0.601952083] A: start futex timedwait 'testmut' timeout 300000
;;; [0.900264670] B: end sleep
;;; [0.900266124] B: waking futex 'testcv'
;;; [0.900280645] B: waking futex 'testmut'
;;; [0.900285623] B: released 'testmut'
;;; [0.900290563] B: try-mutex: acquired 'thread interruptions lock'
;;; [0.900290777] B: released 'thread interruptions lock'
;;; [0.900292127] B: try-mutex: acquired 'session lock'
;;; [0.900293214] B: released 'session lock'
;;; [0.900293703] B: released 'thread result lock'
;;; [0.900294381] A: back from sys_futex 'testmut'
;;; [0.900296267] A: %wait-for-mutex: acquired 'testmut'
;;; [0.900299208] A: waking futex 'testmut'
;;; [0.900300544] A: released 'testmut'
;;; [0.900301101] A: start futex timedwait 'testcv' timeout 1000
;;; [0.901358710] A: back from sys_futex 'testcv'
;;; [0.901359680] A: try-mutex: acquired 'testmut'
;;; [0.901369935] A: enter deadline handler
;;; [0.901370203] A: start sleep
;;; [1.501418757] A: end sleep
;;; [1.501419570] A: waking futex 'testcv'
;;; [1.501456896] A: released 'testmut'
;;; [1.501460896] A: try-mutex: acquired 'thread interruptions lock'
;;; [1.501461171] A: released 'thread interruptions lock'
;;; [1.501462039] A: try-mutex: acquired 'session lock'
;;; [1.501463001] A: released 'session lock'
;;; [1.501463544] A: waking futex 'thread result lock'
;;; [1.501465881] A: released 'thread result lock'
;;; [1.501474477] main thread: back from sys_futex 'thread result lock'
;;; [1.501476447] main thread: %wait-for-mutex: acquired 'thread result lock'
;;; [1.501477283] main thread: waking futex 'thread result lock'
;;; [1.501478429] main thread: released 'thread result lock'
;;; [1.501479467] main thread: try-mutex: acquired 'thread result lock'
;;; [1.501479600] main thread: released 'thread result lock'
;;;
;;; Basically I give up trying to fix this test any more.
(defun signal-dealine-check-interrupt-enablement ()
  (let ((mutex (sb-thread:make-mutex :name "testmut"))
        (waitq (sb-thread:make-waitqueue :name "testcv"))
        (A-holds? :unknown)
        (B-holds? :unknown)
        (A-interrupts-enabled? :unknown)
        (B-interrupts-enabled? :unknown)
        (gate (sb-thread:make-semaphore :name "testsem"))
        (initial-deadline .3)
        (sleep-amount .6)
        (A)
        (B))
    ;; The gate semaphore is just a half-assed attempt to trigger both worker threads
    ;; to start running their bodies at the exact same time. If they don't do that,
    ;; then it's entirely possible that one thread gets run start to finish before
    ;; the other thread does anything at all.  So obviously the test will fail.
    (let ((m (sb-thread::semaphore-mutex gate)))
      (setf (sb-thread:mutex-name m) "gate sem mutex"))
    (let ((cv (sb-thread::semaphore-queue gate)))
      (setf (sb-thread:waitqueue-name cv) "gate sem cv"))
    #+nil (alien-funcall (extern-alien "lisp_mutex_start_eventrecording" (function void)))
    (setq A (sb-thread:make-thread
             (lambda ()
               (sb-thread:wait-on-semaphore gate)
               (handler-bind
                   ((sb-sys:deadline-timeout
                     (lambda (c)
                       (timestamp "enter deadline handler")
                       ;; We came here through the call to DECODE-TIMEOUT
                       ;; in CONDITION-WAIT; hence both here are supposed
                       ;; to evaluate to T.
                       (setq A-holds? (sb-thread:holding-mutex-p mutex))
                       (setq A-interrupts-enabled?
                             sb-sys:*interrupts-enabled*)
                       (timestamp "start sleep")
                       (sleep sleep-amount)
                       (timestamp "end sleep")
                       (sb-thread:condition-broadcast waitq)
                       (sb-sys:defer-deadline 10.0 c)
                       (timestamp "leave deadline handler"))))
                 (sb-sys:with-deadline (:seconds initial-deadline)
                   (timestamp "deadline established")
                   (sb-thread:with-mutex (mutex)
                     (sb-thread:condition-wait waitq mutex)))))
             :name "A"))
    (setq B (sb-thread:make-thread
             (lambda ()
               (sb-thread:wait-on-semaphore gate)
               (handler-bind
                   ((sb-sys:deadline-timeout
                     (lambda (c)
                       (timestamp "enter deadline handler")
                       ;; We came here through the call to DECODE-TIMEOUT
                       ;; in CONDITION-WAIT (contended case of
                       ;; reaquiring the mutex) - so the former will
                       ;; be NIL, but interrupts should still be enabled.
                       (setq B-holds? (sb-thread:holding-mutex-p mutex))
                       (setq B-interrupts-enabled?
                             sb-sys:*interrupts-enabled*)
                       (timestamp "start sleep")
                       (sleep sleep-amount)
                       (timestamp "end sleep")
                       (sb-thread:condition-broadcast waitq)
                       (sb-sys:defer-deadline 10.0 c)
                       (timestamp "leave deadline handler"))))
                 (sb-sys:with-deadline (:seconds initial-deadline)
                   (timestamp "deadline established")
                   (sb-thread:with-mutex (mutex)
                     (sb-thread:condition-wait waitq mutex)))))
             :name "B"))
    (sb-thread:signal-semaphore gate 3)
    (sb-thread:join-thread A)
    (sb-thread:join-thread B)
    #+nil (alien-funcall (extern-alien "lisp_mutex_done_eventrecording" (function void)))
    (let ((A-result (list A-holds? A-interrupts-enabled?))
          (B-result (list B-holds? B-interrupts-enabled?)))
      ;; We also check some subtle behaviour w.r.t. whether a deadline
      ;; handler in CONDITION-WAIT got the mutex, or not. This is most
      ;; probably very internal behaviour (so user should not depend
      ;; on it) -- I added the testing here just to manifest current
      ;; behaviour.
      (cond ((equal A-result '(t t))
             (or (equal B-result '(nil t))
                 (equal B-result '(:unknown :unknown))
                 (error "thread B result: ~s" B-result)))
            ((equal B-result '(t t))
             (or (equal A-result '(nil t))
                 (equal A-result '(:unknown :unknown))
                 (error "thread A result: ~s" A-result)))
            (t
             (error "Failure: fell through with A: ~S, B: ~S"
                    A-result
                    B-result))))))

;;; Test from git rev 5e55f426de8fa579a0d6cfbfb3ac5433d530d3c9 formerly in threads.impure
(test-util:with-test (:name (:deadline :interrupts-enabled) :skipped-on (:not :sb-thread))
  ;; I cranked this up to 1000 (and ran it on lots of machines) to get examples of failure,
  ;; but that's more than a reasonable amount of time to spend in the test.
  (loop repeat 2 do (signal-dealine-check-interrupt-enablement)))
