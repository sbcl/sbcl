#-sb-thread (sb-ext:exit :code 104)

(import '(sb-thread:join-thread
          sb-thread:make-mutex
          sb-thread:make-semaphore
          sb-thread:make-thread
          sb-thread:signal-semaphore
          sb-thread:thread-deadlock
          sb-thread:wait-on-semaphore
          sb-thread:with-mutex))

(when (sb-sys:find-dynamic-foreign-symbol-address "show_gc_generation_throughput")
  (setf (extern-alien "show_gc_generation_throughput" int) 0))

(with-test (:name :deadlock-detection.1)
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

(with-test (:name :deadlock-detection.2)
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

(with-test (:name :deadlock-detection.3)
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

(with-test (:name (:deadlock-detection :interrupts)
            :broken-on :win32)
  (let* ((m1 (sb-thread:make-mutex :name "M1"))
         (m2 (sb-thread:make-mutex :name "M2"))
         (t1-can-go (sb-thread:make-semaphore :name "T1 can go"))
         (t2-can-go (sb-thread:make-semaphore :name "T2 can go"))
         (t1 (sb-thread:make-thread
              (lambda ()
                (sb-thread:with-mutex (m1)
                  (sb-thread:wait-on-semaphore t1-can-go)
                  :ok1))
              :name "T1"))
         (t2 (sb-thread:make-thread
              (lambda ()
                (sb-ext:wait-for (eq t1 (sb-thread:mutex-owner m1)))
                (sb-thread:with-mutex (m1 :wait-p t)
                  (sb-thread:wait-on-semaphore t2-can-go)
                  :ok2))
              :name "T2")))
    (sb-ext:wait-for (eq m1 (sb-thread::thread-waiting-for t2)))
    (sb-thread:interrupt-thread t2 (lambda ()
                                     (sb-thread:with-mutex (m2 :wait-p t)
                                       (sb-ext:wait-for
                                        (eq m2 (sb-thread::thread-waiting-for t1)))
                                       (sb-thread:signal-semaphore t2-can-go))))
    (sb-ext:wait-for (eq t2 (sb-thread:mutex-owner m2)))
    (sb-thread:interrupt-thread t1 (lambda ()
                                     (sb-thread:with-mutex (m2 :wait-p t)
                                       (sb-thread:signal-semaphore t1-can-go))))
    ;; both threads should finish without a deadlock or deadlock
    ;; detection error
    (let ((res (list (sb-thread:join-thread t1)
                     (sb-thread:join-thread t2))))
      (assert (equal '(:ok1 :ok2) res)))))

(with-test (:name (:deadlock-detection :gc))
  ;; To semi-reliably trigger the error (in SBCL's where)
  ;; it was present you had to run this for > 30 seconds,
  ;; but that's a bit long for a single test.
  (let* ((stop (+ 5 (get-universal-time)))
         (m1 (sb-thread:make-mutex :name "m1"))
         (t1 (sb-thread:make-thread
              (lambda ()
                (loop until (> (get-universal-time) stop)
                      do (sb-thread:with-mutex (m1)
                           (eval `(make-array 24))))
                :ok)))
         (t2 (sb-thread:make-thread
              (lambda ()
                (loop until (> (get-universal-time) stop)
                      do (sb-thread:with-mutex (m1)
                           (eval `(make-array 24))))
                :ok))))
    (let ((res (list (sb-thread:join-thread t1)
                     (sb-thread:join-thread t2))))
      (assert (equal '(:ok :ok) res)))))

(defun clock-gettime ()
  (sb-int:dx-let ((a (make-array 2 :element-type 'sb-ext:word)))
    (alien-funcall (extern-alien "clock_gettime" (function void int system-area-pointer))
                   0 (sb-sys:vector-sap a))
    (values (aref a 0) (aref a 1))))

(defun seconds-since (start_sec start_nsec)
  (sb-int:dx-let ((a (make-array 2 :element-type 'sb-ext:word)))
    (alien-funcall (extern-alien "clock_gettime" (function void int system-area-pointer))
                   0 (sb-sys:vector-sap a))
    (+ (/ (coerce (- (aref a 1) start_nsec) 'double-float) 1000000000)
       (- (aref a 0) start_sec))))

(defglobal *max-avl-tree-total* 0)
(defglobal *max-avl-tree-born* 0)
(defglobal *max-avl-tree-running* 0)
(defglobal *max-avl-tree-died* 0)

(defun avl-maptree (fun tree)
  (sb-int:named-let recurse ((node tree))
    (when node
      (funcall fun node)
      (recurse (sb-thread::avlnode-left node))
      (recurse (sb-thread::avlnode-right node)))))

(defun thread-count (&optional (tree sb-thread::*all-threads*))
  (let ((born 0)
        (running 0)
        (died 0))
    #-pauseless-thread-start (setq running (sb-thread::avl-count tree))
    #+pauseless-thread-start
    (sb-int:dx-flet ((mapfun (node)
                       (ecase (sb-thread::thread-%visible (sb-thread::avlnode-data node))
                         (0 (incf born)) ; "can't happen" ?
                         (1 (incf running))
                         (-1 (incf died)))))
      (avl-maptree #'mapfun tree))
    (let ((total (+ born running died)))
      (macrolet ((max-into (global mine)
                   `(let ((old ,global))
                      (loop
                        (when (<= ,mine old) (return))
                        (let ((actual (cas ,global old ,mine)))
                          (if (>= actual old) (return)) (setq old actual))))))
        (max-into *max-avl-tree-total* total)
        (max-into *max-avl-tree-born* born)
        (max-into *max-avl-tree-running* running)
        (max-into *max-avl-tree-died* died))
      (list total born running died))))

;;; I would optimistically guess that this test can no longer fail,
;;; even for :win32, since there is no lisp mutex around *all-threads*.
;;; (There is still the C one)
;;;
;;; With the sesion lock acquisition as part of thread startup,
;;; but with a patch that allows the thread creator to run while
;;; the child is waiting, all this test managed to do was create thousands
;;; of threads all blocked on the *SESSION-LOCK*, because MAKE-THREAD could
;;; return the created thread did anything at all.
;;; The way that happens is as follows:
;;;   thread 0: grab session lock, initiate GC
;;;             ... return from GC but not release session lock yet
;;;   thread 1: start sleeping
;;;   thread 2: try to grab session lock, enter a wait
;;;   thread 3: start sleeping
;;;   thread 4: try to grab session lock, enter a wait
;;;   thread 5: start sleeping
;;;   ...
;;; So the number of threads running simultaneously depends entirely on when
;;; the even numbered threads get CPU time to release the session lock.
;;;
;;; That said, I was curious why this test complete _so_ much slowly
;;; with faster thread start, and more quickly with slower thread start.
;;; Enabling the :deadlock-test-timing feature shows:
;;;
;;; Maxima attained: 2923 1 2923 2916 current=(2923 0 1 2922)
;;;                  ----   ----
;;;     tree node count ^      ^ nunber of threads in "run" state
;;;
;;; The answer is obvious: the time spent in each GC is proportional to
;;; the number of threads, and with faster thread start, we can actually
;;; achieve *nearly* 3000 threads running at the same time.
;;; With each thread allocating slightly over 4MiB of memory, that's
;;; about 12GiB of additional memory for the OS to manage which has
;;; a 2nd-order effect on our runtime as well.
;;; Contrast this with the "old way" where we were essentially
;;; firing off two threads at a time and letting them finish
;;; before getting to the next two.

;; (pushnew :deadlock-test-timing *features*)

(defglobal *message-in-counter* 0)
(defglobal *message-out-counter* 0)
(declaim (fixnum *message-in-counter* *message-out-counter*))
(defparameter *huge-n-threads* 3000)
(defglobal *messages* (make-array (* 2 *huge-n-threads*)))
(declaim (simple-vector *messages*))

(defun show-queued-messages ()
  (loop while (< *message-out-counter* *message-in-counter*)
        do (let ((args (aref *messages* *message-out-counter*)))
             ;; If a store did not get into the array yet, bail out
             ;; and hope it shows up by the next time we're here.
             (when (eql args 0)
               (return-from show-queued-messages))
             (apply #'format t (concatenate 'string "~4d " (car args)) (cdr args))
             (terpri))
           (incf *message-out-counter*)))

;;; Atomically log a message for output by the main thread (to avoid interleaving)
(defun message (control &rest args)
  (let* ((data (cons control args))
         (index (atomic-incf *message-in-counter*)))
    (setf (aref *messages* index) data)))

#+darwin
(test-util::disable-profiling)

;;; This encounters the "backing off for retry" error if attempting
;;; to start too many threads.
(defparameter *max-runnable-threads* #+x86-64 100 #-x86-64 5)
(with-test (:name :gc-deadlock
            :broken-on :win32)
  #+nil (write-line "WARNING: THIS TEST WILL HANG ON FAILURE!")
  ;; Prior to 0.9.16.46 thread exit potentially deadlocked the
  ;; GC due to *all-threads-lock* and session lock. On earlier
  ;; versions and at least on one specific box this test is good enough
  ;; to catch that typically well before the 1500th iteration.
  (loop
     with i = 0
     with n = *huge-n-threads*
     with running = nil
     while (< i n)
     do
       (show-queued-messages)
       (incf i)
       #-deadlock-test-timing
       (when (zerop (mod i 100))
         (write-char #\.)
         (force-output))
       (when (> (length running) *max-runnable-threads*)
         (let ((last (car (last running))))
           (sb-thread:join-thread last)
           (setq running (nbutlast running))))
       (handler-case
          (push
           (if (oddp i)
               (make-join-thread
                (lambda (i &aux (rand (random 0.001)))
                  (declare (ignorable i))
                  #-deadlock-test-timing
                  (sleep rand)
                  #+deadlock-test-timing
                  (multiple-value-bind (t0_sec t0_nsec) (clock-gettime)
                    (message "Sleep ~f, threads=~d" i rand (thread-count))
                    (sleep rand)
                    (message  "Done (~f sec)" i (seconds-since t0_sec t0_nsec))))
                :arguments i
                :name (format nil "SLEEP-~D" i))
               (make-join-thread
                (lambda (i)
                  (declare (ignorable i))
                  ;; KLUDGE: what we are doing here is explicit,
                  ;; but the same can happen because of a regular
                  ;; MAKE-THREAD or LIST-ALL-THREADS, and various
                  ;; session functions.
                  #-deadlock-test-timing
                  (progn
                    (sb-thread::with-session-lock (sb-thread::*session*)
                      (sb-ext:gc)))
                  #+deadlock-test-timing
                  (sb-int:binding* (((t0_sec t0_nsec) (clock-gettime))
                                    ((t1_sec t1_nsec wait-time) (values nil nil nil)))
                    (message "GC, threads=~d" i (thread-count))
                    (sb-thread::with-session-lock (sb-thread::*session*)
                      (setq wait-time (seconds-since t0_sec t0_nsec))
                      (multiple-value-setq (t1_sec t1_nsec) (clock-gettime))
                      (sb-ext:gc))
                    (let ((gc-time (seconds-since t1_sec t1_nsec)))
                      (message "GC Done (~f wait + ~f gc)~A"
                               i wait-time gc-time
                               (if (> gc-time .5) "***" "")))))
                :arguments i
                :name (format nil "GC-~D" i)))
           running)
         (error (e)
           ;; Not sure why this needs to back off - at most it was running 2 threads.
           (format t "~%error creating thread ~D: ~A -- backing off for retry~%" i e)
           (sleep 0.1)
           (incf i))))
  #+deadlock-test-timing
  (progn
    (format t "~&Main thread: draining messages~%") (force-output)
    (loop while (< (cas *message-out-counter* 0 0) (length *messages*))
          do (show-queued-messages)
             (sleep .05))
    (format t "Maxima attained: ~d ~d ~d ~d current=~d~%"
            *max-avl-tree-total*
            *max-avl-tree-born*
            *max-avl-tree-running*
            *max-avl-tree-died*
            (thread-count))
    (sb-thread:make-thread #'list :name "list")
    (format t "After another make-thread: current=~d~%" (thread-count))
    (let (gc-times)
      (sb-int:dovector (x *messages*)
        (when (string= (first x) "GC Done" :end1 7)
          (push (fourth x) gc-times)))
      (let ((min (reduce #'min gc-times))
            (max (reduce #'max gc-times))
            (sum (reduce #'+ gc-times)))
        (format t "~&GC time: min=~f max=~f avg=~f sum=~f~%"
                min max (/ sum (length gc-times)) sum)))))
