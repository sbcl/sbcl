;;;; Stress and regression tests for sb-fiber.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

(unless (and (member :sb-thread *features*)
             (member :sb-fiber *features*)
             (or (member :x86-64 *features*)
                 (member :arm64 *features*))
             (not (member :win32 *features*)))
  (invoke-restart 'run-tests::skip-file))

(require :sb-fiber)
(use-package :sb-fiber)

(defmacro with-main-fiber ((main) &body body)
  `(let ((,main (make-main-fiber)))
     (unwind-protect (progn ,@body)
       (release-fiber ,main))))

(defmacro with-main+child ((main child entry-fn &rest make-args) &body body)
  `(with-main-fiber (,main)
     (with-fiber (,child ,entry-fn ,@make-args) ,@body)))

;;; --- GC integration ---------------------------------------------------

(with-test (:name (:fiber :gc-with-suspended-fibers))
  (with-main-fiber (main)
    (let* ((n 50)
           (fibers (make-array n))
           (ok (make-array n :initial-element nil)))
      (dotimes (i n)
        (let ((idx i))
          (setf (aref fibers i)
                (make-fiber
                 (lambda ()
                   (let ((data (list (make-array 5 :initial-element idx)
                                     (cons idx (* idx idx))
                                     (format nil "fiber-~D" idx))))
                     (switch-fiber (aref fibers idx) main)
                     (setf (aref ok idx)
                           (and (= (aref (first data) 0) idx)
                                (= (car (second data)) idx)
                                (string= (third data)
                                         (format nil "fiber-~D" idx))))
                     (switch-fiber (aref fibers idx) main)))))))
      (dotimes (i n) (switch-fiber main (aref fibers i)))
      (dotimes (_ 3) (sb-ext:gc :full t))
      (dotimes (i n) (switch-fiber main (aref fibers i)))
      (dotimes (i n) (assert (aref ok i) () "fiber ~D corrupted" i))
      (dotimes (i n) (release-fiber (aref fibers i))))))

(with-test (:name (:fiber :gc-during-repeated-switches))
  (let ((count 0))
    (with-main+child (main child
                           (lambda ()
                             (loop (incf count) (make-array 1000)
                                   (yield-fiber))))
      (dotimes (i 200)
        (switch-fiber main child)
        (when (zerop (mod i 20)) (sb-ext:gc :full t)))
      (assert (= count 200)))))

(with-test (:name (:fiber :gc-during-mass-creation))
  (with-fiber-thread ()
    (let (fibers)
      (dotimes (i 500)
        (push (make-fiber (lambda ())) fibers)
        (when (zerop (mod i 50)) (sb-ext:gc :full t)))
      (mapc #'release-fiber fibers))))

(with-test (:name (:fiber :trampoline-vs-gc-stop))
  (let ((done (sb-thread:make-semaphore))
        (stop-gc nil))
    (let ((gc-thread
            (sb-thread:make-thread
             (lambda ()
               (loop until stop-gc do (sb-ext:gc) (sleep 0.0001)))
             :name "trampoline-vs-gc-stop pumper")))
      (sb-thread:make-thread
       (lambda ()
         (with-fiber-thread ()
           (dotimes (i 500)
             (let ((f (make-fiber (lambda () i))))
               (resume-fiber f))))
         (sb-thread:signal-semaphore done))
       :name "trampoline-vs-gc-stop worker")
      (sb-thread:wait-on-semaphore done)
      (setf stop-gc t)
      (sb-thread:join-thread gc-thread))))

(defvar *bs-stress-var* :default)

(with-test (:name (:fiber :binding-stack-stress))
  (with-main-fiber (main)
    (let* ((n 20)
           (fibers (make-array n))
           (results (make-array n :initial-element nil)))
      (dotimes (i n)
        (let ((idx i))
          (setf (aref fibers i)
                (make-fiber
                 (lambda ()
                   (let ((*bs-stress-var* idx))
                     (dotimes (_ 10)
                       (switch-fiber (aref fibers idx) main)
                       (unless (= *bs-stress-var* idx)
                         (setf (aref results idx) :failed)
                         (return)))
                     (setf (aref results idx) :ok))
                   (switch-fiber (aref fibers idx) main))))))
      (dotimes (_ 10)
        (dotimes (i n) (switch-fiber main (aref fibers i)))
        (assert (eq *bs-stress-var* :default)))
      (dotimes (i n) (switch-fiber main (aref fibers i)))
      (dotimes (i n) (assert (eq :ok (aref results i))))
      (dotimes (i n) (release-fiber (aref fibers i))))))

(with-test (:name (:fiber :handler-clusters-across-switch))
  (let ((sem (sb-thread:make-semaphore))
        (inner-caught nil)
        (outer-caught nil))
    (sb-thread:make-thread
     (lambda ()
       (let* ((mf (make-main-fiber))
              (f nil))
         (setf f (make-fiber
                  (lambda ()
                    (handler-case
                        (handler-case
                            (progn (switch-fiber f mf) (error "boom"))
                          (error (e) (setf inner-caught (princ-to-string e))))
                      (error (e) (setf outer-caught (princ-to-string e)))))))
         (switch-fiber mf f)
         (switch-fiber mf f)
         (release-fiber f)
         (release-fiber mf))
       (sb-thread:signal-semaphore sem)))
    (assert (sb-thread:wait-on-semaphore sem :timeout 5))
    (assert (equal "boom" inner-caught))
    (assert (null outer-caught))))

(with-test (:name (:fiber :handler :escape))
  (let (caught)
    (with-main+child (mf f (lambda () (error "error")))
      (handler-case (switch-fiber mf f)
        (error (e) (setf caught (princ-to-string e)))))
    (assert (equal caught "error"))))

(with-test (:name (:fiber :handler :nested))
  (dotimes (i 1000)
    (let (caught)
      (with-main+child (mf f
                           (lambda ()
                             (handler-case
                                 (handler-case
                                     (progn (yield-fiber) (error "iter ~A" i))
                                   (error (e) (setf caught (princ-to-string e))))
                               (error () nil))))
        (switch-fiber mf f)
        (switch-fiber mf f)
        (assert (search (format nil "iter ~A" i) (or caught "")))))))

(with-test (:name (:fiber :multi-thread-independent-fibers))
  (let ((results (make-array 4 :initial-element nil))
        (lock (sb-thread:make-mutex)))
    (let ((threads
            (loop for ti below 4 collect
                  (let ((idx ti))
                    (sb-thread:make-thread
                     (lambda ()
                       (with-fiber-thread ()
                         (let* ((count 0)
                                (child (make-fiber
                                        (lambda ()
                                          (dotimes (_ 1000)
                                            (incf count) (yield-fiber))))))
                           (dotimes (_ 1000)
                             (switch-fiber *current-fiber* child))
                           (release-fiber child)
                           (sb-thread:with-mutex (lock)
                             (setf (aref results idx) count))))))))))
      (mapc #'sb-thread:join-thread threads))
    (dotimes (i 4) (assert (= 1000 (aref results i))))))

(with-test (:name (:fiber :current-fiber-is-per-thread))
  (let* ((n-threads 4)
         (start (sb-thread:make-semaphore))
         (results (make-array n-threads :initial-element nil)))
    (let ((threads
            (loop for i below n-threads collect
                  (let ((idx i))
                    (sb-thread:make-thread
                     (lambda ()
                       (with-fiber-thread ()
                         (let ((c (make-fiber
                                   (lambda () (loop (yield-fiber))))))
                           (sb-thread:wait-on-semaphore start)
                           (handler-case
                               (progn (dotimes (_ 5000)
                                        (switch-fiber *current-fiber* c))
                                      (setf (aref results idx) :ok))
                             (error (e) (setf (aref results idx) e)))
                           (release-fiber c)))))))))
      (sleep 0.05)
      (dotimes (_ n-threads) (sb-thread:signal-semaphore start))
      (mapc #'sb-thread:join-thread threads))
    (loop for r across results
          do (assert (eq r :ok) () "thread errored: ~S" r))))

(sb-alien:define-alien-variable ("lose_on_corruption_p" %lose-on-corruption-p)
    sb-alien:int)

(defmacro without-lose-on-corruption (&body body)
  (let ((saved (gensym)))
    `(let ((,saved %lose-on-corruption-p))
       (unwind-protect (progn (setf %lose-on-corruption-p 0) ,@body)
         (setf %lose-on-corruption-p ,saved)))))

(with-test (:name (:fiber :stack-overflow-recovers))
  (let (caught)
    (with-main+child (main child
                           (lambda ()
                             (declare (optimize (speed 0) (safety 3) (debug 3)))
                             (handler-case
                                 (labels ((blow (n) (1+ (blow (1+ n)))))
                                   (blow 0))
                               (storage-condition () (setf caught t)))
                             (yield-fiber))
                           :stack-size 65536)
      (without-lose-on-corruption (switch-fiber main child)))
    (assert caught)))

(defvar *bs-a*) (defvar *bs-b*) (defvar *bs-c*) (defvar *bs-d*)

(with-test (:name (:fiber :binding-stack-overflow-recovers))
  (let (caught)
    (with-main+child (main child
                           (lambda ()
                             (declare (optimize (speed 0) (safety 3) (debug 3)))
                             (labels ((recur (d)
                                        (declare (type fixnum d))
                                        (progv '(*bs-a* *bs-b* *bs-c* *bs-d*)
                                            '(0 0 0 0)
                                          (recur (1+ d)))))
                               (handler-case (recur 0)
                                 (storage-condition () (setf caught t))))))
      (without-lose-on-corruption (switch-fiber main child)))
    (assert caught)))

(with-test (:name (:fiber :fanout-scaling) :slow t :skipped-on (not :slow))
  (dolist (n '(10 100 1000 10000))
    (with-main-fiber (main)
      (let* ((fibers (make-array n))
             (total  0))
        (dotimes (i n)
          (setf (aref fibers i)
                (make-fiber (lambda ()
                              (loop (incf total) (yield-fiber))))))
        (let* ((rounds (max 1 (floor 100000 n)))
               (start  (get-internal-real-time)))
          (dotimes (_ rounds)
            (dotimes (i n) (switch-fiber main (aref fibers i))))
          (let* ((elapsed (/ (- (get-internal-real-time) start)
                             internal-time-units-per-second))
                 (rate    (if (> elapsed 0) (/ (* rounds n) elapsed) 0)))
            (format t "~&;   ~6D fibers x ~D rounds: ~,2F switches/sec~%"
                    n rounds rate)))
        (assert (> total 0))
        (dotimes (i n) (release-fiber (aref fibers i)))))))

(with-test (:name (:fiber :fanout-gc-under-load) :slow t :skipped-on (not :slow))
  (dolist (n '(100 1000 5000))
    (with-main-fiber (main)
      (let* ((fibers (make-array n))
             (ok     (make-array n :initial-element nil)))
        (dotimes (i n)
          (let ((idx i))
            (setf (aref fibers i)
                  (make-fiber
                   (lambda ()
                     (let ((data (list (cons idx idx)
                                       (make-array 3 :initial-element idx))))
                       (yield-fiber)
                       (setf (aref ok idx)
                             (and (= idx (car (first data)))
                                  (= idx (aref (second data) 0))))
                       (yield-fiber)))))))
        (dotimes (i n) (switch-fiber main (aref fibers i)))
        (sb-ext:gc :full t)
        (dotimes (i n) (switch-fiber main (aref fibers i)))
        (assert (= n (count t ok)) () "GC corruption at n=~D" n)
        (dotimes (i n) (release-fiber (aref fibers i)))))))

(defun %caller-frame-driver ()
  (with-main+child (main child (lambda () (loop (yield-fiber))))
    (dotimes (_ 1000000) (switch-fiber main child))
    ;; Force an INVOKE-WITH-SAVED-FP wrap after the loop.
    (let ((x (get-internal-real-time))) (declare (ignore x)))))

(with-test (:name (:fiber :caller-frame-live-across-1m-switches)
            :slow t :skipped-on (not :slow))
  (let ((witness (list :fiber :liveness-probe)))
    (%caller-frame-driver)
    (assert (consp witness))
    (assert (eq (first witness) :fiber))
    (assert (eq (second witness) :liveness-probe))))

(with-test (:name (:fiber :gc-under-create-destroy-churn))
  (with-fiber-thread ()
    (dotimes (i 200)
      (release-fiber (make-fiber (lambda () (make-array 10))))
      (when (zerop (mod i 25)) (sb-ext:gc :full t)))))

(with-test (:name (:fiber :switch-cycle-survives-gc-churn) :slow t :skipped-on (not :slow))
  (with-main-fiber (main)
    (let ((hits 0))
      (dotimes (i 5000)
        (let ((expected i))
          (with-fiber (child (lambda ()
                               (incf hits)
                               (assert (= expected i))
                               (yield-fiber)))
            (switch-fiber main child)))
        (when (zerop (mod i 250)) (sb-ext:gc :full t)))
      (assert (= hits 5000)))))

(with-test (:name (:fiber :cross-thread-destroy))
  (let* ((created nil)
         (creator-done (sb-thread:make-semaphore))
         (destroyer-ok (sb-thread:make-semaphore))
         (t1 (sb-thread:make-thread
              (lambda ()
                (let ((mf (make-main-fiber)))
                  (setf created (make-fiber (lambda ())))
                  (sb-thread:signal-semaphore creator-done)
                  (sb-thread:wait-on-semaphore destroyer-ok :timeout 5)
                  (release-fiber mf))))))
    (sb-thread:wait-on-semaphore creator-done :timeout 5)
    (release-fiber created)
    (sb-thread:signal-semaphore destroyer-ok)
    (sb-thread:join-thread t1)))

(with-test (:name (:fiber :signal-safe-under-timer-gc))
  (let ((fired 0))
    (with-main+child (main child (lambda () (loop (yield-fiber))))
      (let ((timer (sb-ext:make-timer
                    (lambda ()
                      (incf fired)
                      (when (zerop (mod fired 50)) (sb-ext:gc)))
                    :thread sb-thread:*current-thread*)))
        (sb-ext:schedule-timer timer 0.0005 :repeat-interval 0.0005)
        (unwind-protect
             (dotimes (_ 200000) (switch-fiber main child))
          (sb-ext:unschedule-timer timer))))
    (assert (>= fired 1))))

#+sb-thread
(with-test (:name (:fiber :register-prepare-gc-race))
  (let ((stop nil))
    (let ((gc-th (sb-thread:make-thread
                  (lambda () (loop until stop do (sb-ext:gc) (sleep 0.0005)))))
          (drivers
            (loop for d below 4 collect
                  (sb-thread:make-thread
                   (lambda ()
                     (make-main-fiber)
                     (dotimes (_ 50)
                       (let ((fs (loop repeat 16
                                       collect (make-fiber (lambda ())))))
                         (mapc #'release-fiber fs))))))))
      (mapc #'sb-thread:join-thread drivers)
      (setf stop t)
      (sb-thread:join-thread gc-th))))

(with-test (:name (:fiber :thread-exit-releases-orphaned-fibers))
  (dotimes (_ 50)
    (sb-thread:join-thread
     (sb-thread:make-thread
      (lambda ()
        (make-main-fiber)
        (dotimes (_ 200)
          (make-fiber (lambda () (loop (yield-fiber)))))))))
  (sb-ext:gc :full t))

(with-test (:name (:fiber :deep-stack-under-gc))
  (let* ((depth 200)
         (built nil))
    (with-main+child (main child
                           (lambda ()
                             (labels ((rec (n acc)
                                        (declare (type fixnum n))
                                        (if (zerop n)
                                            (progn (yield-fiber) acc)
                                            (rec (1- n) (cons n acc)))))
                               (setf built (rec depth nil)))))
      (switch-fiber main child)         ; child reaches depth, yields
      (sb-ext:gc :full t)
      (sb-ext:gc :full t)
      (switch-fiber main child))        ; resume, unwind, fiber dies
    (assert (= (length built) depth))
    (assert (= (reduce #'+ built) (/ (* depth (1+ depth)) 2)))))

(with-test (:name (:fiber :scrub-shrunk-path :no-witness-corruption)
            :slow t :skipped-on (not :slow))
  (let* ((iterations 200)
         (recurse-depth 150)
         (witness (loop repeat 64 collect (cons :alive nil))))
    (with-main+child (main child
                           (lambda ()
                             (labels ((rec (n acc)
                                        (declare (type fixnum n))
                                        (if (zerop n)
                                            acc
                                            (rec (1- n) (cons n acc)))))
                               (loop
                                 (rec recurse-depth nil) ; grows ~150 frames + conses
                                 (yield-fiber)))))
      (dotimes (_ iterations)
        (switch-fiber main child)
        (sb-ext:gc :full t)
        (assert (every (lambda (c) (eq (car c) :alive)) witness)
                ()
                "witness corruption -- conservative scanner pinned garbage")))))

(with-test (:name (:fiber :interrupt-fiber :nested-handler-case))
  (let (inner-caught outer-caught)
    (with-main+child (main child
                           (lambda ()
                             (handler-case
                                 (handler-case (yield-fiber)
                                   (simple-error (e) (setf inner-caught e)))
                               (error (e) (setf outer-caught e)))))
      (switch-fiber main child)         ; run to yield
      (interrupt-fiber child (make-condition 'simple-error
                                             :format-control "interrupted"))
      (switch-fiber main child))        ; resume; interrupt fires inside child
    (assert inner-caught   () "inner handler-case did not catch")
    (assert (null outer-caught) () "outer handler-case fired (should not)")
    (assert (search "interrupted" (princ-to-string inner-caught)))))

(with-test (:name (:fiber :interrupt-fiber :handler-transforms-condition))
  (let (received)
    (with-main+child (main child
                           (lambda ()
                             (handler-case (yield-fiber)
                               (simple-error () (error "transformed")))))
      (switch-fiber main child)
      (interrupt-fiber child (make-condition 'simple-error
                                             :format-control "original"))
      (handler-case (switch-fiber main child)
        (simple-error (e) (setf received (princ-to-string e)))))
    (assert (search "transformed" received) ()
            "expected 'transformed', got ~S" received)))

(with-test (:name (:fiber :join-fiber :condition-escape))
  (with-main+child (main child (lambda () (error "escapes-via-join")))
    (handler-case (join-fiber child)
      (simple-error (e)
        (assert (search "escapes-via-join" (princ-to-string e)))))))

;;; --- Partial-failure cleanup --------------------------------------------
;;;
;;; If a step after the C-level fiber allocation signals, the raw SAP must
;;; be released rather than leaked.  We force the failure by shadowing
;;; %FIBER-REGISTER and instrument %FIBER-RELEASE to count cleanup calls.

(defmacro with-injected-register-failure ((release-count-var) &body body)
  (let ((orig-r (gensym)) (orig-rel (gensym)))
    `(let* ((,release-count-var 0)
            (,orig-r   (fdefinition 'sb-fiber::%fiber-register))
            (,orig-rel (fdefinition 'sb-fiber::%fiber-release)))
       (unwind-protect
            (progn
              (setf (fdefinition 'sb-fiber::%fiber-register)
                    (lambda (th sap)
                      (declare (ignore th sap))
                      (error "injected register failure")))
              (setf (fdefinition 'sb-fiber::%fiber-release)
                    (lambda (sap)
                      (incf ,release-count-var)
                      (funcall ,orig-rel sap)))
              ,@body)
         (setf (fdefinition 'sb-fiber::%fiber-register) ,orig-r)
         (setf (fdefinition 'sb-fiber::%fiber-release)  ,orig-rel)))))

(with-test (:name (:fiber :make-fiber :partial-failure-releases-sap))
  (with-main-fiber (main)
    (assert (eq main *current-fiber*))
    (let (got-error)
      (with-injected-register-failure (releases)
        (handler-case (make-fiber (lambda ()))
          (simple-error () (setf got-error t)))
        (assert got-error)
        (assert (= 1 releases) ()
                "expected one %fiber-release call, got ~D" releases))
      ;; State remains usable: a subsequent make-fiber works fine.
      (let ((f (make-fiber (lambda ()))))
        (assert (fiber-alive-p f))
        (release-fiber f)))))

(with-test (:name (:fiber :make-main-fiber :partial-failure-releases-sap))
  (assert (null *current-fiber*))
  (let ((got-error nil))
    (with-injected-register-failure (releases)
      (handler-case (make-main-fiber)
        (simple-error () (setf got-error t)))
      (assert got-error)
      (assert (= 1 releases) ()
              "expected one %fiber-release call, got ~D" releases))
    ;; A failed make-main-fiber must not publish the partial wrapper.
    (assert (null *current-fiber*))
    ;; Recovery: a subsequent make-main-fiber works.
    (let ((m (make-main-fiber)))
      (assert (fiber-alive-p m))
      (release-fiber m))))

;;; --- Cross-thread migration ---

(with-test (:name (:fiber :migrate :metadata-only)
            :skipped-on (not :sb-thread))
  (let* ((fiber-cell (list nil))
         (a-installed (sb-thread:make-semaphore :name "metaonly-installed"))
         (a-released  (sb-thread:make-semaphore :name "metaonly-released"))
         (b-go        (sb-thread:make-semaphore :name "metaonly-b-go")))
    (let* ((a-thread
             (sb-thread:make-thread
              (lambda ()
                (with-main-fiber (a-main)
                  (let ((child (make-fiber (lambda () (yield-fiber) :done))))
                    (resume-fiber child)
                    (setf (car fiber-cell) child)
                    (sb-thread:signal-semaphore a-installed)
                    (sb-thread:wait-on-semaphore a-released))))
              :name "fiber-migrate-meta-src"))
           (b-thread
             (sb-thread:make-thread
              (lambda ()
                (sb-thread:wait-on-semaphore b-go)
                ;; Migrated child sits in B's fiber list.  B releases
                ;; it without ever switching into it.  The wrapper's
                ;; FIBER-THREAD must be B by this point, and
                ;; RELEASE-FIBER must succeed without signalling.
                (release-fiber (car fiber-cell)))
              :name "fiber-migrate-meta-dest")))
      (sb-thread:wait-on-semaphore a-installed)
      (let ((child (car fiber-cell)))
        (assert child)
        (assert (eq (sb-fiber::fiber-thread child) a-thread)
                nil "before migrate, fiber's thread slot should be A")
        (assert (eq child (fiber-migrate child b-thread)))
        (assert (eq (sb-fiber::fiber-thread child) b-thread)
                nil "after migrate, fiber's thread slot should be B"))
      (sb-thread:signal-semaphore b-go)
      (sb-thread:join-thread b-thread)
      (sb-thread:signal-semaphore a-released)
      (sb-thread:join-thread a-thread))))

(with-test (:name (:fiber :migrate :runnable-runs-on-dest)
            :skipped-on (not :sb-thread))
  (let* ((fiber-cell (list nil))
         (a-installed (sb-thread:make-semaphore :name "a-installed"))
         (a-released  (sb-thread:make-semaphore :name "a-released"))
         (migrate-done (sb-thread:make-semaphore :name "migrate-done"))
         (b-result    (list nil))
         (observed-thread (list nil)))
    (let* ((a-thread
             (sb-thread:make-thread
              (lambda ()
                (with-main-fiber (a-main)
                  (let ((child (make-fiber
                                (lambda ()
                                  (yield-fiber)
                                  (setf (car observed-thread)
                                        sb-thread:*current-thread*)
                                  :body-done))))
                    (resume-fiber child)
                    (setf (car fiber-cell) child)
                    (sb-thread:signal-semaphore a-installed)
                    (sb-thread:wait-on-semaphore a-released))))
              :name "fiber-migrate-src")))
      (sb-thread:wait-on-semaphore a-installed)
      (let ((child (car fiber-cell)))
        (assert child)
        (let ((b-thread
                (sb-thread:make-thread
                 (lambda ()
                   (sb-thread:wait-on-semaphore migrate-done)
                   (with-main-fiber (b-main)
                     (setf (car b-result) (join-fiber child))))
                 :name "fiber-migrate-dest")))
          (fiber-migrate child b-thread)
          (assert (eq (sb-fiber::fiber-thread child) b-thread)
                  nil
                  "after migrate, fiber THREAD slot should be B; got ~A"
                  (sb-fiber::fiber-thread child))
          (sb-thread:signal-semaphore migrate-done)
          (sb-thread:join-thread b-thread)
          (assert (eq (car observed-thread) b-thread)
                  nil
                  "fiber body ran on ~A; expected B = ~A"
                  (car observed-thread) b-thread)
          (assert (eq (car b-result) :body-done)
                  nil
                  "join-fiber returned ~A; expected :body-done"
                  (car b-result))))
      (sb-thread:signal-semaphore a-released)
      (sb-thread:join-thread a-thread))))

(with-test (:name (:fiber :migrate :rejects-released)
            :skipped-on (not :sb-thread))
  (let* ((other (sb-thread:make-thread
                 (lambda () (sb-thread:thread-yield) :ok)
                 :name "migrate-validate-dest"))
         (released-fiber nil))
    (with-main-fiber (main)
      (let ((f (make-fiber (lambda () :immediately-done))))
        (join-fiber f)
        (release-fiber f)
        (setf released-fiber f)))
    (handler-case
        (progn (fiber-migrate released-fiber other)
               (error "expected DEAD-FIBER-ERROR; none signaled"))
      (dead-fiber-error () :ok))
    (sb-thread:join-thread other)))
