;;; HASH TABLES

#-sb-thread (invoke-restart 'run-tests::skip-file)
(use-package "SB-THREAD")
(use-package "SB-SYS")

(defvar *table-under-test* nil)

;;; If *table-under-test* is bound to a hash-table, then cause it to have
;;; half as many buckets as it would ordinarily get after enlarging.
(sb-int:encapsulate 'sb-impl::recompute-ht-vector-sizes 'collision-inducement
 (compile nil
          '(lambda (fn tbl)
             (multiple-value-bind (new-size new-n-buckets) (funcall fn tbl)
               (values new-size
                       (ash new-n-buckets
                            (if (and (eq tbl *table-under-test*)
                                     ;; Not for flat tables though
                                     ;; because they use the number of
                                     ;; buckets as their size. See
                                     ;; SB-IMPL::GROW-HASH-TABLE.
                                     (not (sb-impl::flat-hash-table-p tbl)))
                                -1
                                0)))))))

;;; Keep moving everything that can move during each GC
#+generational (setf (generation-number-of-gcs-before-promotion 0) 1000000)

(defvar *errors* nil)

(defun oops (e)
  (setf *errors* e)
  (format t "~&oops: ~A in ~S~%" e *current-thread*)
  (sb-debug:print-backtrace)
  (throw 'done nil))

(with-test (:name (hash-table :unsynchronized)
                  ;; FIXME: This test occasionally eats out craploads
                  ;; of heap instead of expected error early. Not 100%
                  ;; sure if it would finish as expected, but since it
                  ;; hits swap on my system I'm not likely to find out
                  ;; soon. Disabling for now. -- nikodemus
            :broken-on :sbcl)
  ;; We expect a (probable) error here: parellel readers and writers
  ;; on a hash-table are not expected to work -- but we also don't
  ;; expect this to corrupt the image.
  (let* ((hash (make-hash-table))
         (*errors* nil)
         (threads (list (make-kill-thread
                         (lambda ()
                           (catch 'done
                             (handler-bind ((serious-condition 'oops))
                               (loop
                                 ;;(princ "1") (force-output)
                                 (setf (gethash (random 100) hash) 'h)))))
                         :name "writer")
                        (make-kill-thread
                         (lambda ()
                           (catch 'done
                             (handler-bind ((serious-condition 'oops))
                               (loop
                                 ;;(princ "2") (force-output)
                                 (remhash (random 100) hash)))))
                         :name "reader")
                        (make-kill-thread
                         (lambda ()
                           (catch 'done
                             (handler-bind ((serious-condition 'oops))
                               (loop
                                 (sleep (random 1.0))
                                 (sb-ext:gc)))))
                         :name "collector"))))
    (unwind-protect
         (sleep 10)
      (mapc #'terminate-thread threads))))


;;; Structures are hashed by their address for any kind of standard hash-table
;;; other than an EQUALP. This makes them good candidates for exercising gethash-concurrency
;;; tests, because we can force a rehash that changes their hash.
(defstruct (teststruct (:constructor make-teststruct (val))) val)

;;; The new logic for concurrent GETHASH allows multiple threads each to decide to
;;; invoke a GC-provoked rehash, but since we do not want concurrent writers,
;;; only one thread actually rehashes and the other(s) perform linear search.
;;; This is strictly less work than multiple rehashes and should give improved throughput
;;; since no thread waits on rehash. The count of number of linear searches performed
;;; is no longer collected unless explicitly compiled in.
(defmacro with-test-setup ((array (table constructor)) &body body)
  ;; Using fixnums as hash-table keys does not engender a thorough enough test
  ;; as they will not cause the table to need rehash due to GC.
  ;; Using symbols won't work either because they hash stably under EQL
  ;; (but not under EQ) so let's use a bunch of cons cells.
  `(let* ((,array (coerce (loop for i from 0 repeat 100
                                collect (if (oddp i) (make-teststruct i) (cons i i)))
                          'vector))
          (,table ,constructor)
          (*table-under-test* (if shrinkp ,table nil)))
     (when shrinkp
       ;; start with half as many buckets as usual
       (let ((v (sb-impl::hash-table-index-vector ,table)))
         (setf (sb-impl::hash-table-index-vector ,table) (subseq v 0 (/ (length v) 2)))))
     ,@body
     #+hash-table-metrics
     (format t "~&::: INFO: GC-forced-rehash=~D rehash-invalid=~D stamp-changed=~D~%"
             (sb-impl::hash-table-n-rehash+find ,table)
             (sb-impl::hash-table-n-rehash-again ,table)
             (sb-impl::hash-table-n-stamp-change ,table))))

;;; Do *NOT* use (gc :full) in the following tests - a full GC causes all objects
;;; to be promoted into the highest normal generation, which achieves nothing,
;;; and runs the collector less often (because it's slower) relative to the total
;;; test time, making the test less usesful. It's fine for everything in gen0
;;; to stay in gen0, which basically never promotes due to the ludicrously high
;;; threshold set for number of GCs between promotions.

(defparameter *sleep-delay-max* .025)

(with-test (:name (hash-table :synchronized)
            :broken-on :win32)
  (dolist (shrinkp '(nil t))
   (with-test-setup (keys (hash (make-hash-table :synchronized t)))
    (let* ((*errors* nil)
           (threads
            (list (make-join-thread
                   (lambda () (loop (setf (gethash (aref keys (random 100)) hash) 'h)))
                   :name "writer")
                  (make-join-thread
                   (lambda () (loop (remhash (aref keys (random 100)) hash)))
                   :name "remover")
                  (make-join-thread
                   (lambda ()
                     (loop (sleep (random *sleep-delay-max*))
                           (sb-ext:gc)))
                   :name "GC"))))
      (unwind-protect (sleep 2.5)
        (mapc #'terminate-thread threads))
      (assert (not *errors*))))))

(defun test-concurrent-gethash (table-kind)
  (dolist (shrinkp '(nil t))
    (with-test-setup (keys (table (make-hash-table :test table-kind)))
      (let ((*errors* nil)
            (expected (make-array 100 :initial-element nil))
            (actions (make-array 3 :element-type 'sb-ext:word :initial-element 0)))
        (loop repeat 50
              do (let ((i (random 100)))
                   (setf (gethash (aref keys i) table) i)
                   (setf (aref expected i) t)))
        (labels
            ((reader (n random-state)
                 (catch 'done
                   (handler-bind ((serious-condition 'oops))
                     (loop
                      (let* ((i (random 100))
                             (x (gethash (aref keys i) table)))
                        (atomic-incf (aref actions n))
                        (when (and (zerop (random 100 random-state))
                                   (not (sb-impl::flat-hash-table-p table)))
                          (let* ((kvv (sb-impl::hash-table-pairs table))
                                 (epoch (svref kvv 1)))
                            ;; Randomly force a rehash (as if by GC) so that we get "invalid" rehashes,
                            ;; meaning that the ending state is still need-to-rehash.
                            (sb-ext:cas (svref kvv 1) epoch (logior epoch 1))))
                        (cond ((aref expected i) (assert (eq x i)))
                              (t (assert (not x)))))))))
             (start-reader (n)
               (make-kill-thread #'reader :name (format nil "reader ~d" (1+ n))
                                          :arguments (list n (make-random-state t)))))
          (let ((threads
              (list (start-reader 0)
                    (start-reader 1)
                    (start-reader 2)
                    (make-kill-thread
                     (lambda ()
                       (catch 'done
                         (handler-bind ((serious-condition 'oops))
                           (loop (sleep (random *sleep-delay-max*))
                                 (sb-ext:gc)))))
                     :name "collector"))))
            (unwind-protect (sleep 2.5)
              (mapc #'terminate-thread threads))
            #+hash-table-metrics
            (let ((n-gethash (reduce #'+ actions))
                  (n-lsearch (sb-impl::hash-table-n-lsearch table)))
              (format t "~&::: INFO: GETHASH count = ~D = ~D, lsearch=~d (~f%)~%"
                      actions
                      n-gethash
                 ;; With the GC frequency as high as it is for this test,
                 ;; we can get more than 1 lookup in 10^5 needing linear scan.
                 ;; A "normal" amount of GCing often sees this as low as 0.
                      n-lsearch (/ n-lsearch n-gethash)))
            (assert (not *errors*))))))))
(compile 'test-concurrent-gethash)

(with-test (:name (hash-table :parallel-readers-eq-table) :broken-on :win32)
  (test-concurrent-gethash 'eq))
(with-test (:name (hash-table :parallel-readers-eql-table)
            :broken-on (or :win32 :riscv)) ;; memory reordering issues
  (test-concurrent-gethash 'eql))
(with-test (:name (hash-table :parallel-readers-equal-table)
            :broken-on (or :win32 :riscv))
  (test-concurrent-gethash 'equal))

(with-test (:name (hash-table :single-accessor :parallel-gc)
            :broken-on :win32)
  (dolist (shrinkp '(nil t))
    (with-test-setup (keys (hash (make-hash-table)))
      (let ((*errors* nil))
        (let ((threads
               (list (make-kill-thread
                          (lambda ()
                            (handler-bind ((serious-condition 'oops))
                              (loop
                                (let* ((i (random 100))
                                       (k (aref keys i))
                                       (val (gethash k hash)))
                                  (cond (val
                                         (assert (eq val i))
                                         (assert (remhash k hash)))
                                        (t
                                         (setf (gethash k hash) i)))))))
                          :name "accessor")
                         (make-kill-thread
                          (lambda ()
                            (handler-bind ((serious-condition 'oops))
                              (loop
                                (sleep (random *sleep-delay-max*))
                                (sb-ext:gc))))
                          :name "collector"))))
          (unwind-protect (sleep 2.5)
            (mapc #'terminate-thread threads))
          (assert (not *errors*)))))))

;;; Stress GROW-HASH-TABLE's optimization wherein no rehashing may be
;;; done if the index vector is not growing.
(with-test (:name (hash-table :not-growing-index-vector :parallel-gc)
            :broken-on :win32)
  (let ((*errors* nil))
    (let ((threads
            (list (make-kill-thread
                   (lambda ()
                     (handler-bind ((serious-condition 'oops))
                       (loop (let ((h (make-hash-table))
                                   (l (loop for i below (random 200)
                                            collect (make-teststruct i))))
                               (loop for x in l do (setf (gethash x h) x))
                               (loop for x in l
                                     do (assert (eq (gethash x h) x)))))))
                   :name "worker")
                  (make-kill-thread
                   (lambda ()
                     (handler-bind ((serious-condition 'oops))
                       (loop
                         (sb-ext:gc :full t)
                         (sleep (random *sleep-delay-max*)))))
                   :name "collector"))))
      (unwind-protect (sleep 2.5)
        (mapc #'terminate-thread threads))
      (assert (not *errors*)))))
