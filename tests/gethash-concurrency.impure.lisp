;;; HASH TABLES

#-sb-thread (sb-ext:exit :code 104)
(use-package "SB-THREAD")
(use-package "SB-SYS")

(defvar *table-under-test* nil)

;;; If *table-under-test* is bound to a hash-table, then cause it to have
;;; half as many buckets as it would ordinarily get after enlarging.
(sb-int:encapsulate
 'sb-impl::hash-table-new-vectors
 'test
 (compile nil
          '(lambda (fn tbl)
            (if (eq tbl *table-under-test*)
                (multiple-value-bind (kv-vect next-vect hash-vect bucket-vect)
                    (funcall fn tbl)
                  (values kv-vect next-vect hash-vect
                          (subseq bucket-vect 0 (/ (length bucket-vect) 2))))
                (funcall fn tbl)))))

;;; Keep moving everything that can move during each GC
#+gencgc (setf (generation-number-of-gcs-before-promotion 0) 1000000)

(defvar *errors* nil)

(defun oops (e)
  (setf *errors* e)
  (format t "~&oops: ~A in ~S~%" e *current-thread*)
  (sb-debug:print-backtrace)
  (catch 'done))

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

(defmacro with-test-setup ((array (table constructor)) &body body)
  ;; Using fixnums as hash-table keys does not engender a thorough enough test
  ;; as they will not cause the table to need rehash due to GC.
  ;; Using symbols won't work either because they hash stably under EQL
  ;; (but not under EQ) so let's use a bunch of cons cells.
  `(let* ((,array (coerce (loop for i from 0 repeat 100 collect (cons i i)) 'vector))
          (,table ,constructor)
          (*table-under-test* (if shrinkp ,table nil)))
     (when shrinkp
       ;; start with half as many buckets as usual
       (let ((v (sb-impl::hash-table-index-vector ,table)))
         (setf (sb-impl::hash-table-index-vector ,table) (subseq v 0 (/ (length v) 2)))))
     ,@body
     (format t "~&::: INFO: Rehash count = ~D~%"
             (sb-impl::hash-table-n-rehash+find ,table))))

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
    (let*
        ((*errors* nil)
         (threads (list (make-join-thread
                         (lambda ()
                           (catch 'done
                             (handler-bind ((serious-condition 'oops))
                               (loop
                                 ;;(princ "1") (force-output)
                                 (setf (gethash (aref keys (random 100)) hash) 'h)))))
                         :name "writer")
                        (make-join-thread
                         (lambda ()
                           (catch 'done
                             (handler-bind ((serious-condition 'oops))
                               (loop
                                 ;;(princ "2") (force-output)
                                 (remhash (aref keys (random 100)) hash)))))
                         :name "reader")
                        (make-join-thread
                         (lambda ()
                           (catch 'done
                             (handler-bind ((serious-condition 'oops))
                               (loop
                                 (sleep (random *sleep-delay-max*))
                                 (sb-ext:gc)))))
                         :name "collector"))))
      (unwind-protect (sleep 2.5)
        (mapc #'terminate-thread threads))
      (assert (not *errors*))))))

(with-test (:name (hash-table :parallel-readers)
                  :broken-on :win32)
  (dolist (shrinkp '(nil t))
    (with-test-setup (keys (hash (make-hash-table)))
      (let ((*errors* nil)
            (expected (make-array 100 :initial-element nil))
            (actions (make-array 3 :element-type 'sb-ext:word :initial-element 0)))
        (loop repeat 50
              do (let ((i (random 100)))
                   (setf (gethash (aref keys i) hash) i)
                   (setf (aref expected i) t)))
        (flet ((reader (n)
                 (catch 'done
                   (handler-bind ((serious-condition 'oops))
                     (loop
                      (let* ((i (random 100))
                             (x (gethash (aref keys i) hash)))
                        (atomic-incf (aref actions n))
                        (cond ((aref expected i) (assert (eq x i)))
                              (t (assert (not x))))))))))
          (let ((threads
              (list (make-kill-thread #'reader :name "reader 1" :arguments 0)
                    (make-kill-thread #'reader :name "reader 2" :arguments 1)
                    (make-kill-thread #'reader :name "reader 3" :arguments 2)
                    (make-kill-thread
                     (lambda ()
                       (catch 'done
                         (handler-bind ((serious-condition 'oops))
                           (loop (sleep (random *sleep-delay-max*))
                                 (sb-ext:gc)))))
                     :name "collector"))))
            (unwind-protect (sleep 2.5)
              (mapc #'terminate-thread threads))
            (format t "~&::: INFO: GETHASH count = ~D = ~D, lsearch=~d~%"
                    actions
                    (reduce #'+ actions)
                 ;; With the GC frequency as high as it is for this test,
                 ;; we can get more than 1 lookup in 10^5 needing linear scan.
                 ;; A "normal" amount of GCing often sees this as low as 0.
                    (sb-impl::hash-table-n-lsearch hash))
            (assert (not *errors*))))))))

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
