(in-package sb-thread)
(defun shuffle (sequence)
  (typecase sequence
    (list
     (coerce (shuffle (coerce sequence 'vector)) 'list))
    (vector ; destructive
     (let ((vector sequence))
       (loop for lim from (1- (length vector)) downto 0
             for chosen = (random (1+ lim))
             unless (= chosen lim)
             do (rotatef (aref vector chosen) (aref vector lim)))
       vector))))

(defun to-bit-vector (elts size)
  (let ((a (make-array size :element-type 'bit)))
    (dolist (i elts a)
      (setf (aref a i) 1))))

(defun make-workloads (max-n threadcount)
  (let ((all (shuffle (loop for i below max-n collect i)))
        (amount-per-thread (floor max-n threadcount))
        (lists))
    (dotimes (i threadcount)
      (cond ((= i (1- threadcount))
             (push all lists))
            (t
             (let ((sublist (subseq all 0 amount-per-thread))
                   (rest (nthcdr amount-per-thread all)))
               (push sublist lists)
               (setq all rest)))))
    ;(format t "~&worklists are:~%")
    ;(dolist (list lists) (format t " ~s~%" list))
    (map 'vector
         (lambda (x) (to-bit-vector x max-n))
         lists)))

(sb-ext:defglobal *rwl* (make-rwlock))
(sb-ext:defglobal *m* (make-mutex))
(sb-ext:defglobal *locking* 'mutex)

(defun readlocked-gethash (table key)
  (ecase *locking*
    (rwlock
     (rwlock-rdlock *rwl*)
     (multiple-value-prog1 (gethash key table)
       (rwlock-unlock *rwl*)))
    (mutex
     (sb-thread:with-mutex (*m*) (gethash key table)))
    (lockfree
     (info-gethash key table))))

(defun writelocked-puthash (table key value)
  (ecase *locking*
    (rwlock
     (rwlock-wrlock *rwl*)
     (multiple-value-prog1 (setf (gethash key table) value)
       (rwlock-unlock *rwl*)))
    (mutex
     (sb-thread:with-mutex (*m*) (setf (gethash key table) value)))
    (lockfree
     (setf (info-gethash key table) value))))

(defun some-function-of (x) (- x))
(defvar *sem* (sb-thread:make-semaphore))

(defun setup-benchmark (max-n threadcount *random-state*)
  (let ((work (make-workloads max-n threadcount))
        (ht
         (if (eq *locking* 'lockfree)
             (make-info-hashtable :comparator #'eql :hash-function #'sb-impl::eql-hash)
             (make-hash-table))))
    (loop for i from 1 to max-n do
      (when (= (random 2) 0)
        (let ((value (some-function-of i)))
          (if (eq *locking* 'lockfree)
              (setf (info-gethash i ht) value)
              (setf (gethash i ht) value)))))
    (values ht work)))

(sb-ext:defglobal *writer-fraction* .01)

(defun benchmark (max-n threadcount ht work)
  (let ((threads))
    (dotimes (threadno threadcount)
      (push (make-thread
             (lambda (my-arg &aux (total-operations 0)
                                  (total-writes 0))
               (wait-on-semaphore *sem*)
               (dotimes (iter 10)
                 (loop for j from 1 to max-n do
                   (incf total-operations)
                   (let ((answer (readlocked-gethash ht j)))
                     (cond ((and (not answer) (= 1 (sbit my-arg (1- j))))
                            (incf total-writes)
                            (writelocked-puthash ht j (some-function-of j)))
                           ;; try to make it so that about 90% of the operations
                           ;; acquire the lock as a reader. If the number exceeds
                           ;; that, then do an operation as a writer
                           ((and answer
                                 (< (/ (coerce total-writes 'single-float)
                                       (coerce total-operations 'single-float))
                                    *writer-fraction*)
                                 (= 1 (sbit my-arg (1- j))))
                            (incf total-writes)
                            (writelocked-puthash ht j answer))))))
               (list total-writes total-operations))
             :arguments (aref work threadno)
             :name (format nil "worker~d" threadno))
            threads))
    (signal-semaphore *sem* threadcount)
    (let ((answers (mapcar 'join-thread threads)))
      (format t "~&HT: ~S" ht)
      (format t "~&Thread stats: ~S~%" answers))))

(defun time-all-ways (a b)
  (let* ((rs1 (make-random-state))
         (rs2 (make-random-state rs1))
         (rs3 (make-random-state rs1)))
    (setq *locking* 'mutex)
    (multiple-value-bind (ht work) (setup-benchmark a b rs1)
      (time (benchmark a b ht work)))
    (setq *locking* 'rwlock)
    (multiple-value-bind (ht work) (setup-benchmark a b rs2)
      (time (benchmark a b ht work)))
    (setq *locking* 'lockfree)
    (multiple-value-bind (ht work) (setup-benchmark a b rs3)
      (time (benchmark a b ht work)))))

;; If writes represent 1% of the acquires, then the RWLOCK is a win:
;;   * (sb-thread::time-all-ways 50000 8)
;;   mutex = 1.015 seconds of real time
;;   rwlock = 0.857 seconds of real time
;;   lockfree = 0.027 seconds of real time
;; But with just a tiny increase in the writes, to 2%, it isn't:
;;   mutex = 1.162 seconds of real time
;;   rwlock = 1.303 seconds of real time
;;   lockfree = 0.028 seconds of real time
