;;;; Benchmarking SBCL hash tables



;;;; General utilities

(defun shuffle (seq)
  "Copy of SEQ and shuffle it using Fisher-Yates algorithm."
  (if (listp seq)
      (coerce (shuffle-vector! (coerce seq 'vector)) 'list)
      (shuffle-vector! (copy-seq seq))))

(defun shuffle-vector! (vector)
  (loop for idx downfrom (1- (length vector)) to 1
        for other = (random (1+ idx))
        do (unless (= idx other)
             (rotatef (aref vector idx) (aref vector other))))
  vector)

(defun random-permutation (n)
  (let ((v (make-array n)))
    (loop for i below n
          do (setf (aref v i) i))
    (shuffle-vector! v)))

;;; A more efficient (SUBSEQ (RANDOM-PERMUTATION N) 0 M).
(defun subseq-of-random-permutation (n m)
  (if (< (* 2 m) n)
      (let ((v (make-array m))
            (h (make-hash-table)))
        (loop for i below m
              do (loop for x = (random n)
                       until (null (gethash x h))
                       finally (progn
                                 (setf (gethash x h) t)
                                 (setf (aref v i) x))))
        v)
      (subseq (random-permutation n) 0 m)))

(defun mean (seq &key (key #'identity))
  (let ((s 0d0))
    (map nil (lambda (x)
               (incf s (funcall key x)))
         seq)
    (/ s (length seq))))

(defun variance (seq &key (key #'identity))
  (let ((mean (mean seq :key key))
        (s 0d0))
    (map nil (lambda (x)
               (incf s (expt (- (funcall key x) mean) 2)))
         seq)
    (/ s (length seq))))

(defun uniq (seq &key (test #'eql))
  (let ((h (make-hash-table :test test)))
    (if (listp seq)
        (loop for x in seq
              do (setf (gethash x h) t))
        (loop for x across seq
              do (setf (gethash x h) t)))
    (coerce (loop for x being the hash-key in h
                  collect x)
            'vector)))


(defvar *ht-args* ())

(defvar *not-garbage-list*)
(defvar *not-garbage-vector*)

(defvar *all-symbols*)

(defun all-symbols ()
  (if (boundp '*all-symbols*)
      *all-symbols*
      (setq *all-symbols*
            (let ((r ()))
              (sb-vm:map-allocated-objects
               (lambda (obj type-code n-bytes*)
                 (declare (ignore type-code n-bytes*))
                 (when (typep obj 'symbol)
                   (push obj r)))
               :all)
              (uniq r)))))

(defvar *all-strings*)

(defun all-strings ()
  (if (boundp '*all-strings*)
      *all-strings*
      (setq *all-strings*
            (let ((r ()))
              (sb-vm:map-allocated-objects
               (lambda (obj type-code n-bytes*)
                 (declare (ignore type-code n-bytes*))
                 (when (typep obj 'string)
                   (push obj r)))
               :all)
              (uniq r :test #'equal)))))

(defvar *all-symbol-names*)

(defun all-symbol-names ()
  (if (boundp '*all-symbol-names*)
      *all-symbol-names*
      (setq *all-symbol-names*
            (uniq (map 'vector #'prin1-to-string
                       (all-symbols))
                  :test #'equal))))

(defun all-keys-for (type alloc)
  (ecase type
    ((symbol) (all-symbols))
    ((string)
     (ecase alloc
       ((:existing) (all-strings))
       ((:existing-symbol-name) (all-symbol-names))))))

(defun n-available-keys (type alloc)
  (cond ((and (eq type 'symbol)
              (eq (alloc-name alloc) :existing))
         (length (all-symbols)))
        ((and (eq type 'string)
              (eq (alloc-name alloc) :existing))
         (length (all-strings)))
        ((and (eq type 'string)
              (eq (alloc-name alloc) :existing-symbol-name))
         (length (all-symbol-names)))))


;;;; Timing

(defmacro incf-timing (timing delta)
  (sb-int:with-unique-names (key value)
    (sb-int:once-only ((timing1 timing) (delta delta))
      `(cond ((null ,timing1)
              (setq ,timing (list* :deltas (list ,delta)
                                   (copy-list ,delta))))
             (t
              (loop for (,key ,value) on ,timing1 by #'cddr
                    do (unless (eq ,key :deltas)
                         (incf (getf ,timing1 ,key) (getf ,delta ,key))))
              (push ,delta (getf ,timing1 :deltas)))))))

(defun scale-timing (weight timing)
  (loop for (key value) on timing by #'cddr
        append (list key (* weight value))))

(defun timing-value (timing indicator &optional default)
  (getf timing indicator default))

;;; This is *not* the variance of the time it takes to perform an
;;; individual op (e.g. GET) (or *NORMALIZE-TIMINGS-TO-N-KEYS* ops).
;;; This is the variance of the *average* time it takes to perform an
;;; op (e.g. at a given size). This is because we sample keys for the
;;; key sets without replacement, so it is only key sets and thus the
;;; the average time over them that is i.i.d.
(defun timing-variance (timing &key key)
  ;; For all TIMING-VALUEs in it, TIMING is the sum of its :DELTAS.
  ;; The deltas are sums of i.i.d. random variables (the times
  ;; individual key sets take), thus so is TIMING. We estimate the
  ;; sample variance of the time *NORMALIZE-TIMINGS-TO-N-KEYS* ops
  ;; take by computing the sample variance of the deltas and scaling
  ;; it by the number of deltas (because deltas are also i.i.d.).
  (let ((deltas (getf timing :deltas)))
    (if (<= 3 (length deltas))
        (* (variance deltas :key key) (length deltas))
        ;; The estimate would be too unreliable.
        nil)))

(defun timing-run-time-us (timing)
  (+ (timing-value timing :user-run-time-us)
     (timing-value timing :system-run-time-us)))

(defun print-timing (timing)
  (flet ((-> (indicator)
           (timing-value timing indicator 0)))
    (format t "~,1Fns +-~,2F (~,1F user + ~,1F sys, ~,1F gc), ~
               ~,1F cycles +-~,2F~%"
            (* (timing-run-time-us timing) 1000)
            (let ((variance (timing-variance timing :key #'timing-run-time-us)))
              (if variance
                  (* (sqrt variance) 1000)
                  nil))
            (* (-> :user-run-time-us) 1000)
            (* (-> :system-run-time-us) 1000)
            (* (-> :gc-run-time-ms) 1000000)
            (round (-> :processor-cycles))
            (let ((variance (timing-variance
                             timing :key (lambda (delta)
                                           (timing-value delta
                                                         :processor-cycles)))))
              (if variance
                  (sqrt variance)
                  nil)))
    ;; Real time has millisecond resolution. It's difficult to get a
    ;; reliable measurement as a sum of small ones (see
    ;; WITH-TIMING-ADDED and CALC-N-REPEATS-AND-N-CONCURRENT).
    #+nil
    (format t "    Real time: ~,3Fs +- ~,3F (~,3F gc)~%"
            (/ (-> :real-time-ms) 1000)
            (/ (sqrt n-measurements) 1000)
            (/ (-> :gc-real-time-ms) 1000))))

(defmacro with-timing-added (((&rest into-timings) &key (weight 1)) &body body)
  (sb-int:with-unique-names (timing)
    `(sb-ext:call-with-timing
      (lambda (&rest ,timing)
        (let ((,timing (scale-timing ,weight (list* :n-measurements 1
                                                    ,timing))))
          ,@(loop for into-timing in into-timings
                  collect `(incf-timing ,into-timing ,timing))))
      (lambda () ,@body))))


;;; We actually measure times with approximately *TIME-N-KEYS* but
;;; rescale the results to be exactly per
;;; *NORMALIZE-TIMINGS-TO-N-KEYS*.
(defparameter *time-n-keys* 5000000)
(defparameter *normalize-timings-to-n-keys* 1)
;;; Because timing has at best microsecond resolution, measuring how
;;; long it takes to insert a single key into a hash table is not
;;; going to meaningful. We instead create a number of "concurrent"
;;; hash tables (plus key sets) and inside the measurement loop insert
;;; one key into each.
(defparameter *min-n-keys-to-time* 100)
;;; The minimum number of measurements to take. This is relevant the
;;; number of keys in the hash table is large.
(defparameter *min-n-repeats* 3)

(defun calc-n-repeats-and-n-concurrent (n-keys)
  (if (zerop n-keys)
      (values 1 1)
      (let ((n-repeats (max *min-n-repeats*
                            (floor *time-n-keys* n-keys))))
        ;; Have at least 100 keys for each hash table. The tight
        ;; PUTHASH/GETHASH benchmark loops must take long enough so
        ;; that measurement overhead is not a big issue.
        (if (<= *min-n-keys-to-time* n-keys)
            (values n-repeats 1)
            (let ((n-concurrent (ceiling *min-n-keys-to-time* n-keys)))
              (values (ceiling n-repeats n-concurrent) n-concurrent))))))

(defun estimate-call-with-timing-overhead ()
  (let ((timing nil)
        (n 200000))
    (loop repeat n do (with-timing-added ((timing))))
    (* (/ (+ (getf timing :user-run-time-us)
             (getf timing :system-run-time-us))
          (getf timing :n-measurements))
       1000)))


(defun random-string (n)
  (coerce (loop for i below n
                collect (code-char (+ (char-code #\a) (random 26))))
          'string))

(defvar *aaa* (make-array 1000 :element-type 'fixnum))

(defun blow-caches ()
  (declare (optimize speed (safety 0)))
  (let ((a *aaa*)
        (r 0))
    (declare (type (simple-array fixnum (*)) a)
             (type fixnum r))
    (loop for i below (length a) do
      (setq r (logior r (aref a i))))
    r))

(defun ht-n-buckets (ht)
  (max (length (sb-impl::hash-table-index-vector ht))
       ;; This is longer for flat hash tables.
       (/ (- (length (sb-impl::hash-table-pairs ht)) 3)
          2)))

;;; Return a list of key count ranges for which there are no change
;;; HASH-TABLE-SIZE nor in the number of buckets (that is, the length
;;; of INDEX-VECTOR). Each element of the list is like (FROM TO SIZE
;;; N-BUCKETS), meaning that if there are at least FROM but less than
;;; TO keys, then HASH-TABLE-SIZE is SIZE and there are N-BUCKETS. The
;;; smallest FROM is START; the largest TO is END.
(defun list-ht-ranges (start end make-hash-table-args)
  (let ((h (apply #'make-hash-table make-hash-table-args)))
    (dotimes (i start)
      (setf (gethash i h) i))
    (let ((range-size (hash-table-size h))
          (range-n-buckets (ht-n-buckets h))
          (range-start start)
          (ranges ()))
      (loop for i upfrom (1+ start) below end do
        (setf (gethash i h) i)
        (let ((n-buckets (ht-n-buckets h))
              (size (hash-table-size h)))
          (when (or (/= size range-size)
                    (/= n-buckets range-n-buckets))
            (push (list range-start i range-size range-n-buckets) ranges)
            (setq range-size size
                  range-n-buckets n-buckets
                  range-start i))))
      (push (list range-start end range-size range-n-buckets) ranges)
      (reverse ranges))))

(defun allocate-keys (n-keys type alloc &key missp)
  (let ((offset (+ (if missp most-negative-fixnum 0)
                   (if (eq type 'float)
                       (random 10000)
                       (random (ash most-positive-fixnum -2)))))
        (fix8 (random-string 8)))
    (destructuring-bind (alloc &optional (alloc-arg 1))
        (if (listp alloc) alloc (list alloc))
      (flet ((make (i)
               (let ((i (+ offset i)))
                 (ecase type
                   (fixnum i)
                   (bignum (+ most-positive-fixnum i))
                   (float (float i 1.0))
                   ;; (fixnum (if (zerop i)
                   ;;             1
                   ;;             (* 65536 i)))
                   (symbol (make-symbol "x"))
                   (string (format nil "~S" i))
                   (string8 (format nil "~A~S~A" fix8 i fix8))
                   (array0 (make-array 0))
                   (array2 (make-array 2))
                   (array4 (make-array 4))
                   (cons (cons i i))
                   (symnum (if (evenp i)
                               (/ i 2)
                               (make-symbol "x")))
                   (symcons (if (evenp i)
                                (cons i i)
                                (make-symbol "x")))
                   (symarray0 (if (evenp i)
                                  (make-array 1)
                                  (make-symbol "x"))))))
             (waste (n)
               (cond ((eq type 'fixnum)
                      (incf offset n))
                     ((eq type 'symbol)
                      (loop repeat n
                            do (vector-push-extend (make-symbol "w")
                                                   *not-garbage-vector*)))
                     ((eq type 'cons)
                      (vector-push-extend (make-list n)
                                          *not-garbage-vector*))
                     ((eq type 'symcons)
                      (vector-push-extend (make-list (floor n 2))
                                          *not-garbage-vector*)
                      (vector-push-extend (make-array (floor n 2))
                                          *not-garbage-vector*))
                     (t
                      (push (make-array n) *not-garbage-list*)))))
        (ecase alloc
          ((:existing :existing-symbol-name)
           (assert (or (eq type 'symbol) (eq type 'string)))
           (when (eq alloc :existing-symbol-name)
             (assert (eq type 'string)))
           (if (not missp)
               (let* ((keys (make-array n-keys))
                      (all (all-keys-for type alloc))
                      (m (length all))
                      (perm (subseq-of-random-permutation m n-keys)))
                 (loop for i below n-keys
                       do (setf (aref keys i) (aref all (aref perm i))))
                 keys)
               (let ((keys (make-array n-keys)))
                 (loop for i below n-keys
                       do (setf (aref keys i)
                                (ecase type
                                  ((symbol) (gensym "y"))
                                  ((string)
                                   (format nil "%+U2~S~A" i
                                           (random-string (random 40)))))))
                 keys)))
          ((:prog)
           (let ((keys (make-array n-keys)))
             (loop for i below n-keys
                   do (setf (aref keys i)
                            (progn (waste (1- alloc-arg))
                                   (make i))))
             keys))
          ((:rnd)
           (let ((keys (make-array n-keys)))
             (loop for i below n-keys
                   do (setf (aref keys i)
                            (progn (waste (random alloc-arg))
                                   (make i))))
             keys))
          ((:random)
           (subseq (shuffle-vector!
                    (setq *not-garbage-vector*
                          (let ((keys (make-array (* 10 n-keys))))
                            (loop for i below (length keys)
                                  do (setf (aref keys i) (make i)))
                            keys)))
                   0 n-keys)))))))

(defun maybe-copy-key (key)
  (cond ((not (member (getf *ht-args* :test 'eql) '(equal equalp)))
         key)
        ((and (consp key) (not (listp (cdr key))))
         (cons (car key) (cdr key)))
        ((typep key 'sequence)
         (copy-seq key))
        (t
         key)))

(defvar *table-stream* nil)

(defun %%bench-hash (n-keys n-repeats n-concurrent type alloc)
  (declare (type fixnum n-keys n-repeats n-concurrent))
  (let* ((put-timing nil)
         (get-timing nil)
         (miss-timing nil)
         (del-timing nil)
         (sum-vacancy 0)
         (sum-avg-regret 0)
         ;; (sum-avg-cost+ 0)
         (n-bits nil)
         (size nil)
         (hash-fun-counts (make-hash-table))
         ;; We take N-REPEATS basic measurements (timings of put, get,
         ;; miss, and del). Each measurement is the total time it takes
         ;; to perform N-KEYS * N-CONCURRENT operations.
         (n-ops-per-measurement (* n-keys n-concurrent))
         ;; Every TIMING-VALUE in all measurements is scaled by
         ;; WEIGHT. The WEIGHT of e.g. PUT-TIMING is thus (/
         ;; *NORMALIZE-TIMINGS-TO-N-KEYS* N-CONCURRENT).
         (weight (/ *normalize-timings-to-n-keys*
                    (* n-repeats n-ops-per-measurement))))
    (terpri)
    (format t "~S ~Ss, alloc=~S (runs: ~S*~S*~F)"
            n-keys type alloc n-repeats n-concurrent weight)
    (loop for repeat below n-repeats do
      (let* ((*not-garbage-list* ())
             (*not-garbage-vector* (make-array 0 :adjustable t :fill-pointer t))
             (hash-tables (loop repeat n-concurrent
                                collect (apply #'make-hash-table *ht-args*)))
             (key-sets (loop repeat n-concurrent
                             collect (allocate-keys n-keys type alloc)))
             (query-sets (loop for keys in key-sets
                               collect (shuffle (map 'vector #'maybe-copy-key
                                                     keys))))
             (miss-sets (loop repeat n-concurrent
                              collect (shuffle (allocate-keys n-keys type alloc
                                                              :missp t))))
             (r nil))
        ;; Reduce the chance of running out of memory due to the
        ;; SB-SYS:WITHOUT-GCING below. This may also reduce the
        ;; randomness of memory addresses.
        (when (< (expt 2 20) n-keys)
          (sb-ext:gc :full t))
        (sb-sys:without-gcing
          (with-timing-added ((put-timing) :weight weight)
            (locally (declare (optimize speed (safety 0) (debug 0)))
              (loop for h of-type hash-table in hash-tables
                    for keys of-type simple-vector in key-sets
                    do (loop for key across keys
                             do (setf (gethash key h) t))))))
        (loop for h of-type hash-table in hash-tables
              do (assert (= (hash-table-count h) n-keys)))
        (loop for h in hash-tables do
          (let ((index-vector (sb-impl::hash-table-index-vector h)))
            (setq n-bits (integer-length
                          (1- (if (<= (length index-vector) 2)
                                  (hash-table-size h)
                                  (length index-vector)))))
            (setq size (hash-table-size h))
            (let ((s (or (find-symbol "HASH-TABLE-HASH-FUN-STATE" :sb-impl)
                         (find-symbol "HASH-TABLE-%HASH-FUN-STATE" :sb-impl))))
              (when s
                (incf (gethash (funcall (symbol-function s) h)
                               hash-fun-counts 0))))
            (incf sum-vacancy (/ (count 0 index-vector)
                                 (length index-vector)))
            #+nil
            (when (plusp (avg-ht-regret h))
              (let ((*print-length* 160))
                (print (sb-impl::hash-table-hash-fun-state h))
                (print (sb-impl::hash-table-pairs h))
                (print (sb-impl::hash-table-index-vector h))
                (print (sb-impl::hash-table-next-vector h)))
              (break))
            (incf sum-avg-regret (avg-ht-regret h))
            #+nil (incf sum-avg-cost+ (avg-ht-cost+ h))))
        (with-timing-added ((get-timing) :weight weight)
          (locally (declare (optimize speed (safety 0) (debug 0)))
            (loop for h of-type hash-table in hash-tables
                  for queries of-type simple-vector in query-sets
                  do (loop for query across queries
                           do (setq r (gethash query h))))))
        (with-timing-added ((miss-timing) :weight weight)
          (locally (declare (optimize speed (safety 0) (debug 0)))
            (loop for h of-type hash-table in hash-tables
                  for misses of-type simple-vector in miss-sets
                  do (loop for key across misses
                           do (setq r (gethash key h))))))
        (with-timing-added ((del-timing) :weight weight)
          (locally (declare (optimize speed (safety 0) (debug 0)))
            (loop for h of-type hash-table in hash-tables
                  for queries of-type simple-vector in query-sets
                  do (loop for key across queries
                           do (setq r (remhash key h))))))
        (setq *not-garbage-list* r)))
    (loop for (hash-fun count)
            in (sort (loop for hash-fun being the hash-keys of hash-fun-counts
                           for count being the hash-values of hash-fun-counts
                           collect (list hash-fun count))
                     #'> :key #'second)
          for i upfrom 0 below 5
          do (format t ", ~5,1F% #x~X"
                     (* 100 (/ count n-repeats n-concurrent))
                     hash-fun))
    (terpri)
    (when *table-stream*
      (flet ((ns-per-op (timing)
               (/ (* (+ (getf timing :user-run-time-us)
                        (getf timing :system-run-time-us))
                     ;; Convert to nanoseconds.
                     1000)
                  ;; Undo the normalization.
                  *normalize-timings-to-n-keys*)))
        (format *table-stream* "~12D ~12D ~12D ~12,3F ~12,3F ~12,3F ~12,3F ~
                                ~12,3F ~12,3F ~12,3F ~12,3F~%"
                n-keys size (ash 1 n-bits)
                (/ sum-vacancy n-repeats n-concurrent)
                (expected-vacancy-of-random n-keys (expt 2 n-bits))
                (/ sum-avg-regret n-repeats n-concurrent)
                (expected-regret-of-random n-keys (expt 2 n-bits))
                (ns-per-op put-timing)
                (ns-per-op get-timing)
                (ns-per-op miss-timing)
                (ns-per-op del-timing))
        (force-output *table-stream*)))
    (format t "size=~S, bits=~S, vacancy=~3,2F (rnd=~3,2F), ~
               regret=~4,3F (rnd=~4,3F)~%"
            size n-bits
            (/ sum-vacancy n-repeats n-concurrent)
            (expected-vacancy-of-random n-keys (expt 2 n-bits))
            (/ sum-avg-regret n-repeats n-concurrent)
            (expected-regret-of-random n-keys (expt 2 n-bits))
            #+nil (1- (/ sum-avg-cost+ n-repeats n-concurrent)))
    (format t "PUT ") (print-timing put-timing)
    (format t "GET ") (print-timing get-timing)
    (format t "MIS ") (print-timing miss-timing)
    (format t "DEL ") (print-timing del-timing)))

(defun write-table-header (stream)
  (loop for header in '("nkeys" "capacity" "nbuckets"
                        "vacancy" "rndvacancy" "regret" "rndregret"
                        "putns" "getns" "missns" "delns")
        for i upfrom 0
        do (format stream "~A~A"
                   (make-string (max 0 (- (if (zerop i) 12 13)
                                          (length header)))
                                :initial-element #\Space)
                   header))
  (terpri stream)
  (force-output stream))


;;;; Cost and regret

(defun avg-ht-regret (ht)
  (let ((n-keys (hash-table-count ht))
        (n-buckets (length (sb-impl::hash-table-index-vector ht))))
    (cond ((< n-buckets 8)
           ;; It's a flat hash table.
           -1)
          ((zerop n-keys)
           0)
          (t
           (/ (- (cost-of-bins ht) (perfect-cost n-keys n-buckets))
              n-keys)))))

(defun expected-regret-of-random (n m)
  (if (zerop n)
      0
      (- (1+ (/ (1- n) (* 2 m)))
         (/ (perfect-cost n m) n))))

(defun expected-vacancy-of-random (n m)
  (let* ((m (coerce m 'double-float))
         (n-expected-empty (* m (expt (- 1 (/ m)) n))))
    (/ n-expected-empty m)))

(defun cost-of-bins (ht)
  (let ((index-vector (sb-impl::hash-table-index-vector ht))
        (next-vector (sb-impl::hash-table-next-vector ht)))
    (loop for i below (length index-vector)
          sum (cost-of-bin (loop with bucket = (aref index-vector i)
                                 until (zerop bucket)
                                 sum 1
                                 do (setq bucket (aref next-vector bucket)))))))

(defun cost-of-bin (b)
  (declare (type (unsigned-byte 31) b)
           (optimize speed))
  (ash (* b (1+ b)) -1))

;;; Return the total cost of accessing all N keys in a hash table with
;;; S buckets when the keys are distributed as uniformly as possible.
;;; If N = S, then this is simply N.
(defun perfect-cost (n s)
  (multiple-value-bind (q r) (floor n s)
    ;; There are R bins with Q+1 elements. The rest have Q.
    (+ (* r (cost-of-bin (1+ q)))
       (* (- s r) (cost-of-bin q)))))


;;;; Cache-aware notion of cost (UNFINISHED)

;;; Key comparison has a cost of 1 (same as in the non cache-ware
;;; case). Accessing memory not in the cache incurs an additional
;;; CACHE-MISS-COST.
(defun cost-of-bins+ (ht n-keys-in-cache-line cache-miss-cost)
  (let ((index-vector (sb-impl::hash-table-index-vector ht))
        (next-vector (sb-impl::hash-table-next-vector ht)))
    (loop for i below (length index-vector)
          sum (cost-of-bin+ index-vector next-vector i n-keys-in-cache-line
                            cache-miss-cost))))

;;; The normal COST-OF-BIN is a simple quadratic expression of the
;;; chain length, but this cache-aware version needs to look at the
;;; actual buckets to recognize cache misses. Still, the model here is
;;; quite simplisitic: access to a bucket is deemed a cache miss if it
;;; is not on the same cache line than the previous element in the
;;; chain (or it's the first element).
(defun cost-of-bin+ (index-vector next-vector i n-keys-in-cache-line
                     cache-miss-cost)
  ;; SUM-COST keeps track of the cost incurred by iterating over the
  ;; chain. Later elements pay for the cost of all earlier elements.
  (let ((sum-cost 0))
    (loop for prev-bucket = most-negative-fixnum then bucket
          for bucket = (aref index-vector i) then (aref next-vector bucket)
          until (zerop bucket)
          do (let ((same-cache-line
                     (= (floor bucket n-keys-in-cache-line)
                        (floor prev-bucket n-keys-in-cache-line))))
               (incf sum-cost (if same-cache-line 1 (1+ cache-miss-cost))))
          sum sum-cost)))

(defun avg-ht-cost+ (ht)
  (if (zerop (hash-table-count ht))
      0
      (/ (cost-of-bins+ ht 256 4.0d0)
         (hash-table-count ht))))


(defun alloc-name (alloc)
  (if (listp alloc) (first alloc) alloc))

(defun %bench-hash (n-keys type alloc)
  (multiple-value-bind (n-repeats n-concurrent)
      (calc-n-repeats-and-n-concurrent n-keys)
    (%%bench-hash n-keys n-repeats n-concurrent type alloc)))

(defun bench-hash (&key (ht-args '(:test eq)) (type 'fixnum) (alloc '(:prog))
                     (start 1) (end 1000) table-file)
  (format t "Hash table parameters: ~{~S~^ ~}~%" ht-args)
  (format t "type=~S, alloc=~S, start=~S, end=~S~%" type alloc start end)
  (let ((*ht-args* ht-args)
        (max-keys (n-available-keys type alloc)))
    (when max-keys
      (format t "~S keys are available." max-keys)
      (when (< max-keys end)
        (format t " Lowering END.")
        (setq end max-keys))
      (terpri))
    (when (eq (getf ht-args :test 'eql) 'equal)
      (setq *time-n-keys* (floor *time-n-keys* 2)))
    (when table-file
      (format t "Writing table of measurements to ~S.~%" table-file))
    (format t "Run times (cpu) per ~S key(s) are reported.~%~
               These are estimated with ~S keys, ~
               with at least ~S in a single measurement,~%~
               and at least ~S measurements.~%~
               Measurement overhead is about ~Fns.~%"
            *normalize-timings-to-n-keys* *time-n-keys* *min-n-keys-to-time*
            *min-n-repeats* (estimate-call-with-timing-overhead))
    (with-open-file (*table-stream* (or table-file "/dev/null")
                                    :direction :output
                                    :if-does-not-exist :create
                                    :if-exists nil)
      (cond ((and table-file (null *table-stream*))
             (format t "~S exists. Skipping experiment.~%" table-file))
            (t
             (write-table-header *table-stream*)
             (format t "Determining hash table resize ranges ...~%")
             (let ((ranges (list-ht-ranges start end ht-args)))
               (time
                (loop for (from to size n-buckets) in ranges
                      do (format t "~%----- size=~S, n-buckets=~S ------~%"
                                 size n-buckets)
                         (%bench-hash from type alloc)
                         (unless (= (1- to) from)
                           (%bench-hash (1- to) type alloc))))
               (terpri)))))))
