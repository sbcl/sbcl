;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;; Quasi-lockfree concurrent hashtable
;;; ===================================

;; References:
;;  http://trac.clozure.com/ccl/wiki/Internals/LockFreeHashTables
;;  https://github.com/boundary/high-scale-lib/blob/master/src/main/java/org/cliffc/high_scale_lib/NonBlockingHashMap.java

;; The basic hashable is a lightweight one using prime number sizing strategy,
;; and a secondary hash for re-probing that is not necessarily co-prime with
;; the table size (as it would be, ideally), and no support for deletion.

;; The lock-free logic differs from each of the preceding reference algorithms.
;; The Java algorithm is truly lock-free: death of any thread will never impede
;; progress in other threads. The CCL algorithm is only quasi-lock-free, as is
;; ours. Were a rehashing thread to terminate abnormally while holding the
;; rehash mutex, all other threads will likely starve at some point.
;;
;; Unlike the CCL algorithm, we allow reading/writing concurrently with rehash.
;; Most operations will never notice that the rehasher has acquired a mutex,
;; because the only case where writers compete for the mutex is in trying
;; to claim a new cell in a storage vector that is being transported, and where
;; the rehasher has already visited the desired cell. Since the rehasher will
;; never re-visit a cell, it disallows updates that it could not perceive.
;; The Java algorithm has additional complexity imposed upon it by the
;; feature that rehashing work is parceling out fairly to threads,
;; whereas we avoid that by having one rehasher transport all cells.

;; A marker for cells that may be claimed by a writer
(defconstant +empty-key+ 0)
;; A marker for cells that may NOT be claimed by a writer.
(defconstant +unavailable-key+ -1)

;; We have a 24-bit cell index. This is generous,
;; allowing for ~5 million Names assuming 60% load factor.
(deftype info-cell-index () `(unsigned-byte 24))

;; An INFO-STORAGE is a vector of a 3-element header followed by all keys
;; and then all values. This way we can simply use FILL to set all keys to 0
;; and all values to NIL.
(defconstant +info-keys-offset+ 3)
(defstruct (info-storage (:type vector) (:constructor nil) (:copier nil))
  ;; Capacity refers to pairs of vector elements. The lower bound is to
  ;; try (in vain) to elide a check for division by 0 in the secondary hash.
  ;; The upper bound is harmlessly wrong by a factor of 2 because
  ;; INFO-CELL-INDEX should be the max physical cell that can be addressed.
  ;; No problem - you'll run out of memory before hitting the edge anyway.
  (capacity  0 :type (and (integer 3 *) info-cell-index)) ; immutable
  ;; Make a new storage if count exceeds this
  (threshold 0 :type info-cell-index) ; immutable
  (next nil :type simple-vector) ; Pending INFO-STORAGE during rehash
  ;; keys ... data ...
  )

;;; Is X is a positive prime integer?
(defun positive-primep (x)
  ;; This happens to be called only on fixnums, we can limit it to
  ;; fixnums for efficiency. (And if we didn't limit it to fixnums, we
  ;; should use a cleverer algorithm, since this one scales pretty
  ;; badly for huge X.)
  (declare (fixnum x)
           (optimize speed (safety 0)))
  (if (<= x 5)
      (and (>= x 2) (/= x 4))
      (and (not (evenp x))
           (not (zerop (rem x 3)))
           (do ((q 6)
                (r 1)
                (inc 2 (logxor inc 6)) ;; 2,4,2,4...
                (d 5 (+ d inc)))
               ((or (= r 0) (> d q)) d (/= r 0))
             (declare ((and fixnum (integer 1)) inc d))
             (multiple-value-setq (q r) (truncate x d))))))

;;; Given any non-negative integer, return a prime number >= to it.
(declaim (ftype (sfunction (unsigned-byte)
                           (unsigned-byte #.sb-vm:n-positive-fixnum-bits))
                primify))
(defun primify (x)
  (do ((n (logior x 1) (+ n 2)))
      ((positive-primep n) n)))

(defun make-info-storage (n-cells-min &optional (load-factor .7))
  ;; If you ask for 40 entries at 50% load, you get (PRIMIFY 80) entries.
  (let* ((n-cells (primify (ceiling n-cells-min load-factor)))
         (a (make-array (+ +info-keys-offset+ (* 2 n-cells))))
         (end (+ +info-keys-offset+ n-cells)))
    (setf (info-storage-capacity a) n-cells
          ;; The threshold should be approximately the same as
          ;; the number you asked for in the first place.
          (info-storage-threshold a) (floor (* n-cells load-factor))
          (info-storage-next a) #()) ; type-correct initial value
    (fill a +empty-key+ :start +info-keys-offset+ :end end)
    (fill a nil :start end)
    a))

(declaim (ftype (sfunction (t) (unsigned-byte #.sb-vm:n-positive-fixnum-bits))
                globaldb-sxhashoid))

(defstruct (info-hashtable (:conc-name info-env-) (:copier nil))
  (storage (make-info-storage 30) :type simple-vector)
  (comparator #'equal :type function)
  (hash-function #'globaldb-sxhashoid :type function)
  (mutex (sb-thread:make-mutex :name "info hashtable"))
  ;; The number of phantom entries for simulated deletion.
  ;; Our tombstones are not the usual ones. Ordinarily an open-addressing
  ;; table will use tombstone keys that can be written over if inserting a new
  ;; pair into a dead entry. That doesn't work for the lockfree algorithm
  ;; as we lack a portable way to atomically load a <k,v> pair and therefore
  ;; a means to detect having read a value for the wrong key in the case of
  ;; insert K1 / delete K1 / insert K2 where K1 and K2 hash to the same bin.
  (tombstones 0 :type word)
  ;; COUNT is always at *least* as large as the key count.
  ;; If no insertions are in progress, it is exactly right.
  (count 0 :type word))

(defmethod print-object ((self info-hashtable) stream)
  (declare (stream stream))
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~D/~D entr~:@P" (info-env-count self)
            (info-storage-capacity (info-env-storage self)))))

(defmacro info-cas (storage index oldval newval)
  `(cas (svref ,storage ,index) ,oldval ,newval))

;; Similarly we need a way to atomically adjust the hashtable count.
(declaim (inline info-env-adjust-count))
(defun info-env-adjust-count (table delta)
  ;; Inform the compiler that this is not possibly a bignum,
  ;; since its true upper bound is the info storage threshold.
  (truly-the info-cell-index (atomic-incf (info-env-count table) delta)))

(declaim (inline make-info-forwarding-pointer
                 info-forwarding-pointer-target
                 info-value-moved-p)
         (ftype (sfunction (t) simple-vector) info-env-rehash)
         (ftype (sfunction (t t) simple-vector) %wait-for-rehash))

;; Concurrent access relies on a forwarding pointer being placed into
;; transported value cells. Use unbound markers for that.
(defun make-info-forwarding-pointer (index)
  (declare (type info-cell-index index) (optimize (safety 0)))
  (%make-lisp-obj (+ (ash index 8) sb-vm:unbound-marker-widetag)))
(defun info-forwarding-pointer-target (marker)
  (ash (get-lisp-obj-address marker) -8))
(defun info-value-moved-p (x)
  (eq (logand (get-lisp-obj-address x) #xff)
      sb-vm:unbound-marker-widetag))

;; The common skeleton of {Get, Put, Rehash} operations. Probe key cells until
;; either a hit occurs, in which case the :HIT form is executed and looping
;; stops; or an empty slot is seen, in which case the :MISS code is executed.
;; :MISS should contain a GO or RETURN on success, otherwise probing continues.
;; No precaution exists against probing forever, such as could happen if the
;; probing strategy fails to visit all candidate free cells.
;;
;; Stepping is done by subtracting a secondary hash rather than adding,
;; as this allows testing for wraparound by comparison to a constant
;; instead of against storage capacity. Nonlinear probing jumps around in
;; the L1 cache regardless, as they key space is a ring so the "direction"
;; of probing is irrelevant. [In contrast, linear probing usually benefits
;; from scanning physically forward, preferably within a cache line,
;; but for practical purposes linear probing is worse.]
;;
(defmacro !do-probe-sequence ((storage key table &optional hash)
                              &key probe hit miss)
  (with-unique-names (test miss-fn len key-index step)
    (once-only ((storage storage) (key key) (table table)
                (hashval
                 `(the fixnum
                       ,(or hash
                            `(funcall (info-env-hash-function ,table) ,key)))))
      `(macrolet ((key-index () ; expose key+value indices to invoking code
                    ',key-index)
                  (value-index ()
                    '(+ (info-storage-capacity ,storage) ,key-index))
                  (,test ()
                    `(let ((probed-key (svref ,',storage ,',key-index)))
                       ,',probe ; could keep a tally of the probes
                       ;; Optimistically test for hit first, then markers
                       (cond ((funcall (info-env-comparator ,',table)
                                       probed-key ,',key)
                              (go :hit))
                             ((or (eql probed-key +unavailable-key+)
                                  (eql probed-key +empty-key+))
                              (,',miss-fn))))))
         (let* ((,len (info-storage-capacity ,storage))
                (,key-index (+ (rem ,hashval ,len) +info-keys-offset+))
                (,step 0))
           (declare (type info-cell-index ,key-index ,step))
           (dx-flet ((,miss-fn () ,miss))
             (tagbody
                (,test)
                ;; only need the second hash if didn't hit on first try
                (setq ,step (1+ (rem ,hashval (- ,len 2))))
              :loop
                (setq ,key-index (let ((next (- ,key-index ,step)))
                                   (if (< next +info-keys-offset+)
                                       (+ next ,len)
                                       next)))
                (,test)
                (go :loop)
              :HIT))
           ,hit)))))

;; Wait for ENV's storage to change to something other than STORAGE, and
;; return the new one. As long as rehash finishes in finite time, every thread
;; makes progress. We don't protect against untimely death of the thread
;; that holds the lock.
;;
(defun %wait-for-rehash (env storage)
  ;; kinda spin, except not quite that bad
  (loop (sb-thread:thread-yield) ; relinquish time slice, supposing it helps
        (if (eq (info-env-storage env) storage)
            ;; Grab and release the mutex for no other reason than to
            ;; observe that a rehasher doesn't (for the moment) have it.
            (sb-thread::with-recursive-system-lock ((info-env-mutex env))) ; do nothing, retry
            (return (info-env-storage env)))))

;; Look in info-environment ENV for the name KEY. Arguments are like GETHASH.
;;
;; Invariant: any KEY's value is present in at most 1 storage.
;; Uniqueness of the location ensures that when writing with CAS, the place is
;; either current, or CAS fails in a way that informs the writer of a new place.
;; At most one probing sequence occurs, in that hitting a key might entail
;; more than one forwarding but never a re-probing.
;; The same is not true on insert, for which probing restart is quite common.
;; When chasing values, INFO-STORAGE-NEXT may or may not be EQ to what is in
;; INFO-ENV-STORAGE, depending whether rehash is still running, has completed,
;; or started all over again before the reader got a chance to chase one time.
;;
(defun info-gethash (key env &aux (storage (info-env-storage env)))
  (!do-probe-sequence (storage key env)
    :miss (return-from info-gethash nil)
    ;; With 99% certainty the :READ barrier is needed for non-x86, and if not,
    ;; it can't hurt. INFO-STORAGE-NEXT can be empty until at least one cell
    ;; has been forwarded, but in general an unsynchronized read might be
    ;; perceived as executing before a conditional that guards whether the
    ;; read should happen at all. STORAGE-NEXT has the deceptive look of a
    ;; data dependency but it's not - it's a control dependency which, as per
    ;; [1], demands a barrier.
    ;; The subsequent ref to storage (if the loop iterates) has a
    ;; data-dependency, and will never be reordered except on an Alpha :-(
    ;; so we _don't_ need a barrier after resetting STORAGE and INDEX.
    ;; Ad 1: https://www.kernel.org/doc/Documentation/memory-barriers.txt
    :hit (let ((index (value-index)))
           (loop (let ((value
                        (sb-thread:barrier (:read) (svref storage index))))
                   (if (info-value-moved-p value)
                       (setq storage (info-storage-next storage)
                             index (info-forwarding-pointer-target value))
                       (return value)))))))

;; ENV and KEY are as above. UPDATE-PROC is invoked with the old value stored
;; for KEY and it should return the possibly-unchanged new value to put.
;;
;; Note a tiny problem of semantics on 'miss' - for an instant after winning
;; CAS of a new key, that key appears to exist with the default value NIL,
;; causing readers to think "FOUND-P" = T which might have a different sense
;; from "not found".  If a Name is transiently "found" despite having no data,
;; nobody cares, at least for globaldb. Remedies, if desired, involve either:
;;  1. Making a "no value" marker that is distinct from NIL.
;;  2. Placing keys/value pairs in adjacent cells and using double-wide CAS.
;; The reason we don't care about this anomaly is that we have to
;; look in an Info-Vector [q.v.] anyway to get the whole picture.
;;
(defun info-puthash (env key update-proc)
  (declare (dynamic-extent update-proc))
  (aver (not (member key '(0 -1))))
  (labels ((follow/update (array value-index)
             (let ((value ; see INFO-GETHASH for this barrier's rationale
                    (sb-thread:barrier (:read) (svref array value-index))))
               (if (info-value-moved-p value)
                   (follow/update (info-storage-next array)
                                  (info-forwarding-pointer-target value))
                   (update array value-index value))))
           (update (array index oldval)
             ;; invariant: OLDVAL is not a forwarding pointer.
             (let ((newval (funcall update-proc oldval)))
               (if (eq newval oldval)
                   oldval ; forgo update
                   (named-let put ((array array) (index index))
                     (let ((actual-old (info-cas array index oldval newval)))
                       ;; Unlike above, this read of storage-next can not
                       ;; be perceived as having occurred prior to CAS.
                       ;; x86 synchronizes at every locked instruction, and our
                       ;; PPC CAS vops sync as a necessity of performing CAS.
                       (cond ((eq oldval actual-old) newval) ; win
                             ((info-value-moved-p actual-old) ; forwarded
                              (put (info-storage-next array)
                                   (info-forwarding-pointer-target actual-old)))
                             (t ; collision with another writer
                              (update array index actual-old)))))))))
    (named-let probe ((hashval (funcall (info-env-hash-function env) key))
                      (storage (info-env-storage env)))
      (!do-probe-sequence (storage key env hashval)
       :hit (follow/update storage (value-index))
       :miss
       (progn
        (let ((old-count (info-env-adjust-count env 1)))
          (declare (type info-cell-index old-count))
          (when (>= old-count (info-storage-threshold storage))
            (sb-thread::with-recursive-system-lock ((info-env-mutex env))
              ;; any thread could have beaten us to rehashing
              (when (eq (info-env-storage env) storage)
                (info-env-rehash env)))
            (info-env-adjust-count env -1) ; prepare to retry
            (return-from probe (probe hashval (info-env-storage env)))))
        ;; Attempt to claim KEY-INDEX
        (let ((oldkey (info-cas storage (key-index) +empty-key+ key)))
          (when (eql oldkey +empty-key+) ; successful claim
            ;; Optimistically assume that nobody else rewrote the value
            (return-from probe (update storage (value-index) nil)))
          (info-env-adjust-count env -1) ; failed
          ;; The fallthrough branch of this COND is ordinary CAS failure where
          ;; somebody else wanted this slot, and won. Looping tries again.
          (cond ((funcall (info-env-comparator env) oldkey key) ; coincidence
                 (return-from probe (follow/update storage (value-index))))
                ((eql oldkey +unavailable-key+) ; Highly unlikely
                 ;; as preemptive check up above ensured no rehash needed.
                 (return-from probe
                   (probe hashval (%wait-for-rehash env storage)))))))))))

;; Rehash ENV to a larger storage. When writing a key into new storage,
;; key cells are uniquely owned by this thread without contention.
;; Other threads may not look in new storage without first observing that
;; a key's value was definitely moved.
;; The rehasher marks empty cells as unusable so that writers can't insert
;; into the subsequence of storage already visited. The rehasher must of
;; course vie for the cell it is trying to mark as unusable.
;;
(defun info-env-rehash (env)
  (let* ((old-count (info-env-count env))
         (old-storage (info-env-storage env))
         ;; the new storage begins life at ~50% capacity
         (new-storage (make-info-storage (ceiling old-count .5)))
         (old-capacity (info-storage-capacity old-storage))
         (new-capacity (info-storage-capacity new-storage)))

    (sb-thread:barrier (:write) ; Publish NEW-STORAGE before scanning keys.
      (setf (info-storage-next old-storage) new-storage))

    (loop for old-key-index of-type info-cell-index
          from +info-keys-offset+ below (+ +info-keys-offset+ old-capacity)
       ;; If the indexed cell is not in use, try once to prevent subsequent
       ;; writes by changing the empty marker to 'unavailable'. The outcome
       ;; determines whether to continue transporting the cell's value.
          for key = (let ((key (svref old-storage old-key-index)))
                      (if (eql key +empty-key+)
                          (info-cas old-storage old-key-index
                                    +empty-key+ +unavailable-key+)
                          key))
          unless (eql key +empty-key+)
          do (let* ((new-key-index
                      (block nil
                        (!do-probe-sequence (new-storage key env)
                          :hit (bug "Globaldb rehash failure. Mutated key?")
                          :miss (return (key-index)))))
                    (old-value-index (+ old-key-index old-capacity))
                    (new-value-index (+ new-key-index new-capacity))
                    (value (svref old-storage old-value-index)))
               (setf (svref new-storage new-key-index) key) ; Q: barrier needed?
               ;; Copy the current value into the new storage,
               ;; and CAS in a forwarding pointer. Repeat until successful.
               (loop
                 ;; Force VALUE to memory before publishing relocated status
                 (sb-thread:barrier (:write)
                   (setf (svref new-storage new-value-index) value))
                 (let ((actual-old
                        (info-cas
                         old-storage old-value-index value
                         (make-info-forwarding-pointer new-value-index))))
                   (if (eq value actual-old)
                       (return)
                       (setq value actual-old))))))

    ;; Typical of most lockfree algorithms, we've no idea when
    ;; the old storage can be freed. GC will figure it out.
    ;; No write barrier needed. Threads still looking at old storage
    ;; will eventually find all cells unavailable or forwarded.
    (setf (info-env-storage env) new-storage)))

;; This maphash implementation is not threadsafe.
;; It can be made threadsafe by following forwarded values
;; and skipping over unavailable keys.
;;
(defmacro info-maphash (fun env) ; map FUN over each key/value
  (with-unique-names (f storage capacity i key)
    `(let* ((,f ,fun)
            (,storage (info-env-storage ,env))
            (,capacity (info-storage-capacity ,storage)))
       (loop for ,i below ,capacity
             for ,key = (svref ,storage (+ ,i +info-keys-offset+))
             unless (eql ,key +empty-key+)
             do (funcall ,f ,key
                         (svref ,storage
                                (+ ,i +info-keys-offset+ ,capacity)))))))

;; CAS is the primitive operation on an info hashtable,
;; and SETF is implemented in terms of CAS. For the most part it is
;; inadvisable to use this for anything other than tinkering at the REPL.
;; Of if the table has external synchronization, it's ok to use.
(defun (setf info-gethash) (newval key env)
  (dx-flet ((update (old) (declare (ignore old)) newval))
    (info-puthash env key #'update)))

(defun show-info-env (env)
  (info-maphash (lambda (k v) (format t "~S -> ~S~%" k v)) env))
