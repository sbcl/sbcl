;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;;; This file implements abstract types which map globaldb Info-Number/Name
;;;; pairs to data values. The database itself is defined in 'globaldb'.

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

(declaim (ftype (sfunction (unsigned-byte)
                           (unsigned-byte #.sb!vm:n-positive-fixnum-bits))
                primify))
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

(declaim (ftype (sfunction (t) (unsigned-byte #.sb!vm:n-positive-fixnum-bits))
                globaldb-sxhashoid))
(defstruct (info-hashtable (:conc-name info-env-))
  (storage (make-info-storage 30) :type simple-vector)
  (comparator #'equal :type function)
  (hash-function #'globaldb-sxhashoid :type function)
  (mutex (sb!thread:make-mutex))
  ;; COUNT is always at *least* as large as the key count.
  ;; If no insertions are in progress, it is exactly right.
  (count 0 :type word))

(def!method print-object ((self info-hashtable) stream)
  (declare (stream stream))
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~D/~D entr~:@P" (info-env-count self)
            (info-storage-capacity (info-env-storage self)))))

;; We can't assume that the host lisp supports a CAS operation for info-gethash
;; or info-puthash, but that's fine because there's only one thread.
(defmacro info-cas (storage index oldval newval)
  #+sb-xc-host
  `(xc-compare-and-swap-svref ,storage ,index ,oldval ,newval)
  #-sb-xc-host
  `(cas (svref ,storage ,index) ,oldval ,newval))

;; Similarly we need a way to atomically adjust the hashtable count.
(declaim (inline info-env-adjust-count))
(defun info-env-adjust-count (table delta)
  #+sb-xc-host
  (prog1 (info-env-count table) (incf (info-env-count table) delta))
  #-sb-xc-host
  ;; Inform the compiler that this is not possibly a bignum,
  ;; since it's true upper bound is the info storage threshold.
  (truly-the info-cell-index (atomic-incf (info-env-count table) delta)))

(declaim (inline make-info-forwarding-pointer
                 info-forwarding-pointer-target
                 info-value-moved-p)
         (ftype (sfunction (t) simple-vector) info-env-rehash)
         (ftype (sfunction (t t) simple-vector) %wait-for-rehash))

;; Concurrent access relies on a forwarding pointer being placed into
;; transported value cells. Since this is not a fully general hashtable,
;; we can use fixnums as forwarding pointers, as they're otherwise
;; never present as a value.
#+sb-xc-host
(progn
  (defun xc-compare-and-swap-svref (vector index old new)
    (let ((actual-old (svref vector index)))
      (if (eq old actual-old)
          (progn (setf (svref vector index) new) old)
          (error "xc bug. CAS expected ~S got ~S" old actual-old))))
  (defun make-info-forwarding-pointer (index) index)
  (defun info-forwarding-pointer-target (pointer) pointer)
  (defun info-value-moved-p (val) (fixnump val)))

;; However, here is a forwarding-pointer representation that allows fixnums
;; as legal values in the table, so, since it's more general, ... why not?
#-sb-xc-host
(progn
  (defun make-info-forwarding-pointer (index)
    (declare (type info-cell-index index) (optimize (safety 0)))
    (%make-lisp-obj (+ (ash index 8) sb!vm:unbound-marker-widetag)))
  (defun info-forwarding-pointer-target (marker)
    (ash (get-lisp-obj-address marker) -8))
  (defun info-value-moved-p (x)
    (eq (logand (get-lisp-obj-address x) #xff)
        sb!vm:unbound-marker-widetag)))

;; The common skeleton of {Get, Put, Rehash} operations. Probe key cells until
;; either a hit occurs, in which case the :HIT form is executed and looping
;; stops; or an empty slot is seen in which case the :MISS code is executed.
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
;; makes progess. We don't protect against untimely death of the thread
;; that holds the lock.
;;
(defun %wait-for-rehash (env storage)
  ;; kinda spin, except not quite that bad
  (loop (sb!thread:thread-yield) ; relinquish time slice, supposing it helps
        (if (eq (info-env-storage env) storage)
            ;; Grab and release the mutex for no other reason than to
            ;; observe that a rehasher doesn't (for the moment) have it.
            (sb!thread:with-mutex ((info-env-mutex env))) ; do nothing, retry
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
    ;; it can't hurt. 'info-storage-next' can be empty until at least one cell
    ;; has been forwarded, but in general an unsynchronized read might be
    ;; perceived as executing before a conditional that guards whether the
    ;; read should happen at all. 'storage-next' has the deceptive look of a
    ;; data dependency but it's not - it's a control dependency which as per
    ;; the 'memory-barriers.txt' document at kernel.org, demands a barrier.
    ;; The subsequent ref to storage (if the loop iterates) has a
    ;; data-dependency, and will never be reordered except on an Alpha :-(
    ;; so we _don't_ need a barrier after resetting 'storage' and 'index'.
    :hit (let ((index (value-index)))
           (loop (let ((value
                        (sb!thread:barrier (:read) (svref storage index))))
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
  (aver (not (member key '(0 -1))))
  (labels ((follow/update (array value-index)
             (let ((value ; see INFO-GETHASH for this barrier's rationale
                    (sb!thread:barrier (:read) (svref array value-index))))
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
                       ;; PPC CAS vops sync as a nececessity of performing CAS.
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
            (sb!thread:with-mutex ((info-env-mutex env))
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

    (sb!thread:barrier (:write) ; Publish NEW-STORAGE before scanning keys.
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
                 (sb!thread:barrier (:write)
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
;;
(defun (setf info-gethash) (newval key env)
  (dx-flet ((update (old) (declare (ignore old)) newval))
    (info-puthash env key #'update)))

(defun show-info-env (env)
  (info-maphash (lambda (k v) (format t "~S -> ~S~%" k v)) env))

;;;; Info-Vectors
;;;; ============

;;; Info for a Name (an arbitrary object) is stored in an Info-Vector,
;;; which is like is a 2-level association list. Info-Vectors are stored in
;;; symbols for most names, or in the global hashtable for "complicated" names.

;;; Such vectors exists in two variations: packed and unpacked.
;;; The representations are nearly equivalent for lookup, but the packed format
;;; is more space-efficient, though difficult to manipulate except by unpacking.

;;; Consider a family of Names whose "root" is SB-MOP:STANDARD-INSTANCE-ACCESS.
;;;  1. SB-MOP:STANDARD-INSTANCE-ACCESS
;;;  2. (SETF SB-MOP:STANDARD-INSTANCE-ACCESS)
;;;  3. (CAS SB-MOP:STANDARD-INSTANCE-ACCESS)
;;;
;;; Those three names share one Info-Vector. Conceptually the outer alist key
;;; is NIL for the first of those names, and SETF/CAS for the latter two.
;;; The inner alist key is a number identifying a type of info.
;;; If it were actually an alist, it would look like this:
;;;
;;;  ((nil  (63 . #<fdefn SB-MOP:STANDARD-INSTANCE-ACCESS>) (1 . :FUNCTION) ...)
;;;   (SETF (63 . #<fdefn (SETF SB-MOP:STANDARD-INSTANCE-ACCESS)>) ...)
;;;   (CAS  (63 . #<fdefn (CAS SB-MOP:STANDARD-INSTANCE-ACCESS)>) ...)
;;;   ...)
;;;
;;; Note:
;;; * The root name is exogenous to the vector - it is not stored.
;;; * The info-number for (:FUNCTION :DEFINITION) is 63, :KIND is 1, etc.
;;; * Names which are lists of length other than 2, or improper lists,
;;;   or whose elements are not both symbols, are disqualified.

;;; Packed vector layout
;;; --------------------
;;; Because the keys to the inner lists are integers in the range 0 to 63,
;;; either 5 or 10 keys will fit into a fixnum depending on word size.
;;; This permits one memory read to retrieve a collection of keys. In packed
;;; format, an ordered set of keys ("fields") is called a "descriptor".
;;;
;;; Descriptors are stored from element 0 upward in the packed vector,
;;; and data are indexed downward from the last element of the vector.
;;;
;;;  #(descriptor0 descriptor1 ... descriptorN valueN ... value1 value0)
;;;
;;; e.g. The field at absolute index 3 - vector element 0, bit position 18 -
;;; will find its data at index (- END 3).  In this manner, it doesn't matter
;;; how many more descriptors exist.

;;; A "group" comprises all the info for a particular Name, and its list
;;; of types may may span descriptors, though rarely.
;;; An "auxilliary key" is the first element of a 2-list Name. It is interposed
;;; within the data portion of the vector after the preceding info group.
;;; Descriptors are self-delimiting in that the first field in a group
;;; indicates the number of additional fields in the group.

;;; Unpacked vector layout
;;; ----------------------
;;; This representation is used transiently during insertion/deletion.
;;; It is a concatenation of plists as a vector, interposing at the splice
;;; points the auxilliary key for the group, except for the root name which
;;; does not store an auxilliary key.
;;;
;;; Unpacked vector format looks like:
;;;
;;;                                 /- next group starts here
;;;                                 v
;;;  #(length type val type val ... KEY length type val ... KEY length ...)
;;;    ^
;;;    info group for the primary Name, a/k/a "root symbol", starts here
;;;
;;; One can envision that the first info group stores its auxilliary key
;;; at vector index -1 when thinking about the correctness of algorithms
;;; that process unpacked info-vectors.
;;; See !TEST-PACKIFY-INFOS for examples of each format.

;;;;; Some stuff moved from 'globaldb.lisp':

(defconstant info-num-mask (ldb (byte info-number-bits 0) -1)) ; #b111111

;; Using 6 bits per packed field, 5 infos can be described in a 30-bit fixnum,
;; or 10 in a fixnum on 64-bit machines (regardless of n-fixnum-tag-bits).
;; The eval-when seems to be necessary for building with CCL as host.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +infos-per-word+ (floor sb!vm:n-fixnum-bits info-number-bits)))

;; Descriptors are target fixnums
(deftype info-descriptor () `(signed-byte ,sb!vm:n-fixnum-bits))

;; An empty info-vector. Its 0th field describes that there are no more fields.
(defconstant-eqx +nil-packed-infos+ #(0) #'equalp)

;; FDEFINITIONs have an info-number that admits slightly clever logic
;; for INFO-VECTOR-FDEFN. Do not change this constant without
;; careful examination of that function.
(defconstant +fdefn-info-num+ info-num-mask)

;; Extract a field from a packed info descriptor.
;; A field is either a count of info-numbers, or an info-number.
(declaim (inline packed-info-field))
(defun packed-info-field (vector desc-index field-index)
  (ldb (byte info-number-bits
             (* (the (mod #.+infos-per-word+) field-index) info-number-bits))
       (the info-descriptor (svref vector desc-index))))

;; Compute the number of elements needed to hold unpacked VECTOR after packing.
;; This is not "compute-packed-info-size" since that could be misconstrued
;; as expecting the vector to be already packed.
;;
(defun compute-packified-info-size (vector &optional (end (length vector)))
  (declare (simple-vector vector)) ; unpacked format
  (let ((index 0) ; index into the unpacked input vector
        (n-fields 0)) ; will be the total number of packed fields
    (declare (type index index end n-fields))
    (loop
       ;; 'length' is the number of vector elements in this info group,
       ;; including itself but not including its auxilliary key.
       (let ((length (the index (svref vector index))))
         ;; Divide by 2 because we only count one field for the entry, but the
         ;; input vector had 2 cells per entry. Add 1 because the group's field
         ;; count is accounted for in the total packed field count.
         (incf n-fields (1+ (ash length -1)))
         (incf index (1+ length)) ; jump over the entries, +1 for aux-key
         (when (>= index end)
           ;; The first info group lacks an aux-key, making n-fields 1 too high
           ;; in terms of data cells used, but correct for packed fields.
           (return (+ (ceiling n-fields +infos-per-word+) (1- n-fields))))))))

;; MAKE-INFO-DESCRIPTOR is basically ASH-LEFT-MODFX, shifting VAL by SHIFT.
;; It is important that info descriptors be target fixnums, but 'cross-modular'
;; isn't loaded early enough to use 'mask-signed-field'.
;; It's not needed on 64-bit host/target combination because 10 fields (60 bits)
;; never touch the sign bit.
;; FIXME: figure out why the definition of ash-left-modfx is
;; conditionalized out for platforms other than x86[-64].
;; It looks like it ought to work whether or not there are vops.
(defmacro make-info-descriptor (val shift)
  (if (> sb!vm:n-fixnum-bits 30)
      `(ash ,val ,shift)
      `(logior (if (logbitp (- 29 ,shift) ,val) sb!xc:most-negative-fixnum 0)
               (ash ,val ,shift))))

;; Convert unpacked vector to packed vector.
;; 'pack-infos' would be a hypothetical accessor for the 'infos' of a 'pack'
;; (whatever that is ...) so verbifying as such makes it more mnemonic to me.
;;
(defun packify-infos (input &optional (end (length input)))
  (declare (simple-vector input))
  (let* ((output (make-array (compute-packified-info-size input end)))
         (i -1) ; input index: pre-increment to read the next datum
         (j -1) ; descriptor index: pre-increment to write
         (k (length output)) ; data index: pre-decrement to write
         (field-shift 0)
         (word 0))
    (declare (type index-or-minus-1 i j k end)
             (type (mod #.(1+ (* (1- +infos-per-word+) info-number-bits)))
                   field-shift)
             (type info-descriptor word))
    (flet ((put-field (val) ; insert VAL into the current packed descriptor
             (declare (type info-number val))
             (setq word (logior (make-info-descriptor val field-shift) word))
             (if (< field-shift (* (1- +infos-per-word+) info-number-bits))
                 (incf field-shift info-number-bits)
                 (setf (svref output (incf j)) word field-shift 0 word 0))))
      ;; Truncating divide by 2: count = n-elements in the group @ 2 per entry,
      ;; +1 for count itself but not including its aux-key.
      (loop (let ((count (ash (the index (svref input (incf i))) -1)))
              (put-field count) ; how many infos to follow
              (dotimes (iter count)
                (put-field (svref input (incf i))) ; an info-number
                (setf (svref output (decf k)) (svref input (incf i)))) ; value
              (when (>= (incf i) end)
                (return))
              (setf (svref output (decf k)) (svref input i))))) ; an aux-key
    (unless (zerop field-shift) ; store the final descriptor word
      (setf (svref output (incf j)) word))
    (aver (eql (1+ j) k)) ; last descriptor must be adjacent final data cell
    output))

;; Within the scope of BODY, bind GENERATOR to a local function which
;; returns the next field from a descriptor in INPUT-VAR, a packed vector.
;; The generator uses DESCRIPTOR-INDEX and updates it as a side-effect.
;;
(defmacro !with-packed-info-iterator ((generator input-var
                                                 &key descriptor-index)
                                      &body body)
  (with-unique-names (input word count)
    `(let* ((,input (the simple-vector ,input-var))
            (,descriptor-index -1)
            (,count 0)
            (,word 0))
       (declare (type info-descriptor ,word)
                (fixnum ,count)
                (type index-or-minus-1 ,descriptor-index))
       (flet ((,generator ()
                (when (zerop ,count)
                  (incf ,descriptor-index)
                  (setq ,word (svref ,input ,descriptor-index)
                        ,count +infos-per-word+))
                (prog1 (logand ,word info-num-mask)
                  (setq ,word (ash ,word (- info-number-bits)))
                  (decf ,count))))
         ,@body))))

;; Iterate over VECTOR, binding DATA-INDEX to the index of each aux-key in turn.
;; TOTAL-N-FIELDS is deliberately exposed to invoking code.
;;
(defmacro !do-packed-info-vector-aux-key ((vector &optional (data-index (gensym)))
                                          step-form &optional result-form)
  (with-unique-names (descriptor-idx field-idx)
    (once-only ((vector vector))
      `(let ((,data-index (length ,vector))
             (,descriptor-idx 0)
             (,field-idx 0)
             (total-n-fields 0))
         (declare (type index ,data-index ,descriptor-idx total-n-fields)
                  (type (mod #.+infos-per-word+) ,field-idx))
         ;; Loop through the descriptors in random-access fashion.
         ;; Skip 1+ n-infos each time, because the 'n-infos' is itself a field
         ;; that is not accounted for in its own value.
         (loop (let ((n (1+ (packed-info-field ,vector
                                               ,descriptor-idx ,field-idx))))
                 (incf total-n-fields n)
                 (multiple-value-setq (,descriptor-idx ,field-idx)
                   (floor total-n-fields +infos-per-word+))
                 (decf ,data-index n))
               ;; Done when the ascending index and descending index meet
               (unless (< ,descriptor-idx ,data-index)
                 (return ,result-form))
               ,@(if step-form (list step-form)))))))

;; Return all function names that are stored in SYMBOL's info-vector.
;; As an example, (INFO-VECTOR-NAME-LIST 'SB-PCL::DIRECT-SUPERCLASSES) =>
;; ((SB-PCL::SLOT-ACCESSOR :GLOBAL SB-PCL::DIRECT-SUPERCLASSES SB-PCL::READER)
;;  (SB-PCL::SLOT-ACCESSOR :GLOBAL SB-PCL::DIRECT-SUPERCLASSES BOUNDP)
;;  (SB-PCL::SLOT-ACCESSOR :GLOBAL SB-PCL::DIRECT-SUPERCLASSES SB-PCL::WRITER))
(defun info-vector-name-list (symbol)
  (let ((vector (symbol-info-vector symbol))
        (list))
    (when vector
      (!do-packed-info-vector-aux-key (vector key-index)
        (push (construct-globaldb-name (svref vector key-index) symbol)
              list))
      (nconc (and (plusp (packed-info-field vector 0 0)) (list symbol))
             (nreverse list)))))

;; Compute the number of elements needed to hold packed VECTOR after unpacking.
;; The unpacked size is the number of auxilliary keys plus the number of entries
;; @ 2 cells per entry, plus the number of length cells which indicate the
;; number of data cells used (including length cells but not aux key cells).
;; Equivalently, it's the number of packed fields times 2 minus 1.
;;
(defun compute-unpackified-info-size (vector)
  (declare (simple-vector vector))
  (!do-packed-info-vector-aux-key (vector) ()
    ;; off-by-one: the first info group's auxilliary key is imaginary
    (1- (truly-the fixnum (ash total-n-fields 1)))))

;; Convert packed INPUT vector to unpacked.
;; If optional OUTPUT is supplied, it is used, otherwise output is allocated.
;; For efficiency the OUTPUT should be provided as a dynamic-extent array.
;;
(defun unpackify-infos (input &optional (output nil output-supplied-p))
  (declare (simple-vector input))
  (unless output-supplied-p
    (setq output (make-array (compute-unpackified-info-size input))))
  (let ((i (length input)) (j -1))  ; input index and output index respectively
    (declare (type index-or-minus-1 i j))
    (!with-packed-info-iterator (next-field input :descriptor-index desc-idx)
      (loop ; over name
         (let ((n-infos (next-field)))
           ;; store the info group length, including the length cell in the length
           (setf (svref output (incf j)) (1+ (ash n-infos 1)))
           (dotimes (iter n-infos) ; over info-types
             (setf (svref output (incf j)) (next-field) ; type-num
                   (svref output (incf j)) (svref input (decf i))))) ; value
         (if (< desc-idx (decf i)) ; as long as the indices haven't met
             (setf (svref output (incf j)) (svref input i)) ; copy next aux-key
             (return (if output-supplied-p nil output))))))) ; else done

;; Return the index of the 'length' item for an info group having
;; auxilliary-key KEY in unpacked VECTOR bounded by END (exclusive),
;; or NIL if not found.
;;
(defun info-find-aux-key/unpacked (key vector end)
  (declare (type index end))
  (if (eql key +no-auxilliary-key+)
      0 ; the first group's length (= end) is stored here always
      (let ((index 0))
        (declare (type index index))
        (loop
           ;; skip 'length' cells plus the next aux-key
           (incf index (1+ (the index (svref vector index))))
           (cond ((>= index end)
                  (return nil))
                 ;; backward a cell is where the aux-key resides.
                 ((eq (svref vector (1- index)) key)
                  (return index)))))))

;; In packed info VECTOR try to find the auxilliary key SYMBOL.
;; If found, return indices of its data, info descriptor word, and field.
;; If not found, the first value is NIL and the descriptor indices
;; arbitrarily point to the next available descriptor field.
;;
(defun info-find-aux-key/packed (vector symbol)
  ;; explicit bounds checking is done by the code below
  (declare (optimize (safety 0)))
  (aver (simple-vector-p vector))
  (let ((descriptor-idx 0) ; physical index to vector
        (field-idx 0) ; relative index within current descriptor
        ;; On each iteration DATA-IDX points to an aux-key cell
        ;; The first group's imaginary aux-key cell is past the end.
        (data-idx (length (the simple-vector vector))))
    (declare (type index descriptor-idx data-idx)
             (fixnum field-idx)) ; can briefly exceed +infos-per-word+
    ;; Efficiently skip past N-INFOS infos. If decrementing the data index
    ;; hits the descriptor index, we're done. Otherwise increment the field
    ;; index and maybe descriptor index and check again for loop termination.
    (flet ((skip (n-infos &aux (n-fields (1+ n-infos))) ; return T on success
             (cond ((<= (decf data-idx n-fields) descriptor-idx) nil)
                   ;; descriptor-idx < data-idx, so potentially more data.
                   ;; If current descriptor has another field, continue.
                   ((< (incf field-idx n-fields) +infos-per-word+) t)
                   (t ; The descriptor index advances.
                    (loop (incf descriptor-idx)
                          (when (< (decf field-idx +infos-per-word+)
                                   +infos-per-word+)
                            (return (< descriptor-idx data-idx))))))))
      (declare (inline skip))
      ;; While this could compare aux-keys with #'EQUAL, it is not obvious how
      ;; in general one would pick a symbol from the name as that which
      ;; is delegated as the one to hold the info-vector.
      (values (cond ((not (skip (packed-info-field vector 0 0))) nil)
                    ;; At least one aux key is present.
                    ((eq (aref vector data-idx) symbol) data-idx) ; yay
                    ;; aux-key order invariant allows early fail on SETF
                    ((eq symbol 'setf) nil)
                    (t
                     (loop
                      (cond ((not (skip (packed-info-field vector descriptor-idx
                                                           field-idx)))
                             (return nil))
                            ((eq (aref vector data-idx) symbol)
                             (return data-idx))))))
              descriptor-idx field-idx)))) ; can be ignored if 1st val is nil

;; Take a packed info-vector INPUT and insert (AUX-KEY,INFO-NUMBER,VALUE).
;; Packed info-vectors are immutable. Any alteration must create a copy.
;; This is done by unpacking/repacking - it's easy enough and fairly
;; efficient since the temporary vector is stack-allocated.
;;
(defun %packed-info-insert (input aux-key info-number value)
  (declare (simple-vector input) (type info-number info-number))
  (let* ((n-extra-elts
          ;; Test if the aux-key has been seen before or needs to be added.
          (if (and (not (eql aux-key +no-auxilliary-key+))
                   (not (info-find-aux-key/packed input aux-key)))
              4   ; need space for [aux-key, length, type-num, value]
              2)) ; only space for [type-num, value]
         (old-size (compute-unpackified-info-size input))
         (new-size (+ old-size n-extra-elts))
         (new (make-array new-size)))
    (declare (type index old-size new-size)
             (truly-dynamic-extent new))
    (unpackify-infos input new)
    (flet ((insert-at (point v0 v1)
             (unless (eql point old-size) ; slide right
               (replace new new :start1 (+ point n-extra-elts) :start2 point))
             (setf (svref new point) v0
                   (svref new (+ point 1)) v1)))
      (cond ((= n-extra-elts 4)
             ;; creating a new aux key. SETF immediately follows the data
             ;; for the primary Name. All other aux-keys go to the end.
             (let ((point (if (eq aux-key 'setf) (svref new 0) old-size)))
               (insert-at point aux-key 3) ; = add 3 data cells not incl. aux-key
               (setf (svref new (+ point 2)) info-number
                     (svref new (+ point 3)) value)))
            (t
             (let ((data-start (info-find-aux-key/unpacked
                                aux-key new old-size)))
               ;; it had better be found - it was in the packed vector
               (aver data-start)
               ;; fdefn must be the first piece of info for any name.
               ;; This facilitates implementing SYMBOL-FUNCTION without
               ;; without completely decoding the vector.
               (insert-at (+ data-start (if (eql info-number +fdefn-info-num+)
                                            1 (svref new data-start)))
                          info-number value)
               ;; add 2 cells, and verify that re-packing won't
               ;; overflow the 'count' for this info group.
               (aver (typep (ash (incf (svref new data-start) 2) -1)
                            'info-number))))))
    (packify-infos new)))

;; Return T if INFO-VECTOR admits quicker insertion logic - it must have
;; exactly one descriptor for the root name, space for >= 1 more field,
;; and no aux-keys.
(declaim (inline info-quickly-insertable-p))
(defun info-quickly-insertable-p (input)
  (let ((n-infos (packed-info-field input 0 0)))
    ;; We can easily determine if the no-aux-keys constraint is satisfied,
    ;; because a secondary name's info occupies at least two cells,
    ;; one for its aux-key and >= 1 for info values.
    (and (< n-infos (1- +infos-per-word+))
         (eql n-infos (1- (length input))))))

;; Take a packed info-vector INPUT and return a new one with INFO-NUMBER/VALUE
;; added for the root name. The vector must satisfy INFO-QUICKLY-INSERTABLE-P.
;; This code is separate from PACKED-INFO-INSERT to facilitate writing
;; a unit test of this logic against the complete logic.
;;
(defun quick-packed-info-insert (input info-number value)
  ;; Because INPUT contains 1 descriptor and its corresponding values,
  ;; the current length is exactly NEW-N, the new number of fields.
  (let* ((descriptor (svref input 0))
         (new-n (truly-the info-number (length input)))
         (new-vect (make-array (1+ new-n))))
    ;; Two cases: we're either inserting info for the fdefn, or not.
    (cond ((eq info-number +fdefn-info-num+)
           ;; fdefn, if present, must remain the first packed field.
           ;; Replace the lowest field (the count) with +fdefn-info-num+,
           ;; shift everything left 6 bits, then OR in the new count.
           (setf (svref new-vect 0)
                 (logior (make-info-descriptor
                          (dpb +fdefn-info-num+ (byte info-number-bits 0)
                               descriptor) info-number-bits) new-n)
                 ;; Packed vectors are indexed "backwards". The first
                 ;; field's info is in the highest numbered cell.
                 (svref new-vect new-n) value)
           (loop for i from 1 below new-n
                 do (setf (svref new-vect i) (svref input i))))
          (t
           ;; Add a field on the high end and increment the count.
           (setf (svref new-vect 0)
                 (logior (make-info-descriptor
                          info-number (* info-number-bits new-n))
                         (1+ descriptor))
                 (svref new-vect 1) value)
           ;; Slide the old data up 1 cell.
           (loop for i from 2 to new-n
                 do (setf (svref new-vect i) (svref input (1- i))))))
    new-vect))

(declaim (maybe-inline packed-info-insert))
(defun packed-info-insert (vector aux-key info-number newval)
  (if (and (eql aux-key +no-auxilliary-key+)
           (info-quickly-insertable-p vector))
      (quick-packed-info-insert vector info-number newval)
      (%packed-info-insert vector aux-key info-number newval)))

;; Search packed VECTOR for AUX-KEY and INFO-NUMBER, returning
;; the index of the data if found, or NIL if not found.
;;
(defun packed-info-value-index (vector aux-key type-num)
  (declare (optimize (safety 0))) ; vector bounds are AVERed
  (let ((data-idx (length vector)) (descriptor-idx 0) (field-idx 0))
    (declare (type index descriptor-idx)
             (type (mod #.+infos-per-word+) field-idx))
    (unless (eql aux-key +no-auxilliary-key+)
      (multiple-value-setq (data-idx descriptor-idx field-idx)
        (info-find-aux-key/packed vector aux-key))
      (unless data-idx
        (return-from packed-info-value-index nil)))
    ;; Fetch a descriptor and shift out trailing bits that won't be scanned.
    (let* ((descriptor (ash (the info-descriptor (aref vector descriptor-idx))
                            (* (- info-number-bits) field-idx)))
           (n-infos (logand descriptor info-num-mask))
           ;; Compute n things in this descriptor after extracting one field. e.g.
           ;; if starting index = 2, there's space for 7 more fields in 60 bits.
           (swath (min (- +infos-per-word+ field-idx 1) n-infos)))
      ;; Type inference on n-infos deems it to have no lower bound due to DECF.
      (declare (type info-descriptor descriptor)
               (type (unsigned-byte #.info-number-bits) n-infos)
               (type index data-idx))
      ;; Repeatedly shift and mask, which is quicker than extracting a field at
      ;; varying positions. Start by shifting out the n-infos field.
      (setq descriptor (ash descriptor (- info-number-bits)))
      (loop
         (dotimes (j swath)
           (when (eql type-num (logand descriptor info-num-mask))
             (return-from packed-info-value-index
               (the index (- data-idx j 1))))
           (setq descriptor (ash descriptor (- info-number-bits))))
         (when (zerop (decf n-infos swath))
           (return nil))
         (incf descriptor-idx)
         (decf data-idx swath)
         (aver (< descriptor-idx data-idx))
         (setq descriptor (svref vector descriptor-idx)
               swath (min n-infos +infos-per-word+))))))

;; Helper for CLEAR-INFO-VALUES when Name has the efficient form.
;; Given packed info-vector INPUT and auxilliary key KEY2
;; return a new vector in which TYPE-NUMS are absent.
;; When none of TYPE-NUMs were present to begin with, return NIL.
;;
;; While this could determine whether it would do anything before unpacking,
;; clearing does not happen often enough to warrant the pre-check.
;;
(defun packed-info-remove (input key2 type-nums)
  (declare (simple-vector input))
  (when (or (eql (length input) (length +nil-packed-infos+))
            (and (not (eql key2 +no-auxilliary-key+))
                 (not (info-find-aux-key/packed input key2))))
    (return-from packed-info-remove nil)) ; do nothing
  (let* ((end (compute-unpackified-info-size input))
         (new (make-array end))
         (data-start 0))
    (declare (truly-dynamic-extent new) (type index end data-start))
    (unpackify-infos input new)
    (let ((start (info-find-aux-key/unpacked key2 new end)))
      (aver start) ; must be found - it was in the packed vector
      (setq data-start start)) ; point to the group's length cell
    (dolist (type-num type-nums)
      (declare (type info-number type-num))
      (let ((i (loop for probe from (1+ data-start) by 2
                     repeat (ash (svref new data-start) -1) ; =n-entries
                     when (eql (svref new probe) type-num)
                     return probe)))
        ;; N.B. the early termination checks aren't just optimizations,
        ;; they're requirements, because otherwise the loop could smash
        ;; data that does not belong to this auxilliary key.
        (cond ((not i)) ; not found - ignore
              ;; Squash out 2 cells if deleting an info for primary name,
              ;; or for secondary name with at least one more entry.
              ((or (eql data-start 0) (> (svref new data-start) 3))
               (replace new new :start1 i :start2 (+ i 2))
               (decf end 2)
               (when (= (decf (svref new data-start) 2) 1)
                 ;; the group is now comprised solely of its length cell
                 (return))) ; so bail out
              (t
               ;; Delete the sole entry for a secondary name
               ;; by removing aux-key, length, and one entry (a cell pair).
               (replace new new :start1 (- i 2) :start2 (+ i 2))
               (decf end 4) ; shorten by 4 cells, and stop now
               (return)))))
    (let ((delta (- (length new) end)))
      (cond ((zerop delta) nil)
            ;; All empty info-vectors are equivalent, so if
            ;; unpacked vector has no data, return a constant.
            ((eql end 1) +nil-packed-infos+)
            (t (packify-infos new end)))))) ; otherwise repack

;; We need a few magic constants to be shared between the next two functions.
(defconstant-eqx !+pcl-reader-name+ (make-symbol "READER") (constantly t))
(defconstant-eqx !+pcl-writer-name+ (make-symbol "WRITER") (constantly t))
(defconstant-eqx !+pcl-boundp-name+ (make-symbol "BOUNDP") (constantly t))

;; PCL names are physically 4-lists (see "pcl/slot-name")
;; that get treated as 2-component names for globaldb's purposes.
;; Return the kind of PCL slot accessor name that NAME is, if it is one.
;; i.e. it matches (SLOT-ACCESSOR :GLOBAL <sym> READER|WRITER|BOUNDP)
;; When called, NAME is already known to start with 'SLOT-ACCESSOR.
;; This has to be defined before building PCL.
(defun pcl-global-accessor-name-p (name)
  (let* ((cdr (cdr name)) (tail (cdr cdr)) last
         kind)
    (if (and (eq (car cdr) :global)
             (listp tail)
             (symbolp (car tail))
             (listp (setq last (cdr tail)))
             (not (cdr last))
             ;; Return symbols that can't conflict, in case somebody
             ;; legitimates (BOUNDP <f>) via DEFINE-FUNCTION-NAME-SYNTAX.
             ;; Especially important since BOUNDP is an external of CL.
             (setq kind (case (car last)
                          (sb!pcl::reader !+pcl-reader-name+)
                          (sb!pcl::writer !+pcl-writer-name+)
                          (sb!pcl::boundp !+pcl-boundp-name+))))
        ;; The first return value is what matters to WITH-GLOBALDB-NAME
        ;; for deciding whether the name is "simple".
        ;; Return the KIND first, just in case somehow we end up with
        ;; (SLOT-ACCESSOR :GLOBAL NIL WRITER) as a name.
        ;; [It "can't happen" since NIL is a constant though]
        (values kind (car tail))
        (values nil nil))))

;; Construct a name from its parts.
;; For PCL global accessors, produce the real name, not the 2-part name.
;; This operation is not invoked in normal use of globaldb.
;; It is only for mapping over all names.
(defun construct-globaldb-name (aux-symbol stem)
  (cond ((eq aux-symbol !+pcl-reader-name+) (sb!pcl::slot-reader-name stem))
        ((eq aux-symbol !+pcl-writer-name+) (sb!pcl::slot-writer-name stem))
        ((eq aux-symbol !+pcl-boundp-name+) (sb!pcl::slot-boundp-name stem))
        (t (list aux-symbol stem)))) ; something like (SETF frob)

;; Call FUNCTION with each piece of info in packed VECT using ROOT-SYMBOL
;; as the primary name. FUNCTION must accept 3 values (NAME INFO-NUMBER VALUE).
(defun %call-with-each-info (function vect root-symbol)
  (let ((name root-symbol)
        (data-idx (length vect)))
    (declare (type index data-idx))
    (!with-packed-info-iterator (next-field vect :descriptor-index desc-idx)
      (loop ; over name
         (dotimes (i (next-field)) ; number of infos for this name
           (funcall function name (next-field) (svref vect (decf data-idx))))
         (if (< desc-idx (decf data-idx))
             (setq name
                   (construct-globaldb-name (svref vect data-idx) root-symbol))
             (return))))))

#|
Info packing example. This example has 2 auxilliary-keys: SETF and CAS.

(!test-packify-infos '(13 :XYZ 18 "nine" 28 :BAR 7 T)
                     '(SETF 8 NIL 17 :FGX)
                     '(CAS 6 :MUMBLE 2 :BAZ 47 :FOO))
=>
#(109006134805865284 3010 :FOO :BAZ :MUMBLE CAS :FGX NIL SETF T :BAR "nine" :XYZ)

(format nil "~4,'0o ~20,'0o" 3010 109006134805865284)
=> "5702 06032110020734221504"
Reading from right-to-left, converting each 2-digit octal number to decimal:
  4, 13, 18, 28, 7, 2, 8, 17, 3, 6, 2, 47
Which is interpreted as:
  4 infos for the root name.       type numbers: 13, 18, 28, 7
  2 infos for SETF auxilliary-key. type numbers: 8, 17
  3 infos for CAS auxilliary-key.  type numbers: 6, 2, 47

(unpackify-infos (!test-packify-infos ...)) ; same input
=> #(9 13 :XYZ 18 "nine" 28 :BAR 7 T
     SETF 5 8 NIL 17 :FGX
     CAS 7 6 :MUMBLE 2 :BAZ 47 :FOO)
This is interpreted as
  root name, 9 cells: {13->:XYZ,  18->"nine", 28->:BAR, 7->T}
  SETF, 5 cells:      {8->NIL, 17->:FGX}
  CAS, 7 cells:       {6->:MUMBLE, 2->:BAZ, 47->:FOO}
|#

;; Reshape inputs per the above commented example to a packed vector.
;; The info for a symbol's fdefn must precede other info-numbers.
;; and SETF must be the first aux key if other aux keys are present.
;; The test function does not enforce these invariants.
;; N.B. As this function starts with "!", is is omitted from the target image.
(defun !test-packify-infos (&rest lists)
  (flet ((check (plist)
           (and (evenp (length plist))
                (loop for (indicator value) on plist by #'cddr
                      always (typep indicator 'info-number))))
         (add-length-prefix (list) ; computers count better than people
           (cons (1+ (length list)) list)))
    (unless (and (check (first lists))
                 (every (lambda (list)
                          (and (symbolp (car list)) (check (cdr list))))
                        (rest lists)))
      (error "Malformed info entries"))
    (packify-infos
     (coerce (apply #'append
                    (cons (add-length-prefix (first lists))
                          (mapcar (lambda (more)
                                    (cons (car more)
                                          (add-length-prefix (cdr more))))
                                  (rest lists))))
             'vector))))

;; Given a NAME naming a globaldb object, decide whether the NAME has
;; an efficient or "simple" form, versus a general or "hairy" form.
;; The efficient form is either a symbol, a (CONS SYMBOL (CONS SYMBOL NULL)),
;; or a PCL global slot {reader, writer, boundp} function name.
;;
;; If NAME is "simple", bind KEY2 and KEY1 to the elements
;; in that order, and execute the SIMPLE code, otherwise execute the HAIRY code.
;; If ALLOW-ATOM is T - the default - then NAME can be just a symbol
;; in which case its second component is +NO-AUXILLIARY-KEY+.
;;
(defmacro with-globaldb-name ((key1 key2 &optional (allow-atom t))
                              name &key simple hairy)
  (with-unique-names (rest)
    `(let ((,key1 ,name) (,key2 +NO-AUXILLIARY-KEY+))
       (if (or ,@(if allow-atom `((symbolp ,key1)))
               (if (listp ,key1)
                   (let ((,rest (cdr ,key1)))
                     (when (listp ,rest)
                       (cond ((not (cdr ,rest))
                              (setq ,key2 (car ,key1) ,key1 (car ,rest))
                              (and (symbolp ,key1) (symbolp ,key2) ,rest))
                             ((eq (car ,key1) 'sb!pcl::slot-accessor)
                              (multiple-value-setq (,key2 ,key1)
                                (pcl-global-accessor-name-p ,key1))))))))
           ,simple
           ;; The KEYs remain bound, but they should not be used for anything.
           ,hairy))))

;; Given Info-Vector VECT, return the fdefn that it contains for its root name,
;; or nil if there is no value. NIL input is acceptable and will return NIL.
(declaim (inline info-vector-fdefn))
(defun info-vector-fdefn (vect)
  (when vect
    ;; This is safe: Info-Vector invariant requires that it have length >= 1.
    (let ((word (the fixnum (svref vect 0))))
      ;; Test that the first info-number is +fdefn-info-num+ and its n-infos
      ;; field is nonzero. These conditions can be tested simultaneously
      ;; using a SIMD-in-a-register idea. The low 6 bits must be nonzero
      ;; and the next 6 must be exactly #b111111, so considered together
      ;; as a 12-bit unsigned integer it must be >= #b111111000001
      (when (>= (ldb (byte (* info-number-bits 2) 0) word)
                (1+ (ash +fdefn-info-num+ info-number-bits)))
        ;; DATA-REF-WITH-OFFSET doesn't know the info-vector length invariant,
        ;; so depite (safety 0) eliding bounds check, FOLD-INDEX-ADDRESSING
        ;; wasn't kicking in without (TRULY-THE (INTEGER 1 *)).
        (aref vect (1- (truly-the (integer 1 *) (length vect))))))))

;;; Some of this stuff might belong in 'symbol.lisp', but can't be,
;;; because 'symbol.lisp' is :NOT-HOST in build-order.

;; In the target, UPDATE-SYMBOL-INFO is defined in 'symbol.lisp'.
#+sb-xc-host
(defun update-symbol-info (symbol update-fn)
  ;; Never pass NIL to an update-fn. Pass the minimal info-vector instead,
  ;; a vector describing 0 infos and 0 auxilliary keys.
  (let ((newval (funcall update-fn (or (symbol-info-vector symbol)
                                       +nil-packed-infos+))))
    (when newval
      (setf (symbol-info-vector symbol) newval))
    (values)))

;; Return the globaldb info for SYMBOL. With respect to the state diagram
;; presented at the definition of SYMBOL-PLIST, if the object in SYMBOL's
;; info slot is LISTP, it is in state 1 or 3. Either way, take the CDR.
;; Otherwise, it is in state 2 so return the value as-is.
;; In terms of this function being named "-vector", implying always a vector,
;; it is understood that NIL is a proxy for +NIL-PACKED-INFOS+, a vector.
;;
#-sb-xc-host
(progn
 #!-symbol-info-vops (declaim (inline symbol-info-vector))
 (defun symbol-info-vector (symbol)
  (let ((info-holder (symbol-info symbol)))
    (truly-the (or null simple-vector)
               (if (listp info-holder) (cdr info-holder) info-holder)))))

;;; The current *INFO-ENVIRONMENT*, a structure of type INFO-HASHTABLE.
;;; Cheat by setting to nil before the type is proclaimed
;;; so that we can then also proclaim ALWAYS-BOUND.
(defvar *info-environment* nil)
#-sb-xc-host
(declaim (type info-hashtable *info-environment*)
         (always-bound *info-environment*))

;;; Update the INFO-NUMBER for NAME in the global environment,
;;; setting it to NEW-VALUE. This is thread-safe in the presence
;;; of multiple readers/writers. Threads simultaneously writing data
;;; for the same NAME will succeed if the info type numbers differ,
;;; and the winner is indeterminate if the type numbers are the same.
;;;
;;; It is not usually a good idea to think of globaldb as an arbitrary
;;; key-to-value map supporting rapid or frequent update for a key.
;;; This is because each update must shallow-copy any data that existed
;;; for NAME, as a consequence of the very minimal lockfree protocol.
;;;
;;; If, for example, you wanted to track how many times a full-call to
;;; each global function was emitted during compilation, you could create
;;; an info-type (:function :full-calls) and the value of that info-type
;;; could be a cons cell holding an integer. In this way incrementing
;;; the cell contents does not affecting the globaldb. In contrast,
;;; (INCF (INFO :function :full-calls myname)) would perform poorly.
;;;
;;; See also ATOMIC-SET-INFO-VALUE and GET-INFO-VALUE-INITIALIZING
;;; for atomic read/modify/write operations.
;;;
;;; Return the new value so that this can be conveniently used in a
;;; SETF function.
(defun set-info-value (name info-number new-value)
  (when (typep name 'fixnum)
    (error "~D is not a legal INFO name." name))

  ;; Storage of FAST-METHOD and SLOW-METHOD compiler info is largely pointless.
  ;; Why? Because the compiler can't even resolve the "name" of the function in
  ;; many cases. If there are EQL specializers involved, it's clearly impossible
  ;; because the form X in (EQL x) needs to be evaluated. So most of the data
  ;; can't be computed until the file defining the method is loaded. At that
  ;; point, you know that the :KIND is :FUNCTION, and :WHERE-FROM is :DEFINED
  ;; - they can't be anything else. And you also pretty much know the signature,
  ;; since it is based on the specializers, so storing again is a total waste.
  ;;
  ;; FIXME: figure out a way not to store these, or else have globaldb ignore
  ;; the calls to set-info-value, and mock any calls to get-info-value
  ;; so that it looks like the data were stored, if someone tries to read it.
  ;: Or store the data in the method object somehow so that at least dropping
  ;; a method can drop the data. This would work because as mentioned above,
  ;; there is mostly no such thing as purely compile-time info for methods.
  #+nil ; So I can easily re-enable this to figure out what's going on.
  (when (and (consp name)
             (memq (car name) '(sb!pcl::slow-method sb!pcl::fast-method))
             (some #'consp (car (last name))))
    (let ((i (aref sb!c::*info-types* info-number)))
      (warn "Globaldb storing info for ~S~% ~S ~S~% -> ~S"
            name (meta-info-category i) (meta-info-kind i) new-value)))

  (let ((name (uncross name)))
    ;; If the INFO-NUMBER already exists in VECT, then copy it and
    ;; alter one cell; otherwise unpack it, grow the vector, and repack.
    (dx-flet ((augment (vect aux-key) ; VECT is a packed vector, never NIL
                (declare (simple-vector vect))
                (let ((index
                       (packed-info-value-index vect aux-key info-number)))
                  (if (not index)
                      (packed-info-insert vect aux-key info-number new-value)
                      (let ((copy (copy-seq vect)))
                        (setf (svref copy index) new-value)
                        copy)))))
      (with-globaldb-name (key1 key2) name
        :simple
        ;; UPDATE-SYMBOL-INFO never supplies OLD-INFO as NIL.
        (dx-flet ((simple-name (old-info) (augment old-info key2)))
          (update-symbol-info key1 #'simple-name))
        :hairy
        ;; INFO-PUTHASH supplies NIL for OLD-INFO if NAME was absent.
        (dx-flet ((hairy-name (old-info)
                    (augment (or old-info +nil-packed-infos+)
                             +no-auxilliary-key+)))
          (info-puthash *info-environment* name #'hairy-name)))))
  new-value)

;; Instead of accepting a new-value, call NEW-VALUE-FUN to compute it
;; from the existing value.  The function receives two arguments:
;; if there was already a value, that value and T; otherwise two NILs.
;; Return the newly-computed value. If NEW-VALUE-FUN returns the old value
;; (compared by EQ) when there was one, then no globaldb update is made.
(defun %atomic-set-info-value (name info-number new-value-fun)
  (declare (function new-value-fun))
  (when (typep name 'fixnum)
    (error "~D is not a legal INFO name." name))
  (let ((name (uncross name)) new-value)
    (dx-flet ((augment (vect aux-key) ; VECT is a packed vector, never NIL
                (declare (simple-vector vect))
                (let ((index
                       (packed-info-value-index vect aux-key info-number)))
                  (if (not index)
                      (packed-info-insert
                       vect aux-key info-number
                       (setq new-value (funcall new-value-fun nil nil)))
                      (let ((oldval (svref vect index)))
                        (setq new-value (funcall new-value-fun oldval t))
                        (if (eq new-value oldval)
                            vect ; return the old vector
                            (let ((copy (copy-seq vect)))
                              (setf (svref copy index) new-value)
                              copy)))))))
      (with-globaldb-name (key1 key2) name
        :simple
        ;; UPDATE-SYMBOL-INFO never supplies OLD-INFO as NIL.
        (dx-flet ((simple-name (old-info) (augment old-info key2)))
          (update-symbol-info key1 #'simple-name))
        :hairy
        ;; INFO-PUTHASH supplies NIL for OLD-INFO if NAME was absent.
        (dx-flet ((hairy-name (old-info)
                    (augment (or old-info +nil-packed-infos+)
                             +no-auxilliary-key+)))
          (info-puthash *info-environment* name #'hairy-name))))
    new-value))

;; %GET-INFO-VALUE-INITIALIZING is provided as a low-level operation similar
;; to the above because it does not require info metadata for defaulting,
;; nor deal with the keyword-based info type designators at all.
;; In contrast, GET-INFO-VALUE requires metadata.
;; For this operation to make sense, the objects produced should be permanently
;; assigned to their name, such as are fdefns and classoid-cells.
;; Note also that we do not do an initial attempt to read once with INFO,
;; followed up by a double-checking get-or-set operation. It is assumed that
;; the user of this already did an initial check, if such is warranted.
(defun %get-info-value-initializing (info-number name creation-thunk)
  (when (typep name 'fixnum)
    (error "~D is not a legal INFO name." name))
  (let ((name (uncross name))
        result)
    (dx-flet ((get-or-set (info-vect aux-key)
                (let ((index
                       (packed-info-value-index info-vect aux-key info-number)))
                  (cond (index
                         (setq result (svref info-vect index))
                         nil) ; no update to info-vector
                        (t
                         ;; Update conflicts possibly for unrelated info-number
                         ;; can force re-execution. (UNLESS result ...) tries
                         ;; to avoid calling the thunk more than once.
                         (unless result
                           (setq result (funcall creation-thunk)))
                         (packed-info-insert info-vect aux-key info-number
                                             result))))))
      (with-globaldb-name (key1 key2) name
        :simple
        ;; UPDATE-SYMBOL-INFO never supplies OLD-INFO as NIL.
        (dx-flet ((simple-name (old-info) (get-or-set old-info key2)))
          (update-symbol-info key1 #'simple-name))
        :hairy
        ;; INFO-PUTHASH supplies NIL for OLD-INFO if NAME was absent.
        (dx-flet ((hairy-name (old-info)
                    (or (get-or-set (or old-info +nil-packed-infos+)
                                    +no-auxilliary-key+)
                        ;; Return OLD-INFO to elide writeback. Unlike for
                        ;; UPDATE-SYMBOL-INFO, NIL is not a no-op marker.
                        old-info)))
          (info-puthash *info-environment* name #'hairy-name))))
    result))
