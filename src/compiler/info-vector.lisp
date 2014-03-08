;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;;; This file implements abstract types which map globaldb Type-Number/Name
;;;; pairs to data values compatibly with the volatile and compact environment
;;;; structures defined in 'globaldb.lisp'.
;;;; Some bootstrapping glitches prevent the legacy environment storage
;;;; containers from being totally supplanted by the new ones unfortunately.

;;; Quasi-lockfree concurrent hashtable
;;; ===================================

;; References:
;;  http://trac.clozure.com/ccl/wiki/Internals/LockFreeHashTables
;;  https://github.com/boundary/high-scale-lib/blob/master/src/main/java/org/cliffc/high_scale_lib/NonBlockingHashMap.java

;; The basic hashable is a very lightweight one that shares much in common
;; with the COMPACT-INFO-ENVIRONMENT table: prime number sizing strategy,
;; and a secondary hash for re-probing that is not necessarily co-prime with
;; the table size (as it would be, ideally), and no support for deletion.

;; The lock-free logic differs from each of the preceding reference algorithms.
;; The Java algorithm is truly lock-free: death of any thread will never impede
;; progress in other threads. The CCL algorithm is only quasi-lock-free, as is
;; ours. Were a rehashing thread to terminate abnormally whole holding the
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

(defparameter *info-env-load-limit* .74)
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

(defun make-info-storage (n-cells-min)
  (let* ((n-cells (primify n-cells-min))
         (a (make-array (+ +info-keys-offset+ (* 2 n-cells))))
         (end (+ +info-keys-offset+ n-cells)))
    (setf (info-storage-capacity a) n-cells
          (info-storage-threshold a) (ceiling (* *info-env-load-limit* n-cells))
          (info-storage-next a) #()) ; type-correct initial value
    (fill a +empty-key+ :start +info-keys-offset+ :end end)
    (fill a nil :start end)
    a))

(defstruct (info-hashtable (:conc-name info-env-))
  (storage (make-info-storage 30) :type simple-vector)
  (mutex (sb!thread:make-mutex))
  ;; COUNT is always at *least* as large as the key count.
  ;; If no insertions are in progress, it is exactly right.
  (count 0 #|:type sb!ext:word|#)) ; should be a raw slot, can't be :-(
(def!method print-object ((self info-hashtable) stream)
  (declare (stream stream))
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~D/~D entr~:@P" (info-env-count self)
            (info-storage-capacity (info-env-storage self)))))

;; FIXME: ATOMIC-INCF and ATOMIC-DECF would do just fine, but the structure
;; slot for INFO-ENV-COUNT can't be declared as an untagged. It occurs too soon
;; in the build order, prior to *RAW-SLOT-DATA-LIST* being set up.
;; Cold init drops into ldb when %COMPILER-DEFSTRUCT tries to define the
;; stereotyped out-of-line accessor functions. We don't have a lot of structures
;; in the cold core which use untagged slots, and those that do are after
;; 'target-defstruct'.  I have to either move this file later or move the
;; raw-slot data earlier.

(declaim (inline stupid-atomic-bump))
(defun stupid-atomic-bump-count (table delta) ; using CAS instead of ATOMIC-INCF
  (declare (optimize (safety 0)) (type (member +1 -1) delta))
  (let ((old (info-env-count table)))
    (loop (let* ((new (the fixnum (+ (the fixnum old) delta)))
                 (actual-old (cas (info-env-count table) old new)))
            (if (eq old actual-old)
                (return old)
                (setq old actual-old))))))

(declaim (inline make-info-forwarding-pointer
                 info-forwarding-pointer-target
                 info-value-moved-p
                 increment-count)
         (ftype (sfunction (t) simple-vector) info-env-rehash)
         (ftype (sfunction (t t) simple-vector) %wait-for-rehash))

;; Concurrent access relies on a forwarding pointer being placed into
;; transported value cells. Since this is not a fully general hashtable,
;; we could use fixnums as forwarding pointers, as they're otherwise
;; never present as a value.
#+nil
(progn
  (defun make-info-forwarding-pointer (index) index)
  (defun info-forwarding-pointer-target (pointer) pointer)
  (defun info-value-moved-p (val) (fixnump val)))

;; However, here is a forwarding-pointer representation that allows fixnums
;; as legal values in the table, so, since it's more general, ... why not?
(progn
  (defun make-info-forwarding-pointer (index)
    (declare (info-cell-index index) (optimize (safety 0)))
    (sb!kernel:%make-lisp-obj (+ (ash index 8) sb!vm:unbound-marker-widetag)))
  (defun info-forwarding-pointer-target (marker)
    (ash (sb!kernel:get-lisp-obj-address marker) -8))
  (defun info-value-moved-p (x)
    (eq (logand (sb!kernel:get-lisp-obj-address x) #xff)
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
(defmacro do-probe-sequence ((storage key &optional hash) &key probe hit miss)
  (with-unique-names (test miss-fn len key-index step)
    (once-only ((storage storage) (key key)
                (hashval (or hash `(globaldb-sxhashoid ,key))))
      `(macrolet ((key-index () ; expose key+value indices to invoking code
                    ',key-index)
                  (value-index ()
                    '(+ (info-storage-capacity ,storage) ,key-index))
                  (,test ()
                    `(let ((probed-key (svref ,',storage ,',key-index)))
                       ,',probe ; could keep a tally of the probes
                       ;; Optimistically test for hit first, then markers
                       (cond ((equal probed-key ,',key) (go :hit))
                             ((or (eql probed-key +unavailable-key+)
                                  (eql probed-key +empty-key+))
                              (,',miss-fn))))))
         (let* ((,len (info-storage-capacity ,storage))
                (,key-index (+ (rem ,hashval ,len) +info-keys-offset+))
                (,step 0))
           (declare (info-cell-index ,key-index ,step))
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
  (do-probe-sequence (storage key)
    :miss (return-from info-gethash nil)
    :hit (let ((index (value-index)))
           (loop (let ((value (svref storage index)))
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
  (assert (not (member key '(0 -1))))
  (labels ((update (array value-index current-val)
             ;; Loop invariant: CURRENT-VAL is not a forwarding pointer.
             (let ((oldval) (newval (funcall update-proc current-val)))
               ;; On any try, either the proc wants to quit without updating
               ;; or it wants to change the value atomically.
               (cond ((or (eq newval current-val) ; no change
                          (eq (setq oldval (cas (svref array value-index)
                                                current-val newval))
                              current-val)) ; successful swap
                      newval)
                     ((info-value-moved-p oldval) ; cell became forwarded
                      ;; Usually only a forced random delay can induce this.
                      (update-forwarded array oldval))
                     (t ; collision. Decide again what newval should be
                      (update array value-index oldval)))))
           (update-forwarded (array forwarding-marker)
             ;; Another forwarding can happen if rehash finished and table
             ;; overflowed yet again before we got here.
             (update-index (info-storage-next array)
                           (info-forwarding-pointer-target forwarding-marker)))
           (update-index (array value-index)
             (let ((current-val (svref array value-index)))
               (if (info-value-moved-p current-val)
                   (update-forwarded array current-val)
                   (update array value-index current-val)))))
    (named-let retry ((hashval (globaldb-sxhashoid key))
                      (storage (info-env-storage env)))
      (do-probe-sequence (storage key hashval)
       :hit (update-index storage (value-index))
       :miss
       (progn
        (let ((old-count (stupid-atomic-bump-count env 1)))
          (declare (info-cell-index old-count))
          (when (>= old-count (info-storage-threshold storage))
            (sb!thread:with-mutex ((info-env-mutex env))
              (when (eq (info-env-storage env) storage) ; any thread can do it
                (info-env-rehash env)))
            (stupid-atomic-bump-count env -1) ; prepare to retry
            (return-from retry (retry hashval (info-env-storage env)))))
        ;; Attempt to claim KEY-INDEX
        (let ((oldkey (cas (svref storage (key-index)) +empty-key+ key)))
          (when (eql oldkey +empty-key+) ; successful claim
            ;; Optimistically assume that nobody else rewrote the value
            (return-from retry (update storage (value-index) nil)))
          (stupid-atomic-bump-count env -1) ; failed
          ;; The fallthrough branch of this COND is ordinary CAS failure where
          ;; somebody else wanted this slot, and won. Looping tries again.
          (cond ((equal oldkey key) ; pure coincidence
                 (return-from retry (update-index storage (value-index))))
                ((eql oldkey +unavailable-key+) ; Highly unlikely
                 ;; as preemptive check up above ensured no rehash needed.
                 (return-from retry
                   (retry hashval (%wait-for-rehash env storage)))))))))))

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
                          (cas (svref old-storage old-key-index)
                               +empty-key+ +unavailable-key+)
                          key))
          unless (eql key +empty-key+)
          do (let* ((new-key-index
                      (block nil
                        (do-probe-sequence (new-storage key)
                          :hit (bug "Globaldb rehash failure. Mutated key?")
                          :miss (return (key-index)))))
                    (old-value-index (+ old-key-index old-capacity))
                    (new-value-index (+ new-key-index new-capacity)))
               (setf (svref new-storage new-key-index) key) ; Q: barrier needed?
               ;; Copy the current value into the new storage,
               ;; and CAS in a forwarding pointer. Repeat until successful.
               (loop (let ((value (svref old-storage old-value-index)))
                       ;; Force to memory before publishing relocated status
                       (sb!thread:barrier (:write)
                         (setf (svref new-storage new-value-index) value))
                       (when (eq (cas (svref old-storage old-value-index)
                                      value (make-info-forwarding-pointer
                                             new-value-index)) value)
                         (return))))))

    ;; No write barrier needed. Threads still looking at old storage
    ;; will eventually find all cells unavailable or forwarded.
    (setf (info-env-storage env) new-storage)))

;; This maphash is for debugging purposes, and not threadsafe.
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
    (info-puthash key env #'update)))

(defun show-info-env (env)
  (info-maphash (lambda (k v) (format t "~S -> ~S~%" k v)) env))

;;;; Info-Vectors
;;;; ============

;;; Info for a Name (an arbitrary object) is stored in an Info-Vector,
;;; which like is a 2-level association list. Info-Vectors are stored in symbols
;;; for most names, or in the global hashtable for "complicated" names.

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
;;;  ((nil  (1 . #<fdefn SB-MOP:STANDARD-INSTANCE-ACCESS>) (2 . :FUNCTION) ...)
;;;   (SETF (1 . #<fdefn (SETF SB-MOP:STANDARD-INSTANCE-ACCESS)>) ...)
;;;   (CAS  (1 . #<fdefn (CAS SB-MOP:STANDARD-INSTANCE-ACCESS)>) ...)
;;;   ...)
;;;
;;; Note:
;;; * The root name is exogenous to the vector - it is not stored.
;;; * The type-number for (:FUNCTION :DEFINITION) is 1, :KIND is 2, etc.
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

;;;; At run time, we represent the type of info that we want by a small
;;;; non-negative integer.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (def!constant type-number-bits 6))
(deftype type-number () `(unsigned-byte ,type-number-bits))

(defconstant info-type-mask (ldb (byte type-number-bits 0) -1)) ; #b111111

;; Using 6 bits per packed field, 5 infos can be described in a 30-bit fixnum,
;; or 10 in a fixnum on 64-bit machines (regardless of n-fixnum-tag-bits).
;; The eval-when seems to be necessary for building with CCL as host.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +infos-per-word+ (floor sb!vm:n-fixnum-bits type-number-bits)))

;; Descriptors are target fixnums
(deftype info-descriptor () `(signed-byte ,sb!vm:n-fixnum-bits))

;; Every Name amenable to storage in info-vectors has an auxilliary key
;; as explained above, except that the root name itself has none.
(defconstant +no-auxilliary-key+ 0)

;; An empty info-vector. Its 0th field describes that there are no more fields.
(defconstant-eqx +nil-packed-infos+ #(0) #'equalp)

;; FDEFINITIONs can be looked up from C runtime given an info-vector,
;; so the type-number must be wired. Using genesis machinery to define
;; the constant for C would be overkill, as it's the only special case.
(defconstant +fdefn-type-num+ 1)

;; Extract a field from a packed info descriptor.
;; A field is either a count of type-numbers, or a type-number.
(declaim (inline packed-info-field))
(defun packed-info-field (vector desc-index field-index)
  ;; Should not need (THE TYPE-NUMBER) however type inference
  ;; is borked during cross-compilation due to the shadowed LDB
  ;; (see "don't watch:" in cold/defun-load-or-cload-xcompiler)
  (the type-number
    (ldb (byte type-number-bits
               (* (the (mod #.+infos-per-word+) field-index) type-number-bits))
         (the info-descriptor (svref vector desc-index)))))

;; Compute the number of elements needed to hold unpacked VECTOR after packing.
;; This is not "compute-packed-info-size" since that could be misconstrued
;; and wanting the vector to be already packed.
;;
(defun compute-packified-info-size (vector &optional (end (length vector)))
  (declare (simple-vector vector)) ; unpacked format
  (let ((index 0) ; index into the unpacked input vector
        (n-fields 0)) ; will be the total number of packed fields
    (declare (index index end n-fields))
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
    (declare (index-or-minus-1 i j k end)
             (type (mod #.(1+ (* (1- +infos-per-word+) type-number-bits)))
                   field-shift)
             (info-descriptor word))
    ;; XXX: cross-modular isn't loaded early enough to get 'mask-signed-field'
    ;; which is important to keep descriptors as target fixnums.
    ;; Either (1) define mask-signed-field sooner, or (2) use the "host"
    ;; expression that always works but is ugly to me.
    ;; Mask-signed-field is only needed for 30-bit fixnums which use bit
    ;; index 29 as just another bit - the highest bit of the highest field.
    (flet ((put-field (val) ; insert VAL into the current packed descriptor
             (declare (type-number val))
             (setq word ; shift into position
                   (logior
                    word
                    (if (> sb!vm:n-fixnum-bits 30)
                        (ash val field-shift) ; never touches a sign-bit
                        #+sb-xc-host
                        (logior (if (and (logbitp 5 val) (= field-shift 24))
                                    sb!xc:most-negative-fixnum 0)
                                (ash val field-shift))
                        #-sb-xc-host
                        (mask-signed-field 30 (ash val field-shift)))))
             (if (< field-shift (* (1- +infos-per-word+) type-number-bits))
                 (incf field-shift type-number-bits)
                 (setf (svref output (incf j)) word field-shift 0 word 0))))
      ;; Truncating divide by 2: count = n-elements in the group @ 2 per entry,
      ;; +1 for count itself but not including its aux-key.
      (loop (let ((count (ash (the index (svref input (incf i))) -1)))
              (put-field count) ; how many infos to follow
              (dotimes (iter count)
                (put-field (svref input (incf i))) ; an info type-number
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
(defmacro with-packed-info-iterator ((generator input-var
                                                &key descriptor-index)
                                     &body body)
  (with-unique-names (input word count)
    `(let* ((,input (the simple-vector ,input-var))
            (,descriptor-index -1)
            (,count 0)
            (,word 0))
       (declare (info-descriptor ,word)
                (fixnum ,count)
                (type index-or-minus-1 ,descriptor-index))
       (flet ((,generator ()
                (when (zerop ,count)
                  (incf ,descriptor-index)
                  (setq ,word (svref ,input ,descriptor-index)
                        ,count +infos-per-word+))
                (prog1 (logand ,word info-type-mask)
                  (setq ,word (ash ,word (- type-number-bits)))
                  (decf ,count))))
         ,@body))))

;; Compute the number of elements needed to hold packed VECTOR after unpacking.
;; The unpacked size is the number of auxilliary keys plus the number of entries
;; @ 2 cells per entry, plus the number of length cells which indicate the
;; number of data cells used (including length cells but not aux key cells).
;; Equivalently, it's the number of packed fields times 2 minus 1.
;;
(defun compute-unpackified-info-size (vector)
  (declare (simple-vector vector))
  (let ((end (length vector))
        (descriptor-idx 0)
        (field-idx 0)
        (total-n-fields 0))
    (declare (index end descriptor-idx total-n-fields)
             (type (mod #.+infos-per-word+) field-idx))
    ;; Loop through the descriptors in random-access fashion.
    ;; Skip 1+ n-infos each time, because the 'n-infos' is itself a field
    ;; that is not accounted for in its own value.
    (loop (let ((n (1+ (packed-info-field vector descriptor-idx field-idx))))
            (incf total-n-fields n)
            (multiple-value-setq (descriptor-idx field-idx)
              (floor total-n-fields +infos-per-word+))
            (decf end n))
          ;; Done when the ascending index and descending index meet
          (unless (< descriptor-idx end)
            ;; off-by-one: the first info group's auxilliary key is imaginary
            (return (1- (truly-the fixnum (ash total-n-fields 1))))))))

;; Convert packed INPUT vector to unpacked.
;; If optional OUTPUT is supplied, it is used, otherwise output is allocated.
;; For efficiency the OUTPUT should be provided as a dynamic-extent array.
;;
(defun unpackify-infos (input &optional (output nil output-supplied-p))
  (declare (simple-vector input))
  (unless output-supplied-p
    (setq output (make-array (compute-unpackified-info-size input))))
  (let ((i (length input)) (j -1))  ; input index and output index respectively
    (declare (index-or-minus-1 i j))
    (with-packed-info-iterator (next-field input :descriptor-index desc-idx)
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
  (declare (index end))
  (if (eql key +no-auxilliary-key+)
      0 ; the first group's length (= end) is stored here always
      (let ((index 0))
        (declare (index index))
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
    (declare (index descriptor-idx data-idx)
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

;; Take a packed info-vector INPUT and insert (AUX-KEY,TYPE-NUMBER,VALUE).
;; Packed info-vectors are immutable. Any alteration must create a copy.
;; This is done by unpacking/repacking - it's easy enough and fairly
;; efficient since the temporary vector is stack-allocated.
;;
(defun packed-info-insert (input aux-key type-number value)
  (declare (simple-vector input) (type-number type-number))
  ;; Special case of inserting into an empty vector.
  ;; Actually I can do better than this - any insertion into a vector
  ;; with zero aux-keys and one descriptor that has room for at least
  ;; one more field could be done without too much trouble.
  ;; (It's when the descriptor is full that we have to think about it)
  (when (and (eql aux-key +no-auxilliary-key+)
             (eql (length input) (length +nil-packed-infos+)))
    (return-from packed-info-insert
      (vector (logior (ash type-number type-number-bits) 1) ; 1 piece of info
              value)))
  (let* ((n-extra-elts
          ;; Test if the aux-key has been seen before or needs to be added.
          (if (and (not (eql aux-key +no-auxilliary-key+))
                   (not (info-find-aux-key/packed input aux-key)))
              4   ; need space for [aux-key, length, type-num, value]
              2)) ; only space for [type-num, value]
         (old-size (compute-unpackified-info-size input))
         (new-size (+ old-size n-extra-elts))
         (new (make-array new-size)))
    (declare (index old-size new-size)
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
               (setf (svref new (+ point 2)) type-number
                     (svref new (+ point 3)) value)))
            (t
             (let ((data-start (info-find-aux-key/unpacked
                                aux-key new old-size)))
               ;; it had better be found - it was in the packed vector
               (aver data-start)
               ;; fdefn must be the first piece of info for any name.
               ;; This facilitates C runtime determination of SymbolFunction
               ;; without completely decoding the vector.
               (insert-at (+ data-start (if (eql type-number +fdefn-type-num+)
                                            1 (svref new data-start)))
                          type-number value)
               ;; add 2 cells, and verify that re-packing won't
               ;; overflow the 'count' for this info group.
               (aver (typep (ash (incf (svref new data-start) 2) -1)
                            'type-number))))))
    (packify-infos new)))

;; Search packed VECTOR for AUX-KEY and TYPE-NUMBER, returning
;; the index of the data if found, or NIL if not found.
;;
(declaim (ftype (function (simple-vector (or (eql 0) symbol) type-number)
                          (or null index))
                packed-info-value-index))

(defun packed-info-value-index (vector aux-key type-num)
  (declare (simple-vector vector) (type (or (eql 0) symbol) aux-key)
           (type-number type-num)
           (optimize (safety 0))) ; bounds are AVERed
  (let ((data-idx (length vector)) (descriptor-idx 0) (field-idx 0))
    (declare (index descriptor-idx)
             (type (mod #.+infos-per-word+) field-idx))
    (unless (eql aux-key +no-auxilliary-key+)
      (multiple-value-setq (data-idx descriptor-idx field-idx)
        (info-find-aux-key/packed vector aux-key))
      (unless data-idx
        (return-from packed-info-value-index nil)))
    ;; Fetch a descriptor and shift out trailing bits that won't be scanned.
    (let* ((descriptor (ash (the info-descriptor (aref vector descriptor-idx))
                            (* (- type-number-bits) field-idx)))
           (n-infos (logand descriptor info-type-mask))
           ;; Compute n things in this descriptor after extracting one field. e.g.
           ;; if starting index = 2, there's space for 7 more fields in 60 bits.
           (swath (min (- +infos-per-word+ field-idx 1) n-infos)))
      ;; Type inference on n-infos deems it to have no lower bound due to DECF.
      (declare (info-descriptor descriptor)
               (type (unsigned-byte #.type-number-bits) n-infos)
               (index data-idx))
      ;; Repeatedly shift and mask, which is quicker than extracting a field at
      ;; varying positions. Start by shifting out the n-infos field.
      (setq descriptor (ash descriptor (- type-number-bits)))
      (loop
         (dotimes (j swath)
           (when (eql type-num (logand descriptor info-type-mask))
             (return-from packed-info-value-index
               (the index (- data-idx j 1))))
           (setq descriptor (ash descriptor (- type-number-bits))))
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
(defun packed-info-remove (input key2 &rest type-nums)
  (declare (dynamic-extent type-nums)
           (simple-vector input))
  (when (or (eql (length input) (length +nil-packed-infos+))
            (and (not (eql key2 +no-auxilliary-key+))
                 (not (info-find-aux-key/packed input key2))))
    (return-from packed-info-remove nil)) ; do nothing
  (let* ((end (compute-unpackified-info-size input))
         (new (make-array end))
         (data-start 0))
    (declare (truly-dynamic-extent new) (index end data-start))
    (unpackify-infos input new)
    (let ((start (info-find-aux-key/unpacked key2 new end)))
      (aver start) ; must be found - it was in the packed vector
      (setq data-start start)) ; point to the group's length cell
    (dolist (type-num type-nums)
      (declare (type type-number type-num))
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

;; Call FUNCTION with each piece of symbol in packed VECT using ROOT-SYMBOL
;; as the primary name. FUNCTION must accept 3 values (NAME TYPE-NUMBER VALUE).
(defun %call-with-each-info (function vect root-symbol)
  (let ((name root-symbol)
        (data-idx (length vect)))
    (declare (index data-idx))
    (with-packed-info-iterator (next-field vect :descriptor-index desc-idx)
      (loop ; over name
         (dotimes (i (next-field)) ; number of infos for this name
           (funcall function name (next-field) (svref vect (decf data-idx))))
         (if (< desc-idx (decf data-idx))
             (setq name (list (svref vect data-idx) root-symbol))
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
;; The info for a symbol's fdefn must precede other type-numbers.
;; and SETF must be the first aux key if other aux keys are present.
;; The test function does not enforce these invariants.
;; N.B. As this function starts with "!", is is omitted from the target image.
(defun !test-packify-infos (&rest lists)
  (flet ((check (plist)
           (and (evenp (length plist))
                (loop for (indicator value) on plist by #'cddr
                      always (typep indicator 'type-number))))
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

;;; Helper macros for extracting parts of a Name in globaldb.
;;; Names that satisfy the type specifier (CONS SYMBOL (CONS SYMBOL NULL))
;;; are treated in a more efficient way than general names.

;; If NAME matches the pattern (<SYMBOL2> <SYMBOL1>), then bind KEY1 and KEY2
;; to those symbols and execute the body; otherwise skip the body.
;; It's preferable that a name of NIL already be ruled out by a prior
;; test for (SYMBOLP NAME) though it's not strictly necessary.
;;
;; It's conceivable that we extend this to allow KEY2 to be a list,
;; and have INFO-FIND-AUX-KEY/PACKED use EQUALP in that case.
;; Or we could do something ad-hoc for
;;  (PCL::SLOT-ACCESSOR :GLOBAL SB-PCL::some-name SB-PCL::READER)
;; without supporting lists of length N in a fully general way.
;; But for PCL methods it might make sense that fdefinition be stored
;; in PCL metaobjects, and have a hook whereby NAME-FDEFINITION understands
;; that certain kinds of names have a custom lookup technique.
;;
(defmacro with-compound-name ((key1 key2) name &body body)
  (with-unique-names (pair rest)
    `(let ((,pair ,name))
       (if (listp ,pair)
           (let ((,rest (cdr ,pair)))
             ;; Lists of length >= 2 are common, so optimistically assume that
             ;; (LISTP rest) implies (CONSP rest) but fail if length >= 3.
             (if (and (listp ,rest) (not (cdr ,rest)))
                 (let ((,key2 (car ,pair)) (,key1 (car ,rest)))
                   ;; Check for 2 symbols, and finally verify that list wasn't a
                   ;; singleton which accidentally matched.
                   (if (and (symbolp ,key1) (symbolp ,key2) ,rest)
                       (progn ,@body)))))))))

;; Do the same as WITH-COMPOUND-NAME but also accept a symbol by itself,
;; in which case KEY2 becomes +NO-AUXILLIARY-KEY+.
;;
(defmacro with-possibly-compound-name ((key1 key2) name &body body)
  (with-unique-names (rest)
    `(let ((,key1 ,name) (,key2 +NO-AUXILLIARY-KEY+))
       (when (or (symbolp ,key1)
                 (if (listp ,key1)
                     (let ((,rest (cdr ,key1)))
                       (when (and (listp ,rest) (not (cdr ,rest)))
                         (setq ,key2 (car ,key1)
                               ,key1 (car ,rest))
                         (and (symbolp ,key1) (symbolp ,key2) ,rest)))))
         ,@body))))

;; Perform the approximate equivalent operations of retrieving
;; (INFO :class :type <name>), but if no info is found, invoke CREATION-FORM
;; to produce an object that becomes the value for that piece of info,
;; returning it. The entire sequence behaves atomically with the following
;; proviso: the creation form's result may be discarded, and another object
;; returned instead (presumably) from another thread's execution
;; of that same creation form.
;;
;; If constructing the object has either non-trivial cost, or deleterious
;; side-effects from making and discarding its result, do NOT use this macro.
;; A mutex-guarded table would probably be more appropriate in such cases.
;;
;; INFO-CLASS and -TYPE must be keywords, and NAME must evaluate
;; to a symbol. [Eventually this will accept generalized names]
;;
;; FIXME: these does not really seem to belong in SB-C, but that's where
;; all the other info stuff is. Maybe SB-INT ?
;;
(defmacro atomically-get-or-put-symbol-info
    (info-class info-type name creation-form)
  (let ((type-num (type-info-number
                   (type-info-or-lose info-class info-type)))
        (aux-key +no-auxilliary-key+))
    (with-unique-names (proc info-vect index result)
      ;; Concurrent globaldb updates (possibly for unrelated info)
      ;; can force re-execution of this flet, so try to create an
      ;; object one time only, and remember that we did that.
      ;; If CREATION-FORM returns nil - which it shouldn't - the
      ;; form couldbe repeatedly invoked, because there's no
      ;; local state variable such as invoked-creation-form-p.
      `(let (,result)
         (dx-flet ((,proc (,info-vect)
                     ;; pre-check
                     (let ((,index (packed-info-value-index
                                    ,info-vect ,aux-key ,type-num)))
                       (cond (,index
                              (setq ,result (svref ,info-vect ,index))
                              nil) ; no update to symbol-info-vector
                             (t
                              (unless ,result
                                (setq ,result ,creation-form))
                              (packed-info-insert
                               ,info-vect ,aux-key ,type-num ,result))))))
           (update-symbol-info ,name #',proc)
           ,result)))))
