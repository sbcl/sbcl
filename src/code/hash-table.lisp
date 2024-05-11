;;;; the needed-on-the-cross-compilation-host part of HASH-TABLE
;;;; implementation

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;; Our table representation is as illustrated below.
;;; SIZE is always the exact number of K/V entries that can be stored,
;;; and can be any number, not necessarily a power of 2.

;;;            __________________________________________
;;;  K/V       |                                    |   |
;;;  vector    | * | * |  K | V | K | V | ......... | * |
;;;            +________________________________________+
;;;                    | <---       SIZE         -->|
;;;
;;;                      ^--- pair index 1 and so on
;;;

;;; The length of PAIRS (the K/V vector) is the specified :SIZE * 2
;;; plus 3 elements of overhead, 2 at the beginning and one at the end.
;;; (It's slighly strange that extra cells are in two different places,
;;; however there's a reason: we need an indicator for the end of a chain,
;;; and/or unused bin, and we use 0 for that, which means that k/v pair 0
;;; is unusable. But we can't keep indiscriminately adding overhead cells
;;; to the front because that make even more k/v pairs unusable,
;;; whereas adding at the end doesn't cause any such problem)
;;; Pair index 1 is the first pair that stores user data.

;;; The length of the HASH-VECTOR is in direct correspondence with the
;;; physical k/v cells, so that we can store a hash per key and not worry
;;; about one wasted cell. (i.e. the 0th k/v cell can't be used, so neither
;;; can the 0th hash value. To avoid this 1 cell of waste would mean adding
;;; and subtracting 1 here and there, needlessly complicating things)

;;; The INDEX vector is the traditional power-of-2 sized vector mapping a hash
;;; value to a pair. These are what you might calls the "bins" or "buckets" of
;;; the table. The value in a bin is the pair index of the start of the chain
;;; belonging to the bin. The value is 1..SIZE or 0 for an empty bin, which works
;;; well because pair index 0 isn't usable. The NEXT vector is the pointer to
;;; follow from a pair index to the next pair index in the same chain. As with
;;; the hash vector, the NEXT vector is sized at 1 greater than minimally
;;; necessary, to avoid adding and subtracting 1 from a pair index.

;;; The PAIRS vector has an odd length with the following overhead elements:
;;;
;;; [0] = high-water-mark
;;; [1] = rehash-due-to-GC indicator
;;; ...
;;; [length-1] = auxiliary info depending on kind of table
;;;   See KV-VECTOR-AUX-INFO in 'target-hash-table'

;;; HASH-TABLE is implemented as a STRUCTURE-OBJECT.
(sb-xc:deftype hash-table-index () '(unsigned-byte 32))
(sb-xc:defstruct (hash-table (:copier nil)
                             (:constructor %alloc-hash-table
                               (flags
                                gethash-impl
                                puthash-impl
                                remhash-impl
                                clrhash-impl
                                test
                                test-fun
                                hash-fun
                                rehash-size
                                rehash-threshold
                                pairs
                                index-vector
                                next-vector
                                hash-vector)))

  (gethash-impl #'error :type (sfunction * (values t boolean)) :read-only t)
  (puthash-impl #'error :type (sfunction * t) :read-only t)
  (remhash-impl #'error :type (sfunction * t) :read-only t)
  (clrhash-impl #'error :type (sfunction * t) :read-only t)
  ;; The Key-Value pair vector.
  ;; Note: this vector has a "high water mark" which resembles a fill
  ;; pointer, but unlike a fill pointer, GC can ignore elements
  ;; above the high water mark.  If you store non-immediate data past
  ;; that mark, you're sure to have problems.
  (pairs nil :type simple-vector)
  ;; MRU physical index of a key in the k/v vector. If < (LENGTH PAIRS)
  ;; the cell can be examined first in GETHASH and PUTHASH. The "unknown" value
  ;; is not 0 because that would look valid but could accidentally return a
  ;; false match if the user's key is EQ to element 0 in the pair vector.
  (cache (- array-dimension-limit 2) :type index)
  ;; The index vector. This may be larger than the capacity to help
  ;; reduce collisions.
  (index-vector nil :type (simple-array hash-table-index (*)))
  ;; This table parallels the KV vector, and is used to chain together
  ;; the hash buckets and the free list. A slot will only ever be in
  ;; one of these lists.
  ;; (I think that free k/v slots could be linked through the KV vector
  ;; and not the next vector which affords some minor improvements)
  (next-vector nil :type (simple-array hash-table-index (*)))
  ;; This table parallels the KV table, and can be used to store the
  ;; hash associated with the key, saving recalculation. Could be
  ;; useful for EQL, and EQUAL hash tables. This table is not needed
  ;; for EQ hash tables, and when present the value of
  ;; +MAGIC-HASH-VECTOR-VALUE+ represents address-based hashing on the
  ;; respective key.
  (hash-vector nil :type (or null (simple-array hash-table-index (*))))
  ;; flags: WEAKNESS | KIND | WEAKP | {notused} | USERFUNP | SYNCHRONIZED
  ;; WEAKNESS is 2 bits, KIND is 2 bits, the rest are 1 bit each
  ;;   - WEAKNESS     : {K-and-V, K, V, K-or-V}, irrelevant unless WEAKP
  ;;   - KIND         : {EQ, EQL, EQUAL, EQUALP}, irrelevant if USERFUNP
  ;;   - WEAKP        : table is weak
  ;;   - USERFUNP     : table has a nonstandard hash function
  ;;   - SYNCHRONIZED : all operations are automatically guarded by a mutex
  ;; If you change these, be sure to check the definitions of hash_table_weakp()
  ;; and other autogenerated C code (see WRITE-HASH-TABLE-FLAG-EXTRACTORS)
  (flags 0 :type sb-vm:word :read-only t)
  ;; Used for locking GETHASH/(SETF GETHASH)/REMHASH
  ;; The lock is always created for synchronized tables, or created just-in-time
  ;; with nonsynchronized tables that are guarded by WITH-LOCKED-HASH-TABLE
  ;; or an equivalent "system" variant of the locking macro.
  (%lock nil #-c-headers-only :type #-c-headers-only (or null sb-thread:mutex))

  ;; The 4 standard tests functions don't need these next 2 slots:
  ;; (TODO: possibly don't have them in all hash-tables)
  ;; The function used to compare two keys. Returns T if they are the
  ;; same and NIL if not.
  (test-fun nil :type function :read-only t)
  ;; The function used to compute the hashing of a key. Returns two
  ;; values: the index hashing and T if that might change with the
  ;; next GC.
  (hash-fun nil :type function :read-only t)

  ;; The type of hash table this is. Part of the exported interface,
  ;; as well as needed for the MAKE-LOAD-FORM and PRINT-OBJECT methods.
  (test nil :type (or symbol function) :read-only t)
  ;; How much to grow the hash table by when it fills up. If an index,
  ;; then add that amount. If a floating point number, then multiply
  ;; it by that.
  (rehash-size nil :type (or index (single-float (1.0)))
               :read-only t)
  ;; How full the hash table has to get before we rehash
  ;; but only for the initial determination of how many buckets to make.
  ;; Subsequent resizing is at our discretion. i.e. you might think that a
  ;; deliberate choice of rehash size and threshold implies that you want the new
  ;; table to be X amount larger *and* that you care at about what load factor the
  ;; new table gets rehashed, but no, you don't get to pick both every time.
  ;; (CLHS says that these are all just "hints" and we're free to ignore)
  (rehash-threshold nil :type (single-float (0.0) 1.0) :read-only t)
  ;; The current number of entries in the table.
  (%count 0 :type index)
  ;; Index into the Next vector chaining together free slots in the KV
  ;; vector.
  ;; This index is allowed to exceed the high-water-mark by 1 unless
  ;; the HWM is at its maximum in which case this must be 0.
  (next-free-kv 1 :type index)

  ;; Statistics gathering for new gethash algorithm that doesn't
  ;; disable GC during rehash as a consequence of key movement.
  #+hash-table-metrics (n-rehash+find 0 :type word)
  #+hash-table-metrics (n-lsearch     0 :type word)
  ;; this counter is incremented if we observe that GC marked the table invalid
  ;; while already in the midst of being rehashed due to invalidation.
  #+hash-table-metrics (n-rehash-again 0 :type word)
  ;; this counter is incremented if the fast-read-lock (implicit in the
  ;; 'stamp' field) implies that there was an inconsistent view of the table
  #+hash-table-metrics (n-stamp-change 0 :type word)
  )

(sb-xc:defstruct (general-hash-table (:copier nil)
                             (:conc-name hash-table-)
                             (:include hash-table)
                             (:constructor %alloc-general-hash-table
                               (flags
                                gethash-impl
                                puthash-impl
                                remhash-impl
                                clrhash-impl
                                test
                                test-fun
                                hash-fun
                                rehash-size
                                rehash-threshold
                                pairs
                                index-vector
                                next-vector
                                hash-vector)))
  ;; List of (pair-index . bucket-number) which GC smashed and are almost
  ;; equivalent to free cells, except that they are not yet unlinked from
  ;; their chain. Skipping the removal in GC eliminates a race with REMHASH.
  ;; Pushing onto the free list wouldn't actually be difficult,
  ;; but removing from the bucket is impossible without implementing
  ;; lock-free linked lists compatibly between C and Lisp.
  (smashed-cells nil)
  ;; This slot is used to link weak hash tables during GC. When the GC
  ;; isn't running it is always NIL.
  (next-weak-hash-table nil :type null))

(defconstant hash-table-weak-flag         8)
;;; USERFUN-FLAG implies a nonstandard hash function. Such tables may also have
;;; a custom comparator. But you can't have a custom comparator without a custom
;;; hash, because there's no way in general to produce a compatible hash.
(defconstant hash-table-userfun-flag      2)
(defconstant hash-table-synchronized-flag 1)

;;; Keep in sync with weak_ht_alivep_funs[] in gc-common
(defconstant +ht-weak-key-AND-value+ 0)
(defconstant +ht-weak-key+           1)
(defconstant +ht-weak-value+         2)
(defconstant +ht-weak-key-OR-value+  3)

(sb-xc:defmacro hash-table-lock (table)
  `(let ((ht ,table)) (or (hash-table-%lock ht) (install-hash-table-lock ht))))

(defmacro pack-ht-flags-weakness (x) `(logior (ash ,x 6) hash-table-weak-flag))
(defmacro ht-flags-weakness (flags) `(ldb (byte 2 6) ,flags))
;;; KIND corresponds directly to the HASH-TABLE-TEST for the 4 standard tests,
;;; but is not meaningful with a user-provided test or hash function.
(sb-xc:defmacro pack-ht-flags-kind (x) `(ash ,x 4))
(defmacro ht-flags-kind (flags) `(ldb (byte 2 4) ,flags))

(defconstant default-rehash-size 1.5)
;; Don't raise this number to 8 - if you do it'll increase the memory
;; consumption of a default MAKE-HASH-TABLE call by 7% just due to
;; padding slots.  This is a "perfect" minimal size.
(defconstant +min-hash-table-size+ 7)

(sb-xc:defmacro make-system-hash-table (&key test synchronized weakness)
  (multiple-value-bind (kind args)
      (cond ((equal test '(quote eq))  (values 0 '('eq  #'eq  #'eq-hash)))
            ((equal test '(quote eql)) (values 1 '('eql #'eql #'eql-hash)))
            (t
             (bug "Incomplete implementation of MAKE-SYSTEM-HASH-TABLE")
             0))
    `(%make-hash-table
      (logior ,(ecase weakness
                (:key   (pack-ht-flags-weakness +ht-weak-key+))
                (:value (pack-ht-flags-weakness +ht-weak-value+)))
               (pack-ht-flags-kind ,kind)
               ,(if synchronized 'hash-table-synchronized-flag 0))
      ,@args
      ;; Splicing these constant values at expansion time avoids a problem invoking
      ;; MAKE-SYSTEM-HASH-TABLE before the constants are known to make-host-2, as happens
      ;; when compiling type-class. hash-table.lisp can't be moved earlier in build-order
      ;; (without pain) so the expanded code can't use the values.
      ,+min-hash-table-size+
      ,default-rehash-size
      1.0)))

;; Our hash-tables store precomputed hashes to speed rehash and to guard
;; the call of the general comparator.
;; i.e. we take the value from mumble-hash {SXHASH, EQ-HASH, etc}
;; and fuzz the bits some more, then clip to 31 bits and store that.
;; (As a practical matter, this limits tables to 2^31 bins.)
;; Address-sensitive keys can't store a precomputed hash. They instead
;; store this value that indicates address-sensitivity.
(defconstant +magic-hash-vector-value+ #xFFFFFFFF)
