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

;;; HASH-TABLE is implemented as a STRUCTURE-OBJECT.
(sb-xc:defstruct (hash-table (:copier nil)
                             (:constructor %make-hash-table
                               (test
                                test-fun
                                hash-fun
                                rehash-size
                                rehash-threshold
                                rehash-trigger
                                table
                                index-vector
                                next-vector
                                hash-vector
                                flags)))
  ;; The type of hash table this is. Only used for printing and as
  ;; part of the exported interface.
  (test nil :type symbol :read-only t)
  ;; The function used to compare two keys. Returns T if they are the
  ;; same and NIL if not.
  (test-fun nil :type function :read-only t)
  ;; The function used to compute the hashing of a key. Returns two
  ;; values: the index hashing and T if that might change with the
  ;; next GC.
  (hash-fun nil :type function :read-only t)
  ;; How much to grow the hash table by when it fills up. If an index,
  ;; then add that amount. If a floating point number, then multiply
  ;; it by that.
  (rehash-size nil :type (or index (single-float ($1.0)))
               :read-only t)
  ;; How full the hash table has to get before we rehash
  (rehash-threshold nil :type (single-float ($0.0) $1.0) :read-only t)
  ;; The number of entries before a rehash, just one less than the
  ;; size of the next-vector, hash-vector, and half the size of the
  ;; kv-vector.
  (rehash-trigger nil :type index)
  ;; The current number of entries in the table.
  (number-entries 0 :type index)
  ;; The Key-Value pair vector.
  (table nil :type simple-vector)
  ;; This slot is used to link weak hash tables during GC. When the GC
  ;; isn't running it is always NIL.
  (next-weak-hash-table nil :type null)
  ;; flags: WEAKNESS-KIND | FINALIZERSP | SYNCHRONIZEDP | WEAKP
  ;; WEAKNESS-KIND is 2 bits, the rest are 1 bit each
  (flags 0 :type (unsigned-byte 5) :read-only t)
  ;; Index into the Next vector chaining together free slots in the KV
  ;; vector.
  (next-free-kv 0 :type index)
  ;; A cache that is either nil or is an index into the hash table
  ;; that should be checked first
  (cache nil :type (or null index))
  ;; The index vector. This may be larger than the hash size to help
  ;; reduce collisions.
  (index-vector nil :type (simple-array sb-vm:word (*)))
  ;; This table parallels the KV vector, and is used to chain together
  ;; the hash buckets and the free list. A slot will only ever be in
  ;; one of these lists.
  (next-vector nil :type (simple-array sb-vm:word (*)))
  ;; This table parallels the KV table, and can be used to store the
  ;; hash associated with the key, saving recalculation. Could be
  ;; useful for EQL, and EQUAL hash tables. This table is not needed
  ;; for EQ hash tables, and when present the value of
  ;; +MAGIC-HASH-VECTOR-VALUE+ represents EQ-based hashing on the
  ;; respective key.
  (hash-vector nil :type (or null (simple-array sb-vm:word (*))))
  ;; Used for locking GETHASH/(SETF GETHASH)/REMHASH
  (lock (sb-thread:make-mutex :name "hash-table lock")
        #-c-headers-only :type #-c-headers-only sb-thread:mutex
        :read-only t)
  ;; List of values culled out during GC of weak hash table.
  (culled-values nil :type list)
  ;; For detecting concurrent accesses.
  #+sb-hash-table-debug
  (signal-concurrent-access t :type (member nil t))
  #+sb-hash-table-debug
  (reading-thread nil)
  #+sb-hash-table-debug
  (writing-thread nil))

;; as explained by pmai on openprojects #lisp IRC 2002-07-30: #x80000000
;; is bigger than any possible nonEQ hash value, and thus indicates an
;; empty slot; and EQ hash tables don't use HASH-TABLE-HASH-VECTOR.
;; The previous sentence was written when SBCL was 32-bit only. The value
;; now depends on the word size. It is propagated to C in genesis because
;; the generational garbage collector needs to know it.
(defconstant +magic-hash-vector-value+ (ash 1 (1- sb-vm:n-word-bits)))

;;; Return an association list representing the same data as HASH-TABLE.
(defun %hash-table-alist (hash-table)
  (let ((result nil))
    (maphash (lambda (key value)
               (push (cons key value) result))
             hash-table)
    result))
