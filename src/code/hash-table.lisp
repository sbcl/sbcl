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

(in-package "SB!IMPL")

;;; HASH-TABLE is implemented as a STRUCTURE-OBJECT.
(sb!xc:defstruct (hash-table (:constructor %make-hash-table))
  ;; The type of hash table this is. Only used for printing and as
  ;; part of the exported interface.
  (test (missing-arg) :type symbol :read-only t)
  ;; The function used to compare two keys. Returns T if they are the
  ;; same and NIL if not.
  (test-fun (missing-arg) :type function :read-only t)
  ;; The function used to compute the hashing of a key. Returns two
  ;; values: the index hashing and T if that might change with the
  ;; next GC.
  (hash-fun (missing-arg) :type function :read-only t)
  ;; How much to grow the hash table by when it fills up. If an index,
  ;; then add that amount. If a floating point number, then multiply
  ;; it by that.
  (rehash-size (missing-arg) :type (or index (single-float (1.0)))
               :read-only t)
  ;; How full the hash table has to get before we rehash
  (rehash-threshold (missing-arg) :type (single-float (0.0) 1.0) :read-only t)
  ;; The number of entries before a rehash, just one less than the
  ;; size of the next-vector, hash-vector, and half the size of the
  ;; kv-vector.
  (rehash-trigger (missing-arg) :type index)
  ;; The current number of entries in the table.
  (number-entries 0 :type index)
  ;; The Key-Value pair vector.
  (table (missing-arg) :type simple-vector)
  ;; This slot is used to link weak hash tables during GC. When the GC
  ;; isn't running it is always NIL.
  (next-weak-hash-table nil :type null)
  ;; Non-NIL if this is some kind of weak hash table. For details see
  ;; the docstring of MAKE-HASH-TABLE.
  (weakness nil :type (member nil :key :value :key-or-value :key-and-value)
            :read-only t)
  ;; Index into the Next vector chaining together free slots in the KV
  ;; vector.
  (next-free-kv 0 :type index)
  ;; A cache that is either nil or is an index into the hash table
  ;; that should be checked first
  (cache nil :type (or null index))
  ;; The index vector. This may be larger than the hash size to help
  ;; reduce collisions.
  (index-vector (missing-arg) :type (simple-array sb!vm:word (*)))
  ;; This table parallels the KV vector, and is used to chain together
  ;; the hash buckets and the free list. A slot will only ever be in
  ;; one of these lists.
  (next-vector (missing-arg) :type (simple-array sb!vm:word (*)))
  ;; This table parallels the KV table, and can be used to store the
  ;; hash associated with the key, saving recalculation. Could be
  ;; useful for EQL, and EQUAL hash tables. This table is not needed
  ;; for EQ hash tables, and when present the value of
  ;; +MAGIC-HASH-VECTOR-VALUE+ represents EQ-based hashing on the
  ;; respective key.
  (hash-vector nil :type (or null (simple-array sb!vm:word (*))))
  ;; Used for locking GETHASH/(SETF GETHASH)/REMHASH
  (spinlock (sb!thread::make-spinlock :name "hash-table lock")
            :type sb!thread::spinlock :read-only t)
  ;; The GC will set this to T if it moves an EQ-based key. This used
  ;; to be signaled by a bit in the header of the kv vector, but that
  ;; implementation caused some concurrency issues when we stopped
  ;; inhibiting GC during hash-table lookup.
  (needs-rehash-p nil :type (member nil t))
  ;; Has user requested synchronization?
  (synchronized-p nil :type (member nil t) :read-only t)
  ;; For detecting concurrent accesses.
  #!+sb-hash-table-debug
  (signal-concurrent-access t :type (member nil t))
  #!+sb-hash-table-debug
  (reading-thread nil)
  #!+sb-hash-table-debug
  (writing-thread nil))

;; as explained by pmai on openprojects #lisp IRC 2002-07-30: #x80000000
;; is bigger than any possible nonEQ hash value, and thus indicates an
;; empty slot; and EQ hash tables don't use HASH-TABLE-HASH-VECTOR.
;; The previous sentence was written when SBCL was 32-bit only. The value
;; now depends on the word size. It is propagated to C in genesis because
;; the generational garbage collector needs to know it.
(defconstant +magic-hash-vector-value+ (ash 1 (1- sb!vm:n-word-bits)))

(defmacro-mundanely with-hash-table-iterator ((name hash-table) &body body)
  #!+sb-doc
  "WITH-HASH-TABLE-ITERATOR ((name hash-table) &body body)

Provides a method of manually looping over the elements of a hash-table. NAME
is bound to a generator-macro that, within the scope of the invocation,
returns one or three values. The first value tells whether any objects remain
in the hash table. When the first value is non-NIL, the second and third
values are the key and the value of the next object.

Consequences are undefined if HASH-TABLE is mutated during execution of BODY,
except for changing or removing elements corresponding to the current key. The
applies to all threads, not just the curren one -- even for synchronized
hash-tables. If the table may be mutated by another thread during iteration,
use eg. SB-EXT:WITH-LOCKED-HASH-TABLE to protect the WITH-HASH-TABLE-ITERATOR
for."
  ;; This essentially duplicates MAPHASH, so any changes here should
  ;; be reflected there as well.
  (let ((function (make-symbol (concatenate 'string (symbol-name name) "-FUN"))))
    `(let ((,function
            (let* ((table ,hash-table)
                   (length (length (hash-table-next-vector table)))
                   (index 1))
              (declare (type index/2 index))
              (labels
                  ((,name ()
                     ;; (We grab the table again on each iteration just in
                     ;; case it was rehashed by a PUTHASH.)
                     (let ((kv-vector (hash-table-table table)))
                       (do ()
                           ((>= index length) (values nil))
                         (let ((key (aref kv-vector (* 2 index)))
                               (value (aref kv-vector (1+ (* 2 index)))))
                           (incf index)
                           (unless (or (eq key +empty-ht-slot+)
                                       (eq value +empty-ht-slot+))
                             (return (values t key value))))))))
                #',name))))
       (macrolet ((,name () '(funcall ,function)))
         ,@body))))

(defmacro-mundanely with-locked-hash-table ((hash-table) &body body)
  #!+sb-doc
  "Limits concurrent accesses to HASH-TABLE for the duration of BODY.
If HASH-TABLE is synchronized, BODY will execute with exclusive
ownership of the table. If HASH-TABLE is not synchronized, BODY will
execute with other WITH-LOCKED-HASH-TABLE bodies excluded -- exclusion
of hash-table accesses not surrounded by WITH-LOCKED-HASH-TABLE is
unspecified."
  ;; Needless to say, this also excludes some internal bits, but
  ;; getting there is too much detail when "unspecified" says what
  ;; is important -- unpredictable, but harmless.
  `(sb!thread::with-recursive-system-spinlock
       ((hash-table-spinlock ,hash-table))
     ,@body))
