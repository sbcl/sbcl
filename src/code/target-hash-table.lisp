;;;; that part of the implementation of HASH-TABLE which lives solely
;;;; on the target system, not on the cross-compilation host

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;;; utilities

;;;; TODOs:
;;;;  - change recursive locks to nonrecursive.
;;;;    This will, I fear, be impossible because we've exposed and documented
;;;;    an API that pretty much tells users that it's ok to create a synchronized
;;;;    table while *also* wrapping any random operation in the table lock.
;;;;    Hence recursion on the lock. (commit b9a1b17b079d315c)
;;;;  - place the 3 or 4 vectors in a separate structure that can be atomically
;;;;    swapped out for a new instance with new vectors. Remove array bounds
;;;;    checking since all the arrays will be tied together.

(defconstant hash-table-weak-flag         8)
(defconstant hash-table-finalizer-flag    4)
(defconstant hash-table-userfun-flag      2)
(defconstant hash-table-synchronized-flag 1)

;;; T if and only if table has non-null weakness kind.
(declaim (inline hash-table-weak-p))
(defun hash-table-weak-p (ht)
  (logtest (hash-table-flags ht) hash-table-weak-flag))

;;; Value of :synchronized constructor argument.
(declaim (inline hash-table-synchronized-p))
(defun hash-table-synchronized-p (ht)
  (logtest (hash-table-flags ht) hash-table-synchronized-flag))

;;; Keep in sync with weak_ht_alivep_funs[] in gc-common
(declaim (inline decode-hash-table-weakness))
(defun decode-hash-table-weakness (x)
  ;; The bits of 'weakness' are interpreted as follows:
  ;;   bit 0 : live key forces value to be live
  ;;   bit 1 : live value forces key to be live
  ;;   both  : either forces the other to be live
  ;; :KEY-AND-VALUE has two zero bits, as neither object livens the other
  (aref #(:key-and-value :key :value :key-or-value) x))

;;; Non-NIL if this is some kind of weak hash table. For details see
;;; the docstring of MAKE-HASH-TABLE.
(defun hash-table-weakness (ht)
  "Return the WEAKNESS of HASH-TABLE which is one of NIL, :KEY,
:VALUE, :KEY-AND-VALUE, :KEY-OR-VALUE."
  (and (hash-table-weak-p ht)
       (decode-hash-table-weakness (ash (hash-table-flags ht) -4))))

;;; Code for detecting concurrent accesses to the same table from
;;; multiple threads. Only compiled in when the :SB-HASH-TABLE-DEBUG
;;; feature is enabled. The main reason for the existence of this code
;;; is to detect thread-unsafe uses of hash-tables in sbcl itself,
;;; where debugging anythign can be impossible after an important
;;; internal hash-table has been corrupted. It's plausible that this
;;; could be useful for some user code too, but the runtime cost is
;;; really too high to enable it by default.
;;; TODO: This macro needs to be repaired or removed.
(defmacro with-concurrent-access-check (hash-table operation &body body)
  (declare (ignorable hash-table operation)
           (type (member :read :write) operation))
  #-sb-hash-table-debug
  `(progn ,@body)
  #+sb-hash-table-debug
  (let ((thread-slot-accessor (if (eq operation :read)
                                  'hash-table-reading-thread
                                  'hash-table-writing-thread)))
    (once-only ((hash-table hash-table))
      `(progn
         (flet ((body-fun ()
                  ,@body)
                (error-fun ()
                  ;; Don't signal more errors for this table.
                  (setf (hash-table-signal-concurrent-access ,hash-table) nil)
                  (cerror "Ignore the concurrent access"
                          "Concurrent access to ~A" ,hash-table)))
           (declare (inline body-fun))
           (if (hash-table-signal-concurrent-access ,hash-table)
               (unwind-protect
                    (progn
                      (unless (and (null (hash-table-writing-thread
                                          ,hash-table))
                                   ,@(when (eq operation :write)
                                           `((null (hash-table-reading-thread
                                                    ,hash-table)))))
                        (error-fun))
                      (setf (,thread-slot-accessor ,hash-table)
                            sb-thread::*current-thread*)
                      (body-fun))
                 (unless (and ,@(when (eq operation :read)
                                  `((null (hash-table-writing-thread
                                           ,hash-table))))
                              ,@(when (eq operation :write)
                                  ;; no readers are allowed while writing
                                  `((null (hash-table-reading-thread
                                           ,hash-table))
                                    (eq (hash-table-writing-thread
                                         ,hash-table)
                                        sb-thread::*current-thread*))))
                   (error-fun))
                 (when (eq (,thread-slot-accessor ,hash-table)
                           sb-thread::*current-thread*)
                   ;; this is not 100% correct here and may hide
                   ;; concurrent access in rare circumstances.
                   (setf (,thread-slot-accessor ,hash-table) nil)))
               (body-fun)))))))

(declaim (inline sb-vm:is-lisp-pointer))
(defun sb-vm:is-lisp-pointer (addr) ; Same as is_lisp_pointer() in C
  #-64-bit (oddp addr)
  #+64-bit (not (logtest (logxor addr 3) 3)))

#-sb-fluid (declaim (inline eq-hash))
(defun eq-hash (key)
  (declare (values hash (member t nil)))
  ;; I think it would be ok to pick off SYMBOL here and use its hash slot
  ;; as far as semantics are concerned, but EQ-hash is supposed to be
  ;; the lightest-weight in terms of speed, so I'm letting everything use
  ;; address-based hashing, unlike the other standard hash-table hash functions
  ;; which try use the hash slot of certain objects.
  (values (pointer-hash key)
          (sb-vm:is-lisp-pointer (get-lisp-obj-address key))))

#-sb-fluid (declaim (inline eql-hash))
(defun eql-hash (key)
  (declare (values hash (member t nil)))
  (if (%other-pointer-subtype-p
       key
       ;; SYMBOL is listed here so that we can hash symbols address-insensitively.
       ;; Given that we're already picking off a bunch of OTHER-POINTER objects
       ;; and already calling SXHASH, the overhead is minimal. In fact, with suitably
       ;; and rearranged widetags, this would be included in the numeric range.
       '#.(list sb-vm:bignum-widetag sb-vm:ratio-widetag sb-vm:double-float-widetag
                sb-vm:single-float-widetag
                sb-vm:complex-widetag sb-vm:complex-single-float-widetag sb-vm:complex-double-float-widetag
                sb-vm:symbol-widetag))
      (values (if (= (%other-pointer-widetag key) sb-vm:symbol-widetag)
                  (sxhash (truly-the symbol key))
                  (number-sxhash key))
              nil)
      ;; I don't want to add a case for INSTANCE-WITH-HASH-P here,
      ;; but in the EQUAL and EQUAL hash functions, we do that.
      (eq-hash key)))

;;; TODO: add another bit that says that layout pertains to an object
;;; which is STANDARD-OBJECT and not just PCL-OBJECT, in case a user of the MOP
;;; decides to implement PCL objects that do not have a hash-code in the
;;; standard location (either the primitive object or the high header bits
;;; of the slot vector depending on the backend)
(declaim (inline instance-with-hash-p))
(defun instance-with-hash-p (x)
  (and (%instancep x)
       (logtest (layout-flags (%instance-layout x)) +pcl-object-layout-flag+)))

#-sb-fluid (declaim (inline equal-hash))
(defun equal-hash (key)
  (declare (values hash (member t nil)))
  (typecase key
    ;; For some types the definition of EQUAL implies a special hash
    ((or string cons number bit-vector pathname)
     (values (sxhash key) nil))
    ;; Certain objects have space in them wherein we can memoized a hash.
    ;; For those objects, use that hash
    ;; And wow, typecase isn't enough to get use to use the transform
    ;; for (sxhash symbol) without an explicit THE form.
    (symbol (values (sxhash (the symbol key)) nil)) ; transformed
    ((satisfies instance-with-hash-p) (values (std-instance-hash key) nil))
    ;; Otherwise use an EQ hash, rather than SXHASH, since the values
    ;; of SXHASH will be extremely badly distributed due to the
    ;; requirements of the spec fitting badly with our implementation
    ;; strategy.
    (t
     (eq-hash key))))

(defun equalp-hash (key)
  (declare (values hash (member t nil)))
  (typecase key
    ;; Types requiring special treatment. Note that PATHNAME and
    ;; HASH-TABLE are caught by the STRUCTURE-OBJECT test.
    ((or array cons number character structure-object)
     (values (psxhash key) nil))
    ;; As with EQUAL-HASH, use memoized hashes when applicable.
    (symbol (values (sxhash (the symbol key)) nil)) ; transformed
    ((satisfies instance-with-hash-p) (values (std-instance-hash key) nil))
    (t
     (eq-hash key))))

(declaim (inline prefuzz-hash))
(defun prefuzz-hash (hash)
  ;; We're using power of two tables which obviously are very
  ;; sensitive to the exact values of the low bits in the hash
  ;; value. Do a little shuffling of the value to mix the high bits in
  ;; there too. On 32-bit machines, the result is is a positive fixnum,
  ;; but on 64-bit, we use 2 more bits though must avoid conflict with
  ;; the unique value that that denotes an address-based hash.
  (ldb (byte #-64-bit 29 #+64-bit 31 0)
       (+ (logxor #b11100101010001011010100111 hash)
          (ash hash -3)
          (ash hash -12)
          (ash hash -20))))
(declaim (inline mask-hash))
(defun mask-hash (hash mask)
  (truly-the index (logand mask hash)))
(declaim (inline pointer-hash->bucket))
(defun pointer-hash->bucket (hash mask)
  (declare (type hash hash mask))
  (truly-the index (logand mask (prefuzz-hash hash))))

;;;; user-defined hash table tests

(defun register-hash-table-test (name hash-fun)
  (declare (symbol name) (function hash-fun))
  (unless (fboundp name)
    (error "Cannot register ~S has a hash table test: undefined function."
           name))
  (with-single-package-locked-error
      (:symbol name "defining ~S as a hash table test")
    (let* ((test-fun (fdefinition name))
           (this (list name test-fun hash-fun))
           (spec (assoc name *user-hash-table-tests*)))
      (cond (spec
             (unless (and (eq (second spec) test-fun)
                          (eq (third spec) hash-fun))
               (style-warn "Redefining hash table test ~S." name)
               (setf (cdr spec) (cdr this))))
            (t
             (push this *user-hash-table-tests*)))))
  name)

(defmacro define-hash-table-test (name hash-function)
  "Defines NAME as a new kind of hash table test for use with the :TEST
argument to MAKE-HASH-TABLE, and associates a default HASH-FUNCTION with it.

NAME must be a symbol naming a global two argument equivalence predicate.
Afterwards both 'NAME and #'NAME can be used with :TEST argument. In both
cases HASH-TABLE-TEST will return the symbol NAME.

HASH-FUNCTION must be a symbol naming a global hash function consistent with
the predicate, or be a LAMBDA form implementing one in the current lexical
environment. The hash function must compute the same hash code for any two
objects for which NAME returns true, and subsequent calls with already hashed
objects must always return the same hash code.

Note: The :HASH-FUNCTION keyword argument to MAKE-HASH-TABLE can be used to
override the specified default hash-function.

Attempting to define NAME in a locked package as hash-table test causes a
package lock violation.

Examples:

  ;;; 1.

  ;; We want to use objects of type FOO as keys (by their
  ;; names.) EQUALP would work, but would make the names
  ;; case-insensitive -- which we don't want.
  (defstruct foo (name nil :type (or null string)))

  ;; Define an equivalence test function and a hash function.
  (defun foo-name= (f1 f2) (equal (foo-name f1) (foo-name f2)))
  (defun sxhash-foo-name (f) (sxhash (foo-name f)))

  (define-hash-table-test foo-name= sxhash-foo-name)

  ;; #'foo-name would work too.
  (defun make-foo-table () (make-hash-table :test 'foo-name=))

  ;;; 2.

  (defun == (x y) (= x y))

  (define-hash-table-test ==
    (lambda (x)
      ;; Hash codes must be consistent with test, so
      ;; not (SXHASH X), since
      ;;   (= 1 1.0)                   => T
      ;;   (= (SXHASH 1) (SXHASH 1.0)) => NIL
      ;; Note: this doesn't deal with complex numbers or
      ;; bignums too large to represent as double floats.
      (sxhash (coerce x 'double-float))))

  ;; #'== would work too
  (defun make-number-table () (make-hash-table :test '==))
"
  (check-type name symbol)
  (if (member name '(eq eql equal equalp))
      (error "Cannot redefine standard hash table test ~S." name)
      (cond ((symbolp hash-function)
             `(register-hash-table-test ',name (symbol-function ',hash-function)))
            ((and (consp hash-function) (eq 'lambda (car hash-function)))
             `(register-hash-table-test ',name #',hash-function))
            (t
             (error "Malformed HASH-FUNCTION: ~S" hash-function)))))

;;;; construction and simple accessors

;;; The smallest table holds 14 items distributed among 16 buckets.
;;; So we allocate 14 k/v pairs = 28 cells + 3 overhead = 31 cells,
;;; and at maximum load the table will have a load factor of 87.5%
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +min-hash-table-size+ 14)
  (defconstant kv-pairs-overhead-slots 3)
  (defconstant default-rehash-size $1.5))
(defconstant +min-hash-table-rehash-threshold+ (float 1/16 $1.0))

;; The GC will set this to 1 if it moves an address-sensitive key. This used
;; to be signaled by a bit in the header of the kv vector, but that
;; implementation caused some concurrency issues when we stopped
;; inhibiting GC during hash-table lookup.
;;
;; This indicator properly belongs to the k/v vector for at least 2 reasons:
;; - if the vector is on an already-written page but the table is not,
;;   it avoids a write fault when setting to true. This a boon to gencgc
;; - if there were lock-free tables - which presumably operate by atomically
;;   changing out the vector for a new one - whether the vector is bucketized
;;   correctly after GC is an aspect of the vector, not the table
;;
;; We could do it with a single bit by implementing vops for atomic
;; read/modify/write on the header. In C there's sync_or_and_fetch, etc.
(defmacro kv-vector-needs-rehash (vector) `(svref ,vector 1))

;;; The 'table' points to the hash-table if the table is weak,
;;; or to the hash vector if the table is not weak.
(defmacro kv-vector-table (pairs) `(svref ,pairs (1- (length ,pairs))))

(defmacro new-kv-vector (size weakp)
  `(let ((v (make-array (+ (* 2 ,size) kv-pairs-overhead-slots)
                        :initial-element +empty-ht-slot+)))
     (setf (kv-vector-high-water-mark v) 0)
     (setf (kv-vector-needs-rehash v) 0)
     (set-header-data v (if ,weakp
                            (logior sb-vm:vector-weak-subtype
                                    sb-vm:vector-hashing-subtype)
                            sb-vm:vector-hashing-subtype))

     v))

(defun make-hash-table (&key
                        (test 'eql)
                        (size #.+min-hash-table-size+)
                        (rehash-size #.default-rehash-size)
                        (rehash-threshold 1)
                        (hash-function nil)
                        (weakness nil)
                        (synchronized))
  "Create and return a new hash table. The keywords are as follows:

  :TEST
    Determines how keys are compared. Must a designator for one of the
    standard hash table tests, or a hash table test defined using
    SB-EXT:DEFINE-HASH-TABLE-TEST. Additionally, when an explicit
    HASH-FUNCTION is provided (see below), any two argument equivalence
    predicate can be used as the TEST.

  :SIZE
    A hint as to how many elements will be put in this hash table.

  :REHASH-SIZE
    Indicates how to expand the table when it fills up. If an integer, add
    space for that many elements. If a floating point number (which must be
    greater than 1.0), multiply the size by that amount.

  :REHASH-THRESHOLD
    Indicates how dense the table can become before forcing a rehash. Can be
    any positive number <=1, with density approaching zero as the threshold
    approaches 0. Density 1 means an average of one entry per bucket.

  :HASH-FUNCTION
    If NIL (the default), a hash function based on the TEST argument is used,
    which then must be one of the standardized hash table test functions, or
    one for which a default hash function has been defined using
    SB-EXT:DEFINE-HASH-TABLE-TEST. If HASH-FUNCTION is specified, the TEST
    argument can be any two argument predicate consistent with it. The
    HASH-FUNCTION is expected to return a non-negative fixnum hash code.

  :WEAKNESS
    When :WEAKNESS is not NIL, garbage collection may remove entries from the
    hash table. The value of :WEAKNESS specifies how the presence of a key or
    value in the hash table preserves their entries from garbage collection.

    Valid values are:

      :KEY means that the key of an entry must be live to guarantee that the
        entry is preserved.

      :VALUE means that the value of an entry must be live to guarantee that
        the entry is preserved.

      :KEY-AND-VALUE means that both the key and the value must be live to
        guarantee that the entry is preserved.

      :KEY-OR-VALUE means that either the key or the value must be live to
        guarantee that the entry is preserved.

      NIL (the default) means that entries are always preserved.

  :SYNCHRONIZED
    If NIL (the default), the hash-table may have multiple concurrent readers,
    but results are undefined if a thread writes to the hash-table
    concurrently with another reader or writer. If T, all concurrent accesses
    are safe, but note that CLHS 3.6 (Traversal Rules and Side Effects)
    remains in force. See also: SB-EXT:WITH-LOCKED-HASH-TABLE. This keyword
    argument is experimental, and may change incompatibly or be removed in the
    future."
  (declare (type (or function symbol) test))
  (declare (type unsigned-byte size))
  (multiple-value-bind (userfunp test test-fun hash-fun)
      (cond ((or (eq test #'eq) (eq test 'eq)) (values nil 'eq #'eq #'eq-hash))
            ((or (eq test #'eql) (eq test 'eql)) (values nil 'eql #'eql #'eql-hash))
            ((or (eq test #'equal) (eq test 'equal))
             (values nil 'equal #'equal #'equal-hash))
            ((or (eq test #'equalp) (eq test 'equalp))
             (values nil 'equalp #'equalp #'equalp-hash))
            (t
             ;; FIXME: It would be nice to have a compiler-macro
             ;; that resolved this at compile time: we could grab
             ;; the alist cell in a LOAD-TIME-VALUE, etc.
             (dolist (info *user-hash-table-tests*
                      (if hash-function
                          (if (functionp test)
                              (values t (%fun-name test) test nil)
                              (values t test (%coerce-callable-to-fun test) nil))
                       (error "Unknown :TEST for MAKE-HASH-TABLE: ~S"
                              test)))
               (destructuring-bind (test-name test-fun hash-fun) info
                 (when (or (eq test test-name) (eq test test-fun))
                   (return (values t test-name test-fun hash-fun)))))))
    (when hash-function
      (setf hash-fun (%coerce-callable-to-fun hash-function)
            ;; It is permitted to specify your own hash function with one of the
            ;; builtin predicates. This forces use of the general predicate.
            userfunp t))

    (binding*
          ((size (max +min-hash-table-size+
                      ;; Our table sizes are capped by the 32-bit integers used as indices
                      ;; into the chains. Prevent our code from failing if the user specified
                      ;; most-positive-fixnum here. (fndb says that size is 'unsigned-byte')
                      (min size (ash 1 24)))) ; 16M key/value pairs
           (rehash-size (if (integerp rehash-size)
                            rehash-size
                            (float rehash-size $1.0))) ; always single-float
           ;; FIXME: Original REHASH-THRESHOLD default should be 1.0,
           ;; not 1, to make it easier for the compiler to avoid
           ;; boxing.
           (rehash-threshold (max +min-hash-table-rehash-threshold+
                                  (float rehash-threshold $1.0))) ; always single-float
           ;; KLUDGE: The most natural way of expressing the below is
           ;; (round (/ (float size) rehash-threshold)), and indeed
           ;; it was expressed like that until 0.7.0. However,
           ;; MAKE-HASH-TABLE is called very early in cold-init, and
           ;; the SPARC has no primitive instructions for rounding,
           ;; but only for truncating; therefore, we fudge this issue
           ;; a little. The other uses of truncate, below, similarly
           ;; used to be round. -- CSR, 2002-10-01
           ;;
           ;; Note that this has not yet been audited for
           ;; correctness. It just seems to work. -- CSR, 2002-11-02
           (scaled-size (truncate (/ (float size) rehash-threshold)))
           (bucket-count (power-of-two-ceiling
                          (max scaled-size +min-hash-table-size+)))
           (index-vector (make-array bucket-count
                                     :element-type 'hash-table-index
                                     :initial-element 0))
           (kv-vector (new-kv-vector size weakness))
           ;; Needs to be the half the length of the KV vector to link
           ;; KV entries - mapped to indices at 2i and 2i+1 -
           ;; together.
           ;; We don't need this to be initially 0-filled, so don't specify
           ;; an initial element (in case we ever meaningfully distinguish
           ;; between don't-care and 0-fill)
           (next-vector (make-array (1+ size) :element-type 'hash-table-index))
           ;; same here - don't care about initial contents
           ;; A simple space optimization is possible for EQL tables: most keys
           ;; (notably excepting numbers) are address-based, and the interface functions
           ;; don't look at hashes, so there should be no hash-vector stored.
           ;; Rehashing would need to recompute hashes of numbers which is easy enough.
           ;; For GC to know whether to set the 'rehash' flag after moving a key,
           ;; it would need to know which pointer objects aren't hashed by address.
           ;; Conversely, we use a hash-vector for an EQ table if it has
           ;; a user-supplied hash function.
           (hash-vector (when (or hash-function (neq test 'eq))
                          (make-array (1+ size) :element-type 'hash-table-index)))
           (weakness-bits
            (if weakness
                (or (loop for i below 4
                          when (eq (decode-hash-table-weakness i) weakness)
                          do (return (logior (ash i 4) hash-table-weak-flag)))
                    (bug "Unreachable"))
                0))
           ((getter setter remover)
            (if weakness
                (values #'gethash/weak #'puthash/weak #'remhash/weak)
                (pick-table-methods synchronized userfunp test)))
           (table (%make-hash-table
                   getter setter remover #'clrhash-impl
                   test test-fun hash-fun
                   rehash-size rehash-threshold
                   kv-vector index-vector next-vector hash-vector
                   (logior (if userfunp hash-table-userfun-flag 0)
                           ;; FIXME: shouldn't weak tables always return
                           ;; T from HASH-TABLE-SYNCHRONIZED-P ?
                           (if synchronized hash-table-synchronized-flag 0)
                           weakness-bits))))
      (declare (type index scaled-size))
      ;; The trailing metadata element is either the table itself or the hash-vector
      ;; depending on weakness. Non-weak hashing vectors can be GCed without looking
      ;; at the table. Weak hashing vectors need the table.
      (setf (kv-vector-table kv-vector) (if weakness table hash-vector))
      table)))

;;; I guess we might have more than one representation of a table,
;;; hence this small wrapper function. But why not for the others?
(defun hash-table-count (hash-table)
  "Return the number of entries in the given HASH-TABLE."
  (declare (type hash-table hash-table)
           (values index))
  (hash-table-%count hash-table))

(setf (documentation 'hash-table-rehash-size 'function)
      "Return the rehash-size HASH-TABLE was created with.")

(setf (documentation 'hash-table-rehash-threshold 'function)
      "Return the rehash-threshold HASH-TABLE was created with.")

(setf (documentation 'hash-table-synchronized-p 'function)
      "Returns T if HASH-TABLE is synchronized.")

(declaim (inline hash-table-pairs-capacity))
(defun hash-table-pairs-capacity (pairs) (ash (- (length pairs) kv-pairs-overhead-slots) -1))

(defun hash-table-size (hash-table)
  "Return a size that can be used with MAKE-HASH-TABLE to create a hash
   table that can hold however many entries HASH-TABLE can hold without
   having to be grown."
  (hash-table-pairs-capacity (hash-table-pairs hash-table)))

(setf (documentation 'hash-table-test 'function)
      "Return the test HASH-TABLE was created with.")

(defun signal-corrupt-hash-table (hash-table)
  (error "Unsafe concurrent operations on ~A detected." hash-table))
;;; Called when we detect circular chains in a hash-table.
(defun signal-corrupt-hash-table-bucket (hash-table)
  (error "Corrupt NEXT-chain in ~A. This is probably caused by ~
multiple threads accessing the same hash-table without locking."
         hash-table))


;;;; accessing functions

;;; Make new vectors for the table, extending the table based on the
;;; rehash-size.
(defun hash-table-new-vectors (table)
  (let* ((old-next-vector (hash-table-next-vector table))
         (old-hash-vector (hash-table-hash-vector table))
         ;; The NEXT vector's length is 1 greater than "size" - the number
         ;; of k/v pairs at full capacity.
         (old-size (1- (length old-next-vector)))
         (rehash-size (hash-table-rehash-size table))
         (new-size (typecase rehash-size
                     ;; Ensure that if the user specifies a float that is so close
                     ;; to 1.0 as to disappear in the TRUNCATE that we actually grow.
                     ;; (TRUNCATE (* 14 1.01)) => 14
                     (float (max (the index (truncate (* rehash-size old-size))) ; usually
                                 (1+ old-size)))
                     (fixnum (+ rehash-size old-size)))) ; rarely, I imagine
         (new-n-buckets
          (let* ((pow2ceil (power-of-two-ceiling new-size))
                 (full-lf (/ new-size pow2ceil)))
            ;; If the default rehash-size was employed, let's try to keep the
            ;; load factor within a reasonable band. Otherwise don't bother.
            ;; The motivation for this decision is twofold:
            ;; - if using defaults, it would be ideal to attempt to be nominally
            ;;   conscientious of the 1.5x resize amount.
            ;; - we can't really accommodate arbitrary resize amounts, especially if small.
            ;;   (power-of-2 sizing can't do that- doubling is the only possibility)
            ;;   But we can produce the smallest table consistent with the request.
            ;;   Say e.g. REHASH-SIZE was 2 using the default initial size of 14.
            ;;   Resizing computes 16 k/v pairs which coincides exactly with
            ;;   16 buckets (the nearest power of 2). But if we wish to avoid 100% load,
            ;;   what can we do? Re-double the bin count to 32? Decrease the k/v pair count
            ;;   to 15? Clearly neither of those make sense if the user is trying to say
            ;;   that (s)he wants 2 entries more which means "don't increase by a lot".
            ;;   Changing from 8 buckets (at the old size) to 32 buckets is a lot,
            ;;   so why do that? Conversely, it makes no sense to reduce the k/v pair
            ;;   limit just to keep the LF less than 100%. A similar problem occurs
            ;;   if you specify 1.001 or other float near 1.
            ;;   Anyway, chaining supports load factors in excess of 100%
            (when (eql rehash-size default-rehash-size)
              (cond ((> full-lf 9/10)   ; $.9 is unhappy in cross-float due to inexactness
                     ;; If we're going to decrease the size, make sure we definitely
                     ;; don't decrease below the old size.
                     (setq new-size (floor pow2ceil 100/85)))  ; target LF = 85%
                    ((< full-lf 55/100) ; and $.55 is similarly unhappy
                     (setq new-size (floor pow2ceil 100/65))))) ; target LF = 65%
            pow2ceil))
         ;; These vector lengths are exactly analogous to those in MAKE-HASH-TABLE,
         ;; prompting the question of whether we can share some code.
         (new-index-vector (make-array new-n-buckets
                                       :element-type 'hash-table-index
                                       :initial-element 0))
         ;; New kv vector is not marked as weak, even for a weak table
         (new-kv-vector (new-kv-vector new-size nil))
         (new-next-vector (make-array (1+ new-size) :element-type 'hash-table-index))
         (new-hash-vector
           (when old-hash-vector
             (make-array (1+ new-size) :element-type 'hash-table-index))))
    (values new-kv-vector new-next-vector new-hash-vector new-index-vector)))

#-(vop-translates sb-kernel:set-header-bits)
(progn
  (declaim (inline set-header-bits unset-header-bits))
  (defun set-header-bits (vector bits)
    (set-header-data vector (logior (get-header-data vector) bits))
    (values))
  (defun unset-header-bits (vector bits)
    (set-header-data vector (logand (get-header-data vector)
                                    (ldb (byte (- sb-vm:n-word-bits sb-vm:n-widetag-bits) 0)
                                         (lognot bits))))
    (values)))

;;; We don't define +-MODFX for all backends, and I can't figure out
;;; the rationale, nor how to detect this other than by trial and error.
;;; Like why does 64-bit ARM have it but 32-bit not have?
#-(or x86 x86-64 arm64 riscv)
(progn
(declaim (inline sb-vm::+-modfx))
(defun sb-vm::+-modfx (x y)
  (%make-lisp-obj (logand (+ (get-lisp-obj-address (the fixnum x))
                             (get-lisp-obj-address (the fixnum y)))
                          most-positive-word))))

(macrolet
    ((with-pair ((key-var &optional val-var) &body body)
       `(let* ((key-index (* 2 i))
               (,key-var (aref kv-vector key-index))
               ,@(if val-var `((,val-var (aref kv-vector (1+ key-index))))))
          ,@body))
     (push-in-chain (bucket-index-expr)
       `(let ((bucket (the index ,bucket-index-expr)))
          (setf (aref next-vector i) (aref index-vector bucket)
                (aref index-vector bucket) i))))

(defun rehash (kv-vector hash-vector index-vector next-vector
               &aux (mask (1- (length index-vector)))
                    (next-free 0)
                    (hwm (kv-vector-high-water-mark kv-vector)))
  (if hash-vector
      ;; Scan backwards so that chains are in ascending index order.
      (do ((i hwm (1- i))) ((zerop i))
        (declare (type index/2 i))
        (with-pair (pair-key pair-val)
         ;; It's unclear why we check for key AND value being empty - a half
         ;; empty cell could only appear mid-insertion, but concurrent
         ;; insert is forbidden, so how could it happen?
         ;; i.e. I would think KEY emptiness is an adequate test.
         (cond ((and (empty-ht-slot-p pair-key) (empty-ht-slot-p pair-val))
                ;; Slot is empty, push it onto free list.
                (setf (aref next-vector i) next-free next-free i))
               ((/= (aref hash-vector i) +magic-hash-vector-value+)
                ;; Use the existing hash value (not address-based hash)
                (push-in-chain (mask-hash (aref hash-vector i) mask)))
               (t
                ;; Set address-sensitivity BEFORE depending on the bits.
                ;; Precise GC platforms can move any key except the ones which
                ;; are explicitly pinned.
                (set-header-bits kv-vector sb-vm:vector-addr-hashing-subtype)
                (push-in-chain (pointer-hash->bucket
                                (pointer-hash pair-key) mask))))))
      (do ((i hwm (1- i))) ((zerop i))
        (declare (type index/2 i))
        (with-pair (pair-key pair-val)
         (cond ((and (empty-ht-slot-p pair-key) (empty-ht-slot-p pair-val))
                (setf (aref next-vector i) next-free next-free i))
               (t
                (set-header-bits kv-vector sb-vm:vector-addr-hashing-subtype)
                (push-in-chain (pointer-hash->bucket
                                (pointer-hash pair-key) mask)))))))
  ;; This is identical to the calculation of next-free-kv in INSERT-AT.
  (cond ((/= next-free 0) next-free)
        ((= hwm (hash-table-pairs-capacity kv-vector)) 0)
        (t (1+ hwm))))

;;; Rehash due to key movement, and find KEY at the same time.
;;; Finding the key obviates the need for the rehashing thread to loop
;;; testing whether to rehash. Imagine an unlucky schedule wherein each rehash
;;; ends up invalid due to maximally bad timing of GC, and every reader sees
;;; nothing but 'need-to-rehash', and the INDEX vector is repeatedly overwritten
;;; with zeros. The -AND-FIND aspect of this ensures progress.
(defun %rehash-and-find (table epoch key
                         &aux (kv-vector (hash-table-pairs table))
                              (next-vector (hash-table-next-vector table))
                              (hash-vector (hash-table-hash-vector table))
                              (rehashing-state (1+ epoch)))
  (declare (hash-table table) (fixnum epoch))
  (atomic-incf (hash-table-n-rehash+find table))
  ;; Verify some invariants prior to disabling array bounds checking
  (aver (>= (length kv-vector) #.(+ (* 2 +min-hash-table-size+)
                                    kv-pairs-overhead-slots)))
  (aver (= (ash (length kv-vector) -1) (length next-vector)))
  (when hash-vector
    (aver (= (length hash-vector) (length next-vector))))
  ;; Empty cells must be in the free chain already, and no smashed cells exist.
  (aver (null (hash-table-smashed-cells table)))
  ;; Must not permit the rehashing state to stick due to a nonlocal exit.
  ;; All further normal use of the table would be prevented.
  (without-interrupts
   ;; Transitioning from #b01 to #b10 clears the 'rehash' bit and sets the
   ;; rehash-in-progress bit. It also gives this thread exclusive write access
   ;; to the hashing vectors, since at most one thread can win this CAS.
   (when (eq (cas (svref kv-vector 1) epoch rehashing-state) epoch)
     ;; Remove address-sensitivity, preserving the other flags.
     (unset-header-bits kv-vector sb-vm:vector-addr-hashing-subtype)
     ;; Rehash in place. For the duration of the rehash, readers who otherwise
     ;; might have seen intact chains (by which to find address-insensitive keys)
     ;; can't. No big deal. If we were willing to cons new vectors, we could
     ;; rehash into them and CAS them in, but the advantage would be minimal-
     ;; obsolete chains could only work for a possibly-empty subset of keys.
     (let* ((index-vector (fill (hash-table-index-vector table) 0))
            (mask (1- (length index-vector)))
            (hwm (kv-vector-high-water-mark kv-vector))
            (result 0))
       (declare (optimize (sb-c::insert-array-bounds-checks 0)))
       (if hash-vector
           (do ((i hwm (1- i))) ((zerop i))
             (declare (type index/2 i))
             (with-pair (pair-key)
              ;; Leave the free cell chain untouched, since rehashing
              ;; due to key movement can not possibly affect that chain.
              (unless (empty-ht-slot-p pair-key)
                (cond ((/= (aref hash-vector i) +magic-hash-vector-value+)
                       (push-in-chain (mask-hash (aref hash-vector i) mask)))
                      (t
                       (set-header-bits kv-vector sb-vm:vector-addr-hashing-subtype)
                       (push-in-chain (pointer-hash->bucket
                                       (pointer-hash pair-key) mask))))
                (when (eq pair-key key) (setq result key-index)))))
           ;; No hash vector
           (do ((i hwm (1- i))) ((zerop i))
             (declare (type index/2 i))
             (with-pair (pair-key)
              (unless (empty-ht-slot-p pair-key)
                (set-header-bits kv-vector sb-vm:vector-addr-hashing-subtype)
                (push-in-chain (pointer-hash->bucket
                                (pointer-hash pair-key) mask))
                (when (eq pair-key key) (setq result key-index))))))
       ;; Clear rehash bit and bump the epoch, wrapping around to keep it a fixnum
       (let ((new-epoch (sb-vm::+-modfx epoch 3)))
         ;; Setting the new epoch races with GC which might set 'rehash' again.
         ;; At most 2 attempts are needed since no other state change can occur.
         (let ((old (cas (svref kv-vector 1) rehashing-state new-epoch)))
           (unless (eq rehashing-state old)
             (aver (eq old (logior rehashing-state 1))) ; rehashed, but already obsolete
             (aver (eq old (cas (svref kv-vector 1) old (logior new-epoch 1)))))))
       (unless (eql result 0)
         (setf (hash-table-cache table) result))
       result))))
) ; end MACROLET

;;; Enlarge TABLE.  If it is weak, then both the old and new vectors are temporarily
;;; made non-weak so that we don't have to deal with GC-related shenanigans.
(defun grow-hash-table (table)
  (declare (type hash-table table))
  (binding* (((new-kv-vector new-next-vector new-hash-vector new-index-vector)
              (hash-table-new-vectors table))
             (old-kv-vector (hash-table-pairs table))
             (hwm (kv-vector-high-water-mark old-kv-vector)))

    (declare (type simple-vector new-kv-vector)
             (type (simple-array hash-table-index (*)) new-next-vector new-index-vector)
             (type (or null (simple-array hash-table-index (*))) new-hash-vector))

    ;; Rehash + resize only occurs when:
    ;;  (1) every usable pair was at some point filled (so HWM = SIZE)
    ;;  (2) no cells below HWM are available (so COUNT = SIZE)
    (aver (= hwm (hash-table-size table)))
    (unless (hash-table-weak-p table)
      ;; Consider a GC that occurs after we've decided that the table
      ;; is too small, but before getting here. There could be some smashed
      ;; cells, and if so, the count will be less than the SIZE.
      (aver (= (hash-table-count table) hwm)))

    ;; Copy over the hash-vector,
    ;; This is done early because when GC scans the new vector, it needs to see
    ;; each hash to know which keys were hashed address-sensitively.
    (awhen (hash-table-hash-vector table)
      (replace new-hash-vector it :start1 1 :start2 1)) ; 1st element not used

    ;; Preserve only the 'hashing' bit on the OLD-KV-VECTOR so that
    ;; its high-water-mark can meaningfully be reduced to 0 when done.
    ;; Clearing the address-sensitivity is a performance improvement
    ;; since GC won't check on a per-key basis whether to flag the vector
    ;; for rehash (it's going to be zeroed out).
    ;; Clearing the weakness causes all entries to stay alive.
    ;; Furthermore, clearing both makes the trailing metadata ignorable.
    (set-header-data old-kv-vector sb-vm:vector-hashing-subtype)
    (setf (kv-vector-table old-kv-vector) 0)

    ;; The high-water-mark remains unchanged.
    ;; Set this before copying pairs, otherwise they would not be seen
    ;; in the new vector since GC scanning ignores elements below the HWM.
    (setf (kv-vector-high-water-mark new-kv-vector) hwm)
    ;; Reference the hash-vector from the KV vector.
    ;; Normally a weak hash-table's KV vector would reference the table
    ;; (because cull needs to examine the table bucket-by-bucket), and
    ;; not the hash-vector directly. But we can't reference the table
    ;; since using the table's hash-vector gets the OLD hash vector.
    ;; It might be OK to point the table at the new hash-vector now,
    ;; but I'd rather the table remain in a consistent state, in case we
    ;; ever devise a way to allow concurrent reads with a single writer,
    ;; for example.
    (setf (kv-vector-table new-kv-vector) new-hash-vector)

    ;; Copy the k/v pairs excluding leading and trailing metadata.
    (replace new-kv-vector old-kv-vector
             :start1 2 :start2 2 :end2 (* 2 (1+ hwm)))

    (let ((next-free (rehash new-kv-vector new-hash-vector
                             new-index-vector new-next-vector)))
      (setf (hash-table-pairs table)        new-kv-vector
            (hash-table-hash-vector table)  new-hash-vector
            (hash-table-index-vector table) new-index-vector
            (hash-table-next-vector table)  new-next-vector
            (hash-table-next-free-kv table) next-free)

      (when (hash-table-weak-p table)
        (setf (hash-table-smashed-cells table) nil)
        ;; Now that the table points to the right hash-vector
        ;; we can set the vector's backpointer and turn it weak.
        (setf (kv-vector-table new-kv-vector) table)
        (set-header-bits new-kv-vector sb-vm:vector-weak-subtype))

      ;; Zero-fill the old kv-vector. For weak hash-tables this removes the
      ;; strong references to each k/v. For non-weak vectors there is no technical
      ;; reason to do this except for safety. GC will not scavenge past the high water
      ;; mark, but if you had your hands on the old vector and decide to dereference
      ;; it (which is probably indicates a data race), you could deference a dangling
      ;; pointer. In other words, even if the vector were considered a root,
      ;; it wouldn't matter from a heap consistency perspective because it would
      ;; not transitively enliven anything, but you can't stop people from using
      ;; SVREF on it past the high water mark. To to make things safe,
      ;; we sort of have to zero-fill.
      ;; Also fwiw, it would be necessary to fix verify_range() to understand
      ;; that it MUST NOT verify past the hwm, and similary the low-level debugger
      ;; if we didn't zero-fill.
      ;; Or, if we could trust people not to look at the old vector, we could just
      ;; change the widetag into simple-array-word instead of scribbling over it.
      (fill old-kv-vector 0 :start 2 :end (* (1+ hwm) 2))
      (setf (kv-vector-high-water-mark old-kv-vector) 0)
      next-free)))

(defun gethash (key hash-table &optional default)
  "Finds the entry in HASH-TABLE whose key is KEY and returns the
associated value and T as multiple values, or returns DEFAULT and NIL
if there is no such entry. Entries can be added using SETF."
  (declare (type hash-table hash-table)
           (values t (member t nil)))
  (gethash3 key hash-table default))

;;; Define a specialized variation of GETHASH3 for a standard test fun,
;;; or the general case.
;;; A note about in the inner loop: the secondary value of the hash function
;;; indicates whether an address was taken, which implies that we should use EQ
;;; as the comparator, but it is not an "if and only if" - other objects can be
;;; compared by EQ, for example symbols and fixnums.

;;; The code below could be further tweaked by doing a few things:
;;; - removing the probing limit check
;;; - skipping array bounds checks
;;; - eliminating some multiply-by-2 operations
;;; The first two are easy enough. The last means that we would
;;; rejigger the INDEX + NEXT vectors to contain physical elt indices.
;;; We could define safe and unsafe variants of each getter, and an "unsafe"
;;; variant of MAKE-HASH-TABLE that installs the unsafe getter.
;;; A source transform would select the variant of MAKE-HASH-TABLE by policy.

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; macroexpander helper functions. These rely on a naming convention
  ;; to keep things simple so that we don't have to pass in the names
  ;; of local variables to bind.

  (defun ht-hash-setup (std-fn)
    (if std-fn
        `(((hash0 address-based-p)
           ;; so many warnings about generic SXHASH - who cares
           (locally (declare (muffle-conditions compiler-note))
             (,(symbolicate std-fn "-HASH") key)))
          (hash (prefuzz-hash hash0)))
        '((hash0 (funcall (hash-table-hash-fun hash-table) key))
          (address-based-p nil)
          (hash (prefuzz-hash hash0)))))

  (defun ht-probe-setup (std-fn &optional more-bindings)
    `((index-vector (hash-table-index-vector hash-table))
      ;; BUCKET is the masked hash code which acts as the index into index-vector
      (bucket (mask-hash hash (1- (length index-vector))))
      ;; INDEX is the index into the pairs vector obtains from the index-vector
      (index (aref index-vector bucket))
      (next-vector (hash-table-next-vector hash-table))
      ;; Binding of HASH-VECTOR would be flushed for EQ and EQL tables
      ;; automatically, but we forcibly elide some code because "reasons"
      ;; so we need to avoid "unused variable warnings" by eliding this
      ;; binding as well.
      ,@(unless (member std-fn '(eq eql))
          '((hash-vector (hash-table-hash-vector hash-table))))
      ,@(when (member std-fn '(nil *))
          '((test-fun (hash-table-test-fun hash-table))))
      (probe-limit (length next-vector))
      ,@more-bindings))

  (defun ht-probing-should-use-eq (std-fn)
    (ecase std-fn ; try to strength-reduce the test for this key
      (eq 't) ; always use EQ
      ;; EQL vs EQ only matters for numbers which aren't immediate.
      ;; Not worth the trouble to get single-float in here for 64-bit.
      (eql '(or address-based-p (typep key '(or (not number) fixnum))))
      ;; EQUAL and EQUALP hash function say that symbols are NOT address-sensitive
      ;; - which they aren't, as they use the SYMBOL-HASH slot, unlike EQ-HASH
      ;; which takes the address - but we want to compare symbols by EQ.
      ;; There is diminishing payoff from listing other types here, though we might
      ;; want EQUAL comparison on instances to reduce to EQ as long as KEY isn't
      ;; a pathname.  As to NON-NULL-SYMBOL, I'm just being slightly more efficient
      ;; by not checking for NIL, precisely because it's not an important case,
      ;; and SYMBOLP uses more instructions. This is similar in philosophy to the
      ;; EQL branch where I don't care whether single float (on 64-bit) is or isn't
      ;; handled by the EQ comparator.
      ((equal equalp)
       '(or address-based-p (fixnump key) (non-null-symbol-p key)))
      ((nil)
       ;; If the hash-function is nonstandard, it's nonetheless possible
       ;; to use EQ as the comparator.
       '(eq (hash-table-test hash-table) 'eq))))

  (defun ht-key-compare (std-fn pair-index &optional endp-test)
    ;; STD-FN = NIL says that's definitely a user-defined hash function,
    ;; and * says that it could be either standard or user-defined.
    (let* ((hashcompare
            (case std-fn
              ((equal equalp nil) `(= hash (aref hash-vector ,pair-index)))
              ((*) `(or (null hash-vector) (= hash (aref hash-vector ,pair-index))))))
           (keycompare
            (case std-fn
              ;; Use the inline EQL function
              (eql `(%eql key (aref kv-vector (* 2 ,pair-index))))
              ((nil *) `(funcall test-fun key (aref kv-vector (* 2 ,pair-index))))
              (t `(,std-fn key (aref kv-vector (* 2 ,pair-index))))))
           (compare
            (if hashcompare `(and ,hashcompare ,keycompare) keycompare)))
      (cond ((not endp-test) compare)
            ;; For standard general predicates, compare the key before checking
            ;; whether probing must terminate (index = 0). For user predicates
            ;; that might accept only strings, for example, check index first.
            ;; This way, a hit on the first probe avoids one test for standard
            ;; tests. Index is post-checked to deem it a hit or miss.
            (std-fn `(or ,compare (eql ,pair-index 0)))
            (t `(or (eql ,pair-index 0) ,compare))))))

(defmacro check-excessive-probes (n-probes)
  `(when (minusp (decf (truly-the fixnum probe-limit) ,n-probes))
     ;; The next-vector chain is circular. This is caused
     ;; caused by thread-unsafe mutations of the table.
     (signal-corrupt-hash-table-bucket hash-table)))

(defmacro ht-probe-advance (var)
  `(setq ,var (truly-the index/2 (aref next-vector ,var))))

(defmacro kv-vector-rehash-epoch (vector) `(truly-the fixnum (svref ,vector 1)))
(defconstant kv-vector-rehashing 2)

#|
bottom 2 bits of kv-vector-rehash-epoch:

00 = valid hashes
01 = an address-sensitive key moved
10 = rehashing in progress
11 = rehashing + an address-sensitive key moved

'nnnn' is the timestamp ("epoch"). If it changed to MMMM then we can't
reason about the status bits, so restart from the top.

Pre-       Post-     Miss
loookup    lookup    Action
-------    -------   ------
nnnn 00    nnnn 0_   valid (KEY is pinned, so don't care if hashes became invalid)
nnnn 00    nnnn 1_   linear scan
nnnn 00    MMMM __   restart

nnnn 01    nnnn 01   valid if not address-sensitive, otherwise rehash and find
nnnn 01    nnnn 1_   linear scan
nnnn 01    MMMM __   restart

nnnn 1_    any       linear scan
|#

(defmacro define-ht-getter (name std-fn)
  ;; For synchronized GETHASH we've already acquired the lock,
  ;; so this KV-VECTOR is the most current one.
  `(defun ,name (key table default
                     &aux (hash-table (truly-the hash-table table))
                          (kv-vector (hash-table-pairs hash-table)))
     (declare (optimize speed (sb-c::verify-arg-count 0)))
     (let ((cache (hash-table-cache hash-table)))
       ;; First check the cache using EQ, not the test fun, for speed.
       ;; [we prefer to guard calls to the test fun by comparing hashes first,
       ;; but we haven't computed the hash yet]
       ;; We can test cache '/=' 0 last rather than 1st or 2nd so that a miss
       ;; can be determined with only two comparisons, rather than all three.
       ;; Whereas if the '/=' guarded the EQ test, then either a hit or miss
       ;; would need all three tests. "possibly 2" beats "definitely 3" tests.
       (when (and (< cache (length kv-vector))
                  (eq (aref kv-vector cache) key)
                  (/= cache 0)) ; don't falsely match the metadata cell
         (return-from ,name (values (aref kv-vector (1+ cache)) t))))
     (with-pinned-objects (key)
       (binding* (,@(ht-hash-setup std-fn)
                  (eq-test ,(ht-probing-should-use-eq std-fn)))
         (declare (hash hash0))
         (flet ((hash-search (&aux ,@(ht-probe-setup std-fn))
                  (declare (index/2 index))
                  ;; Search next-vector chain for a matching key.
                  (if eq-test
                      (macrolet ((probe ()
                                   '(if ,(ht-key-compare 'eq 'index t)
                                        (return index)
                                        (ht-probe-advance index))))
                        (loop (probe) (probe) (probe) (probe)
                              ;; Optimistically assuming we hit the key, this check
                              ;; never executes. The exact boundary case doesn't matter
                              ;; (this might allow a few too many iterations),
                              ;; but as long as we can detect circularity, it's fine.
                              (check-excessive-probes 4)))
                      ;; We get too many "unreachable code" notes for my liking,
                      ;; if this branch is unconditionally left in.
                      ,(unless (eq std-fn 'eq)
                         `(macrolet ((probe ()
                                       '(if ,(ht-key-compare std-fn 'index t)
                                            (return index)
                                            (ht-probe-advance index))))
                            ;; In the case of EQL, we get most bang-for-buck (performance per
                            ;; line of asm code) by inlining EQL, not by loop unrolling,
                            ;; but we might as well unroll a little bit to lessen the impact
                            ;; of check-for-overflow.
                            ;; You can experiment with unrolling here easily enough.
                            ,(if (eq std-fn 'eql)
                                 '(loop (probe) (probe) (check-excessive-probes 2))
                                 '(loop (probe) (check-excessive-probes 1))))))))
           (named-let retry ((initial-epoch (kv-vector-rehash-epoch kv-vector)))
             (if (logtest initial-epoch kv-vector-rehashing)
                 (truly-the (values t boolean &optional)
                            (hash-table-lsearch hash-table eq-test key hash default))
                 (let ((index (hash-search)))
                   (if (not (eql index 0))
                       (let ((key-index (* 2 (truly-the index/2 index))))
                         (setf (hash-table-cache hash-table) key-index)
                         (values (aref kv-vector (1+ key-index)) t))
                       (let ((epoch (kv-vector-rehash-epoch kv-vector)))
                         (cond ((and (evenp initial-epoch) ; valid hashes at start?
                                     (zerop (logandc2 (logxor epoch initial-epoch) 1)))
                                ;; Provided that the epoch didn't change, the 'rehash'
                                ;; bit can be ignored since rehash did not occur.
                                (values default nil))
                               ((and (oddp initial-epoch) (= epoch initial-epoch))
                                ;; Epoch didn't change, but address-based hashes were
                                ;; not valid at the start of the lookup.
                                ;; Stably hashed (address-insensitive) keys are ok.
                                (if (not address-based-p)
                                    (values default nil)
                                    (let ((key-index (%rehash-and-find hash-table epoch key)))
                                      (cond ((eql key-index 0) ; didn't find, again
                                             (values default nil))
                                            ((not (fixnump key-index))
                                             ;; conflicted with other thread trying to rehash
                                             (retry (kv-vector-rehash-epoch kv-vector)))
                                            (t
                                             (values (aref kv-vector (1+ key-index)) t))))))
                               (t ; epoch changed
                                (retry epoch)))))))))))))

;;;; Weak table variant.

;;; A single function acts as the core of all operations on weak tables.
;;; The advantage is that we could simulate lazy weak lists by removing smashed
;;; pairs while doing any table operation, with no GC support beyond the ability
;;; to change a cell from non-empty to smashed. Therefore if we could leverage a
;;; collector from some other language, it need not understand our weak
;;; hash-table representation in depth.
(macrolet ((with-pair ((&optional (physical-index '(* next 2))) &body body)
             ;; Load the {k,v} of a pair. We will check that they're not smashed
             ;; before using. Note an extreme subtlety: by loading VAL first,
             ;; we decrease the chance of seeing a live key but smashed value.
             `(let* ((physical-index (truly-the index ,physical-index))
                     (probed-val (svref kv-vector (1+ physical-index)))
                     (probed-key (svref kv-vector physical-index)))
                ,@body)))
  (defun findhash-weak (key hash-table hash address-based-p)
    (declare (hash-table hash-table) (optimize speed)
             (type (unsigned-byte #-64-bit 29 #+64-bit 31) hash))
    (macrolet ((return-hit ()
                 '(return-from findhash-weak
                    (values probed-val probed-key physical-index predecessor))))
      ;; FIXME: since a weak table is always synchronized, maybe get rid of
      ;; this loop-over-epoch noise?
      (let ((start-epoch sb-kernel::*gc-epoch*)
            (kv-vector (hash-table-pairs hash-table)))
        (tagbody
           (go entry)
          MISS
           (let ((current-epoch sb-kernel::*gc-epoch*))
             (when (eq start-epoch current-epoch)
               (return-from findhash-weak (values +empty-ht-slot+ 0 0 0)))
             (setq start-epoch current-epoch))
          ENTRY
           (when (oddp (truly-the fixnum
                        (kv-vector-needs-rehash (hash-table-pairs hash-table))))
             (without-interrupts
               ;; Remove weakness and address-sensitivity.
               (set-header-data kv-vector sb-vm:vector-hashing-subtype)
               ;; Clear the need-to-rehash flag. Since there can be no other readers,
               ;; we're not publishing bad information (i.e. allegedly valid hashes
               ;; when they're in an inconsistent state). If GC sets the flag again,
               ;; we'll know that the end state is already obsolete.
               ;; In this regard the algorithm is simpler than that of %REHASH-AND-FIND.
               (setf (kv-vector-needs-rehash kv-vector) 0)
               ;; We don't need to zero-fill the NEXT vector, just the INDEX vector.
               ;; Unless a key slot can be reached by a chain starting from the index
               ;; vector or the 'next' of a previous chain element, we don't read either
               ;; the key or its corresponding 'next'. So we only need to assign a
               ;; 'next' at the moment a slot is linked into a chain.
               (setf (hash-table-next-free-kv hash-table)
                     (rehash kv-vector (hash-table-hash-vector hash-table)
                             (fill (hash-table-index-vector hash-table) 0)
                             (hash-table-next-vector hash-table)))
               ;; Empty cells will have been placed in the ordinary freelist,
               ;; so clear the list of GC-smashed cells.
               (setf (hash-table-smashed-cells hash-table) nil)
               ;; Re-enable weakness
               (set-header-bits kv-vector sb-vm:vector-weak-subtype)))
           ;; Note that it's OK for a GC + another rehash to
           ;; be triggered by another thread after this point, since the
           ;; GC epoch check will catch it.
           (binding* #.(ht-probe-setup '* '((predecessor nil)))
             (declare (index/2 index))
             ;; Everything in an EQ table is address-based - though this is subject
             ;; to change, as we could stably hash symbols, because why not -
             ;; but the hash fun's second value is NIL on immediate objects.
             (if (or address-based-p (eq (hash-table-test hash-table) 'eq))
                 (do ((next index (aref next-vector next)))
                     ((zerop next) (go miss))
                   (declare (type index/2 next))
                   (with-pair () (when (eq key probed-key) (return-hit)))
                   (check-excessive-probes 1)
                   (setq predecessor next))
                 ;; For any other test, we assume that it is not safe to pass the
                 ;; unbound marker to the predicate (though EQUAL and EQUALP are
                 ;; probably OK). Also, compare hashes first.
                 (do ((next index (aref next-vector next)))
                     ((zerop next) (go miss))
                   (declare (type index/2 next))
                   (with-pair ()
                     (when (and (not (empty-ht-slot-p probed-key))
                                (= hash (aref hash-vector next))
                                (funcall test-fun key probed-key))
                       (return-hit)))
                   (check-excessive-probes 1)
                   (setq predecessor next)))))))))

(defmacro with-weak-hash-table-entry (&body body)
  `(with-pinned-objects (key)
     (binding* (((hash0 address-sensitive-p)
                 (funcall (hash-table-hash-fun hash-table) key))
                (address-sensitive-p
                 (unless (logtest (hash-table-flags hash-table) hash-table-userfun-flag)
                   address-sensitive-p))
                (hash (prefuzz-hash (the hash hash0))))
       ;; It would be ideal if we were consistent about all tables NOT having
       ;; synchronization unless created with ":SYNCHRONIZED T".
       ;; But removing the implicit synchronization from weak tables looks tricky
       ;; and doesn't seem worth spending much time on.
       (sb-thread::with-recursive-system-lock ((hash-table-lock hash-table))
         ;; Receive the existing k,v pair to ensure liveness thereof because
         ;; otherwise we have no "certificate of existence" of the pair.
         (binding* (((probed-value probed-key physical-index predecessor)
                     (findhash-weak key hash-table hash address-sensitive-p))
                    (kv-vector (hash-table-pairs hash-table)))
           (declare (index physical-index))
           ,@body)))))

(defun gethash/weak (key hash-table default)
  (declare (type hash-table hash-table) (optimize speed))
  (truly-the (values t t &optional)
             (with-weak-hash-table-entry
               (declare (ignore probed-key physical-index predecessor kv-vector))
        ;; empty can occur in two cases: not found, or GC culled a pair even when
        ;; a key was found - consider a table with weakness kind :VALUE.
        ;; It's best that we be agnostic of the exact order that WITH-PAIR loads
        ;; the parts and the order in which GC writes the empty markers.
               (if (empty-ht-slot-p probed-value)
                   (values default nil)
                   (values probed-value t)))))

(define-ht-getter gethash/eq eq)
(define-ht-getter gethash/eql eql)
(define-ht-getter gethash/equal equal)
(define-ht-getter gethash/equalp equalp)
(define-ht-getter gethash/any nil)

;;; In lieu of racing to rehash in multiple threads due to GC key movement,
;;; or blocking on a mutex to rehash, threads can perform just the FIND
;;; aspect of %REHASH-AND-FIND which is obviously less work than rehashing.
;;; It has the same computational complexity but fewer instructions,
;;; and allows forward progress in all threads.
;;; But unlike %REHASH-AND-FIND which will only be called when the key
;;; in question was hashed by its address, this search can be called for any
;;; key and hash-table-test whenever some thread is rehashing. If hash-based
;;; lookup uses EQ as the comparator for KEY, then linear search does too.
(defun hash-table-lsearch (hash-table eq-test key hash default)
  (declare (optimize (sb-c::insert-array-bounds-checks 0)))
  (declare (type (and fixnum unsigned-byte) hash))
  (atomic-incf (hash-table-n-lsearch hash-table))
  (let* ((kv-vector (hash-table-pairs hash-table))
         (key-index
          (let ((hash-vector (hash-table-hash-vector hash-table))
                (hwm (the index/2 (kv-vector-high-water-mark kv-vector))))
            (cond ((or eq-test (eq (hash-table-test hash-table) 'eq))
                   (loop for i from (* hwm 2) downto 2 by 2
                         when (eq key (aref kv-vector i)) return i))
                  ((eq (hash-table-test hash-table) 'eql)
                   (loop for i from (* hwm 2) downto 2 by 2
                         when (%eql key (aref kv-vector i)) return i))
                  (t
                   ;; EQ and EQL are fine with unbound-marker as an argument,
                   ;; but the general case checks the key for validity first.
                   (let ((test-fun (hash-table-test-fun hash-table)))
                     (aver (= (length hash-vector) (ash (length kv-vector) -1)))
                     (loop for i from hwm downto 1
                           when (and (= hash (aref hash-vector i))
                                     (let ((pair-key (aref kv-vector (* 2 i))))
                                       (and (not (empty-ht-slot-p pair-key))
                                            (funcall test-fun key pair-key))))
                           return i)))))))
    (cond (key-index
           (setf (hash-table-cache hash-table) key-index)
           (values (aref kv-vector (1+ key-index)) t))
          (t
           (values default nil)))))

(defun pick-table-methods (synchronized userp test)
 (macrolet ((gen-cases (wrapping)
              `(case test
                  (eq    (,wrapping gethash/eq puthash/eq remhash/eq))
                  (eql   (,wrapping gethash/eql puthash/eql remhash/eql))
                  (equal (,wrapping gethash/equal puthash/equal remhash/equal))
                  (t     (,wrapping gethash/equalp puthash/equalp remhash/equalp))))
            (locked-methods (getter setter remover)
              `(values (named-lambda ,(symbolicate getter "/LOCK") (key table default)
                         (declare (optimize speed (sb-c::verify-arg-count 0)))
                         (truly-the (values t t &optional)
                           (sb-thread::with-recursive-system-lock
                               ((hash-table-lock (truly-the hash-table table)))
                             (,getter key table default))))
                       (named-lambda ,(symbolicate setter "/LOCK") (key table value)
                         (declare (optimize speed (sb-c::verify-arg-count 0)))
                         (truly-the (values t &optional)
                           (sb-thread::with-recursive-system-lock
                               ((hash-table-lock (truly-the hash-table table)))
                             (,setter key table value))))
                       (named-lambda ,(symbolicate remover "/LOCK") (key table)
                         (declare (optimize speed (sb-c::verify-arg-count 0)))
                         (truly-the (values t &optional)
                           (sb-thread::with-recursive-system-lock
                               ((hash-table-lock (truly-the hash-table table)))
                             (,remover key table))))))
            (methods (getter setter remover)
              `(values #',getter #',setter #',remover)))
    (if userp
        (if synchronized
            (locked-methods gethash/any puthash/any remhash/any)
            (methods gethash/any puthash/any remhash/any))
        (if synchronized
            (gen-cases locked-methods)
            (gen-cases methods)))))

;;; Three argument version of GETHASH
(defun gethash3 (key hash-table default)
  (declare (type hash-table hash-table))
  (funcall (truly-the (sfunction (t t t) (values t boolean))
                      (hash-table-gethash-impl hash-table))
           key hash-table default))

;;; so people can call #'(SETF GETHASH)
;;; FIXME: this function is not mandated. Why do we have it?
(defun (setf gethash) (new-value key table &optional default)
  (declare (ignore default))
  (%puthash key table new-value))

(defun hash-table-next-smashed-kv (hash-table)
  ;; Entries culled by GC are linked into a plain old list of cons cells,
  ;; because we can atomically manipulate that. We can't atomically operate
  ;; on the array-qua-list representation, both because we don't support
  ;; numeric arrays in (CAS AREF) and lockfree deletion from interior nodes
  ;; of singly-linked lists is tricky (a concurrent insert can get lost).
  (when (hash-table-smashed-cells hash-table)
    (binding* ((data (atomic-pop (hash-table-smashed-cells hash-table)))
               ((kv-index bucket) (etypecase data
                                   (fixnum (values (ldb (byte 14 14) data)
                                                   (ldb (byte 14 0) data)))
                                   (cons (values (car data) (cdr data)))))
               (index-vector (hash-table-index-vector hash-table))
               (next-vector (hash-table-next-vector hash-table))
               (this (aref index-vector bucket))
               (successor (aref next-vector this)))
      (if (= kv-index this)
          ;; This pair started a chain. Removing it is easy
          (setf (aref index-vector bucket) successor)
          ;; Else, find the kv-index in the chain and snap it out.
          (do ((predecessor this)
               (this successor))
              ((= this 0) (signal-corrupt-hash-table hash-table))
            (let ((successor (aref next-vector this)))
              (when (= kv-index this)
                (return (setf (aref next-vector predecessor) successor)))
              (setq predecessor this this successor))))
      ;; Set the 'next' at kv-index to the head of the ordinary freelist
      ;; so that when INSERT-AT pops the freelist, it stays correct.
      (setf (aref next-vector kv-index) (hash-table-next-free-kv hash-table))
      kv-index)))

;;; We don't need the looping and checking for GC epoch in PUTHASH
;;; because insertion can not co-occur with any other operation,
;;; unlike GETHASH which we allow to execute in multiple threads.
(defmacro define-ht-setter (name std-fn)
  `(named-lambda ,name (key table value &aux (hash-table (truly-the hash-table table))
                                             (kv-vector (hash-table-pairs hash-table)))
     (declare (optimize speed (sb-c::verify-arg-count 0)))
     (block done
       (let ((cache (hash-table-cache hash-table)))
         ;; Check the cache
         (when (and (< cache (length kv-vector))
                    (eq (aref kv-vector cache) key)
                    (/= cache 0)) ; don't falsely match the metadata cell
           ;; If cached, just store here
           (return-from done (setf (aref kv-vector (1+ cache)) value))))
       (with-pinned-objects (key)
         ;; Read the 'rehash' bit as soon as possible after pinning KEY,
         ;; but not before.  The closer in time we observe the bit vs pinning,
         ;; the more likely it is to reflect at most the moved status of KEY
         ;; and not also the moved status of keys moved after KEY got pinned.
         ;; To illustrate:
         ;;
         ;;   t0 .... t1 .... t2 .... t3 ....
         ;;    ^      ^          ^       ^
         ;;    |      |          | GC    |
         ;;    Pin   Possible           Actual
         ;;          observation        observation
         ;;          of 'rehash'        of 'rehash'
         ;;
         ;; If GC causes the bit to become set, and we don't read the bit
         ;; until t3, we're forced to conclude that KEY's hash might have been wrong.
         ;; Granted that the bit might have been 1 at timestamp 't1',
         ;; but it's best to read it at t1 and not later.
         (binding* ((initial-epoch (kv-vector-rehash-epoch kv-vector))
                    ,@(ht-hash-setup std-fn)
                    ,@(ht-probe-setup std-fn)
                    (eq-test ,(ht-probing-should-use-eq std-fn)))
           (declare (hash hash0) (index/2 index))
           ;; Search next-vector chain for a matching key.
           (if eq-test
               ;; TODO: consider unrolling a few times like in %GETHASH
               (do ((next index (aref next-vector next)))
                   ((zerop next))
                 (declare (type index/2 next))
                 (let ((i (* 2 next)))
                   (when (eq key (aref kv-vector i)) ; Found, just replace the value.
                     (setf (hash-table-cache hash-table) i)
                     (return-from done (setf (aref kv-vector (1+ i)) value))))
                 (check-excessive-probes 1))
               ,(unless (eq std-fn 'eq)
                  `(do ((next index (aref next-vector next)))
                       ((zerop next))
                     (declare (type index/2 next))
                     (when ,(ht-key-compare std-fn 'next) ; Found, just replace the value.
                       (let ((i (* 2 next)))
                         (setf (hash-table-cache hash-table) i)
                         (return-from done (setf (aref kv-vector (1+ i)) value))))
                     (check-excessive-probes 1))))
           ;; Detect whether the failure to find was due to key movement.
           ;; Only the initial state of the 'rehash' bit is important.
           ;; If the bit changed from 0 to 1, then KEY's hash was good because
           ;; it was pinned at the time we observed the rehash status to be 0.
           (when (and address-based-p (oddp initial-epoch))
             ;; The current epoch must be the same as initial-epoch, because
             ;; PUTHASH is disallowed concurrently with any other operation,
             ;; and the 'rehash' bit can't be cleared except by rehashing
             ;; as part of such operation.
             (unless (eq (kv-vector-rehash-epoch kv-vector) initial-epoch)
               (signal-corrupt-hash-table hash-table))
             (let ((key-index (%rehash-and-find hash-table initial-epoch key)))
               ;; If we see NIL here, it means that some other operation is racing
               ;; to rehash. GETHASH can deal with that scenario, PUTHASH can't.
               (cond ((eql key-index 0)) ; fallthrough to insert
                     ((not (fixnump key-index)) (signal-corrupt-hash-table hash-table))
                     (t (return-from done (setf (aref kv-vector (1+ key-index)) value))))))
           ;; Pop a KV slot off the free list
           (insert-at (hash-table-next-free-kv hash-table)
                      hash-table key hash address-based-p value))))))

(defun get-puthash-definitions ()
  (flet ((insert-at (index hash-table key hash address-based-p value)
           (declare (optimize speed) (type index/2 index))
           (when (zerop index)
             (setq index (grow-hash-table hash-table))
             ;; Growing the table can not make the key become found when it was not
             ;; found before, so we can just proceed with insertion.
             (aver (not (zerop index))))
           ;; Grab the vectors AFTER possibly growing the table
           (let ((kv-vector (hash-table-pairs hash-table))
                 (next-vector (hash-table-next-vector hash-table)))
             (setf (hash-table-next-free-kv hash-table)
                   (let ((hwm (kv-vector-high-water-mark kv-vector))
                         (cap (hash-table-pairs-capacity kv-vector)))
                     (cond ((> index hwm) ; raise the high-water-mark
                            (setf (kv-vector-high-water-mark kv-vector) index)
                            ;; CPU must not buffer storing the new HWM versus the
                            ;; stores of the K+V since they wouldn't be seen.
                            (sb-thread:barrier (:write))
                            (cond ((= index cap) 0)
                                  (t (1+ index))))
                           (t ; don't raise
                            (let ((next (aref next-vector index)))
                              (cond ((/= next 0) next)
                                    ((= hwm cap) 0)
                                    (t (1+ hwm))))))))

             ;; We've potentially depended on the bits of the address of KEY
             ;; before informing GC that we've done so, but the key is pinned,
             ;; so as long as the table informs GC that it has the dependency
             ;; by the time the key is free to move, all is well.
             (when address-based-p
               (set-header-bits kv-vector sb-vm:vector-addr-hashing-subtype))

             ;; Store the hash unless an EQ table. Because the key is pinned, it is
             ;; OK that GC would not have seen +magic-hash-vector-value+ for this
             ;; key if applicable.
             (awhen (hash-table-hash-vector hash-table)
               (setf (aref it index)
                     (if address-based-p +magic-hash-vector-value+ hash)))

             ;; Store the pair
             (let ((i (* 2 index)))
               (setf (aref kv-vector i) key (aref kv-vector (1+ i)) value))
             ;; Push this slot onto the front of the chain for its bucket.
             (let* ((index-vector (hash-table-index-vector hash-table))
                    (bucket (mask-hash hash (1- (length index-vector)))))
               (setf (aref next-vector index) (aref index-vector bucket)
                     (aref index-vector bucket) index)))
           (incf (hash-table-%count hash-table))
           value))

  (values (named-lambda puthash/weak (key hash-table value)
            (declare (type hash-table hash-table) (optimize speed))
            (with-weak-hash-table-entry
                (declare (ignore predecessor))
                (cond ((= physical-index 0)
                       ;; There are two kinds of freelists. Prefer a smashed cell
                       ;; so that we might shorten the chain it belonged to.
                       (insert-at (or (hash-table-next-smashed-kv hash-table)
                                      (hash-table-next-free-kv hash-table))
                                  hash-table key hash address-sensitive-p value))
                      ((or (empty-ht-slot-p (cas (svref kv-vector (1+ physical-index))
                                                 probed-value value))
                           (neq (svref kv-vector physical-index) probed-key))
                       (signal-corrupt-hash-table hash-table))
                      (t value))))
          (define-ht-setter puthash/eq eq)
          (define-ht-setter puthash/eql eql)
          (define-ht-setter puthash/equal equal)
          (define-ht-setter puthash/equalp equalp)
          (define-ht-setter puthash/any nil))))

(defun %puthash (key hash-table value)
  (declare (type hash-table hash-table))
  #+hash-table-simulate
  (let ((cell (assoc key (hash-table-%alist hash-table)
                     :test (hash-table-test-fun hash-table))))
    (if cell
        (rplacd cell value)
        (push (cons key value) (hash-table-%alist hash-table))))
  (funcall (hash-table-puthash-impl hash-table) key hash-table value))

(defmacro define-remhash (name std-fn)
  `(named-lambda ,name (key table &aux (hash-table (truly-the hash-table table))
                                       (kv-vector (hash-table-pairs hash-table)))
     (declare (optimize speed (sb-c::verify-arg-count 0)))
     ;; The cache provides no benefit to REMHASH. A hit would just mean there is work
     ;; to do in removing the item from a chain, whereas a miss means we don't know
     ;; if there is work to do, so effectively there is work to do either way.
     (with-pinned-objects (key)
       ;; See comment in DEFINE-HT-SETTER about why to read initial-epoch
       ;; as soon as possible after pinning KEY.
       (binding* ((initial-epoch (kv-vector-rehash-epoch kv-vector))
                  ,@(ht-hash-setup std-fn)
                  ,@(ht-probe-setup std-fn)
                  (eq-test ,(ht-probing-should-use-eq std-fn)))
         (declare (hash hash0) (index/2 index) (ignore probe-limit))
         (block done
           (cond ((zerop index)) ; bucket is empty
                 (,(ht-key-compare std-fn 'index)
                  ;; Removing the key at the header of the chain is exceptional
                  ;; because it has no predecessor,
                  (setf (aref index-vector bucket) (aref next-vector index))
                  (return-from done
                    (clear-slot index hash-table kv-vector next-vector)))
                 (eq-test
                  (when (%remhash/eq key hash-table kv-vector next-vector index)
                    (return-from done t)))
                 ,@(unless (eq std-fn 'eq)
                     `((t
                        (do ((probe-limit (length next-vector))
                             (predecessor index this)
                             (this (aref next-vector index) (aref next-vector this)))
                            ((zerop this) nil)
                          (declare (type index/2 predecessor this))
                          (when ,(ht-key-compare std-fn 'this)
                            (setf (aref next-vector predecessor) (aref next-vector this))
                            (return-from done
                              (clear-slot this hash-table kv-vector next-vector)))
                          (check-excessive-probes 1))))))
           ;; Detect whether the failure to find was due to key movement.
           ;; Only the initial state of the 'rehash' bit is important.
           ;; If the bit changed from 0 to 1, then KEY's hash was good because
           ;; it was pinned at the time we observed the rehash status to be 0.
           (when (and address-based-p (oddp initial-epoch))
             (remove-from-bucket key bucket hash-table initial-epoch)))))))

(defun remhash/weak (key hash-table)
  (declare (type hash-table hash-table) (optimize speed))
  (with-weak-hash-table-entry
        (unless (eql physical-index 0)
          ;; Mark slot as empty.
          (if (or (empty-ht-slot-p (cas (svref kv-vector (1+ physical-index))
                                        probed-value +empty-ht-slot+))
                  (neq (cas (svref kv-vector physical-index)
                            probed-key +empty-ht-slot+)
                       probed-key))
              (signal-corrupt-hash-table hash-table)
              (let* ((index (ash physical-index -1))
                     (next-vector (hash-table-next-vector hash-table))
                     (successor (aref next-vector index)))
                ;; Unlink from bucket's chain
                (if predecessor
                    (setf (aref next-vector predecessor) successor)
                    (let* ((iv (hash-table-index-vector hash-table))
                           (bucket (mask-hash hash (1- (length iv)))))
                      (setf (aref iv bucket) successor)))
                ;; Push onto free chain
                (setf (aref next-vector index) (hash-table-next-free-kv hash-table)
                      (hash-table-next-free-kv hash-table) index)
                (decf (hash-table-%count hash-table))
                t)))))

(defun get-remhash-definitions ()
  (labels ((clear-slot (index hash-table kv-vector next-vector)
             (declare (type index/2 index))
             ;; Mark slot as empty.
             (let ((physindex (* 2 index)))
               (setf (aref kv-vector physindex) +empty-ht-slot+
                     (aref kv-vector (1+ physindex)) +empty-ht-slot+))
             ;; Push KV slot onto free chain.
             ;; Possible optimization: if we linked the free chain through
             ;; the 'value' half of a pair, we could avoid rebuilding the
             ;; free chain in %REHASH because rehashing won't affect it.
             ;; (Maybe it already doesn't?)
             (setf (aref next-vector index) (hash-table-next-free-kv hash-table)
                   (hash-table-next-free-kv hash-table) index)
             ;; On parallel accesses this may turn out to be a
             ;; type-error, so don't turn down the safety!
             (decf (hash-table-%count hash-table))
             t)
           (remove-from-bucket (key bucket hash-table initial-epoch
                                &aux (kv-vector (hash-table-pairs hash-table)))
             ;; Remove KEY from BUCKET which is based on the current address
             ;; of key after pinning.  When the key was not found initially,
             ;; the "problem" so to speak was that the bucket that was searched
             ;; did not hold the key, and not that search was done on the wrong
             ;; bucket. Rehashing will place the key into the expected bucket.
             ;;
             ;; The current epoch must be the same as initial-epoch, because
             ;; REMHASH is disallowed concurrently with any other operation,
             ;; and the 'rehash' bit can't be cleared except by rehashing
             ;; as part of such operation.
             (unless (eq (kv-vector-rehash-epoch kv-vector) initial-epoch)
               (signal-corrupt-hash-table hash-table))
             (let ((key-index (%rehash-and-find hash-table initial-epoch key)))
               ;; If we see NIL here, it means that some other operation is racing
               ;; to rehash. GETHASH can deal with that scenario, REMHASH can't.
               (unless (fixnump key-index)
                 (signal-corrupt-hash-table hash-table))
               (when (/= key-index 0) ; found
                 (let* ((index-vector (hash-table-index-vector hash-table))
                        (start (aref index-vector bucket))
                        (next-vector (hash-table-next-vector hash-table))
                        (next (aref next-vector start))
                        (pair-index (ash key-index -1)))
                   (if (if (= start pair-index) ; remove head of chain
                           (setf (aref index-vector bucket) next)
                           (do ((probe-limit (length next-vector))
                                (predecessor start this)
                                (this next (aref next-vector this)))
                               ((zerop this))
                             (declare (type index/2 predecessor this))
                             (when (eql this pair-index)
                               (return (setf (aref next-vector predecessor)
                                             (aref next-vector this))))
                             (check-excessive-probes 1)))
                       (clear-slot pair-index hash-table kv-vector next-vector)
                       (signal-corrupt-hash-table hash-table))))))
           (%remhash/eq (key hash-table kv-vector next-vector start)
             (do ((probe-limit (length next-vector))
                  (predecessor start this)
                  (this (aref next-vector start) (aref next-vector this)))
                 ((zerop this) nil)
               (declare (type index/2 predecessor this))
               (when (eq key (aref kv-vector (* 2 this)))
                 (setf (aref next-vector predecessor) (aref next-vector this))
                 (return (clear-slot this hash-table kv-vector next-vector)))
               (check-excessive-probes 1))))

    (values (define-remhash remhash/eq eq)
            (define-remhash remhash/eql eql)
            (define-remhash remhash/equal equal)
            (define-remhash remhash/equalp equalp)
            (define-remhash remhash/any nil))))

(defun !cold-init-hash-table-methods ()
  (multiple-value-bind (weak eq eql equal equalp any) (get-puthash-definitions)
    (setf (symbol-function 'puthash/weak)   weak
          (symbol-function 'puthash/eq)     eq
          (symbol-function 'puthash/eql)    eql
          (symbol-function 'puthash/equal)  equal
          (symbol-function 'puthash/equalp) equalp
          (symbol-function 'puthash/any)    any))
  (multiple-value-bind (eq eql equal equalp any) (get-remhash-definitions)
    (setf (symbol-function 'remhash/eq)     eq
          (symbol-function 'remhash/eql)    eql
          (symbol-function 'remhash/equal)  equal
          (symbol-function 'remhash/equalp) equalp
          (symbol-function 'remhash/any)    any)))

(defun remhash (key hash-table)
  "Remove the entry in HASH-TABLE associated with KEY. Return T if
there was such an entry, or NIL if not."
  #+hash-table-simulate
  (let ((cell (assoc key (hash-table-%alist hash-table)
                     :test (hash-table-test-fun hash-table))))
    (when cell
      (setf (hash-table-%alist hash-table) (delq1 cell (hash-table-%alist hash-table)))))
  (funcall (truly-the (sfunction (t t) (values boolean &optional))
                      (hash-table-remhash-impl hash-table))
           key hash-table))

(defun clrhash (hash-table)
  "This removes all the entries from HASH-TABLE and returns the hash
table itself."
  (truly-the (values hash-table &optional)
             (funcall (hash-table-clrhash-impl hash-table) hash-table)))

(defun clrhash-impl (hash-table)
  ;; This used to do nothing at all for tables that has a COUNT of 0,
  ;; but that wasn't quite right, because some steps below pertain to
  ;; getting the initial state back to that of a freshly made table.
  ;; In particular, the need-to-rehash flag should be cleared, and the freelist
  ;; should be reset so that we start to consume k/v cells leftmost first
  ;; instead of whatever random order they were left in.
  ;;
  ;; It is probably not strictly necessary for vectors marked as valid-hashing to
  ;; be initialized with empty-ht-slot, because GC regards the portion of the vector
  ;; beyond the HWM as unused. Though for weak tables, some invariants can
  ;; be checked by having the empty  markers but couldn't be without.
  ;; [Reusing those elements would be a two-step process: set them to 0,
  ;; bump the HWM, set them to desired values - because you can't let GC
  ;; observe junk, but you can't put good value at higher than the HWM]
  #+hash-table-simulate (setf (hash-table-%alist hash-table) nil)
  (when (plusp (kv-vector-high-water-mark (hash-table-pairs hash-table)))
    (sb-thread::with-recursive-system-lock ((hash-table-lock hash-table))
      (let* ((kv-vector (hash-table-pairs hash-table))
             (high-water-mark (kv-vector-high-water-mark kv-vector)))
        (when (hash-table-weak-p hash-table)
          (aver (eq (kv-vector-table kv-vector) hash-table)))
        ;; Remove address-sensitivity.
        (unset-header-bits kv-vector sb-vm:vector-addr-hashing-subtype)
        ;; Do this only after unsetting the address-sensitive bit,
        ;; otherwise GC might come along and touch this bit again.
        (setf (kv-vector-needs-rehash kv-vector) 0)
        ;; We always deposit empty makers into k/v pairs that are REMHASHed,
        ;; so a count of 0 implies no clearing need be done.
        (when (plusp (hash-table-%count hash-table))
          (setf (hash-table-%count hash-table) 0)
          ;; Fill all slots with the empty marker.
          (fill kv-vector +empty-ht-slot+ :start 2 :end (* (1+ high-water-mark) 2))
          ;; Clear the index-vector.
          ;; Don't need to clear the hash-vector or the next-vector.
          (fill (hash-table-index-vector hash-table) 0))
        (setf (hash-table-smashed-cells hash-table) nil
              (hash-table-next-free-kv hash-table) 1
              (kv-vector-high-water-mark kv-vector) 0))))
  hash-table)


;;;; methods on HASH-TABLE

;;; Return a list of keyword args and values to use for MAKE-HASH-TABLE
;;; when reconstructing HASH-TABLE.
;;; FIXME: synchronized? custom function?
(defun %hash-table-ctor-args (hash-table)
  `(:test             ',(hash-table-test             hash-table)
    :size             ',(hash-table-size             hash-table)
    :rehash-size      ',(hash-table-rehash-size      hash-table)
    :rehash-threshold ',(hash-table-rehash-threshold hash-table)
    :weakness         ',(hash-table-weakness         hash-table)))

;;; Stuff an association list into HASH-TABLE. Return the hash table,
;;; so that we can use this for the *PRINT-READABLY* case in
;;; PRINT-OBJECT (HASH-TABLE T) without having to worry about LET
;;; forms and readable gensyms and stuff.
(defun %stuff-hash-table (hash-table alist)
  (dolist (x alist)
    (setf (gethash (car x) hash-table) (cdr x)))
  hash-table)

(defmethod print-object ((hash-table hash-table) stream)
  (declare (type stream stream))
  (cond ((or (not *print-readably*) (not *read-eval*))
         (print-unreadable-object (hash-table stream :type t :identity t)
           (format stream
                   ":TEST ~S~@[ :HASH-FUNCTION ~S~] :COUNT ~S~@[ :WEAKNESS ~S~]"
                   (hash-table-test hash-table)
                   (when (and (eq (hash-table-test hash-table) 'eq)
                              (neq (hash-table-hash-fun hash-table) #'eq-hash))
                     (hash-table-hash-fun hash-table))
                   (hash-table-count hash-table)
                   (hash-table-weakness hash-table))))
        (t
         (write-string "#." stream)
         (write `(%stuff-hash-table (make-hash-table ,@(%hash-table-ctor-args
                                                          hash-table))
                                     ',(%hash-table-alist hash-table))
                :stream stream))))

(defmethod make-load-form ((hash-table hash-table) &optional environment)
  (declare (ignore environment))
  (values `(make-hash-table ,@(%hash-table-ctor-args hash-table))
          `(%stuff-hash-table ,hash-table ',(%hash-table-alist hash-table))))

#|
(defun show-address-sensitivity (&optional tbl)
  (flet ((show1 (tbl)
           (let ((kv (hash-table-pairs tbl))
                 (hashes (hash-table-hash-vector tbl))
                 (address-sensitive 0))
             (loop for i from 2 below (length kv) by 2
                   do
                (unless (unbound-marker-p (aref kv i))
                  (when (eq (aref hashes (ash i -1)) +magic-hash-vector-value+)
                    (incf address-sensitive))))
             (when (plusp address-sensitive)
               (format t "~3d ~s~%" address-sensitive tbl)))))
    (if tbl
        (show1 tbl)
        (dolist (tbl (sb-vm::list-allocated-objects :all :test #'hash-table-p))
          (when (and (plusp (hash-table-count tbl)) (hash-table-hash-vector tbl))
            (show-address-sensitivity tbl))))))

(defun hash-table-chain (tbl pair-index)
  (when (plusp pair-index)
    (nconc (list pair-index)
           (hash-table-chain tbl (aref (hash-table-next-vector tbl) pair-index)))))

(defun hash-table-freelist (tbl)
  (hash-table-chain tbl (hash-table-next-free-kv tbl)))

(defun show-chains (tbl &aux (nv (hash-table-next-vector tbl))
                        (tot-len 0) (max-len 0) (n-chains 0))
  (flet ((show-chain (label next &aux (len 0))
           (unless (eql next 0)
             (write-string label)
             (loop (format t " ~d" next)
                   (incf len)
                   (when (zerop (setq next (aref nv next))) (return)))
             (terpri))
           len))
    (loop for x across (hash-table-index-vector tbl)
          for i from 0
          do (let ((len (show-chain (format nil "Bucket ~d:" i) x)))
               (when (plusp len)
                 (incf tot-len len)
                 (setf max-len (max len max-len))
                 (incf n-chains))))
    (format t "maxlen=~d avglen=~f~%" max-len (/ tot-len n-chains))
    (show-chain "Freelist:" (hash-table-next-free-kv tbl))
    (awhen (hash-table-smashed-cells tbl)
      (format t "smashed=~d~%"
              (mapcar (lambda (x)
                        (if (fixnump x)
                            (cons (ldb (byte (14 14) x)) (ldb (byte (14 0) x)))
                            x))
                      it)))))

(defun show-load-factors ()
  (let ((list (sort (sb-vm::list-allocated-objects
                     :all :test #'hash-table-p)
                    #'>
                    :key #'hash-table-count)))
    (dolist (ht list)
      (format t "~,4f ~7d ~s~%"
              (/ (hash-table-count ht) (length (hash-table-index-vector ht)))
              (hash-table-mem-used ht)
              ht))))

(defun hash-table-mem-used (ht)
  (+ (sb-vm::primitive-object-size (hash-table-pairs ht))
     (sb-vm::primitive-object-size (hash-table-index-vector ht))
     (sb-vm::primitive-object-size (hash-table-next-vector ht))
     (acond ((hash-table-hash-vector ht) (sb-vm::primitive-object-size it))
            (t 0))))

(defun show-growth (&optional (factor 1.5))
  (flet ((memused-str (x)
           (cond ((>= x (expt 1024 4)) (format nil "~dG" (ceiling (/ x (expt 1024 3)))))
                 ((>= x (expt 1024 3)) (format nil "~dM" (ceiling (/ x (expt 1024 2)))))
                 ((>= x (expt 1024 2)) (format nil "~dK" (ceiling (/ x 1024))))
                 (t x)))
         (compute-memused (n-buckets n-cells) ; approximately right,
           ;; disregarding overhead cells and padding words
           (+ (* n-cells (* 2 sb-vm:n-word-bytes)) ; 2 words per cell
              (* n-buckets  4)   ; INDEX-VECTOR = one 32-bit int per bucket
              (* n-cells 4)   ; NEXT-VECTOR  = one 32-bit int per cell
              (* n-cells 4))) ; HASH-VECTOR  = one 32-bit int per cell
         (log2-buckets (n-buckets)
           (integer-length (1- n-buckets))))
  (let* ((size 14)
         (n-buckets (power-of-two-ceiling size))
         (memused (compute-memused n-buckets size)))
    (format t "        size   bits        LF           mem~%")
    (format t "~12d ~6d ~9,4f ~13d~%" size
            (log2-buckets n-buckets) (/ size n-buckets) memused)
    (loop
      (let* ((new-size (truncate (* size factor)))
             (new-n-buckets (power-of-two-ceiling new-size))
             (full-LF (/ new-size new-n-buckets)))
        (when (> (log2-buckets new-n-buckets) 31) (return))
        ;; try to keep the load factor at full load within a certain range
        (cond ((> full-lf .9)
               (setq new-size (floor (* new-n-buckets 85/100))))
              ((< full-lf .55)
               (setq new-size (floor (* new-n-buckets 65/100)))))
        (let ((new-memused (compute-memused new-n-buckets new-size)))
          (format t "~12d ~6d ~9,4f ~13@a  (* ~f)~%"
                  new-size (log2-buckets new-n-buckets)
                  (/ new-size new-n-buckets)
                  (memused-str new-memused) (/ new-memused memused))
          (setq size new-size memused new-memused)))))))

;;; You can't attain an arbitrary rehash size using power-of-2 tables
;;; because all you can do is double the number of buckets.
;;; However, by doubling the count only on alternate resizings, we can
;;; approximate growing at rate slower than doubling in terms of space used.
;;; This table shows that using the default resize factor of 1.5x we see
;;; a fairly smooth increase in memory consumed at each resizing
;;; (about 1.3x to 1.6x the usage at the old size) while bounding the LF
;;; to between .6 and .85 if every k/v cell is in use at the new size.

        size   bits        LF           mem
          14      4    0.8750           400
          21      5    0.6563           632  (* 1.58)
          27      5    0.8438           776  (* 1.227848)
          40      6    0.6250          1216  (* 1.5670103)
          54      6    0.8438          1552  (* 1.2763158)
          81      7    0.6328          2456  (* 1.5824742)
         108      7    0.8438          3104  (* 1.2638437)
         162      8    0.6328          4912  (* 1.5824742)
         217      8    0.8477          6232  (* 1.2687297)
         325      9    0.6348          9848  (* 1.5802311)
         435      9    0.8496         12488  (* 1.2680748)
         652     10    0.6367         19744  (* 1.5810378)
         870     10    0.8496         24976  (* 1.2649919)
        1305     11    0.6372         39512  (* 1.5819987)
        1740     11    0.8496         49952  (* 1.2642236)
        2610     12    0.6372         79024  (* 1.5819987)
        3481     12    0.8499         99928  (* 1.2645272)
        5221     13    0.6373        158072  (* 1.581859)
        6963     13    0.8500        199880  (* 1.264487)
       10444     14    0.6375        316192  (* 1.5819092)
       13926     14    0.8500        399760  (* 1.2642951)
       20889     15    0.6375        632408  (* 1.5819691)
       27852     15    0.8500        799520  (* 1.2642472)
       41778     16    0.6375         1236K  (* 1.5819691)
       55705     16    0.8500         1562K  (* 1.2642661)
       83557     17    0.6375         2471K  (* 1.5819604)
      111411     17    0.8500         3124K  (* 1.2642636)
      167116     18    0.6375         4941K  (* 1.5819635)
      222822     18    0.8500         6247K  (* 1.2642516)
      334233     19    0.6375         9882K  (* 1.5819674)
      445644     19    0.8500        12493K  (* 1.2642486)
      668466     20    0.6375        19764K  (* 1.5819674)
      891289     20    0.8500        24986K  (* 1.2642498)
     1336933     21    0.6375        39527K  (* 1.5819668)
     1782579     21    0.8500        49972K  (* 1.2642497)
     2673868     22    0.6375        79053K  (* 1.581967)
     3565158     22    0.8500        99943K  (* 1.2642488)
     5347737     23    0.6375       158106K  (* 1.5819672)
     7130316     23    0.8500       199885K  (* 1.2642487)
    10695474     24    0.6375       316212K  (* 1.5819672)
    14260633     24    0.8500       399770K  (* 1.2642487)
    21390950     25    0.6375       632423K  (* 1.5819672)
    28521267     25    0.8500       799540K  (* 1.2642487)
    42781904     26    0.6375         1236M  (* 1.5819674)
    57042534     26    0.8500         1562M  (* 1.2642486)
    85563808     27    0.6375         2471M  (* 1.5819674)
   114085068     27    0.8500         3124M  (* 1.2642486)
   171127616     28    0.6375         4941M  (* 1.5819674)
   228170137     28    0.8500         6247M  (* 1.2642486)
   342255232     29    0.6375         9882M  (* 1.5819674)
   456340275     29    0.8500        12493M  (* 1.2642486)
   684510464     30    0.6375        19764M  (* 1.5819674)
   912680550     30    0.8500        24986M  (* 1.2642486)
  1369020928     31    0.6375        39527M  (* 1.5819674)
  1825361100     31    0.8500        49972M  (* 1.2642486)
|#
