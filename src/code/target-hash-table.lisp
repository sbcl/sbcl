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

(in-package "SB!IMPL")

;;;; utilities

;;; Code for detecting concurrent accesses to the same table from
;;; multiple threads. Only compiled in when the :SB-HASH-TABLE-DEBUG
;;; feature is enabled. The main reason for the existence of this code
;;; is to detect thread-unsafe uses of hash-tables in sbcl itself,
;;; where debugging anythign can be impossible after an important
;;; internal hash-table has been corrupted. It's plausible that this
;;; could be useful for some user code too, but the runtime cost is
;;; really too high to enable it by default.
(defmacro with-concurrent-access-check (hash-table operation &body body)
  (declare (ignorable hash-table operation)
           (type (member :read :write) operation))
  #!-sb-hash-table-debug
  `(progn ,@body)
  #!+sb-hash-table-debug
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
                            sb!thread::*current-thread*)
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
                                        sb!thread::*current-thread*))))
                   (error-fun))
                 (when (eq (,thread-slot-accessor ,hash-table)
                           sb!thread::*current-thread*)
                   ;; this is not 100% correct here and may hide
                   ;; concurrent access in rare circumstances.
                   (setf (,thread-slot-accessor ,hash-table) nil)))
               (body-fun)))))))

#!-sb-fluid (declaim (inline eq-hash))
(defun eq-hash (key)
  (declare (values hash (member t nil)))
  (values (pointer-hash key)
          (oddp (get-lisp-obj-address key))))

#!-sb-fluid (declaim (inline equal-hash))
(defun equal-hash (key)
  (declare (values hash (member t nil)))
  (typecase key
    ;; For some types the definition of EQUAL implies a special hash
    ((or string cons number bit-vector pathname)
     (values (sxhash key) nil))
    ;; Otherwise use an EQ hash, rather than SXHASH, since the values
    ;; of SXHASH will be extremely badly distributed due to the
    ;; requirements of the spec fitting badly with our implementation
    ;; strategy.
    (t
     (eq-hash key))))

#!-sb-fluid (declaim (inline eql-hash))
(defun eql-hash (key)
  (declare (values hash (member t nil)))
  (if (numberp key)
      (equal-hash key)
      (eq-hash key)))

(defun equalp-hash (key)
  (declare (values hash (member t nil)))
  (typecase key
    ;; Types requiring special treatment. Note that PATHNAME and
    ;; HASH-TABLE are caught by the STRUCTURE-OBJECT test.
    ((or array cons number character structure-object)
     (values (psxhash key) nil))
    (t
     (eq-hash key))))

(declaim (inline index-for-hashing))
(defun index-for-hashing (hash length)
  (declare (type hash hash length))
  ;; We're using power of two tables which obviously are very
  ;; sensitive to the exact values of the low bits in the hash
  ;; value. Do a little shuffling of the value to mix the high bits in
  ;; there too.
  (truly-the index
             (logand (1- length)
                     (+ (logxor #b11100101010001011010100111
                                hash)
                        (ash hash -3)
                        (ash hash -12)
                        (ash hash -20)))))


;;;; user-defined hash table tests

(defvar *user-hash-table-tests* nil)

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
  #!+sb-doc
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

(defconstant +min-hash-table-size+ 16)
(defconstant +min-hash-table-rehash-threshold+ (float 1/16 1.0))

(defun make-hash-table (&key
                        (test 'eql)
                        (size +min-hash-table-size+)
                        (rehash-size 1.5)
                        (rehash-threshold 1)
                        (hash-function nil)
                        (weakness nil)
                        (synchronized))
  #!+sb-doc
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
  (multiple-value-bind (test test-fun hash-fun)
      (cond ((or (eq test #'eq) (eq test 'eq))
             (values 'eq #'eq #'eq-hash))
            ((or (eq test #'eql) (eq test 'eql))
             (values 'eql #'eql #'eql-hash))
            ((or (eq test #'equal) (eq test 'equal))
             (values 'equal #'equal #'equal-hash))
            ((or (eq test #'equalp) (eq test 'equalp))
             (values 'equalp #'equalp #'equalp-hash))
            (t
             ;; FIXME: It would be nice to have a compiler-macro
             ;; that resolved this at compile time: we could grab
             ;; the alist cell in a LOAD-TIME-VALUE, etc.
             (dolist (info *user-hash-table-tests*
                      (if hash-function
                          (if (functionp test)
                              (values (%fun-name test) test nil)
                              (values test (%coerce-callable-to-fun test) nil))
                       (error "Unknown :TEST for MAKE-HASH-TABLE: ~S"
                              test)))
               (destructuring-bind (test-name test-fun hash-fun) info
                 (when (or (eq test test-name) (eq test test-fun))
                   (return (values test-name test-fun hash-fun)))))))
    (when hash-function
      (setf hash-fun
            ;; Quickly check if the function has return return type which
            ;; guarantees that the secondary return value is always NIL:
            ;; (VALUES * &OPTIONAL), (VALUES * NULL ...) or (VALUES *
            ;; &OPTIONAL NULL ...)
            (let* ((actual (%coerce-callable-to-fun hash-function))
                   (type-spec (%fun-type actual))
                   (return-spec (when (consp type-spec)
                                  (caddr type-spec)))
                   (extra-vals (when (consp return-spec)
                                 (cddr return-spec))))
              (if (and (consp extra-vals)
                       (or (eq 'null (car extra-vals))
                           (and (eq '&optional (car extra-vals))
                                (or (not (cdr extra-vals))
                                    (eq 'null (cadr extra-vals))))))
                  actual
                  ;; If there is a potential secondary value, make sure we
                  ;; don't accidentally claim EQ based hashing...
                  (lambda (object)
                    (declare (optimize (safety 0) (speed 3)))
                    (values (funcall actual object) nil))))))
    (let* ((size (max +min-hash-table-size+
                      (min size
                           ;; SIZE is just a hint, so if the user asks
                           ;; for a SIZE which'd be too big for us to
                           ;; easily implement, we bump it down.
                           (floor array-dimension-limit 1024))))
           (rehash-size (if (integerp rehash-size)
                            rehash-size
                            (float rehash-size 1.0)))
           ;; FIXME: Original REHASH-THRESHOLD default should be 1.0,
           ;; not 1, to make it easier for the compiler to avoid
           ;; boxing.
           (rehash-threshold (max +min-hash-table-rehash-threshold+
                                  (float rehash-threshold 1.0)))
           (size+1 (1+ size))       ; The first element is not usable.
           ;; KLUDGE: The most natural way of expressing the below is
           ;; (round (/ (float size+1) rehash-threshold)), and indeed
           ;; it was expressed like that until 0.7.0. However,
           ;; MAKE-HASH-TABLE is called very early in cold-init, and
           ;; the SPARC has no primitive instructions for rounding,
           ;; but only for truncating; therefore, we fudge this issue
           ;; a little. The other uses of truncate, below, similarly
           ;; used to be round. -- CSR, 2002-10-01
           ;;
           ;; Note that this has not yet been audited for
           ;; correctness. It just seems to work. -- CSR, 2002-11-02
           (scaled-size (truncate (/ (float size+1) rehash-threshold)))
           (length (power-of-two-ceiling (max scaled-size
                                              (1+ +min-hash-table-size+))))
           (index-vector (make-array length
                                     :element-type
                                     '(unsigned-byte #.sb!vm:n-word-bits)
                                     :initial-element 0))
           ;; Needs to be the half the length of the KV vector to link
           ;; KV entries - mapped to indeces at 2i and 2i+1 -
           ;; together.
           (next-vector (make-array size+1
                                    :element-type
                                    '(unsigned-byte #.sb!vm:n-word-bits)))
           (kv-vector (make-array (* 2 size+1)
                                  :initial-element +empty-ht-slot+))
           (table (%make-hash-table
                   test
                   test-fun
                   hash-fun
                   rehash-size
                   rehash-threshold
                   size
                   kv-vector
                   weakness
                   index-vector
                   next-vector
                   (unless (eq test 'eq)
                     (make-array size+1
                                 :element-type '(unsigned-byte
                                                 #.sb!vm:n-word-bits)
                                 :initial-element +magic-hash-vector-value+))
                   synchronized)))
      (declare (type index size+1 scaled-size length))
      ;; Set up the free list, all free. These lists are 0 terminated.
      (do ((i 1 (1+ i)))
          ((>= i size))
        (setf (aref next-vector i) (1+ i)))
      (setf (aref next-vector size) 0)
      (setf (hash-table-next-free-kv table) 1)
      (setf (aref kv-vector 0) table)
      table)))

(defun hash-table-count (hash-table)
  #!+sb-doc
  "Return the number of entries in the given HASH-TABLE."
  (declare (type hash-table hash-table)
           (values index))
  (hash-table-number-entries hash-table))

#!+sb-doc
(setf (fdocumentation 'hash-table-rehash-size 'function)
      "Return the rehash-size HASH-TABLE was created with.")

#!+sb-doc
(setf (fdocumentation 'hash-table-rehash-threshold 'function)
      "Return the rehash-threshold HASH-TABLE was created with.")

#!+sb-doc
(setf (fdocumentation 'hash-table-synchronized-p 'function)
      "Returns T if HASH-TABLE is synchronized.")

(defun hash-table-size (hash-table)
  #!+sb-doc
  "Return a size that can be used with MAKE-HASH-TABLE to create a hash
   table that can hold however many entries HASH-TABLE can hold without
   having to be grown."
  (hash-table-rehash-trigger hash-table))

#!+sb-doc
(setf (fdocumentation 'hash-table-test 'function)
      "Return the test HASH-TABLE was created with.")

#!+sb-doc
(setf (fdocumentation 'hash-table-weakness 'function)
      "Return the WEAKNESS of HASH-TABLE which is one of NIL, :KEY,
:VALUE, :KEY-AND-VALUE, :KEY-OR-VALUE.")

;;; Called when we detect circular chains in a hash-table.
(defun signal-corrupt-hash-table (hash-table)
  (error "Corrupt NEXT-chain in ~A. This is probably caused by ~
multiple threads accessing the same hash-table without locking."
         hash-table))


;;;; accessing functions

;;; Make new vectors for the table, extending the table based on the
;;; rehash-size.
(defun rehash (table)
  (declare (type hash-table table))
  (aver *gc-inhibit*)
  (let* ((old-kv-vector (hash-table-table table))
         (old-next-vector (hash-table-next-vector table))
         (old-hash-vector (hash-table-hash-vector table))
         (old-size (length old-next-vector))
         (new-size
          (power-of-two-ceiling
           (let ((rehash-size (hash-table-rehash-size table)))
             (etypecase rehash-size
               (fixnum
                (+ rehash-size old-size))
               (float
                (the index (truncate (* rehash-size old-size))))))))
         (new-kv-vector (make-array (* 2 new-size)
                                    :initial-element +empty-ht-slot+))
         (new-next-vector
          (make-array new-size
                      :element-type '(unsigned-byte #.sb!vm:n-word-bits)
                      :initial-element 0))
         (new-hash-vector
          (when old-hash-vector
            (make-array new-size
                        :element-type '(unsigned-byte #.sb!vm:n-word-bits)
                        :initial-element +magic-hash-vector-value+)))
         (new-length new-size)
         (new-index-vector
          (make-array new-length
                      :element-type '(unsigned-byte #.sb!vm:n-word-bits)
                      :initial-element 0)))
    (declare (type index new-size new-length old-size))

    ;; Disable GC tricks on the OLD-KV-VECTOR.
    (set-header-data old-kv-vector sb!vm:vector-normal-subtype)

    ;; Non-empty weak hash tables always need GC support.
    (when (and (hash-table-weakness table) (plusp (hash-table-count table)))
      (set-header-data new-kv-vector sb!vm:vector-valid-hashing-subtype))

    ;; FIXME: here and in several other places in the hash table code,
    ;; loops like this one are used when FILL or REPLACE would be
    ;; appropriate.  why are standard CL functions not used?
    ;; Performance issues?  General laziness?  -- NJF, 2004-03-10

    ;; Copy over the kv-vector. The element positions should not move
    ;; in case there are active scans.
    (dotimes (i (* old-size 2))
      (declare (type index i))
      (setf (aref new-kv-vector i) (aref old-kv-vector i)))

    ;; Copy over the hash-vector.
    (when old-hash-vector
      (dotimes (i old-size)
        (setf (aref new-hash-vector i) (aref old-hash-vector i))))

    (setf (hash-table-next-free-kv table) 0)
    ;; Rehash all the entries; last to first so that after the pushes
    ;; the chains are first to last.
    (do ((i (1- new-size) (1- i)))
        ((zerop i))
      (declare (type index/2 i))
      (let ((key (aref new-kv-vector (* 2 i)))
            (value (aref new-kv-vector (1+ (* 2 i)))))
        (cond ((and (eq key +empty-ht-slot+)
                    (eq value +empty-ht-slot+))
               ;; Slot is empty, push it onto the free list.
               (setf (aref new-next-vector i)
                     (hash-table-next-free-kv table))
               (setf (hash-table-next-free-kv table) i))
              ((and new-hash-vector
                    (not (= (aref new-hash-vector i)
                            +magic-hash-vector-value+)))
               ;; Can use the existing hash value (not EQ based)
               (let* ((hashing (aref new-hash-vector i))
                      (index (index-for-hashing hashing new-length))
                      (next (aref new-index-vector index)))
                 (declare (type index index)
                          (type hash hashing))
                 ;; Push this slot into the next chain.
                 (setf (aref new-next-vector i) next)
                 (setf (aref new-index-vector index) i)))
              (t
               ;; EQ base hash.
               ;; Enable GC tricks.
               (set-header-data new-kv-vector
                                sb!vm:vector-valid-hashing-subtype)
               (let* ((hashing (pointer-hash key))
                      (index (index-for-hashing hashing new-length))
                      (next (aref new-index-vector index)))
                 (declare (type index index)
                          (type hash hashing))
                 ;; Push this slot onto the next chain.
                 (setf (aref new-next-vector i) next)
                 (setf (aref new-index-vector index) i))))))
    (setf (hash-table-table table) new-kv-vector)
    (setf (hash-table-index-vector table) new-index-vector)
    (setf (hash-table-next-vector table) new-next-vector)
    (setf (hash-table-hash-vector table) new-hash-vector)
    ;; Fill the old kv-vector with 0 to help the conservative GC. Even
    ;; if nothing else were zeroed, it's important to clear the
    ;; special first cells in old-kv-vector.
    (fill old-kv-vector 0)
    (setf (hash-table-rehash-trigger table) new-size)
    (setf (hash-table-needs-rehash-p table) nil))
  (values))

;;; Use the same size as before, re-using the vectors.
(defun rehash-without-growing (table)
  (declare (type hash-table table))
  (aver *gc-inhibit*)
  (let* ((kv-vector (hash-table-table table))
         (next-vector (hash-table-next-vector table))
         (hash-vector (hash-table-hash-vector table))
         (size (length next-vector))
         (index-vector (hash-table-index-vector table))
         (length (length index-vector)))
    (declare (type index size length))

    ;; Non-empty weak hash tables always need GC support.
    (unless (and (hash-table-weakness table) (plusp (hash-table-count table)))
      ;; Disable GC tricks, they will be re-enabled during the re-hash
      ;; if necessary.
      (set-header-data kv-vector sb!vm:vector-normal-subtype))

    ;; Rehash all the entries.
    (setf (hash-table-next-free-kv table) 0)
    (dotimes (i size)
      (setf (aref next-vector i) 0))
    (dotimes (i length)
      (setf (aref index-vector i) 0))
    (do ((i (1- size) (1- i)))
        ((zerop i))
      (declare (type index/2 i))
      (let ((key (aref kv-vector (* 2 i)))
            (value (aref kv-vector (1+ (* 2 i)))))
        (cond ((and (eq key +empty-ht-slot+)
                    (eq value +empty-ht-slot+))
               ;; Slot is empty, push it onto free list.
               (setf (aref next-vector i) (hash-table-next-free-kv table))
               (setf (hash-table-next-free-kv table) i))
              ((and hash-vector (not (= (aref hash-vector i)
                                        +magic-hash-vector-value+)))
               ;; Can use the existing hash value (not EQ based)
               (let* ((hashing (aref hash-vector i))
                      (index (index-for-hashing hashing length))
                      (next (aref index-vector index)))
                 (declare (type index index))
                 ;; Push this slot into the next chain.
                 (setf (aref next-vector i) next)
                 (setf (aref index-vector index) i)))
              (t
               ;; EQ base hash.
               ;; Enable GC tricks.
               (set-header-data kv-vector sb!vm:vector-valid-hashing-subtype)
               (let* ((hashing (pointer-hash key))
                      (index (index-for-hashing hashing length))
                      (next (aref index-vector index)))
                 (declare (type index index)
                          (type hash hashing))
                 ;; Push this slot into the next chain.
                 (setf (aref next-vector i) next)
                 (setf (aref index-vector index) i)))))))
  ;; Clear the rehash bit only at the very end, otherwise another thread
  ;; might see a partially rehashed table as a normal one.
  (setf (hash-table-needs-rehash-p table) nil)
  (values))

(declaim (inline maybe-rehash))
(defun maybe-rehash (hash-table ensure-free-slot-p)
  (when (hash-table-weakness hash-table)
    (aver *gc-inhibit*))
  (flet ((rehash-p ()
           (and ensure-free-slot-p
                (zerop (hash-table-next-free-kv hash-table))))
         (rehash-without-growing-p ()
           (hash-table-needs-rehash-p hash-table)))
    (declare (inline rehash-p rehash-without-growing-p))
    (cond ((rehash-p)
           ;; Use recursive locks since for weak tables the lock has
           ;; already been acquired. GC must be inhibited to prevent
           ;; the GC from seeing a rehash in progress.
           (sb!thread::with-recursive-system-lock
               ((hash-table-lock hash-table) :without-gcing t)
             ;; Repeat the condition inside the lock to ensure that if
             ;; two reader threads enter MAYBE-REHASH at the same time
             ;; only one rehash is performed.
             (when (rehash-p)
               (rehash hash-table))))
          ((rehash-without-growing-p)
           (sb!thread::with-recursive-system-lock
               ((hash-table-lock hash-table) :without-gcing t)
             (when (rehash-without-growing-p)
               (rehash-without-growing hash-table)))))))

(declaim (inline update-hash-table-cache))
(defun update-hash-table-cache (hash-table index)
  (unless (hash-table-weakness hash-table)
    (setf (hash-table-cache hash-table) index)))

(defmacro with-hash-table-locks ((hash-table
                                  &key (operation :write) inline pin
                                  (synchronized `(hash-table-synchronized-p ,hash-table)))
                                 &body body)
  (declare (type (member :read :write) operation))
  (with-unique-names (body-fun)
    `(flet ((,body-fun ()
              (with-concurrent-access-check ,hash-table ,operation
                (locally (declare (inline ,@inline))
                  ,@body))))
       (if (hash-table-weakness ,hash-table)
           (sb!thread::with-recursive-system-lock
               ((hash-table-lock ,hash-table) :without-gcing t)
             (,body-fun))
           (with-pinned-objects ,pin
             (if ,synchronized
                 ;; We use a "system" lock here because it is very
                 ;; slightly faster, as it doesn't re-enable
                 ;; interrupts.
                 (sb!thread::with-recursive-system-lock
                     ((hash-table-lock ,hash-table))
                   (,body-fun))
                 (,body-fun)))))))

(defun gethash (key hash-table &optional default)
  #!+sb-doc
  "Finds the entry in HASH-TABLE whose key is KEY and returns the
associated value and T as multiple values, or returns DEFAULT and NIL
if there is no such entry. Entries can be added using SETF."
  (declare (type hash-table hash-table)
           (values t (member t nil)))
  (gethash3 key hash-table default))

(declaim (maybe-inline %gethash3))
(defun %gethash3 (key hash-table default)
  (declare (type hash-table hash-table)
           (optimize speed)
           (values t (member t nil)))
  (tagbody
   start
     (let ((start-epoch sb!kernel::*gc-epoch*))
       (macrolet ((result (value foundp)
                    ;; When the table has multiple concurrent readers,
                    ;; it's possible that there was a GC after this
                    ;; thread called MAYBE-REHASH from %GETHASH3, and
                    ;; some other thread then rehashed the table. If
                    ;; this happens, we might not find the key even if
                    ;; it's in the table. To protect against this,
                    ;; redo the lookup if the GC epoch counter has changed.
                    ;; -- JES,  2007-09-30
                    `(if (and (not ,foundp)
                              (not (eq start-epoch sb!kernel::*gc-epoch*)))
                         (go start)
                         (return-from %gethash3 (values ,value ,foundp))))
                  (overflow ()
                    ;; The next-vector chain is circular. This is caused
                    ;; caused by thread-unsafe mutations of the table.
                    `(signal-corrupt-hash-table hash-table)))
         (maybe-rehash hash-table nil)
         ;; Note that it's OK for a GC + a REHASH-WITHOUT-GROWING to
         ;; be triggered by another thread after this point, since the
         ;; GC epoch check will catch it.
         (let ((cache (hash-table-cache hash-table))
               (table (hash-table-table hash-table)))
           ;; First check the cache.  Use EQ here for speed.
           (if (and cache
                    (< cache (length table))
                    (eq (aref table cache) key))
               (let ((value (aref table (1+ cache))))
                 (result value t))
               ;; Search for key in the hash table.
               (multiple-value-bind (hashing eq-based)
                   (funcall (hash-table-hash-fun hash-table) key)
                 (declare (type hash hashing))
                 (let* ((index-vector (hash-table-index-vector hash-table))
                        (length (length index-vector))
                        (index (index-for-hashing hashing length))
                        (next (aref index-vector index))
                        (next-vector (hash-table-next-vector hash-table))
                        (hash-vector (hash-table-hash-vector hash-table))
                        (test-fun (hash-table-test-fun hash-table)))
                   (declare (type index index))
                   ;; Search next-vector chain for a matching key.
                   (if (or eq-based (not hash-vector))
                       (do ((next next (aref next-vector next))
                            (i 0 (1+ i)))
                           ((zerop next) (result default nil))
                         (declare (type index/2 next i))
                         (when (> i length)
                           (overflow))
                         (when (eq key (aref table (* 2 next)))
                           (update-hash-table-cache hash-table (* 2 next))
                           (let ((value (aref table (1+ (* 2 next)))))
                             (result value t))))
                       (do ((next next (aref next-vector next))
                            (i 0 (1+ i)))
                           ((zerop next) (result default nil))
                         (declare (type index/2 next i))
                         (when (> i length)
                           (overflow))
                         (when (and (= hashing (aref hash-vector next))
                                    (funcall test-fun key
                                             (aref table (* 2 next))))
                           ;; Found.
                           (update-hash-table-cache hash-table (* 2 next))
                           (let ((value (aref table (1+ (* 2 next)))))
                             (result value t)))))))))))))
;;; Three argument version of GETHASH
(defun gethash3 (key hash-table default)
  (declare (type hash-table hash-table))
  (with-hash-table-locks (hash-table :operation :read :inline (%gethash3)
                                     :pin (key))
    (%gethash3 key hash-table default)))

;;; so people can call #'(SETF GETHASH)
;;; FIXME: this function is not mandated. Why do we have it?
(defun (setf gethash) (new-value key table &optional default)
  (declare (ignore default))
  (%puthash key table new-value))

(declaim (maybe-inline %%puthash))
(defun %%puthash (key hash-table value)
  (declare (optimize speed))
  ;; We need to rehash here so that a current key can be found if it
  ;; exists. Check that there is room for one more entry. May not be
  ;; needed if the key is already present.
  (maybe-rehash hash-table t)
  ;; Search for key in the hash table.
  (multiple-value-bind (hashing eq-based)
      (funcall (hash-table-hash-fun hash-table) key)
    (declare (type hash hashing))
    (let* ((index-vector (hash-table-index-vector hash-table))
           (length (length index-vector))
           (index (index-for-hashing hashing length))
           (next (aref index-vector index))
           (kv-vector (hash-table-table hash-table))
           (next-vector (hash-table-next-vector hash-table))
           (hash-vector (hash-table-hash-vector hash-table))
           (test-fun (hash-table-test-fun hash-table)))
      (declare (type index index next))
      (when (hash-table-weakness hash-table)
        (set-header-data kv-vector sb!vm:vector-valid-hashing-subtype))
      (cond ((or eq-based (not hash-vector))
             (when eq-based
               (set-header-data kv-vector
                                sb!vm:vector-valid-hashing-subtype))
             ;; Search next-vector chain for a matching key.
             (do ((next next (aref next-vector next))
                  (i 0 (1+ i)))
                 ((zerop next))
               (declare (type index/2 next i))
               (when (> i length)
                 (signal-corrupt-hash-table hash-table))
               (when (eq key (aref kv-vector (* 2 next)))
                 ;; Found, just replace the value.
                 (update-hash-table-cache hash-table (* 2 next))
                 (setf (aref kv-vector (1+ (* 2 next))) value)
                 (return-from %%puthash value))))
            (t
             ;; Search next-vector chain for a matching key.
             (do ((next next (aref next-vector next))
                  (i 0 (1+ i)))
                 ((zerop next))
               (declare (type index/2 next i))
               (when (> i length)
                 (signal-corrupt-hash-table hash-table))
               (when (and (= hashing (aref hash-vector next))
                          (funcall test-fun key
                                   (aref kv-vector (* 2 next))))
                 ;; Found, just replace the value.
                 (update-hash-table-cache hash-table (* 2 next))
                 (setf (aref kv-vector (1+ (* 2 next))) value)
                 (return-from %%puthash value)))))
      ;; Pop a KV slot off the free list
      (let ((free-kv-slot (hash-table-next-free-kv hash-table)))
        (declare (type index/2 free-kv-slot))
        ;; Double-check for overflow.
        (aver (not (zerop free-kv-slot)))
        (setf (hash-table-next-free-kv hash-table)
              (aref next-vector free-kv-slot))
        (incf (hash-table-number-entries hash-table))
        (update-hash-table-cache hash-table (* 2 free-kv-slot))
        (setf (aref kv-vector (* 2 free-kv-slot)) key)
        (setf (aref kv-vector (1+ (* 2 free-kv-slot))) value)
        ;; Setup the hash-vector if necessary.
        (when hash-vector
          (if (not eq-based)
              (setf (aref hash-vector free-kv-slot) hashing)
              (aver (= (aref hash-vector free-kv-slot)
                       +magic-hash-vector-value+))))
        ;; Push this slot into the next chain.
        (setf (aref next-vector free-kv-slot) next)
        (setf (aref index-vector index) free-kv-slot)))
    value))

(defun %puthash (key hash-table value)
  (declare (type hash-table hash-table))
  (aver (hash-table-index-vector hash-table))
  (macrolet ((put-it (lockedp)
               `(let ((cache (hash-table-cache hash-table))
                      (kv-vector (hash-table-table hash-table)))
                  ;; Check the cache
                  (if (and cache
                           (< cache (length kv-vector))
                           (eq (aref kv-vector cache) key))
                      ;; If cached, just store here
                      (setf (aref kv-vector (1+ cache)) value)
                      ;; Otherwise do things the hard way
                      ,(if lockedp
                           '(%%puthash key hash-table value)
                           '(with-hash-table-locks
                             (hash-table :inline (%%puthash) :pin (key)
                              :synchronized nil)
                             (%%puthash key hash-table value)))))))
    (if (hash-table-synchronized-p hash-table)
        (with-hash-table-locks (hash-table :pin (key) :synchronized t)
          (put-it t))
        (put-it nil))))

(declaim (maybe-inline %remhash))
(defun %remhash (key hash-table)
  ;; We need to rehash here so that a current key can be found if it
  ;; exists.
  ;;
  ;; Note that if a GC happens after MAYBE-REHASH returns and another
  ;; thread the accesses the table (triggering a rehash), we might not
  ;; find the key even if it is in the table. But that's ok, since the
  ;; only concurrent case that we safely allow is multiple readers
  ;; with no writers.
  (maybe-rehash hash-table nil)
  ;; Search for key in the hash table.
  (multiple-value-bind (hashing eq-based)
      (funcall (hash-table-hash-fun hash-table) key)
    (declare (type hash hashing))
    (let* ((index-vector (hash-table-index-vector hash-table))
           (length (length index-vector))
           (index (index-for-hashing hashing length))
           (next (aref index-vector index))
           (table (hash-table-table hash-table))
           (next-vector (hash-table-next-vector hash-table))
           (hash-vector (hash-table-hash-vector hash-table))
           (test-fun (hash-table-test-fun hash-table)))
      (declare (type index index)
               (type index/2 next))
      (flet ((clear-slot (chain-vector prior-slot-location slot-location)
               (declare (type index/2 slot-location))
               ;; Mark slot as empty.
               (setf (aref table (* 2 slot-location)) +empty-ht-slot+
                     (aref table (1+ (* 2 slot-location))) +empty-ht-slot+)
               ;; Update the prior pointer in the chain to skip this.
               (setf (aref chain-vector prior-slot-location)
                     (aref next-vector slot-location))
               ;; Push KV slot onto free chain.
               (setf (aref next-vector slot-location)
                     (hash-table-next-free-kv hash-table))
               (setf (hash-table-next-free-kv hash-table) slot-location)
               (when hash-vector
                 (setf (aref hash-vector slot-location)
                       +magic-hash-vector-value+))
               ;; On parallel accesses this may turn out to be a
               ;; type-error, so don't turn down the safety!
               (decf (hash-table-number-entries hash-table))
               t))
        (cond ((zerop next)
               nil)
              ((if (or eq-based (not hash-vector))
                   (eq key (aref table (* 2 next)))
                   (and (= hashing (aref hash-vector next))
                        (funcall test-fun key (aref table (* 2 next)))))
               (clear-slot index-vector index next))
              ;; Search next-vector chain for a matching key.
              ((or eq-based (not hash-vector))
               ;; EQ based
               (do ((prior next next)
                    (i 0 (1+ i))
                    (next (aref next-vector next) (aref next-vector next)))
                   ((zerop next) nil)
                 (declare (type index next))
                 (when (> i length)
                   (signal-corrupt-hash-table hash-table))
                 (when (eq key (aref table (* 2 next)))
                   (return-from %remhash (clear-slot next-vector prior next)))))
              (t
               ;; not EQ based
               (do ((prior next next)
                    (i 0 (1+ i))
                    (next (aref next-vector next) (aref next-vector next)))
                   ((zerop next) nil)
                 (declare (type index/2 next))
                 (when (> i length)
                   (signal-corrupt-hash-table hash-table))
                 (when (and (= hashing (aref hash-vector next))
                            (funcall test-fun key (aref table (* 2 next))))
                   (return-from %remhash
                     (clear-slot next-vector prior next))))))))))

(defun remhash (key hash-table)
  #!+sb-doc
  "Remove the entry in HASH-TABLE associated with KEY. Return T if
there was such an entry, or NIL if not."
  (declare (type hash-table hash-table)
           (values (member t nil)))
  (with-hash-table-locks (hash-table :inline (%remhash) :pin (key))
    ;; For now, just clear the cache
    (setf (hash-table-cache hash-table) nil)
    (%remhash key hash-table)))

(defun clrhash (hash-table)
  #!+sb-doc
  "This removes all the entries from HASH-TABLE and returns the hash
table itself."
  (when (plusp (hash-table-number-entries hash-table))
    (with-hash-table-locks (hash-table)
      (let* ((kv-vector (hash-table-table hash-table))
             (next-vector (hash-table-next-vector hash-table))
             (hash-vector (hash-table-hash-vector hash-table))
             (size (length next-vector))
             (index-vector (hash-table-index-vector hash-table)))
        ;; Disable GC tricks.
        (set-header-data kv-vector sb!vm:vector-normal-subtype)
        ;; Mark all slots as empty by setting all keys and values to magic
        ;; tag.
        (aver (eq (aref kv-vector 0) hash-table))
        (fill kv-vector +empty-ht-slot+ :start 2)
        ;; Set up the free list, all free.
        (do ((i 1 (1+ i)))
            ((>= i (1- size)))
          (setf (aref next-vector i) (1+ i)))
        (setf (aref next-vector (1- size)) 0)
        (setf (hash-table-next-free-kv hash-table) 1)
        ;; Clear the index-vector.
        (fill index-vector 0)
        ;; Clear the hash-vector.
        (when hash-vector
          (fill hash-vector +magic-hash-vector-value+)))
      (setf (hash-table-cache hash-table) nil)
      (setf (hash-table-number-entries hash-table) 0)))
  hash-table)

;; Helper for atomic read/update of a synchronized table
;; in a limited sort of way using a double-checked lock.
;; You don't get to see the old value first.
;; It wouldn't be too hard to add that feature.
(defun puthash-if-absent (key table constructor)
  (or (gethash key table)
      (let ((val (funcall constructor)))
        (with-locked-system-table (table)
          ;; VAL is discarded if KEY is found this time.
          (or (gethash key table) (setf (gethash key table) val))))))

;;;; methods on HASH-TABLE

;;; Return a list of keyword args and values to use for MAKE-HASH-TABLE
;;; when reconstructing HASH-TABLE.
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
                   ":TEST ~S :COUNT ~S~@[ :WEAKNESS ~S~]"
                   (hash-table-test hash-table)
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


