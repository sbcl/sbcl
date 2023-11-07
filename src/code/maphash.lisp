;;;; The implementations of MAPHASH and WITH-HASH-TABLE-ITERATOR,
;;;; which should remain roughly in sync.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;; an internal tag for marking empty slots, which needs to be defined
;;; no later than the compiler-macro for MAPHASH.
;;;
;;; Note that scav_vector() and scav_hash_table_entries() both
;;; have to be aware of the type of object this is.
(define-symbol-macro +empty-ht-slot+ (make-unbound-marker))
(defmacro empty-ht-slot-p (x) `(unbound-marker-p ,x))

;;; like INDEX, but only up to half the maximum. Used by hash-table
;;; code that does plenty to (aref v (* 2 i)) and (aref v (1+ (* 2 i))).
(deftype index/2 () `(integer 0 (,(floor array-dimension-limit 2))))

;;; The high water mark is an element of the pairs vector, and not
;;; a slot in the table.
;;; It is the number of logical pairs in use, so if HWM = 1, then
;;; 1 pair is in use occupying physical element indices 2 and 3.
(defmacro kv-vector-high-water-mark (pairs)
  `(truly-the index/2 (data-vector-ref ,pairs 0)))

;;; When a weak table's backing storage is iterated over, theoretically a read barrier would
;;; be needed for each <k,v> pair. This is especially bad because each access would additionally
;;; have to decide if the table is weak in the first place.
;;; The performance loss can be mitigated by a simple strategy: act as if the vector is
;;; strong. There is a header flag bit that could be toggled for that, but we don't want to
;;; manipulate it from multiple threads. (Concurrent iterating is perfectly legal of course,
;;; subject to the rules about modification during traversal)
;;; Using a technique like *PINNED-OBJECTS* we can inform GC that a vector merits unusual
;;; treatment for the lifetime of the iteration. Binding a special var says to ignore the
;;; weakness bit. GC would only have to read the special when considering whether the bit
;;; value of 1 should be respected. And that in itself is rare since most tables aren't weak.
;;; GC would have to read the TLS of each thread which is still better than requiring
;;; the mutator to strengthen each cell ref one pair at a time. Presuming the iterator doesn't
;;; exit early (most calls to MAPHASH don't), then it's equivalent.
(defvar *unweakened-vectors* nil)

;;; Hash table iteration does not need to perform bounds checks on access to the
;;; k/v pair vector.  We used to reload the local variable holding the k/v vector
;;; on each loop iteration because PUTHASH could assign a new vector at any time.
;;; It no longer will, provided that the restriction against mutating a table
;;; (other than the current key) during traversal is obeyed. Even if violated,
;;; the k/v vector won't shrink. But in any case, we grab the vector once only.
;;; There are AVERs in the insertion code that the high water mark remains valid.

(define-compiler-macro maphash (&whole form function-designator hash-table
                                &environment env)
  (when (sb-c:policy env (> space speed))
    (return-from maphash form))
  (with-unique-names (fun limit i kv-vector key value)
    `(let* ((,fun (%coerce-callable-to-fun ,function-designator))
            (,kv-vector (hash-table-pairs ,hash-table))
            #+weak-vector-readbarrier (*unweakened-vectors*
                                       (cons ,kv-vector *unweakened-vectors*))
            ;; The high water mark needs to be loaded only once due to the
            ;; prohibition against adding keys during traversal.
            (,limit (1+ (* 2 (kv-vector-high-water-mark ,kv-vector)))))
       #+weak-vector-readbarrier (declare (dynamic-extent *unweakened-vectors*))
       ;; Regarding this TRULY-THE: in the theoretical edge case of the largest
       ;; possible NEXT-VECTOR, it is not really true that the I+2 is an index.
       ;; However, for all intents and purposes, it is an INDEX because if not,
       ;; the table's vectors would consume literally all addressable memory.
       ;; And it can't overflow a fixnum even in theory, since ARRAY-DIMENSION-LIMIT
       ;; is smaller than most-positive-fixnum by enough to allow adding 2.
       ;; And it doesn't matter anyway - the compiler uses unsigned word
       ;; arithmetic here on account of (* 2 length) exceeding a fixnum.
       (do ((,i 3 (truly-the index (+ ,i 2))))
           ((> ,i ,limit))
        ;; We are running without locking or WITHOUT-GCING. For a weak
        ;; :VALUE hash table it's possible that the GC hit after KEY
        ;; was read and now the entry is gone. So check if either the
        ;; key or the value is empty.
         (let ((,key (data-vector-ref ,kv-vector (1- ,i)))
               (,value (data-vector-ref ,kv-vector ,i)))
           (unless (or (empty-ht-slot-p ,key)
                       (empty-ht-slot-p ,value))
             (funcall ,fun ,key ,value)))))))

(defun maphash (function-designator hash-table)
  "For each entry in HASH-TABLE, call the designated two-argument function on
the key and value of the entry. Return NIL.

Consequences are undefined if HASH-TABLE is mutated during the call to
MAPHASH, except for changing or removing elements corresponding to the
current key. The applies to all threads, not just the current one --
even for synchronized hash-tables. If the table may be mutated by
another thread during iteration, use eg. SB-EXT:WITH-LOCKED-HASH-TABLE
to protect the MAPHASH call."
  (declare (dynamic-extent function-designator))
  (maphash function-designator hash-table)) ; via compiler-macro

(defmacro with-hash-table-iterator ((name hash-table) &body body)
  "WITH-HASH-TABLE-ITERATOR ((name hash-table) &body body)

Provides a method of manually looping over the elements of a hash-table. NAME
is bound to a generator-macro that, within the scope of the invocation,
returns one or three values. The first value tells whether any objects remain
in the hash table. When the first value is non-NIL, the second and third
values are the key and the value of the next object.

Consequences are undefined if HASH-TABLE is mutated during execution of BODY,
except for changing or removing elements corresponding to the current key. The
applies to all threads, not just the current one -- even for synchronized
hash-tables. If the table may be mutated by another thread during iteration,
use eg. SB-EXT:WITH-LOCKED-HASH-TABLE to protect the WITH-HASH-TABLE-ITERATOR
for."
  ;; If within the BODY code the stepping function is used exactly once, then
  ;; it gets let-converted and so it should make no difference whether it returns
  ;; one or more than one value (it would never need RETURN-MULTIPLE).
  ;; However if the iterator is not let-converted then it is best to return
  ;; a fixed number of values on success or failure.
  (let ((kvv (make-symbol "KVV"))
        (lim (make-symbol "LIMIT"))
        (ind (make-symbol "INDEX"))
        (step (make-symbol "THUNK")))
    `(let* ((,kvv (hash-table-pairs ,hash-table))
            #+weak-vector-readbarrier (*unweakened-vectors*
                                       (cons ,kvv *unweakened-vectors*))
            (,lim (1+ (* 2 (kv-vector-high-water-mark ,kvv))))
            (,ind 3))
       #+weak-vector-readbarrier (declare (dynamic-extent *unweakened-vectors*))
       (declare (fixnum ,ind))
       (dx-flet ((,step ()
                   (loop
                     (when (> ,ind ,lim) (return (values t nil nil)))
                     (let ((i ,ind))
                       (incf (truly-the index ,ind) 2)
                       (let ((k (data-vector-ref ,kvv (1- i)))
                             (v (data-vector-ref ,kvv i)))
                         (unless (or (empty-ht-slot-p k) (empty-ht-slot-p v))
                           (return (values nil k v))))))))
         (macrolet ((,name ()
                      '(multiple-value-bind (endp k v) (,step)
                         (unless endp (values t k v)))))
           ,@body)))))

(define-compiler-macro make-hash-table (&whole form &rest keywords)
  (let ((kind
         (cond ((not keywords) 1)
               ((typep keywords '(cons (eql :test) (cons t null)))
                (let* ((arg (cadr keywords))
                       (test (when (typep arg '(cons (member function quote) (cons t null)))
                               (cadr arg))))
                  (position test #(eq eql equal equalp)))))))
    (if kind
        `(make-hash-table-using-defaults ,kind)
        form)))

(declaim (inline gethash3))
(defun gethash3 (key hash-table default)
  ;; Don't check return values.
  (locally (declare (optimize (safety 0)))
    (funcall (hash-table-gethash-impl hash-table) key hash-table default)))

(declaim (inline %puthash))
(defun %puthash (key hash-table value)
  (funcall (hash-table-puthash-impl hash-table) key hash-table value))
