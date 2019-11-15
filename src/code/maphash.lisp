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
(deftype index/2 () `(integer 0 (,(floor sb-xc:array-dimension-limit 2))))

;;; The high water mark is an element of the pairs vector, and not
;;; a slot in the table.
;;; It is the number of logical pairs in use, so if HWM = 1, then
;;; 1 pair is in use occupying physical element indices 2 and 3.
(defmacro kv-vector-high-water-mark (pairs)
  `(truly-the index/2 (data-vector-ref ,pairs 0)))

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
            ;; The high water mark needs to be loaded only once due to the
            ;; prohibition against adding keys during traversal.
            (,limit (1+ (* 2 (kv-vector-high-water-mark ,kv-vector)))))
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
  (let ((function (gensymify* name "-FUN")))
    `(let ((,function
            (let* ((kv-vector (hash-table-pairs ,hash-table))
                   (limit (1+ (* 2 (kv-vector-high-water-mark kv-vector))))
                   (index 3))
              (declare (fixnum index))
              (flet ((,name ()
                       (loop
                        (when (> index limit) (return nil))
                        (let ((i index))
                          (incf (truly-the index index) 2)
                          (let ((key (data-vector-ref kv-vector (1- i)))
                                (value (data-vector-ref kv-vector i)))
                            (unless (or (empty-ht-slot-p key)
                                        (empty-ht-slot-p value))
                              (return (values t key value))))))))
                #',name))))
       (macrolet ((,name () '(funcall ,function)))
         ,@body))))
