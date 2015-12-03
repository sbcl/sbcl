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

(in-package "SB!IMPL")

(define-compiler-macro maphash (&whole form function-designator hash-table
                                &environment env)
  (when (sb!c:policy env (> space speed))
    (return-from maphash form))
  (with-unique-names (fun table size i kv-vector key value)
    `(let ((,fun (%coerce-callable-to-fun ,function-designator))
           (,table ,hash-table)
           (,size (* 2 (length (hash-table-next-vector ,hash-table)))))
       ;; Regarding this TRULY-THE: in the theoretical edge case of the largest
       ;; possible NEXT-VECTOR, it is not really true that the I+2 is an index.
       ;; However, for all intents and purposes, it is an INDEX because if not,
       ;; the table's vectors would consume literally all addressable memory.
       ;; And it can't overflow a fixnum even in theory, since ARRAY-DIMENSION-LIMIT
       ;; is smaller than most-positive-fixnum by enough to allow adding 2.
       ;; And it doesn't matter anyway - the compiler uses unsigned word
       ;; arithmetic here on account of (* 2 length) exceeding a fixnum.
       (do ((,i 3 (truly-the index (+ ,i 2))))
           ((>= ,i ,size))
        ;; We are running without locking or WITHOUT-GCING. For a weak
        ;; :VALUE hash table it's possible that the GC hit after KEY
        ;; was read and now the entry is gone. So check if either the
        ;; key or the value is empty.
         (let* ((,kv-vector (hash-table-table ,table))
                (,value (aref ,kv-vector ,i)))
           (unless (eq ,value +empty-ht-slot+)
             (let ((,key
                    ;; I is bounded below by 3, and bounded above by INDEX max,
                    ;; so (1- I) isn't checked for being an INDEX, but would
                    ;; nonetheless be checked against the array bound despite
                    ;; being obviously valid; so we force elision of the test.
                    (locally (declare (optimize (sb!c::insert-array-bounds-checks 0)))
                      (aref ,kv-vector (1- ,i)))))
               (unless (eq ,key +empty-ht-slot+)
                 (funcall ,fun ,key ,value)))))))))

(defun maphash (function-designator hash-table)
  #!+sb-doc
  "For each entry in HASH-TABLE, call the designated two-argument function on
the key and value of the entry. Return NIL.

Consequences are undefined if HASH-TABLE is mutated during the call to
MAPHASH, except for changing or removing elements corresponding to the
current key. The applies to all threads, not just the current one --
even for synchronized hash-tables. If the table may be mutated by
another thread during iteration, use eg. SB-EXT:WITH-LOCKED-HASH-TABLE
to protect the MAPHASH call."
  (maphash function-designator hash-table)) ; via compiler-macro

(defmacro with-hash-table-iterator ((name hash-table) &body body)
  #!+sb-doc
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
  (let ((function (make-symbol (concatenate 'string (symbol-name name) "-FUN"))))
    `(let ((,function
            (let* ((table ,hash-table)
                   (size (* 2 (length (hash-table-next-vector table))))
                   (index 3))
              (declare (fixnum index))
              (labels
                  ((,name ()
                     ;; (We grab the table again on each iteration just in
                     ;; case it was rehashed by a PUTHASH.)
                     (let ((kv-vector (hash-table-table table)))
                       (loop
                        (when (>= index size) (return nil))
                        (let ((i index))
                          (incf (truly-the index index) 2)
                          (let ((value (aref kv-vector i)))
                            (unless (eq value +empty-ht-slot+)
                              (let ((key
                                     (locally
                                      (declare (optimize (sb!c::insert-array-bounds-checks 0)))
                                      (aref kv-vector (1- i)))))
                                (unless (eq key +empty-ht-slot+)
                                  (return (values t key value)))))))))))
                #',name))))
       (macrolet ((,name () '(funcall ,function)))
         ,@body))))
