;;; HASH TABLES

;;; Keep moving everything that can move during each GC
#+gencgc (setf (generation-number-of-gcs-before-promotion 0) 1000000)

(defun is-address-sensitive (tbl)
  (let ((data (sb-kernel:get-header-data (sb-impl::hash-table-pairs tbl))))
    (logtest data sb-vm:vector-addr-hashing-subtype)))

(with-test (:name (hash-table :eql-hash-symbol-not-eq-based))
  ;; If you ask for #'EQ as the test, then everything is address-sensitive,
  ;; though this is not technically a requirement.
  (let ((ht (make-hash-table :test 'eq)))
    (setf (gethash (make-symbol "GOO") ht) 1)
    (assert (is-address-sensitive ht)))
  (dolist (test '(eql equal equalp))
    (let ((ht (make-hash-table :test test)))
      (setf (gethash (make-symbol "GOO") ht) 1)
      (assert (not (is-address-sensitive ht))))))

(defclass ship () ())

(with-test (:name (hash-table :equal-hash-std-object-not-eq-based))
  (dolist (test '(eq eql))
    (let ((ht (make-hash-table :test test)))
      (setf (gethash (make-instance 'ship) ht) 1)
      (assert (is-address-sensitive ht))))
  (dolist (test '(equal equalp))
    (let ((ht (make-hash-table :test test)))
      (setf (gethash (make-instance 'ship) ht) 1)
      (assert (not (is-address-sensitive ht))))))

(defvar *gc-after-rehash-me* nil)
(defvar *rehash+gc-count* 0)

(sb-int:encapsulate
 'sb-impl::rehash
 'force-gc-after-rehash
 (compile nil '(lambda (f kvv hv iv nv tbl)
                (prog1 (funcall f kvv hv iv nv tbl)
                  (when (eq tbl *gc-after-rehash-me*)
                    (incf *rehash+gc-count*)
                    (sb-ext:gc))))))

;;; Check that when growing a weak hash-table we don't try to
;;; reference kvv -> table -> hash-vector
;;; until the hash-vector is correct with respect to the KV vector.
;;; For this test, we need address-sensitive keys in a table with a
;;; hash-vector. EQ tables don't have a hash-vector, so that's no good.
;;; EQL tables don't hash symbols address-sensitively,
;;; so use a bunch of cons cells.
(with-test (:name :gc-while-growing-weak-hash-table)
  (let ((h (make-hash-table :weakness :key)))
    (setq *gc-after-rehash-me* h)
    (dotimes (i 14) (setf (gethash (list (gensym)) h) i))
    (setf (gethash (cons 1 2) h) 'foolz))
  (assert (= *rehash+gc-count* 1)))

(defstruct this x)
(defstruct that x)
(with-test (:name :struct-in-list-equal-hash)
  (let ((ht (make-hash-table :test 'equal)))
    (dotimes (i 100)
      (let ((key (cons (make-this :x i) (make-that :x i))))
        (setf (gethash key ht)  i)))
    ;; This used to degenerate the hash table into a linked list,
    ;; because all instances of THIS hashed to the same random fixnum
    ;; and all instances of THAT hashed to the same random fixnum
    ;; (different from THIS, not that it mattered), and the hash
    ;; of the cons was therefore the same.
    (let ((bins-used
            (count-if #'plusp (sb-impl::hash-table-index-vector ht))))
      ;; It's probably even better spread out than this many bins,
      ;; but let's not be too sensitive to the exact bin count in use.
      ;; It's a heck of a lot better than everything in 1 bin.
      (assert (> bins-used 40)))))
