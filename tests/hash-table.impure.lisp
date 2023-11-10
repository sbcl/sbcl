;;; HASH TABLES

;;; Keep moving everything that can move during each GC
#+generational (setf (generation-number-of-gcs-before-promotion 0) 1000000)

;;; Check for GC invariant loss during weak table creation.
;;; This didn't always fail, but might have, and now shouldn't.
(defglobal *number-of-weak-tables* 0)
(defun make-weak-key-table () (make-hash-table :weakness :key))
(defun something-useless (x) (list x))
(defun weak-table-allocation-test ()
  (let ((thread
         (sb-thread:make-thread
           (lambda ()
             (loop
               (sleep .0001)
               (gc)
               (sb-thread:barrier (:read))
               (when (> *number-of-weak-tables* 1000) (return)))))))
    (loop repeat 1001 do
      (something-useless (make-weak-key-table))
      (incf *number-of-weak-tables*)
      (sb-thread:barrier (:write)))
    (sb-thread:join-thread thread)))
;;; Interpreted code is probably too slow to be useful in this test
(compile 'weak-table-allocation-test)

(with-test (:name :weak-table-gc-invariant :skipped-on (not :sb-thread))
  (weak-table-allocation-test))

(defun vector-flag-bits (v)
  (logand (ash (sb-kernel:get-header-data v) (- sb-vm:array-flags-data-position)) #xFF))

(defun is-address-sensitive (tbl)
  (logtest (vector-flag-bits (sb-impl::hash-table-pairs tbl))
           sb-vm:vector-addr-hashing-flag))

(with-test (:name (hash-table :eql-hash-symbol-not-eq-based))
  ;; If you ask for #'EQ as the test, then everything is address-sensitive,
  ;; though this is not technically a requirement.
  (let ((ht (make-hash-table :test 'eq :size 128)))
    (assert (not (sb-impl::flat-hash-table-p ht)))
    (setf (gethash (make-symbol "GOO") ht) 1)
    (assert (is-address-sensitive ht)))
  ;; EQUAL tables don't use SYMBOL-HASH
  (dolist (test '(eql equalp))
    (let ((ht (make-hash-table :test test :size 128)))
      (assert (not (sb-impl::flat-hash-table-p ht)))
      (setf (gethash (make-symbol "GOO") ht) 1)
      (assert (not (is-address-sensitive ht))))))

(defclass ship () ())

(with-test (:name (hash-table :equal-hash-std-object-not-eq-based))
  (dolist (test '(eq eql))
    (let ((ht (make-hash-table :test test :size 128)))
      (assert (not (sb-impl::flat-hash-table-p ht)))
      (setf (gethash (make-instance 'ship) ht) 1)
      (assert (is-address-sensitive ht))))
  ;; EQUAL tables don't use INSTANCES-SXHASH
  (dolist (test '(equalp))
    (let ((ht (make-hash-table :test test :size 128)))
      (assert (not (sb-impl::flat-hash-table-p ht)))
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
(with-test (:name :gc-while-growing-weak-hash-table
            :skipped-on (or :mark-region-gc :gc-stress))
  (let ((h (make-hash-table :weakness :key)))
    (setq *gc-after-rehash-me* h)
    (dotimes (i 50)
      (setf (gethash (list (gensym)) h) i)
      (assert (is-address-sensitive h)))
    (setf (gethash (cons 1 2) h) 'foolz))
  (assert (>= *rehash+gc-count* 10)))

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

(with-test (:name :rehash-no-spurious-address-sensitivity)
  (let ((h (make-hash-table :test 'eq)))
    (dotimes (i 100)
      (setf (gethash i h) (- i)))
    (assert (= (vector-flag-bits (sb-impl::hash-table-pairs h))
               sb-vm:vector-hashing-flag))))

(defmacro kv-vector-needs-rehash (x) `(svref ,x 1))
;;; EQL tables no longer get a hash vector, so the GC has to decide
;;; for itself whether key movement forces rehash.
;;; Let's make sure that works.
;;; I don't love the idea of skipping this, because mark-region *can* move keys
;;; though it generally doesn't. After I started to hack up the test to query
;;; whether keys moved, it looked pretty disastrous. Need to think of way.
(with-test (:name :address-insensitive-eql-hash
                  :skipped-on :mark-region-gc)
  (let ((tbl (make-hash-table :size 20)))
    (dotimes (i 5)
      (let ((key (coerce i 'double-float)))
        (setf (gethash key tbl) (sb-kernel:get-lisp-obj-address key)))
      (let ((key (coerce i '(complex single-float))))
        (setf (gethash key tbl) (sb-kernel:get-lisp-obj-address key)))
      (let ((key (make-symbol (make-string (1+ i) :initial-element #\a))))
        (setf (gethash key tbl) (sb-kernel:get-lisp-obj-address key))))
    (assert (= (vector-flag-bits (sb-impl::hash-table-pairs tbl))
               sb-vm:vector-hashing-flag)) ; noo address-based key
    (let ((foo (cons 0 0)))
      (setf (gethash foo tbl) foo)
      (remhash foo tbl))
    ;; now we've added an address-based key (but removed it)
    (assert (= (vector-flag-bits (sb-impl::hash-table-pairs tbl))
               (+ sb-vm:vector-addr-hashing-flag
                  sb-vm:vector-hashing-flag)))
    (gc)
    (let ((n-keys-moved 0))
      (maphash (lambda (key value)
                 (unless (= value (sb-kernel:get-lisp-obj-address key))
                   (incf n-keys-moved)))
               tbl)
      (assert (plusp n-keys-moved))
      ;; keys were moved, the table is marked as address-based,
      ;; but no key that moved forced a rehash
      (assert (zerop (kv-vector-needs-rehash
                      (sb-impl::hash-table-pairs tbl)))))
    ;; the vector type is unchanged
    (assert (= (vector-flag-bits (sb-impl::hash-table-pairs tbl))
               (+ sb-vm:vector-addr-hashing-flag
                  sb-vm:vector-hashing-flag)))
    (setf (gethash (cons 1 2) tbl) 'one)
    (setf (gethash (cons 3 4) tbl) 'two)
    (setf (gethash (cons 5 6) tbl) 'three)
    (gc)
    ;; now some key should have moved and forced a rehash
    (assert (not (zerop (kv-vector-needs-rehash
                         (sb-impl::hash-table-pairs tbl)))))
    ;; This next thing is impossible to test without some hacks -
    ;; we want to see that the addr-hashing flag can be cleared
    ;; if, on rehash, there is currently no address-sensitive key
    ;; in the table.
    ;; This could happen in the real world, but it's actually very
    ;; difficult to construct an example because it requires controlling
    ;; the addresses of objects.  But the 'rehash' bit had to first get
    ;; set, and then any key that could cause the bit to get set
    ;; has to be removed, which means we had to have successfully found
    ;; and removed address-sensitive keys despite having obsolete hashes.
    ;; That could only happen by random chance.
    ;; However, by stomping on a few keys, we can simulate it.
    (let ((pairs (sb-impl::hash-table-pairs tbl)))
      (loop for i from 2 below (length pairs) by 2
            when (consp (aref pairs i))
            do (setf (aref pairs i) i))) ; highly illegal!
    ;; try to find an address-sensitive key
    (assert (not (gethash '(foo) tbl)))
    ;; Address-sensitivity is sticky.
    (assert (= (vector-flag-bits (sb-impl::hash-table-pairs tbl))
               (+ sb-vm:vector-addr-hashing-flag
                  sb-vm:vector-hashing-flag)))
    (clrhash tbl)
    (assert (= (vector-flag-bits (sb-impl::hash-table-pairs tbl))
               ;; Table is no longer address-sensitive
               sb-vm:vector-hashing-flag))))

(defun actually-address-sensitive-p (ht)
  (let* ((hashfun (sb-impl::hash-table-hash-fun ht))
         (some-actually-address-sensitive-key))
    (maphash (lambda (key value)
               (declare (ignore value))
               (multiple-value-bind (hash address-sensitive)
                   (funcall hashfun key)
                 (declare (ignore hash))
                 (when address-sensitive
                   (setf some-actually-address-sensitive-key t))))
             ht)
    some-actually-address-sensitive-key))

(with-test (:name :unsynchronized-clrhash-no-lock)
  (let ((ht (make-hash-table)))
    (setf (gethash 1 ht) 2)
    (clrhash ht)
    (assert (not (sb-impl::hash-table-%lock ht)))))

;;; Prove that our completely assinine API with regard to locking works,
;;; which is to say, if the user explicitly locks an implicitly locked table,
;;; there is no "Recursive lock attempt" error.
;;; In general, we can't discern between a lock that preserves table invariants
;;; at the implementation level, or the user level. But I guess with "system" locks
;;; it's sort of OK because reentrance isn't really possible, and internally
;;; the table considers the lock to be a "system" lock.
(with-test (:name :weak-hash-table-with-explicit-lock)
  (let ((h (make-hash-table :weakness :key)))
    (with-locked-hash-table (h) (setf (gethash 'foo h) 1))))

(with-test (:name :hash-table-iterator-no-notes
                  :fails-on (:or :arm :ppc :ppc64))
  (let ((f
         (checked-compile
          '(lambda (h)
            (declare (optimize speed))
            (let ((n 0))
              (declare (fixnum n))
              ;; Silly test - count items, unrolling by 2
              (with-hash-table-iterator (iter h)
                (loop
                  (let ((a (iter)))
                    (unless a (return)))
                  (let ((a (iter)))
                    (unless a
                      (incf n)
                      (return)))
                  (incf n 2)))
              n))
          :allow-notes nil)))
    ;; Test F
    (maphash (lambda (classoid layout)
               (declare (ignore layout))
               (let ((subclasses
                      (sb-kernel:classoid-subclasses classoid)))
                 (when (hash-table-p subclasses)
                   (assert (= (hash-table-count subclasses)
                              (funcall f subclasses))))))
             (sb-kernel:classoid-subclasses (sb-kernel:find-classoid 't)))))

(defun verify-table (ht)
  (sb-sys:without-gcing
   (alien-funcall (extern-alien "verify_lisp_hashtable"
                                (function int unsigned unsigned))
                  (- (sb-kernel:get-lisp-obj-address ht) 3)
                  0)))

(with-test (:name :verify-hash-table-hashing :skipped-on (:not :x86-64))
  (let ((tables (sb-vm:list-allocated-objects :all :test #'hash-table-p)))
    (dolist (table tables (format t "::: Examined ~D tables~%" (length tables)))
      (let ((errors (verify-table table)))
        (unless (zerop errors)
          (if (/= 0 (svref (sb-impl::hash-table-pairs table) 1))
              (format t "~S wants rehash~%" table)
              (error "~S should be marked for rehash" table)))))))
