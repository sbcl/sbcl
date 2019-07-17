;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

;;; The return value of SXHASH on non-string/bitvector arrays should not
;;; change when the contents of the array change.
(with-test (:name (sxhash array :independent-of-contents))
  (let* ((a (make-array '(1) :initial-element 1))
         (sxhash (sxhash a))
         (hash (make-hash-table :test 'equal)))
    (setf (gethash a hash) t)
    (setf (aref a 0) 0)
    (assert (= sxhash (sxhash a)))
    ;; Need to make another access to the hash to disable the
    ;; last-seen-element cache.
    (setf (gethash 'y hash) t)
    (assert (gethash a hash))))

;;; Minimum quality checks
(with-test (:name (sxhash :quality :minimum))
  (assert (/= (sxhash "foo") (sxhash "bar")))
  (assert (/= (sxhash (pathname "foo.txt")) (sxhash (pathname "bar.txt"))))
  (assert (/= (sxhash (list 1 2 3)) (sxhash (list 3 2 1))))
  (assert (/= (sxhash #*1010) (sxhash #*0101))))

;;; This test supposes that no un-accounted-for consing occurs.
(with-test (:name :address-based-hash-counter :skipped-on :interpreter)
  ;; It doesn't particularly matter what ADDRESS-BASED-COUNTER-VAL returns,
  ;; but it's best to verify the assumption that each cons bumps the count
  ;; by 1, lest it be violated in a way that affects the quality of CTYPE
  ;; hashes.
  (let ((win 0) (n-trials 10) (prev (sb-int:address-based-counter-val)))
    (dotimes (i n-trials)
      (declare (notinline cons)) ; it's flushable, but don't flush it
      (cons 1 2)
      (let ((ptr (sb-int:address-based-counter-val)))
        (when (= ptr (1+ prev))
          (incf win))
        (setq prev ptr)))
    ;; GC could occur in here. Just check that 9 out of 10 trials succeed.
    (assert (>= win 9))))

;;; The value of SXHASH on bit-vectors of length a multiple of the word
;;; size didn't depend on the contents of the last word, specifically
;;; making it a constant for bit-vectors of length equal to the word
;;; size.
;;; Here we test that at least two different hash codes occur per length.
(with-test (:name (sxhash :quality bit-vector :non-constant))
  (let (;; Up to which length to test.
        (max-length 200)
        ;; How many random bits to use before declaring a test failure.
        (random-bits-to-use 200))
    (loop for length from 1 to max-length do
          (let ((v (make-array length :element-type 'bit)))
            (flet ((randomize-v ()
                     (map-into v (lambda ()
                                   (random 2)))))
              (randomize-v)
              (let ((sxhash (sxhash v))
                    (random-bits-used 0))
                (loop
                  (randomize-v)
                  (when (/= (sxhash v) sxhash)
                    (return))
                  (incf random-bits-used length)
                  (when (>= random-bits-used random-bits-to-use)
                    (error "SXHASH is constant on bit-vectors of length ~a."
                           length)))))))))

;;; See the comment at the previous test.
;;; Here we test that the hash code depends on any of the last N-WORD-BITS
;;; bits.
(with-test (:name (sxhash :quality bit-vector :dependent-on-final-bits))
  (let (;; Up to which length to test.
        (max-length 200)
        ;; How many random bits to use before declaring a test failure.
        (random-bits-to-use 200))
    ;; The previous test covers lengths up to the word size, so start
    ;; above that.
    (loop for length from (1+ sb-vm:n-word-bits) to max-length do
          (let ((v (make-array length :element-type 'bit :initial-element 0)))
            (flet ((randomize-v ()
                     (loop for i downfrom (1- length)
                           repeat sb-vm:n-word-bits
                           do (setf (aref v i) (random 2)))))
              (randomize-v)
              (let ((sxhash (sxhash v)))
                (dotimes (i (ceiling random-bits-to-use sb-vm:n-word-bits)
                          (error "SXHASH on bit-vectors of length ~a ~
                                  does not depend on the final ~a bits."
                                 length sb-vm:n-word-bits))
                  (randomize-v)
                  (when (/= (sxhash v) sxhash)
                    (return)))))))))

(with-test (:name :maphash-multiple-evaluation)
  (assert (null
           (check-function-evaluation-order
            (maphash
             (constantly nil)
             (make-hash-table))))))

(with-test (:name :equalp-hash-float-infinity)
  (let ((table (make-hash-table :test 'equalp)))
    (setf (gethash sb-ext:double-float-positive-infinity table) 1
          (gethash sb-ext:double-float-negative-infinity table) -1)
    (dolist (v (list sb-ext:single-float-positive-infinity
                     sb-ext:double-float-positive-infinity
                     (complex sb-ext:single-float-positive-infinity 0)
                     (complex sb-ext:double-float-positive-infinity 0)))
      (assert (eql (gethash v table) 1)))
    (dolist (v (list sb-ext:single-float-negative-infinity
                     sb-ext:double-float-negative-infinity
                     (complex sb-ext:single-float-negative-infinity 0)
                     (complex sb-ext:double-float-negative-infinity 0)))
      (assert (eql (gethash v table) -1)))))

(with-test (:name (:hash equalp pathname))
  (let* ((map (make-hash-table :test 'equalp))
         (key  #P"some/path/"))
    (setf (gethash key map) "my-value")
    (format (make-broadcast-stream) "Printing: ~A~%" key)
    (assert (remhash key map))
    (assert (= 0 (hash-table-count map)))))

(with-test (:name :clrhash-clears-rehash-p)
  (let ((tbl (make-hash-table)))
    (dotimes (i 10)
      (setf (gethash (cons 'foo (gensym)) tbl) 1))
    (gc)
    ;; The need-to-rehash bit is set
    (assert (eql 1 (svref (sb-impl::hash-table-pairs tbl) 1)))
    (clrhash tbl)
    ;; The need-to-rehash bit is not set
    (assert (eql 0 (svref (sb-impl::hash-table-pairs tbl) 1)))))

(with-test (:name :sxhash-signed-floating-point-zeros)
  (assert (not (eql (sxhash -0f0) (sxhash 0f0))))
  (assert (not (eql (sxhash -0d0) (sxhash 0d0)))))

(with-test (:name :sxhash-simple-bit-vector)
  (let (hashes)
    (let ((v (make-array sb-vm:n-word-bits :element-type 'bit)))
      (dotimes (i sb-vm:n-word-bits)
        (setf (aref v i) 1)
        (push (sxhash v) hashes)
        (setf (aref v i) 0)))
    (assert (= (length (remove-duplicates hashes)) sb-vm:n-word-bits))))

(with-test (:name :eq-hash-nonpointers-not-address-sensitive)
  (let ((tbl (make-hash-table :test 'eq)))
    (setf (gethash #\a tbl) 1)
    #+64-bit (setf (gethash 1.0f0 tbl) 1) ; single-float is a nonpointer
    (let ((data (sb-kernel:get-header-data (sb-impl::hash-table-pairs tbl))))
      (assert (not (logtest data sb-vm:vector-addr-hashing-subtype))))))

(with-test (:name (hash-table :small-rehash-size))
  (let ((ht (make-hash-table :rehash-size 2)))
    (dotimes (i 100)
      (setf (gethash (gensym) ht) 10)))
  (let ((ht (make-hash-table :rehash-size 1.0001)))
    (dotimes (i 100)
      (setf (gethash (gensym) ht) 10))))

(with-test (:name (hash-table :custom-hashfun-with-standard-test))
  (flet ((kv-flag-bits (ht)
           (sb-kernel:get-header-data (sb-impl::hash-table-pairs ht))))
    ;; verify that EQ hashing on symbols is address-sensitive
    (let ((h (make-hash-table :test 'eq)))
      (setf (gethash 'foo h) 1)
      (assert (logtest (kv-flag-bits h) sb-vm:vector-addr-hashing-subtype)))
    (let ((h (make-hash-table :test 'eq :hash-function 'sb-kernel:symbol-hash)))
      (setf (gethash 'foo h) 1)
      (assert (not (logtest (kv-flag-bits h) sb-vm:vector-addr-hashing-subtype))))

    ;; Verify that any standard hash-function on a function is address-sensitive,
    ;; but a custom hash function makes it not so.
    ;; Require 64-bit since this uses %CODE-SERIALNO as the hash,
    ;; and that function doesn't exist on 32-bit (but should!)
    #+64-bit
    (dolist (test '(eq eql equal equalp))
      (let ((h (make-hash-table :test test)))
        (setf (gethash #'car h) 1)
        (assert (logtest (kv-flag-bits h) sb-vm:vector-addr-hashing-subtype)))
      (let ((h (make-hash-table :test test :hash-function
                                (lambda (x)
                                  (sb-kernel:%code-serialno
                                   (sb-kernel:fun-code-header x))))))
        (setf (gethash #'car h) 1)
        (assert (not (logtest (kv-flag-bits h) sb-vm:vector-addr-hashing-subtype)))))))

(defun hash-table-freelist (tbl)
  (sb-int:named-let chain ((index (sb-impl::hash-table-next-free-kv tbl)))
    (when (plusp index)
      (nconc (list index)
             (chain (aref (sb-impl::hash-table-next-vector tbl) index))))))

(defvar *tbl* (make-hash-table :weakness :key))

(import 'sb-impl::hash-table-smashed-cells)
;;; We have a bunch of tests of weakness, but this is testing the new algorithm
;;; which has two different freelists - one of cells that REMHASH has made available
;;; and one of cells that GC has marked as empty. Since we no longer inhibit GC
;;; during table operations, we need to give GC a list of its own to manipulate.
(with-test (:name (hash-table :gc-smashed-cell-list))
  (flet ((f ()
           (dotimes (i 20000) (setf (gethash i *tbl*) (- i)))
           (setf (gethash (cons 1 2) *tbl*) 'foolz)
           (assert (= (sb-impl::kv-vector-high-water-mark (sb-impl::hash-table-pairs *tbl*))
                      20001))
           (loop for i from 10 by 10 repeat 20 do (remhash i *tbl*))))
    ;; Ensure the values remain outside of the stack pointer for scrub-control-stack to work
    (declare (notinline f))
    (f))
  (sb-sys:scrub-control-stack)
  (gc)
  ;; There were 20 items REMHASHed plus the freelist contains a pointer
  ;; to a cell which is one past the high-water-mark, for 21 cells in all.
  (assert (= (length (hash-table-freelist *tbl*)) 21))
  ;; The (1 . 2) cons was removed
  (assert (= (length (hash-table-smashed-cells *tbl*)) 1))
  ;; And its representation in the list of smashed cells doesn't
  ;; fit in a packed integer (because the cell index is 20001)
  (assert (and (consp (hash-table-smashed-cells *tbl*))
               (consp (car (hash-table-smashed-cells *tbl*)))))
  (setf (gethash 'jeebus *tbl*) 9)
  ;; Freelist should not have changed at all.
  (assert (= (length (hash-table-freelist *tbl*)) 21))
  ;; And the smashed cell was used.
  (assert (null (hash-table-smashed-cells *tbl*)))
  (setf (gethash (make-symbol "SANDWICH") *tbl*) 8)
  ;; Now one item should have been popped
  (assert (= (length (hash-table-freelist *tbl*)) 20))
  ;; should have used up the smashed cell
  (sb-sys:scrub-control-stack)
  (gc)
  ;; Should have smashed the uninterned symbol
  (assert (hash-table-smashed-cells *tbl*)))

;;; Immediate values are address-based but not motion-sensitive.
;;; The hash function returns address-based = NIL.
;;; The specialized function GETHASH/EQ never compares hashes,
;;; but the generalized FINDHASH-WEAK forgot to not compare them.
(with-test (:name (hash-table :weak-eq-table-fixnum-key))
  (let ((table (make-hash-table :test 'eq :weakness :key)))
    (setf (gethash 42 table) t)
    (gethash 42 table)))
