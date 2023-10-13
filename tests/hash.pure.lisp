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
;;; But now that we have to two regions for allocation, it's not necessarily
;;; the case that any given allocation bumps the mixed-region free pointer.
(with-test (:name :address-based-hash-counter :skipped-on :interpreter)
  ;; It doesn't particularly matter what ADDRESS-BASED-COUNTER-VAL returns,
  ;; but it's best to verify the assumption that each cons bumps the count
  ;; by 1, lest it be violated in a way that affects the quality of CTYPE
  ;; hashes.
  (let ((win 0) (n-trials 10) (prev (sb-int:address-based-counter-val)))
    (dotimes (i n-trials)
      (locally
          (declare (notinline cons sb-sys:int-sap)) ; it's flushable, but don't flush it
        #+use-cons-region (sb-sys:int-sap #xf00fa) ; 2 words in mixed-region
        #-use-cons-region (cons 1 2))
      (let ((ptr (sb-int:address-based-counter-val)))
        (when (= ptr (1+ prev))
          (incf win))
        (setq prev ptr)))
    ;; GC could occur in here. Just check that 9 out of 10 trials succeed.
    (assert (>= win 9))))

(with-test (:name (sxhash :bit-vector-sxhash-mask-to-length))
  (let ((bv (make-array 5 :element-type 'bit))
        (unsafely-set-bit
         (compile nil
                  '(lambda (bv i val)
                     (declare (optimize (sb-c:insert-array-bounds-checks 0)))
                     (setf (bit bv i) val)))))
    (replace bv '(1 0 1 1 1))
    (let ((hash (sxhash bv)))
      ;; touch all bits of the first data word as well as the padding word
      (loop for i from 5 below (* 2 sb-vm:n-word-bytes)
            do (funcall unsafely-set-bit bv i 1)
               (assert (eql (sxhash bv) hash))
               (funcall unsafely-set-bit bv i 0)))))

(defvar *sbv* (make-array 512 :element-type 'bit))
(defun sxhash-for-bv-test (test-bv)
  (let ((underlying *sbv*)
        (expected-hash (sxhash test-bv)))
    ;; Currently %SXHASH-BIT-VECTOR can only operate on non-simple vectors
    ;; if the displacement on bits aligns to a word boundary,
    ;; or possibly on a byte boundary for some CPUs.
    ;; Otherwise it just copies the non-simple vector as there's no point
    ;; to exercising varying values of displaced-index-offet for that.
    (loop for index-offset
          from 0 by 8 repeat 25
          do (let ((unsimple-bv (make-array 300
                                            :element-type 'bit
                                            :displaced-to underlying
                                            :displaced-index-offset index-offset
                                            :fill-pointer (length test-bv))))
               (flet ((check-unreplaced-bits (expect)
                        ;; make sure REPLACE didn't touch bits outside
                        ;; the expected range. This is a test of REPLACE
                        ;; more so than SXHASH, but is needed to establish
                        ;; that SXHASH of the nonsimple vector isn't
                        ;; looking at bits that it shouldn't.
                        (loop for i from 0 below index-offset
                              do (assert (= (bit underlying i) expect)))
                        (loop for i from (+ index-offset (length test-bv))
                              below (length underlying)
                              do (assert (= (bit underlying i) expect)))))
                 (fill underlying 0)
                 (replace unsimple-bv test-bv)
                 (check-unreplaced-bits 0)
                 (assert (eql (sxhash unsimple-bv) expected-hash))
                 (fill underlying 1)
                 (replace unsimple-bv test-bv)
                 (check-unreplaced-bits 1)
                 (assert (eql (sxhash unsimple-bv) expected-hash)))))
    expected-hash))

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
              (let ((sxhash (sxhash-for-bv-test v))
                    (random-bits-used 0))
                (loop
                  (randomize-v)
                  (when (/= (sxhash-for-bv-test v) sxhash)
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
              (let ((sxhash (sxhash-for-bv-test v)))
                (dotimes (i (ceiling random-bits-to-use sb-vm:n-word-bits)
                          (error "SXHASH on bit-vectors of length ~a ~
                                  does not depend on the final ~a bits."
                                 length sb-vm:n-word-bits))
                  (randomize-v)
                  (when (/= (sxhash-for-bv-test v) sxhash)
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
    ;; Set the need-to-rehash bit
    (setf (svref (sb-impl::hash-table-pairs tbl) 1) 1)
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
      (assert (not (logtest data sb-vm:vector-addr-hashing-flag))))))

(with-test (:name (hash-table :small-rehash-size))
  (let ((ht (make-hash-table :rehash-size 2)))
    (dotimes (i 100)
      (setf (gethash (gensym) ht) 10)))
  (let ((ht (make-hash-table :rehash-size 1.0001)))
    (dotimes (i 100)
      (setf (gethash (gensym) ht) 10))))

(with-test (:name (hash-table :custom-hashfun-with-standard-test))
  (flet ((kv-flag-bits (ht)
           (ash (sb-kernel:get-header-data (sb-impl::hash-table-pairs ht))
                (- sb-vm:array-flags-data-position))))
    ;; verify that EQ hashing on symbols is address-sensitive
    (let ((h (make-hash-table :test 'eq)))
      (setf (gethash 'foo h) 1)
      (assert (logtest (kv-flag-bits h) sb-vm:vector-addr-hashing-flag)))
    (let ((h (make-hash-table :test 'eq :hash-function 'sb-kernel:symbol-hash)))
      (setf (gethash 'foo h) 1)
      (assert (not (logtest (kv-flag-bits h) sb-vm:vector-addr-hashing-flag))))

    ;; Verify that any standard hash-function on a function is address-sensitive,
    ;; but a custom hash function makes it not so.
    ;; Require 64-bit since this uses %CODE-SERIALNO as the hash,
    ;; and that function doesn't exist on 32-bit (but should!)
    #+64-bit
    (dolist (test '(eq eql equal equalp))
      (let ((h (make-hash-table :test test)))
        (setf (gethash #'car h) 1)
        (assert (logtest (kv-flag-bits h) sb-vm:vector-addr-hashing-flag)))
      (let ((h (make-hash-table :test test :hash-function
                                (lambda (x)
                                  (sb-kernel:%code-serialno
                                   (sb-kernel:fun-code-header x))))))
        (setf (gethash #'car h) 1)
        (assert (not (logtest (kv-flag-bits h) sb-vm:vector-addr-hashing-flag)))))))

(defun hash-table-freelist (tbl)
  (sb-int:named-let chain ((index (sb-impl::hash-table-next-free-kv tbl)))
    (when (plusp index)
      (nconc (list index)
             (if (< index
                    (sb-impl::kv-vector-high-water-mark (sb-impl::hash-table-pairs tbl)))
                 (chain (aref (sb-impl::hash-table-next-vector tbl) index)))))))

(defvar *tbl* (make-hash-table :weakness :key))

(import 'sb-impl::hash-table-smashed-cells)
;;; We have a bunch of tests of weakness, but this is testing the new algorithm
;;; which has two different freelists - one of cells that REMHASH has made available
;;; and one of cells that GC has marked as empty. Since we no longer inhibit GC
;;; during table operations, we need to give GC a list of its own to manipulate.
(with-test (:name (hash-table :gc-smashed-cell-list)
            :skipped-on :gc-stress
            :broken-on :mark-region-gc)
  (flet ((f ()
           (dotimes (i 20000) (setf (gethash i *tbl*) (- i)))
           (setf (gethash (cons 1 2) *tbl*) 'foolz)
           (assert (= (sb-impl::kv-vector-high-water-mark (sb-impl::hash-table-pairs *tbl*))
                      20001))
           (loop for i from 10 by 10 repeat 20 do (remhash i *tbl*))))
    ;; Ensure the values remain outside of the stack pointer for scrub-control-stack to work
    (declare (notinline f))
    (f))
  (eval nil)
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

(with-test (:name :write-hash-table-readably)
  (let ((h1 (make-hash-table)))
    (setf (gethash :a h1) 1
          (gethash :b h1) 2
          (gethash :c h1) 3)
    (let* ((s1 (write-to-string h1 :readably t))
           (h2 (read-from-string s1))
           (s2 (write-to-string h2 :readably t)))
      ;; S1 and S2 used to be STRING/= prior to making
      ;; %HASH-TABLE-ALIST iterate backwards.
      (assert (string= s1 s2)))))

(defun test-this-object (table-kind object)
  (let ((store (make-hash-table :test table-kind)))
    (setf (gethash object store) '(1 2 3))
    (assert (equal (gethash object store) '(1 2 3)))
    (assert (remhash object store))
    (assert (= (hash-table-count store) 0))))

;; https://bugs.launchpad.net/sbcl/+bug/1865094
(with-test (:name :remhash-eq-comparable-in-equal-table)
  ;; These objects are all hashed by their address,
  ;; so their stored hash value is the magic marker.
  (test-this-object 'equal (make-hash-table))
  (test-this-object 'equal (sb-kernel:find-defstruct-description 'sb-c::node))
  (test-this-object 'equal #'car)
  (test-this-object 'equal (sb-sys:int-sap 0))
  ;; a CLASS is not hashed address-sensitively, so this wasn't
  ;; actually subject to the bug. Try it anyway.
  (test-this-object 'equal (find-class 'class)))

(with-test (:name :remhash-eq-comparable-in-equalp-table)
  ;; EQUALP tables worked a little better, because more objects are
  ;; hashed non-address-sensitively by EQUALP-HASH relative to EQUAL-HASH,
  ;; and those objects have comparators that descend.
  ;; However, there are still some things hashed by address:
  (test-this-object 'equalp (make-weak-pointer "bleep"))
  (test-this-object 'equalp (sb-int:find-fdefn '(setf car)))
  (test-this-object 'equalp #'car)
  (test-this-object 'equalp (constantly 5))
  (test-this-object 'equal (sb-sys:int-sap 0)))

;;; I don't like that we call SXHASH on layouts, but there was a horrible
;;; regression in which we returned (SXHASH (LAYOUT-OF X)) if X was a layout,
;;; which essentially meant that all layouts hashed to LAYOUT's hash.
;;; This affected the performance of TYPECASE.
(with-test (:name :sxhash-on-layout)
  (dolist (x '(pathname cons array))
    (let ((l (sb-kernel:find-layout x)))
      (assert (= (sxhash l) (sb-kernel:layout-clos-hash l))))))

(with-test (:name :equalp-table-fixnum-equal-to-float)
  (let ((table (make-hash-table :test #'equalp)))
    (assert (eql (setf (gethash 3d0 table) 1)
                 (gethash 3   table)))))

;;; Check that hashing a stringlike thing which is possibly NIL uses
;;; a specialized hasher (after prechecking for NIL via the transform).
(with-test (:name :transform-sxhash-string-and-bv)
  (dolist (case `((bit-vector sb-kernel:%sxhash-bit-vector)
                  (string sb-kernel:%sxhash-string)
                  (simple-bit-vector sb-kernel:%sxhash-simple-bit-vector)
                  (simple-string sb-kernel:%sxhash-simple-string)))
    (let ((f `(lambda (x) (sxhash (truly-the (or null ,(car case)) x)))))
      (assert (equal (ctu:ir1-named-calls f) (cdr case))))))

(with-test (:name :sxhash-on-displaced-string
            :fails-on :sbcl)
  (let* ((adjustable-string
          (make-array 100 :element-type 'character :adjustable t))
         (displaced-string
          (make-array 50 :element-type 'character :displaced-to adjustable-string
                      :displaced-index-offset 19)))
    (adjust-array adjustable-string 68)
    (assert-error (aref displaced-string 0)) ; should not work
    ;; This should fail, but instead it computes the hash of a string of
    ;; length 0 which is what we turn displaced-string into after adjustable-string
    ;; is changed to be too small to hold displaced-string.
    ;; As a possible fix, we could distinguish between safe and unsafe code,
    ;; never do the sxhash transforms in safe code, and have the full call to
    ;; sxhash always check for "obsolete" strings.
    ;; I would guess that all sorts of string transforms are similarly
    ;; suspect in this edge case.
    ;; On the one hand, this is undefined behavior as per CLHS:
    ;;  "If A is displaced to B, the consequences are unspecified if B is adjusted
    ;;   in such a way that it no longer has enough elements to satisfy A."
    ;; But on the other, we always try to be maximally helpful,
    ;; and it's extremely dubious that we're totally silent here.
    ;; Also the same issue exists with bit-vectors.
    (assert-error (sxhash displaced-string))))

(with-test (:name :array-psxhash-non-consing :skipped-on :interpreter
            :fails-on :ppc64)
   (let ((a (make-array 1000 :element-type 'double-float
                        :initial-element (+ 0d0 #+(or arm64 x86-64)
                                                1d300))))
     (ctu:assert-no-consing (sb-int:psxhash a))))

(with-test (:name :array-psxhash)
  (let ((table (make-hash-table :test 'equalp)))
    (let ((x (vector 1.0d0 1.0d0))
          (y (make-array 2 :element-type 'double-float :initial-contents '(1.0d0 1.0d0))))
      (setf (gethash x table) t)
      (assert (gethash y table)))))


#|
;;; Our SXHASH has kinda bad behavior on 64-bit fixnums.
;;; I wonder if we should try to hash fixnum better for users.
;;; Example:
* (dotimes (i 20)
    (let ((a (+ sb-vm:dynamic-space-start (* i 32768))))
      (format t "~4d ~x ~b~%" i a (sxhash a))))
   0 1000000000 1000010010001101110100101010000110101100010111010111001001010
   1 1000008000 1000010010001101110100101010000110101100000111110111001001010
   2 1000010000 1000010010001101110100101010000110101100110110010111001001010
   3 1000018000 1000010010001101110100101010000110101100100110110111001001010
   4 1000020000 1000010010001101110100101010000110101101010101010111001001010
   5 1000028000 1000010010001101110100101010000110101101000101110111001001010
   6 1000030000 1000010010001101110100101010000110101101110100010111001001010
   7 1000038000 1000010010001101110100101010000110101101100100110111001001010
   8 1000040000 1000010010001101110100101010000110101110010011010111001001010
   9 1000048000 1000010010001101110100101010000110101110000011110111001001010
  10 1000050000 1000010010001101110100101010000110101110110010010111001001010
  11 1000058000 1000010010001101110100101010000110101110100010110111001001010
  12 1000060000 1000010010001101110100101010000110101111010001010111001001010
  13 1000068000 1000010010001101110100101010000110101111000001110111001001010
  14 1000070000 1000010010001101110100101010000110101111110000010111001001010
  15 1000078000 1000010010001101110100101010000110101111100000110111001001010
  16 1000080000 1000010010001101110100101010000110101000011111010111001001010
  ...
|#
(with-test (:name :fixnum-hash-with-more-entropy)
  (flet ((try (hasher)
           (let (hashes)
             (dotimes (i 20)
               (let* ((a (+ #+64-bit sb-vm:dynamic-space-start
                            #-64-bit #xD7C83000
                            (* i 32768)))
                      (hash (funcall hasher a)))
                 ;; (format t "~4d ~x ~v,'0b~%" i a sb-vm:n-word-bits hash)
                 (push hash hashes)))
             ;; Try some 4-bit subsequences of the hashes
             ;; as various positions and make sure that there
             ;; are none that match for all inputs.
             ;; For 64-bit machines, SXHASH yields complete overlap
             ;; of all the test cases at various positions.
             ;; 32-bit doesn't seem to suffer from this.
             (dotimes (position (- sb-vm:n-fixnum-bits 4))
               (let ((field
                      (mapcar (lambda (x) (ldb (byte 4 position) x))
                              hashes)))
                 ;; (print `(,position , (length (delete-duplicates field))))
                 (when #-64-bit t #+64-bit (not (eq hasher 'sxhash))
                   (assert (>= (length (remove-duplicates field)) 8))))))))
    (try 'sxhash)
    (try 'sb-int:murmur-hash-word/fixnum)))

;;; Ensure that all layout-clos-hash values have a 1 somewhere
;;; such that LOGANDing any number of nonzero hashes is nonzero.
(with-test (:name :layout-hashes-constant-1-bit)
  (let ((combined most-positive-fixnum))
    (maphash (lambda (classoid layout)
               (declare (ignore classoid))
               (let ((hash (sb-kernel:layout-clos-hash layout)))
                 (setq combined (logand combined hash))))
             (sb-kernel:classoid-subclasses (sb-kernel:find-classoid 't)))
    (assert (/= 0 combined))))

(defun c-murmur-fmix (word)
  (declare (type sb-vm:word word))
  (alien-funcall (extern-alien #+64-bit "murmur3_fmix64"
                               #-64-bit "murmur3_fmix32"
                               (function unsigned unsigned))
                 word))
(compile 'c-murmur-fmix)
;;; Assert that the Lisp translation of the C murmur hash matches.
;;; This is slightly redundant with the :ADDRESS-BASED-SXHASH-GCING test,
;;; though this one is a strictly a unit test of the hashing function,
;;; and the other is a test that GC does the right thing.
;;; So it's not a bad idea to have both.
(defun murmur-compare (random-state n-iter)
  (let ((limit (1+ sb-ext:most-positive-word)))
    (loop repeat (the fixnum n-iter)
          do
       (let* ((n (random limit random-state))
              (lisp-hash (sb-impl::murmur3-fmix-word n))
              (c-hash (c-murmur-fmix n)))
         (assert (= lisp-hash c-hash))))))
(compile 'murmur-compare)

(with-test (:name :mumur-hash-compare)
  (murmur-compare (make-random-state t) 100000))

(with-test (:name :sap-hash)
  (assert (/= (sxhash (sb-sys:int-sap #x1000))
              (sxhash (sb-sys:int-sap 0))))
  #-interpreter
  (let ((list-of-saps
          (loop for i below 1000 collect (sb-sys:int-sap i))))
    (ctu:assert-no-consing
        (opaque-identity
         (let ((foo 0))
           (dolist (sap list-of-saps foo)
             (setq foo (logxor foo (sxhash sap)))))))))
