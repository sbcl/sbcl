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

(in-package :cl-user)

;;; +MAGIC-HASH-VECTOR-VALUE+ is used to mark empty entries in the slot
;;; HASH-VECTOR of hash tables. It must be a value outside of the range
;;; of SXHASH. The range of SXHASH is the non-negative fixnums.
(with-test (:name :magic-hash-vector-value)
  (assert (not (typep sb-impl::+magic-hash-vector-value+
                      '(and fixnum unsigned-byte)))))

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
  (let ((win 0) (n-trials 10) (prev (sb-impl::address-based-counter-val)))
    (dotimes (i n-trials)
      (declare (notinline cons)) ; it's flushable, but don't flush it
      (cons 1 2)
      (let ((ptr (sb-impl::address-based-counter-val)))
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
