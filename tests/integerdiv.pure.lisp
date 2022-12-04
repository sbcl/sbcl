#+(or (not x86-64) interpreter) (invoke-restart 'run-tests::skip-file)

;;; Theoretiacally some of our hash-table-like structures,
;;; most notably PACKAGE-HASHTABLE, which use a prime-number-sized
;;; storage vector could utilize precomputed magic numbers
;;; to perform the REM operation by computing the magic parameters
;;; whenever the table is resized.

(defvar *rs* (make-random-state t)) ; get a random random-state

(with-test (:name :udiv-magic)
  (dotimes (i 1000)
    (let ((divisor (+ 5 (random #xfffffff *rs*))))
      (multiple-value-bind (m a s) (sb-c:compute-udiv32-magic divisor)
        (dotimes (i 10000)
          (let* ((dividend (random (ash 1 32) *rs*))
                 (quotient (sb-vm::udiv32-via-multiply dividend m a s))
                 (expected (truncate dividend divisor)))
            (assert (= quotient expected))))))))

;;; These loops show the advantage of division by using multiplication
;;; even when the CPU directlly implements division.
;;; BASIC-WAY takes about 4 to 6 times as many CPU cycles as TRICKY-WAY.
;;; But these loops are too time-consuming for a regression test.

(defun basic-way (&aux (result 0))
  (declare (fixnum result))
  (loop for divisor from 3 to 1000
        do (dotimes (dividend #xfffff)
             (let ((ans (truncate dividend divisor)))
               (setq result (logxor result ans)))))
  result)

(defun tricky-way (&aux (result 0))
  (declare (fixnum result))
  (loop for divisor from 3 to 1000
        do (multiple-value-bind (m a s) (sb-c:compute-udiv32-magic divisor)
             (dotimes (dividend #xfffff)
               (let ((ans (sb-vm::udiv32-via-multiply dividend m a s)))
                 (setq result (logxor result ans))))))
  result)
