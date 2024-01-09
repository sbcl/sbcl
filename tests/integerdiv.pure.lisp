;;; Assert that all odd divisors less than 32769 have an optimized
;;; remainder coefficient using 32-bit operations.
;;; Symbol hashsets with fewer than that many cells can use fast remainder.
;;; (I could/should implement the 64-bit algorithm I suppose)
(with-test (:name :optimized-remainder-exists)
  (loop for divisor from 3 below 32769 by 2
        do
     (multiple-value-bind (mask c) (sb-impl::optimized-symtbl-remainder-params divisor)
       (assert (and (plusp c) (plusp mask))))))

#+interpreter (invoke-restart 'run-tests::skip-file)

(defvar *rs* (make-random-state t)) ; get a random random-state

#+x86-64
(with-test (:name :udiv-magic)
  (dotimes (i 1000)
    (let ((divisor (+ 5 (random #xfffffff *rs*))))
      (multiple-value-bind (m a s) (sb-c:compute-udiv32-magic divisor)
        (dotimes (i 10000)
          (let* ((dividend (random (ash 1 32) *rs*))
                 (quotient (sb-vm::udiv32-via-multiply dividend m a s))
                 (expected (truncate dividend divisor)))
            (assert (= quotient expected))))))))

#+x86-64
(with-test (:name :urem-magic)
  (dotimes (i 1000)
    (let ((divisor (+ 5 (random #xfffffff *rs*))))
      (multiple-value-bind (m a s) (sb-c:compute-udiv32-magic divisor)
        (dotimes (i 10000)
          (let* ((dividend (random (ash 1 32) *rs*))
                 (remainder (sb-vm::urem32-via-multiply dividend divisor m a s))
                 (expected (rem dividend divisor)))
            (assert (= remainder expected))))))))

(with-test (:name :urem32-ultrafast)
  (dotimes (i 1000)
    ;; The 32-bit algorithm can only handle random 16-bit inputs (or some inputs
    ;; with more than 16 bits, but it's random which inputs are OK)
    ;; A 64-bit variant would handle all 32-bit inputs, but I don't need it.
    (let* ((divisor (+ 5 (random #xffff *rs*)))
           (c (sb-c:compute-fastrem-coefficient divisor 16 32)))
      (dotimes (i 20000)
        (let* ((dividend (random (ash 1 16) *rs*))
               (remainder (sb-vm::fastrem-32 dividend c divisor))
               (expected (rem dividend divisor)))
          (assert (= remainder expected)))))))

#+64-bit
(with-test (:name :urem64-ultrafast)
  (dotimes (i 1000)
    (let* ((divisor (+ 5 (random #xfffffff *rs*)))
           (c (sb-c:compute-fastrem-coefficient divisor 32 64)))
      (dotimes (i 20000)
        (let* ((dividend (random (ash 1 32) *rs*))
               (remainder (sb-vm::fastrem-64 dividend c divisor))
               (expected (rem dividend divisor)))
          (assert (= remainder expected)))))))

;;;; The assembly code for the benchmarks contains no full calls.
;;;; Therefore we're measuring the speed of the vops as accurately as possible.

;;; These loops show the advantage of division by using multiplication
;;; even when the CPU directly implements division.
;;; BASIC-WAY takes about 4 to 6 times as many CPU cycles as TRICKY-WAY.
;;; But these loops are too time-consuming for a regression test.
(defun div-basic-way (&aux (result 0))
  (declare (fixnum result))
  (loop for divisor from 3 to 1000
        do (dotimes (dividend #xfffff)
             (let ((ans (truncate dividend divisor)))
               (setq result (logxor result ans)))))
  result)

#+x86-64
(defun div-tricky-way (&aux (result 0))
  (declare (fixnum result))
  (loop for divisor from 3 to 1000
        do (multiple-value-bind (m a s) (sb-c:compute-udiv32-magic divisor)
             (dotimes (dividend #xfffff)
               (let ((ans (sb-vm::udiv32-via-multiply dividend m a s)))
                 (setq result (logxor result ans))))))
  result)

;;; * (time (rem-basic-way))
;;; Evaluation took:
;;;   30.123 seconds of real time
;;;   30.112920 seconds of total run time (30.112919 user, 0.000001 system)
;;;   99.97% CPU
;;;   84,142,880,093 processor cycles
(defun rem-basic-way (&aux (result 0)) ; about 32 CPU cycles per operation
  (declare (fixnum result))
  (loop for divisor from 3 to 10000
        do (dotimes (dividend #x3ffff)
             (let ((ans (rem dividend divisor)))
               (setq result (logxor result ans)))))
  result)

;;; * (time (rem-tricky-way))
;;; Evaluation took:
;;;   9.999 seconds of real time
;;;   9.998409 seconds of total run time (9.994458 user, 0.003951 system)
;;;   99.99% CPU
;;;   27,938,111,662 processor cycles
#+x86-64
(defun rem-tricky-way (&aux (result 0)) ; about 11 CPU cycles per operation
  (declare (fixnum result))
  (loop for divisor from 3 to 10000
        do (multiple-value-bind (m a s) (sb-c:compute-udiv32-magic divisor)
             (dotimes (dividend #x3ffff)
               (let ((ans (sb-vm::urem32-via-multiply dividend divisor m a s)))
                 (setq result (logxor result ans))))))
  result)

;;; * (time (ultrafastrem-way))
;;; Evaluation took:
;;;   6.175 seconds of real time
;;;   6.171059 seconds of total run time (6.171059 user, 0.000000 system)
;;;   99.94% CPU
;;;   17,243,611,816 processor cycles
(defun ultrafastrem-way (&aux (result 0)) ; about 7 CPU cycles per operation
  (declare (fixnum result))
  (loop for divisor from 3 to 10000
        do (let ((c (sb-c:compute-fastrem-coefficient divisor 18 32)))
             (dotimes (dividend #x3ffff)
               (let ((ans (sb-vm::fastrem-32 dividend c divisor)))
                 (setq result (logxor result ans))))))
  result)

;;; This is the generalized code for the ultrafast way, which isn't actually
;;; ultrafast, but it shows the correctness of the algorithm at various precisions.
(defun try-fastrem (divisor nbits)  ;; nbits = number of bits in numerator
  (macrolet ((try (fraction-bits)
               `(multiple-value-bind (fraction-bits c)
                    (sb-c:compute-fastrem-coefficient divisor nbits ,fraction-bits)
                  ;;(format t "~&F=~D~%" fraction-bits)
                  (dotimes (numerator (ash 1 nbits)) ; exhaustively test all possible numerators
                    (let* ((frac (ldb (byte fraction-bits 0) (* c numerator)))
                           (answer (ash (* frac divisor) (- fraction-bits)))
                           (expect  (mod numerator divisor)))
                      (unless (= answer expect)
                        (format t "~12,'0b ~12,'0b  ~3d ~3d~%"
                                numerator frac answer (mod numerator divisor))))))))
    (try :variable)
    ;(try 16)
    (try 32)))

(defun try-fastrem-bigly ()
  (loop for divisor from 3 to 10000
        do (format t "divisor=~d~%" divisor)
            (try-fastrem divisor 18)))
