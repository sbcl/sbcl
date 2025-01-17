;;; WITH-TIMEOUT should accept more than one form in its body.
(with-test (:name (sb-ext:with-timeout :forms) :slow t)
  (handler-bind ((sb-ext:timeout #'continue))
    (sb-ext:with-timeout 3
      (sleep 2)
      (sleep 2))))

;;; SLEEP should not cons except on 32-bit platforms when
;;; (> (mod seconds 1) (* most-positive-fixnum 1e-9))
(with-test (:name (sleep :non-consing)
            :serial t :skipped-on :interpreter
            :fails-on (and :arm :64-bit-time))
  (handler-case (sb-ext:with-timeout 5
                  (ctu:assert-no-consing (sleep 0.00001s0))
                  (locally (declare (notinline sleep))
                    (ctu:assert-no-consing (sleep 0.00001s0))
                    (ctu:assert-no-consing (sleep 0.00001d0))
                    (ctu:assert-no-consing (sleep 1/100000003))))
    (timeout ())))

;;; Changes to make SLEEP cons less led to SLEEP
;;; not sleeping at all on 32-bit platforms when
;;; (> (mod seconds 1) (* most-positive-fixnum 1e-9)).
(with-test (:name (sleep :bug-1194673))
  (assert (eq :timeout
              (handler-case
                  (with-timeout 0.01
                    (sleep 0.6))
                (timeout ()
                  :timeout)))))

;;; SLEEP should work with large integers as well
(with-test (:name (sleep :pretty-much-forever)
            :skipped-on (:and (:or :linux :darwin) :sb-safepoint)) ; hangs
  (assert (eq :timeout
              (handler-case
                  (sb-ext:with-timeout 1
                    (sleep (ash 1 (* 2 sb-vm:n-word-bits))))
                (sb-ext:timeout ()
                  :timeout)))))

;;; Check that SLEEP called with ratios (with no common factors with
;;; 1000000000, and smaller than 1/1000000000) works more or less as
;;; expected.
(with-test (:name (sleep ratio))
  (let ((fun0a (checked-compile '(lambda () (sleep 1/7))))
        (fun0b (checked-compile '(lambda () (sleep 1/100000000000000000000000000))))
        (fun1 (checked-compile '(lambda (x) (sleep x))))
        (start-time (get-universal-time)))
    (sleep 1/7)
    (sleep 1/100000000000000000000000000)
    (funcall fun0a)
    (funcall fun0b)
    (funcall fun1 1/7)
    (funcall fun1 1/100000000000000000000000000)
    #-gc-stress
    (assert (< (- (get-universal-time) start-time) 2))))

(with-test (:name (sleep :return-value))
  (checked-compile-and-assert ()
      `(lambda () (sleep 0.001))
    (() nil)))
