(unless (vop-existsp "SB-KERNEL:%RAW-INSTANCE-CAS/SIGNED-WORD")
  (invoke-restart 'run-tests::skip-file))

(defstruct s (f 0 :type fixnum) (w 0 :type word) (sw 0 :type signed-word))

(defun swapw (mystruct v) (sb-vm:%atomic-exchange (s-w mystruct) v))
(defun swapsw (mystruct v) (sb-vm:%atomic-exchange (s-sw mystruct) v))
(defun swapf (mystruct v) (sb-vm:%atomic-exchange (s-f mystruct) v))

(mapc 'compile '(swapw swapsw swapf))

(with-test (:name :atomic-exchange-word)
  (let ((s (make-s)))
    (assert (= (swapsw s 5) 0))
    (assert (= (s-sw s) 5))
    (assert (= (swapsw s 1000) 5))
    (assert (= (s-sw s) 1000))))

(with-test (:name :atomic-exchange-signed-word)
  (let ((s (make-s)))
    (assert (= (swapsw s 5) 0))
    (assert (= (s-sw s) 5))
    (assert (= (swapsw s -100) 5))
    (assert (= (s-sw s) -100))))

(with-test (:name :atomic-exchange-fixnum)
  (let ((s (make-s)))
    (assert (= (swapf s 5) 0))
    (assert (= (s-f s) 5))
    (assert (= (swapf s -100) 5))
    (assert (= (s-f s) -100))))
