;;; the test is mostly whether this file can be compiled or not
(defun b () (boundp (load-time-value (intern "DERP"))))

(test-util:with-test (:name :boundp-l-t-v-constant)
  (assert (not (b)))
  (progv '(derp) '(yes) (assert (b)))
  (progv '(derp) '() (assert (not (b))))
  (setf (symbol-value 'derp) 'ok)
  (assert (b)))
