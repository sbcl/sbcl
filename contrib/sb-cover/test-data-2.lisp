(in-package sb-cover-test)

(defun test2 (x)
  (let ((a 0))
    (when (plusp x)
      (incf a))
    a))

;;; This test would show that we do correct detection of non-cons
;;; source forms in non-PROGN-contexts. Which we don't, so this test
;;; is commented out.
#+nil
(defun test2-b (x)
  (let ((a 0))
    (when x
      (incf a))
    a))
