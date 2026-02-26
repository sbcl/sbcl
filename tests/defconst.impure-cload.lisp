;; non-toplevel DEFCONSTANT

(defun make-a-constant () (defconstant zook 93))

(defun constify-boundp ()
  (let ((zook 'haha))
    (declare (special zook))
    (make-a-constant)))

;; If could be argued that a thread-local binding should make DEFCONSTANT fail.
;; Agree or not, one must concur that successful completion of DEFCONSTANT should not leave
;; a symbol globally unbound. Unfortunately I don't see an efficient way to make this test
;; pass on threadless builds without scanning the whole binding stack in %DEFCONSTANT.
(test-util:with-test (:name :defconstant-when-specially-boundp
                      :fails-on (:not :sb-thread))
  (constify-boundp)
  (assert (eq (symbol-value 'zook) 93)))
