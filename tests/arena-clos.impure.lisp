#+(or gc-stress ;; c-find-heap->arena is not gc-safe
      (not system-tlabs) interpreter) (invoke-restart 'run-tests::skip-file)

(defmethod translate ((x (eql :a)) val) (* 1 val))
(defmethod translate ((x (eql :b)) val) (* 2 val))
(defmethod translate ((x (eql :c)) val) (* 3 val))
(defmethod translate ((x (eql :d)) val) (* 4 val))
(defmethod translate ((x (eql :e)) val) (* 5 val))
(defmethod translate ((x (eql :f)) val) (* 6 val))
(defmethod translate ((x (eql :g)) val) (* 7 val))
(defmethod translate ((x (eql :h)) val) (* 8 val))
(defmethod translate ((x (eql :i)) val) (* 9 val))
(defmethod translate ((x (eql :j)) val) (* 10 val))
(defmethod translate ((x (eql :k)) val) (* 11 val))

(defvar *a* (sb-vm:new-arena 1048576))

(defun f (arg) (sb-vm:with-arena (*a*) (translate arg 3)))

(f :c)
(assert (not (sb-vm:c-find-heap->arena)))

(defmethod zook ((x list))
  (format t "is-list~%"))
(defmethod zook ((x null))
  (format t "is-null~%")
  (call-next-method))
(defmethod zook ((x (eql nil)))
  (format t "is-eql-nil~%")
  (call-next-method))
(defvar *a* (sb-vm:new-arena 1048576))
(defun g ()
  (sb-vm:with-arena (*a*)
    (zook nil)))
(g)
(assert (not (sb-vm:c-find-heap->arena)))
