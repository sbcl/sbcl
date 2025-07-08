
;;; Try a 40-way tyepcase on frozen structures
(defglobal *typecase-clauses*
    (loop for i below 40
          collect
          `(,(sb-int:symbolicate "DATA-TYPE-" i) ',(sb-int:symbolicate "DATA-HANDLER<" i ">"))))

(defstruct root-structure)
(macrolet ((define-all-structures ()
             `(progn
                ,@(mapcar (lambda (x) `(defstruct (,(car x) (:include root-structure))))
                          *typecase-clauses*))))
  (define-all-structures))
(declaim (freeze-type root-structure))
(defun data-handler<3> (x) (declare (data-type-3 x)) x 'three)
(macrolet ((dispatch (var) `(etypecase ,var ,@*typecase-clauses*)))
  (defun process-data (x)
    (funcall (dispatch x) x)))
(compile 'process-data)
(with-test (:name :phase-typecase-array-of-values)
  ;; should compile to an array index, not a jump table.
  ;; The resulting code used to be over 1KiB in size, now it's 180 bytes or so
  (assert (= (sb-kernel:code-jump-table-words (sb-kernel:fun-code-header #'process-data))
             1))
  (assert (eq (process-data (make-data-type-3)) 'three))
  (assert-error (process-data (make-root-structure)))
  (assert-error (process-data #p"whatever")))
