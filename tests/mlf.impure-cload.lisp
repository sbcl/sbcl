(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct fool x))
(eval-when (:compile-toplevel)
  (defmethod make-load-form ((self fool) &optional env)
    (declare (ignore env))
    `(make-fool :x ,(fool-x self)))
  (defparameter *crashy* t)
  (defmethod print-object ((self fool) stream)
    (if *crashy*
        (progn
          (setq *crashy* nil)
          (error "Sorry!"))
        (call-next-method))))

;;; We used to try to "name" everything dumped.
;;; If nothing else, the name should have been written with :READABLY NIL
;;; just in case it was going to signal print-not-readable.
;;; Now we don't print-object at all.
(defvar *foolz*
  '#.(list (make-fool :x 1) (make-fool :x 2)))
