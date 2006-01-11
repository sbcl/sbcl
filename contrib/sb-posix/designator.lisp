(in-package :sb-posix)
(defvar *designator-types* (make-hash-table :test #'equal))

(defmacro define-designator (name result &body conversions)
  (let ((type `(quote (or ,@(mapcar #'car conversions))))
        (typename (intern (format nil "~A-~A"
                                  (symbol-name name)
                                  (symbol-name :designator))
                          #.*package*)))
    `(progn
      (eval-when (:compile-toplevel :load-toplevel :execute)
        (deftype ,typename () ,type)
        (setf (gethash ',name *designator-types*) ',result))
      (defun ,(intern (symbol-name name) :sb-posix) (,name)
        (declare (type ,typename ,name))
        (etypecase ,name
          ,@conversions)))))

