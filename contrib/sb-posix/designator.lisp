(in-package :sb-posix)
(defvar *designator-types* (make-hash-table :test #'equal))

(defmacro define-designator (name (lisp-type alien-type) doc
                             &body conversions)
  (let ((designator-type `(or ,@(mapcar #'car conversions)))
        (designator-name (intern (format nil "~A-~A"
                                         (symbol-name name)
                                  (symbol-name :designator))
                                 #.*package*))
        (name (intern (symbol-name name) :sb-posix)))
    `(progn
       (deftype ,name ()
         ,@(when doc (list (first doc)))
         ',lisp-type)
       (deftype ,designator-name ()
         ,@(when doc (list (second doc)))
         ',designator-type)
      (eval-when (:compile-toplevel :load-toplevel :execute)
        (setf (gethash ',name *designator-types*) ',alien-type))
      (declaim (ftype (function (t) (values ,lisp-type &optional)) ,name))
      (defun ,name (,name)
        ,@(when doc (list (third doc)))
        (etypecase ,name
          ,@conversions)))))

