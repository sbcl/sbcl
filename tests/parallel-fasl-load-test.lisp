(defun one-fun ()
  1)

(defun two-fun ()
  2)

(defvar *var* 42 "This is var.")

(defparameter *quux* 13 "This is quux.")

(defclass a-class ()
  ((slot :initarg :slot :reader a-slot)))

(defgeneric gen-fun (x)
  (:method ((a cons)) 'cons))

(defmethod gen-fun ((a a-class)) 'a-class)
