(in-package "CL-USER")

(defun a-struct-referencer-2 (struct)
  (a-struct-slot struct))

(defun a-class-typep (x)
  (typep x 'a-class))

(defstruct a-struct slot)

(defclass a-class ()
  ())
