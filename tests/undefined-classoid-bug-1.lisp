(in-package "CL-USER")

(defun a-struct-referencer-1 (struct)
  (a-struct-slot struct))

(defun a-class-typep-2 (x)
  (typep x 'a-class))
