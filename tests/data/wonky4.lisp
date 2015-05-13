(in-package :cl-user)

(defclass foo ()
  ((name :reader name)))

(defvar *foo* nil)

(defun foo (x)
  (1+ x))

(warn "gotcha")
"junk"
#| hey |#
;;;
#-sbcl (frob)
 (name *foo*)

(defun bar (y)
  (1- y))
