;; Do not alter this file unless you edit test-driver.lisp to match
(declaim (optimize (debug 3)))
(in-package :cl-user)

(defun one (a b c) (+ a b c))

(defgeneric two (a b))
(defmethod two ((a number) b)
  (* 2 a))

(defstruct three four five)


	    