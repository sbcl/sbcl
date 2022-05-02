;;; Example from Jan Moringen

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass foo () ()))

(defconstant +foo+
  (if (boundp '+foo+)
      +foo+
      (make-instance 'foo)))

;;; The `(,+foo+) expression was being compile-time-folded to (#<foo
;;; xxxx>). Consequently it was unclear whether the #<foo> inside that
;;; list should require a load-form. We now track whether something
;;; comes from a named constant reference and dump the constant
;;; appropriately.
(defun foo ()
  `(,+foo+))

(defun bar ()
  `#(,+foo+))
