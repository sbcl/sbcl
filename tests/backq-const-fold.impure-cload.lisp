
;;; Example from Jan Moringen

#|
I saw several ways to make this work:

 (1) treat backquote list constructors as foldable only if all list elements
     are of type (OR list number character array symbol).
     (1a) allow those types plus any instance type that has a MAKE-LOAD-FORM method.
 (2) track the provenance of each element so that after folding we can reliably
     detect which came from defconstants. (Is this even possible?)
 (3) do as the old code did: assume that if a value is EQ to a defconstant,
     that implies that we should use symbol-value on that constant.

In the interest of simplicity, I choose (1), as it is maximally conservative
and therefore least likely to raise any kind of concerns as to applicability.
|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass foo () ()))

(defconstant +foo+
  (if (boundp '+foo+)
      +foo+
      (make-instance 'foo)))

;;; The `(,+foo+) expression was being compile-time-folded to (#<foo xxxx>).
;;; Consequently it is unclear whether the #<foo> inside that list should
;;; require a load-form. On the one hand, it's EQ to the defconstant, so
;;; it shouldn't, but on the other hand, it's not "obvious" that the elements
;;; of a random anonymous list came from any particular place,
;;; and that that place should be assumed to contain the value that you want
;;; to stuff into a particular list cell, and that you should be indifferent
;;; to stuffing in the value at compile-time versus load-time.

;;; Unfortunately, this definition of FOO now conses.
;;; As a further improvement, the transform could wrap the computed
;;; list in a LOAD-TIME-VALUE if the elements are constant
;;; but not satisfying TRIVIALLY-EXTERNALIZABLE-P.
(defun foo ()
  `(,+foo+))
