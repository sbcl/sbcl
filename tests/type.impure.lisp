(in-package :cl-user)

(load "assertoid.lisp")

(let ((types '(character
	       integer fixnum (integer 0 10)
	       single-float (single-float -1.0 1.0) (single-float 0.1)
	       (real 4 8) (real -1 7) (real 2 11)
	       (member #\a #\b #\c) (member 1 #\a) (member 3.0 3.3))))
  (dolist (i types)
    (format t "type I=~S~%" i)
    (dolist (j types)
      (format t "  type J=~S~%" j)
      (assert (subtypep i `(or ,i ,j)))
      (assert (subtypep i `(or ,j ,i)))
      (assert (subtypep i `(or ,i ,i ,j)))
      (assert (subtypep i `(or ,j ,i))))))

(defun type-evidently-= (x y)
  (and (subtypep x y)
       (subtypep y x)))

(assert (subtypep 'single-float 'float))

(assert (type-evidently-= '(integer 0 10) '(or (integer 0 5) (integer 4 10))))

;;; sbcl-0.6.10 did (UPGRADED-ARRAY-ELEMENT-TYPE 'SOME-UNDEF-TYPE)=>T
;;; and (UPGRADED-COMPLEX-PART-TYPE 'SOME-UNDEF-TYPE)=>T.
(assert (raises-error? (upgraded-array-element-type 'some-undef-type)))
(assert (eql (upgraded-array-element-type t) t))
(assert (raises-error? (upgraded-complex-part-type 'some-undef-type)))
(assert (subtypep (upgraded-complex-part-type 'fixnum) 'real))

;;; Do reasonable things with undefined types, and with compound types
;;; built from undefined types.
;;;
;;; part I: TYPEP
(assert (typep #(11) '(simple-array t 1)))
(assert (typep #(11) '(simple-array (or integer symbol) 1)))
(assert (raises-error? (typep #(11) '(simple-array undef-type 1))))
(assert (not (typep 11 '(simple-array undef-type 1))))
;;; part II: SUBTYPEP
(assert (subtypep '(vector some-undef-type) 'vector))
(assert (not (subtypep '(vector some-undef-type) 'integer)))
(macrolet ((nilnil (expr)
	     `(assert (equal '(nil nil) (multiple-value-list ,expr)))))
  (nilnil (subtypep 'utype-1 'utype-2))
  (nilnil (subtypep '(vector utype-1) '(vector utype-2)))
  (nilnil (subtypep '(vector utype-1) '(vector t)))
  (nilnil (subtypep '(vector t) '(vector utype-2))))

;;; success
(quit :unix-status 104)
