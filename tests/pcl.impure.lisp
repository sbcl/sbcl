(defpackage "FOO"
  (:use "CL"))
(in-package "FOO")

;;;; It should be possible to do DEFGENERIC and DEFMETHOD referring to
;;;; structure types defined earlier in the file.

(defstruct struct-a x y)
(defstruct struct-b x y z)

(defmethod wiggle ((a struct-a))
  (+ (struct-a-x a)
     (struct-a-y a)))
(defgeneric jiggle ((arg t)))
(defmethod jiggle ((a struct-a))
  (- (struct-a-x a)
     (struct-a-y a)))
(defmethod jiggle ((b struct-b))
  (- (struct-b-x b)
     (struct-b-y b)
     (struct-b-z b)))

(assert (= (wiggle (make-struct-a :x 6 :y 5))
           (jiggle (make-struct-b :x 19 :y 6 :z 2))))

;;; Compiling DEFGENERIC should prevent "undefined function" style warnings
;;; from code within the same file.

(defgeneric gf-defined-in-this-file ((x number) (y number)))
(defun function-using-gf-defined-in-this-file (x y n)
  (unless (minusp n)
    (gf-defined-in-this-file x y)))
