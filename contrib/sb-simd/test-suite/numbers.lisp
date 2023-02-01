(in-package #:sb-simd-test-suite)

(defparameter *numbers*
  (remove-duplicates
   `(,most-positive-double-float
     ,most-negative-double-float
     ,least-positive-normalized-double-float
     ,least-negative-normalized-double-float
     ,least-positive-double-float
     ,least-negative-double-float
     ,most-positive-single-float
     ,most-negative-single-float
     ,least-positive-normalized-single-float
     ,least-negative-normalized-single-float
     ,least-positive-single-float
     ,least-negative-single-float
     ,@(loop for type in '(single-float double-float integer)
             append (loop for i from 0 to 64
                          for n = (expt 2 i)
                          collect (coerce n type)
                          collect (coerce (- n) type)
                          collect (coerce (1- n) type)
                          collect (coerce (1+ n) type)
                          collect (coerce (1- (- n)) type)
                          collect (coerce (1+ (- n)) type))))))

(defun numbers-of-type (type)
  (remove-if-not (lambda (x) (typep x type)) *numbers*))
