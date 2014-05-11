(defpackage "SB-MPFR-TESTS"
  (:use "COMMON-LISP" "SB-RT"))

(in-package "SB-MPFR-TESTS")

(defun sample ()
  (let ((sb-mpfr:*mpfr-rnd* :MPFR_RNDD))
    (sb-mpfr:with-precision 200
      (let ((u (sb-mpfr:coerce 1.0d0 'sb-mpfr:mpfr-float)))
        (loop for i from 1 to 100
              for v = u then (sb-mpfr:mul v i :MPFR_RNDU)
              collect (sb-mpfr:div u v)
                into vals
              finally (return (sb-mpfr:sum (cons u vals))))))))

(defun sample2 ()
  (let ((sb-mpfr:*mpfr-rnd* :MPFR_RNDD))
    (sb-mpfr:with-precision 200
      (let* ((u (sb-mpfr:coerce 1.0d0 'sb-mpfr:mpfr-float))
             (s u))
        (loop for i from 1 to 100
              for v = u then (sb-mpfr:mul v i :MPFR_RNDU)
              do (setf s (sb-mpfr:add s (sb-mpfr:div u v)))
              finally (return s))))))

(defun sample-pi ()
  (sb-mpfr:set-precision 400)
  (sb-mpfr:const-pi))
