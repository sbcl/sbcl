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


;;; Test that (sb-mpfr:coerce * 'rational) works

(defvar *roundtrip-precision* 100)

(defun rational-roundtrip (rat)
  (sb-mpfr:with-precision *roundtrip-precision*
    (let* ((f    (sb-mpfr:coerce rat 'sb-mpfr:mpfr-float))
           (frat (sb-mpfr:coerce f 'rational)))
      frat)))

(macrolet ((%write-out-idempotence-tests (&rest rationals)
             `(progn
                ,@(loop :for rat :in rationals
                        :collect `(deftest
                                      ;; name
                                      ,(intern (format nil "TEST-RATIONALIZE-~A" rat))
                                      ;; form to test
                                      (<= (abs (- ,rat (rational-roundtrip ,rat)))
                                          (expt 2 (- *roundtrip-precision*)))
                                    ;; result
                                    t)))))
  (%write-out-idempotence-tests
   1/2
   1/4
   1/8
   1/16
   (+ 1/2 1/4 1/8)
   355/113
   pi
   (exp 1)
   (sqrt 2)))
