;;;; tests of irrational floating point functions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;; 
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(in-package :cl-user)

;;;; old bugs

;;; This used to fail with
;;;   The value -0.44579905382680446d0 is not of type (DOUBLE-FLOAT 0.0d0).
;;; MNA's port of Raymond Toy work on CMU CL fixed this in sbcl-0.6.12.53.
(assert (equal (log #c(0.4 0.5)) #C(-0.44579905 0.8960554)))

;;;; other tests

;;; expt
(assert (equal (expt #c(0 1) 2) -1))
(assert (equal (prin1-to-string (expt 2 #c(0 1))) "#C(0.7692389 0.63896126)"))

;;; log
(assert (equal (prin1-to-string (log -3 10)) "#C(0.47712126 1.3643764)"))
(assert (= (log 3 0) 0))

;;; sqrt, isqrt
(assert (= (sqrt 9) 3.0))
(assert (= (sqrt -9.0) #c(0.0 3.0)))
(assert (= (isqrt 9) 3))
(assert (= (isqrt 26) 5))


;;; sin, sinh, asin, asinh
(assert (equal (prin1-to-string (sin (* 8 (/ pi 2)))) "-4.898425415289509d-16"))
(assert (equal (prin1-to-string (sin (expt 10 3))) "0.82687956"))
(assert (= (sinh 0) 0.0))
(assert (equal (prin1-to-string (sinh #c(5.0 -9.6)))
               "#C(-73.06699 12.936809)"))
(assert (= (sin (* #c(0 1) 5)) (* #c(0 1) (sinh 5))))
(assert (= (sinh (* #c(0 1) 5)) (* #c(0 1) (sin 5))))
(assert (equal (prin1-to-string (asin -1)) "-1.5707964"))
(assert (= (asin 0) 0.0))
(assert (= (asin 2) #c(1.5707964 -1.3169578)))
(assert (equal (prin1-to-string (asinh 0.5)) "0.4812118"))
(assert (equal (prin1-to-string (asinh 3/7)) "0.41643077"))

;;; cos, cosh, acos, acosh
(assert (=  (cos 0) 1.0))
(assert (equal (prin1-to-string (cos (/ pi 2))) "6.123031769111886d-17"))
(assert (= (cosh 0) 1.0))
(assert (equal (prin1-to-string (cosh 1)) "1.5430807"))
(assert (= (cos (* #c(0 1) 5)) (cosh 5)))
(assert (= (cosh (* #c(0 1) 5)) (cos 5)))
(assert (equal (prin1-to-string (acos 0)) "1.5707964"))
(assert (equal (prin1-to-string (acos -1)) "3.1415927"))
(assert (equal (prin1-to-string (acos 2)) "#C(0.0 1.3169578)"))
(assert (= (acos 1.00001) #c(0.0 0.0044751678)))
(assert (= (acosh 0) #c(0 1.5707964)))
(assert (= (acosh 1) 0))
(assert (= (acosh -1) #c(0 3.1415927)))

;;; tan, tanh
(assert (equal (prin1-to-string (tan 1)) "1.5574077"))
(assert (equal (prin1-to-string (tan (/ pi 2))) "1.6331778728383844d+16"))
(assert (equal (prin1-to-string (tanh 0.00753)) "0.0075298576"))
(assert (= (tanh 50) 1.0))
(assert (= (tan (* #c(0 1) 5)) (* #c(0 1) (tanh 5))))
(assert (= (atan 1) 0.7853982))
(assert (equal (prin1-to-string (atanh 0.5) ) "0.54930615"))
(assert (equal (prin1-to-string (atanh 3/7)) "0.45814538"))
