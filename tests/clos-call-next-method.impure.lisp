;;;; Testing CALL-NEXT-METHOD.

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

;;; CALL-NEXT-METHOD arguments are only fully checked on high safety.
(declaim (optimize (safety 3)))

;;; Make sure CALL-NEXT-METHOD calls that result in different sets of
;;; applicable methods signal errors.

(defgeneric different-applicable-methods (thing)
  (:method ((thing t))
    (list 't thing))
  (:method ((thing null))
    (list 'null thing))
  (:method ((thing list))
    (list 'list (call-next-method (rest thing))))
  (:method ((thing cons))
    (list 'cons (call-next-method (rest thing)))))

(with-test (:name (call-next-method :different-applicable-methods))
  (assert (equal (different-applicable-methods '(1 2 3)) '(cons (list (t (3))))))
  (assert-error (different-applicable-methods '(1 2)))
  (assert-error (different-applicable-methods '(1)))
  (assert (equal (different-applicable-methods nil) '(null nil))))

;;; Test calling the next method with non-EQL arguments of the same
;;; class.

(defgeneric non-eql-arguments (x)
  (:method ((x t))
    (list 't x))
  (:method ((x number))
    (list 'number (call-next-method (1+ x))))
  (:method ((x real))
    (list 'real (call-next-method (1+ x))))
  (:method ((x integer))
    (list 'integer (call-next-method (1+ x)))))

(with-test (:name (call-next-method :same-applicable-methods :non-eql-arguments))
  (assert (equal (non-eql-arguments 1) '(integer (real (number (t 4))))))
  (assert (equal (non-eql-arguments 1/2) '(real (number (t 5/2)))))
  (assert (equal (non-eql-arguments #C(1 2)) '(number (t #C(2 2))))))
