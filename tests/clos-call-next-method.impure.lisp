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

;;; Utilities
;;;
;;; It makes sense to cover all permutations of arguments since the
;;; order of calls can affect the construction of the CALL-NEXT-METHOD
;;; argument checker.

;;; Assumes unique elements in SEQUENCE.
(defun map-permutations (function sequence)
  (labels ((rec (partial-permutation remainder)
             (if (null remainder)
                 (funcall function partial-permutation)
                 (map nil (lambda (element)
                            (rec (list* element partial-permutation)
                                 (remove element remainder)))
                      remainder))))
    (rec '() sequence)))

;;; RESET-FUNCTION is used to reset the generic function between
;;; permutations so caches are built up from scratch according to the
;;; following call sequence.
(defun map-test-case-permutations (reset-function check-function test-cases)
  (map-permutations
   (lambda (permutation)
     (funcall reset-function)
     (map nil (lambda (arguments-and-expected)
                (destructuring-bind (arguments expected) arguments-and-expected
                  (funcall check-function arguments expected)))
          permutation))
   test-cases))

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
  (map-test-case-permutations
   (lambda ()
     (sb-pcl::update-dfun #'different-applicable-methods ))
   (lambda (arguments expected)
     (flet ((do-it ()
              (apply #'different-applicable-methods arguments)))
       (case expected
         (error (assert-error (do-it)))
         (t (assert (equal (do-it) expected))))))
   '((((1 2 3)) (cons (list (t (3)))))
     (((1 2))   error)
     (((1))     error)
     ((nil)     (null nil)))))

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
  (map-test-case-permutations
   (lambda ()
     (sb-pcl::update-dfun #'non-eql-arguments))
   (lambda (arguments expected)
     (assert (equal (apply #'non-eql-arguments arguments) expected)))
   '(((1)       (integer (real (number (t 4)))))
     ((1/2)     (real (number (t 5/2))))
     ((#C(1 2)) (number (t #C(2 2)))))))

;;; Test EQL specializers which always require a dedicated method in
;;; the CALL-NEXT-METHOD argument checker.

(defgeneric eql-specializer (x)
  (:method ((x t))
    (list 't x))
  (:method ((x number))
    (list 'number (call-next-method (1+ x))))
  (:method ((x real))
    (list 'real (call-next-method (1+ x))))
  (:method ((x integer))
    (list 'integer (call-next-method (1+ x))))
  (:method ((x (eql 4)))
    (list 'eql 4 (call-next-method (1+ x))))
  (:method ((x (eql 5)))
    (list 'eql 5 (call-next-method (1+ x)))))

(with-test (:name (call-next-method :eql-specializer))
  (map-test-case-permutations
   (lambda ()
     (sb-pcl::update-dfun #'eql-specializer))
   (lambda (arguments expected)
     (flet ((do-it ()
              (apply #'eql-specializer arguments)))
       (case expected
         (error (assert-error (do-it)))
         (t (assert (equal (do-it) expected))))))
   '(((0)       (integer (real (number (t 3)))))
     ((1)       error)
     ;; ((2)       error) ; too slow otherwise (exponential scaling)
     ((3)       error)
     ;; ((4)       error)
     ((5)       error)
     ((6)       (integer (real (number (t 9)))))
     ((1/2)     (real (number (t 5/2))))
     ((#C(1 2)) (number (t #C(2 2))))
     ((:foo)    (t :foo)))))
