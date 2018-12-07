;;;; miscellaneous side-effectful tests of the MOP

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

;;; this file tests that user-defined methods can be used in
;;; combination (ahem) with hairy bits of method-combination.

;;; Simple test case from Pascal Costanza
(defgeneric test (arg)
  (:method (arg) (format t "~D" arg) arg))

(defun define-around-test ()
  (multiple-value-bind
        (method-lambda method-args)
      (sb-mop:make-method-lambda
       #'test (sb-mop:class-prototype (sb-mop:generic-function-method-class #'test))
       '(lambda (arg) (call-next-method)) ())
    (let ((method (apply #'make-instance
                         (sb-mop:generic-function-method-class #'test)
                         :qualifiers '(:around)
                         :lambda-list '(arg)
                         :specializers (list (find-class 't))
                         :function (compile nil method-lambda)
                         method-args)))
      (sb-mop:add-method #'test method))))

(defun run-test ()
  (define-around-test)
  (test 42))

(with-test (:name (:mop-20 1))
  (assert (string= (with-output-to-string (*standard-output*)
                     (assert (= (run-test) 42)))
                   "42")))

;;; Slightly more complex test cases, from Bruno Haible (sbcl-devel
;;; 2004-06-11).  First the setup.
(defclass user-method (standard-method) (myslot))

(defmacro def-user-method (name &rest rest)
  (let* ((lambdalist-position (position-if #'listp rest))
         (qualifiers (subseq rest 0 lambdalist-position))
         (lambdalist (elt rest lambdalist-position))
         (body (subseq rest (+ lambdalist-position 1)))
         (required-part
          (subseq lambdalist 0
                  (or (position-if #'(lambda (x)
                                       (member x lambda-list-keywords))
                                   lambdalist)
                      (length lambdalist))))
         (specializers
          (mapcar #'find-class
                  (mapcar #'(lambda (x) (if (consp x) (second x) 't))
                          required-part)))
         (unspecialized-required-part
          (mapcar #'(lambda (x) (if (consp x) (first x) x)) required-part))
         (unspecialized-lambdalist
          (append unspecialized-required-part
                  (subseq required-part (length required-part)))))
    `(progn
      (sb-mop:add-method #',name
       (make-instance 'user-method
        :qualifiers ',qualifiers
        :lambda-list ',unspecialized-lambdalist
        :specializers ',specializers
        :function
        #'(lambda (arguments next-methods-list)
            (flet ((next-method-p () next-methods-list)
                   (call-next-method (&rest new-arguments)
                     (unless new-arguments (setq new-arguments arguments))
                     (if (null next-methods-list)
                         (error "no next method for arguments ~:s" arguments)
                         (funcall (sb-mop:method-function (first next-methods-list))
                                  new-arguments (rest next-methods-list)))))
              (apply #'(lambda ,unspecialized-lambdalist ,@body) arguments)))))
      ',name)))

;;; this one has always worked, as it does not involve MAKE-METHOD in
;;; its effective method.
(progn
  (defgeneric test-um03 (x))
  (defmethod test-um03 ((x integer))
    (list* 'integer x (not (null (next-method-p))) (call-next-method)))
  (def-user-method test-um03 ((x rational))
    (list* 'rational x (not (null (next-method-p))) (call-next-method)))
  (defmethod test-um03 ((x real))
    (list 'real x (not (null (next-method-p))))))

(with-test (:name (:mop-20 2))
  (assert (equal (test-um03 17) '(integer 17 t rational 17 t real 17 nil))))

;;; these two used to fail in slightly different ways
(progn
  (defgeneric test-um10 (x))
  (defmethod test-um10 ((x integer))
    (list* 'integer x (not (null (next-method-p))) (call-next-method)))
  (defmethod test-um10 ((x rational))
    (list* 'rational x (not (null (next-method-p))) (call-next-method)))
  (defmethod test-um10 ((x real))
    (list 'real x (not (null (next-method-p)))))
  (defmethod test-um10 :after ((x real)))
  (def-user-method test-um10 :around ((x integer))
    (list* 'around-integer x (not (null (next-method-p))) (call-next-method)))
  (defmethod test-um10 :around ((x rational))
    (list* 'around-rational x (not (null (next-method-p))) (call-next-method)))
  (defmethod test-um10 :around ((x real))
    (list* 'around-real x (not (null (next-method-p))) (call-next-method))))

(with-test (:name (:mop-20 3))
  (assert (equal (test-um10 17)
                 '(around-integer 17 t
                   around-rational 17 t
                   around-real 17 t
                   integer 17 t
                   rational 17 t
                   real 17 nil))))

(progn
  (defgeneric test-um12 (x))
  (defmethod test-um12 ((x integer))
    (list* 'integer x (not (null (next-method-p))) (call-next-method)))
  (defmethod test-um12 ((x rational))
    (list* 'rational x (not (null (next-method-p))) (call-next-method)))
  (defmethod test-um12 ((x real))
    (list 'real x (not (null (next-method-p)))))
  (defmethod test-um12 :after ((x real)))
  (defmethod test-um12 :around ((x integer))
    (list* 'around-integer x (not (null (next-method-p))) (call-next-method)))
  (defmethod test-um12 :around ((x rational))
    (list* 'around-rational x (not (null (next-method-p))) (call-next-method)))
  (def-user-method test-um12 :around ((x real))
    (list* 'around-real x (not (null (next-method-p))) (call-next-method))))

(with-test (:name (:mop-20 4))
  (assert (equal (test-um12 17)
                 '(around-integer 17 t
                   around-rational 17 t
                   around-real 17 t
                   integer 17 t
                   rational 17 t
                   real 17 nil))))
