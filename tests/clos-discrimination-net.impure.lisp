;;;; testing clos discrimination nets

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

(defgeneric foo (x))
(defmethod foo ((x t)) (list t x))
(defmethod foo ((x standard-object)) (list 'standard-object x))
(defmethod foo ((x sb-mop:funcallable-standard-object))
  (list 'sb-mop:funcallable-standard-object x))

(defvar *fun*
  (let* ((net (sb-pcl::generate-discrimination-net
               #'foo (sb-mop:generic-function-methods #'foo) nil nil))
         (form `(lambda (sb-pcl::.arg0.)
                  (macrolet ((sb-pcl::methods ((&rest ms) (&rest types))
                               (declare (ignore types))
                               (let (result)
                                 (dolist (m ms)
                                   (let ((f (sb-mop:method-function m)))
                                     (push `(funcall ,f (list sb-pcl::.arg0.) nil) result)))
                                 `(list ,@(nreverse result)))))
                    ,net))))
    (compile nil form)))

(with-test (:name :standard-cases)
  (assert (equal (foo 1) (list t 1)))
  (assert (equal (funcall *fun* 1) (list (list t 1))))
  (assert (equal (foo (find-class 'standard-method)) (list 'standard-object (find-class 'standard-method))))
  (assert (equal (funcall *fun* (find-class 'standard-method))
                 (list (list 'standard-object (find-class 'standard-method))
                       (list t (find-class 'standard-method)))))
  (assert (equal (foo #'foo) (list 'sb-mop:funcallable-standard-object #'foo)))
  (assert (equal (funcall *fun* #'foo)
                 (list (list 'sb-mop:funcallable-standard-object #'foo)
                       (list 'standard-object #'foo)
                       (list t #'foo)))))

(with-test (:name :pathname-not-standard-object)
  (let ((pathname #p""))
    (assert (not (typep pathname 'standard-object)))
    (assert (equal (foo pathname) (list t pathname)))
    (assert (equal (funcall *fun* pathname) (list (list t pathname))))))

(defstruct s a)

(with-test (:name :struct-not-standard-object)
  (let ((struct (make-s :a 3)))
    (assert (not (typep struct 'standard-object)))
    (assert (equal (foo struct) (list t struct)))
    (assert (equal (funcall *fun* struct) (list (list t struct))))))

(with-test (:name :condition-not-standard-object)
  (let ((condition (make-condition 'style-warning)))
    (assert (not (typep condition 'standard-object)))
    (assert (equal (foo condition) (list t condition)))
    (assert (equal (funcall *fun* condition) (list (list t condition))))))

;;; %METHOD-FUNCTION is a useful FUNCALLABLE-INSTANCE that is
;;; known not to be a FUNCALLABLE-STANDARD-OBJECT, and is available in
;;; every build configuration.
(with-test (:name :method-fast-function-not-funcallable-standard-object)
  (let* ((method (find-method #'foo nil (list (find-class t))))
         (method-function (sb-mop:method-function method)))
    (assert (not (typep method-function 'sb-mop:funcallable-standard-object)))
    (assert (typep method-function 'sb-pcl::%method-function))
    (assert (equal (foo method-function) (list t method-function)))
    (assert (equal (funcall *fun* method-function) (list (list t method-function))))))
