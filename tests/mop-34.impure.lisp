;;;; Ensuring that COMPUTE-EFFECTIVE-METHOD is usable

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

(defclass foo-generic-function (standard-generic-function)
  ()
  (:metaclass sb-mop:funcallable-standard-class))

(defclass made-method (method)
  ((function :initarg :function :reader sb-mop:method-function)))

(defun make-made-method (form)
  (let ((fun `(lambda (args next-methods)
                (declare (sb-ext:disable-package-locks call-method))
                (macrolet ((call-method (m nexts)
                             (flet ((make-next (next)
                                      (etypecase next
                                        (method next)
                                        ((cons (eql make-method)) (make-made-method (cadr next))))))
                               `(funcall (sb-mop:method-function ,m) args ',(mapcar #'make-next nexts)))))
                  (declare (sb-ext:enable-package-locks call-method))
                  (declare (sb-ext:disable-package-locks call-next-method next-method-p))
                  (flet ((next-method-p () (not (null next-methods)))
                         (call-next-method (&rest args)
                           (let ((next (car next-methods)))
                             (if next
                                 (funcall next args (cdr next-methods))
                                 (error "no next method")))))
                    (declare (ignorable #'next-method-p #'call-next-method))
                    (declare (sb-ext:enable-package-locks call-next-method next-method-p))
                    ,form)))))
    (make-instance 'made-method :function (compile nil fun))))

(defmethod sb-mop:compute-discriminating-function ((gf foo-generic-function))
  (let* ((apo (sb-mop:generic-function-argument-precedence-order gf))
         (nreq (length apo))
         (combin (sb-mop:generic-function-method-combination gf)))
    (lambda (&rest args)
      (let* ((methods (sb-mop:compute-applicable-methods gf (subseq args 0 nreq)))
             (effective-method (sb-mop:compute-effective-method gf combin methods)))
        (let ((fun (compile nil `(lambda (args)
                                   (declare (sb-ext:disable-package-locks call-method))
                                   (macrolet ((call-method (m nexts)
                                                (flet ((make-next (next)
                                                         (etypecase next
                                                           (method next)
                                                           ((cons (eql make-method)) (make-made-method (cadr next))))))
                                                  `(funcall (sb-mop:method-function ,m) args ',(mapcar #'make-next nexts)))))
                                     (declare (sb-ext:enable-package-locks call-method))
                                     ,effective-method)))))
          (funcall fun args))))))

(defgeneric foo (a &key b)
  (:method ((a integer) &key b) (declare (ignore b)) (1+ a))
  (:method ((a string) &key b) (list a b))
  (:method ((a symbol) &key b c) (declare (ignore b)) (list a c))
  (:method :around ((a integer) &key b) (declare (ignore b)) (1+ (call-next-method)))
  (:generic-function-class foo-generic-function))

(with-test (:name (:mop-34 sb-mop:compute-effective-method :interpretable))
  (assert (= (foo 1) 3))
  (assert (= (foo 1 :b 2) 3))
  (assert (equal (foo "a") '("a" nil)))
  (assert (equal (foo "a" :b 2) '("a" 2)))
  (assert (equal (foo 'a) '(a nil)))
  (assert (equal (foo 'a :b 2) '(a nil)))
  (assert (equal (foo 'a :c 2) '(a 2)))
  (assert (equal (foo 'a :b 2 :c 3) '(a 3))))
