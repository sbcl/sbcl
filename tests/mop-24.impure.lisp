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

;;; Some slot-valuish things in combination with user-defined methods

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
      (add-method #',name
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

(defclass super ()
  ((a :initarg :a :initform 3)))
(defclass sub (super)
  ((b :initarg :b :initform 4)))
(defclass subsub (sub)
  ((b :initarg :b :initform 5)
   (a :initarg :a :initform 6)))

;;; reworking of MOP-20 tests, but with slot-valuish things.
(progn
  (defgeneric test-um03 (x))
  (defmethod test-um03 ((x subsub))
    (list* 'subsub (slot-value x 'a) (slot-value x 'b)
           (not (null (next-method-p))) (call-next-method)))
  (def-user-method test-um03 ((x sub))
    (list* 'sub (slot-value x 'a) (slot-value x 'b)
           (not (null (next-method-p))) (call-next-method)))
  (defmethod test-um03 ((x super))
    (list 'super (slot-value x 'a) (not (null (next-method-p))))))

(with-test (:name (:mop-24 1))
  (assert (equal (test-um03 (make-instance 'super)) '(super 3 nil)))
  (assert (equal (test-um03 (make-instance 'sub)) '(sub 3 4 t super 3 nil)))
  (assert (equal (test-um03 (make-instance 'subsub))
                 '(subsub 6 5 t sub 6 5 t super 6 nil))))

(progn
  (defgeneric test-um10 (x))
  (defmethod test-um10 ((x subsub))
    (list* 'subsub (slot-value x 'a) (slot-value x 'b)
           (not (null (next-method-p))) (call-next-method)))
  (defmethod test-um10 ((x sub))
    (list* 'sub (slot-value x 'a) (slot-value x 'b)
           (not (null (next-method-p))) (call-next-method)))
  (defmethod test-um10 ((x super))
    (list 'super (slot-value x 'a) (not (null (next-method-p)))))
  (defmethod test-um10 :after ((x super)))
  (def-user-method test-um10 :around ((x subsub))
    (list* 'around-subsub (slot-value x 'a) (slot-value x 'b)
           (not (null (next-method-p))) (call-next-method)))
  (defmethod test-um10 :around ((x sub))
    (list* 'around-sub (slot-value x 'a) (slot-value x 'b)
           (not (null (next-method-p))) (call-next-method)))
  (defmethod test-um10 :around ((x super))
    (list* 'around-super (slot-value x 'a)
           (not (null (next-method-p))) (call-next-method))))

(with-test (:name (:mop-24 2))
  (assert (equal (test-um10 (make-instance 'super))
                 '(around-super 3 t super 3 nil)))
  (assert (equal (test-um10 (make-instance 'sub))
                 '(around-sub 3 4 t around-super 3 t sub 3 4 t super 3 nil)))
  (assert (equal (test-um10 (make-instance 'subsub))
                 '(around-subsub 6 5 t around-sub 6 5 t around-super 6 t
                   subsub 6 5 t sub 6 5 t super 6 nil))))

(progn
  (defgeneric test-um12 (x))
  (defmethod test-um12 ((x subsub))
    (list* 'subsub (slot-value x 'a) (slot-value x 'b)
           (not (null (next-method-p))) (call-next-method)))
  (defmethod test-um12 ((x sub))
    (list* 'sub (slot-value x 'a) (slot-value x 'b)
           (not (null (next-method-p))) (call-next-method)))
  (defmethod test-um12 ((x super))
    (list 'super (slot-value x 'a) (not (null (next-method-p)))))
  (defmethod test-um12 :after ((x super)))
  (defmethod test-um12 :around ((x subsub))
    (list* 'around-subsub (slot-value x 'a) (slot-value x 'b)
           (not (null (next-method-p))) (call-next-method)))
  (defmethod test-um12 :around ((x sub))
    (list* 'around-sub (slot-value x 'a) (slot-value x 'b)
           (not (null (next-method-p))) (call-next-method)))
  (def-user-method test-um12 :around ((x super))
    (list* 'around-super (slot-value x 'a)
           (not (null (next-method-p))) (call-next-method))))

(with-test (:name (:mop-24 3))
  (assert (equal (test-um12 (make-instance 'super))
                 '(around-super 3 t super 3 nil)))
  (assert (equal (test-um12 (make-instance 'sub))
                 '(around-sub 3 4 t around-super 3 t sub 3 4 t super 3 nil)))
  (assert (equal (test-um12 (make-instance 'subsub))
                 '(around-subsub 6 5 t around-sub 6 5 t around-super 6 t
                   subsub 6 5 t sub 6 5 t super 6 nil))))
