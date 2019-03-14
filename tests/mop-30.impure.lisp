;;;; Standard-instance-access tests and update-protocol abuse

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

(defclass foo ()
  ((bar :initarg :bar)
   (quux :initarg :quux)))

(defclass foomagic ()
  ())

(defun find-slot (name class)
  (let ((class (sb-pcl:ensure-class-finalized (find-class class))))
    (find name (sb-mop:class-slots class) :key #'sb-mop:slot-definition-name)))

(sb-mop:add-dependent (find-class 'foo) (find-class 'foomagic))

(defglobal **bar-loc** (sb-mop:slot-definition-location (find-slot 'bar 'foo)))
(defglobal **quux-loc** (sb-mop:slot-definition-location (find-slot 'quux 'foo)))

(defmethod sb-mop:update-dependent ((meta (eql (find-class 'foo)))
                                    (dep (eql (find-class 'foomagic)))
                                    &key)
  (setf **bar-loc** (sb-mop:slot-definition-location (find-slot 'bar 'foo))
        **quux-loc** (sb-mop:slot-definition-location (find-slot 'quux 'foo))))

(defun foo-bar/quux (foo)
  (declare (type foo foo))
  (values (sb-mop:standard-instance-access foo **bar-loc**)
          (sb-mop:standard-instance-access foo **quux-loc**)))

(defun swap-bar/quux (foo)
  (declare (type foo foo))
  (rotatef (sb-mop:standard-instance-access foo **bar-loc**)
           (sb-mop:standard-instance-access foo **quux-loc**)))

(with-test (:name (:mop-30 sb-mop:standard-instance-access))
  (let ((bar (cons t t))
        (quux (cons nil nil)))
    (multiple-value-bind (bar? quux?)
        (foo-bar/quux (make-instance 'foo :bar bar :quux quux))
      (assert (eq bar bar?))
      (assert (eq quux quux?)))))

(with-test (:name (:mop-30 (setf sb-mop::standard-instance-access)))
  (let* ((bar (cons t t))
         (quux (cons nil nil))
         (foo
          (make-instance 'foo :bar bar :quux quux)))
    (multiple-value-bind (bar? quux?) (foo-bar/quux foo)
      (assert (eq bar bar?))
      (assert (eq quux quux?)))
    (swap-bar/quux foo)
    (multiple-value-bind (bar? quux?) (foo-bar/quux foo)
      (assert (eq quux bar?))
      (assert (eq bar quux?)))))

;;; Sneaky redefinition reorders slots!
(defclass foo ()
  ((quux :initarg :quux)
   (bar :initarg :bar)))

(with-test (:name (:mop-30 sb-mop:standard-instance-access :updated))
  (let ((bar (cons t t))
        (quux (cons nil nil)))
    (multiple-value-bind (bar? quux?)
        (foo-bar/quux (make-instance 'foo :bar bar :quux quux))
      (assert (eq bar bar?))
      (assert (eq quux quux?)))))

(with-test (:name (:mop-30 sb-mop:standard-instance-access slot-unbound))
  (let ((bar (cons t t)))
    (multiple-value-bind (bar? quux?)
        (foo-bar/quux (make-instance 'foo :bar bar))
      (assert (eq bar bar?))
      (assert (eq sb-pcl:+slot-unbound+ quux?)))))
