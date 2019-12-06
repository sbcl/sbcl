;;;; Tests related to extended sequences.

;;;; This file is impure because we want to be able to define methods
;;;; implementing the extended sequence protocol.

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

(with-test (:name (sb-kernel:extended-sequence subtypep :relations))
  (flet ((test-case (type1 type2)
           (assert (equal '(nil t)
                          (multiple-value-list (subtypep type1 type2))))))
    (subtypep 'sb-kernel:extended-sequence 'sb-kernel:instance)
    (subtypep 'sb-kernel:instance 'sb-kernel:extended-sequence)

    (subtypep 'sb-kernel:extended-sequence 'sb-kernel:funcallable-instance)
    (subtypep 'sb-kernel:funcallable-instance 'sb-kernel:extended-sequence)))

;;; For the following situation:
;;; - result type is a type specifier designating a DEFTYPEd type
;;; - the type expands to a the name of a user-defined sequence class
;;; - not all mandatory sequence protocol methods are define for the
;;;   user-define sequence class
;;; MAKE-SEQUENCE used to signal a SIMPLE-TYPE-ERROR referring to the
;;; unexpanded type specifier, instead of signaling a
;;; SEQUENCE:PROTOCOL-UNIMPLEMENTED error.
(defclass bug-1315846-simple-sequence (sequence) ())

(deftype bug-1315846-sequence ()
  'bug-1315846-simple-sequence)

(with-test (:name (make-sequence :result-type deftype :bug-1315846))
  (assert-error (make-sequence 'bug-1315846-sequence 10)
                sequence::protocol-unimplemented))

(with-test (:name (map :result-type deftype :bug-1315846))
  (assert-error (map 'bug-1315846-sequence #'1+ '(1 2 3))
                sequence::protocol-unimplemented))

(with-test (:name (merge :result-type deftype :bug-1315846))
  (assert-error (merge 'bug-1315846-sequence (list 1 2 3) (list 4 5 6) #'<)
                sequence::protocol-unimplemented))

(with-test (:name (concatenate :result-type deftype :bug-1315846))
  (assert-error (concatenate 'bug-1315846-sequence '(1 2) '(3 4))
                sequence::protocol-unimplemented))

(defclass extended-sequence (sequence standard-object) ())

(defmethod sequence:length ((sequence extended-sequence))
  3)

(defmethod sequence:make-sequence-like ((sequence extended-sequence) (length t)
                                        &key &allow-other-keys)
  (make-instance 'extended-sequence))

(defmethod (setf sequence:elt) ((new-value t) (sequence extended-sequence) (index t))
  new-value)

(with-test (:name (map :result-creation))
  (assert (typep (map 'extended-sequence #'1+ '(1 2 3)) 'extended-sequence)))

(with-test (:name (make-sequence :result-type class))
  (assert (typep (make-sequence (find-class 'extended-sequence) 3)
                 'extended-sequence)))

(with-test (:name (map :result-type class))
  (assert (typep (map (find-class 'extended-sequence)
                      #'1+ '(1 2 3))
                 'extended-sequence)))

(with-test (:name (merge :result-type class))
  (assert (typep (merge (find-class 'extended-sequence)
                        (list 1 2 3) (list 4 5 6) #'<)
                 'extended-sequence)))

(with-test (:name (concatenate :result-type class))
  (assert (typep (concatenate (find-class 'extended-sequence) '(1 2) '(3 4))
                 'extended-sequence)))

(with-test (:name :list-iterator-from-end)
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (sb-sequence:with-sequence-iterator-functions
          (next stop value _set _index _copy)
          (x :from-end t)
        (loop until (stop) collect (value) do (next))))
   (('(a b c d)) '(d c b a) :test #'equal)))

(defclass my-list (sequence standard-object)
  ((%nilp :initarg :nilp :initform nil :accessor nilp)
   (%kar :initarg :kar :accessor kar)
   (%kdr :initarg :kdr :accessor kdr)))

(defun my-list (&rest elems)
  (if (null elems)
      (load-time-value (make-instance 'my-list :nilp t) t)
      (make-instance 'my-list
        :kar (first elems) :kdr (apply #'my-list (rest elems)))))

(defmethod sequence:length ((sequence my-list))
  (if (nilp sequence)
      0
      (1+ (length (kdr sequence)))))

(defmethod sequence:make-sequence-iterator
    ((sequence my-list) &key from-end start end)
  (declare (ignore from-end start end))
  (values sequence (my-list) nil
          (lambda (sequence iterator from-end)
            (declare (ignore sequence from-end))
            (kdr iterator))
          (lambda (sequence iterator limit from-end)
            (declare (ignore sequence from-end))
            (eq iterator limit))
          (lambda (sequence iterator)
            (declare (ignore sequence))
            (kar iterator))
          (lambda (new sequence iterator)
            (declare (ignore sequence))
            (setf (kar iterator) new))
          (constantly 0)
          (lambda (sequence iterator)
            (declare (ignore sequence))
            iterator)))

(with-test (:name :map-into)
  (assert (equal (coerce (map-into (my-list 1 2 3) #'identity '(4 5 6)) 'list)
                 '(4 5 6))))
