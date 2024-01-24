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

(with-test (:name (:list-iterator :from-end))
  (checked-compile-and-assert (:allow-notes nil)
   `(lambda (x)
      (sb-sequence:with-sequence-iterator-functions (next stop value)
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
  (declare (ignore from-end))
  (let* ((index (or start 0))
         (iterator (loop for i from 0
                         for iterator = sequence then (kdr iterator)
                         until (= i index)
                         finally (return iterator)))
         (limit (if end
                    (loop for i from index
                          for limit = iterator then (kdr limit)
                          until (= i end)
                          finally (return limit))
                    (my-list))))
    (values iterator limit nil
            (lambda (sequence iterator from-end)
              (declare (ignore sequence from-end))
              (incf index)
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
            (lambda (sequence iterator)
              (declare (ignore sequence iterator))
              index)
            (lambda (sequence iterator)
              (declare (ignore sequence))
              iterator))))

(with-test (:name :map-into)
  (assert (equal (coerce (map-into (my-list 1 2 3) #'identity '(4 5 6)) 'list)
                 '(4 5 6))))

(with-test (:name (write-sequence :user-defined-sequence))
  (let ((sequence (my-list #\a #\b #\c)))
    (assert (string= "abc"
                     (with-output-to-string (stream)
                       (write-sequence sequence stream))))))

(with-test (:name (read-sequence :user-defined-sequence))
  (let ((sequence (my-list #\a #\b #\c #\d)))
    (with-input-from-string (stream "wxyz")
      (let ((position (read-sequence sequence stream :start 1 :end 3)))
        (assert (eql 3 position))
        (assert (equal '(#\a #\w #\x #\d) (coerce sequence 'list)))))))

;;; example code from "User-extensible sequences in Common Lisp"
;;; (Rhodes, 2007)

(defclass queue (sequence standard-object)
  ((%data :accessor %queue-data) (%pointer :accessor %queue-pointer)))
(defmethod initialize-instance :after ((o queue) &key)
  (let ((head (list nil)))
    (setf (%queue-data o) head (%queue-pointer o) head)))
(defgeneric enqueue (data queue)
  (:argument-precedence-order queue data)
  (:method (data (o queue))
    (setf (cdr (%queue-pointer o)) (list data) (%queue-pointer o) (cdr (%queue-pointer o)))
    o))
(defgeneric dequeue (queue)
  (:method ((o queue))
    (prog1 (cadr (%queue-data o))
      (setf (cdr (%queue-data o)) (cddr (%queue-data o))))))

(defclass funcallable-queue (queue sb-mop:funcallable-standard-object)
  ()
  (:metaclass sb-mop:funcallable-standard-class))
(defmethod initialize-instance :after ((o funcallable-queue) &key)
  (flet ((fun (&optional (new nil new-p)) (if new-p (enqueue new o) (dequeue o))))
    (sb-mop:set-funcallable-instance-function o #'fun)))

(defmethod sequence:length ((o queue)) (length (cdr (%queue-data o))))
(defmethod sequence:elt ((o queue) index) (elt (cdr (%queue-data o)) index))
(defmethod (setf sequence:elt) (new-value (o queue) index) (setf (elt (cdr (%queue-data o)) index) new-value))
(defmethod sequence:make-sequence-like ((o queue) length &key (initial-element nil iep) (initial-contents nil icp))
  (let ((result (make-instance (class-of o))))
    (cond
      ((and iep icp)
       (error "supplied both ~S and ~S to ~S" :initial-element :initial-contents 'make-sequence-like))
      (icp (unless (= (length initial-contents) length)
             (error "length mismatch in ~S" 'make-sequence-like))
           (setf (cdr (%queue-data result)) (coerce initial-contents 'list)
                 (%queue-pointer result) (last (%queue-data result))))
      (t (setf (cdr (%queue-data result)) (make-list length :initial-element initial-element)
               (%queue-pointer result) (last (%queue-data result)))))
    result))
(defmethod sequence:adjust-sequence ((o queue) length &key initial-element (initial-contents nil icp))
  (cond
    ((= length 0)
     (setf (cdr (%queue-data o)) nil (%queue-pointer o) (%queue-data o)))
    (t (sequence:adjust-sequence (%queue-data o) (1+ length) :initial-element initial-element)
       (setf (%queue-pointer o) (last (%queue-data o)))
       (when icp (replace (%queue-data o) initial-contents :start1 1)) o)))
(defmethod sequence:make-simple-sequence-iterator ((q queue) &rest args &key from-end start end)
  (declare (ignore from-end start end))
  (apply #'sequence:make-simple-sequence-iterator (cdr (%queue-data q)) args))
(defmethod sequence:iterator-step ((q queue) iterator from-end)
  (sequence:iterator-step (cdr (%queue-data q)) iterator from-end))
(defmethod sequence:iterator-endp ((q queue) iterator limit from-end)
  (sequence:iterator-endp (cdr (%queue-data q)) iterator limit from-end))
(defmethod sequence:iterator-element ((q queue) iterator)
  (sequence:iterator-element (cdr (%queue-data q)) iterator))
(defmethod (setf sequence:iterator-element) (new-value (q queue) iterator)
  (setf (sequence:iterator-element (cdr (%queue-data q)) iterator) new-value))
(defmethod sequence:iterator-index ((q queue) iterator)
  (sequence:iterator-index (cdr (%queue-data q)) iterator))
(defmethod sequence:iterator-copy ((q queue) iterator)
  (sequence:iterator-copy (cdr (%queue-data q)) iterator))

(with-test (:name (:ilc2007 :fig4a))
  (assert (= (length (coerce '(1 2 3 4) 'queue)) 4)))
(with-test (:name (:ilc2007 :fig4b))
  (assert (= (count 1 (coerce '(1 2 3) 'queue)) 1)))
(with-test (:name (:ilc2007 :fig4c))
  (let ((result (remove-if-not #'oddp (coerce '(1 2 3) 'funcallable-queue))))
    (assert (typep result 'funcallable-queue))
    (assert (eql (type-of result) 'funcallable-queue))
    (assert (= (length result) 2))
    (assert (= (funcall result) 1))
    (assert (= (funcall result) 3))
    (assert (= (length result) 0))))
(with-test (:name (:ilc2007 :fig4d))
  (let ((result (remove-duplicates (coerce '(1 2 3 4 5) 'queue) :end 4
                                   :key #'oddp :from-end t)))
    (assert (typep result 'queue))
    (assert (eql (type-of result) 'queue))
    (assert (= (length result) 3))
    (assert (equal (coerce result 'list) '(1 2 5)))))
