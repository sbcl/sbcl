;;;; Constructing and instantiating subclasses of system classes

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

(defmacro assert-subtype (class type)
  `(progn
     (assert-tri-eq t t (subtypep ',class ',type))
     (assert-instance-type ,class ,type)))

(defmacro assert-instance-type (class type)
  `(flet ((compiled (x) (typep x ',type))
          (evaled (x y) (typep x (opaque-identity y))))
     (assert (typep (make-instance ',class) ',type))
     (assert (compiled (make-instance ',class)))
     (assert (evaled (make-instance ',class) ',type))))

(defmacro assert-callable (class)
  `(let ((i (make-instance ',class)))
     (sb-mop:set-funcallable-instance-function i (lambda (x) (1+ x)))
     (assert (= (funcall i 51) 52))))

(macrolet ((test (stype)
             (let ((sname (intern (concatenate 'string "%" (symbol-name stype))))
                   (fsname (intern (concatenate 'string "%F" (symbol-name stype)))))
               `(progn
                  (defclass ,sname (,stype standard-object)
                    ())
                  (with-test (:name (,stype standard-object))
                    (assert-subtype ,sname ,stype)
                    (assert-instance-type ,sname (not function))
                    (assert-instance-type ,sname (not sequence)))

                  (defclass ,fsname (,stype sb-mop:funcallable-standard-object)
                    ()
                    (:metaclass sb-mop:funcallable-standard-class))
                  (with-test (:name (,stype function type))
                    (assert-subtype ,fsname ,stype)
                    (assert-subtype ,fsname function)
                    (assert-instance-type ,fsname (not sequence)))
                  (with-test (:name (,stype function funcall))
                    (assert-callable ,fsname))))))
  (test stream)
  (test file-stream)
  (test string-stream))

(defclass %sequence (sequence standard-object)
  ())
(with-test (:name (sequence standard-object))
  (assert-subtype %sequence sequence)
  (assert-instance-type %sequence (not function))
  (assert-instance-type %sequence (not stream)))

(defclass %fsequence (sequence sb-mop:funcallable-standard-object)
  ()
  (:metaclass sb-mop:funcallable-standard-class))
(with-test (:name (sequence function type))
  (assert-subtype %fsequence sequence)
  (assert-subtype %fsequence function)
  (assert-instance-type %fsequence (not stream)))
(with-test (:name (sequence function funcall))
  (assert-callable %fsequence))

(macrolet ((test (stype)
             (let ((sname (intern (concatenate 'string "%SEQ" (symbol-name stype))))
                   (fsname (intern (concatenate 'string "%FSEQ" (symbol-name stype)))))
               `(progn
                  (defclass ,sname (,stype sequence standard-object)
                    ())
                  (with-test (:name (,stype sequence standard-object))
                    (assert-subtype ,sname ,stype)
                    (assert-instance-type ,sname (not function))
                    (assert-subtype ,sname sequence))

                  (defclass ,fsname (,stype sequence sb-mop:funcallable-standard-object)
                    ()
                    (:metaclass sb-mop:funcallable-standard-class))
                  (with-test (:name (,stype sequence function))
                    (assert-subtype ,fsname ,stype)
                    (assert-subtype ,fsname function)
                    (assert-subtype ,fsname sequence))
                  (with-test (:name (,stype sequence function funcall))
                    (assert-callable ,fsname))))))
  (test stream)
  (test file-stream)
  (test string-stream))

