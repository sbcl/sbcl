;;;; Tests for SBCL's extended specializers.

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

;;; Custom specializer 1
;;;
;;; Always signals an error when parsing the specializer specifier.

(defmethod sb-pcl:parse-specializer-using-class ((generic-function standard-generic-function)
                                                 (specializer-name (eql 'custom-1)))
  (error "Intentional error"))

;;; Custom specializer 2
;;;
;;; Can be parsed but fails to define a method on
;;; SPECIALIZER-TYPE-SPECIFIER.

(defclass custom-2-impl (sb-mop:specializer)
  ())

(defmethod sb-pcl:parse-specializer-using-class ((generic-function standard-generic-function)
                                                 (specializer-name (eql 'custom-2)))
  (make-instance 'custom-2-impl))

;;; Custom specializer 3
;;;
;;; Can be parsed and has a suitable method on
;;; SPECIALIZER-TYPE-SPECIFIER.

(defclass custom-3-impl (sb-mop:specializer)
  ())

(defmethod sb-pcl:parse-specializer-using-class ((generic-function standard-generic-function)
                                                 (specializer-name (eql 'custom-3)))
  (make-instance 'custom-3-impl))

(defmethod sb-pcl:specializer-type-specifier ((proto-generic-function standard-generic-function)
                                              (proto-method standard-method)
                                              (specializer custom-3-impl))
  'custom-3)

;;; Test

(with-test (:name (sb-pcl:parse-specializer-using-class :smoke))
  (let ((proto-gf (sb-mop:class-prototype
                   (find-class 'standard-generic-function))))
    (flet ((test (specializer-name expected)
             (flet ((do-it ()
                      (sb-pcl:parse-specializer-using-class
                       proto-gf specializer-name)))
               (case expected
                 (sb-pcl:specializer-name-syntax-error
                  (assert-error (do-it) sb-pcl:specializer-name-syntax-error))
                 (sb-pcl:class-not-found-error
                  (assert-error (do-it) sb-pcl:class-not-found-error))
                 (t
                  (typecase expected
                    (sb-pcl::class-prototype-specializer
                     (assert (eq (sb-pcl::specializer-object (do-it))
                                 (sb-pcl::specializer-object expected))))
                    (t
                     (assert (eq (do-it) expected)))))))))
      ;; Atoms
      (test 1                                    'sb-pcl:specializer-name-syntax-error)
      (test nil                                  'sb-pcl:class-not-found-error)

      (test t                                    (find-class t))
      (test 'null                                (find-class 'null))
      (test (find-class t)                       (find-class t))
      (test (find-class 'null)                   (find-class 'null))

      ;; Lists
      (test `(1)                                 'sb-pcl:specializer-name-syntax-error)
      (test `(,(find-class t))                   'sb-pcl:specializer-name-syntax-error)

      (test `(class)                             'sb-pcl:specializer-name-syntax-error)
      (test `(class t 2)                         'sb-pcl:specializer-name-syntax-error)
      (test `(class t)                           (find-class t))
      (test `(class ,(find-class t))             (find-class t))

      (test `(sb-pcl::prototype)                 'sb-pcl:specializer-name-syntax-error)
      (test `(sb-pcl::prototype t 2)             'sb-pcl:specializer-name-syntax-error)
      (test `(sb-pcl::prototype t)               (make-instance 'sb-pcl::class-prototype-specializer
                                                                :class (find-class t)))
      (test `(sb-pcl::prototype ,(find-class t)) (make-instance 'sb-pcl::class-prototype-specializer
                                                                :class (find-class t)))

      (test `(sb-pcl::class-eq)                  'sb-pcl:specializer-name-syntax-error)
      (test `(sb-pcl::class-eq t 2)              'sb-pcl:specializer-name-syntax-error)
      (test `(sb-pcl::class-eq t)                (sb-pcl::class-eq-specializer
                                                  (find-class t)))
      (test `(sb-pcl::class-eq ,(find-class t))  (sb-pcl::class-eq-specializer
                                                  (find-class t)))

      (test `(eql)                               'sb-pcl:specializer-name-syntax-error)
      (test `(eql t 2)                           'sb-pcl:specializer-name-syntax-error)
      (test `(eql t)                             (sb-mop:intern-eql-specializer t)))))

(with-test (:name (sb-pcl:specializer-type-specifier :smoke))
  (let* ((proto-gf (sb-mop:class-prototype
                    (find-class 'standard-generic-function)))
         (proto-method (sb-mop:class-prototype
                        (find-class 'standard-method))))
    (flet ((parse (specializer-specifier)
             (sb-pcl:parse-specializer-using-class
              proto-gf specializer-specifier))
           (test (specializer expected)
             (flet ((compute-it ()
                      (sb-pcl:specializer-type-specifier
                       proto-gf proto-method specializer)))
               (case expected
                 (warning
                  (assert (null (assert-signal (compute-it) warning))))
                 (style-warning
                  (assert (null (assert-signal (compute-it) style-warning))))
                 (t
                  (assert (type-evidently-= (compute-it) expected)))))))
      ;; Non-parsed class specializers
      (test 'package                            'package)
      (test 'integer                            'integer)
      (test 'class                              nil)
      (test 'no-such-class                      'style-warning)

      (test '(eql)                              'style-warning)
      (test '(eql 5)                            '(eql 5))
      (test '(eql 5 6)                          'style-warning)
      (test '(sb-pcl::class-eq integer)         'integer)
      (test '(sb-pcl::class-eq class)           nil)

      (test 'custom-1                           'style-warning) ; fails to parse
      (test 'custom-2                           'warning)       ; no method
      (test 'custom-3                           'custom-3)

      ;; Parsed EQL and CLASS-EQ specializers
      (test (parse '(eql 5))                    '(eql 5))
      (test (parse '(sb-pcl::class-eq integer)) 'integer)
      (test (parse '(sb-pcl::class-eq class))   nil)

      ;; Parsed class specializers
      (test (find-class 'package)               'package)
      (test (find-class 'integer)               'integer)
      (test (find-class 'class)                 nil)

      ;; Parsed custom specializer
      (test (make-instance 'custom-3-impl)      'custom-3))))
