(defparameter *bool8type*
  #.(sb-alien-internals:parse-alien-type '(boolean 8) nil))
(defparameter *s13type*
  #.(sb-alien-internals:parse-alien-type '(signed 13) nil))
(defparameter *cstrtype*
  (sb-alien-internals:parse-alien-type
   '(c-string :external-format :church-latin ; we don't validate this?
              :element-type base-char
              :not-null t)
   nil))

(with-test (:name :hash-cons-alien-type-atoms)
  ;; restored as the right metatype
  (assert (sb-alien-internals:alien-boolean-type-p *bool8type*))
  (assert (eq *bool8type* ; and re-parses to the identical object
              (sb-alien-internals:parse-alien-type '(boolean 8) nil)))

  (assert (eq *s13type*
              (sb-alien-internals:parse-alien-type '(signed 13) nil)))

  (assert (eq *cstrtype*
              (sb-alien-internals:parse-alien-type
               '(c-string :external-format :church-latin
                          :element-type base-char
                          :not-null t)
               nil))))
