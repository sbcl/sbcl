;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!ALIEN")

(/show0 "host-c-call.lisp 12")

(define-alien-type-class (c-string :include pointer :include-args (to)))

(define-alien-type-translator c-string ()
  (make-alien-c-string-type
   :to (parse-alien-type 'char (sb!kernel:make-null-lexenv))))

(define-alien-type-method (c-string :unparse) (type)
  (declare (ignore type))
  'c-string)

(define-alien-type-method (c-string :lisp-rep) (type)
  (declare (ignore type))
  '(or simple-string null (alien (* char))))

(define-alien-type-method (c-string :naturalize-gen) (type alien)
  (declare (ignore type))
  `(if (zerop (sap-int ,alien))
       nil
       (%naturalize-c-string ,alien)))

(define-alien-type-method (c-string :deport-gen) (type value)
  (declare (ignore type))
  `(etypecase ,value
     (null (int-sap 0))
     ((alien (* char)) (alien-sap ,value))
     (simple-base-string (vector-sap ,value))
     (simple-string (vector-sap (coerce ,value 'simple-base-string)))))

(/show0 "host-c-call.lisp 42")

(define-alien-type-class (utf8-string :include pointer :include-args (to)))

(define-alien-type-translator utf8-string ()
  (make-alien-utf8-string-type
   :to (parse-alien-type 'char (sb!kernel:make-null-lexenv))))

(define-alien-type-method (utf8-string :unparse) (type)
  (declare (ignore type))
  'utf8-string)

(define-alien-type-method (utf8-string :lisp-rep) (type)
  (declare (ignore type))
  '(or simple-string null (alien (* char))))

(define-alien-type-method (utf8-string :naturalize-gen) (type alien)
  (declare (ignore type))
  `(if (zerop (sap-int ,alien))
       nil
       (%naturalize-utf8-string ,alien)))

(define-alien-type-method (utf8-string :deport-gen) (type value)
  (declare (ignore type))
  `(etypecase ,value
     (null (int-sap 0))
     ((alien (* char)) (alien-sap ,value))
     (simple-base-string (vector-sap ,value))
     (simple-string (vector-sap (%deport-utf8-string ,value)))))

(/show0 "host-c-call.lisp end of file")
