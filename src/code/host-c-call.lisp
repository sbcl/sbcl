;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C-CALL")

(/show0 "host-c-call.lisp 12")

(def-alien-type-class (c-string :include pointer :include-args (to)))

(def-alien-type-translator c-string ()
  (make-alien-c-string-type :to
			    (parse-alien-type 'char
					      (sb!kernel::make-null-lexenv))))

(def-alien-type-method (c-string :unparse) (type)
  (declare (ignore type))
  'c-string)

(def-alien-type-method (c-string :lisp-rep) (type)
  (declare (ignore type))
  '(or simple-base-string null (alien (* char))))

(def-alien-type-method (c-string :naturalize-gen) (type alien)
  (declare (ignore type))
  `(if (zerop (sap-int ,alien))
       nil
       (%naturalize-c-string ,alien)))

(def-alien-type-method (c-string :deport-gen) (type value)
  (declare (ignore type))
  `(etypecase ,value
     (null (int-sap 0))
     ((alien (* char)) (alien-sap ,value))
     (simple-base-string (vector-sap ,value))))

(/show0 "host-c-call.lisp end of file")
