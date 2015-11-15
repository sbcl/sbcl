;;;; ALIEN-related type system stuff, done later
;;;; than other type system stuff because it depends on the definition
;;;; of the ALIEN-VALUE target structure type

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

(/show0 "code/alien-type.lisp 16")

(!begin-collecting-cold-init-forms)

(!define-type-class alien :enumerable nil :might-contain-other-types nil)

(!define-type-method (alien :negate) (type) (make-negation-type type))

(!define-type-method (alien :unparse) (type)
  `(alien ,(unparse-alien-type (alien-type-type-alien-type type))))

(!define-type-method (alien :simple-subtypep) (type1 type2)
  (values (alien-subtype-p (alien-type-type-alien-type type1)
                           (alien-type-type-alien-type type2))
          t))

;;; KLUDGE: This !DEFINE-SUPERCLASSES gets executed much later than the
;;; others (toplevel form time instead of cold load init time) because
;;; ALIEN-VALUE itself is a structure which isn't defined until fairly
;;; late.
(!define-superclasses alien ((alien-value)) progn)

(!define-type-method (alien :simple-=) (type1 type2)
  (let ((alien-type-1 (alien-type-type-alien-type type1))
        (alien-type-2 (alien-type-type-alien-type type2)))
    (values (or (eq alien-type-1 alien-type-2)
                (alien-type-= alien-type-1 alien-type-2))
            t)))

(!def-type-translator alien (&optional (alien-type nil))
  (typecase alien-type
    (null
     (make-alien-type-type))
    (alien-type
     (make-alien-type-type alien-type))
    (t
     (make-alien-type-type (parse-alien-type alien-type (make-null-lexenv))))))

(defun make-alien-type-type (&optional alien-type)
  (if alien-type
      (let ((lisp-rep-type (compute-lisp-rep-type alien-type)))
        (if lisp-rep-type
            (single-value-specifier-type lisp-rep-type)
            (%make-alien-type-type alien-type)))
      *universal-type*))

(!defun-from-collected-cold-init-forms !alien-type-cold-init)

(/show0 "code/alien-type.lisp end of file")
