;;;; miscellaneous non-side-effectful tests of the MOP

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

;;;; Note that the MOP is not in an entirely supported state.
;;;; However, this seems a good a way as any of ensuring that we have
;;;; no regressions.

(assert (subtypep 'sb-mop:funcallable-standard-object 'standard-object))

(assert (find (find-class 'sb-mop:funcallable-standard-object)
              (sb-mop:class-direct-subclasses (find-class 'standard-object))))

(assert (find (find-class 'standard-object)
              (sb-mop:class-direct-superclasses
               (find-class 'sb-mop:funcallable-standard-object))))

(dolist (name '(sb-mop:generic-function
                sb-mop:method sb-mop:method-combination
                sb-mop:slot-definition sb-mop:specializer))
  (assert (find (find-class 'sb-mop:metaobject)
                (sb-mop:class-direct-superclasses (find-class name))))
  (assert (subtypep name 'sb-mop:metaobject)))
