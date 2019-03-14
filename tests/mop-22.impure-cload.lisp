;;;; miscellaneous side-effectful tests of the MOP

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

;;; Forward-referenced classes as specializers.

;;; It's generally unclear to me whether this should be allowed.  On
;;; the one hand, FORWARD-REFERENCED-CLASS is a subclass of CLASS and
;;; hence of SPECIALIZER, and AMOP specifies that as-yet-undefined
;;; superclasses of STANDARD-CLASSes are FORWARD-REFERENCED-CLASSes of
;;; the appropriate proper name.  On the other hand, ANSI specifies
;;; that DEFCLASS defines _a_ class, and that classes should be
;;; defined before they can be used as specializers in DEFMETHOD forms
;;; (though ANSI also allows implementations to extend the object
;;; system in this last respect).  Future maintainers should feel free
;;; to cause this test to fail if it improves the lot of some other
;;; codepath. -- CSR, 2006-08-09

(defclass incomplete (forward) ())

(defgeneric incomplete/1 (x)
  (:method ((x incomplete)) 'incomplete))

(defgeneric forward/1 (x)
  (:method ((x forward)) 'forward))

;;; with many arguments to avoid the precomputed discriminating
;;; function generators
(defgeneric incomplete/7 (a b c d e f g)
  (:method ((a incomplete) (b forward)
            c (d integer) (e condition) (f class) g) t))

(defclass forward () ())

(with-test (:name :mop-22)
  (assert (eq (incomplete/1 (make-instance 'incomplete)) 'incomplete))
  (assert (eq (forward/1 (make-instance 'forward)) 'forward))
  (assert (eq (incomplete/7 (make-instance 'incomplete)
                            (make-instance 'incomplete)
                            t 1 (make-condition 'error)
                            (find-class 'incomplete) 3)
              t)))
