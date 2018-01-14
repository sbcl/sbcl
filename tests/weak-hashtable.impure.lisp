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

;;; Apparently the entirety of 'hash.impure.lisp' is inadequate to
;;; sufficiently exercise weak hash tables.  I had a blatant omission
;;; in setting 'rehash' on a weak table, and yet no tests in that file failed.
;;;
;;; I was able to come up with this minimal examine, which I've separated
;;; out from the other tests, for better repeatability.
;;; The sensitivity to other stuff is that depending on the order in which GC
;;; observes objects, which depends on placement, it may know that weak entries
;;; are a-priori live. That's not interesting/difficult for it.
;;; The interesting case is deferring the liveness decision,
;;; which means that the symbol *V1* has to be reached only after
;;; the symbols holding weak hash tables are reached.
;;;
;;; Additionally this test demonstrates that one object
;;; can trigger multiple other objects - the symbol #:bork
;;; is a key in two tables; and an object can be triggered
;;; by more than one object - #P"turtle" is enlivened
;;; if either #:bork or #P"grumpy" is live.
(defvar *a* (make-hash-table :weakness :key))
(defvar *b* (make-hash-table :weakness :key))
(defvar *c* (make-hash-table :weakness :key))
(defvar *foo* nil)
; (setf (extern-alien "debug_weak_ht" int) 1)
(let ((s (make-symbol "bork"))
      (v (make-array 10))
      (pn1 #P"grumpy")
      (pn2 #P"turtle"))
  (setf (gethash s *a*) pn1)
  (setf (gethash s *b*) pn2)
  (setf (aref v 9) (list (list s)))
  (setf (gethash pn1 *c*) pn2)
  (setq *foo* (make-weak-pointer s))
  (defvar *v1* v))
(gc)
(with-test (:name :weak-table-smoke-test)
  (assert (weak-pointer-value *foo*))
  (assert (pathnamep (gethash (weak-pointer-value *foo*) *a*)))
  (assert (pathnamep (gethash (weak-pointer-value *foo*) *b*))))

(with-test (:name :invalid-objects)
  (let ((hash (make-hash-table :weakness :key-and-value)))
    (setf (gethash 10 hash) (sb-kernel:%make-lisp-obj sb-vm:other-pointer-lowtag))
    (sb-ext:gc :full t)
    hash))
