;;;; weak pointer support

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMUCL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;; "new" weak vectors satisfy weak-pointer-p, not simple-vector-p
#+weak-vector-readbarrier
(progn
  (defun weak-vector-p (x)
    (and (weak-pointer-p x)
         (= (logand (get-header-data x) #xFF) 0)))
  (defun weak-vector-len (thing)
    ;; FIXME: assert that it's a vector-like weak pointer, otherwise it'll see
    ;; the weak-pointer-value slot.
    (%array-fill-pointer thing))
  (defun weak-vector-ref (vector index) ; TODO: needs dimension check and read barrier
    (sb-vm::%weakvec-ref vector index))
  (defun (setf weak-vector-ref) (newval vector index)
    (sb-vm::%weakvec-set vector index newval)
    newval)
  ;; A weak key/value-vector is primitive type SIMPLE-VECTOR based on its widetag,
  ;; but requires a read barrier to ensure that access does not race with GC
  ;; while GC is trying to clear the otherwise-unreachable references.
  ;; These are stubs. The real implementation needs a mutex, thought better
  ;; would be a reader/writer lock, since several threads may all read.
  ;; On the other hand, our weak tables are always synchronized by a lisp mutex
  ;; now and I don't plan to change that any time soon.
  (defun sb-impl::weak-kvv-ref (vector index)
    (svref vector index))
  (defun (cas sb-impl::weak-kvv-ref) (old new vector index)
    (funcall #'(cas svref) old new vector index))
  (defun list-from-weak-vector (v)
    (collect ((result))
      (dotimes (i (weak-vector-len v) (result))
        (result (weak-vector-ref v i))))))

#-weak-vector-readbarrier
;; legacy implementation of weak vector is basically SIMPLE-VECTOR
(defun weak-vector-p (object)
  "See if OBJECT is a weak vector. This implies SIMPLE-VECTOR-P."
  (and (simple-vector-p object)
       (test-header-data-bit object (ash sb-vm:vector-weak-flag sb-vm:array-flags-data-position))))

(defun initial-contents-error (content-length length)
  (error "There are ~W elements in the :INITIAL-CONTENTS, but ~
         the vector length is ~W."
         content-length length))

(defun make-weak-vector (length &key (initial-contents nil contents-p)
                                     (initial-element nil element-p))
  "Allocate a weak vector of LENGTH elements. See MAKE-ARRAY for a
description of INITIAL-CONTENTS and INITIAL-ELEMENT. Note that unlike
in MAKE-ARRAY, here INITIAL-ELEMENT always defaults to NIL.

A weak vector is similar to a general VECTOR whose elements are
WEAK-POINTERs, but in weak vectors the weak pointers are implicit.
Hence, there is no need for MAKE-WEAK-POINTER and WEAK-POINTER-VALUE
when writing or reading its elements.

If an element is garbage collected, it is replaced by NIL in the
vector."
  (declare (index length))
  (when (and element-p contents-p)
    (error "Can't specify both :INITIAL-ELEMENT and :INITIAL-CONTENTS"))
  ;; Since weak vectors are not in theory merely arrays any more, but potentially
  ;; some kind of weak pointers with a varing number of slots, this isn't badly
  ;; OAOO-violating in regard to make-array transforms.
  (when contents-p
    (let ((contents-length (length initial-contents)))
      (unless (eql length contents-length)
        (error "~S has ~D elements, vector length is ~D."
               :initial-contents contents-length length))))
  (let ((v (sb-c::allocate-weak-vector length)))
    (cond (initial-contents
           (unless (= length (length initial-contents))
             (initial-contents-error length (length initial-contents)))
           (replace v initial-contents))
          (t
           (fill v initial-element)))
    v))

(defun make-weak-pointer (object)
  "Allocate and return a WEAK-POINTER that points to OBJECT."
  (make-weak-pointer object))

(declaim (inline weak-pointer-value))
(defun weak-pointer-value (weak-pointer)
  "If WEAK-POINTER is valid, return the value of WEAK-POINTER and T.
If the referent of WEAK-POINTER has been garbage collected,
returns the values NIL and NIL."
  (declare (type weak-pointer weak-pointer))
  (let ((value (sb-vm::%weak-pointer-value weak-pointer)))
    (if (sb-vm::unbound-marker-p value)
        (values nil nil)
        (values value t))))

(defun list-to-weak-vector (list)
  (let ((v (make-weak-vector (length list)))
        (i -1))
    (dolist (object list v)
      (setf (weak-vector-ref v (incf i)) object))))
