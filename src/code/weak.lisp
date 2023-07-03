;;;; weak pointer support

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
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
(defun weak-vector-p (x)
  (and (simple-vector-p x)
       (test-header-data-bit x (ash sb-vm:vector-weak-flag sb-vm:array-flags-data-position))))

(defun make-weak-vector (length &key (initial-contents nil contents-p)
                                     (initial-element nil element-p))
  (declare (index length))
  (when (and element-p contents-p)
    (error "Can't specify both :INITIAL-ELEMENT and :INITIAL-CONTENTS"))
  ;; Since weak vectors are not in theory merely arrays any more, but potentially
  ;; some kind of weak pointers with a varing enumber of slots, this isn't badly
  ;; OAOO-violating in regard to make-array transforms.
  (when contents-p
    (let ((contents-length (length initial-contents)))
      (unless (eql length contents-length)
        (error "~S has ~D elements, vector length is ~D."
               :initial-contents contents-length length))))
  (let ((v (sb-c::allocate-weak-vector length)))
    (if initial-contents
        (dotimes (i length)
          (setf (weak-vector-ref v i) (elt initial-contents i)))
          ;; 0 is the usual default initial element for arrays, but all weak objects use NIL
        (dotimes (i length)
          (setf (weak-vector-ref v i) initial-element)))
    v))
(defun make-weak-pointer (object)
  "Allocate and return a weak pointer which points to OBJECT."
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
