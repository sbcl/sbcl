;;;; miscellaneous kernel-level definitions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;;; CLOSURE type and accessors

;;; FIXME: this should probably exclude the closure name slot, if named
(defmacro do-closure-values ((value closure) &body body)
  (with-unique-names (i nclosure)
    `(let ((,nclosure ,closure))
       (declare (closure ,nclosure))
       (dotimes (,i (- (1+ (get-closure-length ,nclosure)) sb!vm:closure-info-offset))
         (let ((,value (%closure-index-ref ,nclosure ,i)))
           ,@body)))))

(defun %closure-values (closure)
  (declare (closure closure))
  (let (values)
    (do-closure-values (elt closure)
      (push elt values))
    values)) ; no need to reverse - this has no promised iteration order

;;; A unique GC id. This is supplied for code that needs to detect
;;; whether a GC has happened since some earlier point in time. For
;;; example:
;;;
;;;   (let ((epoch *gc-epoch*))
;;;      ...
;;;      (unless (eql epoch *gc-epoch)
;;;        ....))
;;;
;;; This isn't just a fixnum counter since then we'd have theoretical
;;; problems when exactly 2^29 GCs happen between epoch
;;; comparisons. Unlikely, but the cost of using a cons instead is too
;;; small to measure. -- JES, 2007-09-30
(declaim (type cons *gc-epoch*))
(!defglobal *gc-epoch* '(nil . nil))

(declaim (inline lowtag-of))
(defun lowtag-of (x) (logand (get-lisp-obj-address x) sb!vm:lowtag-mask))

;;; Unlike most other "Stub functions" that never called called except
;;; by the interpreter, these two do get called, by MAKE-UNPORTABLE-FLOAT
(defun make-single-float (x) (make-single-float x))
(defun make-double-float (hi lo) (make-double-float hi lo))
