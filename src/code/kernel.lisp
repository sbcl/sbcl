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

;;; Return the 24 bits of data in the header of object X, which must
;;; be an other-pointer object.
(defun get-header-data (x)
  (get-header-data x))

;;; Set the 24 bits of data in the header of object X (which must be
;;; an other-pointer object) to VAL.
(defun set-header-data (x val)
  (set-header-data x val))

;;; the length of the closure X, i.e. one more than the
;;; number of variables closed over
(defun get-closure-length (x)
  (get-closure-length x))

(defun lowtag-of (x)
  (lowtag-of x))

(defun widetag-of (x)
  (widetag-of x))

;;; Return a System-Area-Pointer pointing to the data for the vector
;;; X, which must be simple.
;;;
;;; FIXME: So it should be SIMPLE-VECTOR-SAP, right? (or UNHAIRY-VECTOR-SAP,
;;; if the meaning is (SIMPLE-ARRAY * 1) instead of SIMPLE-VECTOR)
;;; (or maybe SIMPLE-VECTOR-DATA-SAP or UNHAIRY-VECTOR-DATA-SAP?)
(defun vector-sap (x)
  (declare (type (simple-unboxed-array (*)) x))
  (vector-sap x))

;;; Return a System-Area-Pointer pointing to the end of the binding stack.
(defun sb!c::binding-stack-pointer-sap ()
  (sb!c::binding-stack-pointer-sap))

;;; Return a System-Area-Pointer pointing to the next free word of the
;;; current dynamic space.
(defun sb!c::dynamic-space-free-pointer ()
  (sb!c::dynamic-space-free-pointer))

;;; Return a System-Area-Pointer pointing to the end of the control stack.
(defun sb!c::control-stack-pointer-sap ()
  (sb!c::control-stack-pointer-sap))

;;; Return the header typecode for FUNCTION. Can be set with SETF.
(defun function-subtype (function)
  (function-subtype function))
(defun (setf function-subtype) (type function)
  (setf (function-subtype function) type))

;;; Extract the arglist from the function header FUNC.
(defun %simple-fun-arglist (func)
  (%simple-fun-arglist func))

;;; Extract the name from the function header FUNC.
(defun %simple-fun-name (func)
  (%simple-fun-name func))

;;; Extract the type from the function header FUNC.
(defun %simple-fun-type (func)
  (%simple-fun-type func))

(defun %simple-fun-next (simple-fun)
  (%simple-fun-next simple-fun))

(defun %simple-fun-self (simple-fun)
  (%simple-fun-self simple-fun))

;;; Extract the function from CLOSURE.
(defun %closure-fun (closure)
  (%closure-fun closure))

;;; Return the length of VECTOR. There is no reason to use this in
;;; ordinary code, 'cause length (the vector foo)) is the same.
(defun sb!c::vector-length (vector)
  (sb!c::vector-length vector))

;;; Extract the INDEXth slot from CLOSURE.
(defun %closure-index-ref (closure index)
  (%closure-index-ref closure index))

;;; Allocate a unboxed, simple vector with type code TYPE, length LENGTH, and
;;; WORDS words long. Note: it is your responsibility to ensure that the
;;; relation between LENGTH and WORDS is correct.
(defun allocate-vector (type length words)
  (allocate-vector type length words))

;;; Allocate an array header with type code TYPE and rank RANK.
(defun make-array-header (type rank)
  (make-array-header type rank))

;;; Return a SAP pointing to the instructions part of CODE-OBJ.
(defun code-instructions (code-obj)
  (code-instructions code-obj))

;;; Extract the INDEXth element from the header of CODE-OBJ. Can be
;;; set with SETF.
(defun code-header-ref (code-obj index)
  (code-header-ref code-obj index))

(defun code-header-set (code-obj index new)
  (code-header-set code-obj index new))

(defun %raw-bits (object offset)
  (declare (type index offset))
  (sb!kernel:%raw-bits object offset))

(defun %set-raw-bits (object offset value)
  (declare (type index offset))
  (declare (type (unsigned-byte #.sb!vm:n-word-bits) value))
  (setf (sb!kernel:%raw-bits object offset) value))

(defun make-single-float (x) (make-single-float x))
(defun make-double-float (hi lo) (make-double-float hi lo))
#!+long-float
(defun make-long-float (exp hi #!+sparc mid lo)
  (make-long-float exp hi #!+sparc mid lo))
(defun single-float-bits (x) (single-float-bits x))
(defun double-float-high-bits (x) (double-float-high-bits x))
(defun double-float-low-bits (x) (double-float-low-bits x))
#!+long-float
(defun long-float-exp-bits (x) (long-float-exp-bits x))
#!+long-float
(defun long-float-high-bits (x) (long-float-high-bits x))
#!+(and long-float sparc)
(defun long-float-mid-bits (x) (long-float-mid-bits x))
#!+long-float
(defun long-float-low-bits (x) (long-float-low-bits x))
