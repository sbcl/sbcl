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

(defun get-header-data (x)
  #!+sb-doc
  "Return the 24 bits of data in the header of object X, which must be an
  other-pointer object."
  (get-header-data x))

(defun set-header-data (x val)
  #!+sb-doc
  "Sets the 24 bits of data in the header of object X (which must be an
  other-pointer object) to VAL."
  (set-header-data x val))

(defun get-closure-length (x)
  #!+sb-doc
  "Returns the length of the closure X. This is one more than the number
  of variables closed over."
  (get-closure-length x))

(defun get-lowtag (x)
  #!+sb-doc
  "Returns the three-bit lowtag for the object X."
  (get-lowtag x))

(defun get-type (x)
  #!+sb-doc
  "Returns the 8-bit header type for the object X."
  (get-type x))

(defun vector-sap (x)
  #!+sb-doc
  "Return a System-Area-Pointer pointing to the data for the vector X, which
  must be simple."
  (declare (type (simple-unboxed-array (*)) x))
  (vector-sap x))

(defun sb!c::binding-stack-pointer-sap ()
  #!+sb-doc
  "Return a System-Area-Pointer pointing to the end of the binding stack."
  (sb!c::binding-stack-pointer-sap))

(defun sb!c::dynamic-space-free-pointer ()
  #!+sb-doc
  "Returns a System-Area-Pointer pointing to the next free work of the current
  dynamic space."
  (sb!c::dynamic-space-free-pointer))

(defun sb!c::control-stack-pointer-sap ()
  #!+sb-doc
  "Return a System-Area-Pointer pointing to the end of the control stack."
  (sb!c::control-stack-pointer-sap))

(defun function-subtype (function)
  #!+sb-doc
  "Return the header typecode for FUNCTION. Can be set with SETF."
  (function-subtype function))

(defun (setf function-subtype) (type function)
  (setf (function-subtype function) type))

(defun %function-arglist (func)
  #!+sb-doc
  "Extracts the arglist from the function header FUNC."
  (%function-arglist func))

(defun %function-name (func)
  #!+sb-doc
  "Extracts the name from the function header FUNC."
  (%function-name func))

(defun %function-type (func)
  #!+sb-doc
  "Extracts the type from the function header FUNC."
  (%function-type func))

(defun %closure-function (closure)
  #!+sb-doc
  "Extracts the function from CLOSURE."
  (%closure-function closure))

(defun sb!c::vector-length (vector)
  #!+sb-doc
  "Return the length of VECTOR. There is no reason to use this, 'cause
  (length (the vector foo)) is the same."
  (sb!c::vector-length vector))

(defun %closure-index-ref (closure index)
  #!+sb-doc
  "Extract the INDEXth slot from CLOSURE."
  (%closure-index-ref closure index))

(defun allocate-vector (type length words)
  #!+sb-doc
  "Allocate a unboxed, simple vector with type code TYPE, length LENGTH, and
  WORDS words long. Note: it is your responsibility to ensure that the
  relation between LENGTH and WORDS is correct."
  (allocate-vector type length words))

(defun make-array-header (type rank)
  #!+sb-doc
  "Allocate an array header with type code TYPE and rank RANK."
  (make-array-header type rank))

(defun code-instructions (code-obj)
  #!+sb-doc
  "Return a SAP pointing to the instructions part of CODE-OBJ."
  (code-instructions code-obj))

(defun code-header-ref (code-obj index)
  #!+sb-doc
  "Extract the INDEXth element from the header of CODE-OBJ. Can be set with
  setf."
  (code-header-ref code-obj index))

(defun code-header-set (code-obj index new)
  (code-header-set code-obj index new))

(defun %raw-bits (object offset)
  (declare (type index offset))
  (sb!kernel:%raw-bits object offset))

(defun %set-raw-bits (object offset value)
  (declare (type index offset) (type (unsigned-byte #.sb!vm:word-bits) value))
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
