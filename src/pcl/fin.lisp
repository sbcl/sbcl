;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is derived from software originally released by Xerox
;;;; Corporation. Copyright and release statements follow. Later modifications
;;;; to the software are in the public domain and are provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for more
;;;; information.

;;;; copyright information from original PCL sources:
;;;;
;;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;;; All rights reserved.
;;;;
;;;; Use and copying of this software and preparation of derivative works based
;;;; upon this software are permitted. Any distribution of this software or
;;;; derivative works must comply with all applicable United States export
;;;; control laws.
;;;;
;;;; This software is made available AS IS, and Xerox Corporation makes no
;;;; warranty about the software, its performance or its conformity to any
;;;; specification.

(in-package "SB-PCL")

;;; Each implementation must provide the following functions and macros:
;;;
;;;    ALLOCATE-FUNCALLABLE-INSTANCE-1 ()
;;;       should create and return a new funcallable instance. The
;;;       funcallable-instance-data slots must be initialized to NIL.
;;;       This is called by allocate-funcallable-instance and by the
;;;       bootstrapping code.
;;;
;;;    FUNCALLABLE-INSTANCE-P (x)
;;;       the obvious predicate. This should be an INLINE function. It
;;;       must be funcallable, but it would be nice if it compiled open.
;;;
;;;    SET-FUNCALLABLE-INSTANCE-FUNCTION (fin new-value)
;;;       change the fin so that when it is funcalled, the new-value
;;;       function is called. Note that it is legal for new-value
;;;       to be copied before it is installed in the fin, specifically
;;;       there is no accessor for a FIN's function so this function
;;;       does not have to preserve the actual new value. The new-value
;;;       argument can be any funcallable thing, a closure, lambda
;;;       compiled code etc. This function must coerce those values
;;;       if necessary.
;;;       NOTE: new-value is almost always a compiled closure. This
;;;	     is the important case to optimize.
;;;
;;;    FUNCALLABLE-INSTANCE-DATA-1 (fin data-name)
;;;       should return the value of the data named data-name in the fin.
;;;       data-name is one of the symbols in the list which is the value
;;;       of funcallable-instance-data. Since data-name is almost always
;;;       a quoted symbol and funcallable-instance-data is a constant, it
;;;       is possible (and worthwhile) to optimize the computation of
;;;       data-name's offset in the data part of the fin.
;;;       This must be SETF'able.

;;;; implementation of funcallable instances for CMU Common Lisp

(defstruct (pcl-funcallable-instance
	    (:alternate-metaclass sb-kernel:funcallable-instance
				  sb-kernel:random-pcl-class
				  sb-kernel:make-random-pcl-class)
	    (:type sb-kernel:funcallable-structure)
	    (:constructor allocate-funcallable-instance-1 ())
	    (:copier nil)
	    (:conc-name nil))
  ;; Note: The PCL wrapper is in the layout slot.

  ;; PCL data vector.
  (pcl-funcallable-instance-slots nil)
  ;; The debug-name for this function.
  (funcallable-instance-name nil))

(import 'sb-kernel:funcallable-instance-p)

;;; Set the function that is called when FIN is called.
(defun set-funcallable-instance-function (fin new-value)
  (declare (type function new-value))
  (aver (funcallable-instance-p fin))
  (setf (sb-kernel:funcallable-instance-function fin) new-value))

;;; This "works" on non-PCL FINs, which allows us to weaken
;;; FUNCALLABLE-INSTANCE-P to return true for all FINs. This is also
;;; necessary for bootstrapping to work, since the layouts for early GFs are
;;; not initially initialized.
(defmacro funcallable-instance-data-1 (fin slot)
  (ecase (eval slot)
    (wrapper `(sb-kernel:%funcallable-instance-layout ,fin))
    (slots `(sb-kernel:%funcallable-instance-info ,fin 0))))

;;;; slightly higher-level stuff built on the implementation-dependent stuff

(defmacro fsc-instance-p (fin)
  `(funcallable-instance-p ,fin))

(defmacro fsc-instance-class (fin)
  `(wrapper-class (funcallable-instance-data-1 ,fin 'wrapper)))

(defmacro fsc-instance-wrapper (fin)
  `(funcallable-instance-data-1 ,fin 'wrapper))

(defmacro fsc-instance-slots (fin)
  `(funcallable-instance-data-1 ,fin 'slots))
