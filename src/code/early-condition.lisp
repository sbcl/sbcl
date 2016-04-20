;;;; Just %COMPILER-DEFINE-CONDITION, moved out of 'condition'

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;; This used to be in an (EVAL-WHEN (:COMPILE-TOPLEVEL ...))
;;; which no longer works, because at run-the-xc-time the
;;; WITH-SINGLE-PACKAGE-LOCKED-ERROR macro doesn't work yet,
;;; so just use the definition that was loaded from the fasl
;;; when the cross-compiler was compiled.
(defun %compiler-define-condition (name direct-supers layout
                                        all-readers all-writers)
  (declare (notinline find-classoid))
  (with-single-package-locked-error
      (:symbol name "defining ~A as a condition")
    (sb!xc:proclaim `(ftype (function (t) t) ,@all-readers))
    (sb!xc:proclaim `(ftype (function (t t) t) ,@all-writers))
    (multiple-value-bind (class old-layout)
        (insured-find-classoid name
                               #'condition-classoid-p
                               #'make-condition-classoid)
      (setf (layout-classoid layout) class)
      (setf (classoid-direct-superclasses class)
            (mapcar #'find-classoid direct-supers))
      (cond ((not old-layout)
             (register-layout layout))
            ((not *type-system-initialized*)
             (setf (layout-classoid old-layout) class)
             (setq layout old-layout)
             (unless (eq (classoid-layout class) layout)
               (register-layout layout)))
            ((redefine-layout-warning "current"
                                      old-layout
                                      "new"
                                      (layout-length layout)
                                      (layout-inherits layout)
                                      (layout-depthoid layout)
                                      (layout-bitmap layout))
             (register-layout layout :invalidate t))
            ((not (classoid-layout class))
             (register-layout layout)))

      ;; This looks totally bogus - it essentially means that the LAYOUT-INFO
      ;; of a condition is good for nothing, because it describes something
      ;; that is not the condition class being defined.
      ;; In addition to which, the INFO for CONDITION itself describes
      ;; slots which do not exist, viz:
      ;;  (dd-slots (layout-info (classoid-layout (find-classoid 'condition))))
      ;; => (#<DEFSTRUCT-SLOT-DESCRIPTION ACTUAL-INITARGS>
      ;;     #<DEFSTRUCT-SLOT-DESCRIPTION ASSIGNED-SLOTS>)
      (setf (layout-info layout)
            (layout-info (classoid-layout (find-classoid 'condition))))

      (setf (find-classoid name) class)

      ;; Initialize CPL slot.
      (setf (condition-classoid-cpl class)
            (remove-if-not #'condition-classoid-p
                           (std-compute-class-precedence-list class)))))
  (values))
