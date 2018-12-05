;;;; Early support routines for class-related things (including conditions).

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

(defun call-with-defining-class (kind name thunk)
  (declare (ignorable kind name))
  (with-single-package-locked-error
      (:symbol name "defining ~S as a ~(~A~)" kind)
    (funcall thunk)))

(defun preinform-compiler-about-class-type (name forthcoming-info)
  ;; Unless the type system already has an actual type attached to
  ;; NAME (in which case (1) writing a placeholder value over that
  ;; actual type as a compile-time side-effect would probably be a bad
  ;; idea and (2) anyway we don't need to modify it in order to make
  ;; NAME be recognized as a valid type name)
  (when (and forthcoming-info (not (info :type :kind name)))
    ;; Tell the compiler to expect a class with the given NAME, by
    ;; writing a kind of minimal placeholder type information. This
    ;; placeholder will be overwritten later when the class is
    ;; defined.
    (setf (info :type :kind name) :forthcoming-defclass-type)))

(symbol-macrolet
    ((reader-function-type (specifier-type '(function (t) t)))
     (writer-function-type (specifier-type '(function (t t) t))))
  (flet ((proclaim-ftype-for-name (kind name type)
           (ecase kind
             (condition
              (sb-xc:proclaim `(ftype ,(type-specifier type) ,name)))
             (class
              (when (eq (info :function :where-from name) :assumed)
                (sb-c:proclaim-ftype name type nil :defined))))))

    (defun preinform-compiler-about-accessors (kind readers writers)
      (flet ((inform (names type)
               (mapc (lambda (name) (proclaim-ftype-for-name kind name type))
                     names)))
        (inform readers reader-function-type)
        (inform writers writer-function-type)))

    (defun preinform-compiler-about-slot-functions (kind slots)
      (flet ((inform (slots key type)
               (mapc (lambda (slot)
                       (let ((name (funcall key slot)))
                         (proclaim-ftype-for-name kind name type)))
                     slots)))
        (inform slots #'sb-pcl::slot-reader-name reader-function-type)
        (inform slots #'sb-pcl::slot-boundp-name reader-function-type)
        (inform slots #'sb-pcl::slot-writer-name writer-function-type)))))

(defun %%compiler-defclass (name readers writers slots)
  ;; ANSI says (Macro DEFCLASS, section 7.7) that DEFCLASS, if it
  ;; "appears as a top level form, the compiler must make the class
  ;; name be recognized as a valid type name in subsequent
  ;; declarations (as for deftype) and be recognized as a valid class
  ;; name for defmethod parameter specializers and for use as the
  ;; :metaclass option of a subsequent defclass."
  (preinform-compiler-about-class-type name t)
  (preinform-compiler-about-accessors 'class readers writers)
  (preinform-compiler-about-slot-functions 'class slots))

(defun %compiler-defclass (name readers writers slots)
  (call-with-defining-class
   'class name
   (lambda ()
     (%%compiler-defclass name readers writers slots))))

;;; This used to be in an (EVAL-WHEN (:COMPILE-TOPLEVEL ...))
;;; which no longer works, because at run-the-xc-time the
;;; WITH-SINGLE-PACKAGE-LOCKED-ERROR macro doesn't work yet,
;;; so just use the definition that was loaded from the fasl
;;; when the cross-compiler was compiled.
(defun %%compiler-define-condition (name direct-supers layout readers writers)
  (declare (notinline find-classoid))
  (preinform-compiler-about-class-type name nil)
  (preinform-compiler-about-accessors 'condition readers writers)
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

(defun %compiler-define-condition (name direct-supers layout readers writers)
  (call-with-defining-class
   'condition name
   (lambda ()
     (%%compiler-define-condition name direct-supers layout readers writers))))
