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

(flet ((proclaim-ftype-for-name (kind name type)
         (ecase kind
           (condition
            (proclaim `(ftype ,type ,name)))
           (class
            (when (eq (info :function :where-from name) :assumed)
              (sb-c:proclaim-ftype name (specifier-type type) nil :defined))))))

  (defun preinform-compiler-about-accessors (kind readers writers)
    (flet ((inform (names type)
             (dolist (name names)
               (proclaim-ftype-for-name kind name type))))
      (inform readers '(function (t) t))
      (inform writers '(function (t t) t))))

  (defun preinform-compiler-about-slot-functions (kind slots)
    (flet ((inform (slots key type)
             (dolist (slot slots)
               (let ((name (funcall key slot)))
                 (proclaim-ftype-for-name kind name type)))))
      (inform slots #'sb-pcl::slot-reader-name '(function (t) t))
      (inform slots #'sb-pcl::slot-boundp-name '(function (t) t))
      (inform slots #'sb-pcl::slot-writer-name '(function (t t) t)))))

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
