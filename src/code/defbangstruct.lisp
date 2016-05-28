;;;; DEF!STRUCT = bootstrap DEFSTRUCT, a wrapper around DEFSTRUCT which
;;;; provides special features to help at bootstrap time:
;;;;  1. Layout information, inheritance information, and so forth is
;;;;     retained in such a way that we can get to it even on vanilla
;;;;     ANSI Common Lisp at cross-compiler build time.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;; Has the type system been properly initialized? (I.e. is it OK to
;;; use it?)
(!defglobal *type-system-initialized* nil)

;;; machinery used in the implementation of DEF!STRUCT
#+sb-xc-host
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; a description of a DEF!STRUCT call to be stored until we get
  ;; enough of the system running to finish processing it
  (defstruct delayed-def!struct
    (args (missing-arg) :type cons)
    (package (sane-package) :type package))
  ;; a list of DELAYED-DEF!STRUCTs stored until we get DEF!STRUCT
  ;; working fully so that we can apply it to them then. After
  ;; DEF!STRUCT is made to work fully, this list is processed, then
  ;; made unbound, and should no longer be used.
  (defvar *delayed-def!structs* nil))
(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
  ;; Parse the arguments for a DEF!STRUCT call, and return
  ;;   (VALUES NAME DEFSTRUCT-ARGS DEF!STRUCT-SUPERTYPE),
  ;; where NAME is the name of the new type, DEFSTRUCT-ARGS is the
  ;; munged result suitable for passing on to DEFSTRUCT,
  ;; and DEF!STRUCT-SUPERTYPE is the direct supertype of
  ;; the type if it is another DEF!STRUCT-defined type, or NIL
  ;; otherwise.
  (defun parse-def!struct-args (nameoid &rest rest)
    (multiple-value-bind (name options) ; Note: OPTIONS can change below.
        (if (consp nameoid)
            (values (first nameoid) (rest nameoid))
            (values nameoid nil))
      (declare (type list options))
      (let* ((include-clause (find :include options :key #'first))
             (def!struct-supertype nil)) ; may change below
        (when (find :type options :key #'first)
          (error "can't use :TYPE option in DEF!STRUCT"))
        (when include-clause
          (setf def!struct-supertype (second include-clause)))
        (if (eq name 'structure!object) ; if root of hierarchy
            (aver (not include-clause))
            (unless include-clause
              (setf def!struct-supertype 'structure!object)
              (push `(:include ,def!struct-supertype) options)))
        (values name `((,name ,@options) ,@rest) def!struct-supertype)))))

;;; a helper function for DEF!STRUCT in the #+SB-XC-HOST case: Return
;;; DEFSTRUCT-style arguments with any class names in the SB!XC
;;; package (i.e. the name of the class being defined, and/or the
;;; names of classes in :INCLUDE clauses) converted from SB!XC::FOO to
;;; CL::FOO.
#+sb-xc-host
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun uncross-defstruct-args (defstruct-args)
    (destructuring-bind (name-and-options &rest slots-and-doc) defstruct-args
      (multiple-value-bind (name options)
          (if (symbolp name-and-options)
              (values name-and-options nil)
              (values (first name-and-options)
                      (rest name-and-options)))
        (flet ((uncross-option (option)
                 (if (eq (first option) :include)
                     (destructuring-bind
                         (include-keyword included-name &rest rest)
                         option
                       `(,include-keyword
                         ,(uncross included-name)
                         ,@rest))
                   option)))
          `((,(uncross name)
             ,@(mapcar #'uncross-option options))
            ,@slots-and-doc))))))

;;; DEF!STRUCT's arguments are like DEFSTRUCT's arguments.
;;; DEF!STRUCT also does some magic to ensure that anything it defines
;;; includes STRUCTURE!OBJECT.
(defmacro def!struct (&rest args)
  (multiple-value-bind (name defstruct-args def!struct-supertype)
      (apply #'parse-def!struct-args args)
    `(progn
       ;; There are two valid cases here: creating the
       ;; STRUCTURE!OBJECT root of the inheritance hierarchy, or
       ;; inheriting from STRUCTURE!OBJECT somehow.
       ;;
       ;; The invalid case that we want to exclude is when an :INCLUDE
       ;; clause was used, and the included class didn't inherit from
       ;; STRUCTURE!OBJECT.
       ,@(if (eq name 'structure!object)
             (aver (null def!struct-supertype))
             `((aver (subtypep ',def!struct-supertype 'structure!object))))
       (defstruct ,@defstruct-args)
       #+sb-xc-host ,(let ((u (uncross-defstruct-args defstruct-args)))
                       (if (boundp '*delayed-def!structs*)
                           `(push (make-delayed-def!struct :args ',u)
                                  *delayed-def!structs*)
                           `(sb!xc:defstruct ,@u)))
       ',name)))

;;; When building the cross-compiler, this function has to be called
;;; some time after SB!XC:DEFSTRUCT is set up, in order to take care
;;; of any processing which had to be delayed until then.
#+sb-xc-host
(defun force-delayed-def!structs ()
  (if (boundp '*delayed-def!structs*)
      (progn
        (mapcar (lambda (x)
                  (let ((*package* (delayed-def!struct-package x)))
                    ;; KLUDGE(?): EVAL is almost always the wrong thing.
                    ;; However, since we have to map DEFSTRUCT over the
                    ;; list, and since ANSI declined to specify any
                    ;; functional primitives corresponding to the
                    ;; DEFSTRUCT macro, it seems to me that EVAL is
                    ;; required in there somewhere..
                    (eval `(sb!xc:defstruct ,@(delayed-def!struct-args x)))))
                (reverse *delayed-def!structs*))
        ;; We shouldn't need this list any more. Making it unbound
        ;; serves as a signal to DEF!STRUCT that it needn't delay
        ;; DEF!STRUCTs any more. It is also generally a good thing for
        ;; other reasons: it frees garbage, and it discourages anyone
        ;; else from pushing anything else onto the list later.
        (makunbound '*delayed-def!structs*))
      ;; This condition is probably harmless if it comes up when
      ;; interactively experimenting with the system by loading a source
      ;; file into it more than once. But it's worth warning about it
      ;; because it definitely shouldn't come up in an ordinary build
      ;; process.
      (warn "*DELAYED-DEF!STRUCTS* is already unbound.")))

;;; The STRUCTURE!OBJECT abstract class is the base of the type
;;; hierarchy for objects which have/use DEF!STRUCT functionality.
;;; (The extra hackery in DEF!STRUCT-defined things isn't needed for
;;; STRUCTURE-OBJECTs defined by ordinary, post-warm-init programs, so
;;; it's only put into STRUCTURE-OBJECTs which inherit from
;;; STRUCTURE!OBJECT.)
(def!struct (structure!object (:constructor nil) (:copier nil) (:predicate nil)))
