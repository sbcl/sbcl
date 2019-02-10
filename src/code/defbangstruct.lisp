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

(in-package "SB-KERNEL")

;;; Has the type system been properly initialized? (I.e. is it OK to
;;; use it?)
(!define-load-time-global *type-system-initialized* nil)

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

;;; a helper function for DEF!STRUCT in the #+SB-XC-HOST case: Return
;;; DEFSTRUCT-style arguments with any class names in the SB-XC
;;; package (i.e. the name of the class being defined, and/or the
;;; names of classes in :INCLUDE clauses) converted from SB-XC::FOO to
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
          ;; Attempting to define a type named by a CL symbol is an error.
          ;; Therefore NAME is wrong if it uncrosses to something other than itself.
          (assert (eq (uncross name) name))
          `((,name ,@(mapcar #'uncross-option options))
            ,@slots-and-doc))))))

;;; DEF!STRUCT defines a structure for both the host and target.
;;; The host's structure inherits from STRUCTURE!OBJECT so that we can test
;;; for containment in the target's type hierarchy with minimal fuss.
;;; (A similar thing could be achieved by testing the package probably)
;;; When executed by the cross-compiler, DEF!STRUCT is just DEFSTRUCT.
#+sb-xc-host
(defmacro def!struct (&rest args)
  (multiple-value-bind (name supertype options slots)
      (destructuring-bind (nameoid &rest slots) args
        (multiple-value-bind (name options)
            (if (consp nameoid)
                (values (first nameoid) (rest nameoid))
                (values nameoid nil))
          (declare (type list options))
          (let ((include-clause (find :include options :key #'first)))
            (when (find :type options :key #'first)
              (error "can't use :TYPE option in DEF!STRUCT"))
            (values name (second include-clause) options slots))))
    `(progn
       ,@(when supertype
           `((aver (subtypep ',supertype 'structure!object))))
       (defstruct (,name
                   ,@(unless supertype '((:include structure!object)))
                   ,@options) ,@slots)
       ,(let ((u (uncross-defstruct-args args)))
          (if (boundp '*delayed-def!structs*)
              `(push (make-delayed-def!struct :args ',u)
                     *delayed-def!structs*)
              `(sb-xc:defstruct ,@u)))
       ',name)))
#-sb-xc-host
(defmacro def!struct (&rest args) `(defstruct ,@args))

;;; When building the cross-compiler, this function has to be called
;;; some time after SB-XC:DEFSTRUCT is set up, in order to take care
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
                    (eval `(sb-xc:defstruct ,@(delayed-def!struct-args x)))))
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
