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

;;; machinery used in the implementation of SB-XC:DEFSTRUCT delaying

;; a description of a SB-XC:DEFSTRUCT call to be stored until we get
;; enough of the system running to finish processing it
(defstruct (delayed-defstruct (:constructor make-delayed-defstruct (args)))
  (args nil :type cons)
  ;; because the expansion autogenerates slot names based on current package
  (package (package-name *package*) :type string))
;; a list of DELAYED-DEFSTRUCTs stored until we get SB-XC:DEFSTRUCT
;; working fully so that we can apply it to them then. After
;; SB-XC:DEFSTRUCT is made to work fully, this list is processed, then
;; made unbound, and should no longer be used.
(defvar *delayed-defstructs* nil)

;;; DEF!STRUCT defines a structure for both the host and target.
;;; The host's structure inherits from STRUCTURE!OBJECT so that we can test
;;; for containment in the target's type hierarchy with minimal fuss.
;;; (A similar thing could be achieved by testing the package probably)
;;; When executed by the cross-compiler, DEF!STRUCT is just DEFSTRUCT.
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
    ;; Attempting to define a type named by a CL symbol is an error.
    ;; Therefore NAME is wrong if it uncrosses to something other than itself.
    (assert (eq (uncross name) name))
    (assert (equal (uncross options) options))
    `(progn
       (sb-xc:defstruct ,@args)
       ,@(when supertype
           `((aver (cl:subtypep ',supertype 'structure!object))))
       (defstruct (,name
                   ,@(unless supertype '((:include structure!object)))
                   ,@(remove :pure options :key #'car))
         ,@slots))))

;;; Workaround for questionable behavior of CCL - it issues a warning about
;;; a duplicate definition in src/code/defstruct if we use DEFMACRO here,
;;; despite the second occurrence being preceded by FMAKUNBOUND.
;;; The warning can not be remedied by putting this in its own compilation unit,
;;; which I can't even explain. I would have expected at worst a "redefinition"
;;; warning, not a "duplicate" in that case.
#+nil ; It would be this
(defmacro sb-xc:defstruct (&rest args)
  `(push (make-delayed-defstruct ',args) *delayed-defstructs*))
;;; But instead it's this. No eval-when needed, since we LOAD this file.
(setf (cl:macro-function 'sb-xc:defstruct)
      (lambda (form environment)
        (declare (ignore environment))
        `(push (make-delayed-defstruct ',(cdr form)) *delayed-defstructs*)))
