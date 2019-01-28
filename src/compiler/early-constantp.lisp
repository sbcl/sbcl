;;;; inline wrappers on CONSTANTP stuff

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;; Subtypes of this show up as the environment argument to inquiry functions.
(defstruct (abstract-lexenv
             (:constructor nil) (:copier nil) (:predicate nil)))

#-sb-fluid (declaim (inline sb-xc:constantp))
(defun sb-xc:constantp (form &optional (environment nil envp))
  "True of any FORM that has a constant value: self-evaluating objects,
keywords, defined constants, quote forms. Additionally the
constant-foldability of some function calls and special forms is recognized.
If ENVIRONMENT is provided, the FORM is first macroexpanded in it."
  (%constantp form environment envp))

#-sb-fluid (declaim (inline constant-form-value))
(defun constant-form-value (form &optional (environment nil envp))
  "Returns the value of the constant FORM in ENVIRONMENT. Behaviour
is undefined unless CONSTANTP has been first used to determine the
constantness of the FORM in ENVIRONMENT."
  (%constant-form-value form environment envp))

(declaim (inline constant-typep))
(defun constant-typep (form type &optional (environment nil envp))
  (and (%constantp form environment envp)
       ;; FIXME: We probably should be passing the environment to
       ;; TYPEP too, but (1) our XC version of typep AVERs that the
       ;; environment is null (2) our real version ignores it anyhow.
       (sb-xc:typep (%constant-form-value form environment envp) type)))
