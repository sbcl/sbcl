;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-COLD")

;;;; $Header$

;;; Either load or compile-then-load the cross-compiler into the
;;; cross-compilation host Common Lisp.
(defun load-or-cload-xcompiler (load-or-cload-stem)

  ;; The running-in-the-host-Lisp Python cross-compiler defines its
  ;; own versions of a number of functions which should not overwrite
  ;; host-Lisp functions. Instead we put them in a special package.
  ;;
  ;; The common theme of the functions, macros, constants, and so
  ;; forth in this package is that they run in the host and affect the
  ;; compilation of the target.
  (let ((package-name "SB-XC"))
    (make-package package-name :use nil :nicknames nil)
    (dolist (name '("*COMPILE-FILE-PATHNAME*"
		    "*COMPILE-FILE-TRUENAME*"
		    "*COMPILE-PRINT*"
		    "*COMPILE-VERBOSE*"
		    "ARRAY-RANK-LIMIT"
		    "ARRAY-DIMENSION-LIMIT"
		    "ARRAY-TOTAL-SIZE-LIMIT"
		    "BUILT-IN-CLASS"
		    "CLASS" "CLASS-NAME" "CLASS-OF"
		    "COMPILE-FILE"
		    "COMPILE-FILE-PATHNAME"
		    "COMPILER-MACRO-FUNCTION"
		    "CONSTANTP"
		    "DEFCONSTANT"
		    "DEFINE-MODIFY-MACRO"
		    "DEFINE-SETF-EXPANDER"
		    "DEFMACRO" "DEFSETF" "DEFSTRUCT" "DEFTYPE"
		    "FBOUNDP" "FDEFINITION" "FMAKUNBOUND"
		    "FIND-CLASS"
		    "GET-SETF-EXPANSION"
		    "LAMBDA-LIST-KEYWORDS"
		    "LISP-IMPLEMENTATION-TYPE" "LISP-IMPLEMENTATION-VERSION"
		    "MACRO-FUNCTION"
		    "MACROEXPAND" "MACROEXPAND-1" "*MACROEXPAND-HOOK*"
		    "MAKE-LOAD-FORM"
		    "PACKAGE" "PACKAGEP"
		    "PROCLAIM"
		    "SPECIAL-OPERATOR-P"
		    "STANDARD-CLASS"
		    "STRUCTURE-CLASS"
		    "SUBTYPEP"
		    "TYPE-OF" "TYPEP"
		    "WITH-COMPILATION-UNIT"))
      (export (intern name package-name) package-name)))

  ;; Build a version of Python to run in the host Common Lisp, to be
  ;; used only in cross-compilation.
  ;;
  ;; Note that files which are marked :ASSEM, to cause them to be
  ;; processed with SB!C:ASSEMBLE-FILE when we're running under the
  ;; cross-compiler or the target lisp, are still processed here, just
  ;; with the ordinary Lisp compiler, and this is intentional, in
  ;; order to make the compiler aware of the definitions of assembly
  ;; routines.
  (for-stems-and-flags (stem flags)
    (unless (find :not-host flags)
      (funcall load-or-cload-stem
	       stem
	       :ignore-failure-p (find :ignore-failure-p flags))
      #!+sb-show (warn-when-cl-snapshot-diff *cl-snapshot*)))

  ;; If the cross-compilation host is SBCL itself, we can use the
  ;; PURIFY extension to freeze everything in place, reducing the
  ;; amount of work done on future GCs. In machines with limited
  ;; memory, this could help, by reducing the amount of memory which
  ;; needs to be juggled in a full GC. And it can hardly hurt, since
  ;; (in the ordinary build procedure anyway) essentially everything
  ;; which is reachable at this point will remain reachable for the
  ;; entire run.
  #+sbcl (sb-ext:purify)

  (values))
