;;;; Compiler macros that are important for the target system

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;;; We often use a source-transform to do macro-like rewriting of an
;;;; ordinary function call. Source-transforms seem to pre-date the ANSI
;;;; specification and are redundant with compiler-macros.
;;;; In the interest of not multiplying entities needlessly, it should
;;;; be feasible to get rid of source-transforms.
;;;; A problem is namespace clobbering: these must not affect the host Lisp.

(define-compiler-macro append (&whole form &rest lists)
  (case (length lists)
    (0 nil)
    (1 (car lists))
    (2 `(append2 ,@lists))
    (t form)))

;;; A sanity-checker for an extremely common programmer error.
(define-compiler-macro format (&whole form destination control &rest args)
  (declare (ignore control args))
  (when (stringp destination)
    (warn "Literal string as destination in FORMAT:~%  ~S" form))
  form)
