;;;; We proclaim all the special variables in the COMMON-LISP package
;;;; here, in one go, just to try to make sure we don't miss any.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "COMMON-LISP")

(sb!int:file-comment
  "$Header$")

(sb!xc:proclaim '(special cl:*
			  cl:**
			  cl:***
			  cl:*break-on-signals*
			  cl:*compile-file-pathname*
			  cl:*compile-file-truename*
			  cl:*compile-print*
			  cl:*compile-verbose*
			  cl:*debug-io*
			  cl:*debugger-hook*
			  cl:*default-pathname-defaults*
			  cl:*error-output*
			  cl:*features*
			  cl:*gensym-counter*
			  cl:*load-pathname*
			  cl:*load-print*
			  cl:*load-truename*
			  cl:*load-verbose*
			  cl:*macroexpand-hook*
			  cl:*modules*
			  cl:*package*
			  cl:*print-array*
			  cl:*print-base*
			  cl:*print-case*
			  cl:*print-circle*
			  cl:*print-escape*
			  cl:*print-gensym*
			  cl:*print-length*
			  cl:*print-level*
			  cl:*print-lines*
			  cl:*print-miser-width*
			  cl:*print-pprint-dispatch*
			  cl:*print-pretty*
			  cl:*print-radix*
			  cl:*print-readably*
			  cl:*print-right-margin*
			  cl:*query-io*
			  cl:*random-state*
			  cl:*read-base*
			  cl:*read-default-float-format*
			  cl:*read-eval*
			  cl:*read-suppress*
			  cl:*readtable*
			  cl:*standard-input*
			  cl:*standard-output*
			  cl:*terminal-io*
			  cl:*trace-output*
			  cl:+
			  cl:++
			  cl:+++
			  cl:-
			  cl:/
			  cl://
			  cl:///))
