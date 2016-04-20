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

(eval-when (:compile-toplevel :load-toplevel :execute)

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

(sb!xc:proclaim '(type t cl:+ cl:++ cl:+++ cl:- cl:* cl:** cl:***))

;;; generalized booleans
(sb!xc:proclaim '(type t cl:*compile-print* cl:*compile-verbose*
                         cl:*load-print* cl:*load-verbose*
                         cl:*print-array* cl:*print-radix*
                         cl:*print-circle* cl:*print-escape*
                         cl:*print-gensym* cl:*print-pretty*
                         cl:*print-readably* cl:*read-eval*
                         cl:*read-suppress*))

(sb!xc:proclaim '(type sb!pretty::pprint-dispatch-table
                       cl:*print-pprint-dispatch*))

(sb!xc:proclaim '(type readtable cl:*readtable*))

(sb!xc:proclaim '(type (integer 2 36) cl:*print-base* cl:*read-base*))

(sb!xc:proclaim '(type (member :upcase :downcase :capitalize) cl:*print-case*))

(sb!xc:proclaim '(type (member cl:single-float cl:double-float
                        cl:short-float cl:long-float) cl:*read-default-float-format*))

(sb!xc:proclaim '(type list cl:/ cl:// cl:/// cl:*features* cl:*modules*))

(sb!xc:proclaim '(type sb!kernel:type-specifier cl:*break-on-signals*))

(sb!xc:proclaim '(type package cl:*package*))

(sb!xc:proclaim '(type random-state cl:*random-state*))

;; KLUDGE: some of these are more specific than just STREAM.  However,
;; (a) we can't express that portably, and (b) we probably violate
;; these requirements somewhere as of sbcl-0.8.0.  (and maybe we break
;; even this in Gray streams or simple-streams?  apparently not,
;; currently)
(sb!xc:proclaim '(type stream
                       cl:*standard-input*
                       cl:*error-output*
                       cl:*standard-output*
                       cl:*trace-output*
                       cl:*debug-io*
                       cl:*query-io*
                       cl:*terminal-io*))

;;; FIXME: make an SB!INT:FUNCTION-DESIGNATOR type for these
;;; DOUBLE-FIXME: I'm not convinced that either of these variables
;;; is actually allowed to be a CONS.
;;; CLHS would have said "_extended_ function designator"
;;; if it meant to allows (SETF f) as a designator.
(sb!xc:proclaim '(type (or function symbol cons)
                       cl:*debugger-hook*
                       cl:*macroexpand-hook*))

(sb!xc:proclaim '(type unsigned-byte cl:*gensym-counter*))

(sb!xc:proclaim '(type (or unsigned-byte null)
                       cl:*print-length*
                       cl:*print-level*
                       cl:*print-lines*
                       cl:*print-miser-width*
                       cl:*print-right-margin*))

(sb!xc:proclaim '(type pathname cl:*default-pathname-defaults*))

(sb!xc:proclaim '(type (or pathname null)
                       cl:*load-pathname*
                       cl:*load-truename*
                       cl:*compile-file-pathname*
                       cl:*compile-file-truename*))

) ; end EVAL-WHEN
