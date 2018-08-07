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

#.(let ((list '(cl:*
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
                cl:///)))
    `(progn
       (declaim (special ,@list)
                (sb!ext:always-bound ,@list))
       (eval-when (:compile-toplevel :load-toplevel)
         (dolist (symbol ',list)
           (declare (notinline (setf sb!int:info))) ; skirt failure-to-inline warning
           (setf (sb!int:info :variable :wired-tls symbol) t)))))

(declaim (type t cl:+ cl:++ cl:+++ cl:- cl:* cl:** cl:***))

;;; generalized booleans
(declaim (type t         cl:*compile-print* cl:*compile-verbose*
                         cl:*load-print* cl:*load-verbose*
                         cl:*print-array* cl:*print-radix*
                         cl:*print-circle* cl:*print-escape*
                         cl:*print-gensym* cl:*print-pretty*
                         cl:*print-readably* cl:*read-eval*
                         cl:*read-suppress*))

(declaim (type sb!pretty::pprint-dispatch-table cl:*print-pprint-dispatch*))

(declaim (type readtable cl:*readtable*))

(declaim (type (integer 2 36) cl:*print-base* cl:*read-base*))

(declaim (type (member :upcase :downcase :capitalize) cl:*print-case*))

(declaim (type (member cl:single-float cl:double-float
                       cl:short-float cl:long-float) cl:*read-default-float-format*))

(declaim (type list cl:/ cl:// cl:/// cl:*features* cl:*modules*))

(declaim (type sb!kernel:type-specifier cl:*break-on-signals*))

(declaim (type package cl:*package*))

(declaim (type random-state cl:*random-state*))

;; KLUDGE: some of these are more specific than just STREAM.  However,
;; (a) we can't express that portably, and (b) we probably violate
;; these requirements somewhere as of sbcl-0.8.0.  (and maybe we break
;; even this in Gray streams or simple-streams?  apparently not,
;; currently)
(declaim (type stream  cl:*standard-input*
                       cl:*error-output*
                       cl:*standard-output*
                       cl:*trace-output*
                       cl:*debug-io*
                       cl:*query-io*
                       cl:*terminal-io*))

(declaim (type (or function symbol) cl:*debugger-hook* cl:*macroexpand-hook*))

(declaim (type unsigned-byte cl:*gensym-counter*))

(declaim (type (or unsigned-byte null)
                       cl:*print-length*
                       cl:*print-level*
                       cl:*print-lines*
                       cl:*print-miser-width*
                       cl:*print-right-margin*))

(declaim (type pathname cl:*default-pathname-defaults*))

(declaim (type (or pathname null)
                       cl:*load-pathname*
                       cl:*load-truename*
                       cl:*compile-file-pathname*
                       cl:*compile-file-truename*))

;;;; DEFGLOBAL and DEFINE-LOAD-TIME-GLOBAL
;;;; These have alternate definitions (in cross-misc) which rely on
;;;; the underlying host DEFVAR when building the cross-compiler.

(in-package "SB!IMPL")

(defmacro defglobal (name value &optional (doc nil docp))
  "Defines NAME as a global variable that is always bound. VALUE is evaluated
and assigned to NAME both at compile- and load-time, but only if NAME is not
already bound.

Global variables share their values between all threads, and cannot be
locally bound, declared special, defined as constants, and neither bound
nor defined as symbol macros.

See also the declarations SB-EXT:GLOBAL and SB-EXT:ALWAYS-BOUND."
  (let ((boundp (make-symbol "BOUNDP")))
    `(progn
       (eval-when (:compile-toplevel)
         (let ((,boundp (boundp ',name)))
           (%compiler-defglobal ',name :always-bound
                                (unless ,boundp ,value) (not ,boundp))))
       (let ((,boundp (boundp ',name)))
         (%defglobal ',name (unless ,boundp ,value) ,boundp ',doc ,docp
                     (sb!c:source-location))))))

(defmacro define-load-time-global (name value &optional (doc nil docp))
  "Defines NAME as a global variable that is always bound. VALUE is evaluated
and assigned to NAME at load-time, but only if NAME is not already bound.

Attempts to read NAME at compile-time will signal an UNBOUND-VARIABLE error
unless it has otherwise been assigned a value.

See also DEFGLOBAL which assigns the VALUE at compile-time too."
  (let ((boundp (make-symbol "BOUNDP")))
    `(progn
       (eval-when (:compile-toplevel)
         (%compiler-defglobal ',name :eventually nil nil))
       (let ((,boundp (boundp ',name)))
         (%defglobal ',name (unless ,boundp ,value) ,boundp ',doc ,docp
                     (sb!c:source-location))))))
