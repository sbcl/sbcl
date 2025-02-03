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
                #+ansi-compliant-load-truename cl:*load-truename*
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
                cl:///
                ;; extension/internal specials are also proclaimed early
                ;; to benefit from always-bound and precomputed TLS index.
                sb-kernel:*current-level-in-print*
                sb-ext:*print-vector-length*
                sb-ext:*print-circle-not-shared*)))
    `(progn
       (declaim (special ,@list)
                (sb-ext:always-bound ,@list))
       (eval-when (:compile-toplevel :load-toplevel)
         (dolist (symbol ',list)
           (declare (notinline (setf sb-int:info))) ; skirt failure-to-inline warning
           (setf (sb-int:info :variable :wired-tls symbol) t)))))

#-ansi-compliant-load-truename
(progn
;; this could almost go in target-load.lisp were it not for the use in ir1-translators
(defvar sb-fasl::%load-truename nil)
;; "The consequences are unspecified if an attempt is made to assign or bind [...]"
;;  http://www.lispworks.com/documentation/HyperSpec/Body/v_ld_pns.htm#STload-truenameST
;;
;; But the rationale provided in http://www.lispworks.com/documentation/HyperSpec/Issues/iss218_w.htm
;; for the existence of the -TRUENAME* variables is dubious:
;;  "Note that it is not adequate to just have the -PATHNAME* variables
;;   since TRUENAME on these pathnames might not yield the value of the
;;   -TRUENAME* variables if the file has been deleted or protected
;;   since the open occurred (in some implementations)."
;; because in our ANSI-compliant logic, there is a delay between opening a file
;; and determining its truename, which means that it might be impossible to determine
;; the truename even if done ASAP.  Supposing that somebody really were to delete
;; the very file being acted on, it's not LOAD's job to deal with race conditions.
(define-symbol-macro *load-truename*
    (sb-ext:truly-the (values (or pathname null) &optional)
      (sb-fasl::resolve-load-truename 'sb-fasl::%load-truename *load-pathname*))))

(declaim (type t cl:+ cl:++ cl:+++ cl:- cl:* cl:** cl:***))

;;; generalized booleans
(declaim (type t         cl:*compile-print* cl:*compile-verbose*
                         cl:*load-print* cl:*load-verbose*
                         cl:*print-array* cl:*print-radix*
                         cl:*print-circle* cl:*print-escape*
                         cl:*print-gensym* cl:*print-pretty*
                         cl:*print-readably* cl:*read-eval*
                         cl:*read-suppress*))

(declaim (type sb-pretty:pprint-dispatch-table cl:*print-pprint-dispatch*))

(declaim (type readtable cl:*readtable*))

(declaim (type (integer 2 36) cl:*print-base* cl:*read-base*))

(declaim (type (member :upcase :downcase :capitalize) cl:*print-case*))

(declaim (type (member single-float double-float short-float long-float cl:rational)
               cl:*read-default-float-format*))

(declaim (type list cl:/ cl:// cl:/// cl:*features* cl:*modules*))

(declaim (type sb-kernel:type-specifier cl:*break-on-signals*))

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

(declaim (type sb-kernel:function-designator cl:*debugger-hook* cl:*macroexpand-hook*))

(declaim (type unsigned-byte cl:*gensym-counter*))

(declaim (type (or unsigned-byte null)
                       cl:*print-length*
                       sb-ext:*print-vector-length*
                       cl:*print-level*
                       cl:*print-lines*
                       cl:*print-miser-width*
                       cl:*print-right-margin*))

(declaim (type pathname cl:*default-pathname-defaults*))

;;; Some functions for which we should *never* (or almost never) bind eagerly
;;; to the functional definition.
;;; IR2-CONVERT-GLOBAL-VAR won't use a :KNOWN-FUN constant-tn, but will instead
;;; dereference the fdefinition like for most function calls.
;;; They aren't actually "inlined", but they were bypassing the fdefinition in
;;; situations involving (APPLY ...) which rendered encapsulation impossible.
(declaim (notinline open compile-file load compile))

(in-package "SB-IMPL")


;;; Ensure some VM symbols get wired TLS.
(in-package "SB-VM")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (dolist (entry '#.per-thread-c-interface-symbols)
    (let ((symbol (if (consp entry) (car entry) entry)))
      (declare (notinline sb-int:info (setf sb-int:info)))
      ;; CURRENT-{CATCH/UWP}-BLOCK are thread slots,
      ;; so the TLS indices were already assigned.
      ;; There may be other symbols too.
      (unless (sb-int:info :variable :wired-tls symbol)
        (setf (sb-int:info :variable :wired-tls symbol) :always-thread-local))
      (unless (sb-int:info :variable :always-bound symbol)
        (setf (sb-int:info :variable :always-bound symbol) :always-bound)))))
