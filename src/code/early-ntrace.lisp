;;;; a tracing facility

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-DEBUG")

(defvar *trace-indentation-step* 2
  "the increase in trace indentation at each call level")

(defvar *max-trace-indentation* 40
  "If the trace indentation exceeds this value, then indentation restarts at
   0.")

(defvar *trace-encapsulate-default* t
  "the default value for the :ENCAPSULATE option to TRACE")

;;;; internal state

;;; a hash table that maps each traced function to the TRACE-INFO. The
;;; entry for a closure is the shared function entry object.
(define-load-time-global *traced-funs*
    (make-hash-table :test 'eq :synchronized t))

(deftype trace-report-type ()
  '(member nil trace))

;;; A TRACE-INFO object represents all the information we need to
;;; trace a given function.
(defstruct (trace-info
             (:print-object (lambda (x stream)
                              (print-unreadable-object (x stream :type t)
                                (prin1 (trace-info-what x) stream)))))
  ;; the original representation of the thing traced
  (what nil :type (or function cons symbol))
  ;; Is WHAT a function name whose definition we should track?
  (named nil)
  ;; Is tracing to be done by encapsulation rather than breakpoints?
  ;; T implies NAMED.
  (encapsulated *trace-encapsulate-default*)
  ;; Has this trace been untraced?
  (untraced nil)
  ;; breakpoints we set up to trigger tracing
  (start-breakpoint nil :type (or sb-di:breakpoint null))
  (end-breakpoint nil :type (or sb-di:breakpoint null))
  ;; the list of function names for WHEREIN, or NIL if unspecified
  (wherein nil :type list)
  ;; should we trace methods given a generic function to trace?
  (methods nil)

  ;; The following slots represent the forms that we are supposed to
  ;; evaluate on each iteration. Each form is represented by a cons
  ;; (Form . Function), where the Function is the cached result of
  ;; coercing Form to a function. Forms which use the current
  ;; environment are converted with PREPROCESS-FOR-EVAL, which gives
  ;; us a one-arg function. Null environment forms also have one-arg
  ;; functions, but the argument is ignored. NIL means unspecified
  ;; (the default.)

  ;; report type
  (report 'trace :type trace-report-type)
  ;; current environment forms
  (condition nil)
  (break nil)
  ;; List of current environment forms
  (print () :type list)
  ;; null environment forms
  (condition-after nil)
  (break-after nil)
  ;; list of null environment forms
  (print-after () :type list))
(!set-load-form-method trace-info (:target))

;;; This is a list of conses (fun-end-cookie . condition-satisfied),
;;; which we use to note distinct dynamic entries into functions. When
;;; we enter a traced function, we add a entry to this list holding
;;; the new end-cookie and whether the trace condition was satisfied.
;;; We must save the trace condition so that the after breakpoint
;;; knows whether to print. The length of this list tells us the
;;; indentation to use for printing TRACE messages.
;;;
;;; This list also helps us synchronize the TRACE facility dynamically
;;; for detecting non-local flow of control. Whenever execution hits a
;;; :FUN-END breakpoint used for TRACE'ing, we look for the
;;; FUN-END-COOKIE at the top of *TRACED-ENTRIES*. If it is not
;;; there, we discard any entries that come before our cookie.
;;;
;;; When we trace using encapsulation, we bind this variable and add
;;; (NIL . CONDITION-SATISFIED), so a NIL "cookie" marks an
;;; encapsulated tracing.
(defvar *traced-entries* ())
(declaim (list *traced-entries*))

;;; This variable is used to discourage infinite recursions when some
;;; trace action invokes a function that is itself traced. In this
;;; case, we quietly ignore the inner tracing.
(defvar *in-trace* nil)
