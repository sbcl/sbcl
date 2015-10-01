;;;; miscellaneous error stuff that needs to be in the cold load

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

(!defvar *break-on-signals* nil
  #!+sb-doc
  "When (TYPEP condition *BREAK-ON-SIGNALS*) is true, then calls to SIGNAL will
   enter the debugger prior to signalling that condition.")

(defun maybe-break-on-signal (condition)
  (let ((old-bos *break-on-signals*)
        (bos-actually-breaking nil))
    (restart-case
        (let ((break-on-signals *break-on-signals*)
              (*break-on-signals* nil))
          ;; The rebinding encloses the TYPEP so that a bogus
          ;; type specifier will not lead to infinite recursion when
          ;; TYPEP fails.
          (when (typep condition break-on-signals)
            (setf bos-actually-breaking t)
            (break "~A~%BREAK was entered because of *BREAK-ON-SIGNALS* ~
                    (now rebound to NIL)."
                   condition)))
      ;; Give the user a chance to unset *BREAK-ON-SIGNALS* on the
      ;; way out.
      ;;
      ;; (e.g.: Consider a long compilation. After a failed compile
      ;; the user sets *BREAK-ON-SIGNALS* to T, and select the
      ;; RECOMPILE restart. Once the user diagnoses and fixes the
      ;; problem, he selects RECOMPILE again... and discovers that
      ;; he's entered the *BREAK-ON-SIGNALS* hell with no escape,
      ;; unless we provide this restart.)
      (reassign (new-value)
        :report
        (lambda (stream)
          (format stream
                  (if bos-actually-breaking
                      "Return from BREAK and assign a new value to ~
                       *BREAK-ON-SIGNALS*."
                      "Assign a new value to *BREAK-ON-SIGNALS* and ~
                       continue with signal handling.")))
        :interactive
        (lambda ()
          (let (new-value)
            (loop
              (format *query-io*
                      "Enter new value for *BREAK-ON-SIGNALS*. ~
                       Current value is ~S.~%~
                       > "
                      old-bos)
              (force-output *query-io*)
              (let ((*break-on-signals* nil))
                (setf new-value (eval (read *query-io*)))
                (if (typep new-value 'type-specifier)
                    (return)
                    (format *query-io*
                            "~S is not a valid value for *BREAK-ON-SIGNALS* ~
                             (must be a type-specifier).~%"
                            new-value))))
            (list new-value)))
        (setf *break-on-signals* new-value)))))

(defun signal (datum &rest arguments)
  #!+sb-doc
  "Invokes the signal facility on a condition formed from DATUM and
   ARGUMENTS. If the condition is not handled, NIL is returned. If
   (TYPEP condition *BREAK-ON-SIGNALS*) is true, the debugger is invoked
   before any signalling is done."
  (let* ((condition
          (coerce-to-condition datum arguments 'simple-condition 'signal))
         (handler-clusters *handler-clusters*)
         (sb!debug:*stack-top-hint* (or sb!debug:*stack-top-hint* 'signal))
         ;; possible FIXME: despite that fndb says COERCE-TO-CONDITION
         ;; returns CONDITION, compiler doesn't know that CONDITION
         ;; is an INSTANCE and emits a type-check.
         (layout (%instance-layout condition)))
    (when *break-on-signals*
      (maybe-break-on-signal condition))
    (do ((cluster (pop handler-clusters) (pop handler-clusters)))
        ((null cluster)
         nil)
      ;; Remove CLUSTER from *HANDLER-CLUSTERS*: if a condition is
      ;; signaled in either the type test, i.e. (the function (car
      ;; handler)), or the handler, (the function (cdr handler)), the
      ;; recursive SIGNAL call should not consider CLUSTER as doing
      ;; would lead to infinite recursive SIGNAL calls.
      (let ((*handler-clusters* handler-clusters))
        (dolist (handler cluster)
          (macrolet ((cast-to-fun (f possibly-symbolp)
                       ;; For efficiency the cases are tested in this order:
                       ;;  - FUNCTIONP is just a lowtag test
                       ;;  - FDEFN-P is a lowtag + widetag.
                       ;; Avoiding a SYMBOLP test is fine because
                       ;; SYMBOL-FUNCTION rejects bogosity anyway.
                       `(let ((f ,f))
                          (cond ((functionp f) f)
                                (,(if possibly-symbolp `(fdefn-p f) 't)
                                  (sb!c:safe-fdefn-fun f))
                                ,@(if possibly-symbolp
                                      `((t (symbol-function f))))))))
            (let ((test (car (truly-the cons handler))))
              (when (if (%instancep test) ; a condition classoid cell
                        (classoid-cell-typep layout test condition)
                        (funcall (cast-to-fun test nil) condition))
                (funcall (cast-to-fun (cdr handler) t) condition)))))))))

;;;; working with *CURRENT-ERROR-DEPTH* and *MAXIMUM-ERROR-DEPTH*

;;; counts of nested errors (with internal errors double-counted)
(defvar *maximum-error-depth*) ; this gets set to 10 in !COLD-INIT
(!defvar *current-error-depth* 0)

;;; INFINITE-ERROR-PROTECT is used by ERROR and friends to keep us out
;;; of hyperspace.
(defmacro infinite-error-protect (&rest forms)
  `(let ((*current-error-depth* (infinite-error-protector)))
     (/show0 "in INFINITE-ERROR-PROTECT, incremented error depth")
     ;; arbitrary truncation
     #!+sb-show (sb!debug:print-backtrace :count 8)
     ,@forms))

;;; a helper function for INFINITE-ERROR-PROTECT
(defun infinite-error-protector ()
  (/show0 "entering INFINITE-ERROR-PROTECTOR, *CURRENT-ERROR-DEPTH*=..")
  (/hexstr *current-error-depth*)
  ;; *MAXIMUM-ERROR-DEPTH* is not bound during cold-init, and testing BOUNDP
  ;; is superfluous since REALP will return false either way.
  (let ((cur (locally (declare (optimize (safety 0))) *current-error-depth*))
        (max (locally (declare (optimize (safety 0))) *maximum-error-depth*)))
    (cond ((or (not (fixnump cur)) (not (fixnump max)))
           (%primitive print "Argh! corrupted error depth, halting")
           (%primitive sb!c:halt))
          ((> cur max)
           (/show0 "*MAXIMUM-ERROR-DEPTH*=..")
           (/hexstr max)
           (/show0 "in INFINITE-ERROR-PROTECTOR, calling ERROR-ERROR")
           (sb!impl::error-error "Help! "
                        cur
                        " nested errors. "
                        "SB-KERNEL:*MAXIMUM-ERROR-DEPTH* exceeded."))
          (t
           (/show0 "returning normally from INFINITE-ERROR-PROTECTOR")
           (1+ *current-error-depth*)))))

(defun error (datum &rest arguments)
  #!+sb-doc
  "Invoke the signal facility on a condition formed from DATUM and ARGUMENTS.
  If the condition is not handled, the debugger is invoked."
  (/show0 "entering ERROR, argument list=..")
  (/hexstr arguments)

  (/show0 "cold-printing ERROR arguments one by one..")
  #!+sb-show (dolist (argument arguments)
               (sb!impl::cold-print argument))
  (/show0 "done cold-printing ERROR arguments")

  (infinite-error-protect
    (let ((condition (coerce-to-condition datum arguments
                                          'simple-error 'error))
          (sb!debug:*stack-top-hint* (or sb!debug:*stack-top-hint* 'error)))
      (/show0 "done coercing DATUM to CONDITION")
      (/show0 "signalling CONDITION from within ERROR")
      (signal condition)
      (/show0 "done signalling CONDITION within ERROR")
      (invoke-debugger condition))))

(defun cerror (continue-string datum &rest arguments)
  (infinite-error-protect
    (with-simple-restart
        (continue "~A" (apply #'format nil continue-string arguments))
      (let ((condition (coerce-to-condition datum
                                            arguments
                                            'simple-error
                                            'cerror)))
        (with-condition-restarts condition (list (find-restart 'continue))
          (let ((sb!debug:*stack-top-hint* (or sb!debug:*stack-top-hint* 'cerror)))
            (signal condition)
            (invoke-debugger condition))))))
  nil)

;;; like BREAK, but without rebinding *DEBUGGER-HOOK* to NIL, so that
;;; we can use it in system code (e.g. in SIGINT handling) without
;;; messing up --disable-debugger mode (which works by setting
;;; *DEBUGGER-HOOK*); or for that matter, without messing up ordinary
;;; applications which try to do similar things with *DEBUGGER-HOOK*
(defun %break (what &optional (datum "break") &rest arguments)
  (infinite-error-protect
    (with-simple-restart (continue "Return from ~S." what)
      (let ((sb!debug:*stack-top-hint* (or sb!debug:*stack-top-hint* '%break)))
        (invoke-debugger
         (coerce-to-condition datum arguments 'simple-condition what)))))
  nil)

(defun break (&optional (datum "break") &rest arguments)
  #!+sb-doc
  "Print a message and invoke the debugger without allowing any possibility
of condition handling occurring."
  (let ((*debugger-hook* nil) ; as specifically required by ANSI
        (sb!debug:*stack-top-hint* (or sb!debug:*stack-top-hint* 'break)))
    (apply #'%break 'break datum arguments)))

(defun %warn (datum arguments super default-type)
  (infinite-error-protect
    (let ((condition (coerce-to-condition datum arguments default-type 'warn))
          (superclassoid-name (classoid-name super)))
      ;: CONDITION is necessarily an INSTANCE,
      ;; but pedantry requires it be the right subtype of instance.
      (unless (classoid-typep (%instance-layout condition)
                              super condition)
        (error 'simple-type-error
               :datum datum :expected-type superclassoid-name
               :format-control "~S does not designate a ~A class"
               :format-arguments (list datum superclassoid-name)))
      (restart-case (signal condition)
        (muffle-warning ()
          :report "Skip warning."
          (return-from %warn nil)))
      (format *error-output* "~&~@<~S: ~3i~:_~A~:>~%"
              superclassoid-name condition)))
  nil)

(defun warn (datum &rest arguments)
  #!+sb-doc
  "Warn about a situation by signalling a condition formed by DATUM and
   ARGUMENTS. While the condition is being signaled, a MUFFLE-WARNING restart
   exists that causes WARN to immediately return NIL."
  (%warn datum arguments (find-classoid 'warning) 'simple-warning))

(defun style-warn (datum &rest arguments)
  (%warn datum arguments (find-classoid 'style-warning) 'simple-style-warning))
