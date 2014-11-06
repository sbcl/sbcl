;;;; a tracing facility

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-DEBUG") ; (SB-, not SB!, since we're built in warm load.)

;;; FIXME: Why, oh why, doesn't the SB-DEBUG package use the SB-DI
;;; package? That would let us get rid of a whole lot of stupid
;;; prefixes..

(defvar *trace-indentation-step* 2
  #+sb-doc
  "the increase in trace indentation at each call level")

(defvar *max-trace-indentation* 40
  #+sb-doc
  "If the trace indentation exceeds this value, then indentation restarts at
   0.")

(defvar *trace-encapsulate-default* t
  #+sb-doc
  "the default value for the :ENCAPSULATE option to TRACE")

;;;; internal state

;;; a hash table that maps each traced function to the TRACE-INFO. The
;;; entry for a closure is the shared function entry object.
(defvar *traced-funs* (make-hash-table :test 'eq :synchronized t))

;;; A TRACE-INFO object represents all the information we need to
;;; trace a given function.
(def!struct (trace-info
             (:make-load-form-fun sb-kernel:just-dump-it-normally)
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

;;;; utilities

;;; Given a function name, a function or a macro name, return the raw
;;; definition and some information. "Raw" means that if the result is
;;; a closure, we strip off the closure and return the bare code. The
;;; second value is T if the argument was a function name. The third
;;; value is one of :COMPILED, :COMPILED-CLOSURE, :INTERPRETED,
;;; :INTERPRETED-CLOSURE and :FUNCALLABLE-INSTANCE.
(defun trace-fdefinition (x)
  (flet ((get-def ()
           (if (valid-function-name-p x)
               (if (fboundp x)
                   (fdefinition x)
                   (warn "~/sb-impl::print-symbol-with-prefix/ is ~
                          undefined, not tracing." x))
               (warn "~S is not a valid function name, not tracing." x))))
    (multiple-value-bind (res named-p)
        (typecase x
         (symbol
          (cond ((special-operator-p x)
                 (warn "~S is a special operator, not tracing." x))
                ((macro-function x))
                (t
                 (values (get-def) t))))
         (function
          x)
         (t
          (values (get-def) t)))
     (typecase res
       (closure
        (values (sb-kernel:%closure-fun res)
                named-p
                :compiled-closure))
       (funcallable-instance
        (values res named-p :funcallable-instance))
       ;; FIXME: What about SB!EVAL:INTERPRETED-FUNCTION -- it gets picked off
       ;; by the FIN above, is that right?
       (t
        (values res named-p :compiled))))))

;;; When a function name is redefined, and we were tracing that name,
;;; then untrace the old definition and trace the new one.
(defun trace-redefined-update (fname new-value)
  (when (fboundp fname)
    (let* ((fun (trace-fdefinition fname))
           (info (gethash fun *traced-funs*)))
      (when (and info (trace-info-named info))
        (untrace-1 fname)
        (trace-1 fname info new-value)))))
(push #'trace-redefined-update *setf-fdefinition-hook*)

;;; Annotate a FORM to evaluate with pre-converted functions. FORM is
;;; really a cons (EXP . FUNCTION). LOC is the code location to use
;;; for the lexical environment. If LOC is NIL, evaluate in the null
;;; environment. If FORM is NIL, just return NIL.
(defun coerce-form (form loc)
  (when form
    (let ((exp (car form)))
      (if (sb-di:code-location-p loc)
          (let ((fun (sb-di:preprocess-for-eval exp loc)))
            (declare (type function fun))
            (cons exp
                  (lambda (frame &rest args)
                    (declare (ignore args))
                    (let ((*current-frame* frame))
                      (funcall fun frame)))))
          (let* ((body `(locally (declare (disable-package-locks sb-debug:arg))
                          (flet ((sb-debug:arg (n)
                                   (elt args n)))
                            (declare (ignorable #'sb-debug:arg)
                                     (enable-package-locks sb-debug:arg))
                            ,exp)))
                 (fun (coerce `(lambda (&rest args) (declare (ignorable args))
                                 ,body) 'function)))
            (cons exp
                  (lambda (frame &rest args)
                    (declare (ignore frame))
                    (let ((*current-frame* nil))
                      (apply fun args)))))))))

(defun coerce-form-list (forms loc)
  (mapcar (lambda (x) (coerce-form x loc)) forms))

;;; Print indentation according to the number of trace entries.
;;; Entries whose condition was false don't count.
(defun print-trace-indentation ()
  (let ((depth 0))
    (dolist (entry *traced-entries*)
      (when (cdr entry) (incf depth)))
    (format t
            "~V,0@T~W: "
            (+ (mod (* depth *trace-indentation-step*)
                    (- *max-trace-indentation* *trace-indentation-step*))
               *trace-indentation-step*)
            depth)))

;;; Return true if any of the NAMES appears on the stack below FRAME.
(defun trace-wherein-p (frame names)
  (do ((frame (sb-di:frame-down frame) (sb-di:frame-down frame)))
      ((not frame) nil)
    (when (member (sb-di:debug-fun-name (sb-di:frame-debug-fun frame))
                  names
                  :test #'equal)
      (return t))))

;;; Handle PRINT and PRINT-AFTER options.
(defun trace-print (frame forms &rest args)
  (dolist (ele forms)
    (fresh-line)
    (print-trace-indentation)
    (format t "~@<~S ~_= ~:[; No values~;~:*~{~S~^, ~}~]~:>"
            (car ele)
            (multiple-value-list (apply (cdr ele) frame args)))
    (terpri)))

;;; Test a BREAK option, and if true, break.
(defun trace-maybe-break (info break where frame &rest args)
  (when (and break (apply (cdr break) frame args))
    (sb-di:flush-frames-above frame)
    (let ((*stack-top-hint* frame))
      (break "breaking ~A traced call to ~S:"
             where
             (trace-info-what info)))))

;;; Discard any invalid cookies on our simulated stack. Encapsulated
;;; entries are always valid, since we bind *TRACED-ENTRIES* in the
;;; encapsulation.
(defun discard-invalid-entries (frame)
  (loop
    (when (or (null *traced-entries*)
              (let ((cookie (caar *traced-entries*)))
                (or (not cookie)
                    (sb-di:fun-end-cookie-valid-p frame cookie))))
      (return))
    (pop *traced-entries*)))

;;;; hook functions

;;; Return a closure that can be used for a function start breakpoint
;;; hook function and a closure that can be used as the FUN-END-COOKIE
;;; function. The first communicates the sense of the
;;; TRACE-INFO-CONDITION to the second via a closure variable.
(defun trace-start-breakpoint-fun (info)
  (let (conditionp)
    (values
     (lambda (frame bpt &rest args)
       (declare (ignore bpt))
       (discard-invalid-entries frame)
       (let ((condition (trace-info-condition info))
             (wherein (trace-info-wherein info)))
         (setq conditionp
               (and (not *in-trace*)
                    (or (not condition)
                        (apply (cdr condition) frame args))
                    (or (not wherein)
                        (trace-wherein-p frame wherein)))))
       (when conditionp
         (let ((sb-kernel:*current-level-in-print* 0)
               (*standard-output* (make-string-output-stream))
               (*in-trace* t))
           (fresh-line)
           (print-trace-indentation)
           (if (trace-info-encapsulated info)
               (prin1 `(,(trace-info-what info)
                        ,@(mapcar #'ensure-printable-object args)))
               (print-frame-call frame *standard-output*))
           (terpri)
           (apply #'trace-print frame (trace-info-print info) args)
           (write-sequence (get-output-stream-string *standard-output*)
                           *trace-output*)
           (finish-output *trace-output*))
         (apply #'trace-maybe-break info (trace-info-break info) "before"
                frame args)))
     (lambda (frame cookie)
       (declare (ignore frame))
       (push (cons cookie conditionp) *traced-entries*)))))

;;; This prints a representation of the return values delivered.
;;; First, this checks to see that cookie is at the top of
;;; *TRACED-ENTRIES*; if it is not, then we need to adjust this list
;;; to determine the correct indentation for output. We then check to
;;; see whether the function is still traced and that the condition
;;; succeeded before printing anything.
(declaim (ftype (function (trace-info) function) trace-end-breakpoint-fun))
(defun trace-end-breakpoint-fun (info)
  (lambda (frame bpt values cookie)
    (declare (ignore bpt))
    (unless (eq cookie (caar *traced-entries*))
      (setf *traced-entries*
            (member cookie *traced-entries* :key #'car)))

    (let ((entry (pop *traced-entries*)))
      (when (and (not (trace-info-untraced info))
                 (or (cdr entry)
                     (let ((cond (trace-info-condition-after info)))
                       (and cond (apply #'funcall (cdr cond) frame values)))))
        (let ((sb-kernel:*current-level-in-print* 0)
              (*standard-output* (make-string-output-stream))
              (*in-trace* t))
          (fresh-line)
          (let ((*print-pretty* t))
            (pprint-logical-block (*standard-output* nil)
              (print-trace-indentation)
              (pprint-indent :current 2)
              (format t "~S returned" (trace-info-what info))
              (dolist (v values)
                (write-char #\space)
                (pprint-newline :linear)
                (prin1 (ensure-printable-object v))))
            (terpri))
          (apply #'trace-print frame (trace-info-print-after info) values)
          (write-sequence (get-output-stream-string *standard-output*)
                          *trace-output*)
          (finish-output *trace-output*))
        (apply #'trace-maybe-break info (trace-info-break-after info) "after"
               frame values)))))

;;; This function is called by the trace encapsulation. It calls the
;;; breakpoint hook functions with NIL for the breakpoint and cookie,
;;; which we have cleverly contrived to work for our hook functions.
(defun trace-call (info function &rest args)
  (multiple-value-bind (start cookie) (trace-start-breakpoint-fun info)
    (declare (type function start cookie))
    (let ((frame (sb-di:frame-down (sb-di:top-frame))))
      (apply #'funcall start frame nil args)
      (let ((*traced-entries* *traced-entries*))
        (funcall cookie frame nil)
        (let ((vals (multiple-value-list (apply function args))))
          (funcall (trace-end-breakpoint-fun info) frame nil vals nil)
          (values-list vals))))))

;;; Trace one function according to the specified options. We copy the
;;; trace info (it was a quoted constant), fill in the functions, and
;;; then install the breakpoints or encapsulation.
;;;
;;; If non-null, DEFINITION is the new definition of a function that
;;; we are automatically retracing.
(defun trace-1 (function-or-name info &optional definition)
  (multiple-value-bind (fun named kind)
      (if definition
          (values definition t
                  (nth-value 2 (trace-fdefinition definition)))
          (trace-fdefinition function-or-name))
    (when fun
      (when (gethash fun *traced-funs*)
        (warn "~S is already TRACE'd, untracing it first." function-or-name)
        (untrace-1 fun))
      (let* ((debug-fun (sb-di:fun-debug-fun fun))
             (encapsulated
              (if (eq (trace-info-encapsulated info) :default)
                  (ecase kind
                    (:compiled nil)
                    (:compiled-closure
                     (unless (functionp function-or-name)
                       (warn "tracing shared code for ~S:~%  ~S"
                             function-or-name
                             fun))
                     nil)
                    ((:interpreted :interpreted-closure :funcallable-instance)
                     t))
                  (trace-info-encapsulated info)))
             (loc (if encapsulated
                      :encapsulated
                      (sb-di:debug-fun-start-location debug-fun)))
             (info (make-trace-info
                    :what function-or-name
                    :named named
                    :encapsulated encapsulated
                    :wherein (trace-info-wherein info)
                    :methods (trace-info-methods info)
                    :condition (coerce-form (trace-info-condition info) loc)
                    :break (coerce-form (trace-info-break info) loc)
                    :print (coerce-form-list (trace-info-print info) loc)
                    :break-after (coerce-form (trace-info-break-after info) nil)
                    :condition-after
                    (coerce-form (trace-info-condition-after info) nil)
                    :print-after
                    (coerce-form-list (trace-info-print-after info) nil))))

        (dolist (wherein (trace-info-wherein info))
          (unless (or (stringp wherein)
                      (fboundp wherein))
            (warn ":WHEREIN name ~S is not a defined global function."
                  wherein)))

        (cond
          (encapsulated
           (unless named
             (error "can't use encapsulation to trace anonymous function ~S"
                    fun))
           (encapsulate function-or-name 'trace
                        (lambda (function &rest args)
                          (apply #'trace-call info function args))))
          (t
           (multiple-value-bind (start-fun cookie-fun)
               (trace-start-breakpoint-fun info)
             (let ((start (sb-di:make-breakpoint start-fun debug-fun
                                                 :kind :fun-start))
                   (end (sb-di:make-breakpoint
                         (trace-end-breakpoint-fun info)
                         debug-fun :kind :fun-end
                         :fun-end-cookie cookie-fun)))
               (setf (trace-info-start-breakpoint info) start)
               (setf (trace-info-end-breakpoint info) end)
               ;; The next two forms must be in the order in which they
               ;; appear, since the start breakpoint must run before the
               ;; fun-end breakpoint's start helper (which calls the
               ;; cookie function.) One reason is that cookie function
               ;; requires that the CONDITIONP shared closure variable be
               ;; initialized.
               (sb-di:activate-breakpoint start)
               (sb-di:activate-breakpoint end)))))

        (setf (gethash fun *traced-funs*) info))

      (when (and (typep fun 'generic-function)
                 (trace-info-methods info)
                 ;; we are going to trace the method functions directly.
                 (not (trace-info-encapsulated info)))
        (dolist (method (sb-mop:generic-function-methods fun))
          (let ((mf (sb-mop:method-function method)))
            ;; NOTE: this direct style of tracing methods -- tracing the
            ;; pcl-internal method functions -- is only one possible
            ;; alternative.  It fails (a) when encapulation is
            ;; requested, because the function objects themselves are
            ;; stored in the method object; (b) when the method in
            ;; question is particularly simple, when the method
            ;; functionality is in the dfun.  See src/pcl/env.lisp for a
            ;; stub implementation of encapsulating through a
            ;; traced-method class.
            (trace-1 mf info)
            (when (typep mf 'sb-pcl::%method-function)
              (trace-1 (sb-pcl::%method-function-fast-function mf) info)))))

      function-or-name)))

;;;; the TRACE macro

;;; Parse leading trace options off of SPECS, modifying INFO
;;; accordingly. The remaining portion of the list is returned when we
;;; encounter a plausible function name.
(defun parse-trace-options (specs info)
  (let ((current specs))
    (loop
      (when (endp current) (return))
      (let ((option (first current))
            (value (cons (second current) nil)))
        (case option
          (:report (error "stub: The :REPORT option is not yet implemented."))
          (:condition (setf (trace-info-condition info) value))
          (:condition-after
           (setf (trace-info-condition info) (cons nil nil))
           (setf (trace-info-condition-after info) value))
          (:condition-all
           (setf (trace-info-condition info) value)
           (setf (trace-info-condition-after info) value))
          (:wherein
           (setf (trace-info-wherein info)
                 (if (listp (car value)) (car value) value)))
          (:encapsulate
           (setf (trace-info-encapsulated info) (car value)))
          (:methods
           (setf (trace-info-methods info) (car value)))
          (:break (setf (trace-info-break info) value))
          (:break-after (setf (trace-info-break-after info) value))
          (:break-all
           (setf (trace-info-break info) value)
           (setf (trace-info-break-after info) value))
          (:print
           (setf (trace-info-print info)
                 (append (trace-info-print info) (list value))))
          (:print-after
           (setf (trace-info-print-after info)
                 (append (trace-info-print-after info) (list value))))
          (:print-all
           (setf (trace-info-print info)
                 (append (trace-info-print info) (list value)))
           (setf (trace-info-print-after info)
                 (append (trace-info-print-after info) (list value))))
          (t (return)))
        (pop current)
        (unless current
          (error "missing argument to ~S TRACE option" option))
        (pop current)))
    current))

;;; Compute the expansion of TRACE in the non-trivial case (arguments
;;; specified.)
(defun expand-trace (specs)
  (collect ((binds)
            (forms))
    (let* ((global-options (make-trace-info))
           (current (parse-trace-options specs global-options)))
      (loop
        (when (endp current) (return))
        (let ((name (pop current))
              (options (copy-trace-info global-options)))
          (cond
           ((eq name :function)
            (let ((temp (gensym)))
              (binds `(,temp ,(pop current)))
              (forms `(trace-1 ,temp ',options))))
           ((and (keywordp name)
                 (not (or (fboundp name) (macro-function name))))
            (error "unknown TRACE option: ~S" name))
           ((stringp name)
            (let ((package (find-undeleted-package-or-lose name)))
              (do-all-symbols (symbol (find-package name))
                (when (eql package (symbol-package symbol))
                  (when (and (fboundp symbol)
                             (not (macro-function symbol))
                             (not (special-operator-p symbol)))
                    (forms `(trace-1 ',symbol ',options)))
                  (let ((setf-name `(setf ,symbol)))
                    (when (fboundp setf-name)
                      (forms `(trace-1 ',setf-name ',options))))))))
           ;; special-case METHOD: it itself is not a general function
           ;; name symbol, but it (at least here) designates one of a
           ;; pair of such.
           ((and (consp name) (eq (car name) 'method))
            (when (fboundp (list* 'sb-pcl::slow-method (cdr name)))
              (forms `(trace-1 ',(list* 'sb-pcl::slow-method (cdr name))
                               ',options)))
            (when (fboundp (list* 'sb-pcl::fast-method (cdr name)))
              (forms `(trace-1 ',(list* 'sb-pcl::fast-method (cdr name))
                               ',options))))
           (t
            (forms `(trace-1 ',name ',options))))
          (setq current (parse-trace-options current options)))))

    `(let ,(binds)
       (remove nil (list ,@(forms))))))

(defun %list-traced-funs ()
  (loop for x being each hash-value in *traced-funs*
        collect (trace-info-what x)))

(defmacro trace (&rest specs)
  #+sb-doc
  "TRACE {Option Global-Value}* {Name {Option Value}*}*

TRACE is a debugging tool that provides information when specified
functions are called. In its simplest form:

       (TRACE NAME-1 NAME-2 ...)

The NAMEs are not evaluated. Each may be a symbol, denoting an
individual function, or a string, denoting all functions fbound to
symbols whose home package is the package with the given name.

Options allow modification of the default behavior. Each option is a
pair of an option keyword and a value form. Global options are
specified before the first name, and affect all functions traced by a
given use of TRACE. Options may also be interspersed with function
names, in which case they act as local options, only affecting tracing
of the immediately preceding function name. Local options override
global options.

By default, TRACE causes a printout on *TRACE-OUTPUT* each time that
one of the named functions is entered or returns. (This is the basic,
ANSI Common Lisp behavior of TRACE.) As an SBCL extension, the
:REPORT SB-EXT:PROFILE option can be used to instead cause information
to be silently recorded to be inspected later using the SB-EXT:PROFILE
function.

The following options are defined:

   :REPORT Report-Type
       If Report-Type is TRACE (the default) then information is reported
       by printing immediately. If Report-Type is SB-EXT:PROFILE, information
       is recorded for later summary by calls to SB-EXT:PROFILE. If
       Report-Type is NIL, then the only effect of the trace is to execute
       other options (e.g. PRINT or BREAK).

   :CONDITION Form
   :CONDITION-AFTER Form
   :CONDITION-ALL Form
       If :CONDITION is specified, then TRACE does nothing unless Form
       evaluates to true at the time of the call. :CONDITION-AFTER is
       similar, but suppresses the initial printout, and is tested when the
       function returns. :CONDITION-ALL tries both before and after.
       This option is not supported with :REPORT PROFILE.

   :BREAK Form
   :BREAK-AFTER Form
   :BREAK-ALL Form
       If specified, and Form evaluates to true, then the debugger is invoked
       at the start of the function, at the end of the function, or both,
       according to the respective option.

   :PRINT Form
   :PRINT-AFTER Form
   :PRINT-ALL Form
       In addition to the usual printout, the result of evaluating Form is
       printed at the start of the function, at the end of the function, or
       both, according to the respective option. Multiple print options cause
       multiple values to be printed.

   :WHEREIN Names
       If specified, Names is a function name or list of names. TRACE does
       nothing unless a call to one of those functions encloses the call to
       this function (i.e. it would appear in a backtrace.)  Anonymous
       functions have string names like \"DEFUN FOO\". This option is not
       supported with :REPORT PROFILE.

   :ENCAPSULATE {:DEFAULT | T | NIL}
       If T, the tracing is done via encapsulation (redefining the function
       name) rather than by modifying the function. :DEFAULT is the default,
       and means to use encapsulation for interpreted functions and funcallable
       instances, breakpoints otherwise. When encapsulation is used, forms are
       *not* evaluated in the function's lexical environment, but SB-DEBUG:ARG
       can still be used.

   :METHODS {T | NIL}
       If T, any function argument naming a generic function will have its
       methods traced in addition to the generic function itself.

   :FUNCTION Function-Form
       This is a not really an option, but rather another way of specifying
       what function to trace. The Function-Form is evaluated immediately,
       and the resulting function is instrumented, i.e. traced or profiled
       as specified in REPORT.

:CONDITION, :BREAK and :PRINT forms are evaluated in a context which
mocks up the lexical environment of the called function, so that
SB-DEBUG:VAR and SB-DEBUG:ARG can be used.
The -AFTER and -ALL forms can use SB-DEBUG:ARG."
  (if specs
      (expand-trace specs)
      '(%list-traced-funs)))

;;;; untracing

;;; Untrace one function.
(defun untrace-1 (function-or-name)
  (let* ((fun (trace-fdefinition function-or-name))
         (info (when fun (gethash fun *traced-funs*))))
    (cond
      ((and fun (not info))
       (warn "Function is not TRACEd: ~S" function-or-name))
      ((not fun)
       ;; Someone has FMAKUNBOUND it.
       (let ((table *traced-funs*))
         (with-locked-system-table (table)
           (maphash (lambda (fun info)
                      (when (equal function-or-name (trace-info-what info))
                        (remhash fun table)))
                    table))))
      (t
       (cond
         ((trace-info-encapsulated info)
          (unencapsulate (trace-info-what info) 'trace))
         (t
          (sb-di:delete-breakpoint (trace-info-start-breakpoint info))
          (sb-di:delete-breakpoint (trace-info-end-breakpoint info))))
       (setf (trace-info-untraced info) t)
       (remhash fun *traced-funs*)))))

;;; Untrace all traced functions.
(defun untrace-all ()
  (dolist (fun (%list-traced-funs))
    (untrace-1 fun))
  t)

(defun untrace-package (name)
  (let ((package (find-package name)))
    (when package
      (dolist (fun (%list-traced-funs))
        (cond ((and (symbolp fun) (eq package (symbol-package fun)))
               (untrace-1 fun))
              ((and (consp fun) (eq 'setf (car fun))
                    (symbolp (second fun))
                    (eq package (symbol-package (second fun))))
               (untrace-1 fun)))))))

(defmacro untrace (&rest specs)
  #+sb-doc
  "Remove tracing from the specified functions. Untraces all
functions when called with no arguments."
  (if specs
      `(progn
         ,@(loop while specs
                 for name = (pop specs)
                 collect (cond ((eq name :function)
                                `(untrace-1 ,(pop specs)))
                               ((stringp name)
                                `(untrace-package ,name))
                               (t
                                `(untrace-1 ',name))))
         t)
      '(untrace-all)))
