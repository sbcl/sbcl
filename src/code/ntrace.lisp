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

;;; FIXME: Why, oh why, doesn't the SB-DEBUG package use the SB-DI
;;; package? That would let us get rid of a whole lot of stupid
;;; prefixes..

;;;; utilities

;;; Given X -- a function object or an s-expression naming a global function,
;;; local function, method, macro or compiler-macro -- return the following
;;; values:
;;;   * a function object suitable for introspection via
;;;     SB-DI:FUN-DEBUG-FUN. For local functions, their parent function is
;;;     returned. For methods, their generic function is returned, unless
;;;     IF-METHOD is :FAST-METHOD, in which case their respective
;;;     SB-PCL::FAST-METHOD function is returned.
;;;   * The function's BLOCK name, useful for looking up local functions.
;;;   * A keyword symbol describing what X denotes: :ANONYMOUS-FUNCTION,
;;;     :FUNCTION, :LOCAL-FUNCTION, :METHOD, :MACRO-FUNCTION.
;;;   * [For local functions only] A local name suitable for SB-DI:FUN-DEBUG-FUN
;;;     lookup.
(defun %trace-fdefinition (x &key (if-method :gf) definition)
  (typecase x
    (function
     (values x nil :anonymous-function))
    ((cons (eql compiler-macro) (cons t null))
     (let ((fun (or definition (compiler-macro-function (second x)))))
       (if fun
           (values fun (second x) :compiler-macro)
           (warn "~S is undefined, not tracing." x))))
    ((cons (eql method))              ; (METHOD name qualifiers (specializers*))
     (assert (null definition))
     (multiple-value-bind (gf block-name)
         (%trace-fdefinition (second x))
       (when gf
         (if (find-method gf (butlast (cddr x)) (car (last x)) nil)
             (ecase if-method
               (:fast-method
                (%trace-fdefinition `(sb-pcl::fast-method ,@(rest x))))
               (:gf
                (values gf block-name :method)))
             (warn "~S not found, not tracing." x)))))
    ((cons (member flet labels))       ; ({FLET,LABELS} name :IN outer-function)
     (destructuring-bind (flet/labels name &key in) x
       (multiple-value-bind (fun block-name)
           (%trace-fdefinition in :if-method :fast-method :definition definition)
         (when fun
           (let ((local-name `(,flet/labels ,name :in ,block-name)))
             (if (sb-di:fun-debug-fun fun :local-name local-name)
                 (values fun nil :local-function local-name)
                 (warn "~S not found, not tracing." x)))))))
    ((and symbol (satisfies special-operator-p))
     (warn "~S is a special operator, not tracing." x))
    ((and symbol (satisfies macro-function))
     (values (or definition (macro-function x)) x :macro-function))
    (t
     (multiple-value-bind (valid block-name)
         (valid-function-name-p x)
       (cond ((not valid)
              (warn "~S is not a valid function name, not tracing." x))
             ((fboundp x)
              (values (or definition (fdefinition x)) block-name :function))
             (t
              (warn "~/sb-ext:print-symbol-with-prefix/ is ~
                    undefined, not tracing." x)))))))

(defun trace-fdefinition (x &optional definition)
  (multiple-value-bind (fun block-name kind local-name)
      (%trace-fdefinition x :definition definition)
    (declare (ignore block-name))
    (values fun kind local-name)))

(defun retrace-local-funs (fname &optional new-value)
  (dolist (local (gethash fname *traced-locals*))
    (let ((trace-info (gethash local *traced-funs*)))
      (untrace-1 local)
      (trace-1 local trace-info new-value))))

;;; When a function name is redefined, and we were tracing that name,
;;; then untrace the old definition and trace the new one.
(defun maybe-retrace (name new-value)
  (let ((info (gethash name *traced-funs*)))
    (when info
      (untrace-1 name)
      (trace-1 name info new-value))
    (retrace-local-funs name new-value)))

(defun maybe-retrace-function (name new-value)
  (when (fboundp name)
    (maybe-retrace name new-value)))

(push #'maybe-retrace-function *setf-fdefinition-hook*)
(push #'maybe-retrace-function *setf-macro-function-hook*)

(defun maybe-retrace-compiler-macro (name new-value)
  (when (compiler-macro-function name)
    (maybe-retrace `(compiler-macro ,name) new-value)))

(push #'maybe-retrace-compiler-macro *setf-compiler-macro-function-hook*)

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
  (let* ((depth (count-if #'cdr *traced-entries*))
         (step *trace-indentation-step*)
         (max *max-trace-indentation*)
         (indent (+ (mod (* depth step) (- max step)) step)))
    (format t "~V,0@T~W: " indent depth)))

;;; Return true if any of the NAMES appears on the stack below FRAME.
(defun trace-wherein-p (encapsulated frame names)
  ;; When tracing without encapsulation (i.e. when using breakpoints),
  ;; FRAME points to the function being traced, so skip it.
  (let ((initial-frame (if encapsulated frame (sb-di:frame-down frame))))
    (do ((frame initial-frame (sb-di:frame-down frame)))
        ((not frame) nil)
      (when (member (sb-di:debug-fun-name (sb-di:frame-debug-fun frame))
                    names
                    :test #'equal)
        (return t)))))

;;; Handle PRINT and PRINT-AFTER options.
(defun trace-print (frame forms &rest args)
  (dolist (ele forms)
    (fresh-line)
    (print-trace-indentation)
    (format t "~@<~S ~_= ~:[; No values~;~:*~{~S~^, ~}~]~:>"
            (car ele)
            (multiple-value-list (apply (cdr ele) frame args)))
    (terpri)))

;;; Handle PRINT and PRINT-AFTER options when :REPORT style is NIL.
(defun trace-print-unadorned (frame forms &rest args)
  (dolist (ele forms)
    (let ((values (multiple-value-list (apply (cdr ele) frame args))))
      (when values
        (format t "~&~{~A~^, ~}~%" values)))))

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
     ;; HOOK-ARGS holds the function arguments when tracing via
     ;; encapsulation and but is NIL when tracing via breakpoints.
     (lambda (frame bpt &rest hook-args)
       (declare (ignore bpt))
       (discard-invalid-entries frame)
       (let ((condition (trace-info-condition info))
             (wherein (trace-info-wherein info)))
         (setq conditionp
               (and (not *in-trace*)
                    (or (not condition)
                        (apply (cdr condition) frame hook-args))
                    (or (not wherein)
                        (trace-wherein-p (trace-info-encapsulated info) frame wherein)))))
       (when conditionp
         (with-standard-io-syntax
           (let ((*print-readably* nil)
                 (*current-level-in-print* 0)
                 (*standard-output* (make-string-output-stream))
                 (*print-pretty* t)
                 (*in-trace* t))
             (case (trace-info-report info)
               (trace
                (fresh-line)
                (print-trace-indentation)
                (if (trace-info-encapsulated info)
                    (prin1 `(,(trace-info-what info)
                             ,@(mapcar #'ensure-printable-object hook-args)))
                    (print-frame-call frame *standard-output*))
                (terpri)
                (apply #'trace-print frame (trace-info-print info) hook-args))
               ((nil)
                (apply #'trace-print-unadorned frame (trace-info-print info) hook-args))
               (t
                (funcall (trace-info-report info)
                         (count-if #'cdr *traced-entries*)
                         (trace-info-what info) :enter frame
                         (if (trace-info-encapsulated info)
                             hook-args
                             (nth-value 1 (frame-call frame))))
                (apply #'trace-print-unadorned frame (trace-info-print info) hook-args)))
             (write-sequence (get-output-stream-string *standard-output*)
                             *trace-output*)
             (finish-output *trace-output*))
           (apply #'trace-maybe-break info (trace-info-break info) "before"
                  frame hook-args))))
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
  (lambda (frame bpt-or-nle values cookie)
    (unless (eq cookie (caar *traced-entries*))
      (setf *traced-entries*
            (member cookie *traced-entries* :key #'car)))

    (let ((entry (pop *traced-entries*))
          (non-local-exit (eq bpt-or-nle :nle)))
      (when (and (not (trace-info-untraced info))
                 (or (cdr entry)
                     (let ((cond (trace-info-condition-after info)))
                       (and cond (apply #'funcall (cdr cond) frame values)))))
        (let ((*current-level-in-print* 0)
              (*standard-output* (make-string-output-stream))
              (*in-trace* t))
          (case (trace-info-report info)
            (trace
             (fresh-line)
             (let ((*print-pretty* t))
               (pprint-logical-block (*standard-output* nil)
                 (print-trace-indentation)
                 (pprint-indent :current 2)
                 (cond (non-local-exit
                        (format t "~S exited non-locally" (trace-info-what info)))
                       (t
                        (format t "~S returned" (trace-info-what info))
                        (dolist (v values)
                          (write-char #\space)
                          (pprint-newline :linear)
                          (prin1 (ensure-printable-object v))))))
               (terpri))
             (unless non-local-exit
               (apply #'trace-print frame (trace-info-print-after info) values)))
            ((nil)
             (unless non-local-exit
               (apply #'trace-print-unadorned frame (trace-info-print-after info) values)))
            (t
             (funcall (trace-info-report info)
                      (count-if #'cdr *traced-entries*)
                      (trace-info-what info)
                      (if non-local-exit :non-local-exit :exit)
                      frame
                      values)
             (apply #'trace-print-unadorned frame (trace-info-print-after info) values)))
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
        (let* ((non-local-exit '#:nle)
               (vals non-local-exit))
          (unwind-protect
               (setq vals (multiple-value-list (apply function args)))
            (funcall (trace-end-breakpoint-fun info)
                     frame
                     (if (eq vals non-local-exit) :nle nil)
                     (if (eq vals non-local-exit) nil vals)
                     nil))
          (values-list vals))))))

;;; This function is like TRACE-CALL above, but munges the method
;;; calling conventions into something more like what the user might
;;; expect to see -- so not (<args> <next methods>) or (<permutation
;;; vector> <next-emf> <arg> ...), but the method's actual arglist.
(defun trace-method-call (info function fmf-p &rest args)
  (let ((transform (if fmf-p (lambda (x) (nthcdr 2 x)) #'car)))
    (multiple-value-bind (start cookie) (trace-start-breakpoint-fun info)
      (declare (type function start cookie))
      (let ((frame (sb-di:frame-down (sb-di:top-frame))))
        (apply #'funcall start frame nil (funcall transform args))
        (let ((*traced-entries* *traced-entries*))
          (funcall cookie frame nil)
          (let* ((non-local-exit '#:nle)
                 (vals non-local-exit))
            (unwind-protect
                 (setq vals (multiple-value-list (apply function args)))
              (funcall (trace-end-breakpoint-fun info)
                       frame
                       (if (eq vals non-local-exit) :nle nil)
                       (if (eq vals non-local-exit) nil vals)
                       nil))
            (values-list vals)))))))

;;; Trace one function according to the specified options. We copy the
;;; trace info (it was a quoted constant), fill in the functions, and
;;; then install the breakpoints or encapsulation.
;;;
;;; If non-null, DEFINITION is the new definition of a function that
;;; we are automatically retracing.
(defun trace-1 (function-or-name info &optional definition)
  (multiple-value-bind (fun kind local-name)
      (trace-fdefinition function-or-name definition)
    (when fun
      (when (closurep fun)
        (setq fun (%closure-fun fun))
        (when (eq (trace-info-encapsulated info) :default)
          (warn "tracing shared code for ~S:~%  ~S" function-or-name fun)))
      (when (gethash function-or-name *traced-funs*)
        (warn "~S is already TRACE'd, untracing it first." function-or-name)
        (untrace-1 function-or-name))
      (let* ((debug-fun (sb-di:fun-debug-fun fun :local-name local-name))
             (encapsulated
               (ecase kind
                 ((:function :method)
                  (if (eq (trace-info-encapsulated info) :default)
                      (funcallable-instance-p fun)
                      (trace-info-encapsulated info)))
                 ((:anonymous-function :compiler-macro :macro-function :local-function)
                  nil)))
             (loc (if encapsulated
                      :encapsulated
                      (sb-di:debug-fun-start-location debug-fun)))
             (info (make-trace-info
                    :what function-or-name
                    :encapsulated encapsulated
                    :wherein (trace-info-wherein info)
                    :methods (trace-info-methods info)
                    :condition (coerce-form (trace-info-condition info) loc)
                    :break (coerce-form (trace-info-break info) loc)
                    :report (trace-info-report info)
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
           (if (eq kind :method)
               (reinitialize-instance fun)
               (encapsulate function-or-name 'trace
                            (lambda (function &rest args)
                              (apply #'trace-call info function args)))))
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

        (when (eq kind :local-function)
          (push function-or-name
                (gethash (fourth function-or-name) *traced-locals*)))
        (setf (gethash function-or-name *traced-funs*) info))

      (when (and (typep fun 'generic-function)
                 (trace-info-methods info)
                 ;; we are going to trace the method functions directly.
                 (not (trace-info-encapsulated info)))
        (dolist (method (sb-mop:generic-function-methods fun))
          (let ((mf (sb-mop:method-function method)))
            ;; NOTE: this direct style of tracing methods -- tracing the
            ;; pcl-internal method functions -- is only one possible
            ;; alternative.  It fails (a) when encapsulation is
            ;; requested, because the function objects themselves are
            ;; stored in the method object; (b) when the method in
            ;; question is particularly simple, when the method
            ;; functionality is in the dfun.
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
          (:report
           (unless (typep (car value) 'trace-report-type)
             (error "~S is not a valid ~A ~S type."
                    (car value) 'trace :report))
           (setf (trace-info-report info) (car value)))
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
        (let* ((name (pop current))
               (fn (when (eq name :function)
                     (pop current)))
               (options (copy-trace-info global-options)))
          ;; parse options for the current spec.
          (setq current (parse-trace-options current options))
          (cond
           ((eq name :function)
            (let ((temp (gensym)))
              (binds `(,temp ,fn))
              (forms `(trace-1 ,temp ',options))))
           ((and (keywordp name)
                 (not (or (fboundp name) (macro-function name))))
            (error "unknown TRACE option: ~S" name))
           ((stringp name)
            (let ((package (find-undeleted-package-or-lose name)))
              (do-symbols (symbol package)
                (when (eql package (symbol-package symbol))
                  (when (and (fboundp symbol)
                             (not (macro-function symbol))
                             (not (special-operator-p symbol)))
                    (forms `(trace-1 ',symbol ',options)))
                  (let ((setf-name `(setf ,symbol)))
                    (when (fboundp setf-name)
                      (forms `(trace-1 ',setf-name ',options))))))))
           ;; special-case METHOD (without encapsulation): it itself
           ;; is not a general function name symbol, but it (at least
           ;; here) designates one of a pair of such.
           ((and (consp name)
                 (eq (car name) 'method)
                 (not (trace-info-encapsulated options)))
            (when (fboundp (list* 'sb-pcl::slow-method (cdr name)))
              (forms `(trace-1 ',(list* 'sb-pcl::slow-method (cdr name))
                               ',options)))
            (when (fboundp (list* 'sb-pcl::fast-method (cdr name)))
              (forms `(trace-1 ',(list* 'sb-pcl::fast-method (cdr name))
                               ',options))))
           (t
            (forms `(trace-1 ',name ',options)))))))
    `(let ,(binds)
       (remove nil (list ,@(forms))))))

(defun %list-traced-funs ()
  (loop for x being each hash-value in *traced-funs*
        collect (trace-info-what x)))

(defmacro trace (&rest specs)
  "TRACE {Option Global-Value}* {Name {Option Value}*}*

TRACE is a debugging tool that provides information when specified
functions are called. In its simplest form:

       (TRACE NAME-1 NAME-2 ...)

The NAMEs are not evaluated. Each may be one of the following:
  * SYMBOL, denoting a function or macro.
  * FNAME, a valid function name, denoting a function.
  * (METHOD FNAME QUALIFIERS* (SPECIALIZERS*)) denoting a method.
  * (COMPILER-MACRO SYMBOL) denoting a compiler macro.
  * (LABELS FNAME :IN OUTER-NAME) or (FLET FNAME :IN OUTER-NAME)
    denoting a local function where OUTER-NAME may be any of the
    previous names for functions, macros, methods or compiler macros.
    Tracing local functions may require DEBUG policy 3 to inhibit
    inlining.
  * STRING denoting all functions fbound to symbols whose home package
    is the package with the given name.

Options allow modification of the default behavior. Each option is a
pair of an option keyword and a value form. Global options are
specified before the first name, and affect all functions traced by a
given use of TRACE. Options may also be interspersed with function
names, in which case they act as local options, only affecting tracing
of the immediately preceding function name. Local options override
global options.

By default, TRACE causes a printout on *TRACE-OUTPUT* each time that
one of the named functions is entered or returns. (This is the basic,
ANSI Common Lisp behavior of TRACE.)

The following options are defined:

   :REPORT Report-Type
       If Report-Type is TRACE (the default) then information is
       reported by printing immediately. If Report-Type is NIL, then
       the only effect of the trace is to execute other
       options (e.g. PRINT or BREAK). Otherwise, Report-Type is
       treated as a function designator and, for each trace event,
       funcalled with 5 arguments: trace depth (a non-negative
       integer), a function name or a function object, a
       keyword (:ENTER, :EXIT or :NON-LOCAL-EXIT), a stack frame, and
       a list of values (arguments or return values).

   :CONDITION Form
   :CONDITION-AFTER Form
   :CONDITION-ALL Form
       If :CONDITION is specified, then TRACE does nothing unless Form
       evaluates to true at the time of the call. :CONDITION-AFTER is
       similar, but suppresses the initial printout, and is tested when the
       function returns. :CONDITION-ALL tries both before and after.

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
       functions have string names like \"DEFUN FOO\".

   :ENCAPSULATE {:DEFAULT | T | NIL}
       If T, the default, tracing is done via encapsulation (redefining the
       function name) rather than by modifying the function.  :DEFAULT is
       not the default, but means to use encapsulation for interpreted
       functions and funcallable instances, breakpoints otherwise. When
       encapsulation is used, forms are *not* evaluated in the function's
       lexical environment, but SB-DEBUG:ARG can still be used.

   :METHODS {T | NIL}
       If T, any function argument naming a generic function will have its
       methods traced in addition to the generic function itself.

   :FUNCTION Function-Form
       This is a not really an option, but rather another way of specifying
       what function to trace. The Function-Form is evaluated immediately,
       and the resulting function is traced.

:CONDITION, :BREAK and :PRINT forms are evaluated in a context which
mocks up the lexical environment of the called function, so that
SB-DEBUG:VAR and SB-DEBUG:ARG can be used.
The -AFTER and -ALL forms can use also use SB-DEBUG:ARG. In forms
which are evaluated after the function call, (SB-DEBUG:ARG N) returns
the N-th value returned by the function."
  (if specs
      (expand-trace specs)
      '(%list-traced-funs)))

;;;; untracing

;;; Untrace one function.
(defun untrace-1 (function-or-name)
  (multiple-value-bind (fun kind)
      (trace-fdefinition function-or-name)
    (when fun
      (let ((info (gethash function-or-name *traced-funs*)))
        (cond ((and fun (not info))
               (warn "Function is not TRACEd: ~S" function-or-name))
              (t
               (cond
                 ((trace-info-encapsulated info)
                  (if (eq kind :method)
                      (reinitialize-instance fun)
                      (unencapsulate (trace-info-what info) 'trace)))
                 (t
                  (sb-di:delete-breakpoint (trace-info-start-breakpoint info))
                  (sb-di:delete-breakpoint (trace-info-end-breakpoint info))))
               (setf (trace-info-untraced info) t)
               (when (eq kind :local-function)
                 (let ((table *traced-locals*)
                       (outer (fourth function-or-name)))
                   (with-system-mutex ((hash-table-lock table))
                     (let* ((locals (gethash outer table))
                            (remaining (remove function-or-name locals :test #'equal)))
                       (if remaining
                           (setf (gethash outer table) remaining)
                           (remhash outer table))))))))))
    (remhash function-or-name *traced-funs*)))

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
  "Remove tracing from the specified functions. Untraces all
functions when called with no arguments."
  (if specs
      `(progn
         ,@(loop for name = (car specs)
                 while specs
                 do (pop specs)
                 collect (cond ((eq name :function)
                                `(untrace-1 ,(pop specs)))
                               ((stringp name)
                                `(untrace-package ,name))
                               (t
                                `(untrace-1 ',name))))
         t)
      '(untrace-all)))

;;;; Experimental implementation of encapsulation, specifically for TRACE,
;;;; which preserves identity of closures and simple-funs.

;;; Code which is traced has simple-funs whose entry points do not point
;;; to themselves. The garbage collector needs to visit each simple-fun
;;; in the traced object when scavenging. Normally it does not do that,
;;; as relocation of the etry point is performed by the transport method,
;;; not the scavenge method.
(defconstant code-is-traced 1)

(defun set-tracing-bit (code bit)
  (declare (ignorable code bit))
  ;; there are no bits to spare in a 32-bit header word,
  ;; and with darwin-jit it's not worth the extra complexity
  #+(and 64-bit (not darwin-jit))
  (with-pinned-objects (code)
    (let ((sap (int-sap (get-lisp-obj-address code))))
      ;; NB: This is not threadsafe on machines that don't promise that
      ;; stores to single bytes are atomic.
      (setf (sap-ref-8 sap #+little-endian (- 2 sb-vm:other-pointer-lowtag)
                           #+big-endian (- 5 sb-vm:other-pointer-lowtag))
            bit)
      ;; touch the card mark - WHY???
      (setf (code-header-ref code 1) (code-header-ref code 1)))))

;;; FIXME: Symbol is lost by accident
(eval-when (:compile-toplevel :load-toplevel)
  (export 'sb-int::encapsulate-funobj 'sb-int))

;;; Suppose you want to trace function #'FOO no matter how a caller
;;; references it (maybe capturing #'FOO in a variable before asking
;;; to trace FOO). We can do that without resorting to breakpoints,
;;; by replacing the simple-fun entry point in the header of the code
;;; that contains FOO such that it points to a different simple-fun
;;; outside of itself. That other simple-fun calls the tracing routine
;;; and then the real FOO. An entry point can't be replaced with a
;;; closure, because CLOSURE and SIMPLE-FUN are not fungible.
;;;
;;;   +-------------------------+        +--------------------+
;;;   | codeblob foo            |        | codeblob "TRACER"  |
;;;   | ...                     |        |                    |
;;;   | ... boxed data ...      |        | boxed word: #'foo  | -> the "real" FOO
;;;   | ...                     |        |                    |
;;;   | ... unboxed data ...    |        |                    |
;;;   | ...                     |        +--------------------+
;;;   | simple-fun-header #'foo |    --> | call trace helper  |
;;;   | redirected entry point  | --/    +--------------------+
;;;   | instructions of #'FOO   |
;;;   | ...                     |
;;;   +-------------------------+

(defun compile-funobj-encapsulation (wrapper info actual-fun)
  #+(or ppc64 x86 x86-64)
  (let ((code
         ;; Don't actually "compile" - just emulate the result of compiling.
         ;; Cloning a precompiled template object consumes only 272 bytes
         ;; versus about 128KB to invoke the compiler.
         (sb-c::copy-code-object
          (load-time-value
           (let ((c (fun-code-header
                     ;; Immobile code might use relative fixups which won't work
                     ;; when the code gets copied.
                     (let ((sb-c:*compile-to-memory-space* :dynamic))
                       (compile nil
                                `(lambda (&rest args)
                                   ;; The code constants will be overwritten in the copy.
                                   ;; These are just placeholders essentially.
                                   (apply ,#'trace-call ,(make-trace-info) #() args))))))
                 (index sb-vm:code-constants-offset))
             ;; First three args to APPLY must be at the expected offets
             (aver (typep (code-header-ref c (+ index 0)) 'function))
             (aver (typep (code-header-ref c (+ index 1)) 'trace-info))
             (aver (typep (code-header-ref c (+ index 2)) 'simple-vector))
             c)
           t)))
        (index sb-vm:code-constants-offset))
    (setf (code-header-ref code (+ index 0)) (symbol-function wrapper)
          (code-header-ref code (+ index 1)) info
          (code-header-ref code (+ index 2)) actual-fun)
    (%code-entry-point code 0))
  #-(or ppc64 x86 x86-64)
  (values (compile nil `(lambda (&rest args)
                          (apply #',wrapper ,info ,actual-fun args)))))

;;; The usual ENCAPSULATE encapsulates NAME by changing what NAME points to,
;;; that is, by altering the fdefinition.
;;; In contrast, ENCAPSULATE-FUNOBJ encapsulates TRACED-FUN by changing the
;;; entry point of the function to redirect to a tracing wrapper which then
;;; calls back to the correct entry point.
(defun encapsulate-funobj (traced-fun &optional (name nil namep))
  (declare (type (or simple-fun closure) traced-fun))
  (let* ((proxy-fun
           (typecase traced-fun
             (simple-fun
              ;; Generate a "closure" (that closes over nothing) which calls the
              ;; original underlying function so that the original's entry point
              ;; can be redirected to a tracing wrapper, which will produce trace
              ;; output and invoke the closure which invokes the original fun.
              #+(or x86-64 arm64)
              (with-pinned-objects ((%closure-fun traced-fun))
                (sb-vm::%alloc-closure 0 (sb-vm::%closure-callee traced-fun)))
              #-(or x86-64 arm64) (%primitive sb-vm::make-closure traced-fun nil 0 nil))
             (closure
              ;; Same as above, but simpler - the original closure will redirect
              ;; to the tracing wraper, which will invoke a new closure that is
              ;; behaviorally identical to the original closure.
              (sb-impl::copy-closure traced-fun))))
         (info (make-trace-info :what (if namep name (%fun-name traced-fun))
                                :encapsulated t
                                :report 'trace))
         (tracing-wrapper
           (compile-funobj-encapsulation 'trace-call info proxy-fun)))
    (with-pinned-objects (tracing-wrapper)
      (let (#+(or arm64 ppc64 x86 x86-64)
            (tracing-wrapper-entry
              (+ (get-lisp-obj-address tracing-wrapper)
                 (- sb-vm:fun-pointer-lowtag)
                 (ash sb-vm:simple-fun-insts-offset sb-vm:word-shift))))
        (typecase traced-fun
          (simple-fun
           (let ((code (fun-code-header traced-fun)))
             (set-tracing-bit code code-is-traced)
             (let ((fun-header-word-index
                     (with-pinned-objects (code)
                       (let ((delta (- (get-lisp-obj-address traced-fun)
                                       (get-lisp-obj-address code)
                                       sb-vm:fun-pointer-lowtag
                                       (- sb-vm:other-pointer-lowtag))))
                         (aver (not (logtest delta sb-vm:lowtag-mask)))
                         (ash delta (- sb-vm:word-shift))))))
               ;; the entry point in CODE points to the tracing wrapper
               (setf (code-header-ref code (1+ fun-header-word-index))
                     #+(or arm64 ppc64 x86 x86-64) (make-lisp-obj tracing-wrapper-entry)
                     #-(or arm64 ppc64 x86 x86-64) tracing-wrapper))))
          (closure
           (with-pinned-objects (traced-fun)
             ;; redirect the original closure to the tracing wrapper
             #+(or arm64 ppc64 x86 x86-64)
             (setf (sap-ref-word (int-sap (get-lisp-obj-address traced-fun))
                                 (- sb-vm:n-word-bytes sb-vm:fun-pointer-lowtag))
                   tracing-wrapper-entry)
             #-(or arm64 ppc64 x86 x86-64)
             (setf (sap-ref-lispobj (int-sap (get-lisp-obj-address traced-fun))
                                    (- sb-vm:n-word-bytes sb-vm:fun-pointer-lowtag))
                   tracing-wrapper))))))
    ;; Possibly update #'NAME to point to the tracing wrapper
    (when (and namep (eq (fboundp name) traced-fun))
      (fset name tracing-wrapper))
    tracing-wrapper))
