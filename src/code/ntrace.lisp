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
                   (warn "~/sb-ext:print-symbol-with-prefix/ is ~
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
        (values (%closure-fun res) named-p :compiled-closure))
       (funcallable-instance
        (values res named-p :funcallable-instance))
       ;; FIXME: What about SB-KERNEL:INTERPRETED-FUNCTION -- it gets picked off
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
  (let* ((depth (count-if #'cdr *traced-entries*))
         (step *trace-indentation-step*)
         (max *max-trace-indentation*)
         (indent (+ (mod (* depth step) (- max step)) step)))
    (format t "~V,0@T~W: " indent depth)))

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
         (with-standard-io-syntax
           (let ((*print-readably* nil)
                 (*current-level-in-print* 0)
                 (*standard-output* (make-string-output-stream))
                 (*in-trace* t))
             (ecase (trace-info-report info)
               (trace
                (fresh-line)
                (print-trace-indentation)
                (if (trace-info-encapsulated info)
                    (prin1 `(,(trace-info-what info)
                            ,@(mapcar #'ensure-printable-object args)))
                    (print-frame-call frame *standard-output*))
                (terpri)
                (apply #'trace-print frame (trace-info-print info) args))
               ((nil)
                (apply #'trace-print-unadorned frame (trace-info-print info) args)))
             (write-sequence (get-output-stream-string *standard-output*)
                             *trace-output*)
             (finish-output *trace-output*))
           (apply #'trace-maybe-break info (trace-info-break info) "before"
                  frame args))))
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
        (let ((*current-level-in-print* 0)
              (*standard-output* (make-string-output-stream))
              (*in-trace* t))
          (ecase (trace-info-report info)
            (trace
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
             (apply #'trace-print frame (trace-info-print-after info) values))
            ((nil)
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
        (let ((vals (multiple-value-list (apply function args))))
          (funcall (trace-end-breakpoint-fun info) frame nil vals nil)
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
          (let ((vals (multiple-value-list (apply function args))))
            (funcall (trace-end-breakpoint-fun info) frame nil vals nil)
            (values-list vals)))))))

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
            ;; alternative.  It fails (a) when encapsulation is
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
ANSI Common Lisp behavior of TRACE.)

The following options are defined:

   :REPORT Report-Type
       If Report-Type is TRACE (the default) then information is
       reported by printing immediately. If Report-Type is NIL, then
       the only effect of the trace is to execute other
       options (e.g. PRINT or BREAK).

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
  (let* ((fun (trace-fdefinition function-or-name))
         (info (when fun (gethash fun *traced-funs*))))
    (cond
      ((and fun (not info))
       (warn "Function is not TRACEd: ~S" function-or-name))
      ((not fun)
       ;; Someone has FMAKUNBOUND it.
       (let ((table *traced-funs*))
         (with-system-mutex ((hash-table-lock table))
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
  #+64-bit ; there are no bits to spare in a 32-bit header word
  (with-pinned-objects (code)
    (let ((sap (int-sap (get-lisp-obj-address code))))
      ;; NB: This is not threadsafe on machines that don't promise that
      ;; stores to single bytes are atomic.
      (setf (sb-vm::sap-ref-8-jit sap #+little-endian (- 1 sb-vm:other-pointer-lowtag)
                                      #+big-endian (- 6 sb-vm:other-pointer-lowtag))
            bit)
      ;; touch the card mark
      (setf (code-header-ref code 1) (code-header-ref code 1)))))

;;; FIXME: Symbol is lost by accident
(eval-when (:compile-toplevel :load-toplevel)
  (export 'sb-int::encapsulate-funobj 'sb-int))

(defun compile-funobj-encapsulation (wrapper info actual-fun)
  #+(or x86 x86-64)
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
                 (index (+ sb-vm:code-constants-offset sb-vm:code-slots-per-simple-fun)))
             ;; First three args to APPLY must be at the expected offets
             (aver (typep (code-header-ref c (+ index 0)) 'function))
             (aver (typep (code-header-ref c (+ index 1)) 'trace-info))
             (aver (typep (code-header-ref c (+ index 2)) 'simple-vector))
             c)
           t)))
        (index (+ sb-vm:code-constants-offset sb-vm:code-slots-per-simple-fun)))
    (setf (code-header-ref code (+ index 0)) (symbol-function wrapper)
          (code-header-ref code (+ index 1)) info
          (code-header-ref code (+ index 2)) actual-fun)
    (%code-entry-point code 0))
  #-(or x86 x86-64)
  (values (compile nil `(lambda (&rest args)
                          (apply #',wrapper ,info ,actual-fun args)))))

;;; The usual ENCAPSULATE encapsulates NAME by changing what NAME points to,
;;; that is, by altering the fdefinition.
;;; In contrast, ENCAPSULATE-FUNOBJ encapsulates TRACED-FUN by changing the
;;; entry point of the function to redirect to a tracing wrapper which then
;;; calls back to the correct entry point.
(defun encapsulate-funobj (traced-fun &optional fdefn)
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
         (info (make-trace-info :what (cond (fdefn (fdefn-name fdefn))
                                            (t (%fun-name traced-fun)))
                                :encapsulated t
                                :named t
                                :report 'trace))
         (tracing-wrapper
           (compile-funobj-encapsulation 'trace-call info proxy-fun)))
    (with-pinned-objects (tracing-wrapper)
      (let (#+(or x86 x86-64 arm64)
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
                     #+(or x86 x86-64 arm64) (make-lisp-obj tracing-wrapper-entry)
                     #-(or x86 x86-64 arm64) tracing-wrapper))))
          (closure
           (with-pinned-objects (traced-fun)
             ;; redirect the original closure to the tracing wrapper
             #+(or x86 x86-64 arm64)
             (setf (sap-ref-word (int-sap (get-lisp-obj-address traced-fun))
                                 (- sb-vm:n-word-bytes sb-vm:fun-pointer-lowtag))
                   tracing-wrapper-entry)
             #-(or x86 x86-64 arm64)
             (setf (sap-ref-lispobj (int-sap (get-lisp-obj-address traced-fun))
                                    (- sb-vm:n-word-bytes sb-vm:fun-pointer-lowtag))
                   tracing-wrapper))))))
    ;; Update fdefn's raw-addr slot to point to the tracing wrapper
    (when (and fdefn (eq (fdefn-fun fdefn) traced-fun))
      (setf (fdefn-fun fdefn) tracing-wrapper))
    tracing-wrapper))
