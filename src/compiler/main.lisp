;;;; the top level interfaces to the compiler, plus some other
;;;; compiler-related stuff (e.g. CL:CALL-ARGUMENTS-LIMIT) which
;;;; doesn't obviously belong anywhere else

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;; FIXME: Doesn't this belong somewhere else, like early-c.lisp?
(declaim (special *constants* *free-vars* *component-being-compiled*
                  *free-funs* *source-paths*
                  *continuation-number* *continuation-numbers*
                  *number-continuations* *tn-id* *tn-ids* *id-tns*
                  *label-ids* *label-id* *id-labels*
                  *undefined-warnings* *compiler-error-count*
                  *compiler-warning-count* *compiler-style-warning-count*
                  *compiler-note-count*
                  *compiler-error-bailout*
                  #!+sb-show *compiler-trace-output*
                  *last-source-context* *last-original-source*
                  *last-source-form* *last-format-string* *last-format-args*
                  *last-message-count* *last-error-context*
                  *lexenv* *fun-names-in-this-file*
                  *allow-instrumenting*))

;;; Whether reference to a thing which cannot be defined causes a full
;;; warning.
(defvar *flame-on-necessarily-undefined-thing* nil)

(defvar *check-consistency* nil)

;;; Set to NIL to disable loop analysis for register allocation.
(defvar *loop-analyze* t)

;;; Bind this to a stream to capture various internal debugging output.
(defvar *compiler-trace-output* nil)

;;; The current block compilation state. These are initialized to the
;;; :BLOCK-COMPILE and :ENTRY-POINTS arguments that COMPILE-FILE was
;;; called with.
;;;
;;; *BLOCK-COMPILE-ARG* holds the original value of the :BLOCK-COMPILE
;;; argument, which overrides any internal declarations.
(defvar *block-compile*)
(defvar *block-compile-arg*)
(declaim (type (member nil t :specified) *block-compile* *block-compile-arg*))
(defvar *entry-points*)
(declaim (list *entry-points*))

;;; When block compiling, used by PROCESS-FORM to accumulate top level
;;; lambdas resulting from compiling subforms. (In reverse order.)
(defvar *toplevel-lambdas*)
(declaim (list *toplevel-lambdas*))

;;; The current non-macroexpanded toplevel form as printed when
;;; *compile-print* is true.
(defvar *top-level-form-noted* nil)

(defvar sb!xc:*compile-verbose* t
  #!+sb-doc
  "The default for the :VERBOSE argument to COMPILE-FILE.")
(defvar sb!xc:*compile-print* t
  #!+sb-doc
  "The default for the :PRINT argument to COMPILE-FILE.")
(defvar *compile-progress* nil
  #!+sb-doc
  "When this is true, the compiler prints to *STANDARD-OUTPUT* progress
  information about the phases of compilation of each function. (This
  is useful mainly in large block compilations.)")

(defvar sb!xc:*compile-file-pathname* nil
  #!+sb-doc
  "The defaulted pathname of the file currently being compiled, or NIL if not
  compiling.")
(defvar sb!xc:*compile-file-truename* nil
  #!+sb-doc
  "The TRUENAME of the file currently being compiled, or NIL if not
  compiling.")

(declaim (type (or pathname null)
               sb!xc:*compile-file-pathname*
               sb!xc:*compile-file-truename*))

;;; the SOURCE-INFO structure for the current compilation. This is
;;; null globally to indicate that we aren't currently in any
;;; identifiable compilation.
(defvar *source-info* nil)

;;; This is true if we are within a WITH-COMPILATION-UNIT form (which
;;; normally causes nested uses to be no-ops).
(defvar *in-compilation-unit* nil)

;;; Count of the number of compilation units dynamically enclosed by
;;; the current active WITH-COMPILATION-UNIT that were unwound out of.
(defvar *aborted-compilation-unit-count*)

;;; Mumble conditional on *COMPILE-PROGRESS*.
(defun maybe-mumble (&rest foo)
  (when *compile-progress*
    (compiler-mumble "~&")
    (pprint-logical-block (*standard-output* nil :per-line-prefix "; ")
       (apply #'compiler-mumble foo))))

(deftype object () '(or fasl-output core-object null))

(defvar *compile-object* nil)
(declaim (type object *compile-object*))
(defvar *compile-toplevel-object* nil)

(defvar *emit-cfasl* nil)

(defvar *fopcompile-label-counter*)

;; Used during compilation to map code paths to the matching
;; instrumentation conses.
(defvar *code-coverage-records* nil)
;; Used during compilation to keep track of with source paths have been
;; instrumented in which blocks.
(defvar *code-coverage-blocks* nil)
;; Stores the code coverage instrumentation results. Keys are namestrings,
;; the value is a list of (CONS PATH STATE), where STATE is NIL for
;; a path that has not been visited, and T for one that has.
(defvar *code-coverage-info* (make-hash-table :test 'equal))


;;;; WITH-COMPILATION-UNIT and WITH-COMPILATION-VALUES

(defmacro sb!xc:with-compilation-unit (options &body body)
  #!+sb-doc
  "Affects compilations that take place within its dynamic extent. It is
intended to be eg. wrapped around the compilation of all files in the same system.

Following options are defined:

  :OVERRIDE Boolean-Form
      One of the effects of this form is to delay undefined warnings until the
      end of the form, instead of giving them at the end of each compilation.
      If OVERRIDE is NIL (the default), then the outermost
      WITH-COMPILATION-UNIT form grabs the undefined warnings. Specifying
      OVERRIDE true causes that form to grab any enclosed warnings, even if it
      is enclosed by another WITH-COMPILATION-UNIT.

  :POLICY Optimize-Declaration-Form
      Provides dynamic scoping for global compiler optimization qualities and
      restrictions, limiting effects of subsequent OPTIMIZE proclamations and
      calls to SB-EXT:RESTRICT-COMPILER-POLICY to the dynamic scope of BODY.

      If OVERRIDE is false, specified POLICY is merged with current global
      policy. If OVERRIDE is true, current global policy, including any
      restrictions, is discarded in favor of the specified POLICY.

      Supplying POLICY NIL is equivalent to the option not being supplied at
      all, ie. dynamic scoping of policy does not take place.

      This option is an SBCL-specific experimental extension: Interface
      subject to change.

  :SOURCE-NAMESTRING Namestring-Form
      Attaches the value returned by the Namestring-Form to the internal
      debug-source information as the namestring of the source file. Normally
      the namestring of the input-file for COMPILE-FILE is used: this option
      can be used to provide source-file information for functions compiled
      using COMPILE, or to override the input-file of COMPILE-FILE.

      If both an outer and an inner WITH-COMPILATION-UNIT provide a
      SOURCE-NAMESTRING, the inner one takes precedence. Unaffected
      by :OVERRIDE.

      This is an SBCL-specific extension.

  :SOURCE-PLIST Plist-Form
      Attaches the value returned by the Plist-Form to internal debug-source
      information of functions compiled in within the dynamic extent of BODY.

      Primarily for use by development environments, in order to eg. associate
      function definitions with editor-buffers. Can be accessed using
      SB-INTROSPECT:DEFINITION-SOURCE-PLIST.

      If an outer WITH-COMPILATION-UNIT form also provide a SOURCE-PLIST, it
      is appended to the end of the provided SOURCE-PLIST. Unaffected
      by :OVERRIDE.

      This is an SBCL-specific extension.

Examples:

  ;; Prevent proclamations from the file leaking, and restrict
  ;; SAFETY to 3 -- otherwise uses the current global policy.
  (with-compilation-unit (:policy '(optimize))
    (restrict-compiler-policy 'safety 3)
    (load \"foo.lisp\"))

  ;; Using default policy instead of the current global one,
  ;; except for DEBUG 3.
  (with-compilation-unit (:policy '(optimize debug)
                          :override t)
    (load \"foo.lisp\"))

  ;; Same as if :POLICY had not been specified at all: SAFETY 3
  ;; proclamation leaks out from WITH-COMPILATION-UNIT.
  (with-compilation-unit (:policy nil)
    (declaim (optimize safety))
    (load \"foo.lisp\"))
"
  `(%with-compilation-unit (lambda () ,@body) ,@options))

(defvar *source-plist* nil)
(defvar *source-namestring* nil)

(defun %with-compilation-unit (fn &key override policy source-plist source-namestring)
  (declare (type function fn))
  (flet ((with-it ()
           (let ((succeeded-p nil)
                 (*source-plist* (append source-plist *source-plist*))
                 (*source-namestring* (or source-namestring *source-namestring*)))
             (if (and *in-compilation-unit* (not override))
                 ;; Inside another WITH-COMPILATION-UNIT, a WITH-COMPILATION-UNIT is
                 ;; ordinarily (unless OVERRIDE) basically a no-op.
                 (unwind-protect
                      (multiple-value-prog1 (funcall fn) (setf succeeded-p t))
                   (unless succeeded-p
                     (incf *aborted-compilation-unit-count*)))
                 (let ((*aborted-compilation-unit-count* 0)
                       (*compiler-error-count* 0)
                       (*compiler-warning-count* 0)
                       (*compiler-style-warning-count* 0)
                       (*compiler-note-count* 0)
                       (*undefined-warnings* nil)
                       (*in-compilation-unit* t))
                   (handler-bind ((parse-unknown-type
                                    (lambda (c)
                                      (note-undefined-reference
                                       (parse-unknown-type-specifier c)
                                       :type))))
                     (unwind-protect
                          (multiple-value-prog1 (funcall fn) (setf succeeded-p t))
                       (unless succeeded-p
                         (incf *aborted-compilation-unit-count*))
                       (summarize-compilation-unit (not succeeded-p)))))))))
    (if policy
        (let ((*policy* (process-optimize-decl policy (unless override *policy*)))
              (*policy-restrictions* (unless override *policy-restrictions*)))
          (with-it))
        (with-it))))

;;; Is NAME something that no conforming program can rely on
;;; defining?
(defun name-reserved-by-ansi-p (name kind)
  (ecase kind
    (:function
     (eq (symbol-package (fun-name-block-name name))
         *cl-package*))
    (:type
     (let ((symbol (typecase name
                     (symbol name)
                     ((cons symbol) (car name))
                     (t (return-from name-reserved-by-ansi-p nil)))))
       (eq (symbol-package symbol) *cl-package*)))))

;;; This is to be called at the end of a compilation unit. It signals
;;; any residual warnings about unknown stuff, then prints the total
;;; error counts. ABORT-P should be true when the compilation unit was
;;; aborted by throwing out. ABORT-COUNT is the number of dynamically
;;; enclosed nested compilation units that were aborted.
(defun summarize-compilation-unit (abort-p)
  (let (summary)
    (unless abort-p
      (handler-bind ((style-warning #'compiler-style-warning-handler)
                     (warning #'compiler-warning-handler))

        (let ((undefs (sort *undefined-warnings* #'string<
                            :key (lambda (x)
                                   (let ((x (undefined-warning-name x)))
                                     (if (symbolp x)
                                         (symbol-name x)
                                         (prin1-to-string x)))))))
          (dolist (kind '(:variable :function :type))
            (let ((names (mapcar #'undefined-warning-name
                                   (remove kind undefs :test #'neq
                                           :key #'undefined-warning-kind))))
              (when names (push (cons kind names) summary))))
          (dolist (undef undefs)
            (let ((name (undefined-warning-name undef))
                  (kind (undefined-warning-kind undef))
                  (warnings (undefined-warning-warnings undef))
                  (undefined-warning-count (undefined-warning-count undef)))
              (dolist (*compiler-error-context* warnings)
                (if #-sb-xc-host (and (member kind '(:function :type))
                                      (name-reserved-by-ansi-p name kind)
                                      *flame-on-necessarily-undefined-thing*)
                    #+sb-xc-host nil
                    (ecase kind
                      (:function
                       (compiler-warn
                        "~@<The function ~S is undefined, and its name is ~
                            reserved by ANSI CL so that even if it were ~
                            defined later, the code doing so would not be ~
                            portable.~:@>" name))
                      (:type
                       (if (and (consp name) (eq 'quote (car name)))
                           (compiler-warn
                            "~@<Undefined type ~S. The name starts with ~S: ~
                             probably use of a quoted type name in a context ~
                             where the name is not evaluated.~:@>"
                            name 'quote)
                           (compiler-warn
                            "~@<Undefined type ~S. Note that name ~S is ~
                             reserved by ANSI CL, so code defining a type with ~
                             that name would not be portable.~:@>" name
                             name))))
                    (if (eq kind :variable)
                        (compiler-warn "undefined ~(~A~): ~S" kind name)
                        (compiler-style-warn "undefined ~(~A~): ~S" kind name))))
              (let ((warn-count (length warnings)))
                (when (and warnings (> undefined-warning-count warn-count))
                  (let ((more (- undefined-warning-count warn-count)))
                    (if (eq kind :variable)
                        (compiler-warn
                         "~W more use~:P of undefined ~(~A~) ~S"
                         more kind name)
                        (compiler-style-warn
                         "~W more use~:P of undefined ~(~A~) ~S"
                         more kind name))))))))))

    (unless (and (not abort-p)
                 (zerop *aborted-compilation-unit-count*)
                 (zerop *compiler-error-count*)
                 (zerop *compiler-warning-count*)
                 (zerop *compiler-style-warning-count*)
                 (zerop *compiler-note-count*))
      (pprint-logical-block (*error-output* nil :per-line-prefix "; ")
        (format *error-output* "~&compilation unit ~:[finished~;aborted~]"
                abort-p)
        (dolist (cell summary)
          (destructuring-bind (kind &rest names) cell
            (format *error-output*
                    "~&  Undefined ~(~A~)~p:~
                     ~%    ~{~<~% ~1:;~S~>~^ ~}"
                    kind (length names) names)))
        (format *error-output* "~[~:;~:*~&  caught ~W fatal ERROR condition~:P~]~
                                ~[~:;~:*~&  caught ~W ERROR condition~:P~]~
                                ~[~:;~:*~&  caught ~W WARNING condition~:P~]~
                                ~[~:;~:*~&  caught ~W STYLE-WARNING condition~:P~]~
                                ~[~:;~:*~&  printed ~W note~:P~]"
                *aborted-compilation-unit-count*
                *compiler-error-count*
                *compiler-warning-count*
                *compiler-style-warning-count*
                *compiler-note-count*))
      (terpri *error-output*)
      (force-output *error-output*))))

;; Bidrectional map between IR1/IR2/assembler abstractions
;; and a corresponding small integer identifier. One direction could be done
;; by adding the integer ID as an object slot, but we want both directions.
(defstruct (compiler-ir-obj-map (:conc-name objmap-)
                                (:constructor make-compiler-ir-obj-map ())
                                (:copier nil)
                                (:predicate nil))
  (obj-to-id   (make-hash-table :test 'eq) :read-only t)
  (id-to-cont  (make-array 10) :type simple-vector) ; number -> CTRAN or LVAR
  (id-to-tn    (make-array 10) :type simple-vector) ; number -> TN
  (id-to-label (make-array 10) :type simple-vector) ; number -> LABEL
  (cont-num    0 :type fixnum)
  (tn-id       0 :type fixnum)
  (label-id    0 :type fixnum))

(declaim (type compiler-ir-obj-map *compiler-ir-obj-map*))
(defvar *compiler-ir-obj-map*)

;;; Evaluate BODY, then return (VALUES BODY-VALUE WARNINGS-P
;;; FAILURE-P), where BODY-VALUE is the first value of the body, and
;;; WARNINGS-P and FAILURE-P are as in CL:COMPILE or CL:COMPILE-FILE.
(defmacro with-compilation-values (&body body)
  ;; This binding could just as well be in WITH-IR1-NAMESPACE, but
  ;; since it's primarily a debugging tool, it's nicer to have
  ;; a wider unique scope by ID.
  `(let ((*compiler-ir-obj-map* (make-compiler-ir-obj-map)))
       (unwind-protect
            (let ((*warnings-p* nil)
                  (*failure-p* nil))
              (handler-bind ((compiler-error #'compiler-error-handler)
                             (style-warning #'compiler-style-warning-handler)
                             (warning #'compiler-warning-handler))
                  (values (progn ,@body)
                       *warnings-p*
                       *failure-p*)))
         (let ((map *compiler-ir-obj-map*))
           (clrhash (objmap-obj-to-id map))
           (fill (objmap-id-to-cont map) nil)
           (fill (objmap-id-to-tn map) nil)
           (fill (objmap-id-to-label map) nil)))))

;;; THING is a kind of thing about which we'd like to issue a warning,
;;; but showing at most one warning for a given set of <THING,FMT,ARGS>.
;;; The compiler does a good job of making sure not to print repetitive
;;; warnings for code that it compiles, but this solves a different problem.
;;; Specifically, for a warning from PARSE-LAMBDA-LIST, there are three calls:
;;; - once in the expander for defmacro itself, as it calls MAKE-MACRO-LAMBDA
;;;   which calls PARSE-LAMBDA-LIST. This is the toplevel form processing.
;;; - again for :compile-toplevel, where the DS-BIND calls PARSE-LAMBDA-LIST.
;;;   If compiling in compile-toplevel, then *COMPILE-OBJECT* is a core object,
;;;   but if interpreting, then it is still a fasl.
;;; - once for compiling to fasl. *COMPILE-OBJECT* is a fasl.
;;; I'd have liked the data to be associated with the fasl, except that
;;; as indicated above, the second line hides some information.
(defun style-warn-once (thing fmt &rest args)
  (declare (special *compile-object*))
  (let* ((source-info *source-info*)
         (file-info (and (source-info-p source-info)
                         (source-info-file-info source-info)))
         (file-compiling-p (file-info-p file-info)))
    (flet ((match-p (entry &aux (rest (cdr entry)))
             ;; THING is compared by EQ, FMT by STRING=.
             (and (eq (car entry) thing)
                  (string= (car rest) fmt)
                  ;; We don't want to walk into default values,
                  ;; e.g. (&optional (b #<insane-struct))
                  ;; because #<insane-struct> might be circular.
                  (equal-but-no-car-recursion (cdr rest) args))))
      (unless (and file-compiling-p
                   (find-if #'match-p
                            (file-info-style-warning-tracker file-info)))
        (when file-compiling-p
          (push (list* thing fmt args)
                (file-info-style-warning-tracker file-info)))
        (apply 'style-warn fmt args)))))

;;;; component compilation

(defparameter *max-optimize-iterations* 3 ; ARB
  #!+sb-doc
  "The upper limit on the number of times that we will consecutively do IR1
optimization that doesn't introduce any new code. A finite limit is
necessary, since type inference may take arbitrarily long to converge.")

(defevent ir1-optimize-until-done "IR1-OPTIMIZE-UNTIL-DONE called")
(defevent ir1-optimize-maxed-out "hit *MAX-OPTIMIZE-ITERATIONS* limit")

;;; Repeatedly optimize COMPONENT until no further optimizations can
;;; be found or we hit our iteration limit. When we hit the limit, we
;;; clear the component and block REOPTIMIZE flags to discourage the
;;; next optimization attempt from pounding on the same code.
(defun ir1-optimize-until-done (component)
  (declare (type component component))
  (maybe-mumble "opt")
  (event ir1-optimize-until-done)
  (let ((count 0)
        (cleared-reanalyze nil)
        (fastp nil))
    (loop
      (when (component-reanalyze component)
        (setq count 0)
        (setq cleared-reanalyze t)
        (setf (component-reanalyze component) nil))
      (setf (component-reoptimize component) nil)
      (ir1-optimize component fastp)
      (cond ((component-reoptimize component)
             (incf count)
             (when (and (>= count *max-optimize-iterations*)
                        (not (component-reanalyze component))
                        (eq (component-reoptimize component) :maybe))
               (maybe-mumble "*")
               (cond ((retry-delayed-ir1-transforms :optimize)
                      (maybe-mumble "+")
                      (setq count 0))
                     (t
                      (event ir1-optimize-maxed-out)
                      (setf (component-reoptimize component) nil)
                      (do-blocks (block component)
                        (setf (block-reoptimize block) nil))
                      (return)))))
            ((retry-delayed-ir1-transforms :optimize)
             (setf count 0)
             (maybe-mumble "+"))
            (t
             (maybe-mumble " ")
             (return)))
      (setq fastp (>= count *max-optimize-iterations*))
      (maybe-mumble (if fastp "-" ".")))
    (when cleared-reanalyze
      (setf (component-reanalyze component) t)))
  (values))

(defparameter *constraint-propagate* t)

;;; KLUDGE: This was bumped from 5 to 10 in a DTC patch ported by MNA
;;; from CMU CL into sbcl-0.6.11.44, the same one which allowed IR1
;;; transforms to be delayed. Either DTC or MNA or both didn't explain
;;; why, and I don't know what the rationale was. -- WHN 2001-04-28
;;;
;;; FIXME: It would be good to document why it's important to have a
;;; large value here, and what the drawbacks of an excessively large
;;; value are; and it might also be good to make it depend on
;;; optimization policy.
(defparameter *reoptimize-after-type-check-max* 10)

(defevent reoptimize-maxed-out
  "*REOPTIMIZE-AFTER-TYPE-CHECK-MAX* exceeded.")

;;; Iterate doing FIND-DFO until no new dead code is discovered.
(defun dfo-as-needed (component)
  (declare (type component component))
  (when (component-reanalyze component)
    (maybe-mumble "DFO")
    (loop
      (find-dfo component)
      (unless (component-reanalyze component)
        (maybe-mumble " ")
        (return))
      (maybe-mumble ".")))
  (values))

;;; Do all the IR1 phases for a non-top-level component.
(defun ir1-phases (component)
  (declare (type component component))
  (aver-live-component component)
  (let ((*constraint-universe* (make-array 64 ; arbitrary, but don't
                                              ;make this 0.
                                           :fill-pointer 0 :adjustable t))
        (loop-count 1)
        (*delayed-ir1-transforms* nil))
    (declare (special *constraint-universe* *delayed-ir1-transforms*))
    (loop
      (ir1-optimize-until-done component)
      (when (or (component-new-functionals component)
                (component-reanalyze-functionals component))
        (maybe-mumble "locall ")
        (locall-analyze-component component))
      (dfo-as-needed component)
      (when *constraint-propagate*
        (maybe-mumble "constraint ")
        (constraint-propagate component))
      (when (retry-delayed-ir1-transforms :constraint)
        (maybe-mumble "Rtran "))
      (flet ((want-reoptimization-p ()
               (or (component-reoptimize component)
                   (component-reanalyze component)
                   (component-new-functionals component)
                   (component-reanalyze-functionals component))))
        (unless (and (want-reoptimization-p)
                     ;; We delay the generation of type checks until
                     ;; the type constraints have had time to
                     ;; propagate, else the compiler can confuse itself.
                     (< loop-count (- *reoptimize-after-type-check-max* 4)))
          (maybe-mumble "type ")
          (generate-type-checks component)
          (unless (want-reoptimization-p)
            (return))))
      (when (>= loop-count *reoptimize-after-type-check-max*)
        (maybe-mumble "[reoptimize limit]")
        (event reoptimize-maxed-out)
        (return))
      (incf loop-count)))

  (when *check-consistency*
    (do-blocks-backwards (block component)
      (awhen (flush-dead-code block)
        (let ((*compiler-error-context* it))
          (compiler-warn "dead code detected at the end of ~S"
                         'ir1-phases)))))

  (ir1-finalize component)
  (values))

(defun %compile-component (component)
  (let ((*code-segment* nil)
        (*elsewhere* nil)
        #!+inline-constants
        (*constant-segment* nil)
        #!+inline-constants
        (*constant-table* nil)
        #!+inline-constants
        (*constant-vector* nil))
    (maybe-mumble "GTN ")
    (gtn-analyze component)
    (maybe-mumble "LTN ")
    (ltn-analyze component)
    (dfo-as-needed component)
    (maybe-mumble "control ")
    (control-analyze component #'make-ir2-block)

    (when (or (ir2-component-values-receivers (component-info component))
              (component-dx-lvars component))
      (maybe-mumble "stack ")
      (find-dominators component)
      (stack-analyze component)
      ;; Assign BLOCK-NUMBER for any cleanup blocks introduced by
      ;; stack analysis. There shouldn't be any unreachable code after
      ;; control, so this won't delete anything.
      (dfo-as-needed component))

    (unwind-protect
        (progn
          (maybe-mumble "IR2tran ")
          (init-assembler)
          (entry-analyze component)
          (ir2-convert component)

          (when (policy *lexenv* (>= speed compilation-speed))
            (maybe-mumble "copy ")
            (copy-propagate component))

          (ir2-optimize component)

          (select-representations component)

          (when *check-consistency*
            (maybe-mumble "check2 ")
            (check-ir2-consistency component))

          (delete-unreferenced-tns component)

          (maybe-mumble "life ")
          (lifetime-analyze component)

          (when *compile-progress*
            (compiler-mumble "") ; Sync before doing more output.
            (pre-pack-tn-stats component *standard-output*))

          (when *check-consistency*
            (maybe-mumble "check-life ")
            (check-life-consistency component))

          (maybe-mumble "pack ")
          (sb!regalloc:pack component)

          (when *check-consistency*
            (maybe-mumble "check-pack ")
            (check-pack-consistency component))

          (when *compiler-trace-output*
            (describe-component component *compiler-trace-output*)
            (describe-ir2-component component *compiler-trace-output*))

          (maybe-mumble "code ")
          (multiple-value-bind (code-length fixup-notes)
              (generate-code component)

            #-sb-xc-host
            (when *compiler-trace-output*
              (format *compiler-trace-output*
                      "~|~%disassembly of code for ~S~2%" component)
              (sb!disassem:disassemble-assem-segment *code-segment*
                                                     *compiler-trace-output*))

            (etypecase *compile-object*
              (fasl-output
               (maybe-mumble "fasl")
               (fasl-dump-component component
                                    *code-segment*
                                    code-length
                                    fixup-notes
                                    *compile-object*))
              #-sb-xc-host ; no compiling to core
              (core-object
               (maybe-mumble "core")
               (make-core-component component
                                    *code-segment*
                                    code-length
                                    fixup-notes
                                    *compile-object*))
              (null))))))

  ;; We're done, so don't bother keeping anything around.
  (setf (component-info component) :dead)

  (values))

;;; Delete components with no external entry points before we try to
;;; generate code. Unreachable closures can cause IR2 conversion to
;;; puke on itself, since it is the reference to the closure which
;;; normally causes the components to be combined.
(defun delete-if-no-entries (component)
  (dolist (fun (component-lambdas component) (delete-component component))
    (when (functional-has-external-references-p fun)
      (return))
    (case (functional-kind fun)
      (:toplevel (return))
      (:external
       (unless (every (lambda (ref)
                        (eq (node-component ref) component))
                      (leaf-refs fun))
         (return))))))

(defun compile-component (component)

  ;; miscellaneous sanity checks
  ;;
  ;; FIXME: These are basically pretty wimpy compared to the checks done
  ;; by the old CHECK-IR1-CONSISTENCY code. It would be really nice to
  ;; make those internal consistency checks work again and use them.
  (aver-live-component component)
  (do-blocks (block component)
    (aver (eql (block-component block) component)))
  (dolist (lambda (component-lambdas component))
    ;; sanity check to prevent weirdness from propagating insidiously as
    ;; far from its root cause as it did in bug 138: Make sure that
    ;; thing-to-COMPONENT links are consistent.
    (aver (eql (lambda-component lambda) component))
    (aver (eql (node-component (lambda-bind lambda)) component)))

  (let* ((*component-being-compiled* component))

    ;; Record xref information before optimization. This way the
    ;; stored xref data reflects the real source as closely as
    ;; possible.
    (record-component-xrefs component)

    (ir1-phases component)

    (when *loop-analyze*
      (dfo-as-needed component)
      (find-dominators component)
      (loop-analyze component))

    #|
    (when (and *loop-analyze* *compiler-trace-output*)
      (labels ((print-blocks (block)
                 (format *compiler-trace-output* "    ~A~%" block)
                 (when (block-loop-next block)
                   (print-blocks (block-loop-next block))))
               (print-loop (loop)
                 (format *compiler-trace-output* "loop=~A~%" loop)
                 (print-blocks (loop-blocks loop))
                 (dolist (l (loop-inferiors loop))
                   (print-loop l))))
        (print-loop (component-outer-loop component))))
    |#

    ;; This should happen at some point before PHYSENV-ANALYZE, and
    ;; after RECORD-COMPONENT-XREFS.  Beyond that, I haven't really
    ;; thought things through.  -- AJB, 2014-Jun-08
    (eliminate-dead-code component)

    ;; FIXME: What is MAYBE-MUMBLE for? Do we need it any more?
    (maybe-mumble "env ")
    (physenv-analyze component)
    (dfo-as-needed component)

    (delete-if-no-entries component)

    (unless (eq (block-next (component-head component))
                (component-tail component))
      (%compile-component component)))

  (clear-constant-info)

  (values))

;;;; clearing global data structures
;;;;
;;;; FIXME: Is it possible to get rid of this stuff, getting rid of
;;;; global data structures entirely when possible and consing up the
;;;; others from scratch instead of clearing and reusing them?

;;; Clear the INFO in constants in the *FREE-VARS*, etc. In
;;; addition to allowing stuff to be reclaimed, this is required for
;;; correct assignment of constant offsets, since we need to assign a
;;; new offset for each component. We don't clear the FUNCTIONAL-INFO
;;; slots, since they are used to keep track of functions across
;;; component boundaries.
(defun clear-constant-info ()
  (maphash (lambda (k v)
             (declare (ignore k))
             (setf (leaf-info v) nil)
             (setf (constant-boxed-tn v) nil))
           *constants*)
  (maphash (lambda (k v)
             (declare (ignore k))
             (when (constant-p v)
               (setf (leaf-info v) nil)
               (setf (constant-boxed-tn v) nil)))
           *free-vars*)
  (values))

;;; Blow away the REFS for all global variables, and let COMPONENT
;;; be recycled.
(defun clear-ir1-info (component)
  (declare (type component component))
  (labels ((blast (x)
             (maphash (lambda (k v)
                        (declare (ignore k))
                        (when (leaf-p v)
                          (setf (leaf-refs v)
                                (delete-if #'here-p (leaf-refs v)))
                          (when (basic-var-p v)
                            (setf (basic-var-sets v)
                                  (delete-if #'here-p (basic-var-sets v))))))
                      x))
           (here-p (x)
             (eq (node-component x) component)))
    (blast *free-vars*)
    (blast *free-funs*)
    (blast *constants*))
  (values))

;;;; trace output

;;; Print out some useful info about COMPONENT to STREAM.
(defun describe-component (component *standard-output*)
  (declare (type component component))
  (format t "~|~%;;;; component: ~S~2%" (component-name component))
  (print-all-blocks component)
  (values))

(defun describe-ir2-component (component *standard-output*)
  (format t "~%~|~%;;;; IR2 component: ~S~2%" (component-name component))
  (format t "entries:~%")
  (dolist (entry (ir2-component-entries (component-info component)))
    (format t "~4TL~D: ~S~:[~; [closure]~]~%"
            (label-id (entry-info-offset entry))
            (entry-info-name entry)
            (entry-info-closure-tn entry)))
  (terpri)
  (pre-pack-tn-stats component *standard-output*)
  (terpri)
  (print-ir2-blocks component)
  (terpri)
  (values))

;;; Given a pathname, return a SOURCE-INFO structure.
(defun make-file-source-info (file external-format &optional form-tracking-p)
  (make-source-info
   :file-info (make-file-info :name (truename file)
                              :untruename (merge-pathnames file)
                              :external-format external-format
                              :subforms
                              (if form-tracking-p
                                  (make-array 100 :fill-pointer 0 :adjustable t))
                              :write-date (file-write-date file))))

;;; Return a SOURCE-INFO to describe the incremental compilation of FORM.
(defun make-lisp-source-info (form &key parent)
  (make-source-info
   :file-info (make-file-info :name :lisp
                              :forms (vector form)
                              :positions '#(0))
   :parent parent))

;;; Walk up the SOURCE-INFO list until we either reach a SOURCE-INFO
;;; with no parent (e.g., from a REPL evaluation) or until we reach a
;;; SOURCE-INFO whose FILE-INFO denotes a file.
(defun get-toplevelish-file-info (&optional (source-info *source-info*))
  (if source-info
      (do* ((sinfo source-info (source-info-parent sinfo))
            (finfo (source-info-file-info sinfo)
                   (source-info-file-info sinfo)))
           ((or (not (source-info-p (source-info-parent sinfo)))
                (pathnamep (file-info-name finfo)))
            finfo))))

;;; If STREAM is present, return it, otherwise open a stream to the
;;; current file. There must be a current file.
;;;
;;; FIXME: This is probably an unnecessarily roundabout way to do
;;; things now that we process a single file in COMPILE-FILE (unlike
;;; the old CMU CL code, which accepted multiple files). Also, the old
;;; comment said
;;;   When we open a new file, we also reset *PACKAGE* and policy.
;;;   This gives the effect of rebinding around each file.
;;; which doesn't seem to be true now. Check to make sure that if
;;; such rebinding is necessary, it's still done somewhere.
(defun get-source-stream (info)
  (declare (type source-info info))
  (or (source-info-stream info)
      (let* ((file-info (source-info-file-info info))
             (name (file-info-name file-info))
             (external-format (file-info-external-format file-info)))
        (setf sb!xc:*compile-file-truename* name
              sb!xc:*compile-file-pathname* (file-info-untruename file-info)
              (source-info-stream info)
              (let ((stream
                     (open name
                           :direction :input
                           :external-format external-format
                           ;; SBCL stream classes aren't available in the host
                           #-sb-xc-host :class
                           #-sb-xc-host 'form-tracking-stream)))
                (when (file-info-subforms file-info)
                  (setf (form-tracking-stream-observer stream)
                        (make-form-tracking-stream-observer file-info)))
                stream)))))

;;; Close the stream in INFO if it is open.
(defun close-source-info (info)
  (declare (type source-info info))
  (let ((stream (source-info-stream info)))
    (when stream (close stream)))
  (setf (source-info-stream info) nil)
  (values))

;; Loop over forms read from INFO's stream, calling FUNCTION with each.
;; CONDITION-NAME is signaled if there is a reader error, and should be
;; a subtype of not-so-aptly-named INPUT-ERROR-IN-COMPILE-FILE.
(defun %do-forms-from-info (function info condition-name)
  (declare (function function))
  (let* ((file-info (source-info-file-info info))
         (stream (get-source-stream info))
         (pos (file-position stream))
         (form
          ;; Return a form read from STREAM; or for EOF use the trick,
          ;; popularized by Kent Pitman, of returning STREAM itself.
          (handler-case
              (progn
                ;; Reset for a new toplevel form.
                (when (form-tracking-stream-p stream)
                  (setf (form-tracking-stream-form-start-char-pos stream) nil))
                (awhen (file-info-subforms file-info)
                  (setf (fill-pointer it) 0))
                (read-preserving-whitespace stream nil stream))
            (reader-error (condition)
              (compiler-error condition-name
                ;; We don't need to supply :POSITION here because
                ;; READER-ERRORs already know their position in the file.
                              :condition condition
                              :stream stream))
            ;; ANSI, in its wisdom, says that READ should return END-OF-FILE
            ;; (and that this is not a READER-ERROR) when it encounters end of
            ;; file in the middle of something it's trying to read,
            ;; making it unfortunately indistinguishable from legal EOF.
            ;; Were it not for that, it would be more elegant to just
            ;; handle one more condition in the HANDLER-CASE.
            ((or end-of-file error) (condition)
              (compiler-error
               condition-name
               :condition condition
                ;; We need to supply :POSITION here because the END-OF-FILE
                ;; condition doesn't carry the position that the user
                ;; probably cares about, where the failed READ began.
               :position
               (or (and (form-tracking-stream-p stream)
                        (form-tracking-stream-form-start-byte-pos stream))
                   pos)
               :line/col
               (and (form-tracking-stream-p stream)
                    (line/col-from-charpos
                     stream
                     (form-tracking-stream-form-start-char-pos stream)))
               :stream stream)))))
    (unless (eq form stream) ; not EOF
      (funcall function form
               :current-index
               (let* ((forms (file-info-forms file-info))
                      (current-idx (+ (fill-pointer forms)
                                      (file-info-source-root file-info))))
                 (vector-push-extend form forms)
                 (vector-push-extend pos (file-info-positions file-info))
                 current-idx))
      (%do-forms-from-info function info condition-name))))

;;; Loop over FORMS retrieved from INFO.  Used by COMPILE-FILE and
;;; LOAD when loading from a FILE-STREAM associated with a source
;;; file.  ON-ERROR is the name of a condition class that should
;;; be signaled if anything goes wrong during a READ.
(defmacro do-forms-from-info (((form &rest keys) info
                               &optional (on-error ''input-error-in-load))
                              &body body)
  (aver (symbolp form))
  (once-only ((info info))
    `(let ((*source-info* ,info))
       (%do-forms-from-info (lambda (,form &key ,@keys &allow-other-keys)
                              ,@body)
                            ,info ,on-error))))

;;; Read and compile the source file.
(defun sub-sub-compile-file (info)
  (do-forms-from-info ((form current-index) info
                       'input-error-in-compile-file)
    (with-source-paths
      (find-source-paths form current-index)
      (process-toplevel-form
       form `(original-source-start 0 ,current-index) nil)))
  ;; It's easy to get into a situation where cold-init crashes and the only
  ;; backtrace you get from ldb is TOP-LEVEL-FORM, which means you're anywhere
  ;; within the 23000 or so blobs of code deferred until cold-init.
  ;; Seeing each file finish narrows things down without the noise of :sb-show,
  ;; but this hack messes up form positions, so it's not on unless asked for.
  #+nil ; change to #+sb-xc-host if desired
  (let ((file-info (get-toplevelish-file-info info)))
    (declare (ignorable file-info))
    (let* ((forms (file-info-forms file-info))
           (form
            `(write-string
              ,(format nil "Completed TLFs: ~A~%" (file-info-name file-info))))
           (index
            (+ (fill-pointer forms) (file-info-source-root file-info))))
      (with-source-paths
        (find-source-paths form index)
        (process-toplevel-form
         form `(original-source-start 0 ,index) nil)))))

;;; Return the INDEX'th source form read from INFO and the position
;;; where it was read.
(defun find-source-root (index info)
  (declare (type index index) (type source-info info))
  (let ((file-info (source-info-file-info info)))
    (values (aref (file-info-forms file-info) index)
            (aref (file-info-positions file-info) index))))

;;;; processing of top level forms

;;; This is called by top level form processing when we are ready to
;;; actually compile something. If *BLOCK-COMPILE* is T, then we still
;;; convert the form, but delay compilation, pushing the result on
;;; *TOPLEVEL-LAMBDAS* instead.
(defun convert-and-maybe-compile (form path &optional (expand t))
  (declare (list path))
  #+sb-xc-host
  (when sb-cold::*compile-for-effect-only*
    (return-from convert-and-maybe-compile))
  (let ((*top-level-form-noted* (note-top-level-form form t)))
    ;; Don't bother to compile simple objects that just sit there.
    (when (and form (or (symbolp form) (consp form)))
      (if (fopcompilable-p form expand)
          (let ((*fopcompile-label-counter* 0))
            (fopcompile form path nil expand))
          (with-ir1-namespace
            (let ((*lexenv* (make-lexenv
                             :policy *policy*
                             :handled-conditions *handled-conditions*
                             :disabled-package-locks *disabled-package-locks*))
                  (tll (ir1-toplevel form path nil)))
              (if (eq *block-compile* t)
                  (push tll *toplevel-lambdas*)
                  (compile-toplevel (list tll) nil))
              nil))))))

;;; Macroexpand FORM in the current environment with an error handler.
;;; We only expand one level, so that we retain all the intervening
;;; forms in the source path. A compiler-macro takes precedence over
;;; an ordinary macro as specified in CLHS 3.2.3.1
;;; Note that this function is _only_ for processing of toplevel forms.
;;; Non-toplevel forms use IR1-CONVERT-FUNCTOID which considers compiler macros.
(defun preprocessor-macroexpand-1 (form)
  (if (listp form)
      (let ((expansion (expand-compiler-macro form)))
        (if (neq expansion form)
            (return-from preprocessor-macroexpand-1
              (values expansion t)))))
  (handler-case (%macroexpand-1 form *lexenv*)
    (error (condition)
      (compiler-error "(during macroexpansion of ~A)~%~A"
                      (let ((*print-level* 2)
                            (*print-length* 2))
                        (format nil "~S" form))
                      condition))))

;;; Process a PROGN-like portion of a top level form. FORMS is a list of
;;; the forms, and PATH is the source path of the FORM they came out of.
;;; COMPILE-TIME-TOO is as in ANSI "3.2.3.1 Processing of Top Level Forms".
(defun process-toplevel-progn (forms path compile-time-too)
  (declare (list forms) (list path))
  (dolist (form forms)
    (process-toplevel-form form path compile-time-too)))

;;; Process a top level use of LOCALLY, or anything else (e.g.
;;; MACROLET) at top level which has declarations and ordinary forms.
;;; We parse declarations and then recursively process the body.
(defun process-toplevel-locally (body path compile-time-too &key vars funs)
  (declare (list path))
  (multiple-value-bind (forms decls) (parse-body body nil t)
    (with-ir1-namespace
      (let* ((*lexenv* (process-decls decls vars funs))
             ;; FIXME: VALUES declaration
             ;;
             ;; Binding *POLICY* is pretty much of a hack, since it
             ;; causes LOCALLY to "capture" enclosed proclamations. It
             ;; is necessary because CONVERT-AND-MAYBE-COMPILE uses the
             ;; value of *POLICY* as the policy. The need for this hack
             ;; is due to the quirk that there is no way to represent in
             ;; a POLICY that an optimize quality came from the default.
             ;;
             ;; FIXME: Ideally, something should be done so that DECLAIM
             ;; inside LOCALLY works OK. Failing that, at least we could
             ;; issue a warning instead of silently screwing up.
             ;; Here's how to fix this: a POLICY object can in fact represent
             ;; absence of qualitities. Whenever we rebind *POLICY* (here and
             ;; elsewhere), it should be bound to a policy that expresses no
             ;; qualities. Proclamations should update SYMBOL-GLOBAL-VALUE of
             ;; *POLICY*, which can be seen irrespective of dynamic bindings,
             ;; and declarations should update the lexical policy.
             ;; The POLICY macro can be amended to merge the dynamic *POLICY*
             ;; (or whatever it came from, like a LEXENV) with the global
             ;; *POLICY*. COERCE-TO-POLICY can do the merge, employing a 1-line
             ;; cache so that repeated calls for any two fixed policy objects
             ;; return the identical value (since policies are immutable).
             (*policy* (lexenv-policy *lexenv*))
             ;; This is probably also a hack
             (*handled-conditions* (lexenv-handled-conditions *lexenv*))
             ;; ditto
             (*disabled-package-locks* (lexenv-disabled-package-locks *lexenv*)))
        (process-toplevel-progn forms path compile-time-too)))))

;;; Parse an EVAL-WHEN situations list, returning three flags,
;;; (VALUES COMPILE-TOPLEVEL LOAD-TOPLEVEL EXECUTE), indicating
;;; the types of situations present in the list.
(defun parse-eval-when-situations (situations)
  (when (or (not (listp situations))
            (set-difference situations
                            '(:compile-toplevel
                              compile
                              :load-toplevel
                              load
                              :execute
                              eval)))
    (compiler-error "bad EVAL-WHEN situation list: ~S" situations))
  (let ((deprecated-names (intersection situations '(compile load eval))))
    (when deprecated-names
      (style-warn "using deprecated EVAL-WHEN situation names~{ ~S~}"
                  deprecated-names)))
  (values (intersection '(:compile-toplevel compile)
                        situations)
          (intersection '(:load-toplevel load) situations)
          (intersection '(:execute eval) situations)))


;;; utilities for extracting COMPONENTs of FUNCTIONALs
(defun functional-components (f)
  (declare (type functional f))
  (etypecase f
    (clambda (list (lambda-component f)))
    (optional-dispatch (let ((result nil))
                         (flet ((maybe-frob (maybe-clambda)
                                  (when (and maybe-clambda
                                             (promise-ready-p maybe-clambda))
                                    (pushnew (lambda-component
                                              (force maybe-clambda))
                                             result))))
                           (map nil #'maybe-frob (optional-dispatch-entry-points f))
                           (maybe-frob (optional-dispatch-more-entry f))
                           (maybe-frob (optional-dispatch-main-entry f)))
                         result))))

(defun make-functional-from-toplevel-lambda (lambda-expression
                                             &key
                                             name
                                             (path
                                              ;; I'd thought NIL should
                                              ;; work, but it doesn't.
                                              ;; -- WHN 2001-09-20
                                              (missing-arg)))
  (let* ((*current-path* path)
         (component (make-empty-component))
         (*current-component* component)
         (debug-name-tail (or name (name-lambdalike lambda-expression)))
         (source-name (or name '.anonymous.)))
    (setf (component-name component) (debug-name 'initial-component debug-name-tail)
          (component-kind component) :initial)
    (let* ((fun (let ((*allow-instrumenting* t))
                  (funcall #'ir1-convert-lambdalike
                           lambda-expression
                           :source-name source-name)))
           ;; Convert the XEP using the policy of the real function. Otherwise
           ;; the wrong policy will be used for deciding whether to type-check
           ;; the parameters of the real function (via CONVERT-CALL /
           ;; PROPAGATE-TO-ARGS). -- JES, 2007-02-27
           (*lexenv* (make-lexenv :policy (lexenv-policy (functional-lexenv fun))))
           (xep (ir1-convert-lambda (make-xep-lambda-expression fun)
                                    :source-name source-name
                                    :debug-name (debug-name 'tl-xep debug-name-tail)
                                    :system-lambda t)))
      (when name
        (assert-global-function-definition-type name fun))
      (setf (functional-kind xep) :external
            (functional-entry-fun xep) fun
            (functional-entry-fun fun) xep
            (component-reanalyze component) t
            (functional-has-external-references-p xep) t)
      (reoptimize-component component :maybe)
      (locall-analyze-xep-entry-point fun)
      ;; Any leftover REFs to FUN outside local calls get replaced with the
      ;; XEP.
      (substitute-leaf-if (lambda (ref)
                            (let* ((lvar (ref-lvar ref))
                                   (dest (when lvar (lvar-dest lvar)))
                                   (kind (when (basic-combination-p dest)
                                           (basic-combination-kind dest))))
                              (neq :local kind)))
                          xep
                          fun)
      xep)))

;;; Compile LAMBDA-EXPRESSION into *COMPILE-OBJECT*, returning a
;;; description of the result.
;;;   * If *COMPILE-OBJECT* is a CORE-OBJECT, then write the function
;;;     into core and return the compiled FUNCTION value.
;;;   * If *COMPILE-OBJECT* is a fasl file, then write the function
;;;     into the fasl file and return a dump handle.
;;;
;;; If NAME is provided, then we try to use it as the name of the
;;; function for debugging/diagnostic information.
(defun %compile (lambda-expression
                 *compile-object*
                 &key
                 name
                 (path
                  ;; This magical idiom seems to be the appropriate
                  ;; path for compiling standalone LAMBDAs, judging
                  ;; from the CMU CL code and experiment, so it's a
                  ;; nice default for things where we don't have a
                  ;; real source path (as in e.g. inside CL:COMPILE).
                  '(original-source-start 0 0)))
  (when name
    (legal-fun-name-or-type-error name))
  (with-ir1-namespace
    (let* ((*lexenv* (make-lexenv
                      :policy *policy*
                      :handled-conditions *handled-conditions*
                      :disabled-package-locks *disabled-package-locks*))
           (*compiler-sset-counter* 0)
           (fun (make-functional-from-toplevel-lambda lambda-expression
                                                      :name name
                                                      :path path)))

      ;; FIXME: The compile-it code from here on is sort of a
      ;; twisted version of the code in COMPILE-TOPLEVEL. It'd be
      ;; better to find a way to share the code there; or
      ;; alternatively, to use this code to replace the code there.
      ;; (The second alternative might be pretty easy if we used
      ;; the :LOCALL-ONLY option to IR1-FOR-LAMBDA. Then maybe the
      ;; whole FUNCTIONAL-KIND=:TOPLEVEL case could go away..)

      (locall-analyze-clambdas-until-done (list fun))

      (let ((components-from-dfo (find-initial-dfo (list fun))))
        (dolist (component-from-dfo components-from-dfo)
          (compile-component component-from-dfo)
          (replace-toplevel-xeps component-from-dfo))

        (let ((entry-table (etypecase *compile-object*
                             (fasl-output (fasl-output-entry-table
                                           *compile-object*))
                             (core-object (core-object-entry-table
                                           *compile-object*)))))
          (multiple-value-bind (result found-p)
              (gethash (leaf-info fun) entry-table)
            (aver found-p)
            (prog1
                result
              ;; KLUDGE: This code duplicates some other code in this
              ;; file. In the great reorganzation, the flow of program
              ;; logic changed from the original CMUCL model, and that
              ;; path (as of sbcl-0.7.5 in SUB-COMPILE-FILE) was no
              ;; longer followed for CORE-OBJECTS, leading to BUG
              ;; 156. This place is transparently not the right one for
              ;; this code, but I don't have a clear enough overview of
              ;; the compiler to know how to rearrange it all so that
              ;; this operation fits in nicely, and it was blocking
              ;; reimplementation of (DECLAIM (INLINE FOO)) (MACROLET
              ;; ((..)) (DEFUN FOO ...))
              ;;
              ;; FIXME: This KLUDGE doesn't solve all the problem in an
              ;; ideal way, as (1) definitions typed in at the REPL
              ;; without an INLINE declaration will give a NULL
              ;; FUNCTION-LAMBDA-EXPRESSION (allowable, but not ideal)
              ;; and (2) INLINE declarations will yield a
              ;; FUNCTION-LAMBDA-EXPRESSION headed by
              ;; SB-C:LAMBDA-WITH-LEXENV, even for null LEXENV.  -- CSR,
              ;; 2002-07-02
              ;;
              ;; (2) is probably fairly easy to fix -- it is, after all,
              ;; a matter of list manipulation (or possibly of teaching
              ;; CL:FUNCTION about SB-C:LAMBDA-WITH-LEXENV).  (1) is
              ;; significantly harder, as the association between
              ;; function object and source is a tricky one.
              ;;
              ;; FUNCTION-LAMBDA-EXPRESSION "works" (i.e. returns a
              ;; non-NULL list) when the function in question has been
              ;; compiled by (COMPILE <x> '(LAMBDA ...)); it does not
              ;; work when it has been compiled as part of the top-level
              ;; EVAL strategy of compiling everything inside (LAMBDA ()
              ;; ...).  -- CSR, 2002-11-02
              (when (core-object-p *compile-object*)
                (fix-core-source-info *source-info* *compile-object* result))

              (mapc #'clear-ir1-info components-from-dfo))))))))

(defun note-top-level-form (form &optional finalp)
  (when *compile-print*
    (cond ((not *top-level-form-noted*)
           (let ((*print-length* 2)
                 (*print-level* 2)
                 (*print-pretty* nil))
             (with-compiler-io-syntax
                 (compiler-mumble
                  #-sb-xc-host "~&; ~:[compiling~;converting~] ~S"
                  #+sb-xc-host "~&; ~:[x-compiling~;x-converting~] ~S"
                  *block-compile* form)))
             form)
          ((and finalp
                (eq :top-level-forms *compile-print*)
                (neq form *top-level-form-noted*))
           (let ((*print-length* 1)
                 (*print-level* 1)
                 (*print-pretty* nil))
             (with-compiler-io-syntax
                 (compiler-mumble "~&; ... top level ~S" form)))
           form)
          (t
           *top-level-form-noted*))))

;;; Handle the evaluation the a :COMPILE-TOPLEVEL body during
;;; compilation. Normally just evaluate in the appropriate
;;; environment, but also compile if outputting a CFASL.
(defun eval-compile-toplevel (body path)
  (flet ((frob ()
           (eval-tlf `(progn ,@body) (source-path-tlf-number path) *lexenv*)
           (when *compile-toplevel-object*
             (let ((*compile-object* *compile-toplevel-object*))
               (convert-and-maybe-compile `(progn ,@body) path)))))
    (if (null *macro-policy*)
        (frob)
        (let* ((*lexenv*
                (make-lexenv
                 :policy (process-optimize-decl
                          `(optimize ,@(policy-to-decl-spec *macro-policy*))
                          (lexenv-policy *lexenv*))
                 :default *lexenv*))
               ;; In case a null lexenv is created, it needs to get the newly
               ;; effective global policy, not the policy currently in *POLICY*.
               (*policy* (lexenv-policy *lexenv*)))
          (frob)))))

;;; Process a top level FORM with the specified source PATH.
;;;  * If this is a magic top level form, then do stuff.
;;;  * If this is a macro, then expand it.
;;;  * Otherwise, just compile it.
;;;
;;; COMPILE-TIME-TOO is as defined in ANSI
;;; "3.2.3.1 Processing of Top Level Forms".
(defun process-toplevel-form (form path compile-time-too)
  (declare (list path))

  (catch 'process-toplevel-form-error-abort
    (let* ((path (or (get-source-path form) (cons form path)))
           (*current-path* path)
           (*compiler-error-bailout*
            (lambda (&optional condition)
              (convert-and-maybe-compile
               (make-compiler-error-form condition form)
               path)
              (throw 'process-toplevel-form-error-abort nil))))

      (flet ((default-processor (form)
               (let ((*top-level-form-noted* (note-top-level-form form)))
                 ;; When we're cross-compiling, consider: what should we
                 ;; do when we hit e.g.
                 ;;   (EVAL-WHEN (:COMPILE-TOPLEVEL)
                 ;;     (DEFUN FOO (X) (+ 7 X)))?
                 ;; DEFUN has a macro definition in the cross-compiler,
                 ;; and a different macro definition in the target
                 ;; compiler. The only sensible thing is to use the
                 ;; target compiler's macro definition, since the
                 ;; cross-compiler's macro is in general into target
                 ;; functions which can't meaningfully be executed at
                 ;; cross-compilation time. So make sure we do the EVAL
                 ;; here, before we macroexpand.
                 ;;
                 ;; Then things get even dicier with something like
                 ;;   (DEFCONSTANT-EQX SB!XC:LAMBDA-LIST-KEYWORDS ..)
                 ;; where we have to make sure that we don't uncross
                 ;; the SB!XC: prefix before we do EVAL, because otherwise
                 ;; we'd be trying to redefine the cross-compilation host's
                 ;; constants.
                 ;;
                 ;; (Isn't it fun to cross-compile Common Lisp?:-)
                 #+sb-xc-host
                 (progn
                   (when compile-time-too
                     (eval form)) ; letting xc host EVAL do its own macroexpansion
                   (let* (;; (We uncross the operator name because things
                          ;; like SB!XC:DEFCONSTANT and SB!XC:DEFTYPE
                          ;; should be equivalent to their CL: counterparts
                          ;; when being compiled as target code. We leave
                          ;; the rest of the form uncrossed because macros
                          ;; might yet expand into EVAL-WHEN stuff, and
                          ;; things inside EVAL-WHEN can't be uncrossed
                          ;; until after we've EVALed them in the
                          ;; cross-compilation host.)
                          (slightly-uncrossed (cons (uncross (first form))
                                                    (rest form)))
                          (expanded (preprocessor-macroexpand-1
                                     slightly-uncrossed)))
                     (if (eq expanded slightly-uncrossed)
                         ;; (Now that we're no longer processing toplevel
                         ;; forms, and hence no longer need to worry about
                         ;; EVAL-WHEN, we can uncross everything.)
                         (convert-and-maybe-compile expanded path)
                         ;; (We have to demote COMPILE-TIME-TOO to NIL
                         ;; here, no matter what it was before, since
                         ;; otherwise we'd tend to EVAL subforms more than
                         ;; once, because of WHEN COMPILE-TIME-TOO form
                         ;; above.)
                         (process-toplevel-form expanded path nil))))
                 ;; When we're not cross-compiling, we only need to
                 ;; macroexpand once, so we can follow the 1-thru-6
                 ;; sequence of steps in ANSI's "3.2.3.1 Processing of
                 ;; Top Level Forms".
                 #-sb-xc-host
                 (let ((expanded (preprocessor-macroexpand-1 form)))
                   (cond ((eq expanded form)
                          (when compile-time-too
                            (eval-compile-toplevel (list form) path))
                          (convert-and-maybe-compile form path nil))
                         (t
                          (process-toplevel-form expanded
                                                 path
                                                 compile-time-too)))))))
        (if (atom form)
            #+sb-xc-host
            ;; (There are no xc EVAL-WHEN issues in the ATOM case until
            ;; (1) SBCL gets smart enough to handle global
            ;; DEFINE-SYMBOL-MACRO or SYMBOL-MACROLET and (2) SBCL
            ;; implementors start using symbol macros in a way which
            ;; interacts with SB-XC/CL distinction.)
            (convert-and-maybe-compile form path)
            #-sb-xc-host
            (default-processor form)
            (flet ((need-at-least-one-arg (form)
                     (unless (cdr form)
                       (compiler-error "~S form is too short: ~S"
                                       (car form)
                                       form))))
              (case (car form)
                ((eval-when macrolet symbol-macrolet);things w/ 1 arg before body
                 (need-at-least-one-arg form)
                 (destructuring-bind (special-operator magic &rest body) form
                   (ecase special-operator
                     ((eval-when)
                      ;; CT, LT, and E here are as in Figure 3-7 of ANSI
                      ;; "3.2.3.1 Processing of Top Level Forms".
                      (multiple-value-bind (ct lt e)
                          (parse-eval-when-situations magic)
                        (let ((new-compile-time-too (or ct
                                                        (and compile-time-too
                                                             e))))
                          (cond (lt (process-toplevel-progn
                                     body path new-compile-time-too))
                                (new-compile-time-too
                                 (eval-compile-toplevel body path))))))
                     ((macrolet)
                      (funcall-in-macrolet-lexenv
                       magic
                       (lambda (&key funs prepend)
                         (declare (ignore funs))
                         (aver (null prepend))
                         (process-toplevel-locally body
                                                   path
                                                   compile-time-too))
                       :compile))
                     ((symbol-macrolet)
                      (funcall-in-symbol-macrolet-lexenv
                       magic
                       (lambda (&key vars prepend)
                         (aver (null prepend))
                         (process-toplevel-locally body
                                                   path
                                                   compile-time-too
                                                   :vars vars))
                       :compile)))))
                ((locally)
                 (process-toplevel-locally (rest form) path compile-time-too))
                ((progn)
                 (process-toplevel-progn (rest form) path compile-time-too))
                (t (default-processor form))))))))

  (values))

;;;; load time value support
;;;;
;;;; (See EMIT-MAKE-LOAD-FORM.)

;;; Return T if we are currently producing a fasl file and hence
;;; constants need to be dumped carefully.
(defun producing-fasl-file ()
  (fasl-output-p *compile-object*))

;;; Compile FORM and arrange for it to be called at load-time. Return
;;; the dumper handle and our best guess at the type of the object.
;;; It would be nice if L-T-V forms were generally eligible
;;; for fopcompilation, as it could eliminate special cases below.
(defun compile-load-time-value (form)
  (let ((ctype
         (cond
          ;; Ideally any ltv would test FOPCOMPILABLE-P on its form,
          ;; but be that as it may, this case is picked off because of
          ;; its importance during cross-compilation to ensure that
          ;; compiled lambdas don't cause a chicken-and-egg problem.
          ((typep form '(cons (eql find-package) (cons string null)))
           (specifier-type 'package))
          #+sb-xc-host
          ((typep form '(cons (eql find-classoid-cell)
                              (cons (cons (eql quote)))))
           (aver (eq (getf (cddr form) :create) t))
           (specifier-type 'sb!kernel::classoid-cell))
          ;; Special case for the cross-compiler, necessary for at least
          ;; SETUP-PRINTER-STATE, but also anything that would be dumped
          ;; using FOP-KNOWN-FUN in the target compiler, to avoid going
          ;; through an fdefn.
          ;; I'm pretty sure that as of change 00298ec6, it works to
          ;; compile #'F before the defun would have been seen by Genesis.
          #+sb-xc-host
          ((typep form '(cons (eql function) (cons symbol null)))
           (specifier-type 'function)))))
    (when ctype
      (fopcompile form nil t)
      (return-from compile-load-time-value
        (values (sb!fasl::dump-pop *compile-object*) ctype))))
  (let ((lambda (compile-load-time-stuff form t)))
    (values
     (fasl-dump-load-time-value-lambda lambda *compile-object*)
     (let ((type (leaf-type lambda)))
       (if (fun-type-p type)
           (single-value-type (fun-type-returns type))
           *wild-type*)))))

;;; Compile the FORMS and arrange for them to be called (for effect,
;;; not value) at load time.
(defun compile-make-load-form-init-forms (forms)
  (let ((lambda (compile-load-time-stuff `(progn ,@forms) nil)))
    (fasl-dump-toplevel-lambda-call lambda *compile-object*)))

;;; Do the actual work of COMPILE-LOAD-TIME-VALUE or
;;; COMPILE-MAKE-LOAD-FORM-INIT-FORMS.
(defun compile-load-time-stuff (form for-value)
  (with-ir1-namespace
   (let* ((*lexenv* (make-null-lexenv))
          (lambda (ir1-toplevel form *current-path* for-value nil)))
     (compile-toplevel (list lambda) t)
     lambda)))

;;; This is called by COMPILE-TOPLEVEL when it was passed T for
;;; LOAD-TIME-VALUE-P (which happens in COMPILE-LOAD-TIME-STUFF). We
;;; don't try to combine this component with anything else and frob
;;; the name. If not in a :TOPLEVEL component, then don't bother
;;; compiling, because it was merged with a run-time component.
(defun compile-load-time-value-lambda (lambdas)
  (aver (null (cdr lambdas)))
  (let* ((lambda (car lambdas))
         (component (lambda-component lambda)))
    (when (eql (component-kind component) :toplevel)
      (setf (component-name component) (leaf-debug-name lambda))
      (compile-component component)
      (clear-ir1-info component))))

;;;; COMPILE-FILE

(defun object-call-toplevel-lambda (tll)
  (declare (type functional tll))
  (let ((object *compile-object*))
    (etypecase object
      (fasl-output (fasl-dump-toplevel-lambda-call tll object))
      (core-object (core-call-toplevel-lambda      tll object))
      (null))))

;;; Smash LAMBDAS into a single component, compile it, and arrange for
;;; the resulting function to be called.
(defun sub-compile-toplevel-lambdas (lambdas)
  (declare (list lambdas))
  (when lambdas
    (multiple-value-bind (component tll) (merge-toplevel-lambdas lambdas)
      (compile-component component)
      (clear-ir1-info component)
      (object-call-toplevel-lambda tll)))
  (values))

;;; Compile top level code and call the top level lambdas. We pick off
;;; top level lambdas in non-top-level components here, calling
;;; SUB-c-t-l-l on each subsequence of normal top level lambdas.
(defun compile-toplevel-lambdas (lambdas)
  (declare (list lambdas))
  (let ((len (length lambdas)))
    (flet ((loser (start)
             (or (position-if (lambda (x)
                                (not (eq (component-kind
                                          (node-component (lambda-bind x)))
                                         :toplevel)))
                              lambdas
                              ;; this used to read ":start start", but
                              ;; start can be greater than len, which
                              ;; is an error according to ANSI - CSR,
                              ;; 2002-04-25
                              :start (min start len))
                 len)))
      (do* ((start 0 (1+ loser))
            (loser (loser start) (loser start)))
           ((>= start len))
        (sub-compile-toplevel-lambdas (subseq lambdas start loser))
        (unless (= loser len)
          (object-call-toplevel-lambda (elt lambdas loser))))))
  (values))

;;; Compile LAMBDAS (a list of CLAMBDAs for top level forms) into the
;;; object file.
;;;
;;; LOAD-TIME-VALUE-P seems to control whether it's MAKE-LOAD-FORM and
;;; COMPILE-LOAD-TIME-VALUE stuff. -- WHN 20000201
(defun compile-toplevel (lambdas load-time-value-p)
  (declare (list lambdas))

  (maybe-mumble "locall ")
  (locall-analyze-clambdas-until-done lambdas)

  (maybe-mumble "IDFO ")
  (multiple-value-bind (components top-components hairy-top)
      (find-initial-dfo lambdas)
    (let ((all-components (append components top-components)))
      (when *check-consistency*
        (maybe-mumble "[check]~%")
        (check-ir1-consistency all-components))

      (dolist (component (append hairy-top top-components))
        (pre-physenv-analyze-toplevel component))

      (dolist (component components)
        (compile-component component)
        (replace-toplevel-xeps component))

      (when *check-consistency*
        (maybe-mumble "[check]~%")
        (check-ir1-consistency all-components))

      (if load-time-value-p
          (compile-load-time-value-lambda lambdas)
          (compile-toplevel-lambdas lambdas))

      (mapc #'clear-ir1-info components)))
  (values))

;;; Actually compile any stuff that has been queued up for block
;;; compilation.
(defun finish-block-compilation ()
  (when *block-compile*
    (when *compile-print*
      (compiler-mumble "~&; block compiling converted top level forms..."))
    (when *toplevel-lambdas*
      (compile-toplevel (nreverse *toplevel-lambdas*) nil)
      (setq *toplevel-lambdas* ()))
    (setq *block-compile* nil)
    (setq *entry-points* nil)))

(flet ((get-handled-conditions ()
         (let ((ctxt *compiler-error-context*))
           (lexenv-handled-conditions
            (etypecase ctxt
             (node (node-lexenv ctxt))
             (compiler-error-context
              (let ((lexenv (compiler-error-context-lexenv ctxt)))
                (aver lexenv)
                lexenv))
             ;; Is this right? I would think that if lexenv is null
             ;; we should look at *HANDLED-CONDITIONS*.
             (null *lexenv*)))))
       (handle-p (condition ctype)
         #+sb-xc-host (typep condition (type-specifier ctype))
         #-sb-xc-host (%%typep condition ctype)))
  (declare (inline handle-p))

  (defun handle-condition-p (condition)
    (dolist (muffle (get-handled-conditions) nil)
      (destructuring-bind (ctype . restart-name) muffle
        (when (and (handle-p condition ctype)
                   (find-restart restart-name condition))
          (return t)))))

  (defun handle-condition-handler (condition)
    (let ((muffles (get-handled-conditions)))
      (aver muffles) ; FIXME: looks redundant with "fell through"
      (dolist (muffle muffles (bug "fell through"))
        (destructuring-bind (ctype . restart-name) muffle
          (when (handle-p condition ctype)
            (awhen (find-restart restart-name condition)
              (invoke-restart it)))))))

  ;; WOULD-MUFFLE-P is called (incorrectly) only by NOTE-UNDEFINED-REFERENCE.
  ;; It is not wrong per se, but as used, it is wrong, making it nearly
  ;; impossible to muffle a subset of undefind warnings whose NAME and KIND
  ;; slots match specific things tested by a user-defined predicate.
  ;; Attempting to do that might muffle everything, depending on how your
  ;; predicate responds to a vanilla WARNING. Consider e.g.
  ;;   (AND WARNING (NOT (SATISFIES HAIRYFN)))
  ;; where HAIRYFN depends on the :FORMAT-CONTROL and :FORMAT-ARGUMENTS.
  (defun would-muffle-p (condition)
    (let ((ctype (rassoc 'muffle-warning
                         (lexenv-handled-conditions *lexenv*))))
      (and ctype (handle-p condition (car ctype))))))

;;; Read all forms from INFO and compile them, with output to OBJECT.
;;; Return (VALUES ABORT-P WARNINGS-P FAILURE-P).
(defun sub-compile-file (info)
  (declare (type source-info info))
  (let ((*package* (sane-package))
        (*readtable* *readtable*)
        (sb!xc:*compile-file-pathname* nil) ; really bound in
        (sb!xc:*compile-file-truename* nil) ; SUB-SUB-COMPILE-FILE
        (*policy* *policy*)
        (*macro-policy* *macro-policy*)
        (*code-coverage-records* (make-hash-table :test 'equal))
        (*code-coverage-blocks* (make-hash-table :test 'equal))
        (*handled-conditions* *handled-conditions*)
        (*disabled-package-locks* *disabled-package-locks*)
        (*lexenv* (make-null-lexenv))
        (*block-compile* *block-compile-arg*)
        (*toplevel-lambdas* ())
        (*fun-names-in-this-file* ())
        (*allow-instrumenting* nil)
        (*compiler-error-bailout*
         (lambda (&optional error)
           (declare (ignore error))
           (return-from sub-compile-file (values t t t))))
        (*current-path* nil)
        (*last-source-context* nil)
        (*last-original-source* nil)
        (*last-source-form* nil)
        (*last-format-string* nil)
        (*last-format-args* nil)
        (*last-message-count* 0)
        (*compiler-sset-counter* 0)
        (sb!xc:*gensym-counter* 0))
    (handler-case
        (handler-bind (((satisfies handle-condition-p) #'handle-condition-handler))
          (with-compilation-values
            (sb!xc:with-compilation-unit ()
              (with-world-lock ()
                (sub-sub-compile-file info)
                (unless (zerop (hash-table-count *code-coverage-records*))
                  ;; Dump the code coverage records into the fasl.
                  (with-source-paths
                    (fopcompile `(record-code-coverage
                                  ',(namestring *compile-file-pathname*)
                                  ',(let (list)
                                      (maphash (lambda (k v)
                                                 (declare (ignore k))
                                                 (push v list))
                                               *code-coverage-records*)
                                      list))
                                nil
                                nil)))
                (finish-block-compilation)
                (let ((object *compile-object*))
                  (etypecase object
                    (fasl-output (fasl-dump-source-info info object))
                    (core-object (fix-core-source-info info object))
                    (null)))
                nil))))
      ;; Some errors are sufficiently bewildering that we just fail
      ;; immediately, without trying to recover and compile more of
      ;; the input file.
      (fatal-compiler-error (condition)
       (signal condition)
       (fresh-line *error-output*)
       (pprint-logical-block (*error-output* nil :per-line-prefix "; ")
         (format *error-output*
                 "~@<~@:_compilation aborted because of fatal error: ~2I~_~A~@:_~:>"
                 (encapsulated-condition condition)))
       (finish-output *error-output*)
       (values t t t)))))

;;; Return a pathname for the named file. The file must exist.
(defun verify-source-file (pathname-designator)
  (let* ((pathname (pathname pathname-designator))
         (default-host (make-pathname :host (pathname-host pathname))))
    (flet ((try-with-type (path type error-p)
             (let ((new (merge-pathnames
                         path (make-pathname :type type
                                             :defaults default-host))))
               (if (probe-file new)
                   new
                   (and error-p (truename new))))))
      (cond ((typep pathname 'logical-pathname)
             (try-with-type pathname "LISP" t))
            ((probe-file pathname) pathname)
            ((try-with-type pathname "lisp"  nil))
            ((try-with-type pathname "lisp"  t))))))

(defun elapsed-time-to-string (internal-time-delta)
  (multiple-value-bind (tsec remainder)
      (truncate internal-time-delta internal-time-units-per-second)
    (let ((ms (truncate remainder (/ internal-time-units-per-second 1000))))
      (multiple-value-bind (tmin sec) (truncate tsec 60)
        (multiple-value-bind (thr min) (truncate tmin 60)
          (format nil "~D:~2,'0D:~2,'0D.~3,'0D" thr min sec ms))))))

;;; Print some junk at the beginning and end of compilation.
(defun print-compile-start-note (source-info)
  (declare (type source-info source-info))
  (let ((file-info (source-info-file-info source-info)))
    (compiler-mumble #+sb-xc-host "~&; ~A file ~S (written ~A):~%"
                     #+sb-xc-host (if sb-cold::*compile-for-effect-only*
                                      "preloading"
                                      "cross-compiling")
                     #-sb-xc-host "~&; compiling file ~S (written ~A):~%"
                     (namestring (file-info-name file-info))
                     (sb!int:format-universal-time nil
                                                   (file-info-write-date
                                                    file-info)
                                                   :style :government
                                                   :print-weekday nil
                                                   :print-timezone nil)))
  (values))

(defun print-compile-end-note (source-info won)
  (declare (type source-info source-info))
  (compiler-mumble "~&; compilation ~:[aborted after~;finished in~] ~A~&"
                   won
                   (elapsed-time-to-string
                    (- (get-internal-real-time)
                       (source-info-start-real-time source-info))))
  (values))

;;; Open some files and call SUB-COMPILE-FILE. If something unwinds
;;; out of the compile, then abort the writing of the output file, so
;;; that we don't overwrite it with known garbage.
(defun sb!xc:compile-file
    (input-file
     &key

     ;; ANSI options
     (output-file (cfp-output-file-default input-file))
     ;; FIXME: ANSI doesn't seem to say anything about
     ;; *COMPILE-VERBOSE* and *COMPILE-PRINT* being rebound by this
     ;; function..
     ((:verbose sb!xc:*compile-verbose*) sb!xc:*compile-verbose*)
     ((:print sb!xc:*compile-print*) sb!xc:*compile-print*)
     (external-format :default)

     ;; extensions
     (trace-file nil)
     ((:block-compile *block-compile-arg*) nil)
     (emit-cfasl *emit-cfasl*))
  #!+sb-doc
  "Compile INPUT-FILE, producing a corresponding fasl file and
returning its filename.

  :PRINT
     If true, a message per non-macroexpanded top level form is printed
     to *STANDARD-OUTPUT*. Top level forms that whose subforms are
     processed as top level forms (eg. EVAL-WHEN, MACROLET, PROGN) receive
     no such message, but their subforms do.

     As an extension to ANSI, if :PRINT is :top-level-forms, a message
     per top level form after macroexpansion is printed to *STANDARD-OUTPUT*.
     For example, compiling an IN-PACKAGE form will result in a message about
     a top level SETQ in addition to the message about the IN-PACKAGE form'
     itself.

     Both forms of reporting obey the SB-EXT:*COMPILER-PRINT-VARIABLE-ALIST*.

  :BLOCK-COMPILE
     Though COMPILE-FILE accepts an additional :BLOCK-COMPILE
     argument, it is not currently supported. (non-standard)

  :TRACE-FILE
     If given, internal data structures are dumped to the specified
     file, or if a value of T is given, to a file of *.trace type
     derived from the input file name. (non-standard)

  :EMIT-CFASL
     (Experimental). If true, outputs the toplevel compile-time effects
     of this file into a separate .cfasl file."
;;; Block compilation is currently broken.
#|
  "Also, as a workaround for vaguely-non-ANSI behavior, the
:BLOCK-COMPILE argument is quasi-supported, to determine whether
multiple functions are compiled together as a unit, resolving function
references at compile time. NIL means that global function names are
never resolved at compilation time. Currently NIL is the default
behavior, because although section 3.2.2.3, \"Semantic Constraints\",
of the ANSI spec allows this behavior under all circumstances, the
compiler's runtime scales badly when it tries to do this for large
files. If/when this performance problem is fixed, the block
compilation default behavior will probably be made dependent on the
SPEED and COMPILATION-SPEED optimization values, and the
:BLOCK-COMPILE argument will probably become deprecated."
|#
  (let* ((fasl-output nil)
         (cfasl-output nil)
         (output-file-name nil)
         (coutput-file-name nil)
         (abort-p t)
         (warnings-p nil)
         (failure-p t) ; T in case error keeps this from being set later
         (input-pathname (verify-source-file input-file))
         (source-info
          (make-file-source-info input-pathname external-format
                                 #-sb-xc-host t)) ; can't track, no SBCL streams
         (*compiler-trace-output* nil)) ; might be modified below

    (unwind-protect
        (progn
          (when output-file
            (setq output-file-name
                  (sb!xc:compile-file-pathname input-file
                                               :output-file output-file))
            (setq fasl-output
                  (open-fasl-output output-file-name
                                    (namestring input-pathname))))
          (when emit-cfasl
            (setq coutput-file-name
                  (make-pathname :type "cfasl"
                                 :defaults output-file-name))
            (setq cfasl-output
                  (open-fasl-output coutput-file-name
                                    (namestring input-pathname))))
          (when trace-file
            (let* ((default-trace-file-pathname
                     (make-pathname :type "trace" :defaults input-pathname))
                   (trace-file-pathname
                    (if (eql trace-file t)
                        default-trace-file-pathname
                        (merge-pathnames trace-file
                                         default-trace-file-pathname))))
              (setf *compiler-trace-output*
                    (open trace-file-pathname
                          :if-exists :supersede
                          :direction :output))))

          (when sb!xc:*compile-verbose*
            (print-compile-start-note source-info))

          (let ((*compile-object* fasl-output)
                (*compile-toplevel-object* cfasl-output))
            (setf (values abort-p warnings-p failure-p)
                  (sub-compile-file source-info))))

      (close-source-info source-info)

      (when fasl-output
        (close-fasl-output fasl-output abort-p)
        (setq output-file-name
              (pathname (fasl-output-stream fasl-output)))
        (when (and (not abort-p) sb!xc:*compile-verbose*)
          (compiler-mumble "~2&; ~A written~%" (namestring output-file-name))))

      (when cfasl-output
        (close-fasl-output cfasl-output abort-p)
        (when (and (not abort-p) sb!xc:*compile-verbose*)
          (compiler-mumble "; ~A written~%" (namestring coutput-file-name))))

      (when sb!xc:*compile-verbose*
        (print-compile-end-note source-info (not abort-p)))

      (when *compiler-trace-output*
        (close *compiler-trace-output*)))

    ;; CLHS says that the first value is NIL if the "file could not
    ;; be created". We interpret this to mean "a valid fasl could not
    ;; be created" -- which can happen if the compilation is aborted
    ;; before the whole file has been processed, due to eg. a reader
    ;; error.
    (values (when (and (not abort-p) output-file)
              ;; Hack around filesystem race condition...
              (or (probe-file output-file-name) output-file-name))
            warnings-p
            failure-p)))

;;; a helper function for COMPILE-FILE-PATHNAME: the default for
;;; the OUTPUT-FILE argument
;;;
;;; ANSI: The defaults for the OUTPUT-FILE are taken from the pathname
;;; that results from merging the INPUT-FILE with the value of
;;; *DEFAULT-PATHNAME-DEFAULTS*, except that the type component should
;;; default to the appropriate implementation-defined default type for
;;; compiled files.
(defun cfp-output-file-default (input-file)
  (let* ((defaults (merge-pathnames input-file *default-pathname-defaults*))
         (retyped (make-pathname :type *fasl-file-type* :defaults defaults)))
    retyped))

;;; KLUDGE: Part of the ANSI spec for this seems contradictory:
;;;   If INPUT-FILE is a logical pathname and OUTPUT-FILE is unsupplied,
;;;   the result is a logical pathname. If INPUT-FILE is a logical
;;;   pathname, it is translated into a physical pathname as if by
;;;   calling TRANSLATE-LOGICAL-PATHNAME.
;;; So I haven't really tried to make this precisely ANSI-compatible
;;; at the level of e.g. whether it returns logical pathname or a
;;; physical pathname. Patches to make it more correct are welcome.
;;; -- WHN 2000-12-09
(defun sb!xc:compile-file-pathname (input-file
                                    &key
                                    (output-file nil output-file-p)
                                    &allow-other-keys)
  #!+sb-doc
  "Return a pathname describing what file COMPILE-FILE would write to given
   these arguments."
  (if output-file-p
      (merge-pathnames output-file (cfp-output-file-default input-file))
      (cfp-output-file-default input-file)))

;;;; MAKE-LOAD-FORM stuff

;;; The entry point for MAKE-LOAD-FORM support. When IR1 conversion
;;; finds a constant structure, it invokes this to arrange for proper
;;; dumping. If it turns out that the constant has already been
;;; dumped, then we don't need to do anything.
;;;
;;; If the constant hasn't been dumped, then we check to see whether
;;; we are in the process of creating it. We detect this by
;;; maintaining the special *CONSTANTS-BEING-CREATED* as a list of all
;;; the constants we are in the process of creating. Actually, each
;;; entry is a list of the constant and any init forms that need to be
;;; processed on behalf of that constant.
;;;
;;; It's not necessarily an error for this to happen. If we are
;;; processing the init form for some object that showed up *after*
;;; the original reference to this constant, then we just need to
;;; defer the processing of that init form. To detect this, we
;;; maintain *CONSTANTS-CREATED-SINCE-LAST-INIT* as a list of the
;;; constants created since the last time we started processing an
;;; init form. If the constant passed to emit-make-load-form shows up
;;; in this list, then there is a circular chain through creation
;;; forms, which is an error.
;;;
;;; If there is some intervening init form, then we blow out of
;;; processing it by throwing to the tag PENDING-INIT. The value we
;;; throw is the entry from *CONSTANTS-BEING-CREATED*. This is so the
;;; offending init form can be tacked onto the init forms for the
;;; circular object.
;;;
;;; If the constant doesn't show up in *CONSTANTS-BEING-CREATED*, then
;;; we have to create it. We call MAKE-LOAD-FORM and check to see
;;; whether the creation form is the magic value
;;; :SB-JUST-DUMP-IT-NORMALLY. If it is, then we don't do anything. The
;;; dumper will eventually get its hands on the object and use the
;;; normal structure dumping noise on it.
;;;
;;; Otherwise, we bind *CONSTANTS-BEING-CREATED* and
;;; *CONSTANTS-CREATED-SINCE- LAST-INIT* and compile the creation form
;;; much the way LOAD-TIME-VALUE does. When this finishes, we tell the
;;; dumper to use that result instead whenever it sees this constant.
;;;
;;; Now we try to compile the init form. We bind
;;; *CONSTANTS-CREATED-SINCE-LAST-INIT* to NIL and compile the init
;;; form (and any init forms that were added because of circularity
;;; detection). If this works, great. If not, we add the init forms to
;;; the init forms for the object that caused the problems and let it
;;; deal with it.
(defvar *constants-being-created* nil)
(defvar *constants-created-since-last-init* nil)
;;; FIXME: Shouldn't these^ variables be unbound outside LET forms?
(defun emit-make-load-form (constant &optional (name nil namep)
                                     &aux (fasl *compile-object*))
  (aver (fasl-output-p *compile-object*))
  (unless (or (fasl-constant-already-dumped-p constant fasl)
              ;; KLUDGE: This special hack is because I was too lazy
              ;; to rework DEF!STRUCT so that the MAKE-LOAD-FORM
              ;; function of LAYOUT returns nontrivial forms when
              ;; building the cross-compiler but :IGNORE-IT when
              ;; cross-compiling or running under the target Lisp. --
              ;; WHN 19990914
              #+sb-xc-host (typep constant 'layout))
    (let ((circular-ref (assoc constant *constants-being-created* :test #'eq)))
      (when circular-ref
        (when (find constant *constants-created-since-last-init* :test #'eq)
          (throw constant t))
        (throw 'pending-init circular-ref)))
    (multiple-value-bind (creation-form init-form)
        (if namep
            ;; If the constant is a reference to a named constant, we can
            ;; just use SYMBOL-VALUE during LOAD.
            (values `(symbol-value ',name) nil)
            (handler-case
                (sb!xc:make-load-form constant (make-null-lexenv))
              (error (condition)
                (compiler-error condition))))
      #-sb-xc-host
      (when (and (not namep)
                 (listp creation-form) ; skip if already a magic keyword
                 (typep constant 'structure-object)
                 (sb!kernel::canonical-slot-saving-forms-p
                  constant creation-form init-form))
        (setq creation-form :sb-just-dump-it-normally))
      (case creation-form
        (:sb-just-dump-it-normally
         (fasl-validate-structure constant fasl)
         t)
        (:ignore-it
         nil)
        (t
         (let* ((name (write-to-string constant :level 1 :length 2))
                (info (if init-form
                          (list constant name init-form)
                          (list constant))))
           (let ((*constants-being-created*
                  (cons info *constants-being-created*))
                 (*constants-created-since-last-init*
                  (cons constant *constants-created-since-last-init*)))
             (when
                 (catch constant
                   (fasl-note-handle-for-constant
                    constant
                    (or (fopcompile-allocate-instance fasl creation-form)
                        (compile-load-time-value creation-form))
                    fasl)
                   nil)
               (compiler-error "circular references in creation form for ~S"
                               constant)))
           (when (cdr info)
             (let* ((*constants-created-since-last-init* nil)
                    (circular-ref
                     (catch 'pending-init
                       (loop for (name form) on (cdr info) by #'cddr
                         collect name into names
                         collect form into forms
                         finally (or (fopcompile-constant-init-forms fasl forms)
                                     (compile-make-load-form-init-forms forms)))
                       nil)))
               (when circular-ref
                 (setf (cdr circular-ref)
                       (append (cdr circular-ref) (cdr info)))))))
         nil)))))


;;;; Host compile time definitions
#+sb-xc-host
(defun compile-in-lexenv (name lambda lexenv)
  (declare (ignore lexenv))
  (compile name lambda))

#+sb-xc-host
(defun eval-tlf (form index &optional lexenv)
  (declare (ignore index lexenv))
  (eval form))
