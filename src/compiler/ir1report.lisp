;;;; machinery for reporting errors/warnings/notes/whatnot from
;;;; the compiler

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;;; compiler error context determination

(declaim (special *current-path*))

(defvar *enclosing-source-cutoff* 1
  "The maximum number of enclosing non-original source forms (i.e. from
  macroexpansion) that we print in full. For additional enclosing forms, we
  print only the CAR.")
(declaim (type unsigned-byte *enclosing-source-cutoff*))

;;; We separate the determination of compiler error contexts from the
;;; actual signalling of those errors by objectifying the error
;;; context. This allows postponement of the determination of how (and
;;; if) to signal the error.
;;;
;;; We take care not to reference any of the IR1 so that pending
;;; potential error messages won't prevent the IR1 from being GC'd. To
;;; this end, we convert source forms to strings so that source forms
;;; that contain IR1 references (e.g. %DEFUN) don't hold onto the IR.
(defstruct (compiler-error-context
            (:print-object (lambda (x stream)
                             (print-unreadable-object (x stream :type t))))
            (:copier nil))
  ;; a list of the stringified CARs of the enclosing non-original source forms
  ;; exceeding the *enclosing-source-cutoff*
  (%enclosing-source nil :type list)
  ;; a list of stringified enclosing non-original source forms
  (%source nil :type list)
  (original-form (missing-arg))
  (original-form-string nil)
  ;; a list of prefixes of "interesting" forms that enclose original-source
  (context nil :type list)
  ;; the FILE-INFO-NAME for the relevant FILE-INFO
  (file-name (missing-arg) :type (or pathname (member :lisp :stream)))
  ;; the file position at which the top level form starts, if applicable
  (file-position nil :type (or index null))
  path
  format-args
  initialized
  ;; the original source part of the source path
  (original-source-path nil :type list)
  ;; lexenv-handled-conditions of the lexenv active at the time
  (handled-conditions nil))
(declaim (freeze-type compiler-error-context))

;;; Delay computing some source information, since it may not actually be ever used
(defun compiler-error-context-original-source (context)
  (let ((source (compiler-error-context-original-form-string context)))
    (if (stringp source)
        source
        (setf (compiler-error-context-original-form-string context)
              (stringify-form (compiler-error-context-original-form context))))))

(defun compiler-error-context-source (context)
  (setup-compiler-error-context context)
  (compiler-error-context-%source context))

(defun compiler-error-context-enclosing-source (context)
  (setup-compiler-error-context context)
  (compiler-error-context-%enclosing-source context))

(defun setup-compiler-error-context (context)
  (unless (compiler-error-context-initialized context)
    (setf (compiler-error-context-initialized context) t)
    (let ((path (compiler-error-context-path context))
          (args (compiler-error-context-format-args context)))
      (collect ((full nil cons)
                (short nil cons))
        (flet ((no-transforms (x)
                 (let ((pos (position 'transformed x :from-end t)))
                   (if pos
                       (subseq x (1+ pos))
                       x)))
               (hide-inlines (x)
                 (loop for elt = (pop x)
                       while x
                       if (eq elt 'inlined)
                       do (pop x)
                       else
                       collect elt)))
          (let* ((forms (source-path-forms path))
                 (n 0)
                 (forms (if (member (first forms) args)
                            (rest forms)
                            forms))
                 (forms (hide-inlines (no-transforms forms))))
            (dolist (src forms)
              (if (>= n *enclosing-source-cutoff*)
                  (short (stringify-form (if (consp src)
                                             (car src)
                                             src)
                                         nil))
                  (full (stringify-form src)))
              (incf n))
            (setf (compiler-error-context-%enclosing-source context) (short)
                  (compiler-error-context-%source context) (full))))))))

;;; If true, this is the node which is used as context in compiler warning
;;; messages.
(declaim (type (or null compiler-error-context node
                   lvar-annotation ctran) *compiler-error-context*))
(defvar *compiler-error-context* nil)

;;; a plist mapping macro names to source context parsers. Each parser
;;; function returns the source-context list for that form.
(defglobal *source-context-methods* nil)

;;; documentation originally from cmu-user.tex:
;;;   This macro defines how to extract an abbreviated source context from
;;;   the \var{name}d form when it appears in the compiler input.
;;;   \var{lambda-list} is a \code{defmacro} style lambda-list used to
;;;   parse the arguments. The \var{body} should return a list of
;;;   subforms that can be printed on about one line. There are
;;;   predefined methods for \code{defstruct}, \code{defmethod}, etc. If
;;;   no method is defined, then the first two subforms are returned.
;;;   Note that this facility implicitly determines the string name
;;;   associated with anonymous functions.
;;; So even though SBCL itself only uses this macro within this file,
;;; it's a reasonable thing to put in SB-EXT in case some dedicated
;;; user wants to do some heavy tweaking to make SBCL give more
;;; informative output about his code.
(defmacro define-source-context (name lambda-list &body body)
  "DEFINE-SOURCE-CONTEXT Name Lambda-List Form*
   This macro defines how to extract an abbreviated source context from the
   Named form when it appears in the compiler input. Lambda-List is a DEFMACRO
   style lambda-list used to parse the arguments. The Body should return a
   list of subforms suitable for a \"~{~S ~}\" format string."
  (with-unique-names (whole)
    `(setf (getf *source-context-methods* ',name)
           (lambda (,whole)
             (destructuring-bind ,lambda-list ,whole ,@body)))))

(define-source-context defstruct (name-or-options &rest slots)
  (declare (ignore slots))
  `(defstruct ,(if (consp name-or-options)
                   (car name-or-options)
                   name-or-options)))

(define-source-context function (thing)
  (if (and (consp thing) (eq (first thing) 'lambda) (consp (rest thing)))
      `(lambda ,(second thing))
      `(function ,thing)))

(define-source-context named-lambda (name lambda-list &body forms)
  (declare (ignore lambda-list forms))
  (if (and (consp name) (eq 'eval (first name)))
      (second name)
      `(named-lambda ,name)))

(defvar *source-form-context-alist* nil)

;;; Return the first two elements of FORM if FORM is a list. Take the
;;; CAR of the second form if appropriate.
(defun source-form-context (form)
  (flet ((get-it (form)
           (cond ((atom form) nil)
                 ((list-of-length-at-least-p form 2)
                  (let* ((context-fun-default
                           (lambda (x)
                             (declare (ignore x))
                             (list (first form) (second form))))
                         (context-fun
                           (getf *source-context-methods*
                                 (first form)
                                 context-fun-default)))
                    (declare (type function context-fun))
                    (funcall context-fun (rest form))))
                 (t
                  form))))
    (get-it (or (cdr (assoc form *source-form-context-alist* :test #'eq))
                form))))

;;; Given a source path, return the original source form and a
;;; description of the interesting aspects of the context in which it
;;; appeared. The context is a list of lists, one sublist per context
;;; form. The sublist is a list of some of the initial subforms of the
;;; context form.
;;;
;;; For now, we use the first two subforms of each interesting form. A
;;; form is interesting if the first element is a symbol beginning
;;; with "DEF" and it is not the source form. If there is no
;;; DEF-mumble, then we use the outermost containing form. If the
;;; second subform is a list, then in some cases we return the CAR of
;;; that form rather than the whole form (i.e. don't show DEFSTRUCT
;;; options, etc.)
(defun find-original-source (path)
  (declare (list path))
  (let* ((rpath (reverse (source-path-original-source path)))
         (tlf (first rpath))
         (root (find-source-root tlf *source-info*)))
    (collect ((context))
      (let ((form root)
            (current (rest rpath)))
        (loop
         (when (comma-p form)
           (setf form (comma-expr form)))
         (when (atom form)
            (aver (null current))
            (return))
          (let ((head (first form)))
            (when (symbolp head)
              (let ((name (symbol-name head)))
                (when (and (>= (length name) 3) (string= name "DEF" :end1 3))
                  (context (source-form-context form))))))
          (when (null current) (return))
          (setq form (nth (pop current) form)))

        (cond ((context)
               (values form (context)))
              ((and path root)
               (let ((c (source-form-context root)))
                 (values form (if c (list c) nil))))
              (t
               (values '(unable to locate source)
                       '((some strange place)))))))))

;;; Convert a source form to a string, suitably formatted for use in
;;; compiler warnings.
(defun stringify-form (form &optional (pretty t))
  (with-standard-io-syntax
    (with-compiler-io-syntax
      (let ((*print-pretty* pretty)
            (*print-ir-nodes-pretty* t))
        (if pretty
            (format nil "~<~@;  ~S~:>" (list form))
            (prin1-to-string form))))))

;;; Return a COMPILER-ERROR-CONTEXT structure describing the current
;;; error context, or NIL if we can't figure anything out. ARGS is a
;;; list of things that are going to be printed out in the error
;;; message, and can thus be blown off when they appear in the source
;;; context.
;;;
;;; If OLD-CONTEXTS is passed in, and includes a context with the
;;; same original source path as the new context would have, the old
;;; context is reused instead, and a secondary value of T is returned.
(defun find-error-context (args &optional old-contexts (old-contexts-key #'identity))
  (let ((context *compiler-error-context*))
    (if (compiler-error-context-p context)
        (values context t)
        (let* ((path (cond ((node-p context)
                            (node-source-path context))
                           ((lvar-annotation-p context)
                            (lvar-annotation-source-path context))
                           ((ctran-p context)
                            (ctran-source-path context))
                           ((boundp '*current-path*)
                            *current-path*)))
               (old
                (find (when path (source-path-original-source path))
                      old-contexts
                      :test #'equal
                      :key (lambda (x)
                             (and x
                                  (compiler-error-context-original-source-path
                                   (funcall old-contexts-key x)))))))
          (if old
              (values old t)
              (when (and *source-info* path)
                (let ((tlf (source-path-tlf-number path))
                      (file-info (source-info-file-info *source-info*)))
                  (multiple-value-bind (form src-context) (find-original-source path)
                    (values
                     (make-compiler-error-context
                      :original-form form
                      :format-args args
                      :context src-context
                      :file-name (if (symbolp (file-info-truename file-info)) ; :LISP or :STREAM
                                     ;; (pathname will be NIL in those two cases)
                                     (file-info-truename file-info)
                                     (file-info-pathname file-info))
                      :file-position
                      (nth-value 1 (find-source-root tlf *source-info*))
                      :path path
                      :original-source-path (source-path-original-source path)
                      :handled-conditions
                      (let ((lexenv (cond ((node-p context)
                                           (node-lexenv context))
                                          ((lvar-annotation-p context)
                                           (lvar-annotation-lexenv context))
                                          ((boundp '*lexenv*)
                                           *lexenv*))))
                        (and lexenv
                             (lexenv-handled-conditions lexenv))))
                     nil)))))))))

;;;; printing error messages

;;; We save the context information that we printed out most recently
;;; so that we don't print it out redundantly.

;;; The last COMPILER-ERROR-CONTEXT that we printed.
(defvar *last-error-context*)
(declaim (type (or compiler-error-context null) *last-error-context*))

;;; The format string and args for the last error we printed.
(define-symbol-macro *last-format-string*
  (the (or string null) (cadr *last-message-count*)))
(define-symbol-macro *last-format-args* (cddr *last-message-count*))

;;; The number of times that the last error message has been emitted,
;;; so that we can compress duplicate error messages.
(defvar *last-message-count*)
(declaim (type (cons index (cons (or string null) t)) *last-message-count*))

;;; If the last message was given more than once, then print out an
;;; indication of how many times it was repeated. We reset the message
;;; count when we are done.
(defun note-message-repeats (stream &optional (terpri t)
                                    &aux (count (car *last-message-count*)))
  (cond ((= count 1)
         (when terpri
           (terpri stream)))
        ((> count 1)
         (format stream "~&; [Last message occurs ~W times.]~2%"
                 count)))
  (setf (car *last-message-count*) 0))

;;; Print out the message, with appropriate context if we can find it.
;;; If the context is different from the context of the last message
;;; we printed, then we print the context. If the original source is
;;; different from the source we are working on, then we print the
;;; current source in addition to the original source.
;;;
;;; We suppress printing of messages identical to the previous, but
;;; record the number of times that the message is repeated.
(defun print-compiler-message (stream format-string format-args)
  (with-compiler-io-syntax
    (%print-compiler-message stream format-string format-args)))

(defun %print-compiler-message (stream format-string format-args)
  (declare (type simple-string format-string))
  (declare (type list format-args))
  (let ((context (find-error-context format-args)))
    (cond (context
           (let ((file (compiler-error-context-file-name context))
                 (in (compiler-error-context-context context))
                 (form (compiler-error-context-original-source context))
                 (enclosing (compiler-error-context-enclosing-source context))
                 (source (compiler-error-context-source context))
                 (last *last-error-context*))

             (unless  (and last
                           (equal file (compiler-error-context-file-name last)))
               (when (pathnamep file)
                 (note-message-repeats stream)
                 (setq last nil)
                 (format stream "~2&; file: ~A~%" (namestring file))))

             (unless (and last
                          (equal in (compiler-error-context-context last)))
               (note-message-repeats stream)
               (setq last nil)
               (pprint-logical-block (stream nil :per-line-prefix "; ")
                 (format stream "in:~{~<~%    ~4:;~{ ~:S~}~>~^ =>~}" in))
               (terpri stream))

             (unless (and last
                          (string= form
                                   (compiler-error-context-original-source last)))
               (note-message-repeats stream)
               (setq last nil)
               (pprint-logical-block (stream nil :per-line-prefix ";   ")
                 (princ form stream))
               (fresh-line stream))

             (unless (and last
                          (equal enclosing
                                 (compiler-error-context-enclosing-source last)))
               (when enclosing
                 (note-message-repeats stream)
                 (setq last nil)
                 (format stream "~&; --> ~{~<~%; --> ~1:;~A~> ~}~%" enclosing)))

             (unless (and last
                          (equal source (compiler-error-context-source last)))
               (setq *last-format-string* nil)
               (when source
                 (note-message-repeats stream)
                 (dolist (src source)
                   (fresh-line stream)
                   (write-string "; ==>" stream)
                   (terpri stream)
                   (pprint-logical-block (stream nil :per-line-prefix "; ")
                     (write-string src stream)))))))
          (t
           (fresh-line stream)
           (note-message-repeats stream)
           (setq *last-format-string* nil)))

    (setq *last-error-context* context))

  ;; FIXME: this testing for effective equality of compiler messages
  ;; is ugly, and really ought to be done at a higher level.
  (unless (and (equal format-string *last-format-string*)
               (tree-equal format-args *last-format-args*))
    (note-message-repeats stream nil)
    (setq *last-format-string* format-string)
    (setq *last-format-args* format-args)
    (fresh-line stream)
    (pprint-logical-block (stream nil :per-line-prefix "; ")
      (format stream "~&~?" format-string format-args))
    (fresh-line stream))

  (incf (car *last-message-count*))
  (values))

(defun print-compiler-condition (condition)
  (declare (type condition condition))
  (let (;; These different classes of conditions have different
        ;; effects on the return codes of COMPILE-FILE, so it's nice
        ;; for users to be able to pick them out by lexical search
        ;; through the output.
        (what (etypecase condition
                (style-warning 'style-warning)
                (warning 'warning)
                ((or error compiler-error) 'error))))
    (print-compiler-message
     *error-output*
     (format nil "caught ~S:~%~~@<  ~~@;~~A~~:>" what)
     (list (princ-to-string condition)))))

;;; The act of signalling one of these beasts must not cause WARNINGSP
;;; (or FAILUREP) to be set from COMPILE or COMPILE-FILE, so we can't
;;; inherit from WARNING or STYLE-WARNING.
;;;
;;; FIXME: the handling of compiler-notes could be unified with
;;; warnings and style-warnings (see the various handler functions
;;; below).
(define-condition compiler-note (condition) ()
  (:documentation
   "Root of the hierarchy of conditions representing information discovered
by the compiler that the user might wish to know, but which does not merit
a STYLE-WARNING (or any more serious condition)."))
(define-condition simple-compiler-note (simple-condition compiler-note) ())
(define-condition code-deletion-note (simple-compiler-note) ()
  (:documentation
   "A condition type signalled when the compiler deletes code that the user
has written, having proved that it is unreachable."))
(define-condition unknown-typep-note (simple-compiler-note) ())

(define-condition compiler-macro-application-missed-warning
    (style-warning)
  ((count :initarg :count
          :reader compiler-macro-application-missed-warning-count)
   (function :initarg :function
             :reader compiler-macro-application-missed-warning-function))
  (:default-initargs
   :count (missing-arg)
    :function (missing-arg))
  (:report
   (lambda (condition stream)
     ;; Grammar note - starting a sentence with a numeral is wrong.
     (format stream
              "~@<~@(~D~) call~:P to ~
               ~/sb-ext:print-symbol-with-prefix/ ~
               ~2:*~[~;was~:;were~] compiled before a compiler-macro ~
               was defined for it. A declaration of NOTINLINE at the ~
               call site~:P will eliminate this warning, as will ~
               defining the compiler-macro before its first potential ~
               use.~@:>"
             (compiler-macro-application-missed-warning-count condition)
             (compiler-macro-application-missed-warning-function condition)))))

(macrolet ((with-condition ((condition datum args) &body body)
             (with-unique-names (block)
               `(block ,block
                  (let ((,condition
                          (apply #'coerce-to-condition ,datum
                                 'simple-compiler-note 'with-condition
                                 ,args)))
                    (restart-case
                        (signal ,condition)
                      (muffle-warning ()
                        (return-from ,block (values))))
                    ,@body
                    (values))))))

  (defun compiler-notify (datum &rest args)
    (unless (if *compiler-error-context*
                (policy (if (ctran-p *compiler-error-context*)
                            (ctran-next *compiler-error-context*)
                            *compiler-error-context*)
                    (= inhibit-warnings 3))
                (policy *lexenv* (= inhibit-warnings 3)))
      (with-condition (condition datum args)
        (incf *compiler-note-count*)
        (print-compiler-message
         *error-output*
         (format nil "note: ~~A")
         (list (princ-to-string condition)))))
    (values))

  ;; Issue a note when we might or might not be in the compiler.
  (defun maybe-compiler-notify (datum &rest args)
    (if (boundp '*lexenv*) ; if we're in the compiler
        (apply #'compiler-notify datum args)
        (with-condition (condition datum args)
          (let ((stream *error-output*))
            (pprint-logical-block (stream nil :per-line-prefix ";")
              (format stream " note: ~3I~_")
              (pprint-logical-block (stream nil)
                (format stream "~A" condition)))
            ;; (outside logical block, no per-line-prefix)
            (fresh-line stream))))))

;;; The politically correct way to print out progress messages and
;;; such like. We clear the current error context so that we know that
;;; it needs to be reprinted, and we also FORCE-OUTPUT so that the
;;; message gets seen right away.
(defun compiler-mumble (control &rest args)
  (let ((stream *standard-output*))
    (note-message-repeats stream)
    (setq *last-error-context* nil)
    (apply #'format stream control args)
    (force-output stream)
    (values)))

;;; Return a string that somehow names the code in COMPONENT. We use
;;; the source path for the bind node for an arbitrary entry point to
;;; find the source context, then return that as a string.
(declaim (ftype (function (component) simple-string) find-component-name))
(defun find-component-name (component)
  (let ((ep (first (block-succ (component-head component)))))
    (aver ep) ; else no entry points??
    (multiple-value-bind (form context)
        (find-original-source (node-source-path (block-start-node ep)))
      (declare (ignore form))
      (let ((*print-level* 2)
            (*print-pretty* nil))
        ;; It's arbitrary how this name is stringified.
        ;; Using ~A in lieu of ~S prevents "SB-" strings from getting in.
        (format nil
                "~{~{~A~^ ~}~^ => ~}"
                #+sb-xc-host (list (list (caar context)))
                #-sb-xc-host context)))))

;;;; condition system interface

;;; Keep track of how many times each kind of condition happens.
(defvar *compiler-error-count*)
(defvar *compiler-warning-count*)
(defvar *compiler-style-warning-count*)
(defvar *compiler-note-count*)

(defvar *methods-in-compilation-unit*)

;;; Keep track of whether any surrounding COMPILE or COMPILE-FILE call
;;; should return WARNINGS-P or FAILURE-P.
(defvar *failure-p*)
(defvar *warnings-p*)

;;; condition handlers established by the compiler. We re-signal the
;;; condition, then if it isn't handled, we increment our warning
;;; counter and print the error message.
(defun compiler-error-handler (condition)
  (signal condition)
  (incf *compiler-error-count*)
  (setf *warnings-p* t
        *failure-p* t)
  (print-compiler-condition condition)
  (continue condition))
(defun compiler-warning-handler (condition)
  (signal condition)
  (incf *compiler-warning-count*)
  (setf *warnings-p* t
        *failure-p* t)
  (print-compiler-condition condition)
  (muffle-warning condition))
(defun compiler-style-warning-handler (condition)
  (signal condition)
  (incf *compiler-style-warning-count*)
  (setf *warnings-p* t)
  (print-compiler-condition condition)
  (muffle-warning condition))

;;;; undefined warnings

(defvar *undefined-warning-limit* 3
  "If non-null, then an upper limit on the number of unknown function or type
  warnings that the compiler will print for any given name in a single
  compilation. This prevents excessive amounts of output when the real
  problem is a missing definition (as opposed to a typo in the use.)")

;;; Make an entry in the *UNDEFINED-WARNINGS* describing a reference
;;; to NAME of the specified KIND. If we have exceeded the warning
;;; limit, then just increment the count, otherwise note the current
;;; error context.
;;;
;;; Undefined types are noted by a condition handler in
;;; WITH-COMPILATION-UNIT, which can potentially be invoked outside
;;; the compiler, hence the BOUNDP check.
(defun note-undefined-reference (name kind)
  #+sb-xc-host
  ;; Allowlist functions are looked up prior to UNCROSS,
  ;; so that we can distinguish CL:SOMEFUN from SB-XC:SOMEFUN.
  (when (and (eq kind :function)
             (gethash name sb-cold:*undefined-fun-allowlist*))
    (return-from note-undefined-reference (values)))
  (setq name (uncross name))
  (unless (and
           ;; Check for boundness so we don't blow up if we're called
           ;; when IR1 conversion isn't going on.
           (boundp '*lexenv*)
           (or
            ;; FIXME: I'm pretty sure the INHIBIT-WARNINGS test below
            ;; isn't a good idea; we should have INHIBIT-WARNINGS
            ;; affect compiler notes, not STYLE-WARNINGs. And I'm not
            ;; sure what the BOUNDP '*LEXENV* test above is for; it's
            ;; likely a good idea, but it probably deserves an
            ;; explanatory comment.
            (policy *lexenv* (= inhibit-warnings 3))
            ;; KLUDGE: weird decoupling between here and where we're
            ;; going to signal the condition.  I don't think we can
            ;; rewrite this using SIGNAL and RESTART-CASE (to take
            ;; advantage of the (SATISFIES HANDLE-CONDITION-P)
            ;; handler, because if that doesn't handle it the ordinary
            ;; compiler handlers will trigger.
            (would-muffle-p
             (ecase kind
               (:variable (make-condition 'warning))
               ((:function :type) (make-condition 'style-warning))))))
    (let* ((found (dolist (warning *undefined-warnings* nil)
                    (when (and (equal (undefined-warning-name warning) name)
                               (eq (undefined-warning-kind warning) kind))
                      (return warning))))
           (res (or found
                    (make-undefined-warning :name name :kind kind))))
      (unless found (push res *undefined-warnings*))
      (multiple-value-bind (context old)
          (find-error-context (list name) (undefined-warning-warnings res))
        (unless old
          (when (or (not *undefined-warning-limit*)
                    (< (undefined-warning-count res) *undefined-warning-limit*))
            (push context (undefined-warning-warnings res)))
          (incf (undefined-warning-count res))))))
  (values))

(defun note-key-arg-mismatch (name keys)
  (let* ((found (find name
                      *argument-mismatch-warnings*
                      :key #'argument-mismatch-warning-name))
         (res (or found
                  (make-argument-mismatch-warning :name name))))
    (unless found
      (push res *argument-mismatch-warnings*))
    (multiple-value-bind (context old)
        (find-error-context (list name) (argument-mismatch-warning-warnings res) #'cdr)
      (unless old
        (push (cons keys context) (argument-mismatch-warning-warnings res))))))

(defun report-key-arg-mismatches ()
  #-sb-xc-host
  (loop for warning in *argument-mismatch-warnings*
        for name = (argument-mismatch-warning-name warning)
        for type = (sb-pcl::compute-gf-ftype name)
        when (and (fun-type-p type)
                  (not (fun-type-allowp type)))
        do
        (loop for (keys . context) in (argument-mismatch-warning-warnings warning)
              for bad = (loop for key in keys
                              when (not (member key (fun-type-keywords type)
                                                :key #'key-info-name))
                              collect key)
              do (let ((*compiler-error-context* context))
                   (cond ((cdr bad)
                          (compiler-style-warn "~@<~{~S~^, ~} and ~S are not a known argument keywords.~:@>"
                                               (butlast bad)
                                               (car (last bad))))
                         (bad
                          (compiler-style-warn "~S is not a known argument keyword."
                                               (car bad))))))))

;; The compiler tracks full calls that were emitted so that it is possible
;; to detect a definition of a compiler-macro occuring after the first
;; compile-time observed use of (vs. actual call of) that function name.
;;
;; The call count is not reset if the function gets redefined (where the
;; macro could briefly be out-of-sync), but this choice is deliberate.
;; We're not trying to find and report all possible ways that users can
;; introduce semantic glitches, only trying to signal something that is
;; otherwise not always obvious in a totally working built-from-scratch
;; user system, absent any interactive changes.
;;
;; Note on implementation: originally I thought about doing something
;; based on whether the name got an APPROXIMATE-FUN-TYPE and the :WHERE-FROM
;; was :ASSUMED - which together imply that the function did not exist *and*
;; that it was not a NOTINLINE call, however that proved to be fragile.
;; The current approach is reliable, at a cost of ~3 words per function.
;;
(defun warn-if-compiler-macro-dependency-problem (name)
  (unless (compiler-macro-function name)
    (let ((status (car (info :function :emitted-full-calls name)))) ; TODO use emitted-full-call-count?
      (when (and (integerp status) (oddp status))
        ;; Show the total number of calls, because otherwise the warning
        ;; would be worded rather obliquely: "N calls were compiled
        ;; not in the scope of a notinline declaration" which is, to me,
        ;; worse than matter-of-factly stating that N calls were compiled.
        ;; This is why I don't bother collecting both statistics.
        ;; It's the tail wagging the dog: the message dictates what to track.
        (compiler-style-warn
         'compiler-macro-application-missed-warning
         :count (ash status -2) :function name)))))

;; Inlining failure scenario 1 [at time of proclamation]:
;; Full call to F is emitted not in the scope of a NOTINLINE, with no definition
;; of F available, and then it's proclaimed INLINE. If F was defined already,
;; it would have been used, unless the expansion limit was hit.
;;
(defun warn-if-inline-failed/proclaim (name new-inlinep)
  (when (eq new-inlinep 'inline)
    (let ((warning-count (sb-impl::emitted-full-call-count name)))
      (when (and warning-count
                 ;; Warn only if the the compiler did not have the expansion.
                 (not (fun-name-inline-expansion name))
                 ;; and if nothing was previously known about inline status
                 ;; so that repeated proclamations don't warn. NIL is a valid
                 ;; value for :inlinep in the globaldb so use the 2nd result.
                 (not (nth-value 1 (info :function :inlinep name))))
        ;; This will be a STYLE-WARNING for the target, but a full warning
        ;; for the host. There's no constraint to use _only_ STYLE-WARN
        ;; to signal a (subtype of) STYLE-WARNING. But conversely we enforce
        ;; that STYLE-WARN not signal things that aren't style-warnings.
        (compiler-warn
         'inlining-dependency-failure
         :format-control
         "~@<Proclaiming ~/sb-ext:print-symbol-with-prefix/ to be INLINE, but ~D call~:P to it ~
~:*~[~;was~:;were~] previously compiled. A declaration of NOTINLINE ~
at the call site~:P will eliminate this warning, as will proclaiming ~
and defining the function before its first potential use.~@:>"
         :format-arguments (list name warning-count))))))

;; Inlining failure scenario 2 [at time of call]:
;; F is not defined, but either proclaimed INLINE and not declared
;; locally notinline, or expressly declared locally inline.
;; Warn about emitting a full call at that time.
;;
;; It could be friendlier to present this warning as one summary
;; at the end of a compilation unit, but that is not as important as
;; just getting the warning across.
;; [The point of deferring a warning is that some future event can resolve it
;; - like an undefined function becoming defined - but there's nothing
;; that can resolve absence of a definition at a point when it was needed]
;;
;; Should we regard it as more serious if the inline-ness of the global
;; function was lexically declared? Is "Inline F here" stronger than
;; "It would generally be a good idea to inline F everywhere"?
;;
;; Don't be too put off by the above concerns though. It's not customary
;; to write (DECLAIM INLINE) after the function, or so far separated from it
;; that intervening callers know it to be proclaimed inline, and would have
;; liked to have a definition, but didn't.
;;
(defun warn-if-inline-failed/call (name lexenv count-cell)
  ;; Do nothing if the inline expansion is known - it wasn't used
  ;; because of the expansion limit, which is a different problem.
  (unless (or (logtest 2 (car count-cell)) ; warn at most once per name
              (fun-name-inline-expansion name))
    ;; This function is only called by PONDER-FULL-CALL when NAME
    ;; is not lexically NOTINLINE, so therefore if it is globally INLINE,
    ;; there was no local declaration to the contrary.
    (when (or (eq (info :function :inlinep name) 'inline)
              (let ((fun (let ((*lexenv* lexenv))
                           (lexenv-find name funs :test #'equal))))
                (and fun
                     (defined-fun-p fun)
                     (eq (defined-fun-inlinep fun) 'inline))))
      ;; Set a bit saying that a warning about the call was generated,
      ;; which suppresses the warning about either a later
      ;; call or a later proclamation.
      (setf (car count-cell) (logior (car count-cell) 2))
      ;; While there could be a different style-warning for
      ;;   "You should put the DEFUN after the DECLAIM"
      ;; if they appeared reversed, it's not ideal to warn as soon as that.
      ;; It's only a problem if something failed to be inlined in account of it.
      (compiler-style-warn
       'inlining-dependency-failure
       :format-control
       (if (info :function :assumed-type name)
           (sb-format:tokens "~@<Call to ~/sb-ext:print-symbol-with-prefix/ ~
                              could not be inlined because no definition ~
                              for it was seen prior to its first use.~:@>")
         ;; This message sort of implies that source form is the
         ;; only reasonable representation in which an inline definition
         ;; could have been saved, which isn't in general true - it could
         ;; be saved as a parsed AST - but I don't really know how else to
         ;; phrase this. And it happens to be true in SBCL, so it's not wrong.
           (sb-format:tokens "~@<Call to ~/sb-ext:print-symbol-with-prefix/ could ~
not be inlined because its source code was not saved. A global INLINE ~
or SB-EXT:MAYBE-INLINE proclamation must be ~
in effect to save function definitions for inlining.~:@>"))
       :format-arguments (list name)))))
