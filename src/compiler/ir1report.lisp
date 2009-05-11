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

(in-package "SB!C")

;;;; compiler error context determination

(declaim (special *current-path*))

(defvar *enclosing-source-cutoff* 1
  #!+sb-doc
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
            #-no-ansi-print-object
            (:print-object (lambda (x stream)
                             (print-unreadable-object (x stream :type t))))
            (:copier nil))
  ;; a list of the stringified CARs of the enclosing non-original source forms
  ;; exceeding the *enclosing-source-cutoff*
  (enclosing-source nil :type list)
  ;; a list of stringified enclosing non-original source forms
  (source nil :type list)
  ;; the stringified form in the original source that expanded into SOURCE
  (original-source (missing-arg) :type simple-string)
  ;; a list of prefixes of "interesting" forms that enclose original-source
  (context nil :type list)
  ;; the FILE-INFO-NAME for the relevant FILE-INFO
  (file-name (missing-arg) :type (or pathname (member :lisp :stream)))
  ;; the file position at which the top level form starts, if applicable
  (file-position nil :type (or index null))
  ;; the original source part of the source path
  (original-source-path nil :type list)
  ;; the lexenv active at the time
  (lexenv nil :type (or null lexenv)))

;;; If true, this is the node which is used as context in compiler warning
;;; messages.
(declaim (type (or null compiler-error-context node) *compiler-error-context*))
(defvar *compiler-error-context* nil)

;;; a hashtable mapping macro names to source context parsers. Each parser
;;; function returns the source-context list for that form.
(defvar *source-context-methods* (make-hash-table))

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
  #!+sb-doc
  "DEFINE-SOURCE-CONTEXT Name Lambda-List Form*
   This macro defines how to extract an abbreviated source context from the
   Named form when it appears in the compiler input. Lambda-List is a DEFMACRO
   style lambda-list used to parse the arguments. The Body should return a
   list of subforms suitable for a \"~{~S ~}\" format string."
  (with-unique-names (whole)
    `(setf (gethash ',name *source-context-methods*)
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

;;; Return the first two elements of FORM if FORM is a list. Take the
;;; CAR of the second form if appropriate.
(defun source-form-context (form)
  (cond ((atom form) nil)
        ((>= (length form) 2)
         (let* ((context-fun-default (lambda (x)
                                       (declare (ignore x))
                                       (list (first form) (second form))))
                (context-fun (gethash (first form)
                                      *source-context-methods*
                                      context-fun-default)))
           (declare (type function context-fun))
           (funcall context-fun (rest form))))
        (t
         form)))

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
        (let ((*print-pretty* pretty))
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
(defun find-error-context (args &optional old-contexts)
  (let ((context *compiler-error-context*))
    (if (compiler-error-context-p context)
        (values context t)
        (let* ((path (or (and (node-p context) (node-source-path context))
                         (and (boundp '*current-path*) *current-path*)))
               (old
                (find (when path (source-path-original-source path))
                      (remove-if #'null old-contexts)
                      :test #'equal
                      :key #'compiler-error-context-original-source-path)))
          (if old
              (values old t)
              (when (and *source-info* path)
                (multiple-value-bind (form src-context) (find-original-source path)
                  (collect ((full nil cons)
                            (short nil cons))
                    (let ((forms (source-path-forms path))
                          (n 0))
                      (dolist (src (if (member (first forms) args)
                                       (rest forms)
                                       forms))
                        (if (>= n *enclosing-source-cutoff*)
                            (short (stringify-form (if (consp src)
                                                       (car src)
                                                       src)
                                                   nil))
                            (full (stringify-form src)))
                        (incf n)))

                    (let* ((tlf (source-path-tlf-number path))
                           (file-info (source-info-file-info *source-info*)))
                      (values
                       (make-compiler-error-context
                        :enclosing-source (short)
                        :source (full)
                        :original-source (stringify-form form)
                        :context src-context
                        :file-name (file-info-name file-info)
                        :file-position
                        (multiple-value-bind (ignore pos)
                            (find-source-root tlf *source-info*)
                          (declare (ignore ignore))
                          pos)
                        :original-source-path (source-path-original-source path)
                        :lexenv (if context
                                    (node-lexenv context)
                                    (if (boundp '*lexenv*) *lexenv* nil)))
                       nil))))))))))

;;;; printing error messages

;;; We save the context information that we printed out most recently
;;; so that we don't print it out redundantly.

;;; The last COMPILER-ERROR-CONTEXT that we printed.
(defvar *last-error-context* nil)
(declaim (type (or compiler-error-context null) *last-error-context*))

;;; The format string and args for the last error we printed.
(defvar *last-format-string* nil)
(defvar *last-format-args* nil)
(declaim (type (or string null) *last-format-string*))
(declaim (type list *last-format-args*))

;;; The number of times that the last error message has been emitted,
;;; so that we can compress duplicate error messages.
(defvar *last-message-count* 0)
(declaim (type index *last-message-count*))

;;; If the last message was given more than once, then print out an
;;; indication of how many times it was repeated. We reset the message
;;; count when we are done.
(defun note-message-repeats (stream &optional (terpri t))
  (cond ((= *last-message-count* 1)
         (when terpri
           (terpri stream)))
        ((> *last-message-count* 1)
         (format stream "~&; [Last message occurs ~W times.]~2%"
                 *last-message-count*)))
  (setq *last-message-count* 0))

;;; Print out the message, with appropriate context if we can find it.
;;; If the context is different from the context of the last message
;;; we printed, then we print the context. If the original source is
;;; different from the source we are working on, then we print the
;;; current source in addition to the original source.
;;;
;;; We suppress printing of messages identical to the previous, but
;;; record the number of times that the message is repeated.
(defmacro print-compiler-message (stream format-string format-args)
  `(with-compiler-io-syntax
     (%print-compiler-message ,stream ,format-string ,format-args)))

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
                 (format stream "in:~{~<~%    ~4:;~{ ~S~}~>~^ =>~}" in))
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

  (incf *last-message-count*)
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

(macrolet ((with-condition ((condition datum args) &body body)
             (with-unique-names (block)
               `(block ,block
                  (let ((,condition
                         (coerce-to-condition ,datum ,args
                                              'simple-compiler-note
                                              'with-condition)))
                    (restart-case
                        (signal ,condition)
                      (muffle-warning ()
                        (return-from ,block (values))))
                    ,@body
                    (values))))))

  (defun compiler-notify (datum &rest args)
    (unless (if *compiler-error-context*
              (policy *compiler-error-context* (= inhibit-warnings 3))
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
(declaim (ftype (function (string &rest t) (values)) compiler-mumble))
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
        (format nil "~{~{~S~^ ~}~^ => ~}"
                #+sb-xc-host (list (list (caar context)))
                #-sb-xc-host context)))))

;;;; condition system interface

;;; Keep track of how many times each kind of condition happens.
(defvar *compiler-error-count*)
(defvar *compiler-warning-count*)
(defvar *compiler-style-warning-count*)
(defvar *compiler-note-count*)

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
  #!+sb-doc
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
            (typep
             (ecase kind
               (:variable (make-condition 'warning))
               ((:function :type) (make-condition 'style-warning)))
             (car
              (rassoc 'muffle-warning
                      (lexenv-handled-conditions *lexenv*))))))
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
