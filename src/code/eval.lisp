;;;; EVAL and friends

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

(defparameter *eval-calls* 0)

;;;; Turns EXPR into a lambda-form we can pass to COMPILE. Returns
;;;; a secondary value of T if we must call the resulting function
;;;; to evaluate EXPR -- if EXPR is already a lambda form, there's
;;;; no need.
(defun make-eval-lambda (expr)
  (let ((lambda (if (typep expr '(cons (eql function) (cons t null)))
                    (cadr expr)
                    expr)))
    (if (typep lambda '(cons (member lambda named-lambda lambda-with-lexenv)))
        (values lambda nil)
        (values `(lambda ()
                 ;; why PROGN? So that attempts to eval free declarations
                 ;; signal errors rather than return NIL. -- CSR, 2007-05-01
                 ;; But only force in a PROGN if it's actually needed to flag
                 ;; that situation as an error. Macros can't expand into DECLARE,
                 ;; so anything other than DECLARE can be left alone.
                   ,(if (and (consp expr) (eq (car expr) 'declare))
                        `(progn ,expr)
                        expr))
                t))))

;;; FIXME: what does "except in that it can't handle toplevel ..." mean?
;;; Is there anything wrong with the implementation, or is the comment obsolete?
;;; general case of EVAL (except in that it can't handle toplevel
;;; EVAL-WHEN magic properly): Delegate to #'COMPILE.
(defun %simple-eval (expr lexenv)
  (multiple-value-bind (lambda call) (make-eval-lambda expr)
    (let ((fun
            ;; This tells the compiler where the lambda comes from, in case it
            ;; wants to report any problems.
            (let ((sb-c::*source-form-context-alist*
                    (acons lambda *eval-source-context*
                           sb-c::*source-form-context-alist*)))
              (handler-bind (;; Compiler notes just clutter up the REPL:
                             ;; anyone caring about performance should not
                             ;; be using EVAL.
                             (compiler-note #'muffle-warning))
                (sb-c:compile-in-lexenv lambda lexenv nil *eval-source-info*
                                        *eval-tlf-index* nil (not call))))))
      (declare (function fun))
      (if call
          (funcall fun)
          fun))))

;;; Handle PROGN and implicit PROGN.
#-sb-fasteval
(progn
(defun list-with-length-p (x)
  ;; Is X a list for which LENGTH is meaningful, i.e. a list which is
  ;; not improper and which is not circular?
  (values (ignore-errors (list-length x))))
(defun simple-eval-progn-body (progn-body lexenv)
  (unless (list-with-length-p progn-body)
    (let ((*print-circle* t))
      (%program-error "~@<not a proper list in PROGN or implicit PROGN: ~
                       ~2I~_~S~:>"
                      progn-body)))
  ;; Note:
  ;;   * We can't just use (MAP NIL #'EVAL PROGN-BODY) here, because we
  ;;     need to take care to return all the values of the final EVAL.
  ;;   * It's left as an exercise to the reader to verify that this
  ;;     gives the right result when PROGN-BODY is NIL, because
  ;;     (FIRST NIL) = (REST NIL) = NIL.
  (do* ((i progn-body rest-i)
        (rest-i (rest i) (rest i)))
      (nil)
    (if rest-i ; if not last element of list
        (simple-eval-in-lexenv (first i) lexenv)
        (return (simple-eval-in-lexenv (first i) lexenv)))))

(defun simple-eval-locally (exp lexenv &key vars funs)
  (multiple-value-bind (body decls) (parse-body exp nil)
    (let ((lexenv
           ;; KLUDGE: Uh, yeah.  I'm not anticipating
           ;; winning any prizes for this code, which was
           ;; written on a "let's get it to work" basis.
           ;; These seem to be the variables that need
           ;; bindings for PROCESS-DECLS to work
           ;; (*IR1-NAMESPACE* so that
           ;; references to free functions and variables
           ;; in the declarations can be noted;
           ;; *UNDEFINED-WARNINGS* so that warnings about
           ;; undefined things can be accumulated [and
           ;; then thrown away, as it happens]). -- CSR,
           ;; 2002-10-24
           (let* ((sb-c:*lexenv* lexenv)
                  (sb-c::*ir1-namespace* (sb-c::make-ir1-namespace))
                  (sb-c::*undefined-warnings* nil)
                  sb-c::*argument-mismatch-warnings*)
             ;; FIXME: VALUES declaration
             (sb-c::process-decls decls
                                  vars
                                  funs
                                  :lexenv lexenv
                                  :context :eval))))
      (simple-eval-progn-body body lexenv))))
) ; end PROGN

;;;; EVAL-ERROR
;;;;
;;;; Analogous to COMPILER-ERROR, but simpler.

(define-condition eval-error (encapsulated-condition)
  ()
  (:report (lambda (condition stream)
             (print-object (encapsulated-condition condition) stream))))

(defun eval-error (condition)
  (signal 'eval-error :condition condition)
  (bug "Unhandled EVAL-ERROR"))

#-sb-fasteval
(progn
;; See comment in 'full-eval' at its definition of INTERPRETED-PROGRAM-ERROR
;; which is not the same as this one. Since neither is more globally used
;; or appropriate per se, neither is exported from SB-KERNEL or SB-INT.
(define-condition interpreted-program-error
    (program-error encapsulated-condition)
  ;; Unlike COMPILED-PROGRAM-ERROR, we don't need to dump these, so
  ;; storing the original condition and form is OK.
  ((form :initarg :form :reader program-error-form))
  (:report (lambda (condition stream)
             (format stream "~&Evaluation of~%  ~S~%~
                             caused error:~%  ~A~%"
                     (program-error-form condition)
                     (encapsulated-condition condition)))))

;;; Pick off a few easy cases, and the various top level EVAL-WHEN
;;; magical cases, and call %SIMPLE-EVAL for the rest.
(defun simple-eval-in-lexenv (original-exp lexenv)
  (declare (optimize (safety 1)))
  ;; (aver (lexenv-simple-p lexenv))
  (incf *eval-calls*)
  (sb-c:with-compiler-error-resignalling
    (let ((exp (macroexpand original-exp lexenv)))
      (handler-bind ((eval-error
                       (lambda (condition)
                         (error 'interpreted-program-error
                                :condition (encapsulated-condition condition)
                                :form exp))))
        (typecase exp
          (symbol
           (ecase (info :variable :kind exp)
             ((:special :global :constant :unknown)
              (symbol-value exp))
             ;; FIXME: This special case here is a symptom of non-ANSI
             ;; weirdness in SBCL's ALIEN implementation, which could
             ;; cause problems for e.g. code walkers. It'd probably be
             ;; good to ANSIfy it by making alien variable accessors
             ;; into ordinary forms, e.g. (SB-UNIX:ENV) and (SETF
             ;; SB-UNIX:ENV), instead of magical symbols, e.g. plain
             ;; SB-UNIX:ENV. Then if the old magical-symbol syntax is to
             ;; be retained for compatibility, it can be implemented
             ;; with DEFINE-SYMBOL-MACRO, keeping the code walkers
             ;; happy.
             (:alien
              (sb-alien-internals:alien-value exp))))
          (list
           (let ((name (first exp))
                 (n-args (1- (length exp))))
             (case name
               ((function)
                (unless (= n-args 1)
                  (error "wrong number of args to FUNCTION:~% ~S" exp))
                (let ((name (second exp)))
                  (if (and (legal-fun-name-p name)
                           (not (consp (sb-c:lexenv-find name funs :lexenv lexenv))))
                      (%coerce-name-to-fun name)
                      ;; FIXME: This is a bit wasteful: it would be nice to call
                      ;; COMPILE-IN-LEXENV with the lambda-form directly, but
                      ;; getting consistent source context and muffling compiler notes
                      ;; is easier this way.
                      (%simple-eval original-exp lexenv))))
               ((quote)
                (unless (= n-args 1)
                  (error "wrong number of args to QUOTE:~% ~S" exp))
                (second exp))
               (setq
                (unless (evenp n-args)
                  (error "odd number of args to SETQ:~% ~S" exp))
                (unless (zerop n-args)
                  (do ((name (cdr exp) (cddr name)))
                      ((null name)
                       (do ((args (cdr exp) (cddr args)))
                           ((null (cddr args))
                            ;; We duplicate the call to SET so that the
                            ;; correct value gets returned.
                            (set (first args)
                                 (simple-eval-in-lexenv (second args) lexenv)))
                         (set (first args)
                              (simple-eval-in-lexenv (second args) lexenv))))
                    (let ((symbol (first name)))
                      (case (info :variable :kind symbol)
                        (:special)
                        (t (return (%simple-eval original-exp lexenv))))
                      (unless (type= (info :variable :type symbol)
                                     *universal-type*)
                        ;; let the compiler deal with type checking
                        (return (%simple-eval original-exp lexenv)))))))
               ((progn)
                (simple-eval-progn-body (rest exp) lexenv))
               ((eval-when)
                ;; FIXME: DESTRUCTURING-BIND returns ARG-COUNT-ERROR
                ;; instead of PROGRAM-ERROR when there's something wrong
                ;; with the syntax here (e.g. missing SITUATIONS). This
                ;; could be fixed by hand-crafting clauses to catch and
                ;; report each possibility, but it would probably be
                ;; cleaner to write a new macro
                ;; DESTRUCTURING-BIND-PROGRAM-SYNTAX which does
                ;; DESTRUCTURING-BIND and promotes any mismatch to
                ;; PROGRAM-ERROR, then to use it here and in (probably
                ;; dozens of) other places where the same problem
                ;; arises.
                (destructuring-bind (eval-when situations &rest body) exp
                  (declare (ignore eval-when))
                  (multiple-value-bind (ct lt e)
                      (sb-c:parse-eval-when-situations situations)
                    ;; CLHS 3.8 - Special Operator EVAL-WHEN: The use of
                    ;; the situation :EXECUTE (or EVAL) controls whether
                    ;; evaluation occurs for other EVAL-WHEN forms; that
                    ;; is, those that are not top level forms, or those
                    ;; in code processed by EVAL or COMPILE. If the
                    ;; :EXECUTE situation is specified in such a form,
                    ;; then the body forms are processed as an implicit
                    ;; PROGN; otherwise, the EVAL-WHEN form returns NIL.
                    (declare (ignore ct lt))
                    (when e
                      (simple-eval-progn-body body lexenv)))))
               ((locally)
                (simple-eval-locally (rest exp) lexenv))
               ((macrolet)
                (destructuring-bind (definitions &rest body) (rest exp)
                  (let ((sb-c:*lexenv* lexenv))
                    (sb-c::funcall-in-macrolet-lexenv
                     definitions
                     (lambda (&optional funs)
                       (simple-eval-locally body sb-c:*lexenv*
                                            :funs funs))
                     :eval))))
               ((symbol-macrolet)
                (destructuring-bind (definitions &rest body) (rest exp)
                  (let ((sb-c:*lexenv* lexenv))
                    (sb-c::funcall-in-symbol-macrolet-lexenv
                     definitions
                     (lambda (&optional vars)
                       (simple-eval-locally body sb-c:*lexenv*
                                            :vars vars))
                     :eval))))
               ((if)
                (destructuring-bind (test then &optional else) (rest exp)
                  (eval-in-lexenv (if (eval-in-lexenv test lexenv)
                                      then
                                      else)
                                  lexenv)))
               ((let let*)
                (%simple-eval exp lexenv))
               (t
                (if (and (symbolp name)
                         (eq (info :function :kind name) :function))
                    (collect ((args))
                      (dolist (arg (rest exp))
                        (args (eval-in-lexenv arg lexenv)))
                      (apply (symbol-function name) (args)))
                    (%simple-eval exp lexenv))))))
          (t
           exp))))))
) ; end PROGN

;;; This definition will be replaced after the interpreter is compiled.
;;; Until then we just always compile.
#+sb-fasteval
(defun sb-interpreter:eval-in-environment (exp lexenv)
  (let ((exp (macroexpand exp lexenv)))
    (if (symbolp exp)
        (symbol-value exp)
        (%simple-eval exp (or lexenv (make-null-lexenv))))))

(defun eval-in-lexenv (exp lexenv)
  #+sb-eval
  (let ((lexenv (or lexenv (make-null-lexenv))))
    (if (eq *evaluator-mode* :compile)
        (simple-eval-in-lexenv exp lexenv)
        (sb-eval:eval-in-native-environment exp lexenv)))
  #+sb-fasteval
  (sb-c:with-compiler-error-resignalling
   (sb-interpreter:eval-in-environment exp lexenv))
  #-(or sb-eval sb-fasteval)
  (simple-eval-in-lexenv exp (or lexenv (make-null-lexenv))))

(defun eval (original-exp)
  "Evaluate the argument in a null lexical environment, returning the
   result or results."
  (let ((*eval-source-context* original-exp)
        (*eval-tlf-index* nil)
        (*eval-source-info* nil))
    (eval-in-lexenv original-exp nil)))

(defun eval-tlf (original-exp tlf-index &optional lexenv)
  (let ((*eval-source-context* original-exp)
        (*eval-tlf-index* tlf-index)
        (*eval-source-info* sb-c::*source-info*))
    (eval-in-lexenv original-exp lexenv)))

;;; miscellaneous full function definitions of things which are
;;; ordinarily handled magically by the compiler

(defun apply (function arg &rest arguments)
  "Apply FUNCTION to a list of arguments produced by evaluating ARGUMENTS in
  the manner of LIST*. That is, a list is made of the values of all but the
  last argument, appended to the value of the last argument, which must be a
  list."
  (cond ((atom arguments)
         (apply function arg))
        ((atom (cdr arguments))
         (apply function (cons arg (car arguments))))
        (t (do* ((a1 arguments a2)
                 (a2 (cdr arguments) (cdr a2)))
                ((atom (cdr a2))
                 (rplacd a1 (car a2))
                 (apply function (cons arg arguments)))))))

(defun funcall (function &rest arguments)
  "Call FUNCTION with the given ARGUMENTS."
  (apply function arguments))

(defun values (&rest values)
  "Return all arguments, in order, as values."
  (declare (truly-dynamic-extent values))
  (values-list values))

(defun values-list (list)
  "Return all of the elements of LIST, in order, as values."
  (values-list list))
