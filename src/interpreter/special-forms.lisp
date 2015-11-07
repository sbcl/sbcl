;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-INTERPRETER")

;;; Return a THE form that wraps EXPRESSION, but if CTYPE is NIL,
;;; just return EXPRESSION.
(defun cast-to (ctype expression)
  (aver (neq ctype *universal-type*)) ; should never store this
  (if ctype `(the ,ctype ,expression) expression))

;;; Given a list of FORMS, return a vector of SEXPRs that will eval those.
;;; There "should" be more than one SEXPR because any PROGN-like form with
;;; less than 2 subforms should have the singleton (or NIL) form lifted
;;; to its containing SEXPR by that SEXPR's digester. Lifting happens
;;; automatically when you use %PROGN to build a list of subforms.
;;; i.e. (PROGN (PROGN FOO)) == FOO after twice lifting.
;;;
;;; Pathological silly examples:
;;; 1) Loop using a circular list does not work.
;;;    This works ok in purely recursive-descent EVAL, but is precluded
;;;    by VECTOR-OF-SEXPR asking for list-length of the PROGN form.
;;;     (BLOCK X (let ((I 0)) .
;;;       #1=((IF (< (INCF I) 10) (PRINT I) (RETURN-FROM X)) . #1#)))
;;;
;;; 2) Factorial by recursing in syntax rather than semantics works
;;;    because it's CAR circular, not CDR circular.
;;;      (#1=(LAMBDA (N) (IF (ZEROP N) 1 (* N (#1# (1- N))))) 6)
;;;    Athough it forces an awful lot of consing on each recursion.
(defun vector-of-sexpr (forms)
  (let ((n (list-length forms)))
    (cond ((null n) (error "Circular form"))
          ((eql n 0) '#()) ; all empty vectors are the same to me
          (t
           (let ((a (make-array n)))
             (loop for i from 0 for elt in forms
                   do (setf (svref a i) (%sexpr elt)))
             a)))))

;;; Return a handler for a SEXPR which reads SYMBOL in ENV.
;;; If TYPE is non-nil, it is a CTYPE to check against.
;;; Unlike for handlers defined with DEFSPECIAL, this one both
;;; preprocesses the sexpr *and* evals the symbol immediately.
;;;
;;; Todo: some re-testing of (INFO :VARIABLE :KIND) can be eliminated if
;;; we just touch the *globaldb-cookie* whenever a global macro or symbol-macro
;;; is defined or redefined (incl. a global variable changing its :KIND).
;;; This might be more efficient than, and is definitely simpler than, the
;;; checks below. On the other hand, we only need to kill memoized data
;;; when strange stuff happens that can't be anticipated, of which there
;;; are vanishingly few reasons now.
;;;
(defun symeval (symbol env sexpr)
  (binding* (((binding kind frame-ptr expansion) (find-lexical-var env symbol))
             (type (var-type-assertion env symbol binding :read)))
    (if kind ; lexically apparent binding (possibly special or macro)
        (if (eq kind :macro)
            ;; digest-form would lose the type constraint, so pass it in using
            ;; a THE wrapper. We use the same hack as in the compiler:
            ;; THE is supposed to take a type-specifier but we allow a CTYPE
            ;; to avoid unparsing and reparsing.
            (digest-form (cast-to type (symexpand env symbol expansion))
                         env sexpr)
            (progn
              (setf (sexpr-handler sexpr)
                    (if (eq kind :special)
                        (specvar-reffer symbol type)
                        (lexvar-reffer frame-ptr type)))
              (%dispatch sexpr env))) ; DISPATCH would be fine but for the noise
        (case (info :variable :kind symbol)
          (:macro
           ;; Interestingly it's not even a style warning to redefine
           ;; a global symbol-macro.
           (let* ((expanded-form (symexpand env symbol))
                  (expanded-sexpr (%sexpr (cast-to type expanded-form))))
             (setf
              (sexpr-handler sexpr)
              (if (and type (neq type *universal-type*))
                  ;; Don't need to additionally capture the original expansion
                  ;; because it is available as the third subform of (THE).
                  (hlambda SYMEVAL (symbol expanded-sexpr) (env sexpr)
                    (multiple-value-bind (x winp)
                        (info :variable :macro-expansion symbol)
                      (if (and winp (eq (third (sexpr-form expanded-sexpr)) x))
                          (dispatch expanded-sexpr env)
                          (digest-form symbol env sexpr))))
                  (hlambda SYMEVAL (symbol expanded-sexpr) (env sexpr)
                    (multiple-value-bind (x winp)
                        (info :variable :macro-expansion symbol)
                      (if (and winp (eq (sexpr-form expanded-sexpr) x))
                          (dispatch expanded-sexpr env)
                          (digest-form symbol env sexpr))))))
             (dispatch expanded-sexpr env)))
          (:alien
           ;; can there be a type restriction?
           (let ((info (info :variable :alien-info symbol)))
             (setf (sexpr-handler sexpr)
                   (hlambda SYMEVAL (symbol info) (env sexpr)
                     (if (eq (info :variable :alien-info symbol) info)
                         (%heap-alien info)
                         (digest-form symbol env sexpr))))
             (%heap-alien info)))
          (t ; everything else
           ;; The PARANOID argument to SPECVAR-REFFER is not a safety-related
           ;; quality - it affects convenience, which is considered inversely
           ;; correlated to SPEED.
           (setf (sexpr-handler sexpr)
                 (specvar-reffer symbol type (< (policy env speed) 2)))
           (%dispatch sexpr env)))))) ; DISPATCH would be fine but for the noise

(defun immediate-setq-1 (symbol newval env)
  (binding* (((binding kind frame-ptr value) (find-lexical-var env symbol))
             (type (var-type-assertion env symbol binding :write)))
    (flet ((setf-it (expansion)
             (%eval `(setf ,expansion ,(cast-to type newval)) env))
           (eval-it ()
            (let ((newval (%eval newval env)))
              (when type
                (unless (itypep newval type)
                  (typecheck-fail symbol newval type)))
              newval)))
      (if kind ; lexically apparent binding (possibly special or macro)
          (if (eq kind :macro)
              (setf-it (symexpand env symbol value))
              (let ((newval (eval-it)))
                (if (eq kind :special)
                    (set symbol newval)
                    (setf (%cell-ref env frame-ptr) newval))))
          (case (info :variable :kind symbol)
            (:macro
             (setf-it (symexpand env symbol)))
            (:alien
             (locally
                 (declare (muffle-conditions compiler-note)) ; can't open-code
               (setf (%heap-alien (info :variable :alien-info symbol))
                     (eval-it))))
            (t ; everything else - no need to check for constants
             ;; as the runtime will signal an error.
             (set symbol (eval-it))))))))

;;; Perform a SETQ, verifying that the variable still is of the kind it was
;;; when the form was digested.
;;; Like SYMEVAL and unlike handlers defined with DEFSPECIAL, this one both
;;; preprocesses the sexpr *and* performs its action.
;;; Todo: figure out what global changes in :KIND are actually possible,
;;; such as :macro -> :constant. Some of them generate continuable errors
;;; and some not; but by hacking on globaldb you can do anything you want.
;;; The globaldb cookie will take care of any loose ends.
(defun deferred-setq-1 (symbol newval env sexpr)
  (binding* (((binding kind frame-ptr expansion) (find-lexical-var env symbol))
             (type (var-type-assertion env symbol binding :write)))
    (aver (neq type *universal-type*))
    (if kind ; lexically apparent binding (possibly special or macro)
        (if (eq kind :macro)
            ;; symbol-macros are handled as if the form were SETF
            (digest-form `(setf ,(symexpand env symbol expansion)
                                ,(cast-to type newval)) env sexpr)
            (let ((newval (%sexpr newval)))
              (setf (sexpr-handler sexpr)
                    (if (eq kind :special)
                        ;; It is unusual to more restrictively constrain a
                        ;; special var than by its globally declaimed type
                        ;; which is checked by SET, but check again anyway.
                        (if type
                            (hlambda SETQ (newval symbol type) (env)
                              (let ((newval (dispatch newval env)))
                                (if (itypep newval type)
                                    (set symbol newval)
                                    (typecheck-fail symbol newval type))))
                            (hlambda SETQ (newval symbol) (env)
                              (set symbol (dispatch newval env))))
                        (lexvar-setter frame-ptr newval type)))
              (%dispatch sexpr env)))
        (case (info :variable :kind symbol)
          (:macro
           (digest-form `(setf ,(symexpand env symbol) ,(cast-to type newval))
                        env sexpr)
           (%dispatch sexpr env))
          (:alien
           (let ((info (info :variable :alien-info symbol))
                 (newval (%sexpr newval)))
             ;; don't care that we're unable to optimize %HEAP-ALIEN
             (setf (sexpr-handler sexpr)
                   (hlambda SETQ (newval symbol info) (env sexpr)
                     (if (eq (info :variable :alien-info symbol) info)
                         (locally (declare (muffle-conditions compiler-note))
                           (setf (%heap-alien info) (dispatch newval env)))
                         (digest-form (sexpr-form sexpr) env sexpr))))
             (%dispatch sexpr env)))
          (t ; everything else
           ;; FIXME: missing type check
           (let ((newval (%sexpr newval)))
             (setf (sexpr-handler sexpr)
                   (hlambda SETQ (newval symbol) (env sexpr)
                     (if (member (info :variable :kind symbol) '(:alien :macro))
                         (digest-form (sexpr-form sexpr) env sexpr)
                         (set symbol (dispatch newval env)))))
             (set symbol (dispatch newval env))))))))

;; Return a handler for, but do not evaluate, a list of FORMS.
(defun digest-progn (forms)
  (cond ((not forms) (return-constant nil))
        ((not (cdr forms)) (handler #'digest-form (first forms)))
        ((not (cddr forms))
         (let ((one (%sexpr (first forms))) (two (%sexpr (second forms))))
         (hlambda PROGN (one two) (env)
           (dispatch one env) (dispatch two env))))
        ((not (list-length forms))
         (error "Circular list"))
        (t
         ;; Convert to a vector of sexprs.
         ;; Alternatively, could express in terms of prog2 and progn,
         ;; which would allow this evaluator to handle circular forms,
         ;; albeit with great inefficiency.
         (let ((forms (nreverse (vector-of-sexpr forms))))
           (hlambda PROGN (forms) (env)
             (declare (simple-vector forms))
             (let ((i (1- (length forms))))
               (loop (dispatch (svref forms i) env)
                  (if (zerop (decf i)) (return))))
             (dispatch (svref forms 0) env))))))

;;; Some simple handlers

(defun eval-progn (body env) ; Immediate-mode handler for PROGN
  (let ((previous-exp nil))
    (dolist (exp body)
      (if previous-exp
          (%eval previous-exp env))
      (setf previous-exp exp))
    ;; Preserve tail call
    (%eval previous-exp env)))

(defspecial progn (&rest forms)
  :immediate (env) (eval-progn forms env)
  :deferred () (digest-progn forms))

(defspecial quote (object)
  :immediate () object
  :deferred () (return-constant object))

;;; Special operator THE can use local-call for its helper functions.
(flet ((enforce-values-types (type &rest values)
         (declare (optimize (safety 0)))
         ;; Check VALUES against TYPE and return all the values.
         ;; This should not cons except on error.
         ;; NTH on a &REST list is random-access, not a pointer traversal.
         ;; [cf. comment in src/code/numbers.lisp re. "very clever"
         ;; versus "charmingly naive"]
         (let ((i 0) (n (length values)) (rest (values-type-rest type)))
           (declare (index i))
           (tagbody
              (dolist (x (values-type-required type))
                (if (or (>= i n) (not (itypep (nth i values) x)))
                    (go fail))
                (incf i))
              (dolist (x (values-type-optional type))
                (cond ((>= i n) (go done))
                      ((not (itypep (nth i values) x))
                       (go fail)))
                (incf i))
              (if (= i n) (go done))
              (if (not rest) (go fail))
            rest
              (unless (itypep (nth i values) rest)
                (go fail))
              (if (< (incf i) n) (go rest))
            done
              (return-from enforce-values-types (apply #'values values))
            fail)
           ;; Punting keeps the consing out of this function so that I can
           ;; determine that the non-consing claim remains true.
           (apply #'values-typecheck-fail type values)))

       ;; Check the first of VALUES against TYPE and return all values.
       ;; Unlike ENFORCE-TYPE, this returns all VALUES though only one
       ;; was expected. This should not cons except on error.
       (enforce-single-type (type &rest values)
         (cond ((null values)
                (error "~S received no values"
                       (list 'the (specifier-from-checkfun type))))
               ((itypep (first values) type)
                (apply #'values values))
               (t
                (error 'type-error
                       :datum (first values)
                       :expected-type (specifier-from-checkfun type)))))

       (parse-type (spec-or-obj)
         (type-checker
          (if (ctype-p spec-or-obj)
              spec-or-obj
              (values-specifier-type spec-or-obj)))))

  ;; If a THE form returns multiple values we have to propagate all of them
  ;; even if it was not (THE (VALUES ...))
  ;; If TYPE contains an unknown type, it'll be allowed it as long as it
  ;; is not reached. (typep 3 '(and (not integer) no-such-type)) returns NIL.
  (defspecial the (type-specifier form)
    :immediate (env)
    ;; If speed is more important than safety, don't process THE forms.
    (if (policy env (> speed safety))
        (%eval form env)
        (let ((type (parse-type type-specifier)))
          (multiple-value-call (if (sb-kernel::%values-type-p type)
                                   #'enforce-values-types
                                   #'enforce-single-type)
            type (%eval form env))))
    :deferred (env)
    (if (policy env (> speed safety))
        (handler #'digest-form form)
        (let ((type (parse-type type-specifier)))
          (if (eq type *universal-type*) ; don't type-check if T
              (handler #'digest-form form)
              (let ((form (%sexpr form)))
                (if (sb-kernel::%values-type-p type)
                    (hlambda THE/MULTI (type form) (env)
                       (multiple-value-call #'enforce-values-types
                         type (dispatch form env)))
                    (hlambda THE/SINGLE (type form) (env)
                       (multiple-value-call #'enforce-single-type
                         type (dispatch form env))))))))))

;;; Even though TRULY-THE has a macroexpander into THE, it would
;;; be suboptimal to use that because it would force type-checking
;;; when the entire point is to skip checking.
(defspecial truly-the (type-specifier form)
  ;; Perhaps this should at least parse the specifier?
  :immediate (env) type-specifier ; ignoring it
  (%eval form env)
  :deferred () type-specifier ; ignoring it
  (handler #'digest-form form))

(defspecial if (test then &optional else)
  :immediate (env) (if (%eval test env) (%eval then env) (%eval else env))
  :deferred ()
  ;; Many common idioms (builtin or otherwise) expand to IF with
  ;; only one consequent.
  (let ((test (%sexpr test)) (then (%sexpr then)) (else (%sexpr else)))
    (cond ((and then else)
           (hlambda IF (test then else) (env)
             (if (dispatch test env) (dispatch then env) (dispatch else env))))
          ((and then (not else))
           (hlambda IF (test then) (env)
             (if (dispatch test env) (dispatch then env))))
          ((and (not then) else)
           (hlambda IF (test else) (env)
             (if (dispatch test env) nil (dispatch else env))))
          (t
           ;; Does (IF (HAIRY-TEST) nil nil) actually occur in real life?
           (hlambda IF (test) (env)
             (dispatch test env)
             nil)))))

(defspecial catch (tag &body forms)
  :immediate (env) (catch (%eval tag env) (eval-progn forms env))
  :deferred ()
  (let ((tag (%sexpr tag)) (forms (%progn forms)))
    (hlambda CATCH (tag forms) (env)
      (catch (dispatch tag env) (dispatch forms env)))))

(defspecial throw (tag result)
  :immediate (env) (throw (%eval tag env) (%eval result env))
  :deferred ()
  (let ((tag (%sexpr tag)) (result (%sexpr result)))
    (hlambda THROW (tag result) (env)
      (throw (dispatch tag env) (dispatch result env)))))

(defspecial unwind-protect (protected-form &rest cleanup-forms)
  :immediate (env)
  (unwind-protect (%eval protected-form env)
    (eval-progn cleanup-forms env))
  :deferred ()
  (let ((protected-form (%sexpr protected-form))
        (cleanup-forms (%progn cleanup-forms)))
    (hlambda UNWIND-PROTECT (protected-form cleanup-forms) (env)
      (unwind-protect (dispatch protected-form env)
        (dispatch cleanup-forms env)))))

(defspecial progv (symbols values &body forms)
  :immediate (env)
  (progv (%eval symbols env) (%eval values env) (eval-progn forms env))
  :deferred ()
  (let ((symbols (%sexpr symbols)) (values (%sexpr values))
        (forms (%progn forms)))
    (hlambda PROGV (symbols values forms) (env)
      (progv (dispatch symbols env) (dispatch values env)
        (dispatch forms env)))))

(defspecial load-time-value (form &optional read-only-p)
  (declare (ignore read-only-p))
  :immediate () (%eval form nil)
  ;; full-eval used the enviroment here. CLHS says use the null environment.
  ;; I wonder if the true intent was to use the macro and symbol-macro
  ;; environment but otherwise have no lexical bindings visible?
  ;; In other words, is this allowed?
  ;;  (macrolet ((foo (x) `(car ,x))) (load-time-value (foo (thing)))
  ;; The compiler seems not to think so, but CLHS is ambiguous.
  :deferred () (return-constant (%eval form nil)))

(defspecial multiple-value-call (function &rest forms)
  :immediate (env)
  (apply (%eval function env)
         (loop for form in forms
               nconc (multiple-value-list (%eval form env))))
  ;; Todo: specialize when FUNCTION satisfies QUOTED-FUNCTION-NAME-P ?
  :deferred ()
  (if (not forms)
      (handler #'digest-form `(funcall ,function))
      (let ((function (%sexpr function)))
        (cond ((not (cdr forms))
               (let ((form (%sexpr (car forms))))
                 (hlambda MULTIPLE-VALUE-CALL (function form) (env)
                   (locally (declare (muffle-conditions compiler-note))
                     (multiple-value-call (dispatch function env)
                       (dispatch form env))))))
              ((not (cddr forms))
               (let ((a (%sexpr (car forms))) (b (%sexpr (cadr forms))))
                 (hlambda MULTIPLE-VALUE-CALL (function a b) (env)
                   (locally (declare (muffle-conditions compiler-note))
                     (multiple-value-call (dispatch function env)
                       (dispatch a env) (dispatch b env))))))
              (t
               (let ((forms (vector-of-sexpr forms)))
                 (hlambda MULTIPLE-VALUE-CALL (function forms) (env)
                   (locally (declare (muffle-conditions compiler-note))
                     (apply (dispatch function env)
                            (loop for x across (the simple-vector forms)
                               nconc (multiple-value-list
                                      (dispatch x env))))))))))))

(defspecial multiple-value-prog1 (first-form &rest more-forms)
  :immediate (env)
  (multiple-value-prog1 (%eval first-form env) (eval-progn more-forms env))
  :deferred ()
  (let ((first-form (%sexpr first-form)) (more-forms (%progn more-forms)))
    (hlambda MULTIPLE-VALUE-PROG1 (first-form more-forms) (env)
      (multiple-value-prog1 (dispatch first-form env)
        (dispatch more-forms env)))))

(defspecial eval-when (situations &body body &aux ct lt ex)
  ;; FIXME: this might signal a compiler error instead of an IP-ERROR.
  (multiple-value-setq (ct lt ex)
    (parse-eval-when-situations situations))
  :immediate (env) (if ex (eval-progn body env))
  :deferred () (if ex (digest-progn body) (return-constant nil)))

;;; BLOCK and RETURN-FROM
(macrolet ((eval-it (eval-fn)
             ;; An ENV can not represent itself as a catch point, because
             ;; freezing changes its identity. Use a fresh cons cell instead.
             `(let ((env (make-block-env env (list name) nil *vacuous-decls*)))
                (catch (env-payload env) (,eval-fn forms env)))))
  (defspecial block (name &rest forms)
    (unless (symbolp name)
      (ip-error "~@<The block name ~S is not a symbol.~:@>" name))
    :immediate (env) (eval-it eval-progn)
    :deferred ()
    (let ((forms (%progn forms)))
      (hlambda BLOCK (name forms) (env) (eval-it dispatch)))))

;;; Establishing a handler to catch SIMPLE-CONTROL-ERROR would incorrectly
;;; alter the handler chain as established by user code, in addition
;;; to being needlessly slow. The code in 'interr' can recognize when
;;; an invalid control transfer was initiated by interpreted code,
;;; and it adjusts the error message accordingly.
(macrolet ((eval-it (tag eval-fn)
             `(throw ,tag (,eval-fn result env))))
  (defspecial return-from (name &optional result &aux (block-env env) (depth 0))
    ;; unfortunately the only way to write this OAOO is with an &AUX var
    (declare (fixnum depth))
    (unless (symbolp name)
      (ip-error "~@<The block name ~S is not a symbol.~:@>" name))
    (loop (if (null block-env)
              (ip-error "~@<Return for unknown block: ~S~:@>" name))
          (when (eq (typecase block-env
                      (lambda-env (car (lambda-env-block block-env)))
                      (block-env (car (env-payload block-env)))
                      (t 0)) name)
            (return))
          (incf depth)
          (setq block-env (env-parent block-env)))
    :immediate (env)
    (eval-it (if (lambda-env-p block-env)
                 (lambda-env-block block-env) (env-payload block-env))
             %eval)
    :deferred (env)
    (let ((result (%sexpr result)))
      (if (lambda-env-p block-env)
          (hlambda RETURN-FROM (depth result) (env)
            (eval-it (lambda-env-block (env-ancestor env depth)) dispatch))
          (hlambda RETURN-FROM (depth result) (env)
            (eval-it (env-payload (env-ancestor env depth)) dispatch))))))

;;;;
#+Nil
(defun map-into-vector-reversed (f list length)
  (let ((output (make-array length)) (i length))
    (loop (if (minusp (decf i)) (return))
          (setf (svref output i) (funcall f (pop list))))
    output))

;;; A TAGBODY is parsed into a vector of expressions each of which is either
;;; a SEXPR, an unconditional GO, or a conditional GO. Preprocessing speeds
;;; up (DOTIMES (I N) ..) by at least a factor of 10, especially the hoisting
;;; of conditional GO, which would otherwise entail throwing to the catch point
;;; and restarting. Such catch/throw is necessary when the GO is embedded in
;;; deeper subforms, but picking off the shallow cases is easy.
;;;
(defun parse-tagbody (body env)
  (collect ((tags) (new-body))
    ;; First pass: just collect the tags
    (let (any-forms)
      (dolist (item body)
        (cond ((consp item) (setq any-forms t))
              ((or (symbolp item) (integerp item))
               (if (assoc item (tags))
                   (ip-error "Duplicate tag ~S in tagbody" item)
                   (tags (list item))))
              (t
               (ip-error "Bad thing to appear in a tagbody: ~S" item))))
      (cond ((not any-forms)
             (return-from parse-tagbody (return-constant nil)))
            ((not (tags))
             (return-from parse-tagbody (digest-progn `(,@body nil))))))
    (flet ((go-p (form)
             (and (typep form '(cons (eql go) (cons t null)))
                  (assoc (cadr form) (tags)))))
      ;; Second pass: rewrite conditioned GO forms appearing directly in
      ;; this tagbody which transfer control to a tag in this tagbody.
      ;; Local function bindings for WHEN/UNLESS will inhibit this.
      (dolist (item (unless (or (find-lexical-fun env 'when)
                                (find-lexical-fun env 'unless))
                      body))
        (or (and (typep item '(cons (eql if)
                                    (cons t (cons t (or null (cons t null))))))
                 ;; Look for IF forms in which either consequent is a branch.
                 (destructuring-bind (test then &optional else) (cdr item)
                   (multiple-value-bind (op branch fallthru)
                       (cond ((go-p then) (values 'when then else))
                             ((go-p else) (values 'unless else then)))
                     (when op
                       (new-body `(,op ,test ,branch))
                       (when fallthru
                         (new-body
                          (if (atom fallthru) `(progn ,fallthru) fallthru)))
                       t))))
            ;; Transform a WHEN or UNLESS if there is more than one subform
            ;; in the consequent, and the last subform is a control transfer.
            ;; This is necessary because the tagbody handler recognizes
            ;; only (WHEN|UNLESS c (GO tag)) as a fast conditional GO.
            ;; (WHEN (TEST) (stmt1) ... (stmtN) (GO tag)) becomes
            ;;   (WHEN (WHEN (TEST) (stmt1) ... (stmtN) T) (GO tag))
            ;; (UNLESS (TEST) (stmt1) ... (stmtN) (GO tag)) becomes
            ;;   (WHEN (UNLESS (TEST) (stmt1) ... (stmtN) T) (GO tag))
            (and (typep item '(cons (member when unless)))
                 (cdddr item)
                 (awhen (go-p (car (last item)))
                   (new-body
                    `(when (,(car item) ,(cadr item) ,@(butlast (cddr item)) t)
                       (go ,(car it))))))
            (new-body item))) ; everything else
      (let ((body (or (new-body) body)))
        ;; Next assign tags their indices. Interleaving this with the final
        ;; pass would need a fixup step for forward branches, so do this first.
        (let ((line-num 1))
          (dolist (item body)
            (if (atom item)
                (rplacd (assoc item (tags)) line-num)
                (incf line-num))))
        ;; Collect the executable statements.
        (let ((line-num 1)
              (lines (make-array (- (1+ (length body)) (length (tags))))))
          (setf (aref lines 0) (tags))
          (dolist (form body (handler #'eval-tagbody lines))
            (unless (atom form)
              (setf (aref lines (prog1 line-num (incf line-num)))
                    (acond ((go-p form) (cdr it)) ; just the line number
                           ((and (typep form '(cons (member when unless)))
                                 (singleton-p (cddr form))
                                 (go-p (third form)))
                            (let ((line (cdr it)))
                              (cons (%sexpr (second form))
                                    (if (eq (car form) 'when) line (- line)))))
                           (t (%sexpr form)))))))))))

(defun eval-tagbody (code env sexpr)
  (declare (simple-vector code) (ignore sexpr)
           #.+handler-optimize+)
  ;; Cons a fresh object as the catch tag.
  (let* ((env (make-tagbody-env env (list (svref code 0)) nil *vacuous-decls*))
         ;; If the element 1 is an unconditional GO label, such as in a
         ;; trailing-test loop, then START is that value, otherwise it is 1.
         (start (let ((elt (svref code 1)))
                  (if (fixnump elt) elt 1)))
         (end (length code)))
    (declare (index start end))
    (loop
       (setq start
             (catch (env-payload env)
               (let ((line-num start))
                 (declare (index line-num))
                 (loop
                    (when (>= line-num end)
                      (return line-num))
                    (setq
                     line-num
                     (let ((line (svref code line-num)))
                       (cond ((%instancep line)
                              (dispatch line env)
                              (1+ line-num))
                             ((fixnump line) line) ; unconditional GO
                             (t ; otherwise, a conditional GO
                              (let ((test (dispatch (car line) env))
                                    (target (the fixnum (cdr line))))
                                (if (minusp target)
                                    (if test (1+ line-num) (- target))
                                    (if test target (1+ line-num))))))))))))
       (if (>= start end) (return)))))

;;; There is no immediate mode for TAGBODY because its primary use is for loops,
;;; which benefit from preprocessing even when used at toplevel and discarded.
(defspecial tagbody (&rest body)
  :deferred (env) (parse-tagbody body env))

;;; As a corollary to above, there's no immediate mode processor for GO.
(defspecial go (tag)
  :deferred (env)
  (let ((depth 0) cell)
    (declare (fixnum depth))
    (loop (typecase env
            (null (ip-error "~@<Attempt to GO to nonexistent tag: ~S~:@>" tag))
            (tagbody-env
             (when (setq cell (assoc tag (car (env-payload env)))) (return))))
       (setq env (env-parent env)) (incf depth))
    (let ((index (make-frame-ptr (cdr cell) depth)))
      (hlambda GO (index) (env)
        (throw (env-payload (env-ancestor env index))
          (frame-ptr-cell-index index))))))

;;; FIXME: this special case makes it a bit difficult to turn the intepreter
;;; into exactly a minimal compiler, since SETQ always wants to do something
;;; that has an immediate effect.
(defun eval-setq (assignments env sexpr) ; SEXPR is nil for immediate mode
  (cond ((not assignments)
         (when sexpr
           (setf (sexpr-handler sexpr) (return-constant nil)))
         nil)
        ((oddp (list-length assignments)) (ip-error "Bad syntax in SETQ"))
        ((cddr assignments)
         (let ((form `(progn ,@(loop for (sym val) on assignments by #'cddr
                                     collect `(setq ,sym ,val)))))
           (if sexpr
               (digest-form form env sexpr)
               (%eval form env))))
        (t
         (let ((sym (first assignments)) (val (second assignments)))
           (unless (symbolp sym)
             (ip-error "~S is not a symbol" sym))
           (if sexpr
               (deferred-setq-1 sym val env sexpr)
               (immediate-setq-1 sym val env))))))

;;;; Comment from 'full-eval'
;;; The expansion of SB-SYS:WITH-PINNED-OBJECTS on GENCGC uses some
;;; VOPs which can't be reasonably implemented in the interpreter. So
;;; we special-case the macro.
;;;
;;; Unlikely to appear at toplevel, so should not occur in :IMMEDIATE mode.
;;; If it does, just dispatch to the deferred handler.
(defspecial sb-sys:with-pinned-objects (objects &body forms)
  :deferred ()
  (let ((objects (mapcar #'%sexpr objects)) (forms (%progn forms)))
    (hlambda sb-sys:with-pinned-objects (objects forms) (env)
      (labels ((recurse (list forms)
                 (if (not list)
                     (dispatch forms env)
                     (sb-sys:with-pinned-objects ((car list))
                       (recurse (cdr list) forms)))))
        (recurse objects forms)))))

;;; Now for the complicated stuff, starting with the simplest
;;; of the complicated ...

(defun digest-locally (env body)
  (multiple-value-bind (forms decls) (parse-body body nil t)
    (if (not decls)
        (digest-progn forms)
        (let* ((specials (free-specials env decls))
               (scope (process-typedecls
                       (%make-local-scope decls (new-policy env decls)
                                          (%progn forms) specials)
                       env 0 specials)))
          (hlambda LOCALLY (scope) (env)
            (enforce-types scope env)
            (dispatch (local-scope-body scope)
                      (make-basic-env env nil
                                      (local-scope-specials scope) scope)))))))

(defspecial locally (&body body)
  :immediate (env)
  (multiple-value-bind (forms decls) (parse-body body nil t)
    (let* ((specials (free-specials env decls))
           (scope (process-typedecls
                   (make-decl-scope decls (new-policy env decls))
                   env 0 specials)))
      (enforce-types scope env)
      (eval-progn forms (make-basic-env env nil specials scope))))
  :deferred (env) (digest-locally env body))

(defun parse-symbol-macrolet (env bindings body wrap-fn)
  (multiple-value-bind (forms decls) (parse-body body nil t)
    (binding* (((specials n-specials) (declared-specials decls))
               (n-macros (length bindings))
               (symbols (make-array (+ n-macros n-specials)))
               (expansions (make-array n-macros))
               (checker (sb-c::symbol-macrolet-definitionize-fun :eval))
               (index -1))
      (with-package-lock-context (env)
        (dolist (binding bindings)
          (let* ((binding (funcall checker binding))
                 (symbol (car binding))
                 (expansion (cddr binding)))
            (setf (svref symbols (incf index)) (list symbol)
                  (svref expansions index) expansion)))
        (assert-declarable-as-special env specials))
      (dolist (symbol specials)
        (if (find symbol symbols :end n-macros :key #'car)
            (ip-error "~S can not be both a symbol-macro and special" symbol))
        (setf (svref symbols (incf index)) (find-special-binding env symbol)))
      (process-typedecls
       (%make-symbol-macro-scope decls (new-policy env decls)
                                 symbols expansions (funcall wrap-fn forms))
       env n-macros symbols))))

(macrolet ((new-env ()
             `(make-symbol-macro-env env
                                     (symbol-macro-expansions scope)
                                     (symbol-macro-symbols scope)
                                     scope)))
  (defspecial symbol-macrolet (defs &rest body)
    :immediate (env)
    (let ((scope (parse-symbol-macrolet env defs body #'identity)))
      (enforce-types scope env)
      (eval-progn (symbol-macro-body scope) (new-env)))
    :deferred (env)
    (if (not defs)
        (digest-locally env body)
        (let ((scope (parse-symbol-macrolet env defs body #'%progn)))
          (hlambda SYMBOL-MACROLET (scope) (env)
            (enforce-types scope env)
            (dispatch (symbol-macro-body scope) (new-env)))))))

(defun parse-let (maker env bindings body specials-listifier)
  (multiple-value-bind (forms decls) (parse-body body nil t)
    (binding* (((declared-specials n-declared) (declared-specials decls))
               (n-bindings 0)
               (n-free-specials
                (let ((boundp 0)) ; mask over declared-specials of bound ones
                  (dolist (binding bindings (- n-declared (logcount boundp)))
                    (let ((p (posq (binding-symbol binding) declared-specials)))
                      (when p (setf (logbitp p boundp) t)))
                    (incf n-bindings))))
               (symbols (make-array (+ n-bindings n-free-specials)))
               (values (make-array n-bindings))
               (index -1))
      (dolist (binding bindings)
        (multiple-value-bind (symbol value-form)
            (if (atom binding)
                (values binding nil)
                (with-subforms (symbol &optional value) binding
                  (values symbol value)))
          (unless (symbolp symbol)
            (ip-error "~S is not a symbol" symbol))
          (incf index)
          (setf (svref symbols index) (list symbol)
                (svref values index) value-form)))
      (dolist (sym declared-specials)
        (unless (find (the symbol sym) symbols :end n-bindings :key #'car)
          (setf (svref symbols (incf index)) (find-special-binding env sym))))
      (let ((special-b ; mask over symbols of special ones
             (mark-bound-specials env declared-specials symbols n-bindings)))
        (process-typedecls
         (funcall maker decls (new-policy env decls) symbols special-b
                  values (%progn forms)
                  (funcall specials-listifier
                           (collect-progv-symbols symbols n-bindings
                                                  special-b)))
         env n-bindings symbols)))))

(defglobal *let-processor* nil)

(macrolet
    ((defspecial* (operator transform-specials)
       `(defspecial ,operator (bindings &body body)
          :deferred (env)
          (unless bindings
            (return-from ,operator (digest-locally env body)))
          ;; For 1 binding always use LET which is semantically the same.
          ;; It is ever-so-slightly more efficient to use LET than LET*.
          ,@(when (eq operator 'let*)
              `((when (singleton-p bindings)
                  (return-from let*
                   (funcall *let-processor* `(,bindings ,@body) env)))))
          (let ((frame (parse-let ',(symbolicate "MAKE-" operator "-FRAME")
                                  env bindings body ,transform-specials)))
            (let ((values (frame-values frame)))
              (map-into values #'%sexpr values))
            (symbol-macrolet ((symbols (frame-symbols frame)))
              (cond ((zerop (frame-special-b frame))
                     (hlambda ,operator (frame) (old-env)
                       (let ((values (make-array (frame-size frame))))
                         (eval-it (frame-values frame) dispatch
                                  (dispatch (frame-sexpr frame) new-env)
                                  :specialp nil))))
                    ((fixnump (frame-special-b frame))
                     (symbol-macrolet
                         ((special-b (the fixnum (frame-special-b frame))))
                       (hlambda ,(sb-int:symbolicate operator "/SPEC")
                                (frame) (old-env)
                         (let ((values (make-array (frame-size frame)))
                               (specials (frame-specials frame)))
                           (eval-it (frame-values frame) dispatch
                                    (dispatch (frame-sexpr frame) new-env))))))
                    (t ; a ton of specials
                     (symbol-macrolet ((special-b (frame-special-b frame)))
                       (hlambda ,(sb-int:symbolicate operator "/SPEC")
                                (frame) (old-env)
                         (let ((values (make-array (frame-size frame)))
                               (specials (frame-specials frame)))
                           (eval-it (frame-values frame) dispatch
                                    (dispatch (frame-sexpr frame)
                                              new-env))))))))))))
  (macrolet
      ((eval-it (value-source-v eval-fn eval-body &rest more)
         `(let ((new-env (make-var-env old-env values symbols frame))
                (type-restrictions (binding-typechecks frame)))
            (with-let-bindings
                (values (length values) ,@more
                 :value (enforce-type
                         (,eval-fn (svref ,value-source-v frame-index) old-env)
                         type-restrictions frame-index symbols)
                 :specials specials)
              (enforce-types frame env) ; the OLD env
              ,eval-body))))
    ;; WITH-LET-BINDINGS accumulates special bindings in reverse
    ;; so we have to reverse the symbols too.
    (defspecial* LET #'nreverse))

  (macrolet
      ((eval-it (value-source-v eval-fn eval-body &rest more)
         ;; (length . vector) is a proxy for a vector with fill-pointer
         `(let* ((symbol-cells (cons 0 symbols))
                 (new-env (make-var-env old-env values symbol-cells frame))
                 (type-restrictions (binding-typechecks frame)))
            (with-let*-binder
                (values (car symbol-cells) ,@more
                 :value (enforce-type
                         (,eval-fn (svref ,value-source-v frame-index) new-env)
                         type-restrictions frame-index symbols)
                 :specials (pop specials))
              ;; expression to start the recursion
              (let*-bind 0 (length values))
              ;; post-binding actions
              ;; FIXME: this step is possibly not thread-safe. Figure it out.
              ;; Maybe "once a mutable env, always a mutable env"?
              (setf (env-symbols new-env) symbols)
              (enforce-types frame env) ; the OLD env
              ,eval-body))))
    ;; WITH-LET*-BINDER needs each special in a singleton list
    (defspecial* LET* (lambda (x) (mapcar #'list x)))))

(setq *let-processor* (cdr (!special-form-handler (find-fdefn 'let))))

;;;; Macrolet

;;; Unlike with local functions, this encloses the macro lambda in the
;;; parse-time environment which is why this is EVAL- and not DIGEST.
;;; It also does not go to the trouble of hiding (making inaccessible)
;;; any lexical names that should not be visible.
;;; This is permissible per CLHS in the description of MACROLET:
;;;   "the consequences are undefined if the local macro definitions
;;;    reference any local variable or function bindings that are
;;;    visible in that lexical environment."
;;; This means: you're wrong for doing that; but in this implementation
;;; your macro functions can look at things they shouldn't.
;;;
(defun eval-local-macros (env defs)
  ;; Is it necessary to freeze a macro env? You shouldn't look at
  ;; any lexical constructs except macros anyway.
  (when (must-freeze-p env) (setq env (freeze-env env)))
  (map 'vector
       (lambda (def)
         (with-subforms (name lambda-list &body body) def
           (unless (and (symbolp name)
                        (neq (info :function :kind name) :special-form))
             (ip-error "~S is not a valid macro name" name))
           (when (fboundp name)
             (with-package-lock-context (env)
               (program-assert-symbol-home-package-unlocked
                :eval name "binding ~S as a local macro")))
           (make-function
            (make-proto-fn (make-macro-lambda `(macrolet ,name) lambda-list body
                                              'macrolet name))
            env)))
       (the list defs)))

;;; MACROLET has an immediate-mode handler since it is common at toplevel.
(defspecial macrolet (defs &body body)
  :immediate (env)
  (multiple-value-bind (forms decls) (parse-body body nil t)
    (let* ((specials (free-specials env decls))
           (scope (process-typedecls
                   (make-decl-scope decls (new-policy env decls))
                   env 0 specials)))
      (enforce-types scope env)
      (eval-progn forms
                  (make-macro-env env
                                  (eval-local-macros env defs)
                                  specials scope))))
  :deferred (env)
  (if (not defs)
      (digest-locally env body)
      (multiple-value-bind (forms decls) (parse-body body nil t)
        (let ((scope (make-local-fn-scope decls (eval-local-macros env defs)
                                          forms env)))
          (hlambda MACROLET (scope) (env)
            (enforce-types scope env)
            (dispatch
             (local-fn-scope-body scope)
             (make-macro-env env
                             (local-fn-scope-funs scope)
                             (local-fn-scope-specials scope)
                             scope)))))))

;;;; FLET and LABELS

(defun digest-local-fns (env kind bindings body) ; KIND is FLET or LABELS
  (flet ((proto-functionize (def)
           (with-subforms (name lambda-list &body body) def
             (multiple-value-bind (forms decls docstring) (parse-body body t t)
               ;; *** Test LEGAL-FUN-NAME-P before asking for an info-value.
               ;; This is because (INFO :FUNCTION :KIND) uses FBOUNDP when it
               ;; does not have an entry for NAME. But calling FBOUNDP
               ;; might call LEGAL-FUN-NAME-OR-TYPE-ERROR.
               ;; This ends up producing a different error message instead
               ;; of a consistent message as per the format string below.
               (unless (and (legal-fun-name-p name)
                            (neq (info :function :kind name) :special-form))
                 (ip-error "~S is not a legal function name" name))
               (when (fboundp name)
                 (with-package-lock-context (env)
                   (program-assert-symbol-home-package-unlocked
                    :eval name "binding ~S as a local function")))
               ;; Return T if SEXPR contains (RETURN-FROM BLOCK-NAME ...),
               ;; or NIL if it definitely does not. While this is an inelegant
               ;; kludge, it is extremely effective. Avoiding creation of a
               ;; CATCH frame, makes local fun application properly
               ;; tail recursive, barring other inhibitors.
               ;; FIXME: Macros can be nondeterministic :-( so if any macro
               ;; in a non-builtin package is seen, conservatively return T.
               (flet ((has-return-p (sexpr env block-name)
                        (handler-case
                            (progn
                              (sb-walker:walk-form
                               sexpr env
                               (lambda (subform context env)
                                 (declare (ignore env))
                                 (when (and (eq context :eval)
                                            (typep subform
                                                   '(cons (eql return-from) list))
                                            (eq (cadr subform) block-name))
                                   (return-from has-return-p t))
                                 subform))
                              nil)
                          (condition () t)))) ; conservative answer
                 (let* ((block (fun-name-block-name name))
                        (forms (if (has-return-p `(progn ,@forms) env block)
                                   `((block ,block ,@forms))
                                   forms)))
                   (%make-proto-fn `(,kind ,name) lambda-list decls forms
                                   docstring)))))))
    (multiple-value-bind (forms decls) (parse-body body nil t)
      (make-local-fn-scope decls (map 'vector #'proto-functionize bindings)
                           forms env))))

;;; There's an improvement that could be made - unless a function name
;;; appears as the operatnd to FUNCTION, all internals call could use
;;; different convention for function application which avoids consing
;;; a funcallable instance. But you can't now that without walking into the
;;; body, which means you lose the otherwise nice laziness aspect.
(macrolet ((defspecial* (operator)
             `(defspecial ,operator (defs &body body)
                :deferred (env)
                (cond ((not defs) (digest-locally env body))
                      ((not body) (return-constant nil))
                      (t
                       (let ((frame (digest-local-fns env ',operator defs body)))
                         (if (must-freeze-p env)
                             (handler-guts (freeze-env env))
                             (handler-guts env))))))))

  (macrolet ((handler-guts (closed-over-env)
               `(hlambda FLET (frame) (env)
                  (enforce-types frame env)
                  (let* ((closure-env ,closed-over-env)
                         (funs
                          (map 'vector
                               (lambda (proto-fn)
                                 (make-function proto-fn closure-env))
                               (local-fn-scope-funs frame))))
                    (dispatch
                     (local-fn-scope-body frame)
                     (make-function-env
                      env funs (local-fn-scope-specials frame) frame))))))
    (defspecial* flet))

  (macrolet ((handler-guts (closed-over-env)
               `(hlambda LABELS (frame) (env)
                  (enforce-types frame env)
                  (let* ((funs (make-array (length (local-fn-scope-funs frame))))
                         (closure-env (make-function-env ,closed-over-env funs
                                                         nil *vacuous-decls*)))
                    (map-into funs (lambda (proto-fn)
                                     (make-function proto-fn closure-env))
                              (local-fn-scope-funs frame))
                    (dispatch
                     (local-fn-scope-body frame)
                     ;; In the absence of decls, the env in which the body
                     ;; is eval'ed is the same as the functions' env.
                     ;; Otherwise, it is yet another new env.
                     (if (declarations frame)
                         (make-basic-env closure-env nil
                                         (local-fn-scope-specials frame) frame)
                         closure-env))))))
    (defspecial* labels)))

(macrolet ((not-a-function (name)
             `(ip-error "~S is a macro." ,name)))
  (defspecial function (name)
    (if (and (symbolp name) (eq (info :function :kind name) :special-form))
        (ip-error "~S names a special operator." name))
    ;; again it's sad that I can't wrap code around both modes
    :immediate (env)
    (if (and (listp name) (memq (car name) '(named-lambda lambda)))
        ;; Defining a function at toplevel (as is almost always the case)
        ;; needs to capture the current global policy.
        (enclose (make-proto-fn name (not (null env)))
                 ;; No parent, no payload, no symbols, no declarations.
                 (or env (make-basic-env nil nil nil
                                         (make-decl-scope nil sb-c::*policy*)))
                 nil)
        ;; immediate mode calls FDEFINITION as needed, so wil err as it should.
        (multiple-value-bind (definition macro-p) (get-function name env)
          (if macro-p (not-a-function name) definition)))
    :deferred (env)
    (if (and (listp name) (memq (car name) '(named-lambda lambda)))
        (handler (if (must-freeze-p env) #'enclose-freeze #'enclose)
                 (make-proto-fn name))
        (multiple-value-bind (kind definition frame-ptr)
            (find-lexical-fun env name)
          (if definition
              (if (eq kind :macro)
                  (not-a-function name)
                  (hlambda FUNCTION (frame-ptr) (env)
                    (local-fdefinition frame-ptr env)))
              ;; Consider (DEFUN GET-THING () #'THING) - it shouldn't return
              ;; THING's error trampoline if THING is redefined after
              ;; GET-THING was called once.
              (let ((fdefn (find-or-create-fdefn name)))
                (if (symbolp name) ; could be a macro
                    (hlambda FUNCTION (fdefn) (env)
                      (declare (ignore env))
                      (let ((fun (sb-c:safe-fdefn-fun fdefn)))
                        (if (%looks-like-macro-p fun)
                            (not-a-function (fdefn-name fdefn))
                            fun)))
                    (hlambda FUNCTION (fdefn) (env) ; could not be a macro
                      (declare (ignore env))
                      (sb-c:safe-fdefn-fun fdefn)))))))))

;;;; some extra handlers

#+nil
(macrolet
    ((define-boole (op identity test)
       `(defspecial ,op (&rest forms)
         :immediate :none ; just recurse
         :deferred ()
         ;; following the model of PROGN
         (case (length forms)
           (0 (return-constant ,identity))
           (1 (handler #'digest-form (first forms)))
           (2 (let ((one (%sexpr (first forms))) (two (%sexpr (second forms))))
                (hlambda ,op (one two) (env)
                  (,op (dispatch one env) (dispatch two env)))))
           (t
            (let ((forms (nreverse (vector-of-sexpr forms))))
              (hlambda ,op (forms) (env)
                (declare (simple-vector forms))
                (let ((i (1- (length forms))))
                  (loop (let ((result (dispatch (svref forms i) env))) ,test)
                     (if (zerop (decf i))
                         (return (dispatch (svref forms 0) env))))))))))))
  (define-boole OR nil (when result (return result)))
  (define-boole AND t (unless result (return nil))))

;;;; CL functions that are handled semi-magically

(flet ((local-dispatch (sexpr env)
         (declare #.+handler-optimize+)
         (if (%instancep sexpr) (%dispatch sexpr env) sexpr)))

 ;; hand-generated lists of useful functions
 (setq *unary-functions*
  (sb-impl::%stuff-hash-table
   (make-hash-table :test #'eq)
   (macrolet
       ((def-wrapper (&rest input)
          (cons 'list
                (mapcar
                  (lambda (f)
                    `(cons ',f
                           (named-lambda (EVAL ,f) (arg env sexpr)
                             (declare (ignore sexpr)
                                      (optimize (sb-c:verify-arg-count 0))
                                      (muffle-conditions compiler-note))
                             (,f (local-dispatch arg env)))))
                  input))))
    (def-wrapper
      ;; non-alphabetic
      - / /= 1+ 1- < <= = > >=
      ;; CxR
      CAR CDR CAAR CADR CDAR CDDR
      CAAAR CAADR CADAR CADDR CDAAR CDADR CDDAR CDDDR
      CAAAAR CAAADR CAADAR CAADDR CADAAR CADADR CADDAR CADDDR
      CDAAAR CDAADR CDADAR CDADDR CDDAAR CDDADR CDDDAR CDDDDR
      ;; other list accessors
      FIRST REST
      SECOND THIRD FOURTH FIFTH SIXTH SEVENTH EIGHTH NINTH TENTH
      ;; alphabetical order from here down
      ABS
      ADJUSTABLE-ARRAY-P
      ALPHA-CHAR-P ALPHANUMERICP
      ARRAY-DIMENSIONS ARRAY-ELEMENT-TYPE ARRAY-HAS-FILL-POINTER-P
      ARRAY-IN-BOUNDS-P ARRAY-RANK ARRAY-TOTAL-SIZE ARRAYP
      ATOM
      BIT-NOT BIT-VECTOR-P BOTH-CASE-P BOUNDP
      BUTLAST BYTE-POSITION BYTE-SIZE
      CEILING CHAR-CODE CHAR-DOWNCASE CHAR-EQUAL CHAR-GREATERP
      CHAR-INT CHAR-LESSP CHAR-NOT-EQUAL
      CHAR-NOT-GREATERP CHAR-NOT-LESSP CHAR-UPCASE
      CHAR/= CHAR< CHAR<= CHAR= CHAR> CHAR>=
      CHARACTER CHARACTERP CLASS-NAME CLASS-OF
      CODE-CHAR COMPILED-FUNCTION-P COMPLEMENT
      COMPLEX COMPLEXP CONSP
      DENOMINATOR
      DIGIT-CHAR DIGIT-CHAR-P
      ENDP EVENP FBOUNDP FDEFINITION FIND-SYMBOL
      FLOAT FLOATP FLOOR FUNCALL FUNCTIONP
      GRAPHIC-CHAR-P
      HASH-TABLE-P
      IDENTITY
      INTEGER-LENGTH
      INTEGERP
      INTERN
      ISQRT
      KEYWORDP
      LAST LENGTH LIST* LIST-LENGTH LISTP
      LOGCOUNT LOGNOT LOWER-CASE-P
      MAX MIN MINUSP
      NBUTLAST NOT NREVERSE
      NSTRING-CAPITALIZE NSTRING-DOWNCASE NSTRING-UPCASE
      NULL NUMBERP NUMERATOR
      ODDP
      PACKAGEP PATHNAMEP PLUSP
      REALP REVERSE ROUND
      SBIT SIGNUM
      SIMPLE-BIT-VECTOR-P SIMPLE-STRING-P SIMPLE-VECTOR-P
      SPECIAL-OPERATOR-P SQRT
      STANDARD-CHAR-P
      STREAMP STRING
      STRING-CAPITALIZE STRING-DOWNCASE STRING-UPCASE STRINGP SXHASH
      SYMBOL-FUNCTION SYMBOL-NAME SYMBOL-PACKAGE SYMBOL-PLIST
      SYMBOL-VALUE SYMBOLP
      TRUNCATE TYPE-OF
      UPPER-CASE-P
      VALUES-LIST
      VECTOR-POP
      VECTORP
      ZEROP))))

 (setq *binary-functions*
  (sb-impl::%stuff-hash-table
   (make-hash-table :test #'eq)
   (macrolet
       ((def-wrapper (&rest input)
          (cons 'list
                (mapcar
                 (lambda (f)
                   `(cons ',f
                          (named-lambda (.eval. ,(symbolicate "2-ARG-" f))
                                        (data env sexpr)
                            (declare (ignore sexpr)
                                     (optimize (sb-c:verify-arg-count 0))
                                     (muffle-conditions compiler-note))
                            (,f (local-dispatch (car data) env)
                                (local-dispatch (cdr data) env)))))
                 input))))
   (def-wrapper + - * / = < > <= >= min max
                string= string< string<= string> string>=
                char= char< char<= char> char>=
                eq eql equal equalp
                cons list list*))))
) ; end FLET
