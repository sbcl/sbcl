;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-INTERPRETER")

;;;; This interpreter is a hybrid of a traditional recursive descent EVAL,
;;;; augmented with some semantic preprocessing. But unlike most other
;;;; preprocessing implementations which produce code which emulates compiled
;;;; code as closely as possible by performing macroexpansion once only,
;;;; we attempt to emulate a non-preprocessing interpreter.
;;;; The motivation for this is parenthetically revealed in the X3J13 issue
;;;; discussing the removal of COMPILER-LET, saying:
;;;;   "Some users have indicated they dislike interpreters which do a semantic
;;;;    prepass, because they like to be able to dynamically redefine macros
;;;;    while debugging."
;;;; In addition, preprocessing of a form is done as late as possible -
;;;; only when a form is reached - and only enough to descend one level.

;;; SEXPRs are the basic building blocks of interpreted code.
;;; We store a function to call (in the HANDLER) as well as the original
;;; list representation of the form in case it has to be re-preprocessed.
(defstruct (sexpr (:conc-name sexpr-)
                  (:constructor %make-sexpr (handler %form))
                  (:copier nil))
  (handler nil :type cons)
  (%form nil :read-only t)) ; the original source form

;;; A SEXPR's HANDLER is a cons. This constructs a HANDLER.
(defun %handler (function payload) (cons function payload))

;;; Not all "sexprs" are actually instances of SEXPR,
;;; because self-evaluating constants are stored as-is.
;;; Get the original source out of a SEXPR-equivalent object.
(defun sexpr-form (object)
  (if (%instancep object)
      (progn (aver (sexpr-p object)) (sexpr-%form object))
      object))

;;; Wrapper to hide the argument passing convention.
;;; This is preferable to using a closure over the handler-function and args
;;; because it makes for better inspectability and is often more compact.
;;; The smallest closure requires 4 words; this has a lower bound of 2 words.
;;;
;;; FIXME: the preceding claim about inspectability is no longer true now that
;;; we have STORE-CLOSURE-DEBUG-POINTER which can preserve both the lexical
;;; and dynamic state. And the efficiency claim might never have been true.
(defmacro handler (function &rest args)
  (cond ((not args)
         `(%handler ,function 0))
        ((not (cdr args))
         `(%handler ,function ,(first args)))
        ((not (cddr args))
         `(%handler ,function (cons ,(first args) ,(second args))))
        (t
         `(%handler ,function (vector ,@args)))))

(defmacro with-form-data (names input &body body)
  (declare (list names))
  `(symbol-macrolet
       ,(case (length names)
         (1 `((,(first names) ,input)))
         (2 `((,(car names) (car ,input)) (,(cadr names) (cdr ,input))))
         (t (loop for name in names for i from 0
                  collect `(,name (svref ,input ,i)))))
     ,@body))

;;; Wrapper on LAMBDA for writing handlers.
;;; (HLAMBDA NAME (DATA ENV SEXPR) ...)
;;;  -> (NAMED-LAMBDA (EVAL NAME) (#:DATA ENV SEXPR)
;;;       (WITH-FORM-DATA DATA #:DATA ...)
;;;
;;; If DATA is a symbol, it is used literally as the first formal parameter
;;; in the lambda; if it is a list, the lambda receives one thing which is
;;; destructured according to the argument passing convention.
;;;
;;; If SEXPR is omitted it becomes a gensym which gets declared ignore.
;;; All handlers have identical signature but most don't use the third argument.
;;; Only a handler which can restart by discarding internal state needs it.
;;;
(defmacro hlambda (name captured-vars vars &body body)
  (declare (list captured-vars))
  (let ((data (gensym "DATA")) (ignore))
    ;; Allow &AUX vars: (HLAMBDA NAME (DATA ENV &AUX FOO) ...) has no 3rd arg.
    (let ((aux-vars (member '&aux vars)))
      (when (= (length (ldiff vars aux-vars)) 1)
        (let ((sexpr (make-symbol "SEXPR")))
          (setq vars (nconc (subseq vars 0 1) (list sexpr) aux-vars)
                ignore `((ignore ,sexpr))))))
    (setq name (cons '.eval. (if (listp name) name (list name))))
    `(handler ,(if captured-vars
                   (if (= (length captured-vars) 1)
                       `(named-lambda ,name (,(first captured-vars) ,@vars)
                          (declare ,+handler-optimize+ ,@ignore)
                          ,@body)
                       `(named-lambda ,name (,data ,@vars)
                          (declare ,+handler-optimize+ ,@ignore)
                          (with-form-data ,captured-vars ,data ,@body)))
                  `(named-lambda ,name (,data ,@vars)
                     (declare ,+handler-optimize+ (ignore ,data) ,@ignore)
                     ,@body))
              ,@captured-vars)))

;;; DISPATCH receives a SEXPR object or sexpr-equivalent object.
;;; If a SEXPR, its handler is called to produce a value. All other Lisp
;;; objects are treated as constants.
;;; N.B.: herein SEXPR sometimes mean either a SEXPR object, or a constant
;;; object that needs no wrapping. The union of those could be envisioned
;;; as a deftype DISPATCHABLE - which is nowise helpful in practice.
;;;
;;; It is always ok to call DISPATCH on a SEXPR-equivalent object, but in two
;;; places %DISPATCH is used to avoid a spurious code-deletion note where
;;; SEXPR is known to be a dispatchable instance. This is one of the
;;; "spurious but not wrong" notes that the compiler can emit.
(declaim (inline dispatch %dispatch))
(defun %dispatch (sexpr env)
  (declare #.+handler-optimize+)
  (let ((handler (sexpr-handler sexpr)))
    (funcall (the function (car handler)) (cdr handler) env sexpr)))
(defun dispatch (sexpr env)
  (declare #.+handler-optimize+)
  (if (%instancep sexpr) (%dispatch sexpr env) sexpr))

;;; Helper for RETURN-CONSTANT.
;;; Would be a local function except for some uses of LOAD-TIME-VALUE.
(defun %const (x env sexpr)
  (declare (ignore env sexpr) #.+handler-optimize+)
  x)

;;; Return a handler that returns a constant.
;;; The %SEXPR constructor elides a handler for constants,
;;; but there are cases where NIL sneaks through and demands a callable handler.
;;; We avoid generating N copies of such handler. Same goes for 0 and T.
(defun return-constant (object)
  (cond ((null object) (load-time-value (handler #'%const nil)))
        ((eq object t) (load-time-value (handler #'%const t)))
        ((eql object 0) (load-time-value (handler #'%const 0)))
        (t (handler #'%const object))))

(defun return-constant-values (values env sexpr)
  (declare (ignore env sexpr) #.+handler-optimize+)
  (values-list values))

;; Forward defs
(declaim (ftype function digest-form %eval eval-setq))

;;; Return a SEXPR object that will, when dispatched, evaluate FORM.
;;; Unlike optimizations on SEXPRS whereby they install the most-specific
;;; applicable handler for a given source form when first dispatched, constants
;;; can't do that because by the time DIGEST-FORM is dispatched, it's too late
;;; to install a function other than #'RETURN-CONSTANT. The goal here is to
;;; avoid funcalling anything on constant forms. Therefore:
;;; 1. For most self-evaluating objects, FORM itself is returned.
;;;    Structures are self-evaluating, but have to be wrapped because DISPATCH
;;;    assumes that anything satisfying %INSTANCEP is a dispatchable object.
;;; 2. If FORM is a syntactically correct QUOTE form, return the quoted thing
;;;    subject to the constraint in condition 1. SEXPRs are prepared in advance
;;;    of being EVALed, so no errors may be signaled on malformed QUOTE forms.
;;;
;;; Todo: there is still more opportunity for lifting singleton PROGN forms.
;;; Lots of standard macros expand into (PROGN (ONE-THING)), which, if it appears
;;; as the argument to %SEXPR should just become a SEXPR for (ONE-THING).
;;;
(defun %sexpr (form)
  (let (cdr obj)
    (cond ((and (atom form)
                (not (%instancep form))
                (or (not (symbolp form))
                    ;; Only builtin constants and keywords are assumed
                    ;; to be written in stone. Others generate a continuable
                    ;; error on redefinition, and this eval will always
                    ;; get the current definition.
                    (and (eq (info :variable :kind form) :constant)
                         (memq (symbol-package form)
                               (load-time-value (list (find-package "KEYWORD")
                                                      (find-package "CL"))))
                         (progn (setq form (symbol-value form)) t))))
           form)
          ((and (consp form)
                (eq (car form) 'quote)
                (consp (setq cdr (cdr form)))
                (null (cdr cdr))
                (not (%instancep (setq obj (car cdr)))))
           obj)
          (t ; DIGEST-FORM is the generic handler
           (%make-sexpr (handler #'digest-form form) form)))))

;;; Return a SEXPR that will evaluate zero or more FORMS.
(defun %progn (forms)
  ;; The handler for PROGN contains code to handle 0, 1, 2, 3+ subforms
  ;; but it's worthwhile to special-case 0 and 1 here too
  ;; because it avoids some consing and preprocessing.
  (%sexpr (if (and (listp forms) (not (cdr forms)))
              (first forms) ; ok if (PROGN) or (PROGN form)
              `(progn ,@forms))))

;;; Return the binding of a lexical function indexed by INDEX in ENV.
;;; INDEX must be the frame-ptr returned from a call to FIND-LEXICAL-FUN.
;;; This is for runtime use by a SEXPR handler.
;;; The name is borrowed from the standard function "FDEFINITION" -
;;; this does not have anything to do with #<fdefn> objects.
(defun local-fdefinition (index env)
  (declare (fixnum index) #.+handler-optimize+)
  (%cell-ref env index))

;;; Return a frame-pointer for FNAME if it is a local function,
;;; or return :MACRO if it names either a local *or* global macro.
;;; Return NIL if none of the above.
(defun local-fn-frame-ptr (fname env)
  (multiple-value-bind (kind definition frame-ptr) (find-lexical-fun env fname)
    (if definition
        (if (eq kind :macro) :macro frame-ptr)
        (if (and (symbolp fname) (macro-function fname)) :macro nil))))

;;; Fast handlers for lexical var access. This is a speed and space win,
;;; the latter since it avoids some consing for small fixed frame-ptr.
;;;
;;; (REF 0 N) -> (SVREF (ENV-VARS ENV) N)
;;; (REF 1 N) -> (SVREF (ENV-VARS (ENV-PARENT ENV)) N)
;;; (REF 2 N) -> (SVREF (ENV-VARS (ENV-PARENT (ENV-PARENT ENV))) N)
;;; etc
;;; A fast assembler could generate accessors on demand - and then I'd cache
;;; them - as the code is entirely boilerplate. Lacking that, hardwire some.
(declaim ((simple-array t (4 10)) *fast-lexvar-reffers* *fast-lexvar-setters*))
(macrolet ((ref (up across)
             (declare (optimize (speed 0))) ; silence the generic math note
             `(svref (env-payload ,(let ((e 'env))
                                    (dotimes (i up e)
                                      (setq e `(env-parent ,e)))))
                     ,across))
           (array-of (n-depths n-cells)
             (declare (optimize (speed 0))) ; silence the generic math note
             `(make-array
               ',(list n-depths n-cells)
               :initial-contents
               (list ,@(loop for depth below n-depths
                             collect
                             `(list ,@(loop for j below n-cells
                                            collect `(access ,depth ,j))))))))
  (define-load-time-global *fast-lexvar-reffers*
    (macrolet ((access (up across)
                 `(hlambda GET-VAR () (env) (ref ,up ,across))))
      (array-of 4 10))) ; for 4 scopes and 10 names per scope you can go fast
  (define-load-time-global *fast-lexvar-setters* ; These are lambdas, not handlers
    (macrolet ((access (up across)
                 `(named-lambda (eval SET-VAR) (form env sexpr)
                    (declare #.+handler-optimize+ (ignore sexpr))
                    (setf (ref ,up ,across) (dispatch form env)))))
      (array-of 4 10))))

(declaim (ftype (function (symbol t t) nil) typecheck-fail typecheck-fail/ref)
         (ftype (function (t &rest t) nil) values-typecheck-fail)
         (ftype (function (function fixnum) nil)
                err-too-few-args err-too-many-args))

(macrolet ((with-indices (special-case general-case)
             `(let ((depth (frame-ptr-depth frame-ptr))
                    (slot (frame-ptr-cell-index frame-ptr)))
                (if (and (< depth (array-dimension *fast-lexvar-reffers* 0))
                         (< slot (array-dimension *fast-lexvar-reffers* 1)))
                    ,special-case
                    ,general-case))))

  ;; Return a handler that accesses a lexical var given its FRAME-PTR.
  ;; Anything not in *FAST-LEXVAR-REFFERS* gets a general-purpose routine.
  (defun lexvar-reffer (frame-ptr type)
    (if type
        ;; There are no specialized reffers with type-checking
        ;; because the check is slow anyway.
        (hlambda GET-VAR (frame-ptr type) (env)
          (let ((val (%cell-ref env frame-ptr)))
            (if (itypep val type)
                val
                (typecheck-fail/ref (frame-symbol env frame-ptr) val type))))
        (with-indices (aref *fast-lexvar-reffers* depth slot)
          (hlambda GET-VAR (frame-ptr) (env) (%cell-ref env frame-ptr)))))

  (defun lexvar-setter (frame-ptr newval type)
    (if type
        ;; There are no no specialized setters with type-checking
        ;; because the check is slow anyway.
        (hlambda SET-VAR (newval frame-ptr type) (env)
          (let ((newval (dispatch newval env)))
            (if (itypep newval type)
                (setf (%cell-ref env frame-ptr) newval)
                (typecheck-fail (frame-symbol env frame-ptr) newval type))))
        (with-indices (handler (aref *fast-lexvar-setters* depth slot) newval)
          (hlambda SET-VAR (newval frame-ptr) (env)
            (setf (%cell-ref env frame-ptr) (dispatch newval env)))))))

;;; Access SYMBOL's dynamic value, enforcing TYPE if non-nil,
;;; and checking for change of its :VARIABLE :KIND if PARANOID,
;;; allowing the possibility that a DEFVAR changed to DEFINE-SYMBOL-MACRO
;;; for example.
(defun specvar-reffer (symbol type &optional paranoid)
  (macrolet ((safe-symbol-value ()
               ;; HLAMBDA lowers safety because the interpreter itself
               ;; is (hopefully) free of bugs, but we always want to check
               ;; that a special variable is BOUNDP.
               '(locally (declare (optimize (safety 3)))
                 (symbol-value symbol))))
    (cond ((and type paranoid)
           (hlambda SYMEVAL (symbol) (env sexpr)
             (if (member (info :variable :kind symbol) '(:alien :macro))
                 (digest-form symbol env sexpr)
                 (let ((val (safe-symbol-value)))
                   (if (itypep val type)
                       val
                       (typecheck-fail/ref symbol val type))))))
          ((and type (not paranoid))
           (hlambda SYMEVAL (symbol type) (env)
             (progn env) ; ignore
             (let ((val (safe-symbol-value)))
               (if (itypep val type)
                   val
                   (typecheck-fail/ref symbol val type)))))
          (paranoid
           (hlambda SYMEVAL (symbol type) (env sexpr)
           (if (member (info :variable :kind symbol) '(:alien :macro))
               (digest-form symbol env sexpr)
               (safe-symbol-value))))
          (t
           (hlambda SYMEVAL (symbol) (env)
             (progn env) ; ignore
             (safe-symbol-value))))))

(defconstant +mode-mask-shift+ 4)
(defconstant +arg-mode-mask+   #b1110)
(defconstant +optional-arg+    #b0000)
(defconstant +keyword-arg+     #b0010)
(defconstant +supplied-p-var+  #b0100)
(defconstant +aux-var+         #b0110)
(defconstant +rest-arg+        #b1000)
;;; This constant MUST be 1 so it can be used as an index skip count
;;; versus having to write (if (logtest foo +has-default+) (incf thing))
(defconstant +has-default+     #b0001)

(defmacro argument-keyword-index (encoding)
  `(ash ,encoding (- +mode-mask-shift+)))

;;; Compress LAMBDA-LIST into a representation that encodes which arguments
;;; have defaulting forms and/or supplied-p variables.
(defun encode-lambda-bindings (lambda-list form-converter)
  (let ((n-optional 0) (n-keys -1) fancy-vars decoder keys)
    (multiple-value-bind (llks required optional rest keyword-args aux)
        (parse-lambda-list lambda-list
                           :accept (lambda-list-keyword-mask
                                    '(&optional &rest &key &allow-other-keys &aux))
                           :silent t)
      ;; Do I need more syntax checking? PARSE-LAMBDA-LIST enforces syntactic
      ;; validity but not that the vars are bindable; however the specialness
      ;; check enforces the latter, so we're probably ok.
      (flet ((add-var (x mode)
               (multiple-value-bind (key var)
                   (if (eq mode +keyword-arg+)
                       (cond ((atom x)        (values (keywordicate x) x))
                             ((consp (car x)) (values (caar x) (cadar x)))
                             (t               (values (keywordicate (car x))
                                                      (car x))))
                       (values nil (if (symbolp x) x (car x))))
                 (let* ((default (if (listp x) (funcall form-converter x)))
                        (encoding (logior mode (if default +has-default+ 0))))
                   (push var fancy-vars)
                   (when (eq mode +keyword-arg+)
                     (setq encoding (logior (ash (incf n-keys) +mode-mask-shift+)
                                            encoding))
                     (push key keys))
                   (push encoding decoder)
                   (when default
                     (push default decoder)))
                 (when (and (/= mode +aux-var+) (proper-list-of-length-p x 3))
                   (push (third x) fancy-vars)
                   (push +supplied-p-var+ decoder)))))
        (dolist (x optional)
          (incf n-optional)
          (add-var x +optional-arg+))
        (when rest
          (push (car rest) fancy-vars)
          (push +rest-arg+ decoder))
        (dolist (x keyword-args)
          (add-var x +keyword-arg+))
        (dolist (x aux)
          (add-var x +aux-var+)))
      (values required (nreverse fancy-vars)
              ;; compute (length keys) before nreconc smashes them
              (make-keyword-bits (length keys) rest
                                 (ll-kwds-keyp llks) (ll-kwds-allowp llks))
              (if (or (ll-kwds-keyp llks) decoder)
                  (coerce (nreconc keys (nreverse decoder)) 'vector))
              n-optional))))

;;; If BODY matches (BLOCK <name> . <forms>) then return the block name, the
;;; forms, and an indicator of whether it is OK to create the block and bind
;;; variables at the same time. If not a block, return 0 and forms unaltered.
;;;
;;; The optimization of eliding an extra ENV is performed only if no nontrivial
;;; defaulting SEXPRs exist. A "trivial" one is either a constant or a reference
;;; to a variable (not a macro). Absent such a thing, nobody could notice that
;;; there aren't actually two different lexical contours, so creating variable
;;; bindings and a block may occur simultaneously.
;;; [In fact, in such case, there is no difference between parallel and serial
;;; binding, so the LET* part of the binding loop could be absorbed by the LET
;;; part, which would have to be informed how to decode a fancy-arg vector]
;;; The test is extremely paranoid by "wrongly" saying that a form can be a
;;; macro despite the lambda list itself creating a binding for the symbol
;;; in question. LAMBDA can cause fewer symbols to be macros, but never more.
;;; Therefore it is fine to disregard the lambda decoder. For example in:
;;;   (symbol-macrolet ((foo (hair))) (lambda (x &optional (foo x)) ...)
;;; FOO is thought to be a macro.
;;;
(defun extract-lambda-block (body decoder env)
  (labels ((definitely-var-ref-p (x)
             (and (symbolp x)
                  (case (nth-value 1 (find-lexical-var env x))
                    ((:normal :special) t)
                    (:macro nil)
                    (t (neq (info :variable :kind x) :macro)))))
           (trivial-expression-p (x)
             (or (not (sexpr-p x)) ; a constant
                 (definitely-var-ref-p (sexpr-form x)))))
  (if (and (listp body)
           (listp (car body))
           (eq (caar body) 'block)
           (not (cdr body))
           (symbolp (cadar body)))
      (let ((body (cdar body))) ; = (BLOCK-NAME . FORMS)
        (values (car body) (cdr body)
                (every #'trivial-expression-p decoder)))
      (values 0 body nil))))

;;; Produce a LAMBDA-FRAME for a PROTO-FN.
;;; ENV is for package-lock checks and also to make a quick guess about
;;; whether a block can be created at the same time as the variable bindings.
(defun digest-lambda (env proto-fn)
  (binding*
      (((required-args other-args keyword-bits decoder n-opt)
        (encode-lambda-bindings (proto-fn-lambda-list proto-fn)
                                (lambda (x) (%sexpr (second x)))))
       (decls (proto-fn-decls proto-fn))
       (n-required (length required-args))
       (n-other (length other-args))
       (n-lambda-vars (+ n-required n-other))
       (declared-specials (declared-specials decls))
       (free-specials
        (remove-if (lambda (sym)
                     (or (memq sym required-args) (memq sym other-args)))
                   declared-specials))
       (symbols
        (let ((a (make-array (+ n-lambda-vars (length free-specials)))))
          (replace a required-args)
          (replace a other-args :start1 n-required)
          (replace a free-specials :start1 n-lambda-vars)
          (dotimes (i n-lambda-vars a) ; Unique-ify the binding cells.
            (setf (svref a i) (list (svref a i))))))
       (special-b
        (mark-bound-specials env declared-specials symbols n-lambda-vars))
       (required-mask (lognot (ash -1 n-required)))
       (required-specials ; one list for PROGV in WITH-LET-BINDINGS
        (nreverse ; WITH-LET-BINDINGS uses PUSH to accumulate values
         (collect-progv-symbols
          symbols n-lambda-vars (logand special-b required-mask))))
       (other-specials ; one list per PROGV in WITH-LET*-BINDINGS.
        (mapcar #'list
                (collect-progv-symbols
                 symbols n-lambda-vars (logandc2 special-b required-mask))))
       ((block-name forms block-share-env-p)
        (extract-lambda-block (proto-fn-forms proto-fn) decoder env))
       (frame (make-lambda-frame
               :min-args n-required :n-optional n-opt
               :n-bound-vars n-lambda-vars :keyword-bits keyword-bits
               :symbols symbols :values (or decoder #())
               :special-b special-b
               :specials (if required-specials
                             (cons required-specials other-specials)
                             other-specials)
               :declarations decls :%policy (new-policy env decls)
               :block-name block-name :share-block-p block-share-env-p
               :sexpr (%progn forms))))
    (process-typedecls frame env n-lambda-vars symbols)
    (setf (proto-fn-%frame proto-fn) frame
          (proto-fn-cookie proto-fn) *globaldb-cookie*)
    (values frame *globaldb-cookie*)))

(defun make-local-fn-scope (decls funs forms env)
  (let ((specials (free-specials env decls)))
    (process-typedecls (%make-local-fn-scope
                        decls (new-policy env decls)
                        funs (%progn forms) specials)
                       env 0 specials)))

(defun err-too-few-args (fun n-args)
  (declare (explicit-check))
  (let ((frame (interpreted-function-frame fun)))
    (%program-error "~S received ~D argument~:P but expects~:[ at least~;~] ~D."
                    (name-for-fun fun) n-args
                    (eql (lambda-frame-min-args frame)
                         (lambda-frame-max-args frame))
                    (lambda-frame-min-args frame))))

(defun err-too-many-args (fun n-args)
  (declare (explicit-check))
  (let ((frame (interpreted-function-frame fun)))
    (%program-error "~S received ~D argument~:P but expects~:[ at most~;~] ~D"
                    (name-for-fun fun) n-args
                    (eql (lambda-frame-min-args frame)
                         (lambda-frame-max-args frame))
                    (lambda-frame-max-args frame))))

(defmacro with-lambda-frame ((frame-var fun if-invalid) &body if-valid)
  ;; If any global variable has changed its :KIND, this function's lambda
  ;; variables will be re-checked for specialness, constness, etc.;
  ;; also, internally cached cached macroexpansions get discarded.
  `(if (eq (interpreted-function-cookie ,fun) *globaldb-cookie*)
       (let ((,frame-var (interpreted-function-frame ,fun)))
         ,@if-valid)
       (progn (atomic-incf *invalidation-count*) ; for testing cache eviction
              ,if-invalid)))

;;; LIST is a list of supplied keywords arguments to a function.
;;; CONTROL-BITS are from the lambda frame, and ALLOWED is a vector
;;; of the permitted keywords but has extra stuff after them, so scanning
;;; must restrict itself to strictly the N-KEYS specified by CONTROL-BITS.
(defun validate-keywords (list control-bits allowed)
  (declare (list list) (fixnum control-bits) (simple-vector allowed))
  (labels ((check-odd-length (tail n-seen)
             (let ((n-more (length tail))) ; just check for oddness
               (if (oddp n-more)
                   (fail-odd-length (+ n-seen n-more)))))
           (fail-odd-length (n)
             (%program-error "odd number of &KEY arguments: ~D" n))
           (fail-other-key ()
             (let ((n-allowed (keyword-bits-n-keys control-bits)) bad)
               (loop for (key val) on list by #'cddr ; rescan to collect them
                     unless (or (eq key :allow-other-keys)
                                (find key allowed :end n-allowed))
                     do (pushnew key bad))
               (let ((plural (cdr bad)))
                 (%program-error "Keyword~*~:[~;s~]~2:* ~{~S~^,~} ~
                                  ~:[is~;are~] not ~:[allowed.~;in the ~
                                  allowed set ~:*~S~]"
                                 (nreverse bad) plural
                                 (replace (make-list n-allowed) allowed))))))
    (if (keyword-bits-allowp control-bits)
        (check-odd-length list 0)
        (let ((n-allowed (keyword-bits-n-keys control-bits))
              (allow-other +none+) ; the observed value of :ALLOW-OTHER-KEYS
              (n-visited 0)
              (tail list)
              (seen-unknown-key-p nil))
          (loop
           (when (endp tail)
             (if seen-unknown-key-p
                 (fail-other-key)
                 (return)))
           (let ((key (pop tail))
                 (value (if tail (pop tail) (fail-odd-length (1+ n-visited)))))
             (incf n-visited 2)
             ;; The code below is designed to pass or fail as soon as possible
             (cond ((neq key :allow-other-keys)
                    (unless (find key allowed :end n-allowed)
                      (when (eq allow-other nil)
                        (fail-other-key))
                      (setq seen-unknown-key-p t)))
                   ((eq allow-other +none+) ; = unseen so far
                    (if value
                        (return (check-odd-length tail n-visited))
                        (progn
                           (when seen-unknown-key-p
                             (fail-other-key))
                           (setq allow-other nil)))))))))))

(declaim (ftype (sfunction (function) function) interpreted-applicator))
;;; Get a parsed LAMBDA-FRAME from the proto-fn, reinstall a more specific
;;; lambda applicator, and jump to it.
(defun interpreter-trampoline (fun &rest args)
  (setf (values (interpreted-function-frame fun)
                (interpreted-function-cookie fun))
        (proto-fn-frame (fun-proto-fn fun) (interpreted-function-env fun)))
  (apply (setf (%funcallable-instance-fun fun) (interpreted-applicator fun))
         args))

;;; The most general case of interpreted function application.
;;; All combinations of fancy args are allowed and any var can be special.
;;; This is a ton of code to inline, but it uses the same concept as for
;;; inlined %%EVAL - there is fast version and a slow version with extra
;;; extra wrapping for when hooks are in use.
;;;
(define-symbol-macro *absent* (%make-lisp-obj sb-vm:unbound-marker-widetag))
(declaim (maybe-inline apply-lambda))
(defun apply-lambda (frame fun n-args args)
  (declare (list args))
  (let* ((values (make-array (lambda-frame-n-bound-vars frame)))
         (symbol-cells (cons (lambda-frame-min-args frame)
                             (frame-symbols frame)))
         (new-env
          (if (lambda-frame-share-block-p frame)
              (make-lambda-env
               (interpreted-function-env fun) values symbol-cells frame
               (list (lambda-frame-block-name frame)))
              (make-var-env
               (interpreted-function-env fun) values symbol-cells frame)))
         (special-b (frame-special-b frame))
         (specials (frame-specials frame))
         (decoder-index
          (1- (keyword-bits-n-keys (lambda-frame-keyword-bits frame))))
         (arg-supplied-p)
         (tail args))
    (declare (index-or-minus-1 decoder-index) (list tail))
    (labels ((instruction ()
               (svref (frame-values frame) (incf decoder-index)))
             (fancy-arg (&aux (mode (the fixnum (instruction))))
               (case (logand mode +arg-mode-mask+)
                 (#.+optional-arg+
                  (cond ((setq arg-supplied-p tail)
                         (incf decoder-index (logand mode +has-default+))
                         (pop tail))
                        ((logtest mode +has-default+)
                         (dispatch (instruction) new-env))))
                 (#.+keyword-arg+
                  (let* ((key (svref (frame-values frame)
                                     (argument-keyword-index mode)))
                         (val (getf tail key *absent*)))
                    (cond ((setq arg-supplied-p (neq val *absent*))
                           (incf decoder-index (logand mode +has-default+))
                           val)
                          ((logtest mode +has-default+)
                           (dispatch (instruction) new-env)))))
                 (#.+supplied-p-var+ (if arg-supplied-p t nil))
                 (#.+aux-var+
                  (if (logtest mode +has-default+)
                      (dispatch (instruction) new-env)))
                 (#.+rest-arg+ tail))))
      (declare (inline instruction))
      (with-let*-binder
          (values (car symbol-cells)
           :value (enforce-type
                   (fancy-arg) (binding-typechecks frame)
                   frame-index (frame-symbols frame))
           :specials (pop specials))
        ;; form to start with
        (with-let-bindings
            (values (lambda-frame-min-args frame)
             :value (enforce-type
                     (if tail (pop tail) (err-too-few-args fun n-args))
                     (binding-typechecks frame)
                     frame-index (frame-symbols frame))
             :specials (pop specials))
          ;; maxargs/keywords should be checked now because it would be
          ;; strange to eval some defaulting forms and then croak.
          (cond ((logtest (lambda-frame-keyword-bits frame) +keyp-bit+)
                 (validate-keywords
                  (nthcdr (lambda-frame-n-optional frame) tail)
                  (lambda-frame-keyword-bits frame)
                  (frame-values frame)))
                ((and (not (logtest (lambda-frame-keyword-bits frame)
                                    +restp-bit+))
                      ;; FIXME: this call to LENGTH can be avoided by counting
                      ;; bindings made and subtracting from N-ARGS.
                      (> (length (truly-the list tail))
                         (lambda-frame-n-optional frame)))
                 (err-too-many-args fun n-args)))
          (let*-bind frame-index (lambda-frame-n-bound-vars frame)))
        ;; done with sequential bindings
        (setf (env-symbols new-env) (frame-symbols frame))
        (enforce-types frame (interpreted-function-env fun))
        ;; Three cases: no block, block and vars share the ENV,
        ;; or allocate another ENV
        (cond ((eql (lambda-frame-block-name frame) 0)
               (dispatch (lambda-frame-sexpr frame) new-env))
              ((lambda-frame-share-block-p frame)
               (catch (lambda-env-block new-env)
                 (dispatch (lambda-frame-sexpr frame) new-env)))
              (t
               (let ((exit (list (lambda-frame-block-name frame))))
                 (catch exit
                   (dispatch (lambda-frame-sexpr frame)
                             (make-block-env new-env exit nil
                                             *vacuous-decls*))))))))))

(defun applicator/general (fun)
  (lambda (&rest args)
    (declare #.+handler-optimize+)
    (declare (inline apply-lambda))
    (with-lambda-frame (frame fun (apply #'interpreter-trampoline fun args))
      (apply-lambda frame fun (length args) args))))

;;; Positional applicator is for lambda-lists that do not use any
;;; lambda-list keyword, perform no type checks and make no special bindings.
;;; Choose the most efficient applicator based on N-args and
;;; whether or not there is a block name. Since there are no defaulting forms,
;;; the block name (if any) and variables can have the same extent.
;;; User-supplied code could not detect the difference.
(defun applicator/positional (fun)
  (macrolet
      ((new-env (constructor storage-cells &rest more-args)
         `(,constructor (interpreted-function-env fun) ,storage-cells
                        (frame-symbols frame) frame ,@more-args))
       (invoke (n &aux (args (subseq '(arg1 arg2 arg3 arg4 arg5) 0 n)))
         `(if (eql (lambda-frame-block-name (interpreted-function-frame fun))
                   0)
              (named-lambda (.apply. ,n) ,args
                (declare #.+handler-optimize+ (optimize sb-c:verify-arg-count))
                (with-lambda-frame
                    (frame fun (interpreter-trampoline fun ,@args))
                  (dispatch (lambda-frame-sexpr frame)
                            (new-env make-var-env
                                     ,(if (zerop n) #() `(vector ,@args))))))
              (named-lambda (.apply. ,n) ,args
                (declare #.+handler-optimize+ (optimize sb-c:verify-arg-count))
                (with-lambda-frame
                    (frame fun (interpreter-trampoline fun ,@args))
                  (let ((exit (list (lambda-frame-block-name frame))))
                    (catch exit
                      (dispatch (lambda-frame-sexpr frame)
                                (new-env make-lambda-env
                                         ,(if (zerop n) #() `(vector ,@args))
                                         exit)))))))))
    (case (lambda-frame-min-args (interpreted-function-frame fun))
      (5 (invoke 5))
      (4 (invoke 4))
      (3 (invoke 3))
      (2 (invoke 2))
      (1 (invoke 1))
      (0 (invoke 0))
      (t
       (macrolet
           ((with-args (form)
              `(progn
                 (let* ((n-actual (length args))
                        (excess (- n-actual (lambda-frame-min-args frame))))
                   (cond ((minusp excess) (err-too-few-args fun n-actual))
                         ((plusp excess) (err-too-many-args fun n-actual))))
                 (let ((cells (make-array (lambda-frame-n-bound-vars frame))))
                   (dotimes (i (lambda-frame-min-args frame) ,form)
                     (setf (svref cells i) (nth i args)))))))
         (if (eql (lambda-frame-block-name (interpreted-function-frame fun)) 0)
             (named-lambda (.apply.) (&rest args)
               (declare #.+handler-optimize+)
               (with-lambda-frame
                   (frame fun (apply #'interpreter-trampoline fun args))
                 (with-args (dispatch (lambda-frame-sexpr frame)
                                      (new-env make-var-env cells)))))
             (named-lambda (.apply.) (&rest args)
               (declare #.+handler-optimize+)
               (with-lambda-frame
                   (frame fun (apply #'interpreter-trampoline fun args))
                 (with-args
                     (let ((exit (list (lambda-frame-block-name frame))))
                       (catch exit
                         (dispatch (lambda-frame-sexpr frame)
                                   (new-env make-lambda-env cells
                                            exit)))))))))))))

;;; /&Optional applicator disallows &REST and &KEY because the stack arguments
;;; are never listified (it's an implicit &MORE arg). I don't want to write code
;;; to manually listify, nor hand-roll GETF. Also no special bindings permitted.
(defun applicator/&optional (fun)
  (named-lambda (.apply. &optional) (&rest args)
    (declare #.+handler-optimize+)
    (with-lambda-frame (frame fun (apply #'interpreter-trampoline fun args))
      (let* ((n-actual (let* ((min (lambda-frame-min-args frame))
                              (max (+ min (lambda-frame-n-optional frame)))
                              (n (length args)))
                         (when (< n min)
                           (err-too-few-args fun n))
                         (when (> n max)
                           (err-too-many-args fun n))
                         n))
             (values (make-array (lambda-frame-n-bound-vars frame)))
             (symbol-cells (cons (lambda-frame-min-args frame)
                                 (frame-symbols frame)))
             (new-env
              (if (lambda-frame-share-block-p frame)
                  (make-lambda-env
                   (interpreted-function-env fun) values symbol-cells frame
                   (list (lambda-frame-block-name frame)))
                  (make-var-env
                   (interpreted-function-env fun) values symbol-cells frame)))
             (decoder-index -1)
             (arg-index -1)
             (arg-supplied-p))
        (declare (index-or-minus-1 decoder-index arg-index))
        (with-let-bindings (values (lambda-frame-min-args frame)
                            :value (nth (incf arg-index) args) :specialp nil)
          nil) ; empty body. no special bindings, so no PROGV
        (flet ((instruction ()
                 (svref (frame-values frame) (incf decoder-index))))
          (declare (inline instruction))
          (with-let*-binder
              (values (car symbol-cells)
               :value
               (let ((mode (the fixnum (instruction))))
                 (case (logand mode +arg-mode-mask+)
                   (#.+optional-arg+
                    (cond ((setq arg-supplied-p (< (incf arg-index) n-actual))
                           (incf decoder-index (logand mode +has-default+))
                           (nth arg-index args))
                          ((logtest mode +has-default+)
                           (dispatch (instruction) new-env))))
                   (#.+supplied-p-var+ (if arg-supplied-p t nil))
                   (#.+aux-var+ (if (logtest mode +has-default+)
                                    (dispatch (instruction) new-env)))))
               :specialp nil)
            ;; form to start with
            (let*-bind (lambda-frame-min-args frame)
                       (lambda-frame-n-bound-vars frame))
            ;; done with sequential bindings
            (setf (env-symbols new-env) (frame-symbols frame))
            ;; Three cases: no block, block and vars share the ENV,
            ;; or allocate another ENV
            (cond ((eql (lambda-frame-block-name frame) 0)
                   (dispatch (lambda-frame-sexpr frame) new-env))
                  ((lambda-frame-share-block-p frame)
                   (catch (lambda-env-block new-env)
                     (dispatch (lambda-frame-sexpr frame) new-env)))
                  (t
                   (let ((exit (list (lambda-frame-block-name frame))))
                     (catch exit
                       (dispatch (lambda-frame-sexpr frame)
                                 (make-block-env new-env exit
                                                 nil *vacuous-decls*))))))))))))

(declaim (type (or null compiled-function) *self-applyhook*))
(defvar *self-applyhook* nil) ; not quite *applyhook* as outlined in CLtL

;;; A trampoline which never installs a more-specific trampoline,
;;; and checks for a binding of *SELF-APPLYHOOK* on each call.
(defun interpreter-hooked-trampoline (fun &rest args)
  (multiple-value-bind (frame cookie)
      (proto-fn-frame (fun-proto-fn fun) (interpreted-function-env fun))
    (setf (values (interpreted-function-frame fun)
                  (interpreted-function-cookie fun)) (values frame cookie))
    ;; *SELF-APPLYHOOK* isn't the *APPLYHOOK* as described by CLtL.
    ;; When bound, each hooked function will funcall the hook with
    ;; itself and the arguments and a continuation of two arguments
    ;; that the hook should call to actually perform the application.
    ;; APPLY-LAMBDA is not inlined here. If you're using the hook,
    ;; things are running at less than top speed anyway.
    (if *self-applyhook* ; must be a compiled-function
        (funcall *self-applyhook* fun args
                 (lambda (self args)
                   (apply-lambda (interpreted-function-frame self)
                                 self (length args) args)))
        ;; Assuming the compiler is doing the right thing,
        ;; this LENGTH is gotten from the passing location.
        (apply-lambda frame fun (length args) args))))

;;; Compute the function that should be used when applying FUN.
;;; This uses highly specialized code for small fixed N args <= 5,
;;; and slightly more general code for any fixed number of args > 5.
;;; Bound special arguments require the most general entry.
;;;
(defun interpreted-applicator (fun)
  (let ((frame (interpreted-function-frame fun)))
    (if (and (zerop (frame-special-b frame)) ; no bound specials
             (not (logtest (lambda-frame-keyword-bits frame) ; fixed upper bound
                           (logior +restp-bit+ +keyp-bit+))) ; on arg count
             (eql (binding-typechecks frame) +none+)
             (eql (extra-typechecks frame) +none+))
        (if (zerop (length (frame-values frame)))
            (applicator/positional fun)
            (applicator/&optional fun))
        (applicator/general fun))))


#|
Test case.
(defun get-thinginator () #'thing)
(defun thing () 'the-thing)
(funcall (get-thinginator)) ; returns THE-THING
(defmacro thing () ''thing-macro) ; system warns about this
(get-thinginator) ; errs - it correctly perceives the redefinition
|#

;;; Return T if SYMBOL might get redefined as a macro when it was previously
;;; a function and vice versa. Extra checks are done on every use of the symbol
;;; as a function name.
(defun fluid-def-p (symbol)
  ;; Todo: add most system-internal packages here probably
  (not (memq (symbol-package symbol)
             (load-time-value
              (mapcar #'find-package '("CL" "SB-KERNEL" "SB-INT" "SB-IMPL"
                                       "SB-C" "SB-VM" "SB-ALIEN" "SB-ALIEN-INTERNALS"
                                       "SB-LOOP"
                                       "SB-SYS" #+sb-thread "SB-THREAD"))))))

#|
* (defmacro baz (n) `(nth ,n *mumble*))
* (define-symbol-macro mumble (cdr (feep)))
* (defmacro feep () '(aref *x* 1))
* (nth-value 1 (tracing-macroexpand-1 '(shiftf (baz 3) (feep)) nil))
  => ((#<INTERPRETED-FUNCTION (DEFMACRO BAZ)> . BAZ) (MUMBLE CDR (FEEP))
      (#<INTERPRETED-FUNCTION (DEFMACRO FEEP)> . FEEP))
|#

(defvar *show-macroexpansion* nil)
;;; Expand FORM once, capturing all "interesting" expansions occuring within.
;;; For example (INCF FOO) is a builtin macro whose functional definition is
;;; not considered per se interesting, however if FOO is a symbol macro,
;;; then the meaning of that form changes if FOO gets redefined.
;;;
;;; This is still not powerful enough to work all the time - we really need to
;;; know if anything so much as inquired whether a symbol is a macro.
;;; Continuing the above example, if FOO is not a macro, then making it into
;;; a macro later will have no effect, because there is no object that
;;; stands for the identity of FOO as a macro. This is not a problem with
;;; macro functions.
;;; Also note that CLtL2 says
;;; "macro definitions should not depend on the time at which they are expanded"
;;;
(defun tracing-macroexpand-1 (form env &optional (predicate #'fluid-def-p)
                              &aux (original-hook (valid-macroexpand-hook))
                                   expanders)
  (unless (allow-macro-redefinition env)
    (return-from tracing-macroexpand-1
      (values (macroexpand-1 form env) nil)))
  (flet ((macroexpand-hook (function form env)
           (let ((expansion (funcall original-hook function form env)))
             (if (atom form)
                 ;; All global symbol-macros are recorded - there are no builtins
                 ;; which are candidates for removal. symbol-macrolet expansions
                 ;; aren't recorded, unless they happen to be EQ to a global
                 ;; expansion, which is unlikely and nothing to worry about.
                 (if (neq expansion (info :variable :macro-expansion form))
                     nil ; I have no idea what to record
                     (push (cons form expansion) expanders))
                 ;; If the expander is EQ to the global one and the symbol
                 ;; satisfies the interestingness test.
                 (let* ((head (car form)) (global-fn (macro-function head nil)))
                   (if (and (eq function global-fn) (funcall predicate head))
                       (push (cons global-fn head) expanders))))
             expansion)))
    (let ((expansion (let ((*macroexpand-hook* #'macroexpand-hook))
                       (macroexpand-1 form env))))
      (setq expanders (nreverse expanders))
      ;; condition on #+eval-show also?
      (when *show-macroexpansion*
        (format t "~&Expanded ~S~%    into ~S~%~@[   using ~S~]~%"
                form expansion expanders))
      (values expansion expanders))))

;;; Return T if the evaluator should always consider that macros
;;; might be redefined. If NIL then cached expansions are permanent.
(defun allow-macro-redefinition (env)
  (if (policy env (and (= speed 3) (= debug 0) (= safety 0)))
      nil
      t))

(defun arglist-to-sexprs (args)
  (let ((argc (or (list-length args)
                  (%program-error "Malformed function call"))))
    (values (mapcar #'%sexpr args) argc)))

;;; Return a handler which decides whether its supplied SEXPR needs
;;; to have macroexpansion performed again due to changes to global macros.
;;; If not, just dispatch the previously computed expansion.
;;;
;;; Just knowing the macroexpander isn't enough. We need to collect _all_
;;; expanders that ran during 1 round of expansion, which can't be determined
;;; without running the outermost and tracing what happens.
;;;
;;; EXPANSION is a SEXPR for the overall result of expansion.
;;; FNAME is the symbol at the head of the original form.
;;; Each element of KEYS is (#<FUNCTION> . SYM) or (SYM . SYMBOL-EXPANSION)
;;; The representation is unambiguous because a symbol is not a function,
;;; whereas (SYM . EXPANSION|FUNCTION) is ambiguous because through contortions
;;; it is possible to have a symbol's expansion be a function object.
;;; If any key is changed, restart using the original sexpr form.
;;;
(defun digest-macro-form (expansion fname keys)
  (if (and (endp (cdr keys)) ; if one key
           (let ((k (car keys))) ; which is the function that we expect
             (and (functionp (car k)) (eq (cdr k) fname))))
      ;; Assume that if MACRO-FUNCTION for the form's head is EQ to what it was
      ;; previously, that it will produce the same expansion (in this ENV).
      ;; This can of course is easily violated by nondeterministic macros.
      (let ((macro-fn (caar keys)))
        (hlambda MACRO/1 (fname macro-fn expansion) (env old-sexpr)
          (if (eq (macro-function fname) macro-fn)
              (dispatch expansion env)
              (progn
                #+eval-show
                (format t "~&Changed expander: ~S~%" fname)
                (digest-form (sexpr-form old-sexpr) env old-sexpr)))))
      ;; Same as above but generalized to N keys.
      (hlambda MACRO+ (keys expansion) (env old-sexpr)
        (if (every (lambda (k)
                     (if (functionp (car k))
                         (eq (car k) (macro-function (cdr k)))
                         (eq (info :variable :macro-expansion (car k))
                             (cdr k))))
                   (the list keys))
            (dispatch expansion env)
            (progn
              #+eval-show
              (format t "~&Changed expanders: ~S~%" keys)
              (digest-form (sexpr-form old-sexpr) env old-sexpr))))))

#|
(CASES n (1 5 frob) (4 (exceptional-case)) (t (fallback-case)))
-> (CASE N
    (1 (FROB 1))
    (2 (FROB 2))
    (3 (FROB 3))
    (4 (EXCEPTIONAL-CASE))
    (5 (FROB 5))
    (T (FALLBACK-CASE)))
|#

(defmacro cases (test-var (min max template) &rest specified-cases)
  `(case ,test-var
     ,@(loop for i from min to max
             collect
             (or (assoc i specified-cases)
                 `(,i (,template ,i))))
     (t
      ,@(cdr (assoc t specified-cases)))))

;;; A local-call uses LOCAL-FDEFINITION to obtain the function, which
;;; is *always* a function, never a macro and never undefined.
;;; Some clever macrology might share the handler-generator
;;; with DIGEST-GLOBAL-CALL
(defun digest-local-call (frame-ptr args &aux (n-args 0))
  (multiple-value-setq (args n-args) (arglist-to-sexprs args))
  (macrolet ((funcall-n (n)
               `(hlambda (LOCAL-CALL ,N) (data) (env)
                  (funcall (local-fdefinition (svref data 0) env)
                           ,@(loop for i from 1 repeat n
                                   collect `(dispatch (svref data ,i) env))))))
    (let ((data (if (> n-args 1) (coerce (cons frame-ptr args) 'vector))))
      (cases n-args (0 5 funcall-n)
        (0 (hlambda (LOCAL-CALL 0) (frame-ptr) (env)
             (funcall (local-fdefinition frame-ptr env))))
        (1 (let ((arg (first args)))
             (hlambda (LOCAL-CALL 1) (frame-ptr arg) (env)
               (funcall (local-fdefinition frame-ptr env) (dispatch arg env)))))
        (t (hlambda LOCAL-CALL (data) (env)
             (declare (simple-vector data))
             (let* ((arglist (make-list (1- (length data))))
                    (tail arglist))
               (dotimes (i (1- (length data))
                           (apply (local-fdefinition (svref data 0) env)
                                  arglist))
                 (rplaca tail (dispatch (svref data (1+ i)) env))
                 (pop tail)))))))))

;;; Apply what is probably a function - it was when the form was digested.
;;; This carefully mimics the compiler's behavior of referencing the
;;; function only after evaluation of its args. In particular, supposing that
;;; BAZ is not defined, this works in compiled code:
;;;  (DEFUN FOO () (BAZ (SETF (SYMBOL-FUNCTION 'BAZ) (LAMBDA (X) `(HI ,X)))))
;;;
;;; Interpreted code needs an explicit check for NIL in an fdefn-fun.
;;; Compiled code doesn't because the 'raw-addr' slot is always
;;; something valid to jump to.
(defun apply-probably-fun (fdefinition args env &aux (n-args 0))
  (multiple-value-setq (args n-args) (arglist-to-sexprs args))
  (macrolet
      ((funcall-n (n)
         (let* ((arg-names (subseq '(a b c d e) 0 n))
                (bindings
                  (loop for arg in arg-names for i from 1 repeat n
                        collect `(,arg
                                  (dispatch
                                   ,(if (= n 1) '(cdr data) `(svref data ,i))
                                   env)))))
           `(hlambda (GLOBAL-CALL ,n) (data) (env sexpr)
              (symbol-macrolet ((fdefn ,(case n
                                          (0 'data)
                                          (1 '(car data))
                                          (t '(svref data 0)))))
                (if (re-expand-p)
                    (digest-form (sexpr-form sexpr) env sexpr)
                    (let ,bindings
                      (funcall (sb-c:safe-fdefn-fun fdefn) ,@arg-names)))))))
       (generate-switch ()
         `(case n-args
            (0 (let ((data fdefinition)) (funcall-n 0)))
            (1 (let ((data (cons fdefinition (first args)))) (funcall-n 1)))
            (t
             (let ((data (coerce (cons fdefinition args) 'vector)))
               (cases n-args (2 5 funcall-n)
                (t (hlambda GLOBAL-CALL (data) (env sexpr)
                    (declare (simple-vector data))
                    (symbol-macrolet ((fdefn (svref data 0)))
                      (if (re-expand-p)
                          (digest-form (sexpr-form sexpr) env sexpr)
                          (let* ((arglist (make-list (1- (length data))))
                                 (tail arglist))
                            (dotimes (i (1- (length data))
                                        (apply (sb-c:safe-fdefn-fun fdefn) arglist))
                              (rplaca tail (dispatch (svref data (1+ i)) env))
                              (pop tail)))))))))))))
    (if (allow-macro-redefinition env)
        (macrolet ((re-expand-p ()
                     '(let ((f (fdefn-fun fdefn)))
                        (and f (sb-impl::macro/special-guard-fun-p f)))))
          (generate-switch))
        (macrolet ((re-expand-p () nil)) (generate-switch)))))

;;; Evaluate the arguments to a function that can't be called,
;;; then call it. Very weird, yes! But this is reached in two situations:
;;; 1. the user wrote (funcall 'IF ...)
;;; 2. the user defined a new special operator that the interpreter
;;;    does not know about.
;;; In either case %LOOKS-LIKE-MACRO-P will return T,
;;; because it knows that the fdefn-fun is an error-invoking trampoline.
;;; But there is no macroexpander, so we have to do "something".
;;; This handler is the way we punt, because the careful handler (above)
;;; would see that the thing to be applied is a guard trampoline,
;;; and would start over again at digest-global-call.
;;;
;;; Note also the subtle difference between these two:
;;;  (funcall 'IF 'FOO 'BAR)
;;;  (funcall #'IF ...)
;;; In the former, the FUNCALL is reached, because every argument
;;; to the funcall was legally evaluable. But in the latter, it is not
;;; reached since the first argument to funcall signals an error.
(defun apply-definitely-not-fun (fname args)
  (let ((data (coerce (cons fname (arglist-to-sexprs args)) 'vector)))
    (hlambda GLOBAL-CALL (data) (env)
      (declare (simple-vector data))
      (let* ((arglist (make-list (1- (length data))))
             (tail arglist))
        ;; "unable to optimize apply"
        (declare (muffle-conditions compiler-note))
        (dotimes (i (1- (length data))
                    (apply (the symbol (svref data 0)) arglist))
          (rplaca tail (dispatch (svref data (1+ i)) env))
          (pop tail))))))

;;; Handler is specialized only if the function is builtin.
;;; In such cases, reference the function directly, eliding the deref
;;; through an FDEFINITION.
;;; It can always be kicked out of the cache by touching the globaldb cookie.
(defun digest-global-call (fname args env)
  ;; For user-defined functions declared inline, don't bother
  ;; checking for being redefined as a macro.
  ;; The globaldb cookie will take care of redefinition.
  ;;  (neq (info :function :inlinep fname) 'inline))

  (when (symbolp fname)
    (when (eq (info :function :kind fname) :special-form)
      (return-from digest-global-call
        (apply-definitely-not-fun fname args)))

    ;; Structure-accessor: interpreted accessors are *terrible*.
    ;; We could use a handler (need to respect NOTINLINE though),
    ;; or just COMPILE the accessor, since who's to say
    ;; that DEFSTRUCT doesn't somehow magically produce
    ;; compiled accessors via closures or a LAP assembler.
    ;; Frankly the latter ought to be possible.
    ;; And this doesn't fix the problem with SETF.
    (when (fboundp fname)
      (let ((f (symbol-function fname)))
        (when (and (typep f 'interpreted-function)
                   (structure-instance-accessor-p fname))
          ;: Compile the accessor using an explicit call to COMPILE with a
          ;; lambda expression. Don't simply call (COMPILE FNAME) because
          ;; if it was defined in a non-null environment, conversion to a
          ;; lexenv could say "too complex". Additionally the debug source
          ;; info would be misleading.
          ;; We can be confident that the expression doesn't need a lexenv,
          ;; because if the function were incompatible with the source-transform,
          ;; %DEFUN would have cleared the :source-transform, and fname would not
          ;; satisfy STRUCTURE-INSTANCE-ACCESSOR-P.
          #+nil (format t "~&; Interpreter: Compiling ~S~%" fname)
          ;; FIXME: ensure that the compiled function is safe.
          (let* ((compiled-fun (compile nil (function-lambda-expression f)))
                 (code (fun-code-header compiled-fun))
                 (cdi (%code-debug-info code))
                 (source (sb-c::compiled-debug-info-source cdi)))
            ;; Ensure that the debug-source-namestring of the compiled thing
            ;; reflects the source file of the original interpreted function.
            (setf (debug-source-namestring source)
                  (sb-kernel::function-file-namestring f)
                  (debug-source-created source) nil) ; = unknown
            ;; Clobber the TLF-NUM + OFFSET. We should do better that that,
            ;; but interpreted functions do not track their file offset,
            ;; so the conservative thing is to plug in NILs, not bogus values
            ;; from whatever file (if any) is currently being loaded.
            (setf (sb-c::compiled-debug-info-tlf-num+offset cdi) 0)
            ;; Skirt a package lock by avoiding (SETF SYMBOL-FUNCTION)
            ;; *technically* this should be a compare-and-swap to ensure that the
            ;; original lambda expression is as expected. To do that, we need
            ;; to get to the point where fdefns do not store "both" representations
            ;; of one function pointer, because we can't assume that multi-word CAS
            ;; is a thing.  Or use a mutex (horrible).
            (setf (fdefn-fun (find-fdefn fname)) compiled-fun)))))

    (when (fluid-def-p fname)
      ;; Return a handler that calls FNAME very carefully
      (return-from digest-global-call
        (apply-probably-fun (find-or-create-fdefn fname) args env))))

  ;; Try to recognize (FUNCALL constant-fun ...)
  ;; This syntax is required when using SETF functions, and it should
  ;; be no less efficient than (F args).
  ;; But, [FIXME?] can FUNCALL be rebound lexically?
  (when (and (eq fname 'funcall)
             (not (endp args)) ; FUNCALL demands at least one arg
             (typep (first args)
                    '(or (cons (eql function)
                               (cons (satisfies legal-fun-name-p) null))
                         (cons (eql quote) (cons symbol null)))))
    (let* ((function-form (first args))
           (fname (second function-form)))
      ;; (FUNCALL 'SYMBOL args...) => (SYMBOL args...) without the lexenv.
      (when (eq (car function-form) 'quote)
        (return-from digest-global-call
          (digest-global-call fname (cdr args) env)))

      ;; It's (FUNCALL #'FUNCTION ...)
      (let ((frame-ptr (local-fn-frame-ptr fname env)))
        (case frame-ptr
          ((nil) ; global function (or special operator, which will barf on you)
           (when (symbolp fname) (coerce fname 'function)) ; for effect
           (return-from digest-global-call
             (digest-global-call fname (cdr args) env)))
          (:macro) ; do not process - let the FUNCTION operator complain
          (t (return-from digest-global-call
               (digest-local-call frame-ptr (cdr args))))))))

  (let ((n-args 0)
        (fun (if (typep fname '(cons (eql sb-pcl::slot-accessor)))
                 (funcall 'sb-pcl::ensure-accessor fname)
                 (fdefinition fname))))
    (multiple-value-setq (args n-args) (arglist-to-sexprs args))

    ;; Fold if every arg when trivially constant and the function is foldable.
    ;; "trivially" means without needing propagation to decide that.
    (when (notany #'sexpr-p args)
      (let ((info (info :function :info fname)))
        (when (and info (sb-c::ir1-attributep (sb-c::fun-info-attributes info)
                                              sb-c::foldable))
          (let ((values (multiple-value-list (apply fname args))))
            (return-from digest-global-call
              (if (or (cdr values) (null values))
                  (handler #'return-constant-values values)
                  (return-constant (first values))))))))

    ;; Todo: redefining any function in one of the builtin packages should
    ;; increment the globaldb cookie to unmemoized stored #<FUNCTION> objects.
    ;; Btw, it's weird that FDEFINITION strips out tracing wrappers
    ;; since FDEFINITION is the canonical way to get the function given a
    ;; general name, and seems like it's supposed to be just the
    ;; straightforward generalization of SYMBOL-FUNCTION.
    (macrolet ((funcall-n (n)
                 `(hlambda (FAST-GLOBAL-CALL ,n) (data) (env)
                    (funcall (the function (svref data 0))
                             ,@(loop for i from 1 repeat n collect
                                     `(dispatch (svref data ,i) env))))))
      (let ((data (if (> n-args 1) (coerce (cons fun args) 'vector))))
        (cases n-args (0 5 funcall-n)
          (0 (hlambda (FAST-GLOBAL-CALL 0) (fun) (env)
               (declare (ignore env))
               (funcall (the function fun))))
          (1 (let ((arg (first args))
                   (handler-fn (gethash fname *unary-functions*)))
               (if handler-fn
                   (handler handler-fn arg)
                   (hlambda (FAST-GLOBAL-CALL 1) (fun arg) (env)
                     (funcall (the function fun) (dispatch arg env))))))
          (2 (let ((handler-fn (gethash fname *binary-functions*)))
               (if handler-fn
                   (handler handler-fn (cons (first args) (second args)))
                   (funcall-n 2))))
          (t
           (hlambda GLOBAL-CALL (data) (env)
             (declare (simple-vector data))
             (let* ((arglist (make-list (1- (length data)))) (tail arglist))
               (dotimes (i (1- (length data))
                           (apply (the function (svref data 0)) arglist))
                 (rplaca tail (dispatch (svref data (1+ i)) env))
                 (pop tail))))))))))

(defmethod print-object ((obj basic-env) stream)
  (print-unreadable-object (obj stream :identity t :type t)
    #+eval-show
    (write-string (env-to-string obj) stream)))

(defmethod print-object ((obj sexpr) stream)
  ;; It's very confusing to debug this code if sexprs are visibly indistinct
  ;; from their lists, but it looks more pleasing in backtraces.
  ;; Maybe need a toggle switch that is not one of the standard ones?
  (if *print-escape*
      (let ((string (write-to-string (sexpr-form obj))))
        (format stream "#<sexpr ~A... #x~X>"
                (subseq string 0 (min (length string) 30))
                (get-lisp-obj-address obj)))
      (write (sexpr-form obj) :stream stream)))
