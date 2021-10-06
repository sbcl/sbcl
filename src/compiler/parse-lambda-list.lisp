;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

(declaim (special *lexenv*))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant-eqx lambda-list-parser-states
    #(:required &optional &rest &more &key &aux &environment &whole
      &allow-other-keys &body :post-env :post-rest :post-more)
  #'equalp))

;; Return a bitmask representing the LIST of lambda list keywords.
(defmacro lambda-list-keyword-mask (list)
  (if (cl:constantp list)
      ;; When invoked with a quoted constant, some flexibility
      ;; is allowed, in that the input may be a single symbol.
      (let ((val (#+sb-xc constant-form-value #-sb-xc eval list)))
        (loop for symbol in (cond ((eq val 'destructuring-bind)
                                   '(&whole &optional &rest &body
                                     &key &allow-other-keys &aux))
                                  ((and val (symbolp val)) (list val))
                                  (t val))
              for weight = (or (position symbol lambda-list-parser-states)
                               (error "Not a parser state: ~S" symbol))
              sum (ash 1 weight)))
      ;; Otherwise the input is required to be a list of symbols.
      (with-unique-names (k)
        `(loop for ,k in ,list
               sum (ash 1 (position ,k lambda-list-parser-states))))))

(defun ll-kwds-restp (bits)
  (when (logtest (lambda-list-keyword-mask '(&rest &body &more)) bits)
    ;; Test &BODY first because if present, &REST bit is also set.
    (cond ((logtest (lambda-list-keyword-mask '&body) bits) '&body)
          ((logtest (lambda-list-keyword-mask '&more) bits) '&more)
          (t '&rest))))

;;; Some accessors to distinguish a parse of (values &optional) from (values)
;;; and (lambda (x &key)) from (lambda (x)).
(declaim (inline ll-kwds-keyp ll-kwds-allowp))
(defun ll-kwds-keyp (bits)
  (logtest (lambda-list-keyword-mask '&key) bits))
(defun ll-kwds-allowp (bits)
  (logtest (lambda-list-keyword-mask '&allow-other-keys) bits))

;;; Break something like a lambda list (but not necessarily actually a
;;; lambda list, e.g. the representation of argument types which is
;;; used within an FTYPE specification) into its component parts. We
;;; return eight values:
;;;  1. a bitmask of lambda-list keywords which were present;
;;;  2. a list of the required args;
;;;  3. a list of the &OPTIONAL arg specs;
;;;  4. a singleton list of the &REST arg if present;
;;;     or a 2-list of the &MORE context and count if present
;;;  5. a list of the &KEY arg specs;
;;;  6. a list of the &AUX specifiers;
;;;  7. a singleton list of the &ENVIRONMENT arg if present
;;;  8. a singleton list of the &WHOLE arg if present
;;;
;;; The top level lambda list syntax is checked for validity, but the
;;; arg specifiers are just passed through untouched. If something is
;;; wrong, we use COMPILER-ERROR, aborting compilation to the last
;;; recovery point.
(declaim (ftype (sfunction
                 (list &key (:context t) (:accept integer) (:silent t)
                            (:condition-class symbol))
                 (values (unsigned-byte 13) list list list list list list list))
                parse-lambda-list))

;;; Note: CLHS 3.4.4 [macro lambda list] allows &ENVIRONMENT anywhere,
;;; but 3.4.7 [defsetf lambda list] has it only at the end.
;;; This is possibly surprising to people since there seems to be some
;;; expectation that a DEFSETF lambda list is a macro lambda list,
;;; which it isn't. We'll relax and accept &ENVIRONMENT in the middle.
;;;
(defun parse-lambda-list
    (list &key (context "an ordinary lambda list")
               (accept (lambda-list-keyword-mask
                        '(&optional &rest &more &key &allow-other-keys &aux)))
               (condition-class 'simple-program-error)
               ;; For internal method functions, just shut up about everything
               ;; that we could style-warn about. Interpreters tend to scan the
               ;; lambda list at various inopportune times depending on strategy
               ;; (macro caching, etc) and it's really annoying to see the
               ;; &OPTIONAL/&KEY message randomly repeated, especially if it's
               ;; someone else's code. Fwiw, 'full-eval' muffles warnings during
               ;; all calls to this parser anyway.
               (silent (typep list '(cons (eql sb-pcl::.pv.))))
          &aux (seen 0) required optional rest more keys aux env whole tail
               (rest-bits 0))
  (declare (optimize speed))
  (declare (type (unsigned-byte 13) accept seen) (type (mod 4) rest-bits))
  (macrolet ((state (name)
               (position (the symbol name) lambda-list-parser-states))
             (state= (x y) `(= ,x (state ,y)))
             (bits (&rest list) `(lambda-list-keyword-mask ',list))
             (begin-list (val)
               (declare (optimize (speed 0))) ; suppress generic math notes
               `(case state
                  ,@(loop for i from 0
                          for s in '(required optional rest more
                                     keys aux env whole)
                          collect `(,i (setq ,s ,val))))))
    (labels ((destructuring-p ()
               (logtest (bits &whole) accept))
             (probably-ll-keyword-p (arg)
               ;; Compiler doesn't see that the check is manually done. :-(
               #-sb-xc-host
               (declare (optimize (sb-c:insert-array-bounds-checks 0)))
               (and (symbolp arg)
                    (let ((name (symbol-name arg)))
                      (and (plusp (length name))
                           (char= (char name 0) #\&)))))
             (check-suspicious (kind form)
               (and (probably-ll-keyword-p form)
                    (member form lambda-list-keywords)
                    (report-suspicious kind form)))
             (report-suspicious (kind what)
               (style-warn-once list (sb-format:tokens
                                      "suspicious ~A ~S in lambda list: ~
                                       ~/sb-impl:print-lambda-list/.")
                                kind what list)
               nil) ; Avoid "return convention is not fixed" optimizer note
             (need-arg (state)
               (croak (sb-format:tokens
                       "expecting variable after ~A in: ~
                        ~/sb-impl:print-lambda-list/")
                      state list))
             (need-symbol (x why)
               (unless (symbolp x)
                 (croak "~A is not a symbol: ~S" why x)))
             (need-bindable (x why)
               ;; "Bindable" means symbol or cons, but only if destructuring.
               (unless (or (symbolp x) (and (consp x) (destructuring-p)))
                 (if (destructuring-p)
                     (croak "~A is not a symbol or list: ~S" why x)
                     (croak "~A is not a symbol: ~S" why x))))
             (defaultp (x what-kind)
               (cond ((symbolp x) nil)
                     ((listp x) t)
                     (t (croak "~A parameter is not a symbol or cons: ~S"
                               what-kind x))))
             (croak (string &optional (a1 0 a1p) (a2 0 a2p) (a3 0 a3p))
               ;; Don't care that FUNCALL can't elide fdefinition here.
               (declare (optimize (speed 1)))
               (let ((l (if a1p (list a1 a2 a3))))
                 (if (and l (not a3p)) (rplacd (if a2p (cdr l) l) nil))
                 ;; KLUDGE: When this function was limited to parsing
                 ;; ordinary lambda lists, this error call was always
                 ;; COMPILER-ERROR, which must be used, not plain old ERROR,
                 ;; to avoid the compiler itself crashing. But outside of
                 ;; the compiler, it must be ERROR. This discrepancy is sad
                 ;; since DESTRUCTURING-BIND herein can cause a compiler crash.
                 ;; It seems that the right thing is for the compiler to wrap
                 ;; a condition handler around PARSE-LAMBDA-LIST.
                 ;; Expecting a callee to understand how to signal conditions
                 ;; tailored to a particular caller is not how things are
                 ;; supposed to work.
                 (funcall (if (or (destructuring-p) (eq context 'defmethod))
                              'error
                              'compiler-error)
                          condition-class
                          :format-control string :format-arguments l))))
      (prog ((input list)
             (saved-state 0)
             (state (state :required))
             (arg nil)
             (last-arg nil))
         (declare (type (mod 13) state saved-state))
         LOOP
         (when (atom input)
           (cond ((not input)
                  (if (logbitp state (bits &whole &rest &more &environment))
                      (need-arg arg)))
                 ;; Whenever &BODY is accepted, so is a dotted tail.
                 ((and (logtest (bits &body) accept)
                       (not (logtest (bits &rest &key &aux) seen))
                       (symbolp input))
                  (setf rest (list input)))
                 (t
                  (croak (sb-format:tokens
                          "illegal dotted lambda list: ~
                           ~/sb-impl:print-lambda-list/")
                         list)))
           (return))
         (shiftf last-arg arg (pop input))

         (when (probably-ll-keyword-p arg)
           ;; Handle a probable lambda list keyword
           (multiple-value-bind (from-states to-state)
              (case arg
               (&optional (values (bits :required) (state &optional)))
               (&rest     (values (bits :required &optional) (state &rest)))
               (&more     (values (bits :required &optional) (state &more)))
               (&key      (values (bits :required &optional :post-rest :post-more)
                                  (state &key)))
               (&allow-other-keys (values (bits &key) (state &allow-other-keys)))
               (&aux (values (bits :post-more :required &optional :post-rest
                                   &key &allow-other-keys) (state &aux)))
               (&environment
                (setq saved-state state)
                (values (bits :required &optional :post-rest &key
                              &allow-other-keys &aux) (state &environment)))
               ;; If &BODY is accepted, then it is folded into &REST state,
               ;; but if it should be rejected, then it gets its own bit.
               ;; Error message production is thereby confined to one spot.
               (&body (values (bits :required &optional)
                              (if (logtest (bits &body) accept)
                                  (state &rest) (state &body))))
               (&whole
                (values (if (and (state= state :required) (not required)
                                 (not (logtest (bits &environment) seen)))
                            (bits :required) 0)
                        (state &whole))))
             (when from-states
               (unless (logbitp to-state accept)
                 (let ((where ; Keyword never legal in this flavor lambda list.
                        (case context
                          (:function-type "a FUNCTION type specifier")
                          (:values-type "a VALUES type specifier")
                          (:macro "a macro lambda list")
                          (destructuring-bind "a destructuring lambda list")
                          (defmethod "a specialized lambda list")
                          (t context))))
                   (croak (sb-format:tokens
                           "~A is not allowed in ~A: ~
                            ~/sb-impl:print-lambda-list/")
                          arg where list)))

               ;; &ENVIRONMENT can't intercede between &KEY,&ALLOW-OTHER-KEYS.
               ;; For all other cases it's as if &ENVIRONMENT were never there.
               (when (and (state= state :post-env)
                          (not (state= saved-state &key)))
                 (shiftf state saved-state 0)) ; pop the state

               (when (state= to-state &rest) ; store a disambiguation bit
                 (setq rest-bits (logior rest-bits (if (eq arg '&body) 1 2))))

               ;; Try to avoid using the imprecise "Misplaced" message if
               ;; a better thing can be said, e.g. &WHOLE must go to the front.
               (cond ((logbitp to-state seen) ; Oops! Been here before.
                      (if (= rest-bits 3)
                          (croak (sb-format:tokens
                                  "~S and ~S are mutually exclusive: ~
                                   ~/sb-impl:print-lambda-list/")
                                 '&body '&rest list)
                          (croak (sb-format:tokens
                                  "repeated ~S in lambda list: ~
                                   ~/sb-impl:print-lambda-list/")
                                 arg list)))
                     ((logbitp state from-states) ; valid transition
                      (setq state to-state
                            seen (logior seen (ash 1 state))
                            tail nil)) ; Reset the accumulator.
                     ((logbitp state (bits &whole &rest &more &environment))
                      (need-arg last-arg)) ; Variable expected.
                     (t
                      (croak (sb-format:tokens
                              "~:[misplaced ~A in lambda list~;~
                               ~A must appear first in a lambda list~]:
                               ~/sb-impl:print-lambda-list/")
                             (state= to-state &whole) arg list)))
               (go LOOP)))
           ;; Fell through, so warn if desired, and fall through some more.
           (unless silent (report-suspicious "variable" arg)))

         ;; Handle a lambda variable
         (when (logbitp state (bits &allow-other-keys ; Not a collecting state.
                                    :post-env :post-rest :post-more))
           (croak (sb-format:tokens
                   "expected lambda list keyword at ~S in: ~
                    ~/sb-impl:print-lambda-list/")
                  arg list))
         (let ((item (list arg)))
           (setq tail (if tail (setf (cdr tail) item) (begin-list item))))
         (when (logbitp state (bits &rest &more &whole &environment))
           (let ((next (cond ((state= state &rest) (state :post-rest))
                             ((state= state &whole) (state :required))
                             ((state= state &more) ; Should consume 2 symbols
                              (if (cdr more) (state :post-more)))
                             ;; Current state must be &ENVIRONMENT
                             ((and (state= saved-state :required) (not required))
                              (state :required)) ; Back to start state
                             (t
                              (state :post-env))))) ; Need a lambda-list-keyword
             (when next ; Advance to new state.
               (setq state next tail nil))))
         (go LOOP))

      #-sb-xc-host ;; Supress &OPTIONAL + &KEY syle-warning on xc host
      (when (and (logtest (bits &key) seen) optional (not silent))
        (style-warn-once
         list
         (make-condition '&optional-and-&key-in-lambda-list
                         :format-control (sb-format:tokens
                                          "&OPTIONAL and &KEY found in the same lambda list: ~
                                           ~/sb-impl:print-lambda-list/")
                         :format-arguments (list list))))

      ;; For CONTEXT other than :VALUES-TYPE/:FUNCTION-TYPE we reject
      ;; illegal list elements. Type specifiers have arbitrary shapes,
      ;; such as (VALUES (ARRAY (MUMBLE) (1 2)) &OPTIONAL (MEMBER X Y Z)).
      ;; But why don't we reject constant symbols here?
      (unless (member context '(:values-type :function-type))
        (when whole
          ;; Refer to the comment above the :destructuring-whole test
          ;; in lambda-list.pure as to why &WHOLE has two personalities.
          (funcall (if (logtest (bits &environment) accept)
                       #'need-symbol #'need-bindable)
                   (car whole) "&WHOLE argument"))
        (dolist (arg required)
          (if (eq context 'defmethod)
              (unless (or (and (symbolp arg) (not (null arg)))
                          (and (listp arg) (singleton-p (cdr arg))))
                (croak "arg is not a non-NIL symbol or a list of two elements: ~A"
                       arg))
              (need-bindable arg "Required argument")))
        ;; FIXME: why not check symbol-ness of supplied-p variables now?
        (flet ((scan-opt/key (list what-kind description)
                 (dolist (arg list)
                   (when (defaultp arg what-kind)
                     ;; FIXME:  (DEFUN F (&OPTIONAL (A B C D)) 42) crashes the
                     ;; compiler, but not as consequence of the new parser.
                     ;; (This is not a regression)
                     (destructuring-bind (var &optional default sup-p) arg
                       (if (and (consp var) (eq what-kind '&key))
                           (cond ((singleton-p (cdr var))
                                  (destructuring-bind (keyword-name var) var
                                    (unless (symbolp keyword-name)
                                      (croak "keyword-name in ~S is not a symbol" arg))
                                    (need-bindable var description)))
                                 (t
                                  (croak "invalid &KEY syntax: ~S" var)))
                           (need-bindable var description))
                       ;; Inform the user about a possibly malformed
                       ;; destructuring list (&OPTIONAL (A &OPTIONAL B)).
                       ;; It's technically legal but unlikely to be right,
                       ;; as A's default form is the symbol &OPTIONAL,
                       ;; which is an unlikely name for a local variable,
                       ;; and an illegal name for a DEFVAR or such,
                       ;; being in the CL package.
                       (unless silent
                         (check-suspicious "default" default)
                         (check-suspicious "supplied-p variable" sup-p)))))))
          (scan-opt/key optional '&optional "&OPTIONAL parameter name")
          (when rest
            (need-bindable (car rest) "&REST argument"))
          (scan-opt/key keys '&key "&KEY parameter name")
          (dolist (arg aux)
            (when (defaultp arg '&aux)
              ;; FIXME: also potentially compiler-crash-inducing
              (destructuring-bind (var &optional init-form) arg
                (declare (ignore init-form))
                ;; &AUX is not destructured
                (need-symbol var "&AUX parameter name"))))))

    ;; Voila.
      (values (logior seen (if (oddp rest-bits) (bits &body) 0))
              required optional (or rest more) keys aux env whole))))

;;; Check the variable names and keywords in the sections of the
;;; lambda list for illegal and repeated ones.
;;;
;;; Can be wrapped around PARSE-LAMBDA-LIST like this:
;;;
;;;   (multiple-value-call #'check-lambda-list-names
;;;     (parse-lambda-list ...)
;;;     :context ...)
(defun check-lambda-list-names (llks required optional rest keys aux env whole
                                &key
                                  (context "an ordinary lambda list")
                                  (signal-via #'compiler-error)
                                  (allow-symbol-macro t))
  (let ((names (make-repeated-name-check :signal-via signal-via))
        (keywords (make-repeated-name-check
                   :kind "keyword" :signal-via signal-via)))
    (flet ((check-name (name &key allow-repeating)
             (check-variable-name-for-binding
              name :context context :signal-via signal-via
              :allow-symbol-macro allow-symbol-macro)
             (unless allow-repeating
               (funcall names name))))
      (mapc #'check-name required)
      (mapc (lambda (spec)
              (multiple-value-bind (name default suppliedp-var)
                  (parse-optional-arg-spec spec)
                (declare (ignore default))
                (check-name name)
                (when suppliedp-var
                  (check-name (first suppliedp-var)))))
            optional)
      (mapc #'check-name rest)
      (mapc (lambda (spec)
              (check-name (if (consp spec)
                              (car spec)
                              spec)
                          :allow-repeating t))
            aux)
      (mapc (lambda (spec)
              (multiple-value-bind (keyword name default suppliedp-var)
                  (parse-key-arg-spec spec)
                (declare (ignore default))
                (check-name name)
                (when suppliedp-var
                  (check-name (first suppliedp-var)))
                (funcall keywords keyword)))
            keys)))
  (values llks required optional rest keys aux env whole))

;;; Construct an abstract representation of a destructuring lambda list
;;; from its source form, recursing as necessary.
;;; Warn if it looks like a default expression will cause pattern mismatch.
;;; There are other things we could issue style warnings about:
;;; - a &REST arg that destructures preceded by any optional args.
;;;   It's suspicious because if &REST destructures, then in essence it
;;;   must not be NIL, which means the optionals aren't really optional.
(defun parse-ds-lambda-list (lambda-list
                             &key silent
                             (condition-class 'simple-program-error))
  (multiple-value-bind (llks required optional rest keys aux env whole)
      (parse-lambda-list lambda-list
                         :accept (lambda-list-keyword-mask 'destructuring-bind)
                         :context 'destructuring-bind
                         :silent silent :condition-class condition-class)
   (declare (ignore env) (notinline mapcar))
   (labels ((parse (list)
              (if (listp list)
                  (parse-ds-lambda-list list :silent silent)
                  list))
            (parse* (list arg-specifier)
              (let ((parse (parse list)))
                (when (and (not silent) (vectorp parse)) ; is destructuring
                  (let ((default (and (cdr arg-specifier) ; have an explicit default
                                      (cadr arg-specifier))))
                    (when (and (cl:constantp default)
                               (not (ds-lambda-list-match-p
                                     (#+sb-xc constant-form-value #-sb-xc eval
                                      default)
                                     (meta-abstractify-ds-lambda-list parse))))
                      (style-warn
                       "Default expression ~S does not match ~S in ~S"
                       default list lambda-list))))
                parse)))
     (vector llks
             (mapcar #'parse whole) ; a singleton or NIL
             (mapcar #'parse required)
             (mapcar (lambda (x)
                       (if (atom x) x (cons (parse* (car x) x) (cdr x))))
                     optional)
             (mapcar #'parse rest) ; a singleton or NIL
             (mapcar (lambda (x)
                       (if (typep x '(cons cons))
                           (cons (list (caar x) (parse* (cadar x) x)) (cdr x))
                           x))
                     keys)
             aux))))

;; Bind the parts of the abstract representation of a destructuring
;; lambda list, a (SIMPLE-VECTOR 7), to individual symbols.
(defmacro with-ds-lambda-list-parts ((&rest parts-names) parts &body body)
  (aver (<= 1 (length parts-names) 7))
  (once-only ((parts `(the (simple-vector 7) ,parts)))
    `(let ,(loop for i from 0 for sym in parts-names
                 when sym collect `(,sym (svref ,parts ,i)))
       ,@body)))

;;; Split an optional argument specifier into the bound variable
;;; or destructuring pattern, the default, and supplied-p var.
;;; If present the supplied-p var is in a singleton list.
;;; DEFAULT should be specified as '* when parsing a DEFTYPE lambda-list.
(defun parse-optional-arg-spec (spec &optional default)
  (etypecase spec
    (symbol (values spec default nil nil))
    (cons (values (car spec)
                  (if (cdr spec) (cadr spec) default)
                  (cddr spec)
                  (when (cdr spec) t)))))

;;; Split a keyword argument specifier into the keyword, the bound variable
;;; or destructuring pattern, the default, and supplied-p var.
;;; If present the supplied-p var is in a singleton list.
;;; DEFAULT should be specified as '* when parsing a DEFTYPE lambda-list.
(defun parse-key-arg-spec (spec &optional default)
  (etypecase spec
    (symbol (values (keywordicate spec) spec default nil nil))
    (cons (destructuring-bind (var &optional (def default defaultp) . sup-p-var)
              spec
            (if (symbolp var)
                (values (keywordicate var) var def sup-p-var defaultp)
                (values (car var) (cadr var) def sup-p-var defaultp))))))

;;; Return a "twice abstracted" representation of DS-LAMBDA-LIST that removes
;;; all variable names, &AUX parameters, supplied-p variables, and defaults.
;;; The result is a list with trailing suffixes of NIL dropped, and which can
;;; be given to an AST matcher yielding a boolean answer as to whether some
;;; input matches, with one caveat: Destructured &OPTIONAL or &KEY default
;;; forms may cause failure of a destructuring-bind due to inner expressions
;;; causing mismatch. In most cases this can not be anticipated.
(defun meta-abstractify-ds-lambda-list (parsed-ds-lambda-list)
  (labels ((process-opt/key (x) (recurse (if (listp x) (car x) x)))
           (recurse (thing)
             (when (symbolp thing)
               (return-from recurse t))
             (with-ds-lambda-list-parts (llks whole req opt rest keys) thing
               (let ((keys
                      (when (ll-kwds-keyp llks)
                        (cons (ll-kwds-allowp llks)
                              (mapcar (lambda (x)
                                        (cons (parse-key-arg-spec x)
                                              (if (typep x '(cons cons))
                                                  (recurse (cadar x))
                                                  (process-opt/key x))))
                                      keys))))
                     ;; Compute reversed representation of req, opt, rest.
                     (repr (list (when rest (recurse (car rest)))
                                 (mapcar #'process-opt/key opt)
                                 (mapcar #'recurse req))))
                 ;; If &KEYS are present, then req, opt, rest must be too.
                 ;; But if not, then pop leading NILs (which become trailing
                 ;; NILs). Missing parts aren't stored.
                 ;; A degenerate ds-lambda-list accepting 0 args is just ().
                 (unless keys
                   (loop (if (or (null repr) (car repr)) (return) (pop repr))))
                 (let ((result (nreconc repr keys))
                       (whole (car whole)))
                   (if (vectorp whole) ; Destructuring. Ugh.
                       ;; The input must match two things - a tree implied by
                       ;; the nested &WHOLE, and a tree that contains it.
                       `(:both ,(recurse whole) ,@result)
                       result))))))
    (recurse parsed-ds-lambda-list)))

;; Construct a lambda list from sublists.
;; If &WHOLE and REST are present, they must be singleton lists.
;; Any sublists that were obtained by parsing a destructuring
;; lambda list must be supplied in their unparsed form.
(defun make-lambda-list (llks whole required &optional optional rest keys aux)
  (append (when whole (cons '&whole whole))
          required
          (when (logtest (lambda-list-keyword-mask '&optional) llks)
            (cons '&optional optional))
          (let ((restp (ll-kwds-restp llks)))
            (if (and rest (not restp)) ; lambda list was "dotted"
                (car rest)
                (append
                 (when rest (cons restp rest))
                 (if (ll-kwds-keyp llks) (cons '&key keys)) ; KEYS can be nil
                 (if (ll-kwds-allowp llks) '(&allow-other-keys))
                 ;; Should &AUX be inserted even if empty? Probably not.
                 (if aux (cons '&aux aux)))))))

;;; Produce a destructuring lambda list from its internalized representation,
;;; excluding any parts that don't constrain the shape of the expected input.
;;; &AUX, supplied-p vars, and defaults do not impose shape constraints.
(defun unparse-ds-lambda-list (parsed-lambda-list &key cache (remove-defaults t))
  (cond ((symbolp parsed-lambda-list) parsed-lambda-list)
        ((cdr (assq parsed-lambda-list (cdr cache))))
        (t
         (with-ds-lambda-list-parts (llks whole req optional rest keys)
             parsed-lambda-list
           (labels ((process-opt (spec)
                      (if (atom spec)
                          spec
                          (cons (recurse (car spec)) (maybe-default spec))))
                    (maybe-default (spec)
                      (let ((def (cdr spec)))
                        (when (and def (not remove-defaults))
                          (list (car def))))) ; Remove any supplied-p var.
                    (recurse (x) (unparse-ds-lambda-list x :cache cache :remove-defaults remove-defaults))
                    (memoize (input output)
                      (when cache (push (cons input output) (cdr cache)))
                      output))
             (memoize
              parsed-lambda-list
              (make-lambda-list
               llks
               ;; &WHOLE is omitted unless it destructures something.
               (when (vectorp (car whole)) (list (recurse (car whole))))
               (mapcar #'recurse req)
               (mapcar #'process-opt optional)
               (when rest (list (recurse (car rest))))
               (mapcar (lambda (x)
                         (if (typep x '(cons cons))
                             (cons (list (caar x) (recurse (cadar x)))
                                   (maybe-default x))
                             (process-opt x)))
                       keys))))))))

;;; Return the list of variables bound by a destructuring lambda list.
;;; One purpose for this is to help expand destructuring-bind using
;;; a strategy that delivers values to an MV-BIND.
;;; It would otherwise be difficult to wrap a condition handler
;;; around only the binding creation forms and not the body
;;; of the destructuring-bind. Consider e.g.
#|
   (DESTRUCTURING-BIND (A &OPTIONAL (B 0) (C 'DEF)) L (DOER))
     -> (multiple-value-bind (a b c)
            (handler-bind ((error (lambda (c) (return-from somewhere))))
              (values (pop (the cons L))
                      (if L (pop L) 0)
                      (cond ((endp L) 'def)
                            ((endp (cdr L)) (car L))
                            (t (error "Excess args")))))
          (doer))
|#
(defun ds-lambda-list-variables (parsed-lambda-list &optional (include-aux t))
  (collect ((output))
    (labels ((recurse (x) (if (vectorp x) (scan x) (output x)))
             (copy (x) (dolist (elt x) (recurse elt)))
             (suppliedp-var (spec) (if (cddr spec) (output (third spec))))
             (scan (parts)
               (with-ds-lambda-list-parts (nil whole req opt rest key aux) parts
                 (copy whole)
                 (copy req)
                 (dolist (x opt)
                   (cond ((symbolp x) (output x))
                         (t (recurse (car x))
                            (suppliedp-var x))))
                 (copy rest)
                 (dolist (x key)
                   (cond ((symbolp x) (output x))
                         (t (let ((k (car x)))
                              (if (symbolp k) (output k) (recurse (cadr k)))
                              (suppliedp-var x)))))
                 (when include-aux
                   (dolist (x aux)
                     (output (if (symbolp x) x (car x))))))))
      (recurse parsed-lambda-list)
      (output))))

;;; Return T if OBJECT matches TEMPLATE, where TEMPLATE is a meta-abstractified
;;; destructuring lambda list. Mnemonic: the arguments are like TYPEP.
;;; [Indeed the AST could be a monstrous type specifier involving {CONS,AND,OR}
;;; except for lambda lists that involve keywords.]
;;;
(defun ds-lambda-list-match-p (object template)
  (macrolet ((pop-template () '(pop (truly-the list template)))
             (accept ()
               '(unless template (return-from recurse (null args))))
             (fail ()
               '(return-from recurse nil)))
    ;; When a failure occurs, we could return all the way out, but that would
    ;; mean establishing a dynamic exit. Instead let failure bubble up.
    (labels ((recurse (template args)
               (accept) ; Exit if no required, optional, rest, key args.
               (when (eq (car (truly-the list template)) :both)
                 (return-from recurse
                   (and (recurse (cadr template) args)
                        (recurse (cddr template) args))))
               ;; Required args
               (dolist (subpat (pop-template)) ; For each required argument
                 (let ((arg (if (atom args) (fail) (pop args))))
                   (when (and (listp subpat) (not (recurse subpat arg)))
                     (fail))))
               (accept) ; Exit if no optional, rest, key args.
               ;; &OPTIONAL args
               (dolist (subpat (pop-template))
                 (let ((arg (cond ((not (listp args)) (fail))
                                  ((null args)
                                   ;; Why not just return T now?
                                   ;; Because destructured &REST maybe.
                                   (if template
                                       (return)
                                       (return-from recurse t)))
                                  (t (pop args)))))
                   (when (and (listp subpat) (not (recurse subpat arg)))
                     (fail))))
               (accept) ; Exit if no rest, key args.
               ;; If REST is not a cons, that's fine - it's either T, meaning
               ;; that it was present but not a pattern, or NIL, meaning
               ;; absent, in which case &KEY must have been present,
               ;; otherwise the preceding (ACCEPT) would have returned.
               (let ((rest (pop-template)))
                 (when (and (consp rest) (not (recurse rest args)))
                   (fail)))
               (when (null template) ; No keys.
                 (return-from recurse t))
               ;; Now test all keywords against the allowed ones, even if
               ;; &ALLOW-OTHER-KEYS was present. Any key's value might bind
               ;; to a subpattern, and the lambda list could be insane as well:
               ;;   (&KEY ((:allow-other-keys (x)) '(foo)))
               ;; where the value of :ALLOW-OTHER-KEYS must apparently
               ;; be a cons. Yeesh.
               (prog ((allowp (if (pop template) t 0)) seen-other)
                LOOP
                  (when (null args)
                    (return (or (not seen-other) (eq allowp t))))
                  (unless (listp args) (return nil))
                  (let* ((next (cdr args))
                         (key (car args))
                         (cell (assq key template)))
                    (unless (consp next) (return nil))
                    (if (not cell)
                        (setq seen-other t)
                        (let ((pattern (cdr cell)))
                          (when (and (listp pattern)
                                     (not (recurse pattern (car next))))
                            (fail))))
                    (when (and (eq key :allow-other-keys) (eql allowp 0))
                      (setq allowp (if (car next) t nil)))
                    (setq args (cdr next)))
                  (go loop))))
      (recurse template object))))

;;; Return the AST that recognizes inputs matching DS-LAMBDA-LIST.
(defun ds-lambda-list-matcher (ds-lambda-list)
  (meta-abstractify-ds-lambda-list (parse-ds-lambda-list ds-lambda-list)))

;;; Emit a form to test whether INPUT matches DS-LAMBDA-LIST.
;;; It's up to this function to decide (perhaps based on policy)
;;; how to generate the code. There are a few simple cases that avoid
;;; function calls. Others could easily be added. e.g. a 2-list could be:
;;;   (TYPEP INPUT '(CONS T (CONS T NULL)))
(defun emit-ds-lambda-list-match (input ds-lambda-list)
  (let ((matcher (ds-lambda-list-matcher ds-lambda-list)))
    ;; To match exactly 1 required arg, use SINGLETON-P.
    (cond ((equal matcher '((t))) `(singleton-p ,input))
          ;; Matching 0 required, 0 optional, and rest is trivially T.
          ((equal matcher '(() () t)) t)
          (t `(ds-lambda-list-match-p ,input ',matcher)))))

;;; Emit a correctness check for one level of structure in PARSED-LAMBDA-LIST
;;; which receives values from INPUT.
;;; MACRO-CONTEXT provides context for the diagnostic message.
;;; MEMO-TABLE is an alist of previously-unparsed parsed-lambda-lists.
;;; The checker returns INPUT if it was well-formed, or signals an error.
;;;
;;; There is a better way (not implemented) to check &KEY arguments: assume
;;; optimistically that unknown/duplicate keywords aren't frequent, and perform
;;; all GETF operations for known keywords into temp vars; count the ones that
;;; found something, and compare to the plist length/2.  If not equal, then do
;;; a further check. Otherwise we've done most of the work of parsing;
;;; just move the temps into their final places in turn.
;;;
(defun emit-ds-bind-check (parsed-lambda-list input macro-context memo-table)
  (with-ds-lambda-list-parts (llks nil req opt rest keys) parsed-lambda-list
    (let* ((display (unparse-ds-lambda-list parsed-lambda-list :cache memo-table))
           (pattern `',(if macro-context (cons macro-context display) display))
           (min (length req))
           (max (+ min (length opt)))
           (bounds (list min max)))
      (cond ((ll-kwds-keyp llks)
             `(,(if (typep macro-context
                           '(cons t (cons t (eql define-compiler-macro))))
                    'cmacro-check-ds-list/&key
                    'check-ds-list/&key)
                ,input ,@bounds ,pattern
                ,(unless (ll-kwds-allowp llks)
                   (map 'vector #'parse-key-arg-spec keys))))
            ;; The case that need not check anything at all:
            ;; no keys, no required, no optional, and a &rest arg.
            ((and rest (eql max 0)) input) ; nothing to check
            (rest `(check-ds-list/&rest ,input ,@bounds ,pattern))
            (t `(check-ds-list ,input ,@bounds ,pattern))))))

;;; Produce the binding clauses for a BINDING* form that destructures
;;; LAMBDA-LIST from input in DATA.
;;; EMIT-PRE-TEST, if true, will diagnose most (but not all) structural
;;; errors [*] before executing user-supplied code in defaulting forms.
;;; EXPLICIT-CAST is one of {THE, TRULY-THE, NIL} to insert casts or not.
;;; Optional MACRO-CONTEXT provides context for the error strings.
;;; DEFAULT-DEFAULT, which defaults to NIL, supplies the value for optional
;;; and key arguments which were absent and had no explicit defaulting form.
;;;
;;; Without explicit casts, the input must satisfy LISTP at each CAR/CDR step.
;;; If pre-tests were done and user code did not smash the input, then it
;;; will satisfy LISTP, and EXPLICIT-CAST may be specified as 'TRULY-THE
;;; to omit compiler-generated ("checkgen") tests. If pre-tests were not done,
;;; then EXPLICIT-CAST should be specified as 'THE to strengthen type tests
;;; into (THE CONS x) at mandatory arguments.
;;;
;;; [*] "Structural errors" are those due to mismatch of the input against
;;; the template; in the case of one list level, an error can be signaled
;;; before defaults are evaluated, but with nested destructuring, this is not
;;; always possible. Previously there was an attempt to check outer lists
;;; before proceeding to inner lists, but this required departure from
;;; customary left-to-right evaluation order of source forms as written.
;;; The new scheme seems more in accordance with other Lisp implementations.
;;; Portable code should probably not rely on the order in which structural
;;; errors are tested for. If input is well-formed - it matches the template -
;;; then there is no possibility of user code sensing the order in which
;;; well-formedness tests ran.
;;;
(defun expand-ds-bind (lambda-list data emit-pre-test explicit-cast
                       &optional macro-context default-default)
  (collect ((cache (list nil)) ; This is a "scratchpad" for the unparser.
            (bind))
    (labels
        (;; Bind VAR from VAL-FORM. VAR can be a symbol or ds-lambda-list.
         (bind-pat (var val-form)
           (if (symbolp var) (bind `(,var ,val-form)) (descend var val-form)))
         ;; Conditionally bind VAR from VAL-FORM based on SUP-P-FORM.
         (bind-if (sense sup-p-form val-form var sup-p-var def)
           (let* ((suppliedp (car sup-p-var)) ; could be nil
                  (vals (gen-test sense sup-p-form
                                  (if sup-p-var `(values ,val-form t) val-form)
                                  (if sup-p-var `(values ,def nil) def))))
             (cond ((not sup-p-var) (bind-pat var vals))
                   ((not (symbolp var))
                    (let ((var-temp (sb-xc:gensym))
                          (sup-p-temp (copy-symbol suppliedp)))
                      (bind `((,var-temp ,sup-p-temp) ,vals))
                      (descend var var-temp)
                      (bind `(,suppliedp ,sup-p-temp))))
                   ((eq var suppliedp)
                    (bind `((nil ,suppliedp) ,vals)))
                   (t
                    (bind `((,var ,suppliedp) ,vals))))))
         (gen-test (sense test then else)
           (cond ((eq sense t) `(if ,test ,then ,@(if else (list else))))
                 (else `(if ,test ,else ,then)) ; flip the branches
                 (t `(if (not ,test) ,then)))) ; invert the test
         (descend (parsed-lambda-list input)
           (with-ds-lambda-list-parts (llks whole required opt rest keys aux)
               parsed-lambda-list
             ;; There could be nothing but &AUX vars in the lambda list.
             ;; If nothing to bind from INPUT, then ignore "ds-check" result.
             ;; But if keywords are accepted, always call the checker.
             ;; A feature of BINDING* is that binding something to () means,
             ;; in theory, (MULTIPLE-VALUE-BIND () (EXPR) ...)
             ;; but in practice it becomes a binding of an ignored gensym.
             (let* ((bindings-p (or whole required opt rest keys))
                    (temp (and bindings-p (sb-xc:gensym))))
               (bind `(,temp
                       ,(cond ((or emit-pre-test (ll-kwds-keyp llks))
                               (emit-ds-bind-check parsed-lambda-list input
                                                   macro-context (cache)))
                              ((or bindings-p (not explicit-cast)) input)
                              ;; If nothing gets bound, then input must be NIL,
                              ;; unless &KEY is accepted, which was done above.
                              (t `(,explicit-cast null ,input)))))
               (setq input temp))
             ;; I would think it totally absurd to use something
             ;; other than a symbol for &WHOLE, but the spec allows it.
             (when whole (bind-pat (car whole) input))

             (flet ((cast/pop (typed-list-expr more-to-go)
                      `(prog1 (car ,typed-list-expr)
                         ,(if (or more-to-go rest (ll-kwds-keyp llks))
                              `(setq ,input (cdr ,input))
                              `(,explicit-cast null (cdr ,input))))))
               ;; Mandatory args. Only the rightmost need check that it sees
               ;; a CONS. The predecessors will naturally assert that the
               ;; input so far was of type LIST, which is enough.
               (do ((elts required (cdr elts)))
                   ((endp elts))
                 (bind-pat (car elts)
                           (if explicit-cast
                               (cast/pop `(,explicit-cast
                                           ,(if (cdr elts) 'list 'cons) ,input)
                                         (or (cdr elts) opt))
                               `(pop ,input))))
               ;; Optionals.
               (do ((elts opt (cdr elts)))
                   ((endp elts))
                 (multiple-value-bind (var def sup-p-var)
                     (parse-optional-arg-spec (car elts) default-default)
                   (bind-if t input
                            (if explicit-cast
                                (cast/pop `(,explicit-cast list ,input)
                                          (cdr elts))
                                `(pop ,input))
                            var sup-p-var def))))

             ;; The spec allows the inane use of (A B &REST (C D)) = (A B C D).
             ;; The former is less efficient, since it is "nested", only not.
             (when rest (bind-pat (car rest) input))

             ;; Keywords.
             (dolist (elt keys)
               (multiple-value-bind (keyword var def sup-p-var)
                   (parse-key-arg-spec elt default-default)
                 (let ((temp (sb-xc:gensym)))
                   (bind `(,temp (ds-getf ,input ',keyword)))
                   (bind-if :not `(eql ,temp 0) `(car (truly-the cons ,temp))
                            var sup-p-var def))))

             ;; &AUX bindings aren't destructured. Finally something easy.
             (dolist (elt aux)
               (multiple-value-bind (var val)
                   (if (listp elt) (values (car elt) (cadr elt)) elt)
                 (bind `(,var ,val)))))))

      (descend (parse-ds-lambda-list lambda-list) data)
      (bind))))

;;; Runtime support

;; Given FORM as the input to a compiler-macro, return the argument forms
;; that the called function would receive, skipping over FUNCALL.
(defun compiler-macro-args (form)
  (cdr (if (eql (car form) 'funcall) (cdr form) form)))

;; Extract the context from a destructuring-bind pattern as represented in
;; a call to a checking function. A "context" is a non-bindable subpattern
;; headed by :MACRO (if it came from any macro-like thing, e.g. DEFTYPE),
;; or :SPECIAL-FORM.
;; Return three values: CONTEXT-NAME, CONTEXT-KIND, and the real pattern.
(defun get-ds-bind-context (pattern)
  (let ((marker (car pattern)))
    (case (and (listp marker) (car marker))
      (:macro
       (let ((context (cdr marker)))
         (values (car context) (cdr context) (cdr pattern))))
      (:special-form
       (values (cdr marker) :special-form (cdr pattern)))
      (:eval
       (values nil :eval (cdr pattern)))
      (t
       (values nil 'destructuring-bind pattern)))))

;;; Helpers for the variations on CHECK-DS-mumble.
(defun ds-bind-error (input min max pattern)
  (multiple-value-bind (name kind lambda-list) (get-ds-bind-context pattern)
    #-sb-xc-host
    (declare (optimize allow-non-returning-tail-call))
    (case kind
     (:special-form
      ;; IR1 translators should call COMPILER-ERROR instead of
      ;; ERROR. To ingrain that knowledge into the CHECK-DS-foo
      ;; functions is a bit of a hack, but to do otherwise
      ;; changes how DS-BIND has to expand.
      (compiler-error 'sb-kernel::arg-count-error
                      :kind "special operator" :name name
                      :args input :lambda-list lambda-list
                      :minimum min :maximum max))
     #+sb-eval
     (:eval
      (error 'sb-eval::arg-count-program-error
             ;; This is stupid. Maybe we should just say
             ;;  "error parsing special form"?
             ;; It would be more sensible than mentioning
             ;; a random nonstandard macro.
             :kind 'sb-eval::program-destructuring-bind
             :args input :lambda-list lambda-list
             :minimum min :maximum max))
     (t
      (error 'sb-kernel::arg-count-error
             :kind kind :name name
             :args input :lambda-list lambda-list
             :minimum min :maximum max)))))

(defun check-ds-bind-keys (input plist valid-keys pattern)
  ;; Check just the keyword portion of the input in PLIST
  ;; against VALID-KEYS. If VALID-KEYS = NIL then we don't care what
  ;; the keys are - &ALLOW-OTHER-KEYS was present in the lambda-list,
  ;; and we don't care if non-symbols are found in keyword position.
  ;; Always enforce that the list has even length though.
  (let* (seen-allowp seen-other bad-key
         (tail plist)
         (problem
          (loop
           (when (null tail)
             (if seen-other
                 (return :unknown-keyword)
                 (return-from check-ds-bind-keys input)))
           (unless (listp tail) (return :dotted-list))
           (let ((next (cdr tail)))
             (when (null next) (return :odd-length))
             (unless (listp next) (return :dotted-list))
             (let ((key (car tail)))
               (when valid-keys
                 (if (eq key :allow-other-keys) ; always itself allowed
                     (unless seen-allowp
                       (setf seen-allowp t)
                       (when (car next) ; :allow-other-keys <non-nil>
                         (setf seen-other nil valid-keys nil)))
                     (unless (or seen-other
                                 (find key (truly-the simple-vector valid-keys)
                                       :test 'eq))
                       (setq seen-other t bad-key key)))))
             (setq tail (cdr next))))))
    (multiple-value-bind (kind name) (get-ds-bind-context pattern)
      #-sb-xc-host
      (declare (optimize allow-non-returning-tail-call))
      ;; KLUDGE: Compiling (COERCE x 'list) transforms to COERCE-TO-LIST,
      ;; but COERCE-TO-LIST is an inline function not yet defined, and
      ;; its subsequent definition would signal an inlining failure warning.
      (declare (notinline coerce))
      (error 'sb-kernel::defmacro-lambda-list-broken-key-list-error
             :kind kind :name name
             :problem problem
             :info (if (eq problem :unknown-keyword)
                       ;; show any one unaccepted keyword
                       (list bad-key (coerce valid-keys 'list))
                       plist)))))

(macrolet ((scan-req-opt ((input min max pattern list-name actual-max)
                          &key if-list-exhausted if-max-reached)
             ;; Decide whether the input matches up to the end of
             ;; the required and/or optional arguments.
             ;; MAX is the limit on number of CDR operations performed
             ;; in the loop. ACTUAL-MAX describes the upper bound
             ;; in a condition reporting function.
             ;; e.g. (A &OPTIONAL B &REST C) has MAX = 2, ACTUAL-MAX = NIL.
             ;;      The input must be a proper list up to 2 arguments,
             ;;      but beyond that may be dotted.
             `(let ((,list-name ,input) (count ,max))
                (declare (type index count))
                (loop (when (zerop count) (return ,if-max-reached))
                      (when (null ,list-name)
                        (return
                         (if (< (- max count) ,min)
                             (ds-bind-error ,input ,min ,actual-max ,pattern)
                             ,if-list-exhausted)))
                      (unless (listp ,list-name) ; dotted list error
                        (return
                         (ds-bind-error ,input ,min ,actual-max ,pattern)))
                      (decf count)
                      (setq ,list-name (cdr ,list-name))))))

  ;; Assert that INPUT has the requisite number of elements as
  ;; specified by MIN/MAX. PATTERN does not contain &REST or &KEY.
  (defun check-ds-list (input min max pattern)
    (declare (type index min max) (optimize speed))
    (scan-req-opt (input min max pattern list max)
                  ;; If 'count' became zero, then since there was
                  ;; no &REST, the LIST had better be NIL.
                  :if-max-reached
                  (if list (ds-bind-error input min max pattern) input)
                  ;; The loop checks for dotted tail and >= MIN elements,
                  ;; so end of list means a valid match to the pattern.
                  :if-list-exhausted input))

  ;; As above, but the pattern contains &REST.
  ;; Elements beyond the final optional arg can form a dotted list.
  (defun check-ds-list/&rest (input min max pattern)
    (declare (type index min max) (optimize speed))
    (scan-req-opt (input min max pattern list nil)
                  :if-list-exhausted input :if-max-reached input))

  ;; The pattern contains &KEY. Anything beyond the final optional arg
  ;; must be a well-formed property list regardless of existence of &REST.
  (defun check-ds-list/&key (input min max pattern valid-keys)
    (declare (type index min max) (optimize speed))
    (scan-req-opt (input min max pattern list nil)
                  :if-list-exhausted input
                  :if-max-reached (check-ds-bind-keys
                                   input list valid-keys pattern)))

    ;; Compiler-macro lambda lists are macro lambda lists -- meaning that
    ;; &key ((a a) t) should match a literal A, not a form evaluating to A
    ;; as in an ordinary lambda list.
    ;;
    ;; That, however, breaks the evaluation model unless A is also a
    ;; constant evaluating to itself. So, signal a condition telling the
    ;; compiler to punt on the expansion.
    ;; Moreover it has to be assumed that any non-constant might
    ;; evaluate to :ALLOW-OTHER-KEYS.
    ;;
    ;; The reason this is its own function is for separation of concerns.
    ;; Suppose that CHECK-DS-LIST/&KEY had a short-circuit exit wherein
    ;; seeing ":ALLOW-OTHER-KEYS <non-nil>" stopped testing for keywords in
    ;; the accepted list, but instead quickly scanned for a proper tail.
    ;; (It doesn't, but suppose it did). A compiler-macro must nonetheless
    ;; finish looking for all non-constant symbols in keyword positions.
    ;; More realistically, if the optimization for &KEY mentioned above
    ;; EMIT-DS-BIND-CHECK were implemented, where perhaps we elide a call
    ;; to validate keywords, a compiler-macro is probably always best
    ;; handled by a out-of-line call on account of the extra hair.
    ;;
  (defun cmacro-check-ds-list/&key (input min max pattern valid-keys)
    (declare (type index min max) (optimize speed))
    (scan-req-opt (input min max pattern list nil)
                  :if-list-exhausted input
                  :if-max-reached
                  ;; Signal a condition if the compiler should give up
                  ;; on expanding. Well-formedness of the plist
                  ;; makes no difference, since CHECK-DS-BIND-KEYS is stricter.
                  ;; If the condition isn't handled, we just press onward.
                  (let ((plist list))
                    (loop (when (atom plist) (return))
                          (let ((key (pop plist)))
                            (when (atom plist) (return))
                            (pop plist)
                            (unless (or (keywordp key)
                                        (and (symbolp key)
                                             (cl:constantp key)
                                             (eq key (symbol-value key))))
                              (signal 'compiler-macro-keyword-problem
                                      :argument key))))
                    (check-ds-bind-keys input list valid-keys pattern)))))

;; Like GETF but return CDR of the cell whose CAR contained the found key,
;; instead of CADR; and return 0 for not found.
;; This helps destructuring-bind slightly by avoiding a secondary value as a
;; found/not-found indicator, and using 0 is better for backends which don't
;; wire a register to NIL. Also, NIL would accidentally allow taking its CAR
;; if the caller were to try, whereas we'd want to see a explicit error.
(defun ds-getf (place indicator)
  (do ((plist place (cddr plist)))
      ((null plist) 0)
    (cond ((atom (cdr plist))
           (error 'simple-type-error
                  :format-control "malformed property list: ~S."
                  :format-arguments (list place)
                  :datum (cdr plist)
                  :expected-type 'cons))
          ((eq (car plist) indicator)
           ;; Typecheck the next cell so that calling code doesn't get an atom.
           (return (the cons (cdr plist)))))))

;;; Make a lambda expression that receives an s-expression, destructures it
;;; according to LAMBDA-LIST, and executes BODY.
;;; NAME and KIND provide error-reporting context.
;;; DOC-STRING-ALLOWED can be :INTERNAL to allow a docstring which is kept
;;; inside the lambda, or :EXTERNAL to pull it out and return it, or NIL.
;;; ENVIRONMENT can be NIL to disallow an &ENVIRONMENT variable,
;;; or :IGNORE to allow it, but bind the corresponding symbol to NIL.
;;; WRAP-BLOCK, if true, will place a block named NAME around body.
;;;
;;; The secondary value is a docstring, if requested as :EXTERNAL.
;;;
;;; The lambda contains an internal declaration of its argument list
;;; that discards &ENVIRONMENT, &WHOLE, and/or anything else that does
;;; not document the expected list shape.
;;;
;;; The CLtl2 name for this operation is PARSE-MACRO.
(defun make-macro-lambda
    (lambda-name lambda-list body kind name
     &key (accessor 'cdr) (doc-string-allowed :internal)
          ((:environment envp) t) (wrap-block name))
  (declare (type (member t nil :ignore) envp))
  (declare (type (member nil :external :internal) doc-string-allowed))
  (binding* (((forms decls docstring) (parse-body body doc-string-allowed))
             ;; Parse the lambda list, but not recursively.
             ((llks req opt rest keys aux env whole)
              (parse-lambda-list
               lambda-list
               :accept (logior
                        (if envp (lambda-list-keyword-mask '&environment) 0)
                        (lambda-list-keyword-mask 'destructuring-bind))
               ;; Why :silent? We first parse to deconstruct and reconstruct
               ;; without &WHOLE and &ENV, which is an implementation detail.
               ;; When it comes to actually processing the entire lambda
               ;; list again, that's when any warning(s) will be issued.
               :context :macro :silent t))
             ((outer-decls decls) (extract-var-decls decls (append env whole)))
             (ll-env (when (eq envp t) (or env (list (make-symbol "ENV")))))
             ;; We want a hidden WHOLE arg for the lambda - not the user's -
             ;; in case one was present and declared IGNORE.
             ;; Conversely, if the user asks for &WHOLE, doesn't use it,
             ;; and doesn't declare it ignored, that deserves a warning.
             (ll-whole (make-symbol "EXPR"))
             ;; Then bind the user's WHOLE from the lambda's.
             (ll-aux
              (append (when (and (eq envp :ignore) env) `((,(car env) nil)))
                      (when whole `((,(car whole) ,ll-whole)))))
             ;; Drop &WHOLE and &ENVIRONMENT
             (new-ll (make-lambda-list llks nil req opt rest keys aux))
             #-sb-xc-host
             (*lexenv* (process-muffle-decls decls
                                             (if (boundp '*lexenv*)
                                                 *lexenv*
                                                 (make-null-lexenv))))
             (parse (parse-ds-lambda-list new-ll))
             ((declared-lambda-list decls)
              (let ((ll
                      (loop for (nil . declarations) in decls
                            thereis
                            (loop for x in declarations
                                  when (and (consp x)
                                            (eql (car x) 'lambda-list))
                                  return x))))
                (values
                 (or ll
                     ;; Normalize the lambda list by unparsing.
                     `(lambda-list ,(unparse-ds-lambda-list parse :remove-defaults nil)))
                 (if ll
                     (loop for (declare . declarations) in decls
                           collect (list* declare
                                          (remove 'lambda-list declarations :key #'car)))
                     decls))))
             (variables (ds-lambda-list-variables parse nil)))
    ;; Signal a style warning for duplicate names, but disregard &AUX variables
    ;; because most folks agree that (LET* ((X (F)) (X (G X))) ..) makes sense
    ;; - some would even say that it is idiomatic - and &AUX bindings are just
    ;; LET* bindings.
    ;; The obsolete PARSE-DEFMACRO signaled an error, but that seems harsh.
    ;; Other implementations permit (X &OPTIONAL X),
    ;; and the allowance for nesting makes this issue even less clear.
    (mapl (lambda (tail)
            (when (memq (car tail) (cdr tail))
              (style-warn-once lambda-list "variable ~S occurs more than once"
                               (car tail))))
          (append whole env variables))
    ;; Maybe kill docstring, but only under the cross-compiler.
    #+(and (not sb-doc) sb-xc-host) (setq docstring nil)
    ;; Note that we *NEVER* declare macro lambdas as a toplevel named lambda.
    ;; Code such as:
    ;;  `(setf (symbol-function ',myfun) ,(make-macro-lambda whatever))
    ;; with the intent to render MYFUN as having status as a known global function
    ;; ("known" in the sense of existing at all, and preventing an "undefined"
    ;; warning, _not_ "known" in the sense of 'fndb' knowing about it specially),
    ;; then that code is misguided.  Macro-like objects can not cause global
    ;; function names to be defined. Only DEFUN can do that.
    (values `(,@(if lambda-name `(named-lambda ,lambda-name) '(lambda))
                (,ll-whole ,@ll-env ,@(and ll-aux (cons '&aux ll-aux)))
              ,@(when (and docstring (eq doc-string-allowed :internal))
                  (prog1 (list docstring) (setq docstring nil)))
              ;; MACROLET doesn't produce an object capable of reflection,
              ;; so don't bother inserting a different lambda-list.
              ,@(unless (eq kind 'macrolet)
                  `((declare ,declared-lambda-list)))
              ,@(if outer-decls (list outer-decls))
              ,@(and (not env) (eq envp t) `((declare (ignore ,@ll-env))))
              ,@(sb-c:macro-policy-decls)
              (,@(if kind
                     `(named-ds-bind ,(if (eq kind :special-form)
                                          `(:special-form . ,name)
                                          `(:macro ,name . ,kind)))
                     '(destructuring-bind))
               ,new-ll (,accessor ,ll-whole)
               #-sb-xc-host
               (declare (constant-value ,@variables))
               ,@decls
               ,@(if wrap-block
                     `((block ,(fun-name-block-name name) ,@forms))
                     forms)))
            docstring)))

;;; Functions should probably not retain &AUX variables as part
;;; of their reflected lambda list, but this is selectable
;;; because some users might claim that dropping &AUX is wrong.
;;; For system code, it's a measurably large waste of space,
;;; given how DEFTRANSFORM and a few other macros expand such
;;; that argument parsing is performed in &AUX var initforms.
(defvar *strip-lamba-list-retain-aux* #+sb-xc t #-sb-xc nil)

;;; Return LAMBDA-LIST with some pieces removed.
(defun strip-lambda-list (lambda-list how)
  (handler-case (parse-lambda-list lambda-list :silent t)
   (error () lambda-list)
   (:no-error (llks req opt rest keys aux &rest ignore)
     (declare (ignore ignore))
     (multiple-value-bind (opt keys aux)
         (ecase how
          (:arglist
           (values opt keys (if *strip-lamba-list-retain-aux* aux nil)))
          ;; The name of an anonymous lambda is an arbitrary list,
          ;; not necessarily the original list.
          (:name (values (mapcar #'parse-optional-arg-spec opt); Keep name.
                         (mapcar #'parse-key-arg-spec keys) ; Keep keyword.
                         nil))) ; Discard &AUX vars
       (let ((new (make-lambda-list llks nil req opt rest keys aux)))
         ;; It is harmful to the space-saving effect of this function
         ;; if reconstituting the list results in an unnecessary copy.
         (if (equal new lambda-list) lambda-list new))))))

(declaim (ftype (sfunction * function) make-repeated-name-check))
(defun make-repeated-name-check (&key
                                   (kind "variable")
                                   (context "lambda list")
                                   (signal-via #'compiler-error))
  (let ((seen '()))
    (lambda (name)
      (when (member name seen :test #'eq)
        (funcall signal-via "~@<The ~A ~S occurs more than once in ~
                             the ~A.~@:>"
                 kind name context))
      (push name seen)
      name)))

;;; Verify that NAME is a legal name for a variable.
(declaim (ftype (function (t &key
                             (:context t) (:allow-special t) (:allow-symbol-macro t)
                             (:signal-via (or symbol function)))
                          (values symbol keyword))
                check-variable-name-for-binding))
(defun check-variable-name-for-binding (name
                                        &key
                                          context
                                          (allow-special t)
                                          (allow-symbol-macro t)
                                          (signal-via #'compiler-error))
  (check-variable-name name :signal-via signal-via)
  (flet ((lose (kind)
           (funcall signal-via
                    (sb-format:tokens "~@<~/sb-ext:print-symbol-with-prefix/ names a ~
                               ~A, and cannot be used in ~A.~:@>")
                    name kind context)))
    (let ((kind (info :variable :kind name)))
      (case kind
        (:macro
         (unless allow-symbol-macro
           (program-assert-symbol-home-package-unlocked
            :compile name (format nil "lexically binding global ~
                                       symbol-macro ~~A in ~A"
                                  context))))
        ((:constant)
         (lose "defined constant"))
        ((:global)
         (lose "global lexical variable"))
        (:special
         (unless allow-special
           (lose "special variable"))))
      (values name kind))))

;; This is a variant of destructuring-bind that provides the name
;; of the containing construct in generated error messages.
(macrolet (#+sb-xc-host ; Bootstrap NAMED-DS-BIND
           (named-ds-bind (name lambda-list expression &body body)
             (declare (ignore name))
               `(cl:destructuring-bind ,lambda-list ,expression ,@body)))
  (defmacro named-ds-bind (name lambda-list expression &body body
                                                       &environment env)
    (declare (ignore env)) ; could be policy-sensitive (but isn't)
    `(binding* ,(sb-c::expand-ds-bind lambda-list expression t nil name
                 (and (eq (car name) :macro)
                      (eq (cddr name) 'deftype)
                      ''*))
       ,@body)))
