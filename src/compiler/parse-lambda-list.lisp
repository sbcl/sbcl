;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

(/show0 "parse-lambda-list.lisp 12")

(defconstant-eqx lambda-list-parser-states
    #(:required &optional &rest &more &key &aux &environment &whole
      &allow-other-keys &body :post-env :post-rest :post-more)
  #'equalp)
(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
  ;; Note: you usually want #. around LAMBDA-LIST-KEYWORD-MASK because for
  ;; a variety of reasons it shouldn't be a macro; and I don't want to rely
  ;; on a compiler-macro for other build hosts since processing is optional;
  ;; and declaring it inline is the wrong way to go since semantic analysis
  ;; is too weak to prove it constant; and FOLDABLE would only work on sbcl;
  ;; and iterating over a const list for every PARSE-LAMBDA-LIST annoys me.
  (defun lambda-list-keyword-mask (list)
    (if (eq list 'destructuring-bind)
        (lambda-list-keyword-mask
         '(&optional &rest &body &key &allow-other-keys &aux &whole))
        (loop for symbol in list
              sum (ash 1 (position symbol lambda-list-parser-states))))))

(defun ll-kwds-restp (bits)
  (when (logtest (lambda-list-keyword-mask '(&rest &body &more)) bits)
    ;; Test &BODY first because if present, &REST bit is also set.
    (cond ((logtest #.(lambda-list-keyword-mask '(&body)) bits) '&body)
          ((logtest #.(lambda-list-keyword-mask '(&more)) bits) '&more)
          (t '&rest))))

;;; Some accessors to distinguish a parse of (values &optional) from (values)
;;; and (lambda (x &key)) from (lambda (x)).
(declaim (inline ll-kwds-keyp ll-kwds-allowp))
(defun ll-kwds-keyp (bits)
  (logtest #.(lambda-list-keyword-mask '(&key)) bits))
(defun ll-kwds-allowp (bits)
  (logtest #.(lambda-list-keyword-mask '(&allow-other-keys)) bits))

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
                 (list &key (:context t) (:accept integer) (:silent boolean)
                            (:condition-class symbol))
                 (values (unsigned-byte 13) list list list list list list list))
                parse-lambda-list))

;;; Note: CLHS 3.4.4 [macro lambda list] allows &ENVIRONMENT anywhere,
;;; but 3.4.7 [defsetf lambda list] has it only at the end.
;;; This is possibly surprising to people since there seems to be some
;;; expectation that a DEFSETF lambda list is a macro lambda list,
;;; which it isn't. We'll relax and accept &ENVIRONMENT in the middle.
;;; But we won't accept the ugly syntax that parse-defmacro accidentally
;;; allows of (A B &ENVIRONMENT E X Y) which has 4 positional parameters.
;;; Nor can it appear between &KEY and &ALLOW-OTHER-KEYS.
;;;
(defun parse-lambda-list
    (list &key (context "an ordinary lambda list")
               (accept #.(lambda-list-keyword-mask
                          '(&optional &rest &more &key &allow-other-keys &aux)))
               (condition-class 'simple-program-error)
               silent
          &aux (seen 0) required optional rest more keys aux env whole tail
               (rest-bits 0))
  (declare (optimize speed))
  (declare (type (unsigned-byte 13) accept seen))
  (macrolet ((state (name) (position name lambda-list-parser-states))
             (state= (x y) `(= ,x (state ,y)))
             (bits (&rest list) (lambda-list-keyword-mask list))
             (begin-list (val)
               `(case state
                  ,@(loop for i from 0
                          for s in '(required optional rest more
                                     keys aux env whole)
                          collect `(,i (setq ,s ,val))))))
    (labels ((destructuring-p ()
               (logbitp (state &whole) accept))
             (need-arg (state)
               (croak "expecting variable after ~A in: ~S" state list))
             (need-symbol (x why)
               (unless (symbolp x)
                 (croak "~A is not a symbol: ~S" why x)))
             (need-bindable (x why)
               ;; "Bindable" means symbol or cons, but only if destructuring.
               (unless (or (symbolp x) (and (consp x) (destructuring-p)))
                 (if (destructuring-p)
                     (croak "~A is not a symbol or list: ~S" why x)
                     (croak "~A is not a symbol: ~S" why x))))
             (croak (string &optional (a1 0 a1p) (a2 0 a2p) (a3 0 a3p))
               (let ((l (if a1p (list a1 a2 a3))))
                 (if (and l (not a3p)) (rplacd (if a2p (cdr l) l) nil))
                 ;; KLUDGE: When this function was limited to parsing
                 ;; ordinary lambda lists, this error call was COMPILER-ERROR.
                 ;; To make all tests pass, it has to decide what to be.
                 ;; It's possible the tests are poorly designed and are
                 ;; acting as "change detectors"
                 (funcall (if (destructuring-p) 'error 'compiler-error)
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
                 ((and (symbolp input)
                       (not (logtest (bits &rest &key &aux) seen))
                       (memq context '(destructuring-bind :macro)))
                  (setf rest (list input)))
                 (t
                  (croak "illegal dotted lambda list: ~S" list)))
           (return))
         (shiftf last-arg arg (pop input))

         (when (and (symbolp arg)
                    (let ((name (symbol-name arg)))
                      (and (plusp (length name)) (char= (char name 0) #\&))))
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
                              (if (logbitp (state &body) accept)
                                  (state &rest) (state &body))))
               (&whole
                (values (if (and (state= state :required) (not required)
                                 (not (logbitp (state &environment) seen)))
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
                          (t context))))
                   (croak "~A is not allowed in ~A: ~S" arg where list)))

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
                          (croak "~S and ~S are mutually exclusive: ~S"
                                 '&body '&rest list)
                          (croak "repeated ~S in lambda list: ~S" arg list)))
                     ((logbitp state from-states) ; valid transition
                      (setq state to-state
                            seen (logior seen (ash 1 state))
                            tail nil)) ; Reset the accumulator.
                     ((logbitp state (bits &whole &rest &more &environment))
                      (need-arg last-arg)) ; Variable expected.
                     (t
                      (croak (if (state= to-state &whole)
                                 "~A must appear first in a lambda list: ~S"
                                 "misplaced ~A in lambda list: ~S")
                             arg list)))
               (go LOOP)))
           ;; Fell through, so warn if desired, and fall through some more.
           (unless silent
             (style-warn
              "suspicious variable ~S in lambda list: ~S." arg list)))

         ;; Handle a lambda variable
         (when (logbitp state (bits &allow-other-keys ; Not a collecting state.
                                    :post-env :post-rest :post-more))
           (croak "expected lambda list keyword at ~S in: ~S" arg list))
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
      (when (and (logbitp (state &key) seen) optional (not silent))
        (style-warn
         "&OPTIONAL and &KEY found in the same lambda list: ~S" list))

      ;; For CONTEXT other than :VALUES-TYPE/:FUNCTION-TYPE we reject
      ;; illegal list elements. Type specifiers have arbitrary shapes,
      ;; such as (VALUES (ARRAY (MUMBLE) (1 2)) &OPTIONAL (MEMBER X Y Z)).
      ;; But why don't we reject constant symbols here?
      (unless (member context '(:values-type :function-type))
        (dolist (arg required)
          (need-bindable arg "Required argument"))
        ;; FIXME: why not check symbol-ness of supplied-p variables now?
        (flet ((defaultp (x what-kind)
                 (cond ((symbolp x) nil)
                       ((listp x) t)
                       (t (croak "~A parameter is not a symbol or cons: ~S"
                                 what-kind x)))))
          (dolist (arg optional)
            (when (defaultp arg '&optional)
              (destructuring-bind (var &optional init-form supplied-p) arg
                (declare (ignore init-form supplied-p))
                (need-bindable var "&OPTIONAL parameter name"))))
          (when rest
            (need-bindable (car rest) "&REST argument"))
          (dolist (arg keys)
            (when (defaultp arg '&key)
              (destructuring-bind (var-or-kv &optional init-form supplied-p) arg
                (declare (ignore init-form supplied-p))
                (if (atom var-or-kv)
                    (need-symbol var-or-kv "&KEY parameter name")
                    (destructuring-bind (keyword-name var) var-or-kv
                      (declare (ignore keyword-name))
                      (need-bindable var "&KEY parameter name"))))))
          (dolist (arg aux)
            (when (defaultp arg '&aux)
              (destructuring-bind (var &optional init-form) arg
                (declare (ignore init-form))
                ;; &AUX is not destructured
                (need-symbol var "&AUX parameter name"))))))

    ;; Voila.
      (values (logior seen (if (oddp rest-bits) (bits &body) 0))
              required optional (or rest more) keys aux env whole))))

;; Split a keyword argument specifier into the keyword, the bound variable
;; or destructuring pattern, the default, and supplied-p var. If present the
;; supplied-p var is in a singleton list.
;; DEFAULT should be specified as '* when parsing a DEFTYPE lambda-list.
(defun parse-key-arg-spec (spec &optional default)
  (etypecase spec
    (symbol (values (keywordicate spec) spec default nil))
    (cons (destructuring-bind (var &optional (def default) . sup-p-var) spec
              (if (symbolp var)
                  (values (keywordicate var) var def sup-p-var)
                  (values (car var) (cadr var) def sup-p-var))))))

;; Invert the parsing operation.
(defun build-lambda-list (llks required &optional optional rest keys aux)
  (append required
          (if optional (cons '&optional optional))
          (let ((restp (ll-kwds-restp llks)))
            (if (and rest (not restp)) ; lambda list was "dotted"
                (car rest)
                (append
                 (when rest (cons restp rest))
                 (if (ll-kwds-keyp llks) (cons '&key keys)) ; KEYS can be nil
                 (if (ll-kwds-allowp llks) '(&allow-other-keys))
                 ;; Should &AUX be inserted even if empty? Probably not.
                 (if aux (cons '&aux aux)))))))

(/show0 "parse-lambda-list.lisp end of file")
