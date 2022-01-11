;;;; the backquote reader macro

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;; An unquoting COMMA struct.
(defstruct (comma (:constructor unquote (expr &optional (kind 0)))
                  #+sb-xc-host (:include structure!object)
                  ;; READing unpretty commas requires a default constructor.
                  ;; Not needed on the host.
                  #-sb-xc-host (:constructor %default-comma-constructor)
                  (:copier nil))
 (expr nil :read-only t)
 (kind nil :read-only t :type (member 0 1 2)))
(declaim (freeze-type comma))

(defconstant !+comma-dot+ 1)
(defconstant !+comma-at+  2)
(defun unquote-nsplice (x) (unquote x !+comma-dot+))
(defun unquote-splice (x) (unquote x !+comma-at+))
(defun unquote* (list) (mapcar #'unquote list))
(defun unquote*-splice (list) (mapcar #'unquote-splice list))
(declaim (inline comma-constructor comma-splicing-p))
(defun comma-constructor (x)
  (svref #(unquote unquote-nsplice unquote-splice) (comma-kind x)))
(defun comma-splicing-p (comma) (not (zerop (comma-kind comma))))
(!set-load-form-method comma (:host :xc :target))

(declaim (type (and fixnum unsigned-byte) *backquote-depth*))
(defvar *backquote-depth* 0 "how deep we are into backquotes")
(defvar *bq-error* "Comma not inside a backquote.")

;;; the actual character macro
(defun backquote-charmacro (stream char)
  (declare (ignore char))
  (let* ((expr (let ((*backquote-depth* (1+ *backquote-depth*)))
                 (read stream t nil t)))
         (result (list 'quasiquote expr)))
    (if (and (comma-p expr) (comma-splicing-p expr))
        ;; use RESULT rather than EXPR in the error so it pprints nicely
        (simple-reader-error
         stream "~S is not a well-formed backquote expression" result)
        result)))

(defun comma-charmacro (stream char)
  (declare (type stream stream)
           (ignore char)
           (notinline read-char unread-char))
  (unless (> *backquote-depth* 0)
    (when *read-suppress*
      (return-from comma-charmacro nil))
    (simple-reader-error stream *bq-error*))
  (let ((flag (let ((c (read-char stream)))
                (case c
                  (#\@ !+comma-at+)
                  (#\. !+comma-dot+)
                  (t (unread-char c stream) 0))))
        (x (peek-char t stream t nil t)))
    (when (and (char= x #\)) (eq (get-macro-character x) 'read-right-paren))
                 ;; Easier to figure out than an "unmatched parenthesis".
      (simple-reader-error stream "Trailing ~A in backquoted expression."
                           (svref #("comma" "comma-dot" "comma-at") flag)))
    (unquote (let ((*backquote-depth* (1- *backquote-depth*)))
               (read stream t nil t)) flag)))

;; A QQ-SUBFORM is a cons whose car is an arbitrary S-expression, and
;; cdr one of {EVAL,QUOTE,NCONC,|Append|} signifying how to treat the car.
;; QUOTE and EVAL mean that a single element should be inserted,
;; literally or after being evaluated; NCONC/Append evaluate and splice.
(declaim (inline qq-subform-splicing-p))
(defun qq-subform-splicing-p (subform)
  (case (cdr subform)
    (|Append| '|Append|)
    (nconc 'nconc)))

(defun expand-quasiquote (thing compiler-p)
  ;; QQ-TEMPLATE-TO-SEXPR returns the parts of a QQ-SUBFORM as 2 values.
  (multiple-value-bind (expr operator)
      (qq-template-to-sexpr thing 0 compiler-p)
    (ecase operator ; Splicing is illegal at toplevel
      (eval expr)
      (quote (list 'quote expr)))))

;; The compiler macro for QUASIQUOTE assumes that it's fine to use
;; the foldable list constructors.
(define-compiler-macro quasiquote (thing)
  (expand-quasiquote thing t))

;; The ordinary macro uses CL-standard list constructors for a few reasons:
;; - It makes COMPILE do slightly less work than COMPILE-FILE
;; - If expanded forms are leaked to the user, it looks nicer.
;;   Pending discussion of how to or whether to prettify the value
;;   of (MACROEXPAND-1 '`(FOO ,X)) this could be irrelevant.
(defmacro quasiquote (thing)
  (expand-quasiquote thing nil))

;; Convert a quasi-quote template to a Lisp form that when evaluated constructs
;; the template, substituting into the outermost commas. Return two values:
;; the S-expression, and an indicator of how to incorporate it into its parent.
(defun qq-template-to-sexpr (expr depth compiler-p)
  (declare (type fixnum depth))
  (cond ((not expr) (values nil 'quote))
        ((listp expr)
         (qq-template-1 expr (+ (if (eq (car expr) 'quasiquote) 1 0) depth)
                        compiler-p))
        ((simple-vector-p expr) (qq-template-1 expr depth compiler-p))
        ((not (comma-p expr)) (values expr 'quote))
        ((zerop depth)
         (values (comma-expr expr)
                 (svref #(eval nconc |Append|) (comma-kind expr))))
        (t
         ;; A comma is "pure data" if deeper than the current backquote depth.
         ;; If its expression interpolates 1 item, reconstruct it using its
         ;; ordinary constructor, otherwise its multi-constructor.
         (multiple-value-bind (subexpr operator)
             (qq-template-to-sexpr (comma-expr expr) (1- depth) compiler-p)
           (when (eq operator 'quote)
             (setq subexpr (list 'quote subexpr) operator 'eval))
           (values (list (cond ((eq operator 'eval) (comma-constructor expr))
                               ((comma-splicing-p expr) 'unquote*-splice)
                               (t 'unquote*))
                         subexpr)
                   operator)))))

;; Find the longest suffix comprised wholly of self-evaluating and/or quoted
;; SUBFORMS. DOTTED-P indicates that the last item represents what was in the
;; CDR of the last cons of the original list. Return the modified SUBFORMS
;; as a proper list, and new DOTTED-P flag. i.e. Conceptually:
;;    `(a ,[@]b c d)   -> `(a ,[@]b . (c d))
;;    `(a ,[@]b c . d) -> `(a ,[@]b . (c . d))
(defun qq-fold-suffix (subforms dotted-p vectorp)
  (labels ((const-tailp (list)
             (if list
                 (let* ((rest (cdr list))
                        (const-part (const-tailp rest)))
                   (if (and (eq const-part rest) (eq (cdar list) 'quote))
                       list
                       const-part)))))
    (let ((const-tail (and (cdr subforms) (const-tailp subforms))))
      (if const-tail
          (let* ((constants (mapcar #'car const-tail))
                 (new-tail (if dotted-p (apply 'list* constants) constants)))
            (setq subforms (nconc (ldiff subforms const-tail)
                                  (list (cons new-tail 'quote)))
                  dotted-p t)))))
  ;; If the only splicing operator is in the last element of a proper list,
  ;; get rid of the splice and make it an improper list.
  (labels ((convertible-p (list)
             (if (cdr list)
                 (and (not (qq-subform-splicing-p (car list)))
                      (convertible-p (cdr list)))
                 (qq-subform-splicing-p (car list)))))
    (when (and (not dotted-p) (not vectorp) (convertible-p subforms))
      (let ((tail (car (last subforms))))
        (setq subforms (nconc (nbutlast subforms) (list (list (car tail))))
              dotted-p t))))
  (values subforms dotted-p))

;; Map TEMPLATE-TO-SEXPR over INPUT, a list or simple-vector, producing a list
;; as if by MAP. The cdr of the last cons of the input (if a list) may be a
;; non-nil atom. Return a secondary value indicating whether it was or not.
;; The output list never "dots" its last cons, regardless of the input.
(defun qq-map-template-to-list (input depth compiler-p)
  (declare (type fixnum depth))
  (let ((original input) list dotted-p)
    (flet ((to-sexpr (x)
             (multiple-value-call #'cons
               (qq-template-to-sexpr x depth compiler-p))))
      (typecase input
        (cons
         (loop
            (push (to-sexpr (pop input)) list)
            ;; Ensure that QQ-TEMPLATE-TO-SEXPR sees each occurrence of
            ;; (QUASIQUOTE <form>) as a proper list so that it can
            ;; bump the depth counter. The oddball case `(a . `(b))
            ;; would otherwise be seen as not nested `(a quasiquote (b)).
            (cond ((null input) (return))
                  ((comma-p input) ; (... . ,<expr>)
                   (when (comma-splicing-p input) ; uncaught by reader
                     ;; Actually I don't even know how to get this error
                     (error "~S is not a well-formed backquote expression"
                            original))
                   ;; (A B . ,X) becomes (A B ,@X). It matters only if there
                   ;; are commas in X like (... . ,,@C). Otherwise no effect.
                   (push (to-sexpr (unquote-splice (comma-expr input))) list)
                   (return))
                  ((or (not (listp input)) (eq (car input) 'quasiquote))
                   (push (to-sexpr input) list)
                   (setq dotted-p t)
                   (return))))
         (setq list (nreverse list)))
        (simple-vector
         (setq list (map 'list #'to-sexpr input)))))
    ;; For lists, find the longest suffix comprised wholly of literals.
    ;; For vectors without splicing we don't do that because (VECTOR 'A B 'C 'D)
    ;; is better than (COERCE (LIST* 'A B '(C D)) 'VECTOR) by avoiding a step.
    ;; But if splicing is required then we're going to construct the interim
    ;; list no matter what. It could theoretically be avoided by doing:
    ;;  (MULTIPLE-VALUE-CALL #'VECTOR ... (VALUES-LIST <splice>) ...)
    (if (or (listp original)
            ;; The target compiler open-codes SOME but the cross-compiler
            ;; seems not to without (THE LIST) to help it out.
            (some #'qq-subform-splicing-p (the list list)))
        (qq-fold-suffix list dotted-p (vectorp input))
        (values list dotted-p))))

;; Return an expression to quasi-quote INPUT, which is either a list
;; or simple-vector, by recursing over its subexpressions.
;; The expansion is in terms of CL-standard functions for MACROEXPAND,
;; but SBCL-private functions for the compiler-macro.
;; This is mainly for aesthetics. If users expressly macroexpand a sexpr
;; and then compile it, they miss out on the opportunity for the minor
;; advantage provided by the foldable functions, but why would they do that?
(defun qq-template-1 (input depth compiler-p)
  (declare (type fixnum depth))
  (multiple-value-bind (subforms dot-p)
      (qq-map-template-to-list input depth compiler-p)
    (labels ((const-p (subform) ; is SUBFORM constant?
               ;; This needs to notice only the QQ-SUBFORM kind of QUOTE,
               ;; but it helps to get EVAL forms whose expression is (QUOTE x).
               ;; Otherwise, work is deferred to IR1 in processing `(A ,'B C).
               (or (eq (cdr subform) 'quote) ; either the kind is QUOTE
                   (let ((exp (car subform)))
                     (if (atom exp) ; or it's a self-evaluating atom
                         (atom-const-p exp)
                         ;; or (QUOTE <thing>)
                         (and (eq (car exp) 'quote) (singleton-p (cdr exp)))))))
             (atom-const-p (atom) ; is known to be an atom
               (typep atom '(or (not symbol) (member t nil) keyword)))
             (const-val (subform) ; given that it is known CONST-P
               (let ((exp (car subform)))
                 (if (or (eq (cdr subform) 'quote) (atom exp))
                     exp
                     (second exp)))) ; (QUOTE x) in a for-evaluation position
             (render (subform) ; Return a sexpr that evaluates to SUBFORM
               ;; For subform kind = QUOTE, wrap it in a QUOTE unless
               ;; the quoted object is self-evaluating, then elide the QUOTE.
               (let ((exp (car subform)))
                 (if (and (eq (cdr subform) 'quote)
                          (not (and (atom exp) (atom-const-p exp))))
                     (list 'quote exp)
                     exp)))
             (normalize-fn (fn-name)
               (if (or compiler-p (eq fn-name 'nconc))
                   fn-name
                   (ecase fn-name
                     (|Append| 'append)
                     (|List|   'list)
                     (|List*|  'list*)
                     (|Vector| 'vector))))
             (recurse (list &aux (elt (car list)) (rest (cdr list)))
               (if (endp rest)
                   (cond ((or dot-p (qq-subform-splicing-p elt))
                          (let ((tail (render elt)))
                            (if (vectorp input)
                                ;; When splicing pieces into a vector,
                                ;; force the tail to be a list.
                                (list (normalize-fn '|Append|) tail nil)
                                tail)))
                         ((const-p elt) (list 'quote (list (const-val elt))))
                         (t (list (normalize-fn '|List|)
                                  (render elt)))) ; singleton list
                   (let ((fn (normalize-fn
                              (or (qq-subform-splicing-p elt) '|List*|)))
                         (head (render elt))
                         (tail (recurse rest)))
                     (if (and (listp tail) (eq (car tail) fn))
                         (list* fn head (cdr tail)) ; (F a (F b c)) -> (F a b c)
                         (list fn head tail))))))
      (let ((vect-p (vectorp input)))
        ;; If at least one splicing comma, use the recursive algorithm.
        (if (some #'qq-subform-splicing-p (the list subforms))
            (let ((x (recurse subforms)))
              (values (if vect-p (list 'coerce x ''simple-vector) x) 'eval))
            (let ((fn (cond (vect-p '|Vector|) (dot-p '|List*|) (t '|List|))))
              (if (every #'const-p subforms)
                  (values (apply fn (mapcar #'const-val subforms)) 'quote)
                  (values (cons (normalize-fn fn)
                                (mapcar #'render subforms)) 'eval))))))))

;;; COMPILE-FILE may treat anything as constant that is part of quoted
;;; structure, including quasi-quoted structure (lp#1026439).
;;; As such, we use foldable versions of the standard sequence constructors
;;; which are otherwise identical to their ordinary counterparts.
;;; Pretty-printing doesn't care about these names, only recognizing QUASIQUOTE.
;;; Generated code looks nicer to me without prepending BACKQ-.
;;; Also note there is no alter-ego of CONS or NCONC.
(setf (symbol-function '|Append|) #'append
      (symbol-function '|List|)   #'list
      (symbol-function '|List*|)  #'list*
      (symbol-function '|Vector|) #'vector)

;;;; initialization

;;; Install BACKQ stuff in the current *READTABLE*.
;;;
;;; In the target Lisp, we have to wait to do this until the readtable
;;; has been created. In the cross-compilation host Lisp, we can do
;;; this right away. (You may ask: In the cross-compilation host,
;;; which already has its own implementation of the backquote
;;; readmacro, why do we do this at all? Because the cross-compilation
;;; host might -- as SBCL itself does -- express the backquote
;;; expansion in terms of internal, nonportable functions. By
;;; redefining backquote in terms of functions which are guaranteed to
;;; exist on the target Lisp, we ensure that backquote expansions in
;;; code-generating code work properly.)

(defun !backq-cold-init (&optional (rt *readtable*))
  (set-macro-character #\` 'backquote-charmacro nil rt)
  (set-macro-character #\, 'comma-charmacro nil rt))
;;; This is a load-time effect, not compile-time, and *READTABLE* will have been
;;; reverted to the standard one, so be sure to assign into ours, not that.
#+sb-xc-host (!backq-cold-init sb-cold:*xc-readtable*)

;;; Since our backquote is installed on the host lisp, and since
;;; developers make mistakes with backquotes and commas too, let's
;;; ensure that we can report errors rather than get an undefined
;;; function condition on SIMPLE-READER-ERROR.
#+sb-xc-host ; proper definition happens for the target
(defun simple-reader-error (stream format-string &rest format-args)
  (error "READER-ERROR on stream ~S: ~?" stream format-string format-args))
