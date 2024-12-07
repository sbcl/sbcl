;;;; the LOOP iteration macro

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This code was modified by William Harold Newman beginning
;;;; 19981106, originally to conform to the new SBCL bootstrap package
;;;; system and then subsequently to address other cross-compiling
;;;; bootstrap issues, SBCLification (e.g. DECLARE used to check
;;;; argument types), and other maintenance. Whether or not it then
;;;; supported all the environments implied by the reader conditionals
;;;; in the source code (e.g. #+CLOE-RUNTIME) before that
;;;; modification, it sure doesn't now. It might perhaps, by blind
;;;; luck, be appropriate for some other CMU-CL-derived system, but
;;;; really it only attempts to be appropriate for SBCL.

;;;; This software is derived from software originally released by the
;;;; Massachusetts Institute of Technology and Symbolics, Inc. Copyright and
;;;; release statements follow. Later modifications to the software are in
;;;; the public domain and are provided with absolutely no warranty. See the
;;;; COPYING and CREDITS files for more information.

;;;; Portions of LOOP are Copyright (c) 1986 by the Massachusetts Institute
;;;; of Technology. All Rights Reserved.
;;;;
;;;; Permission to use, copy, modify and distribute this software and its
;;;; documentation for any purpose and without fee is hereby granted,
;;;; provided that the M.I.T. copyright notice appear in all copies and that
;;;; both that copyright notice and this permission notice appear in
;;;; supporting documentation. The names "M.I.T." and "Massachusetts
;;;; Institute of Technology" may not be used in advertising or publicity
;;;; pertaining to distribution of the software without specific, written
;;;; prior permission. Notice must be given in supporting documentation that
;;;; copying distribution is by permission of M.I.T. M.I.T. makes no
;;;; representations about the suitability of this software for any purpose.
;;;; It is provided "as is" without express or implied warranty.
;;;;
;;;;      Massachusetts Institute of Technology
;;;;      77 Massachusetts Avenue
;;;;      Cambridge, Massachusetts  02139
;;;;      United States of America
;;;;      +1-617-253-1000

;;;; Portions of LOOP are Copyright (c) 1989, 1990, 1991, 1992 by Symbolics,
;;;; Inc. All Rights Reserved.
;;;;
;;;; Permission to use, copy, modify and distribute this software and its
;;;; documentation for any purpose and without fee is hereby granted,
;;;; provided that the Symbolics copyright notice appear in all copies and
;;;; that both that copyright notice and this permission notice appear in
;;;; supporting documentation. The name "Symbolics" may not be used in
;;;; advertising or publicity pertaining to distribution of the software
;;;; without specific, written prior permission. Notice must be given in
;;;; supporting documentation that copying distribution is by permission of
;;;; Symbolics. Symbolics makes no representations about the suitability of
;;;; this software for any purpose. It is provided "as is" without express
;;;; or implied warranty.
;;;;
;;;; Symbolics, CLOE Runtime, and Minima are trademarks, and CLOE, Genera,
;;;; and Zetalisp are registered trademarks of Symbolics, Inc.
;;;;
;;;;      Symbolics, Inc.
;;;;      8 New England Executive Park, East
;;;;      Burlington, Massachusetts  01803
;;;;      United States of America
;;;;      +1-617-221-1000

(in-package "SB-LOOP")

;;;; The design of this LOOP is intended to permit, using mostly the same
;;;; kernel of code, up to three different "loop" macros:
;;;;
;;;; (1) The unextended, unextensible ANSI standard LOOP;
;;;;
;;;; (2) A clean "superset" extension of the ANSI LOOP which provides
;;;; functionality similar to that of the old LOOP, but "in the style of"
;;;; the ANSI LOOP. For instance, user-definable iteration paths, with a
;;;; somewhat cleaned-up interface.
;;;;
;;;; (3) Extensions provided in another file which can make this LOOP
;;;; kernel behave largely compatibly with the Genera-vintage LOOP macro,
;;;; with only a small addition of code (instead of two whole, separate,
;;;; LOOP macros).
;;;;
;;;; Each of the above three LOOP variations can coexist in the same LISP
;;;; environment.
;;;;
;;;; KLUDGE: In SBCL, we only really use variant (1), and any generality
;;;; for the other variants is wasted. -- WHN 20000121

;;;; list collection macrology

(defstruct (loop-collector
            (:copier nil)
            (:predicate nil))
  (name nil :read-only t)
  (class nil :read-only t)
  (history nil)
  (tempvars nil)
  (specified-type nil :read-only t)
  dtype
  (data nil)) ;collector-specific data
(declaim (sb-ext:freeze-type loop-collector))

(sb-xc:defmacro with-loop-list-collection-head
    ((collector head-var tail-var &optional user-head-var) &body body)
  (let ((l (and user-head-var (list (list user-head-var nil)))))
    `(let* ((,head-var ,(if (loop for how in (loop-collector-history collector)
                                  always (eq how 'list))
                            `(unaligned-dx-cons nil)
                            `(list nil)))
            (,tail-var ,head-var) ,@l)
       (declare (dynamic-extent ,head-var)
                ,@(and user-head-var `((list ,user-head-var))))
       ,@body)))

(sb-xc:defmacro loop-collect-rplacd
    (&environment env (head-var tail-var &optional user-head-var) form)
  (setq form (%macroexpand form env))
  (flet ((cdr-wrap (form n)
           (declare (fixnum n))
           (do () ((<= n 4) (setq form `(,(case n
                                            (1 'cdr)
                                            (2 'cddr)
                                            (3 'cdddr)
                                            (4 'cddddr))
                                         ,form)))
             (setq form `(cddddr ,form) n (- n 4)))))
    (let ((tail-form form) (ncdrs nil))
      ;; Determine whether the form being constructed is a list of known
      ;; length.
      (when (consp form)
        (cond ((eq (car form) 'list)
               (setq ncdrs (1- (length (cdr form)))))
              ((member (car form) '(list* cons))
               (when (and (cddr form) (member (car (last form)) '(nil 'nil)))
                 (setq ncdrs (- (length (cdr form)) 2))))))
      (let ((answer
              (cond ((null ncdrs)
                     (if (typep tail-form '(cons (eql copy-list)))
                         `(setf ,tail-var (sb-impl::copy-list-to ,(second tail-form)  ,tail-var))
                         `(when (setf (cdr ,tail-var) ,tail-form)
                            (setq ,tail-var (last (cdr ,tail-var))))))
                    ((< ncdrs 0) (return-from loop-collect-rplacd nil))
                    ((= ncdrs 0)
                     ;; @@@@ Here we have a choice of two idioms:
                     ;;   (RPLACD TAIL (SETQ TAIL TAIL-FORM))
                     ;;   (SETQ TAIL (SETF (CDR TAIL) TAIL-FORM)).
                     ;; Genera and most others I have seen do better with the
                     ;; former.
                     `(rplacd ,tail-var (setq ,tail-var ,tail-form)))
                    (t `(setq ,tail-var ,(cdr-wrap `(setf (cdr ,tail-var)
                                                          ,tail-form)
                                                   ncdrs))))))
        ;; If not using locatives or something similar to update the
        ;; user's head variable, we've got to set it... It's harmless
        ;; to repeatedly set it unconditionally, and probably faster
        ;; than checking.
        (when user-head-var
          (setq answer
                `(progn ,answer
                        (setq ,user-head-var (sb-ext:truly-the list (cdr ,head-var))))))
        answer))))

(sb-xc:defmacro loop-collect-answer (head-var
                                     &optional user-head-var)
  `(sb-ext:truly-the list ,(or user-head-var
                               `(cdr ,head-var))))

;;;; maximization technology

#|
The basic idea of all this minimax randomness here is that we have to
have constructed all uses of maximize and minimize to a particular
"destination" before we can decide how to code them. The goal is to not
have to have any kinds of flags, by knowing both that (1) the type is
something which we can provide an initial minimum or maximum value for
and (2) know that a MAXIMIZE and MINIMIZE are not being combined.

SO, we have a datastructure which we annotate with all sorts of things,
incrementally updating it as we generate loop body code, and then use
a wrapper and internal macros to do the coding when the loop has been
constructed.
|#

(defstruct (loop-minimax
             (:constructor make-loop-minimax-internal)
             (:copier nil)
             (:predicate nil))
  (answer-variable nil :read-only t)
  (type nil :read-only t)
  (temp-variable nil :read-only t)
  (flag-variable nil)
  (operations nil)
  (infinity-data nil :read-only t))
(declaim (sb-ext:freeze-type loop-minimax))

(defconstant-eqx +loop-minimax-type-infinities-alist+
    '((fixnum            most-positive-fixnum                  most-negative-fixnum)
      (single-float      sb-ext:single-float-positive-infinity sb-ext:single-float-negative-infinity)
      (double-float      sb-ext:double-float-positive-infinity sb-ext:double-float-negative-infinity))
  #'equal)

(defun make-loop-minimax (answer-variable type)
  (let ((infinity-data (cdr (assoc type
                                   +loop-minimax-type-infinities-alist+
                                   :test #'subtypep))))
    (make-loop-minimax-internal
      :answer-variable answer-variable
      :type type
      :temp-variable (gensym "MINMAXTMP")
      :flag-variable (and (not infinity-data) (gensym "MINMAXFLAG"))
      :operations nil
      :infinity-data infinity-data)))

(defun loop-note-minimax-operation (operation minimax)
  (pushnew (the symbol operation) (loop-minimax-operations minimax))
  (when (and (cdr (loop-minimax-operations minimax))
             (not (loop-minimax-flag-variable minimax)))
    (setf (loop-minimax-flag-variable minimax) (gensym "MINMAXFLAG")))
  operation)

(sb-xc:defmacro with-minimax-value (lm &body body)
  (let ((init (loop-typed-init (loop-minimax-type lm)))
        (which (car (loop-minimax-operations lm)))
        (infinity-data (loop-minimax-infinity-data lm))
        (answer-var (loop-minimax-answer-variable lm))
        (temp-var (loop-minimax-temp-variable lm))
        (flag-var (loop-minimax-flag-variable lm))
        (type (loop-minimax-type lm)))
    (if flag-var
        `(let ((,answer-var ,init) (,temp-var ,init) (,flag-var nil))
           (declare (type ,type ,answer-var ,temp-var))
           ,@body)
        `(let ((,answer-var ,(if (eq which 'min)
                                 (first infinity-data)
                                 (second infinity-data)))
               (,temp-var ,init))
           (declare (type ,type ,answer-var ,temp-var))
           ,@body))))

(sb-xc:defmacro loop-accumulate-minimax-value (lm operation form)
  (let* ((answer-var (loop-minimax-answer-variable lm))
         (temp-var (loop-minimax-temp-variable lm))
         (flag-var (loop-minimax-flag-variable lm))
         (test `(,(ecase operation
                    (min '<)
                    (max '>))
                 ,temp-var ,answer-var)))
    `(progn
       (setq ,temp-var ,form)
       (when ,(if flag-var `(or (not ,flag-var) ,test) test)
         (setq ,@(and flag-var `(,flag-var t))
               ,answer-var ,temp-var)))))

;;;; LOOP keyword tables

#|
LOOP keyword tables are hash tables string keys and a test of EQUAL.

The actual descriptive/dispatch structure used by LOOP is called a "loop
universe" contains a few tables and parameterizations. The basic idea is
that we can provide a non-extensible ANSI-compatible loop environment,
an extensible ANSI-superset loop environment, and (for such environments
as CLOE) one which is "sufficiently close" to the old Genera-vintage
LOOP for use by old user programs without requiring all of the old LOOP
code to be loaded.
|#

;;;; token hackery

;;; Compare two "tokens". The first is the frob out of (SOURCE-CODE *LOOP*),
;;; the second a symbol to check against.
(defun loop-tequal (x1 x2)
  (and (symbolp x1) (string= x1 x2)))

(defun loop-tassoc (kwd alist)
  (and (symbolp kwd) (assoc kwd alist :test #'string=)))

(defun loop-tmember (kwd list)
  (and (symbolp kwd) (member kwd list :test #'string=)))

(defun loop-lookup-keyword (loop-token table)
  (and (symbolp loop-token)
       (values (gethash (symbol-name loop-token) table))))

(sb-xc:defmacro loop-store-table-data (symbol table datum)
  `(setf (gethash (symbol-name ,symbol) ,table) ,datum))

;;; "4.2.3 Type Specifiers" lists the standardized atomic type specifiers.
;;; While in theory we might want to include all of them,
;;; in practice it seems silly to allow "for x arithmetic-error in ..."
(defun std-atom-type-specifier-p (symbol)
  ;; The check for symbols in CL is an optimization that skips calling INFO on
  ;; most atoms. The real test is whether type = :PRIMITIVE, as there are CL
  ;; symbols naming types which are not standard builtin type specifiers, e.g.
  ;; ARRAY-RANK, CHAR-CODE. (One almost wonders if that is technically wrong).
  (and (eq (sb-xc:symbol-package symbol) *cl-package*)
       (or (eq (info :type :kind symbol) :primitive)
           ;; allow certain :instance types, but not all of them
           (member symbol '(hash-table package pathname random-state readtable)))
       (neq symbol '*)
       symbol))

(defstruct (loop-universe
             (:constructor !make-loop-universe)
             (:copier nil)
             (:predicate nil))
  (keywords nil :read-only t)             ; hash table, value = (fn-name . extra-data)
  (iteration-keywords nil :read-only t)   ; hash table, value = (fn-name . extra-data)
  (for-keywords nil :read-only t)         ; hash table, value = (fn-name . extra-data)
  (path-keywords nil :read-only t))       ; hash table, value = (fn-name . extra-data)
(declaim (sb-ext:freeze-type loop-universe))
(defmethod print-object ((u loop-universe) stream)
  (print-unreadable-object (u stream :type t :identity t)))

(defun !make-standard-loop-universe (&key keywords for-keywords
                                          iteration-keywords path-keywords)
  (flet ((maketable (entries)
           (let ((ht (make-hash-table :size (max 10 (length entries))
                                      :test 'equal)))
             (dolist (x entries)
               (setf (gethash (symbol-name (car x)) ht) (cadr x)))
             ht)))
    (!make-loop-universe
      :keywords (maketable keywords)
      :for-keywords (maketable for-keywords)
      :iteration-keywords (maketable iteration-keywords)
      :path-keywords (maketable path-keywords))))

;;;; SETQ hackery, including destructuring ("DESETQ")

(defun loop-make-psetq (frobs)
  (and frobs
       (loop-make-desetq
         (list (car frobs)
               (if (null (cddr frobs)) (cadr frobs)
                   `(prog1 ,(cadr frobs)
                           ,(loop-make-psetq (cddr frobs))))))))

(defun loop-make-desetq (var-val-pairs)
  (if var-val-pairs (cons 'loop-desetq var-val-pairs)))

(sb-ext:defglobal *loop-desetq-temporary*
        (make-symbol "LOOP-DESETQ-TEMP"))

(sb-xc:defmacro loop-desetq (&environment env &rest var-val-pairs)
  (labels ((find-non-null (var)
             ;; See whether there's any non-null thing here. Recurse
             ;; if the list element is itself a list.
             (do ((tail var)) ((not (consp tail)) tail)
               (when (find-non-null (pop tail)) (return t))))
           (loop-desetq-internal (var val &optional temp)
             ;; Check for well-formed use of MULTIPLE-VALUE-LIST
             (when (and (typep val '(cons (eql multiple-value-list) (cons t null)))
                        (proper-list-p var))
               (return-from loop-desetq-internal
                 (let ((temps (make-gensym-list (length var))))
                   `((multiple-value-bind ,temps ,(cadr val)
                       ,@(when (member nil var)
                           `((declare (ignore ,@(mapcan (lambda (var val)
                                                          (unless var (list val)))
                                                        var temps)))))
                       ,@(mapcan (lambda (var val)
                                   (if var (list `(loop-desetq ,var ,val))))
                                 var temps))))))
             ;; returns a list of actions to be performed
             (typecase var
               (null
                 (when (consp val)
                   ;; Don't lose possible side effects.
                   ;; FIXME: this special case is just wrong. Either (DESETQ NIL THING)
                   ;; evaluates THING, or it doesn't. If it does, then it mustn't touch
                   ;; THING at all, because the user could have written
                   ;;  (LOOP FOR NIL = (PROG1 AAA BBB)) where AAA and BBB are
                   ;; symbol-macros the expansions of which are, let's say, (INCF C)
                   ;; and (WHEN (PLUSP C) (RETURN)) respectively. The desired effect is
                   ;; to perform the incf, ignore it, and then return.  But we turn that
                   ;; whole form into NIL because of a PROG1. Wtf???
                   (if (eq (car val) 'prog1)
                       ;; These can come from PSETQ or DESETQ below.
                       ;; Throw away the value, keep the side effects.
                       ;; Special case is for handling an expanded POP.
                       (mapcan (lambda (x)
                                 (and (consp x)
                                      (or (neq (car x) 'car)
                                          (not (symbolp (cadr x)))
                                          (not (symbolp (setq x (%macroexpand x env)))))
                                      (cons x nil)))
                               (cdr val))
                       `(,val))))
               (cons
                 (let* ((car (car var))
                        (cdr (cdr var))
                        (car-non-null (find-non-null car))
                        (cdr-non-null (find-non-null cdr)))
                   (when (or car-non-null cdr-non-null)
                     (if cdr-non-null
                         (let* ((temp-p temp)
                                (temp (or temp *loop-desetq-temporary*))
                                (body `(,@(loop-desetq-internal car
                                                                `(car ,temp))
                                          (setq ,temp (cdr ,temp))
                                          ,@(loop-desetq-internal cdr
                                                                  temp
                                                                  temp))))
                           (if temp-p
                               `(,@(unless (eq temp val)
                                     `((setq ,temp ,val)))
                                 ,@body)
                               `((let ((,temp ,val))
                                   ,@body))))
                         ;; no CDRing to do
                         (loop-desetq-internal car `(car ,val) temp)))))
               (otherwise
                 (unless (eq var val)
                   `((setq ,var ,val)))))))
    (do ((actions))
        ((null var-val-pairs)
         (if (null (cdr actions)) (car actions) `(progn ,@(nreverse actions))))
      (setq actions (revappend
                      (loop-desetq-internal (pop var-val-pairs)
                                            (pop var-val-pairs))
                      actions)))))

;;;; LOOP-local variables

(defstruct (macro-state
            (:copier nil) (:predicate nil) (:conc-name nil)
            (:constructor make-loop (source-code macro-environment universe)))
;;; This is the "current" pointer into the LOOP source code.
  (source-code)

;;; This is the pointer to the original, for things like NAMED that
;;; insist on being in a particular position
  (original-source-code nil)

;;; This is (source-code *loop*) as of the "last" clause. It is used
;;; primarily for generating error messages (see loop-error, loop-warn).
  (source-context nil)

;;; list of names for the LOOP, supplied by the NAMED clause
  (names nil)

;;; The macroexpansion environment given to the macro.
  (macro-environment)

;;; This holds variable names specified with the USING clause.
;;; See LOOP-NAMED-VAR.
  (named-vars nil)

;;; LETlist-like list being accumulated for current group of bindings.
  (vars nil)

;;; List of declarations being accumulated in parallel with
;;; VARS.
  (declarations nil)

;;; Declarations for destructuring bindings
  (desetq-declarations nil)

;;; This is used by LOOP for destructuring binding, if it is doing
;;; that itself. See LOOP-MAKE-VAR.
  (desetq nil)

;;; list of wrapping forms, innermost first, which go immediately
;;; inside the current set of parallel bindings being accumulated in
;;; VARS. The wrappers are appended onto a body. E.g., this list could
;;; conceivably have as its value
;;;   ((WITH-OPEN-FILE (G0001 G0002 ...))),
;;; with G0002 being one of the bindings in VARS (This is why the
;;; wrappers go inside of the variable bindings).
  (wrappers nil)

;;; This accumulates lists of previous values of VARS and the other
;;; lists above, for each new nesting of bindings. See
;;; LOOP-BIND-BLOCK.
  (bind-stack nil)

;;; list of prologue forms of the loop, accumulated in reverse order
  (prologue nil)

  (before-loop nil)
  (after-body nil)

;;; This is T if we have emitted any body code, so that iteration
;;; driving clauses can be disallowed. This is not strictly the same
;;; as checking (BODY *LOOP*), because we permit some clauses such as
;;; RETURN to not be considered "real" body (so as to permit the user
;;; to "code" an abnormal return value "in loop").
  (emitted-body nil)

;;; list of epilogue forms (supplied by FINALLY generally), accumulated
;;; in reverse order
  (epilogue nil)

;;; list of epilogue forms which are supplied after the above "user"
;;; epilogue. "Normal" termination return values are provide by
;;; putting the return form in here. Normally this is done using
;;; LOOP-EMIT-FINAL-VALUE, q.v.
  (after-epilogue nil)

;;; the "culprit" responsible for supplying a final value from the
;;; loop. This is so LOOP-DISALLOW-AGGREGATE-BOOLEANS can moan about
;;; disallowed anonymous collections.
  (final-value-culprit nil)

;;; If not NIL, this is a temporary bound around the loop for holding
;;; the temporary value for "it" in things like "when (f) collect it".
;;; It may be used as a supertemporary by some other things.
  (when-it-var nil)

;;; Sometimes we decide we need to fold together parts of the loop,
;;; but some part of the generated iteration code is different for the
;;; first and remaining iterations. This variable will be the
;;; temporary which is the flag used in the loop to tell whether we
;;; are in the first or remaining iterations.
  (never-stepped-var nil)

;;; list of all the value-accumulation descriptor structures in the
;;; loop. See LOOP-GET-COLLECTION-INFO.
  (collection-cruft nil) ; for multiple COLLECTs (etc.)

;;; This is the "current" loop context in use when we are expanding a
;;; loop. It gets bound on each invocation of LOOP.
  (universe nil :type loop-universe))
(declaim (sb-ext:freeze-type macro-state))

(defvar *loop*)
(declaim (type macro-state *loop*))

(defvar *loop-body*)

;;; If this is true, we are in some branch of a conditional. Some
;;; clauses may be disallowed.
(defvar *loop-inside-conditional*)

;;;; code analysis stuff

(defun loop-constant-fold-if-possible (form &optional expected-type)
  (let* ((constantp (constantp form))
         (value (and constantp (constant-form-value form))))
    (when (and constantp expected-type)
      (unless (sb-xc:typep value expected-type)
        (loop-warn "~@<The form ~S evaluated to ~S, which was not of ~
                    the anticipated type ~S.~:@>"
                   form value expected-type)
        (setq constantp nil value nil)))
    (values form constantp value)))

(defun gen-loop-body (prologue before-loop main-body after-loop epilogue)
  (unless (= (length before-loop) (length after-loop))
    ;; FIXME: should be (bug) ?
    (error "GEN-LOOP-BODY called with non-synched before- and after-loop lists"))
  ;; All our work is done from these copies, working backwards from the end
  (let ((rbefore (reverse before-loop))
        (rafter (reverse after-loop)))
    ;; Go backwards from the ends of before-loop and after-loop
    ;; merging all the equivalent forms into the body.
    (do ()
        ((or (null rbefore)
             (not (equal (car rbefore) (car rafter)))))
      (push (pop rbefore) main-body)
      (pop rafter))
    `(tagbody
        ,@(remove nil prologue)
        ,@(nreverse (remove nil rbefore))
      next-loop
        ,@(remove nil main-body)
        ,@(nreverse (remove nil rafter))
        (go next-loop)
      end-loop
        ,@(remove nil epilogue))))

;;;; loop errors

(defun loop-context (&aux (loop *loop*))
  (do ((l (source-context loop) (cdr l)) (new nil (cons (car l) new)))
      ((eq l (cdr (source-code loop))) (nreverse new))))

(define-error-wrapper loop-error (format-string &rest format-args)
  (%program-error "~?~%current LOOP context:~{ ~S~}."
                  format-string format-args (loop-context)))

(defun loop-warn (format-string &rest format-args)
  (warn "~?~%current LOOP context:~{ ~S~}."
        format-string
        format-args
        (loop-context)))

(defun loop-check-data-type (specified-type required-type
                             &optional (default-type required-type))
  (if (null specified-type)
      default-type
      (multiple-value-bind (a b) (subtypep specified-type required-type)
        (cond ((not b)
               (loop-warn "LOOP couldn't verify that ~S is a subtype of the required type ~S."
                          specified-type required-type))
              ((not a)
               (loop-error "The specified data type ~S is not a subtype of ~S."
                           specified-type required-type)))
        specified-type)))

;;; Transform the LOOP kind of destructuring into the DESTRUCTURING-BIND kind
;;; basically by adding &optional and ignored &rest dotted list
(defun transform-destructuring (tree)
  (let (ignores)
    (labels ((transform (tree)
               (do ((result (list '&optional))
                    (cdr tree (cdr cdr)))
                   (())
                 (cond ((null cdr)
                        (return (nreconc result
                                         (car (push (gensym "_") ignores)))))
                       ((atom cdr)
                        (return (nreconc result cdr)))
                       ((consp (car cdr))
                        (push (list (transform (car cdr))) result))
                       ((null (car cdr))
                        (push (car (push (gensym "_") ignores))
                              result))
                       (t
                        (push (car cdr) result))))))
      (values (transform tree) ignores))))

(sb-xc:defmacro loop-destructuring-bind
    (lambda-list args &rest body)
  (multiple-value-bind (d-lambda-list ignores)
      (transform-destructuring lambda-list)
    `(destructuring-bind ,d-lambda-list ,args
       (declare (ignore ,@ignores))
       ,@body)))

(defun loop-build-destructuring-bindings (crocks forms)
  (if crocks
      `((loop-destructuring-bind ,(car crocks) ,(cadr crocks)
        ,@(loop-build-destructuring-bindings (cddr crocks) forms)))
      forms))

(defun loop-translate (loop &aux (*loop* loop))
  (let ((*loop-body* nil)
        (*loop-inside-conditional* nil))
    (loop-iteration-driver loop)
    (loop-bind-block)
    (let ((answer (gen-loop-body
                   (nreverse (prologue loop))
                   (nreverse (before-loop loop))
                   (nreverse *loop-body*)
                   (nreverse (after-body loop))
                   (nreconc (epilogue loop)
                            (nreverse (after-epilogue loop))))))
      (dolist (entry (bind-stack loop))
        (destructuring-bind (vars dcls desetq desetq-decls wrappers) entry
          (dolist (w wrappers)
            (setq answer (append w (list answer))))
          (when (or vars dcls desetq)
            (let ((forms (list answer)))
              (when desetq-decls
                (push `(declare ,@desetq-decls) forms))
              (setq answer `(,(if vars 'let 'locally)
                             ,vars
                             (declare ,@dcls)
                             ,@(loop-build-destructuring-bindings desetq
                                                                  forms)))))))
      (do () (nil)
        (setq answer `(block ,(pop (names loop)) ,answer))
        (unless (names loop) (return nil)))
      answer)))

(defun loop-iteration-driver (loop &aux (universe (universe loop)))
  (do ()
      ((null (source-code loop)))
    (let ((keyword (car (source-code loop))) (tem nil))
      (cond ((not (symbolp keyword))
             (loop-error "~S found where LOOP keyword expected" keyword))
            (t (setf (source-context loop) (source-code loop))
               (loop-pop-source)
               (cond ((setq tem
                            (loop-lookup-keyword keyword
                                                 (loop-universe-keywords universe)))
                      ;; It's a "miscellaneous" toplevel LOOP keyword (DO,
                      ;; COLLECT, NAMED, etc.)
                      (apply (symbol-function (first tem)) (rest tem)))
                     ((setq tem
                            (loop-lookup-keyword keyword
                                                 (loop-universe-iteration-keywords universe)))
                      (loop-hack-iteration tem))
                     ((loop-tmember keyword '(and else))
                      ;; The alternative is to ignore it, i.e. let it go
                      ;; around to the next keyword...
                      (loop-error "secondary clause misplaced at top level in LOOP macro: ~S ~S ~S ..."
                                  keyword
                                  (car (source-code loop))
                                  (cadr (source-code loop))))
                     (t (loop-error "unknown LOOP keyword: ~S" keyword))))))))

(defun loop-pop-source (&aux (loop *loop*))
  (if (source-code loop)
      (pop (source-code loop))
      (loop-error "LOOP source code ran out when another token was expected.")))

(defun loop-get-form ()
  (if (source-code *loop*)
      (loop-pop-source)
      (loop-error "LOOP code ran out where a form was expected.")))

(defun loop-get-compound-form ()
  (let ((form (loop-get-form)))
    (unless (consp form)
      (loop-error "A compound form was expected, but ~S found." form))
    form))

(defun loop-get-progn (&aux (loop *loop*))
  (do ((forms (list (loop-get-compound-form))
              (cons (loop-get-compound-form) forms))
       (nextform (car (source-code loop))
                 (car (source-code loop))))
      ((atom nextform)
       (if (null (cdr forms)) (car forms) (cons 'progn (nreverse forms))))))

(defun loop-construct-return (form)
  `(return-from ,(car (names *loop*)) ,form))

(defun loop-pseudo-body (form &aux (loop *loop*))
  (cond ((or (emitted-body loop) *loop-inside-conditional*)
         (push form *loop-body*))
        (t (push form (before-loop loop)) (push form (after-body loop)))))

(defun loop-emit-body (form)
  (setf (emitted-body *loop*) t)
  (loop-pseudo-body form))

(defun loop-emit-final-value (&optional (form nil form-supplied-p) &aux (loop *loop*))
  (when form-supplied-p
    (push (loop-construct-return form) (after-epilogue loop)))
  (setf (final-value-culprit loop) (car (source-context loop))))

(defun loop-disallow-conditional (&optional kwd)
  (when *loop-inside-conditional*
    (loop-error "~:[This LOOP~;The LOOP ~:*~S~] clause is not permitted inside a conditional." kwd)))

(defun loop-disallow-anonymous-collectors ()
  (when (find-if-not 'loop-collector-name (collection-cruft *loop*))
    (loop-error "This LOOP clause is not permitted with anonymous collectors.")))

(defun loop-disallow-aggregate-booleans ()
  (when (loop-tmember (final-value-culprit *loop*) '(always never thereis))
    (loop-error "This anonymous collection LOOP clause is not permitted with aggregate booleans.")))

;;;; loop types

(defun loop-typed-init (data-type &optional step-var-p)
  ;; FIXME: can't tell if unsupplied or NIL, but it has to be rare.
  ;; FIXME: returns either 2 values or 1 value, which is poor style.
  (unless data-type
    (return-from loop-typed-init (values nil nil)))
  (let ((ctype (specifier-type data-type)))
    (when (eq ctype *empty-type*)
      (return-from loop-typed-init (values nil t)))
    (cond ((csubtypep ctype (specifier-type 'number))
            (let ((init (if step-var-p 1 0)))
              (flet ((like (&rest types)
                       (coerce init (find-if (lambda (spec)
                                               (csubtypep ctype (specifier-type spec)))
                                             types))))
                (cond ((csubtypep ctype (specifier-type 'float))
                       (like 'single-float 'double-float
                             'short-float 'long-float 'float))
                      ((csubtypep ctype (specifier-type '(complex float)))
                       (like '(complex single-float)
                             '(complex double-float)
                             '(complex short-float)
                             '(complex long-float)
                             '(complex float)))
                      (t
                       init)))))
          ((csubtypep ctype (specifier-type 'vector))
           (cond ((array-type-p ctype)
                  (let ((etype (type-*-to-t
                                (array-type-specialized-element-type ctype))))
                    (make-array 0 :element-type (type-specifier etype))))
                 ((csubtypep ctype (specifier-type 'string))
                  "")))
           #+sb-unicode
           ((csubtypep ctype (specifier-type 'extended-char))
            #+sb-xc-host
            (error "Unimplemented on cross-compiler.")
            #-sb-xc-host
            (code-char base-char-code-limit))
           ((csubtypep ctype (specifier-type 'character))
            #\x)
           (t
            nil))))

(defun loop-optional-type (&optional variable &aux (loop *loop*))
  ;; No variable specified implies that no destructuring is permissible.
  (and (source-code loop) ; Don't get confused by NILs..
       (let ((z (car (source-code loop))))
         (cond ((loop-tequal z 'of-type)
                ;; This is the syntactically unambigous form in that
                ;; the form of the type specifier does not matter.
                ;; Also, it is assumed that the type specifier is
                ;; unambiguously, and without need of translation, a
                ;; common lisp type specifier or pattern (matching the
                ;; variable) thereof.
                (loop-pop-source)
                (loop-pop-source))

               ((symbolp z)
                ;; This is the (sort of) "old" syntax, even though we
                ;; didn't used to support all of these type symbols.
                (let ((type-spec (std-atom-type-specifier-p z)))
                  (when type-spec
                    (loop-pop-source)
                    type-spec)))
               (t
                ;; This is our sort-of old syntax. But this is only
                ;; valid for when we are destructuring, so we will be
                ;; compulsive (should we really be?) and require that
                ;; we in fact be doing variable destructuring here. We
                ;; must translate the old keyword pattern typespec
                ;; into a fully-specified pattern of real type
                ;; specifiers here.
                (if (consp variable)
                    (unless (consp z)
                     (loop-error
                        "~S found where a LOOP keyword, LOOP type keyword, or LOOP type pattern expected"
                        z))
                    (loop-error "~S found where a LOOP keyword or LOOP type keyword expected" z))
                (loop-pop-source)
                (labels ((translate (k v)
                           (cond ((null k) nil)
                                 ((atom k)
                                  (replicate
                                    (or (std-atom-type-specifier-p k)
                                        (loop-error
                                          "The destructuring type pattern ~S contains the unrecognized type keyword ~S."
                                          z k))
                                    v))
                                 ((atom v)
                                  (loop-error
                                    "The destructuring type pattern ~S doesn't match the variable pattern ~S."
                                    z variable))
                                 (t (cons (translate (car k) (car v))
                                          (translate (cdr k) (cdr v))))))
                         (replicate (typ v)
                           (if (atom v)
                               typ
                               (cons (replicate typ (car v))
                                     (replicate typ (cdr v))))))
                  (translate z variable)))))))

;;;; loop variables

(defun loop-bind-block (&aux (loop *loop*))
  (when (or (vars loop) (declarations loop) (wrappers loop) (desetq loop))
    (push (list (nreverse (vars loop))
                (declarations loop)
                (desetq loop)
                (desetq-declarations loop)
                (wrappers loop))
          (bind-stack loop))
    (setf (vars loop) nil
          (declarations loop) nil
          (desetq loop) nil
          (desetq-declarations loop) nil
          (wrappers loop) nil)))

(defun check-var-name (name &optional (context "") &aux (loop *loop*))
  (labels ((map-name (function name)
             (do ((x (pop name) (pop name)))
                 (())
               (typecase x
                 (null)
                 (cons (map-name function x))
                 (symbol (funcall function x))
                 (t
                  (loop-error "Bad variable ~s~a" x context)))
               (typecase name
                 (cons)
                 (null
                  (return))
                 (symbol
                  (funcall function name)
                  (return))
                 (t
                  (loop-error "Bad variable ~s~a" name context)))))
           (duplicate (x)
             (loop-error "Duplicated variable ~s~a" x context))
           (find-in-desetq (name desetqs)
             (do* ((desetq desetqs (cddr desetq))
                   (var (car desetq) (car desetq)))
                  ((null desetq))
               (map-name (lambda (x)
                           (when (eql name x)
                             (duplicate name)))
                         var))))
    (cond ((consp name)
           (map-name (lambda (x) (check-var-name x context)) name))
          ((assoc name (vars loop))
           (duplicate name))
          ((find-in-desetq name (desetq loop)))
          (t
           (do ((entry (bind-stack loop) (cdr entry)))
               (nil)
             (cond
               ((null entry) (return nil))
               ((assoc name (caar entry) :test #'eq)
                (duplicate name))
               (t
                (find-in-desetq name (caddar entry)))))))))

(defun loop-make-var (name initialization dtype &optional step-var-p
                                                &aux (loop *loop*))
  (cond ((null name)
         (setq name (gensym "_"))
         (push (list name (or initialization (loop-typed-init dtype step-var-p)))
               (vars loop))
         (push `(ignore ,name) (declarations loop))
         (loop-declare-var name dtype))
        ((atom name)
         (check-var-name name)
         (loop-declare-var name dtype :step-var-p step-var-p
                                      :initialization initialization)
         ;; IGNORABLEize every variable because neither binding nor assignment constitutes
         ;; a "use". Unfortunately there is no syntax for declaring what to ignore in LOOP.
         ;; The idiom is to use NIL as a variable, but sometimes users don't, because they
         ;; want variables names as information within a destructuring operation,
         ;;  e.g. (for (this . that-not-used) in stuff do (frob this))
         (push `(ignorable ,name) (declarations loop))
         ;; We use ASSOC on this list to check for duplications (above),
         ;; so don't optimize out this list:
         (push (list name (or initialization (loop-typed-init dtype step-var-p)))
               (vars loop)))
        (initialization
         (check-var-name name)
         (let ((newvar (gensym "DS")))
           (loop-declare-var name dtype :desetq t)
           (push (list newvar initialization) (vars loop))
           ;; (DESETQ *LOOP*) gathered in reverse order.
           (setf (desetq loop)
                 (list* name newvar (desetq loop)))))
        (t
         (let ((tcar nil) (tcdr nil))
           (if (atom dtype) (setq tcar (setq tcdr dtype))
               (setq tcar (car dtype) tcdr (cdr dtype)))
           (loop-make-var (car name) nil tcar)
           (when (cdr name)
             (loop-make-var (cdr name) nil tcdr)))))
  name)

;;; Find a suitable type for default initialization
(defun type-for-default-init (type &optional step-var-p)
  (multiple-value-bind (init empty-type)
      (loop-typed-init type step-var-p)
    (values
     (cond (empty-type
            ;; Don't wrap empty types `(or ...), otherwise the will no
            ;; longer be empty and the compiler won't produce
            ;; warnings.
            type)
           ((sb-xc:typep init type)
            type)
           ((sb-xc:typep init '(simple-array * (*)))
            ;; The cross-compiler must not inquire of the host
            ;; for an array's element type.
            #+sb-xc-host (bug "Can't get here")
            ;; type-of lets the size in
            `(or (simple-array ,(array-element-type init) (*)) ,type))
           (t
            (let ((disjunct
                   #+sb-xc-host (if (null init)
                                    'null
                                    (error "Unexpected need for (TYPE-OF ~S) in xc"
                                           init))
                   #-sb-xc-host (type-of init)))
              `(or ,disjunct ,type))))
     init)))

(defun loop-declare-var (name dtype &key step-var-p initialization
                                         desetq &aux (loop *loop*))
  (cond ((or (null name) (null dtype) (eq dtype t)) nil)
        ((symbolp name)
         (unless (or (subtypep t dtype)
                     (and (eq (sb-xc:symbol-package name) *cl-package*)
                          (eq :special (info :variable :kind name))))
           (let ((dtype `(type ,(if initialization
                                    dtype
                                    (type-for-default-init dtype step-var-p))
                               ,name)))
             (if desetq
                 (push dtype (desetq-declarations loop))
                 (push dtype (declarations loop))))))
        ((consp name)
         (cond ((consp dtype)
                (loop-declare-var (car name) (car dtype)
                                  :desetq desetq)
                (loop-declare-var (cdr name) (cdr dtype)
                                  :desetq desetq))
               (t (loop-declare-var (car name) dtype
                                    :desetq desetq)
                  (loop-declare-var (cdr name) dtype
                                    :desetq desetq))))
        (t (error "invalid LOOP variable passed in: ~S" name))))


(defun loop-do-if (for negatep &aux (loop *loop*) (universe (universe loop)))
  (let ((form (loop-get-form))
        (*loop-inside-conditional* t)
        (it-p nil)
        (first-clause-p t))
    (flet ((get-clause (for)
             (do ((body nil)) (nil)
               (let ((key (car (source-code loop))) (*loop-body* nil) data)
                 (cond ((not (symbolp key))
                        (loop-error
                          "~S found where keyword expected getting LOOP clause after ~S"
                          key for))
                       (t (setf (source-context loop) (source-code loop))
                          (loop-pop-source)
                          (when (and (loop-tequal (car (source-code loop)) 'it)
                                     first-clause-p)
                            (setf (source-code loop)
                                  (cons (or it-p
                                            (setq it-p
                                                  (loop-when-it-var)))
                                        (cdr (source-code loop)))))
                          (cond ((or (not (setq data (loop-lookup-keyword
                                                       key (loop-universe-keywords universe))))
                                     (progn (apply (symbol-function (car data))
                                                   (cdr data))
                                            (null *loop-body*)))
                                 (loop-error
                                   "~S does not introduce a LOOP clause that can follow ~S."
                                   key for))
                                (t (setq body (nreconc *loop-body* body)))))))
               (setq first-clause-p nil)
               (if (loop-tequal (car (source-code loop)) :and)
                   (loop-pop-source)
                   (return (if (cdr body)
                               `(progn ,@(nreverse body))
                               (car body)))))))
      (let ((then (get-clause for))
            (else (when (loop-tequal (car (source-code loop)) :else)
                    (loop-pop-source)
                    (list (get-clause :else)))))
        (when (loop-tequal (car (source-code loop)) :end)
          (loop-pop-source))
        (when it-p (setq form `(setq ,it-p ,form)))
        (loop-pseudo-body
          `(if ,(if negatep `(not ,form) form)
               ,then
               ,@else))))))

(defun loop-do-initially ()
  (loop-disallow-conditional :initially)
  (push (loop-get-progn) (prologue *loop*)))

(defun loop-do-finally ()
  (loop-disallow-conditional :finally)
  (push (loop-get-progn) (epilogue *loop*)))

(defun loop-do-do ()
  (loop-emit-body (loop-get-progn)))

(defun loop-do-named (&aux (loop *loop*))
  (let ((name (loop-pop-source)))
    (unless (symbolp name)
      (loop-error "~S is an invalid name for your LOOP" name))
    (when (or (before-loop loop) *loop-body* (after-epilogue loop) *loop-inside-conditional*)
      (loop-error "The NAMED ~S clause occurs too late." name))
    (when (names loop)
      (loop-error "You may only use one NAMED clause in your loop: NAMED ~S ... NAMED ~S."
                  (car (names loop)) name))
    (setf (names loop) (list name))))

(defun loop-do-return ()
  (loop-emit-body (loop-construct-return (loop-get-form))))

(sb-xc:defmacro with-sum-count (lc &body body)
  (let* ((type (loop-collector-dtype lc))
         (temp-var (car (loop-collector-tempvars lc))))
    (multiple-value-bind (type init)
        (type-for-default-init type)
      `(let ((,temp-var ,init))
         (declare (type ,type ,temp-var))
         ,@body))))

(defun loop-get-collection-info (collector class default-type &aux (loop *loop*))
  (let ((form (loop-get-form))
        (name (when (loop-tequal (car (source-code loop)) 'into)
                (loop-pop-source)
                (loop-pop-source))))
    (when (not (symbolp name))
      (loop-error "The value accumulation recipient name, ~S, is not a symbol." name))
    (unless name
      (loop-disallow-aggregate-booleans))
    (let* ((specified-type (loop-optional-type))
           (dtype (or specified-type default-type))
           (cruft (find (the symbol name) (collection-cruft loop)
                        :key #'loop-collector-name)))
      (cond ((not cruft)
             (check-var-name name " in INTO clause")
             (push (setq cruft (make-loop-collector
                                :name name :class class
                                :history (list collector)
                                :specified-type specified-type
                                :dtype dtype))
                   (collection-cruft loop)))
            (t (unless (eq (loop-collector-class cruft) class)
                 (loop-error
                  "incompatible kinds of LOOP value accumulation specified for collecting~@
                    ~:[as the value of the LOOP~;~:*INTO ~S~]: ~S and ~S"
                  name (car (loop-collector-history cruft)) collector))
               (cond ((equal dtype (loop-collector-dtype cruft)))
                     ((and (null specified-type)
                           (null (loop-collector-specified-type cruft)))
                      ;; Unionize types only for default types, most
                      ;; likely, SUM and COUNT which have number and
                      ;; fixnum respectively.
                      (setf (loop-collector-dtype cruft)
                            (type-specifier
                             (type-union
                              (specifier-type dtype)
                              (specifier-type (loop-collector-dtype cruft))))))
                     (t
                      (loop-warn
                       "unequal datatypes specified in different LOOP value accumulations~@
                        into ~S: ~S and ~S"
                       name dtype (loop-collector-dtype cruft))
                      (when (eq (loop-collector-dtype cruft) t)
                        (setf (loop-collector-dtype cruft) dtype))))
               (push collector (loop-collector-history cruft))))
      (values cruft form))))

(defun loop-list-collection (specifically)      ; NCONC, LIST, or APPEND
  (multiple-value-bind (lc form)
      (loop-get-collection-info specifically 'list 'list)
    (let ((tempvars (loop-collector-tempvars lc)))
      (unless tempvars
        (setf (loop-collector-tempvars lc)
              (setq tempvars (list* (gensym "HEAD")
                                    (gensym "TAIL")
                                    (and (loop-collector-name lc)
                                         (list (loop-collector-name lc))))))
        (push `(with-loop-list-collection-head (,lc ,@tempvars)) (wrappers *loop*))
        (unless (loop-collector-name lc)
          (loop-emit-final-value `(loop-collect-answer ,(car tempvars)
                                                       ,@(cddr tempvars)))))
      (ecase specifically
        (list (setq form `(list ,form)))
        (nconc nil)
        (append (unless (and (consp form) (eq (car form) 'list))
                  (setq form `(copy-list ,form)))))
      (loop-emit-body `(loop-collect-rplacd ,tempvars ,form)))))

;;;; value accumulation: MAX, MIN, SUM, COUNT

(defun loop-sum-collection (specifically required-type default-type);SUM, COUNT
  (multiple-value-bind (lc form)
      (loop-get-collection-info specifically 'sum default-type)
    (loop-check-data-type (loop-collector-dtype lc) required-type)
    (let ((tempvars (loop-collector-tempvars lc)))
      (unless tempvars
        (setf (loop-collector-tempvars lc)
              (setq tempvars (list (or (loop-collector-name lc)
                                       (gensym "SUM")))))
        (unless (loop-collector-name lc)
          (loop-emit-final-value (car (loop-collector-tempvars lc))))
        (push `(with-sum-count ,lc) (wrappers *loop*)))
      (loop-emit-body
        (if (eq specifically 'count)
            `(when ,form
               (setq ,(car tempvars)
                     (1+ ,(car tempvars))))
            `(setq ,(car tempvars)
                   (+ ,(car tempvars)
                      ,form)))))))

(defun loop-maxmin-collection (specifically)
  (multiple-value-bind (lc form)
      (loop-get-collection-info specifically 'maxmin 'real)
    (loop-check-data-type (loop-collector-dtype lc) 'real)
    (let ((data (loop-collector-data lc)))
      (unless data
        (setf (loop-collector-data lc)
              (setq data (make-loop-minimax
                          (or (loop-collector-name lc)
                              (gensym "MINMAX"))
                          (loop-collector-dtype lc))))
        (unless (loop-collector-name lc)
          (loop-emit-final-value (loop-minimax-answer-variable data)))
        (push `(with-minimax-value ,data) (wrappers *loop*)))
      (loop-note-minimax-operation specifically data)
      (loop-emit-body `(loop-accumulate-minimax-value ,data
                                                      ,specifically
                                                      ,form)))))

;;;; value accumulation: aggregate booleans

;;; handling the ALWAYS and NEVER loop keywords
;;;
;;; Under ANSI these are not permitted to appear under conditionalization.
(defun loop-do-always (restrictive negate)
  (let ((form (loop-get-form)))
    (when restrictive (loop-disallow-conditional))
    (loop-disallow-anonymous-collectors)
    (loop-emit-body `(,(if negate 'when 'unless) ,form
                      ,(loop-construct-return nil)))
    (loop-emit-final-value t)))

;;; handling the THEREIS loop keyword
;;;
;;; Under ANSI this is not permitted to appear under conditionalization.
(defun loop-do-thereis (restrictive)
  (when restrictive (loop-disallow-conditional))
  (loop-disallow-anonymous-collectors)
  (loop-emit-final-value)
  (loop-emit-body `(when (setq ,(loop-when-it-var) ,(loop-get-form))
                    ,(loop-construct-return (when-it-var *loop*)))))

(defun loop-do-while (negate kwd &aux (form (loop-get-form)))
  (loop-disallow-conditional kwd)
  (loop-pseudo-body `(,(if negate 'when 'unless) ,form (go end-loop))))

(defun loop-do-repeat (&aux (loop *loop*))
  (loop-disallow-conditional :repeat)
  (let* ((form (loop-get-form))
         (count (and (constantp form) ; FIXME: lexical environment constants
                     (constant-form-value form)))
         (type (cond ((not (realp count))
                      'integer)
                     ((plusp count)
                      `(mod ,(1+ (ceiling count))))
                     (t
                      `(integer ,(ceiling count))))))
    (let ((var (loop-make-var (gensym "REP") `(ceiling ,form) type)))
      (push `(if (<= ,var 0) (go end-loop) (decf ,var)) (before-loop loop))
      (push `(if (<= ,var 0) (go end-loop) (decf ,var)) (after-body loop))
      ;; FIXME: What should
      ;;   (loop count t into a
      ;;         repeat 3
      ;;         count t into b
      ;;         finally (return (list a b)))
      ;; return: (3 3) or (4 3)? PUSHes above are for the former
      ;; variant, L-P-B below for the latter.
      #+nil (loop-pseudo-body `(when (minusp (decf ,var)) (go end-loop))))))

(defun loop-do-with (&aux (loop *loop*))
  (loop-disallow-conditional :with)
  (do ((var) (val) (dtype))
      (nil)
    (setq var (loop-pop-source)
          dtype (loop-optional-type var)
          val (cond ((loop-tequal (car (source-code loop)) :=)
                     (loop-pop-source)
                     (loop-get-form))
                    (t nil)))
    (loop-make-var var val dtype)
    (if (loop-tequal (car (source-code loop)) :and)
        (loop-pop-source)
        (return (loop-bind-block)))))

;;;; the iteration driver

(defun loop-hack-iteration (entry &aux (loop *loop*))
  (flet ((make-endtest (list-of-forms)
           (cond ((null list-of-forms) nil)
                 ((member t list-of-forms) '(go end-loop))
                 (t `(when ,(if (null (cdr (setq list-of-forms
                                                 (nreverse list-of-forms))))
                                (car list-of-forms)
                                (cons 'or list-of-forms))
                       (go end-loop))))))
    (do ((pre-step-tests nil)
         (steps nil)
         (post-step-tests nil)
         (pseudo-steps nil)
         (pre-loop-pre-step-tests nil)
         (pre-loop-steps nil)
         (pre-loop-post-step-tests nil)
         (pre-loop-pseudo-steps nil)
         (tem) (data))
        (nil)
      ;; Note that we collect endtests in reverse order, but steps in correct
      ;; order. MAKE-ENDTEST does the nreverse for us.
      (setq tem (setq data
                      (apply (symbol-function (first entry)) (rest entry))))
      (and (car tem) (push (car tem) pre-step-tests))
      (setq steps (nconc steps (copy-list (car (setq tem (cdr tem))))))
      (and (car (setq tem (cdr tem))) (push (car tem) post-step-tests))
      (setq pseudo-steps
            (nconc pseudo-steps (copy-list (car (setq tem (cdr tem))))))
      (setq tem (cdr tem))
      (when (emitted-body loop)
        (loop-error "iteration in LOOP follows body code"))
      (unless tem (setq tem data))
      (when (car tem) (push (car tem) pre-loop-pre-step-tests))
      ;; FIXME: This (SETF FOO (NCONC FOO BAR)) idiom appears often enough
      ;; that it might be worth making it into an NCONCF macro.
      (setq pre-loop-steps
            (nconc pre-loop-steps (copy-list (car (setq tem (cdr tem))))))
      (when (car (setq tem (cdr tem)))
        (push (car tem) pre-loop-post-step-tests))
      (setq pre-loop-pseudo-steps
            (nconc pre-loop-pseudo-steps (copy-list (cadr tem))))
      (unless (loop-tequal (car (source-code loop)) :and)
        (setf (before-loop loop)
              (list* (loop-make-desetq pre-loop-pseudo-steps)
                     (make-endtest pre-loop-post-step-tests)
                     (loop-make-psetq pre-loop-steps)
                     (make-endtest pre-loop-pre-step-tests)
                     (before-loop loop)))
        (setf (after-body loop)
              (list* (loop-make-desetq pseudo-steps)
                     (make-endtest post-step-tests)
                     (loop-make-psetq steps)
                     (make-endtest pre-step-tests)
                     (after-body loop)))
        (loop-bind-block)
        (return nil))
      (loop-pop-source)))) ; Flush the "AND".

;;;; main iteration drivers

;;; FOR variable keyword ..args..
(defun loop-do-for ()
  (let* ((var (loop-pop-source))
         (data-type (loop-optional-type var))
         (keyword (loop-pop-source))
         (first-arg nil)
         (tem nil))
    (setq first-arg (loop-get-form))
    (unless (and (symbolp keyword)
                 (setq tem (loop-lookup-keyword
                             keyword
                             (loop-universe-for-keywords (universe *loop*)))))
      (loop-error "~S is an unknown keyword in FOR or AS clause in LOOP."
                  keyword))
    (apply (car tem) var first-arg data-type (cdr tem))))

(defun loop-when-it-var (&aux (loop *loop*))
  (or (when-it-var loop)
      (setf (when-it-var loop)
            (loop-make-var (gensym "IT") nil nil))))

;;;; various FOR/AS subdispatches

;;; ANSI "FOR x = y [THEN z]" is sort of like the old Genera one when
;;; the THEN is omitted (other than being more stringent in its
;;; placement), and like the old "FOR x FIRST y THEN z" when the THEN
;;; is present. I.e., the first initialization occurs in the loop body
;;; (first-step), not in the variable binding phase.
(defun loop-ansi-for-equals (var val data-type)
  (loop-make-var var nil data-type)
  (cond ((loop-tequal (car (source-code *loop*)) :then)
         ;; Then we are the same as "FOR x FIRST y THEN z".
         (loop-pop-source)
         `(() (,var ,(loop-get-form)) () ()
           () (,var ,val) () ()))
        (t ;; We are the same as "FOR x = y".
         `(() (,var ,val) () ()))))

(defun loop-for-across (var val data-type)
  (loop-make-var var nil data-type)
  (let ((vector-var (gensym "V"))
        (data-var (gensym "D"))
        (index-var (gensym "I"))
        (compiling (sb-c::compiling-p (macro-environment *loop*))))
    (multiple-value-bind (vector-form constantp vector-value)
        (loop-constant-fold-if-possible val 'vector)
      (loop-make-var
       vector-var vector-form
       (if (and (consp vector-form) (eq (car vector-form) 'the))
           (cadr vector-form)
           'vector))
      (cond (compiling
             (push `(multiple-value-bind (,data-var ,index-var)
                        (%data-vector-and-index/known ,vector-var 0))
                   (wrappers *loop*)))
            (t
             (loop-make-var index-var 0 'fixnum)
             (setf data-var vector-var)))
      (let* ((length-form (if constantp
                              (length vector-value)
                              (let ((v (gensym "LIM")))
                                (push (if compiling
                                          `(let ((,v (+ (length ,vector-var) ,index-var)))
                                             (sb-c::%in-bounds-constraint ,data-var ,v))
                                          `(let ((,v (length ,vector-var)))))
                                         (wrappers *loop*))
                                   v)))
             (test `(>= ,index-var ,length-form))
             (step `(,var (aref ,data-var ,index-var)))
             (pstep `(,index-var (1+ ,index-var))))
        `(,test ,step () ,pstep)))))

;;;; list iteration

(defun loop-list-step (listvar)
  (let ((stepper (cond ((loop-tequal (car (source-code *loop*)) :by)
                        (loop-pop-source)
                        (loop-get-form))
                       (t '(function cdr)))))
    (cond ((and (consp stepper) (eq (car stepper) 'quote))
           `(funcall ,stepper ,listvar))
          ((and (consp stepper) (eq (car stepper) 'function))
           (list (cadr stepper) listvar))
          (t
           `(funcall ,(loop-make-var (gensym "F") stepper 'function)
                     ,listvar)))))

(defun loop-for-on (var val data-type)
  (multiple-value-bind (list constantp list-value)
      (loop-constant-fold-if-possible val)
    (let ((listvar var))
      (cond ((and var (symbolp var))
             (loop-make-var var
                            ;; Don't want to assert the type, as ENDP will do that
                            `(the* (list :use-annotations t :source-form ,list) ,list)
                            data-type))
            (t
             (loop-make-var (setq listvar (gensym)) list 't)
             (loop-make-var var nil data-type)))
      (let ((list-step (loop-list-step listvar)))
        (let* ((first-endtest
                ;; the following should use `atom' instead of `endp',
                ;; per 6.1.2.1.3
                `(atom ,listvar))
               (other-endtest first-endtest))
          (when (and constantp (listp list-value))
            (setq first-endtest (null list-value)))
          (cond ((eq var listvar)
                 ;; The contour of the loop is different because we
                 ;; use the user's variable...
                 `(() (,listvar ,list-step)
                   ,other-endtest () () () ,first-endtest ()))
                (t (let ((step `(,var ,listvar))
                         (pseudo `(,listvar ,list-step)))
                     `(,other-endtest ,step () ,pseudo
                       ,@(and (neq first-endtest other-endtest)
                              `(,first-endtest ,step () ,pseudo)))))))))))

(defun loop-for-in (var val data-type)
  (cond ((and (typep val '(cons (eql reverse) (cons t null)))
              (not (sb-c::fun-lexically-notinline-p 'reverse
                                                    (macro-environment *loop*)))
              (let ((stepper (and (loop-tequal (car (source-code *loop*)) :by)
                                  (source-code *loop*))))
                (cond ((member (cadr stepper) '(#'cddr 'cddr) :test #'equal)
                       (loop-pop-source)
                       (loop-pop-source)
                       (loop-for-across var `(list-reverse-into-vector-cddr ,(second val)) data-type))
                      ((not stepper)
                       (loop-for-across var `(list-reverse-into-vector ,(second val)) data-type))))))
        (t
         (multiple-value-bind (list constantp list-value)
             (loop-constant-fold-if-possible val)
           (let ((listvar (gensym "L")))
             (loop-make-var var nil data-type)
             (loop-make-var listvar
                            ;; Don't want to assert the type, as ENDP will do that
                            `(the* (list :use-annotations t :source-form ,list) ,list)
                            t)
             (let ((list-step (loop-list-step listvar)))
               (let* ((first-endtest `(endp ,listvar))
                      (other-endtest first-endtest)
                      (step `(,var (car ,listvar)))
                      (pseudo-step `(,listvar ,list-step)))
                 (when (and constantp (listp list-value))
                   (setq first-endtest (null list-value)))
                 `(,other-endtest ,step () ,pseudo-step
                                  ,@(and (neq first-endtest other-endtest)
                                         `(,first-endtest ,step () ,pseudo-step))))))))))

;;;; iteration paths

(defstruct (loop-path
            (:copier nil)
            (:predicate nil))
  (names nil :read-only t)
  (preposition-groups nil :read-only t)
  (inclusive-permitted nil :read-only t)
  (function nil :read-only t)
  (user-data nil :read-only t))
(declaim (sb-ext:freeze-type loop-path))

(defun add-loop-path (names function universe
                      &key preposition-groups inclusive-permitted user-data)
  (declare (type loop-universe universe))
  (let* ((names (ensure-list names))
         (ht (loop-universe-path-keywords universe))
         (lp (make-loop-path
              :names (mapcar #'symbol-name names)
              :function function
              :user-data user-data
              :preposition-groups (mapcar #'ensure-list preposition-groups)
              :inclusive-permitted inclusive-permitted)))
    (dolist (name names)
      (setf (gethash (symbol-name name) ht) lp))
    lp))

;;; Note: Path functions are allowed to use LOOP-MAKE-VAR, hack
;;; the prologue, etc.
(defun loop-for-being (var val data-type &aux (loop *loop*) (universe (universe loop)))
  ;; FOR var BEING each/the pathname prep-phrases using-stuff... each/the =
  ;; EACH or THE. Not clear if it is optional, so I guess we'll warn.
  (let ((path nil)
        (data nil)
        (inclusive nil)
        (stuff nil)
        (initial-prepositions nil))
    (cond ((loop-tmember val '(:each :the)) (setq path (loop-pop-source)))
          ((loop-tequal (car (source-code loop)) :and)
           (loop-pop-source)
           (setq inclusive t)
           (unless (loop-tmember (car (source-code loop))
                                 '(:its :each :his :her))
             (loop-error "~S was found where ITS or EACH expected in LOOP iteration path syntax."
                         (car (source-code loop))))
           (loop-pop-source)
           (setq path (loop-pop-source))
           (setq initial-prepositions `((:in ,val))))
          (t (loop-error "unrecognizable LOOP iteration path syntax: missing EACH or THE?")))
    (cond ((not (symbolp path))
           (loop-error
            "~S was found where a LOOP iteration path name was expected."
            path))
          ((not (setq data (loop-lookup-keyword path (loop-universe-path-keywords universe))))
           (loop-error "~S is not the name of a LOOP iteration path." path))
          ((and inclusive (not (loop-path-inclusive-permitted data)))
           (loop-error "\"Inclusive\" iteration is not possible with the ~S LOOP iteration path." path)))
    (let ((fun (loop-path-function data))
          (preps (nconc initial-prepositions
                        (loop-collect-prepositional-phrases
                         (loop-path-preposition-groups data)
                         t)))
          (user-data (loop-path-user-data data)))
      (when (symbolp fun) (setq fun (symbol-function fun)))
      (setq stuff (if inclusive
                      (apply fun var data-type preps :inclusive t user-data)
                      (apply fun var data-type preps user-data))))
    (when (named-vars loop)
      (loop-error "Unused USING vars: ~S." (named-vars loop)))
    ;; STUFF is now (bindings prologue-forms . stuff-to-pass-back).
    ;; Protect the system from the user and the user from himself.
    (unless (member (length stuff) '(6 10))
      (loop-error "Value passed back by LOOP iteration path function for path ~S has invalid length."
                  path))
    (do ((l (car stuff) (cdr l)) (x)) ((null l))
      (if (atom (setq x (car l)))
          (loop-make-var x nil nil)
          (loop-make-var (car x) (cadr x) (caddr x))))
    (setf (prologue loop) (nconc (reverse (cadr stuff)) (prologue loop)))
    (cddr stuff)))

(defun loop-named-var (name &aux (loop *loop*))
  (let ((tem (loop-tassoc name (named-vars loop))))
    (declare (list tem))
    (cond ((null tem) (values (gensym) nil))
          (t (setf (named-vars loop) (delete tem (named-vars loop)))
             (values (cdr tem) t)))))

(defun loop-collect-prepositional-phrases (preposition-groups
                                           &optional
                                           using-allowed
                                           initial-phrases &aux (loop *loop*))
  (flet ((in-group-p (x group) (car (loop-tmember x group))))
    (do ((token nil)
         (prepositional-phrases initial-phrases)
         (this-group nil nil)
         (this-prep nil nil)
         (disallowed-prepositions
           (mapcan (lambda (x)
                     (copy-list
                      (find (car x) preposition-groups :test #'in-group-p)))
                   initial-phrases))
         (used-prepositions (mapcar #'car initial-phrases)))
        ((null (source-code loop)) (nreverse prepositional-phrases))
      (declare (symbol this-prep))
      (setq token (car (source-code loop)))
      (dolist (group preposition-groups)
        (when (setq this-prep (in-group-p token group))
          (return (setq this-group group))))
      (cond (this-group
             (when (member this-prep disallowed-prepositions)
               (loop-error
                 (if (member this-prep used-prepositions)
                     "A ~S prepositional phrase occurs multiply for some LOOP clause."
                     "Preposition ~S was used when some other preposition has subsumed it.")
                 token))
             (setq used-prepositions (if (listp this-group)
                                         (append this-group used-prepositions)
                                         (cons this-group used-prepositions)))
             (loop-pop-source)
             (push (list this-prep (loop-get-form)) prepositional-phrases))
            ((and using-allowed (loop-tequal token 'using))
             (loop-pop-source)
             (do ((z (loop-pop-source) (loop-pop-source)) (tem)) (nil)
               (when (cadr z)
                 (if (setq tem (loop-tassoc (car z) (named-vars loop)))
                     (loop-error
                       "The variable substitution for ~S occurs twice in a USING phrase,~@
                        with ~S and ~S."
                       (car z) (cadr z) (cadr tem))
                     (push (cons (car z) (cadr z)) (named-vars loop))))
               (when (or (null (source-code loop))
                         (symbolp (car (source-code loop))))
                 (return nil))))
            (t (return (nreverse prepositional-phrases)))))))

;;;; master sequencer function

(defun loop-sequencer (indexv indexv-type
                       variable variable-type
                       sequence-variable sequence-type
                       step-hack default-top
                       prep-phrases &aux (loop *loop*))
   (let ((endform nil) ; form (constant or variable) with limit value
         (sequencep nil) ; T if sequence arg has been provided
         (testfn nil) ; endtest function
         (test nil) ; endtest form
         (stepby (1+ (or (loop-typed-init indexv-type) 0))) ; our increment
         (stepby-constantp t)
         (step nil) ; step form
         (dir nil) ; direction of stepping: NIL, :UP, :DOWN
         (inclusive-iteration nil) ; T if include last index
         (start-given nil) ; T when prep phrase has specified start
         (start-value nil)
         (start-constantp nil)
         (limit-given nil) ; T when prep phrase has specified end
         (limit-constantp nil)
         (limit-value nil))
     ;; Silence the assigned-but-never-set warnings that CCL and CLISP emit
     (declare (ignorable start-constantp start-value))
     (flet ((assert-index-for-arithmetic (index)
              (unless (atom index)
                (loop-error "Arithmetic index must be an atom."))))
       (when variable (loop-make-var variable nil variable-type))
       (do ((l prep-phrases (cdr l)) (prep) (form) (odir)) ((null l))
         (setq prep (caar l) form (cadar l))
         (case prep
           ((:of :in)
            (setq sequencep t)
            (loop-make-var sequence-variable form sequence-type))
           ((:from :downfrom :upfrom)
            (setq start-given t)
            (cond ((eq prep :downfrom) (setq dir ':down))
                  ((eq prep :upfrom) (setq dir ':up)))
            (multiple-value-setq (form start-constantp start-value)
              (loop-constant-fold-if-possible form indexv-type))
            (assert-index-for-arithmetic indexv)
            ;; KLUDGE: loop-make-var generates a temporary symbol for
            ;; indexv if it is NIL. We have to use it to have the index
            ;; actually count
            (setq indexv (loop-make-var indexv form indexv-type)))
           ((:upto :to :downto :above :below)
            (cond ((loop-tequal prep :upto) (setq inclusive-iteration
                                                  (setq dir ':up)))
                  ((loop-tequal prep :to) (setq inclusive-iteration t))
                  ((loop-tequal prep :downto) (setq inclusive-iteration
                                                    (setq dir ':down)))
                  ((loop-tequal prep :above) (setq dir ':down))
                  ((loop-tequal prep :below) (setq dir ':up)))
            (setq limit-given t)
            (multiple-value-setq (form limit-constantp limit-value)
              (loop-constant-fold-if-possible form `(and ,indexv-type real)))
            (setq endform (if limit-constantp
                              `',limit-value
                              (loop-make-var
                                 (gensym "LIM") form
                                 `(and ,indexv-type real)))))
           (:by
            (multiple-value-setq (form stepby-constantp stepby)
              (loop-constant-fold-if-possible form
                                              `(and ,indexv-type (real (0)))))
            (unless stepby-constantp
              (loop-make-var (setq stepby (gensym "STEP"))
                 form
                 `(and ,indexv-type (real (0)))
                 t)))
           (t (loop-error
                 "~S invalid preposition in sequencing or sequence path;~@
              maybe invalid prepositions were specified in iteration path descriptor?"
                 prep)))
         (when (and odir dir (neq dir odir))
           (loop-error
             "conflicting stepping directions in LOOP sequencing path"))
         (setq odir dir))
       (when (and sequence-variable (not sequencep))
         (loop-error "missing OF or IN phrase in sequence path"))
       ;; Now fill in the defaults.
       (cond ((not start-given)
              ;; default start
              ;; DUPLICATE KLUDGE: loop-make-var generates a temporary
              ;; symbol for indexv if it is NIL. See also the comment in
              ;; the (:from :downfrom :upfrom) case
              (assert-index-for-arithmetic indexv)
              (setq indexv
                    (loop-make-var
                     indexv
                     (setq start-constantp t
                           start-value (or (loop-typed-init indexv-type) 0))
                     `(and ,indexv-type real))))
             (limit-given
              ;; if both start and limit are given, they had better both
              ;; be REAL.  We already enforce the REALness of LIMIT,
              ;; above; here's the KLUDGE to enforce the type of START.
              (flet ((type-declaration-of (x)
                       (and (eq (car x) 'type) (caddr x))))
                (let ((decl (find indexv (declarations loop)
                                  :key #'type-declaration-of))
                      (%decl (find indexv (declarations loop)
                                   :key #'type-declaration-of
                                   :from-end t)))
                  (aver (eq decl %decl))
                  (when decl
                    (setf (cadr decl)
                          `(and real ,(cadr decl))))))))
       (cond ((member dir '(nil :up))
              (when (or limit-given default-top)
                (unless limit-given
                  (loop-make-var (setq endform (gensym "LIM"))
                     nil
                     indexv-type)
                  (push `(setq ,endform ,default-top) (prologue loop)))
                (setq testfn (if inclusive-iteration '> '>=)))
              (setq step (if (eql stepby 1) `(1+ ,indexv) `(+ ,indexv ,stepby))))
             (t (unless start-given
                  (unless default-top
                    (loop-error "don't know where to start stepping"))
                  (push `(setq ,indexv (1- ,default-top)) (prologue loop)))
                (when (and default-top (not endform))
                  (setq endform (loop-typed-init indexv-type)
                        inclusive-iteration t))
                (when endform (setq testfn (if inclusive-iteration  '< '<=)))
                (setq step
                      (if (eql stepby 1) `(1- ,indexv) `(- ,indexv ,stepby)))))
       (when testfn
         (setq test
               `(,testfn ,indexv ,endform)))
       (when step-hack
         (setq step-hack
               `(,variable ,step-hack)))
       (let ((first-test test) (remaining-tests test))
         ;; As far as I can tell, the effect of the following code is
         ;; to detect cases where we know statically whether the first
         ;; iteration of the loop will be executed. Depending on the
         ;; situation, we can either:
         ;;  a) save one jump and one comparison per loop (not per iteration)
         ;;     when it will get executed
         ;;  b) remove the loop body completely when it won't be executed
         ;;
         ;; Noble goals. However, the code generated in case a) will
         ;; fool the loop induction variable detection, and cause
         ;; code like (LOOP FOR I TO 10 ...) to use generic addition
         ;; (bug #278a).
         ;;
         ;; Since the gain in case a) is rather minimal and Python is
         ;; generally smart enough to handle b) without any extra
         ;; support from the loop macro, I've disabled this code for
         ;; now. The code and the comment left here in case somebody
         ;; extends the induction variable bound detection to work
         ;; with code where the stepping precedes the test.
         ;; -- JES 2005-11-30
         #+nil
         (when (and stepby-constantp start-constantp limit-constantp
                    (realp start-value) (realp limit-value))
           (when (setq first-test
                       (funcall (symbol-function testfn)
                                start-value
                                limit-value))
             (setq remaining-tests t)))
         `(() (,indexv ,step)
           ,remaining-tests ,step-hack () () ,first-test ,step-hack)))))

;;;; interfaces to the master sequencer

(defun loop-for-arithmetic (var val data-type kwd)
  (loop-sequencer
   var (loop-check-data-type data-type 'number)
   nil nil nil nil nil nil
   (loop-collect-prepositional-phrases
    '((:from :upfrom :downfrom) (:to :upto :downto :above :below) (:by))
    nil (list (list kwd val)))))


;;;; builtin LOOP iteration paths

#||
(loop for v being the hash-values of ht do (print v))
(loop for k being the hash-keys of ht do (print k))
(loop for v being the hash-values of ht using (hash-key k) do (print (list k v)))
(loop for k being the hash-keys of ht using (hash-value v) do (print (list k v)))
||#

(defun loop-hash-table-iteration-path (variable data-type prep-phrases
                                       &key (which (missing-arg)))
  (declare (type (member :hash-key :hash-value) which))
  (cond ((or (cdr prep-phrases) (not (member (caar prep-phrases) '(:in :of))))
         (loop-error "Too many prepositions!"))
        ((null prep-phrases)
         (loop-error "missing OF or IN in ~S iteration path")))
  (let ((ht-var (gensym "HT")) ; the table
        (next-fn (gensym "NEXT")) ; WITH-HASH-TABLE-ITERATOR name
        (dummy-predicate-var nil)
        (post-steps nil))
    (multiple-value-bind (other-var other-p)
        (loop-named-var (ecase which
                          (:hash-key 'hash-value)
                          (:hash-value 'hash-key)))
      ;; @@@@ LOOP-NAMED-VAR returns a second value of T if the name
      ;; was actually specified, so clever code can throw away the
      ;; GENSYM'ed-up variable if it isn't really needed. The
      ;; following is for those implementations in which we cannot put
      ;; dummy NILs into MULTIPLE-VALUE-SETQ variable lists.
      (setq other-p t
            dummy-predicate-var (loop-when-it-var))
      (let* ((key-var nil)
             (val-var nil)
             (variable (or variable (gensym "VAR")))
             (bindings `((,variable nil ,data-type)
                         (,ht-var ,(cadar prep-phrases))
                         ,@(and other-p other-var `((,other-var nil))))))
        (ecase which
          (:hash-key (setq key-var variable
                           val-var (and other-p other-var)))
          (:hash-value (setq key-var (and other-p other-var)
                             val-var variable)))
        (push `(with-hash-table-iterator (,next-fn ,ht-var)) (wrappers *loop*))
        (when (or (consp key-var) data-type)
          (setq post-steps
                `(,key-var ,(setq key-var (gensym "KTMP"))
                           ,@post-steps))
          (push `(,key-var nil) bindings))
        (when (or (consp val-var) data-type)
          (setq post-steps
                `(,val-var ,(setq val-var (gensym "VTMP"))
                           ,@post-steps))
          (push `(,val-var nil) bindings))
        `(,bindings                     ;bindings
          ()                            ;prologue
          ()                            ;pre-test
          ()                            ;parallel steps
          (not (multiple-value-setq (,dummy-predicate-var ,key-var ,val-var)
                 (,next-fn)))           ;post-test
          ,post-steps)))))

(defun loop-package-symbols-iteration-path (variable data-type prep-phrases
                                            &key symbol-types)
  (cond ((and prep-phrases (cdr prep-phrases))
         (loop-error "Too many prepositions!"))
        ((and prep-phrases (not (member (caar prep-phrases) '(:in :of))))
         (bug "Unknown preposition ~S." (caar prep-phrases))))
  (unless (symbolp variable)
    (loop-error "Destructuring is not valid for package symbol iteration."))
  (let ((pkg-var (gensym "SYM"))
        (next-fn (gensym "NEXT")) ; WITH-PACKAGE-ITERATOR name
        (variable (or variable (gensym "VAR")))
        (package (or (cadar prep-phrases) '*package*)))
    (push `(with-package-iterator (,next-fn ,pkg-var ,@symbol-types))
          (wrappers *loop*))
    `(((,variable nil ,data-type) (,pkg-var ,package))
      ()
      ()
      ()
      (not (multiple-value-setq (,(loop-when-it-var)
                                 ,variable)
             (,next-fn)))
      ())))

;;;; ANSI LOOP

(sb-ext:define-load-time-global *loop-ansi-universe*
  (let ((w (!make-standard-loop-universe
             :keywords '((named (loop-do-named))
                         (initially (loop-do-initially))
                         (finally (loop-do-finally))
                         (do (loop-do-do))
                         (doing (loop-do-do))
                         (return (loop-do-return))
                         (collect (loop-list-collection list))
                         (collecting (loop-list-collection list))
                         (append (loop-list-collection append))
                         (appending (loop-list-collection append))
                         (nconc (loop-list-collection nconc))
                         (nconcing (loop-list-collection nconc))
                         (count (loop-sum-collection count
                                 ;; This could be REAL, but when
                                 ;; combined with SUM, it has to be
                                 ;; NUMBER.
                                 number
                                 fixnum))
                         (counting (loop-sum-collection count
                                                        number
                                                        fixnum))
                         (sum (loop-sum-collection sum number number))
                         (summing (loop-sum-collection sum number number))
                         (maximize (loop-maxmin-collection max))
                         (minimize (loop-maxmin-collection min))
                         (maximizing (loop-maxmin-collection max))
                         (minimizing (loop-maxmin-collection min))
                         (always (loop-do-always t nil)) ; Normal, do always
                         (never (loop-do-always t t)) ; Negate test on always.
                         (thereis (loop-do-thereis t))
                         (while (loop-do-while nil :while)) ; Normal, do while
                         (until (loop-do-while t :until)) ;Negate test on while
                         (when (loop-do-if when nil))   ; Normal, do when
                         (if (loop-do-if if nil))       ; synonymous
                         (unless (loop-do-if unless t)) ; Negate test on when
                         (with (loop-do-with))
                         (repeat (loop-do-repeat)))
             :for-keywords '((= (loop-ansi-for-equals))
                             (across (loop-for-across))
                             (in (loop-for-in))
                             (on (loop-for-on))
                             (from (loop-for-arithmetic :from))
                             (downfrom (loop-for-arithmetic :downfrom))
                             (upfrom (loop-for-arithmetic :upfrom))
                             (below (loop-for-arithmetic :below))
                             (above (loop-for-arithmetic :above))
                             (to (loop-for-arithmetic :to))
                             (upto (loop-for-arithmetic :upto))
                             (downto (loop-for-arithmetic :downto))
                             (by (loop-for-arithmetic :by))
                             (being (loop-for-being)))
             :iteration-keywords '((for (loop-do-for))
                                   (as (loop-do-for))))))
    (add-loop-path '(hash-key hash-keys) 'loop-hash-table-iteration-path w
                   :preposition-groups '((:of :in))
                   :inclusive-permitted nil
                   :user-data '(:which :hash-key))
    (add-loop-path '(hash-value hash-values) 'loop-hash-table-iteration-path w
                   :preposition-groups '((:of :in))
                   :inclusive-permitted nil
                   :user-data '(:which :hash-value))
    (add-loop-path '(symbol symbols) 'loop-package-symbols-iteration-path w
                   :preposition-groups '((:of :in))
                   :inclusive-permitted nil
                   :user-data '(:symbol-types (:internal
                                               :external
                                               :inherited)))
    (add-loop-path '(external-symbol external-symbols)
                   'loop-package-symbols-iteration-path w
                   :preposition-groups '((:of :in))
                   :inclusive-permitted nil
                   :user-data '(:symbol-types (:external)))
    (add-loop-path '(present-symbol present-symbols)
                   'loop-package-symbols-iteration-path w
                   :preposition-groups '((:of :in))
                   :inclusive-permitted nil
                   :user-data '(:symbol-types (:internal
                                               :external)))
    (add-loop-path '(element elements)
                   'loop-elements-iteration-path w
                   :preposition-groups '((:of :in))
                   :inclusive-permitted nil)
    w))

(defun loop-standard-expansion (keywords-and-forms environment universe)
  (if (and keywords-and-forms (symbolp (car keywords-and-forms)))
      (loop-translate (make-loop keywords-and-forms environment universe))
      (let ((tag (gensym)))
        `(block nil (tagbody ,tag (progn ,@keywords-and-forms) (go ,tag))))))

(sb-xc:defmacro loop (&environment env &rest keywords-and-forms)
  (loop-standard-expansion keywords-and-forms
                           env
                           *loop-ansi-universe*))

(sb-xc:defmacro loop-finish ()
  "Cause the iteration to terminate \"normally\", the same as implicit
termination by an iteration driving clause, or by use of WHILE or
UNTIL -- the epilogue code (if any) will be run, and any implicitly
collected result will be returned as the value of the LOOP."
  '(go end-loop))

;;; Hack for clsql
(define-symbol-macro *loop-epilogue* (epilogue *loop*))
