;;;; various user-level definitions which need to be done particularly
;;;; early

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;;; target constants which need to appear as early as possible

;;; an internal tag for marking empty slots, which needs to be defined
;;; as early as possible because it appears in macroexpansions for
;;; iteration over hash tables
;;;
;;; CMU CL 18b used :EMPTY for this purpose, which was somewhat nasty
;;; since it's easily accessible to the user, so that e.g.
;;;     (DEFVAR *HT* (MAKE-HASH-TABLE))
;;;     (SETF (GETHASH :EMPTY *HT*) :EMPTY)
;;;     (MAPHASH (LAMBDA (K V) (FORMAT T "~&~S ~S~%" K V)))
;;; gives no output -- oops!
;;;
;;; FIXME: It'd probably be good to use the unbound marker for this.
;;; However, there might be some gotchas involving assumptions by
;;; e.g. AREF that they're not going to return the unbound marker,
;;; and there's also the noted-below problem that the C-level code
;;; contains implicit assumptions about this marker.
;;;
;;; KLUDGE: Note that as of version 0.pre7 there's a dependence in the
;;; gencgc.c code on this value being a symbol. (This is only one of
;;; several nasty dependencies between that code and this, alas.)
;;; -- WHN 2001-08-17
(eval-when (:compile-toplevel :load-toplevel :execute)
  (def!constant +empty-ht-slot+ '%empty-ht-slot%))
;;; We shouldn't need this mess now that EVAL-WHEN works.

;;; KLUDGE: Using a private symbol still leaves us vulnerable to users
;;; getting nonconforming behavior by messing around with
;;; DO-ALL-SYMBOLS. That seems like a fairly obscure problem, so for
;;; now we just don't worry about it. If for some reason it becomes
;;; worrisome and the magic value needs replacement:
;;;   * The replacement value needs to be LOADable with EQL preserved,
;;;     so that the macroexpansion for WITH-HASH-TABLE-ITERATOR will
;;;     work when compiled into a file and loaded back into SBCL.
;;;     (Thus, just uninterning %EMPTY-HT-SLOT% doesn't work.)
;;;   * The replacement value needs to be acceptable to the
;;;     low-level gencgc.lisp hash table scavenging code.
;;;   * The change will break binary compatibility, since comparisons
;;;     against the value used at the time of compilation are wired
;;;     into FASL files.
;;; -- WHN 20000622

;;;; DO-related stuff which needs to be visible on the cross-compilation host

(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
  (defun frob-do-body (varlist endlist decls-and-code bind step name block)
    (let* ((r-inits nil) ; accumulator for reversed list
           (r-steps nil) ; accumulator for reversed list
           (label-1 (gensym))
           (label-2 (gensym)))
      ;; Check for illegal old-style DO.
      (when (or (not (listp varlist)) (atom endlist))
        (error "ill-formed ~S -- possibly illegal old style DO?" name))
      ;; Parse VARLIST to get R-INITS and R-STEPS.
      (dolist (v varlist)
        (flet (;; (We avoid using CL:PUSH here so that CL:PUSH can be
               ;; defined in terms of CL:SETF, and CL:SETF can be
               ;; defined in terms of CL:DO, and CL:DO can be defined
               ;; in terms of the current function.)
               (push-on-r-inits (x)
                 (setq r-inits (cons x r-inits)))
               ;; common error-handling
               (illegal-varlist ()
                 (error "~S is an illegal form for a ~S varlist." v name)))
          (cond ((symbolp v) (push-on-r-inits v))
                ((listp v)
                 (unless (symbolp (first v))
                   (error "~S step variable is not a symbol: ~S"
                          name
                          (first v)))
                 (let ((lv (length v)))
                   ;; (We avoid using CL:CASE here so that CL:CASE can
                   ;; be defined in terms of CL:SETF, and CL:SETF can
                   ;; be defined in terms of CL:DO, and CL:DO can be
                   ;; defined in terms of the current function.)
                   (cond ((= lv 1)
                          (push-on-r-inits (first v)))
                         ((= lv 2)
                          (push-on-r-inits v))
                         ((= lv 3)
                          (push-on-r-inits (list (first v) (second v)))
                          (setq r-steps (list* (third v) (first v) r-steps)))
                         (t (illegal-varlist)))))
                (t (illegal-varlist)))))
      ;; Construct the new form.
      (multiple-value-bind (code decls)
          (parse-body decls-and-code :doc-string-allowed nil)
        `(block ,block
           (,bind ,(nreverse r-inits)
                  ,@decls
                  (tagbody
                     (go ,label-2)
                     ,label-1
                     (tagbody ,@code)
                     (,step ,@(nreverse r-steps))
                     ,label-2
                     (unless ,(first endlist) (go ,label-1))
                     (return-from ,block (progn ,@(rest endlist))))))))))

;;; This is like DO, except it has no implicit NIL block. Each VAR is
;;; initialized in parallel to the value of the specified INIT form.
;;; On subsequent iterations, the VARS are assigned the value of the
;;; STEP form (if any) in parallel. The TEST is evaluated before each
;;; evaluation of the body FORMS. When the TEST is true, the
;;; EXIT-FORMS are evaluated as a PROGN, with the result being the
;;; value of the DO.
(defmacro do-anonymous (varlist endlist &rest body)
  (frob-do-body varlist endlist body 'let 'psetq 'do-anonymous (gensym)))

;;;; GENSYM tricks

;;; GENSYM variant for easier debugging and better backtraces: append
;;; the closest enclosing non-nil block name to the provided stem.
(defun block-gensym (&optional (name "G") (env (when (boundp 'sb!c::*lexenv*)
                                             (symbol-value 'sb!c::*lexenv*))))
  (let ((block-name (when env
                      (car (find-if #'car (sb!c::lexenv-blocks env))))))
    (if block-name
        (sb!xc:gensym (format nil "~A[~A]" name block-name))
        (sb!xc:gensym name))))

;;; Compile a version of BODY for all TYPES, and dispatch to the
;;; correct one based on the value of VAR. This was originally used
;;; only for strings, hence the name. Renaming it to something more
;;; generic might not be a bad idea.
(defmacro string-dispatch ((&rest types) var &body body)
  (let ((fun (sb!xc:gensym "STRING-DISPATCH-FUN")))
    `(flet ((,fun (,var)
              ,@body))
       (declare (inline ,fun))
       (etypecase ,var
         ,@(loop for type in types
                 ;; TRULY-THE allows transforms to take advantage of the type
                 ;; information without need for constraint propagation.
                 collect `(,type (,fun (truly-the ,type ,var))))))))

;;; Automate an idiom often found in macros:
;;;   (LET ((FOO (GENSYM "FOO"))
;;;         (MAX-INDEX (GENSYM "MAX-INDEX-")))
;;;     ...)
;;;
;;; "Good notation eliminates thought." -- Eric Siggia
;;;
;;; Incidentally, this is essentially the same operator which
;;; _On Lisp_ calls WITH-GENSYMS.
(defmacro with-unique-names (symbols &body body)
  `(let ,(mapcar (lambda (symbol)
                   (let* ((symbol-name (symbol-name symbol))
                          (stem (if (every #'alpha-char-p symbol-name)
                                    symbol-name
                                    (concatenate 'string symbol-name "-"))))
                     `(,symbol (block-gensym ,stem))))
                 symbols)
     ,@body))

;;; Return a list of N gensyms. (This is a common suboperation in
;;; macros and other code-manipulating code.)
(declaim (ftype (function (index) list) make-gensym-list))
(defun make-gensym-list (n)
  (loop repeat n collect (block-gensym)))

;;;; miscellany

;;; Lots of code wants to get to the KEYWORD package or the
;;; COMMON-LISP package without a lot of fuss, so we cache them in
;;; variables. TO DO: How much does this actually buy us? It sounds
;;; sensible, but I don't know for sure that it saves space or time..
;;; -- WHN 19990521
;;;
;;; (The initialization forms here only matter on the cross-compilation
;;; host; In the target SBCL, these variables are set in cold init.)
(declaim (type package *cl-package* *keyword-package*))
(defvar *cl-package*      (find-package "COMMON-LISP"))
(defvar *keyword-package* (find-package "KEYWORD"))

;;; Concatenate together the names of some strings and symbols,
;;; producing a symbol in the current package.
(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
  (defun symbolicate (&rest things)
    (let* ((length (reduce #'+ things
                           :key (lambda (x) (length (string x)))))
           (name (make-array length :element-type 'character)))
      (let ((index 0))
        (dolist (thing things (values (intern name)))
          (let* ((x (string thing))
                 (len (length x)))
            (replace name x :start1 index)
            (incf index len)))))))

;;; like SYMBOLICATE, but producing keywords
(defun keywordicate (&rest things)
  (let ((*package* *keyword-package*))
    (apply #'symbolicate things)))

;;; Access *PACKAGE* in a way which lets us recover when someone has
;;; done something silly like (SETF *PACKAGE* :CL-USER). (Such an
;;; assignment is undefined behavior, so it's sort of reasonable for
;;; it to cause the system to go totally insane afterwards, but it's a
;;; fairly easy mistake to make, so let's try to recover gracefully
;;; instead.)
(defun sane-package ()
  (let ((maybe-package *package*))
    (cond ((and (packagep maybe-package)
                ;; For good measure, we also catch the problem of
                ;; *PACKAGE* being bound to a deleted package.
                ;; Technically, this is not undefined behavior in itself,
                ;; but it will immediately lead to undefined to behavior,
                ;; since almost any operation on a deleted package is
                ;; undefined.
                (package-name maybe-package))
           maybe-package)
          (t
           ;; We're in the undefined behavior zone. First, munge the
           ;; system back into a defined state.
           (let ((really-package (find-package :cl-user)))
             (setf *package* really-package)
             ;; Then complain.
             (error 'simple-type-error
                    :datum maybe-package
                    :expected-type '(and package (satisfies package-name))
                    :format-control
                    "~@<~S can't be a ~A: ~2I~_~S has been reset to ~S.~:>"
                    :format-arguments (list '*package*
                                            (if (packagep maybe-package)
                                                "deleted package"
                                                (type-of maybe-package))
                                            '*package* really-package)))))))

;;; Access *DEFAULT-PATHNAME-DEFAULTS*, issuing a warning if its value
;;; is silly. (Unlike the vaguely-analogous SANE-PACKAGE, we don't
;;; actually need to reset the variable when it's silly, since even
;;; crazy values of *DEFAULT-PATHNAME-DEFAULTS* don't leave the system
;;; in a state where it's hard to recover interactively.)
(defun sane-default-pathname-defaults ()
  (let* ((dfd *default-pathname-defaults*)
         (dfd-dir (pathname-directory dfd)))
    ;; It's generally not good to use a relative pathname for
    ;; *DEFAULT-PATHNAME-DEFAULTS*, since relative pathnames
    ;; are defined by merging into a default pathname (which is,
    ;; by default, *DEFAULT-PATHNAME-DEFAULTS*).
    (when (and (consp dfd-dir)
               (eql (first dfd-dir) :relative))
      (warn
       "~@<~S is a relative pathname. (But we'll try using it anyway.)~@:>"
       '*default-pathname-defaults*))
    dfd))

;;; Give names to elements of a numeric sequence.
(defmacro defenum ((&key (start 0) (step 1))
                   &rest identifiers)
  (let ((results nil)
        (index 0)
        (start (eval start))
        (step (eval step)))
    (dolist (id identifiers)
      (when id
        (multiple-value-bind (sym docs)
            (if (consp id)
                (values (car id) (cdr id))
                (values id nil))
          (push `(def!constant ,sym
                   ,(+ start (* step index))
                   ,@docs)
                results)))
      (incf index))
    `(progn
       ,@(nreverse results))))

;;; generalization of DEFCONSTANT to values which are the same not
;;; under EQL but under e.g. EQUAL or EQUALP
;;;
;;; DEFCONSTANT-EQX is to be used instead of DEFCONSTANT for values
;;; which are appropriately compared using the function given by the
;;; EQX argument instead of EQL.
;;;
;;; Note: Be careful when using this macro, since it's easy to
;;; unintentionally pessimize your code. A good time to use this macro
;;; is when the values defined will be fed into optimization
;;; transforms and never actually appear in the generated code; this
;;; is especially common when defining BYTE expressions. Unintentional
;;; pessimization can result when the values defined by this macro are
;;; actually used in generated code: because of the way that the
;;; dump/load system works, you'll typically get one copy of consed
;;; structure for each object file which contains code referring to
;;; the value, plus perhaps one more copy bound to the SYMBOL-VALUE of
;;; the constant. If you don't want that to happen, you should
;;; probably use DEFPARAMETER instead; or if you truly desperately
;;; need to avoid runtime indirection through a symbol, you might be
;;; able to do something with LOAD-TIME-VALUE or MAKE-LOAD-FORM.
(defmacro defconstant-eqx (symbol expr eqx &optional doc)
  `(def!constant ,symbol
     (%defconstant-eqx-value ',symbol ,expr ,eqx)
     ,@(when doc (list doc))))
(defun %defconstant-eqx-value (symbol expr eqx)
  (declare (type function eqx))
  (flet ((bummer (explanation)
           (error "~@<bad DEFCONSTANT-EQX ~S ~2I~_~S: ~2I~_~A ~S~:>"
                  symbol
                  expr
                  explanation
                  (symbol-value symbol))))
    (cond ((not (boundp symbol))
           expr)
          ((not (constantp symbol))
           (bummer "already bound as a non-constant"))
          ((not (funcall eqx (symbol-value symbol) expr))
           (bummer "already bound as a different constant value"))
          (t
           (symbol-value symbol)))))

;;; a helper function for various macros which expect clauses of a
;;; given length, etc.
;;;
;;; Return true if X is a proper list whose length is between MIN and
;;; MAX (inclusive).
(defun proper-list-of-length-p (x min &optional (max min))
  ;; FIXME: This implementation will hang on circular list
  ;; structure. Since this is an error-checking utility, i.e. its
  ;; job is to deal with screwed-up input, it'd be good style to fix
  ;; it so that it can deal with circular list structure.
  (cond ((minusp max) nil)
        ((null x) (zerop min))
        ((consp x)
         (and (plusp max)
              (proper-list-of-length-p (cdr x)
                                       (if (plusp (1- min))
                                           (1- min)
                                           0)
                                       (1- max))))
        (t nil)))

;;; Helpers for defining error-signalling NOP's for "not supported
;;; here" operations.
(defmacro define-unsupported-fun (name &optional
                                  (doc "Unsupported on this platform.")
                                  (control
                                   "~S is unsupported on this platform ~
                                    (OS, CPU, whatever)."
                                   controlp)
                                  arguments)
  `(defun ,name (&rest args)
    ,doc
    (declare (ignore args))
    (error 'unsupported-operator
     :format-control ,control
     :format-arguments (if ,controlp ',arguments (list ',name)))))
