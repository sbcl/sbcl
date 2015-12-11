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
      (multiple-value-bind (code decls) (parse-body decls-and-code nil)
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

;; Define "exchanged subtract" So that DECF on a symbol requires no LET binding:
;;  (DECF I (EXPR)) -> (SETQ I (XSUBTRACT (EXPR) I))
;; which meets the CLHS 5.1.3 requirement to eval (EXPR) prior to reading
;; the old value of I. Formerly in 'setf' but too late to avoid full calls.
(declaim (inline xsubtract))
(defun xsubtract (a b) (- b a))

;;;; GENSYM tricks

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
  (declare (notinline every)) ; because we can't inline ALPHA-CHAR-P
  `(let ,(mapcar (lambda (symbol)
                   (let* ((symbol-name (symbol-name symbol))
                          (stem (if (every #'alpha-char-p symbol-name)
                                    symbol-name
                                    (concatenate 'string symbol-name "-"))))
                     `(,symbol (sb!xc:gensym ,stem))))
                 symbols)
     ,@body))

;;; Return a list of N gensyms. (This is a common suboperation in
;;; macros and other code-manipulating code.)
(declaim (ftype (function (unsigned-byte &optional t) (values list &optional))
                make-gensym-list))
(defun make-gensym-list (n &optional name)
  (let ((arg (if name (string name) "G")))
    (loop repeat n collect (sb!xc:gensym arg))))

;;;; miscellany

;;; Lots of code wants to get to the KEYWORD package or the
;;; COMMON-LISP package without a lot of fuss, so we cache them in
;;; variables on the host, or use L-T-V forms on the target.
(macrolet ((def-it (sym expr)
             #+sb-xc-host
             `(progn (declaim (type package ,sym))
                     (defglobal ,sym ,expr))
             #-sb-xc-host
             ;; We don't need to declaim the type. FIND-PACKAGE
             ;; returns a package, and L-T-V propagates types.
             ;; It's ugly how it achieves that, but it's a separate concern.
             `(define-symbol-macro ,sym (load-time-value ,expr t))))
  (def-it *cl-package* (find-package "COMMON-LISP"))
  (def-it *keyword-package* (find-package "KEYWORD")))

;;; Concatenate together the names of some strings and symbols,
;;; producing a symbol in the current package.
(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
  (defun symbolicate (&rest things)
    (declare (dynamic-extent things))
    (values
     (intern
      (if (singleton-p things)
          (string (first things))
          (let* ((length (reduce #'+ things
                                 :key (lambda (x) (length (string x)))))
                 (name (make-array length :element-type 'character))
                 (index 0))
            (dolist (thing things name)
              (let ((x (string thing)))
                (replace name x :start1 index)
                (incf index (length x))))))))))

(defun gensymify (x)
  (if (symbolp x)
      (sb!xc:gensym (symbol-name x))
      (sb!xc:gensym)))

;;; like SYMBOLICATE, but producing keywords
(defun keywordicate (&rest things)
  (let ((*package* *keyword-package*))
    (apply #'symbolicate things)))

;;; Access *PACKAGE* in a way which lets us recover when someone has
;;; done something silly like (SETF *PACKAGE* :CL-USER) in unsafe code.
;;; (Such an assignment is undefined behavior, so it's sort of reasonable for
;;; it to cause the system to go totally insane afterwards, but it's a
;;; fairly easy mistake to make, so let's try to recover gracefully instead.)
;;; This function is called while compiling this file because DO-ANONYMOUS
;;; is a delayed-def!macro, the constructor for which calls SANE-PACKAGE.
(eval-when (:load-toplevel :execute #+sb-xc-host :compile-toplevel)
(defun sane-package ()
  ;; Perhaps it's possible for *PACKAGE* to be set to a non-package in some
  ;; host Lisp, but in SBCL it isn't, and the PACKAGEP test below would be
  ;; elided unless forced to be NOTINLINE.
  (declare (notinline packagep))
  (let* ((maybe-package *package*)
         (packagep (packagep maybe-package)))
    ;; And if we don't also always check for deleted packages - as was true
    ;; when the "#+sb-xc-host" reader condition was absent - then half of the
    ;; COND becomes unreachable, making this function merely return *PACKAGE*
    ;; in the cross-compiler, producing a code deletion note.
    (cond ((and packagep
                ;; For good measure, we also catch the problem of
                ;; *PACKAGE* being bound to a deleted package.
                ;; Technically, this is not undefined behavior in itself,
                ;; but it will immediately lead to undefined to behavior,
                ;; since almost any operation on a deleted package is
                ;; undefined.
                (package-%name maybe-package))
           maybe-package)
          (t
           ;; We're in the undefined behavior zone. First, munge the
           ;; system back into a defined state.
           (let ((really-package
                  (load-time-value (find-package :cl-user) t)))
             (setf *package* really-package)
             ;; Then complain.
             (error 'simple-type-error
                    :datum maybe-package
                    :expected-type '(and package (satisfies package-name))
                    :format-control
                    "~@<~S can't be a ~A: ~2I~_It has been reset to ~S.~:>"
                    :format-arguments (list '*package*
                                            (if packagep
                                                "deleted package"
                                                (type-of maybe-package))
                                            really-package))))))))

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

;;; Compile a version of BODY for all TYPES, and dispatch to the
;;; correct one based on the value of VAR. This was originally used
;;; only for strings, hence the name. Renaming it to something more
;;; generic might not be a bad idea.
(def!macro string-dispatch ((&rest types) var &body body)
  (let ((fun (sb!xc:gensym "STRING-DISPATCH-FUN")))
    `(flet ((,fun (,var)
              ,@body))
       (declare (inline ,fun))
       (etypecase ,var
         ,@(loop for type in types
                 ;; TRULY-THE allows transforms to take advantage of the type
                 ;; information without need for constraint propagation.
                 collect `(,type (,fun (truly-the ,type ,var))))))))

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
(defmacro defconstant-eqx (symbol expr eqx &optional doc)
  `(def!constant ,symbol
     (%defconstant-eqx-value ',symbol ,expr ,eqx)
     ,@(when doc (list doc))))

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

(defun proper-list-p (x)
  (unless (consp x)
    (return-from proper-list-p (null x)))
  (let ((rabbit (cdr x))
        (turtle x))
    (flet ((pop-rabbit ()
             (when (eql rabbit turtle) ; circular
               (return-from proper-list-p nil))
             (when (atom rabbit)
               (return-from proper-list-p (null rabbit)))
             (pop rabbit)))
      (loop (pop-rabbit)
            (pop-rabbit)
            (pop turtle)))))

;;; Helpers for defining error-signalling NOP's for "not supported
;;; here" operations.
(defmacro define-unsupported-fun (name &optional
                                  (doc "Unsupported on this platform.")
                                  (control
                                   "~S is unsupported on this platform ~
                                    (OS, CPU, whatever)."
                                   controlp)
                                  arguments)
  (declare (ignorable doc))
  `(defun ,name (&rest args)
     #!+sb-doc
     ,doc
     (declare (ignore args))
     (error 'unsupported-operator
            :format-control ,control
            :format-arguments (if ,controlp ',arguments (list ',name)))))

;;; This is like DO, except it has no implicit NIL block.
(def!macro do-anonymous (varlist endlist &rest body)
  (frob-do-body varlist endlist body 'let 'psetq 'do-anonymous (gensym)))

;;; Anaphoric macros
(defmacro awhen (test &body body)
  `(let ((it ,test))
     (when it ,@body)))

(defmacro acond (&rest clauses)
  (if (null clauses)
      `()
      (destructuring-bind ((test &body body) &rest rest) clauses
        (let ((it (copy-symbol 'it)))
          `(let ((,it ,test))
             (if ,it
                 ;; Just like COND - no body means return the tested value.
                 ,(if body
                      `(let ((it ,it)) (declare (ignorable it)) ,@body)
                      it)
                 (acond ,@rest)))))))
