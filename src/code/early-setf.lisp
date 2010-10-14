;;;; SETF and friends (except for stuff defined with COLLECT, which
;;;; comes later)
;;;;
;;;; Note: The expansions for SETF and friends sometimes create
;;;; needless LET-bindings of argument values. The compiler will
;;;; remove most of these spurious bindings, so SETF doesn't worry too
;;;; much about creating them.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;; The inverse for a generalized-variable reference function is stored in
;;; one of two ways:
;;;
;;; A SETF inverse property corresponds to the short form of DEFSETF. It is
;;; the name of a function takes the same args as the reference form, plus a
;;; new-value arg at the end.
;;;
;;; A SETF method expander is created by the long form of DEFSETF or
;;; by DEFINE-SETF-EXPANDER. It is a function that is called on the reference
;;; form and that produces five values: a list of temporary variables, a list
;;; of value forms, a list of the single store-value form, a storing function,
;;; and an accessing function.
(declaim (ftype (function (t &optional (or null sb!c::lexenv))) sb!xc:get-setf-expansion))
(defun sb!xc:get-setf-expansion (form &optional environment)
  #!+sb-doc
  "Return five values needed by the SETF machinery: a list of temporary
   variables, a list of values with which to fill them, a list of temporaries
   for the new values, the setting function, and the accessing function."
  (let (temp)
    (cond ((symbolp form)
           (multiple-value-bind (expansion expanded)
               (sb!xc:macroexpand-1 form environment)
             (if expanded
                 (sb!xc:get-setf-expansion expansion environment)
                 (let ((new-var (sb!xc:gensym "NEW")))
                   (values nil nil (list new-var)
                           `(setq ,form ,new-var) form)))))
          ;; Local functions inhibit global SETF methods.
          ((and environment
                (let ((name (car form)))
                  (dolist (x (sb!c::lexenv-funs environment))
                    (when (and (eq (car x) name)
                               (not (sb!c::defined-fun-p (cdr x))))
                      (return t)))))
           (expand-or-get-setf-inverse form environment))
          ((setq temp (info :setf :inverse (car form)))
           (get-setf-method-inverse form `(,temp) nil environment))
          ((setq temp (info :setf :expander (car form)))
           ;; KLUDGE: It may seem as though this should go through
           ;; *MACROEXPAND-HOOK*, but the ANSI spec seems fairly explicit
           ;; that *MACROEXPAND-HOOK* is a hook for MACROEXPAND-1, not
           ;; for macroexpansion in general. -- WHN 19991128
           (funcall temp
                    form
                    ;; As near as I can tell from the ANSI spec,
                    ;; macroexpanders have a right to expect an actual
                    ;; lexical environment, not just a NIL which is to
                    ;; be interpreted as a null lexical environment.
                    ;; -- WHN 19991128
                    (coerce-to-lexenv environment)))
          (t
           (expand-or-get-setf-inverse form environment)))))

;;; GET-SETF-METHOD existed in pre-ANSI Common Lisp, and various code inherited
;;; from CMU CL uses it repeatedly, so rather than rewrite a lot of code to not
;;; use it, we just define it in terms of ANSI's GET-SETF-EXPANSION (or
;;; actually, the cross-compiler version of that, i.e.
;;; SB!XC:GET-SETF-EXPANSION).
(declaim (ftype (function (t &optional (or null sb!c::lexenv))) get-setf-method))
(defun get-setf-method (form &optional environment)
  #!+sb-doc
  "This is a specialized-for-one-value version of GET-SETF-EXPANSION (and
a relic from pre-ANSI Common Lisp). Portable ANSI code should use
GET-SETF-EXPANSION directly."
  (multiple-value-bind (temps value-forms store-vars store-form access-form)
      (sb!xc:get-setf-expansion form environment)
    (when (cdr store-vars)
      (error "GET-SETF-METHOD used for a form with multiple store ~
              variables:~%  ~S"
             form))
    (values temps value-forms store-vars store-form access-form)))

;;; If a macro, expand one level and try again. If not, go for the
;;; SETF function.
(declaim (ftype (function (t (or null sb!c::lexenv)))
                expand-or-get-setf-inverse))
(defun expand-or-get-setf-inverse (form environment)
  (multiple-value-bind (expansion expanded)
      (sb!xc:macroexpand-1 form environment)
    (if expanded
        (sb!xc:get-setf-expansion expansion environment)
        (get-setf-method-inverse form
                                 `(funcall #'(setf ,(car form)))
                                 t
                                 environment))))

(defun get-setf-method-inverse (form inverse setf-fun environment)
  (let ((new-var (sb!xc:gensym "NEW"))
        (vars nil)
        (vals nil)
        (args nil))
    (dolist (x (reverse (cdr form)))
      (cond ((sb!xc:constantp x environment)
             (push x args))
            (t
             (let ((temp (gensym "TMP")))
               (push temp args)
               (push temp vars)
               (push x vals)))))
    (values vars
            vals
            (list new-var)
            (if setf-fun
                `(,@inverse ,new-var ,@args)
                `(,@inverse ,@args ,new-var))
            `(,(car form) ,@args))))

;;;; SETF itself

;;; Except for atoms, we always call GET-SETF-EXPANSION, since it has
;;; some non-trivial semantics. But when there is a setf inverse, and
;;; G-S-E uses it, then we return a call to the inverse, rather than
;;; returning a hairy LET form. This is probably important mainly as a
;;; convenience in allowing the use of SETF inverses without the full
;;; interpreter.
(defmacro-mundanely setf (&rest args &environment env)
  #!+sb-doc
  "Takes pairs of arguments like SETQ. The first is a place and the second
  is the value that is supposed to go into that place. Returns the last
  value. The place argument may be any of the access forms for which SETF
  knows a corresponding setting form."
  (let ((nargs (length args)))
    (cond
     ((= nargs 2)
      (let ((place (first args))
            (value-form (second args)))
        (if (atom place)
          `(setq ,place ,value-form)
          (multiple-value-bind (dummies vals newval setter getter)
              (sb!xc:get-setf-expansion place env)
            (declare (ignore getter))
            (let ((inverse (info :setf :inverse (car place))))
              (if (and inverse (eq inverse (car setter)))
                `(,inverse ,@(cdr place) ,value-form)
                `(let* (,@(mapcar #'list dummies vals))
                   (multiple-value-bind ,newval ,value-form
                     ,setter))))))))
     ((oddp nargs)
      (error "odd number of args to SETF"))
     (t
      (do ((a args (cddr a))
           (reversed-setfs nil))
          ((null a)
           `(progn ,@(nreverse reversed-setfs)))
        (push (list 'setf (car a) (cadr a)) reversed-setfs))))))

;;;; various SETF-related macros

(defmacro-mundanely shiftf (&whole form &rest args &environment env)
  #!+sb-doc
  "One or more SETF-style place expressions, followed by a single
   value expression. Evaluates all of the expressions in turn, then
   assigns the value of each expression to the place on its left,
   returning the value of the leftmost."
  (when (< (length args) 2)
    (error "~S called with too few arguments: ~S" 'shiftf form))
  (let (let*-bindings mv-bindings setters getters)
    (dolist (arg (butlast args))
      (multiple-value-bind (temps subforms store-vars setter getter)
          (sb!xc:get-setf-expansion arg env)
        (mapc (lambda (tmp form)
                (push `(,tmp ,form) let*-bindings))
              temps
              subforms)
        (push store-vars mv-bindings)
        (push setter setters)
        (push getter getters)))
    ;; Handle the last arg specially here. The getter is just the last
    ;; arg itself.
    (push (car (last args)) getters)

    ;; Reverse the collected lists so last bit looks nicer.
    (setf let*-bindings (nreverse let*-bindings)
          mv-bindings (nreverse mv-bindings)
          setters (nreverse setters)
          getters (nreverse getters))

    (labels ((thunk (mv-bindings getters)
               (if mv-bindings
                   `((multiple-value-bind
                           ,(car mv-bindings)
                         ,(car getters)
                       ,@(thunk (cdr mv-bindings) (cdr getters))))
                   `(,@setters))))
      `(let ,let*-bindings
        (multiple-value-bind ,(car mv-bindings)
            ,(car getters)
          ,@(thunk mv-bindings (cdr getters))
          (values ,@(car mv-bindings)))))))

(defmacro-mundanely push (obj place &environment env)
  #!+sb-doc
  "Takes an object and a location holding a list. Conses the object onto
  the list, returning the modified list. OBJ is evaluated before PLACE."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-method place env)
    (let ((g (gensym)))
      `(let* ((,g ,obj)
              ,@(mapcar #'list dummies vals)
              (,(car newval) (cons ,g ,getter)))
         ,setter))))

(defmacro-mundanely pushnew (obj place &rest keys
                             &key key test test-not &environment env)
  #!+sb-doc
  "Takes an object and a location holding a list. If the object is
  already in the list, does nothing; otherwise, conses the object onto
  the list. Returns the modified list. If there is a :TEST keyword, this
  is used for the comparison."
  (declare (ignore key test test-not))
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-method place env)
    (let ((g (gensym)))
      `(let* ((,g ,obj)
              ,@(mapcar #'list dummies vals)
              (,(car newval) (adjoin ,g ,getter ,@keys)))
         ,setter))))

(defmacro-mundanely pop (place &environment env)
  #!+sb-doc
  "The argument is a location holding a list. Pops one item off the front
  of the list and returns it."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-method place env)
    (do* ((d dummies (cdr d))
          (v vals (cdr v))
          (let-list nil))
         ((null d)
          (push (list (car newval) getter) let-list)
          `(let* ,(nreverse let-list)
             (prog1 (car ,(car newval))
               (setq ,(car newval) (cdr ,(car newval)))
               ,setter)))
      (push (list (car d) (car v)) let-list))))

(defmacro-mundanely remf (place indicator &environment env)
  #!+sb-doc
  "Place may be any place expression acceptable to SETF, and is expected
  to hold a property list or (). This list is destructively altered to
  remove the property specified by the indicator. Returns T if such a
  property was present, NIL if not."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-method place env)
    (do* ((d dummies (cdr d))
          (v vals (cdr v))
          (let-list nil)
          (ind-temp (gensym))
          (local1 (gensym))
          (local2 (gensym)))
         ((null d)
          ;; See ANSI 5.1.3 for why we do out-of-order evaluation
          (push (list ind-temp indicator) let-list)
          (push (list (car newval) getter) let-list)
          `(let* ,(nreverse let-list)
             (do ((,local1 ,(car newval) (cddr ,local1))
                  (,local2 nil ,local1))
                 ((atom ,local1) nil)
               (cond ((atom (cdr ,local1))
                      (error "Odd-length property list in REMF."))
                     ((eq (car ,local1) ,ind-temp)
                      (cond (,local2
                             (rplacd (cdr ,local2) (cddr ,local1))
                             (return t))
                            (t (setq ,(car newval) (cddr ,(car newval)))
                               ,setter
                               (return t))))))))
      (push (list (car d) (car v)) let-list))))

;;; we can't use DEFINE-MODIFY-MACRO because of ANSI 5.1.3
(defmacro-mundanely incf (place &optional (delta 1) &environment env)
  #!+sb-doc
  "The first argument is some location holding a number. This number is
  incremented by the second argument, DELTA, which defaults to 1."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-method place env)
    (let ((d (gensym)))
      `(let* (,@(mapcar #'list dummies vals)
              (,d ,delta)
              (,(car newval) (+ ,getter ,d)))
         ,setter))))

(defmacro-mundanely decf (place &optional (delta 1) &environment env)
  #!+sb-doc
  "The first argument is some location holding a number. This number is
  decremented by the second argument, DELTA, which defaults to 1."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-method place env)
    (let ((d (gensym)))
      `(let* (,@(mapcar #'list dummies vals)
              (,d ,delta)
              (,(car newval) (- ,getter ,d)))
         ,setter))))

;;;; DEFINE-MODIFY-MACRO stuff

(def!macro sb!xc:define-modify-macro (name lambda-list function &optional doc-string)
  #!+sb-doc
  "Creates a new read-modify-write macro like PUSH or INCF."
  (let ((other-args nil)
        (rest-arg nil)
        (env (make-symbol "ENV"))          ; To beautify resulting arglist.
        (reference (make-symbol "PLACE"))) ; Note that these will be nonexistent
                                           ;  in the final expansion anyway.
    ;; Parse out the variable names and &REST arg from the lambda list.
    (do ((ll lambda-list (cdr ll))
         (arg nil))
        ((null ll))
      (setq arg (car ll))
      (cond ((eq arg '&optional))
            ((eq arg '&rest)
             (if (symbolp (cadr ll))
               (setq rest-arg (cadr ll))
               (error "Non-symbol &REST argument in definition of ~S." name))
             (if (null (cddr ll))
               (return nil)
               (error "Illegal stuff after &REST argument.")))
            ((memq arg '(&key &allow-other-keys &aux))
             (error "~S not allowed in DEFINE-MODIFY-MACRO lambda list." arg))
            ((symbolp arg)
             (push arg other-args))
            ((and (listp arg) (symbolp (car arg)))
             (push (car arg) other-args))
            (t (error "Illegal stuff in lambda list."))))
    (setq other-args (nreverse other-args))
    `(#-sb-xc-host sb!xc:defmacro
      #+sb-xc-host defmacro-mundanely
         ,name (,reference ,@lambda-list &environment ,env)
       ,doc-string
       (multiple-value-bind (dummies vals newval setter getter)
           (get-setf-method ,reference ,env)
         (do ((d dummies (cdr d))
              (v vals (cdr v))
              (let-list nil (cons (list (car d) (car v)) let-list)))
             ((null d)
              (push (list (car newval)
                          ,(if rest-arg
                             `(list* ',function getter ,@other-args ,rest-arg)
                             `(list ',function getter ,@other-args)))
                    let-list)
              `(let* ,(nreverse let-list)
                 ,setter)))))))

;;;; DEFSETF

(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
  ;;; Assign SETF macro information for NAME, making all appropriate checks.
  (defun assign-setf-macro (name expander inverse doc)
    (with-single-package-locked-error
        (:symbol name "defining a setf-expander for ~A"))
    (cond ((gethash name sb!c:*setf-assumed-fboundp*)
           (warn
            "defining setf macro for ~S when ~S was previously ~
             treated as a function"
            name
            `(setf ,name)))
          ((not (fboundp `(setf ,name)))
           ;; All is well, we don't need any warnings.
           (values))
          ((not (eq (symbol-package name) (symbol-package 'aref)))
           (style-warn "defining setf macro for ~S when ~S is fbound"
                       name `(setf ,name))))
    (remhash name sb!c:*setf-assumed-fboundp*)
    ;; FIXME: It's probably possible to join these checks into one form which
    ;; is appropriate both on the cross-compilation host and on the target.
    (when (or inverse (info :setf :inverse name))
      (setf (info :setf :inverse name) inverse))
    (when (or expander (info :setf :expander name))
      (setf (info :setf :expander name) expander))
    (when doc
      (setf (fdocumentation name 'setf) doc))
    name))

(def!macro sb!xc:defsetf (access-fn &rest rest)
  #!+sb-doc
  "Associates a SETF update function or macro with the specified access
  function or macro. The format is complex. See the manual for details."
  (cond ((and (not (listp (car rest))) (symbolp (car rest)))
         `(eval-when (:load-toplevel :compile-toplevel :execute)
            (assign-setf-macro ',access-fn
                               nil
                               ',(car rest)
                                ,(when (and (car rest) (stringp (cadr rest)))
                                   `',(cadr rest)))))
        ((and (cdr rest) (listp (cadr rest)))
         (destructuring-bind
             (lambda-list (&rest store-variables) &body body)
             rest
           (with-unique-names (whole access-form environment)
             (multiple-value-bind (body local-decs doc)
                 (parse-defmacro `(,lambda-list ,@store-variables)
                                 whole body access-fn 'defsetf
                                 :environment environment
                                 :anonymousp t)
               `(eval-when (:compile-toplevel :load-toplevel :execute)
                  (assign-setf-macro
                   ',access-fn
                   (lambda (,access-form ,environment)
                     ,@local-decs
                     (%defsetf ,access-form ,(length store-variables)
                               (lambda (,whole)
                                 ,body)))
                   nil
                   ',doc))))))
        (t
         (error "ill-formed DEFSETF for ~S" access-fn))))

(defun %defsetf (orig-access-form num-store-vars expander)
  (declare (type function expander))
  (let (subforms
        subform-vars
        subform-exprs
        store-vars)
    (dolist (subform (cdr orig-access-form))
      (if (constantp subform)
        (push subform subforms)
        (let ((var (gensym)))
          (push var subforms)
          (push var subform-vars)
          (push subform subform-exprs))))
    (dotimes (i num-store-vars)
      (push (gensym) store-vars))
    (let ((r-subforms (nreverse subforms))
          (r-subform-vars (nreverse subform-vars))
          (r-subform-exprs (nreverse subform-exprs))
          (r-store-vars (nreverse store-vars)))
      (values r-subform-vars
              r-subform-exprs
              r-store-vars
              (funcall expander (cons r-subforms r-store-vars))
              `(,(car orig-access-form) ,@r-subforms)))))

;;;; DEFMACRO DEFINE-SETF-EXPANDER and various DEFINE-SETF-EXPANDERs

;;; DEFINE-SETF-EXPANDER is a lot like DEFMACRO.
(def!macro sb!xc:define-setf-expander (access-fn lambda-list &body body)
  #!+sb-doc
  "Syntax like DEFMACRO, but creates a setf expander function. The body
  of the definition must be a form that returns five appropriate values."
  (unless (symbolp access-fn)
    (error "~S access-function name ~S is not a symbol."
           'sb!xc:define-setf-expander access-fn))
  (with-unique-names (whole environment)
    (multiple-value-bind (body local-decs doc)
        (parse-defmacro lambda-list whole body access-fn
                        'sb!xc:define-setf-expander
                        :environment environment)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (assign-setf-macro ',access-fn
                            (lambda (,whole ,environment)
                              ,@local-decs
                              ,body)
                            nil
                            ',doc)))))

(sb!xc:define-setf-expander getf (place prop
                                  &optional default
                                  &environment env)
  (declare (type sb!c::lexenv env))
  (multiple-value-bind (temps values stores set get)
      (get-setf-method place env)
    (let ((newval (gensym))
          (ptemp (gensym))
          (def-temp (if default (gensym))))
      (values `(,@temps ,ptemp ,@(if default `(,def-temp)))
              `(,@values ,prop ,@(if default `(,default)))
              `(,newval)
              `(let ((,(car stores) (%putf ,get ,ptemp ,newval)))
                 ,set
                 ,newval)
              `(getf ,get ,ptemp ,@(if default `(,def-temp)))))))

(sb!xc:define-setf-expander get (symbol prop &optional default)
  (let ((symbol-temp (gensym))
        (prop-temp (gensym))
        (def-temp (gensym))
        (newval (gensym)))
    (values `(,symbol-temp ,prop-temp ,@(if default `(,def-temp)))
            `(,symbol ,prop ,@(if default `(,default)))
            (list newval)
            `(%put ,symbol-temp ,prop-temp ,newval)
            `(get ,symbol-temp ,prop-temp ,@(if default `(,def-temp))))))

(sb!xc:define-setf-expander gethash (key hashtable &optional default)
  (let ((key-temp (gensym))
        (hashtable-temp (gensym))
        (default-temp (gensym))
        (new-value-temp (gensym)))
    (values
     `(,key-temp ,hashtable-temp ,@(if default `(,default-temp)))
     `(,key ,hashtable ,@(if default `(,default)))
     `(,new-value-temp)
     `(%puthash ,key-temp ,hashtable-temp ,new-value-temp)
     `(gethash ,key-temp ,hashtable-temp ,@(if default `(,default-temp))))))

(sb!xc:define-setf-expander logbitp (index int &environment env)
  (declare (type sb!c::lexenv env))
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-method int env)
    (let ((ind (gensym))
          (store (gensym))
          (stemp (first stores)))
      (values `(,ind ,@temps)
              `(,index
                ,@vals)
              (list store)
              `(let ((,stemp
                      (dpb (if ,store 1 0) (byte 1 ,ind) ,access-form)))
                 ,store-form
                 ,store)
              `(logbitp ,ind ,access-form)))))

;;; CMU CL had a comment here that:
;;;   Evil hack invented by the gnomes of Vassar Street (though not as evil as
;;;   it used to be.)  The function arg must be constant, and is converted to
;;;   an APPLY of the SETF function, which ought to exist.
;;;
;;; It may not be clear (wasn't to me..) that this is a standard thing, but See
;;; "5.1.2.5 APPLY Forms as Places" in the ANSI spec. I haven't actually
;;; verified that this code has any correspondence to that code, but at least
;;; ANSI has some place for SETF APPLY. -- WHN 19990604
(sb!xc:define-setf-expander apply (functionoid &rest args)
  (unless (and (listp functionoid)
               (= (length functionoid) 2)
               (eq (first functionoid) 'function)
               (symbolp (second functionoid)))
    (error "SETF of APPLY is only defined for function args like #'SYMBOL."))
  (let ((function (second functionoid))
        (new-var (gensym))
        (vars (make-gensym-list (length args))))
    (values vars args (list new-var)
            `(apply #'(setf ,function) ,new-var ,@vars)
            `(apply #',function ,@vars))))

;;; Special-case a BYTE bytespec so that the compiler can recognize it.
(sb!xc:define-setf-expander ldb (bytespec place &environment env)
  #!+sb-doc
  "The first argument is a byte specifier. The second is any place form
  acceptable to SETF. Replace the specified byte of the number in this
  place with bits from the low-order end of the new value."
  (declare (type sb!c::lexenv env))
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-method place env)
    (if (and (consp bytespec) (eq (car bytespec) 'byte))
        (let ((n-size (gensym))
              (n-pos (gensym))
              (n-new (gensym)))
          (values (list* n-size n-pos dummies)
                  (list* (second bytespec) (third bytespec) vals)
                  (list n-new)
                  `(let ((,(car newval) (dpb ,n-new (byte ,n-size ,n-pos)
                                             ,getter)))
                     ,setter
                     ,n-new)
                  `(ldb (byte ,n-size ,n-pos) ,getter)))
        (let ((btemp (gensym))
              (gnuval (gensym)))
          (values (cons btemp dummies)
                  (cons bytespec vals)
                  (list gnuval)
                  `(let ((,(car newval) (dpb ,gnuval ,btemp ,getter)))
                     ,setter
                     ,gnuval)
                  `(ldb ,btemp ,getter))))))

(sb!xc:define-setf-expander mask-field (bytespec place &environment env)
  #!+sb-doc
  "The first argument is a byte specifier. The second is any place form
  acceptable to SETF. Replaces the specified byte of the number in this place
  with bits from the corresponding position in the new value."
  (declare (type sb!c::lexenv env))
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-method place env)
    (let ((btemp (gensym))
          (gnuval (gensym)))
      (values (cons btemp dummies)
              (cons bytespec vals)
              (list gnuval)
              `(let ((,(car newval) (deposit-field ,gnuval ,btemp ,getter)))
                 ,setter
                 ,gnuval)
              `(mask-field ,btemp ,getter)))))

(sb!xc:define-setf-expander the (type place &environment env)
  (declare (type sb!c::lexenv env))
  (multiple-value-bind (temps subforms store-vars setter getter)
      (sb!xc:get-setf-expansion place env)
    (values temps subforms store-vars
            `(multiple-value-bind ,store-vars
                 (the ,type (values ,@store-vars))
               ,setter)
            `(the ,type ,getter))))
