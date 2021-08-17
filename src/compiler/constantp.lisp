;;;; implementation of CONSTANTP, needs both INFO and IR1-ATTRIBUTES

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

(defvar *special-constant-variables* nil)

(defun %constantp (form environment envp)
  ;; Pick off quasiquote prior to macroexpansion.
  (when (typep form '(cons (eql quasiquote) (cons t null)))
    (return-from %constantp
      (constant-quasiquote-form-p (cadr form) environment envp)))
  (let ((form (if envp
                  (handler-case
                      (%macroexpand form environment)
                    (error ()
                      (return-from %constantp)))
                  form)))
    (typecase form
      ;; This INFO test catches KEYWORDs as well as explicitly
      ;; DEFCONSTANT symbols.
      (symbol
       (or (eq (info :variable :kind form) :constant)
           (constant-special-variable-p form)))
      (list
       (let ((answer (constant-special-form-p form environment envp)))
         (if (eq answer :maybe)
             (values (constant-function-call-p form environment envp))
             answer)))
      (t t))))

(defun constant-quasiquote-form-p (expr environment envp)
  ;; This is an utter cinch because we haven't macroexpanded.
  ;; Parse just enough to recognize (DEFTYPE <T2> () (<T1> ,THING)) etc.
  (named-let recurse ((expr expr))
    (cond ((atom expr)
           (cond ((comma-p expr)
                  (%constantp (comma-expr expr) environment envp))
                 ((simple-vector-p expr) (every #'recurse expr))
                 (t)))
          ((eq (car expr) 'quasiquote) nil) ; give up
          (t (and (recurse (car expr)) (recurse (cdr expr)))))))

(defun %constant-form-value (form environment envp)
  (let ((form (if (or envp
                      (typep form '(cons (eql quasiquote) (cons t null))))
                  (%macroexpand form environment)
                  form)))
    (typecase form
      (symbol
       (symbol-value form))
      (list
       (multiple-value-bind (specialp value)
           (constant-special-form-value form environment envp)
         (if specialp value (constant-function-call-value
                             form environment envp))))
      (t
       form))))

(defun constant-special-variable-p (name)
  (and (member name *special-constant-variables*) t))

;;; FIXME: It would be nice to deal with inline functions
;;; too.
(defun constant-function-call-p (form environment envp)
  (let ((name (car form)))
    (if (and (legal-fun-name-p name)
             (eq :function (info :function :kind name))
             (let ((info (info :function :info name)))
               (and info (ir1-attributep (fun-info-attributes info)
                                         foldable)))
             (and (every (lambda (arg)
                           (%constantp arg environment envp))
                         (cdr form))))
        ;; Even though the function may be marked as foldable
        ;; the call may still signal an error -- eg: (CAR 1).
        (handler-case
            (values t (constant-function-call-value form environment envp))
          (error ()
            (values nil nil)))
        (values nil nil))))

(defun constant-function-call-value (form environment envp)
  (apply (fdefinition (car form))
         (mapcar (lambda (arg)
                   (%constant-form-value arg environment envp))
                 (cdr form))))

;;;; NOTE!!!
;;;;
;;;; If you add new special forms, check that they do not
;;;; alter the logic of existing ones: eg, currently
;;;; CONSTANT-FORM-VALUE directly evaluates the last expression
;;;; of a PROGN, as no assignment is allowed. If you extend
;;;; analysis to assignments then other forms must take this
;;;; into account.

(eval-when (:compile-toplevel :execute)
(defparameter *!special-form-constantp-defs* (make-array 20 :fill-pointer 0)))

(defmacro !defconstantp (operator lambda-list &key test eval)
  (let ((args (make-symbol "ARGS")))
    (flet
        ;; FIXME: DESTRUCTURING-BIND should have the option to expand this way.
        ;; It would be useful for DEFINE-SOURCE-TRANSFORM as well.
        ((binding-maker (input on-error)
           (multiple-value-bind (llks req opt rest key aux env whole)
               (parse-lambda-list
                lambda-list
                :accept (lambda-list-keyword-mask '(&whole &optional &rest &body)))
             (declare (ignore llks key aux env))
             (aver (every (lambda (x) (and (symbolp x) x)) (append req opt rest)))
             (flet ((bind (var pred enforce-end)
                      `(,(car var)
                        ,(if enforce-end
                             `(if (and (,pred ,args) (not (cdr ,args)))
                                  (car ,args)
                                  ,on-error)
                             `(if (,pred ,args) (pop ,args) ,on-error)))))
               `((,args ,input)
                 ,@(when whole
                     ;; If both &WHOLE and &REST are present, the &WHOLE var
                     ;; must be a list, although we don't know that just yet.
                     ;; It will be verified when the &REST arg is bound.
                     `((,(car whole) ,(if rest `(truly-the list ,args) args))))
                 ,@(maplist (lambda (x)
                              (bind x (if (cdr x) 'listp 'consp)
                                    (and (not (cdr x)) (not opt) (not rest))))
                            req)
                 ,@(maplist (lambda (x) (bind x 'listp (and (not (cdr x)) (not rest))))
                            opt)
                 ,@(when rest
                     `((,(car rest)
                        (if (proper-list-p ,args)
                            (truly-the list ,args) ; to open-code EVERY #'P on &REST arg
                            ,on-error)))))))))
      `(eval-when (:compile-toplevel :execute)
         (vector-push-extend ',(list* operator test eval
                                      (binding-maker 'args '(go fail)))
                             *!special-form-constantp-defs*)))))

;;; NOTE: special forms are tested in the order as written,
;;; so there is some benefit to listing important ones earliest.

(!defconstantp quote (value)
   :test t
   :eval value)

(!defconstantp if (test then &optional else)
   :test
   (and (constantp* test)
        (constantp* (if (constant-form-value* test)
                        then
                        else)))
   :eval (if (constant-form-value* test)
             (constant-form-value* then)
             (constant-form-value* else)))

;; FIXME: isn't it sufficient for non-final forms to be flushable and/or
;; maybe satisfy some other conditions? e.g. (PROGN (LIST 1) 'FOO) is constant.
(!defconstantp progn (&body forms)
   :test (every #'constantp* forms)
   :eval (constant-form-value* (car (last forms))))

(!defconstantp the (type form)
   ;; We can't call TYPEP because the form might be (THE (FUNCTION (t) t) #<fn>)
   ;; which is valid for declaration but not for discrimination.
   ;; CTYPEP handles unknown types and SATISFIES with non-foldable functions.
   :test (and (constantp* form)
              (let ((parsed (careful-specifier-type type)))
                (and parsed
                     (ctypep (constant-form-value* form) parsed))))
   :eval (constant-form-value* form))

(!defconstantp unwind-protect (&whole subforms protected-form &body cleanup-forms)
   :test (every #'constantp* subforms)
   :eval (constant-form-value* protected-form))

(!defconstantp block (name &body forms)
   ;; We currently fail to detect cases like
   ;;
   ;; (BLOCK FOO
   ;;   ...CONSTANT-FORMS...
   ;;   (RETURN-FROM FOO CONSTANT-VALUE)
   ;;   ...ANYTHING...)
   ;;
   ;; Right now RETURN-FROM kills the constantness unequivocally.
   :test (and (symbolp name)
              (every #'constantp* forms))
   :eval (constant-form-value* (car (last forms))))

(!defconstantp multiple-value-prog1 (&whole subforms first-form &body forms)
   :test (every #'constantp* subforms)
   :eval (constant-form-value* first-form))

(!defconstantp progv (symbols values &body forms)
   :test (and (constantp* symbols)
              (constantp* values)
              (let* ((symbols (constant-form-value* symbols))
                     (values (constant-form-value* values)))
                (and (proper-list-p values)
                     (proper-list-p symbols)
                     (>= (length values)
                         (length symbols))
                     (loop for symbol in symbols
                           for value in values
                           always (and (symbolp symbol)
                                       (not (constantp symbol))
                                       (memq (info :variable :kind symbol)
                                             '(:unknown :special))
                                       (multiple-value-bind (type declaredp)
                                           (info :variable :type symbol)
                                         (or (not declaredp)
                                             (ctypep value type)))))
                     (let ((*special-constant-variables*
                             (append symbols *special-constant-variables*)))
                       (progv symbols values
                         (and forms
                              (every #'constantp* forms)))))))
   :eval (progv
             (constant-form-value* symbols)
             (constant-form-value* values)
           (constant-form-value* (car (last forms)))))

(!defconstantp with-source-form (source-form form)
   :test (constantp* form)
   :eval (constant-form-value* form))

(!defconstantp the* (options form)
   :test (destructuring-bind (type &key use-annotations
                              &allow-other-keys)
             options
           (declare (ignore type))
           (and use-annotations
                (constantp* form)))
   :eval (constant-form-value* form))

;;;

(macrolet
    ((expand-cases (expr-selector default-clause)
       `(flet ((constantp* (x) (%constantp x environment envp))
               (constant-form-value* (x) (%constant-form-value x environment envp)))
          (declare (optimize speed) (ignorable #'constantp*)
                   (ftype (sfunction (t) t) constantp* constant-form-value*))
          (let ((args (cdr (truly-the list form))))
            (case (car form)
              ,@(map 'list
                     (lambda (spec &aux (bindings (cdddr spec)))
                       `(,(first spec)
                         (let* ,bindings
                           (declare (ignorable ,@(mapcar #'car bindings)))
                           ,(nth expr-selector spec))))
                     *!special-form-constantp-defs*)
              (t
               ,default-clause))))))

  (defun constant-special-form-p (form environment envp)
    (let (result)
      (tagbody (setq result (expand-cases 1 :maybe)) fail)
      result))

  (defun constant-special-form-value (form environment envp)
    (let ((result))
      (tagbody
         (setq result (expand-cases 2 (return-from constant-special-form-value
                                        (values nil nil))))
         (return-from constant-special-form-value (values t result))
       fail))
    ;; Mutatation of FORM could cause failure. It's user error, not a bug.
    (error "CONSTANT-FORM-VALUE called with invalid expression ~S" form)))
