;;;; the PARSE-DEFMACRO function and related code

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;; variables for accumulating the results of parsing a DEFMACRO. (Declarations
;;; in DEFMACRO are the reason this isn't as easy as it sounds.)
(defvar *arg-tests*) ; tests that do argument counting at expansion time
(declaim (type list *arg-tests*))
(defvar *system-lets*) ; LET bindings done to allow lambda-list parsing
(declaim (type list *system-lets*))
(defvar *user-lets*) ; LET bindings that the user has explicitly supplied
(declaim (type list *user-lets*))
(defvar *env-var*) ; &ENVIRONMENT variable name

;; the default default for unsupplied &OPTIONAL and &KEY args
(defvar *default-default*)

;;; temps that we introduce and might not reference
(defvar *ignorable-vars*)
(declaim (type list *ignorable-vars*))

;;; Return, as multiple values, a body, possibly a DECLARE form to put
;;; where this code is inserted, the documentation for the parsed
;;; body, and bounds on the number of arguments.
(defun parse-defmacro (lambda-list whole-var body name context
                       &key
                       (anonymousp nil)
                       (doc-string-allowed t)
                       ((:environment env-arg-name))
                       ((:default-default *default-default*))
                       (error-fun 'error)
                       (wrap-block t))
  (unless (listp lambda-list)
    (bad-type lambda-list 'list "~S lambda-list is not a list: ~S"
              context lambda-list))
  (multiple-value-bind (forms declarations documentation)
      (parse-body body :doc-string-allowed doc-string-allowed)
    (let ((*arg-tests* ())
          (*user-lets* ())
          (*system-lets* ())
          (*ignorable-vars* ())
          (*env-var* nil))
      (multiple-value-bind (env-arg-used minimum maximum)
          (parse-defmacro-lambda-list lambda-list whole-var name context
                                      :error-fun error-fun
                                      :anonymousp anonymousp)
        (values `(let* (,@(nreverse *system-lets*))
                   #-sb-xc-host
                   (declare (muffle-conditions sb!ext:code-deletion-note))
                   ,@(when *ignorable-vars*
                       `((declare (ignorable ,@*ignorable-vars*))))
                   ,@*arg-tests*
                   (let* (,@(when env-arg-used
                            `((,*env-var* ,env-arg-name)))
                          ,@(nreverse *user-lets*))
                     ,@declarations
                     ,@(if wrap-block
                           `((block ,(fun-name-block-name name)
                               ,@forms))
                           forms)))
                `(,@(when (and env-arg-name (not env-arg-used))
                      `((declare (ignore ,env-arg-name)))))
                documentation
                minimum
                maximum)))))

(defun parse-defmacro-lambda-list (possibly-dotted-lambda-list
                                   whole-var
                                   name
                                   context
                                   &key
                                   error-fun
                                   anonymousp
                                   env-illegal
                                   sublist)
  (let* (;; PATH is a sort of pointer into the part of the lambda list we're
         ;; considering at this point in the code. PATH-0 is the root of the
         ;; lambda list, which is the initial value of PATH.
         (path-0 (if (or anonymousp sublist) whole-var `(cdr ,whole-var)))
         (path path-0) ; will change below
         (compiler-macro-whole (gensym "CMACRO-&WHOLE"))
         (now-processing :required)
         (maximum 0)
         (minimum 0)
         (keys ())
         (key-seen nil)
         (aux-seen nil)
         (optional-seen nil)
         ;; ANSI specifies that dotted lists are "treated exactly as if the
         ;; parameter name that ends the list had appeared preceded by &REST."
         ;; We force this behavior by transforming dotted lists into ordinary
         ;; lists with explicit &REST elements.
         (lambda-list (do ((in-pdll possibly-dotted-lambda-list (cdr in-pdll))
                           (reversed-result nil))
                          ((atom in-pdll)
                           (nreverse (if in-pdll
                                         (list* in-pdll '&rest reversed-result)
                                         reversed-result)))
                        (push (car in-pdll) reversed-result)))
         rest-name restp allow-other-keys-p env-arg-used)
    (when (member '&whole (rest lambda-list))
      (error "&WHOLE may only appear first in ~S lambda-list." context))
    ;; Special case compiler-macros: if car of the form is FUNCALL,
    ;; skip over it for destructuring, pretending cdr of the form is
    ;; the actual form. Save original for &WHOLE.
    (when (and (not sublist) (eq context 'define-compiler-macro))
      (push-let-binding compiler-macro-whole whole-var :system t)
      (push compiler-macro-whole *ignorable-vars*)
      (push-let-binding whole-var whole-var
                        :system t
                        :when `(not (eq 'funcall (car ,whole-var)))
                        ;; Do we need to SETF too?
                        :else `(setf ,whole-var (cdr ,whole-var))))
    (do ((rest-of-lambda-list lambda-list (cdr rest-of-lambda-list)))
        ((null rest-of-lambda-list))
      (macrolet ((process-sublist (var kind path)
                   (once-only ((var var))
                     `(if (listp ,var)
                          (let ((sublist-name (gensym ,kind)))
                            (push-sublist-binding sublist-name ,path ,var
                                                  name context error-fun)
                            (parse-defmacro-lambda-list ,var sublist-name name
                                                        context
                                                        :error-fun error-fun
                                                        :sublist t))
                          (push-let-binding ,var ,path))))
                 (normalize-singleton (var)
                   `(when (null (cdr ,var))
                     (setf (cdr ,var) (list *default-default*)))))
        (let ((var (car rest-of-lambda-list)))
          (typecase var
            (list
             (case now-processing
               ((:required)
                (when restp
                  (defmacro-error (format nil "required argument after ~A"
                                          restp)
                      context name))
                (when (process-sublist var "REQUIRED-" `(car ,path))
                  ;; Note &ENVIRONMENT from DEFSETF sublist
                  (aver (eq context 'defsetf))
                  (setf env-arg-used t))
                (setq path `(cdr ,path)
                      minimum (1+ minimum)
                      maximum (1+ maximum)))
               ((:optionals)
                (normalize-singleton var)
                (destructuring-bind
                      (varname &optional default-form suppliedp-name)
                    var
                  (push-optional-binding varname default-form suppliedp-name
                                         :is-supplied-p `(not (null ,path))
                                         :path `(car ,path)
                                         :name name
                                         :context context
                                         :error-fun error-fun))
                (setq path `(cdr ,path)
                      maximum (1+ maximum)))
               ((:keywords)
                (normalize-singleton var)
                (let* ((keyword-given (consp (car var)))
                       (variable (if keyword-given
                                     (cadar var)
                                     (car var)))
                       (keyword (if keyword-given
                                    (caar var)
                                    (keywordicate variable)))
                       (default-form (cadr var))
                       (suppliedp-name (caddr var)))
                  (push-optional-binding variable default-form suppliedp-name
                                         :is-supplied-p
                                         `(keyword-supplied-p ',keyword
                                                              ,rest-name)
                                         :path
                                         `(lookup-keyword ',keyword ,rest-name)
                                         :name name
                                         :context context
                                         :error-fun error-fun)
                  (push keyword keys)))
               ((:auxs)
                (push-let-binding (car var) (cadr var)))))
            ((and symbol (not (eql nil)))
             (case var
               (&whole
                (cond ((cdr rest-of-lambda-list)
                       (pop rest-of-lambda-list)
                       (process-sublist (car rest-of-lambda-list)
                                        "WHOLE-LIST-"
                                        (if (eq 'define-compiler-macro context)
                                            compiler-macro-whole
                                            whole-var)))
                      (t
                       (defmacro-error "&WHOLE" context name))))
               (&environment
                (cond (env-illegal
                       (error "&ENVIRONMENT is not valid with ~S." context))
                      ;; DEFSETF explicitly allows &ENVIRONMENT, and we get
                      ;; it here in a sublist.
                      ((and sublist (neq context 'defsetf))
                       (error "&ENVIRONMENT is only valid at top level of ~
                             lambda-list."))
                      (env-arg-used
                       (error "Repeated &ENVIRONMENT.")))
                (cond ((and (cdr rest-of-lambda-list)
                            (symbolp (cadr rest-of-lambda-list)))
                       (setq rest-of-lambda-list (cdr rest-of-lambda-list))
                       (check-defmacro-arg (car rest-of-lambda-list))
                       (setq *env-var* (car rest-of-lambda-list)
                             env-arg-used t))
                      (t
                       (defmacro-error "&ENVIRONMENT" context name))))
               ((&rest &body)
                (cond ((or key-seen aux-seen)
                       (error "~A after ~A in ~A"
                              var (or key-seen aux-seen) context))
                      ((and (not restp) (cdr rest-of-lambda-list))
                       (setq rest-of-lambda-list (cdr rest-of-lambda-list)
                             restp var)
                       (process-sublist (car rest-of-lambda-list)
                                        "REST-LIST-" path))
                      (t
                       (defmacro-error (symbol-name var) context name))))
               (&optional
                (when (or key-seen aux-seen restp)
                  (error "~A after ~A in ~A lambda-list."
                         var (or key-seen aux-seen restp) context))
                (when optional-seen
                  (error "Multiple ~A in ~A lambda list." var context))
                (setq now-processing :optionals
                      optional-seen var))
               (&key
                (when aux-seen
                  (error "~A after ~A in ~A lambda-list." '&key '&aux context))
                (when key-seen
                  (error "Multiple ~A in ~A lambda-list." '&key context))
                (setf now-processing :keywords
                      rest-name (gensym "KEYWORDS-")
                      restp var
                      key-seen var)
                (push rest-name *ignorable-vars*)
                (push-let-binding rest-name path :system t))
               (&allow-other-keys
                (unless (eq now-processing :keywords)
                  (error "~A outside ~A section of lambda-list in ~A."
                         var '&key context))
                (when allow-other-keys-p
                  (error "Multiple ~A in ~A lambda-list." var context))
                (setq allow-other-keys-p t))
               (&aux
                (when (eq context 'defsetf)
                  (error "~A not allowed in a ~A lambda-list." var context))
                (when aux-seen
                  (error "Multiple ~A in ~A lambda-list." '&aux context))
                (setq now-processing :auxs
                      aux-seen var))
               ;; FIXME: Other lambda list keywords.
               (t
                (case now-processing
                  ((:required)
                   (when restp
                     (defmacro-error (format nil "required argument after ~A"
                                             restp)
                         context name))
                   (push-let-binding var `(car ,path))
                   (setq minimum (1+ minimum)
                         maximum (1+ maximum)
                         path `(cdr ,path)))
                  ((:optionals)
                   (push-let-binding var `(car ,path)
                                     :when `(not (null ,path)))
                   (setq path `(cdr ,path)
                         maximum (1+ maximum)))
                  ((:keywords)
                   (let ((key (keywordicate var)))
                     (push-let-binding
                      var
                      `(lookup-keyword ,key ,rest-name)
                      :when `(keyword-supplied-p ,key ,rest-name))
                     (push key keys)))
                  ((:auxs)
                   (push-let-binding var nil))))))
            (t
             (error "non-symbol in lambda-list: ~S" var))))))
    (let (;; common subexpression, suitable for passing to functions
          ;; which expect a MAXIMUM argument regardless of whether
          ;; there actually is a maximum number of arguments
          ;; (expecting MAXIMUM=NIL when there is no maximum)
          (explicit-maximum (and (not restp) maximum)))
      (unless (and restp (zerop minimum))
        (push (let ((args-form (if (eq 'define-compiler-macro context)
                                   `(if (eq 'funcall (car ,whole-var))
                                        (cdr ,path-0)
                                        ,path-0)
                                   path-0)))
                (with-unique-names (args)
                  `(let ((,args ,args-form))
                     (unless ,(if restp
                                  ;; (If RESTP, then the argument list
                                  ;; might be dotted, in which case
                                  ;; ordinary LENGTH won't work.)
                                  `(list-of-length-at-least-p ,args ,minimum)
                                  `(proper-list-of-length-p ,args
                                                            ,minimum
                                                            ,maximum))
                       ,(if (eq error-fun 'error)
                            `(arg-count-error ',context ',name ,args
                                              ',lambda-list ,minimum
                                              ,explicit-maximum)
                            `(,error-fun 'arg-count-error
                                         :kind ',context
                                         ,@(when name `(:name ',name))
                                         :args ,args
                                         :lambda-list ',lambda-list
                                         :minimum ,minimum
                                         :maximum ,explicit-maximum))))))
              *arg-tests*))
      (when key-seen
        (with-unique-names (problem info)
          (push `(multiple-value-bind (,problem ,info)
                     (verify-keywords ,rest-name
                                      ',keys
                                      ',allow-other-keys-p)
                   (when ,problem
                     (,error-fun
                      'defmacro-lambda-list-broken-key-list-error
                      :kind ',context
                      ,@(when name `(:name ',name))
                      :problem ,problem
                      :info ,info)))
                *arg-tests*)))
      (values env-arg-used minimum explicit-maximum))))

;;; We save space in macro definitions by calling this function.
(defun arg-count-error (context name args lambda-list minimum maximum)
  (let (#-sb-xc-host
        (sb!debug:*stack-top-hint* (nth-value 1 (find-caller-name-and-frame))))
    (error 'arg-count-error
           :kind context
           :name name
           :args args
           :lambda-list lambda-list
           :minimum minimum
           :maximum maximum)))

(defun push-sublist-binding (variable path object name context error-fun)
  (check-defmacro-arg variable)
  (let ((var (gensym "TEMP-")))
    (push `(,variable
            (let ((,var ,path))
              (if (listp ,var)
                ,var
                (,error-fun 'defmacro-bogus-sublist-error
                            :kind ',context
                            ,@(when name `(:name ',name))
                            :object ,var
                            :lambda-list ',object))))
          *system-lets*)))

(defun push-let-binding (variable form
                         &key system when (else *default-default*))
  (check-defmacro-arg variable)
  (let ((let-form (if when
                      `(,variable (if ,when ,form ,else))
                      `(,variable ,form))))
    (if system
        (push let-form *system-lets*)
        (push let-form *user-lets*))))

(defun push-optional-binding (value-var init-form suppliedp-name
                              &key is-supplied-p path name context error-fun)
  (unless suppliedp-name
    (setq suppliedp-name (gensym "SUPPLIEDP-")))
  (push-let-binding suppliedp-name is-supplied-p :system t)
  (cond ((consp value-var)
         (let ((whole-thing (gensym "OPTIONAL-SUBLIST-")))
           (push-sublist-binding whole-thing
                                 `(if ,suppliedp-name ,path ,init-form)
                                 value-var name context error-fun)
           (parse-defmacro-lambda-list value-var whole-thing name
                                       context
                                       :error-fun error-fun
                                       :sublist t)))
        ((symbolp value-var)
         (push-let-binding value-var path :when suppliedp-name :else init-form))
        (t
         (error "illegal optional variable name: ~S" value-var))))

(defun defmacro-error (problem context name)
  (error "illegal or ill-formed ~A argument in ~A~@[ ~S~]"
         problem context name))

(defun check-defmacro-arg (arg)
  (when (or (and *env-var* (eq arg *env-var*))
            (member arg *system-lets* :key #'car)
            (member arg *user-lets* :key #'car))
    (error "variable ~S occurs more than once" arg)))

;;; Determine whether KEY-LIST is a valid list of keyword/value pairs.
;;; Do not signal the error directly, 'cause we don't know how it
;;; should be signaled.
(defun verify-keywords (key-list valid-keys allow-other-keys)
  (do ((already-processed nil)
       (unknown-keyword nil)
       (remaining key-list (cddr remaining)))
      ((null remaining)
       (if (and unknown-keyword
                (not allow-other-keys)
                (not (lookup-keyword :allow-other-keys key-list)))
           (values :unknown-keyword (list unknown-keyword valid-keys))
           (values nil nil)))
    (cond ((not (and (consp remaining) (listp (cdr remaining))))
           (return (values :dotted-list key-list)))
          ((null (cdr remaining))
           (return (values :odd-length key-list)))
          ((or (eq (car remaining) :allow-other-keys)
               (member (car remaining) valid-keys))
           (push (car remaining) already-processed))
          (t
           (setq unknown-keyword (car remaining))))))

(defun lookup-keyword (keyword key-list)
  (do ((remaining key-list (cddr remaining)))
      ((endp remaining))
    (when (eq keyword (car remaining))
      (return (cadr remaining)))))

(defun keyword-supplied-p (keyword key-list)
  (do ((remaining key-list (cddr remaining)))
      ((endp remaining))
    (when (eq keyword (car remaining))
      (return t))))
