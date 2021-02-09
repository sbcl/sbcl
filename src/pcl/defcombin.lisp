;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is derived from software originally released by Xerox
;;;; Corporation. Copyright and release statements follow. Later modifications
;;;; to the software are in the public domain and are provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for more
;;;; information.

;;;; copyright information from original PCL sources:
;;;;
;;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;;; All rights reserved.
;;;;
;;;; Use and copying of this software and preparation of derivative works based
;;;; upon this software are permitted. Any distribution of this software or
;;;; derivative works must comply with all applicable United States export
;;;; control laws.
;;;;
;;;; This software is made available AS IS, and Xerox Corporation makes no
;;;; warranty about the software, its performance or its conformity to any
;;;; specification.

(in-package "SB-PCL")

;;; FIXME: according to ANSI 3.4.10 this is supposed to allow &WHOLE
;;; in the long syntax. But it clearly does not, because if you write
;;; (&WHOLE v) then you get (LAMBDA (&WHOLE V ...) ...) which is illegal
;;;
(defmacro define-method-combination (&whole form name . args)
  (declare (ignore args))
  (check-designator name define-method-combination)
  `(progn
     (with-single-package-locked-error
         (:symbol ',name "defining ~A as a method combination"))
     ,(if (and (cddr form)
               (listp (caddr form)))
          (expand-long-defcombin form)
          (expand-short-defcombin form))))

(defstruct method-combination-info
  (lambda-list nil :type list)
  (constructor (error "missing arg") :type function)
  (cache nil :type list)
  (source-location nil :type (or null sb-c:definition-source-location)))
(defglobal **method-combinations** (make-hash-table :test 'eql))

(defmethod find-method-combination ((generic-function generic-function)
                                    name options)
  (let ((info (gethash name **method-combinations**)))
    (when info
      (or (cdr (assoc options (method-combination-info-cache info) :test #'equal))
          (cdar (push (cons options (funcall (method-combination-info-constructor info) options))
                      (method-combination-info-cache info)))))))

;;;; standard method combination
(setf (gethash 'standard **method-combinations**)
      (make-method-combination-info
       :constructor (lambda (options) (when options (method-combination-error "STANDARD method combination accepts no options.")) *standard-method-combination*)
       :cache (list (cons nil *standard-method-combination*))))

(defun update-mcs (name new old frobmc)
  (setf (gethash name **method-combinations**) new)
  ;; for correctness' sake we should probably lock
  ;; **METHOD-COMBINATIONS** while we're updating things, to defend
  ;; against defining gfs in one thread while redefining the method
  ;; combination in another thread.
  (when old
    (setf (method-combination-info-cache new) (method-combination-info-cache old))
    (setf (method-combination-info-cache old) nil)
    (dolist (entry (method-combination-info-cache new))
      (let* ((mc (cdr entry))
             (gfs (method-combination-%generic-functions mc)))
        (funcall frobmc mc)
        (flet ((flush (gf ignore)
                 (declare (ignore ignore))
                 (flush-effective-method-cache gf)
                 (reinitialize-instance gf)))
          (maphash #'flush gfs))))))

;;;; short method combinations
;;;;
;;;; Short method combinations all follow the same rule for computing the
;;;; effective method. So, we just implement that rule once. Each short
;;;; method combination object just reads the parameters out of the object
;;;; and runs the same rule.

(defun expand-short-defcombin (whole)
  (let* ((canary (cons nil nil))
         (type-name (cadr whole))
         (documentation (getf (cddr whole) :documentation canary))
         (ioa (getf (cddr whole) :identity-with-one-argument nil))
         (operator
           (getf (cddr whole) :operator type-name)))
    (unless (or (eq documentation canary)
                (stringp documentation))
      (%program-error "~@<~S argument to the short form of ~S must be a string.~:@>"
                      :documentation 'define-method-combination))
    `(load-short-defcombin
      ',type-name ',operator ',ioa
      ',(and (neq documentation canary)
             documentation)
      (sb-c:source-location))))

(defun load-short-defcombin (type-name operator ioa doc source-location)
  (let ((info (make-method-combination-info
               :lambda-list '(&optional (order :most-specific-first))
               :source-location source-location
               :constructor (lambda (options)
                              (short-combine-methods type-name options operator ioa source-location doc))))
        (old-info (gethash type-name **method-combinations**)))
    (flet ((frobber (mc)
             (change-class mc 'short-method-combination
                           :operator operator :identity-with-one-argument ioa
                           'source source-location :documentation doc)))
      (update-mcs type-name info old-info #'frobber)))
  (setf (random-documentation type-name 'method-combination) doc)
  type-name)

(defun short-combine-methods (type-name options operator ioa source-location doc)
  (cond ((null options) (setq options '(:most-specific-first)))
        ((equal options '(:most-specific-first)))
        ((equal options '(:most-specific-last)))
        (t
         (method-combination-error
          "Illegal options to a short method combination type.~%~
           The method combination type ~S accepts one option which~%~
           must be either :MOST-SPECIFIC-FIRST or :MOST-SPECIFIC-LAST."
          type-name)))
  (make-instance 'short-method-combination
                 :type-name type-name
                 :options options
                 :operator operator
                 :identity-with-one-argument ioa
                 'source source-location
                 :documentation doc))

(defmethod invalid-qualifiers ((gf generic-function)
                               (combin short-method-combination)
                               method)
  (let* ((qualifiers (method-qualifiers method))
         (qualifier (first qualifiers))
         (type-name (method-combination-type-name combin))
         (why (cond
                ((null qualifiers)
                 "has no qualifiers")
                ((cdr qualifiers)
                 "has too many qualifiers")
                (t
                 (aver (not (short-method-combination-qualifier-p
                             type-name qualifier)))
                 "has an invalid qualifier"))))
    (invalid-method-error
     method
     "~@<The method ~S on ~S ~A.~
      ~@:_~@:_~
      The method combination type ~S was defined with the short form ~
      of DEFINE-METHOD-COMBINATION and so requires all methods have ~
      either ~{the single qualifier ~S~^ or ~}.~@:>"
     method gf why type-name (short-method-combination-qualifiers type-name))))

;;;; long method combinations

(defun expand-long-defcombin (form)
  (let ((type-name (cadr form))
        (lambda-list (caddr form))
        (method-group-specifiers-presentp (cdddr form))
        (method-group-specifiers (cadddr form))
        (body (cddddr form))
        (args-option ())
        (gf-var nil))
    (unless method-group-specifiers-presentp
      (%program-error "~@<The long form of ~S requires a list of method group specifiers.~:@>"
                      'define-method-combination))
    (when (and (consp (car body)) (eq (caar body) :arguments))
      (setq args-option (cdr (pop body))))
    (when (and (consp (car body)) (eq (caar body) :generic-function))
      (unless (and (cdar body) (symbolp (cadar body)) (null (cddar body)))
        (%program-error "~@<The argument to the ~S option of ~S must be a single symbol.~:@>"
                        :generic-function 'define-method-combination))
      (setq gf-var (cadr (pop body))))
    (multiple-value-bind (documentation function)
        (make-long-method-combination-function
          type-name lambda-list method-group-specifiers args-option gf-var
          body)
      `(load-long-defcombin ',type-name ',documentation #',function
                            ',lambda-list ',args-option (sb-c:source-location)))))

(define-load-time-global *long-method-combination-functions* (make-hash-table :test 'eq))

(defun load-long-defcombin (type-name doc function lambda-list args-lambda-list source-location)
  (let ((info (make-method-combination-info
               :lambda-list lambda-list
               :constructor (lambda (options)
                              (make-instance 'long-method-combination
                                             :type-name type-name
                                             :options options
                                             :args-lambda-list args-lambda-list
                                             'source source-location
                                             :documentation doc))
               :source-location source-location))
        (old-info (gethash type-name **method-combinations**)))
    (flet ((frobber (mc)
             (change-class mc 'long-method-combination
                           :type-name type-name :args-lambda-list args-lambda-list
                           'source source-location :documentation doc)))
      (update-mcs type-name info old-info #'frobber)))
  (setf (gethash type-name *long-method-combination-functions*) function)
  (setf (random-documentation type-name 'method-combination) doc)
  type-name)

(defmethod compute-effective-method ((generic-function generic-function)
                                     (combin long-method-combination)
                                     applicable-methods)
  (funcall (gethash (method-combination-type-name combin)
                    *long-method-combination-functions*)
           generic-function
           combin
           applicable-methods))

(defun make-long-method-combination-function
       (type-name ll method-group-specifiers args-option gf-var body)
  (declare (ignore type-name))
  (multiple-value-bind (real-body declarations documentation)
      (parse-body body t)
    (let ((wrapped-body
            (wrap-method-group-specifier-bindings method-group-specifiers
                                                  declarations
                                                  real-body)))
      (when gf-var
        (push `(,gf-var .generic-function.) (cadr wrapped-body)))

      (when args-option
        (setq wrapped-body (deal-with-args-option wrapped-body args-option)))

      (when ll
        (setq wrapped-body
              `(apply #'(lambda ,ll ,wrapped-body)
                      (method-combination-options .method-combination.))))

      (values
        documentation
        `(lambda (.generic-function. .method-combination. .applicable-methods.)
           (declare (ignorable .generic-function.
                     .method-combination. .applicable-methods.))
           (block .long-method-combination-function. ,wrapped-body))))))

(define-condition long-method-combination-error
    (reference-condition simple-error)
  ()
  (:default-initargs
   :references '((:ansi-cl :macro define-method-combination))))

;;; NOTE:
;;;
;;; The semantics of long form method combination in the presence of
;;; multiple methods with the same specializers in the same method
;;; group are unclear by the spec: a portion of the standard implies
;;; that an error should be signalled, and another is more lenient.
;;;
;;; It is reasonable to allow a single method group of * to bypass all
;;; rules, as this is explicitly stated in the standard.

(defun group-cond-clause (name tests specializer-cache star-only)
  (let ((maybe-error-clause
         (if star-only
             `(setq ,specializer-cache .specializers.)
             `(if (and (equal ,specializer-cache .specializers.)
                       (not (null .specializers.)))
                  (return-from .long-method-combination-function.
                    '(error 'long-method-combination-error
                      :format-control "More than one method of type ~S ~
                                       with the same specializers."
                      :format-arguments (list ',name)))
                  (setq ,specializer-cache .specializers.)))))
    `((or ,@tests)
      ,maybe-error-clause
      (push .method. ,name))))

(defun wrap-method-group-specifier-bindings
    (method-group-specifiers declarations real-body)
  (let (names specializer-caches cond-clauses required-checks order-cleanups)
    (let ((nspecifiers (length method-group-specifiers)))
      (dolist (method-group-specifier method-group-specifiers
               (push `(t (return-from .long-method-combination-function.
                           `(invalid-method-error , .method.
                             "~@<is applicable, but does not belong ~
                              to any method group~@:>")))
                     cond-clauses))
        (multiple-value-bind (name tests description order required)
            (parse-method-group-specifier method-group-specifier)
          (declare (ignore description))
          (let ((specializer-cache (gensym)))
            (push name names)
            (push specializer-cache specializer-caches)
            (push (group-cond-clause name tests specializer-cache
                                     (and (eq (cadr method-group-specifier) '*)
                                          (= nspecifiers 1)))
                  cond-clauses)
            (when required
              (push `(when (null ,name)
                      (return-from .long-method-combination-function.
                        '(error 'long-method-combination-error
                          :format-control "No ~S methods."
                          :format-arguments (list ',name))))
                    required-checks))
            (loop (unless (and (constantp order)
                               (neq order (setq order
                                                (constant-form-value order))))
                    (return t)))
            (push (cond ((eq order :most-specific-first)
                         `(setq ,name (nreverse ,name)))
                        ((eq order :most-specific-last) ())
                        (t
                         `(ecase ,order
                           (:most-specific-first
                            (setq ,name (nreverse ,name)))
                           (:most-specific-last))))
                  order-cleanups))))
      `(let (,@(nreverse names) ,@specializer-caches)
        (declare (ignorable ,@specializer-caches))
        ,@declarations
        (dolist (.method. .applicable-methods.)
          (let ((.qualifiers. (method-qualifiers .method.))
                (.specializers. (method-specializers .method.)))
            (declare (ignorable .qualifiers. .specializers.))
            (cond ,@(nreverse cond-clauses))))
        ,@(nreverse required-checks)
        ,@(nreverse order-cleanups)
        ,@real-body))))

(defun parse-method-group-specifier (method-group-specifier)
  (unless (symbolp (car method-group-specifier))
    (%program-error "~@<Method group specifiers in the long form of ~S ~
                     must begin with a symbol.~:@>" 'define-method-combination))
  (let* ((name (pop method-group-specifier))
         (patterns ())
         (tests
           (let (collect)
             (block collect-tests
               (loop
                 (if (or (null method-group-specifier)
                         (memq (car method-group-specifier)
                               '(:description :order :required)))
                     (return-from collect-tests t)
                     (let ((pattern (pop method-group-specifier)))
                       (push pattern patterns)
                       (push (parse-qualifier-pattern name pattern)
                             collect)))))
             (nreverse collect))))
    (when (null patterns)
      (%program-error "~@<Method group specifiers in the long form of ~S ~
                       must have at least one qualifier pattern or predicate.~@:>"
                      'define-method-combination))
    (values name
            tests
            (getf method-group-specifier :description
                  (make-default-method-group-description patterns))
            (getf method-group-specifier :order :most-specific-first)
            (getf method-group-specifier :required nil))))

(defun parse-qualifier-pattern (name pattern)
  (cond ((eq pattern '()) `(null .qualifiers.))
        ((eq pattern '*) t)
        ((symbolp pattern) `(,pattern .qualifiers.))
        ((listp pattern) `(qualifier-check-runtime ',pattern .qualifiers.))
        (t (error "In the method group specifier ~S,~%~
                   ~S isn't a valid qualifier pattern."
                  name pattern))))

(defun qualifier-check-runtime (pattern qualifiers)
  (loop (cond ((and (null pattern) (null qualifiers))
               (return t))
              ((eq pattern '*) (return t))
              ((and pattern qualifiers (eq (car pattern) (car qualifiers)))
               (pop pattern)
               (pop qualifiers))
              (t (return nil)))))

(defun make-default-method-group-description (patterns)
  (if (cdr patterns)
      (format nil
              "methods matching one of the patterns: ~{~S, ~} ~S"
              (butlast patterns) (car (last patterns)))
      (format nil
              "methods matching the pattern: ~S"
              (car patterns))))

;;; This baby is a complete mess. I can't believe we put it in this
;;; way. No doubt this is a large part of what drives MLY crazy.
;;;
;;; At runtime (when the effective-method is run), we bind an intercept
;;; lambda-list to the arguments to the generic function.
;;;
;;; At compute-effective-method time, the symbols in the :arguments
;;; option are bound to the symbols in the intercept lambda list.
(defun deal-with-args-option (wrapped-body args-lambda-list)
  (binding* (((llks required optional rest key aux env whole)
              (parse-lambda-list
               args-lambda-list
               :context "a define-method-combination arguments lambda list"
               :accept (lambda-list-keyword-mask '(&allow-other-keys &aux &key &optional &rest &whole)))))
    (check-lambda-list-names llks required optional rest key aux env whole
                             :context "a define-method-combination arguments lambda list"
                             :signal-via #'%program-error)
    (let (intercept-rebindings)
      (flet ((intercept (sym) (push `(,sym ',sym) intercept-rebindings)))
        (when whole (intercept (car whole)))
        (dolist (arg required)
          (intercept arg))
        (dolist (arg optional)
          (multiple-value-bind (name default suppliedp)
              (parse-optional-arg-spec arg)
            (declare (ignore default))
            (intercept name)
            (when suppliedp (intercept (car suppliedp)))))
        (when rest (intercept (car rest)))
        (dolist (arg key)
          (multiple-value-bind (keyword name default suppliedp)
              (parse-key-arg-spec arg)
            (declare (ignore keyword default))
            (intercept name)
            (when suppliedp (intercept (car suppliedp)))))
        (dolist (arg aux)
          (intercept (if (consp arg) (car arg) arg)))
        ;; cosmetic only
        (setq intercept-rebindings (nreverse intercept-rebindings)))
      ;; This assumes that the head of WRAPPED-BODY is a let, and it
      ;; injects let-bindings of the form (ARG 'SYM) for all variables
      ;; of the argument-lambda-list; SYM is a gensym.
      (aver (memq (first wrapped-body) '(let let*)))
      (setf (second wrapped-body)
            (append intercept-rebindings (second wrapped-body)))
      ;; Be sure to fill out the args lambda list so that it can be too
      ;; short if it wants to.
      (unless (or (memq '&rest args-lambda-list)
                  (memq '&allow-other-keys args-lambda-list))
        (let ((aux (memq '&aux args-lambda-list)))
          (setq args-lambda-list
                (append (ldiff args-lambda-list aux)
                        (if (memq '&key args-lambda-list)
                            '(&allow-other-keys)
                            '(&rest .ignore.))
                        aux))))
      ;; .GENERIC-FUNCTION. is bound to the generic function in the
      ;; method combination function, and .GF-ARGS* is bound to the
      ;; generic function arguments in effective method functions
      ;; created for generic functions having a method combination that
      ;; uses :ARGUMENTS.
      ;;
      ;; The DESTRUCTURING-BIND binds the parameters of the
      ;; ARGS-LAMBDA-LIST to actual generic function arguments.  Because
      ;; ARGS-LAMBDA-LIST may be shorter or longer than the generic
      ;; function's lambda list, which is only known at run time, this
      ;; destructuring has to be done on a slighly modified list of
      ;; actual arguments, from which values might be stripped or added.
      ;;
      ;; Using one of the variable names in the body inserts a symbol
      ;; into the effective method, and running the effective method
      ;; produces the value of actual argument that is bound to the
      ;; symbol.
      `(let ((inner-result. ,wrapped-body)
             (gf-lambda-list (generic-function-lambda-list .generic-function.)))
         `(destructuring-bind ,',args-lambda-list
              ;; FIXME: we know enough (generic function lambda list,
              ;; args lambda list) at generate-effective-method-time
              ;; that we could partially evaluate this frobber, to
              ;; inline specific argument list manipulation rather
              ;; than the generic code currently contained in
              ;; FROB-COMBINED-METHOD-ARGS.
              (frob-combined-method-args
               .gf-args. ',gf-lambda-list
               ,',(length required) ,',(length optional))
            ,,(when (memq '.ignore. args-lambda-list)
                ''(declare (ignore .ignore.)))
            ;; If there is a &WHOLE in the args-lambda-list, let
            ;; it result in the actual arguments of the generic-function
            ;; not the frobbed list.
            ,,(when whole
                ``(setq ,',whole .gf-args.))
            ,inner-result.)))))

;;; Partition VALUES into three sections: required, optional, and the
;;; rest, according to required, optional, and other parameters in
;;; LAMBDA-LIST.  Make the required and optional sections NREQ and
;;; NOPT elements long by discarding values or adding NILs, except
;;; don't extend the optional section when there are no more VALUES.
;;; Value is the concatenated list of required and optional sections,
;;; and what is left as rest from VALUES.
(defun frob-combined-method-args (values lambda-list nreq nopt)
  (loop with section = 'required
        for arg in lambda-list
        if (memq arg lambda-list-keywords) do
          (setq section arg)
          (unless (eq section '&optional)
            (loop-finish))
        else if (eq section 'required)
          count t into nr
          and collect (pop values) into required
        else if (and values (eq section '&optional))
          count t into no
          and collect (pop values) into optional
        finally
          (flet ((frob (list n m lengthenp)
                   (cond ((> n m) (butlast list (- n m)))
                         ((and (< n m) lengthenp) (nconc list (make-list (- m n))))
                         (t list))))
            (return (nconc (frob required nr nreq t)
                           (frob optional no nopt values)
                           values)))))
