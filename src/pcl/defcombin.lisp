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
(defmacro define-method-combination (&whole form &rest args)
  (declare (ignore args))
  `(progn
     (with-single-package-locked-error
         (:symbol ',(second form) "defining ~A as a method combination"))
     ,(if (and (cddr form)
               (listp (caddr form)))
          (expand-long-defcombin form)
          (expand-short-defcombin form))))

;;;; standard method combination

;;; The STANDARD method combination type is implemented directly by
;;; the class STANDARD-METHOD-COMBINATION. The method on
;;; COMPUTE-EFFECTIVE-METHOD does standard method combination directly
;;; and is defined by hand in the file combin.lisp. The method for
;;; FIND-METHOD-COMBINATION must appear in this file for bootstrapping
;;; reasons.
(defmethod find-method-combination ((generic-function generic-function)
                                    (type-name (eql 'standard))
                                    options)
  (when options
    (method-combination-error
      "STANDARD method combination accepts no options."))
  *standard-method-combination*)

;;;; short method combinations
;;;;
;;;; Short method combinations all follow the same rule for computing the
;;;; effective method. So, we just implement that rule once. Each short
;;;; method combination object just reads the parameters out of the object
;;;; and runs the same rule.

(defun expand-short-defcombin (whole)
  (let* ((type-name (cadr whole))
         (documentation
           (getf (cddr whole) :documentation))
         (identity-with-one-arg
           (getf (cddr whole) :identity-with-one-argument nil))
         (operator
           (getf (cddr whole) :operator type-name)))
    `(load-short-defcombin
     ',type-name ',operator ',identity-with-one-arg ',documentation
      (sb-c:source-location))))

(defun load-short-defcombin (type-name operator ioa doc source-location)
  (let* ((specializers
           (list (find-class 'generic-function)
                 (intern-eql-specializer type-name)
                 *the-class-t*))
         (old-method
           (get-method #'find-method-combination () specializers nil))
         (new-method nil))
    (setq new-method
          (make-instance 'standard-method
            :qualifiers ()
            :specializers specializers
            :lambda-list '(generic-function type-name options)
            :function (lambda (args nms &rest cm-args)
                        (declare (ignore nms cm-args))
                        (apply
                         (lambda (gf type-name options)
                           (declare (ignore gf))
                           (short-combine-methods
                            type-name options operator ioa new-method doc))
                         args))
            :definition-source source-location))
    (when old-method
      (remove-method #'find-method-combination old-method))
    (add-method #'find-method-combination new-method)
    (setf (random-documentation type-name 'method-combination) doc)
    type-name))

(defun short-combine-methods (type-name options operator ioa method doc)
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
                 :definition-source method
                 :documentation doc))

(defmethod compute-effective-method ((generic-function generic-function)
                                     (combin short-method-combination)
                                     applicable-methods)
  (let ((type-name (method-combination-type-name combin))
        (operator (short-combination-operator combin))
        (ioa (short-combination-identity-with-one-argument combin))
        (order (car (method-combination-options combin)))
        (around ())
        (primary ()))
    (flet ((invalid (gf combin m)
             (return-from compute-effective-method
               `(%invalid-qualifiers ',gf ',combin ',m))))
      (dolist (m applicable-methods)
        (let ((qualifiers (method-qualifiers m)))
          (cond ((null qualifiers) (invalid generic-function combin m))
                ((cdr qualifiers) (invalid generic-function combin m))
                ((eq (car qualifiers) :around)
                 (push m around))
                ((eq (car qualifiers) type-name)
                 (push m primary))
                (t (invalid generic-function combin m))))))
    (setq around (nreverse around))
    (ecase order
      (:most-specific-last) ; nothing to be done, already in correct order
      (:most-specific-first
       (setq primary (nreverse primary))))
    (let ((main-method
            (if (and (null (cdr primary))
                     (not (null ioa)))
                `(call-method ,(car primary) ())
                `(,operator ,@(mapcar (lambda (m) `(call-method ,m ()))
                                      primary)))))
      (cond ((null primary)
             ;; As of sbcl-0.8.0.80 we don't seem to need to need
             ;; to do anything messy like
             ;;        `(APPLY (FUNCTION (IF AROUND
             ;;                              'NO-PRIMARY-METHOD
             ;;                              'NO-APPLICABLE-METHOD)
             ;;                           ',GENERIC-FUNCTION
             ;;                           .ARGS.)
             ;; here because (for reasons I don't understand at the
             ;; moment -- WHN) control will never reach here if there
             ;; are no applicable methods, but instead end up
             ;; in NO-APPLICABLE-METHODS first.
             ;;
             ;; FIXME: The way that we arrange for .ARGS. to be bound
             ;; here seems weird. We rely on EXPAND-EFFECTIVE-METHOD-FUNCTION
             ;; recognizing any form whose operator is %NO-PRIMARY-METHOD
             ;; as magical, and carefully surrounding it with a
             ;; LAMBDA form which binds .ARGS. But...
             ;;   1. That seems fragile, because the magicalness of
             ;;      %NO-PRIMARY-METHOD forms is scattered around
             ;;      the system. So it could easily be broken by
             ;;      locally-plausible maintenance changes like,
             ;;      e.g., using the APPLY expression above.
             ;;   2. That seems buggy w.r.t. to MOPpish tricks in
             ;;      user code, e.g.
             ;;         (DEFMETHOD COMPUTE-EFFECTIVE-METHOD :AROUND (...)
             ;;           `(PROGN ,(CALL-NEXT-METHOD) (INCF *MY-CTR*)))
             `(%no-primary-method ',generic-function .args.))
            ((null around) main-method)
            (t
             `(call-method ,(car around)
                           (,@(cdr around) (make-method ,main-method))))))))

(defmethod invalid-qualifiers ((gf generic-function)
                               (combin short-method-combination)
                               method)
  (let ((qualifiers (method-qualifiers method))
        (type-name (method-combination-type-name combin)))
    (let ((why (cond
                 ((null qualifiers) "has no qualifiers")
                 ((cdr qualifiers) "has too many qualifiers")
                 (t (aver (and (neq (car qualifiers) type-name)
                               (neq (car qualifiers) :around)))
                    "has an invalid qualifier"))))
      (invalid-method-error
       method
       "The method ~S on ~S ~A.~%~
        The method combination type ~S was defined with the~%~
        short form of DEFINE-METHOD-COMBINATION and so requires~%~
        all methods have either the single qualifier ~S or the~%~
        single qualifier :AROUND."
       method gf why type-name type-name))))

;;;; long method combinations

(defun expand-long-defcombin (form)
  (let ((type-name (cadr form))
        (lambda-list (caddr form))
        (method-group-specifiers (cadddr form))
        (body (cddddr form))
        (args-option ())
        (gf-var nil))
    (when (and (consp (car body)) (eq (caar body) :arguments))
      (setq args-option (cdr (pop body))))
    (when (and (consp (car body)) (eq (caar body) :generic-function))
      (setq gf-var (cadr (pop body))))
    (multiple-value-bind (documentation function)
        (make-long-method-combination-function
          type-name lambda-list method-group-specifiers args-option gf-var
          body)
      `(load-long-defcombin ',type-name ',documentation #',function
                            ',args-option (sb-c:source-location)))))

(defvar *long-method-combination-functions* (make-hash-table :test 'eq))

(defun load-long-defcombin
    (type-name doc function args-lambda-list source-location)
  (let* ((specializers
           (list (find-class 'generic-function)
                 (intern-eql-specializer type-name)
                 *the-class-t*))
         (old-method
           (get-method #'find-method-combination () specializers nil))
         (new-method
           (make-instance 'standard-method
             :qualifiers ()
             :specializers specializers
             :lambda-list '(generic-function type-name options)
             :function (lambda (args nms &rest cm-args)
                         (declare (ignore nms cm-args))
                         (apply
                          (lambda (generic-function type-name options)
                            (declare (ignore generic-function))
                            (make-instance 'long-method-combination
                                           :type-name type-name
                                           :options options
                                           :args-lambda-list args-lambda-list
                                           :documentation doc))
                          args))
             :definition-source source-location)))
    (setf (gethash type-name *long-method-combination-functions*) function)
    (when old-method (remove-method #'find-method-combination old-method))
    (add-method #'find-method-combination new-method)
    (setf (random-documentation type-name 'method-combination) doc)
    type-name))

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
      :references (list '(:ansi-cl :macro define-method-combination))))

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
      `(let (,@(nreverse names) ,@(nreverse specializer-caches))
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
  ;;(declare (values name tests description order required))
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
;;;
;;; FIXME: in here we have not one but two mini-copies of a weird
;;; hybrid of PARSE-LAMBDA-LIST and (obsolete) PARSE-DEFMACRO-LAMBDA-LIST.
(defun deal-with-args-option (wrapped-body args-lambda-list)
  (let ((intercept-rebindings
         (let (rebindings)
           (dolist (arg args-lambda-list (nreverse rebindings))
             (unless (member arg lambda-list-keywords :test #'eq)
               (typecase arg
                 (symbol (push `(,arg ',arg) rebindings))
                 (cons
                  (unless (symbolp (car arg))
                    (error "invalid lambda-list specifier: ~S." arg))
                  (push `(,(car arg) ',(car arg)) rebindings))
                 (t (error "invalid lambda-list-specifier: ~S." arg)))))))
        (nreq 0)
        (nopt 0)
        (whole nil))
    ;; Count the number of required and optional parameters in
    ;; ARGS-LAMBDA-LIST into NREQ and NOPT, and set WHOLE to the
    ;; name of a &WHOLE parameter, if any.
    (when (member '&whole (rest args-lambda-list))
      (error 'simple-program-error
             :format-control "~@<The value of the :ARGUMENTS option of ~
                DEFINE-METHOD-COMBINATION is~2I~_~S,~I~_but &WHOLE may ~
                only appear first in the lambda list.~:>"
             :format-arguments (list args-lambda-list)))
    (loop with state = 'required
          for arg in args-lambda-list do
            (if (memq arg lambda-list-keywords)
                (setq state arg)
                (case state
                  (required (incf nreq))
                  (&optional (incf nopt))
                  (&whole (setq whole arg state 'required)))))
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
            (frob-combined-method-args
             .gf-args. ',gf-lambda-list
             ,',nreq ,',nopt)
          ,,(when (memq '.ignore. args-lambda-list)
              ''(declare (ignore .ignore.)))
          ;; If there is a &WHOLE in the args-lambda-list, let
          ;; it result in the actual arguments of the generic-function
          ;; not the frobbed list.
          ,,(when whole
              ``(setq ,',whole .gf-args.))
          ,inner-result.))))

;;; Partition VALUES into three sections: required, optional, and the
;;; rest, according to required, optional, and other parameters in
;;; LAMBDA-LIST.  Make the required and optional sections NREQ and
;;; NOPT elements long by discarding values or adding NILs.  Value is
;;; the concatenated list of required and optional sections, and what
;;; is left as rest from VALUES.
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
        else if (eq section '&optional)
          count t into no
          and collect (pop values) into optional
        finally
          (flet ((frob (list n m)
                   (cond ((> n m) (butlast list (- n m)))
                         ((< n m) (nconc list (make-list (- m n))))
                         (t list))))
            (return (nconc (frob required nr nreq)
                           (frob optional no nopt)
                           values)))))

