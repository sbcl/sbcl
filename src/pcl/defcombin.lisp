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

(defmacro define-method-combination (&whole form &rest args)
  (declare (ignore args))
  (if (and (cddr form)
	   (listp (caddr form)))
      (expand-long-defcombin form)
      (expand-short-defcombin form)))

;;;; standard method combination

;;; The STANDARD method combination type is implemented directly by
;;; the class STANDARD-METHOD-COMBINATION. The method on
;;; COMPUTE-EFFECTIVE-METHOD does standard method combination directly
;;; and is defined by hand in the file combin.lisp. The method for
;;; FIND-METHOD-COMBINATION must appear in this file for bootstrapping
;;; reasons.
(defmethod find-method-combination ((generic-function generic-function)
				    (type (eql 'standard))
				    options)
  (when options
    (method-combination-error
      "The method combination type STANDARD accepts no options."))
  *standard-method-combination*)

;;;; short method combinations
;;;;
;;;; Short method combinations all follow the same rule for computing the
;;;; effective method. So, we just implement that rule once. Each short
;;;; method combination object just reads the parameters out of the object
;;;; and runs the same rule.

(defclass short-method-combination (standard-method-combination)
     ((operator
	:reader short-combination-operator
	:initarg :operator)
      (identity-with-one-argument
	:reader short-combination-identity-with-one-argument
	:initarg :identity-with-one-argument))
  (:predicate-name short-method-combination-p))

(defun expand-short-defcombin (whole)
  (let* ((type (cadr whole))
	 (documentation
	   (getf (cddr whole) :documentation ""))
	 (identity-with-one-arg
	   (getf (cddr whole) :identity-with-one-argument nil))
	 (operator
	   (getf (cddr whole) :operator type)))
    `(load-short-defcombin
     ',type ',operator ',identity-with-one-arg ',documentation)))

(defun load-short-defcombin (type operator ioa doc)
  (let* ((truename *load-truename*)
	 (specializers
	   (list (find-class 'generic-function)
		 (intern-eql-specializer type)
		 *the-class-t*))
	 (old-method
	   (get-method #'find-method-combination () specializers nil))
	 (new-method nil))
    (setq new-method
	  (make-instance 'standard-method
	    :qualifiers ()
	    :specializers specializers
	    :lambda-list '(generic-function type options)
	    :function (lambda (args nms &rest cm-args)
			(declare (ignore nms cm-args))
			(apply
			 (lambda (gf type options)
			   (declare (ignore gf))
			   (short-combine-methods
			    type options operator ioa new-method doc))
			 args))
	    :definition-source `((define-method-combination ,type) ,truename)))
    (when old-method
      (remove-method #'find-method-combination old-method))
    (add-method #'find-method-combination new-method)))

(defun short-combine-methods (type options operator ioa method doc)
  (cond ((null options) (setq options '(:most-specific-first)))
	((equal options '(:most-specific-first)))
	((equal options '(:most-specific-last)))
	(t
	 (method-combination-error
	  "Illegal options to a short method combination type.~%~
	   The method combination type ~S accepts one option which~%~
	   must be either :MOST-SPECIFIC-FIRST or :MOST-SPECIFIC-LAST."
	  type)))
  (make-instance 'short-method-combination
		 :type type
		 :options options
		 :operator operator
		 :identity-with-one-argument ioa
		 :definition-source method
		 :documentation doc))

(defmethod compute-effective-method ((generic-function generic-function)
				     (combin short-method-combination)
				     applicable-methods)
  (let ((type (method-combination-type combin))
	(operator (short-combination-operator combin))
	(ioa (short-combination-identity-with-one-argument combin))
	(order (car (method-combination-options combin)))
	(around ())
	(primary ()))
    (dolist (m applicable-methods)
      (let ((qualifiers (method-qualifiers m)))
	(flet ((lose (method why)
		 (invalid-method-error
		   method
		   "The method ~S ~A.~%~
		    The method combination type ~S was defined with the~%~
		    short form of DEFINE-METHOD-COMBINATION and so requires~%~
		    all methods have either the single qualifier ~S or the~%~
		    single qualifier :AROUND."
		   method why type type)))
	  (cond ((null qualifiers)
		 (lose m "has no qualifiers"))
		((cdr qualifiers)
		 (lose m "has more than one qualifier"))
		((eq (car qualifiers) :around)
		 (push m around))
		((eq (car qualifiers) type)
		 (push m primary))
		(t
		 (lose m "has an illegal qualifier"))))))
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
	     `(error "No ~S methods for the generic function ~S."
		     ',type ',generic-function))
	    ((null around) main-method)
	    (t
	     `(call-method ,(car around)
			   (,@(cdr around) (make-method ,main-method))))))))

;;;; long method combinations

(defclass long-method-combination (standard-method-combination)
     ((function :initarg :function
		:reader long-method-combination-function)))

(defun expand-long-defcombin (form)
  (let ((type (cadr form))
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
	  type lambda-list method-group-specifiers args-option gf-var
	  body)
      `(load-long-defcombin ',type ',documentation #',function))))

(defvar *long-method-combination-functions* (make-hash-table :test 'eq))

(defun load-long-defcombin (type doc function)
  (let* ((specializers
	   (list (find-class 'generic-function)
		 (intern-eql-specializer type)
		 *the-class-t*))
	 (old-method
	   (get-method #'find-method-combination () specializers nil))
	 (new-method
	   (make-instance 'standard-method
	     :qualifiers ()
	     :specializers specializers
	     :lambda-list '(generic-function type options)
	     :function (lambda (args nms &rest cm-args)
			 (declare (ignore nms cm-args))
			 (apply
			  (lambda (generic-function type options)
			    (declare (ignore generic-function))
			    (make-instance 'long-method-combination
					   :type type
					   :options options
					   :documentation doc))
			  args))
	     :definition-source `((define-method-combination ,type)
			      ,*load-truename*))))
    (setf (gethash type *long-method-combination-functions*) function)
    (when old-method (remove-method #'find-method-combination old-method))
    (add-method #'find-method-combination new-method)))

(defmethod compute-effective-method ((generic-function generic-function)
				     (combin long-method-combination)
				     applicable-methods)
  (funcall (gethash (method-combination-type combin)
		    *long-method-combination-functions*)
	   generic-function
	   combin
	   applicable-methods))

(defun make-long-method-combination-function
       (type ll method-group-specifiers args-option gf-var body)
  (declare (ignore type))
  (multiple-value-bind (real-body declarations documentation)
      ;; (Note that PARSE-BODY ignores its second arg ENVIRONMENT.)
      (parse-body body nil)

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
	   (progn .generic-function. .method-combination. .applicable-methods.)
	   (block .long-method-combination-function. ,wrapped-body))))))

;; parse-method-group-specifiers parse the method-group-specifiers

(defun wrap-method-group-specifier-bindings
       (method-group-specifiers declarations real-body)
  (let (names
        specializer-caches
        cond-clauses
        required-checks
        order-cleanups)
      (dolist (method-group-specifier method-group-specifiers)
	(multiple-value-bind (name tests description order required)
	    (parse-method-group-specifier method-group-specifier)
	  (declare (ignore description))
	  (let ((specializer-cache (gensym)))
	    (push name names)
	    (push specializer-cache specializer-caches)
	    (push `((or ,@tests)
		      (if  (equal ,specializer-cache .specializers.)
			   (return-from .long-method-combination-function.
			     '(error "More than one method of type ~S ~
				      with the same specializers."
				     ',name))
			   (setq ,specializer-cache .specializers.))
		      (push .method. ,name))
		    cond-clauses)
	    (when required
	      (push `(when (null ,name)
			 (return-from .long-method-combination-function.
			   '(error "No ~S methods." ',name)))
		      required-checks))
	    (loop (unless (and (constantp order)
			       (neq order (setq order (eval order))))
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
	  (progn .qualifiers. .specializers.)
	  (cond ,@(nreverse cond-clauses))))
      ,@(nreverse required-checks)
      ,@(nreverse order-cleanups)
      ,@real-body)))

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
(defun deal-with-args-option (wrapped-body args-option)
  (let* ((intercept-lambda-list
	   (let (collect)
	     (dolist (arg args-option)
	       (if (memq arg lambda-list-keywords)
		   (push arg collect)
		   (push (gensym) collect)))
             (nreverse collect)))
	 (intercept-rebindings
           (loop for arg in args-option
                 for int in intercept-lambda-list
                 unless (memq arg lambda-list-keywords)
                 collect `(,arg ',int))))
    (setf (cadr wrapped-body)
	  (append intercept-rebindings (cadr wrapped-body)))

    ;; Be sure to fill out the intercept lambda list so that it can
    ;; be too short if it wants to.
    (cond ((memq '&rest intercept-lambda-list))
	  ((memq '&allow-other-keys intercept-lambda-list))
	  ((memq '&key intercept-lambda-list)
	   (setq intercept-lambda-list
		 (append intercept-lambda-list '(&allow-other-keys))))
	  (t
	   (setq intercept-lambda-list
		 (append intercept-lambda-list '(&rest .ignore.)))))

    `(let ((inner-result. ,wrapped-body))
       `(apply #'(lambda ,',intercept-lambda-list
		   ,,(when (memq '.ignore. intercept-lambda-list)
		       ''(declare (ignore .ignore.)))
		   ,inner-result.)
	       .combined-method-args.))))
