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

(sb-int:file-comment
  "$Header$")

;;; This file is (almost) functionally equivalent to dlap.lisp, but easier to
;;; read.

;;; Might generate faster code, too, depending on the compiler and whether an
;;; implementation-specific lap assembler was used.

(defun emit-one-class-reader (class-slot-p)
  (emit-reader/writer :reader 1 class-slot-p))

(defun emit-one-class-writer (class-slot-p)
  (emit-reader/writer :writer 1 class-slot-p))

(defun emit-two-class-reader (class-slot-p)
  (emit-reader/writer :reader 2 class-slot-p))

(defun emit-two-class-writer (class-slot-p)
  (emit-reader/writer :writer 2 class-slot-p))

;;; --------------------------------

(defun emit-one-index-readers (class-slot-p)
  (emit-one-or-n-index-reader/writer :reader nil class-slot-p))

(defun emit-one-index-writers (class-slot-p)
  (emit-one-or-n-index-reader/writer :writer nil class-slot-p))

(defun emit-n-n-readers ()
  (emit-one-or-n-index-reader/writer :reader t nil))

(defun emit-n-n-writers ()
  (emit-one-or-n-index-reader/writer :writer t nil))

;;; --------------------------------

(defun emit-checking (metatypes applyp)
  (emit-checking-or-caching nil nil metatypes applyp))

(defun emit-caching (metatypes applyp)
  (emit-checking-or-caching t nil metatypes applyp))

(defun emit-in-checking-cache-p (metatypes)
  (emit-checking-or-caching nil t metatypes nil))

(defun emit-constant-value (metatypes)
  (emit-checking-or-caching t t metatypes nil))

;;; --------------------------------

(defvar *precompiling-lap* nil)
(defvar *emit-function-p* t)

(defun emit-default-only (metatypes applyp)
  (when (and (null *precompiling-lap*) *emit-function-p*)
    (return-from emit-default-only
      (emit-default-only-function metatypes applyp)))
  (let* ((dlap-lambda-list (make-dlap-lambda-list metatypes applyp))
	 (args (remove '&rest dlap-lambda-list))
	 (restl (when applyp '(.lap-rest-arg.))))
    (generating-lisp '(emf)
		     dlap-lambda-list
      `(invoke-effective-method-function emf ,applyp ,@args ,@restl))))

(defmacro emit-default-only-macro (metatypes applyp)
  (let ((*emit-function-p* nil)
	(*precompiling-lap* t))
    (values
     (emit-default-only metatypes applyp))))

;;; --------------------------------

(defun generating-lisp (closure-variables args form)
  (let* ((rest (memq '&rest args))
	 (ldiff (and rest (ldiff args rest)))
	 (args (if rest (append ldiff '(&rest .lap-rest-arg.)) args))
	 (lambda `(lambda ,closure-variables
		    ,@(when (member 'miss-fn closure-variables)
			`((declare (type function miss-fn))))
		    #'(sb-kernel:instance-lambda ,args
			(let ()
			  (declare #.*optimize-speed*)
			  ,form)))))
    (values (if *precompiling-lap*
		`#',lambda
		(compile-lambda lambda))
	    nil)))

;;; note on implementation for CMU 17 and later (including SBCL):
;;; Since std-instance-p is weakened, that branch may run on non-pcl
;;; instances (structures). The result will be the non-wrapper layout
;;; for the structure, which will cause a miss. The "slots" will be
;;; whatever the first slot is, but will be ignored. Similarly,
;;; fsc-instance-p returns true on funcallable structures as well as
;;; PCL fins.
(defun emit-reader/writer (reader/writer 1-or-2-class class-slot-p)
  (when (and (null *precompiling-lap*) *emit-function-p*)
    (return-from emit-reader/writer
      (emit-reader/writer-function reader/writer 1-or-2-class class-slot-p)))
  (let ((instance nil)
	(arglist  ())
	(closure-variables ())
	(field (first-wrapper-cache-number-index))
	(readp (eq reader/writer :reader))
	(read-form (emit-slot-read-form class-slot-p 'index 'slots)))
    ;;we need some field to do the fast obsolete check
    (ecase reader/writer
      (:reader (setq instance (dfun-arg-symbol 0)
		     arglist  (list instance)))
      (:writer (setq instance (dfun-arg-symbol 1)
		     arglist  (list (dfun-arg-symbol 0) instance))))
    (ecase 1-or-2-class
      (1 (setq closure-variables '(wrapper-0 index miss-fn)))
      (2 (setq closure-variables '(wrapper-0 wrapper-1 index miss-fn))))
    (generating-lisp closure-variables
		     arglist
       `(let* (,@(unless class-slot-p `((slots nil)))
	       (wrapper (cond ((std-instance-p ,instance)
			       ,@(unless class-slot-p
				   `((setq slots (std-instance-slots ,instance))))
			       (std-instance-wrapper ,instance))
			      ((fsc-instance-p ,instance)
			       ,@(unless class-slot-p
				   `((setq slots (fsc-instance-slots ,instance))))
			       (fsc-instance-wrapper ,instance)))))
	  (block access
	    (when (and wrapper
		       (/= (wrapper-cache-number-vector-ref wrapper ,field) 0)
		       ,@(if (eql 1 1-or-2-class)
			     `((eq wrapper wrapper-0))
			     `((or (eq wrapper wrapper-0)
				   (eq wrapper wrapper-1)))))
	      ,@(if readp
		    `((let ((value ,read-form))
			(unless (eq value *slot-unbound*)
			  (return-from access value))))
		    `((return-from access (setf ,read-form ,(car arglist))))))
	    (funcall miss-fn ,@arglist))))))

(defun emit-slot-read-form (class-slot-p index slots)
  (if class-slot-p
      `(cdr ,index)
      `(%instance-ref ,slots ,index)))

(defun emit-boundp-check (value-form miss-fn arglist)
  `(let ((value ,value-form))
     (if (eq value *slot-unbound*)
	 (funcall ,miss-fn ,@arglist)
	 value)))

(defun emit-slot-access (reader/writer class-slot-p slots index miss-fn arglist)
  (let ((read-form (emit-slot-read-form class-slot-p index slots)))
    (ecase reader/writer
      (:reader (emit-boundp-check read-form miss-fn arglist))
      (:writer `(setf ,read-form ,(car arglist))))))

(defmacro emit-reader/writer-macro (reader/writer 1-or-2-class class-slot-p)
  (let ((*emit-function-p* nil)
	(*precompiling-lap* t))
    (values
     (emit-reader/writer reader/writer 1-or-2-class class-slot-p))))

(defun emit-one-or-n-index-reader/writer (reader/writer cached-index-p class-slot-p)
  (when (and (null *precompiling-lap*) *emit-function-p*)
    (return-from emit-one-or-n-index-reader/writer
      (emit-one-or-n-index-reader/writer-function
       reader/writer cached-index-p class-slot-p)))
  (multiple-value-bind (arglist metatypes)
      (ecase reader/writer
	(:reader (values (list (dfun-arg-symbol 0))
			 '(standard-instance)))
	(:writer (values (list (dfun-arg-symbol 0) (dfun-arg-symbol 1))
			 '(t standard-instance))))
    (generating-lisp `(cache ,@(unless cached-index-p '(index)) miss-fn)
		     arglist
      `(let (,@(unless class-slot-p '(slots))
	     ,@(when cached-index-p '(index)))
	 ,(emit-dlap arglist metatypes
		     (emit-slot-access reader/writer class-slot-p
				       'slots 'index 'miss-fn arglist)
		     `(funcall miss-fn ,@arglist)
		     (when cached-index-p 'index)
		     (unless class-slot-p '(slots)))))))

(defmacro emit-one-or-n-index-reader/writer-macro
    (reader/writer cached-index-p class-slot-p)
  (let ((*emit-function-p* nil)
	(*precompiling-lap* t))
    (values
     (emit-one-or-n-index-reader/writer reader/writer cached-index-p class-slot-p))))

(defun emit-miss (miss-fn args &optional applyp)
  (let ((restl (when applyp '(.lap-rest-arg.))))
    (if restl
	`(apply ,miss-fn ,@args ,@restl)
	`(funcall ,miss-fn ,@args ,@restl))))

(defun emit-checking-or-caching (cached-emf-p return-value-p metatypes applyp)
  (when (and (null *precompiling-lap*) *emit-function-p*)
    (return-from emit-checking-or-caching
      (emit-checking-or-caching-function
       cached-emf-p return-value-p metatypes applyp)))
  (let* ((dlap-lambda-list (make-dlap-lambda-list metatypes applyp))
	 (args (remove '&rest dlap-lambda-list))
	 (restl (when applyp '(.lap-rest-arg.))))
    (generating-lisp `(cache ,@(unless cached-emf-p '(emf)) miss-fn)
		     dlap-lambda-list
      `(let (,@(when cached-emf-p '(emf)))
	 ,(emit-dlap args
		     metatypes
		     (if return-value-p
			 (if cached-emf-p 'emf t)
			 `(invoke-effective-method-function emf ,applyp
			   ,@args ,@restl))
		     (emit-miss 'miss-fn args applyp)
		     (when cached-emf-p 'emf))))))

(defmacro emit-checking-or-caching-macro (cached-emf-p return-value-p metatypes applyp)
  (let ((*emit-function-p* nil)
	(*precompiling-lap* t))
    (values
     (emit-checking-or-caching cached-emf-p return-value-p metatypes applyp))))

(defun emit-dlap (args metatypes hit miss value-reg &optional slot-regs)
  (let* ((index -1)
	 (wrapper-bindings (mapcan #'(lambda (arg mt)
				       (unless (eq mt 't)
					 (incf index)
					 `((,(intern (format nil
							     "WRAPPER-~D"
							     index)
						     *pcl-package*)
					    ,(emit-fetch-wrapper mt arg 'miss
					      (pop slot-regs))))))
				   args metatypes))
	 (wrappers (mapcar #'car wrapper-bindings)))
    (declare (fixnum index))
    (unless wrappers (error "Every metatype is T."))
    `(block dfun
       (tagbody
	  (let ((field (cache-field cache))
		(cache-vector (cache-vector cache))
		(mask (cache-mask cache))
		(size (cache-size cache))
		(overflow (cache-overflow cache))
		,@wrapper-bindings)
	    (declare (fixnum size field mask))
	    ,(cond ((cdr wrappers)
		    (emit-greater-than-1-dlap wrappers 'miss value-reg))
		   (value-reg
		    (emit-1-t-dlap (car wrappers) 'miss value-reg))
		   (t
		    (emit-1-nil-dlap (car wrappers) 'miss)))
	    (return-from dfun ,hit))
	miss
	  (return-from dfun ,miss)))))

(defun emit-1-nil-dlap (wrapper miss-label)
  `(let* ((primary ,(emit-1-wrapper-compute-primary-cache-location wrapper miss-label))
	  (location primary))
     (declare (fixnum primary location))
     (block search
       (loop (when (eq ,wrapper (cache-vector-ref cache-vector location))
	       (return-from search nil))
	     (setq location (the fixnum (+ location 1)))
	     (when (= location size)
	       (setq location 0))
	     (when (= location primary)
	       (dolist (entry overflow)
		 (when (eq (car entry) ,wrapper)
		   (return-from search nil)))
	       (go ,miss-label))))))

(defmacro get-cache-vector-lock-count (cache-vector)
  `(let ((lock-count (cache-vector-lock-count ,cache-vector)))
     (unless (typep lock-count 'fixnum)
       (error "My cache got freed somehow."))
     (the fixnum lock-count)))

(defun emit-1-t-dlap (wrapper miss-label value)
  `(let ((primary ,(emit-1-wrapper-compute-primary-cache-location wrapper miss-label))
	 (initial-lock-count (get-cache-vector-lock-count cache-vector)))
     (declare (fixnum primary initial-lock-count))
     (let ((location primary))
       (declare (fixnum location))
       (block search
	 (loop (when (eq ,wrapper (cache-vector-ref cache-vector location))
		 (setq ,value (cache-vector-ref cache-vector (1+ location)))
		 (return-from search nil))
	       (setq location (the fixnum (+ location 2)))
	       (when (= location size)
		 (setq location 0))
	       (when (= location primary)
		 (dolist (entry overflow)
		   (when (eq (car entry) ,wrapper)
		     (setq ,value (cdr entry))
		     (return-from search nil)))
		 (go ,miss-label))))
       (unless (= initial-lock-count
		  (get-cache-vector-lock-count cache-vector))
	 (go ,miss-label)))))

(defun emit-greater-than-1-dlap (wrappers miss-label value)
  (declare (type list wrappers))
  (let ((cache-line-size (compute-line-size (+ (length wrappers) (if value 1 0)))))
    `(let ((primary 0) (size-1 (the fixnum (- size 1))))
       (declare (fixnum primary size-1))
       ,(emit-n-wrapper-compute-primary-cache-location wrappers miss-label)
       (let ((initial-lock-count (get-cache-vector-lock-count cache-vector)))
	 (declare (fixnum initial-lock-count))
	 (let ((location primary) (next-location 0))
	   (declare (fixnum location next-location))
	   (block search
	     (loop (setq next-location (the fixnum (+ location ,cache-line-size)))
		   (when (and ,@(mapcar
				 #'(lambda (wrapper)
				     `(eq ,wrapper
				       (cache-vector-ref cache-vector
					(setq location
					 (the fixnum (+ location 1))))))
				 wrappers))
		     ,@(when value
			 `((setq location (the fixnum (+ location 1)))
			   (setq ,value (cache-vector-ref cache-vector location))))
		     (return-from search nil))
		   (setq location next-location)
		   (when (= location size-1)
		     (setq location 0))
		   (when (= location primary)
		     (dolist (entry overflow)
		       (let ((entry-wrappers (car entry)))
			 (when (and ,@(mapcar #'(lambda (wrapper)
						  `(eq ,wrapper (pop entry-wrappers)))
					      wrappers))
			   ,@(when value
			       `((setq ,value (cdr entry))))
			   (return-from search nil))))
		     (go ,miss-label))))
	   (unless (= initial-lock-count
		      (get-cache-vector-lock-count cache-vector))
	     (go ,miss-label)))))))

(defun emit-1-wrapper-compute-primary-cache-location (wrapper miss-label)
  `(let ((wrapper-cache-no (wrapper-cache-number-vector-ref ,wrapper field)))
     (declare (fixnum wrapper-cache-no))
     (when (zerop wrapper-cache-no) (go ,miss-label))
     ,(let ((form `(logand mask wrapper-cache-no)))
	`(the fixnum ,form))))

(defun emit-n-wrapper-compute-primary-cache-location (wrappers miss-label)
  (declare (type list wrappers))
  ;; This returns 1 less that the actual location.
  `(progn
     ,@(let ((adds 0) (len (length wrappers)))
	 (declare (fixnum adds len))
	 (mapcar #'(lambda (wrapper)
		     `(let ((wrapper-cache-no (wrapper-cache-number-vector-ref
					       ,wrapper field)))
			(declare (fixnum wrapper-cache-no))
			(when (zerop wrapper-cache-no) (go ,miss-label))
			(setq primary (the fixnum (+ primary wrapper-cache-no)))
			,@(progn
			    (incf adds)
			    (when (or (zerop (mod adds wrapper-cache-number-adds-ok))
				      (eql adds len))
			      `((setq primary
				      ,(let ((form `(logand primary mask)))
					 `(the fixnum ,form))))))))
		 wrappers))))

;;; CMU17 (and SBCL) note: Since STD-INSTANCE-P is weakened in the CMU/SBCL
;;; approach of using funcallable instances, that branch may run
;;; on non-pcl instances (structures). The result will be the
;;; non-wrapper layout for the structure, which will cause a miss. The "slots"
;;; will be whatever the first slot is, but will be ignored. Similarly,
;;; fsc-instance-p returns true on funcallable structures as well as PCL fins.
(defun emit-fetch-wrapper (metatype argument miss-label &optional slot)
  (ecase metatype
    ((standard-instance) 
     `(cond ((std-instance-p ,argument)
	     ,@(when slot `((setq ,slot (std-instance-slots ,argument))))
	     (std-instance-wrapper ,argument))
	    ((fsc-instance-p ,argument)
	     ,@(when slot `((setq ,slot (fsc-instance-slots ,argument))))
	     (fsc-instance-wrapper ,argument))
	    (t
	     (go ,miss-label))))
    (class
     (when slot (error "can't do a slot reg for this metatype"))
     `(wrapper-of-macro ,argument))
    ((built-in-instance structure-instance)
     (when slot (error "can't do a slot reg for this metatype"))
     `(built-in-or-structure-wrapper
       ,argument))))

