;;;; macros, global variable definitions, and other miscellaneous support stuff
;;;; used by the rest of the PCL subsystem

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

(/show "starting pcl/macros.lisp")

(declaim (declaration
	  ;; As of sbcl-0.7.0.6, SBCL actively uses this declaration
	  ;; to propagate information needed to set up nice debug
	  ;; names (as seen e.g. in BACKTRACE) for method functions.
	  %method-name
	  ;; These nonstandard declarations seem to be used privately
	  ;; within PCL itself to pass information around, so we can't
	  ;; just delete them.
	  %class
	  %method-lambda-list
	  ;; This declaration may also be used within PCL to pass
	  ;; information around, I'm not sure. -- WHN 2000-12-30
	  %variable-rebinding))

(/show "done with DECLAIM DECLARATION")

(defun get-declaration (name declarations &optional default)
  (dolist (d declarations default)
    (dolist (form (cdr d))
      (when (and (consp form) (eq (car form) name))
	(return-from get-declaration (cdr form))))))

(/show "pcl/macros.lisp 85")

(defmacro doplist ((key val) plist &body body)
  `(let ((.plist-tail. ,plist) ,key ,val)
     (loop (when (null .plist-tail.) (return nil))
	   (setq ,key (pop .plist-tail.))
	   (when (null .plist-tail.)
	     (error "malformed plist, odd number of elements"))
	   (setq ,val (pop .plist-tail.))
	   (progn ,@body))))

(/show "pcl/macros.lisp 101")

(defmacro dolist-carefully ((var list improper-list-handler) &body body)
  `(let ((,var nil)
	 (.dolist-carefully. ,list))
     (loop (when (null .dolist-carefully.) (return nil))
	   (if (consp .dolist-carefully.)
	       (progn
		 (setq ,var (pop .dolist-carefully.))
		 ,@body)
	       (,improper-list-handler)))))

;;;; FIND-CLASS
;;;;
;;;; This is documented in the CLOS specification. FIXME: Except that
;;;; SBCL deviates from the spec by having CL:FIND-CLASS distinct from
;;;; PCL:FIND-CLASS, alas.

(/show "pcl/macros.lisp 119")

(defvar *find-class* (make-hash-table :test 'eq))

(defmacro find-class-cell-class (cell)
  `(car ,cell))

(defmacro find-class-cell-predicate (cell)
  `(cadr ,cell))

(defmacro find-class-cell-make-instance-function-keys (cell)
  `(cddr ,cell))

(defmacro make-find-class-cell (class-name)
  (declare (ignore class-name))
  '(list* nil #'constantly-nil nil))

(defun find-class-cell (symbol &optional dont-create-p)
  (or (gethash symbol *find-class*)
      (unless dont-create-p
	(unless (legal-class-name-p symbol)
	  (error "~S is not a legal class name." symbol))
	(setf (gethash symbol *find-class*) (make-find-class-cell symbol)))))

(/show "pcl/macros.lisp 157")

(defvar *create-classes-from-internal-structure-definitions-p* t)

(defun find-class-from-cell (symbol cell &optional (errorp t))
  (or (find-class-cell-class cell)
      (and *create-classes-from-internal-structure-definitions-p*
	   (structure-type-p symbol)
	   (find-structure-class symbol))
      (cond ((null errorp) nil)
	    ((legal-class-name-p symbol)
	     (error "There is no class named ~S." symbol))
	    (t
	     (error "~S is not a legal class name." symbol)))))

(defun find-class-predicate-from-cell (symbol cell &optional (errorp t))
  (unless (find-class-cell-class cell)
    (find-class-from-cell symbol cell errorp))
  (find-class-cell-predicate cell))

(defun legal-class-name-p (x)
  (and (symbolp x)
       (not (keywordp x))))

(defun find-class (symbol &optional (errorp t) environment)
  (declare (ignore environment))
  (find-class-from-cell symbol
			(find-class-cell symbol errorp)
			errorp))

(defun find-class-predicate (symbol &optional (errorp t) environment)
  (declare (ignore environment))
  (find-class-predicate-from-cell symbol
				  (find-class-cell symbol errorp)
				  errorp))

;;; This DEFVAR was originally in defs.lisp, now moved here.
;;;
;;; Possible values are NIL, EARLY, BRAID, or COMPLETE.
;;;
;;; KLUDGE: This should probably become
;;;   (DECLAIM (TYPE (MEMBER NIL :EARLY :BRAID :COMPLETE) *BOOT-STATE*))
(defvar *boot-state* nil)

(/show "pcl/macros.lisp 187")

;;; Note that in SBCL as in CMU CL,
;;;   COMMON-LISP:FIND-CLASS /= SB-PCL:FIND-CLASS.
;;; (Yes, this is a KLUDGE!)
(define-compiler-macro find-class (&whole form
				   symbol &optional (errorp t) environment)
  (declare (ignore environment))
  (if (and (constantp symbol)
	   (legal-class-name-p (eval symbol))
	   (constantp errorp)
	   (member *boot-state* '(braid complete)))
      (let ((symbol (eval symbol))
	    (errorp (not (null (eval errorp))))
	    (class-cell (make-symbol "CLASS-CELL")))	
	`(let ((,class-cell (load-time-value (find-class-cell ',symbol))))
	   (or (find-class-cell-class ,class-cell)
	       ,(if errorp
		    `(find-class-from-cell ',symbol ,class-cell t)
		    `(and (sb-kernel:class-cell-class
			   ',(sb-kernel:find-class-cell symbol))
			  (find-class-from-cell ',symbol ,class-cell nil))))))
      form))

(defun (setf find-class) (new-value symbol)
  (if (legal-class-name-p symbol)
      (let ((cell (find-class-cell symbol)))
	(setf (find-class-cell-class cell) new-value)
	(when (or (eq *boot-state* 'complete)
		  (eq *boot-state* 'braid))
	  (when (and new-value (class-wrapper new-value))
	    (setf (find-class-cell-predicate cell)
		  (fdefinition (class-predicate-name new-value))))
	  (when (and new-value (not (forward-referenced-class-p new-value)))

	    (dolist (keys+aok (find-class-cell-make-instance-function-keys
			       cell))
	      (update-initialize-info-internal
	       (initialize-info new-value (car keys+aok) nil (cdr keys+aok))
	       'make-instance-function))))
	new-value)
      (error "~S is not a legal class name." symbol)))

(/show "pcl/macros.lisp 230")

(defun (setf find-class-predicate)
       (new-value symbol)
  (if (legal-class-name-p symbol)
    (setf (find-class-cell-predicate (find-class-cell symbol)) new-value)
    (error "~S is not a legal class name." symbol)))

(defun find-wrapper (symbol)
  (class-wrapper (find-class symbol)))

(/show "pcl/macros.lisp 241")

(defmacro function-funcall (form &rest args)
  `(funcall (the function ,form) ,@args))

(defmacro function-apply (form &rest args)
  `(apply (the function ,form) ,@args))

(/show "pcl/macros.lisp 249")

(defun get-setf-fun-name (name)
  `(setf ,name))

(defsetf slot-value set-slot-value)

(defun misplaced-lambda-list-keyword (lambda-list keyword)
  (error "Lambda list keyword ~S is misplaced in ~S." keyword lambda-list))

(defmacro process-lambda-list (lambda-list &rest clauses)
  ;; (process-lambda-list '(a b &optional (c 1))
  ;;                      (&required)
  ;;                      ((&optional (print "Started processing optional arguments"))
  ;;                       (format "Optional argument: ~S~%" it))
  ;;                      (&rest (print "Rest")))
  (let ((clauses (loop for clause in clauses
                    collect
                      (cond ((symbolp (car clause))
                             `(,(car clause) nil . ,(cdr clause)))
                            ((consp (car clause))
                             `(,(caar clause) ,(cdar clause) . ,(cdr clause)))
                            (t (error "Invalid clause format: ~S." clause)))))
        (ll (gensym "LL"))
        (state (gensym "STATE"))
        (restp (gensym "RESTP"))
        (check-state (gensym "CHECK-STATE")))
    `(let ((,ll ,lambda-list)
           (,state '&required)
           (,restp nil))
       (dolist (it ,ll)
         (flet ((,check-state (possible)
                  (unless (memq ,state possible)
                    (misplaced-lambda-list-keyword ,ll it))))
           (cond ((memq it lambda-list-keywords)
                  (case it
                    (&optional (,check-state '(&required))
                               ,@(cadr (assoc '&optional clauses)))
                    (&rest (,check-state '(&required &optional))
                           ,@(cadr (assoc '&rest clauses)))
                    (&key (,check-state '(&required &optional &rest))
                          (when (and (eq ,state '&rest)
                                     (not ,restp))
                            (error "Omitted &REST variable in ~S." ,ll))
                          ,@(cadr (assoc '&key clauses)))
                    (&allow-other-keys (,check-state '(&key))
                                       ,@(cadr (assoc '&allow-other-keys clauses)))
                    (&aux (when (and (eq ,state '&rest)
                                     (not ,restp))
                            (error "Omitted &REST variable in ~S." ,ll))
                          ,@(cadr (assoc '&aux clauses)))
                    (t (error "Unsupported lambda list keyword ~S in ~S."
                              it ,ll)))
                  (setq ,state it))
                 (t (case ,state
                      (&required ,@(cddr (assoc '&required clauses)))
                      (&optional ,@(cddr (assoc '&optional clauses)))
                      (&rest (when ,restp
                               (error "Too many variables after &REST in ~S." ,ll))
                             (setq ,restp t)
                             ,@(cddr (assoc '&rest clauses)))
                      (&key ,@(cddr (assoc '&key clauses)))
                      (&allow-other-keys (error "Variable ~S after &ALLOW-OTHER-KEY in ~S."
                                                it ,ll))
                      (&aux ,@(cddr (assoc '&aux clauses))))))))
       (when (and (eq ,state '&rest)
                  (not ,restp))
         (error "Omitted &REST variable in ~S." ,ll)))))

(/show "finished with pcl/macros.lisp")
