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

;;; GET-FUNCTION is the main user interface to this code. It is like
;;; COMPILE, only more efficient. It achieves this efficiency by
;;; reducing the number of times that the compiler needs to be called.
;;; Calls to GET-FUNCTION in which the lambda forms differ only by constants
;;; can use the same piece of compiled code. (For example, dispatch dfuns and
;;; combined method functions can often be shared, if they differ only
;;; by referring to different methods.)
;;;
;;; If GET-FUNCTION is called with a lambda expression only, it will return
;;; a corresponding function. The optional constant-converter argument
;;; can be a function which will be called to convert each constant appearing
;;; in the lambda to whatever value should appear in the function.
;;;
;;; There are three internal functions which operate on the lambda argument
;;; to GET-FUNCTION:
;;;   compute-test converts the lambda into a key to be used for lookup,
;;;   compute-code is used by get-new-function-generator-internal to
;;;		generate the actual lambda to be compiled, and
;;;   compute-constants is used to generate the argument list that is
;;;		to be passed to the compiled function.
;;;
(defun get-function (lambda
		      &optional (test-converter     #'default-test-converter)
				(code-converter     #'default-code-converter)
				(constant-converter #'default-constant-converter))
  (function-apply (get-function-generator lambda test-converter code-converter)
		  (compute-constants      lambda constant-converter)))

(defun get-function1 (lambda
		      &optional (test-converter     #'default-test-converter)
				(code-converter     #'default-code-converter)
				(constant-converter #'default-constant-converter))
  (values (the function (get-function-generator lambda test-converter code-converter))
	  (compute-constants      lambda constant-converter)))

(defun default-constantp (form)
  (and (constantp form)
       (not (typep (eval form) '(or symbol fixnum)))))

(defun default-test-converter (form)
  (if (default-constantp form)
      '.constant.
      form))

(defun default-code-converter  (form)
  (if (default-constantp form)
      (let ((gensym (gensym))) (values gensym (list gensym)))
      form))

(defun default-constant-converter (form)
  (if (default-constantp form)
      (list (eval form))
      nil))

;;; *FGENS* is a list of all the function generators we have so far. Each
;;; element is a FGEN structure as implemented below. Don't ever touch this
;;; list by hand, use STORE-FGEN.
(defvar *fgens* ())

(defun store-fgen (fgen)
  (let ((old (lookup-fgen (fgen-test fgen))))
    (if old
	(setf (svref old 2) (fgen-generator fgen)
	      (svref old 4) (or (svref old 4)
				(fgen-system fgen)))
	(setq *fgens* (nconc *fgens* (list fgen))))))

(defun lookup-fgen (test)
  (find test (the list *fgens*) :key #'fgen-test :test #'equal))

(defun make-fgen (test gensyms generator generator-lambda system)
  (let ((new (make-array 6)))
    (setf (svref new 0) test
	  (svref new 1) gensyms
	  (svref new 2) generator
	  (svref new 3) generator-lambda
	  (svref new 4) system)
    new))

(defun fgen-test	     (fgen) (svref fgen 0))
(defun fgen-gensyms	     (fgen) (svref fgen 1))
(defun fgen-generator	     (fgen) (svref fgen 2))
(defun fgen-generator-lambda (fgen) (svref fgen 3))
(defun fgen-system	     (fgen) (svref fgen 4))

(defun get-function-generator (lambda test-converter code-converter)
  (let* ((test (compute-test lambda test-converter))
	 (fgen (lookup-fgen test)))
    (if fgen
	(fgen-generator fgen)
	(get-new-function-generator lambda test code-converter))))

(defun get-new-function-generator (lambda test code-converter)
  (multiple-value-bind (gensyms generator-lambda)
      (get-new-function-generator-internal lambda code-converter)
    (let* ((generator (compile nil generator-lambda))
	   (fgen (make-fgen test gensyms generator generator-lambda nil)))
      (store-fgen fgen)
      generator)))

(defun get-new-function-generator-internal (lambda code-converter)
  (multiple-value-bind (code gensyms)
      (compute-code lambda code-converter)
    (values gensyms `(lambda ,gensyms (function ,code)))))

(defun compute-test (lambda test-converter)
  (let ((*walk-form-expand-macros-p* t))
    (walk-form lambda
	       nil
	       #'(lambda (f c e)
		   (declare (ignore e))
		   (if (neq c :eval)
		       f
		       (let ((converted (funcall test-converter f)))
			 (values converted (neq converted f))))))))

(defun compute-code (lambda code-converter)
  (let ((*walk-form-expand-macros-p* t)
	(gensyms ()))
    (values (walk-form lambda
		       nil
		       #'(lambda (f c e)
			   (declare (ignore e))
			   (if (neq c :eval)
			       f
			       (multiple-value-bind (converted gens)
				   (funcall code-converter f)
				 (when gens (setq gensyms (append gensyms gens)))
				 (values converted (neq converted f))))))
	      gensyms)))

(defun compute-constants (lambda constant-converter)
  (let ((*walk-form-expand-macros-p* t)) ; doesn't matter here.
    (macrolet ((appending ()
		 `(let ((result ()))
		   (values #'(lambda (value) (setq result (append result value)))
		    #'(lambda ()result)))))
      (gathering1 (appending)
		  (walk-form lambda
			     nil
			     #'(lambda (f c e)
				 (declare (ignore e))
				 (if (neq c :eval)
				     f
				     (let ((consts (funcall constant-converter f)))
				       (if consts
					   (progn (gather1 consts) (values f t))
					   f)))))))))

(defmacro precompile-function-generators (&optional system)
  `(progn
    ,@(gathering1 (collecting)
                  (dolist (fgen *fgens*)
                    (when (or (null (fgen-system fgen))
                              (eq (fgen-system fgen) system))
                      (when system (setf (svref fgen 4) system))
                      (gather1
                       `(load-function-generator
                         ',(fgen-test fgen)
                         ',(fgen-gensyms fgen)
                         (function ,(fgen-generator-lambda fgen))
                         ',(fgen-generator-lambda fgen)
                         ',system)))))))

(defun load-function-generator (test gensyms generator generator-lambda system)
  (store-fgen (make-fgen test gensyms generator generator-lambda system)))

