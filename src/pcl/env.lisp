;;;; basic environmental stuff

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

;;; FIXME: This stuff isn't part of the ANSI spec, and isn't even
;;; exported from PCL, but it looks as though it might be useful,
;;; so I don't want to just delete it. Perhaps it should go in
;;; a "contrib" directory eventually?

#|
;;; TRACE-METHOD and UNTRACE-METHOD accept method specs as arguments. A
;;; method-spec should be a list like:
;;;   (<generic-function-spec> qualifiers* (specializers*))
;;; where <generic-function-spec> should be either a symbol or a list
;;; of (SETF <symbol>).
;;;
;;;   For example, to trace the method defined by:
;;;
;;;     (defmethod foo ((x spaceship)) 'ss)
;;;
;;;   You should say:
;;;
;;;     (trace-method '(foo (spaceship)))
;;;
;;;   You can also provide a method object in the place of the method
;;;   spec, in which case that method object will be traced.
;;;
;;; For UNTRACE-METHOD, if an argument is given, that method is untraced.
;;; If no argument is given, all traced methods are untraced.
(defclass traced-method (method)
     ((method :initarg :method)
      (function :initarg :function
		:reader method-function)
      (generic-function :initform nil
			:accessor method-generic-function)))

(defmethod method-lambda-list ((m traced-method))
  (with-slots (method) m (method-lambda-list method)))

(defmethod method-specializers ((m traced-method))
  (with-slots (method) m (method-specializers method)))

(defmethod method-qualifiers ((m traced-method))
  (with-slots (method) m (method-qualifiers method)))

(defmethod accessor-method-slot-name ((m traced-method))
  (with-slots (method) m (accessor-method-slot-name method)))

(defvar *traced-methods* ())

(defun trace-method (spec &rest options)
  (multiple-value-bind (gf omethod name)
      (parse-method-or-spec spec)
    (let* ((tfunction (trace-method-internal (method-function omethod)
					     name
					     options))
	   (tmethod (make-instance 'traced-method
				   :method omethod
				   :function tfunction)))
      (remove-method gf omethod)
      (add-method gf tmethod)
      (pushnew tmethod *traced-methods*)
      tmethod)))

(defun untrace-method (&optional spec)
  (flet ((untrace-1 (m)
	   (let ((gf (method-generic-function m)))
	     (when gf
	       (remove-method gf m)
	       (add-method gf (slot-value m 'method))
	       (setq *traced-methods* (remove m *traced-methods*))))))
    (if (not (null spec))
	(multiple-value-bind (gf method)
	    (parse-method-or-spec spec)
	  (declare (ignore gf))
	  (if (memq method *traced-methods*)
	      (untrace-1 method)
	      (error "~S is not a traced method?" method)))
	(dolist (m *traced-methods*) (untrace-1 m)))))

(defun trace-method-internal (ofunction name options)
  (eval `(untrace ,name))
  (setf (fdefinition name) ofunction)
  (eval `(trace ,name ,@options))
  (fdefinition name))
|#

;;;; MAKE-LOAD-FORM

;; Overwrite the old bootstrap non-generic MAKE-LOAD-FORM function with a
;; shiny new generic function.
(fmakunbound 'make-load-form)
(defgeneric make-load-form (object &optional environment))

;; Link bootstrap-time how-to-dump-it information into the shiny new
;; CLOS system.
(defmethod make-load-form ((obj sb-sys:structure!object)
			   &optional (env nil env-p))
  (if env-p
      (sb-sys:structure!object-make-load-form obj env)
      (sb-sys:structure!object-make-load-form obj)))

(defmethod make-load-form ((object wrapper) &optional env)
  (declare (ignore env))
  (let ((pname (sb-kernel:classoid-proper-name
		(sb-kernel:layout-classoid object))))
    (unless pname
      (error "can't dump wrapper for anonymous class:~%  ~S"
	     (sb-kernel:layout-classoid object)))
    `(sb-kernel:classoid-layout (sb-kernel:find-classoid ',pname))))

