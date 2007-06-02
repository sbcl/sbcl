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
(defun parse-method-or-spec (spec &optional (errorp t))
  (let (gf method name temp)
    (if (method-p spec)
        (setq method spec
              gf (method-generic-function method)
              temp (and gf (generic-function-name gf))
              name (if temp
                       (make-method-spec temp
                                         (method-qualifiers method)
                                         (unparse-specializers
                                          (method-specializers method)))
                       (make-symbol (format nil "~S" method))))
        (multiple-value-bind (gf-spec quals specls)
            (parse-defmethod spec)
          (and (setq gf (and (or errorp (fboundp gf-spec))
                             (gdefinition gf-spec)))
               (let ((nreq (compute-discriminating-function-arglist-info gf)))
                 (setq specls (append (parse-specializers specls)
                                      (make-list (- nreq (length specls))
                                                 :initial-element
                                                 *the-class-t*)))
                 (and
                   (setq method (get-method gf quals specls errorp))
                   (setq name
                         (make-method-spec
                          gf-spec quals (unparse-specializers specls))))))))
    (values gf method name)))

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

#|
;;;; Helper for slightly newer trace implementation, based on
;;;; breakpoint stuff.  The above is potentially still useful, so it's
;;;; left in, commented.

;;; (this turned out to be a roundabout way of doing things)
(defun list-all-maybe-method-names (gf)
  (let (result)
    (dolist (method (generic-function-methods gf) (nreverse result))
      (let ((spec (nth-value 2 (parse-method-or-spec method))))
        (push spec result)
        (push (list* 'fast-method (cdr spec)) result)))))
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
  (let ((pname (classoid-proper-name
                (layout-classoid object))))
    (unless pname
      (error "can't dump wrapper for anonymous class:~%  ~S"
             (layout-classoid object)))
    `(classoid-layout (find-classoid ',pname))))

(defmethod make-load-form ((object structure-object) &optional env)
  (declare (ignore env))
  (error "~@<don't know how to dump ~S (default ~S method called).~@>"
         object 'make-load-form))

(defmethod make-load-form ((object standard-object) &optional env)
  (declare (ignore env))
  (error "~@<don't know how to dump ~S (default ~S method called).~@>"
         object 'make-load-form))

(defmethod make-load-form ((object condition) &optional env)
  (declare (ignore env))
  (error "~@<don't know how to dump ~S (default ~S method called).~@>"
         object 'make-load-form))

(defun make-load-form-saving-slots (object &key (slot-names nil slot-names-p) environment)
  (declare (ignore environment))
  (let ((class (class-of object)))
    (collect ((inits))
      (dolist (slot (class-slots class))
        (let ((slot-name (slot-definition-name slot)))
          (when (or (memq slot-name slot-names)
                    (and (not slot-names-p)
                         (eq :instance (slot-definition-allocation slot))))
            (if (slot-boundp-using-class class object slot)
                (let ((value (slot-value-using-class class object slot)))
                  (if (typep object 'structure-object)
                      ;; low-level but less noisy initializer form
                      ;; FIXME: why not go class->layout->info == dd?
                      (let* ((dd (find-defstruct-description
                                  (class-name class)))
                             (dsd (find slot-name (dd-slots dd)
                                        :key #'dsd-name)))
                        (inits `(,(slot-setter-lambda-form dd dsd)
                                 ',value ,object)))
                      (inits `(setf (slot-value ,object ',slot-name) ',value))))
                (inits `(slot-makunbound ,object ',slot-name))))))
      (values `(allocate-instance (find-class ',(class-name class)))
              `(progn ,@(inits))))))
