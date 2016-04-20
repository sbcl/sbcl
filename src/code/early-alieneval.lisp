;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!ALIEN")

(defvar *alien-type-classes* (make-hash-table :test 'eq))

(defvar *new-auxiliary-types* nil)

;;; the list of record types that have already been unparsed. This is
;;; used to keep from outputting the slots again if the same structure
;;; shows up twice.
(defvar *record-types-already-unparsed*)

;;; not documented in CMU CL:-(
;;;
;;; reverse engineering observations:
;;;   * seems to be set when translating return values
;;;   * seems to enable the translation of (VALUES), which is the
;;;     Lisp idiom for C's return type "void" (which is likely
;;;     why it's set when when translating return values)
(defvar *values-type-okay* nil)

(defvar *default-c-string-external-format* nil)

(defmacro define-alien-type-translator (name lambda-list &body body)
  (let ((defun-name (symbolicate "ALIEN-" name "-TYPE-TRANSLATOR")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (symbol-function ',defun-name)
             ,(make-macro-lambda defun-name lambda-list body
                                 'define-alien-type-translator name))
       (%define-alien-type-translator ',name #',defun-name))))

;;; Process stuff in a new scope.
(defmacro with-auxiliary-alien-types (env &body body)
  ``(symbol-macrolet ((&auxiliary-type-definitions&
                       ,(append *new-auxiliary-types*
                                (auxiliary-type-definitions ,env))))
      ,(let ((*new-auxiliary-types* nil))
         ,@body)))

(defmacro define-alien-type (name type &environment env)
  #!+sb-doc
  "Define the alien type NAME to be equivalent to TYPE. Name may be NIL for
   STRUCT and UNION types, in which case the name is taken from the type
   specifier."
  (with-auxiliary-alien-types env
    (let ((alien-type (parse-alien-type type env)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         ,@(when *new-auxiliary-types*
             `((%def-auxiliary-alien-types ',*new-auxiliary-types*
                                           (sb!c:source-location))))
         ,@(when name
             `((%define-alien-type ',name ',alien-type)
               (setf (info :source-location :alien-type ',name)
                     (sb!c:source-location))))))))

(defstruct (alien-type-class (:copier nil))
  (name nil :type symbol)
  (defstruct-name nil :type symbol)
  (include nil :type (or null alien-type-class))
  (unparse nil :type (or null function))
  (type= nil :type (or null function))
  (lisp-rep nil :type (or null function))
  (alien-rep nil :type (or null function))
  (extract-gen nil :type (or null function))
  (deposit-gen nil :type (or null function))
  (naturalize-gen nil :type (or null function))
  (deport-gen nil :type (or null function))
  (deport-alloc-gen nil :type (or null function))
  (deport-pin-p nil :type (or null function))
  ;; Cast?
  (arg-tn nil :type (or null function))
  (result-tn nil :type (or null function))
  (subtypep nil :type (or null function)))

(defmethod print-object ((type-class alien-type-class) stream)
  (print-unreadable-object (type-class stream :type t)
    (prin1 (alien-type-class-name type-class) stream)))

(defun alien-type-class-or-lose (name)
  (or (gethash name *alien-type-classes*)
      (error "no alien type class ~S" name)))

(defun create-alien-type-class-if-necessary (name defstruct-name include)
  (let ((old (gethash name *alien-type-classes*))
        (include (and include (alien-type-class-or-lose include))))
    (if old
        (setf (alien-type-class-include old) include)
        (setf (gethash name *alien-type-classes*)
              (make-alien-type-class :name name
                                     :defstruct-name defstruct-name
                                     :include include)))))

(defconstant-eqx +method-slot-alist+
  '((:unparse . alien-type-class-unparse)
    (:type= . alien-type-class-type=)
    (:subtypep . alien-type-class-subtypep)
    (:lisp-rep . alien-type-class-lisp-rep)
    (:alien-rep . alien-type-class-alien-rep)
    (:extract-gen . alien-type-class-extract-gen)
    (:deposit-gen . alien-type-class-deposit-gen)
    (:naturalize-gen . alien-type-class-naturalize-gen)
    (:deport-gen . alien-type-class-deport-gen)
    (:deport-alloc-gen . alien-type-class-deport-alloc-gen)
    (:deport-pin-p . alien-type-class-deport-pin-p)
    ;; cast?
    (:arg-tn . alien-type-class-arg-tn)
    (:result-tn . alien-type-class-result-tn))
  #'equal)

(defun method-slot (method)
  (cdr (or (assoc method +method-slot-alist+)
           (error "no method ~S" method))))

(defmacro invoke-alien-type-method (method type &rest args)
  (let ((slot (method-slot method)))
    (once-only ((type type))
      `(funcall (do ((class (alien-type-class-or-lose (alien-type-class ,type))
                            (alien-type-class-include class)))
                    ((null class)
                     (error "method ~S not defined for ~S"
                            ',method (alien-type-class ,type)))
                  (let ((fn (,slot class)))
                    (when fn
                      (return fn))))
                ,type ,@args))))

#+sb-xc
(defmacro maybe-with-pinned-objects (variables types &body body)
  (declare (ignorable variables types))
  (let ((pin-variables
         ;; Only pin things on GENCGC, since on CHENEYGC it'd imply
         ;; disabling the GC.  Which is something we don't want to do
         ;; every time we're calling to C.
         #!+gencgc
         (loop for variable in variables
            for type in types
            when (invoke-alien-type-method :deport-pin-p type)
            collect variable)))
    (if pin-variables
        `(with-pinned-objects ,pin-variables
           ,@body)
        `(progn
           ,@body))))
