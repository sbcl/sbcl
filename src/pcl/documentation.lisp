;;;; implementation of CL:DOCUMENTATION

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is in the public domain and is provided with absolutely no
;;;; warranty. See the COPYING and CREDITS files for more information.

(in-package "SB-PCL")

;;; FIXME: Lots of bare calls to INFO here could be handled
;;; more cleanly by calling the FDOCUMENTATION function instead.

;;; functions, macros, and special forms
(defmethod documentation ((x function) (doc-type (eql 't)))
  (if (typep x 'generic-function)
      (slot-value x 'documentation)
      (%fun-doc x)))

(defmethod documentation ((x function) (doc-type (eql 'function)))
  (if (typep x 'generic-function)
      (slot-value x 'documentation)
      (%fun-doc x)))

(defmethod documentation ((x list) (doc-type (eql 'function)))
  (and (legal-fun-name-p x)
       (fboundp x)
       (documentation (fdefinition x) t)))

(defmethod documentation ((x list) (doc-type (eql 'compiler-macro)))
  (random-documentation x 'compiler-macro))

(defmethod documentation ((x symbol) (doc-type (eql 'function)))
  (or (values (info :function :documentation x))
      ;; Try the pcl function documentation.
      (and (fboundp x) (documentation (fdefinition x) t))))

(defmethod documentation ((x symbol) (doc-type (eql 'compiler-macro)))
  (random-documentation x 'compiler-macro))

(defmethod documentation ((x symbol) (doc-type (eql 'setf)))
  (values (info :setf :documentation x)))

(defmethod (setf documentation) (new-value (x function) (doc-type (eql 't)))
  (if (typep x 'generic-function)
      (setf (slot-value x 'documentation) new-value)
      (let ((name (%fun-name x)))
	(when (and name (typep name '(or symbol cons)))
	  (setf (info :function :documentation name) new-value))))
  new-value)

(defmethod (setf documentation)
    (new-value (x function) (doc-type (eql 'function)))
  (if (typep x 'generic-function)
      (setf (slot-value x 'documentation) new-value)
      (let ((name (%fun-name x)))
	(when (and name (typep name '(or symbol cons)))
	  (setf (info :function :documentation name) new-value))))
  new-value)

(defmethod (setf documentation) (new-value (x list) (doc-type (eql 'function)))
  (setf (info :function :documentation x) new-value))

(defmethod (setf documentation)
    (new-value (x list) (doc-type (eql 'compiler-macro)))
  (setf (random-documentation x 'compiler-macro) new-value))

(defmethod (setf documentation) (new-value
				 (x symbol)
				 (doc-type (eql 'function)))
  (setf (info :function :documentation x) new-value))

(defmethod (setf documentation)
    (new-value (x symbol) (doc-type (eql 'compiler-macro)))
  (setf (random-documentation x 'compiler-macro) new-value))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'setf)))
  (setf (info :setf :documentation x) new-value))

;;; method combinations
(defmethod documentation ((x method-combination) (doc-type (eql 't)))
  (slot-value x 'documentation))

(defmethod documentation
    ((x method-combination) (doc-type (eql 'method-combination)))
  (slot-value x 'documentation))

(defmethod documentation ((x symbol) (doc-type (eql 'method-combination)))
  (random-documentation x 'method-combination))

(defmethod (setf documentation)
    (new-value (x method-combination) (doc-type (eql 't)))
  (setf (slot-value x 'documentation) new-value))

(defmethod (setf documentation)
    (new-value (x method-combination) (doc-type (eql 'method-combination)))
  (setf (slot-value x 'documentation) new-value))

(defmethod (setf documentation)
    (new-value (x symbol) (doc-type (eql 'method-combination)))
  (setf (random-documentation x 'method-combination) new-value))

;;; methods
(defmethod documentation ((method standard-method) (doc-type (eql 't)))
  (slot-value slotd 'documentation))

(defmethod (setf documentation)
    (new-value (method standard-method) (doc-type (eql 't)))
  (setf (slot-value method 'documentation) new-value))

;;; packages

;;; KLUDGE: It's nasty having things like this accessor
;;; (PACKAGE-DOC-STRING) floating around out in this mostly-unrelated
;;; source file. Perhaps it would be better to support WARM-INIT-FORMS
;;; by analogy with the existing !COLD-INIT-FORMS and have them be
;;; EVAL'ed after basic warm load is done? That way things like this
;;; could be defined alongside the other code which does low-level
;;; hacking of packages.. -- WHN 19991203

(defmethod documentation ((x package) (doc-type (eql 't)))
  (package-doc-string x))

(defmethod (setf documentation) (new-value (x package) (doc-type (eql 't)))
  (setf (package-doc-string x) new-value))

;;; types, classes, and structure names
(defmethod documentation ((x structure-class) (doc-type (eql 't)))
  (values (info :type :documentation (class-name x))))

(defmethod documentation ((x structure-class) (doc-type (eql 'type)))
  (values (info :type :documentation (class-name x))))

(defmethod documentation ((x standard-class) (doc-type (eql 't)))
  (slot-value x 'documentation))

(defmethod documentation ((x standard-class) (doc-type (eql 'type)))
  (slot-value x 'documentation))

(defmethod documentation ((x symbol) (doc-type (eql 'type)))
  (or (values (info :type :documentation x))
      (let ((class (find-class x nil)))
	(when class
	  (slot-value class 'documentation)))))

(defmethod documentation ((x symbol) (doc-type (eql 'structure)))
  (when (eq (info :type :kind x) :instance)
    (values (info :type :documentation x))))

(defmethod (setf documentation) (new-value
				 (x structure-class)
				 (doc-type (eql 't)))
  (setf (info :type :documentation (class-name x)) new-value))

(defmethod (setf documentation) (new-value
				 (x structure-class)
				 (doc-type (eql 'type)))
  (setf (info :type :documentation (class-name x)) new-value))

(defmethod (setf documentation) (new-value
				 (x standard-class)
				 (doc-type (eql 't)))
  (setf (slot-value x 'documentation) new-value))

(defmethod (setf documentation) (new-value
				 (x standard-class)
				 (doc-type (eql 'type)))
  (setf (slot-value x 'documentation) new-value))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'type)))
  (if (or (structure-type-p x) (condition-type-p x))
      (setf (info :type :documentation x) new-value)
      (let ((class (find-class x nil)))
	(if class
	    (setf (slot-value class 'documentation) new-value)
	    (setf (info :type :documentation x) new-value)))))

(defmethod (setf documentation) (new-value
				 (x symbol)
				 (doc-type (eql 'structure)))
  (unless (eq (info :type :kind x) :instance)
    (error "~S is not the name of a structure type." x))
  (setf (info :type :documentation x) new-value))

;;; variables
(defmethod documentation ((x symbol) (doc-type (eql 'variable)))
  (values (info :variable :documentation x)))

(defmethod (setf documentation) (new-value
				 (x symbol)
				 (doc-type (eql 'variable)))
  (setf (info :variable :documentation x) new-value))

;;; default if DOC-TYPE doesn't match one of the specified types
(defmethod documentation (object doc-type)
  (warn "unsupported DOCUMENTATION: type ~S for object ~S"
	doc-type
	(type-of object))
  nil)

;;; default if DOC-TYPE doesn't match one of the specified types
(defmethod (setf documentation) (new-value object doc-type)
  ;; CMU CL made this an error, but since ANSI says that even for supported
  ;; doc types an implementation is permitted to discard docs at any time
  ;; for any reason, this feels to me more like a warning. -- WHN 19991214
  (warn "discarding unsupported DOCUMENTATION of type ~S for object ~S"
	doc-type
	(type-of object))
  new-value)

;;; extra-standard methods, for getting at slot documentation
(defmethod documentation ((slotd standard-slot-definition) (doc-type (eql 't)))
  (declare (ignore doc-type))
  (slot-value slotd 'documentation))

(defmethod (setf documentation)
    (new-value (slotd standard-slot-definition) (doc-type (eql 't)))
  (declare (ignore doc-type))
  (setf (slot-value slotd 'documentation) new-value))

;;; Now that we have created the machinery for setting documentation, we can
;;; set the documentation for the machinery for setting documentation.
#+sb-doc
(setf (documentation 'documentation 'function)
      "Return the documentation string of Doc-Type for X, or NIL if
  none exists. System doc-types are VARIABLE, FUNCTION, STRUCTURE, TYPE,
  SETF, and T.")
