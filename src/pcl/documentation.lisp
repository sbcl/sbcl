;;;; implementation of CL:DOCUMENTATION

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is in the public domain and is provided with absolutely no
;;;; warranty. See the COPYING and CREDITS files for more information.

(in-package "SB-PCL")

(defun fun-doc (x)
  (if (typep x 'generic-function)
      (slot-value x '%documentation)
      (%fun-doc x)))

(defun (setf fun-doc) (new-value x)
  (if (typep x 'generic-function)
      (setf (slot-value x '%documentation) new-value)
      (setf (%fun-doc x) new-value)))

;;; functions, macros, and special forms
(defmethod documentation ((x function) (doc-type (eql 't)))
  (fun-doc x))

(defmethod documentation ((x function) (doc-type (eql 'function)))
  (fun-doc x))

(defmethod documentation ((x list) (doc-type (eql 'compiler-macro)))
  (awhen (compiler-macro-function x)
    (documentation it t)))

(defmethod documentation ((x list) (doc-type (eql 'function)))
  (when (legal-fun-name-p x)
    (or (random-documentation x 'function)
        (when (fboundp x)
          (fun-doc (fdefinition x))))))

(defmethod documentation ((x symbol) (doc-type (eql 'function)))
  (when (legal-fun-name-p x)
    (or (random-documentation x 'function)
        ;; Nothing under the name, check the function object.
        (when (fboundp x)
          (fun-doc (or (macro-function x) (fdefinition x)))))))

(defmethod documentation ((x symbol) (doc-type (eql 'compiler-macro)))
  (awhen (compiler-macro-function x)
    (documentation it t)))

(defmethod documentation ((x symbol) (doc-type (eql 'setf)))
  (fdocumentation x 'setf))

(defmethod documentation ((x symbol) (doc-type (eql 'optimize)))
  (random-documentation x 'optimize))

(defmethod (setf documentation) (new-value (x function) (doc-type (eql 't)))
  (setf (fun-doc x) new-value))

(defmethod (setf documentation) (new-value (x function) (doc-type (eql 'function)))
  (setf (fun-doc x) new-value))

(defmethod (setf documentation) (new-value (x list) (doc-type (eql 'function)))
  (when (legal-fun-name-p x)
    (setf (random-documentation x 'function) new-value)))

(defmethod (setf documentation) (new-value (x list) (doc-type (eql 'compiler-macro)))
  (awhen (compiler-macro-function x)
    (setf (documentation it t) new-value)))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'function)))
  (when (legal-fun-name-p x)
    (setf (random-documentation x 'function) new-value)))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'compiler-macro)))
  (awhen (compiler-macro-function x)
    (setf (documentation it t) new-value)))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'setf)))
  (setf (fdocumentation x 'setf) new-value))

;;; method combinations
(defmethod documentation ((x method-combination) (doc-type (eql 't)))
  (slot-value x '%documentation))

(defmethod documentation
    ((x method-combination) (doc-type (eql 'method-combination)))
  (slot-value x '%documentation))

(defmethod documentation ((x symbol) (doc-type (eql 'method-combination)))
  (random-documentation x 'method-combination))

(defmethod (setf documentation)
    (new-value (x method-combination) (doc-type (eql 't)))
  (setf (slot-value x '%documentation) new-value))

(defmethod (setf documentation)
    (new-value (x method-combination) (doc-type (eql 'method-combination)))
  (setf (slot-value x '%documentation) new-value))

(defmethod (setf documentation)
    (new-value (x symbol) (doc-type (eql 'method-combination)))
  (setf (random-documentation x 'method-combination) new-value))

;;; methods
(defmethod documentation ((x standard-method) (doc-type (eql 't)))
  (slot-value x '%documentation))

(defmethod (setf documentation)
    (new-value (x standard-method) (doc-type (eql 't)))
  (setf (slot-value x '%documentation) new-value))

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
  (fdocumentation (class-name x) 'type))

(defmethod documentation ((x structure-class) (doc-type (eql 'type)))
  (fdocumentation (class-name x) 'type))

(defmethod documentation ((x standard-class) (doc-type (eql 't)))
  (slot-value x '%documentation))

(defmethod documentation ((x standard-class) (doc-type (eql 'type)))
  (slot-value x '%documentation))

;;; although the CLHS doesn't mention this, it is reasonable to assume
;;; that parallel treatment of condition-class was intended (if
;;; condition-class is in fact not implemented as a standard-class or
;;; structure-class).
(defmethod documentation ((x condition-class) (doc-type (eql 't)))
  (fdocumentation (class-name x) 'type))

(defmethod documentation ((x condition-class) (doc-type (eql 'type)))
  (fdocumentation (class-name x) 'type))

(defmethod documentation ((x symbol) (doc-type (eql 'type)))
  (or (fdocumentation x 'type)
      (let ((class (find-class x nil)))
        (when class
          (slot-value class '%documentation)))))

(defmethod documentation ((x symbol) (doc-type (eql 'structure)))
  (fdocumentation x 'structure))

(defmethod (setf documentation) (new-value
                                 (x structure-class)
                                 (doc-type (eql 't)))
  (setf (fdocumentation (class-name x) 'type) new-value))

(defmethod (setf documentation) (new-value
                                 (x structure-class)
                                 (doc-type (eql 'type)))
  (setf (fdocumentation (class-name x) 'type) new-value))

(defmethod (setf documentation) (new-value
                                 (x standard-class)
                                 (doc-type (eql 't)))
  (setf (slot-value x '%documentation) new-value))

(defmethod (setf documentation) (new-value
                                 (x standard-class)
                                 (doc-type (eql 'type)))
  (setf (slot-value x '%documentation) new-value))

(defmethod (setf documentation) (new-value
                                 (x condition-class)
                                 (doc-type (eql 't)))
  (setf (fdocumentation (class-name x) 'type) new-value))

(defmethod (setf documentation) (new-value
                                 (x condition-class)
                                 (doc-type (eql 'type)))
  (setf (fdocumentation (class-name x) 'type) new-value))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'type)))
  (if (or (structure-type-p x) (condition-type-p x))
      (setf (fdocumentation x 'type) new-value)
      (let ((class (find-class x nil)))
        (if class
            (setf (slot-value class '%documentation) new-value)
            (setf (fdocumentation x 'type) new-value)))))

(defmethod (setf documentation) (new-value
                                 (x symbol)
                                 (doc-type (eql 'structure)))
  (setf (fdocumentation x 'structure) new-value))

;;; variables
(defmethod documentation ((x symbol) (doc-type (eql 'variable)))
  (fdocumentation x 'variable))

(defmethod (setf documentation) (new-value
                                 (x symbol)
                                 (doc-type (eql 'variable)))
  (setf (fdocumentation x 'variable) new-value))

;;; default if DOC-TYPE doesn't match one of the specified types
(defmethod documentation (object doc-type)
  (warn "unsupported DOCUMENTATION: type ~S for object of type ~S"
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
  (slot-value slotd '%documentation))

(defmethod (setf documentation)
    (new-value (slotd standard-slot-definition) (doc-type (eql 't)))
  (declare (ignore doc-type))
  (setf (slot-value slotd '%documentation) new-value))

;;; Now that we have created the machinery for setting documentation, we can
;;; set the documentation for the machinery for setting documentation.
#+sb-doc
(setf (documentation 'documentation 'function)
      "Return the documentation string of Doc-Type for X, or NIL if none
exists. System doc-types are VARIABLE, FUNCTION, STRUCTURE, TYPE, SETF, and T.

Function documentation is stored separately for function names and objects:
DEFUN, LAMBDA, &co create function objects with the specified documentation
strings.

 \(SETF (DOCUMENTATION NAME 'FUNCTION) STRING)

sets the documentation string stored under the specified name, and

 \(SETF (DOCUMENTATION FUNC T) STRING)

sets the documentation string stored in the function object.

 \(DOCUMENTATION NAME 'FUNCTION)

returns the documentation stored under the function name if any, and
falls back on the documentation in the function object if necessary.")
