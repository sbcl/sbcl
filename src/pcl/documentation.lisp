;;;; implementation of CL:DOCUMENTATION

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is in the public domain and is provided with absolutely no
;;;; warranty. See the COPYING and CREDITS files for more information.

(in-package "SB-PCL")

;;; Note some cases are handled by the documentation methods in
;;; std-class.lisp.
;;; FIXME: Those should probably be moved into this file too.

;;; FIXME: Lots of bare calls to INFO here could be handled
;;; more cleanly by calling the FDOCUMENTATION function instead.

;;; FIXME: Neither SBCL nor Debian CMU CL 2.4.17 handles
;;;   (DEFUN FOO ())
;;;   (SETF (DOCUMENTATION #'FOO 'FUNCTION) "testing")
;;; They fail with
;;;   Can't change the documentation of #<interpreted function FOO {900BF51}>.
;;; The coverage of the DOCUMENTATION methods ought to be systematically
;;; compared to the ANSI specification of DOCUMENTATION.

;;; functions, macros, and special forms
(defmethod documentation ((x function) (doc-type (eql 't)))
  (%fun-doc x))

(defmethod documentation ((x function) (doc-type (eql 'function)))
  (%fun-doc x))

(defmethod documentation ((x list) (doc-type (eql 'function)))
  ;; FIXME: could test harder to see whether it's a SETF function name,
  ;; then call WARN
  (when (eq (first x) 'setf)	; Give up if not a setf function name.
    (or (values (info :setf :documentation (second x)))
	;; Try the pcl function documentation.
	(and (fboundp x) (documentation (fdefinition x) t)))))

(defmethod documentation ((x symbol) (doc-type (eql 'function)))
  (or (values (info :function :documentation x))
      ;; Try the pcl function documentation.
      (and (fboundp x) (documentation (fdefinition x) t))))

(defmethod documentation ((x symbol) (doc-type (eql 'setf)))
  (values (info :setf :documentation x)))

(defmethod (setf documentation) (new-value (x list) (doc-type (eql 'function)))
  (setf (info :setf :documentation (cadr x)) new-value))

(defmethod (setf documentation) (new-value
				 (x symbol)
				 (doc-type (eql 'function)))
  (setf (info :function :documentation x) new-value))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'setf)))
  (setf (info :setf :documentation x) new-value))

;;; packages
(defmethod documentation ((x package) (doc-type (eql 't)))
  (package-doc-string x))

(defmethod (setf documentation) (new-value (x package) (doc-type (eql 't)))
  (setf (package-doc-string x) new-value))
;;; KLUDGE: It's nasty having things like this accessor floating around
;;; out in this mostly-unrelated source file. Perhaps it would be
;;; better to support WARM-INIT-FORMS by analogy with the existing
;;; !COLD-INIT-FORMS and have them be EVAL'ed after basic warm load is
;;; done? That way things like this could be defined alongside the
;;; other code which does low-level hacking of packages.. -- WHN 19991203

;;; types, classes, and structure names
(defmethod documentation ((x cl:structure-class) (doc-type (eql 't)))
  (values (info :type :documentation (cl:class-name x))))

(defmethod documentation ((x structure-class) (doc-type (eql 't)))
  (values (info :type :documentation (class-name x))))

(defmethod documentation ((x cl:standard-class) (doc-type (eql 't)))
  (or (values (info :type :documentation (cl:class-name x)))
      (let ((pcl-class (sb-kernel:class-pcl-class x)))
	(and pcl-class (plist-value pcl-class 'documentation)))))

(defmethod documentation ((x cl:structure-class) (doc-type (eql 'type)))
  (values (info :type :documentation (cl:class-name x))))

(defmethod documentation ((x structure-class) (doc-type (eql 'type)))
  (values (info :type :documentation (class-name x))))

(defmethod documentation ((x cl:standard-class) (doc-type (eql 'type)))
  (or (values (info :type :documentation (cl:class-name x)))
      (let ((pcl-class (sb-kernel:class-pcl-class x)))
	(and pcl-class (plist-value pcl-class 'documentation)))))

(defmethod documentation ((x symbol) (doc-type (eql 'type)))
  (or (values (info :type :documentation x))
      (let ((class (find-class x nil)))
	(when class
	  (plist-value class 'documentation)))))

(defmethod documentation ((x symbol) (doc-type (eql 'structure)))
  (when (eq (info :type :kind x) :instance)
    (values (info :type :documentation x))))

(defmethod (setf documentation) (new-value
				 (x cl:structure-class)
				 (doc-type (eql 't)))
  (setf (info :type :documentation (cl:class-name x)) new-value))

(defmethod (setf documentation) (new-value
				 (x structure-class)
				 (doc-type (eql 't)))
  (setf (info :type :documentation (class-name x)) new-value))

(defmethod (setf documentation) (new-value
				 (x cl:structure-class)
				 (doc-type (eql 'type)))
  (setf (info :type :documentation (cl:class-name x)) new-value))

(defmethod (setf documentation) (new-value
				 (x structure-class)
				 (doc-type (eql 'type)))
  (setf (info :type :documentation (class-name x)) new-value))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'type)))
  (if (structure-type-p x)	; Catch structures first.
      (setf (info :type :documentation x) new-value)
      (let ((class (find-class x nil)))
	(if class
	    (setf (plist-value class 'documentation) new-value)
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

;;; miscellaneous documentation. Compiler-macro documentation is stored
;;; as random-documentation and handled here.
(defmethod documentation ((x symbol) (doc-type symbol))
  (cdr (assoc doc-type
	      (values (info :random-documentation :stuff x)))))

(defmethod (setf documentation) (new-value (x symbol) (doc-type symbol))
  (let ((pair (assoc doc-type (info :random-documentation :stuff x))))
    (if pair
	(setf (cdr pair) new-value)
	(push (cons doc-type new-value)
	      (info :random-documentation :stuff x))))
  new-value)

;;; FIXME: The ((X SYMBOL) (DOC-TYPE SYMBOL)) method and its setf method should
;;; have parallel versions which accept LIST-valued X arguments (for function
;;; names in the (SETF FOO) style).

;;; Now that we have created the machinery for setting documentation, we can
;;; set the documentation for the machinery for setting documentation.
#+sb-doc
(setf (documentation 'documentation 'function)
      "Return the documentation string of Doc-Type for X, or NIL if
  none exists. System doc-types are VARIABLE, FUNCTION, STRUCTURE, TYPE,
  SETF, and T.")
