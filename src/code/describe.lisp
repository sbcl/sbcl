;;;; most of the DESCRIBE mechanism -- that part which isn't derived
;;;; from PCL code

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL") ;(SB-IMPL, not SB!IMPL, since we're built in warm load.)

(defvar *describe-indentation-step* 3
  #+sb-doc
  "the number of spaces that sets off each line of a recursive description")

(declaim (ftype (function (t stream)) describe-object))
(defgeneric describe-object ((x t) stream))

(defun describe (x &optional (stream-designator *standard-output*))
  #+sb-doc
  "Print a description of the object X."
  (let ((stream (out-synonym-of stream-designator)))
    (pprint-logical-block (stream nil)
      (fresh-line stream)
      (describe-object x stream)
      (fresh-line stream)))
  (values))

;;;; miscellaneous DESCRIBE-OBJECT methods

(defmethod describe-object ((x t) s)
  (format s "~@<~S ~_is a ~S.~:>" x (type-of x)))

(defmethod describe-object ((x cons) s)
  (call-next-method)
  (when (and (legal-fun-name-p x)
	     (fboundp x))
    (%describe-function (fdefinition x) s :function x)
    ;;was: (format s "~@:_Its FDEFINITION is ~S.~@:_" (fdefinition x))
    ;; TO DO: should check for SETF documentation.
    ;; TO DO: should make it clear whether the definition is a
    ;; DEFUN (SETF FOO) or DEFSETF FOO or what.
    ))

(defmethod describe-object ((x array) s)
  (let ((rank (array-rank x)))
    (cond ((> rank 1)
	   (format s "~S ~_is " x)
	   (write-string (if (%array-displaced-p x) "a displaced" "an") s)
	   (format s " array of rank ~S." rank)
	   (format s "~@:_Its dimensions are ~S." (array-dimensions x)))
	  (t
	   (format s
		   "~@:_~S is a ~:[~;displaced ~]vector of length ~S." x
		   (and (array-header-p x) (%array-displaced-p x)) (length x))
	   (when (array-has-fill-pointer-p x)
	     (format s "~@:_It has a fill pointer, currently ~S."
		     (fill-pointer x))))))
  (let ((array-element-type (array-element-type x)))
    (unless (eq array-element-type t)
      (format s
	      "~@:_Its element type is specialized to ~S."
	      array-element-type))))

(defmethod describe-object ((x hash-table) s)
  (declare (type stream s))
  (format s "~@<~S ~_is an ~S hash table.~:>" x (hash-table-test x))
  (format s "~_Its SIZE is ~S." (hash-table-size x))
  (format s
	  "~@:_~@<Its REHASH-SIZE is ~S. ~_Its REHASH-THRESHOLD is ~S.~:>"
	  (hash-table-rehash-size x)
	  (hash-table-rehash-threshold x))
  (let ((count (hash-table-count x)))
    (format s "~@:_It holds ~S key/value pair~:P~:[: ~2I~_~;.~]"
	    count (zerop count))
    (let ((n 0))
      (declare (type index n))
      (dohash (k v x)
	(unless (zerop n)
	  (write-char #\space s))
	(incf n)
	(when (and *print-length* (> n *print-length*))
	  (format s "~:_...")
	  (return))
	(format s "~:_(~@<~S ~:_~S~:>)" k v)))))

(defmethod describe-object ((condition condition) s)
  (sb-kernel:describe-condition condition s))

;;;; DESCRIBE-OBJECT methods for symbols and functions, including all
;;;; sorts of messy stuff about documentation, type information,
;;;; packaging, function implementation, etc..

;;; Print the specified kind of documentation about the given NAME. If
;;; NAME is null, or not a valid name, then don't print anything.
(declaim (ftype (function (t stream t t) (values)) %describe-doc))
(defun %describe-doc (name s kind kind-doc)
  (when (and name (typep name '(or symbol cons)))
    (let ((doc (fdocumentation name kind)))
      (when doc
	(format s "~_~@(~A documentation:~)~@:_  ~A"
		(or kind-doc kind) doc))))
  (values))

;;; Describe various stuff about the functional semantics attached to
;;; the specified NAME, if NAME is the kind of thing you can look
;;; up as a name. (In the case of anonymous closures and other
;;; things, it might not be.) TYPE-SPEC is the function type specifier
;;; extracted from the definition, or NIL if none.
(declaim (ftype (function (t stream t)) %describe-fun-name))
(defun %describe-fun-name (name s type-spec) 
  (when (and name (typep name '(or symbol cons)))
    (multiple-value-bind (type where)
	(if (or (symbolp name) (and (listp name) (eq (car name) 'setf)))
	    (values (type-specifier (info :function :type name))
		    (info :function :where-from name))
	    (values type-spec :defined))
      (when (consp type)
	(format s "~@:_Its ~(~A~) argument types are:~@:_  ~S"
		where (second type))
	(format s "~@:_Its result type is:~@:_  ~S" (third type))))
    (let ((inlinep (info :function :inlinep name)))
      (when inlinep
	(format s
		"~@:_It is currently declared ~(~A~);~
		 ~:[no~;~] expansion is available."
		inlinep (info :function :inline-expansion-designator name))))))

;;; Print information from the debug-info about where CODE-OBJ was
;;; compiled from.
(defun %describe-compiled-from (code-obj s)
  (declare (type stream s))
  (let ((info (sb-kernel:%code-debug-info code-obj)))
    (when info
      (let ((sources (sb-c::debug-info-source info)))
	(when sources
	  (format s "~@:_On ~A it was compiled from:"
		  ;; FIXME: The FORMAT-UNIVERSAL-TIME calls in the system
		  ;; should become more consistent, probably not using
		  ;; any nondefault options.
		  (format-universal-time nil
					 (sb-c::debug-source-compiled
					  (first sources))
					 :style :abbreviated))
	  (dolist (source sources)
	    (let ((name (sb-c::debug-source-name source)))
	      (ecase (sb-c::debug-source-from source)
		(:file
		 (format s "~@:_~A~@:_  Created: " (namestring name))
		 (format-universal-time s (sb-c::debug-source-created
					   source)))
		(:lisp (format s "~@:_~S" name))))))))))

;;; Describe a compiled function. The closure case calls us to print
;;; the guts.
(defun %describe-function-compiled (x s kind name)
  (declare (type stream s))
  ;; FIXME: The lowercaseness of %SIMPLE-FUN-ARGLIST results, and the
  ;; non-sentenceness of the "Arguments" label, makes awkward output.
  ;; Better would be "Its arguments are: ~S" (with uppercase argument
  ;; names) when arguments are known, and otherwise "There is no
  ;; information available about its arguments." or "It has no
  ;; arguments." (And why is %SIMPLE-FUN-ARGLIST a string instead of a
  ;; list of symbols anyway?)
  (let ((args (%simple-fun-arglist x)))
    (format s "~@:_~@(~@[~A ~]arguments:~@:_~)" kind)
    (cond ((not args)
	   (format s "  There is no argument information available."))
	  ((string= args "()")
	   (write-string "  There are no arguments." s))
	  (t
	   (write-string "  " s)
	   (pprint-logical-block (s nil)
	     (pprint-indent :current 2)
	     (write-string args s)))))
  (let ((name (or name (%simple-fun-name x))))
    (%describe-doc name s 'function kind)
    (unless (eq kind :macro)
      (%describe-fun-name name s (%simple-fun-type x))))
  (%describe-compiled-from (sb-kernel:fun-code-header x) s))

;;; Describe a function with the specified kind and name. The latter
;;; arguments provide some information about where the function came
;;; from. KIND=NIL means not from a name.
(defun %describe-function (x s &optional (kind nil) name)
  (declare (type function x))
  (declare (type stream s))
  (declare (type (member :macro :function nil) kind))
  (fresh-line s)
  (ecase kind
    (:macro (format s "Macro-function: ~S" x))
    (:function (format s "Function: ~S" x))
    ((nil) (format s "~S is a function." x)))
  (case (widetag-of x)
    (#.sb-vm:closure-header-widetag
     (%describe-function-compiled (%closure-fun x) s kind name)
     (format s "~@:_Its closure environment is:")
     (pprint-logical-block (s nil)
       (pprint-indent :current 8)
       (dotimes (i (- (get-closure-length x) (1- sb-vm:closure-info-offset)))
	 (format s "~@:_~S: ~S" i (%closure-index-ref x i)))))
    ((#.sb-vm:simple-fun-header-widetag #.sb-vm:closure-fun-header-widetag)
     (%describe-function-compiled x s kind name))
    (#.sb-vm:funcallable-instance-header-widetag
     (typecase x
       (standard-generic-function
	;; There should be a special method for this case; we'll
	;; delegate to that.
	(describe-object x s))
       (t
	(format s "~@:_It is an unknown type of funcallable instance."))))
    (t
     (format s "~@:_It is an unknown type of function."))))

(defmethod describe-object ((x function) s)
  (%describe-function x s))
  
(defmethod describe-object ((x symbol) s)
  (declare (type stream s))

  ;; Describe the packaging.
  (let ((package (symbol-package x)))
    (if package
	(multiple-value-bind (symbol status)
	    (find-symbol (symbol-name x) package)
	  (declare (ignore symbol))
	  (format s "~S is ~_an ~(~A~) symbol ~_in ~S."
		  x status (symbol-package x)))
	(format s "~S is ~_an uninterned symbol." x)))
  ;; TO DO: We could grovel over all packages looking for and
  ;; reporting other phenomena, e.g. IMPORT and SHADOW, or
  ;; availability in some package even after (SYMBOL-PACKAGE X) has
  ;; been set to NIL.

  ;; Describe the value cell.
  (let* ((kind (info :variable :kind x))
	 (wot (ecase kind
		(:special "special variable")
		(:constant "constant")
		(:global "undefined variable")
		(:alien nil))))
    (cond
     ((eq kind :alien)
      (let ((info (info :variable :alien-info x)))
	(format s "~@:_~@<It is an alien at #X~8,'0X of type ~3I~:_~S.~:>~@:_"
		(sap-int (eval (sb-alien::heap-alien-info-sap-form info)))
		(sb-alien-internals:unparse-alien-type
		 (sb-alien::heap-alien-info-type info)))
	(format s "~@<Its current value is ~3I~:_~S.~:>"
		(eval x))))
     ((boundp x)
      (format s "~@:_It is a ~A; its ~_value is ~S." wot (symbol-value x)))
     ((not (eq kind :global))
      (format s "~@:_It is a ~A; no current value." wot)))

    (when (eq (info :variable :where-from x) :declared)
      (format s "~@:_Its declared type ~_is ~S."
	      (type-specifier (info :variable :type x))))

    (%describe-doc x s 'variable kind))

  ;; Print out properties.
  (format s "~@[~@:_Its SYMBOL-PLIST is ~@<~2I~_~S~:>.~]" (symbol-plist x))

  ;; Describe the function cell.
  (cond ((macro-function x)
	 (%describe-function (macro-function x) s :macro x))
	((special-operator-p x)
	 (%describe-doc x s 'function "Special form"))
	((fboundp x)
	 (%describe-function (fdefinition x) s :function x)))

  ;; FIXME: Print out other stuff from the INFO database:
  ;;   * Does it name a type?
  ;;   * Is it a structure accessor? (This is important since those are 
  ;;     magical in some ways, e.g. blasting the structure if you 
  ;;     redefine them.)

  ;; Print other documentation.
  (%describe-doc x s 'structure "Structure")
  (%describe-doc x s 'type "Type")
  (%describe-doc x s 'setf "Setf macro")

  (dolist (assoc (info :random-documentation :stuff x))
    (format s
	    "~@:_Documentation on the ~(~A~):~@:_~A"
	    (car assoc)
	    (cdr assoc)))
  
  ;; Describe the associated class, if any.
  (let ((symbol-named-class (cl:find-class x nil)))
    (when symbol-named-class
      (format t "~&It names a class ~A." symbol-named-class)
      (describe symbol-named-class))))
