;;;; This file contains definitions and declarations for the
;;;; EXTENSIONS package which must be available at early cross-compile
;;;; time, and perhaps also some things which might as well be built
;;;; at cross-compile time even if they're not strictly needed, since
;;;; that's harmless. Things which can't be built at cross-compile
;;;; time (e.g. because they need machinery which only exists inside
;;;; CMU CL's implementation of the LISP package) do not belong in
;;;; this file.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;; something not EQ to anything we might legitimately READ
(defparameter *eof-object* (make-symbol "EOF-OBJECT"))

;;; a type used for indexing into arrays, and for related quantities
;;; like lengths of lists
;;;
;;; It's intentionally limited to one less than the
;;; ARRAY-DIMENSION-LIMIT for efficiency reasons, because in SBCL
;;; ARRAY-DIMENSION-LIMIT is MOST-POSITIVE-FIXNUM, and staying below
;;; that lets the system know it can increment a value of this type
;;; without having to worry about using a bignum to represent the
;;; result.
;;;
;;; (It should be safe to use ARRAY-DIMENSION-LIMIT as an exclusive
;;; bound because ANSI specifies it as an exclusive bound.)
(def!type index () `(integer 0 (,sb!xc:array-dimension-limit)))

;;; the default value used for initializing character data. The ANSI
;;; spec says this is arbitrary. CMU CL used #\NULL, which we avoid
;;; because it's not in the ANSI table of portable characters.
(defconstant default-init-char #\space)

;;; CHAR-CODE values for ASCII characters which we care about but
;;; which aren't defined in section "2.1.3 Standard Characters" of the
;;; ANSI specification for Lisp
;;;
;;; KLUDGE: These are typically used in the idiom (CODE-CHAR
;;; FOO-CHAR-CODE). I suspect that the current implementation is
;;; expanding this idiom into a full call to CODE-CHAR, which is an
;;; annoying overhead. I should check whether this is happening, and
;;; if so, perhaps implement a DEFTRANSFORM or something to stop it.
;;; (or just find a nicer way of expressing characters portably?) --
;;; WHN 19990713
(defconstant bell-char-code 7)
(defconstant tab-char-code 9)
(defconstant form-feed-char-code 12)
(defconstant return-char-code 13)
(defconstant escape-char-code 27)
(defconstant rubout-char-code 127)

;;;; miscellaneous iteration extensions

(defmacro dovector ((elt vector &optional result) &rest forms)
  #!+sb-doc
  "just like DOLIST, but with one-dimensional arrays"
  (let ((index (gensym))
	(length (gensym))
	(vec (gensym)))
    `(let ((,vec ,vector))
       (declare (type vector ,vec))
       (do ((,index 0 (1+ ,index))
	    (,length (length ,vec)))
	   ((>= ,index ,length) ,result)
	 (let ((,elt (aref ,vec ,index)))
	   ,@forms)))))

(defmacro dohash ((key-var value-var table &optional result) &body body)
  #!+sb-doc
  "DOHASH (Key-Var Value-Var Table [Result]) Declaration* Form*
   Iterate over the entries in a hash-table."
  (multiple-value-bind (forms decls) (parse-body body nil)
    (let ((gen (gensym))
	  (n-more (gensym)))
      `(with-hash-table-iterator (,gen ,table)
	 (loop
	  (multiple-value-bind (,n-more ,key-var ,value-var) (,gen)
	    ,@decls
	    (unless ,n-more (return ,result))
	    ,@forms))))))

;;;; hash cache utility

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *profile-hash-cache* nil))

;;; :INIT-WRAPPER is set to COLD-INIT-FORMS in type system definitions
;;; so that caches will be created before top-level forms run.
(defmacro define-hash-cache (name args &key hash-function hash-bits default
				  (init-wrapper 'progn)
				  (values 1))
  #!+sb-doc
  "DEFINE-HASH-CACHE Name ({(Arg-Name Test-Function)}*) {Key Value}*
  Define a hash cache that associates some number of argument values to a
  result value. The Test-Function paired with each Arg-Name is used to compare
  the value for that arg in a cache entry with a supplied arg. The
  Test-Function must not error when passed NIL as its first arg, but need not
  return any particular value. Test-Function may be any thing that can be
  placed in CAR position.

  Name is used to define these functions:

  <name>-CACHE-LOOKUP Arg*
      See whether there is an entry for the specified Args in the cache. If
      not present, the :DEFAULT keyword (default NIL) determines the result(s).

  <name>-CACHE-ENTER Arg* Value*
      Encache the association of the specified args with Value.

  <name>-CACHE-CLEAR
      Reinitialize the cache, invalidating all entries and allowing the
      arguments and result values to be GC'd.

  These other keywords are defined:

  :HASH-BITS <n>
      The size of the cache as a power of 2.

  :HASH-FUNCTION function
      Some thing that can be placed in CAR position which will compute a value
      between 0 and (1- (expt 2 <hash-bits>)).

  :VALUES <n>
      The number of values cached.

  :INIT-WRAPPER <name>
      The code for initializing the cache is wrapped in a form with the
      specified name. Default PROGN."

  (let* ((var-name (symbolicate "*" name "-CACHE-VECTOR*"))
	 (nargs (length args))
	 (entry-size (+ nargs values))
	 (size (ash 1 hash-bits))
	 (total-size (* entry-size size))
	 (default-values (if (and (consp default) (eq (car default) 'values))
			     (cdr default)
			     (list default)))
	 (n-index (gensym))
	 (n-cache (gensym)))

    (unless (= (length default-values) values)
      (error "The number of default values ~S differs from :VALUES ~D."
	     default values))

    (collect ((inlines)
	      (forms)
	      (inits)
	      (tests)
	      (sets)
	      (arg-vars)
	      (values-indices)
	      (values-names))
      (dotimes (i values)
	(values-indices `(+ ,n-index ,(+ nargs i)))
	(values-names (gensym)))
      (let ((n 0))
        (dolist (arg args)
          (unless (= (length arg) 2)
            (error "bad argument spec: ~S" arg))
          (let ((arg-name (first arg))
                (test (second arg)))
            (arg-vars arg-name)
            (tests `(,test (svref ,n-cache (+ ,n-index ,n)) ,arg-name))
            (sets `(setf (svref ,n-cache (+ ,n-index ,n)) ,arg-name)))
          (incf n)))

      (when *profile-hash-cache*
	(let ((n-probe (symbolicate "*" name "-CACHE-PROBES*"))
	      (n-miss (symbolicate "*" name "-CACHE-MISSES*")))
	  (inits `(setq ,n-probe 0))
	  (inits `(setq ,n-miss 0))
	  (forms `(defvar ,n-probe))
	  (forms `(defvar ,n-miss))
	  (forms `(declaim (fixnum ,n-miss ,n-probe)))))

      (let ((fun-name (symbolicate name "-CACHE-LOOKUP")))
	(inlines fun-name)
	(forms
	 `(defun ,fun-name ,(arg-vars)
	    ,@(when *profile-hash-cache*
		`((incf ,(symbolicate  "*" name "-CACHE-PROBES*"))))
	    (let ((,n-index (* (,hash-function ,@(arg-vars)) ,entry-size))
		  (,n-cache ,var-name))
	      (declare (type fixnum ,n-index))
	      (cond ((and ,@(tests))
		     (values ,@(mapcar (lambda (x) `(svref ,n-cache ,x))
				       (values-indices))))
		    (t
		     ,@(when *profile-hash-cache*
			 `((incf ,(symbolicate  "*" name "-CACHE-MISSES*"))))
		     ,default))))))

      (let ((fun-name (symbolicate name "-CACHE-ENTER")))
	(inlines fun-name)
	(forms
	 `(defun ,fun-name (,@(arg-vars) ,@(values-names))
	    (let ((,n-index (* (,hash-function ,@(arg-vars)) ,entry-size))
		  (,n-cache ,var-name))
	      (declare (type fixnum ,n-index))
	      ,@(sets)
	      ,@(mapcar #'(lambda (i val)
			    `(setf (svref ,n-cache ,i) ,val))
			(values-indices)
			(values-names))
	      (values)))))

      (let ((fun-name (symbolicate name "-CACHE-CLEAR")))
	(forms
	 `(defun ,fun-name ()
	    (do ((,n-index ,(- total-size entry-size) (- ,n-index ,entry-size))
		 (,n-cache ,var-name))
		((minusp ,n-index))
	      (declare (type fixnum ,n-index))
	      ,@(collect ((arg-sets))
		  (dotimes (i nargs)
		    (arg-sets `(setf (svref ,n-cache (+ ,n-index ,i)) nil)))
		  (arg-sets))
	      ,@(mapcar #'(lambda (i val)
			    `(setf (svref ,n-cache ,i) ,val))
			(values-indices)
			default-values))
	    (values)))
	(forms `(,fun-name)))

      (inits `(unless (boundp ',var-name)
		(setq ,var-name (make-array ,total-size))))

      `(progn
	 (defvar ,var-name)
	 (declaim (type (simple-vector ,total-size) ,var-name))
	 #!-sb-fluid (declaim (inline ,@(inlines)))
	 (,init-wrapper ,@(inits))
	 ,@(forms)
	 ',name))))

(defmacro defun-cached ((name &rest options &key (values 1) default
			      &allow-other-keys)
			args &body body-decls-doc)
  #!+sb-doc
  "DEFUN-CACHED (Name {Key Value}*) ({(Arg-Name Test-Function)}*) Form*
  Some syntactic sugar for defining a function whose values are cached by
  DEFINE-HASH-CACHE."
  (let ((default-values (if (and (consp default) (eq (car default) 'values))
			    (cdr default)
			    (list default)))
	(arg-names (mapcar #'car args)))
    (collect ((values-names))
      (dotimes (i values)
	(values-names (gensym)))
      (multiple-value-bind (body decls doc) (parse-body body-decls-doc)
	`(progn
	   (define-hash-cache ,name ,args ,@options)
	   (defun ,name ,arg-names
	     ,@decls
	     ,doc
	     (multiple-value-bind ,(values-names)
		 (,(symbolicate name "-CACHE-LOOKUP") ,@arg-names)
	       (if (and ,@(mapcar #'(lambda (val def)
				      `(eq ,val ,def))
				  (values-names) default-values))
		   (multiple-value-bind ,(values-names)
					(progn ,@body)
		     (,(symbolicate name "-CACHE-ENTER") ,@arg-names
		      ,@(values-names))
		     (values ,@(values-names)))
		   (values ,@(values-names))))))))))

;;;; package idioms

;;; Note: Almost always you want to use FIND-UNDELETED-PACKAGE-OR-LOSE
;;; instead of this function. (The distinction only actually matters when
;;; PACKAGE-DESIGNATOR is actually a deleted package, and in that case
;;; you generally do want to signal an error instead of proceeding.)
(defun %find-package-or-lose (package-designator)
  (or (find-package package-designator)
      (error 'sb!kernel:simple-package-error
	     :package package-designator
	     :format-control "The name ~S does not designate any package."
	     :format-arguments (list package-designator))))

;;; ANSI specifies (in the section for FIND-PACKAGE) that the
;;; consequences of most operations on deleted packages are
;;; unspecified. We try to signal errors in such cases.
(defun find-undeleted-package-or-lose (package-designator)
  (let ((maybe-result (%find-package-or-lose package-designator)))
    (if (package-name maybe-result)     ; if not deleted
	maybe-result
	(error 'sb!kernel:simple-package-error
	       :package maybe-result
	       :format-control "The package ~S has been deleted."
	       :format-arguments (list maybe-result)))))

;;;; miscellany

;;; FIXME: What is this used for that SYMBOLICATE couldn't be used for instead?
;;; If nothing, replace it.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun concat-pnames (name1 name2)
    (declare (symbol name1 name2))
    (if name1
	(intern (concatenate 'simple-string
			     (symbol-name name1)
			     (symbol-name name2)))
	name2)))

;;; Is NAME a legal function name?
(defun legal-function-name-p (name)
  (or (symbolp name)
      (and (consp name)
           (eq (car name) 'setf)
           (consp (cdr name))
           (symbolp (cadr name))
           (null (cddr name)))))

;;; Given a function name, return the name for the BLOCK which
;;; encloses its body (e.g. in DEFUN, DEFINE-COMPILER-MACRO, or FLET).
(declaim (ftype (function ((or symbol cons)) symbol) function-name-block-name))
(defun function-name-block-name (function-name)
  (cond ((symbolp function-name)
	 function-name)
	((and (consp function-name)
	      (= (length function-name) 2)
	      (eq (first function-name) 'setf))
	 (second function-name))
	(t
	 (error "not legal as a function name: ~S" function-name))))

;;; Is X a (possibly-improper) list of at least N elements?
(declaim (ftype (function (t index)) list-of-length-at-least-p))
(defun list-of-length-at-least-p (x n)
  (or (zerop n) ; since anything can be considered an improper list of length 0
      (and (consp x)
	   (list-of-length-at-least-p (cdr x) (1- n)))))

;;; Return a list of N gensyms. (This is a common suboperation in
;;; macros and other code-manipulating code.)
(declaim (ftype (function (index) list) make-gensym-list))
(defun make-gensym-list (n)
  (loop repeat n collect (gensym)))

;;; ANSI guarantees that some symbols are self-evaluating. This
;;; function is to be called just before a change which would affect
;;; that. (We don't absolutely have to call this function before such
;;; changes, since such changes are given as undefined behavior. In
;;; particular, we don't if the runtime cost would be annoying. But
;;; otherwise it's nice to do so.)
(defun about-to-modify (symbol)
  (declare (type symbol symbol))
  (cond ((eq symbol t)
	 (error "Veritas aeterna. (can't change T)"))
	((eq symbol nil)
	 (error "Nihil ex nihil. (can't change NIL)"))
	((keywordp symbol)
	 (error "Keyword values can't be changed."))
	;; (Just because a value is CONSTANTP is not a good enough
	;; reason to complain here, because we want DEFCONSTANT to
	;; be able to use this function, and it's legal to DEFCONSTANT
	;; a constant as long as the new value is EQL to the old
	;; value.)
	))

;;;; DEFPRINTER

;;; These functions are called by the expansion of the DEFPRINTER
;;; macro to do the actual printing.
(declaim (ftype (function (symbol t stream &optional t) (values))
		defprinter-prin1 defprinter-princ))
(defun defprinter-prin1 (name value stream &optional indent)
  (declare (ignore indent))
  (defprinter-prinx #'prin1 name value stream))
(defun defprinter-princ (name value stream &optional indent)
  (declare (ignore indent))
  (defprinter-prinx #'princ name value stream))
(defun defprinter-prinx (prinx name value stream)
  (declare (type function prinx))
  (when *print-pretty*
    (pprint-newline :linear stream))
  (format stream ":~A " name)
  (funcall prinx value stream)
  (values))
(defun defprinter-print-space (stream)
  (write-char #\space stream))

;;; Define some kind of reasonable PRINT-OBJECT method for a
;;; STRUCTURE-OBJECT class.
;;;
;;; NAME is the name of the structure class, and CONC-NAME is the same
;;; as in DEFSTRUCT.
;;;
;;; The SLOT-DESCS describe how each slot should be printed. Each
;;; SLOT-DESC can be a slot name, indicating that the slot should
;;; simply be printed. A SLOT-DESC may also be a list of a slot name
;;; and other stuff. The other stuff is composed of keywords followed
;;; by expressions. The expressions are evaluated with the variable
;;; which is the slot name bound to the value of the slot. These
;;; keywords are defined:
;;;
;;; :PRIN1    Print the value of the expression instead of the slot value.
;;; :PRINC    Like :PRIN1, only princ the value
;;; :TEST     Only print something if the test is true.
;;;
;;; If no printing thing is specified then the slot value is printed
;;; as if by PRIN1.
;;;
;;; The structure being printed is bound to STRUCTURE and the stream
;;; is bound to STREAM.
(defmacro defprinter ((name &key (conc-name (concatenate 'simple-string
							 (symbol-name name)
							 "-")))
		      &rest slot-descs)
  (let ((first? t)
	maybe-print-space
	(reversed-prints nil)
	(stream (gensym "STREAM")))
    (flet ((sref (slot-name)
	     `(,(symbolicate conc-name slot-name) structure)))
      (dolist (slot-desc slot-descs)
	(if first?
	    (setf maybe-print-space nil
		  first? nil)
	    (setf maybe-print-space `(defprinter-print-space ,stream)))
	(cond ((atom slot-desc)
	       (push maybe-print-space reversed-prints)
	       (push `(defprinter-prin1 ',slot-desc ,(sref slot-desc) ,stream)
		     reversed-prints))
	      (t
	       (let ((sname (first slot-desc))
		     (test t))
		 (collect ((stuff))
		   (do ((option (rest slot-desc) (cddr option)))
		       ((null option)
			(push `(let ((,sname ,(sref sname)))
				 (when ,test
				   ,maybe-print-space
				   ,@(or (stuff)
					 `((defprinter-prin1
					     ',sname ,sname ,stream)))))
			      reversed-prints))
		     (case (first option)
		       (:prin1
			(stuff `(defprinter-prin1
				  ',sname ,(second option) ,stream)))
		       (:princ
			(stuff `(defprinter-princ
				  ',sname ,(second option) ,stream)))
		       (:test (setq test (second option)))
		       (t
			(error "bad option: ~S" (first option)))))))))))
    `(def!method print-object ((structure ,name) ,stream)
       ;; FIXME: should probably be byte-compiled
       (pprint-logical-block (,stream nil)
	 (print-unreadable-object (structure ,stream :type t)
	   (when *print-pretty*
	     (pprint-indent :block 2 ,stream))
	   ,@(nreverse reversed-prints))))))

#|
;;; REMOVEME when done testing byte cross-compiler
(defun byte-compiled-foo (x y)
  (declare (optimize (speed 0) (debug 1)))
  (if x
      x
      (cons y y)))
|#