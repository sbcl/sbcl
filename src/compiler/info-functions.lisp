;;;; miscellaneous functions which use INFO
;;;;
;;;; (In CMU CL, these were in globaldb.lisp. They've been moved here
;;;; because references to INFO can't be compiled correctly until
;;;; globaldb initialization is complete, and the SBCL technique for
;;;; initializing the global database in the cross-compiler isn't
;;;; completed until load time.)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;;; internal utilities defined in terms of INFO

;;; Check that NAME is a valid function name, returning the name if
;;; OK, and signalling an error if not. In addition to checking for
;;; basic well-formedness, we also check that symbol names are not NIL
;;; or the name of a special form.
(defun check-fun-name (name)
  (typecase name
    (list
     (unless (and (consp name) (consp (cdr name))
		  (null (cddr name)) (eq (car name) 'setf)
		  (symbolp (cadr name)))
       (compiler-error "illegal function name: ~S" name)))
    (symbol
     (when (eq (info :function :kind name) :special-form)
       (compiler-error "Special form is an illegal function name: ~S" name)))
    (t
     (compiler-error "illegal function name: ~S" name)))
  (values))

;;; Record a new function definition, and check its legality.
(defun proclaim-as-fun-name (name)

  ;; legal name?
  (check-fun-name name)

  ;; scrubbing old data I: possible collision with old definition
  (when (fboundp name)
    (ecase (info :function :kind name)
      (:function) ; happy case
      ((nil)) ; another happy case
      (:macro ; maybe-not-so-good case
       (compiler-style-warn "~S was previously defined as a macro." name)
       (setf (info :function :where-from name) :assumed)
       (clear-info :function :macro-function name))))

  ;; scrubbing old data II: dangling forward references
  ;;
  ;; (This could happen if someone executes PROCLAIM FTYPE at
  ;; macroexpansion time, which is bad style, or at compile time, e.g.
  ;; in EVAL-WHEN (:COMPILE) inside something like DEFSTRUCT, in which
  ;; case it's reasonable style. Either way, NAME is no longer a free
  ;; function.)
  (when (boundp '*free-funs*) ; when compiling
    (remhash name *free-funs*))

  ;; recording the ordinary case
  (setf (info :function :kind name) :function)
  (note-if-setf-fun-and-macro name)

  (values))

;;; This is called to do something about SETF functions that overlap
;;; with SETF macros. Perhaps we should interact with the user to see
;;; whether the macro should be blown away, but for now just give a
;;; warning. Due to the weak semantics of the (SETF FUNCTION) name, we
;;; can't assume that they aren't just naming a function (SETF FOO)
;;; for the heck of it. NAME is already known to be well-formed.
(defun note-if-setf-fun-and-macro (name)
  (when (consp name)
    (when (or (info :setf :inverse name)
	      (info :setf :expander name))
      (compiler-style-warn
       "defining as a SETF function a name that already has a SETF macro:~
       ~%  ~S"
       name)))
  (values))

;;; Make NAME no longer be a function name: clear everything back to
;;; the default.
(defun undefine-fun-name (name)
  (when name
    (macrolet ((frob (type &optional val)
		 `(unless (eq (info :function ,type name) ,val)
		    (setf (info :function ,type name) ,val))))
      (frob :info)
      (frob :type (specifier-type 'function))
      (frob :where-from :assumed)
      (frob :inlinep)
      (frob :kind)
      (frob :inline-expansion-designator)
      (frob :source-transform)
      (frob :assumed-type)))
  (values))

;;; part of what happens with DEFUN, also with some PCL stuff: Make
;;; NAME known to be a function definition.
(defun become-defined-fun-name (name)
  (proclaim-as-fun-name name)
  (when (eq (info :function :where-from name) :assumed)
    (setf (info :function :where-from name) :defined)
    (if (info :function :assumed-type name)
	(setf (info :function :assumed-type name) nil))))

;;; Decode any raw (INFO :FUNCTION :INLINE-EXPANSION-DESIGNATOR FUN-NAME)
;;; value into a lambda expression, or return NIL if there is none.
(declaim (ftype (function ((or symbol cons)) list) fun-name-inline-expansion))
(defun fun-name-inline-expansion (fun-name)
  (let ((info (info :function :inline-expansion-designator fun-name)))
    (if (functionp info)
	(funcall info)
	info)))

;;;; ANSI Common Lisp functions which are defined in terms of the info
;;;; database

(defun sb!xc:constantp (object &optional environment)
  #!+sb-doc
  "True of any Lisp object that has a constant value: types that eval to
  themselves, keywords, constants, and list whose car is QUOTE."
  ;; FIXME: Should STRUCTURE-OBJECT and/or STANDARD-OBJECT be here?
  ;; They eval to themselves..
  ;;
  ;; FIXME: Someday it would be nice to make the code recognize foldable
  ;; functions and call itself recursively on their arguments, so that
  ;; more of the examples in the ANSI CL definition are recognized.
  ;; (e.g. (+ 3 2), (SQRT PI), and (LENGTH '(A B C)))
  (declare (ignore environment))
  (typecase object
    (number t)
    (character t)
    (array t)
    ;; (Note that the following test on INFO catches KEYWORDs as well as
    ;; explicitly DEFCONSTANT symbols.)
    (symbol (eq (info :variable :kind object) :constant))
    (list (eq (car object) 'quote))))

(declaim (ftype (function (symbol &optional (or null sb!c::lexenv))) sb!xc:macro-function))
(defun sb!xc:macro-function (symbol &optional env)
  #!+sb-doc
  "If SYMBOL names a macro in ENV, returns the expansion function,
   else returns NIL. If ENV is unspecified or NIL, use the global
   environment only."
  (declare (symbol symbol))
  (let* ((fenv (when env (sb!c::lexenv-functions env)))
	 (local-def (cdr (assoc symbol fenv))))
    (cond (local-def
	   (if (and (consp local-def) (eq (car local-def) 'MACRO))
	       (cdr local-def)
	       nil))
	  ((eq (info :function :kind symbol) :macro)
	   (values (info :function :macro-function symbol)))
	  (t
	   nil))))

;;; Note: Technically there could be an ENV optional argument to SETF
;;; MACRO-FUNCTION, but since ANSI says that the consequences of
;;; supplying that optional argument are undefined, we don't allow it.
;;; (Thus our implementation of this unspecified behavior is to
;;; complain that the wrong number of arguments was supplied. Since
;;; the behavior is unspecified, this is conforming.:-)
(defun (setf sb!xc:macro-function) (function symbol)
  (declare (symbol symbol) (type function function))
  (when (eq (info :function :kind symbol) :special-form)
    (error "~S names a special form." symbol))
  (setf (info :function :kind symbol) :macro)
  (setf (info :function :macro-function symbol) function)
  ;; This is a nice thing to have in the target SBCL, but in the
  ;; cross-compilation host it's not nice to mess with
  ;; (SYMBOL-FUNCTION FOO) where FOO might be a symbol in the
  ;; cross-compilation host's COMMON-LISP package.
  #-sb-xc-host
  (setf (symbol-function symbol)
	(lambda (&rest args)
	  (declare (ignore args))
	  ;; (ANSI specification of FUNCALL says that this should be
	  ;; an error of type UNDEFINED-FUNCTION, not just SIMPLE-ERROR.)
	  (error 'undefined-function :name symbol)))
  function)

(defun sb!xc:compiler-macro-function (name &optional env)
  #!+sb-doc
  "If NAME names a compiler-macro, returns the expansion function,
   else returns NIL. Note: if the name is shadowed in ENV by a local
   definition, or declared NOTINLINE, NIL is returned. Can be
   set with SETF."
  (let ((found (and env
		    (cdr (assoc name (sb!c::lexenv-functions env)
				:test #'equal)))))
    (unless (eq (cond ((sb!c::defined-fun-p found)
		       (sb!c::defined-fun-inlinep found))
		      (found :notinline)
		      (t
		       (info :function :inlinep name)))
		:notinline)
      (values (info :function :compiler-macro-function name)))))
(defun (setf sb!xc:compiler-macro-function) (function name)
  (declare (type (or symbol list) name)
	   (type (or function null) function))
  (when (eq (info :function :kind name) :special-form)
    (error "~S names a special form." name))
  (setf (info :function :compiler-macro-function name) function)
  function)

;;;; a subset of DOCUMENTATION functionality for bootstrapping

;;; FDOCUMENTATION is like DOCUMENTATION, but with less functionality,
;;; and implemented with DEFUN instead of DEFGENERIC so that it can
;;; run before CLOS is set up. Supported DOC-TYPE values are
;;;   FUNCTION
;;;   SETF
;;;   STRUCTURE
;;;   T
;;;   TYPE
;;;   VARIABLE
;;; FIXME: Other types end up in INFO :RANDOM-DOCUMENTATION :STUFF. I
;;; should add some code to monitor this and make sure that nothing is
;;; unintentionally being sent to never never land this way.
;;; FIXME: Rename FDOCUMENTATION to BDOCUMENTATION, by analogy with
;;; DEF!STRUCT and DEF!MACRO and so forth. And consider simply saving
;;; all the BDOCUMENTATION entries in a *BDOCUMENTATION* hash table
;;; and slamming them into PCL once PCL gets going.
(defun fdocumentation (x doc-type)
  (flet ((try-cmucl-random-doc (x doc-type)
	   (declare (symbol doc-type))
	   (cdr (assoc doc-type
		       (values (info :random-documentation :stuff x))))))
    (case doc-type
      (variable
       (typecase x
	 (symbol (values (info :variable :documentation x)))))
      (function
       (cond ((functionp x)
	      (%fun-doc x))
	     ((legal-fun-name-p x)
	      ;; FIXME: Is it really right to make
	      ;; (DOCUMENTATION '(SETF FOO) 'FUNCTION) equivalent to
	      ;; (DOCUMENTATION 'FOO 'FUNCTION)? That's what CMU CL
	      ;; did, so we do it, but I'm not sure it's what ANSI wants.
	      (values (info :function :documentation
			    (fun-name-block-name x))))))
      (structure
       (typecase x
	 (symbol (when (eq (info :type :kind x) :instance)
		   (values (info :type :documentation x))))))
      (type
       (typecase x
	 (structure-class (values (info :type :documentation (class-name x))))
	 (t (and (typep x 'symbol) (values (info :type :documentation x))))))
      (setf (info :setf :documentation x))
      ((t)
       (typecase x
	 (function (%fun-doc x))
	 (package (package-doc-string x))
	 (structure-class (values (info :type :documentation (class-name x))))
	 (symbol (try-cmucl-random-doc x doc-type))))
      (t
       (typecase x
	 ;; FIXME: This code comes from CMU CL, but
	 ;; TRY-CMUCL-RANDOM-DOC doesn't seem to be defined anywhere
	 ;; in CMU CL. Perhaps it could be defined by analogy with the
	 ;; corresponding SETF FDOCUMENTATION code.
	 (symbol (try-cmucl-random-doc x doc-type)))))))
(defun (setf fdocumentation) (string name doc-type)
  ;; FIXME: I think it should be possible to set documentation for
  ;; things (e.g. compiler macros) named (SETF FOO). fndb.lisp
  ;; declares DOC-TYPE to be a SYMBOL, which contradicts that. What
  ;; should be done?
  (case doc-type
    (variable (setf (info :variable :documentation name) string))
    (function (setf (info :function :documentation name) string))
    (structure (if (eq (info :type :kind name) :instance)
		   (setf (info :type :documentation name) string)
		   (error "~S is not the name of a structure type." name)))
    (type (setf (info :type :documentation name) string))
    (setf (setf (info :setf :documentation name) string))
    (t
     (let ((pair (assoc doc-type (info :random-documentation :stuff name))))
       (if pair
	   (setf (cdr pair) string)
	   (push (cons doc-type string)
		 (info :random-documentation :stuff name))))))
  string)
