;;;; functions from classic CMU CL src/compiler/main.lisp which are
;;;; needed only (and which may make sense only) on the
;;;; cross-compilation target, not the cross-compilation host

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;;; CL:COMPILE

(defun get-lambda-to-compile (definition)
  (if (consp definition)
      definition
      (multiple-value-bind (def env-p)
			   (function-lambda-expression definition)
	(when env-p
	  (error "~S was defined in a non-null environment." definition))
	(unless def
	  (error "Can't find a definition for ~S." definition))
	def)))

;;; Find the function that is being compiled by COMPILE and bash its name to
;;; NAME. We also substitute for any references to name so that recursive
;;; calls will be compiled direct. Lambda is the top-level lambda for the
;;; compilation. A REF for the real function is the only thing in the
;;; top-level lambda other than the bind and return, so it isn't too hard to
;;; find.
(defun compile-fix-function-name (lambda name)
  (declare (type clambda lambda) (type (or symbol cons) name))
  (when name
    (let ((fun (ref-leaf
		(continuation-next
		 (node-cont (lambda-bind lambda))))))
      (setf (leaf-name fun) name)
      (let ((old (gethash name *free-functions*)))
	(when old (substitute-leaf fun old)))
      name)))

;;; Handle the nontrivial case of CL:COMPILE.
(defun actually-compile (name definition)
  (with-compilation-values
    (sb!xc:with-compilation-unit ()
      (let* (;; FIXME: Do we need this rebinding here? It's a literal
	     ;; translation of the old CMU CL rebinding to
	     ;; (OR *BACKEND-INFO-ENVIRONMENT* *INFO-ENVIRONMENT*),
	     ;; and it's not obvious whether the rebinding to itself is
	     ;; needed that SBCL doesn't need *BACKEND-INFO-ENVIRONMENT*.
	     (*info-environment* *info-environment*)
	     (*lexenv* (make-null-lexenv))
	     (form `#',(get-lambda-to-compile definition))
	     (*source-info* (make-lisp-source-info form))
	     (*top-level-lambdas* ())
	     (*converting-for-interpreter* nil)
	     (*block-compile* nil)
	     (*compiler-error-bailout*
	      #'(lambda ()
		  (compiler-mumble
		   "~2&fatal error, aborting compilation~%")
		  (return-from actually-compile (values nil t nil))))
	     (*current-path* nil)
	     (*last-source-context* nil)
	     (*last-original-source* nil)
	     (*last-source-form* nil)
	     (*last-format-string* nil)
	     (*last-format-args* nil)
	     (*last-message-count* 0)
	     (*compile-object* (make-core-object))
	     (*gensym-counter* 0)
	     ;; FIXME: ANSI doesn't say anything about CL:COMPILE
	     ;; interacting with these variables, so we shouldn't. As
	     ;; of SBCL 0.6.7, COMPILE-FILE controls its verbosity by
	     ;; binding these variables, so as a quick hack we do so
	     ;; too. But a proper implementation would have verbosity
	     ;; controlled by function arguments and lexical variables.
	     (*compile-verbose* nil)
	     (*compile-print* nil))
	(clear-stuff)
	(find-source-paths form 0)
	(let ((lambda (ir1-top-level form '(original-source-start 0 0) t)))

	  (compile-fix-function-name lambda name)
	  (let* ((component
		  (block-component (node-block (lambda-bind lambda))))
		 (*all-components* (list component)))
	    (local-call-analyze component))

	  (multiple-value-bind (components top-components)
			       (find-initial-dfo (list lambda))
	    (let ((*all-components* (append components top-components)))
	      (dolist (component *all-components*)
		(compile-component component))))

	  (let ((compiled-fun (core-call-top-level-lambda lambda
							  *compile-object*)))
	    (fix-core-source-info *source-info* *compile-object* compiled-fun)
	    compiled-fun))))))

(defun compile (name &optional (definition (fdefinition name)))
  #!+sb-doc
  "Coerce DEFINITION (by default, the function whose name is NAME)
  to a compiled function, returning (VALUES THING WARNINGS-P FAILURE-P),
  where if NAME is NIL, THING is the result of compilation, and
  otherwise THING is NAME. When NAME is not NIL, the compiled function
  is also set into (MACRO-FUNCTION NAME) if NAME names a macro, or into
  (FDEFINITION NAME) otherwise."
  (multiple-value-bind (compiled-definition warnings-p failure-p)
      (if (compiled-function-p definition)
	  (values definition nil nil)
	  (actually-compile name definition))
    (cond (name
	   (if (macro-function name)
	       (setf (macro-function name) compiled-definition)
	       (setf (fdefinition name) compiled-definition))
	   (values name warnings-p failure-p))
	  (t
	   (values compiled-definition warnings-p failure-p)))))
