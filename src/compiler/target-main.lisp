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

;;;; COMPILE and UNCOMPILE

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

(defun compile (name &optional (definition (fdefinition name)))
  #!+sb-doc
  "Compiles the function whose name is Name. If Definition is supplied,
  it should be a lambda expression that is compiled and then placed in the
  function cell of Name. If Name is Nil, the compiled code object is
  returned."
  (with-compilation-values
    (sb!xc:with-compilation-unit ()
      (let* ((*info-environment* (or *backend-info-environment*
				     *info-environment*))
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
		  (return-from compile (values nil t nil))))
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

	  (let* ((res1 (core-call-top-level-lambda lambda *compile-object*))
		 (result (or name res1)))
	    (fix-core-source-info *source-info* *compile-object* res1)
	    (when name
	      (setf (fdefinition name) res1))
	    result))))))
