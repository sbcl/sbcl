(cl:defpackage :sb-aclrepl
  (:use :cl :sb-ext))

(cl:in-package :sb-aclrepl)

(defvar *noprint* nil "boolean: T if don't print prompt and output")
(defvar *break-level* 0 "current break level")
(defvar *inspect-break* nil "boolean: T if break caused by inspect")
(defvar *continuable-break* nil "boolean: T if break caused by continuable error")

(shadowing-import '(sb-impl::scrub-control-stack
		    sb-int:*repl-prompt-fun* sb-int:*repl-read-form-fun*)
		  :sb-aclrepl)
	  

(defun repl (&key
	     (break-level (1+ *break-level*))
	     (noprint *noprint*)
	     (inspect nil)
	     (continuable nil))
  (let ((*noprint* noprint)
	(*break-level* break-level)
	(*inspect-break* inspect)
	(*continuable-break* continuable))
    (sb-int:/show0 "entering REPL")
    (loop
     (multiple-value-bind (reason reason-param)
	 (catch 'repl-catcher
	   (loop
	    (rep-one)))
       (cond
	 ((and (eq reason :inspect)
	       (plusp *break-level*))
	  (return-from repl))
	 ((and (eq reason :pop)
	       (plusp *break-level*))
	  (return-from repl)))))))

(defun rep-one ()
  "Read-Eval-Print one form"
  ;; (See comment preceding the definition of SCRUB-CONTROL-STACK.)
  (scrub-control-stack)
  (unless *noprint*
    (funcall *repl-prompt-fun* *standard-output*)
    ;; (Should *REPL-PROMPT-FUN* be responsible for doing its own
    ;; FORCE-OUTPUT? I can't imagine a valid reason for it not to
    ;; be done here, so leaving it up to *REPL-PROMPT-FUN* seems
    ;; odd. But maybe there *is* a valid reason in some
    ;; circumstances? perhaps some deadlock issue when being driven
    ;; by another process or something...)
    (force-output *standard-output*))
  (let* ((form (funcall *repl-read-form-fun*
			*standard-input*
			*standard-output*))
	 (results (multiple-value-list (sb-impl::interactive-eval form))))
    (unless *noprint*
      (dolist (result results)
	;; FIXME: Calling fresh-line before a result ensures the result starts
	;; on a newline, but it usually generates an empty line.
	;; One solution would be to have the newline's entered on the
	;; input stream inform the output stream that the column should be
	;; reset to the beginning of the line.
	(fresh-line *standard-output*)
	(prin1 result *standard-output*)))))

(defun repl-fun (noprint)
  (repl :noprint noprint :break-level 0))

(when (boundp 'sb-impl::*repl-fun*)
  (setq sb-impl::*repl-fun* #'repl-fun))
