;;;; Toplevel for sb-aclrepl

(cl:defpackage :sb-aclrepl
  (:use :cl :sb-ext))

(cl:in-package :sb-aclrepl)

(defvar *break-level* 0 "Current break level")
(defvar *inspect-reason* nil
  "Boolean if break level was started for inspecting.")
(defvar *continuable-reason* nil
  "Boolean if break level was started by continuable error.")
(defvar *noprint* nil "Boolean is output should be displayed")
(defvar *input* nil "Input stream")
(defvar *output* nil "Output stream")

(defun aclrepl (&key
		(break-level (1+ *break-level*))
		;; Break level is started to inspect an object
		inspect
		;; Signals a continuable error
		continuable)
  (let ((*break-level* break-level)
	(*inspect-reason* inspect)
	(*continuable-reason* continuable))
    (loop
     ;; (See comment preceding the definition of SCRUB-CONTROL-STACK.)
     (sb-impl::scrub-control-stack)
     (unless *noprint*
       (funcall (the function sb-int:*repl-prompt-fun*) *output*)
       (force-output *output*))
     (let* ((form (funcall (the function sb-int:*repl-read-form-fun*)
			   *input* *output*))
	    (results (multiple-value-list (interactive-eval form))))
       (unless *noprint*
	 (dolist (result results)
	   (fresh-line *output*)
	   (prin1 result *output*)))))))


;;; read-eval-print loop for the default system toplevel
(defun toplevel-aclrepl-fun (noprint)
  (let ((* nil) (** nil) (*** nil)
	(- nil)
	(+ nil) (++ nil) (+++ nil)
	(/// nil) (// nil) (/ nil))
    ;; WITH-SIMPLE-RESTART doesn't actually restart its body as some
    ;; (like WHN for an embarrassingly long time ca. 2001-12-07) might
    ;; think, but instead drops control back out at the end. So when a
    ;; TOPLEVEL or outermost-ABORT restart happens, we need this outer
    ;; LOOP wrapper to grab control and start over again. (And it also
    ;; wraps CATCH 'TOPLEVEL-CATCHER for similar reasons.)
    (loop
     ;; There should only be one TOPLEVEL restart, and it's here, so
     ;; restarting at TOPLEVEL always bounces you all the way out here.
     (with-simple-restart (toplevel
			   "Restart at toplevel READ/EVAL/PRINT loop.")
       ;; We add a new ABORT restart for every debugger level, so 
       ;; restarting at ABORT in a nested debugger gets you out to the
       ;; innermost enclosing debugger, and only when you're in the
       ;; outermost, unnested debugger level does restarting at ABORT 
       ;; get you out to here.
       (with-simple-restart
	   (abort
	    "~@<Reduce debugger level (leaving debugger, returning to toplevel).~@:>")
	 (catch 'toplevel-catcher
	   #-sunos (sb-unix:unix-sigsetmask 0)	; FIXME: What is this for?
	   ;; in the event of a control-stack-exhausted-error, we should
	   ;; have unwound enough stack by the time we get here that this
	   ;; is now possible
	   (sb-kernel::protect-control-stack-guard-page 1)
	   (let ((*noprint* noprint)
		 (*input* *standard-input*)
		 (*output* *standard-output*))
	     (aclrepl :break-level 0))
	   (sb-impl::critically-unreachable "after REPL")))))))

#+ignore
(when (boundp 'sb-impl::*toplevel-repl-fun*)
  (setq sb-impl::*toplevel-repl-fun* #'toplevel-aclrepl-fun))
