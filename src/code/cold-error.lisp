;;;; miscellaneous error stuff that needs to be in the cold load

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

(defvar *break-on-signals* nil
  #!+sb-doc
  "When (TYPEP condition *BREAK-ON-SIGNALS*) is true, then calls to SIGNAL will
   enter the debugger prior to signalling that condition.")

(defun signal (datum &rest arguments)
  #!+sb-doc
  "Invokes the signal facility on a condition formed from DATUM and
   ARGUMENTS. If the condition is not handled, NIL is returned. If
   (TYPEP condition *BREAK-ON-SIGNALS*) is true, the debugger is invoked
   before any signalling is done."
  (/noshow0 "entering SIGNAL")
  (let ((condition (coerce-to-condition datum
					arguments
					'simple-condition
					'signal))
	(*handler-clusters* *handler-clusters*))
    (let ((old-bos *break-on-signals*)
	  (*break-on-signals* nil))
      (when (typep condition old-bos)
	(/noshow0 "doing BREAK in because of *BREAK-ON-SIGNALS*")
	(break "~A~%BREAK was entered because of *BREAK-ON-SIGNALS* (now rebound to NIL)."
	       condition)))
    (loop
      (unless *handler-clusters*
	(/noshow0 "leaving LOOP because of unbound *HANDLER-CLUSTERS*")
	(return))
      (let ((cluster (pop *handler-clusters*)))
	(/noshow0 "got CLUSTER=..")
	(/nohexstr cluster)
	(dolist (handler cluster)
	  (/noshow0 "looking at HANDLER=..")
	  (/nohexstr handler)
	  (when (typep condition (car handler))
	    (funcall (cdr handler) condition)))))
    
    (/noshow0 "returning from SIGNAL")
    nil))

;;; a shared idiom in ERROR, CERROR, and BREAK: The user probably
;;; doesn't want to hear that the error "occurred in" one of these
;;; functions, so we try to point the top of the stack to our caller
;;; instead.
(eval-when (:compile-toplevel :execute)
  (defmacro-mundanely maybe-find-stack-top-hint ()
    `(or sb!debug:*stack-top-hint*
	 (nth-value 1 (find-caller-name-and-frame)))))

(defun error (datum &rest arguments)
  #!+sb-doc
  "Invoke the signal facility on a condition formed from DATUM and ARGUMENTS.
  If the condition is not handled, the debugger is invoked."
  (/show0 "entering ERROR, argument list=..")
  (/hexstr arguments)

  (/show0 "cold-printing ERROR arguments one by one..")
  #!+sb-show (dolist (argument arguments)
	       (sb!impl::cold-print argument))
  (/show0 "done cold-printing ERROR arguments")

  (infinite-error-protect
    (let ((condition (coerce-to-condition datum arguments
					  'simple-error 'error))
	  (sb!debug:*stack-top-hint* (maybe-find-stack-top-hint)))
      (/show0 "done coercing DATUM to CONDITION")
      (let ((sb!debug:*stack-top-hint* nil))
	(/show0 "signalling CONDITION from within ERROR")
	(signal condition))
      (/show0 "done signalling CONDITION within ERROR")
      (invoke-debugger condition))))

(defun cerror (continue-string datum &rest arguments)
  (infinite-error-protect
    (with-simple-restart
	(continue "~A" (apply #'format nil continue-string arguments))
      (let ((condition (coerce-to-condition datum
					    arguments
					    'simple-error
					    'error))
	    (sb!debug:*stack-top-hint* (maybe-find-stack-top-hint)))
	(with-condition-restarts condition (list (find-restart 'continue))
	  (let ((sb!debug:*stack-top-hint* nil))
	    (signal condition))
	  (invoke-debugger condition)))))
  nil)

;;; like BREAK, but without rebinding *DEBUGGER-HOOK* to NIL, so that
;;; we can use it in system code (e.g. in SIGINT handling) without
;;; messing up --disable-debugger mode (which works by setting
;;; *DEBUGGER-HOOK*); or for that matter, without messing up ordinary
;;; applications which try to do similar things with *DEBUGGER-HOOK*
(defun %break (what &optional (datum "break") &rest arguments)
  (infinite-error-protect
    (with-simple-restart (continue "Return from ~S." what)
      (let ((sb!debug:*stack-top-hint* (maybe-find-stack-top-hint)))
	(invoke-debugger
	 (coerce-to-condition datum arguments 'simple-condition what)))))
  nil)

(defun break (&optional (datum "break") &rest arguments)
  #!+sb-doc
  "Print a message and invoke the debugger without allowing any possibility
   of condition handling occurring."
  (let ((*debugger-hook* nil)) ; as specifically required by ANSI
    (apply #'%break 'break datum arguments)))
	    
(defun warn (datum &rest arguments)
  #!+sb-doc
  "Warn about a situation by signalling a condition formed by DATUM and
   ARGUMENTS. While the condition is being signaled, a MUFFLE-WARNING restart
   exists that causes WARN to immediately return NIL."
  (/show0 "entering WARN")
  ;; KLUDGE: The current cold load initialization logic causes several calls
  ;; to WARN, so we need to be able to handle them without dying. (And calling
  ;; FORMAT or even PRINC in cold load is a good way to die.) Of course, the
  ;; ideal would be to clean up cold load so that it doesn't call WARN..
  ;; -- WHN 19991009
  (if (not *cold-init-complete-p*)
      (progn
	(/show0 "ignoring WARN in cold init, arguments=..")
	#!+sb-show (dolist (argument arguments)
		     (sb!impl::cold-print argument)))
      (infinite-error-protect
       (/show0 "doing COERCE-TO-CONDITION")
       (let ((condition (coerce-to-condition datum arguments
					     'simple-warning 'warn)))
	 (/show0 "back from COERCE-TO-CONDITION, doing ENFORCE-TYPE")
	 (enforce-type condition warning)
	 (/show0 "back from ENFORCE-TYPE, doing RESTART-CASE MUFFLE-WARNING")
	 (restart-case (signal condition)
	   (muffle-warning ()
	     :report "Skip warning."
	     (return-from warn nil)))
	 (/show0 "back from RESTART-CASE MUFFLE-WARNING (i.e. normal return)")

	 (let ((badness (etypecase condition
			  (style-warning 'style-warning)
			  (warning 'warning))))
	   (/show0 "got BADNESS, calling FORMAT")
	   (format *error-output*
		   "~&~@<~S: ~3i~:_~A~:>~%"
		   badness
		   condition)
	   (/show0 "back from FORMAT, voila!")))))
  nil)
