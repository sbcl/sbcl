;;;; the debugger

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!DEBUG")

;;;; variables and constants

;;; things to consider when tweaking these values:
;;;   * We're afraid to just default them to NIL and NIL, in case the
;;;     user inadvertently causes a hairy data structure to be printed
;;;     when he inadvertently enters the debugger.
;;;   * We don't want to truncate output too much. These days anyone
;;;     can easily run their Lisp in a windowing system or under Emacs,
;;;     so it's not the end of the world even if the worst case is a
;;;     few thousand lines of output.
;;;   * As condition :REPORT methods are converted to use the pretty
;;;     printer, they acquire *PRINT-LEVEL* constraints, so e.g. under
;;;     sbcl-0.7.1.28's old value of *DEBUG-PRINT-LEVEL*=3, an
;;;     ARG-COUNT-ERROR printed as 
;;;       error while parsing arguments to DESTRUCTURING-BIND:
;;;         invalid number of elements in
;;;           #
;;;         to satisfy lambda list
;;;           #:
;;;         exactly 2 expected, but 5 found
(defvar *debug-print-level* 5
  #!+sb-doc
  "*PRINT-LEVEL* for the debugger")
(defvar *debug-print-length* 7
  #!+sb-doc
  "*PRINT-LENGTH* for the debugger")

(defvar *debug-readtable*
  ;; KLUDGE: This can't be initialized in a cold toplevel form,
  ;; because the *STANDARD-READTABLE* isn't initialized until after
  ;; cold toplevel forms have run. So instead we initialize it
  ;; immediately after *STANDARD-READTABLE*. -- WHN 20000205
  nil
  #!+sb-doc
  "*READTABLE* for the debugger")

(defvar *in-the-debugger* nil
  #!+sb-doc
  "This is T while in the debugger.")

;;; nestedness inside debugger command loops
(defvar *debug-command-level* 0)

;;; If this is bound before the debugger is invoked, it is used as the
;;; stack top by the debugger.
(defvar *stack-top-hint* nil)

(defvar *stack-top* nil)
(defvar *real-stack-top* nil)

(defvar *current-frame* nil)

;;; Beginner-oriented help messages are important because you end up
;;; in the debugger whenever something bad happens, or if you try to
;;; get out of the system with Ctrl-C or (EXIT) or EXIT or whatever.
;;; But after memorizing them the wasted screen space gets annoying..
(defvar *debug-beginner-help-p* t
  "Should the debugger display beginner-oriented help messages?")

(defun debug-prompt (stream)
  (sb!thread::get-foreground)
  (format stream
	  "~%~W~:[~;[~W~]] "
	  (sb!di:frame-number *current-frame*)
	  (> *debug-command-level* 1)
	  *debug-command-level*))
  
(defparameter *debug-help-string*
"The debug prompt is square brackets, with number(s) indicating the current
  control stack level and, if you've entered the debugger recursively, how
  deeply recursed you are.
Any command -- including the name of a restart -- may be uniquely abbreviated.
The debugger rebinds various special variables for controlling i/o, sometimes
  to defaults (much like WITH-STANDARD-IO-SYNTAX does) and sometimes to 
  its own special values, e.g. SB-DEBUG:*DEBUG-PRINT-LEVEL*.
Debug commands do not affect *, //, and similar variables, but evaluation in
  the debug loop does affect these variables.
SB-DEBUG:*FLUSH-DEBUG-ERRORS* controls whether errors at the debug prompt
  drop you deeper into the debugger.

Getting in and out of the debugger:
  RESTART  invokes restart numbered as shown (prompt if not given).
  ERROR    prints the error condition and restart cases.
  The number of any restart, or its name, or a unique abbreviation for its
    name, is a valid command, and is the same as using RESTART to invoke
    that restart.

Changing frames:
  U      up frame     D    down frame
  B  bottom frame     F n  frame n (n=0 for top frame)

Inspecting frames:
  BACKTRACE [n]  shows n frames going down the stack.
  LIST-LOCALS, L lists locals in current function.
  PRINT, P       displays current function call.
  SOURCE [n]     displays frame's source form with n levels of enclosing forms.

Breakpoints and steps:
  LIST-LOCATIONS [{function | :C}]   List the locations for breakpoints.
                                     Specify :C for the current frame.
    Abbreviation: LL
  LIST-BREAKPOINTS                   List the active breakpoints.
    Abbreviations: LB, LBP
  DELETE-BREAKPOINT [n]              Remove breakpoint n or all breakpoints.
    Abbreviations: DEL, DBP
  BREAKPOINT {n | :end | :start} [:break form] [:function function]
             [{:print form}*] [:condition form]
                                     Set a breakpoint.
    Abbreviations: BR, BP
  STEP [n]                           Step to the next location or step n times.

Function and macro commands:
 (SB-DEBUG:ARG n)
    Return the n'th argument in the current frame.
 (SB-DEBUG:VAR string-or-symbol [id])
    Returns the value of the specified variable in the current frame.

Other commands:
  RETURN expr
    [EXPERIMENTAL] Return the values resulting from evaluation of expr
    from the current frame, if this frame was compiled with a sufficiently
    high DEBUG optimization quality.
  SLURP
    Discard all pending input on *STANDARD-INPUT*. (This can be
    useful when the debugger was invoked to handle an error in
    deeply nested input syntax, and now the reader is confused.)")

;;; This is used to communicate to DEBUG-LOOP that we are at a step breakpoint.
(define-condition step-condition (simple-condition) ())

;;;; breakpoint state

(defvar *only-block-start-locations* nil
  #!+sb-doc
  "When true, the LIST-LOCATIONS command only displays block start locations.
   Otherwise, all locations are displayed.")

(defvar *print-location-kind* nil
  #!+sb-doc
  "When true, list the code location type in the LIST-LOCATIONS command.")

;;; a list of the types of code-locations that should not be stepped
;;; to and should not be listed when listing breakpoints
(defvar *bad-code-location-types* '(:call-site :internal-error))
(declaim (type list *bad-code-location-types*))

;;; code locations of the possible breakpoints
(defvar *possible-breakpoints*)
(declaim (type list *possible-breakpoints*))

;;; a list of the made and active breakpoints, each is a
;;; BREAKPOINT-INFO structure
(defvar *breakpoints* nil)
(declaim (type list *breakpoints*))

;;; a list of BREAKPOINT-INFO structures of the made and active step
;;; breakpoints
(defvar *step-breakpoints* nil)
(declaim (type list *step-breakpoints*))

;;; the number of times left to step
(defvar *number-of-steps* 1)
(declaim (type integer *number-of-steps*))

;;; This is used when listing and setting breakpoints.
(defvar *default-breakpoint-debug-fun* nil)
(declaim (type (or list sb!di:debug-fun) *default-breakpoint-debug-fun*))

;;;; code location utilities

;;; Return the first code-location in the passed debug block.
(defun first-code-location (debug-block)
  (let ((found nil)
	(first-code-location nil))
    (sb!di:do-debug-block-locations (code-location debug-block)
      (unless found
	(setf first-code-location code-location)
	(setf found t)))
    first-code-location))

;;; Return a list of the next code-locations following the one passed.
;;; One of the *BAD-CODE-LOCATION-TYPES* will not be returned.
(defun next-code-locations (code-location)
  (let ((debug-block (sb!di:code-location-debug-block code-location))
	(block-code-locations nil))
    (sb!di:do-debug-block-locations (block-code-location debug-block)
      (unless (member (sb!di:code-location-kind block-code-location)
		      *bad-code-location-types*)
	(push block-code-location block-code-locations)))
    (setf block-code-locations (nreverse block-code-locations))
    (let* ((code-loc-list (rest (member code-location block-code-locations
					:test #'sb!di:code-location=)))
	   (next-list (cond (code-loc-list
			     (list (first code-loc-list)))
			    ((map 'list #'first-code-location
				  (sb!di:debug-block-successors debug-block)))
			    (t nil))))
      (when (and (= (length next-list) 1)
		 (sb!di:code-location= (first next-list) code-location))
	(setf next-list (next-code-locations (first next-list))))
      next-list)))

;;; Return a list of code-locations of the possible breakpoints of DEBUG-FUN.
(defun possible-breakpoints (debug-fun)
  (let ((possible-breakpoints nil))
    (sb!di:do-debug-fun-blocks (debug-block debug-fun)
      (unless (sb!di:debug-block-elsewhere-p debug-block)
	(if *only-block-start-locations*
	    (push (first-code-location debug-block) possible-breakpoints)
	    (sb!di:do-debug-block-locations (code-location debug-block)
	      (when (not (member (sb!di:code-location-kind code-location)
				 *bad-code-location-types*))
		(push code-location possible-breakpoints))))))
    (nreverse possible-breakpoints)))

;;; Search the info-list for the item passed (CODE-LOCATION,
;;; DEBUG-FUN, or BREAKPOINT-INFO). If the item passed is a debug
;;; function then kind will be compared if it was specified. The kind
;;; if also compared if a breakpoint-info is passed since it's in the
;;; breakpoint. The info structure is returned if found.
(defun location-in-list (place info-list &optional (kind nil))
  (when (breakpoint-info-p place)
    (setf kind (sb!di:breakpoint-kind (breakpoint-info-breakpoint place)))
    (setf place (breakpoint-info-place place)))
  (cond ((sb!di:code-location-p place)
	 (find place info-list
	       :key #'breakpoint-info-place
	       :test (lambda (x y) (and (sb!di:code-location-p y)
					(sb!di:code-location= x y)))))
	(t
	 (find place info-list
	       :test (lambda (x-debug-fun y-info)
		       (let ((y-place (breakpoint-info-place y-info))
			     (y-breakpoint (breakpoint-info-breakpoint
					    y-info)))
			 (and (sb!di:debug-fun-p y-place)
			      (eq x-debug-fun y-place)
			      (or (not kind)
				  (eq kind (sb!di:breakpoint-kind
					    y-breakpoint))))))))))

;;; If LOC is an unknown location, then try to find the block start
;;; location. Used by source printing to some information instead of
;;; none for the user.
(defun maybe-block-start-location (loc)
  (if (sb!di:code-location-unknown-p loc)
      (let* ((block (sb!di:code-location-debug-block loc))
	     (start (sb!di:do-debug-block-locations (loc block)
		      (return loc))))
	(cond ((and (not (sb!di:debug-block-elsewhere-p block))
		    start)
	       ;; FIXME: Why output on T instead of *DEBUG-FOO* or something?
	       (format t "~%unknown location: using block start~%")
	       start)
	      (t
	       loc)))
      loc))

;;;; the BREAKPOINT-INFO structure

;;; info about a made breakpoint
(defstruct (breakpoint-info (:copier nil)
			    (:constructor %make-breakpoint-info))
  ;; where we are going to stop
  (place (missing-arg)
	 :type (or sb!di:code-location sb!di:debug-fun)
	 :read-only t)
  ;; the breakpoint returned by SB!DI:MAKE-BREAKPOINT
  (breakpoint (missing-arg) :type sb!di:breakpoint :read-only t)
  ;; the function returned from SB!DI:PREPROCESS-FOR-EVAL. If result is
  ;; non-NIL, drop into the debugger.
  (break #'identity :type function :read-only t)
  ;; the function returned from SB!DI:PREPROCESS-FOR-EVAL. If result is
  ;; non-NIL, eval (each) print and print results.
  (condition #'identity :type function :read-only t)
  ;; the list of functions from SB!DI:PREPROCESS-FOR-EVAL to evaluate.
  ;; Results are conditionally printed. CAR of each element is the
  ;; function, CDR is the form it goes with.
  (print nil :type list :read-only t)
  ;; the number used when listing the possible breakpoints within a
  ;; function; or could also be a symbol such as START or END
  (code-location-selector (missing-arg) :type (or symbol integer) :read-only t)
  ;; the number used when listing the active breakpoints, and when
  ;; deleting breakpoints
  (breakpoint-number (missing-arg) :type integer :read-only t))

(defun create-breakpoint-info (place breakpoint code-location-selector
				     &key (break #'identity)
				     (condition #'identity) (print nil))
  (setf *breakpoints*
	(sort *breakpoints* #'< :key #'breakpoint-info-breakpoint-number))
  (let ((breakpoint-number
	 (do ((i 1 (incf i)) (breakpoints *breakpoints* (rest breakpoints)))
	     ((or (> i (length *breakpoints*))
		  (not (= i (breakpoint-info-breakpoint-number
			     (first breakpoints)))))

	      i))))
    (%make-breakpoint-info :place place
			   :breakpoint breakpoint
			   :code-location-selector code-location-selector
			   :breakpoint-number breakpoint-number
			   :break break
			   :condition condition
			   :print print)))

(defun print-breakpoint-info (breakpoint-info)
  (let ((place (breakpoint-info-place breakpoint-info))
	(bp-number (breakpoint-info-breakpoint-number breakpoint-info)))
    (case (sb!di:breakpoint-kind (breakpoint-info-breakpoint breakpoint-info))
      (:code-location
       (print-code-location-source-form place 0)
       (format t
	       "~&~S: ~S in ~S"
	       bp-number
	       (breakpoint-info-code-location-selector breakpoint-info)
	       (sb!di:debug-fun-name (sb!di:code-location-debug-fun place))))
      (:fun-start
       (format t "~&~S: FUN-START in ~S" bp-number
	       (sb!di:debug-fun-name place)))
      (:fun-end
       (format t "~&~S: FUN-END in ~S" bp-number
	       (sb!di:debug-fun-name place))))))

;;;; MAIN-HOOK-FUN for steps and breakpoints

;;; This must be passed as the hook function. It keeps track of where
;;; STEP breakpoints are.
(defun main-hook-fun (current-frame breakpoint &optional return-vals
				    fun-end-cookie)
  (setf *default-breakpoint-debug-fun*
	(sb!di:frame-debug-fun current-frame))
  (dolist (step-info *step-breakpoints*)
    (sb!di:delete-breakpoint (breakpoint-info-breakpoint step-info))
    (let ((bp-info (location-in-list step-info *breakpoints*)))
      (when bp-info
	(sb!di:activate-breakpoint (breakpoint-info-breakpoint bp-info)))))
  (let ((*stack-top-hint* current-frame)
	(step-hit-info
	 (location-in-list (sb!di:breakpoint-what breakpoint)
			   *step-breakpoints*
			   (sb!di:breakpoint-kind breakpoint)))
	(bp-hit-info
	 (location-in-list (sb!di:breakpoint-what breakpoint)
			   *breakpoints*
			   (sb!di:breakpoint-kind breakpoint)))
	(break)
	(condition)
	(string ""))
    (setf *step-breakpoints* nil)
    (labels ((build-string (str)
	       (setf string (concatenate 'string string str)))
	     (print-common-info ()
	       (build-string
		(with-output-to-string (*standard-output*)
		  (when fun-end-cookie
		    (format t "~%Return values: ~S" return-vals))
		  (when condition
		    (when (breakpoint-info-print bp-hit-info)
		      (format t "~%")
		      (print-frame-call current-frame))
		    (dolist (print (breakpoint-info-print bp-hit-info))
		      (format t "~& ~S = ~S" (rest print)
			      (funcall (first print) current-frame))))))))
      (when bp-hit-info
	(setf break (funcall (breakpoint-info-break bp-hit-info)
			     current-frame))
	(setf condition (funcall (breakpoint-info-condition bp-hit-info)
				 current-frame)))
      (cond ((and bp-hit-info step-hit-info (= 1 *number-of-steps*))
	     (build-string (format nil "~&*Step (to a breakpoint)*"))
	     (print-common-info)
	     (break string))
	    ((and bp-hit-info step-hit-info break)
	     (build-string (format nil "~&*Step (to a breakpoint)*"))
	     (print-common-info)
	     (break string))
	    ((and bp-hit-info step-hit-info)
	     (print-common-info)
	     (format t "~A" string)
	     (decf *number-of-steps*)
	     (set-step-breakpoint current-frame))
	    ((and step-hit-info (= 1 *number-of-steps*))
	     (build-string "*Step*")
	     (break (make-condition 'step-condition :format-control string)))
	    (step-hit-info
	     (decf *number-of-steps*)
	     (set-step-breakpoint current-frame))
	    (bp-hit-info
	     (when break
	       (build-string (format nil "~&*Breakpoint hit*")))
	     (print-common-info)
	     (if break
		 (break string)
		 (format t "~A" string)))
	    (t
	     (break "unknown breakpoint"))))))

;;; Set breakpoints at the next possible code-locations. After calling
;;; this, either (CONTINUE) if in the debugger or just let program flow
;;; return if in a hook function.
(defun set-step-breakpoint (frame)
  (cond
   ((sb!di:debug-block-elsewhere-p (sb!di:code-location-debug-block
				    (sb!di:frame-code-location frame)))
    ;; FIXME: FORMAT T is used for error output here and elsewhere in
    ;; the debug code.
    (format t "cannot step, in elsewhere code~%"))
   (t
    (let* ((code-location (sb!di:frame-code-location frame))
	   (next-code-locations (next-code-locations code-location)))
      (cond
       (next-code-locations
	(dolist (code-location next-code-locations)
	  (let ((bp-info (location-in-list code-location *breakpoints*)))
	    (when bp-info
	      (sb!di:deactivate-breakpoint (breakpoint-info-breakpoint
					    bp-info))))
	  (let ((bp (sb!di:make-breakpoint #'main-hook-fun code-location
					   :kind :code-location)))
	    (sb!di:activate-breakpoint bp)
	    (push (create-breakpoint-info code-location bp 0)
		  *step-breakpoints*))))
       (t
	(let* ((debug-fun (sb!di:frame-debug-fun *current-frame*))
	       (bp (sb!di:make-breakpoint #'main-hook-fun debug-fun
					  :kind :fun-end)))
	  (sb!di:activate-breakpoint bp)
	  (push (create-breakpoint-info debug-fun bp 0)
		*step-breakpoints*))))))))

;;;; STEP

;;; ANSI specifies that this macro shall exist, even if only as a
;;; trivial placeholder like this.
(defmacro step (form)
  "a trivial placeholder implementation of the CL:STEP macro required by
   the ANSI spec"
  `(progn
     ,form))

;;;; BACKTRACE

(defun backtrace (&optional (count most-positive-fixnum)
			    (*standard-output* *debug-io*))
  #!+sb-doc
  "Show a listing of the call stack going down from the current frame. In the
   debugger, the current frame is indicated by the prompt. COUNT is how many
   frames to show."
  (fresh-line *standard-output*)
  (do ((frame (if *in-the-debugger* *current-frame* (sb!di:top-frame))
	      (sb!di:frame-down frame))
       (count count (1- count)))
      ((or (null frame) (zerop count)))
    (print-frame-call frame :number t))
  (fresh-line *standard-output*)
  (values))

(defun backtrace-as-list (&optional (count most-positive-fixnum))
  #!+sb-doc "Return a list representing the current BACKTRACE."
  (do ((reversed-result nil)
       (frame (if *in-the-debugger* *current-frame* (sb!di:top-frame))
	      (sb!di:frame-down frame))
       (count count (1- count)))
      ((or (null frame) (zerop count))
       (nreverse reversed-result))
    (push (frame-call-as-list frame) reversed-result)))

(defun frame-call-as-list (frame)
  (cons (sb!di:debug-fun-name (sb!di:frame-debug-fun frame))
	(frame-args-as-list frame)))

;;;; frame printing

(eval-when (:compile-toplevel :execute)

;;; This is a convenient way to express what to do for each type of
;;; lambda-list element.
(sb!xc:defmacro lambda-list-element-dispatch (element
					      &key
					      required
					      optional
					      rest
					      keyword
					      deleted)
  `(etypecase ,element
     (sb!di:debug-var
      ,@required)
     (cons
      (ecase (car ,element)
	(:optional ,@optional)
	(:rest ,@rest)
	(:keyword ,@keyword)))
     (symbol
      (aver (eq ,element :deleted))
      ,@deleted)))

(sb!xc:defmacro lambda-var-dispatch (variable location deleted valid other)
  (let ((var (gensym)))
    `(let ((,var ,variable))
       (cond ((eq ,var :deleted) ,deleted)
	     ((eq (sb!di:debug-var-validity ,var ,location) :valid)
	      ,valid)
	     (t ,other)))))

) ; EVAL-WHEN

;;; This is used in constructing arg lists for debugger printing when
;;; the arg list is unavailable, some arg is unavailable or unused, etc.
(defstruct (unprintable-object
	    (:constructor make-unprintable-object (string))
	    (:print-object (lambda (x s)
			     (print-unreadable-object (x s)
			       (write-string (unprintable-object-string x)
					     s))))
	    (:copier nil))
  string)

;;; Extract the function argument values for a debug frame.
(defun frame-args-as-list (frame)
  (let ((debug-fun (sb!di:frame-debug-fun frame))
	(loc (sb!di:frame-code-location frame))
	(reversed-result nil))
    (handler-case
	(progn
	  (dolist (ele (sb!di:debug-fun-lambda-list debug-fun))
	    (lambda-list-element-dispatch ele
	     :required ((push (frame-call-arg ele loc frame) reversed-result))
	     :optional ((push (frame-call-arg (second ele) loc frame)
			      reversed-result))
	     :keyword ((push (second ele) reversed-result)
		       (push (frame-call-arg (third ele) loc frame)
			     reversed-result))
	     :deleted ((push (frame-call-arg ele loc frame) reversed-result))
	     :rest ((lambda-var-dispatch (second ele) loc
		     nil
		     (progn
		       (setf reversed-result
			     (append (reverse (sb!di:debug-var-value
					       (second ele) frame))
				     reversed-result))
		       (return))
		     (push (make-unprintable-object
			    "unavailable &REST argument")
		     reversed-result)))))
	  ;; As long as we do an ordinary return (as opposed to SIGNALing
	  ;; a CONDITION) from the DOLIST above:
	  (nreverse reversed-result))
      (sb!di:lambda-list-unavailable
       ()
       (make-unprintable-object "unavailable lambda list")))))

;;; Print FRAME with verbosity level 1. If we hit a &REST arg, then
;;; print as many of the values as possible, punting the loop over
;;; lambda-list variables since any other arguments will be in the
;;; &REST arg's list of values.
(defun print-frame-call-1 (frame)
  (let ((debug-fun (sb!di:frame-debug-fun frame)))

    (pprint-logical-block (*standard-output* nil :prefix "(" :suffix ")")
      (let ((args (ensure-printable-object (frame-args-as-list frame))))
	;; Since we go to some trouble to make nice informative function
	;; names like (PRINT-OBJECT :AROUND (CLOWN T)), let's make sure
	;; that they aren't truncated by *PRINT-LENGTH* and *PRINT-LEVEL*.
	(let ((*print-length* nil)
	      (*print-level* nil))
	  (prin1 (ensure-printable-object (sb!di:debug-fun-name debug-fun))))
	;; For the function arguments, we can just print normally.
        (if (listp args)
            (format t "~{ ~_~S~}" args)
            (format t " ~S" args))))

    (when (sb!di:debug-fun-kind debug-fun)
      (write-char #\[)
      (prin1 (sb!di:debug-fun-kind debug-fun))
      (write-char #\]))))

(defun ensure-printable-object (object)
  (handler-case
      (with-open-stream (out (make-broadcast-stream))
	(prin1 object out)
	object)
    (error (cond)
      (declare (ignore cond))
      (make-unprintable-object "error printing object"))))

(defun frame-call-arg (var location frame)
  (lambda-var-dispatch var location
    (make-unprintable-object "unused argument")
    (sb!di:debug-var-value var frame)
    (make-unprintable-object "unavailable argument")))

;;; Prints a representation of the function call causing FRAME to
;;; exist. VERBOSITY indicates the level of information to output;
;;; zero indicates just printing the DEBUG-FUN's name, and one
;;; indicates displaying call-like, one-liner format with argument
;;; values.
(defun print-frame-call (frame &key (verbosity 1) (number nil))
  (cond
   ((zerop verbosity)
    (when number
      (format t "~&~S: " (sb!di:frame-number frame)))
    (format t "~S" frame))
   (t
    (when number
      (format t "~&~S: " (sb!di:frame-number frame)))
    (print-frame-call-1 frame)))
  (when (>= verbosity 2)
    (let ((loc (sb!di:frame-code-location frame)))
      (handler-case
	  (progn
	    (sb!di:code-location-debug-block loc)
	    (format t "~%source: ")
	    (print-code-location-source-form loc 0))
	(sb!di:debug-condition (ignore) ignore)
	(error (c) (format t "error finding source: ~A" c))))))

;;;; INVOKE-DEBUGGER

(defvar *debugger-hook* nil
  #!+sb-doc
  "This is either NIL or a function of two arguments, a condition and the value
   of *DEBUGGER-HOOK*. This function can either handle the condition or return
   which causes the standard debugger to execute. The system passes the value
   of this variable to the function because it binds *DEBUGGER-HOOK* to NIL
   around the invocation.")

;;; These are bound on each invocation of INVOKE-DEBUGGER.
(defvar *debug-restarts*)
(defvar *debug-condition*)
(defvar *nested-debug-condition*)

(defun invoke-debugger (condition)
  #!+sb-doc
  "Enter the debugger."
  (let ((old-hook *debugger-hook*))
    (when old-hook
      (let ((*debugger-hook* nil))
	(funcall old-hook condition old-hook))))

  ;; If we're a background thread and *background-threads-wait-for-debugger*
  ;; is NIL, this will invoke a restart

  ;; Note: CMU CL had (SB-UNIX:UNIX-SIGSETMASK 0) here. I deleted it
  ;; around sbcl-0.7.8.5 (by which time it had mutated to have a
  ;; #!-SUNOS prefix and a FIXME note observing that it wasn't needed
  ;; on SunOS and no one knew why it was needed anywhere else either).
  ;; So if something mysteriously breaks that has worked since the CMU
  ;; CL days, that might be why. -- WHN 2002-09-28

  ;; We definitely want *PACKAGE* to be of valid type.
  ;;
  ;; Elsewhere in the system, we use the SANE-PACKAGE function for
  ;; this, but here causing an exception just as we're trying to handle
  ;; an exception would be confusing, so instead we use a special hack.
  (unless (and (packagep *package*)
	       (package-name *package*))
    (setf *package* (find-package :cl-user))
    (format *error-output*
	    "The value of ~S was not an undeleted PACKAGE. It has been
reset to ~S."
	    '*package* *package*))

  ;; Try to force the other special variables into a useful state.
  (let (;; Protect from WITH-STANDARD-IO-SYNTAX some variables where
	;; any default we might use is less useful than just reusing
	;; the global values.
	(original-package *package*)
	(original-print-pretty *print-pretty*))
    (with-standard-io-syntax
     (let ((*debug-condition* condition)
	   (*debug-restarts* (compute-restarts condition))
	   (*nested-debug-condition* nil)
	   ;; We want the printer and reader to be in a useful state,
	   ;; regardless of where the debugger was invoked in the
	   ;; program. WITH-STANDARD-IO-SYNTAX did much of what we
	   ;; want, but
	   ;;   * It doesn't affect our internal special variables 
	   ;;     like *CURRENT-LEVEL-IN-PRINT*.
	   ;;   * It isn't customizable.
	   ;;   * It doesn't set *PRINT-READABLY* to the same value
	   ;;     as the toplevel default.
	   ;;   * It sets *PACKAGE* to COMMON-LISP-USER, which is not
	   ;;     helpful behavior for a debugger.
	   ;;   * There's no particularly good debugger default for
	   ;;     *PRINT-PRETTY*, since T is usually what you want
	   ;;     -- except absolutely not what you want when you're
	   ;;     debugging failures in PRINT-OBJECT logic.
	   ;; We try to address all these issues with explicit
	   ;; rebindings here.
	   (sb!kernel:*current-level-in-print* 0)
	   (*print-length* *debug-print-length*)
	   (*print-level* *debug-print-level*)
	   (*readtable* *debug-readtable*)
	   (*print-readably* nil)
	   (*package* original-package)
	   (background-p nil)
	   (*print-pretty* original-print-pretty))

       ;; Before we start our own output, finish any pending output.
       ;; Otherwise, if the user tried to track the progress of his
       ;; program using PRINT statements, he'd tend to lose the last
       ;; line of output or so, which'd be confusing.
       (flush-standard-output-streams)

       ;; (The initial output here goes to *ERROR-OUTPUT*, because the
       ;; initial output is not interactive, just an error message,
       ;; and when people redirect *ERROR-OUTPUT*, they could
       ;; reasonably expect to see error messages logged there,
       ;; regardless of what the debugger does afterwards.)
       (handler-case
	   (format *error-output*
		   "~2&~@<debugger invoked on condition of type ~S: ~
                    ~2I~_~A~:>~%"
		   (type-of *debug-condition*)
		   *debug-condition*)
	 (error (condition)
           (setf *nested-debug-condition* condition)
	   (let ((ndc-type (type-of *nested-debug-condition*)))
	     (format *error-output*
		     "~&~@<(A ~S was caught when trying to print ~S when ~
                      entering the debugger. Printing was aborted and the ~
                      ~S was stored in ~S.)~@:>~%"
		     ndc-type
		     '*debug-condition*
		     ndc-type
		     '*nested-debug-condition*))
	   (when (typep condition 'cell-error)
	     ;; what we really want to know when it's e.g. an UNBOUND-VARIABLE:
	     (format *error-output*
		     "~&(CELL-ERROR-NAME ~S) = ~S~%"
		     '*debug-condition*
		     (cell-error-name *debug-condition*)))))

       ;; After the initial error/condition/whatever announcement to
       ;; *ERROR-OUTPUT*, we become interactive, and should talk on
       ;; *DEBUG-IO* from now on. (KLUDGE: This is a normative
       ;; statement, not a description of reality.:-| There's a lot of
       ;; older debugger code which was written to do i/o on whatever
       ;; stream was in fashion at the time, and not all of it has
       ;; been converted to behave this way. -- WHN 2000-11-16)

       (setf background-p
	     (sb!thread::debugger-wait-until-foreground-thread *debug-io*))
       (unwind-protect
       (let (;; FIXME: Rebinding *STANDARD-OUTPUT* here seems wrong,
	     ;; violating the principle of least surprise, and making
	     ;; it impossible for the user to do reasonable things
	     ;; like using PRINT at the debugger prompt to send output
	     ;; to the program's ordinary (possibly
	     ;; redirected-to-a-file) *STANDARD-OUTPUT*. (CMU CL
	     ;; used to rebind *STANDARD-INPUT* here too, but that's
	     ;; been fixed already.)
	     (*standard-output* *debug-io*)
	     ;; This seems reasonable: e.g. if the user has redirected
	     ;; *ERROR-OUTPUT* to some log file, it's probably wrong
	     ;; to send errors which occur in interactive debugging to
	     ;; that file, and right to send them to *DEBUG-IO*.
	     (*error-output* *debug-io*))
	 (unless (typep condition 'step-condition)
	   (when *debug-beginner-help-p*
	     (format *debug-io*
		     "~%~@<Within the debugger, you can type HELP for help. ~
                      At any command prompt (within the debugger or not) you ~
                      can type (SB-EXT:QUIT) to terminate the SBCL ~
                      executable. The condition which caused the debugger to ~
                      be entered is bound to ~S. You can suppress this ~
                      message by clearing ~S.~:@>~2%"
		     '*debug-condition*
		     '*debug-beginner-help-p*))
	   (show-restarts *debug-restarts* *debug-io*))
	      (internal-debug))
	 (when background-p (sb!thread::release-foreground)))))))

(defun show-restarts (restarts s)
  (cond ((null restarts)
	 (format s
		 "~&(no restarts: If you didn't do this on purpose, ~
                  please report it as a bug.)~%"))
	(t
	 (format s "~&restarts:~%")
	 (let ((count 0)
	       (names-used '(nil))
	       (max-name-len 0))
	   (dolist (restart restarts)
	     (let ((name (restart-name restart)))
	       (when name
		 (let ((len (length (princ-to-string name))))
		   (when (> len max-name-len)
		     (setf max-name-len len))))))
	   (unless (zerop max-name-len)
	     (incf max-name-len 3))
	   (dolist (restart restarts)
	     (let ((name (restart-name restart)))
	       (cond ((member name names-used)
		      (format s "~& ~2D: ~V@T~A~%" count max-name-len restart))
		     (t
		      (format s "~& ~2D: [~VA] ~A~%"
			      count (- max-name-len 3) name restart)
		      (push name names-used))))
	     (incf count))))))

;;; This calls DEBUG-LOOP, performing some simple initializations
;;; before doing so. INVOKE-DEBUGGER calls this to actually get into
;;; the debugger. SB!KERNEL::ERROR-ERROR calls this in emergencies
;;; to get into a debug prompt as quickly as possible with as little
;;; risk as possible for stepping on whatever is causing recursive
;;; errors.
(defun internal-debug ()
  (let ((*in-the-debugger* t)
	(*read-suppress* nil))
    (unless (typep *debug-condition* 'step-condition)
      (clear-input *debug-io*))
    (debug-loop)))

;;;; DEBUG-LOOP

;;; Note: This defaulted to T in CMU CL. The changed default in SBCL
;;; was motivated by desire to play nicely with ILISP.
(defvar *flush-debug-errors* nil
  #!+sb-doc
  "When set, avoid calling INVOKE-DEBUGGER recursively when errors occur while
   executing in the debugger.")

(defun debug-loop ()
  (let* ((*debug-command-level* (1+ *debug-command-level*))
	 (*real-stack-top* (sb!di:top-frame))
	 (*stack-top* (or *stack-top-hint* *real-stack-top*))
	 (*stack-top-hint* nil)
	 (*current-frame* *stack-top*))
    (handler-bind ((sb!di:debug-condition
		    (lambda (condition)
		      (princ condition *debug-io*)
		      (/show0 "handling d-c by THROWing DEBUG-LOOP-CATCHER")
		      (throw 'debug-loop-catcher nil))))
      (fresh-line)
      (print-frame-call *current-frame* :verbosity 2)
      (loop
	(catch 'debug-loop-catcher
	  (handler-bind ((error (lambda (condition)
				  (when *flush-debug-errors*
				    (clear-input *debug-io*)
				    (princ condition)
				    ;; FIXME: Doing input on *DEBUG-IO*
				    ;; and output on T seems broken.
				    (format t
					    "~&error flushed (because ~
					     ~S is set)"
					    '*flush-debug-errors*)
				    (/show0 "throwing DEBUG-LOOP-CATCHER")
				    (throw 'debug-loop-catcher nil)))))
	    ;; We have to bind LEVEL for the restart function created by
	    ;; WITH-SIMPLE-RESTART.
	    (let ((level *debug-command-level*)
		  (restart-commands (make-restart-commands)))
	      (with-simple-restart (abort
				   "~@<Reduce debugger level (to debug level ~W).~@:>"
				    level)
		(debug-prompt *debug-io*)
		(force-output *debug-io*)
		(let* ((exp (read *debug-io*))
		       (cmd-fun (debug-command-p exp restart-commands)))
		  (cond ((not cmd-fun)
			 (debug-eval-print exp))
			((consp cmd-fun)
			 (format t "~&Your command, ~S, is ambiguous:~%"
				 exp)
			 (dolist (ele cmd-fun)
			   (format t "   ~A~%" ele)))
			(t
			 (funcall cmd-fun))))))))))))

;;; FIXME: We could probably use INTERACTIVE-EVAL for much of this logic.
(defun debug-eval-print (expr)
  (/noshow "entering DEBUG-EVAL-PRINT" expr)
  (/noshow (fboundp 'compile))
  (setq +++ ++ ++ + + - - expr)
  (let* ((values (multiple-value-list (eval -)))
	 (*standard-output* *debug-io*))
    (/noshow "done with EVAL in DEBUG-EVAL-PRINT")
    (fresh-line)
    (if values (prin1 (car values)))
    (dolist (x (cdr values))
      (fresh-line)
      (prin1 x))
    (setq /// // // / / values)
    (setq *** ** ** * * (car values))
    ;; Make sure that nobody passes back an unbound marker.
    (unless (boundp '*)
      (setq * nil)
      (fresh-line)
      ;; FIXME: The way INTERACTIVE-EVAL does this seems better.
      (princ "Setting * to NIL (was unbound marker)."))))

;;;; debug loop functions

;;; These commands are functions, not really commands, so that users
;;; can get their hands on the values returned.

(eval-when (:execute :compile-toplevel)

(sb!xc:defmacro define-var-operation (ref-or-set &optional value-var)
  `(let* ((temp (etypecase name
		  (symbol (sb!di:debug-fun-symbol-vars
			   (sb!di:frame-debug-fun *current-frame*)
			   name))
		  (simple-string (sb!di:ambiguous-debug-vars
				  (sb!di:frame-debug-fun *current-frame*)
				  name))))
	  (location (sb!di:frame-code-location *current-frame*))
	  ;; Let's only deal with valid variables.
	  (vars (remove-if-not (lambda (v)
				 (eq (sb!di:debug-var-validity v location)
				     :valid))
			       temp)))
     (declare (list vars))
     (cond ((null vars)
	    (error "No known valid variables match ~S." name))
	   ((= (length vars) 1)
	    ,(ecase ref-or-set
	       (:ref
		'(sb!di:debug-var-value (car vars) *current-frame*))
	       (:set
		`(setf (sb!di:debug-var-value (car vars) *current-frame*)
		       ,value-var))))
	   (t
	    ;; Since we have more than one, first see whether we have
	    ;; any variables that exactly match the specification.
	    (let* ((name (etypecase name
			   (symbol (symbol-name name))
			   (simple-string name)))
		   ;; FIXME: REMOVE-IF-NOT is deprecated, use STRING/=
		   ;; instead.
		   (exact (remove-if-not (lambda (v)
					   (string= (sb!di:debug-var-symbol-name v)
						    name))
					 vars))
		   (vars (or exact vars)))
	      (declare (simple-string name)
		       (list exact vars))
	      (cond
	       ;; Check now for only having one variable.
	       ((= (length vars) 1)
		,(ecase ref-or-set
		   (:ref
		    '(sb!di:debug-var-value (car vars) *current-frame*))
		   (:set
		    `(setf (sb!di:debug-var-value (car vars) *current-frame*)
			   ,value-var))))
	       ;; If there weren't any exact matches, flame about
	       ;; ambiguity unless all the variables have the same
	       ;; name.
	       ((and (not exact)
		     (find-if-not
		      (lambda (v)
			(string= (sb!di:debug-var-symbol-name v)
				 (sb!di:debug-var-symbol-name (car vars))))
		      (cdr vars)))
		(error "specification ambiguous:~%~{   ~A~%~}"
		       (mapcar #'sb!di:debug-var-symbol-name
			       (delete-duplicates
				vars :test #'string=
				:key #'sb!di:debug-var-symbol-name))))
	       ;; All names are the same, so see whether the user
	       ;; ID'ed one of them.
	       (id-supplied
		(let ((v (find id vars :key #'sb!di:debug-var-id)))
		  (unless v
		    (error
		     "invalid variable ID, ~W: should have been one of ~S"
		     id
		     (mapcar #'sb!di:debug-var-id vars)))
		  ,(ecase ref-or-set
		     (:ref
		      '(sb!di:debug-var-value v *current-frame*))
		     (:set
		      `(setf (sb!di:debug-var-value v *current-frame*)
			     ,value-var)))))
	       (t
		(error "Specify variable ID to disambiguate ~S. Use one of ~S."
		       name
		       (mapcar #'sb!di:debug-var-id vars)))))))))

) ; EVAL-WHEN

;;; FIXME: This doesn't work. It would be real nice we could make it
;;; work! Alas, it doesn't seem to work in CMU CL X86 either..
(defun var (name &optional (id 0 id-supplied))
  #!+sb-doc
  "Return a variable's value if possible. NAME is a simple-string or symbol.
   If it is a simple-string, it is an initial substring of the variable's name.
   If name is a symbol, it has the same name and package as the variable whose
   value this function returns. If the symbol is uninterned, then the variable
   has the same name as the symbol, but it has no package.

   If name is the initial substring of variables with different names, then
   this return no values after displaying the ambiguous names. If name
   determines multiple variables with the same name, then you must use the
   optional id argument to specify which one you want. If you left id
   unspecified, then this returns no values after displaying the distinguishing
   id values.

   The result of this function is limited to the availability of variable
   information. This is SETF'able."
  (define-var-operation :ref))
(defun (setf var) (value name &optional (id 0 id-supplied))
  (define-var-operation :set value))

;;; This returns the COUNT'th arg as the user sees it from args, the
;;; result of SB!DI:DEBUG-FUN-LAMBDA-LIST. If this returns a
;;; potential DEBUG-VAR from the lambda-list, then the second value is
;;; T. If this returns a keyword symbol or a value from a rest arg,
;;; then the second value is NIL.
;;;
;;; FIXME: There's probably some way to merge the code here with
;;; FRAME-ARGS-AS-LIST. (A fair amount of logic is already shared
;;; through LAMBDA-LIST-ELEMENT-DISPATCH, but I suspect more could be.)
(declaim (ftype (function (index list)) nth-arg))
(defun nth-arg (count args)
  (let ((n count))
    (dolist (ele args (error "The argument specification ~S is out of range."
			     n))
      (lambda-list-element-dispatch ele
	:required ((if (zerop n) (return (values ele t))))
	:optional ((if (zerop n) (return (values (second ele) t))))
	:keyword ((cond ((zerop n)
			 (return (values (second ele) nil)))
			((zerop (decf n))
			 (return (values (third ele) t)))))
	:deleted ((if (zerop n) (return (values ele t))))
	:rest ((let ((var (second ele)))
		 (lambda-var-dispatch var (sb!di:frame-code-location
					   *current-frame*)
		   (error "unused &REST argument before n'th argument")
		   (dolist (value
			    (sb!di:debug-var-value var *current-frame*)
			    (error
			     "The argument specification ~S is out of range."
			     n))
		     (if (zerop n)
			 (return-from nth-arg (values value nil))
			 (decf n)))
		   (error "invalid &REST argument before n'th argument")))))
      (decf n))))

(defun arg (n)
  #!+sb-doc
  "Return the N'th argument's value if possible. Argument zero is the first
   argument in a frame's default printed representation. Count keyword/value
   pairs as separate arguments."
  (multiple-value-bind (var lambda-var-p)
      (nth-arg n (handler-case (sb!di:debug-fun-lambda-list
				(sb!di:frame-debug-fun *current-frame*))
		   (sb!di:lambda-list-unavailable ()
		     (error "No argument values are available."))))
    (if lambda-var-p
	(lambda-var-dispatch var (sb!di:frame-code-location *current-frame*)
	  (error "Unused arguments have no values.")
	  (sb!di:debug-var-value var *current-frame*)
	  (error "invalid argument value"))
	var)))

;;;; machinery for definition of debug loop commands

(defvar *debug-commands* nil)

;;; Interface to *DEBUG-COMMANDS*. No required arguments in args are
;;; permitted.
(defmacro !def-debug-command (name args &rest body)
  (let ((fun-name (symbolicate name "-DEBUG-COMMAND")))
    `(progn
       (setf *debug-commands*
	     (remove ,name *debug-commands* :key #'car :test #'string=))
       (defun ,fun-name ,args
	 (unless *in-the-debugger*
	   (error "invoking debugger command while outside the debugger"))
	 ,@body)
       (push (cons ,name #',fun-name) *debug-commands*)
       ',fun-name)))

(defun !def-debug-command-alias (new-name existing-name)
  (let ((pair (assoc existing-name *debug-commands* :test #'string=)))
    (unless pair (error "unknown debug command name: ~S" existing-name))
    (push (cons new-name (cdr pair)) *debug-commands*))
  new-name)

;;; This takes a symbol and uses its name to find a debugger command,
;;; using initial substring matching. It returns the command function
;;; if form identifies only one command, but if form is ambiguous,
;;; this returns a list of the command names. If there are no matches,
;;; this returns nil. Whenever the loop that looks for a set of
;;; possibilities encounters an exact name match, we return that
;;; command function immediately.
(defun debug-command-p (form &optional other-commands)
  (if (or (symbolp form) (integerp form))
      (let* ((name
	      (if (symbolp form)
		  (symbol-name form)
		  (format nil "~W" form)))
	     (len (length name))
	     (res nil))
	(declare (simple-string name)
		 (fixnum len)
		 (list res))

	;; Find matching commands, punting if exact match.
	(flet ((match-command (ele)
		 (let* ((str (car ele))
			(str-len (length str)))
		   (declare (simple-string str)
			    (fixnum str-len))
		   (cond ((< str-len len))
			 ((= str-len len)
			  (when (string= name str :end1 len :end2 len)
			    (return-from debug-command-p (cdr ele))))
			 ((string= name str :end1 len :end2 len)
			  (push ele res))))))
	  (mapc #'match-command *debug-commands*)
	  (mapc #'match-command other-commands))

	;; Return the right value.
	(cond ((not res) nil)
	      ((= (length res) 1)
	       (cdar res))
	      (t ; Just return the names.
	       (do ((cmds res (cdr cmds)))
		   ((not cmds) res)
		 (setf (car cmds) (caar cmds))))))))

;;; Return a list of debug commands (in the same format as
;;; *DEBUG-COMMANDS*) that invoke each active restart.
;;;
;;; Two commands are made for each restart: one for the number, and
;;; one for the restart name (unless it's been shadowed by an earlier
;;; restart of the same name, or it is NIL).
(defun make-restart-commands (&optional (restarts *debug-restarts*))
  (let ((commands)
	(num 0))			; better be the same as show-restarts!
    (dolist (restart restarts)
      (let ((name (string (restart-name restart))))
        (let ((restart-fun
                (lambda ()
		  (/show0 "in restart-command closure, about to i-r-i")
		  (invoke-restart-interactively restart))))
          (push (cons (prin1-to-string num) restart-fun) commands)
          (unless (or (null (restart-name restart)) 
                      (find name commands :key #'car :test #'string=))
            (push (cons name restart-fun) commands))))
    (incf num))
  commands))

;;;; frame-changing commands

(!def-debug-command "UP" ()
  (let ((next (sb!di:frame-up *current-frame*)))
    (cond (next
	   (setf *current-frame* next)
	   (print-frame-call next))
	  (t
	   (format t "~&Top of stack.")))))

(!def-debug-command "DOWN" ()
  (let ((next (sb!di:frame-down *current-frame*)))
    (cond (next
	   (setf *current-frame* next)
	   (print-frame-call next))
	  (t
	   (format t "~&Bottom of stack.")))))

(!def-debug-command-alias "D" "DOWN")

;;; CMU CL had this command, but SBCL doesn't, since it's redundant
;;; with "FRAME 0", and it interferes with abbreviations for the
;;; TOPLEVEL restart.
;;;(!def-debug-command "TOP" ()
;;;  (do ((prev *current-frame* lead)
;;;       (lead (sb!di:frame-up *current-frame*) (sb!di:frame-up lead)))
;;;      ((null lead)
;;;       (setf *current-frame* prev)
;;;       (print-frame-call prev))))

(!def-debug-command "BOTTOM" ()
  (do ((prev *current-frame* lead)
       (lead (sb!di:frame-down *current-frame*) (sb!di:frame-down lead)))
      ((null lead)
       (setf *current-frame* prev)
       (print-frame-call prev))))

(!def-debug-command-alias "B" "BOTTOM")

(!def-debug-command "FRAME" (&optional
			     (n (read-prompting-maybe "frame number: ")))
  (setf *current-frame*
	(multiple-value-bind (next-frame-fun limit-string)
	    (if (< n (sb!di:frame-number *current-frame*))
		(values #'sb!di:frame-up "top")
	      (values #'sb!di:frame-down "bottom"))
	  (do ((frame *current-frame*))
	      ((= n (sb!di:frame-number frame))
	       frame)
	    (let ((next-frame (funcall next-frame-fun frame)))
	      (cond (next-frame
		     (setf frame next-frame))
		    (t
		     (format t
			     "The ~A of the stack was encountered.~%"
			     limit-string)
		     (return frame)))))))
  (print-frame-call *current-frame*))

(!def-debug-command-alias "F" "FRAME")

;;;; commands for entering and leaving the debugger

;;; CMU CL supported this QUIT debug command, but SBCL provides this
;;; functionality with a restart instead. (The QUIT debug command was
;;; removed because it's confusing to have "quit" mean two different
;;; things in the system, "restart the top level REPL" in the debugger
;;; and "terminate the Lisp system" as the SB-EXT:QUIT function.)
;;;
;;;(!def-debug-command "QUIT" ()
;;;  (throw 'sb!impl::toplevel-catcher nil))

;;; CMU CL supported this GO debug command, but SBCL doesn't -- in
;;; SBCL you just type the CONTINUE restart name instead (or "C" or
;;; "RESTART CONTINUE", that's OK too).
;;;(!def-debug-command "GO" ()
;;;  (continue *debug-condition*)
;;;  (error "There is no restart named CONTINUE."))

(!def-debug-command "RESTART" ()
  (/show0 "doing RESTART debug-command")
  (let ((num (read-if-available :prompt)))
    (when (eq num :prompt)
      (show-restarts *debug-restarts* *debug-io*)
      (write-string "restart: ")
      (force-output)
      (setf num (read *debug-io*)))
    (let ((restart (typecase num
		     (unsigned-byte
		      (nth num *debug-restarts*))
		     (symbol
		      (find num *debug-restarts* :key #'restart-name
			    :test (lambda (sym1 sym2)
				    (string= (symbol-name sym1)
					     (symbol-name sym2)))))
		     (t
		      (format t "~S is invalid as a restart name.~%" num)
		      (return-from restart-debug-command nil)))))
      (/show0 "got RESTART")
      (if restart
	  (invoke-restart-interactively restart)
	  ;; FIXME: Even if this isn't handled by WARN, it probably
	  ;; shouldn't go to *STANDARD-OUTPUT*, but *ERROR-OUTPUT* or
	  ;; *QUERY-IO* or something. Look through this file to
	  ;; straighten out stream usage.
	  (princ "There is no such restart.")))))

;;;; information commands

(!def-debug-command "HELP" ()
  ;; CMU CL had a little toy pager here, but "if you aren't running
  ;; ILISP (or a smart windowing system, or something) you deserve to
  ;; lose", so we've dropped it in SBCL. However, in case some
  ;; desperate holdout is running this on a dumb terminal somewhere,
  ;; we tell him where to find the message stored as a string.
  (format *debug-io*
	  "~&~A~2%(The HELP string is stored in ~S.)~%"
	  *debug-help-string*
	  '*debug-help-string*))

(!def-debug-command-alias "?" "HELP")

(!def-debug-command "ERROR" ()
  (format *debug-io* "~A~%" *debug-condition*)
  (show-restarts *debug-restarts* *debug-io*))

(!def-debug-command "BACKTRACE" ()
  (backtrace (read-if-available most-positive-fixnum)))

(!def-debug-command "PRINT" ()
  (print-frame-call *current-frame*))

(!def-debug-command-alias "P" "PRINT")

(!def-debug-command "LIST-LOCALS" ()
  (let ((d-fun (sb!di:frame-debug-fun *current-frame*)))
    (if (sb!di:debug-var-info-available d-fun)
	(let ((*standard-output* *debug-io*)
	      (location (sb!di:frame-code-location *current-frame*))
	      (prefix (read-if-available nil))
	      (any-p nil)
	      (any-valid-p nil))
	  (dolist (v (sb!di:ambiguous-debug-vars
			d-fun
			(if prefix (string prefix) "")))
	    (setf any-p t)
	    (when (eq (sb!di:debug-var-validity v location) :valid)
	      (setf any-valid-p t)
	      (format t "~S~:[#~W~;~*~]  =  ~S~%"
		      (sb!di:debug-var-symbol v)
		      (zerop (sb!di:debug-var-id v))
		      (sb!di:debug-var-id v)
		      (sb!di:debug-var-value v *current-frame*))))

	  (cond
	   ((not any-p)
	    (format t "There are no local variables ~@[starting with ~A ~]~
		       in the function."
		    prefix))
	   ((not any-valid-p)
	    (format t "All variables ~@[starting with ~A ~]currently ~
		       have invalid values."
		    prefix))))
	(write-line "There is no variable information available."))))

(!def-debug-command-alias "L" "LIST-LOCALS")

(!def-debug-command "SOURCE" ()
  (fresh-line)
  (print-code-location-source-form (sb!di:frame-code-location *current-frame*)
				   (read-if-available 0)))

;;;; source location printing

;;; We cache a stream to the last valid file debug source so that we
;;; won't have to repeatedly open the file.
;;;
;;; KLUDGE: This sounds like a bug, not a feature. Opening files is fast
;;; in the 1990s, so the benefit is negligible, less important than the
;;; potential of extra confusion if someone changes the source during
;;; a debug session and the change doesn't show up. And removing this
;;; would simplify the system, which I like. -- WHN 19990903
(defvar *cached-debug-source* nil)
(declaim (type (or sb!di:debug-source null) *cached-debug-source*))
(defvar *cached-source-stream* nil)
(declaim (type (or stream null) *cached-source-stream*))

;;; To suppress the read-time evaluation #. macro during source read,
;;; *READTABLE* is modified. *READTABLE* is cached to avoid
;;; copying it each time, and invalidated when the
;;; *CACHED-DEBUG-SOURCE* has changed.
(defvar *cached-readtable* nil)
(declaim (type (or readtable null) *cached-readtable*))

(pushnew (lambda ()
	   (setq *cached-debug-source* nil *cached-source-stream* nil
		 *cached-readtable* nil))
	 *before-save-initializations*)

;;; We also cache the last toplevel form that we printed a source for
;;; so that we don't have to do repeated reads and calls to
;;; FORM-NUMBER-TRANSLATIONS.
(defvar *cached-toplevel-form-offset* nil)
(declaim (type (or index null) *cached-toplevel-form-offset*))
(defvar *cached-toplevel-form*)
(defvar *cached-form-number-translations*)

;;; Given a code location, return the associated form-number
;;; translations and the actual top level form. We check our cache ---
;;; if there is a miss, we dispatch on the kind of the debug source.
(defun get-toplevel-form (location)
  (let ((d-source (sb!di:code-location-debug-source location)))
    (if (and (eq d-source *cached-debug-source*)
	     (eql (sb!di:code-location-toplevel-form-offset location)
		  *cached-toplevel-form-offset*))
	(values *cached-form-number-translations* *cached-toplevel-form*)
	(let* ((offset (sb!di:code-location-toplevel-form-offset location))
	       (res
		(ecase (sb!di:debug-source-from d-source)
		  (:file (get-file-toplevel-form location))
		  (:lisp (svref (sb!di:debug-source-name d-source) offset)))))
	  (setq *cached-toplevel-form-offset* offset)
	  (values (setq *cached-form-number-translations*
			(sb!di:form-number-translations res offset))
		  (setq *cached-toplevel-form* res))))))

;;; Locate the source file (if it still exists) and grab the top level
;;; form. If the file is modified, we use the top level form offset
;;; instead of the recorded character offset.
(defun get-file-toplevel-form (location)
  (let* ((d-source (sb!di:code-location-debug-source location))
	 (tlf-offset (sb!di:code-location-toplevel-form-offset location))
	 (local-tlf-offset (- tlf-offset
			      (sb!di:debug-source-root-number d-source)))
	 (char-offset
	  (aref (or (sb!di:debug-source-start-positions d-source)
		    (error "no start positions map"))
		local-tlf-offset))
	 (name (sb!di:debug-source-name d-source)))
    (unless (eq d-source *cached-debug-source*)
      (unless (and *cached-source-stream*
		   (equal (pathname *cached-source-stream*)
			  (pathname name)))
	(setq *cached-readtable* nil)
	(when *cached-source-stream* (close *cached-source-stream*))
	(setq *cached-source-stream* (open name :if-does-not-exist nil))
	(unless *cached-source-stream*
	  (error "The source file no longer exists:~%  ~A" (namestring name)))
	(format t "~%; file: ~A~%" (namestring name)))

	(setq *cached-debug-source*
	      (if (= (sb!di:debug-source-created d-source)
		     (file-write-date name))
		  d-source nil)))

    (cond
     ((eq *cached-debug-source* d-source)
      (file-position *cached-source-stream* char-offset))
     (t
      (format t "~%; File has been modified since compilation:~%;   ~A~@
		 ; Using form offset instead of character position.~%"
	      (namestring name))
      (file-position *cached-source-stream* 0)
      (let ((*read-suppress* t))
	(dotimes (i local-tlf-offset)
	  (read *cached-source-stream*)))))
    (unless *cached-readtable*
      (setq *cached-readtable* (copy-readtable))
      (set-dispatch-macro-character
       #\# #\.
       (lambda (stream sub-char &rest rest)
	 (declare (ignore rest sub-char))
	 (let ((token (read stream t nil t)))
	   (format nil "#.~S" token)))
       *cached-readtable*))
    (let ((*readtable* *cached-readtable*))
      (read *cached-source-stream*))))

(defun print-code-location-source-form (location context)
  (let* ((location (maybe-block-start-location location))
	 (form-num (sb!di:code-location-form-number location)))
    (multiple-value-bind (translations form) (get-toplevel-form location)
      (unless (< form-num (length translations))
	(error "The source path no longer exists."))
      (prin1 (sb!di:source-path-context form
					(svref translations form-num)
					context)))))

;;; breakpoint and step commands

;;; Step to the next code-location.
(!def-debug-command "STEP" ()
  (setf *number-of-steps* (read-if-available 1))
  (set-step-breakpoint *current-frame*)
  (continue *debug-condition*)
  (error "couldn't continue"))

;;; List possible breakpoint locations, which ones are active, and
;;; where the CONTINUE restart will transfer control. Set
;;; *POSSIBLE-BREAKPOINTS* to the code-locations which can then be
;;; used by sbreakpoint.
(!def-debug-command "LIST-LOCATIONS" ()
  (let ((df (read-if-available *default-breakpoint-debug-fun*)))
    (cond ((consp df)
	   (setf df (sb!di:fun-debug-fun (eval df)))
	   (setf *default-breakpoint-debug-fun* df))
	  ((or (eq ':c df)
	       (not *default-breakpoint-debug-fun*))
	   (setf df (sb!di:frame-debug-fun *current-frame*))
	   (setf *default-breakpoint-debug-fun* df)))
    (setf *possible-breakpoints* (possible-breakpoints df)))
  (let ((continue-at (sb!di:frame-code-location *current-frame*)))
    (let ((active (location-in-list *default-breakpoint-debug-fun*
				    *breakpoints* :fun-start))
	  (here (sb!di:code-location=
		 (sb!di:debug-fun-start-location
		  *default-breakpoint-debug-fun*) continue-at)))
      (when (or active here)
	(format t "::FUN-START ")
	(when active (format t " *Active*"))
	(when here (format t " *Continue here*"))))

    (let ((prev-location nil)
	  (prev-num 0)
	  (this-num 0))
      (flet ((flush ()
	       (when prev-location
		 (let ((this-num (1- this-num)))
		   (if (= prev-num this-num)
		       (format t "~&~W: " prev-num)
		       (format t "~&~W-~W: " prev-num this-num)))
		 (print-code-location-source-form prev-location 0)
		 (when *print-location-kind*
		   (format t "~S " (sb!di:code-location-kind prev-location)))
		 (when (location-in-list prev-location *breakpoints*)
		   (format t " *Active*"))
		 (when (sb!di:code-location= prev-location continue-at)
		   (format t " *Continue here*")))))
	
	(dolist (code-location *possible-breakpoints*)
	  (when (or *print-location-kind*
		    (location-in-list code-location *breakpoints*)
		    (sb!di:code-location= code-location continue-at)
		    (not prev-location)
		    (not (eq (sb!di:code-location-debug-source code-location)
			     (sb!di:code-location-debug-source prev-location)))
		    (not (eq (sb!di:code-location-toplevel-form-offset
			      code-location)
			     (sb!di:code-location-toplevel-form-offset
			      prev-location)))
		    (not (eq (sb!di:code-location-form-number code-location)
			     (sb!di:code-location-form-number prev-location))))
	    (flush)
	    (setq prev-location code-location  prev-num this-num))

	  (incf this-num))))

    (when (location-in-list *default-breakpoint-debug-fun*
			    *breakpoints*
			    :fun-end)
      (format t "~&::FUN-END *Active* "))))

(!def-debug-command-alias "LL" "LIST-LOCATIONS")

;;; Set breakpoint at the given number.
(!def-debug-command "BREAKPOINT" ()
  (let ((index (read-prompting-maybe "location number, :START, or :END: "))
	(break t)
	(condition t)
	(print nil)
	(print-functions nil)
	(function nil)
	(bp)
	(place *default-breakpoint-debug-fun*))
    (flet ((get-command-line ()
	     (let ((command-line nil)
		   (unique '(nil)))
	       (loop
		 (let ((next-input (read-if-available unique)))
		   (when (eq next-input unique) (return))
		   (push next-input command-line)))
	       (nreverse command-line)))
	   (set-vars-from-command-line (command-line)
	     (do ((arg (pop command-line) (pop command-line)))
		 ((not arg))
	       (ecase arg
		 (:condition (setf condition (pop command-line)))
		 (:print (push (pop command-line) print))
		 (:break (setf break (pop command-line)))
		 (:function
		  (setf function (eval (pop command-line)))
		  (setf *default-breakpoint-debug-fun*
			(sb!di:fun-debug-fun function))
		  (setf place *default-breakpoint-debug-fun*)
		  (setf *possible-breakpoints*
			(possible-breakpoints
			 *default-breakpoint-debug-fun*))))))
	   (setup-fun-start ()
	     (let ((code-loc (sb!di:debug-fun-start-location place)))
	       (setf bp (sb!di:make-breakpoint #'main-hook-fun
					       place
					       :kind :fun-start))
	       (setf break (sb!di:preprocess-for-eval break code-loc))
	       (setf condition (sb!di:preprocess-for-eval condition code-loc))
	       (dolist (form print)
		 (push (cons (sb!di:preprocess-for-eval form code-loc) form)
		       print-functions))))
	   (setup-fun-end ()
	     (setf bp
		   (sb!di:make-breakpoint #'main-hook-fun
					  place
					  :kind :fun-end))
	     (setf break
		   ;; FIXME: These and any other old (COERCE `(LAMBDA ..) ..)
		   ;; forms should be converted to shiny new (LAMBDA ..) forms.
		   ;; (Search the sources for "coerce.*\(lambda".)
		   (coerce `(lambda (dummy)
			      (declare (ignore dummy)) ,break)
			   'function))
	     (setf condition (coerce `(lambda (dummy)
					(declare (ignore dummy)) ,condition)
				     'function))
	     (dolist (form print)
	       (push (cons
		      (coerce `(lambda (dummy)
				 (declare (ignore dummy)) ,form) 'function)
		      form)
		     print-functions)))
	   (setup-code-location ()
	     (setf place (nth index *possible-breakpoints*))
	     (setf bp (sb!di:make-breakpoint #'main-hook-fun place
					     :kind :code-location))
	     (dolist (form print)
	       (push (cons
		      (sb!di:preprocess-for-eval form place)
		      form)
		     print-functions))
	     (setf break (sb!di:preprocess-for-eval break place))
	     (setf condition (sb!di:preprocess-for-eval condition place))))
      (set-vars-from-command-line (get-command-line))
      (cond
       ((or (eq index :start) (eq index :s))
	(setup-fun-start))
       ((or (eq index :end) (eq index :e))
	(setup-fun-end))
       (t
	(setup-code-location)))
      (sb!di:activate-breakpoint bp)
      (let* ((new-bp-info (create-breakpoint-info place bp index
						  :break break
						  :print print-functions
						  :condition condition))
	     (old-bp-info (location-in-list new-bp-info *breakpoints*)))
	(when old-bp-info
	  (sb!di:deactivate-breakpoint (breakpoint-info-breakpoint
					old-bp-info))
	  (setf *breakpoints* (remove old-bp-info *breakpoints*))
	  (format t "previous breakpoint removed~%"))
	(push new-bp-info *breakpoints*))
      (print-breakpoint-info (first *breakpoints*))
      (format t "~&added"))))

(!def-debug-command-alias "BP" "BREAKPOINT")

;;; List all breakpoints which are set.
(!def-debug-command "LIST-BREAKPOINTS" ()
  (setf *breakpoints*
	(sort *breakpoints* #'< :key #'breakpoint-info-breakpoint-number))
  (dolist (info *breakpoints*)
    (print-breakpoint-info info)))

(!def-debug-command-alias "LB" "LIST-BREAKPOINTS")
(!def-debug-command-alias "LBP" "LIST-BREAKPOINTS")

;;; Remove breakpoint N, or remove all breakpoints if no N given.
(!def-debug-command "DELETE-BREAKPOINT" ()
  (let* ((index (read-if-available nil))
	 (bp-info
	  (find index *breakpoints* :key #'breakpoint-info-breakpoint-number)))
    (cond (bp-info
	   (sb!di:delete-breakpoint (breakpoint-info-breakpoint bp-info))
	   (setf *breakpoints* (remove bp-info *breakpoints*))
	   (format t "breakpoint ~S removed~%" index))
	  (index (format t "The breakpoint doesn't exist."))
	  (t
	   (dolist (ele *breakpoints*)
	     (sb!di:delete-breakpoint (breakpoint-info-breakpoint ele)))
	   (setf *breakpoints* nil)
	   (format t "all breakpoints deleted~%")))))

(!def-debug-command-alias "DBP" "DELETE-BREAKPOINT")

;;; miscellaneous commands

(!def-debug-command "DESCRIBE" ()
  (let* ((curloc (sb!di:frame-code-location *current-frame*))
	 (debug-fun (sb!di:code-location-debug-fun curloc))
	 (function (sb!di:debug-fun-fun debug-fun)))
    (if function
	(describe function)
	(format t "can't figure out the function for this frame"))))

(!def-debug-command "SLURP" ()
  (loop while (read-char-no-hang *standard-input*)))

(!def-debug-command "RETURN" (&optional
			      (return (read-prompting-maybe
				       "return: ")))
  (let ((tag (find-if (lambda (x)
			(and (typep (car x) 'symbol)
			     (not (symbol-package (car x)))
			     (string= (car x) "SB-DEBUG-CATCH-TAG")))
		      (sb!di::frame-catches *current-frame*))))
    (if tag
	(throw (car tag)
	  (funcall (sb!di:preprocess-for-eval
		    return
		    (sb!di:frame-code-location *current-frame*))
		   *current-frame*))
	(format t "~@<can't find a tag for this frame ~
                   ~2I~_(hint: try increasing the DEBUG optimization quality ~
                   and recompiling)~:@>"))))

;;;; debug loop command utilities

(defun read-prompting-maybe (prompt)
  (unless (sb!int:listen-skip-whitespace *debug-io*)
    (princ prompt)
    (force-output))
  (read *debug-io*))

(defun read-if-available (default)
  (if (sb!int:listen-skip-whitespace *debug-io*)
      (read *debug-io*)
      default))
