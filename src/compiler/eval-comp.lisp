;;;; This file represents the current state of on-going development on
;;;; compiler hooks for an interpreter that takes the compiler's IR1 of
;;;; a program.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;; FIXME: Doesn't this belong somewhere else, like early-c.lisp?
(declaim (special *constants* *free-variables* *component-being-compiled*
		  *code-vector* *next-location* *result-fixups*
		  *free-functions* *source-paths* *failed-optimizations*
		  *seen-blocks* *seen-functions* *list-conflicts-table*
		  *continuation-number* *continuation-numbers*
		  *number-continuations* *tn-id* *tn-ids* *id-tns*
		  *label-ids* *label-id* *id-labels*
		  *compiler-error-count* *compiler-warning-count*
		  *compiler-style-warning-count* *compiler-note-count*
		  *compiler-error-bailout*
		  #!+sb-show *compiler-trace-output*
		  *last-source-context* *last-original-source*
		  *last-source-form* *last-format-string* *last-format-args*
		  *last-message-count* *check-consistency*
		  *all-components* *converting-for-interpreter*
		  *source-info* *block-compile* *current-path*
		  *current-component* *lexenv*))

;;; Translate form into the compiler's IR1 and perform environment
;;; analysis. This is sort of a combination of COMPILE-FILE,
;;; SUB-COMPILE-FILE, COMPILE-TOP-LEVEL, and COMPILE-COMPONENT.
(defun compile-for-eval (form quietly)
  (with-ir1-namespace
    (let* ((*block-compile* nil)
	   (*lexenv* (make-null-lexenv))
	   (*compiler-error-bailout*
	    #'(lambda () (error "fatal error, aborting evaluation")))
	   (*current-path* nil)
	   (*last-source-context* nil)
	   (*last-original-source* nil)
	   (*last-source-form* nil)
	   (*last-format-string* nil)
	   (*last-format-args* nil)
	   (*last-message-count* 0)
	   ;; These are now bound by WITH-COMPILATION-UNIT. -- WHN 20000308
	   #+nil (*compiler-error-count* 0)
	   #+nil (*compiler-warning-count* 0)
	   #+nil (*compiler-style-warning-count* 0)
	   #+nil (*compiler-note-count* 0)
	   (*source-info* (make-lisp-source-info form))
	   (*converting-for-interpreter* t)
	   (*gensym-counter* 0)
	   (*warnings-p* nil)
	   (*failure-p* nil))

      (clear-stuff nil)
      (find-source-paths form 0)
      ;; This LET comes from COMPILE-TOP-LEVEL.
      ;; The noted DOLIST is a splice from a call that COMPILE-TOP-LEVEL makes.
      (sb!xc:with-compilation-unit ()
	(let ((lambdas (list (ir1-top-level form
					    '(original-source-start 0 0)
					    t))))
	  (declare (list lambdas))
	  (dolist (lambda lambdas)
	    (let* ((component
		    (block-component (node-block (lambda-bind lambda))))
		   (*all-components* (list component)))
	      (local-call-analyze component)))
	  (multiple-value-bind (components top-components)
	      (find-initial-dfo lambdas)
	    (let ((*all-components* (append components top-components)))
	      (when *check-consistency*
		(check-ir1-consistency *all-components*))
	      ;; This DOLIST body comes from the beginning of
	      ;; COMPILE-COMPONENT.
	      (dolist (component *all-components*)
		(ir1-finalize component)
		(let ((*component-being-compiled* component))
		  (environment-analyze component))
		(annotate-component-for-eval component))
	    (when *check-consistency*
	      (check-ir1-consistency *all-components*))))
	  (car lambdas))))))

;;;; annotating IR1 for interpretation

(defstruct (lambda-eval-info (:constructor make-lambda-eval-info
					   (frame-size args-passed entries))
			     (:copier nil))
  frame-size		; number of stack locations needed to hold locals
  args-passed		; number of referenced arguments passed to lambda
  entries		; a-list mapping entry nodes to stack locations
  (function nil))	; a function object corresponding to this lambda
(def!method print-object ((obj lambda-eval-info) str)
  (print-unreadable-object (obj str :type t)))

(defstruct (entry-node-info (:constructor make-entry-node-info
					  (st-top nlx-tag))
			    (:copier nil))
  st-top	; stack top when we encounter the entry node
  nlx-tag)	; tag to which to throw to get back entry node's context
(def!method print-object ((obj entry-node-info) str)
  (print-unreadable-object (obj str :type t)))

;;; Some compiler funny functions have definitions, so the interpreter can
;;; call them. These require special action to coordinate the interpreter,
;;; system call stack, and the environment. The annotation prepass marks the
;;; references to these as :unused, so the interpreter doesn't try to fetch
;;; functions through these undefined symbols.
(defconstant undefined-funny-funs
  '(%special-bind %special-unbind %more-arg-context %unknown-values %catch
    %unwind-protect %catch-breakup %unwind-protect-breakup
    %lexical-exit-breakup %continue-unwind %nlx-entry))

;;; Some kinds of functions are only passed as arguments to funny functions,
;;; and are never actually evaluated at run time.
(defconstant non-closed-function-kinds '(:cleanup :escape))

;;; This annotates continuations, lambda-vars, and lambdas. For each
;;; continuation, we cache how its destination uses its value. This only buys
;;; efficiency when the code executes more than once, but the overhead of this
;;; part of the prepass for code executed only once should be negligible.
;;;
;;; As a special case to aid interpreting local function calls, we sometimes
;;; note the continuation as :unused. This occurs when there is a local call,
;;; and there is no actual function object to call; we mark the continuation as
;;; :unused since there is nothing to push on the interpreter's stack.
;;; Normally we would see a reference to a function that we would push on the
;;; stack to later pop and apply to the arguments on the stack. To determine
;;; when we have a local call with no real function object, we look at the node
;;; to see whether it is a reference with a destination that is a :local
;;; combination whose function is the reference node's continuation.
;;;
;;; After checking for virtual local calls, we check for funny functions the
;;; compiler refers to for calling to note certain operations. These functions
;;; are undefined, and if the interpreter tried to reference the function cells
;;; of these symbols, it would get an error. We mark the continuations
;;; delivering the values of these references as :unused, so the reference
;;; never takes place.
;;;
;;; For each lambda-var, including a lambda's vars and its let's vars, we note
;;; the stack offset used to access and store that variable. Then we note the
;;; lambda with the total number of variables, so we know how big its stack
;;; frame is. Also in the lambda's info is the number of its arguments that it
;;; actually references; the interpreter never pushes or pops an unreferenced
;;; argument, so we can't just use LENGTH on LAMBDA-VARS to know how many args
;;; the caller passed.
;;;
;;; For each entry node in a lambda, we associate in the lambda-eval-info the
;;; entry node with a stack offset. Evaluation code stores the frame pointer
;;; in this slot upon processing the entry node to aid stack cleanup and
;;; correct frame manipulation when processing exit nodes.
(defun annotate-component-for-eval (component)
  (do-blocks (b component)
    (do-nodes (node cont b)
      (let* ((dest (continuation-dest cont))
	     (refp (typep node 'ref))
	     (leaf (if refp (ref-leaf node))))
	(setf (continuation-info cont)
	      (cond ((and refp dest (typep dest 'basic-combination)
			  (eq (basic-combination-kind dest) :local)
			  (eq (basic-combination-fun dest) cont))
		     :unused)
		    ((and leaf (typep leaf 'global-var)
			  (eq (global-var-kind leaf) :global-function)
			  (member (sb!c::global-var-name leaf)
				  undefined-funny-funs
				  :test #'eq))
		     :unused)
		    ((and leaf (typep leaf 'clambda)
			  (member (functional-kind leaf)
				  non-closed-function-kinds))
		     (assert (not (eq (functional-kind leaf) :escape)))
		     :unused)
		    (t
		     (typecase dest
		       ;; Change locations in eval.lisp that think :RETURN
		       ;; could occur.
		       ((or mv-combination creturn exit) :multiple)
		       (null :unused)
		       (t :single))))))))
  (dolist (lambda (component-lambdas component))
    (let ((locals-count 0)
	  (args-passed-count 0))
      (dolist (var (lambda-vars lambda))
	(setf (leaf-info var) locals-count)
	(incf locals-count)
	(when (leaf-refs var) (incf args-passed-count)))
      (dolist (let (lambda-lets lambda))
	(dolist (var (lambda-vars let))
	  (setf (leaf-info var) locals-count)
	  (incf locals-count)))
      (let ((entries nil))
	(dolist (e (lambda-entries lambda))
	  (ecase (process-entry-node-p e)
	    (:blow-it-off)
	    (:local-lexical-exit
	     (push (cons e (make-entry-node-info locals-count nil))
		   entries)
	     (incf locals-count))
	    (:non-local-lexical-exit
	     (push (cons e
			 (make-entry-node-info locals-count
					       (incf locals-count)))
		   entries)
	     (incf locals-count))))
	(setf (lambda-info lambda)
	      (make-lambda-eval-info locals-count
				     args-passed-count
				     entries))))))

(defun process-entry-node-p (entry)
  (let ((entry-cleanup (entry-cleanup entry)))
    (dolist (nlx (environment-nlx-info (node-environment entry))
		 :local-lexical-exit)
      (let ((cleanup (nlx-info-cleanup nlx)))
	(when (eq entry-cleanup cleanup)
	  (ecase (cleanup-kind cleanup)
	    ((:block :tagbody)
	     (return :non-local-lexical-exit))
	    ((:catch :unwind-protect)
	     (return :blow-it-off))))))))

;;; Sometime consider annotations to exclude processing of exit nodes when
;;; we want to do a tail-p thing.

;;;; defining funny functions for interpreter

#|
%listify-rest-args %more-arg %verify-argument-count %argument-count-error
%odd-keyword-arguments-error %unknown-keyword-argument-error
|#

(defun %verify-argument-count (supplied-args defined-args)
  (unless (= supplied-args defined-args)
    (error "Wrong argument count, wanted ~D and got ~D."
	   defined-args supplied-args))
  (values))

;;; Use (SETF SYMBOL-FUNCTION) instead of DEFUN so that the compiler
;;; doesn't try to compile the hidden %THROW MV-CALL in the throw below as
;;; a local recursive call.
(setf (symbol-function '%throw)
      #'(lambda (tag &rest args)
	  (throw tag (values-list args))))

(defun %more-arg (args index)
  (nth index args))

(defun %listify-rest-args (ptr count)
  (declare (ignore count))
  ptr)

(defun %more-arg-values (args start count)
  (values-list (subseq args start count)))

(defun %argument-count-error (args-passed-count)
  (error 'simple-program-error
	 :format-control "wrong number of arguments passed: ~S"
	 :format-arguments (list args-passed-count)))

(defun %odd-keyword-arguments-error ()
  (error 'simple-program-error
	 :format-control "function called with odd number of keyword arguments"
	 :format-arguments nil))

(defun %unknown-keyword-argument-error (keyword)
  (error 'simple-program-error
	 :format-control "unknown keyword argument: ~S"
	 :format-arguments (list keyword)))

(defun %cleanup-point ())

(defun value-cell-ref (x) (value-cell-ref x))
