;;;; This file contains the IR1 interpreter. We first convert to the
;;;; compiler's IR1, then interpret that.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!EVAL")

;;;; interpreter stack

(defvar *interpreted-function-cache-minimum-size* 25
  #!+sb-doc
  "If the interpreted function cache has more functions than this come GC time,
  then attempt to prune it according to
  *INTERPRETED-FUNCTION-CACHE-THRESHOLD*.")

(defvar *interpreted-function-cache-threshold* 3
  #!+sb-doc
  "If an interpreted function goes uncalled for more than this many GCs, then
  it is eligible for flushing from the cache.")

(declaim (type (and fixnum unsigned-byte)
	       *interpreted-function-cache-minimum-size*
	       *interpreted-function-cache-threshold*))

;;; The list of INTERPRETED-FUNCTIONS that have translated definitions.
(defvar *interpreted-function-cache* nil)
(declaim (type list *interpreted-function-cache*))

;;; Setting this causes the stack operations to dump a trace.
#+!sb-show
(defvar *eval-stack-trace* nil)

;;; Push value on *EVAL-STACK*, growing the stack if necessary. This
;;; returns value. We save *EVAL-STACK-TOP* in a local and increment
;;; the global before storing value on the stack to prevent a GC
;;; timing problem. If we stored value on the stack using
;;; *EVAL-STACK-TOP* as an index, and we GC'ed before incrementing
;;; *EVAL-STACK-TOP*, then INTERPRETER-GC-HOOK would clear the
;;; location.
(defun eval-stack-push (value)
  (let ((len (length (the simple-vector *eval-stack*))))
    (when (= len *eval-stack-top*)
      #+!sb-show (when *eval-stack-trace*
		   (format t "[PUSH: growing stack.]~%"))
      (let ((new-stack (make-array (ash len 1))))
	(replace new-stack *eval-stack* :end1 len :end2 len)
	(setf *eval-stack* new-stack))))
  (let ((top *eval-stack-top*))
    #+!sb-show (when *eval-stack-trace* (format t "pushing ~D.~%" top))
    (incf *eval-stack-top*)
    (setf (svref *eval-stack* top) value)))

;;; Return the last value pushed on *EVAL-STACK* and decrement the top
;;; pointer. We forego setting elements off the end of the stack to
;;; nil for GC purposes because there is a *BEFORE-GC-HOOK* to take
;;; care of this for us. However, because of the GC hook, we must be
;;; careful to grab the value before decrementing *EVAL-STACK-TOP*
;;; since we could GC between the decrement and the reference, and the
;;; hook would clear the stack slot.
(defun eval-stack-pop ()
  (when (zerop *eval-stack-top*)
    (error "attempt to pop empty eval stack"))
  (let* ((new-top (1- *eval-stack-top*))
	 (value (svref *eval-stack* new-top)))
    #+!sb-show (when *eval-stack-trace*
		 (format t "popping ~D --> ~S.~%" new-top value))
    (setf *eval-stack-top* new-top)
    value))

;;; Allocate N locations on the stack, bumping the top pointer and
;;; growing the stack if necessary. We set new slots to nil in case we
;;; GC before having set them; we don't want to hold on to potential
;;; garbage from old stack fluctuations.
(defun eval-stack-extend (n)
  (let ((len (length (the simple-vector *eval-stack*))))
    (when (> (+ n *eval-stack-top*) len)
      #+!sb-show (when *eval-stack-trace*
		   (format t "[EXTEND: growing stack.]~%"))
      (let ((new-stack (make-array (+ n (ash len 1)))))
	(replace new-stack *eval-stack* :end1 len :end2 len)
	(setf *eval-stack* new-stack))))
  (let ((new-top (+ *eval-stack-top* n)))
    #+!sb-show (when *eval-stack-trace*
		 (format t "extending to ~D.~%" new-top))
    (do ((i *eval-stack-top* (1+ i)))
	((= i new-top))
      (setf (svref *eval-stack* i) nil))
    (setf *eval-stack-top* new-top)))

;;; the antithesis of EVAL-STACK-EXTEND
(defun eval-stack-shrink (n)
  #+!sb-show (when *eval-stack-trace*
	       (format t "shrinking to ~D.~%" (- *eval-stack-top* n)))
  (decf *eval-stack-top* n))

;;; This is used to shrink the stack back to a previous frame pointer.
(defun eval-stack-reset-top (ptr)
  #+!sb-show (when *eval-stack-trace*
	       (format t "setting top to ~D.~%" ptr))
  (setf *eval-stack-top* ptr))

;;; Return a local variable from the current stack frame. This is used
;;; for references the compiler represents as a lambda-var leaf. It is
;;; a macro as a quick and dirty way of making it SETFable.
;;;
;;; FIXME: used only in this file, needn't be in runtime
(defmacro eval-stack-local (fp offset)
  `(svref *eval-stack* (+ ,fp ,offset)))

;;;; interpreted functions

;;; the list of INTERPRETED-FUNCTIONS that have translated definitions
(defvar *interpreted-function-cache* nil)
(declaim (type list *interpreted-function-cache*))

;;; Return a function that will lazily convert LAMBDA when called, and
;;; will cache translations.
(defun make-interpreted-function (lambda)
  (let ((res (%make-interpreted-function :lambda lambda
					 :arglist (second lambda))))
    (setf (funcallable-instance-function res)
	  #'(instance-lambda (&rest args)
	       (let ((fun (interpreted-function-definition res))
		     (args (cons (length args) args)))
		 (setf (interpreted-function-gcs res) 0)
		 (internal-apply (or fun (convert-interpreted-fun res))
				 args '#()))))
    res))

;;; Eval a FUNCTION form, grab the definition and stick it in.
(defun convert-interpreted-fun (fun)
  (declare (type interpreted-function fun))
  (let* ((new (interpreted-function-definition
	       (internal-eval `#',(interpreted-function-lambda fun)))))
    (setf (interpreted-function-definition fun) new)
    (setf (interpreted-function-converted-once fun) t)
    (let ((name (interpreted-function-%name fun)))
      (setf (sb!c::leaf-name new) name)
      (setf (sb!c::leaf-name (sb!c::main-entry
			      (sb!c::functional-entry-function new)))
	    name))
    (push fun *interpreted-function-cache*)
    new))

;;; Get the CLAMBDA for the XEP, then look at the inline expansion info in
;;; the real function.
(defun interpreted-function-lambda-expression (x)
  (let ((lambda (interpreted-function-lambda x)))
    (if lambda
	(values lambda nil (interpreted-function-%name x))
	(let ((fun (sb!c::functional-entry-function
		    (interpreted-function-definition x))))
	  (values (sb!c::functional-inline-expansion fun)
		  (if (let ((env (sb!c::functional-lexenv fun)))
			(or (sb!c::lexenv-functions env)
			    (sb!c::lexenv-variables env)
			    (sb!c::lexenv-blocks env)
			    (sb!c::lexenv-tags env)))
		      t nil)
		  (or (interpreted-function-%name x)
		      (sb!c::component-name
		       (sb!c::block-component
 			(sb!c::node-block
 			 (sb!c::lambda-bind (sb!c::main-entry fun)))))))))))

;;; Return a FUNCTION-TYPE describing an eval function. We just grab the
;;; LEAF-TYPE of the definition, converting the definition if not currently
;;; cached.
(defvar *already-looking-for-type-of* nil)
(defun interpreted-function-type (fun)
  (if (member fun *already-looking-for-type-of*)
      (specifier-type 'function)
      (let* ((*already-looking-for-type-of*
	      (cons fun *already-looking-for-type-of*))
	     (def (or (interpreted-function-definition fun)
		      (sb!sys:without-gcing
		       (convert-interpreted-fun fun)
		       (interpreted-function-definition fun)))))
	(sb!c::leaf-type (sb!c::functional-entry-function def)))))

(defun interpreted-function-name (x)
  (multiple-value-bind (ig1 ig2 res) (interpreted-function-lambda-expression x)
    (declare (ignore ig1 ig2))
    res))
(defun (setf interpreted-function-name) (val x)
  (let ((def (interpreted-function-definition x)))
    (when def
      (setf (sb!c::leaf-name def) val)
      (setf (sb!c::leaf-name (sb!c::main-entry (sb!c::functional-entry-function
						def)))
	    val))
    (setf (interpreted-function-%name x) val)))

(defun interpreter-gc-hook ()
  ;; Clear the unused portion of the eval stack.
  (let ((len (length (the simple-vector *eval-stack*))))
    (do ((i *eval-stack-top* (1+ i)))
	((= i len))
      (setf (svref *eval-stack* i) nil)))

  ;; KLUDGE: I'd like to get rid of this, since it adds complexity and causes
  ;; confusion. (It's not just academic that it causes confusion. When working
  ;; on the original cross-compiler, I ran across what looked
  ;; as though it might be a subtle writing-to-the-host-SBCL-compiler-data bug
  ;; in my cross-compiler code, which turned out to be just a case of compiler
  ;; warnings coming from recompilation of a flushed-from-the-cache interpreted
  ;; function. Since it took me a long while to realize how many things the
  ;; problem depended on (since it was tied up with magic numbers of GC cycles,
  ;; egads!) I blew over a day trying to isolate the problem in a small test
  ;; case.
  ;;
  ;; The cache-flushing seems to be motivated by efficiency concerns, which
  ;; seem misplaced when the user chooses to use the interpreter. However, it
  ;; also interacts with SAVE, and I veered off from deleting it wholesale when
  ;; I noticed that. After the whole system is working, though, I'd like to
  ;; revisit this decision. -- WHN 19990713
  (let ((num (- (length *interpreted-function-cache*)
		*interpreted-function-cache-minimum-size*)))
    (when (plusp num)
      (setq *interpreted-function-cache*
	    (delete-if #'(lambda (x)
			   (when (>= (interpreted-function-gcs x)
				     *interpreted-function-cache-threshold*)
			     (setf (interpreted-function-definition x) nil)
			     t))
		       *interpreted-function-cache*
		       :count num))))
  (dolist (fun *interpreted-function-cache*)
    (incf (interpreted-function-gcs fun))))
(pushnew 'interpreter-gc-hook sb!ext:*before-gc-hooks*)

;;; Clear all entries in the eval function cache. This allows the internal
;;; representation of the functions to be reclaimed, and also lazily forces
;;; macroexpansions to be recomputed.
(defun flush-interpreted-function-cache ()
  (dolist (fun *interpreted-function-cache*)
    (setf (interpreted-function-definition fun) nil))
  (setq *interpreted-function-cache* ()))

;;;; INTERNAL-APPLY-LOOP macros

;;;; These macros are intimately related to INTERNAL-APPLY-LOOP. They assume
;;;; variables established by this function, and they assume they can return
;;;; from a block by that name. This is sleazy, but we justify it as follows:
;;;; They are so specialized in use, and their invocation became lengthy, that
;;;; we allowed them to slime some access to things in their expanding
;;;; environment. These macros don't really extend our Lisp syntax, but they do
;;;; provide some template expansion service; it is these cleaner circumstance
;;;; that require a more rigid programming style.
;;;;
;;;; Since these are macros expanded almost solely for COMBINATION nodes,
;;;; they cascade from the end of this logical page to the beginning here.
;;;; Therefore, it is best you start looking at them from the end of this
;;;; section, backwards from normal scanning mode for Lisp code.

;;; This runs a function on some arguments from the stack. If the combination
;;; occurs in a tail recursive position, then we do the call such that we
;;; return from tail-p-function with whatever values the call produces. With a
;;; :local call, we have to restore the stack to its previous frame before
;;; doing the call. The :full call mechanism does this for us. If it is NOT a
;;; tail recursive call, and we're in a multiple value context, then then push
;;; a list of the returned values. Do the same thing if we're in a :return
;;; context. Push a single value, without listifying it, for a :single value
;;; context. Otherwise, just call for side effect.
;;;
;;; Node is the combination node, and cont is its continuation. Frame-ptr
;;; is the current frame pointer, and closure is the current environment for
;;; closure variables. Call-type is either :full or :local, and when it is
;;; local, lambda is the IR1 lambda to apply.
;;;
;;; This assumes the following variables are present: node, cont, frame-ptr,
;;; and closure. It also assumes a block named internal-apply-loop.
;;;
;;; FIXME: used only in this file, needn't be in runtime
;;; FIXME: down with DO-FOO names for non-iteration constructs!
(defmacro do-combination (call-type lambda mv-or-normal)
  (let* ((args (gensym))
	 (calling-closure (gensym))
	 (invoke-fun (ecase mv-or-normal
		       (:mv-call 'mv-internal-invoke)
		       (:normal 'internal-invoke)))
	 (args-form (ecase mv-or-normal
		      (:mv-call
		       `(mv-eval-stack-args
			 (length (sb!c::mv-combination-args node))))
		      (:normal
		       `(eval-stack-args (sb!c:lambda-eval-info-args-passed
					  (sb!c::lambda-info ,lambda))))))
	 (call-form (ecase call-type
		      (:full `(,invoke-fun
			       (length (sb!c::basic-combination-args node))))
		      (:local `(internal-apply
				,lambda ,args-form
				(compute-closure node ,lambda frame-ptr
						 closure)
				nil))))
	 (tailp-call-form
	  (ecase call-type
	    (:full `(return-from
		     internal-apply-loop
		     ;; INVOKE-FUN takes care of the stack itself.
		     (,invoke-fun (length (sb!c::basic-combination-args node))
				  frame-ptr)))
	    (:local `(let ((,args ,args-form)
			   (,calling-closure
			    (compute-closure node ,lambda frame-ptr closure)))
		       ;; No need to clean up stack slots for GC due to
		       ;; SB!EXT:*BEFORE-GC-HOOK*.
		       (eval-stack-reset-top frame-ptr)
		       (return-from
			internal-apply-loop
			(internal-apply ,lambda ,args ,calling-closure
					nil)))))))
    `(cond ((sb!c::node-tail-p node)
	    ,tailp-call-form)
	   (t
	    (ecase (sb!c::continuation-info cont)
	      ((:multiple :return)
	       (eval-stack-push (multiple-value-list ,call-form)))
	      (:single
	       (eval-stack-push ,call-form))
	      (:unused ,call-form))))))

;;; This sets the variable block in INTERNAL-APPLY-LOOP, and it announces this
;;; by setting set-block-p for later loop iteration maintenance.
;;;
;;; FIXME: used only in this file, needn't be in runtime
(defmacro set-block (exp)
  `(progn
     (setf block ,exp)
     (setf set-block-p t)))

;;; This sets all the iteration variables in INTERNAL-APPLY-LOOP to iterate
;;; over a new block's nodes. Block-exp is optional because sometimes we have
;;; already set block, and we only need to bring the others into agreement.
;;; If we already set block, then clear the variable that announces this,
;;; set-block-p.
;;;
;;; FIXME: used only in this file, needn't be in runtime
(defmacro change-blocks (&optional block-exp)
  `(progn
     ,(if block-exp
	  `(setf block ,block-exp)
	  `(setf set-block-p nil))
     (setf node (sb!c::continuation-next (sb!c::block-start block)))
     (setf last-cont (sb!c::node-cont (sb!c::block-last block)))))

;;; This controls printing visited nodes in INTERNAL-APPLY-LOOP. We use it
;;; here, and INTERNAL-INVOKE uses it to print function call looking output
;;; to further describe sb!c::combination nodes.
#!+sb-show (defvar *internal-apply-node-trace* nil)
#!+sb-show
(defun maybe-trace-funny-fun (node name &rest args)
  (when *internal-apply-node-trace*
    (format t "(~S ~{ ~S~})  c~S~%"
	    name args (sb!c::cont-num (sb!c::node-cont node)))))

;;; This implements the intention of the virtual function name. This is a
;;; macro because some of these actions must occur without a function call.
;;; For example, calling a dispatch function to implement special binding would
;;; be a no-op because returning from that function would cause the system to
;;; undo any special bindings it established.
;;;
;;; NOTE: update SB!C:ANNOTATE-COMPONENT-FOR-EVAL and/or
;;; sb!c::undefined-funny-funs if you add or remove branches in this routine.
;;;
;;; This assumes the following variables are present: node, cont, frame-ptr,
;;; args, closure, block, and last-cont. It also assumes a block named
;;; internal-apply-loop.
;;;
;;; FIXME: used only in this file, needn't be in runtime
;;; FIXME: down with DO-FOO names for non-iteration constructs!
(defmacro do-funny-function (funny-fun-name)
  (let ((name (gensym)))
    `(let ((,name ,funny-fun-name))
       (ecase ,name
	 (sb!c::%special-bind
	  (let ((value (eval-stack-pop))
		(global-var (eval-stack-pop)))
	    #!+sb-show (maybe-trace-funny-fun node ,name global-var value)
	    (sb!sys:%primitive sb!c:bind
			       value
			       (sb!c::global-var-name global-var))))
	 (sb!c::%special-unbind
	  ;; Throw away arg telling me which special, and tell the dynamic
	  ;; binding mechanism to unbind one variable.
	  (eval-stack-pop)
	  #!+sb-show (maybe-trace-funny-fun node ,name)
	  (sb!sys:%primitive sb!c:unbind))
	 (sb!c::%catch
	  (let* ((tag (eval-stack-pop))
		 (nlx-info (eval-stack-pop))
		 (fell-through-p nil)
		 ;; Ultimately THROW and CATCH will fix the interpreter's stack
		 ;; since this is necessary for compiled CATCH's and those in
		 ;; the initial top level function.
		 (stack-top *eval-stack-top*)
		 (values
		  (multiple-value-list
		   (catch tag
		     #!+sb-show (maybe-trace-funny-fun node ,name tag)
		     (multiple-value-setq (block node cont last-cont)
		       (internal-apply-loop (sb!c::continuation-next cont)
					    frame-ptr lambda args closure))
		     (setf fell-through-p t)))))
	    (cond (fell-through-p
		   ;; We got here because we just saw the SB!C::%CATCH-BREAKUP
		   ;; funny function inside the above recursive call to
		   ;; INTERNAL-APPLY-LOOP. Therefore, we just received and
		   ;; stored the current state of evaluation for falling
		   ;; through.
		   )
		  (t
		   ;; Fix up the interpreter's stack after having thrown here.
		   ;; We won't need to do this in the final implementation.
		   (eval-stack-reset-top stack-top)
		   ;; Take the values received in the list bound above, and
		   ;; massage them into the form expected by the continuation
		   ;; of the non-local-exit info.
		   (ecase (sb!c::continuation-info
			   (sb!c::nlx-info-continuation nlx-info))
		     (:single
		      (eval-stack-push (car values)))
		     ((:multiple :return)
		      (eval-stack-push values))
		     (:unused))
		   ;; We want to continue with the code after the CATCH body.
		   ;; The non-local-exit info tells us where this is, but we
		   ;; know that block only contains a call to the funny
		   ;; function SB!C::%NLX-ENTRY, which simply is a place holder
		   ;; for the compiler IR1. We want to skip the target block
		   ;; entirely, so we say it is the block we're in now and say
		   ;; the current cont is the last-cont. This makes the COND
		   ;; at the end of INTERNAL-APPLY-LOOP do the right thing.
		   (setf block (sb!c::nlx-info-target nlx-info))
		   (setf cont last-cont)))))
	 (sb!c::%unwind-protect
	  ;; Cleanup function not pushed due to special-case :UNUSED
	  ;; annotation in ANNOTATE-COMPONENT-FOR-EVAL.
	  (let* ((nlx-info (eval-stack-pop))
		 (fell-through-p nil)
		 (stack-top *eval-stack-top*))
	    (unwind-protect
		(progn
		  #!+sb-show (maybe-trace-funny-fun node ,name)
		  (multiple-value-setq (block node cont last-cont)
		    (internal-apply-loop (sb!c::continuation-next cont)
					 frame-ptr lambda args closure))
		  (setf fell-through-p t))
	      (cond (fell-through-p
		     ;; We got here because we just saw the
		     ;; SB!C::%UNWIND-PROTECT-BREAKUP funny function inside the
		     ;; above recursive call to INTERNAL-APPLY-LOOP.
		     ;; Therefore, we just received and stored the current
		     ;; state of evaluation for falling through.
		     )
		    (t
		     ;; Fix up the interpreter's stack after having thrown
		     ;; here. We won't need to do this in the final
		     ;; implementation.
		     (eval-stack-reset-top stack-top)
		     ;; Push some bogus values for exit context to keep the
		     ;; MV-BIND in the UNWIND-PROTECT translation happy.
		     (eval-stack-push '(nil nil 0))
		     (let ((node (sb!c::continuation-next
				  (sb!c::block-start
				   (car (sb!c::block-succ
					 (sb!c::nlx-info-target nlx-info)))))))
		       (internal-apply-loop node frame-ptr lambda args
					    closure)))))))
	 ((sb!c::%catch-breakup
	   sb!c::%unwind-protect-breakup
	   sb!c::%continue-unwind)
	  ;; This shows up when we locally exit a CATCH body -- fell through.
	  ;; Return the current state of evaluation to the previous invocation
	  ;; of INTERNAL-APPLY-LOOP which happens to be running in the
	  ;; SB!C::%CATCH branch of this code.
	  #!+sb-show (maybe-trace-funny-fun node ,name)
	  (return-from internal-apply-loop
		       (values block node cont last-cont)))
	 (sb!c::%nlx-entry
	  #!+sb-show (maybe-trace-funny-fun node ,name)
	  ;; This just marks a spot in the code for CATCH, UNWIND-PROTECT, and
	  ;; non-local lexical exits (GO or RETURN-FROM).
	  ;; Do nothing since sb!c::%catch does it all when it catches a THROW.
	  ;; Do nothing since sb!c::%unwind-protect does it all when
	  ;; it catches a THROW.
	  )
	 (sb!c::%more-arg-context
	  (let* ((fixed-arg-count (1+ (eval-stack-pop)))
		 ;; Add 1 to actual fixed count for extra arg expected by
		 ;; external entry points (XEP) which some IR1 lambdas have.
		 ;; The extra arg is the number of arguments for arg count
		 ;; consistency checking. SB!C::%MORE-ARG-CONTEXT always runs
		 ;; within an XEP, so the lambda has an extra arg.
		 (more-args (nthcdr fixed-arg-count args)))
	    #!+sb-show (maybe-trace-funny-fun node ,name fixed-arg-count)
	    (aver (eq (sb!c::continuation-info cont) :multiple))
	    (eval-stack-push (list more-args (length more-args)))))
	 (sb!c::%unknown-values
	  (error "SB!C::%UNKNOWN-VALUES should never be in interpreter's IR1."))
	 (sb!c::%lexical-exit-breakup
	  ;; We see this whenever we locally exit the extent of a lexical
	  ;; target. That is, we are truly locally exiting an extent we could
	  ;; have non-locally lexically exited. Return the :fell-through flag
	  ;; and the current state of evaluation to the previous invocation
	  ;; of INTERNAL-APPLY-LOOP which happens to be running in the
	  ;; SB!C::ENTRY branch of INTERNAL-APPLY-LOOP.
	  #!+sb-show (maybe-trace-funny-fun node ,name)
	  ;; Discard the NLX-INFO arg...
	  (eval-stack-pop)
	  (return-from internal-apply-loop
		       (values :fell-through block node cont last-cont)))))))

;;; This expands for the two types of combination nodes INTERNAL-APPLY-LOOP
;;; sees. Type is either :mv-call or :normal. Node is the combination node,
;;; and cont is its continuation. Frame-ptr is the current frame pointer, and
;;; closure is the current environment for closure variables.
;;;
;;; Most of the real work is done by DO-COMBINATION. This first determines if
;;; the combination node describes a :full call which DO-COMBINATION directly
;;; handles. If the call is :local, then we either invoke an IR1 lambda, or we
;;; just bind some LET variables. If the call is :local, and type is :mv-call,
;;; then we can only be binding multiple values. Otherwise, the combination
;;; node describes a function known to the compiler, but this may be a funny
;;; function that actually isn't ever defined. We either take some action for
;;; the funny function or do a :full call on the known true function, but the
;;; interpreter doesn't do optimizing stuff for functions known to the
;;; compiler.
;;;
;;; This assumes the following variables are present: node, cont, frame-ptr,
;;; and closure. It also assumes a block named internal-apply-loop.
;;;
;;; FIXME: used only in this file, needn't be in runtime
(defmacro combination-node (type)
  (let* ((kind (gensym))
	 (fun (gensym))
	 (lambda (gensym))
	 (letp (gensym))
	 (letp-bind (ecase type
		      (:mv-call nil)
		      (:normal
		       `((,letp (eq (sb!c::functional-kind ,lambda) :let))))))
	 (local-branch
	  (ecase type
	    (:mv-call
	     `(store-mv-let-vars ,lambda frame-ptr
				 (length (sb!c::mv-combination-args node))))
	    (:normal
	     `(if ,letp
		  (store-let-vars ,lambda frame-ptr)
		  (do-combination :local ,lambda ,type))))))
    `(let ((,kind (sb!c::basic-combination-kind node))
	   (,fun (sb!c::basic-combination-fun node)))
       (cond ((member ,kind '(:full :error))
	      (do-combination :full nil ,type))
	     ((eq ,kind :local)
	      (let* ((,lambda (sb!c::ref-leaf (sb!c::continuation-use ,fun)))
		     ,@letp-bind)
		,local-branch))
	     ((eq (sb!c::continuation-info ,fun) :unused)
	      (aver (typep ,kind 'sb!c::function-info))
	      (do-funny-function (sb!c::continuation-function-name ,fun)))
	     (t
	      (aver (typep ,kind 'sb!c::function-info))
	      (do-combination :full nil ,type))))))

;;;; INTERNAL-EVAL

;;; Evaluate an arbitary form. We convert the form, then call internal
;;; APPLY on it. If *ALREADY-EVALED-THIS* is true, then we bind it to
;;; NIL around the apply to limit the inhibition to the lexical scope
;;; of the EVAL-WHEN.
(defun internal-eval (form)
  (let ((res (sb!c:compile-for-eval form)))
    (if *already-evaled-this*
	(let ((*already-evaled-this* nil))
	  (internal-apply res nil '#()))
	(internal-apply res nil '#()))))

;;; This passes on a node's value appropriately, possibly returning from
;;; function to do so. When we are tail-p, don't push the value, return it on
;;; the system's actual call stack; when we blow out of function this way, we
;;; must return the interpreter's stack to the its state before this call to
;;; function. When we're in a multiple value context or heading for a return
;;; node, we push a list of the value for easier handling later. Otherwise,
;;; just push the value on the interpreter's stack.
;;;
;;; FIXME: maybe used only in this file, if so, needn't be in runtime
(defmacro value (node info value frame-ptr function)
  `(cond ((sb!c::node-tail-p ,node)
	  (eval-stack-reset-top ,frame-ptr)
	  (return-from ,function ,value))
	 ((member ,info '(:multiple :return) :test #'eq)
	  (eval-stack-push (list ,value)))
	 (t (aver (eq ,info :single))
	    (eval-stack-push ,value))))

#!+sb-show
(defun maybe-trace-nodes (node)
  (when *internal-apply-node-trace*
    (format t "<~A-node> c~S~%"
	    (type-of node)
	    (sb!c::cont-num (sb!c::node-cont node)))))

;;; Interpret LAMBDA, a compiler IR1 data structure representing a
;;; function, applying it to ARGS. CLOSURE is the environment in which
;;; to run LAMBDA, the variables and such closed over to form LAMBDA.
;;; The call occurs on the interpreter's stack, so save the current
;;; top and extend the stack for this lambda's call frame. Then store
;;; the args into locals on the stack.
;;;
;;; ARGS is the list of arguments to apply to. If IGNORE-UNUSED is
;;; true, then values for un-read variables are present in the
;;; argument list, and must be discarded (always true except in a
;;; local call.) ARGS may run out of values before VARS runs out of
;;; variables (in the case of an XEP with optionals); we just do CAR
;;; of NIL and store NIL. This is not the proper defaulting (which is
;;; done by explicit code in the XEP.)
(defun internal-apply (lambda args closure &optional (ignore-unused t))
  (let ((frame-ptr *eval-stack-top*))
    (eval-stack-extend (sb!c:lambda-eval-info-frame-size (sb!c::lambda-info lambda)))
    (do ((vars (sb!c::lambda-vars lambda) (cdr vars))
	 (args args))
	((null vars))
      (let ((var (car vars)))
	(cond ((sb!c::leaf-refs var)
	       (setf (eval-stack-local frame-ptr (sb!c::lambda-var-info var))
		     (if (sb!c::lambda-var-indirect var)
			 (sb!c::make-value-cell (pop args))
			 (pop args))))
	      (ignore-unused (pop args)))))
    (internal-apply-loop (sb!c::lambda-bind lambda) frame-ptr lambda args
			 closure)))

;;; This does the work of INTERNAL-APPLY. This also calls itself
;;; recursively for certain language features, such as CATCH. First is
;;; the node at which to start interpreting. FRAME-PTR is the current
;;; frame pointer for accessing local variables. LAMBDA is the IR1
;;; lambda from which comes the nodes a given call to this function
;;; processes, and CLOSURE is the environment for interpreting LAMBDA.
;;; ARGS is the argument list for the lambda given to INTERNAL-APPLY,
;;; and we have to carry it around with us in case of &more-arg or
;;; &rest-arg processing which is represented explicitly in the
;;; compiler's IR1.
;;;
;;; KLUDGE: Due to having a truly tail recursive interpreter, some of
;;; the branches handling a given node need to RETURN-FROM this
;;; routine. Also, some calls this makes to do work for it must occur
;;; in tail recursive positions. Because of this required access to
;;; this function lexical environment and calling positions, we often
;;; are unable to break off logical chunks of code into functions. We
;;; have written macros intended solely for use in this routine, and
;;; due to all the local stuff they need to access and length complex
;;; calls, we have written them to sleazily access locals from this
;;; routine. In addition to assuming a block named internal-apply-loop
;;; exists, they set and reference the following variables: NODE,
;;; CONT, FRAME-PTR, CLOSURE, BLOCK, LAST-CONT, and SET-BLOCK-P.
;;; FIXME: Perhaps this kludge could go away if we convert to a
;;; compiler-only implementation?
(defun internal-apply-loop (first frame-ptr lambda args closure)
  ;; FIXME: This will cause source code location information to be compiled
  ;; into the executable, which will probably cause problems for users running
  ;; without the sources and/or without the build-the-system readtable.
  (declare (optimize (debug 2)))
  (let* ((block (sb!c::node-block first))
	 (last-cont (sb!c::node-cont (sb!c::block-last block)))
	 (node first)
	 (set-block-p nil))
      (loop
	(let ((cont (sb!c::node-cont node)))
	  (etypecase node
	    (sb!c::ref
	     #!+sb-show (maybe-trace-nodes node)
	     (let ((info (sb!c::continuation-info cont)))
	       (unless (eq info :unused)
		 (value node info (leaf-value node frame-ptr closure)
			frame-ptr internal-apply-loop))))
	    (sb!c::combination
	     #!+sb-show (maybe-trace-nodes node)
	     (combination-node :normal))
	    (sb!c::cif
	     #!+sb-show (maybe-trace-nodes node)
	     ;; IF nodes always occur at the end of a block, so pick another.
	     (set-block (if (eval-stack-pop)
			    (sb!c::if-consequent node)
			    (sb!c::if-alternative node))))
	    (sb!c::bind
	     #!+sb-show (maybe-trace-nodes node)
	     ;; Ignore bind nodes since INTERNAL-APPLY extends the
	     ;; stack for all of a lambda's locals, and the
	     ;; SB!C::COMBINATION branch handles LET binds (moving
	     ;; values off stack top into locals).
	     )
	    (sb!c::cset
	     #!+sb-show (maybe-trace-nodes node)
	     (let ((info (sb!c::continuation-info cont))
		   (res (set-leaf-value node frame-ptr closure
					(eval-stack-pop))))
	       (unless (eq info :unused)
		 (value node info res frame-ptr internal-apply-loop))))
	    (sb!c::entry
	     #!+sb-show (maybe-trace-nodes node)
	     (let ((info (cdr (assoc node (sb!c:lambda-eval-info-entries
					   (sb!c::lambda-info lambda))))))
	       ;; No info means no-op entry for CATCH or UNWIND-PROTECT.
	       (when info
		 ;; Store stack top for restoration in local exit
		 ;; situation in SB!C::EXIT branch.
		 (setf (eval-stack-local frame-ptr
					 (sb!c:entry-node-info-st-top info))
		       *eval-stack-top*)
		 (let ((tag (sb!c:entry-node-info-nlx-tag info)))
		   (when tag
		     ;; Non-local lexical exit (someone closed over a
		     ;; GO tag or BLOCK name).
		     (let ((unique-tag (cons nil nil))
			   values)
		       (setf (eval-stack-local frame-ptr tag) unique-tag)
		       (if (eq cont last-cont)
			   (change-blocks (car (sb!c::block-succ block)))
			   (setf node (sb!c::continuation-next cont)))
		       (loop
			 (multiple-value-setq (values block node cont last-cont)
			   (catch unique-tag
			     (internal-apply-loop node frame-ptr
						  lambda args closure)))

			 (when (eq values :fell-through)
			   ;; We hit a %LEXICAL-EXIT-BREAKUP.
			   ;; Interpreting state is set with MV-SETQ above.
			   ;; Just get out of this branch and go on.
			   (return))

			 (unless (eq values :non-local-go)
			   ;; We know we're non-locally exiting from a
			   ;; BLOCK with values (saw a RETURN-FROM).
			   (ecase (sb!c::continuation-info cont)
			     (:single
			      (eval-stack-push (car values)))
			     ((:multiple :return)
			      (eval-stack-push values))
			     (:unused)))
			 ;; Start interpreting again at the target, skipping
			 ;; the %NLX-ENTRY block.
			 (setf node
			       (sb!c::continuation-next
				(sb!c::block-start
				 (car (sb!c::block-succ block))))))))))))
	    (sb!c::exit
	     #!+sb-show (maybe-trace-nodes node)
	     (let* ((incoming-values (sb!c::exit-value node))
		    (values (if incoming-values (eval-stack-pop))))
	       (cond
		((eq (sb!c::lambda-environment lambda)
		     (sb!c::block-environment
		      (sb!c::node-block (sb!c::exit-entry node))))
		 ;; Local exit.
		 ;; Fixup stack top and massage values for destination.
		 (eval-stack-reset-top
		  (eval-stack-local frame-ptr
				    (sb!c:entry-node-info-st-top
				     (cdr (assoc (sb!c::exit-entry node)
						 (sb!c:lambda-eval-info-entries
						  (sb!c::lambda-info lambda)))))))
		 (ecase (sb!c::continuation-info cont)
		   (:single
		    (aver incoming-values)
		    (eval-stack-push (car values)))
		   ((:multiple :return)
		    (aver incoming-values)
		    (eval-stack-push values))
		   (:unused)))
		(t
		 (let ((info (sb!c::find-nlx-info (sb!c::exit-entry node)
						  cont)))
		   (throw
		    (svref closure
			   (position info
				     (sb!c::environment-closure
				      (sb!c::node-environment node))
				     :test #'eq))
		    (if incoming-values
			(values values (sb!c::nlx-info-target info) nil cont)
			(values :non-local-go (sb!c::nlx-info-target info)))))))))
	    (sb!c::creturn
	     #!+sb-show (maybe-trace-nodes node)
	     (let ((values (eval-stack-pop)))
	       (eval-stack-reset-top frame-ptr)
	       (return-from internal-apply-loop (values-list values))))
	    (sb!c::mv-combination
	     #!+sb-show (maybe-trace-nodes node)
	     (combination-node :mv-call)))
	  ;; See function doc below.
	  (reference-this-var-to-keep-it-alive node)
	  (reference-this-var-to-keep-it-alive frame-ptr)
	  (reference-this-var-to-keep-it-alive closure)
	  (cond ((not (eq cont last-cont))
		 (setf node (sb!c::continuation-next cont)))
		;; Currently only the last node in a block causes this loop to
		;; change blocks, so we never just go to the next node when
		;; the current node's branch tried to change blocks.
		(set-block-p
		 (change-blocks))
		(t
		 ;; CIF nodes set the block for us, but other last
		 ;; nodes do not.
		 (change-blocks (car (sb!c::block-succ block)))))))))

;;; This function allows a reference to a variable that the compiler cannot
;;; easily eliminate as unnecessary. We use this at the end of the node
;;; dispatch in INTERNAL-APPLY-LOOP to make sure the node variable has a
;;; valid value. Each node branch tends to reference it at the beginning,
;;; and then there is no reference but a set at the end; the compiler then
;;; kills the variable between the reference in the dispatch branch and when
;;; we set it at the end. The problem is that most error will occur in the
;;; interpreter within one of these node dispatch branches.
(defun reference-this-var-to-keep-it-alive (node)
  node)

;;; This sets a SB!C::CSET node's var to value, returning value. When
;;; var is local, we have to compare its home environment to the
;;; current one, node's environment. If they're the same, we check to
;;; see whether the var is indirect, and store the value on the stack
;;; or in the value cell as appropriate. Otherwise, var is a closure
;;; variable, and since we're setting it, we know its location
;;; contains an indirect value object.
(defun set-leaf-value (node frame-ptr closure value)
  (let ((var (sb!c::set-var node)))
    (etypecase var
      (sb!c::lambda-var
       (set-leaf-value-lambda-var node var frame-ptr closure value))
      (sb!c::global-var
       (setf (symbol-value (sb!c::global-var-name var)) value)))))

;;; This does SET-LEAF-VALUE for a LAMBDA-VAR leaf. The debugger tools'
;;; internals use this also to set interpreted local variables.
(defun set-leaf-value-lambda-var (node var frame-ptr closure value)
  ;; Note: We avoid trying to set a lexical variable with no refs
  ;; because the compiler deletes such variables.
  (when (sb!c::leaf-refs var)
    (let ((env (sb!c::node-environment node)))
      (cond ((not (eq (sb!c::lambda-environment (sb!c::lambda-var-home var))
                      env))
	     (sb!c::value-cell-set
              (svref closure
                     (position var (sb!c::environment-closure env)
                               :test #'eq))
              value))
            ((sb!c::lambda-var-indirect var)
	     (sb!c::value-cell-set
              (eval-stack-local frame-ptr (sb!c::lambda-var-info var))
              value))
            (t
	     (setf (eval-stack-local frame-ptr (sb!c::lambda-var-info var))
		   value))))))

;;; This figures out how to return a value for a ref node. LEAF is the
;;; ref's structure that tells us about the value, and it is one of
;;; the following types:
;;;    constant   -- It knows its own value.
;;;    global-var -- It's either a value or function reference. Get it right.
;;;    local-var  -- This may on the stack or in the current closure, the
;;; 		     environment for the lambda INTERNAL-APPLY is currently
;;;		     executing. If the leaf's home environment is the same
;;;		     as the node's home environment, then the value is on the
;;;		     stack, else it's in the closure since it came from another
;;;		     environment. Whether the var comes from the stack or the
;;;		     closure, it could have come from a closure, and it could
;;;		     have been closed over for setting. When this happens, the
;;;		     actual value is stored in an indirection object, so
;;;		     indirect. See COMPUTE-CLOSURE for the description of
;;;		     the structure of the closure argument to this function.
;;;    functional -- This is a reference to an interpreted function that may
;;;		     be passed or called anywhere. We return a real function
;;;		     that calls INTERNAL-APPLY, closing over the leaf. We also
;;;		     have to compute a closure, running environment, for the
;;;		     lambda in case it references stuff in the current
;;;		     environment. If the closure is empty and there is no
;;;		  functional environment, then we use
;;;		  MAKE-INTERPRETED-FUNCTION to make a cached translation.
;;;		  Since it is too late to lazily convert, we set up the
;;;		  INTERPRETED-FUNCTION to be already converted.
(defun leaf-value (node frame-ptr closure)
  (let ((leaf (sb!c::ref-leaf node)))
    (etypecase leaf
      (sb!c::constant
       (sb!c::constant-value leaf))
      (sb!c::global-var
       (locally (declare (optimize (safety 1)))
	 (if (eq (sb!c::global-var-kind leaf) :global-function)
	     (let ((name (sb!c::global-var-name leaf)))
	       (if (symbolp name)
		   (symbol-function name)
		   (fdefinition name)))
	     (symbol-value (sb!c::global-var-name leaf)))))
      (sb!c::lambda-var
       (leaf-value-lambda-var node leaf frame-ptr closure))
      (sb!c::functional
       (let* ((calling-closure (compute-closure node leaf frame-ptr closure))
	      (real-fun (sb!c::functional-entry-function leaf))
	      (arg-doc (sb!c::functional-arg-documentation real-fun)))
	 (cond ((sb!c:lambda-eval-info-function (sb!c::leaf-info leaf)))
	       ((and (zerop (length calling-closure))
		     (null (sb!c::lexenv-functions
			    (sb!c::functional-lexenv real-fun))))
		(let ((res (make-interpreted-function
			    (sb!c::functional-inline-expansion real-fun))))
		  (push res *interpreted-function-cache*)
		  (setf (interpreted-function-definition res) leaf)
		  (setf (interpreted-function-converted-once res) t)
		  (setf (interpreted-function-arglist res) arg-doc)
		  (setf (interpreted-function-%name res)
			(sb!c::leaf-name real-fun))
		  (setf (sb!c:lambda-eval-info-function
			 (sb!c::leaf-info leaf)) res)
		  res))
	       (t
		(let ((res (%make-interpreted-function
			    :definition leaf
			    :%name (sb!c::leaf-name real-fun)
			    :arglist arg-doc
			    :closure calling-closure)))
		  (setf (funcallable-instance-function res)
			#'(instance-lambda (&rest args)
			    (declare (list args))
			    (internal-apply
			     (interpreted-function-definition res)
			     (cons (length args) args)
			     (interpreted-function-closure res))))
		  res))))))))

;;; This does LEAF-VALUE for a lambda-var leaf. The debugger tools' internals
;;; uses this also to reference interpreted local variables.
(defun leaf-value-lambda-var (node leaf frame-ptr closure)
  (let* ((env (sb!c::node-environment node))
	 (temp
	  (if (eq (sb!c::lambda-environment (sb!c::lambda-var-home leaf))
		  env)
	      (eval-stack-local frame-ptr (sb!c::lambda-var-info leaf))
	      (svref closure
		     (position leaf (sb!c::environment-closure env)
			       :test #'eq)))))
    (if (sb!c::lambda-var-indirect leaf)
	(sb!c::value-cell-ref temp)
	temp)))

;;; Compute a closure for a local call and for returned call'able
;;; closure objects. Sometimes the closure is a SIMPLE-VECTOR of no
;;; elements. NODE is either a reference node or a combination node.
;;; LEAF is either the leaf of the reference node or the lambda to
;;; internally apply for the combination node. FRAME-PTR is the
;;; current frame pointer for fetching current values to store in the
;;; closure. CLOSURE is the current closure, the closed-over
;;; environment of the currently interpreting LAMBDA.
;;;
;;; A computed closure is a vector corresponding to the list of
;;; closure variables described in an environment. The position of a
;;; lambda-var in this closure list is the index into the closure
;;; vector of values.
(defun compute-closure (node leaf frame-ptr closure)
  (let* ((current-env (sb!c::node-environment node))
	 (current-closure-vars (sb!c::environment-closure current-env))
	 ;; FUNCTIONAL-ENV is the environment description for leaf,
	 ;; the lambda for which we're computing a closure. This
	 ;; environment describes which of lambda's vars we find in
	 ;; lambda's closure when it's running, versus finding them on
	 ;; the stack.
	 (functional-env (sb!c::lambda-environment leaf))
	 (functional-closure-vars (sb!c::environment-closure functional-env))
	 (functional-closure (make-array (length functional-closure-vars))))
    ;; For each lambda-var VAR in the functional environment's closure
    ;; list, if the VAR's home environment is the current environment,
    ;; then get a value off the stack and store it in the closure
    ;; we're computing. Otherwise VAR's value comes from somewhere
    ;; else, but we have it in our current closure, the environment
    ;; we're running in as we compute this new closure. Find this
    ;; value the same way we do in LEAF-VALUE, by finding VAR's
    ;; position in the current environment's description of the
    ;; current closure.
    (do ((vars functional-closure-vars (cdr vars))
	 (i 0 (1+ i)))
	((null vars))
      (let ((ele (car vars)))
	(setf (svref functional-closure i)
	      (etypecase ele
		(sb!c::lambda-var
		 (if (eq (sb!c::lambda-environment (sb!c::lambda-var-home ele))
			 current-env)
		     (eval-stack-local frame-ptr (sb!c::lambda-var-info ele))
		     (svref closure
			    (position ele current-closure-vars
				      :test #'eq))))
		(sb!c::nlx-info
		 (if (eq (sb!c::block-environment (sb!c::nlx-info-target ele))
			 current-env)
		     (eval-stack-local
		      frame-ptr
		      (sb!c:entry-node-info-nlx-tag
		       (cdr (assoc ;; entry node for non-local extent
			     (sb!c::cleanup-mess-up
			      (sb!c::nlx-info-cleanup ele))
			     (sb!c::lambda-eval-info-entries
			      (sb!c::lambda-info
			       ;; the lambda INTERNAL-APPLY-LOOP tosses around
			       (sb!c::environment-function
				(sb!c::node-environment node))))))))
		     (svref closure
			    (position ele current-closure-vars
				      :test #'eq))))))))
    functional-closure))

;;; INTERNAL-APPLY uses this to invoke a function from the
;;; interpreter's stack on some arguments also taken from the stack.
;;; When tail-p is non-nil, control does not return to INTERNAL-APPLY
;;; to further interpret the current IR1 lambda, so INTERNAL-INVOKE
;;; must clean up the current interpreter's stack frame.
(defun internal-invoke (arg-count &optional tailp)
  (let ((args (eval-stack-args arg-count)) ;LET says this init form runs first.
	(fun (eval-stack-pop)))
    (when tailp (eval-stack-reset-top tailp))
    #!+sb-show (when *internal-apply-node-trace*
		 (format t "(~S~{ ~S~})~%" fun args))
    (apply fun args)))

;;; This is almost just like INTERNAL-INVOKE. We call
;;; MV-EVAL-STACK-ARGS, and our function is in a list on the stack
;;; instead of simply on the stack.
(defun mv-internal-invoke (arg-count &optional tailp)
  (let ((args (mv-eval-stack-args arg-count)) ; LET runs this init form first.
	(fun (car (eval-stack-pop))))
    (when tailp (eval-stack-reset-top tailp))
    #!+sb-show (when *internal-apply-node-trace*
		 (format t "(~S~{ ~S~})~%" fun args))
    (apply fun args)))

;;; Return a list of the top arg-count elements on the interpreter's
;;; stack. This removes them from the stack.
(defun eval-stack-args (arg-count)
  (let ((args nil))
    (dotimes (i arg-count args)
      (push (eval-stack-pop) args))))

;;; This assumes the top count elements on interpreter's stack are
;;; lists. This returns a single list with all the elements from these
;;; lists.
(defun mv-eval-stack-args (count)
  (if (= count 1)
      (eval-stack-pop)
      (let ((last (eval-stack-pop)))
	(dotimes (i (1- count))
	  (let ((next (eval-stack-pop)))
	    (setf last
		  (if next (nconc next last) last))))
	last)))

;;; This stores lambda's vars, stack locals, from values popped off the stack.
;;; When a var has no references, the compiler computes IR1 such that the
;;; continuation delivering the value for the unreference var appears unused.
;;; Because of this, the interpreter drops the value on the floor instead of
;;; saving it on the stack for binding, so we only pop a value when the var has
;;; some reference. INTERNAL-APPLY uses this for sb!c::combination nodes
;;; representing LET's.
;;;
;;; When storing the local, if it is indirect, then someone closes over it for
;;; setting instead of just for referencing. We then store an indirection cell
;;; with the value, and the referencing code for locals knows how to get the
;;; actual value.
(defun store-let-vars (lambda frame-ptr)
  (let* ((vars (sb!c::lambda-vars lambda))
	 (args (eval-stack-args (count-if #'sb!c::leaf-refs vars))))
    (declare (list vars args))
    (dolist (v vars)
      (when (sb!c::leaf-refs v)
	(setf (eval-stack-local frame-ptr (sb!c::lambda-var-info v))
	      (if (sb!c::lambda-var-indirect v)
		  (sb!c::make-value-cell (pop args))
		  (pop args)))))))

;;; This is similar to STORE-LET-VARS, but the values for the locals
;;; appear on the stack in a list due to forms that delivered multiple
;;; values to this lambda/let. Unlike STORE-LET-VARS, there is no
;;; control over the delivery of a value for an unreferenced var, so
;;; we drop the corresponding value on the floor when no one
;;; references it. INTERNAL-APPLY uses this for sb!c::mv-combination
;;; nodes representing LET's.
(defun store-mv-let-vars (lambda frame-ptr count)
  (aver (= count 1))
  (let ((args (eval-stack-pop)))
    (dolist (v (sb!c::lambda-vars lambda))
      (if (sb!c::leaf-refs v)
	  (setf (eval-stack-local frame-ptr (sb!c::lambda-var-info v))
		(if (sb!c::lambda-var-indirect v)
		    (sb!c::make-value-cell (pop args))
		    (pop args)))
	  (pop args)))))

#|
;;; This stores lambda's vars, stack locals, from multiple values stored on the
;;; top of the stack in a list. Since these values arrived multiply, there is
;;; no control over the delivery of each value for an unreferenced var, so
;;; unlike STORE-LET-VARS, we have values for variables never used. We drop
;;; the value corresponding to an unreferenced var on the floor.
;;; INTERNAL-APPLY uses this for sb!c::mv-combination nodes representing LET's.
;;;
;;; IR1 represents variables bound from multiple values in a list in the
;;; opposite order of the values list. We use STORE-MV-LET-VARS-AUX to recurse
;;; down the vars list until we bottom out, storing values on the way back up
;;; the recursion. You must do this instead of NREVERSE'ing the args list, so
;;; when we run out of values, we store nil's in the correct lambda-vars.
(defun store-mv-let-vars (lambda frame-ptr count)
  (aver (= count 1))
  (print  (sb!c::lambda-vars lambda))
  (store-mv-let-vars-aux frame-ptr (sb!c::lambda-vars lambda) (eval-stack-pop)))
(defun store-mv-let-vars-aux (frame-ptr vars args)
  (if vars
      (let ((remaining-args (store-mv-let-vars-aux frame-ptr (cdr vars) args))
	    (v (car vars)))
	(when (sb!c::leaf-refs v)
	  (setf (eval-stack-local frame-ptr (sb!c::lambda-var-info v))
		(if (sb!c::lambda-var-indirect v)
		    (sb!c::make-value-cell (car remaining-args))
		    (car remaining-args))))
	(cdr remaining-args))
      args))
|#
