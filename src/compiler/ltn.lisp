;;;; This file contains the LTN pass in the compiler. LTN allocates
;;;; expression evaluation TNs, makes nearly all the implementation
;;;; policy decisions, and also does a few other miscellaneous things.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;;; utilities

;;; Return the policies keyword indicated by the node policy.
(defun translation-policy (node)
  (declare (type node node))
  (policy node
	  (let ((eff-space (max space
				;; on the theory that if the code is
				;; smaller, it will take less time to
				;; compile (could lose if the smallest
				;; case is out of line, and must
				;; allocate many linkage registers):
				compilation-speed)))
	    (if (zerop safety)
		(if (>= speed eff-space) :fast :small)
		(if (>= speed eff-space) :fast-safe :safe)))))

;;; Return true if POLICY is a safe policy.
#!-sb-fluid (declaim (inline policy-safe-p))
(defun policy-safe-p (policy)
  (declare (type policies policy))
  (or (eq policy :safe) (eq policy :fast-safe)))

;;; Called when an unsafe policy indicates that no type check should
;;; be done on CONT. We delete the type check unless it is :ERROR
;;; (indicating a compile-time type error.)
#!-sb-fluid (declaim (inline flush-type-check))
(defun flush-type-check (cont)
  (declare (type continuation cont))
  (when (member (continuation-type-check cont) '(t :no-check))
    (setf (continuation-%type-check cont) :deleted))
  (values))

;;; An annotated continuation's primitive-type.
#!-sb-fluid (declaim (inline continuation-ptype))
(defun continuation-ptype (cont)
  (declare (type continuation cont))
  (ir2-continuation-primitive-type (continuation-info cont)))

;;; Return true if a constant LEAF is of a type which we can legally
;;; directly reference in code. Named constants with arbitrary pointer
;;; values cannot, since we must preserve EQLness.
(defun legal-immediate-constant-p (leaf)
  (declare (type constant leaf))
  (or (null (leaf-name leaf))
      (typecase (constant-value leaf)
	((or number character) t)
	(symbol (symbol-package (constant-value leaf)))
	(t nil))))

;;; If CONT is used only by a REF to a leaf that can be delayed, then
;;; return the leaf, otherwise return NIL.
(defun continuation-delayed-leaf (cont)
  (declare (type continuation cont))
  (let ((use (continuation-use cont)))
    (and (ref-p use)
	 (let ((leaf (ref-leaf use)))
	   (etypecase leaf
	     (lambda-var (if (null (lambda-var-sets leaf)) leaf nil))
	     (constant (if (legal-immediate-constant-p leaf) leaf nil))
	     ((or functional global-var) nil))))))

;;; Annotate a normal single-value continuation. If its only use is a
;;; ref that we are allowed to delay the evaluation of, then we mark
;;; the continuation for delayed evaluation, otherwise we assign a TN
;;; to hold the continuation's value. If the continuation has a type
;;; check, we make the TN according to the proven type to ensure that
;;; the possibly erroneous value can be represented.
(defun annotate-1-value-continuation (cont)
  (declare (type continuation cont))
  (let ((info (continuation-info cont)))
    (assert (eq (ir2-continuation-kind info) :fixed))
    (cond
     ((continuation-delayed-leaf cont)
      (setf (ir2-continuation-kind info) :delayed))
     ((member (continuation-type-check cont) '(:deleted nil))
      (setf (ir2-continuation-locs info)
	    (list (make-normal-tn (ir2-continuation-primitive-type info)))))
     (t
      (setf (ir2-continuation-locs info)
	    (list (make-normal-tn
		   (primitive-type
		    (single-value-type (continuation-proven-type cont)))))))))
  (values))

;;; Make an IR2-CONTINUATION corresponding to the continuation type
;;; and then do ANNOTATE-1-VALUE-CONTINUATION. If POLICY-KEYWORD isn't
;;; a safe policy keyword, then we clear the TYPE-CHECK flag.
(defun annotate-ordinary-continuation (cont policy-keyword)
  (declare (type continuation cont)
	   (type policies policy-keyword))
  (let ((info (make-ir2-continuation
	       (primitive-type (continuation-type cont)))))
    (setf (continuation-info cont) info)
    (unless (policy-safe-p policy-keyword)
      (flush-type-check cont))
    (annotate-1-value-continuation cont))
  (values))

;;; Annotate the function continuation for a full call. If the only
;;; reference is to a global function and Delay is true, then we delay
;;; the reference, otherwise we annotate for a single value.
;;;
;;; Unlike for an argument, we only clear the type check flag when the
;;; policy is unsafe, since the check for a valid function object must
;;; be done before the call.
(defun annotate-function-continuation (cont policy &optional (delay t))
  (declare (type continuation cont) (type policies policy))
  (unless (policy-safe-p policy)
    (flush-type-check cont))
  (let* ((ptype (primitive-type (continuation-type cont)))
	 (tn-ptype (if (member (continuation-type-check cont) '(:deleted nil))
		       ptype
		       (primitive-type
			(single-value-type
			 (continuation-proven-type cont)))))
	 (info (make-ir2-continuation ptype)))
    (setf (continuation-info cont) info)
    (let ((name (continuation-function-name cont t)))
      (if (and delay name)
	  (setf (ir2-continuation-kind info) :delayed)
	  (setf (ir2-continuation-locs info)
		(list (make-normal-tn tn-ptype))))))
  (values))

;;; If TAIL-P is true, then we check to see whether the call can really
;;; be a tail call by seeing if this function's return convention is :UNKNOWN.
;;; If so, we move the call block succssor link from the return block to
;;; the component tail (after ensuring that they are in separate blocks.)
;;; This allows the return to be deleted when there are no non-tail uses.
(defun flush-full-call-tail-transfer (call)
  (declare (type basic-combination call))
  (let ((tails (and (node-tail-p call)
		    (lambda-tail-set (node-home-lambda call)))))
    (when tails
      (cond ((eq (return-info-kind (tail-set-info tails)) :unknown)
	     (node-ends-block call)
	     (let ((block (node-block call)))
	       (unlink-blocks block (first (block-succ block)))
	       (link-blocks block (component-tail (block-component block)))))
	    (t
	     (setf (node-tail-p call) nil)))))
  (values))

;;; We set the kind to :FULL or :FUNNY, depending on whether there is an
;;; IR2-CONVERT method. If a funny function, then we inhibit tail recursion
;;; and type check normally, since the IR2 convert method is going to want to
;;; deliver values normally. We still annotate the function continuation,
;;; since IR2tran might decide to call after all.
;;;
;;; If not funny, we always flush arg type checks, but do it after
;;; annotation when the policy is safe, since we don't want to choose the TNs
;;; according to a type assertions that may not hold.
;;;
;;; Note that args may already be annotated because template selection can
;;; bail out to here.
(defun ltn-default-call (call policy)
  (declare (type combination call) (type policies policy))
  (let ((kind (basic-combination-kind call)))
    (annotate-function-continuation (basic-combination-fun call) policy)

    (cond
     ((and (function-info-p kind)
	   (function-info-ir2-convert kind))
      (setf (basic-combination-info call) :funny)
      (setf (node-tail-p call) nil)
      (dolist (arg (basic-combination-args call))
	(unless (continuation-info arg)
	  (setf (continuation-info arg)
		(make-ir2-continuation
		 (primitive-type
		  (continuation-type arg)))))
	(annotate-1-value-continuation arg)))
     (t
      (let ((safe-p (policy-safe-p policy)))
	(dolist (arg (basic-combination-args call))
	  (unless safe-p (flush-type-check arg))
	  (unless (continuation-info arg)
	    (setf (continuation-info arg)
		  (make-ir2-continuation
		   (primitive-type
		    (continuation-type arg)))))
	  (annotate-1-value-continuation arg)
	  (when safe-p (flush-type-check arg))))
      (when (eq kind :error)
	(setf (basic-combination-kind call) :full))
      (setf (basic-combination-info call) :full)
      (flush-full-call-tail-transfer call))))

  (values))

;;; Annotate a continuation for unknown multiple values:
;;; -- Delete any type check, regardless of policy, since we IR2 conversion
;;;    isn't prepared to check unknown-values continuations. If we delete a
;;;    type check when the policy is safe, then we emit a warning.
;;; -- Add the continuation to the IR2-Block-Popped if it is used across a
;;;    block boundary.
;;; -- Assign a :Unknown IR2-Continuation.
;;;
;;; Note: it is critical that this be called only during LTN analysis of Cont's
;;; DEST, and called in the order that the continuations are received.
;;; Otherwise the IR2-Block-Popped and IR2-Component-Values-XXX will get all
;;; messed up.
(defun annotate-unknown-values-continuation (cont policy)
  (declare (type continuation cont) (type policies policy))
  (when (eq (continuation-type-check cont) t)
    (let* ((dest (continuation-dest cont))
	   (*compiler-error-context* dest))
      (when (and (policy-safe-p policy)
		 (policy dest (>= safety inhibit-warnings)))
	(compiler-note "unable to check type assertion in unknown-values ~
			context:~% ~S"
		       (continuation-asserted-type cont))))
    (setf (continuation-%type-check cont) :deleted))

  (let* ((block (node-block (continuation-dest cont)))
	 (use (continuation-use cont))
	 (2block (block-info block)))
    (unless (and use (eq (node-block use) block))
      (setf (ir2-block-popped 2block)
	    (nconc (ir2-block-popped 2block) (list cont)))))

  (let ((2cont (make-ir2-continuation nil)))
    (setf (ir2-continuation-kind 2cont) :unknown)
    (setf (ir2-continuation-locs 2cont) (make-unknown-values-locations))
    (setf (continuation-info cont) 2cont))

  (values))

;;; Annotate Cont for a fixed, but arbitrary number of values, of the
;;; specified primitive Types. If the continuation has a type check, we
;;; annotate for the number of values indicated by Types, but only use proven
;;; type information.
(defun annotate-fixed-values-continuation (cont policy types)
  (declare (type continuation cont) (type policies policy) (list types))
  (unless (policy-safe-p policy) (flush-type-check cont))

  (let ((res (make-ir2-continuation nil)))
    (if (member (continuation-type-check cont) '(:deleted nil))
	(setf (ir2-continuation-locs res) (mapcar #'make-normal-tn types))
	(let* ((proven (mapcar #'(lambda (x)
				   (make-normal-tn (primitive-type x)))
			       (values-types
				(continuation-proven-type cont))))
	       (num-proven (length proven))
	       (num-types (length types)))
	  (setf (ir2-continuation-locs res)
		(cond
		 ((< num-proven num-types)
		  (append proven
			  (make-n-tns (- num-types num-proven)
				      *backend-t-primitive-type*)))
		 ((> num-proven num-types)
		  (subseq proven 0 num-types))
		 (t
		  proven)))))
    (setf (continuation-info cont) res))

  (values))

;;;; node-specific analysis functions

;;; Annotate the result continuation for a function. We use the Return-Info
;;; computed by GTN to determine how to represent the return values within the
;;; function:
;;; -- If the tail-set has a fixed values count, then use that many values.
;;; -- If the actual uses of the result continuation in this function have a
;;;    fixed number of values (after intersection with the assertion), then use
;;;    that number. We throw out TAIL-P :FULL and :LOCAL calls, since we know
;;;    they will truly end up as TR calls. We can use the
;;;    BASIC-COMBINATION-INFO even though it is assigned by this phase, since
;;;    the initial value NIL doesn't look like a TR call.
;;;
;;;    If there are *no* non-tail-call uses, then it falls out that we annotate
;;;    for one value (type is NIL), but the return will end up being deleted.
;;;
;;;    In non-perverse code, the DFO walk will reach all uses of the result
;;;    continuation before it reaches the RETURN. In perverse code, we may
;;;    annotate for unknown values when we didn't have to.
;;; -- Otherwise, we must annotate the continuation for unknown values.
(defun ltn-analyze-return (node policy)
  (declare (type creturn node) (type policies policy))
  (let* ((cont (return-result node))
	 (fun (return-lambda node))
	 (returns (tail-set-info (lambda-tail-set fun)))
	 (types (return-info-types returns)))
    (if (eq (return-info-count returns) :unknown)
	(collect ((res *empty-type* values-type-union))
	  (do-uses (use (return-result node))
	    (unless (and (node-tail-p use)
			 (basic-combination-p use)
			 (member (basic-combination-info use) '(:local :full)))
	      (res (node-derived-type use))))

	  (let ((int (values-type-intersection
		      (res)
		      (continuation-asserted-type cont))))
	    (multiple-value-bind (types kind)
		(values-types (if (eq int *empty-type*) (res) int))
	      (if (eq kind :unknown)
		  (annotate-unknown-values-continuation cont policy)
		  (annotate-fixed-values-continuation
		   cont policy
		   (mapcar #'primitive-type types))))))
	(annotate-fixed-values-continuation cont policy types)))

  (values))

;;; Annotate the single argument continuation as a fixed-values
;;; continuation. We look at the called lambda to determine number and type of
;;; return values desired. It is assumed that only a function that
;;; Looks-Like-An-MV-Bind will be converted to a local call.
(defun ltn-analyze-mv-bind (call policy)
  (declare (type mv-combination call)
	   (type policies policy))
  (setf (basic-combination-kind call) :local)
  (setf (node-tail-p call) nil)
  (annotate-fixed-values-continuation
   (first (basic-combination-args call)) policy
   (mapcar #'(lambda (var)
	       (primitive-type (basic-var-type var)))
	   (lambda-vars
	    (ref-leaf
	     (continuation-use
	      (basic-combination-fun call))))))
  (values))

;;; We force all the argument continuations to use the unknown values
;;; convention. The continuations are annotated in reverse order, since the
;;; last argument is on top, thus must be popped first. We disallow delayed
;;; evaluation of the function continuation to simplify IR2 conversion of MV
;;; call.
;;;
;;; We could be cleverer when we know the number of values returned by the
;;; continuations, but optimizations of MV-Call are probably unworthwhile.
;;;
;;; We are also responsible for handling THROW, which is represented in IR1
;;; as an mv-call to the %THROW funny function. We annotate the tag
;;; continuation for a single value and the values continuation for unknown
;;; values.
(defun ltn-analyze-mv-call (call policy)
  (declare (type mv-combination call))
  (let ((fun (basic-combination-fun call))
	(args (basic-combination-args call)))
    (cond ((eq (continuation-function-name fun) '%throw)
	   (setf (basic-combination-info call) :funny)
	   (annotate-ordinary-continuation (first args) policy)
	   (annotate-unknown-values-continuation (second args) policy)
	   (setf (node-tail-p call) nil))
	  (t
	   (setf (basic-combination-info call) :full)
	   (annotate-function-continuation (basic-combination-fun call)
					   policy nil)
	   (dolist (arg (reverse args))
	     (annotate-unknown-values-continuation arg policy))
	   (flush-full-call-tail-transfer call))))

  (values))

;;; Annotate the arguments as ordinary single-value continuations. And check
;;; the successor.
(defun ltn-analyze-local-call (call policy)
  (declare (type combination call)
	   (type policies policy))
  (setf (basic-combination-info call) :local)

  (dolist (arg (basic-combination-args call))
    (when arg
      (annotate-ordinary-continuation arg policy)))

  (when (node-tail-p call)
    (set-tail-local-call-successor call))
  (values))

;;; Make sure that a tail local call is linked directly to the bind
;;; node. Usually it will be, but calls from XEPs and calls that might have
;;; needed a cleanup after them won't have been swung over yet, since we
;;; weren't sure they would really be TR until now. Also called by byte
;;; compiler.
(defun set-tail-local-call-successor (call)
  (let ((caller (node-home-lambda call))
	(callee (combination-lambda call)))
    (assert (eq (lambda-tail-set caller)
		(lambda-tail-set (lambda-home callee))))
    (node-ends-block call)
    (let ((block (node-block call)))
      (unlink-blocks block (first (block-succ block)))
      (link-blocks block (node-block (lambda-bind callee)))))
  (values))

;;; Annotate the value continuation.
(defun ltn-analyze-set (node policy)
  (declare (type cset node) (type policies policy))
  (setf (node-tail-p node) nil)
  (annotate-ordinary-continuation (set-value node) policy)
  (values))

;;; If the only use of the Test continuation is a combination annotated with
;;; a conditional template, then don't annotate the continuation so that IR2
;;; conversion knows not to emit any code, otherwise annotate as an ordinary
;;; continuation. Since we only use a conditional template if the call
;;; immediately precedes the IF node in the same block, we know that any
;;; predicate will already be annotated.
(defun ltn-analyze-if (node policy)
  (declare (type cif node) (type policies policy))
  (setf (node-tail-p node) nil)
  (let* ((test (if-test node))
	 (use (continuation-use test)))
    (unless (and (combination-p use)
		 (let ((info (basic-combination-info use)))
		   (and (template-p info)
			(eq (template-result-types info) :conditional))))
      (annotate-ordinary-continuation test policy)))
  (values))

;;; If there is a value continuation, then annotate it for unknown values.
;;; In this case, the exit is non-local, since all other exits are deleted or
;;; degenerate by this point.
(defun ltn-analyze-exit (node policy)
  (setf (node-tail-p node) nil)
  (let ((value (exit-value node)))
    (when value
      (annotate-unknown-values-continuation value policy)))
  (values))

;;; We need a special method for %Unwind-Protect that ignores the cleanup
;;; function. We don't annotate either arg, since we don't need them at
;;; run-time.
;;;
;;; [The default is o.k. for %Catch, since environment analysis converted the
;;; reference to the escape function into a constant reference to the
;;; NLX-Info.]
(defoptimizer (%unwind-protect ltn-annotate) ((escape cleanup) node policy)
  policy ; Ignore...
  (setf (basic-combination-info node) :funny)
  (setf (node-tail-p node) nil))

;;; Both of these functions need special LTN-annotate methods, since we only
;;; want to clear the Type-Check in unsafe policies. If we allowed the call to
;;; be annotated as a full call, then no type checking would be done.
;;;
;;; We also need a special LTN annotate method for %Slot-Setter so that the
;;; function is ignored. This is because the reference to a SETF function
;;; can't be delayed, so IR2 conversion would have already emitted a call to
;;; FDEFINITION by the time the IR2 convert method got control.
(defoptimizer (%slot-accessor ltn-annotate) ((struct) node policy)
  (setf (basic-combination-info node) :funny)
  (setf (node-tail-p node) nil)
  (annotate-ordinary-continuation struct policy))
(defoptimizer (%slot-setter ltn-annotate) ((struct value) node policy)
  (setf (basic-combination-info node) :funny)
  (setf (node-tail-p node) nil)
  (annotate-ordinary-continuation struct policy)
  (annotate-ordinary-continuation value policy))

;;;; known call annotation

;;; Return true if Restr is satisfied by Type. If T-OK is true, then a T
;;; restriction allows any operand type. This is also called by IR2tran when
;;; it determines whether a result temporary needs to be made, and by
;;; representation selection when it is deciding which move VOP to use.
;;; Cont and TN are used to test for constant arguments.
#!-sb-fluid (declaim (inline operand-restriction-ok))
(defun operand-restriction-ok (restr type &key cont tn (t-ok t))
  (declare (type (or (member *) cons) restr)
	   (type primitive-type type)
	   (type (or continuation null) cont)
	   (type (or tn null) tn))
  (if (eq restr '*)
      t
      (ecase (first restr)
	(:or
	 (dolist (mem (rest restr) nil)
	   (when (or (and t-ok (eq mem *backend-t-primitive-type*))
		     (eq mem type))
	     (return t))))
	(:constant
	 (cond (cont
		(and (constant-continuation-p cont)
		     (funcall (second restr) (continuation-value cont))))
	       (tn
		(and (eq (tn-kind tn) :constant)
		     (funcall (second restr) (tn-value tn))))
	       (t
		(error "Neither CONT nor TN supplied.")))))))

;;; Check that the argument type restriction for Template are satisfied in
;;; call. If an argument's TYPE-CHECK is :NO-CHECK and our policy is safe,
;;; then only :SAFE templates are o.k.
(defun template-args-ok (template call safe-p)
  (declare (type template template)
	   (type combination call))
  (let ((mtype (template-more-args-type template)))
    (do ((args (basic-combination-args call) (cdr args))
	 (types (template-arg-types template) (cdr types)))
	((null types)
	 (cond ((null args) t)
	       ((not mtype) nil)
	       (t
		(dolist (arg args t)
		  (unless (operand-restriction-ok mtype
						  (continuation-ptype arg))
		    (return nil))))))
      (when (null args) (return nil))
      (let ((arg (car args))
	    (type (car types)))
	(when (and (eq (continuation-type-check arg) :no-check)
		   safe-p
		   (not (eq (template-policy template) :safe)))
	  (return nil))
	(unless (operand-restriction-ok type (continuation-ptype arg)
					:cont arg)
	  (return nil))))))

;;; Check that Template can be used with the specifed Result-Type. Result
;;; type checking is pretty different from argument type checking due to the
;;; relaxed rules for values count. We succeed if for each required result,
;;; there is a positional restriction on the value that is at least as good.
;;; If we run out of result types before we run out of restrictions, then we
;;; only succeed if the leftover restrictions are *. If we run out of
;;; restrictions before we run out of result types, then we always win.
(defun template-results-ok (template result-type)
  (declare (type template template)
	   (type ctype result-type))
  (when (template-more-results-type template)
    (error "~S has :MORE results with :TRANSLATE." (template-name template)))
  (let ((types (template-result-types template)))
    (cond
     ((values-type-p result-type)
      (do ((ltypes (append (args-type-required result-type)
			   (args-type-optional result-type))
		   (rest ltypes))
	   (types types (rest types)))
	  ((null ltypes)
	   (dolist (type types t)
	     (unless (eq type '*)
	       (return nil))))
	(when (null types) (return t))
	(let ((type (first types)))
	  (unless (operand-restriction-ok type
					  (primitive-type (first ltypes)))
	    (return nil)))))
     (types
      (operand-restriction-ok (first types) (primitive-type result-type)))
     (t t))))

;;; Return true if Call is an ok use of Template according to Safe-P.
;;; -- If the template has a Guard that isn't true, then we ignore the
;;;    template, not even considering it to be rejected.
;;; -- If the argument type restrictions aren't satisfied, then we reject the
;;;    template.
;;; -- If the template is :Conditional, then we accept it only when the
;;;    destination of the value is an immediately following IF node.
;;; -- If either the template is safe or the policy is unsafe (i.e. we can
;;;    believe output assertions), then we test against the intersection of the
;;;    node derived type and the continuation asserted type. Otherwise, we
;;;    just use the node type. If TYPE-CHECK is null, there is no point in
;;;    doing the intersection, since the node type must be a subtype of the
;;;    assertion.
;;;
;;; If the template is *not* ok, then the second value is a keyword indicating
;;; which aspect failed.
(defun is-ok-template-use (template call safe-p)
  (declare (type template template) (type combination call))
  (let* ((guard (template-guard template))
	 (cont (node-cont call))
	 (atype (continuation-asserted-type cont))
	 (dtype (node-derived-type call)))
    (cond ((and guard (not (funcall guard)))
	   (values nil :guard))
	  ((not (template-args-ok template call safe-p))
	   (values nil
		   (if (and safe-p (template-args-ok template call nil))
		       :arg-check
		       :arg-types)))
	  ((eq (template-result-types template) :conditional)
	   (let ((dest (continuation-dest cont)))
	     (if (and (if-p dest)
		      (immediately-used-p (if-test dest) call))
		 (values t nil)
		 (values nil :conditional))))
	  ((template-results-ok
	    template
	    (if (and (or (eq (template-policy template) :safe)
			 (not safe-p))
		     (continuation-type-check cont))
		(values-type-intersection dtype atype)
		dtype))
	   (values t nil))
	  (t
	   (values nil :result-types)))))

;;; Use operand type information to choose a template from the list
;;; Templates for a known Call. We return three values:
;;; 1. The template we found.
;;; 2. Some template that we rejected due to unsatisfied type restrictions, or
;;;    NIL if none.
;;; 3. The tail of Templates for templates we haven't examined yet.
;;;
;;; We just call IS-OK-TEMPLATE-USE until it returns true.
(defun find-template (templates call safe-p)
  (declare (list templates) (type combination call))
  (do ((templates templates (rest templates))
       (rejected nil))
      ((null templates)
       (values nil rejected nil))
    (let ((template (first templates)))
      (when (is-ok-template-use template call safe-p)
	(return (values template rejected (rest templates))))
      (setq rejected template))))

;;; Given a partially annotated known call and a translation policy, return
;;; the appropriate template, or NIL if none can be found. We scan the
;;; templates (ordered by increasing cost) looking for a template whose
;;; restrictions are satisfied and that has our policy.
;;;
;;; If we find a template that doesn't have our policy, but has a legal
;;; alternate policy, then we also record that to return as a last resort. If
;;; our policy is safe, then only safe policies are O.K., otherwise anything
;;; goes.
;;;
;;; If we find a template with :SAFE policy, then we return it, or any cheaper
;;; fallback template. The theory behind this is that if it is cheapest, small
;;; and safe, we can't lose. If it is not cheapest, then we use the fallback,
;;; which won't have the desired policy, but :SAFE isn't desired either, so we
;;; might as well go with the cheaper one. The main reason for doing this is
;;; to make sure that cheap safe templates are used when they apply and the
;;; current policy is something else. This is useful because :SAFE has the
;;; additional semantics of implicit argument type checking, so we may be
;;; forced to define a template with :SAFE policy when it is really small and
;;; fast as well.
(defun find-template-for-policy (call policy)
  (declare (type combination call)
	   (type policies policy))
  (let ((safe-p (policy-safe-p policy))
	(current (function-info-templates (basic-combination-kind call)))
	(fallback nil)
	(rejected nil))
    (loop
     (multiple-value-bind (template this-reject more)
	 (find-template current call safe-p)
       (unless rejected
	 (setq rejected this-reject))
       (setq current more)
       (unless template
	 (return (values fallback rejected)))

       (let ((tpolicy (template-policy template)))
	 (cond ((eq tpolicy policy)
		(return (values template rejected)))
	       ((eq tpolicy :safe)
		(return (values (or fallback template) rejected)))
	       ((or (not safe-p) (eq tpolicy :fast-safe))
		(unless fallback
		  (setq fallback template)))))))))

(defvar *efficiency-note-limit* 2
  #!+sb-doc
  "This is the maximum number of possible optimization alternatives will be
  mentioned in a particular efficiency note. NIL means no limit.")
(declaim (type (or index null) *efficiency-note-limit*))

(defvar *efficiency-note-cost-threshold* 5
  #!+sb-doc
  "This is the minumum cost difference between the chosen implementation and
  the next alternative that justifies an efficiency note.")
(declaim (type index *efficiency-note-cost-threshold*))

;;;    This function is called by NOTE-REJECTED-TEMPLATES when it can't figure
;;; out any reason why Template was rejected. Users should never see these
;;; messages, but they can happen in situations where the VM definition is
;;; messed up somehow.
(defun strange-template-failure (template call policy frob)
  (declare (type template template) (type combination call)
	   (type policies policy) (type function frob))
  (funcall frob "This shouldn't happen!  Bug?")
  (multiple-value-bind (win why)
      (is-ok-template-use template call (policy-safe-p policy))
    (assert (not win))
    (ecase why
      (:guard
       (funcall frob "template guard failed"))
      (:arg-check
       (funcall frob "The template isn't safe, yet we were counting on it."))
      (:arg-types
       (funcall frob "argument types invalid")
       (funcall frob "argument primitive types:~%  ~S"
		(mapcar #'(lambda (x)
			    (primitive-type-name
			     (continuation-ptype x)))
			(combination-args call)))
       (funcall frob "argument type assertions:~%  ~S"
		(mapcar #'(lambda (x)
			    (if (atom x)
				x
				(ecase (car x)
				  (:or `(:or .,(mapcar #'primitive-type-name
						       (cdr x))))
				  (:constant `(:constant ,(third x))))))
			(template-arg-types template))))
      (:conditional
       (funcall frob "conditional in a non-conditional context"))
      (:result-types
       (funcall frob "result types invalid")))))

;;; This function emits efficiency notes describing all of the templates
;;; better (faster) than Template that we might have been able to use if there
;;; were better type declarations. Template is null when we didn't find any
;;; template, and thus must do a full call.
;;;
;;; In order to be worth complaining about, a template must:
;;; -- be allowed by its guard,
;;; -- be safe if the current policy is safe,
;;; -- have argument/result type restrictions consistent with the known type
;;;    information, e.g. we don't consider float templates when an operand is
;;;    known to be an integer,
;;; -- be disallowed by the stricter operand subtype test (which resembles, but
;;;    is not identical to the test done by Find-Template.)
;;;
;;; Note that there may not be any possibly applicable templates, since we are
;;; called whenever any template is rejected. That template might have the
;;; wrong policy or be inconsistent with the known type.
;;;
;;; We go to some trouble to make the whole multi-line output into a single
;;; call to Compiler-Note so that repeat messages are suppressed, etc.
(defun note-rejected-templates (call policy template)
  (declare (type combination call) (type policies policy)
	   (type (or template null) template))

  (collect ((losers))
    (let ((safe-p (policy-safe-p policy))
	  (verbose-p (policy call (= inhibit-warnings 0)))
	  (max-cost (- (template-cost
			(or template
			    (template-or-lose 'call-named)))
		       *efficiency-note-cost-threshold*)))
      (dolist (try (function-info-templates (basic-combination-kind call)))
	(when (> (template-cost try) max-cost) (return))
	(let ((guard (template-guard try)))
	  (when (and (or (not guard) (funcall guard))
		     (or (not safe-p)
			 (policy-safe-p (template-policy try)))
		     (or verbose-p
			 (and (template-note try)
			      (valid-function-use
			       call (template-type try)
			       :argument-test #'types-intersect
			       :result-test #'values-types-intersect))))
	    (losers try)))))

    (when (losers)
      (collect ((messages)
		(count 0 +))
	(flet ((frob (string &rest stuff)
		 (messages string)
		 (messages stuff)))
	  (dolist (loser (losers))
	    (when (and *efficiency-note-limit*
		       (>= (count) *efficiency-note-limit*))
	      (frob "etc.")
	      (return))
	    (let* ((type (template-type loser))
		   (valid (valid-function-use call type))
		   (strict-valid (valid-function-use call type
						     :strict-result t)))
	      (frob "unable to do ~A (cost ~D) because:"
		    (or (template-note loser) (template-name loser))
		    (template-cost loser))
	      (cond
	       ((and valid strict-valid)
		(strange-template-failure loser call policy #'frob))
	       ((not valid)
		(assert (not (valid-function-use call type
						 :error-function #'frob
						 :warning-function #'frob))))
	       (t
		(assert (policy-safe-p policy))
		(frob "can't trust output type assertion under safe policy")))
	      (count 1))))

	(let ((*compiler-error-context* call))
	  (compiler-note "~{~?~^~&~6T~}"
			 (if template
			     `("forced to do ~A (cost ~D)"
			       (,(or (template-note template)
				     (template-name template))
				,(template-cost template))
			       . ,(messages))
			     `("forced to do full call"
			       nil
			       . ,(messages))))))))
  (values))

;;; Flush type checks according to policy. If the policy is
;;; unsafe, then we never do any checks. If our policy is safe, and
;;; we are using a safe template, then we can also flush arg and
;;; result type checks. Result type checks are only flushed when the
;;; continuation as a single use. Result type checks are not flush if
;;; the policy is safe because the selection of template for results
;;; readers assumes the type check is done (uses the derived type
;;; which is the intersection of the proven and asserted types).
(defun flush-type-checks-according-to-policy (call policy template)
  (declare (type combination call) (type policies policy)
	   (type template template))
  (let ((safe-op (eq (template-policy template) :safe)))
    (when (or (not (policy-safe-p policy)) safe-op)
      (dolist (arg (basic-combination-args call))
	(flush-type-check arg)))
    (when safe-op
      (let ((cont (node-cont call)))
	(when (and (eq (continuation-use cont) call)
		   (not (policy-safe-p policy)))
	  (flush-type-check cont)))))

  (values))

;;; If a function has a special-case annotation method use that, otherwise
;;; annotate the argument continuations and try to find a template
;;; corresponding to the type signature. If there is none, convert a full call.
(defun ltn-analyze-known-call (call policy)
  (declare (type combination call)
	   (type policies policy))
  (let ((method (function-info-ltn-annotate (basic-combination-kind call)))
	(args (basic-combination-args call)))
    (when method
      (funcall method call policy)
      (return-from ltn-analyze-known-call (values)))

    (dolist (arg args)
      (setf (continuation-info arg)
	    (make-ir2-continuation (primitive-type (continuation-type arg)))))

    (multiple-value-bind (template rejected)
	(find-template-for-policy call policy)
      ;; If we are unable to use some templates due to unsatisfied operand type
      ;; restrictions and our policy enables efficiency notes, then we call
      ;; Note-Rejected-Templates.
      (when (and rejected
		 (policy call (> speed inhibit-warnings)))
	(note-rejected-templates call policy template))
      ;; If we are forced to do a full call, we check to see whether the
      ;; function called is the same as the current function. If so, we
      ;; give a warning, as this is probably a botched interpreter stub.
      (unless template
	(when (and (eq (continuation-function-name (combination-fun call))
		       (leaf-name
			(environment-function
			 (node-environment call))))
		   (let ((info (basic-combination-kind call)))
		     (not (or (function-info-ir2-convert info)
			      (ir1-attributep (function-info-attributes info)
					      recursive)))))
	  (let ((*compiler-error-context* call))
	    (compiler-warning "recursive known function definition")))
	(ltn-default-call call policy)
	(return-from ltn-analyze-known-call (values)))
      (setf (basic-combination-info call) template)
      (setf (node-tail-p call) nil)

      (flush-type-checks-according-to-policy call policy template)

      (dolist (arg args)
	(annotate-1-value-continuation arg))))

  (values))

;;;; interfaces

;;;    We make the main per-block code in for LTN into a macro so that it can
;;; be shared between LTN-Analyze and LTN-Analyze-Block, yet can cache policy
;;; across blocks in the normal (full component) case.
;;;
;;;    This code computes the policy and then dispatches to the appropriate
;;; node-specific function.
;;;
;;; Note: we deliberately don't use the DO-NODES macro, since the block can be
;;; split out from underneath us, and DO-NODES would scan past the block end in that
;;; case.
(macrolet ((frob ()
	     '(do* ((node (continuation-next (block-start block))
			  (continuation-next cont))
		    (cont (node-cont node) (node-cont node))
		    ;; KLUDGE: Since LEXENV and POLICY seem to be only used
		    ;; inside this FROB, why not define them in here instead of
		    ;; requiring them to be defined externally both in
		    ;; LTN-ANALYZE and LTN-ANALYZE-BLOCK? Or perhaps just
		    ;; define this whole FROB as an inline function? (Right now
		    ;; I don't want to make even a small unnecessary change
		    ;; like this, but'd prefer to wait until the system runs so
		    ;; that I can test it immediately after the change.)
		    ;; -- WHN 19990808
		    )
		  (())
		(unless (eq (node-lexenv node) lexenv)
		  (setq policy (translation-policy node))
		  (setq lexenv (node-lexenv node)))

		(etypecase node
		  (ref)
		  (combination
		   (case (basic-combination-kind node)
		     (:local (ltn-analyze-local-call node policy))
		     ((:full :error) (ltn-default-call node policy))
		     (t
		      (ltn-analyze-known-call node policy))))
		  (cif
		   (ltn-analyze-if node policy))
		  (creturn
		   (ltn-analyze-return node policy))
		  ((or bind entry))
		  (exit
		   (ltn-analyze-exit node policy))
		  (cset (ltn-analyze-set node policy))
		  (mv-combination
		   (ecase (basic-combination-kind node)
		     (:local (ltn-analyze-mv-bind node policy))
		     ((:full :error) (ltn-analyze-mv-call node policy)))))

		(when (eq node (block-last block))
		  (return)))))

;;; Loop over the blocks in Component, doing stuff to nodes that receive
;;; values. In addition to the stuff done by FROB, we also see whether there
;;; are any unknown values receivers, making notations in the components
;;; Generators and Receivers as appropriate.
;;;
;;; If any unknown-values continations are received by this block (as
;;; indicated by IR2-Block-Popped, then we add the block to the
;;; IR2-Component-Values-Receivers.
;;;
;;; This is where we allocate IR2 blocks because it is the first place we
;;; need them.
(defun ltn-analyze (component)
  (declare (type component component))
  (let ((2comp (component-info component))
	(lexenv nil)
	policy)
    (do-blocks (block component)
      (assert (not (block-info block)))
      (let ((2block (make-ir2-block block)))
	(setf (block-info block) 2block)
	(frob)
	(let ((popped (ir2-block-popped 2block)))
	  (when popped
	    (push block (ir2-component-values-receivers 2comp)))))))
  (values))

;;; This function is used to analyze blocks that must be added to the flow
;;; graph after the normal LTN phase runs. Such code is constrained not to
;;; use weird unknown values (and probably in lots of other ways).
(defun ltn-analyze-block (block)
  (declare (type cblock block))
  (let ((lexenv nil)
	policy)
    (frob))
  (assert (not (ir2-block-popped (block-info block))))
  (values))

) ; MACROLET FROB
