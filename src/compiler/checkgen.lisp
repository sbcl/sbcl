;;;; This file implements type check generation. This is a phase that
;;;; runs at the very end of IR1. If a type check is too complex for
;;;; the back end to directly emit in-line, then we transform the check
;;;; into an explicit conditional using TYPEP.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;;; cost estimation

;;; Return some sort of guess about the cost of a call to a function.
;;; If the function has some templates, we return the cost of the
;;; cheapest one, otherwise we return the cost of CALL-NAMED. Calling
;;; this with functions that have transforms can result in relatively
;;; meaningless results (exaggerated costs.)
;;;
;;; We special-case NULL, since it does have a source tranform and is
;;; interesting to us.
(defun function-cost (name)
  (declare (symbol name))
  (let ((info (info :function :info name))
	(call-cost (template-cost (template-or-lose 'call-named))))
    (if info
	(let ((templates (function-info-templates info)))
	  (if templates
	      (template-cost (first templates))
	      (case name
		(null (template-cost (template-or-lose 'if-eq)))
		(t call-cost))))
	call-cost)))

;;; Return some sort of guess for the cost of doing a test against
;;; TYPE. The result need not be precise as long as it isn't way out
;;; in space. The units are based on the costs specified for various
;;; templates in the VM definition.
(defun type-test-cost (type)
  (declare (type ctype type))
  (or (let ((check (type-check-template type)))
	(if check
	    (template-cost check)
	    (let ((found (cdr (assoc type *backend-type-predicates*
				     :test #'type=))))
	      (if found
		  (+ (function-cost found) (function-cost 'eq))
		  nil))))
      (typecase type
	(compound-type
	 (reduce #'+ (compound-type-types type) :key 'type-test-cost))
	(member-type
	 (* (length (member-type-members type))
	    (function-cost 'eq)))
	(numeric-type
	 (* (if (numeric-type-complexp type) 2 1)
	    (function-cost
	     (if (csubtypep type (specifier-type 'fixnum)) 'fixnump 'numberp))
	    (+ 1
	       (if (numeric-type-low type) 1 0)
	       (if (numeric-type-high type) 1 0))))
	(cons-type
	 (+ (type-test-cost (specifier-type 'cons))
	    (function-cost 'car)
	    (type-test-cost (cons-type-car-type type))
	    (function-cost 'cdr)
	    (type-test-cost (cons-type-cdr-type type))))
	(t
	 (function-cost 'typep)))))

;;;; checking strategy determination

;;; Return the type we should test for when we really want to check
;;; for TYPE. If speed, space or compilation speed is more important
;;; than safety, then we return a weaker type if it is easier to
;;; check. First we try the defined type weakenings, then look for any
;;; predicate that is cheaper.
;;;
;;; If the supertype is equal in cost to the type, we prefer the
;;; supertype. This produces a closer approximation of the right thing
;;; in the presence of poor cost info.
(defun maybe-weaken-check (type cont)
  (declare (type ctype type) (type continuation cont))
  (cond ((policy (continuation-dest cont)
		 (and (<= speed safety)
		      (<= space safety)
		      (<= compilation-speed safety)))
	 type)
	(t
	 (let ((min-cost (type-test-cost type))
	       (min-type type)
	       (found-super nil))
	   (dolist (x *backend-type-predicates*)
	     (let ((stype (car x)))
	       (when (and (csubtypep type stype)
			  (not (union-type-p stype)))
		 (let ((stype-cost (type-test-cost stype)))
		   (when (or (< stype-cost min-cost)
			     (type= stype type))
		     (setq found-super t
			   min-type stype
			   min-cost stype-cost))))))
	   (if found-super
	       min-type
	       *universal-type*)))))

;;; Like VALUES-TYPES, only mash any complex function types to FUNCTION.
(defun no-function-values-types (type)
  (declare (type ctype type))
  (multiple-value-bind (res count) (values-types type)
    (values (mapcar (lambda (type)
		      (if (fun-type-p type)
			  (specifier-type 'function)
			  type))
		    res)
	    count)))

;;; Switch to disable check complementing, for evaluation.
(defvar *complement-type-checks* t)

;;; CONT is a continuation we are doing a type check on and TYPES is a
;;; list of types that we are checking its values against. If we have
;;; proven that CONT generates a fixed number of values, then for each
;;; value, we check whether it is cheaper to then difference between
;;; the proven type and the corresponding type in TYPES. If so, we opt
;;; for a :HAIRY check with that test negated. Otherwise, we try to do
;;; a simple test, and if that is impossible, we do a hairy test with
;;; non-negated types. If true, FORCE-HAIRY forces a hairy type check.
;;;
;;; When doing a non-negated check, we call MAYBE-WEAKEN-CHECK to
;;; weaken the test to a convenient supertype (conditional on policy.)
;;; If SPEED is 3, or DEBUG-INFO is not particularly important (DEBUG
;;; <= 1), then we allow weakened checks to be simple, resulting in
;;; less informative error messages, but saving space and possibly
;;; time.
;;;
;;; FIXME: I don't quite understand this, but it looks as though
;;; that means type checks are weakened when SPEED=3 regardless of
;;; the SAFETY level, which is not the right thing to do.
(defun maybe-negate-check (cont types force-hairy)
  (declare (type continuation cont) (list types))
  (multiple-value-bind (ptypes count)
      (no-function-values-types (continuation-proven-type cont))
    (if (eq count :unknown)
	(if (and (every #'type-check-template types) (not force-hairy))
	    (values :simple types)
	    (values :hairy
		    (mapcar (lambda (x)
			      (list nil (maybe-weaken-check x cont) x))
			    types)))
	(let ((res (mapcar (lambda (p c)
			     (let ((diff (type-difference p c))
				   (weak (maybe-weaken-check c cont)))
			       (if (and diff
					(< (type-test-cost diff)
					   (type-test-cost weak))
					*complement-type-checks*)
				   (list t diff c)
				   (list nil weak c))))
			   ptypes types)))
	  (cond ((or force-hairy (find-if #'first res))
		 (values :hairy res))
		((every #'type-check-template types)
		 (values :simple types))
		((policy (continuation-dest cont)
			 (or (<= debug 1) (and (= speed 3) (/= debug 3))))
		 (let ((weakened (mapcar #'second res)))
		   (if (every #'type-check-template weakened)
		       (values :simple weakened)
		       (values :hairy res))))
		(t
		 (values :hairy res)))))))

;;; Determines whether CONT's assertion is:
;;;  -- checkable by the back end (:SIMPLE), or
;;;  -- not checkable by the back end, but checkable via an explicit 
;;;     test in type check conversion (:HAIRY), or
;;;  -- not reasonably checkable at all (:TOO-HAIRY).
;;;
;;; A type is checkable if it either represents a fixed number of
;;; values (as determined by VALUES-TYPES), or it is the assertion for
;;; an MV-Bind. A type is simply checkable if all the type assertions
;;; have a TYPE-CHECK-TEMPLATE. In this :SIMPLE case, the second value
;;; is a list of the type restrictions specified for the leading
;;; positional values.
;;;
;;; We force a check to be hairy even when there are fixed values if
;;; we are in a context where we may be forced to use the unknown
;;; values convention anyway. This is because IR2tran can't generate
;;; type checks for unknown values continuations but people could
;;; still be depending on the check being done. We only care about
;;; EXIT and RETURN (not MV-COMBINATION) since these are the only
;;; contexts where the ultimate values receiver
;;;
;;; In the :HAIRY case, the second value is a list of triples of
;;; the form:
;;;    (NOT-P TYPE ORIGINAL-TYPE)
;;;
;;; If true, the NOT-P flag indicates a test that the corresponding
;;; value is *not* of the specified TYPE. ORIGINAL-TYPE is the type
;;; asserted on this value in the continuation, for use in error
;;; messages. When NOT-P is true, this will be different from TYPE.
;;;
;;; This allows us to take what has been proven about CONT's type into
;;; consideration. If it is cheaper to test for the difference between
;;; the derived type and the asserted type, then we check for the
;;; negation of this type instead.
(defun continuation-check-types (cont)
  (declare (type continuation cont))
  (let ((type (continuation-asserted-type cont))
	(dest (continuation-dest cont)))
    (aver (not (eq type *wild-type*)))
    (multiple-value-bind (types count) (no-function-values-types type)
      (cond ((not (eq count :unknown))
	     (if (or (exit-p dest)
		     (and (return-p dest)
			  (multiple-value-bind (ignore count)
			      (values-types (return-result-type dest))
			    (declare (ignore ignore))
			    (eq count :unknown))))
		 (maybe-negate-check cont types t)
		 (maybe-negate-check cont types nil)))
	    ((and (mv-combination-p dest)
		  (eq (basic-combination-kind dest) :local))
	     (aver (values-type-p type))
	     (maybe-negate-check cont (args-type-optional type) nil))
	    (t
	     (values :too-hairy nil))))))

;;; Return true if CONT is a continuation whose type the back end is
;;; likely to want to check. Since we don't know what template the
;;; back end is going to choose to implement the continuation's DEST,
;;; we use a heuristic. We always return T unless:
;;;  -- nobody uses the value, or
;;;  -- safety is totally unimportant, or
;;;  -- the continuation is an argument to an unknown function, or
;;;  -- the continuation is an argument to a known function that has 
;;;     no IR2-Convert method or :FAST-SAFE templates that are
;;;     compatible with the call's type.
;;;
;;; We must only return NIL when it is *certain* that a check will not
;;; be done, since if we pass up this chance to do the check, it will
;;; be too late. The penalty for being too conservative is duplicated
;;; type checks. The penalty for erring by being too speculative is
;;; much nastier, e.g. falling through without ever being able to find
;;; an appropriate VOP.
;;;
;;; If there is a compile-time type error, then we always return true
;;; unless the DEST is a full call. With a full call, the theory is
;;; that the type error is probably from a declaration in (or on) the
;;; callee, so the callee should be able to do the check. We want to
;;; let the callee do the check, because it is possible that the error
;;; is really in the callee, not the caller. We don't want to make
;;; people recompile all calls to a function when they were originally
;;; compiled with a bad declaration (or an old type assertion derived
;;; from a definition appearing after the call.)
(defun probable-type-check-p (cont)
  (declare (type continuation cont))
  (let ((dest (continuation-dest cont)))
    (cond ((eq (continuation-type-check cont) :error)
	   (if (and (combination-p dest)
		    (eq (combination-kind dest) :error))
	       nil
	       t))
	  ((or (not dest)
	       (policy dest (zerop safety)))
	   nil)
	  ((basic-combination-p dest)
	   (let ((kind (basic-combination-kind dest)))
	     (cond ((eq cont (basic-combination-fun dest)) t)
		   ((eq kind :local) t)
		   ((member kind '(:full :error)) nil)
		   ((function-info-ir2-convert kind) t)
		   (t
		    (dolist (template (function-info-templates kind) nil)
		      (when (eq (template-ltn-policy template) :fast-safe)
			(multiple-value-bind (val win)
			    (valid-function-use dest (template-type template))
			  (when (or val (not win)) (return t)))))))))
	  (t t))))

;;; Return a form that we can convert to do a hairy type check of the
;;; specified TYPES. TYPES is a list of the format returned by
;;; CONTINUATION-CHECK-TYPES in the :HAIRY case. In place of the
;;; actual value(s) we are to check, we use 'DUMMY. This constant
;;; reference is later replaced with the actual values continuation.
;;;
;;; Note that we don't attempt to check for required values being
;;; unsupplied. Such checking is impossible to efficiently do at the
;;; source level because our fixed-values conventions are optimized
;;; for the common MV-BIND case.
;;;
;;; We can always use MULTIPLE-VALUE-BIND, since the macro is clever
;;; about binding a single variable.
(defun make-type-check-form (types)
  (let ((temps (make-gensym-list (length types))))
    `(multiple-value-bind ,temps 'dummy
       ,@(mapcar (lambda (temp type)
		   (let* ((spec
			   (let ((*unparse-fun-type-simplify* t))
			     (type-specifier (second type))))
			  (test (if (first type) `(not ,spec) spec)))
		     `(unless (typep ,temp ',test)
			(%type-check-error
			 ,temp
			 ',(type-specifier (third type))))))
		 temps
		 types)
       (values ,@temps))))

;;; Splice in explicit type check code immediately before the node
;;; which is CONT's DEST. This code receives the value(s) that were
;;; being passed to CONT, checks the type(s) of the value(s), then
;;; passes them on to CONT.
(defun convert-type-check (cont types)
  (declare (type continuation cont) (type list types))
  (with-ir1-environment-from-node (continuation-dest cont)

    ;; Ensuring that CONT starts a block lets us freely manipulate its uses.
    (ensure-block-start cont)

    ;; Make a new continuation and move CONT's uses to it.
    (let* ((new-start (make-continuation))
	   (dest (continuation-dest cont))
	   (prev (node-prev dest)))
      (continuation-starts-block new-start)
      (substitute-continuation-uses new-start cont)

      ;; Setting TYPE-CHECK in CONT to :DELETED indicates that the
      ;; check has been done.
      (setf (continuation-%type-check cont) :deleted)

      ;; Make the DEST node start its block so that we can splice in
      ;; the type check code.
      (when (continuation-use prev)
	(node-ends-block (continuation-use prev)))

      (let* ((prev-block (continuation-block prev))
	     (new-block (continuation-block new-start))
	     (dummy (make-continuation)))

	;; Splice in the new block before DEST, giving the new block
	;; all of DEST's predecessors.
	(dolist (block (block-pred prev-block))
	  (change-block-successor block prev-block new-block))

	;; Convert the check form, using the new block start as START
	;; and a dummy continuation as CONT.
	(ir1-convert new-start dummy (make-type-check-form types))

	;; TO DO: Why should this be true? -- WHN 19990601
	(aver (eq (continuation-block dummy) new-block))

	;; KLUDGE: Comments at the head of this function in CMU CL
	;; said that somewhere in here we
	;;   Set the new block's start and end cleanups to the *start*
	;;   cleanup of PREV's block. This overrides the incorrect
	;;   default from WITH-BELATED-IR1-ENVIRONMENT.
	;; Unfortunately I can't find any code which corresponds to this.
	;; Perhaps it was a stale comment? Or perhaps I just don't
	;; understand.. -- WHN 19990521

       	(let ((node (continuation-use dummy)))
	  (setf (block-last new-block) node)
	  ;; Change the use to a use of CONT. (We need to use the
	  ;; dummy continuation to get the control transfer right,
	  ;; because we want to go to PREV's block, not CONT's.)
	  (delete-continuation-use node)
	  (add-continuation-use node cont))
	;; Link the new block to PREV's block.
	(link-blocks new-block prev-block))

      ;; MAKE-TYPE-CHECK-FORM generated a form which checked the type
      ;; of 'DUMMY, not a real form. At this point we convert to the
      ;; real form by finding 'DUMMY and overwriting it with the new
      ;; continuation. (We can find 'DUMMY because no LET conversion
      ;; has been done yet.) The [mv-]combination code from the
      ;; mv-bind in the check form will be the use of the new check
      ;; continuation. We substitute for the first argument of this
      ;; node.
      (let* ((node (continuation-use cont))
	     (args (basic-combination-args node))
	     (victim (first args)))
	(aver (and (= (length args) 1)
		     (eq (constant-value
			  (ref-leaf
			   (continuation-use victim)))
			 'dummy)))
	(substitute-continuation new-start victim)))

    ;; Invoking local call analysis converts this call to a LET.
    (locall-analyze-component *current-component*))

  (values))

;;; Emit a type warning for NODE. If the value of NODE is being used
;;; for a variable binding, we figure out which one for source
;;; context. If the value is a constant, we print it specially. We
;;; ignore nodes whose type is NIL, since they are supposed to never
;;; return.
(defun do-type-warning (node)
  (declare (type node node))
  (let* ((*compiler-error-context* node)
	 (cont (node-cont node))
	 (atype-spec (type-specifier (continuation-asserted-type cont)))
	 (dtype (node-derived-type node))
	 (dest (continuation-dest cont))
	 (what (when (and (combination-p dest)
			  (eq (combination-kind dest) :local))
		 (let ((lambda (combination-lambda dest))
		       (pos (position-or-lose cont (combination-args dest))))
		   (format nil "~:[A possible~;The~] binding of ~S"
			   (and (continuation-use cont)
				(eq (functional-kind lambda) :let))
			   (leaf-source-name (elt (lambda-vars lambda)
						  pos)))))))
    (cond ((eq dtype *empty-type*))
	  ((and (ref-p node) (constant-p (ref-leaf node)))
	   (compiler-warning "~:[This~;~:*~A~] is not a ~<~%~9T~:;~S:~>~%  ~S"
			     what atype-spec (constant-value (ref-leaf node))))
	  (t
	   (compiler-warning
	    "~:[Result~;~:*~A~] is a ~S, ~<~%~9T~:;not a ~S.~>"
	    what (type-specifier dtype) atype-spec))))
  (values))

;;; Mark CONT as being a continuation with a manifest type error. We
;;; set the kind to :ERROR, and clear any FUNCTION-INFO if the
;;; continuation is an argument to a known call. The last is done so
;;; that the back end doesn't have to worry about type errors in
;;; arguments to known functions. This clearing is inhibited for
;;; things with IR2-CONVERT methods, since we can't do a full call to
;;; funny functions.
(defun mark-error-continuation (cont)
  (declare (type continuation cont))
  (setf (continuation-%type-check cont) :error)
  (let ((dest (continuation-dest cont)))
    (when (and (combination-p dest)
	       (let ((kind (basic-combination-kind dest)))
		 (or (eq kind :full)
		     (and (function-info-p kind)
			  (not (function-info-ir2-convert kind))))))
      (setf (basic-combination-kind dest) :error)))
  (values))

;;; Loop over all blocks in COMPONENT that have TYPE-CHECK set,
;;; looking for continuations with TYPE-CHECK T. We do two mostly
;;; unrelated things: detect compile-time type errors and determine if
;;; and how to do run-time type checks.
;;;
;;; If there is a compile-time type error, then we mark the
;;; continuation and emit a warning if appropriate. This part loops
;;; over all the uses of the continuation, since after we convert the
;;; check, the :DELETED kind will inhibit warnings about the types of
;;; other uses.
;;;
;;; If a continuation is too complex to be checked by the back end, or
;;; is better checked with explicit code, then convert to an explicit
;;; test. Assertions that can checked by the back end are passed
;;; through. Assertions that can't be tested are flamed about and
;;; marked as not needing to be checked.
;;;
;;; If we determine that a type check won't be done, then we set
;;; TYPE-CHECK to :NO-CHECK. In the non-hairy cases, this is just to
;;; prevent us from wasting time coming to the same conclusion again
;;; on a later iteration. In the hairy case, we must indicate to LTN
;;; that it must choose a safe implementation, since IR2 conversion
;;; will choke on the check.
;;;
;;; The generation of the type checks is delayed until all the type
;;; check decisions have been made because the generation of the type
;;; checks creates new nodes whose derived types aren't always updated
;;; which may lead to inappropriate template choices due to the
;;; modification of argument types.
(defun generate-type-checks (component)
  (collect ((conts))
    (do-blocks (block component)
      (when (block-type-check block)
	(do-nodes (node cont block)
	  (let ((type-check (continuation-type-check cont)))
	    (unless (member type-check '(nil :error :deleted))
	      (let ((atype (continuation-asserted-type cont)))
		(do-uses (use cont)
		  (unless (values-types-equal-or-intersect
			   (node-derived-type use) atype)
		    (mark-error-continuation cont)
		    (unless (policy node (= inhibit-warnings 3))
		      (do-type-warning use))))))
	    (when (eq type-check t)
	      (cond ((probable-type-check-p cont)
		     (conts cont))
		    (t
		     (setf (continuation-%type-check cont) :no-check))))))
	(setf (block-type-check block) nil)))
    (dolist (cont (conts))
      (multiple-value-bind (check types) (continuation-check-types cont)
	(ecase check
	  (:simple)
	  (:hairy
	   (convert-type-check cont types))
	  (:too-hairy
	   (let* ((context (continuation-dest cont))
		  (*compiler-error-context* context))
	     (when (policy context (>= safety inhibit-warnings))
	       (compiler-note
		"type assertion too complex to check:~% ~S."
		(type-specifier (continuation-asserted-type cont)))))
	   (setf (continuation-%type-check cont) :deleted))))))
  (values))
