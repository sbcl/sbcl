;;;; This file contains the virtual-machine-independent parts of the
;;;; code which does the actual translation of nodes to VOPs.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;;; moves and type checks

;;; Move X to Y unless they are EQ.
(defun emit-move (node block x y)
  (declare (type node node) (type ir2-block block) (type tn x y))
  (unless (eq x y)
    (vop move node block x y))
  (values))

;;; If there is any CHECK-xxx template for TYPE, then return it,
;;; otherwise return NIL.
(defun type-check-template (type)
  (declare (type ctype type))
  (multiple-value-bind (check-ptype exact) (primitive-type type)
    (if exact
	(primitive-type-check check-ptype)
	(let ((name (hairy-type-check-template-name type)))
	  (if name
	      (template-or-lose name)
	      nil)))))

;;; Emit code in BLOCK to check that VALUE is of the specified TYPE,
;;; yielding the checked result in RESULT. VALUE and result may be of
;;; any primitive type. There must be CHECK-xxx VOP for TYPE. Any
;;; other type checks should have been converted to an explicit type
;;; test.
(defun emit-type-check (node block value result type)
  (declare (type tn value result) (type node node) (type ir2-block block)
	   (type ctype type))
  (emit-move-template node block (type-check-template type) value result)
  (values))

;;; Allocate an indirect value cell. Maybe do some clever stack
;;; allocation someday.
;;;
;;; FIXME: DO-MAKE-VALUE-CELL is a bad name, since it doesn't make
;;; clear what's the distinction between it and the MAKE-VALUE-CELL
;;; VOP, and since the DO- further connotes iteration, which has
;;; nothing to do with this. Clearer, more systematic names, anyone?
(defevent make-value-cell-event "Allocate heap value cell for lexical var.")
(defun do-make-value-cell (node block value res)
  (event make-value-cell-event node)
  (vop make-value-cell node block value res))

;;;; leaf reference

;;; Return the TN that holds the value of THING in the environment ENV.
(declaim (ftype (function ((or nlx-info lambda-var) physenv) tn)
		find-in-physenv))
(defun find-in-physenv (thing physenv)
  (or (cdr (assoc thing (ir2-physenv-closure (physenv-info physenv))))
      (etypecase thing
	(lambda-var
	 ;; I think that a failure of this assertion means that we're
	 ;; trying to access a variable which was improperly closed
	 ;; over. The PHYSENV describes a physical environment. Every
	 ;; variable that a form refers to should either be in its
	 ;; physical environment directly, or grabbed from a
	 ;; surrounding physical environment when it was closed over.
	 ;; The ASSOC expression above finds closed-over variables, so
	 ;; if we fell through the ASSOC expression, it wasn't closed
	 ;; over. Therefore, it must be in our physical environment
	 ;; directly. If instead it is in some other physical
	 ;; environment, then it's bogus for us to reference it here
	 ;; without it being closed over. -- WHN 2001-09-29
	 (aver (eq physenv (lambda-physenv (lambda-var-home thing))))
	 (leaf-info thing))
	(nlx-info
	 (aver (eq physenv (block-physenv (nlx-info-target thing))))
	 (ir2-nlx-info-home (nlx-info-info thing))))
      (bug "~@<~2I~_~S ~_not found in ~_~S~:>" thing physenv)))

;;; If LEAF already has a constant TN, return that, otherwise make a
;;; TN for it.
(defun constant-tn (leaf)
  (declare (type constant leaf))
  (or (leaf-info leaf)
      (setf (leaf-info leaf)
	    (make-constant-tn leaf))))

;;; Return a TN that represents the value of LEAF, or NIL if LEAF
;;; isn't directly represented by a TN. ENV is the environment that
;;; the reference is done in.
(defun leaf-tn (leaf env)
  (declare (type leaf leaf) (type physenv env))
  (typecase leaf
    (lambda-var
     (unless (lambda-var-indirect leaf)
       (find-in-physenv leaf env)))
    (constant (constant-tn leaf))
    (t nil)))

;;; This is used to conveniently get a handle on a constant TN during
;;; IR2 conversion. It returns a constant TN representing the Lisp
;;; object VALUE.
(defun emit-constant (value)
  (constant-tn (find-constant value)))

;;; Convert a REF node. The reference must not be delayed.
(defun ir2-convert-ref (node block)
  (declare (type ref node) (type ir2-block block))
  (let* ((cont (node-cont node))
	 (leaf (ref-leaf node))
	 (locs (continuation-result-tns
		cont (list (primitive-type (leaf-type leaf)))))
	 (res (first locs)))
    (etypecase leaf
      (lambda-var
       (let ((tn (find-in-physenv leaf (node-physenv node))))
	 (if (lambda-var-indirect leaf)
	     (vop value-cell-ref node block tn res)
	     (emit-move node block tn res))))
      (constant
       (if (legal-immediate-constant-p leaf)
	   (emit-move node block (constant-tn leaf) res)
	   (let* ((name (leaf-source-name leaf))
		  (name-tn (emit-constant name)))
	     (if (policy node (zerop safety))
		 (vop fast-symbol-value node block name-tn res)
		 (vop symbol-value node block name-tn res)))))
      (functional
       (ir2-convert-closure node block leaf res))
      (global-var
       (let ((unsafe (policy node (zerop safety)))
	     (name (leaf-source-name leaf)))
	 (ecase (global-var-kind leaf)
	   ((:special :global)
	    (aver (symbolp name))
	    (let ((name-tn (emit-constant name)))
	      (if unsafe
		  (vop fast-symbol-value node block name-tn res)
		  (vop symbol-value node block name-tn res))))
	   (:global-function
	    (let ((fdefn-tn (make-load-time-constant-tn :fdefinition name)))
	      (if unsafe
		  (vop fdefn-fun node block fdefn-tn res)
		  (vop safe-fdefn-fun node block fdefn-tn res))))))))
    (move-continuation-result node block locs cont))
  (values))

;;; some sanity checks for a CLAMBDA passed to IR2-CONVERT-CLOSURE
(defun assertions-on-ir2-converted-clambda (clambda)
  ;; This assertion was sort of an experiment. It would be nice and
  ;; sane and easier to understand things if it were *always* true,
  ;; but experimentally I observe that it's only *almost* always
  ;; true. -- WHN 2001-01-02
  #+nil 
  (aver (eql (lambda-component clambda)
	     (block-component (ir2-block-block ir2-block))))
  ;; Check for some weirdness which came up in bug
  ;; 138, 2002-01-02.
  ;;
  ;; The MAKE-LOAD-TIME-CONSTANT-TN call above puts an :ENTRY record
  ;; into the IR2-COMPONENT-CONSTANTS table. The dump-a-COMPONENT
  ;; code
  ;;   * treats every HANDLEless :ENTRY record into a
  ;;     patch, and
  ;;   * expects every patch to correspond to an
  ;;     IR2-COMPONENT-ENTRIES record.
  ;; The IR2-COMPONENT-ENTRIES records are set by ENTRY-ANALYZE
  ;; walking over COMPONENT-LAMBDAS. Bug 138b arose because there
  ;; was a HANDLEless :ENTRY record which didn't correspond to an
  ;; IR2-COMPONENT-ENTRIES record. That problem is hard to debug
  ;; when it's caught at dump time, so this assertion tries to catch
  ;; it here.
  (aver (member clambda
		(component-lambdas (lambda-component clambda))))
  ;; another bug-138-related issue: COMPONENT-NEW-FUNCTIONALS is
  ;; used as a queue for stuff pending to do in IR1, and now that
  ;; we're doing IR2 it should've been completely flushed (but
  ;; wasn't).
  (aver (null (component-new-functionals (lambda-component clambda))))
  (values))

;;; Emit code to load a function object implementing FUNCTIONAL into
;;; RES. This gets interesting when the referenced function is a
;;; closure: we must make the closure and move the closed-over values
;;; into it.
;;;
;;; FUNCTIONAL is either a :TOPLEVEL-XEP functional or the XEP lambda
;;; for the called function, since local call analysis converts all
;;; closure references. If a :TOPLEVEL-XEP, we know it is not a
;;; closure.
;;;
;;; If a closed-over LAMBDA-VAR has no refs (is deleted), then we
;;; don't initialize that slot. This can happen with closures over
;;; top level variables, where optimization of the closure deleted the
;;; variable. Since we committed to the closure format when we
;;; pre-analyzed the top level code, we just leave an empty slot.
(defun ir2-convert-closure (ref ir2-block functional res)
  (declare (type ref ref)
	   (type ir2-block ir2-block)
	   (type functional functional)
	   (type tn res))
  (aver (not (eql (functional-kind functional) :deleted)))
  (unless (leaf-info functional)
    (setf (leaf-info functional)
	  (make-entry-info :name (functional-debug-name functional))))
  (let ((entry (make-load-time-constant-tn :entry functional))
	(closure (etypecase functional
		   (clambda
		    (assertions-on-ir2-converted-clambda functional)
		    (physenv-closure (get-lambda-physenv functional)))
		   (functional
		    (aver (eq (functional-kind functional) :toplevel-xep))
		    nil))))

    (cond (closure
	   (let ((this-env (node-physenv ref)))
	     (vop make-closure ref ir2-block entry (length closure) res)
	     (loop for what in closure and n from 0 do
	       (unless (and (lambda-var-p what)
			    (null (leaf-refs what)))
		 (vop closure-init ref ir2-block
		      res
		      (find-in-physenv what this-env)
		      n)))))
	  (t
	   (emit-move ref ir2-block entry res))))
  (values))

;;; Convert a SET node. If the NODE's CONT is annotated, then we also
;;; deliver the value to that continuation. If the var is a lexical
;;; variable with no refs, then we don't actually set anything, since
;;; the variable has been deleted.
(defun ir2-convert-set (node block)
  (declare (type cset node) (type ir2-block block))
  (let* ((cont (node-cont node))
	 (leaf (set-var node))
	 (val (continuation-tn node block (set-value node)))
	 (locs (if (continuation-info cont)
		   (continuation-result-tns
		    cont (list (primitive-type (leaf-type leaf))))
		   nil)))
    (etypecase leaf
      (lambda-var
       (when (leaf-refs leaf)
	 (let ((tn (find-in-physenv leaf (node-physenv node))))
	   (if (lambda-var-indirect leaf)
	       (vop value-cell-set node block tn val)
	       (emit-move node block val tn)))))
      (global-var
       (ecase (global-var-kind leaf)
	 ((:special :global)
	  (aver (symbolp (leaf-source-name leaf)))
	  (vop set node block (emit-constant (leaf-source-name leaf)) val)))))
    (when locs
      (emit-move node block val (first locs))
      (move-continuation-result node block locs cont)))
  (values))

;;;; utilities for receiving fixed values

;;; Return a TN that can be referenced to get the value of CONT. CONT
;;; must be LTN-ANNOTATED either as a delayed leaf ref or as a fixed,
;;; single-value continuation. If a type check is called for, do it.
;;;
;;; The primitive-type of the result will always be the same as the
;;; IR2-CONTINUATION-PRIMITIVE-TYPE, ensuring that VOPs are always
;;; called with TNs that satisfy the operand primitive-type
;;; restriction. We may have to make a temporary of the desired type
;;; and move the actual continuation TN into it. This happens when we
;;; delete a type check in unsafe code or when we locally know
;;; something about the type of an argument variable.
(defun continuation-tn (node block cont)
  (declare (type node node) (type ir2-block block) (type continuation cont))
  (let* ((2cont (continuation-info cont))
	 (cont-tn
	  (ecase (ir2-continuation-kind 2cont)
	    (:delayed
	     (let ((ref (continuation-use cont)))
	       (leaf-tn (ref-leaf ref) (node-physenv ref))))
	    (:fixed
	     (aver (= (length (ir2-continuation-locs 2cont)) 1))
	     (first (ir2-continuation-locs 2cont)))))
	 (ptype (ir2-continuation-primitive-type 2cont)))

    (cond ((eq (tn-primitive-type cont-tn) ptype) cont-tn)
	  (t
	   (let ((temp (make-normal-tn ptype)))
	     (emit-move node block cont-tn temp)
	     temp)))))

;;; This is similar to CONTINUATION-TN, but hacks multiple values. We
;;; return continuations holding the values of CONT with PTYPES as
;;; their primitive types. CONT must be annotated for the same number
;;; of fixed values are there are PTYPES.
;;;
;;; If the continuation has a type check, check the values into temps
;;; and return the temps. When we have more values than assertions, we
;;; move the extra values with no check.
(defun continuation-tns (node block cont ptypes)
  (declare (type node node) (type ir2-block block)
	   (type continuation cont) (list ptypes))
  (let* ((locs (ir2-continuation-locs (continuation-info cont)))
	 (nlocs (length locs)))
    (aver (= nlocs (length ptypes)))

    (mapcar (lambda (from to-type)
              (if (eq (tn-primitive-type from) to-type)
                  from
                  (let ((temp (make-normal-tn to-type)))
                    (emit-move node block from temp)
                    temp)))
            locs
            ptypes)))

;;;; utilities for delivering values to continuations

;;; Return a list of TNs with the specifier TYPES that can be used as
;;; result TNs to evaluate an expression into the continuation CONT.
;;; This is used together with MOVE-CONTINUATION-RESULT to deliver
;;; fixed values to a continuation.
;;;
;;; If the continuation isn't annotated (meaning the values are
;;; discarded) or is unknown-values, the then we make temporaries for
;;; each supplied value, providing a place to compute the result in
;;; until we decide what to do with it (if anything.)
;;;
;;; If the continuation is fixed-values, and wants the same number of
;;; values as the user wants to deliver, then we just return the
;;; IR2-CONTINUATION-LOCS. Otherwise we make a new list padded as
;;; necessary by discarded TNs. We always return a TN of the specified
;;; type, using the continuation locs only when they are of the
;;; correct type.
(defun continuation-result-tns (cont types)
  (declare (type continuation cont) (type list types))
  (let ((2cont (continuation-info cont)))
    (if (not 2cont)
	(mapcar #'make-normal-tn types)
	(ecase (ir2-continuation-kind 2cont)
	  (:fixed
	   (let* ((locs (ir2-continuation-locs 2cont))
		  (nlocs (length locs))
		  (ntypes (length types)))
	     (if (and (= nlocs ntypes)
		      (do ((loc locs (cdr loc))
			   (type types (cdr type)))
			  ((null loc) t)
			(unless (eq (tn-primitive-type (car loc)) (car type))
			  (return nil))))
		 locs
		 (mapcar (lambda (loc type)
			   (if (eq (tn-primitive-type loc) type)
			       loc
			       (make-normal-tn type)))
			 (if (< nlocs ntypes)
			     (append locs
				     (mapcar #'make-normal-tn
					     (subseq types nlocs)))
			     locs)
			 types))))
	  (:unknown
	   (mapcar #'make-normal-tn types))))))

;;; Make the first N standard value TNs, returning them in a list.
(defun make-standard-value-tns (n)
  (declare (type unsigned-byte n))
  (collect ((res))
    (dotimes (i n)
      (res (standard-arg-location i)))
    (res)))

;;; Return a list of TNs wired to the standard value passing
;;; conventions that can be used to receive values according to the
;;; unknown-values convention. This is used with together
;;; MOVE-CONTINUATION-RESULT for delivering unknown values to a fixed
;;; values continuation.
;;;
;;; If the continuation isn't annotated, then we treat as 0-values,
;;; returning an empty list of temporaries.
;;;
;;; If the continuation is annotated, then it must be :FIXED.
(defun standard-result-tns (cont)
  (declare (type continuation cont))
  (let ((2cont (continuation-info cont)))
    (if 2cont
	(ecase (ir2-continuation-kind 2cont)
	  (:fixed
	   (make-standard-value-tns (length (ir2-continuation-locs 2cont)))))
	())))

;;; Just move each SRC TN into the corresponding DEST TN, defaulting
;;; any unsupplied source values to NIL. We let EMIT-MOVE worry about
;;; doing the appropriate coercions.
(defun move-results-coerced (node block src dest)
  (declare (type node node) (type ir2-block block) (list src dest))
  (let ((nsrc (length src))
	(ndest (length dest)))
    (mapc (lambda (from to)
	    (unless (eq from to)
	      (emit-move node block from to)))
	  (if (> ndest nsrc)
	      (append src (make-list (- ndest nsrc)
				     :initial-element (emit-constant nil)))
	      src)
	  dest))
  (values))

;;; Move each SRC TN into the corresponding DEST TN, checking types
;;; and defaulting any unsupplied source values to NIL
(defun move-results-checked (node block src dest types)
  (declare (type node node) (type ir2-block block) (list src dest types))
  (let ((nsrc (length src))
	(ndest (length dest))
        (ntypes (length types)))
    (mapc (lambda (from to type)
            (if type
                (emit-type-check node block from to type)
                (emit-move node block from to)))
	  (if (> ndest nsrc)
	      (append src (make-list (- ndest nsrc)
				     :initial-element (emit-constant nil)))
	      src)
	  dest
          (if (> ndest ntypes)
	      (append types (make-list (- ndest ntypes)))
	      types)))
  (values))

;;; If necessary, emit coercion code needed to deliver the RESULTS to
;;; the specified continuation. NODE and BLOCK provide context for
;;; emitting code. Although usually obtained from STANDARD-RESULT-TNs
;;; or CONTINUATION-RESULT-TNs, RESULTS my be a list of any type or
;;; number of TNs.
;;;
;;; If the continuation is fixed values, then move the results into
;;; the continuation locations. If the continuation is unknown values,
;;; then do the moves into the standard value locations, and use
;;; PUSH-VALUES to put the values on the stack.
(defun move-continuation-result (node block results cont)
  (declare (type node node) (type ir2-block block)
	   (list results) (type continuation cont))
  (let* ((2cont (continuation-info cont)))
    (when 2cont
      (ecase (ir2-continuation-kind 2cont)
	(:fixed
	 (let ((locs (ir2-continuation-locs 2cont)))
	   (unless (eq locs results)
	     (move-results-coerced node block results locs))))
	(:unknown
	 (let* ((nvals (length results))
		(locs (make-standard-value-tns nvals)))
	   (move-results-coerced node block results locs)
	   (vop* push-values node block
		 ((reference-tn-list locs nil))
		 ((reference-tn-list (ir2-continuation-locs 2cont) t))
		 nvals))))))
  (values))

;;; CAST
(defun ir2-convert-cast (node block)
  (declare (type cast node)
           (type ir2-block block))
  (let* ((cont (node-cont node))
         (2cont (continuation-info cont))
         (value (cast-value node))
         (2value (continuation-info value)))
    (cond ((not 2cont))
          ((eq (ir2-continuation-kind 2cont) :unused))
          ((and (eq (ir2-continuation-kind 2cont) :unknown)
                (eq (ir2-continuation-kind 2value) :unknown))
           (aver (not (cast-type-check node)))
           (move-results-coerced node block
                                 (ir2-continuation-locs 2value)
                                 (ir2-continuation-locs 2cont)))
          ((and (eq (ir2-continuation-kind 2cont) :fixed)
                (eq (ir2-continuation-kind 2value) :fixed))
           (if (cast-type-check node)
               (move-results-checked node block
                                     (ir2-continuation-locs 2value)
                                     (ir2-continuation-locs 2cont)
                                     (multiple-value-bind (check types)
                                         (cast-check-types node nil)
                                       (aver (eq check :simple))
                                       types))
               (move-results-coerced node block
                                     (ir2-continuation-locs 2value)
                                     (ir2-continuation-locs 2cont))))
          ;; FIXME: unknown values packing/unpacking -- APD, 2002-02-11
          (t (bug "quux")))))

;;;; template conversion

;;; Build a TN-REFS list that represents access to the values of the
;;; specified list of continuations ARGS for TEMPLATE. Any :CONSTANT
;;; arguments are returned in the second value as a list rather than
;;; being accessed as a normal argument. NODE and BLOCK provide the
;;; context for emitting any necessary type-checking code.
(defun reference-args (node block args template)
  (declare (type node node) (type ir2-block block) (list args)
	   (type template template))
  (collect ((info-args))
    (let ((last nil)
	  (first nil))
      (do ((args args (cdr args))
	   (types (template-arg-types template) (cdr types)))
	  ((null args))
	(let ((type (first types))
	      (arg (first args)))
	  (if (and (consp type) (eq (car type) ':constant))
	      (info-args (continuation-value arg))
	      (let ((ref (reference-tn (continuation-tn node block arg) nil)))
		(if last
		    (setf (tn-ref-across last) ref)
		    (setf first ref))
		(setq last ref)))))

      (values (the (or tn-ref null) first) (info-args)))))

;;; Convert a conditional template. We try to exploit any
;;; drop-through, but emit an unconditional branch afterward if we
;;; fail. NOT-P is true if the sense of the TEMPLATE's test should be
;;; negated.
(defun ir2-convert-conditional (node block template args info-args if not-p)
  (declare (type node node) (type ir2-block block)
	   (type template template) (type (or tn-ref null) args)
	   (list info-args) (type cif if) (type boolean not-p))
  (aver (= (template-info-arg-count template) (+ (length info-args) 2)))
  (let ((consequent (if-consequent if))
	(alternative (if-alternative if)))
    (cond ((drop-thru-p if consequent)
	   (emit-template node block template args nil
			  (list* (block-label alternative) (not not-p)
				 info-args)))
	  (t
	   (emit-template node block template args nil
			  (list* (block-label consequent) not-p info-args))
	   (unless (drop-thru-p if alternative)
	     (vop branch node block (block-label alternative)))))))

;;; Convert an IF that isn't the DEST of a conditional template.
(defun ir2-convert-if (node block)
  (declare (type ir2-block block) (type cif node))
  (let* ((test (if-test node))
	 (test-ref (reference-tn (continuation-tn node block test) nil))
	 (nil-ref (reference-tn (emit-constant nil) nil)))
    (setf (tn-ref-across test-ref) nil-ref)
    (ir2-convert-conditional node block (template-or-lose 'if-eq)
			     test-ref () node t)))

;;; Return a list of primitive-types that we can pass to
;;; CONTINUATION-RESULT-TNS describing the result types we want for a
;;; template call. We duplicate here the determination of output type
;;; that was done in initially selecting the template, so we know that
;;; the types we find are allowed by the template output type
;;; restrictions.
(defun find-template-result-types (call cont template rtypes)
  (declare (type combination call) (type continuation cont)
	   (type template template) (list rtypes))
  (let* ((dtype (node-derived-type call))
	 (type dtype)
	 (types (mapcar #'primitive-type
			(if (values-type-p type)
			    (append (values-type-required type)
				    (values-type-optional type))
			    (list type)))))
    (let ((nvals (length rtypes))
	  (ntypes (length types)))
      (cond ((< ntypes nvals)
	     (append types
		     (make-list (- nvals ntypes)
				:initial-element *backend-t-primitive-type*)))
	    ((> ntypes nvals)
	     (subseq types 0 nvals))
	    (t
	     types)))))

;;; Return a list of TNs usable in a CALL to TEMPLATE delivering
;;; values to CONT. As an efficiency hack, we pick off the common case
;;; where the continuation is fixed values and has locations that
;;; satisfy the result restrictions. This can fail when there is a
;;; type check or a values count mismatch.
(defun make-template-result-tns (call cont template rtypes)
  (declare (type combination call) (type continuation cont)
	   (type template template) (list rtypes))
  (let ((2cont (continuation-info cont)))
    (if (and 2cont (eq (ir2-continuation-kind 2cont) :fixed))
	(let ((locs (ir2-continuation-locs 2cont)))
	  (if (and (= (length rtypes) (length locs))
		   (do ((loc locs (cdr loc))
			(rtype rtypes (cdr rtype)))
		       ((null loc) t)
		     (unless (operand-restriction-ok
			      (car rtype)
			      (tn-primitive-type (car loc))
			      :t-ok nil)
		       (return nil))))
	      locs
	      (continuation-result-tns
	       cont
	       (find-template-result-types call cont template rtypes))))
	(continuation-result-tns
	 cont
	 (find-template-result-types call cont template rtypes)))))

;;; Get the operands into TNs, make TN-REFs for them, and then call
;;; the template emit function.
(defun ir2-convert-template (call block)
  (declare (type combination call) (type ir2-block block))
  (let* ((template (combination-info call))
	 (cont (node-cont call))
	 (rtypes (template-result-types template)))
    (multiple-value-bind (args info-args)
	(reference-args call block (combination-args call) template)
      (aver (not (template-more-results-type template)))
      (if (eq rtypes :conditional)
	  (ir2-convert-conditional call block template args info-args
				   (continuation-dest cont) nil)
	  (let* ((results (make-template-result-tns call cont template rtypes))
		 (r-refs (reference-tn-list results t)))
	    (aver (= (length info-args)
		     (template-info-arg-count template)))
	    (if info-args
		(emit-template call block template args r-refs info-args)
		(emit-template call block template args r-refs))
	    (move-continuation-result call block results cont)))))
  (values))

;;; We don't have to do much because operand count checking is done by
;;; IR1 conversion. The only difference between this and the function
;;; case of IR2-CONVERT-TEMPLATE is that there can be codegen-info
;;; arguments.
(defoptimizer (%%primitive ir2-convert) ((template info &rest args) call block)
  (let* ((template (continuation-value template))
	 (info (continuation-value info))
	 (cont (node-cont call))
	 (rtypes (template-result-types template))
	 (results (make-template-result-tns call cont template rtypes))
	 (r-refs (reference-tn-list results t)))
    (multiple-value-bind (args info-args)
	(reference-args call block (cddr (combination-args call)) template)
      (aver (not (template-more-results-type template)))
      (aver (not (eq rtypes :conditional)))
      (aver (null info-args))

      (if info
	  (emit-template call block template args r-refs info)
	  (emit-template call block template args r-refs))

      (move-continuation-result call block results cont)))
  (values))

;;;; local call

;;; Convert a LET by moving the argument values into the variables.
;;; Since a LET doesn't have any passing locations, we move the
;;; arguments directly into the variables. We must also allocate any
;;; indirect value cells, since there is no function prologue to do
;;; this.
(defun ir2-convert-let (node block fun)
  (declare (type combination node) (type ir2-block block) (type clambda fun))
  (mapc (lambda (var arg)
	  (when arg
	    (let ((src (continuation-tn node block arg))
		  (dest (leaf-info var)))
	      (if (lambda-var-indirect var)
		  (do-make-value-cell node block src dest)
		  (emit-move node block src dest)))))
	(lambda-vars fun) (basic-combination-args node))
  (values))

;;; Emit any necessary moves into assignment temps for a local call to
;;; FUN. We return two lists of TNs: TNs holding the actual argument
;;; values, and (possibly EQ) TNs that are the actual destination of
;;; the arguments. When necessary, we allocate temporaries for
;;; arguments to preserve parallel assignment semantics. These lists
;;; exclude unused arguments and include implicit environment
;;; arguments, i.e. they exactly correspond to the arguments passed.
;;;
;;; OLD-FP is the TN currently holding the value we want to pass as
;;; OLD-FP. If null, then the call is to the same environment (an
;;; :ASSIGNMENT), so we only move the arguments, and leave the
;;; environment alone.
(defun emit-psetq-moves (node block fun old-fp)
  (declare (type combination node) (type ir2-block block) (type clambda fun)
	   (type (or tn null) old-fp))
  (let ((actuals (mapcar (lambda (x)
			   (when x
			     (continuation-tn node block x)))
			 (combination-args node))))
    (collect ((temps)
	      (locs))
      (dolist (var (lambda-vars fun))
	(let ((actual (pop actuals))
	      (loc (leaf-info var)))
	  (when actual
	    (cond
	     ((lambda-var-indirect var)
	      (let ((temp
		     (make-normal-tn *backend-t-primitive-type*)))
		(do-make-value-cell node block actual temp)
		(temps temp)))
	     ((member actual (locs))
	      (let ((temp (make-normal-tn (tn-primitive-type loc))))
		(emit-move node block actual temp)
		(temps temp)))
	     (t
	      (temps actual)))
	    (locs loc))))

      (when old-fp
	(let ((this-1env (node-physenv node))
	      (called-env (physenv-info (lambda-physenv fun))))
	  (dolist (thing (ir2-physenv-closure called-env))
	    (temps (find-in-physenv (car thing) this-1env))
	    (locs (cdr thing)))
	  (temps old-fp)
	  (locs (ir2-physenv-old-fp called-env))))

      (values (temps) (locs)))))

;;; A tail-recursive local call is done by emitting moves of stuff
;;; into the appropriate passing locations. After setting up the args
;;; and environment, we just move our return-pc into the called
;;; function's passing location.
(defun ir2-convert-tail-local-call (node block fun)
  (declare (type combination node) (type ir2-block block) (type clambda fun))
  (let ((this-env (physenv-info (node-physenv node))))
    (multiple-value-bind (temps locs)
	(emit-psetq-moves node block fun (ir2-physenv-old-fp this-env))

      (mapc (lambda (temp loc)
	      (emit-move node block temp loc))
	    temps locs))

    (emit-move node block
	       (ir2-physenv-return-pc this-env)
	       (ir2-physenv-return-pc-pass
		(physenv-info
		 (lambda-physenv fun)))))

  (values))

;;; Convert an :ASSIGNMENT call. This is just like a tail local call,
;;; except that the caller and callee environment are the same, so we
;;; don't need to mess with the environment locations, return PC, etc.
(defun ir2-convert-assignment (node block fun)
  (declare (type combination node) (type ir2-block block) (type clambda fun))
    (multiple-value-bind (temps locs) (emit-psetq-moves node block fun nil)

      (mapc (lambda (temp loc)
	      (emit-move node block temp loc))
	    temps locs))
  (values))

;;; Do stuff to set up the arguments to a non-tail local call
;;; (including implicit environment args.) We allocate a frame
;;; (returning the FP and NFP), and also compute the TN-REFS list for
;;; the values to pass and the list of passing location TNs.
(defun ir2-convert-local-call-args (node block fun)
  (declare (type combination node) (type ir2-block block) (type clambda fun))
  (let ((fp (make-stack-pointer-tn))
	(nfp (make-number-stack-pointer-tn))
	(old-fp (make-stack-pointer-tn)))
    (multiple-value-bind (temps locs)
	(emit-psetq-moves node block fun old-fp)
      (vop current-fp node block old-fp)
      (vop allocate-frame node block
	   (physenv-info (lambda-physenv fun))
	   fp nfp)
      (values fp nfp temps (mapcar #'make-alias-tn locs)))))

;;; Handle a non-TR known-values local call. We emit the call, then
;;; move the results to the continuation's destination.
(defun ir2-convert-local-known-call (node block fun returns cont start)
  (declare (type node node) (type ir2-block block) (type clambda fun)
	   (type return-info returns) (type continuation cont)
	   (type label start))
  (multiple-value-bind (fp nfp temps arg-locs)
      (ir2-convert-local-call-args node block fun)
    (let ((locs (return-info-locations returns)))
      (vop* known-call-local node block
	    (fp nfp (reference-tn-list temps nil))
	    ((reference-tn-list locs t))
	    arg-locs (physenv-info (lambda-physenv fun)) start)
      (move-continuation-result node block locs cont)))
  (values))

;;; Handle a non-TR unknown-values local call. We do different things
;;; depending on what kind of values the continuation wants.
;;;
;;; If CONT is :UNKNOWN, then we use the "multiple-" variant, directly
;;; specifying the continuation's LOCS as the VOP results so that we
;;; don't have to do anything after the call.
;;;
;;; Otherwise, we use STANDARD-RESULT-TNS to get wired result TNs, and
;;; then call MOVE-CONTINUATION-RESULT to do any necessary type checks
;;; or coercions.
(defun ir2-convert-local-unknown-call (node block fun cont start)
  (declare (type node node) (type ir2-block block) (type clambda fun)
	   (type continuation cont) (type label start))
  (multiple-value-bind (fp nfp temps arg-locs)
      (ir2-convert-local-call-args node block fun)
    (let ((2cont (continuation-info cont))
	  (env (physenv-info (lambda-physenv fun)))
	  (temp-refs (reference-tn-list temps nil)))
      (if (and 2cont (eq (ir2-continuation-kind 2cont) :unknown))
	  (vop* multiple-call-local node block (fp nfp temp-refs)
		((reference-tn-list (ir2-continuation-locs 2cont) t))
		arg-locs env start)
	  (let ((locs (standard-result-tns cont)))
	    (vop* call-local node block
		  (fp nfp temp-refs)
		  ((reference-tn-list locs t))
		  arg-locs env start (length locs))
	    (move-continuation-result node block locs cont)))))
  (values))

;;; Dispatch to the appropriate function, depending on whether we have
;;; a let, tail or normal call. If the function doesn't return, call
;;; it using the unknown-value convention. We could compile it as a
;;; tail call, but that might seem confusing in the debugger.
(defun ir2-convert-local-call (node block)
  (declare (type combination node) (type ir2-block block))
  (let* ((fun (ref-leaf (continuation-use (basic-combination-fun node))))
	 (kind (functional-kind fun)))
    (cond ((eq kind :let)
	   (ir2-convert-let node block fun))
	  ((eq kind :assignment)
	   (ir2-convert-assignment node block fun))
	  ((node-tail-p node)
	   (ir2-convert-tail-local-call node block fun))
	  (t
	   (let ((start (block-label (lambda-block fun)))
		 (returns (tail-set-info (lambda-tail-set fun)))
		 (cont (node-cont node)))
	     (ecase (if returns
			(return-info-kind returns)
			:unknown)
	       (:unknown
		(ir2-convert-local-unknown-call node block fun cont start))
	       (:fixed
		(ir2-convert-local-known-call node block fun returns
					      cont start)))))))
  (values))

;;;; full call

;;; Given a function continuation FUN, return (VALUES TN-TO-CALL
;;; NAMED-P), where TN-TO-CALL is a TN holding the thing that we call
;;; NAMED-P is true if the thing is named (false if it is a function).
;;;
;;; There are two interesting non-named cases:
;;;   -- We know it's a function. No check needed: return the
;;;      continuation LOC.
;;;   -- We don't know what it is.
(defun fun-continuation-tn (node block cont)
  (declare (type continuation cont))
  (let ((2cont (continuation-info cont)))
    (if (eq (ir2-continuation-kind 2cont) :delayed)
	(let ((name (continuation-fun-name cont t)))
	  (aver name)
	  (values (make-load-time-constant-tn :fdefinition name) t))
	(let* ((locs (ir2-continuation-locs 2cont))
	       (loc (first locs))
	       (function-ptype (primitive-type-or-lose 'function)))
	  (aver (and (eq (ir2-continuation-kind 2cont) :fixed)
		     (= (length locs) 1)))
          (aver (eq (tn-primitive-type loc) function-ptype))
	  (values loc nil)))))

;;; Set up the args to NODE in the current frame, and return a TN-REF
;;; list for the passing locations.
(defun move-tail-full-call-args (node block)
  (declare (type combination node) (type ir2-block block))
  (let ((args (basic-combination-args node))
	(last nil)
	(first nil))
    (dotimes (num (length args))
      (let ((loc (standard-arg-location num)))
	(emit-move node block (continuation-tn node block (elt args num)) loc)
	(let ((ref (reference-tn loc nil)))
	  (if last
	      (setf (tn-ref-across last) ref)
	      (setf first ref))
	  (setq last ref))))
      first))

;;; Move the arguments into the passing locations and do a (possibly
;;; named) tail call.
(defun ir2-convert-tail-full-call (node block)
  (declare (type combination node) (type ir2-block block))
  (let* ((env (physenv-info (node-physenv node)))
	 (args (basic-combination-args node))
	 (nargs (length args))
	 (pass-refs (move-tail-full-call-args node block))
	 (old-fp (ir2-physenv-old-fp env))
	 (return-pc (ir2-physenv-return-pc env)))

    (multiple-value-bind (fun-tn named)
	(fun-continuation-tn node block (basic-combination-fun node))
      (if named
	  (vop* tail-call-named node block
		(fun-tn old-fp return-pc pass-refs)
		(nil)
		nargs)
	  (vop* tail-call node block
		(fun-tn old-fp return-pc pass-refs)
		(nil)
		nargs))))

  (values))

;;; like IR2-CONVERT-LOCAL-CALL-ARGS, only different
(defun ir2-convert-full-call-args (node block)
  (declare (type combination node) (type ir2-block block))
  (let* ((args (basic-combination-args node))
	 (fp (make-stack-pointer-tn))
	 (nargs (length args)))
    (vop allocate-full-call-frame node block nargs fp)
    (collect ((locs))
      (let ((last nil)
	    (first nil))
	(dotimes (num nargs)
	  (locs (standard-arg-location num))
	  (let ((ref (reference-tn (continuation-tn node block (elt args num))
				   nil)))
	    (if last
		(setf (tn-ref-across last) ref)
		(setf first ref))
	    (setq last ref)))
	
	(values fp first (locs) nargs)))))

;;; Do full call when a fixed number of values are desired. We make
;;; STANDARD-RESULT-TNS for our continuation, then deliver the result
;;; using MOVE-CONTINUATION-RESULT. We do named or normal call, as
;;; appropriate.
(defun ir2-convert-fixed-full-call (node block)
  (declare (type combination node) (type ir2-block block))
  (multiple-value-bind (fp args arg-locs nargs)
      (ir2-convert-full-call-args node block)
    (let* ((cont (node-cont node))
	   (locs (standard-result-tns cont))
	   (loc-refs (reference-tn-list locs t))
	   (nvals (length locs)))
      (multiple-value-bind (fun-tn named)
	  (fun-continuation-tn node block (basic-combination-fun node))
	(if named
	    (vop* call-named node block (fp fun-tn args) (loc-refs)
		  arg-locs nargs nvals)
	    (vop* call node block (fp fun-tn args) (loc-refs)
		  arg-locs nargs nvals))
	(move-continuation-result node block locs cont))))
  (values))

;;; Do full call when unknown values are desired.
(defun ir2-convert-multiple-full-call (node block)
  (declare (type combination node) (type ir2-block block))
  (multiple-value-bind (fp args arg-locs nargs)
      (ir2-convert-full-call-args node block)
    (let* ((cont (node-cont node))
	   (locs (ir2-continuation-locs (continuation-info cont)))
	   (loc-refs (reference-tn-list locs t)))
      (multiple-value-bind (fun-tn named)
	  (fun-continuation-tn node block (basic-combination-fun node))
	(if named
	    (vop* multiple-call-named node block (fp fun-tn args) (loc-refs)
		  arg-locs nargs)
	    (vop* multiple-call node block (fp fun-tn args) (loc-refs)
		  arg-locs nargs)))))
  (values))

;;; stuff to check in PONDER-FULL-CALL
;;;
;;; There are some things which are intended always to be optimized
;;; away by DEFTRANSFORMs and such, and so never compiled into full
;;; calls. This has been a source of bugs so many times that it seems
;;; worth listing some of them here so that we can check the list
;;; whenever we compile a full call.
;;;
;;; FIXME: It might be better to represent this property by setting a
;;; flag in DEFKNOWN, instead of representing it by membership in this
;;; list.
(defvar *always-optimized-away*
  '(;; This should always be DEFTRANSFORMed away, but wasn't in a bug
    ;; reported to cmucl-imp 2000-06-20.
    %instance-ref
    ;; These should always turn into VOPs, but wasn't in a bug which
    ;; appeared when LTN-POLICY stuff was being tweaked in
    ;; sbcl-0.6.9.16. in sbcl-0.6.0
    data-vector-set
    data-vector-ref))

;;; more stuff to check in PONDER-FULL-CALL
;;;
;;; These came in handy when troubleshooting cold boot after making
;;; major changes in the package structure: various transforms and
;;; VOPs and stuff got attached to the wrong symbol, so that
;;; references to the right symbol were bogusly translated as full
;;; calls instead of primitives, sending the system off into infinite
;;; space. Having a report on all full calls generated makes it easier
;;; to figure out what form caused the problem this time.
#!+sb-show (defvar *show-full-called-fnames-p* nil)
#!+sb-show (defvar *full-called-fnames* (make-hash-table :test 'equal))

;;; Do some checks (and store some notes relevant for future checks)
;;; on a full call:
;;;   * Is this a full call to something we have reason to know should
;;;     never be full called? (Except as of sbcl-0.7.18 or so, we no
;;;     longer try to ensure this behavior when *FAILURE-P* has already
;;;     been detected.)
;;;   * Is this a full call to (SETF FOO) which might conflict with
;;;     a DEFSETF or some such thing elsewhere in the program?
(defun ponder-full-call (node)
  (let* ((cont (basic-combination-fun node))
	 (fname (continuation-fun-name cont t)))
    (declare (type (or symbol cons) fname))

    #!+sb-show (unless (gethash fname *full-called-fnames*)
		 (setf (gethash fname *full-called-fnames*) t))
    #!+sb-show (when *show-full-called-fnames-p*
		 (/show "converting full call to named function" fname)
		 (/show (basic-combination-args node))
		 (/show (policy node speed) (policy node safety))
		 (/show (policy node compilation-speed))
		 (let ((arg-types (mapcar (lambda (maybe-continuation)
					    (when maybe-continuation
					      (type-specifier
					       (continuation-type
						maybe-continuation))))
					  (basic-combination-args node))))
		   (/show arg-types)))

    ;; When illegal code is compiled, all sorts of perverse paths
    ;; through the compiler can be taken, and it's much harder -- and
    ;; probably pointless -- to guarantee that always-optimized-away
    ;; functions are actually optimized away. Thus, we skip the check
    ;; in that case.
    (unless *failure-p*
      (when (memq fname *always-optimized-away*)
	(/show (policy node speed) (policy node safety))
	(/show (policy node compilation-speed))
	(bug "full call to ~S" fname)))

    (when (consp fname)
      (destructuring-bind (setf stem) fname
	(aver (eq setf 'setf))
	(setf (gethash stem *setf-assumed-fboundp*) t)))))

;;; If the call is in a tail recursive position and the return
;;; convention is standard, then do a tail full call. If one or fewer
;;; values are desired, then use a single-value call, otherwise use a
;;; multiple-values call.
(defun ir2-convert-full-call (node block)
  (declare (type combination node) (type ir2-block block))
  (ponder-full-call node)
  (let ((2cont (continuation-info (node-cont node))))
    (cond ((node-tail-p node)
	   (ir2-convert-tail-full-call node block))
	  ((and 2cont
		(eq (ir2-continuation-kind 2cont) :unknown))
	   (ir2-convert-multiple-full-call node block))
	  (t
	   (ir2-convert-fixed-full-call node block))))
  (values))

;;;; entering functions

;;; Do all the stuff that needs to be done on XEP entry:
;;; -- Create frame.
;;; -- Copy any more arg.
;;; -- Set up the environment, accessing any closure variables.
;;; -- Move args from the standard passing locations to their internal
;;;    locations.
(defun init-xep-environment (node block fun)
  (declare (type bind node) (type ir2-block block) (type clambda fun))
  (let ((start-label (entry-info-offset (leaf-info fun)))
	(env (physenv-info (node-physenv node))))
    (let ((ef (functional-entry-fun fun)))
      (cond ((and (optional-dispatch-p ef) (optional-dispatch-more-entry ef))
	     ;; Special case the xep-allocate-frame + copy-more-arg case.
	     (vop xep-allocate-frame node block start-label t)
	     (vop copy-more-arg node block (optional-dispatch-max-args ef)))
	    (t
	     ;; No more args, so normal entry.
	     (vop xep-allocate-frame node block start-label nil)))
      (if (ir2-physenv-closure env)
	  (let ((closure (make-normal-tn *backend-t-primitive-type*)))
	    (vop setup-closure-environment node block start-label closure)
	    (when (getf (functional-plist ef) :fin-function)
	      (vop funcallable-instance-lexenv node block closure closure))
	    (let ((n -1))
	      (dolist (loc (ir2-physenv-closure env))
		(vop closure-ref node block closure (incf n) (cdr loc)))))
	  (vop setup-environment node block start-label)))

    (unless (eq (functional-kind fun) :toplevel)
      (let ((vars (lambda-vars fun))
	    (n 0))
	(when (leaf-refs (first vars))
	  (emit-move node block (make-arg-count-location)
		     (leaf-info (first vars))))
	(dolist (arg (rest vars))
	  (when (leaf-refs arg)
	    (let ((pass (standard-arg-location n))
		  (home (leaf-info arg)))
	      (if (lambda-var-indirect arg)
		  (do-make-value-cell node block pass home)
		  (emit-move node block pass home))))
	  (incf n))))

    (emit-move node block (make-old-fp-passing-location t)
	       (ir2-physenv-old-fp env)))

  (values))

;;; Emit function prolog code. This is only called on bind nodes for
;;; functions that allocate environments. All semantics of let calls
;;; are handled by IR2-CONVERT-LET.
;;;
;;; If not an XEP, all we do is move the return PC from its passing
;;; location, since in a local call, the caller allocates the frame
;;; and sets up the arguments.
(defun ir2-convert-bind (node block)
  (declare (type bind node) (type ir2-block block))
  (let* ((fun (bind-lambda node))
	 (env (physenv-info (lambda-physenv fun))))
    (aver (member (functional-kind fun)
		  '(nil :external :optional :toplevel :cleanup)))

    (when (xep-p fun)
      (init-xep-environment node block fun)
      #!+sb-dyncount
      (when *collect-dynamic-statistics*
	(vop count-me node block *dynamic-counts-tn*
	     (block-number (ir2-block-block block)))))

    (emit-move node
	       block
	       (ir2-physenv-return-pc-pass env)
	       (ir2-physenv-return-pc env))

    (let ((lab (gen-label)))
      (setf (ir2-physenv-environment-start env) lab)
      (vop note-environment-start node block lab)))

  (values))

;;;; function return

;;; Do stuff to return from a function with the specified values and
;;; convention. If the return convention is :FIXED and we aren't
;;; returning from an XEP, then we do a known return (letting
;;; representation selection insert the correct move-arg VOPs.)
;;; Otherwise, we use the unknown-values convention. If there is a
;;; fixed number of return values, then use RETURN, otherwise use
;;; RETURN-MULTIPLE.
(defun ir2-convert-return (node block)
  (declare (type creturn node) (type ir2-block block))
  (let* ((cont (return-result node))
	 (2cont (continuation-info cont))
	 (cont-kind (ir2-continuation-kind 2cont))
	 (fun (return-lambda node))
	 (env (physenv-info (lambda-physenv fun)))
	 (old-fp (ir2-physenv-old-fp env))
	 (return-pc (ir2-physenv-return-pc env))
	 (returns (tail-set-info (lambda-tail-set fun))))
    (cond
     ((and (eq (return-info-kind returns) :fixed)
	   (not (xep-p fun)))
      (let ((locs (continuation-tns node block cont
				    (return-info-types returns))))
	(vop* known-return node block
	      (old-fp return-pc (reference-tn-list locs nil))
	      (nil)
	      (return-info-locations returns))))
     ((eq cont-kind :fixed)
      (let* ((types (mapcar #'tn-primitive-type (ir2-continuation-locs 2cont)))
	     (cont-locs (continuation-tns node block cont types))
	     (nvals (length cont-locs))
	     (locs (make-standard-value-tns nvals)))
	(mapc (lambda (val loc)
		(emit-move node block val loc))
	      cont-locs
	      locs)
	(if (= nvals 1)
	    (vop return-single node block old-fp return-pc (car locs))
	    (vop* return node block
		  (old-fp return-pc (reference-tn-list locs nil))
		  (nil)
		  nvals))))
     (t
      (aver (eq cont-kind :unknown))
      (vop* return-multiple node block
	    (old-fp return-pc
		    (reference-tn-list (ir2-continuation-locs 2cont) nil))
	    (nil)))))

  (values))

;;;; debugger hooks

;;; This is used by the debugger to find the top function on the
;;; stack. It returns the OLD-FP and RETURN-PC for the current
;;; function as multiple values.
(defoptimizer (sb!kernel:%caller-frame-and-pc ir2-convert) (() node block)
  (let ((ir2-physenv (physenv-info (node-physenv node))))
    (move-continuation-result node block
			      (list (ir2-physenv-old-fp ir2-physenv)
				    (ir2-physenv-return-pc ir2-physenv))
			      (node-cont node))))

;;;; multiple values

;;; This is almost identical to IR2-CONVERT-LET. Since LTN annotates
;;; the continuation for the correct number of values (with the
;;; continuation user responsible for defaulting), we can just pick
;;; them up from the continuation.
(defun ir2-convert-mv-bind (node block)
  (declare (type mv-combination node) (type ir2-block block))
  (let* ((cont (first (basic-combination-args node)))
	 (fun (ref-leaf (continuation-use (basic-combination-fun node))))
	 (vars (lambda-vars fun)))
    (aver (eq (functional-kind fun) :mv-let))
    (mapc (lambda (src var)
	    (when (leaf-refs var)
	      (let ((dest (leaf-info var)))
		(if (lambda-var-indirect var)
		    (do-make-value-cell node block src dest)
		    (emit-move node block src dest)))))
	  (continuation-tns node block cont
			    (mapcar (lambda (x)
				      (primitive-type (leaf-type x)))
				    vars))
	  vars))
  (values))

;;; Emit the appropriate fixed value, unknown value or tail variant of
;;; CALL-VARIABLE. Note that we only need to pass the values start for
;;; the first argument: all the other argument continuation TNs are
;;; ignored. This is because we require all of the values globs to be
;;; contiguous and on stack top.
(defun ir2-convert-mv-call (node block)
  (declare (type mv-combination node) (type ir2-block block))
  (aver (basic-combination-args node))
  (let* ((start-cont (continuation-info (first (basic-combination-args node))))
	 (start (first (ir2-continuation-locs start-cont)))
	 (tails (and (node-tail-p node)
		     (lambda-tail-set (node-home-lambda node))))
	 (cont (node-cont node))
	 (2cont (continuation-info cont)))
    (multiple-value-bind (fun named)
	(fun-continuation-tn node block (basic-combination-fun node))
      (aver (and (not named)
		 (eq (ir2-continuation-kind start-cont) :unknown)))
      (cond
       (tails
	(let ((env (physenv-info (node-physenv node))))
	  (vop tail-call-variable node block start fun
	       (ir2-physenv-old-fp env)
	       (ir2-physenv-return-pc env))))
       ((and 2cont
	     (eq (ir2-continuation-kind 2cont) :unknown))
	(vop* multiple-call-variable node block (start fun nil)
	      ((reference-tn-list (ir2-continuation-locs 2cont) t))))
       (t
	(let ((locs (standard-result-tns cont)))
	  (vop* call-variable node block (start fun nil)
		((reference-tn-list locs t)) (length locs))
	  (move-continuation-result node block locs cont)))))))

;;; Reset the stack pointer to the start of the specified
;;; unknown-values continuation (discarding it and all values globs on
;;; top of it.)
(defoptimizer (%pop-values ir2-convert) ((continuation) node block)
  (let ((2cont (continuation-info (continuation-value continuation))))
    (aver (eq (ir2-continuation-kind 2cont) :unknown))
    (vop reset-stack-pointer node block
	 (first (ir2-continuation-locs 2cont)))))

;;; Deliver the values TNs to CONT using MOVE-CONTINUATION-RESULT.
(defoptimizer (values ir2-convert) ((&rest values) node block)
  (let ((tns (mapcar (lambda (x)
		       (continuation-tn node block x))
		     values)))
    (move-continuation-result node block tns (node-cont node))))

;;; In the normal case where unknown values are desired, we use the
;;; VALUES-LIST VOP. In the relatively unimportant case of VALUES-LIST
;;; for a fixed number of values, we punt by doing a full call to the
;;; VALUES-LIST function. This gets the full call VOP to deal with
;;; defaulting any unsupplied values. It seems unworthwhile to
;;; optimize this case.
(defoptimizer (values-list ir2-convert) ((list) node block)
  (let* ((cont (node-cont node))
	 (2cont (continuation-info cont)))
    (cond ((and 2cont
                (eq (ir2-continuation-kind 2cont) :unknown))
           (let ((locs (ir2-continuation-locs 2cont)))
             (vop* values-list node block
                   ((continuation-tn node block list) nil)
                   ((reference-tn-list locs t)))))
          (t (aver (or (not 2cont) ; i.e. we want to check the argument
                       (eq (ir2-continuation-kind 2cont) :fixed)))
             (ir2-convert-full-call node block)))))

(defoptimizer (%more-arg-values ir2-convert) ((context start count) node block)
  (let* ((cont (node-cont node))
	 (2cont (continuation-info cont)))
    (when 2cont
      (ecase (ir2-continuation-kind 2cont)
	(:fixed (ir2-convert-full-call node block))
	(:unknown
	 (let ((locs (ir2-continuation-locs 2cont)))
	   (vop* %more-arg-values node block
		 ((continuation-tn node block context)
		  (continuation-tn node block start)
		  (continuation-tn node block count)
		  nil)
		 ((reference-tn-list locs t)))))))))

;;;; special binding

;;; This is trivial, given our assumption of a shallow-binding
;;; implementation.
(defoptimizer (%special-bind ir2-convert) ((var value) node block)
  (let ((name (leaf-source-name (continuation-value var))))
    (vop bind node block (continuation-tn node block value)
	 (emit-constant name))))
(defoptimizer (%special-unbind ir2-convert) ((var) node block)
  (vop unbind node block))

;;; ### It's not clear that this really belongs in this file, or
;;; should really be done this way, but this is the least violation of
;;; abstraction in the current setup. We don't want to wire
;;; shallow-binding assumptions into IR1tran.
(def-ir1-translator progv ((vars vals &body body) start cont)
  (ir1-convert
   start cont
   (let ((bind (gensym "BIND"))
         (unbind (gensym "UNBIND")))
     (once-only ((n-save-bs '(%primitive current-binding-pointer)))
                `(unwind-protect
                      (progn
                        (labels ((,unbind (vars)
                                   (declare (optimize (speed 2) (debug 0)))
                                   (dolist (var vars)
                                     (%primitive bind nil var)
                                     (makunbound var)))
                                 (,bind (vars vals)
                                   (declare (optimize (speed 2) (debug 0)))
                                   (cond ((null vars))
                                         ((null vals) (,unbind vars))
                                         (t (%primitive bind (car vals) (car vars))
                                            (,bind (cdr vars) (cdr vals))))))
                          (,bind ,vars ,vals))
                        nil
                        ,@body)
                   (%primitive unbind-to-here ,n-save-bs))))))

;;;; non-local exit

;;; Convert a non-local lexical exit. First find the NLX-INFO in our
;;; environment. Note that this is never called on the escape exits
;;; for CATCH and UNWIND-PROTECT, since the escape functions aren't
;;; IR2 converted.
(defun ir2-convert-exit (node block)
  (declare (type exit node) (type ir2-block block))
  (let ((loc (find-in-physenv (find-nlx-info (exit-entry node)
					     (node-cont node))
			      (node-physenv node)))
	(temp (make-stack-pointer-tn))
	(value (exit-value node)))
    (vop value-cell-ref node block loc temp)
    (if value
	(let ((locs (ir2-continuation-locs (continuation-info value))))
	  (vop unwind node block temp (first locs) (second locs)))
	(let ((0-tn (emit-constant 0)))
	  (vop unwind node block temp 0-tn 0-tn))))

  (values))

;;; %CLEANUP-POINT doesn't do anything except prevent the body from
;;; being entirely deleted.
(defoptimizer (%cleanup-point ir2-convert) (() node block) node block)

;;; This function invalidates a lexical exit on exiting from the
;;; dynamic extent. This is done by storing 0 into the indirect value
;;; cell that holds the closed unwind block.
(defoptimizer (%lexical-exit-breakup ir2-convert) ((info) node block)
  (vop value-cell-set node block
       (find-in-physenv (continuation-value info) (node-physenv node))
       (emit-constant 0)))

;;; We have to do a spurious move of no values to the result
;;; continuation so that lifetime analysis won't get confused.
(defun ir2-convert-throw (node block)
  (declare (type mv-combination node) (type ir2-block block))
  (let ((args (basic-combination-args node)))
    (check-catch-tag-type (first args))
    (vop* throw node block
	  ((continuation-tn node block (first args))
	   (reference-tn-list
	    (ir2-continuation-locs (continuation-info (second args)))
	    nil))
	  (nil)))
  (move-continuation-result node block () (node-cont node))
  (values))

;;; Emit code to set up a non-local exit. INFO is the NLX-INFO for the
;;; exit, and TAG is the continuation for the catch tag (if any.) We
;;; get at the target PC by passing in the label to the vop. The vop
;;; is responsible for building a return-PC object.
(defun emit-nlx-start (node block info tag)
  (declare (type node node) (type ir2-block block) (type nlx-info info)
	   (type (or continuation null) tag))
  (let* ((2info (nlx-info-info info))
	 (kind (cleanup-kind (nlx-info-cleanup info)))
	 (block-tn (physenv-live-tn
		    (make-normal-tn (primitive-type-or-lose 'catch-block))
		    (node-physenv node)))
	 (res (make-stack-pointer-tn))
	 (target-label (ir2-nlx-info-target 2info)))

    (vop current-binding-pointer node block
	 (car (ir2-nlx-info-dynamic-state 2info)))
    (vop* save-dynamic-state node block
	  (nil)
	  ((reference-tn-list (cdr (ir2-nlx-info-dynamic-state 2info)) t)))
    (vop current-stack-pointer node block (ir2-nlx-info-save-sp 2info))

    (ecase kind
      (:catch
       (vop make-catch-block node block block-tn
	    (continuation-tn node block tag) target-label res))
      ((:unwind-protect :block :tagbody)
       (vop make-unwind-block node block block-tn target-label res)))

    (ecase kind
      ((:block :tagbody)
       (do-make-value-cell node block res (ir2-nlx-info-home 2info)))
      (:unwind-protect
       (vop set-unwind-protect node block block-tn))
      (:catch)))

  (values))

;;; Scan each of ENTRY's exits, setting up the exit for each lexical exit.
(defun ir2-convert-entry (node block)
  (declare (type entry node) (type ir2-block block))
  (dolist (exit (entry-exits node))
    (let ((info (find-nlx-info node (node-cont exit))))
      (when (and info
		 (member (cleanup-kind (nlx-info-cleanup info))
			 '(:block :tagbody)))
	(emit-nlx-start node block info nil))))
  (values))

;;; Set up the unwind block for these guys.
(defoptimizer (%catch ir2-convert) ((info-cont tag) node block)
  (check-catch-tag-type tag)
  (emit-nlx-start node block (continuation-value info-cont) tag))
(defoptimizer (%unwind-protect ir2-convert) ((info-cont cleanup) node block)
  (emit-nlx-start node block (continuation-value info-cont) nil))

;;; Emit the entry code for a non-local exit. We receive values and
;;; restore dynamic state.
;;;
;;; In the case of a lexical exit or CATCH, we look at the exit
;;; continuation's kind to determine which flavor of entry VOP to
;;; emit. If unknown values, emit the xxx-MULTIPLE variant to the
;;; continuation locs. If fixed values, make the appropriate number of
;;; temps in the standard values locations and use the other variant,
;;; delivering the temps to the continuation using
;;; MOVE-CONTINUATION-RESULT.
;;;
;;; In the UNWIND-PROTECT case, we deliver the first register
;;; argument, the argument count and the argument pointer to our
;;; continuation as multiple values. These values are the block exited
;;; to and the values start and count.
;;;
;;; After receiving values, we restore dynamic state. Except in the
;;; UNWIND-PROTECT case, the values receiving restores the stack
;;; pointer. In an UNWIND-PROTECT cleanup, we want to leave the stack
;;; pointer alone, since the thrown values are still out there.
(defoptimizer (%nlx-entry ir2-convert) ((info-cont) node block)
  (let* ((info (continuation-value info-cont))
	 (cont (nlx-info-continuation info))
	 (2cont (continuation-info cont))
	 (2info (nlx-info-info info))
	 (top-loc (ir2-nlx-info-save-sp 2info))
	 (start-loc (make-nlx-entry-arg-start-location))
	 (count-loc (make-arg-count-location))
	 (target (ir2-nlx-info-target 2info)))

    (ecase (cleanup-kind (nlx-info-cleanup info))
      ((:catch :block :tagbody)
       (if (and 2cont (eq (ir2-continuation-kind 2cont) :unknown))
	   (vop* nlx-entry-multiple node block
		 (top-loc start-loc count-loc nil)
		 ((reference-tn-list (ir2-continuation-locs 2cont) t))
		 target)
	   (let ((locs (standard-result-tns cont)))
	     (vop* nlx-entry node block
		   (top-loc start-loc count-loc nil)
		   ((reference-tn-list locs t))
		   target
		   (length locs))
	     (move-continuation-result node block locs cont))))
      (:unwind-protect
       (let ((block-loc (standard-arg-location 0)))
	 (vop uwp-entry node block target block-loc start-loc count-loc)
	 (move-continuation-result
	  node block
	  (list block-loc start-loc count-loc)
	  cont))))

    #!+sb-dyncount
    (when *collect-dynamic-statistics*
      (vop count-me node block *dynamic-counts-tn*
	   (block-number (ir2-block-block block))))

    (vop* restore-dynamic-state node block
	  ((reference-tn-list (cdr (ir2-nlx-info-dynamic-state 2info)) nil))
	  (nil))
    (vop unbind-to-here node block
	 (car (ir2-nlx-info-dynamic-state 2info)))))

;;;; n-argument functions

(macrolet ((def (name)
	     `(defoptimizer (,name ir2-convert) ((&rest args) node block)
		(let* ((refs (move-tail-full-call-args node block))
		       (cont (node-cont node))
		       (res (continuation-result-tns
			     cont
			     (list (primitive-type (specifier-type 'list))))))
		  (vop* ,name node block (refs) ((first res) nil)
			(length args))
		  (move-continuation-result node block res cont)))))
  (def list)
  (def list*))

;;; Convert the code in a component into VOPs.
(defun ir2-convert (component)
  (declare (type component component))
  (let (#!+sb-dyncount
	(*dynamic-counts-tn*
	 (when *collect-dynamic-statistics*
	   (let* ((blocks
		   (block-number (block-next (component-head component))))
		  (counts (make-array blocks
				      :element-type '(unsigned-byte 32)
				      :initial-element 0))
		  (info (make-dyncount-info
			 :for (component-name component)
			 :costs (make-array blocks
					    :element-type '(unsigned-byte 32)
					    :initial-element 0)
			 :counts counts)))
	     (setf (ir2-component-dyncount-info (component-info component))
		   info)
	     (emit-constant info)
	     (emit-constant counts)))))
    (let ((num 0))
      (declare (type index num))
      (do-ir2-blocks (2block component)
	(let ((block (ir2-block-block 2block)))
	  (when (block-start block)
	    (setf (block-number block) num)
	    #!+sb-dyncount
	    (when *collect-dynamic-statistics*
	      (let ((first-node (continuation-next (block-start block))))
		(unless (or (and (bind-p first-node)
				 (xep-p (bind-lambda first-node)))
			    (eq (continuation-fun-name
				 (node-cont first-node))
				'%nlx-entry))
		  (vop count-me
		       first-node
		       2block
		       #!+sb-dyncount *dynamic-counts-tn* #!-sb-dyncount nil
		       num))))
	    (ir2-convert-block block)
	    (incf num))))))
  (values))

;;; If necessary, emit a terminal unconditional branch to go to the
;;; successor block. If the successor is the component tail, then
;;; there isn't really any successor, but if the end is an unknown,
;;; non-tail call, then we emit an error trap just in case the
;;; function really does return.
(defun finish-ir2-block (block)
  (declare (type cblock block))
  (let* ((2block (block-info block))
	 (last (block-last block))
	 (succ (block-succ block)))
    (unless (if-p last)
      (aver (and succ (null (rest succ))))
      (let ((target (first succ)))
	(cond ((eq target (component-tail (block-component block)))
	       (when (and (basic-combination-p last)
			  (eq (basic-combination-kind last) :full))
		 (let* ((fun (basic-combination-fun last))
			(use (continuation-use fun))
			(name (and (ref-p use)
				   (leaf-has-source-name-p (ref-leaf use))
				   (leaf-source-name (ref-leaf use)))))
		   (unless (or (node-tail-p last)
			       (info :function :info name)
			       (policy last (zerop safety)))
		     (vop nil-fun-returned-error last 2block
			  (if name
			      (emit-constant name)
			      (multiple-value-bind (tn named)
				  (fun-continuation-tn last 2block fun)
				(aver (not named))
				tn)))))))
	      ((not (eq (ir2-block-next 2block) (block-info target)))
	       (vop branch last 2block (block-label target)))))))

  (values))

;;; Convert the code in a block into VOPs.
(defun ir2-convert-block (block)
  (declare (type cblock block))
  (let ((2block (block-info block)))
    (do-nodes (node cont block)
      (etypecase node
	(ref
	 (let ((2cont (continuation-info cont)))
	   (when (and 2cont
		      (not (eq (ir2-continuation-kind 2cont) :delayed)))
	     (ir2-convert-ref node 2block))))
	(combination
	 (let ((kind (basic-combination-kind node)))
	   (case kind
	     (:local
	      (ir2-convert-local-call node 2block))
	     (:full
	      (ir2-convert-full-call node 2block))
	     (t
	      (let ((fun (fun-info-ir2-convert kind)))
		(cond (fun
		       (funcall fun node 2block))
		      ((eq (basic-combination-info node) :full)
		       (ir2-convert-full-call node 2block))
		      (t
		       (ir2-convert-template node 2block))))))))
	(cif
	 (when (continuation-info (if-test node))
	   (ir2-convert-if node 2block)))
	(bind
	 (let ((fun (bind-lambda node)))
	   (when (eq (lambda-home fun) fun)
	     (ir2-convert-bind node 2block))))
	(creturn
	 (ir2-convert-return node 2block))
	(cset
	 (ir2-convert-set node 2block))
        (cast
         (ir2-convert-cast node 2block))
	(mv-combination
	 (cond
	  ((eq (basic-combination-kind node) :local)
	   (ir2-convert-mv-bind node 2block))
	  ((eq (continuation-fun-name (basic-combination-fun node))
	       '%throw)
	   (ir2-convert-throw node 2block))
	  (t
	   (ir2-convert-mv-call node 2block))))
	(exit
	 (when (exit-entry node)
	   (ir2-convert-exit node 2block)))
	(entry
	 (ir2-convert-entry node 2block)))))

  (finish-ir2-block block)

  (values))
