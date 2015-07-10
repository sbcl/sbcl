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

;;; Return the LTN-POLICY indicated by the node policy.
;;;
;;; FIXME: It would be tidier to use an LTN-POLICY object (an instance
;;; of DEFSTRUCT LTN-POLICY) instead of a keyword, and have queries
;;; like LTN-POLICY-SAFE-P become slot accessors. If we do this,
;;; grep for and carefully review use of literal keywords, so that
;;; things like
;;;   (EQ (TEMPLATE-LTN-POLICY TEMPLATE) :SAFE)
;;; don't get overlooked.
;;;
;;; FIXME: Classic CMU CL went to some trouble to cache LTN-POLICY
;;; values in LTN-ANALYZE so that they didn't have to be recomputed on
;;; every block. I stripped that out (the whole DEFMACRO FROB thing)
;;; because I found it too confusing. Thus, it might be that the
;;; new uncached code spends an unreasonable amount of time in
;;; this lookup function. This function should be profiled, and if
;;; it's a significant contributor to runtime, we can cache it in
;;; some more local way, e.g. by adding a CACHED-LTN-POLICY slot to
;;; the NODE structure, and doing something like
;;;   (DEFUN NODE-LTN-POLICY (NODE)
;;;     (OR (NODE-CACHED-LTN-POLICY NODE)
;;;         (SETF (NODE-CACHED-LTN-POLICY NODE)
;;;               (NODE-UNCACHED-LTN-POLICY NODE)))
(defun node-ltn-policy (node)
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

;;; Return true if LTN-POLICY is a safe policy.
(defun ltn-policy-safe-p (ltn-policy)
  (ecase ltn-policy
    ((:safe :fast-safe) t)
    ((:small :fast) nil)))

;;; For possibly-new blocks, make sure that there is an associated
;;; IR2-BLOCK.
(defun ensure-block-has-ir2-block (new-block)
  ;; If BLOCK-START is NIL then it's the component-tail block (which
  ;; we don't care about), and if BLOCK-INFO is not null then it
  ;; doesn't need to be overwritten.
  (when (and (block-start new-block)
             (not (block-info new-block)))
    (setf (block-info new-block)
          (make-ir2-block new-block))))

;;; When splitting an existing block, make sure that unknown-values
;;; killed information is distributed appropriately.
(defun fixup-ir2-blocks-for-split-block (old-block new-block)
  (ensure-block-has-ir2-block new-block)
  (let* ((old-ir2-block (block-info old-block))
         (new-ir2-block (block-info new-block)))
    (collect ((old-popped) (new-popped))
      (dolist (lvar (ir2-block-popped old-ir2-block))
        (if (eq (node-block (lvar-dest lvar)) old-block)
            (old-popped lvar)
            (new-popped lvar))
        (setf (ir2-block-popped old-ir2-block) (old-popped))
        (setf (ir2-block-popped new-ir2-block) (new-popped))))))

;;; an annotated lvar's primitive-type
#!-sb-fluid (declaim (inline lvar-ptype))
(defun lvar-ptype (lvar)
  (declare (type lvar lvar))
  (ir2-lvar-primitive-type (lvar-info lvar)))

;;; If LVAR is used only by a REF to a leaf that can be delayed, then
;;; return the leaf, otherwise return NIL.
(defun lvar-delayed-leaf (lvar)
  (declare (type lvar lvar))
  (unless (lvar-dynamic-extent lvar)
    (let ((use (lvar-uses lvar)))
      (and (ref-p use)
           (let ((leaf (ref-leaf use)))
             (etypecase leaf
               (lambda-var (if (null (lambda-var-sets leaf)) leaf nil))
               (constant leaf)
               ((or functional global-var) nil)))))))

;;; Annotate a normal single-value lvar. If its only use is a ref that
;;; we are allowed to delay the evaluation of, then we mark the lvar
;;; for delayed evaluation, otherwise we assign a TN to hold the
;;; lvar's value.
(defun annotate-1-value-lvar (lvar)
  (declare (type lvar lvar))
  (let ((info (lvar-info lvar)))
    (aver (eq (ir2-lvar-kind info) :fixed))
    (cond
     ((lvar-delayed-leaf lvar)
      (setf (ir2-lvar-kind info) :delayed))
     (t (let ((tn (make-normal-tn (ir2-lvar-primitive-type info))))
          (setf (ir2-lvar-locs info) (list tn))
          (when (lvar-dynamic-extent lvar)
            (setf (ir2-lvar-stack-pointer info)
                  (make-stack-pointer-tn)))))))
  (ltn-annotate-casts lvar)
  (values))

;;; Make an IR2-LVAR corresponding to the lvar type and then do
;;; ANNOTATE-1-VALUE-LVAR.
(defun annotate-ordinary-lvar (lvar)
  (declare (type lvar lvar))
  (let ((info (make-ir2-lvar
               (primitive-type (lvar-type lvar)))))
    (setf (lvar-info lvar) info)
    (annotate-1-value-lvar lvar))
  (values))

;;; Annotate the function lvar for a full call. If the only reference
;;; is to a global function and DELAY is true, then we delay the
;;; reference, otherwise we annotate for a single value.
(defun annotate-fun-lvar (lvar &optional (delay t))
  (declare (type lvar lvar))
  (aver (not (lvar-dynamic-extent lvar)))
  (let* ((tn-ptype (primitive-type (lvar-type lvar)))
         (info (make-ir2-lvar tn-ptype)))
    (setf (lvar-info lvar) info)
    (let ((name (lvar-fun-name lvar t)))
      (if (and delay name)
          (setf (ir2-lvar-kind info) :delayed)
          (setf (ir2-lvar-locs info)
                (list (make-normal-tn tn-ptype))))))
  (ltn-annotate-casts lvar)
  (values))

;;; If TAIL-P is true, then we check to see whether the call can
;;; really be a tail call by seeing if this function's return
;;; convention is :UNKNOWN. If so, we move the call block successor
;;; link from the return block to the component tail (after ensuring
;;; that they are in separate blocks.) This allows the return to be
;;; deleted when there are no non-tail uses.
(defun flush-full-call-tail-transfer (call)
  (declare (type basic-combination call))
  (let ((tails (and (node-tail-p call)
                    (lambda-tail-set (node-home-lambda call)))))
    (when tails
      (cond ((eq (return-info-kind (tail-set-info tails)) :unknown)
             (node-ends-block call)
             (let* ((block (node-block call))
                    (new-block (first (block-succ block))))
               (fixup-ir2-blocks-for-split-block block new-block)
               (unlink-blocks block new-block)
               (link-blocks block (component-tail (block-component block)))))
            (t
             (setf (node-tail-p call) nil)))))
  (values))

;;; We set the kind to :FULL or :FUNNY, depending on whether there is
;;; an IR2-CONVERT method. If a funny function, then we inhibit tail
;;; recursion normally, since the IR2 convert method is going to want
;;; to deliver values normally. We still annotate the function lvar,
;;; since IR2tran might decide to call after all.
;;;
;;; Note that args may already be annotated because template selection
;;; can bail out to here.
(defun ltn-default-call (call)
  (declare (type combination call))
  (let ((kind (basic-combination-kind call))
        (info (basic-combination-fun-info call)))
    (annotate-fun-lvar (basic-combination-fun call))

    (dolist (arg (basic-combination-args call))
      (unless (lvar-info arg)
        (setf (lvar-info arg)
              (make-ir2-lvar (primitive-type (lvar-type arg)))))
      (annotate-1-value-lvar arg))

    (cond
      ((and (eq kind :known)
            (fun-info-p info)
            (fun-info-ir2-convert info))
       (setf (basic-combination-info call) :funny)
       (setf (node-tail-p call) nil))
      (t
       (when (eq kind :error)
         (setf (basic-combination-kind call) :full))
       (setf (basic-combination-info call) :full)
       (flush-full-call-tail-transfer call))))

  (values))

;;; Annotate an lvar for unknown multiple values:
;;; -- Add the lvar to the IR2-BLOCK-POPPED if it is used across a
;;;    block boundary.
;;; -- Assign an :UNKNOWN IR2-LVAR.
;;;
;;; Note: it is critical that this be called only during LTN analysis
;;; of LVAR's DEST, and called in the order that the lvarss are
;;; received. Otherwise the IR2-BLOCK-POPPED and
;;; IR2-COMPONENT-VALUES-FOO would get all messed up.
(defun annotate-unknown-values-lvar (lvar)
  (declare (type lvar lvar))

  (aver (not (lvar-dynamic-extent lvar)))
  (let ((2lvar (make-ir2-lvar nil)))
    (setf (ir2-lvar-kind 2lvar) :unknown)
    (setf (ir2-lvar-locs 2lvar) (make-unknown-values-locations))
    (setf (lvar-info lvar) 2lvar))

  ;; The CAST chain with corresponding lvars constitute the same
  ;; "principal lvar", so we must preserve only inner annotation order
  ;; and the order of the whole p.l. with other lvars. -- APD,
  ;; 2003-02-27
  (ltn-annotate-casts lvar)

  (let* ((block (node-block (lvar-dest lvar)))
         (use (lvar-uses lvar))
         (2block (block-info block)))
    (unless (and (not (listp use)) (eq (node-block use) block))
      (setf (ir2-block-popped 2block)
            (nconc (ir2-block-popped 2block) (list lvar)))))

  (values))

;;; Annotate LVAR for a fixed, but arbitrary number of values, of the
;;; specified primitive TYPES.
(defun annotate-fixed-values-lvar (lvar types)
  (declare (type lvar lvar) (list types))
  (let ((info (make-ir2-lvar nil)))
    (setf (ir2-lvar-locs info) (mapcar #'make-normal-tn types))
    (setf (lvar-info lvar) info)
    (when (lvar-dynamic-extent lvar)
      (aver (proper-list-of-length-p types 1))
      (setf (ir2-lvar-stack-pointer info)
            (make-stack-pointer-tn))))
  (ltn-annotate-casts lvar)
  (values))

;;;; node-specific analysis functions

;;; Annotate the result lvar for a function. We use the RETURN-INFO
;;; computed by GTN to determine how to represent the return values
;;; within the function:
;;;  * If the TAIL-SET has a fixed values count, then use that many
;;;    values.
;;;  * If the actual uses of the result lvar in this function
;;;    have a fixed number of values (after intersection with the
;;;    assertion), then use that number. We throw out TAIL-P :FULL
;;;    and :LOCAL calls, since we know they will truly end up as TR
;;;    calls. We can use the BASIC-COMBINATION-INFO even though it
;;;    is assigned by this phase, since the initial value NIL doesn't
;;;    look like a TR call.
;;;      If there are *no* non-tail-call uses, then it falls out
;;;    that we annotate for one value (type is NIL), but the return
;;;    will end up being deleted.
;;;      In non-perverse code, the DFO walk will reach all uses of the
;;;    result lvar before it reaches the RETURN. In perverse code, we
;;;    may annotate for unknown values when we didn't have to.
;;; * Otherwise, we must annotate the lvar for unknown values.
(defun ltn-analyze-return (node)
  (declare (type creturn node))
  (let* ((lvar (return-result node))
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

          (let ((int (res)))
            (multiple-value-bind (types kind)
                (if (eq int *empty-type*)
                    (values nil :unknown)
                    (values-types int))
              (if (eq kind :unknown)
                  (annotate-unknown-values-lvar lvar)
                  (annotate-fixed-values-lvar
                   lvar (mapcar #'primitive-type types))))))
        (annotate-fixed-values-lvar lvar types)))

  (values))

;;; Annotate the single argument lvar as a fixed-values lvar. We look
;;; at the called lambda to determine number and type of return values
;;; desired. It is assumed that only a function that
;;; LOOKS-LIKE-AN-MV-BIND will be converted to a local call.
(defun ltn-analyze-mv-bind (call)
  (declare (type mv-combination call))
  (setf (basic-combination-kind call) :local)
  (setf (node-tail-p call) nil)
  (annotate-fixed-values-lvar
   (first (basic-combination-args call))
   (mapcar (lambda (var)
             (primitive-type (basic-var-type var)))
           (lambda-vars
            (ref-leaf (lvar-use (basic-combination-fun call))))))
  (values))

;;; We force all the argument lvars to use the unknown values
;;; convention. The lvars are annotated in reverse order, since the
;;; last argument is on top, thus must be popped first. We disallow
;;; delayed evaluation of the function lvar to simplify IR2 conversion
;;; of MV call.
;;;
;;; We could be cleverer when we know the number of values returned by
;;; the lvars, but optimizations of MV call are probably unworthwhile.
;;;
;;; We are also responsible for handling THROW, which is represented
;;; in IR1 as an MV call to the %THROW funny function. We annotate the
;;; tag lvar for a single value and the values lvar for unknown
;;; values.
(defun ltn-analyze-mv-call (call)
  (declare (type mv-combination call))
  (let ((fun (basic-combination-fun call))
        (args (basic-combination-args call)))
    (cond ((eq (lvar-fun-name fun) '%throw)
           (setf (basic-combination-info call) :funny)
           (annotate-ordinary-lvar (first args))
           (annotate-unknown-values-lvar (second args))
           (setf (node-tail-p call) nil))
          (t
           (setf (basic-combination-info call) :full)
           (annotate-fun-lvar (basic-combination-fun call) nil)
           (dolist (arg (reverse args))
             (annotate-unknown-values-lvar arg))
           (flush-full-call-tail-transfer call))))

  (values))

;;; Annotate the arguments as ordinary single-value lvars. And check
;;; the successor.
(defun ltn-analyze-local-call (call)
  (declare (type combination call))
  (setf (basic-combination-info call) :local)
  (dolist (arg (basic-combination-args call))
    (when arg
      (annotate-ordinary-lvar arg)))
  (when (node-tail-p call)
    (set-tail-local-call-successor call))
  (values))

;;; Make sure that a tail local call is linked directly to the bind
;;; node. Usually it will be, but calls from XEPs and calls that might have
;;; needed a cleanup after them won't have been swung over yet, since we
;;; weren't sure they would really be TR until now.
(defun set-tail-local-call-successor (call)
  (let ((caller (node-home-lambda call))
        (callee (combination-lambda call)))
    (aver (eq (lambda-tail-set caller)
              (lambda-tail-set (lambda-home callee))))
    (node-ends-block call)
    (let* ((block (node-block call))
           (new-block (first (block-succ block))))
      (fixup-ir2-blocks-for-split-block block new-block)
      (unlink-blocks block new-block)
      (link-blocks block (lambda-block callee))))
  (values))

;;; Annotate the value lvar.
(defun ltn-analyze-set (node)
  (declare (type cset node))
  (setf (node-tail-p node) nil)
  (annotate-ordinary-lvar (set-value node))
  (values))

;;; If the only use of the TEST lvar is a combination annotated with a
;;; conditional template, then don't annotate the lvar so that IR2
;;; conversion knows not to emit any code, otherwise annotate as an
;;; ordinary lvar. Since we only use a conditional template if the
;;; call immediately precedes the IF node in the same block, we know
;;; that any predicate will already be annotated.
(defun ltn-analyze-if (node)
  (declare (type cif node))
  (setf (node-tail-p node) nil)
  (let* ((test (if-test node))
         (use (lvar-uses test)))
    (unless (and (combination-p use)
                 (let ((info (basic-combination-info use)))
                   (and (template-p info)
                        (template-conditional-p info))))
      (annotate-ordinary-lvar test)))
  (values))

;;; If there is a value lvar, then annotate it for unknown values. In
;;; this case, the exit is non-local, since all other exits are
;;; deleted or degenerate by this point.
(defun ltn-analyze-exit (node)
  (setf (node-tail-p node) nil)
  (let ((value (exit-value node)))
    (when value
      (annotate-unknown-values-lvar value)))
  (values))

;;; We need a special method for %UNWIND-PROTECT that ignores the
;;; cleanup function. We don't annotate either arg, since we don't
;;; need them at run-time.
;;;
;;; (The default is o.k. for %CATCH, since environment analysis
;;; converted the reference to the escape function into a constant
;;; reference to the NLX-INFO.)
(defoptimizer (%unwind-protect ltn-annotate) ((escape cleanup)
                                              node
                                              ltn-policy)
  (declare (ignore escape cleanup ltn-policy))
  (setf (basic-combination-info node) :funny)
  (setf (node-tail-p node) nil))

;;; Make sure that arguments of magic functions are not annotated.
;;; (Otherwise the compiler may dump its internal structures as
;;; constants :-()
(defoptimizer (%pop-values ltn-annotate) ((%lvar) node ltn-policy)
  (declare (ignore %lvar node ltn-policy)))
(defoptimizer (%nip-values ltn-annotate) ((last-nipped last-preserved
                                                       &rest moved)
                                          node ltn-policy)
  (declare (ignore last-nipped last-preserved moved node ltn-policy)))
(defoptimizer (%dummy-dx-alloc ltn-annotate) ((target source)
                                              node ltn-policy)
  (declare (ignore target source node ltn-policy)))


;;;; known call annotation

;;; Return true if RESTR is satisfied by TYPE. If T-OK is true, then a
;;; T restriction allows any operand type. This is also called by IR2
;;; translation when it determines whether a result temporary needs to
;;; be made, and by representation selection when it is deciding which
;;; move VOP to use. LVAR and TN are used to test for constant
;;; arguments.
(defun operand-restriction-ok (restr type &key lvar tn (t-ok t))
  (declare (type (or (member *) cons) restr)
           (type primitive-type type)
           (type (or lvar null) lvar)
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
         (cond (lvar
                ;; Can't use constant-lvar-p, because it returns T for
                ;; things for which the derived type is an EQL type,
                ;; but there may be already a variable allocated for
                ;; it, which can cause problems when there's a closure
                ;; over it.
                ;; See  :vop-on-eql-type test in compiler.pure for an example.
                ;;
                ;; And the value is already loaded into a register,
                ;; which is usually cheaper/more compactly encoded
                ;; than a constant.
                (and (strictly-constant-lvar-p lvar)
                     (funcall (second restr) (lvar-value lvar))))
               (tn
                (and (eq (tn-kind tn) :constant)
                     (funcall (second restr) (tn-value tn))))
               (t
                (error "Neither LVAR nor TN supplied.")))))))

;;; Check that the argument type restriction for TEMPLATE are
;;; satisfied in call. If an argument's TYPE-CHECK is :NO-CHECK and
;;; our policy is safe, then only :SAFE templates are OK.
(defun template-args-ok (template call safe-p)
  (declare (type template template)
           (type combination call))
  (declare (ignore safe-p))
  (let ((mtype (template-more-args-type template)))
    (do ((args (basic-combination-args call) (cdr args))
         (types (template-arg-types template) (cdr types)))
        ((null types)
         (cond ((null args) t)
               ((not mtype) nil)
               (t
                (dolist (arg args t)
                  (unless (operand-restriction-ok mtype
                                                  (lvar-ptype arg))
                    (return nil))))))
      (when (null args) (return nil))
      (let ((arg (car args))
            (type (car types)))
        (unless (operand-restriction-ok type (lvar-ptype arg)
                                        :lvar arg)
          (return nil))))))

;;; Check that TEMPLATE can be used with the specifed RESULT-TYPE.
;;; Result type checking is pretty different from argument type
;;; checking due to the relaxed rules for values count. We succeed if
;;; for each required result, there is a positional restriction on the
;;; value that is at least as good. If we run out of result types
;;; before we run out of restrictions, then we only succeed if the
;;; leftover restrictions are *. If we run out of restrictions before
;;; we run out of result types, then we always win.
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

;;; Return true if CALL is an ok use of TEMPLATE according to SAFE-P.
;;; -- If the template has a GUARD that isn't true, then we ignore the
;;;    template, not even considering it to be rejected.
;;; -- If the argument type restrictions aren't satisfied, then we
;;;    reject the template.
;;; -- If the template is :CONDITIONAL, then we accept it only when the
;;;    destination of the value is an immediately following IF node.
;;; -- If either the template is safe or the policy is unsafe (i.e. we
;;;    can believe output assertions), then we test against the
;;;    intersection of the node derived type and the lvar
;;;    asserted type. Otherwise, we just use the node type. If
;;;    TYPE-CHECK is null, there is no point in doing the intersection,
;;;    since the node type must be a subtype of the  assertion.
;;;
;;; If the template is *not* ok, then the second value is a keyword
;;; indicating which aspect failed.
(defun is-ok-template-use (template call safe-p)
  (declare (type template template) (type combination call))
  (let* ((guard (template-guard template))
         (lvar (node-lvar call))
         (dtype (node-derived-type call)))
    (cond ((and guard (not (funcall guard)))
           (values nil :guard))
          ((not (template-args-ok template call safe-p))
           (values nil
                   (if (and safe-p (template-args-ok template call nil))
                       :arg-check
                       :arg-types)))
          ((template-conditional-p template)
           (let ((dest (lvar-dest lvar)))
             (if (and (if-p dest)
                      (immediately-used-p (if-test dest) call))
                 (values t nil)
                 (values nil :conditional))))
          ((template-results-ok template dtype)
           (values t nil))
          (t
           (values nil :result-types)))))

;;; Use operand type information to choose a template from the list
;;; TEMPLATES for a known CALL. We return three values:
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

;;; Given a partially annotated known call and a translation policy,
;;; return the appropriate template, or NIL if none can be found. We
;;; scan the templates (ordered by increasing cost) looking for a
;;; template whose restrictions are satisfied and that has our policy.
;;;
;;; If we find a template that doesn't have our policy, but has a
;;; legal alternate policy, then we also record that to return as a
;;; last resort. If our policy is safe, then only safe policies are
;;; O.K., otherwise anything goes.
;;;
;;; If we find a template with :SAFE policy, then we return it, or any
;;; cheaper fallback template. The theory behind this is that if it is
;;; cheapest, small and safe, we can't lose. If it is not cheapest,
;;; then we use the fallback, which won't have the desired policy, but
;;; :SAFE isn't desired either, so we might as well go with the
;;; cheaper one. The main reason for doing this is to make sure that
;;; cheap safe templates are used when they apply and the current
;;; policy is something else. This is useful because :SAFE has the
;;; additional semantics of implicit argument type checking, so we may
;;; be forced to define a template with :SAFE policy when it is really
;;; small and fast as well.
(defun find-template-for-ltn-policy (call ltn-policy)
  (declare (type combination call)
           (type ltn-policy ltn-policy))
  (let ((safe-p (ltn-policy-safe-p ltn-policy))
        (current (fun-info-templates (basic-combination-fun-info call)))
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
       (let ((tcpolicy (template-ltn-policy template)))
         (cond ((eq tcpolicy ltn-policy)
                (return (values template rejected)))
               ((eq tcpolicy :safe)
                (return (values (or fallback template) rejected)))
               ((or (not safe-p) (eq tcpolicy :fast-safe))
                (unless fallback
                  (setq fallback template)))))))))

(defvar *efficiency-note-limit* 2
  #!+sb-doc
  "This is the maximum number of possible optimization alternatives will be
  mentioned in a particular efficiency note. NIL means no limit.")
(declaim (type (or index null) *efficiency-note-limit*))

(defvar *efficiency-note-cost-threshold* 5
  #!+sb-doc
  "This is the minimum cost difference between the chosen implementation and
  the next alternative that justifies an efficiency note.")
(declaim (type index *efficiency-note-cost-threshold*))

;;; This function is called by NOTE-REJECTED-TEMPLATES when it can't
;;; figure out any reason why TEMPLATE was rejected. Users should
;;; never see these messages, but they can happen in situations where
;;; the VM definition is messed up somehow.
(defun strange-template-failure (template call ltn-policy frob)
  (declare (type template template) (type combination call)
           (type ltn-policy ltn-policy) (type function frob))
  (funcall frob "This shouldn't happen!  Bug?")
  (multiple-value-bind (win why)
      (is-ok-template-use template call (ltn-policy-safe-p ltn-policy))
    (aver (not win))
    (ecase why
      (:guard
       (funcall frob "template guard failed"))
      (:arg-check
       (funcall frob "The template isn't safe, yet we were counting on it."))
      (:arg-types
       (funcall frob "argument types invalid")
       (funcall frob "argument primitive types:~%  ~S"
                (mapcar (lambda (x)
                          (primitive-type-name
                           (lvar-ptype x)))
                        (combination-args call)))
       (funcall frob "argument type assertions:~%  ~S"
                (mapcar (lambda (x)
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

;;; This function emits efficiency notes describing all of the
;;; templates better (faster) than TEMPLATE that we might have been
;;; able to use if there were better type declarations. Template is
;;; null when we didn't find any template, and thus must do a full
;;; call.
;;;
;;; In order to be worth complaining about, a template must:
;;; -- be allowed by its guard,
;;; -- be safe if the current policy is safe,
;;; -- have argument/result type restrictions consistent with the
;;;    known type information, e.g. we don't consider float templates
;;;    when an operand is known to be an integer,
;;; -- be disallowed by the stricter operand subtype test (which
;;;    resembles, but is not identical to the test done by
;;;    FIND-TEMPLATE.)
;;;
;;; Note that there may not be any possibly applicable templates,
;;; since we are called whenever any template is rejected. That
;;; template might have the wrong policy or be inconsistent with the
;;; known type.
;;;
;;; We go to some trouble to make the whole multi-line output into a
;;; single call to COMPILER-NOTIFY so that repeat messages are
;;; suppressed, etc.
(defun note-rejected-templates (call ltn-policy template)
  (declare (type combination call) (type ltn-policy ltn-policy)
           (type (or template null) template))

  (collect ((losers))
    (let ((safe-p (ltn-policy-safe-p ltn-policy))
          (verbose-p (policy call (= inhibit-warnings 0)))
          (max-cost (- (template-cost
                        (or template
                            (template-or-lose 'call-named)))
                       *efficiency-note-cost-threshold*)))
      (dolist (try (fun-info-templates (basic-combination-fun-info call)))
        (when (> (template-cost try) max-cost) (return)) ; FIXME: UNLESS'd be cleaner.
        (let ((guard (template-guard try)))
          (when (and (or (not guard) (funcall guard))
                     (or (not safe-p)
                         (ltn-policy-safe-p (template-ltn-policy try)))
                     ;; :SAFE is also considered to be :SMALL-SAFE,
                     ;; while the template cost describes time cost;
                     ;; so the fact that (< (t-cost try) (t-cost
                     ;; template)) does not mean that TRY is better
                     (not (and (eq ltn-policy :safe)
                               (eq (template-ltn-policy try) :fast-safe)))
                     (or verbose-p
                         (and (template-note try)
                              (valid-fun-use
                               call (template-type try)
                               :argument-test #'types-equal-or-intersect
                               :result-test
                               #'values-types-equal-or-intersect))))
            (losers try)))))

    (when (losers)
      (collect ((messages)
                (notes 0 +))
        (flet ((lose1 (string &rest stuff)
                 (messages string)
                 (messages stuff)))
          (dolist (loser (losers))
            (when (and *efficiency-note-limit*
                       (>= (notes) *efficiency-note-limit*))
              (lose1 "etc.")
              (return))
            (let* ((type (template-type loser))
                   (valid (valid-fun-use call type))
                   (strict-valid (valid-fun-use call type)))
              (lose1 "unable to do ~A (cost ~W) because:"
                     (or (template-note loser) (template-name loser))
                     (template-cost loser))
              (cond
               ((and valid strict-valid)
                (strange-template-failure loser call ltn-policy #'lose1))
               ((not valid)
                (aver (not (valid-fun-use call type
                                          :lossage-fun #'lose1
                                          :unwinnage-fun #'lose1))))
               (t
                (aver (ltn-policy-safe-p ltn-policy))
                (lose1 "can't trust output type assertion under safe policy")))
              (notes 1))))

        (let ((*compiler-error-context* call))
          (compiler-notify "~{~?~^~&~6T~}"
                           (if template
                               `("forced to do ~A (cost ~W)"
                                 (,(or (template-note template)
                                       (template-name template))
                                  ,(template-cost template))
                                 . ,(messages))
                               `("forced to do full call"
                                 nil
                                 . ,(messages))))))))
  (values))

;;; If a function has a special-case annotation method use that,
;;; otherwise annotate the argument lvars and try to find a template
;;; corresponding to the type signature. If there is none, convert a
;;; full call.
(defun ltn-analyze-known-call (call)
  (declare (type combination call))
  (let ((ltn-policy (node-ltn-policy call))
        (method (fun-info-ltn-annotate (basic-combination-fun-info call)))
        (args (basic-combination-args call)))
    (when method
      (funcall method call ltn-policy)
      (return-from ltn-analyze-known-call (values)))

    (dolist (arg args)
      (setf (lvar-info arg)
            (make-ir2-lvar (primitive-type (lvar-type arg)))))

    (multiple-value-bind (template rejected)
        (find-template-for-ltn-policy call ltn-policy)
      ;; If we are unable to use some templates due to unsatisfied
      ;; operand type restrictions and our policy enables efficiency
      ;; notes, then we call NOTE-REJECTED-TEMPLATES.
      (when (and rejected
                 (policy call (> speed inhibit-warnings)))
        (note-rejected-templates call ltn-policy template))
      ;; If we are forced to do a full call, we check to see whether
      ;; the function called is the same as the current function. If
      ;; so, we give a warning, as this is probably a botched attempt
      ;; to implement an out-of-line version in terms of inline
      ;; transforms or VOPs or whatever.
      (unless template
        (when (let ((funleaf (physenv-lambda (node-physenv call))))
                (and (leaf-has-source-name-p funleaf)
                     (eq (lvar-fun-name (combination-fun call))
                         (leaf-source-name funleaf))
                     (let ((info (basic-combination-fun-info call)))
                       (not (or (fun-info-ir2-convert info)
                                (ir1-attributep (fun-info-attributes info)
                                                recursive))))))
          (let ((*compiler-error-context* call))
            (compiler-warn "~@<recursion in known function definition~2I ~
                            ~_policy=~S ~_arg types=~S~:>"
                           (lexenv-policy (node-lexenv call))
                           (mapcar (lambda (arg)
                                     (type-specifier (lvar-type arg)))
                                   args))))
        (ltn-default-call call)
        (return-from ltn-analyze-known-call (values)))
      (setf (basic-combination-info call) template)
      (setf (node-tail-p call) nil)

      (dolist (arg args)
        (annotate-1-value-lvar arg))))

  (values))

;;; CASTs are merely lvar annotations than nodes. So we wait until
;;; value consumer deside how values should be passed, and after that
;;; we propagate this decision backwards through CAST chain. The
;;; exception is a dangling CAST with a type check, which we process
;;; immediately.
(defun ltn-analyze-cast (cast)
  (declare (type cast cast))
  (setf (node-tail-p cast) nil)
  (when (and (cast-type-check cast)
             (not (node-lvar cast)))
    ;; FIXME
    (bug "IR2 type checking of unused values is not implemented.")
    )
  (values))

(defun ltn-annotate-casts (lvar)
  (declare (type lvar lvar))
  (do-uses (node lvar)
    (when (cast-p node)
      (ltn-annotate-cast node))))

(defun ltn-annotate-cast (cast)
  (declare (type cast))
  (let ((2lvar (lvar-info (node-lvar cast)))
        (value (cast-value cast)))
    (aver 2lvar)
    ;; XXX
    (ecase (ir2-lvar-kind 2lvar)
      (:unknown
       (annotate-unknown-values-lvar value))
      (:fixed
       (let* ((count (length (ir2-lvar-locs 2lvar)))
              (ctype (lvar-derived-type value)))
         (multiple-value-bind (types rest)
             (values-type-types ctype (specifier-type 'null))
           (annotate-fixed-values-lvar
            value
            (mapcar #'primitive-type
                    (adjust-list types count rest))))))))
  (values))


;;;; interfaces

;;; most of the guts of the two interface functions: Compute the
;;; policy and dispatch to the appropriate node-specific function.
;;;
;;; Note: we deliberately don't use the DO-NODES macro, since the
;;; block can be split out from underneath us, and DO-NODES would scan
;;; past the block end in that case.
(defun ltn-analyze-block (block)
  (do* ((node (block-start-node block)
              (ctran-next ctran))
        (ctran (node-next node) (node-next node)))
      (nil)
    (etypecase node
      (ref)
      (combination
       (ecase (basic-combination-kind node)
         (:local (ltn-analyze-local-call node))
         ((:full :error) (ltn-default-call node))
         (:known
          (ltn-analyze-known-call node))))
      (cif (ltn-analyze-if node))
      (creturn (ltn-analyze-return node))
      ((or bind entry))
      (exit (ltn-analyze-exit node))
      (cset (ltn-analyze-set node))
      (cast (ltn-analyze-cast node))
      (mv-combination
       (ecase (basic-combination-kind node)
         (:local
          (ltn-analyze-mv-bind node))
         ((:full :error)
          (ltn-analyze-mv-call node)))))
    (when (eq node (block-last block))
      (return))))

;;; Loop over the blocks in COMPONENT, doing stuff to nodes that
;;; receive values. In addition to the stuff done by FROB, we also see
;;; whether there are any unknown values receivers, making notations
;;; in the components' GENERATORS and RECEIVERS as appropriate.
;;;
;;; If any unknown-values lvars are received by this block (as
;;; indicated by IR2-BLOCK-POPPED), then we add the block to the
;;; IR2-COMPONENT-VALUES-RECEIVERS.
;;;
;;; This is where we allocate IR2 blocks because it is the first place
;;; we need them.
(defun ltn-analyze (component)
  (declare (type component component))
  (let ((2comp (component-info component)))
    (do-blocks (block component)
      ;; Set up the IR2 blocks in a separate pass, because CAST nodes
      ;; could be out-of-order with respect to their result LVAR
      ;; DESTs, and we need their IR1 blocks to have associated IR2
      ;; blocks.
      (aver (not (block-info block)))
      (setf (block-info block) (make-ir2-block block)))
    (do-blocks (block component)
      (ltn-analyze-block block))
    (do-blocks (block component)
      (let ((2block (block-info block)))
        (let ((popped (ir2-block-popped 2block)))
          (when popped
            (push block (ir2-component-values-receivers 2comp)))))))
  (values))

;;; This function is used to analyze blocks that must be added to the
;;; flow graph after the normal LTN phase runs. Such code is
;;; constrained not to use weird unknown values (and probably in lots
;;; of other ways).
(defun ltn-analyze-belated-block (block)
  (declare (type cblock block))
  (ltn-analyze-block block)
  (aver (not (ir2-block-popped (block-info block))))
  (values))

