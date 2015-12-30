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

;;; Determine whether we should emit a single-stepper breakpoint
;;; around a call / before a vop.
(defun emit-step-p (node)
  (if (and (policy node (> insert-step-conditions 1))
           (typep node 'combination))
      (combination-step-info node)
      nil))

;;; Allocate an indirect value cell.
(defevent make-value-cell-event "Allocate heap value cell for lexical var.")
(defun emit-make-value-cell (node block value res)
  (event make-value-cell-event node)
  (vop make-value-cell node block value nil res))

;;;; leaf reference

;;; Return the TN that holds the value of THING in the environment ENV.
(declaim (ftype (function ((or nlx-info lambda-var clambda) physenv) tn)
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
         (ir2-nlx-info-home (nlx-info-info thing)))
        (clambda
         (aver (xep-p thing))
         (entry-info-closure-tn (lambda-info thing))))
      (bug "~@<~2I~_~S ~_not found in ~_~S~:>" thing physenv)))

;;; If LEAF already has a constant TN, return that, otherwise make a
;;; TN for it.
(defun constant-tn (leaf boxedp)
  (declare (type constant leaf))
  ;; When convenient we can have both a boxed and unboxed TN for
  ;; constant.
  (if boxedp
      (or (constant-boxed-tn leaf)
          (setf (constant-boxed-tn leaf) (make-constant-tn leaf t)))
      (or (leaf-info leaf)
          (setf (leaf-info leaf) (make-constant-tn leaf nil)))))

;;; Return a TN that represents the value of LEAF, or NIL if LEAF
;;; isn't directly represented by a TN. ENV is the environment that
;;; the reference is done in.
(defun leaf-tn (leaf env boxedp)
  (declare (type leaf leaf) (type physenv env))
  (typecase leaf
    (lambda-var
     (unless (lambda-var-indirect leaf)
       (find-in-physenv leaf env)))
    (constant (constant-tn leaf boxedp))
    (t nil)))

;;; This is used to conveniently get a handle on a constant TN during
;;; IR2 conversion. It returns a constant TN representing the Lisp
;;; object VALUE.
(defun emit-constant (value)
  (constant-tn (find-constant value) t))

(defun %return-is-boxed (node)
  (declare (type creturn node))
  (let* ((fun (return-lambda node))
         (returns (tail-set-info (lambda-tail-set fun))))
    (or (xep-p fun)
        (eq (return-info-kind returns) :unknown))))

(defun boxed-ref-p (ref)
  (let ((dest (lvar-dest (ref-lvar ref))))
    (cond ((and (basic-combination-p dest)
                (call-full-like-p dest))
           t)
          ((and (return-p dest) (%return-is-boxed dest)))
          (t
           nil))))

;;; Convert a REF node. The reference must not be delayed.
(defun ir2-convert-ref (node block)
  (declare (type ref node) (type ir2-block block))
  (let* ((lvar (node-lvar node))
         (leaf (ref-leaf node))
         (locs (lvar-result-tns
                lvar (list (primitive-type (leaf-type leaf)))))
         (res (first locs)))
    (etypecase leaf
      (lambda-var
       (let ((tn (find-in-physenv leaf (node-physenv node)))
             (indirect (lambda-var-indirect leaf))
             (explicit (lambda-var-explicit-value-cell leaf)))
         (cond
          ((and indirect explicit)
           (vop value-cell-ref node block tn res))
          ((and indirect
                (not (eq (node-physenv node)
                         (lambda-physenv (lambda-var-home leaf)))))
           (let ((reffer (third (primitive-type-indirect-cell-type
                                 (primitive-type (leaf-type leaf))))))
             (if reffer
                 (funcall reffer node block tn (leaf-info leaf) res)
                 (vop ancestor-frame-ref node block tn (leaf-info leaf) res))))
          (t (emit-move node block tn res)))))
      (constant
       (emit-move node block (constant-tn leaf (boxed-ref-p node)) res))
      (functional
       (ir2-convert-closure node block leaf res))
      (global-var
       (ir2-convert-global-var node block leaf res)))
    (move-lvar-result node block locs lvar))
  (values))

(defun ir2-convert-global-var (node block leaf res)
  (let ((unsafe (policy node (zerop safety)))
        (name (leaf-source-name leaf)))
    (ecase (global-var-kind leaf)
      ((:special :unknown)
       (aver (symbolp name))
       (let ((name-tn (emit-constant name)))
         (if (or unsafe (always-boundp name))
             (vop fast-symbol-value node block name-tn res)
             (vop symbol-value node block name-tn res))))
      (:global
       (aver (symbolp name))
       (let ((name-tn (emit-constant name)))
         (if (or unsafe (always-boundp name))
             (vop fast-symbol-global-value node block name-tn res)
             (vop symbol-global-value node block name-tn res))))
      (:global-function
       ;; In cross-compilation, testing (INFO :function :definition) is not
       ;; sensible (or possible) but we can assume that things with fun-info
       ;; will eventually be defined. If that's untrue, e.g. if we referred
       ;; to #'DESCRIBE during cold-load, we'd just fix it locally by declaring
       ;; DESCRIBE notinline.
       ;; But in the target, more caution is warranted because users might
       ;; DEFKNOWN a function but fail to define it. And they shouldn't be
       ;; expected to understand the failure mode and the remedy.
       (cond ((and #-sb-xc-host (info :function :definition name)
                   (info :function :info name)
                   (let ((*lexenv* (node-lexenv node)))
                     (not (fun-lexically-notinline-p name))))
              ;; Known functions can be dumped without going through fdefns.
              ;; But if NOTINLINEd, don't early-bind to the functional value
              ;; because that disallows redefinition, including but not limited
              ;; to encapsulations, which in turn makes TRACE not work, which
              ;; leads to extreme frustration when debugging.
              (emit-move node block (make-load-time-constant-tn :known-fun name)
                         res))
             (t
              (let ((fdefn-tn (make-load-time-constant-tn :fdefinition name)))
                (if unsafe
                    (vop fdefn-fun node block fdefn-tn res)
                    (vop safe-fdefn-fun node block fdefn-tn res)))))))))

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
  (flet ((prepare ()
           (aver (not (eql (functional-kind functional) :deleted)))
           (unless (leaf-info functional)
             (setf (leaf-info functional)
                   (make-entry-info :name
                                    (functional-debug-name functional))))))
    (let ((closure (etypecase functional
                     (clambda
                      (assertions-on-ir2-converted-clambda functional)
                      (physenv-closure (get-lambda-physenv functional)))
                     (functional
                      (aver (eq (functional-kind functional) :toplevel-xep))
                      nil)))
          global-var)
      (cond (closure
             (prepare)
             (let* ((physenv (node-physenv ref))
                    (tn (find-in-physenv functional physenv)))
               (emit-move ref ir2-block tn res)))
            ;; we're about to emit a reference to a "closure" that's actually
            ;; an inlinable global function.
            ((and (global-var-p (setf global-var
                                      (functional-inline-expanded functional)))
                  (eq :global-function (global-var-kind global-var)))
             (ir2-convert-global-var ref ir2-block global-var res))
            (t
             ;; if we're here, we should have either a toplevel-xep (some
             ;; global scope function in a different component) or an external
             ;; reference to the "closure"'s body.
             (prepare)
             (aver (memq (functional-kind functional) '(:external :toplevel-xep)))
             (let ((entry (make-load-time-constant-tn :entry functional)))
               (emit-move ref ir2-block entry res))))))
  (values))

(defun closure-initial-value (what this-env current-fp)
  (declare (type (or nlx-info lambda-var clambda) what)
           (type physenv this-env)
           (type (or tn null) current-fp))
  ;; If we have an indirect LAMBDA-VAR that does not require an
  ;; EXPLICIT-VALUE-CELL, and is from this environment (not from being
  ;; closed over), we need to store the current frame pointer.
  (if (and (lambda-var-p what)
           (lambda-var-indirect what)
           (not (lambda-var-explicit-value-cell what))
           (eq (lambda-physenv (lambda-var-home what))
               this-env))
    current-fp
    (find-in-physenv what this-env)))

(defoptimizer (%allocate-closures ltn-annotate) ((leaves) node ltn-policy)
  (declare (ignore ltn-policy))
  (when (lvar-dynamic-extent leaves)
    (let ((info (make-ir2-lvar *backend-t-primitive-type*)))
      (setf (ir2-lvar-kind info) :delayed)
      (setf (lvar-info leaves) info)
      (setf (ir2-lvar-stack-pointer info)
            (make-stack-pointer-tn)))))

(defoptimizer (%allocate-closures ir2-convert) ((leaves) call 2block)
  (let ((dx-p (lvar-dynamic-extent leaves)))
    (collect ((delayed))
      (when dx-p
        (vop current-stack-pointer call 2block
             (ir2-lvar-stack-pointer (lvar-info leaves))))
      (dolist (leaf (lvar-value leaves))
        (binding* ((xep (awhen (functional-entry-fun leaf)
                          ;; if the xep's been deleted then we can skip it
                          (if (eq (functional-kind it) :deleted)
                              nil it))
                        :exit-if-null)
                   (nil (aver (xep-p xep)))
                   (entry-info (lambda-info xep) :exit-if-null)
                   (tn (entry-info-closure-tn entry-info) :exit-if-null)
                   (closure (physenv-closure (get-lambda-physenv xep)))
                   (entry (make-load-time-constant-tn :entry xep)))
          (let ((this-env (node-physenv call))
                (leaf-dx-p (and dx-p (leaf-dynamic-extent leaf))))
            (vop make-closure call 2block entry (length closure)
                 leaf-dx-p tn)
            (loop for what in closure and n from 0 do
                  (unless (and (lambda-var-p what)
                               (null (leaf-refs what)))
                    ;; In LABELS a closure may refer to another closure
                    ;; in the same group, so we must be sure that we
                    ;; store a closure only after its creation.
                    ;;
                    ;; TODO: Here is a simple solution: we postpone
                    ;; putting of all closures after all creations
                    ;; (though it may require more registers).
                    (if (lambda-p what)
                      (delayed (list tn (find-in-physenv what this-env) n))
                      (let ((initial-value (closure-initial-value
                                            what this-env nil)))
                        (if initial-value
                          (vop closure-init call 2block
                               tn initial-value n)
                          ;; An initial-value of NIL means to stash
                          ;; the frame pointer... which requires a
                          ;; different VOP.
                          (vop closure-init-from-fp call 2block tn n)))))))))
      (loop for (tn what n) in (delayed)
            do (vop closure-init call 2block
                    tn what n))))
  (values))

;;; Convert a SET node. If the NODE's LVAR is annotated, then we also
;;; deliver the value to that lvar. If the var is a lexical variable
;;; with no refs, then we don't actually set anything, since the
;;; variable has been deleted.
(defun ir2-convert-set (node block)
  (declare (type cset node) (type ir2-block block))
  (let* ((lvar (node-lvar node))
         (leaf (set-var node))
         (val (lvar-tn node block (set-value node)))
         (locs (if lvar
                   (lvar-result-tns
                    lvar (list (primitive-type (leaf-type leaf))))
                   nil)))
    (etypecase leaf
      (lambda-var
       (when (leaf-refs leaf)
         (let ((tn (find-in-physenv leaf (node-physenv node)))
               (indirect (lambda-var-indirect leaf))
               (explicit (lambda-var-explicit-value-cell leaf)))
           (cond
            ((and indirect explicit)
             (vop value-cell-set node block tn val))
            ((and indirect
                  (not (eq (node-physenv node)
                           (lambda-physenv (lambda-var-home leaf)))))
             (let ((setter (fourth (primitive-type-indirect-cell-type
                                    (primitive-type (leaf-type leaf))))))
             (if setter
                 (funcall setter node block tn val (leaf-info leaf))
                 (vop ancestor-frame-set node block tn val (leaf-info leaf)))))
            (t (emit-move node block val tn))))))
      (global-var
       (aver (symbolp (leaf-source-name leaf)))
       (ecase (global-var-kind leaf)
         ((:special)
          (vop set node block (emit-constant (leaf-source-name leaf)) val))
         ((:global)
          (vop %set-symbol-global-value node
               block (emit-constant (leaf-source-name leaf)) val)))))
    (when locs
      (emit-move node block val (first locs))
      (move-lvar-result node block locs lvar)))
  (values))

;;;; utilities for receiving fixed values

;;; Return a TN that can be referenced to get the value of LVAR. LVAR
;;; must be LTN-ANNOTATED either as a delayed leaf ref or as a fixed,
;;; single-value lvar.
;;;
;;; The primitive-type of the result will always be the same as the
;;; IR2-LVAR-PRIMITIVE-TYPE, ensuring that VOPs are always called with
;;; TNs that satisfy the operand primitive-type restriction. We may
;;; have to make a temporary of the desired type and move the actual
;;; lvar TN into it. This happens when we delete a type check in
;;; unsafe code or when we locally know something about the type of an
;;; argument variable.
(defun lvar-tn (node block lvar)
  (declare (type node node) (type ir2-block block) (type lvar lvar))
  (let* ((2lvar (lvar-info lvar))
         (lvar-tn
          (ecase (ir2-lvar-kind 2lvar)
            (:delayed
             (let ((ref (lvar-uses lvar)))
               (leaf-tn (ref-leaf ref) (node-physenv ref) (boxed-ref-p ref))))
            (:fixed
             (aver (= (length (ir2-lvar-locs 2lvar)) 1))
             (first (ir2-lvar-locs 2lvar)))))
         (ptype (ir2-lvar-primitive-type 2lvar)))

    (cond ((eq (tn-primitive-type lvar-tn) ptype) lvar-tn)
          (t
           (let ((temp (make-normal-tn ptype)))
             (emit-move node block lvar-tn temp)
             temp)))))

;;; This is similar to LVAR-TN, but hacks multiple values. We return
;;; TNs holding the values of LVAR with PTYPES as their primitive
;;; types. LVAR must be annotated for the same number of fixed values
;;; are there are PTYPES.
;;;
;;; If the lvar has a type check, check the values into temps and
;;; return the temps. When we have more values than assertions, we
;;; move the extra values with no check.
(defun lvar-tns (node block lvar ptypes)
  (declare (type node node) (type ir2-block block)
           (type lvar lvar) (list ptypes))
  (let* ((locs (ir2-lvar-locs (lvar-info lvar)))
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

;;;; utilities for delivering values to lvars

;;; Return a list of TNs with the specifier TYPES that can be used as
;;; result TNs to evaluate an expression into LVAR. This is used
;;; together with MOVE-LVAR-RESULT to deliver fixed values to
;;; an lvar.
;;;
;;; If the lvar isn't annotated (meaning the values are discarded) or
;;; is unknown-values, then we make temporaries for each supplied
;;; value, providing a place to compute the result in until we decide
;;; what to do with it (if anything.)
;;;
;;; If the lvar is fixed-values, and wants the same number of values
;;; as the user wants to deliver, then we just return the
;;; IR2-LVAR-LOCS. Otherwise we make a new list padded as necessary by
;;; discarded TNs. We always return a TN of the specified type, using
;;; the lvar locs only when they are of the correct type.
(defun lvar-result-tns (lvar types)
  (declare (type (or lvar null) lvar) (type list types))
  (if (not lvar)
      (mapcar #'make-normal-tn types)
      (let ((2lvar (lvar-info lvar)))
        (ecase (ir2-lvar-kind 2lvar)
          (:fixed
           (let* ((locs (ir2-lvar-locs 2lvar))
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
;;; unknown-values convention. This is used together with
;;; MOVE-LVAR-RESULT for delivering unknown values to a fixed values
;;; lvar.
;;;
;;; If the lvar isn't annotated, then we treat as 0-values, returning
;;; an empty list of temporaries.
;;;
;;; If the lvar is annotated, then it must be :FIXED.
(defun standard-result-tns (lvar)
  (declare (type (or lvar null) lvar))
  (if lvar
      (let ((2lvar (lvar-info lvar)))
        (ecase (ir2-lvar-kind 2lvar)
          (:fixed
           (make-standard-value-tns (length (ir2-lvar-locs 2lvar))))))
      nil))

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

;;; If necessary, emit coercion code needed to deliver the RESULTS to
;;; the specified lvar. NODE and BLOCK provide context for emitting
;;; code. Although usually obtained from STANDARD-RESULT-TNs or
;;; LVAR-RESULT-TNs, RESULTS may be a list of any type or
;;; number of TNs.
;;;
;;; If the lvar is fixed values, then move the results into the lvar
;;; locations. If the lvar is unknown values, then do the moves into
;;; the standard value locations, and use PUSH-VALUES to put the
;;; values on the stack.
(defun move-lvar-result (node block results lvar)
  (declare (type node node) (type ir2-block block)
           (list results) (type (or lvar null) lvar))
  (when lvar
    (let ((2lvar (lvar-info lvar)))
      (ecase (ir2-lvar-kind 2lvar)
        (:fixed
         (let ((locs (ir2-lvar-locs 2lvar)))
           (unless (eq locs results)
             (move-results-coerced node block results locs))))
        (:unknown
         (let* ((nvals (length results))
                (locs (make-standard-value-tns nvals)))
           (move-results-coerced node block results locs)
           (vop* push-values node block
                 ((reference-tn-list locs nil))
                 ((reference-tn-list (ir2-lvar-locs 2lvar) t))
                 nvals))))))
  (values))

;;; CAST
(defun ir2-convert-cast (node block)
  (declare (type cast node)
           (type ir2-block block))
  (binding* ((lvar (node-lvar node) :exit-if-null)
             (2lvar (lvar-info lvar))
             (value (cast-value node))
             (2value (lvar-info value)))
    (ecase (ir2-lvar-kind 2lvar)
      (:unused)
      ((:unknown :fixed)
       (aver (not (cast-type-check node)))
       (move-results-coerced node block
                             (ir2-lvar-locs 2value)
                             (ir2-lvar-locs 2lvar))))))

(defoptimizer (%check-bound ir2-convert)
    ((array bound index) node block)
  (when (constant-lvar-p bound)
    (let* ((bound-type (specifier-type `(integer 0 (,(lvar-value bound)))))
           (index-type (lvar-type index)))
      (when (eq (type-intersection bound-type index-type)
                *empty-type*)
        (let ((*compiler-error-context* node))
          (compiler-warn "Derived type ~s is not a suitable index for ~s."
                         (type-specifier index-type)
                         (type-specifier (lvar-type array)))))))
  (ir2-convert-template node block))

;;;; template conversion

;;; Build a TN-REFS list that represents access to the values of the
;;; specified list of lvars ARGS for TEMPLATE. Any :CONSTANT arguments
;;; are returned in the second value as a list rather than being
;;; accessed as a normal argument. NODE and BLOCK provide the context
;;; for emitting any necessary type-checking code.
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
              (info-args (lvar-value arg))
              (let ((ref (reference-tn (lvar-tn node block arg) nil)))
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
  (let ((consequent (if-consequent if))
        (alternative (if-alternative if))
        (flags       (and (consp (template-result-types template))
                          (rest (template-result-types template)))))
    (aver (= (template-info-arg-count template)
             (+ (length info-args)
                (if flags 0 2))))
    (when not-p
      (rotatef consequent alternative)
      (setf not-p nil))
    (when (drop-thru-p if consequent)
      (rotatef consequent alternative)
      (setf not-p t))
    (cond ((not flags)
           (emit-template node block template args nil
                          (list* (block-label consequent) not-p
                                 info-args))
           (if (drop-thru-p if alternative)
               (register-drop-thru alternative)
               (vop branch node block (block-label alternative))))
          (t
           (emit-template node block template args nil info-args)
           (vop branch-if node block (block-label consequent) flags not-p)
           (if (drop-thru-p if alternative)
               (register-drop-thru alternative)
               (vop branch node block (block-label alternative)))))))

;;; Convert an IF that isn't the DEST of a conditional template.
(defun ir2-convert-if (node block)
  (declare (type ir2-block block) (type cif node))
  (let* ((test (if-test node))
         (test-ref (reference-tn (lvar-tn node block test) nil))
         (nil-ref (reference-tn (emit-constant nil) nil)))
    (setf (tn-ref-across test-ref) nil-ref)
    (ir2-convert-conditional node block (template-or-lose 'if-eq)
                             test-ref () node t)))

;;; Return a list of primitive-types that we can pass to LVAR-RESULT-TNS
;;; describing the result types we want for a template call. We are really
;;; only interested in the number of results required: in normal case
;;; TEMPLATE-RESULTS-OK has already checked them.
(defun find-template-result-types (call rtypes)
  (let* ((type (node-derived-type call))
         (types
          (mapcar #'primitive-type
                  (if (args-type-p type)
                      (append (args-type-required type)
                              (args-type-optional type))
                      (list type))))
         (primitive-t *backend-t-primitive-type*))
    (loop for rtype in rtypes
          for type = (or (pop types) primitive-t)
          collect type)))

;;; Return a list of TNs usable in a CALL to TEMPLATE delivering values to
;;; LVAR. As an efficiency hack, we pick off the common case where the LVAR is
;;; fixed values and has locations that satisfy the result restrictions. This
;;; can fail when there is a type check or a values count mismatch.
(defun make-template-result-tns (call lvar rtypes)
  (declare (type combination call) (type (or lvar null) lvar)
           (list rtypes))
  (let ((2lvar (when lvar (lvar-info lvar))))
    (if (and 2lvar (eq (ir2-lvar-kind 2lvar) :fixed))
        (let ((locs (ir2-lvar-locs 2lvar)))
          (if (and (= (length rtypes) (length locs))
                   (do ((loc locs (cdr loc))
                        (rtypes rtypes (cdr rtypes)))
                       ((null loc) t)
                     (unless (operand-restriction-ok
                              (car rtypes)
                              (tn-primitive-type (car loc))
                              :t-ok nil)
                       (return nil))))
              locs
              (lvar-result-tns
               lvar
               (find-template-result-types call rtypes))))
        (lvar-result-tns
         lvar
         (find-template-result-types call rtypes)))))

;;; Get the operands into TNs, make TN-REFs for them, and then call
;;; the template emit function.
(defun ir2-convert-template (call block)
  (declare (type combination call) (type ir2-block block))
  (let* ((template (combination-info call))
         (lvar (node-lvar call))
         (rtypes (template-result-types template)))
    (multiple-value-bind (args info-args)
        (reference-args call block (combination-args call) template)
      (aver (not (template-more-results-type template)))
      (if (template-conditional-p template)
          (ir2-convert-conditional call block template args info-args
                                   (lvar-dest lvar) nil)
          (let* ((results (make-template-result-tns call lvar rtypes))
                 (r-refs (reference-tn-list results t)))
            (aver (= (length info-args)
                     (template-info-arg-count template)))
            (when (and lvar (lvar-dynamic-extent lvar))
              (vop current-stack-pointer call block
                   (ir2-lvar-stack-pointer (lvar-info lvar))))
            (when (emit-step-p call)
              (vop sb!vm::step-instrument-before-vop call block))
            (if info-args
                (emit-template call block template args r-refs info-args)
                (emit-template call block template args r-refs))
            (move-lvar-result call block results lvar)))))
  (values))

;;; We don't have to do much because operand count checking is done by
;;; IR1 conversion. The only difference between this and the function
;;; case of IR2-CONVERT-TEMPLATE is that there can be codegen-info
;;; arguments.
(defoptimizer (%%primitive ir2-convert) ((template info &rest args) call block)
  (declare (ignore args))
  (let* ((template (lvar-value template))
         (info (lvar-value info))
         (lvar (node-lvar call))
         (rtypes (template-result-types template))
         (results (make-template-result-tns call lvar rtypes))
         (r-refs (reference-tn-list results t)))
    (multiple-value-bind (args info-args)
        (reference-args call block (cddr (combination-args call)) template)
      (aver (not (template-more-results-type template)))
      (aver (not (template-conditional-p template)))
      (aver (null info-args))

      (if info
          (emit-template call block template args r-refs info)
          (emit-template call block template args r-refs))

      (move-lvar-result call block results lvar)))
  (values))

(defoptimizer (%%primitive derive-type) ((template info &rest args))
  (declare (ignore info args))
  (let ((type (template-type (lvar-value template))))
    (if (fun-type-p type)
        (fun-type-returns type)
        *wild-type*)))

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
            (let ((src (lvar-tn node block arg))
                  (dest (leaf-info var)))
              (if (and (lambda-var-indirect var)
                       (lambda-var-explicit-value-cell var))
                  (emit-make-value-cell node block src dest)
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
;;;
;;; CLOSURE-FP is for calling a closure that has "implicit" value
;;; cells (stored in the allocating stack frame), and is the frame
;;; pointer TN to use for values allocated in the outbound stack
;;; frame.  This is distinct from OLD-FP for the specific case of a
;;; tail-local-call.
(defun emit-psetq-moves (node block fun old-fp &optional (closure-fp old-fp))
  (declare (type combination node) (type ir2-block block) (type clambda fun)
           (type (or tn null) old-fp closure-fp))
  (let ((actuals (mapcar (lambda (x)
                           (when x
                             (lvar-tn node block x)))
                         (combination-args node))))
    (collect ((temps)
              (locs))
      (dolist (var (lambda-vars fun))
        (let ((actual (pop actuals))
              (loc (leaf-info var)))
          (when actual
            (cond
             ((and (lambda-var-indirect var)
                   (lambda-var-explicit-value-cell var))
              (let ((temp
                     (make-normal-tn *backend-t-primitive-type*)))
                (emit-make-value-cell node block actual temp)
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
            (temps (closure-initial-value (car thing) this-1env closure-fp))
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
  (let ((this-env (physenv-info (node-physenv node)))
        (current-fp (make-stack-pointer-tn)))
    (multiple-value-bind (temps locs)
        (emit-psetq-moves node block fun
                          (ir2-physenv-old-fp this-env) current-fp)

      ;; If we're about to emit a move from CURRENT-FP then we need to
      ;; initialize it.
      (when (find current-fp temps)
        (vop current-fp node block current-fp))

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
;;; move the results to the lvar's destination.
(defun ir2-convert-local-known-call (node block fun returns lvar start)
  (declare (type node node) (type ir2-block block) (type clambda fun)
           (type return-info returns) (type (or lvar null) lvar)
           (type label start))
  (multiple-value-bind (fp nfp temps arg-locs)
      (ir2-convert-local-call-args node block fun)
    (let ((locs (return-info-locations returns)))
      (vop* known-call-local node block
            (fp nfp (reference-tn-list temps nil))
            ((reference-tn-list locs t))
            arg-locs (physenv-info (lambda-physenv fun)) start)
      (move-lvar-result node block locs lvar)))
  (values))

;;; Handle a non-TR unknown-values local call. We do different things
;;; depending on what kind of values the lvar wants.
;;;
;;; If LVAR is :UNKNOWN, then we use the "multiple-" variant, directly
;;; specifying the lvar's LOCS as the VOP results so that we don't
;;; have to do anything after the call.
;;;
;;; Otherwise, we use STANDARD-RESULT-TNS to get wired result TNs, and
;;; then call MOVE-LVAR-RESULT to do any necessary type checks or
;;; coercions.
(defun ir2-convert-local-unknown-call (node block fun lvar start)
  (declare (type node node) (type ir2-block block) (type clambda fun)
           (type (or lvar null) lvar) (type label start))
  (multiple-value-bind (fp nfp temps arg-locs)
      (ir2-convert-local-call-args node block fun)
    (let ((2lvar (and lvar (lvar-info lvar)))
          (env (physenv-info (lambda-physenv fun)))
          (temp-refs (reference-tn-list temps nil)))
      (if (and 2lvar (eq (ir2-lvar-kind 2lvar) :unknown))
          (vop* multiple-call-local node block (fp nfp temp-refs)
                ((reference-tn-list (ir2-lvar-locs 2lvar) t))
                arg-locs env start)
          (let ((locs (standard-result-tns lvar)))
            (vop* call-local node block
                  (fp nfp temp-refs)
                  ((reference-tn-list locs t))
                  arg-locs env start (length locs))
            (move-lvar-result node block locs lvar)))))
  (values))

;;; Dispatch to the appropriate function, depending on whether we have
;;; a let, tail or normal call. If the function doesn't return, call
;;; it using the unknown-value convention. We could compile it as a
;;; tail call, but that might seem confusing in the debugger.
(defun ir2-convert-local-call (node block)
  (declare (type combination node) (type ir2-block block))
  (let* ((fun (ref-leaf (lvar-uses (basic-combination-fun node))))
         (kind (functional-kind fun)))
    (cond ((eq kind :let)
           (ir2-convert-let node block fun))
          ((eq kind :assignment)
           (ir2-convert-assignment node block fun))
          ((node-tail-p node)
           (ir2-convert-tail-local-call node block fun))
          (t
           (let ((start (block-trampoline (lambda-block fun)))
                 (returns (tail-set-info (lambda-tail-set fun)))
                 (lvar (node-lvar node)))
             (ecase (if returns
                        (return-info-kind returns)
                        :unknown)
               (:unknown
                (ir2-convert-local-unknown-call node block fun lvar start))
               (:fixed
                (ir2-convert-local-known-call node block fun returns
                                              lvar start)))))))
  (values))

;;;; full call

;;; Given a function lvar FUN, return (VALUES TN-TO-CALL NAMED-P),
;;; where TN-TO-CALL is a TN holding the thing that we call NAMED-P is
;;; true if the thing is named (false if it is a function).
;;;
;;; There are two interesting non-named cases:
;;;   -- We know it's a function. No check needed: return the
;;;      lvar LOC.
;;;   -- We don't know what it is.
(defun fun-lvar-tn (node block lvar)
  (declare (ignore node block))
  (declare (type lvar lvar))
  (let ((2lvar (lvar-info lvar)))
    (if (eq (ir2-lvar-kind 2lvar) :delayed)
        (let ((name (lvar-fun-name lvar t)))
          (aver name)
          (values (make-load-time-constant-tn :fdefinition name) t))
        (let* ((locs (ir2-lvar-locs 2lvar))
               (loc (first locs))
               (function-ptype (primitive-type-or-lose 'function)))
          (aver (and (eq (ir2-lvar-kind 2lvar) :fixed)
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
        (emit-move node block (lvar-tn node block (elt args num)) loc)
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
        (fun-lvar-tn node block (basic-combination-fun node))
      (if named
          (vop* tail-call-named node block
                (fun-tn old-fp return-pc pass-refs)
                (nil)
                nargs
                (emit-step-p node))
          (vop* tail-call node block
                (fun-tn old-fp return-pc pass-refs)
                (nil)
                nargs
                (emit-step-p node)))))

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
          (let ((ref (reference-tn (lvar-tn node block (elt args num))
                                   nil)))
            (if last
                (setf (tn-ref-across last) ref)
                (setf first ref))
            (setq last ref)))

        (values fp first (locs) nargs)))))

;;; Do full call when a fixed number of values are desired. We make
;;; STANDARD-RESULT-TNS for our lvar, then deliver the result using
;;; MOVE-LVAR-RESULT. We do named or normal call, as appropriate.
(defun ir2-convert-fixed-full-call (node block)
  (declare (type combination node) (type ir2-block block))
  (multiple-value-bind (fp args arg-locs nargs)
      (ir2-convert-full-call-args node block)
    (let* ((lvar (node-lvar node))
           (locs (standard-result-tns lvar))
           (loc-refs (reference-tn-list locs t))
           (nvals (length locs)))
      (multiple-value-bind (fun-tn named)
          (fun-lvar-tn node block (basic-combination-fun node))
        (if named
            (vop* call-named node block (fp fun-tn args) (loc-refs)
                  arg-locs nargs nvals (emit-step-p node))
            (vop* call node block (fp fun-tn args) (loc-refs)
                  arg-locs nargs nvals (emit-step-p node)))
        (move-lvar-result node block locs lvar))))
  (values))

;;; Do full call when unknown values are desired.
(defun ir2-convert-multiple-full-call (node block)
  (declare (type combination node) (type ir2-block block))
  (multiple-value-bind (fp args arg-locs nargs)
      (ir2-convert-full-call-args node block)
    (let* ((lvar (node-lvar node))
           (locs (ir2-lvar-locs (lvar-info lvar)))
           (loc-refs (reference-tn-list locs t)))
      (multiple-value-bind (fun-tn named)
          (fun-lvar-tn node block (basic-combination-fun node))
        (if named
            (vop* multiple-call-named node block (fp fun-tn args) (loc-refs)
                  arg-locs nargs (emit-step-p node))
            (vop* multiple-call node block (fp fun-tn args) (loc-refs)
                  arg-locs nargs (emit-step-p node))))))
  (values))

;;; stuff to check in PONDER-FULL-CALL
;;;
;;; These came in handy when troubleshooting cold boot after making
;;; major changes in the package structure: various transforms and
;;; VOPs and stuff got attached to the wrong symbol, so that
;;; references to the right symbol were bogusly translated as full
;;; calls instead of primitives, sending the system off into infinite
;;; space. Having a report on all full calls generated makes it easier
;;; to figure out what form caused the problem this time.
(declaim (type (member :minimal :detailed :very-detailed :maximal)
               *track-full-called-fnames*))
(defvar *track-full-called-fnames* :minimal)

;;; Do some checks (and store some notes relevant for future checks)
;;; on a full call:
;;;   * Is this a full call to something we have reason to know should
;;;     never be full called? (Except as of sbcl-0.7.18 or so, we no
;;;     longer try to ensure this behavior when *FAILURE-P* has already
;;;     been detected.)
(defun ponder-full-call (node)
  (let* ((lvar (basic-combination-fun node))
         (fname (lvar-fun-name lvar t)))
    (declare (type (or symbol cons) fname))

    ;; Warn about cross-compiling certain full-calls,
    ;; as it is indicative of dependency order problems.
    #+sb-xc-host
    (let ((compname (component-name (node-component node))))
      ;; Don't care too much about macro performance.
      (unless (and (stringp compname) (string/= compname "DEF!MACRO"))
        ;; Catch FOO and (SETF FOO) both.
        (let ((stem (if (atom fname) fname (second fname))))
          (when (member stem
                        sb-cold::*full-calls-to-warn-about*
                        :test #'string=)
            (warn "Full call to ~S" fname)))))

    (let* ((inlineable-p (not (let ((*lexenv* (node-lexenv node)))
                                (fun-lexically-notinline-p fname))))
           (inlineable-bit (if inlineable-p 1 0))
           (cell (info :function :emitted-full-calls fname)))
      (if (not cell)
          ;; The low bit indicates whether any not-NOTINLINE call was seen.
          ;; The next-lowest bit is magic. Refer to %COMPILER-DEFMACRO
          ;; and WARN-IF-INLINE-FAILED/CALL for the pertinent logic.
          (setf cell (list (logior 4 inlineable-bit))
                (info :function :emitted-full-calls fname) cell)
          (incf (car cell) (+ 4 (if (oddp (car cell)) 0 inlineable-bit))))
      ;; If the full call was wanted, don't record anything.
      ;; (This was originally for debugging SBCL self-compilation)
      (when inlineable-p
        (unless *failure-p*
          (warn-if-inline-failed/call fname (node-lexenv node) cell))
        (case *track-full-called-fnames*
          (:detailed
           (when (boundp 'sb!xc:*compile-file-pathname*)
             (pushnew sb!xc:*compile-file-pathname* (cdr cell)
                      :test #'equal)))
          (:very-detailed
           (pushnew (component-name *component-being-compiled*)
                    (cdr cell) :test #'equalp)))))

    ;; Special mode, usually only for the cross-compiler
    ;; and only with the feature enabled.
    #!+sb-show (when (eq *track-full-called-fnames* :maximal)
                 (/show "converting full call to named function" fname)
                 (/show (basic-combination-args node))
                 (/show (policy node speed) (policy node safety))
                 (/show (policy node compilation-speed))
                 (let ((arg-types (mapcar (lambda (lvar)
                                            (when lvar
                                              (type-specifier
                                               (lvar-type lvar))))
                                          (basic-combination-args node))))
                   (/show arg-types)))

    ;; When illegal code is compiled, all sorts of perverse paths
    ;; through the compiler can be taken, and it's much harder -- and
    ;; probably pointless -- to guarantee that always-optimized-away
    ;; functions are actually optimized away. Thus, we skip the check
    ;; in that case.
    (unless *failure-p*
      ;; check to see if we know anything about the function
      (let ((info (info :function :info fname)))
        ;; if we know something, check to see if the full call was valid
        (when (and info (ir1-attributep (fun-info-attributes info)
                                        always-translatable))
          (/show (policy node speed) (policy node safety))
          (/show (policy node compilation-speed))
          (bug "full call to ~S" fname))))

    (when (consp fname)
      (aver (legal-fun-name-p fname))))) ;; FIXME: needless check?

;;; If the call is in a tail recursive position and the return
;;; convention is standard, then do a tail full call. If one or fewer
;;; values are desired, then use a single-value call, otherwise use a
;;; multiple-values call.
(defun ir2-convert-full-call (node block)
  (declare (type combination node) (type ir2-block block))
  (ponder-full-call node)
  (cond ((node-tail-p node)
         (ir2-convert-tail-full-call node block))
        ((let ((lvar (node-lvar node)))
           (and lvar
                (eq (ir2-lvar-kind (lvar-info lvar)) :unknown)))
         (ir2-convert-multiple-full-call node block))
        (t
         (ir2-convert-fixed-full-call node block)))
  (values))

;;;; entering functions

#!+precise-arg-count-error
(defun xep-verify-arg-count (node block fun arg-count-location)
  (when (policy fun (plusp verify-arg-count))
    (let* ((ef (functional-entry-fun fun))
           (optional (optional-dispatch-p ef))
           (min (and optional
                     (optional-dispatch-min-args ef)))
           (max (cond ((not optional)
                       (1- (length (lambda-vars fun))))
                      ((and optional
                            (not (optional-dispatch-more-entry ef)))
                       (optional-dispatch-max-args ef)))))
      (unless (and (eql min 0) (not max))
        (vop verify-arg-count node block
             arg-count-location
             min
             max)))))

;;; Do all the stuff that needs to be done on XEP entry:
;;; -- Create frame.
;;; -- Copy any more arg.
;;; -- Set up the environment, accessing any closure variables.
;;; -- Move args from the standard passing locations to their internal
;;;    locations.
(defun init-xep-environment (node block fun)
  (declare (type bind node) (type ir2-block block) (type clambda fun))
  (let ((start-label (entry-info-offset (leaf-info fun)))
        (env (physenv-info (node-physenv node)))
        arg-count-tn)
    (let ((ef (functional-entry-fun fun)))
      (vop xep-allocate-frame node block start-label)
      ;; Arg verification needs to be done before the stack pointer is adjusted
      ;; so that the extra arguments are still present when the error is signalled
      (unless (eq (functional-kind fun) :toplevel)
        (setf arg-count-tn (make-arg-count-location))
        #!+precise-arg-count-error
        (xep-verify-arg-count node block fun arg-count-tn))
      (cond ((and (optional-dispatch-p ef) (optional-dispatch-more-entry ef))
             ;; COPY-MORE-ARG should handle SP adjustemnt, but it
             ;; isn't done on all targets.
             #!-precise-arg-count-error
             (vop xep-setup-sp node block)
             (vop copy-more-arg node block (optional-dispatch-max-args ef)))
            (t
             (vop xep-setup-sp node block)))
      (when (ir2-physenv-closure env)
        (let ((closure (make-normal-tn *backend-t-primitive-type*)))
          (when (policy fun (> store-closure-debug-pointer 1))
            ;; Save the closure pointer on the stack.
            (let ((closure-save (make-representation-tn
                                 *backend-t-primitive-type*
                                 (sc-number-or-lose 'sb!vm::control-stack))))
              (vop setup-closure-environment node block start-label
                   closure-save)
              (setf (ir2-physenv-closure-save-tn env) closure-save)
              (component-live-tn closure-save)))
          (vop setup-closure-environment node block start-label closure)
          (let ((n -1))
            (dolist (loc (ir2-physenv-closure env))
              (vop closure-ref node block closure (incf n) (cdr loc)))))))
    (unless (eq (functional-kind fun) :toplevel)
      (let ((vars (lambda-vars fun))
            (n 0))
        (when (leaf-refs (first vars))
          (emit-move node block arg-count-tn (leaf-info (first vars))))
        (dolist (arg (rest vars))
          (when (leaf-refs arg)
            (let ((pass (standard-arg-location n))
                  (home (leaf-info arg)))
              (if (and (lambda-var-indirect arg)
                       (lambda-var-explicit-value-cell arg))
                  (emit-make-value-cell node block pass home)
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

#!+unwind-to-frame-and-call-vop
(defun save-bsp (node block env)
  ;; Save BSP on stack so that the binding environment can be restored
  ;; when restarting frames.
  ;; This is done inside functions, which leaves XEPs without saved
  ;; BSP, though the code in XEPs doesn't bind any variables, it can
  ;; call arbitrary code through the SATISFIES declaration.
  ;; And functions called by SATISFIES are not inlined, except for
  ;; source transforms, but these usually do not bind anything.
  ;; Thus when restarting it needs to check that the interrupt was in
  ;; the XEP itself.
  ;;
  ;; It could be saved from the XEP, but some functions have both
  ;; external and internal entry points, so it will be saved twice.
  (let ((temp (make-normal-tn *backend-t-primitive-type*))
        (bsp-save-tn (make-representation-tn
                      *backend-t-primitive-type*
                      (sc-number-or-lose 'sb!vm::control-stack))))
    (vop current-binding-pointer node block temp)
    (emit-move node block temp bsp-save-tn)
    (setf (ir2-physenv-bsp-save-tn env) bsp-save-tn)
    (component-live-tn bsp-save-tn)))

(defun ir2-convert-bind (node block)
  (declare (type bind node) (type ir2-block block))
  (let* ((fun (bind-lambda node))
         (env (physenv-info (lambda-physenv fun))))
    (aver (member (functional-kind fun)
                  '(nil :external :optional :toplevel :cleanup)))

    (cond ((xep-p fun)
           (init-xep-environment node block fun)
           #!+sb-dyncount
           (when *collect-dynamic-statistics*
             (vop count-me node block *dynamic-counts-tn*
                  (block-number (ir2-block-block block)))))
          ((policy fun (> store-closure-debug-pointer 1))
           ;; Propagate the location of the closure pointer from the
           ;; enclosing functions. (FIXME: Should make sure that this
           ;; handles closures inside closures correctly). [remark by JES]
           (let* ((entry-fun (lambda-entry-fun fun)))
             (when entry-fun
               (let ((2env (physenv-info (lambda-physenv fun)))
                     (entry-2env (physenv-info (lambda-physenv entry-fun))))
                 (setf (ir2-physenv-closure-save-tn 2env)
                       (ir2-physenv-closure-save-tn entry-2env)))))))

    (emit-move node
               block
               (ir2-physenv-return-pc-pass env)
               (ir2-physenv-return-pc env))
    #!+unwind-to-frame-and-call-vop
    (when (and (lambda-allow-instrumenting fun)
               (not (lambda-inline-expanded fun))
               (policy fun (>= insert-debug-catch 1)))
      (save-bsp node block env))

    (let ((lab (gen-label)))
      (setf (ir2-physenv-environment-start env) lab)
      (vop note-environment-start node block lab)
      #!+sb-safepoint
      (unless (policy fun (>= inhibit-safepoints 2))
        (vop sb!vm::insert-safepoint node block))))

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
  (let* ((lvar (return-result node))
         (2lvar (lvar-info lvar))
         (lvar-kind (ir2-lvar-kind 2lvar))
         (fun (return-lambda node))
         (env (physenv-info (lambda-physenv fun)))
         (old-fp (ir2-physenv-old-fp env))
         (return-pc (ir2-physenv-return-pc env))
         (returns (tail-set-info (lambda-tail-set fun))))
    (cond
     ((and (eq (return-info-kind returns) :fixed)
           (not (xep-p fun)))
      (let ((locs (lvar-tns node block lvar
                                    (return-info-types returns))))
        (vop* known-return node block
              (old-fp return-pc (reference-tn-list locs nil))
              (nil)
              (return-info-locations returns))))
     ((eq lvar-kind :fixed)
      (let* ((types (mapcar #'tn-primitive-type (ir2-lvar-locs 2lvar)))
             (lvar-locs (lvar-tns node block lvar types))
             (nvals (length lvar-locs))
             (locs (make-standard-value-tns nvals)))
        (mapc (lambda (val loc)
                (emit-move node block val loc))
              lvar-locs
              locs)
        (if (= nvals 1)
            (vop return-single node block old-fp return-pc (car locs))
            (vop* return node block
                  (old-fp return-pc (reference-tn-list locs nil))
                  (nil)
                  nvals))))
     (t
      (aver (eq lvar-kind :unknown))
      (vop* return-multiple node block
            (old-fp return-pc
                    (reference-tn-list (ir2-lvar-locs 2lvar) nil))
            (nil)))))

  (values))

;;;; debugger hooks
;;;;
;;;; These are used by the debugger to find the top function on the
;;;; stack. They return the OLD-FP and RETURN-PC for the current
;;;; function as multiple values.

(defoptimizer (%caller-frame ir2-convert) (() node block)
  (let ((ir2-physenv (physenv-info (node-physenv node))))
    (move-lvar-result node block
                      (list (ir2-physenv-old-fp ir2-physenv))
                      (node-lvar node))))

(defoptimizer (%caller-pc ir2-convert) (() node block)
  (let ((ir2-physenv (physenv-info (node-physenv node))))
    (move-lvar-result node block
                      (list (ir2-physenv-return-pc ir2-physenv))
                      (node-lvar node))))

;;;; multiple values

;;; This is almost identical to IR2-CONVERT-LET. Since LTN annotates
;;; the lvar for the correct number of values (with the lvar user
;;; responsible for defaulting), we can just pick them up from the
;;; lvar.
(defun ir2-convert-mv-bind (node block)
  (declare (type mv-combination node) (type ir2-block block))
  (let* ((lvar (first (basic-combination-args node)))
         (fun (ref-leaf (lvar-uses (basic-combination-fun node))))
         (vars (lambda-vars fun)))
    (aver (eq (functional-kind fun) :mv-let))
    (mapc (lambda (src var)
            (when (leaf-refs var)
              (let ((dest (leaf-info var)))
                (if (and (lambda-var-indirect var)
                         (lambda-var-explicit-value-cell var))
                    (emit-make-value-cell node block src dest)
                    (emit-move node block src dest)))))
          (lvar-tns node block lvar
                            (mapcar (lambda (x)
                                      (primitive-type (leaf-type x)))
                                    vars))
          vars))
  (values))

;;; Emit the appropriate fixed value, unknown value or tail variant of
;;; CALL-VARIABLE. Note that we only need to pass the values start for
;;; the first argument: all the other argument lvar TNs are
;;; ignored. This is because we require all of the values globs to be
;;; contiguous and on stack top.
(defun ir2-convert-mv-call (node block)
  (declare (type mv-combination node) (type ir2-block block))
  (aver (basic-combination-args node))
  (let* ((start-lvar (lvar-info (first (basic-combination-args node))))
         (start (first (ir2-lvar-locs start-lvar)))
         (tails (and (node-tail-p node)
                     (lambda-tail-set (node-home-lambda node))))
         (lvar (node-lvar node))
         (2lvar (and lvar (lvar-info lvar))))
    (multiple-value-bind (fun named)
        (fun-lvar-tn node block (basic-combination-fun node))
      (aver (and (not named)
                 (eq (ir2-lvar-kind start-lvar) :unknown)))
      (cond
       (tails
        (let ((env (physenv-info (node-physenv node))))
          (vop tail-call-variable node block start fun
               (ir2-physenv-old-fp env)
               (ir2-physenv-return-pc env))))
       ((and 2lvar
             (eq (ir2-lvar-kind 2lvar) :unknown))
        (vop* multiple-call-variable node block (start fun nil)
              ((reference-tn-list (ir2-lvar-locs 2lvar) t))
              (emit-step-p node)))
       (t
        (let ((locs (standard-result-tns lvar)))
          (vop* call-variable node block (start fun nil)
                ((reference-tn-list locs t)) (length locs)
                (emit-step-p node))
          (move-lvar-result node block locs lvar)))))))

;;; Reset the stack pointer to the start of the specified
;;; unknown-values lvar (discarding it and all values globs on top of
;;; it.)
(defoptimizer (%pop-values ir2-convert) ((%lvar) node block)
  (let* ((lvar (lvar-value %lvar))
         (2lvar (lvar-info lvar)))
    (cond ((eq (ir2-lvar-kind 2lvar) :unknown)
           (vop reset-stack-pointer node block
                (first (ir2-lvar-locs 2lvar))))
          ((lvar-dynamic-extent lvar)
           (vop reset-stack-pointer node block
                (ir2-lvar-stack-pointer 2lvar)))
          (t (bug "Trying to pop a not stack-allocated LVAR ~S."
                  lvar)))))

(defoptimizer (%nip-values ir2-convert) ((last-nipped last-preserved
                                                      &rest moved)
                                         node block)
  (let* ( ;; pointer immediately after the nipped block
         (after (lvar-value last-nipped))
         (2after (lvar-info after))
         ;; pointer to the first nipped word
         (first (lvar-value last-preserved))
         (2first (lvar-info first))

         (moved-tns (loop for lvar-ref in moved
                          for lvar = (lvar-value lvar-ref)
                          for 2lvar = (lvar-info lvar)
                                        ;when 2lvar
                          collect (first (ir2-lvar-locs 2lvar)))))
    (aver (or (eq (ir2-lvar-kind 2after) :unknown)
              (lvar-dynamic-extent after)))
    (aver (eq (ir2-lvar-kind 2first) :unknown))
    (when *check-consistency*
      ;; we cannot move stack-allocated DX objects
      (dolist (moved-lvar moved)
        (aver (eq (ir2-lvar-kind (lvar-info (lvar-value moved-lvar)))
                  :unknown))))
    (flet ((nip-aligned (nipped)
             (vop* %%nip-values node block
                   (nipped
                    (first (ir2-lvar-locs 2first))
                    (reference-tn-list moved-tns nil))
                   ((reference-tn-list moved-tns t)))))
      (cond ((eq (ir2-lvar-kind 2after) :unknown)
             (nip-aligned (first (ir2-lvar-locs 2after))))
            ((lvar-dynamic-extent after)
             (nip-aligned (ir2-lvar-stack-pointer 2after)))
            (t
             (bug "Trying to nip a not stack-allocated LVAR ~S." after))))))

(defoptimizer (%dummy-dx-alloc ir2-convert) ((target source) node block)
  (let* ((target-lvar (lvar-value target))
         (source-lvar (lvar-value source))
         (target-2lvar (lvar-info target-lvar))
         (source-2lvar (and source-lvar (lvar-info source-lvar))))
    (aver (lvar-dynamic-extent target-lvar))
    (cond ((not source-lvar)
           (vop current-stack-pointer node block
                (ir2-lvar-stack-pointer target-2lvar)))
          ((lvar-dynamic-extent source-lvar)
           (emit-move node block
                      (ir2-lvar-stack-pointer source-2lvar)
                      (ir2-lvar-stack-pointer target-2lvar)))
          ((eq (ir2-lvar-kind source-2lvar) :unknown)
           (emit-move node block
                      (first (ir2-lvar-locs source-2lvar))
                      (ir2-lvar-stack-pointer target-2lvar)))
          (t (bug "Trying to dummy up DX allocation from a ~
not stack-allocated LVAR ~S." source-lvar)))))

;;; Deliver the values TNs to LVAR using MOVE-LVAR-RESULT.
(defoptimizer (values ir2-convert) ((&rest values) node block)
  (let ((tns (mapcar (lambda (x)
                       (lvar-tn node block x))
                     values)))
    (move-lvar-result node block tns (node-lvar node))))

;;; In the normal case where unknown values are desired, we use the
;;; VALUES-LIST VOP. In the relatively unimportant case of VALUES-LIST
;;; for a fixed number of values, we punt by doing a full call to the
;;; VALUES-LIST function. This gets the full call VOP to deal with
;;; defaulting any unsupplied values. It seems unworthwhile to
;;; optimize this case.
(defoptimizer (values-list ir2-convert) ((list) node block)
  (let* ((lvar (node-lvar node))
         (2lvar (and lvar (lvar-info lvar))))
    (cond ((and 2lvar
                (eq (ir2-lvar-kind 2lvar) :unknown))
           (let ((locs (ir2-lvar-locs 2lvar)))
             (vop* values-list node block
                   ((lvar-tn node block list) nil)
                   ((reference-tn-list locs t)))))
          (t (aver (or (not 2lvar) ; i.e. we want to check the argument
                       (eq (ir2-lvar-kind 2lvar) :fixed)))
             (ir2-convert-full-call node block)))))

(defoptimizer (%more-arg-values ir2-convert) ((context start count) node block)
  (binding* ((lvar (node-lvar node) :exit-if-null)
             (2lvar (lvar-info lvar)))
    (ecase (ir2-lvar-kind 2lvar)
      (:fixed
       ;; KLUDGE: this is very much unsafe, and can leak random stack values.
       ;; OTOH, I think the :FIXED case can only happen with (safety 0) in the
       ;; first place.
       ;;  -PK
       (loop for loc in (ir2-lvar-locs 2lvar)
             for idx upfrom 0
             do (vop sb!vm::more-arg node block
                     (lvar-tn node block context)
                     (emit-constant idx)
                     loc)))
      (:unknown
       (let ((locs (ir2-lvar-locs 2lvar)))
         (vop* %more-arg-values node block
               ((lvar-tn node block context)
                (lvar-tn node block start)
                (lvar-tn node block count)
                nil)
               ((reference-tn-list locs t))))))))

;;;; special binding

;;; This is trivial, given our assumption of a shallow-binding
;;; implementation.
(defoptimizer (%special-bind ir2-convert) ((var value) node block)
  (let ((name (leaf-source-name (lvar-value var))))
    #!-(and sb-thread x86-64)
    (vop bind node block (lvar-tn node block value) (emit-constant name))
    #!+(and sb-thread x86-64)
    (progn
      ;; GC must understand that the symbol is implicitly live even though
      ;; binding makes no references to the object.
      (emit-constant name)
      (vop sb!vm::bind/let node block (lvar-tn node block value) name))))

(defoptimizer (%special-unbind ir2-convert) ((var) node block)
  (declare (ignore var))
  (vop unbind node block))

;;; ### It's not clear that this really belongs in this file, or
;;; should really be done this way, but this is the least violation of
;;; abstraction in the current setup. We don't want to wire
;;; shallow-binding assumptions into IR1tran.
(def-ir1-translator progv
    ((vars vals &body body) start next result)
  (ir1-convert
   start next result
   (with-unique-names (bind unbind)
     (once-only ((n-save-bs '(%primitive current-binding-pointer)))
       `(unwind-protect
             (progn
               (labels ((,unbind (vars)
                          (declare (optimize (speed 2) (debug 0)))
                          (let ((unbound-marker (%primitive make-unbound-marker)))
                            (dolist (var vars)
                              ;; CLHS says "bound and then made to have no value" -- user
                              ;; should not be able to tell the difference between that and this.
                              (about-to-modify-symbol-value var 'progv)
                              (%primitive bind unbound-marker var))))
                        (,bind (vars vals)
                          (declare (optimize (speed 2) (debug 0)
                                             (insert-debug-catch 0)))
                          (cond ((null vars))
                                ((null vals) (,unbind vars))
                                (t
                                 (let ((val (car vals))
                                       (var (car vars)))
                                   (about-to-modify-symbol-value var 'progv val t)
                                   (%primitive bind val var))
                                 (,bind (cdr vars) (cdr vals))))))
                 (,bind ,vars ,vals))
               nil
               ,@body)
          ;; Technically ANSI CL doesn't allow declarations at the
          ;; start of the cleanup form. SBCL happens to allow for
          ;; them, due to the way the UNWIND-PROTECT ir1 translation
          ;; is implemented; the cleanup forms are directly spliced
          ;; into an FLET definition body. And a declaration here
          ;; actually has exactly the right scope for what we need
          ;; (ensure that debug instrumentation is not emitted for the
          ;; cleanup function). -- JES, 2007-06-16
          (declare (optimize (insert-debug-catch 0)))
          (%primitive unbind-to-here ,n-save-bs))))))

;;;; non-local exit

;;; Convert a non-local lexical exit. First find the NLX-INFO in our
;;; environment. Note that this is never called on the escape exits
;;; for CATCH and UNWIND-PROTECT, since the escape functions aren't
;;; IR2 converted.
(defun ir2-convert-exit (node block)
  (declare (type exit node) (type ir2-block block))
  (let* ((nlx (exit-nlx-info node))
         (loc (find-in-physenv nlx (node-physenv node)))
         (temp (make-stack-pointer-tn))
         (value (exit-value node)))
    (if (nlx-info-safe-p nlx)
        (vop value-cell-ref node block loc temp)
        (emit-move node block loc temp))
    (if value
        (let ((locs (ir2-lvar-locs (lvar-info value))))
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
  (let ((nlx (lvar-value info)))
    (when (nlx-info-safe-p nlx)
      (vop value-cell-set node block
           (find-in-physenv nlx (node-physenv node))
           (emit-constant 0)))))

;;; We have to do a spurious move of no values to the result lvar so
;;; that lifetime analysis won't get confused.
(defun ir2-convert-throw (node block)
  (declare (type mv-combination node) (type ir2-block block))
  (let ((args (basic-combination-args node)))
    (check-catch-tag-type (first args))
    (vop* throw node block
          ((lvar-tn node block (first args))
           (reference-tn-list
            (ir2-lvar-locs (lvar-info (second args)))
            nil))
          (nil)))
  (move-lvar-result node block () (node-lvar node))
  (values))

;;; Emit code to set up a non-local exit. INFO is the NLX-INFO for the
;;; exit, and TAG is the lvar for the catch tag (if any.) We get at
;;; the target PC by passing in the label to the vop. The vop is
;;; responsible for building a return-PC object.
(defun emit-nlx-start (node block info tag)
  (declare (type node node) (type ir2-block block) (type nlx-info info)
           (type (or lvar null) tag))
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
            (lvar-tn node block tag) target-label res))
      ((:unwind-protect :block :tagbody)
       (vop make-unwind-block node block block-tn target-label res)))

    (ecase kind
      ((:block :tagbody)
       (if (nlx-info-safe-p info)
           (emit-make-value-cell node block res (ir2-nlx-info-home 2info))
           (emit-move node block res (ir2-nlx-info-home 2info))))
      (:unwind-protect
       (vop set-unwind-protect node block block-tn))
      (:catch)))

  (values))

;;; Scan each of ENTRY's exits, setting up the exit for each lexical exit.
(defun ir2-convert-entry (node block)
  (declare (type entry node) (type ir2-block block))
  (let ((nlxes '()))
    (dolist (exit (entry-exits node))
      (let ((info (exit-nlx-info exit)))
        (when (and info
                   (not (memq info nlxes))
                   (member (cleanup-kind (nlx-info-cleanup info))
                           '(:block :tagbody)))
          (push info nlxes)
          (emit-nlx-start node block info nil)))))
  (values))

;;; Set up the unwind block for these guys.
(defoptimizer (%catch ir2-convert) ((info-lvar tag) node block)
  (check-catch-tag-type tag)
  (emit-nlx-start node block (lvar-value info-lvar) tag))
(defoptimizer (%unwind-protect ir2-convert) ((info-lvar cleanup) node block)
  (declare (ignore cleanup))
  (emit-nlx-start node block (lvar-value info-lvar) nil))

;;; Emit the entry code for a non-local exit. We receive values and
;;; restore dynamic state.
;;;
;;; In the case of a lexical exit or CATCH, we look at the exit lvar's
;;; kind to determine which flavor of entry VOP to emit. If unknown
;;; values, emit the xxx-MULTIPLE variant to the lvar locs. If fixed
;;; values, make the appropriate number of temps in the standard
;;; values locations and use the other variant, delivering the temps
;;; to the lvar using MOVE-LVAR-RESULT.
;;;
;;; In the UNWIND-PROTECT case, we deliver the first register
;;; argument, the argument count and the argument pointer to our lvar
;;; as multiple values. These values are the block exited to and the
;;; values start and count.
;;;
;;; After receiving values, we restore dynamic state. Except in the
;;; UNWIND-PROTECT case, the values receiving restores the stack
;;; pointer. In an UNWIND-PROTECT cleanup, we want to leave the stack
;;; pointer alone, since the thrown values are still out there.
(defoptimizer (%nlx-entry ir2-convert) ((info-lvar) node block)
  (let* ((info (lvar-value info-lvar))
         (lvar (node-lvar node))
         (2info (nlx-info-info info))
         (top-loc (ir2-nlx-info-save-sp 2info))
         (start-loc (make-nlx-entry-arg-start-location))
         (count-loc (make-arg-count-location))
         (target (ir2-nlx-info-target 2info)))

    (ecase (cleanup-kind (nlx-info-cleanup info))
      ((:catch :block :tagbody)
       (let ((2lvar (and lvar (lvar-info lvar))))
         (if (and 2lvar (eq (ir2-lvar-kind 2lvar) :unknown))
             (vop* nlx-entry-multiple node block
                   (top-loc start-loc count-loc nil)
                   ((reference-tn-list (ir2-lvar-locs 2lvar) t))
                   target)
             (let ((locs (standard-result-tns lvar)))
               (vop* nlx-entry node block
                     (top-loc start-loc count-loc nil)
                     ((reference-tn-list locs t))
                     target
                     (length locs))
               (move-lvar-result node block locs lvar)))))
      (:unwind-protect
       (let ((block-loc (standard-arg-location 0)))
         (vop uwp-entry node block target block-loc start-loc count-loc)
         (move-lvar-result
          node block
          (list block-loc start-loc count-loc)
          lvar))))

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
                (cond #!+gencgc
                      ((>= (length args)
                           (/ sb!vm:large-object-size
                              (* sb!vm:n-word-bytes 2)))
                       ;; The VOPs will try to allocate all space at once
                       ;; And it'll end up in large objects, and no conses
                       ;; are welcome there.
                       (ir2-convert-full-call node block))
                      (t
                       (let* ((refs (reference-tn-list
                                     (loop for arg in args
                                           for tn = (make-normal-tn *backend-t-primitive-type*)
                                           do
                                           (emit-move node block (lvar-tn node block arg) tn)
                                           collect tn)
                                     nil))
                              (lvar (node-lvar node))
                              (res (lvar-result-tns
                                    lvar
                                    (list (primitive-type (specifier-type 'list))))))
                         (when (and lvar (lvar-dynamic-extent lvar))
                           (vop current-stack-pointer node block
                                (ir2-lvar-stack-pointer (lvar-info lvar))))
                         (vop* ,name node block (refs) ((first res) nil)
                               (length args))
                         (move-lvar-result node block res lvar)))))))
  (def list)
  (def list*))


(defoptimizer (mask-signed-field ir2-convert) ((width x) node block)
  (block nil
    (when (constant-lvar-p width)
      (case (lvar-value width)
        (#.(- sb!vm:n-word-bits sb!vm:n-fixnum-tag-bits)
         (when (or (csubtypep (lvar-type x)
                              (specifier-type 'word))
                   (csubtypep (lvar-type x)
                              (specifier-type 'sb!vm:signed-word)))
           (let* ((lvar (node-lvar node))
                  (temp (make-normal-tn
                         (if (csubtypep (lvar-type x)
                                        (specifier-type 'word))
                             (primitive-type-of most-positive-word)
                             (primitive-type-of
                              (- (ash most-positive-word -1))))))
                  (results (lvar-result-tns
                            lvar
                            (list (primitive-type-or-lose 'fixnum)))))
             (emit-move node block (lvar-tn node block x) temp)
             (vop sb!vm::move-from-word/fixnum node block
                  temp (first results))
             (move-lvar-result node block results lvar)
             (return))))
        (#.sb!vm:n-word-bits
         (when (csubtypep (lvar-type x) (specifier-type 'word))
           (let* ((lvar (node-lvar node))
                  (temp (make-normal-tn
                         (primitive-type-of most-positive-word)))
                  (results (lvar-result-tns
                            lvar
                            (list (primitive-type
                                   (specifier-type 'sb!vm:signed-word))))))
             (emit-move node block (lvar-tn node block x) temp)
             (vop sb!vm::word-move node block
                  temp (first results))
             (move-lvar-result node block results lvar)
             (return))))))
    (if (template-p (basic-combination-info node))
        (ir2-convert-template node block)
        (ir2-convert-full-call node block))))

;; just a fancy identity
(defoptimizer (%typep-wrapper ir2-convert) ((value variable type) node block)
  (declare (ignore variable type))
  (let* ((lvar (node-lvar node))
         (results (lvar-result-tns lvar (list (primitive-type-or-lose t)))))
    (emit-move node block (lvar-tn node block value) (first results))
    (move-lvar-result node block results lvar)))

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
              (let ((first-node (block-start-node block)))
                (unless (or (and (bind-p first-node)
                                 (xep-p (bind-lambda first-node)))
                            (eq (lvar-fun-name
                                 (node-lvar first-node))
                                '%nlx-entry))
                  (vop count-me
                       first-node
                       2block
                       #!+sb-dyncount *dynamic-counts-tn* #!-sb-dyncount nil
                       num))))
              #!+sb-safepoint
              (let ((first-node (block-start-node block)))
                (unless (or (and (bind-p first-node)
                                 ;; Bind-nodes already have safepoints
                                 (eq (bind-lambda first-node)
                                     (lambda-home (bind-lambda first-node))))
                            (and (valued-node-p first-node)
                                 (node-lvar first-node)
                                 (eq (lvar-fun-name
                                      (node-lvar first-node))
                                     '%nlx-entry)))
                  (when (and (rest (block-pred block))
                             (block-loop block)
                             (member (loop-kind (block-loop block))
                                     '(:natural :strange))
                             (eq block (loop-head (block-loop block)))
                             (policy first-node (< inhibit-safepoints 2)))
                    (vop sb!vm::insert-safepoint first-node 2block))))
            (ir2-convert-block block)
            (incf num))))))
  (values))

;;; If necessary, emit a terminal unconditional branch to go to the
;;; successor block. If the successor is the component tail, then
;;; there isn't really any successor, but if the end is a non-tail
;;; call to a function that's not *known* to never return, then we
;;; emit an error trap just in case the function really does return.
;;;
;;; Trapping after known calls makes it easier to understand type
;;; derivation bugs at runtime: they show up as nil-fun-returned-error,
;;; rather than the execution of arbitrary code or error traps.
(defun finish-ir2-block (block)
  (declare (type cblock block))
  (let* ((2block (block-info block))
         (last (block-last block))
         (succ (block-succ block)))
    (unless (if-p last)
      (aver (singleton-p succ))
      (let ((target (first succ)))
        (cond ((eq target (component-tail (block-component block)))
               (when (and (basic-combination-p last)
                          (or (eq (basic-combination-kind last) :full)
                              (and (eq (basic-combination-kind last) :known)
                                   (eq (basic-combination-info last) :full))))
                 (let* ((fun (basic-combination-fun last))
                        (use (lvar-uses fun))
                        (name (and (ref-p use)
                                   (leaf-has-source-name-p (ref-leaf use))
                                   (leaf-source-name (ref-leaf use))))
                        (ftype (and (info :function :info name) ; only use the FTYPE if
                                    (proclaimed-ftype name)))) ; NAME was DEFKNOWN
                   (unless (or (node-tail-p last)
                               (policy last (zerop safety))
                               (and (fun-type-p ftype)
                                    (eq *empty-type* (fun-type-returns ftype))))
                     (vop nil-fun-returned-error last 2block
                          (if name
                              (emit-constant name)
                              (multiple-value-bind (tn named)
                                  (fun-lvar-tn last 2block fun)
                                (aver (not named))
                                tn)))))))
              ((not (eq (ir2-block-next 2block) (block-info target)))
               (vop branch last 2block (block-label target)))
              (t
               (register-drop-thru target))))))

  (values))

;;; Convert the code in a block into VOPs.
(defun ir2-convert-block (block)
  (declare (type cblock block))
  (let ((2block (block-info block)))
    (do-nodes (node lvar block)
      (etypecase node
        (ref
         (when lvar
           (let ((2lvar (lvar-info lvar)))
             ;; function REF in a local call is not annotated
             (when (and 2lvar (not (eq (ir2-lvar-kind 2lvar) :delayed)))
               (ir2-convert-ref node 2block)))))
        (combination
         (let ((kind (basic-combination-kind node)))
           (ecase kind
             (:local
              (ir2-convert-local-call node 2block))
             (:full
              (ir2-convert-full-call node 2block))
             (:known
              (let* ((info (basic-combination-fun-info node))
                     (fun (fun-info-ir2-convert info)))
                (cond (fun
                       (funcall fun node 2block))
                      ((eq (basic-combination-info node) :full)
                       (ir2-convert-full-call node 2block))
                      (t
                       (ir2-convert-template node 2block))))))))
        (cif
         (when (lvar-info (if-test node))
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
           ((eq (lvar-fun-name (basic-combination-fun node))
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
