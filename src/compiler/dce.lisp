;;;; This file implements a dead-code elimination phase for the
;;;; compiler.  We perform a flow-sensitive analysis of a component
;;;; from a set of externally-referenced CLAMBDAs, finding all
;;;; reachable blocks, and delete any unreferenced CLAMBDAs and
;;;; blocks.

(in-package "SB-C")

;;; A CLAMBDA is deemed to be "externally referenced" if:
;;;   - It is of KIND :TOPLEVEL (a toplevel CLAMBDA).
;;;   - It is LAMBDA-HAS-EXTERNAL-REFERENCES-P true (from COMPILE
;;;     or from the fopcompiler, possibly other causes).
;;;   - It has a REF which has a NODE-COMPONENT other than the
;;;     LAMBDA-COMPONENT of the CLAMBDA.
;;;
;;; Arranging for CLAMBDAs of KIND :TOPLEVEL to be set
;;; LAMBDA-HAS-EXTERNAL-REFERENCES-P true is trivial, but doesn't gain
;;; us overmuch.  Arranging for the REF-based check to be cached or
;;; optimistically computed might gain us more, but is not trivial to
;;; implement.
(defun lambda-externally-referenced-p (clambda)
  (or (lambda-has-external-references-p clambda)
      (eq (lambda-kind clambda) :toplevel)
      (let ((home-component (lambda-component clambda)))
        (some (lambda (ref)
                (not (eq (node-component ref)
                         home-component)))
              (lambda-refs clambda)))))

(defun dce-analyze-ref (ref)
  (let ((leaf (ref-leaf ref)))
    (typecase leaf
      (clambda
       ;; If a CLAMBDA points to this component, mark its blocks as
       ;; being live.
       ;;
       ;; FLUSH-DEAD-CODE is supposed to have killed :ZOMBIE CLAMBDAs
       ;; (see commentary on DELETE-LET in IR1OPT), but may not have
       ;; run yet, or may not have been able to kill the CLAMBDA in
       ;; question, but the LAMBDA-BIND would be NIL, so just ignore
       ;; :DELETED and :ZOMBIE CLAMBDAs here.
       (unless (member (functional-kind leaf)
                       '(:deleted :zombie
                         ;; Follow actual combinations, not refs
                         :mv-let :let :assignment))
         (when (eq (lambda-component leaf)
                   (node-component ref))
           (dce-analyze-one-fun leaf))))
      ;; KLUDGE: Pick off CONSTANTs that have an NLX-INFO as the value
      ;; in order to find the NLX entry blocks.  Should probably be
      ;; checking for COMBINATIONs of %UNWIND-PROTECT and %CATCH
      ;; instead.
      (constant
       (let ((value (constant-value leaf)))
         (when (and (nlx-info-p value)
                    (nlx-info-target value))
           (dce-analyze-block (nlx-info-target value))))))))

(defun dce-analyze-block (block)
  (unless (block-flag block)
    (setf (block-flag block) t)

    (do-nodes (node nil block)
      (typecase node
        (ref
         (dce-analyze-ref node))
        (basic-combination
         (when (eq (basic-combination-kind node) :local)
           (let ((fun (ref-leaf (lvar-use (basic-combination-fun node)))))
             (unless (memq (functional-kind fun) '(:deleted :zombie))
               (dce-analyze-one-fun fun)))))))
    (loop for (succ . next) on (block-succ block)
          if next
          do (dce-analyze-block succ)
          else
          ;; Tail call the last block
          return (dce-analyze-block succ))))

(defun dce-analyze-optional-dispatch (optional-dispatch)
  (flet ((analyze (fun)
           (when (and fun
                      (not (memq (functional-kind fun) '(:deleted :zombie))))
             (dce-analyze-one-fun fun))))
    (loop for fun in (optional-dispatch-entry-points optional-dispatch)
          do (analyze (and (promise-ready-p fun)
                           (force fun))))
    (analyze (optional-dispatch-more-entry optional-dispatch))))

(defun dce-analyze-one-fun (clambda)
  (when (and (eq (functional-kind clambda) :external)
             (optional-dispatch-p (functional-entry-fun clambda)))
    (dce-analyze-optional-dispatch (functional-entry-fun clambda)))
  (dce-analyze-block
   (node-block
    (lambda-bind clambda))))

(defun eliminate-dead-code (component)
  (clear-flags component)

  (dolist (fun (component-lambdas component))
    (when (lambda-externally-referenced-p fun)
      (dce-analyze-one-fun fun)))

  ;; For reasons that I completely fail to ascertain, simply calling
  ;; DELETE-BLOCK directly messes up FIND-DFO, as does calling
  ;; DELETE-BLOCK-LAZILY followed by a CLEAN-COMPONENT, but calling
  ;; DELETE-BLOCK-LAZILY followed by FIND-DFO seems to work.  -- AJB,
  ;; 2014-Jun-08
  (do-blocks (block component)
    (unless (block-flag block)
      (delete-block-lazily block)))
  (find-dfo component))

;;; Run before find-initial-dfo
(defun initial-eliminate-dead-code (lambdas)
  (loop for lambda in lambdas
        for component = (lambda-component lambda)
        do
        (dolist (fun (component-lambdas component))
          (when (lambda-externally-referenced-p fun)
            (dce-analyze-one-fun fun))))

  (loop for lambda in lambdas
        for component = (lambda-component lambda)
        do
        (do-blocks (block component)
          (unless (block-flag block)
            (delete-block-lazily block))))

  (loop for lambda in lambdas
        for component = (lambda-component lambda)
        do
        (clear-flags component)))
