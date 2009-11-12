;;;; Code in this file handles VM-independent details of run-time
;;;; function representation that primarily concern IR2 conversion and
;;;; the dumper/loader.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;; This phase runs before IR2 conversion, initializing each XEP's
;;; ENTRY-INFO structure. We call the VM-supplied
;;; SELECT-COMPONENT-FORMAT function to make VM-dependent
;;; initializations in the IR2-COMPONENT. This includes setting the
;;; IR2-COMPONENT-KIND and allocating fixed implementation overhead in
;;; the constant pool. If there was a forward reference to a function,
;;; then the ENTRY-INFO will already exist, but will be uninitialized.
(defun entry-analyze (component)
  (let ((2comp (component-info component)))
    (dolist (fun (component-lambdas component))
      (when (xep-p fun)
        (let ((info (or (leaf-info fun)
                        (setf (leaf-info fun) (make-entry-info)))))
          (compute-entry-info fun info)
          (push info (ir2-component-entries 2comp))))))
  (select-component-format component)
  (values))

;;; Initialize INFO structure to correspond to the XEP LAMBDA FUN.
(defun compute-entry-info (fun info)
  (declare (type clambda fun) (type entry-info info))
  (let ((bind (lambda-bind fun))
        (internal-fun (functional-entry-fun fun)))
    (setf (entry-info-closure-tn info)
          (if (physenv-closure (lambda-physenv fun))
              (make-normal-tn *backend-t-primitive-type*)
              nil))
    (setf (entry-info-offset info) (gen-label))
    (setf (entry-info-name info)
          (leaf-debug-name internal-fun))
    (let ((doc (functional-documentation internal-fun))
          (xrefs (pack-xref-data (functional-xref internal-fun))))
      (setf (entry-info-info info) (if (and doc xrefs)
                                       (cons doc xrefs)
                                       (or doc xrefs))))
    (when (policy bind (>= debug 1))
      (let ((args (functional-arg-documentation internal-fun)))
        (aver (not (eq args :unspecified)))
        ;; When the component is dumped, the arglists of the entry
        ;; points will be dumped.  If they contain values that need
        ;; make-load-form processing then we need to do it now (bug
        ;; 310132).
        (setf (entry-info-arguments info)
              (constant-value (find-constant args))))
      (setf (entry-info-type info) (type-specifier (leaf-type internal-fun)))))
  (values))

;;; Replace all references to COMPONENT's non-closure XEPs that appear
;;; in top level or externally-referenced components, changing to
;;; :TOPLEVEL-XEP FUNCTIONALs. If the cross-component ref is not in a
;;; :TOPLEVEL/externally-referenced component, or is to a closure,
;;; then substitution is suppressed.
;;;
;;; When a cross-component ref is not substituted, we return T to
;;; indicate that early deletion of this component's IR1 should not be
;;; done. We also return T if this component contains
;;; :TOPLEVEL/externally-referenced lambdas (though it is not a
;;; :TOPLEVEL component.)
;;;
;;; We deliberately don't use the normal reference deletion, since we
;;; don't want to trigger deletion of the XEP (although it shouldn't
;;; hurt, since this is called after COMPONENT is compiled.) Instead,
;;; we just clobber the REF-LEAF.
(defun replace-toplevel-xeps (component)
  (let ((res nil))
    (dolist (lambda (component-lambdas component))
      (case (functional-kind lambda)
        (:external
         (unless (lambda-has-external-references-p lambda)
           (let* ((ef (functional-entry-fun lambda))
                  (new (make-functional
                        :kind :toplevel-xep
                        :info (leaf-info lambda)
                        :%source-name (functional-%source-name ef)
                        :%debug-name (functional-%debug-name ef)
                        :lexenv (make-null-lexenv)))
                  (closure (physenv-closure
                            (lambda-physenv (main-entry ef)))))
             (dolist (ref (leaf-refs lambda))
               (let ((ref-component (node-component ref)))
                 (cond ((eq ref-component component))
                       ((or (not (component-toplevelish-p ref-component))
                            closure)
                        (setq res t))
                       (t
                        (setf (ref-leaf ref) new)
                        (push ref (leaf-refs new))
                        (setf (leaf-refs lambda)
                              (delq ref (leaf-refs lambda))))))))))
        (:toplevel
         (setq res t))))
    res))
