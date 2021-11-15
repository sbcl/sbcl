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

(in-package "SB-C")

;;; This phase runs before IR2 conversion, initializing each XEP's
;;; ENTRY-INFO structure.  If there was a forward reference to a function,
;;; then the ENTRY-INFO will already exist, but will be uninitialized.
(defun entry-analyze (component)
  (let ((2comp (component-info component)))
    (dolist (fun (component-lambdas component))
      (when (xep-p fun)
        (let ((info (or (leaf-info fun)
                        (setf (leaf-info fun) (make-entry-info)))))
          (compute-entry-info fun info)
          (push info (ir2-component-entries 2comp))))))
  (values))

;;; An "effectively" null environment captures at most
;;; a compilation policy, nothing more.
;;; This is to make the 2nd value of FUNCTION-LAMBDA-EXPRESSION
;;; accurate; but is it really important to do that? Nah.
#+nil
(defun effectively-null-lexenv-p (lexenv)
  (or (null-lexenv-p lexenv)
      (and (not (or (lexenv-funs lexenv)
                    (lexenv-vars lexenv)
                    (lexenv-blocks lexenv)
                    (lexenv-tags lexenv)
                    (lexenv-type-restrictions lexenv)
                    (lexenv-lambda lexenv)
                    (lexenv-cleanup lexenv)
                    (lexenv-handled-conditions lexenv)
                    (lexenv-disabled-package-locks lexenv)
                    (lexenv-user-data lexenv)))
           (or (not (lexenv-parent lexenv))
               (null-lexenv-p (lexenv-parent lexenv))))))

;;; Initialize INFO structure to correspond to the XEP LAMBDA FUN.
(defun compute-entry-info (fun info)
  (declare (type clambda fun) (type entry-info info))
  (let ((bind (lambda-bind fun))
        (internal-fun (functional-entry-fun fun)))
    (setf (entry-info-closure-tn info)
          (if (environment-closure (lambda-environment fun))
              (make-normal-tn *backend-t-primitive-type*)
              nil))
    (setf (entry-info-offset info) (gen-label))
    (setf (entry-info-name info)
          (leaf-debug-name internal-fun))
    (setf (entry-info-xref info) (pack-xref-data (functional-xref internal-fun)))
    (let* ((inline-expansion (functional-inline-expansion internal-fun))
           (form  (if (fasl-output-p *compile-object*)
                      ;; If compiling to a file, we only store sources if the STORE-SOURCE
                      ;; quality value is 3. If to memory, any nonzero value will do.
                      (and (policy bind (= store-source-form 3))
                           ;; Downgrade the error to a warning if this was signaled
                           ;; by SB-PCL::DONT-KNOW-HOW-TO-DUMP.
                           ;; If not that, let the error propagate.
                           (block nil
                             (handler-bind
                                 ((compiler-error
                                   ;; Everything about error handling sucks so badly.
                                   ;; Why turn a perfectly good error into a different
                                   ;; and not necessarily better error?
                                   (lambda (e &aux (c (encapsulated-condition e)))
                                     ;; And why the heck isn't this just NO-APPLICABLE-METHOD
                                     ;; on MAKE-LOAD-FORM?  Why did we choose to further obfuscate
                                     ;; a condition that was reflectable and instead turn it
                                     ;; into a dumb text string?
                                     (if (and (typep c 'simple-error)
                                              (stringp (simple-condition-format-control c))
                                              (search "know how to dump"
                                                      (simple-condition-format-control c)))
                                         ;; This might be worth a full warning. Dunno.
                                         ;; After all, the user asked to do what can't be done.
                                         (compiler-style-warn
                                        "Can't preserve function source - ~
missing MAKE-LOAD-FORM methods?")
                                         (compiler-style-warn
                                          "Can't preserve function source: ~A"
                                          (princ-to-string c)))
                                       (return nil))))
                               (constant-value (find-constant inline-expansion)))))
                      (and (policy bind (> store-source-form 0))
                           inline-expansion)))
           (doc (functional-documentation internal-fun)))
      (setf (entry-info-form/doc info)
            (if (and form doc) (cons form doc) (or form doc))))
    (when (policy bind (>= debug 1))
      (let ((args (functional-arg-documentation internal-fun)))
        ;; When the component is dumped, the arglists of the entry
        ;; points will be dumped.  If they contain values that need
        ;; make-load-form processing then we need to do it now (bug
        ;; 310132).
        (setf (entry-info-arguments info)
              (constant-value (find-constant args))))
      ;; Arguably we should not parse/unparse if the type was obtained from
      ;; a proclamation. On the other hand, this preserves exact semantics
      ;; if a later DEFTYPE changes something. Be that as it may, storing
      ;; just <X> instead of (VALUES <X> &OPTIONAL) saves 6 words per entry.
      (let ((spec (type-specifier (leaf-type internal-fun)))
            (result))
        (setf (entry-info-type info)
              (constant-value
               (find-constant
                (if (and (listp spec)
                         (typep (setq result (third spec))
                                '(cons (eql values)
                                  (cons t (cons (eql &optional) null)))))
                    `(sfunction ,(cadr spec) ,(cadr result))
                    spec)))))))
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
                  (main-entry (main-entry ef))
                  (closure (and
                            ;; It may have been deleted due to none of
                            ;; the optional entries reaching it.
                            (neq (functional-kind main-entry) :deleted)
                            (environment-closure (lambda-environment main-entry)))))
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
                              (delq1 ref (leaf-refs lambda))))))))))
        (:toplevel
         (setq res t))))
    res))
