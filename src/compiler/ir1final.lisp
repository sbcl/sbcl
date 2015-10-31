;;;; This file implements the IR1 finalize phase, which checks for
;;;; various semantic errors.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;; Give the user grief about optimizations that we weren't able to
;;; do. It is assumed that the user wants to hear about this, or there
;;; wouldn't be any entries in the table. If the node has been deleted
;;; or is no longer a known call, then do nothing; some other
;;; optimization must have gotten to it.
(defun note-failed-optimization (node failures)
  (declare (type combination node) (list failures))
  (unless (or (node-deleted node)
              (not (eq :known (combination-kind node))))
    (let ((*compiler-error-context* node))
      (dolist (failure failures)
        (let ((what (cdr failure))
              (note (transform-note (car failure))))
          (cond
           ((consp what)
            (compiler-notify "~@<unable to ~2I~_~A ~I~_because: ~2I~_~?~:>"
                             note (first what) (rest what)))
           ((valid-fun-use node what
                           :argument-test #'types-equal-or-intersect
                           :result-test #'values-types-equal-or-intersect)
            (collect ((messages))
              (flet ((give-grief (string &rest stuff)
                       (messages string)
                       (messages stuff)))
                (valid-fun-use node what
                               :unwinnage-fun #'give-grief
                               :lossage-fun #'give-grief))
              (compiler-notify "~@<unable to ~
                                ~2I~_~A ~
                                ~I~_due to type uncertainty: ~
                                ~2I~_~{~?~^~@:_~}~:>"
                             note (messages))))
           ;; As best I can guess, it's OK to fall off the end here
           ;; because if it's not a VALID-FUNCTION-USE, the user
           ;; doesn't want to hear about it. The things I caught when
           ;; I put ERROR "internal error: unexpected FAILURE=~S" here
           ;; didn't look like things we need to report. -- WHN 2001-02-07
           ))))))

;;; For each named function with an XEP, note the definition of that
;;; name, and add derived type information to the INFO environment. We
;;; also delete the FUNCTIONAL from *FREE-FUNS* to eliminate the
;;; possibility that new references might be converted to it.
(defun finalize-xep-definition (fun)
  (let* ((leaf (functional-entry-fun fun))
         (defined-ftype (definition-type leaf)))
    (setf (leaf-type leaf) defined-ftype)
    (when (and (leaf-has-source-name-p leaf)
               (eq (leaf-source-name leaf) (functional-debug-name leaf)))
      (let ((source-name (leaf-source-name leaf)))
        (let* ((where (info :function :where-from source-name))
               (*compiler-error-context* (lambda-bind (main-entry leaf)))
               (global-def (gethash source-name *free-funs*))
               (global-p (defined-fun-p global-def)))
          (note-name-defined source-name :function)
          (when global-p
            (remhash source-name *free-funs*))
          (ecase where
            (:assumed
             (let ((approx-type (info :function :assumed-type source-name)))
               (when (and approx-type (fun-type-p defined-ftype))
                 (valid-approximate-type approx-type defined-ftype))
               ;; globaldb can't enforce invariants such as :assumed-type and
               ;; :type being mutually exclusive. For that reason it would have
               ;; made sense to use a single info-type holding either a true
               ;; function type or an approximate-fun-type. Regardless, it is
               ;; slightly preferable to clear the old before setting the new.
               (clear-info :function :assumed-type source-name)
               (setf (info :function :type source-name) defined-ftype))
             (setf (info :function :where-from source-name) :defined))
            ((:declared :defined-method)
             (let ((declared-ftype (proclaimed-ftype source-name)))
               (unless (defined-ftype-matches-declared-ftype-p
                         defined-ftype declared-ftype)
                 (compiler-style-warn
                  "~@<The previously declared FTYPE~2I ~_~S~I ~_~
                   conflicts with the definition type ~2I~_~S~:>"
                  (type-specifier declared-ftype)
                  (type-specifier defined-ftype)))))
            (:defined
             (setf (info :function :type source-name) defined-ftype)))))))
  (values))

;;; Find all calls in COMPONENT to assumed functions and update the
;;; assumed type information. This is delayed until now so that we
;;; have the best possible information about the actual argument
;;; types.
(defun note-assumed-types (component name var)
  (when (and (eq (leaf-where-from var) :assumed)
             (not (and (defined-fun-p var)
                       (eq (defined-fun-inlinep var) :notinline)))
             (eq (info :function :where-from name) :assumed)
             (eq (info :function :kind name) :function))
    (let ((atype (info :function :assumed-type name)))
      (dolist (ref (leaf-refs var))
        (let ((dest (node-dest ref)))
          (when (and (eq (node-component ref) component)
                     (combination-p dest)
                     (eq (lvar-uses (basic-combination-fun dest)) ref))
            (setq atype (note-fun-use dest atype)))))
      (setf (info :function :assumed-type name) atype))))

;;; Merge CASTs with preceding/following nodes.
(defun ir1-merge-casts (component)
  (do-blocks-backwards (block component)
    (do-nodes-backwards (node lvar block)
      (let ((dest (when lvar (lvar-dest lvar))))
        (cond ((and (cast-p dest)
                    (not (cast-type-check dest))
                    (immediately-used-p lvar node))
               (let ((dtype (node-derived-type node))
                     (atype (node-derived-type dest)))
                 (when (values-types-equal-or-intersect
                        dtype atype)
                   ;; FIXME: We do not perform pathwise CAST->type-error
                   ;; conversion, and type errors can later cause
                   ;; backend failures. On the other hand, this version
                   ;; produces less efficient code.
                   ;;
                   ;; This is sorta DERIVE-NODE-TYPE, but does not try
                   ;; to optimize the node.
                   (setf (node-derived-type node)
                         (values-type-intersection dtype atype)))))
              ((and (cast-p node)
                    (eq (cast-type-check node) :external))
               (aver (basic-combination-p dest))
               (delete-filter node lvar (cast-value node))))))))

;;; Do miscellaneous things that we want to do once all optimization
;;; has been done:
;;;  -- Record the derived result type before the back-end trashes the
;;;     flow graph.
;;;  -- Note definition of any entry points.
;;;  -- Note any failed optimizations.
(defun ir1-finalize (component)
  (declare (type component component))
  (dolist (fun (component-lambdas component))
    (case (functional-kind fun)
      (:external
       (finalize-xep-definition fun))
      ((nil :toplevel)
       (setf (leaf-type fun) (definition-type fun)))))

  (maphash #'note-failed-optimization
           (component-failed-optimizations component))

  (maphash (lambda (k v)
             (note-assumed-types component k v))
           *free-funs*)

  (ir1-merge-casts component)

  (values))
