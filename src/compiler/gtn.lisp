;;;; This file contains the GTN pass in the compiler. GTN allocates
;;;; the TNs that hold the values of lexical variables and determines
;;;; the calling conventions and passing locations used in function
;;;; calls.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;; We make a pass over the component's environments, assigning argument
;;; passing locations and return conventions and TNs for local variables.
(defun gtn-analyze (component)
  (setf (component-info component) (make-ir2-component))
  (let ((funs (component-lambdas component))
        #+fp-and-pc-standard-save
        (old-fp (make-old-fp-save-location))
        #+fp-and-pc-standard-save
        (old-pc (make-return-pc-save-location)))
    (dolist (fun funs)
      (assign-ir2-environment fun
                              #+fp-and-pc-standard-save old-fp
                              #+fp-and-pc-standard-save old-pc)
      (assign-ir2-nlx-info fun)
      (assign-lambda-var-tns fun nil)
      (dolist (let (lambda-lets fun))
        (assign-lambda-var-tns let t))))

  (values))

;;; We have to allocate the home TNs for variables before we can call
;;; ASSIGN-IR2-ENVIRONMENT so that we can close over TNs that haven't
;;; had their home environment assigned yet. Here we evaluate the
;;; DEBUG-INFO/SPEED tradeoff to determine how variables are
;;; allocated. If SPEED is 3, then all variables are subject to
;;; lifetime analysis. Otherwise, only LET-P variables are allocated
;;; normally, and that can be inhibited by DEBUG-INFO = 3.
(defun assign-lambda-var-tns (fun let-p)
  (declare (type clambda fun))
  (dolist (var (lambda-vars fun))
    (when (leaf-refs var)
      (let* (ptype-info
             (type (if (lambda-var-indirect var)
                       (if (lambda-var-explicit-value-cell var)
                           *backend-t-primitive-type*
                           (or (first
                                (setf ptype-info
                                      (primitive-type-indirect-cell-type
                                       (primitive-type (leaf-type var)))))
                               *backend-t-primitive-type*))
                       (primitive-type (leaf-type var))))
             (res (make-normal-tn type))
             (node (lambda-bind fun))
             (debug-variable-p (not (or (and let-p (policy node (< debug 3)))
                                        (policy node (zerop debug))
                                        (policy node (= speed 3))))))
        (cond
          ((and (lambda-var-indirect var)
                (not (lambda-var-explicit-value-cell var)))
           ;; Force closed-over indirect LAMBDA-VARs without explicit
           ;; VALUE-CELLs to the stack, and make sure that they are
           ;; live over the dynamic contour of the environment.
           (setf (tn-sc res) (if ptype-info
                                 (second ptype-info)
                                 (sc-or-lose 'sb-vm::control-stack)))
           (environment-live-tn res (lambda-environment fun)))

          (debug-variable-p
           ;; If it's a constant it may end up being never read,
           ;; replaced by COERCE-FROM-CONSTANT.
           ;; Yet it might get saved on the stack, but since it's
           ;; never read no stack space is allocated for it in the
           ;; callee frame.
           (unless (type-singleton-p (leaf-type var))
             (environment-debug-live-tn res (lambda-environment fun)))))

        (setf (tn-leaf res) var)
        (setf (tn-type res) (leaf-type var))
        (setf (leaf-info var) res))))
  (values))

;;; Give CLAMBDA an IR2-ENVIRONMENT structure. (And in order to
;;; properly initialize the new structure, we make the TNs which hold
;;; environment values and the old-FP/return-PC.)
(defun assign-ir2-environment (clambda #+fp-and-pc-standard-save old-fp
                                       #+fp-and-pc-standard-save old-pc)
  (declare (type clambda clambda))
  (let* ((lambda-environment (lambda-environment clambda))
         (indirect-fp-tns)
         (ir2-environment-alist
           (loop for thing in (environment-closure lambda-environment)
                 collect
                 (cons thing
                       (etypecase thing
                         (lambda-var
                          (cond ((not (lambda-var-indirect thing))
                                 (make-normal-tn
                                  (primitive-type (leaf-type thing))))
                                ((not (lambda-var-explicit-value-cell thing))
                                 (let ((env (lambda-environment (lambda-var-home thing))))
                                   (or (getf indirect-fp-tns env)
                                       (let ((tn (make-normal-tn *backend-t-primitive-type*)))
                                         (push tn indirect-fp-tns)
                                         (push env indirect-fp-tns)
                                         tn))))
                                (t
                                 (make-normal-tn *backend-t-primitive-type*))))
                         (nlx-info
                          (make-normal-tn *backend-t-primitive-type*))
                         (clambda
                          (make-normal-tn *backend-t-primitive-type*)))))))
    (let ((res (make-ir2-environment
                :closure ir2-environment-alist
                :return-pc-pass #+fp-and-pc-standard-save
                                old-pc
                                #-fp-and-pc-standard-save
                                (make-return-pc-passing-location (xep-p clambda)))))
      (setf (environment-info lambda-environment) res)
      (setf (ir2-environment-old-fp res)
            #-fp-and-pc-standard-save
            (make-old-fp-save-location lambda-environment)
            #+fp-and-pc-standard-save
            old-fp)
      (setf (ir2-environment-return-pc res)
            #-fp-and-pc-standard-save
            (make-return-pc-save-location lambda-environment)
            #+fp-and-pc-standard-save
            old-pc)
      #+fp-and-pc-standard-save
      (progn
        (push old-fp (ir2-environment-live-tns (environment-info lambda-environment)))
        (push old-pc (ir2-environment-live-tns (environment-info lambda-environment))))))

  (values))

;;; Return true if we should use the standard (unknown) return
;;; convention for a TAIL-SET. We use the standard return convention
;;; when:
;;; -- If it has an XEP.
;;;    it could break the tail call in this case, but it usually
;;;    doesn't produce better code and makes for worse debugging.
;;; -- It appears to be more efficient to use the standard convention,
;;;    since there are no non-TR local calls that could benefit from
;;;    a non-standard convention.
;;; -- We're compiling with RETURN-FROM-FRAME instrumentation, which
;;;    only works (on x86, x86-64, arm) for the standard convention.
(defun use-standard-returns (tails)
  (declare (type tail-set tails))
  (let ((funs (tail-set-funs tails)))
    (or (find-if #'xep-p funs)
        (some (lambda (fun) (policy fun (>= insert-debug-catch 2))) funs)
        (block punt
          (dolist (fun funs t)
            (dolist (ref (leaf-refs fun))
              (let* ((lvar (node-lvar ref))
                     (dest (and lvar (lvar-dest lvar))))
                (flet ((all-returns-tail-calls-p (call)
                         (let* ((lambda (combination-lambda call))
                                (return (lambda-return lambda)))
                           (when return
                             (do-uses (node (return-result return) t)
                               (unless (and (basic-combination-p node)
                                            (eq (basic-combination-info node) :full))
                                 (return)))))))
                 (when (and (basic-combination-p dest)
                            (not (node-tail-p dest))
                            (eq (basic-combination-fun dest) lvar)
                            (eq (basic-combination-kind dest) :local)
                            (not (all-returns-tail-calls-p dest)))
                   (return-from punt nil))))))))))

;;; If policy indicates, give an efficiency note about our inability to
;;; use the known return convention. We try to find a function in the
;;; tail set with non-constant return values to use as context. If
;;; there is no such function, then be more vague.
(defun return-value-efficiency-note (tails)
  (declare (type tail-set tails))
  (let ((funs (tail-set-funs tails)))
    (when (policy (lambda-bind (first funs))
                  (> (max speed space)
                     inhibit-warnings))
      (dolist (fun funs
                   (let ((*compiler-error-context* (lambda-bind (first funs))))
                     (compiler-notify
                      "Return value count mismatch prevents known return ~
                       from these functions:~
                       ~{~%  ~A~}"
                      (mapcar #'leaf-source-name
                              (remove-if-not #'leaf-has-source-name-p funs)))))
        (let ((ret (lambda-return fun)))
          (when ret
            (let ((rtype (return-result-type ret)))
              (multiple-value-bind (ignore count) (values-types rtype)
                (declare (ignore ignore))
                (when (eq count :unknown)
                  (let ((*compiler-error-context* (lambda-bind fun)))
                    (compiler-notify
                     "Return type not fixed values, so can't use known return ~
                      convention:~%  ~S"
                     (type-specifier rtype)))
                  (return)))))))))
  (values))

;;; Return a RETURN-INFO structure describing how we should return
;;; from functions in the specified tail set. We use the unknown
;;; values convention if the number of values is unknown, or if it is
;;; a good idea for some other reason. Otherwise we allocate passing
;;; locations for a fixed number of values.
(defun return-info-for-set (tails)
  (declare (type tail-set tails))
  (multiple-value-bind (types count) (values-types (tail-set-type tails))
    (let ((ptypes (mapcar #'primitive-type types))
          (use-standard (use-standard-returns tails)))
      (when (and (eq count :unknown) (not use-standard)
                 (not (eq (tail-set-type tails) *empty-type*)))
        (return-value-efficiency-note tails))
      (if (or (eq count :unknown) use-standard)
          (make-return-info :kind :unknown
                            :count count
                            :primitive-types ptypes
                            :types types)
          (make-return-info :kind :fixed
                            :count count
                            :primitive-types ptypes
                            :types types
                            :locations (mapcar #'make-normal-tn ptypes))))))

;;; If TAIL-SET doesn't have any INFO, then make a RETURN-INFO for it.
(defun assign-return-locations (fun)
  (declare (type clambda fun))
  (let* ((tails (lambda-tail-set fun))
         (returns (or (tail-set-info tails)
                      (setf (tail-set-info tails)
                            (return-info-for-set tails))))
         (return (lambda-return fun)))
    (when (and return
               (xep-p fun))
      (aver (eq (return-info-kind returns) :unknown))))
  (values))

;;; Make an IR2-NLX-INFO structure for each NLX entry point recorded.
;;; We call a VM supplied function to make the SAVE-SP restricted on
;;; the stack. The NLX-ENTRY VOP's :FORCE-TO-STACK SAVE-P value
;;; doesn't do this, since the SP is an argument to the VOP, and thus
;;; isn't live afterwards.
(defun assign-ir2-nlx-info (fun)
  (declare (type clambda fun))
  (let ((env (lambda-environment fun)))
    (dolist (nlx (environment-nlx-info env))
      (setf (nlx-info-info nlx)
            (make-ir2-nlx-info
             :home (when (member (cleanup-kind (nlx-info-cleanup nlx))
                                 '(:block :tagbody))
                     (if (nlx-info-safe-p nlx)
                         (make-normal-tn *backend-t-primitive-type*)
                         (make-stack-pointer-tn)))
             :save-sp (unless (eq (cleanup-kind (nlx-info-cleanup nlx))
                                  :unwind-protect)
                        (make-nlx-sp-tn env))
             :block-tn (environment-live-tn
                        (make-normal-tn
                         (primitive-type-or-lose
                          (ecase (cleanup-kind (nlx-info-cleanup nlx))
                            (:catch
                                'catch-block)
                            ((:unwind-protect :block :tagbody)
                             'unwind-block))))
                        env)))))
  (values))
