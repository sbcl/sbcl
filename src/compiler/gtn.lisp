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

(in-package "SB!C")

;;; We make a pass over the component's environments, assigning argument
;;; passing locations and return conventions and TNs for local variables.
(defun gtn-analyze (component)
  (setf (component-info component) (make-ir2-component))
  (let ((funs (component-lambdas component)))
    (dolist (fun funs)
      (assign-ir2-physenv fun)
      (assign-return-locations fun)
      (assign-ir2-nlx-info fun)
      (assign-lambda-var-tns fun nil)
      (dolist (let (lambda-lets fun))
        (assign-lambda-var-tns let t))))

  (values))

;;; We have to allocate the home TNs for variables before we can call
;;; ASSIGN-IR2-PHYSENV so that we can close over TNs that haven't
;;; had their home environment assigned yet. Here we evaluate the
;;; DEBUG-INFO/SPEED tradeoff to determine how variables are
;;; allocated. If SPEED is 3, then all variables are subject to
;;; lifetime analysis. Otherwise, only LET-P variables are allocated
;;; normally, and that can be inhibited by DEBUG-INFO = 3.
(defun assign-lambda-var-tns (fun let-p)
  (declare (type clambda fun))
  (dolist (var (lambda-vars fun))
    (when (leaf-refs var)
      (let* ((type (if (lambda-var-indirect var)
                       *backend-t-primitive-type*
                       (primitive-type (leaf-type var))))
             (temp (make-normal-tn type))
             (node (lambda-bind fun))
             (res (if (or (and let-p (policy node (< debug 3)))
                          (policy node (zerop debug))
                          (policy node (= speed 3)))
                      temp
                      (physenv-debug-live-tn temp (lambda-physenv fun)))))
        (setf (tn-leaf res) var)
        (setf (leaf-info var) res))))
  (values))

;;; Give CLAMBDA an IR2-PHYSENV structure. (And in order to
;;; properly initialize the new structure, we make the TNs which hold
;;; environment values and the old-FP/return-PC.)
(defun assign-ir2-physenv (clambda)
  (declare (type clambda clambda))
  (let ((lambda-physenv (lambda-physenv clambda))
        (reversed-ir2-physenv-alist nil))
    ;; FIXME: should be MAPCAR, not DOLIST
    (dolist (thing (physenv-closure lambda-physenv))
      (let ((ptype (etypecase thing
                     (lambda-var
                      (if (lambda-var-indirect thing)
                          *backend-t-primitive-type*
                          (primitive-type (leaf-type thing))))
                     (nlx-info *backend-t-primitive-type*)
                     (clambda *backend-t-primitive-type*))))
        (push (cons thing (make-normal-tn ptype))
              reversed-ir2-physenv-alist)))

    (let ((res (make-ir2-physenv
                :closure (nreverse reversed-ir2-physenv-alist)
                :return-pc-pass (make-return-pc-passing-location
                                 (xep-p clambda)))))
      (setf (physenv-info lambda-physenv) res)
      (setf (ir2-physenv-old-fp res)
            (make-old-fp-save-location lambda-physenv))
      (setf (ir2-physenv-return-pc res)
            (make-return-pc-save-location lambda-physenv))))

  (values))

;;; Return true if FUN's result is used in a tail-recursive full
;;; call. We only consider explicit :FULL calls. It is assumed that
;;; known calls are never part of a tail-recursive loop, so we don't
;;; need to enforce tail-recursion. In any case, we don't know which
;;; known calls will actually be full calls until after LTN.
(defun has-full-call-use (fun)
  (declare (type clambda fun))
  (let ((return (lambda-return fun)))
    (and return
         (do-uses (use (return-result return) nil)
           (when (and (node-tail-p use)
                      (basic-combination-p use)
                      (eq (basic-combination-kind use) :full))
             (return t))))))

;;; Return true if we should use the standard (unknown) return
;;; convention for a TAIL-SET. We use the standard return convention
;;; when:
;;; -- We must use the standard convention to preserve tail-recursion,
;;;    since the TAIL-SET contains both an XEP and a TR full call.
;;; -- It appears to be more efficient to use the standard convention,
;;;    since there are no non-TR local calls that could benefit from
;;;    a non-standard convention.
;;; -- We're compiling with RETURN-FROM-FRAME instrumentation, which
;;;    only works (on x86 and x86-64) for the standard convention.
(defun use-standard-returns (tails)
  (declare (type tail-set tails))
  (let ((funs (tail-set-funs tails)))
    (or (and (find-if #'xep-p funs)
             (find-if #'has-full-call-use funs))
        (some (lambda (fun) (policy fun (>= insert-debug-catch 2))) funs)
        (block punt
          (dolist (fun funs t)
            (dolist (ref (leaf-refs fun))
              (let* ((lvar (node-lvar ref))
                     (dest (and lvar (lvar-dest lvar))))
                (when (and (basic-combination-p dest)
                           (not (node-tail-p dest))
                           (eq (basic-combination-fun dest) lvar)
                           (eq (basic-combination-kind dest) :local))
                  (return-from punt nil)))))))))

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
                            :types ptypes)
          (make-return-info :kind :fixed
                            :count count
                            :types ptypes
                            :locations (mapcar #'make-normal-tn ptypes))))))

;;; If TAIL-SET doesn't have any INFO, then make a RETURN-INFO for it.
;;; If we choose a return convention other than :UNKNOWN, and this
;;; environment is for an XEP, then break tail recursion on the XEP
;;; calls, since we must always use unknown values when returning from
;;; an XEP.
(defun assign-return-locations (fun)
  (declare (type clambda fun))
  (let* ((tails (lambda-tail-set fun))
         (returns (or (tail-set-info tails)
                      (setf (tail-set-info tails)
                            (return-info-for-set tails))))
         (return (lambda-return fun)))
    (when (and return
               (not (eq (return-info-kind returns) :unknown))
               (xep-p fun))
      (do-uses (use (return-result return))
        (setf (node-tail-p use) nil))))
  (values))

;;; Make an IR2-NLX-INFO structure for each NLX entry point recorded.
;;; We call a VM supplied function to make the SAVE-SP restricted on
;;; the stack. The NLX-ENTRY VOP's :FORCE-TO-STACK SAVE-P value
;;; doesn't do this, since the SP is an argument to the VOP, and thus
;;; isn't live afterwards.
(defun assign-ir2-nlx-info (fun)
  (declare (type clambda fun))
  (let ((physenv (lambda-physenv fun)))
    (dolist (nlx (physenv-nlx-info physenv))
      (setf (nlx-info-info nlx)
            (make-ir2-nlx-info
             :home (when (member (cleanup-kind (nlx-info-cleanup nlx))
                                 '(:block :tagbody))
                     (if (nlx-info-safe-p nlx)
                         (make-normal-tn *backend-t-primitive-type*)
                         (make-stack-pointer-tn)))
             :save-sp (make-nlx-sp-tn physenv)))))
  (values))
