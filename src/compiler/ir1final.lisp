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
	      (not (function-info-p (combination-kind node))))
    (let ((*compiler-error-context* node))
      (dolist (failure failures)
	(let ((what (cdr failure))
	      (note (transform-note (car failure))))
	  (cond
	   ((consp what)
	    (compiler-note "~@<unable to ~2I~_~A ~I~_because: ~2I~_~?~:>"
			   note (first what) (rest what)))
	   ((valid-function-use node what
				:argument-test #'types-equal-or-intersect
				:result-test #'values-types-equal-or-intersect)
	    (collect ((messages))
	      (flet ((frob (string &rest stuff)
		       (messages string)
		       (messages stuff)))
		(valid-function-use node what
				    :warning-function #'frob
				    :error-function #'frob))
	      (compiler-note "~@<unable to ~
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
;;; name, and add derived type information to the info environment. We
;;; also delete the FUNCTIONAL from *FREE-FUNCTIONS* to eliminate the
;;; possibility that new references might be converted to it.
(defun finalize-xep-definition (fun)
  (let* ((leaf (functional-entry-function fun))
	 (name (leaf-name leaf))
	 (defined-ftype (definition-type leaf)))
    (setf (leaf-type leaf) defined-ftype)
    (when (or (and name (symbolp name))
	      (and (consp name) (eq (car name) 'setf)))
      (let* ((where (info :function :where-from name))
	     (*compiler-error-context* (lambda-bind (main-entry leaf)))
	     (global-def (gethash name *free-functions*))
	     (global-p (defined-function-p global-def)))
	(note-name-defined name :function)
	(when global-p
	  (remhash name *free-functions*))
	(ecase where
	  (:assumed
	   (let ((approx-type (info :function :assumed-type name)))
	     (when (and approx-type (function-type-p defined-ftype))
	       (valid-approximate-type approx-type defined-ftype))
	     (setf (info :function :type name) defined-ftype)
	     (setf (info :function :assumed-type name) nil))
	   (setf (info :function :where-from name) :defined))
	  (:declared
	   (let ((declared-ftype (info :function :type name)))
	     (unless (defined-ftype-matches-declared-ftype-p
		       defined-ftype declared-ftype)
	       (note-lossage "~@<The previously declared FTYPE~2I ~_~S~I ~_~
                              conflicts with the definition type ~2I~_~S~:>"
			     (type-specifier declared-ftype)
			     (type-specifier defined-ftype)))))
	  (:defined
           (when global-p
             (setf (info :function :type name) defined-ftype)))))))
  (values))

;;; Find all calls in COMPONENT to assumed functions and update the
;;; assumed type information. This is delayed until now so that we
;;; have the best possible information about the actual argument
;;; types.
(defun note-assumed-types (component name var)
  (when (and (eq (leaf-where-from var) :assumed)
	     (not (and (defined-function-p var)
		       (eq (defined-function-inlinep var) :notinline)))
	     (eq (info :function :where-from name) :assumed)
	     (eq (info :function :kind name) :function))
    (let ((atype (info :function :assumed-type name)))
      (dolist (ref (leaf-refs var))
	(let ((dest (continuation-dest (node-cont ref))))
	  (when (and (eq (block-component (node-block ref)) component)
		     (combination-p dest)
		     (eq (continuation-use (basic-combination-fun dest)) ref))
	    (setq atype (note-function-use dest atype)))))
      (setf (info :function :assumed-type name) atype))))

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
      ((nil)
       (setf (leaf-type fun) (definition-type fun)))))

  (maphash #'note-failed-optimization
	   (component-failed-optimizations component))

  (maphash #'(lambda (k v)
	       (note-assumed-types component k v))
	   *free-functions*)
  (values))
