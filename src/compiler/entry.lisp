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

(file-comment
  "$Header$")

;;; This phase runs before IR2 conversion, initializing each XEP's
;;; Entry-Info structure. We call the VM-supplied
;;; Select-Component-Format function to make VM-dependent
;;; initializations in the IR2-Component. This includes setting the
;;; IR2-Component-Kind and allocating fixed implementation overhead in
;;; the constant pool. If there was a forward reference to a function,
;;; then the ENTRY-INFO will already exist, but will be uninitialized.
(defun entry-analyze (component)
  (let ((2comp (component-info component)))
    (dolist (fun (component-lambdas component))
      (when (external-entry-point-p fun)
	(let ((info (or (leaf-info fun)
			(setf (leaf-info fun) (make-entry-info)))))
	  (compute-entry-info fun info)
	  (push info (ir2-component-entries 2comp))))))

  (select-component-format component)
  (values))

;;; Takes the list representation of the debug arglist and turns it
;;; into a string.
;;;
;;; FIXME: Why don't we just save this as a list instead of converting
;;; it to a string?
(defun make-arg-names (x)
  (declare (type functional x))
  (let ((args (functional-arg-documentation x)))
    (assert (not (eq args :unspecified)))
    (if (null args)
	"()"
	(let ((*print-pretty* t)
	      (*print-escape* t)
	      (*print-base* 10)
	      (*print-radix* nil)
	      (*print-case* :downcase))
	  (write-to-string args)))))

;;; Initialize Info structure to correspond to the XEP lambda Fun.
(defun compute-entry-info (fun info)
  (declare (type clambda fun) (type entry-info info))
  (let ((bind (lambda-bind fun))
	(internal-fun (functional-entry-function fun)))
    (setf (entry-info-closure-p info)
	  (not (null (environment-closure (lambda-environment fun)))))
    (setf (entry-info-offset info) (gen-label))
    (setf (entry-info-name info)
	  (let ((name (leaf-name internal-fun)))
	    (or name
		(component-name (block-component (node-block bind))))))
    (when (policy bind (>= debug 1))
      (setf (entry-info-arguments info) (make-arg-names internal-fun))
      (setf (entry-info-type info) (type-specifier (leaf-type internal-fun)))))
  (values))

;;; Replace all references to Component's non-closure XEPS that appear in
;;; top-level components, changing to :TOP-LEVEL-XEP functionals. If the
;;; cross-component ref is not in a :TOP-LEVEL component, or is to a closure,
;;; then substitution is suppressed.
;;;
;;; When a cross-component ref is not substituted, we return T to indicate that
;;; early deletion of this component's IR1 should not be done. We also return
;;; T if this component contains :TOP-LEVEL lambdas (though it is not a
;;; :TOP-LEVEL component.)
;;;
;;; We deliberately don't use the normal reference deletion, since we don't
;;; want to trigger deletion of the XEP (although it shouldn't hurt, since this
;;; is called after Component is compiled.)  Instead, we just clobber the
;;; REF-LEAF.
(defun replace-top-level-xeps (component)
  (let ((res nil))
    (dolist (lambda (component-lambdas component))
      (case (functional-kind lambda)
	(:external
	 (let* ((ef (functional-entry-function lambda))
		(new (make-functional :kind :top-level-xep
				      :info (leaf-info lambda)
				      :name (leaf-name ef)
				      :lexenv (make-null-lexenv)))
		(closure (environment-closure
			  (lambda-environment (main-entry ef)))))
	   (dolist (ref (leaf-refs lambda))
	     (let ((ref-component (block-component (node-block ref))))
	       (cond ((eq ref-component component))
		     ((or (not (eq (component-kind ref-component) :top-level))
			  closure)
		      (setq res t))
		     (t
		      (setf (ref-leaf ref) new)
		      (push ref (leaf-refs new))))))))
	(:top-level
	 (setq res t))))
    res))
