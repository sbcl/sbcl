;;;; code to detect whether a package has changed

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-COLD")

(defstruct snapshot
  (hash-table (make-hash-table :test 'eq)
	      :type hash-table
	      :read-only t))

;;; Return a SNAPSHOT object representing the current state of the
;;; package associated with PACKAGE-DESIGNATOR.
;;;
;;; This could be made more sensitive, checking for more things, such as
;;; type definitions and documentation strings.
(defun take-snapshot (package-designator)
  (let ((package (find-package package-designator))
	(result (make-snapshot)))
    (unless package
      (error "can't find package ~S" package-designator))
    (do-symbols (symbol package)
      (multiple-value-bind (symbol-ignore status)
	  (find-symbol (symbol-name symbol) package)
	(declare (ignore symbol-ignore))
	(let ((symbol-properties nil))
	  (ecase status
	    (:inherited
	     (values))
	    ((:internal :external)
	     (when (boundp symbol)
	       (push (cons :symbol-value (symbol-value symbol))
		     symbol-properties))
	     (when (fboundp symbol)
	       (push (cons :symbol-function (symbol-function symbol))
		     symbol-properties))
	     (when (macro-function symbol)
	       (push (cons :macro-function (macro-function symbol))
		     symbol-properties))
	     (when (special-operator-p symbol)
	       (push :special-operator
		     symbol-properties))))
	  (push status symbol-properties)
	  (setf (gethash symbol (snapshot-hash-table result))
		symbol-properties))))
    result))
(compile 'take-snapshot)

(defun snapshot-diff (x y)
  (let ((xh (snapshot-hash-table x))
	(yh (snapshot-hash-table y))
	(result nil))
    (flet ((1way (ah bh)
	     (maphash (lambda (key avalue)
			(declare (ignore avalue))
			(multiple-value-bind (bvalue bvalue?) (gethash key bh)
			  (declare (ignore bvalue))
			  (unless bvalue?
			    (push (list key ah)
				  result))))
		      ah)))
      (1way xh yh)
      (1way yh xh))
    (maphash (lambda (key xvalue)
	       (multiple-value-bind (yvalue yvalue?) (gethash key yh)
		 (when yvalue?
		   (unless (equalp xvalue yvalue)
		     (push (list key xvalue yvalue)
			   result)))))
	     xh)
    result))
(compile 'snapshot-diff)

;;;; symbols in package COMMON-LISP which change regularly in the course of
;;;; execution even if we don't mess with them, so that reporting changes
;;;; would be more confusing than useful
(defparameter
  *cl-ignorable-diffs*
  (let ((result (make-hash-table :test 'eq)))
    (dolist (symbol `(;; These change regularly:
		      * ** ***
		      / // ///
		      + ++ +++
		      -
		      *gensym-counter*
		      ;; These are bound when compiling and/or loading:
		      *package*
		      *compile-file-truename*
		      *compile-file-pathname*
		      *load-truename*
		      *load-pathname*
		      ;; These change because CMU CL uses them as internal
		      ;; variables:
		      ,@'
		      #-cmu nil
		      #+cmu (cl::*gc-trigger*
			     cl::inch-ptr
			     cl::*internal-symbol-output-function*
			     cl::ouch-ptr
			     cl::*previous-case*
			     cl::read-buffer
			     cl::read-buffer-length
			     cl::*string-output-streams*
			     cl::*available-buffers*
			     cl::*current-unwind-protect-block*
			     cl::*load-depth*
			     cl::*free-fop-tables*
			     ;; These two are changed by PURIFY.
			     cl::*static-space-free-pointer*
			     cl::*static-space-end-pointer*)
		      ))
      (setf (gethash symbol result) t))
    result))

;;; specialized version of SNAPSHOT-DIFF to check on the COMMON-LISP package,
;;; throwing away reports of differences in variables which are known to change
;;; regularly
;;;
;;; Note: The warnings from this code were somewhat useful when first setting
;;; up the cross-compilation system, have a rather low signal/noise ratio in
;;; the mature system. They can generally be safely ignored.
#!+sb-show
(progn
  (defun cl-snapshot-diff (cl-snapshot)
    (remove-if (lambda (entry)
		 (gethash (first entry) *cl-ignorable-diffs*))
	       (snapshot-diff cl-snapshot (take-snapshot :common-lisp))))
  (defun warn-when-cl-snapshot-diff (cl-snapshot)
    (let ((cl-snapshot-diff (cl-snapshot-diff cl-snapshot)))
      (when cl-snapshot-diff
	(let ((*print-length* 30)
	      (*print-circle* t))
	  (warn "CL snapshot differs:")
	  (print cl-snapshot-diff *error-output*)))))
  (compile 'cl-snapshot-diff)
  (compile 'warn-when-cl-snapshot-diff))
