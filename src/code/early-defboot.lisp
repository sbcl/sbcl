;;;; target bootstrapping stuff which needs to be visible on the
;;;; cross-compilation host too

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!EXT")

(file-comment
  "$Header$")

;;; helper function for various macros which expect clauses of a given
;;; length, etc. 
;;;
;;; KLUDGE: This implementation will hang on circular list structure. Since
;;; this is an error-checking utility, i.e. its job is to deal with screwed-up
;;; input, it'd be good style to fix it so that it can deal with circular list
;;; structure.
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Return true if X is a proper list whose length is between MIN and
  ;; MAX (inclusive).
  (defun proper-list-of-length-p (x min &optional (max min))
    (cond ((minusp max)
	   nil)
	  ((null x)
	   (zerop min))
	  ((consp x)
	   (and (plusp max)
		(proper-list-of-length-p (cdr x)
					 (if (plusp (1- min))
					   (1- min)
					   0)
					 (1- max))))
	  (t nil))))

;;;; DO-related stuff which needs to be visible on the cross-compilation host

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun do-do-body (varlist endlist decls-and-code bind step name block)
    (let* ((r-inits nil) ; accumulator for reversed list
	   (r-steps nil) ; accumulator for reversed list
	   (label-1 (gensym))
	   (label-2 (gensym)))
      ;; Check for illegal old-style DO.
      (when (or (not (listp varlist)) (atom endlist))
	(error "Ill-formed ~S -- possibly illegal old style DO?" name))
      ;; Parse VARLIST to get R-INITS and R-STEPS.
      (dolist (v varlist)
	(flet (;; (We avoid using CL:PUSH here so that CL:PUSH can be defined
	       ;; in terms of CL:SETF, and CL:SETF can be defined in terms of
	       ;; CL:DO, and CL:DO can be defined in terms of the current
	       ;; function.)
	       (push-on-r-inits (x)
		 (setq r-inits (cons x r-inits)))
	       ;; common error-handling
	       (illegal-varlist ()
		 (error "~S is an illegal form for a ~S varlist." v name)))
	  (cond ((symbolp v) (push-on-r-inits v))
		((listp v)
		 (unless (symbolp (first v))
		   (error "~S step variable is not a symbol: ~S"
			  name
			  (first v)))
		 (let ((lv (length v)))
		   ;; (We avoid using CL:CASE here so that CL:CASE can be
		   ;; defined in terms of CL:SETF, and CL:SETF can be defined
		   ;; in terms of CL:DO, and CL:DO can be defined in terms of
		   ;; the current function.)
		   (cond ((= lv 1)
			  (push-on-r-inits (first v)))
			 ((= lv 2)
			  (push-on-r-inits v))
			 ((= lv 3)
			  (push-on-r-inits (list (first v) (second v)))
			  (setq r-steps (list* (third v) (first v) r-steps)))
			 (t (illegal-varlist)))))
		(t (illegal-varlist)))))
      ;; Construct the new form.
      (multiple-value-bind (code decls) (parse-body decls-and-code nil)
	`(block ,block
	   (,bind ,(nreverse r-inits)
		  ,@decls
		  (tagbody
		   (go ,label-2)
		   ,label-1
		   ,@code
		   (,step ,@(nreverse r-steps))
		   ,label-2
		   (unless ,(first endlist) (go ,label-1))
		   (return-from ,block (progn ,@(rest endlist))))))))))

(defmacro do-anonymous (varlist endlist &rest body)
  #!+sb-doc
  "DO-ANONYMOUS ({(Var [Init] [Step])}*) (Test Exit-Form*) Declaration* Form*
  Like DO, but has no implicit NIL block. Each Var is initialized in parallel
  to the value of the specified Init form. On subsequent iterations, the Vars
  are assigned the value of the Step form (if any) in parallel. The Test is
  evaluated before each evaluation of the body Forms. When the Test is true,
  the Exit-Forms are evaluated as a PROGN, with the result being the value
  of the DO."
  (do-do-body varlist endlist body 'let 'psetq 'do-anonymous (gensym)))
