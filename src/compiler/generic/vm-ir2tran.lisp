;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

(defoptimizer ir2-convert-reffer ((object) node block name offset lowtag)
  (let* ((cont (node-cont node))
	 (locs (continuation-result-tns cont
					(list *backend-t-primitive-type*)))
	 (res (first locs)))
    (vop slot node block (continuation-tn node block object)
	 name offset lowtag res)
    (move-continuation-result node block locs cont)))

(defoptimizer ir2-convert-setter ((object value) node block name offset lowtag)
  (let ((value-tn (continuation-tn node block value)))
    (vop set-slot node block (continuation-tn node block object) value-tn
	 name offset lowtag)
    (move-continuation-result node block (list value-tn) (node-cont node))))

(defoptimizer ir2-convert-setfer ((value object) node block name offset lowtag)
  (let ((value-tn (continuation-tn node block value)))
    (vop set-slot node block (continuation-tn node block object) value-tn
	 name offset lowtag)
    (move-continuation-result node block (list value-tn) (node-cont node))))

(defun do-inits (node block name result lowtag inits args)
  (let ((unbound-marker-tn nil))
    (dolist (init inits)
      (let ((kind (car init))
	    (slot (cdr init)))
	(vop set-slot node block result
	     (ecase kind
	       (:arg
		(aver args)
		(continuation-tn node block (pop args)))
	       (:unbound
		(or unbound-marker-tn
		    (setf unbound-marker-tn
			  (let ((tn (make-restricted-tn
				     nil
				     (sc-number-or-lose 'sb!vm::any-reg))))
			    (vop make-unbound-marker node block tn)
			    tn))))
	       (:null
		(emit-constant nil)))
	     name slot lowtag))))
  (aver (null args)))

(defun do-fixed-alloc (node block name words type lowtag result)
  (vop fixed-alloc node block name words type lowtag result))

(defoptimizer ir2-convert-fixed-allocation
	      ((&rest args) node block name words type lowtag inits)
  (let* ((cont (node-cont node))
	 (locs (continuation-result-tns cont
					(list *backend-t-primitive-type*)))
	 (result (first locs)))
    (do-fixed-alloc node block name words type lowtag result)
    (do-inits node block name result lowtag inits args)
    (move-continuation-result node block locs cont)))

(defoptimizer ir2-convert-variable-allocation
	      ((extra &rest args) node block name words type lowtag inits)
  (let* ((cont (node-cont node))
	 (locs (continuation-result-tns cont
					(list *backend-t-primitive-type*)))
	 (result (first locs)))
    (if (constant-continuation-p extra)
	(let ((words (+ (continuation-value extra) words)))
	  (do-fixed-alloc node block name words type lowtag result))
	(vop var-alloc node block (continuation-tn node block extra) name words
	     type lowtag result))
    (do-inits node block name result lowtag inits args)
    (move-continuation-result node block locs cont)))
