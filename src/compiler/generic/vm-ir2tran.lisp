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

#!+gengc
(defun needs-remembering (cont)
  (if (csubtypep (continuation-type cont)
		 (load-time-value (specifier-type '(or fixnum character
						       (member t nil)))))
      nil
      t))

(defoptimizer ir2-convert-setter ((object value) node block name offset lowtag)
  (let ((value-tn (continuation-tn node block value)))
    (vop set-slot node block (continuation-tn node block object) value-tn
	 name offset lowtag #!+gengc (needs-remembering value))
    (move-continuation-result node block (list value-tn) (node-cont node))))

(defoptimizer ir2-convert-setfer ((value object) node block name offset lowtag)
  (let ((value-tn (continuation-tn node block value)))
    (vop set-slot node block (continuation-tn node block object) value-tn
	 name offset lowtag #!+gengc (needs-remembering value))
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
	     name slot lowtag #!+gengc nil))))
  (aver (null args)))

(defun do-fixed-alloc (node block name words type lowtag result)
  #!-gengc
  (vop fixed-alloc node block name words type lowtag result)
  #!+gengc
  (if (>= words sb!vm:large-object-cutoff)
      (vop large-alloc node block (emit-constant (logandc2 (1+ words) 1))
	   (emit-constant lowtag) (emit-constant type) (emit-constant 0) name
	   result)
      (vop fixed-alloc node block name words type lowtag result)))

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



;;;; other allocation support

#!+gengc
(defoptimizer (make-array-header ir2-convert) ((type rank) node block)
  (let* ((cont (node-cont node))
	 (locs (continuation-result-tns cont
					(list *backend-t-primitive-type*)))
	 (result (first locs)))
    (if (and (constant-continuation-p type)
	     (constant-continuation-p rank))
	(do-fixed-alloc node block 'make-array-header
			(+ (continuation-value rank)
			   sb!vm:array-dimensions-offset)
			(continuation-value type)
			sb!vm:other-pointer-type result)
	(vop make-array-header node block (continuation-tn node block type)
	     (continuation-tn node block rank) result))
    (move-continuation-result node block locs cont)))

;;;; replacements for stuff in ir2tran to make gengc work

#!+gengc
(defun ir2-convert-closure (node block leaf res)
  (declare (type ref node) (type ir2-block block)
	   (type functional leaf) (type tn res))
  (unless (leaf-info leaf)
    (setf (leaf-info leaf) (make-entry-info)))
  (let ((entry (make-load-time-constant-tn :entry leaf))
	(closure (etypecase leaf
		   (clambda
		    (environment-closure (get-lambda-environment leaf)))
		   (functional
		    (aver (eq (functional-kind leaf) :top-level-xep))
		    nil))))
    (if closure
	(let ((this-env (node-environment node)))
	  #!+gengc (let ((temp (make-normal-tn *backend-t-primitive-type*)))
		     (do-fixed-alloc node block 'make-closure
				     (+ (length closure)
					sb!vm:closure-info-offset)
				     sb!vm:closure-header-type
				     sb!vm:function-pointer-type
				     res)
		     (emit-move node block entry temp)
		     (vop %set-function-self node block temp res temp))
	  ;; KLUDGE: #!-GENGC nested inside #!+GENGC doesn't make much sense;
	  ;; it's just a literal translation of the CMU CL distinction between
	  ;; host and backend. If GENGC code is ever revived, this should be
	  ;; cleaned up.
	  #!-gengc (vop make-closure node block entry (length closure) res)
	  (loop for what in closure and n from 0 do
	    (unless (and (lambda-var-p what)
			 (null (leaf-refs what)))
	      (vop closure-init node block
		   res
		   (find-in-environment what this-env)
		   n
		   nil))))
	(emit-move node block entry res)))
  (values))

#!+gengc
(defun ir2-convert-set (node block)
  (declare (type cset node) (type ir2-block block))
  (let* ((cont (node-cont node))
	 (leaf (set-var node))
	 (value (set-value node))
	 (val-tn (continuation-tn node block value))
	 (locs (if (continuation-info cont)
		   (continuation-result-tns
		    cont (list (primitive-type (leaf-type leaf))))
		   nil)))
    (etypecase leaf
      (lambda-var
       (when (leaf-refs leaf)
	 (let ((tn (find-in-environment leaf (node-environment node))))
	   (if (lambda-var-indirect leaf)
	       (vop value-cell-set node block tn val-tn
		    (needs-remembering value))
	       (emit-move node block val-tn tn)))))
      (global-var
       (ecase (global-var-kind leaf)
	 ((:special :global)
	  (aver (symbolp (leaf-name leaf)))
	  (vop set node block (emit-constant (leaf-name leaf)) val-tn
	       (needs-remembering value))))))

    (when locs
      (emit-move node block val-tn (first locs))
      (move-continuation-result node block locs cont)))
  (values))

#!+gengc
(defoptimizer (%lexical-exit-breakup ir2-convert) ((info) node block)
  (vop value-cell-set node block
       (find-in-environment (continuation-value info) (node-environment node))
       (emit-constant 0)
       nil))

#!+gengc
(defoptimizer (%slot-setter ir2-convert) ((value str) node block)
  (let ((val (continuation-tn node block value)))
    (vop instance-set node block
	 (continuation-tn node block str)
	 val
	 (dsd-index
	  (slot-accessor-slot
	   (ref-leaf
	    (continuation-use
	     (combination-fun node)))))
	 (needs-remembering value))

    (move-continuation-result node block (list val) (node-cont node))))
