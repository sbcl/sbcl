;;;; This file implements LOAD-TIME-VALUE.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

(defknown %load-time-value (t) t (flushable movable))

(def-ir1-translator load-time-value ((form &optional read-only-p) start cont)
  #!+sb-doc
  "Arrange for FORM to be evaluated at load-time and use the value produced
   as if it were a constant. If READ-ONLY-P is non-NIL, then the resultant
   object is guaranteed to never be modified, so it can be put in read-only
   storage."
  (if (producing-fasl-file)
      (multiple-value-bind (handle type)
	  (compile-load-time-value (if read-only-p
				       form
				       `(make-value-cell ,form)))
	(declare (ignore type))
	(ir1-convert start cont
		     (if read-only-p
			 `(%load-time-value ',handle)
			 `(value-cell-ref (%load-time-value ',handle)))))
      (let ((value
	     (handler-case (eval form)
	       (error (condition)
		 (compiler-error "(during EVAL of LOAD-TIME-VALUE)~%~A"
				 condition)))))
	(ir1-convert start cont
		     (if read-only-p
			 `',value
			 `(value-cell-ref ',(make-value-cell value)))))))

(defoptimizer (%load-time-value ir2-convert) ((handle) node block)
  (assert (constant-continuation-p handle))
  (let ((cont (node-cont node))
	(tn (make-load-time-value-tn (continuation-value handle)
				     *universal-type*)))
    (move-continuation-result node block (list tn) cont)))
