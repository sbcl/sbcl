;;;; numeric things needed within the cross-compiler

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

(file-comment
  "$Header$")

;;; FIXME: This probably belongs in SB-INT instead of SB-KERNEL.
;;; And couldn't it be limited to FIXNUM arguments?
(defun positive-primep (x)
  #!+sb-doc
  "Returns T iff X is a positive prime integer."
  (declare (integer x))
  (if (<= x 5)
      (and (>= x 2) (/= x 4))
      (and (not (evenp x))
	   (not (zerop (rem x 3)))
	   (do ((q 6)
		(r 1)
		(inc 2 (logxor inc 6)) ;; 2,4,2,4...
		(d 5 (+ d inc)))
	       ((or (= r 0) (> d q)) (/= r 0))
	     (declare (fixnum inc))
	     (multiple-value-setq (q r) (truncate x d))))))
