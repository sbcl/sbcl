;;;; that part of SXHASH logic which runs not only in the target Lisp but
;;;; in the cross-compilation host Lisp

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

(sb!xc:define-modify-macro mixf (y) mix)

;;; SXHASH of FLOAT values is defined directly in terms of DEFTRANSFORM in
;;; order to avoid boxing.
(deftransform sxhash ((x) (single-float))
  '(let ((bits (single-float-bits x)))
     (logxor 66194023
	     (sxhash (the fixnum
			  (logand most-positive-fixnum
				  (logxor bits
					  (ash bits -7))))))))
(deftransform sxhash ((x) (double-float))
  '(let* ((val x)
	  (hi (double-float-high-bits val))
	  (lo (double-float-low-bits val))
	  (hilo (logxor hi lo)))
     (logxor 475038542
	     (sxhash (the fixnum
			  (logand most-positive-fixnum
				  (logxor hilo
					  (ash hilo -7))))))))

;;; SXHASH of FIXNUM values is defined as a DEFTRANSFORM because it's so
;;; simple.
(deftransform sxhash ((x) (fixnum))
  '(logand most-positive-fixnum
	   (logxor x
		   (ash x -3) ; to get sign bit into hash
		   361475658)))

;;; Some other common SXHASH cases are defined as DEFTRANSFORMs in
;;; order to avoid having to do TYPECASE at runtime.
;;;
;;; We also take the opportunity to handle the cases of constant
;;; strings, and of symbols whose names are known at compile time;
;;; except that since SXHASH on the cross-compilation host is not in
;;; general compatible with SXHASH on the target SBCL, we can't so
;;; easily do this optimization in the cross-compiler, and SBCL itself
;;; doesn't seem to need this optimization, so we don't try.
(deftransform sxhash ((x) (simple-string))
  (if #+sb-xc-host nil #-sb-xc-host (constant-continuation-p x)
      (sxhash (continuation-value x))
      '(%sxhash-simple-string x)))
(deftransform sxhash ((x) (symbol))
  (if #+sb-xc-host nil #-sb-xc-host (constant-continuation-p x)
      (sxhash (continuation-value x))
      '(%sxhash-simple-string (symbol-name x))))
