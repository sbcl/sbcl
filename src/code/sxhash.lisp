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
  '(let* ((val (+ 0.0f0 x))
	  (bits (single-float-bits val)))
     (logxor 66194023
	     (sxhash (the fixnum
			  (logand most-positive-fixnum
				  (logxor bits
					  (ash bits -7))))))))
(deftransform sxhash ((x) (double-float))
  '(let* ((val (+ 0.0d0 x))
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
	   (logxor (ash (logand x (ash most-positive-fixnum -4)) 4) 
		   (ash x -1) ; to get sign bit into hash
		   361475658)))

;;; SXHASH of SIMPLE-BIT-VECTOR values is defined as a DEFTRANSFORM
;;; because it is endian-dependent.
(deftransform sxhash ((x) (simple-bit-vector))
  `(let ((result 410823708))
    (declare (type fixnum result))
    (let ((length (length x)))
      (cond
	((= length 0) (mix result (sxhash 0)))
	(t
	 (mixf result (sxhash (length x)))
	 (do* ((i sb!vm:vector-data-offset (+ i 1))
	       ;; FIXME: should we respect DEPTHOID?  SXHASH on
	       ;; strings doesn't seem to...
	       (end-1 (+ sb!vm:vector-data-offset
			 (floor (1- length) sb!vm:n-word-bits))))
	      ((= i end-1)
	       (let ((num
		      (logand
		       (ash (1- (ash 1 (mod length sb!vm:n-word-bits)))
			    ,(ecase sb!c:*backend-byte-order*
			       (:little-endian 0)
			       (:big-endian
				'(- sb!vm:n-word-bits
				    (mod length sb!vm:n-word-bits)))))
		       (%raw-bits x i))))
		 (declare (type (unsigned-byte 32) num))
		 (mix result ,(ecase sb!c:*backend-byte-order*
				(:little-endian
				 '(logand num most-positive-fixnum))
				(:big-endian
				 '(ash num (- sb!vm:n-lowtag-bits)))))))
	   (declare (type index i end-1))
	   (let ((num (%raw-bits x i)))
	     (declare (type (unsigned-byte 32) num))
	     (mixf result ,(ecase sb!c:*backend-byte-order*
			     (:little-endian
			      '(logand num most-positive-fixnum))
			     ;; FIXME: I'm not certain that
			     ;; N-LOWTAG-BITS is the clearest way of
			     ;; expressing this: it's essentially the
			     ;; difference between `(UNSIGNED-BYTE
			     ;; ,SB!VM:N-WORD-BITS) and (AND FIXNUM
			     ;; UNSIGNED-BYTE).
			     (:big-endian
			      '(ash num (- sb!vm:n-lowtag-bits))))))))))))

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
  (if #+sb-xc-host nil #-sb-xc-host (constant-lvar-p x)
      (sxhash (lvar-value x))
      '(%sxhash-simple-string x)))
(deftransform sxhash ((x) (symbol))
  (if #+sb-xc-host nil #-sb-xc-host (constant-lvar-p x)
      (sxhash (lvar-value x))
      '(%sxhash-simple-string (symbol-name x))))


