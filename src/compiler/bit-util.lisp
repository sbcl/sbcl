;;;; bit-vector hacking utilities, potentially implementation-dependent
;;;; for speed

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

#!-sb-fluid
(declaim (inline clear-bit-vector set-bit-vector bit-vector-replace
		 bit-vector-copy))

;;; Clear a SIMPLE-BIT-VECTOR to zeros.
(defun clear-bit-vector (vec)
  (declare (type simple-bit-vector vec))
  (bit-xor vec vec t))

;;; The old (pre-1999) code had a more-efficient-looking, but also
;;; less-portable implementation of CLEAR-BIT-VECTOR:
;;;  (do ((i sb!vm:vector-data-offset (1+ i))
;;;       (end (+ sb!vm:vector-data-offset
;;;	       (ash (+ (length vec) (1- sb!vm:n-word-bits))
;;;		    (- (1- (integer-length sb!vm:n-word-bits)))))))
;;;      ((= i end) vec)
;;;    (setf (sb!kernel:%raw-bits vec i) 0)))
;;; We could use this in the target SBCL if the new version turns out to be a
;;; bottleneck. I (WHN 19990321) will stick to the portable version for now.
;;; And by the way, if we do revisit this file with efficiency on our mind, it
;;; might be good to check whether it's really that helpful to implement
;;; all these functions as INLINE. (How expensive can it be to call a
;;; 1-argument function? How expensive is it to fill up our cache with
;;; a bunch of redundant loop expansions?)
;;;
;;; FIXME: Perhaps do simple benchmarks against CMU CL to check this.

;;; Fill a bit vector with ones.
(defun set-bit-vector (vec)
  (declare (type simple-bit-vector vec))
  (bit-orc2 vec vec t))

;;; Replace the bits in To with the bits in From.
(defun bit-vector-replace (to from)
  (declare (type simple-bit-vector to from))
  (bit-ior from from to))

;;; Copy a bit-vector.
(defun bit-vector-copy (vec)
  (declare (type simple-bit-vector vec))
  (bit-ior vec vec (make-array (length vec) :element-type 'bit)))
