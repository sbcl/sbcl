;;;; various array operations that are too expensive (in space) to do
;;;; inline

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; allocation

(define-assembly-routine (allocate-vector
			  (:policy :fast-safe)
			  (:translate allocate-vector)
			  (:arg-types positive-fixnum
				      positive-fixnum
				      positive-fixnum))
			 ((:arg type unsigned-reg eax-offset)
			  (:arg length any-reg ebx-offset)
			  (:arg words any-reg ecx-offset)
			  (:res result descriptor-reg edx-offset))
  (inst mov result (+ (1- (ash 1 n-lowtag-bits))
		      (* vector-data-offset word-bytes)))
  (inst add result words)
  (inst and result (lognot sb!vm:lowtag-mask))
  (pseudo-atomic
   (allocation result result)
   (inst lea result (make-ea :byte :base result :disp other-pointer-lowtag))
   (storew type result 0 other-pointer-lowtag)
   (storew length result vector-length-slot other-pointer-lowtag))
  (inst ret))

;;;; Note: CMU CL had assembly language primitives for hashing strings,
;;;; but SBCL doesn't.
