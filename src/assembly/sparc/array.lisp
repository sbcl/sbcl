;;;; support routines for arrays and vectors

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

(define-assembly-routine (allocate-vector
			  (:policy :fast-safe)
			  (:translate allocate-vector)
			  (:arg-types positive-fixnum
				      positive-fixnum
				      positive-fixnum))
			 ((:arg type any-reg a0-offset)
			  (:arg length any-reg a1-offset)
			  (:arg words any-reg a2-offset)
			  (:res result descriptor-reg a0-offset)

			  (:temp ndescr non-descriptor-reg nl0-offset)
			  (:temp vector descriptor-reg a3-offset))
  (pseudo-atomic ()
    (inst or vector alloc-tn other-pointer-lowtag)
    (inst add ndescr words (* (1+ vector-data-offset) n-word-bytes))
    (inst andn ndescr 7)
    (inst add alloc-tn ndescr)
    (inst srl ndescr type word-shift)
    (storew ndescr vector 0 other-pointer-lowtag)
    (storew length vector vector-length-slot other-pointer-lowtag))
  ;; This makes sure the zero byte at the end of a string is paged in so
  ;; the kernel doesn't bitch if we pass it the string.
  (storew zero-tn alloc-tn 0)
  (move result vector))



;;;; Hash primitives

;;; this is commented out in the alpha port. I'm therefore going to
;;; comment it out here pending explanation -- CSR, 2001-08-31.

#|
#+assembler
(defparameter sxhash-simple-substring-entry (gen-label))

(define-assembly-routine (sxhash-simple-string
			  (:translate %sxhash-simple-string)
			  (:policy :fast-safe)
			  (:result-types positive-fixnum))
			 ((:arg string descriptor-reg a0-offset)
			  (:res result any-reg a0-offset)

			  (:temp length any-reg a1-offset)
			  (:temp accum non-descriptor-reg nl0-offset)
			  (:temp data non-descriptor-reg nl1-offset)
			  (:temp temp non-descriptor-reg nl2-offset)
			  (:temp offset non-descriptor-reg nl3-offset))

  (declare (ignore result accum data temp offset))

  (inst b sxhash-simple-substring-entry)
  (loadw length string vector-length-slot other-pointer-lowtag))


(define-assembly-routine (sxhash-simple-substring
			  (:translate %sxhash-simple-substring)
			  (:policy :fast-safe)
			  (:arg-types * positive-fixnum)
			  (:result-types positive-fixnum))
			 ((:arg string descriptor-reg a0-offset)
			  (:arg length any-reg a1-offset)
			  (:res result any-reg a0-offset)

			  (:temp accum non-descriptor-reg nl0-offset)
			  (:temp data non-descriptor-reg nl1-offset)
			  (:temp temp non-descriptor-reg nl2-offset)
			  (:temp offset non-descriptor-reg nl3-offset))
  (emit-label sxhash-simple-substring-entry)

  (inst li offset (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
  (inst b test)
  (move accum zero-tn)

  LOOP

  (inst xor accum data)
  (inst sll temp accum 27)
  (inst srl accum 5)
  (inst or accum temp)
  (inst add offset 4)

  TEST

  (inst subcc length (fixnumize 4))
  (inst b :ge loop)
  (inst ld data string offset)

  (inst addcc length (fixnumize 4))
  (inst b :eq done)
  (inst neg length)
  (inst sll length 1)
  (inst srl data length)
  (inst xor accum data)

  DONE

  (inst sll result accum 5)
  (inst srl result result 3))
|#
