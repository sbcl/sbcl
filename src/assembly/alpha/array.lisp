;;; -*- Package: ALPHA -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;;    This file contains the support routines for arrays and vectors.
;;;
;;; Written by William Lott.
;;; Conversion by Sean Hallgren
;;; 
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

			  (:temp ndescr non-descriptor-reg nl0-offset))
  ;; This is kinda sleezy, changing words like this.  But we can because
  ;; the vop thinks it is temporary.
  (inst addq words (+ (1- (ash 1 lowtag-bits))
		      (* vector-data-offset word-bytes))
	words)
  (inst li (lognot lowtag-mask) ndescr)
  (inst and words ndescr words)
  (inst srl type word-shift ndescr)

  (pseudo-atomic ()
    (inst bis alloc-tn other-pointer-type result)
    (inst addq alloc-tn words alloc-tn)
    (storew ndescr result 0 other-pointer-type)
    (storew length result vector-length-slot other-pointer-type)))


;;;; Hash primitives
#|
(define-assembly-routine (sxhash-simple-string
			  (:translate %sxhash-simple-string)
			  (:policy :fast-safe)
			  (:result-types positive-fixnum))
			 ((:arg string descriptor-reg a0-offset)
			  (:res result any-reg a0-offset)

			  (:temp length any-reg a1-offset)

			  (:temp lip interior-reg lip-offset)
			  (:temp accum non-descriptor-reg nl0-offset)
			  (:temp data non-descriptor-reg nl1-offset)
			  (:temp byte non-descriptor-reg nl2-offset)
			  (:temp retaddr non-descriptor-reg nl3-offset)
			  (:temp temp1 non-descriptor-reg nl4-offset))

  ;; These are needed after we jump into sxhash-simple-substring.
  (progn result lip accum data byte  retaddr)

  (inst li (make-fixup 'sxhash-simple-substring :assembly-routine) temp1)
  (loadw length string vector-length-slot other-pointer-type)
  (inst jmp zero-tn temp1
	(make-fixup 'sxhash-simple-substring :assembly-routine)))

(define-assembly-routine (sxhash-simple-substring
			  (:translate %sxhash-simple-substring)
			  (:policy :fast-safe)
			  (:arg-types * positive-fixnum)
			  (:result-types positive-fixnum))
			 ((:arg string descriptor-reg a0-offset)
			  (:arg length any-reg a1-offset)
			  (:res result any-reg a0-offset)

			  (:temp lip interior-reg lip-offset)
			  (:temp accum non-descriptor-reg nl0-offset)
			  (:temp data non-descriptor-reg nl1-offset)
			  (:temp byte non-descriptor-reg nl2-offset)
			  (:temp retaddr non-descriptor-reg nl3-offset))

  ;; Save the return address
  (inst subq lip code-tn retaddr)

  ;; Get a pointer to the data.
  (inst addq string
	(- (* vector-data-offset word-bytes) other-pointer-type)
	lip)
  (move zero-tn accum)
  (inst br zero-tn test)

  loop

  (inst and data #xff byte)
  (inst xor accum byte accum)
  (inst sll accum 5 byte)
  (inst srl accum 27 accum)
  (inst mskll accum 4 accum)
  (inst bis accum byte accum)

  (inst srl data 8 byte)
  (inst and byte #xff byte)
  (inst xor accum byte accum)
  (inst sll accum 5 byte)
  (inst srl accum 27 accum)
  (inst mskll accum 4 accum)
  (inst bis accum byte accum)

  (inst srl data 16 byte)
  (inst and byte #xff byte)
  (inst xor accum byte accum)
  (inst sll accum 5 byte)
  (inst srl accum 27 accum)
  (inst mskll accum 4 accum)
  (inst bis accum byte accum)

  (inst srl data 24 byte)
  (inst xor accum byte accum)
  (inst sll accum 5 byte)
  (inst srl accum 27 accum)
  (inst mskll accum 4 accum)
  (inst bis accum byte accum)

  (inst addq lip 4 lip)

  test

  (inst subq length (fixnum 4) length)
  (inst ldl data 0 lip)
  (inst bge length loop)

  (inst addq length (fixnum 3) length)
  (inst beq length one-more)
  (inst subq length (fixnum 1) length)
  (inst beq length two-more)
  (inst bne length done)

  (inst srl data 16 byte)
  (inst and byte #xff byte)
  (inst xor accum byte accum)
  (inst sll accum 5 byte)
  (inst srl accum 27 accum)
  (inst mskll accum 4 accum)
  (inst bis accum byte accum)
  (inst addq length (fixnum 1) length)

  two-more

  (inst subq length (fixnum 1) length)
  (inst srl data 8 byte)
  (inst and byte #xff byte)
  (inst xor accum byte accum)
  (inst sll accum 5 byte)
  (inst srl accum 27 accum)
  (inst mskll accum 4 accum)
  (inst bis accum byte accum)
  (inst addq length (fixnum 1) length)

  one-more

  (inst subq length (fixnum 1) length)
  (inst and data #xff byte)
  (inst xor accum byte accum)
  (inst sll accum 5 byte)
  (inst srl accum 27 accum)
  (inst mskll accum 4 accum)
  (inst bis accum byte accum)

  done

  (inst sll accum 5 result)
  (inst mskll result 4 result)
  (inst srl result 3 result)

  ;; Restore the return address.
  (inst addq code-tn retaddr lip))
|#