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
			  (:temp pa-flag non-descriptor-reg nl3-offset)
			  (:temp vector descriptor-reg a3-offset))
  (pseudo-atomic (pa-flag)
    (inst ori vector alloc-tn sb!vm:other-pointer-lowtag)
    (inst addi ndescr words (* (1+ sb!vm:vector-data-offset) sb!vm:n-word-bytes))
    (inst clrrwi ndescr ndescr n-lowtag-bits)
    (inst add alloc-tn alloc-tn ndescr)
    (inst srwi ndescr type sb!vm:word-shift)
    (storew ndescr vector 0 sb!vm:other-pointer-lowtag)
    (storew length vector sb!vm:vector-length-slot sb!vm:other-pointer-lowtag))
  (move result vector))



;;;; Hash primitives

#+sb-assembling
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

  (loadw length string sb!vm:vector-length-slot sb!vm:other-pointer-lowtag)
  (inst b sxhash-simple-substring-entry))


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
  (move accum zero-tn)
  (inst b test)

  LOOP

  (inst xor accum accum data)
  (inst slwi temp accum 27)
  (inst srwi accum accum 5)
  (inst or accum accum temp)
  (inst addi offset offset 4)

  TEST

  (inst subic. length length (fixnumize 4))
  (inst lwzx data string offset)
  (inst bge loop)

  (inst addic. length length (fixnumize 4))
  (inst neg length length)
  (inst beq done)
  (inst slwi length length 1)
  (inst srw data data length)
  (inst xor accum accum data)

  DONE

  (inst slwi result accum 5)
  (inst srwi result result 3))
