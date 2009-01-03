(in-package "SB!VM")

;;;; Hash primitives

;;; FIXME: This looks kludgy bad and wrong.
#+sb-assembling
(defparameter *sxhash-simple-substring-entry* (gen-label))

(define-assembly-routine
    (sxhash-simple-string
     (:translate %sxhash-simple-string)
     (:policy :fast-safe)
     (:result-types positive-fixnum))
    ((:arg string descriptor-reg a0-offset)
     (:res result any-reg a0-offset)

     (:temp length any-reg a1-offset)
     (:temp accum non-descriptor-reg nl0-offset)
     (:temp data non-descriptor-reg nl1-offset)
     (:temp offset non-descriptor-reg nl2-offset))

  (declare (ignore result accum data offset))

  ;; Save the return address.
  (inst b *sxhash-simple-substring-entry*)
  (loadw length string vector-length-slot other-pointer-lowtag))

(define-assembly-routine
    (sxhash-simple-substring
     (:translate %sxhash-simple-substring)
     (:policy :fast-safe)
     (:arg-types * positive-fixnum)
     (:result-types positive-fixnum))

    ((:arg string descriptor-reg a0-offset)
     (:arg length any-reg a1-offset)
     (:res result any-reg a0-offset)

     (:temp accum non-descriptor-reg nl0-offset)
     (:temp data non-descriptor-reg nl1-offset)
     (:temp offset non-descriptor-reg nl2-offset))

  (emit-label *sxhash-simple-substring-entry*)

  (inst li (- (* vector-data-offset n-word-bytes) other-pointer-lowtag) offset)
  (inst b test)
  (move zero-tn accum)

  LOOP
  (inst xor accum data accum)
  (inst shd accum accum 5 accum)

  TEST
  (inst ldwx offset string data)
  (inst addib :>= (fixnumize -4) length loop)
  (inst addi (fixnumize 1) offset offset)

  (inst addi (fixnumize 4) length length)
  (inst comb := zero-tn length done :nullify t)
  (inst sub zero-tn length length)
  (inst sll length 1 length)
  (inst mtctl length :sar)
  (inst shd zero-tn data :variable data)
  (inst xor accum data accum)

  DONE

  (inst sll accum 5 result)
  (inst srl result 3 result))
