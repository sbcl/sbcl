;;;; allocating simple objects

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; from signed/unsigned

;;; KLUDGE: Why don't we want vops for this one and the next
;;; one? -- WHN 19990916
#+sb-assembling ; We don't want a vop for this one.
(define-assembly-routine
    (move-from-signed)
    ((:temp eax unsigned-reg eax-offset)
     (:temp ebx unsigned-reg ebx-offset))
  (inst mov ebx eax)
  (inst shl ebx 1)
  (inst jmp :o bignum)
  (inst shl ebx 1)
  (inst jmp :o bignum)
  (inst shl ebx 1)
  (inst jmp :o bignum)
  (inst ret)
  BIGNUM

  (with-fixed-allocation (ebx bignum-widetag (+ bignum-digits-offset 1))
    (storew eax ebx bignum-digits-offset other-pointer-lowtag))

  (inst ret))

#+sb-assembling ; We don't want a vop for this one either.
(define-assembly-routine
  (move-from-unsigned)
  ((:temp eax unsigned-reg eax-offset)
   (:temp ebx unsigned-reg ebx-offset))

  (inst bsr ebx eax)
  (inst cmp ebx 61)
  (inst jmp :z DONE)
  (inst jmp :ge BIGNUM)
  ;; Fixnum
  (inst mov ebx eax)
  (inst shl ebx 3)
  DONE
  (inst ret)

  BIGNUM
  (with-fixed-allocation (ebx bignum-widetag (+ bignum-digits-offset 2))
    (storew eax ebx bignum-digits-offset other-pointer-lowtag))
  (inst ret))


