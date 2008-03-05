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

;;;; Signed and unsigned bignums from word-sized integers. Argument
;;;; and return in the same register. No VOPs, as these are only used
;;;; as out-of-line versions: MOVE-FROM-[UN]SIGNED VOPs handle the
;;;; fixnum cases inline.

;;; #+SB-ASSEMBLING as we don't need VOPS, just the asm routines:
;;; these are out-of-line versions called by VOPs.

#+sb-assembling
(macrolet
    ((def (reg)
       `(define-assembly-routine (,(symbolicate "ALLOC-SIGNED-BIGNUM-IN-" reg))
            ((:temp number unsigned-reg ,(symbolicate reg "-OFFSET")))
          (inst push number)
          (with-fixed-allocation (number bignum-widetag (+ bignum-digits-offset 1))
            (popw number bignum-digits-offset other-pointer-lowtag))
          (inst ret))))
  (def rax)
  (def rcx)
  (def rdx)
  (def rbx)
  (def rsi)
  (def rdi)
  (def r8)
  (def r9)
  (def r10)
  (def r12)
  (def r13)
  (def r14)
  (def r15))

#+sb-assembling
(macrolet
    ((def (reg)
       `(define-assembly-routine (,(symbolicate "ALLOC-UNSIGNED-BIGNUM-IN-" reg))
            ((:temp number unsigned-reg ,(symbolicate reg "-OFFSET")))
          (inst push number)
          (inst jmp :ns one-word-bignum)
          ;; Two word bignum
          (with-fixed-allocation (number bignum-widetag (+ bignum-digits-offset 2))
            (popw number bignum-digits-offset other-pointer-lowtag))
          (inst ret)
          ONE-WORD-BIGNUM
          (with-fixed-allocation (number bignum-widetag (+ bignum-digits-offset 1))
            (popw number bignum-digits-offset other-pointer-lowtag))
          (inst ret))))
  (def rax)
  (def rcx)
  (def rdx)
  (def rbx)
  (def rsi)
  (def rdi)
  (def r8)
  (def r9)
  (def r10)
  (def r12)
  (def r13)
  (def r14)
  (def r15))
