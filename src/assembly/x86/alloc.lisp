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

(file-comment
  "$Header$")

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
  (inst ret)
  BIGNUM

  (with-fixed-allocation (ebx bignum-type (+ bignum-digits-offset 1))
    (storew eax ebx bignum-digits-offset other-pointer-type))

  (inst ret))

#+sb-assembling ; We don't want a vop for this one either.
(define-assembly-routine
  (move-from-unsigned)
  ((:temp eax unsigned-reg eax-offset)
   (:temp ebx unsigned-reg ebx-offset))

  (inst test eax #xe0000000)
  (inst jmp :nz bignum)
  ;; Fixnum
  (inst mov ebx eax)
  (inst shl ebx 2)
  (inst ret)

  BIGNUM
  ;;; Note: On the mips port space for a two word bignum is always
  ;;; allocated and the header size is set to either one or two words
  ;;; as appropriate. On the mips port this is faster, and smaller
  ;;; inline, but produces more garbage. The inline x86 version uses
  ;;; the same approach, but here we save garbage and allocate the
  ;;; smallest possible bignum.
  (inst jmp :ns one-word-bignum)
  (inst mov ebx eax)

  ;; Two word bignum
  (with-fixed-allocation (ebx bignum-type (+ bignum-digits-offset 2))
    (storew eax ebx bignum-digits-offset other-pointer-type))
  (inst ret)

  ONE-WORD-BIGNUM
  (with-fixed-allocation (ebx bignum-type (+ bignum-digits-offset 1))
    (storew eax ebx bignum-digits-offset other-pointer-type))
  (inst ret))
