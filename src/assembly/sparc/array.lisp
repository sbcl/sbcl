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
    ;; boxed words == unboxed bytes
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
