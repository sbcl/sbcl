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
(macrolet ((def (reg)
             (let ((tn (symbolicate reg "-TN")))
               `(define-assembly-routine (,(symbolicate "ALLOC-SIGNED-BIGNUM-IN-" reg)) ()
                  (inst push ,tn)
                  (with-fixed-allocation (,tn bignum-widetag (+ bignum-digits-offset 1))
                    (popw ,tn bignum-digits-offset other-pointer-lowtag))
                  (inst ret)))))
  (def eax)
  (def ebx)
  (def ecx)
  (def edx)
  (def edi)
  (def esi))

#+sb-assembling
(macrolet ((def (reg)
             (let ((tn (symbolicate reg "-TN")))
               `(define-assembly-routine (,(symbolicate "ALLOC-UNSIGNED-BIGNUM-IN-" reg)) ()
                  (inst push ,tn)
                  ;; Sign flag is set by the caller! Note: The inline
                  ;; version always allocates space for two words, but
                  ;; here we minimize garbage.
                  (inst jmp :ns one-word-bignum)
                  ;; Two word bignum
                  (with-fixed-allocation (,tn bignum-widetag (+ bignum-digits-offset 2))
                    (popw ,tn bignum-digits-offset other-pointer-lowtag))
                  (inst ret)
                  ONE-WORD-BIGNUM
                  (with-fixed-allocation (,tn bignum-widetag (+ bignum-digits-offset 1))
                    (popw ,tn bignum-digits-offset other-pointer-lowtag))
                  (inst ret)))))
  (def eax)
  (def ebx)
  (def ecx)
  (def edx)
  (def edi)
  (def esi))

;;; FIXME: This is dead, right? Can it go?
#+sb-assembling
(defun frob-allocation-assembly-routine (obj lowtag arg-tn)
  `(define-assembly-routine (,(intern (format nil "ALLOCATE-~A-TO-~A" obj arg-tn)))
     ((:temp ,arg-tn descriptor-reg ,(intern (format nil "~A-OFFSET" arg-tn))))
     (pseudo-atomic
      (allocation ,arg-tn (pad-data-block ,(intern (format nil "~A-SIZE" obj))))
      (inst lea ,arg-tn (make-ea :byte :base ,arg-tn :disp ,lowtag)))
     (inst ret)))

#+sb-assembling
(macrolet ((frob-cons-routines ()
             (let ((routines nil))
               (dolist (tn-offset *dword-regs*
                        `(progn ,@routines))
                 (push (frob-allocation-assembly-routine 'cons
                                                         list-pointer-lowtag
                                                         (intern (aref *dword-register-names* tn-offset)))
                       routines)))))
  (frob-cons-routines))
