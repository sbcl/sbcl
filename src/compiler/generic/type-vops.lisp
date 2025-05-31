;;;; generic type testing and checking VOPs

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.
(in-package "SB-VM")


(defglobal *other-pointer-type-vops*
  ;; A special case due to NIL
  `(symbolp (,symbol-widetag)))

;;; We use a default definition of the vop for PRED-NAME only if it was not
;;; already defined by the backend in {arch}/type-vops.  DEFINE-VOP has a compile-time
;;; effect of storing the vop name in *BACKEND-PARSED-VOPS*, so it's correct to
;;; look in that hash-table at macroexpansion time here.
;;; But due to multiple processing passes, the vop could exist in the table
;;; from its default definition, so that's gotta be allowed, or else make-host-2
;;; would produce a null expansion for every type-vop.
(defmacro define-type-vop (pred-name type-codes)
  ;; This EVAL is a bit sloppy but it (somewhat surprisingly) serves a real purpose
  ;; in that you can gives the set of types code as symbols and/or integers.
  (let ((cost (if (> (reduce #'max type-codes :key #'eval) lowtag-limit)
                  7
                  4)))
    `(progn
       (let ((type-codes (list ,@type-codes)))
         (when (loop for type in type-codes
                     never (or (< type lowtag-limit)
                               (memq type +immediate-types+)
                               (memq type +function-widetags+)))
           (setf (getf *other-pointer-type-vops* ',pred-name)
                 (canonicalize-widetags type-codes))))
       ,(unless (awhen (gethash pred-name sb-c::*backend-parsed-vops*)
                  (string/= (sb-c::vop-parse-note it) "defaulted"))
          `(define-vop (,pred-name type-predicate)
             (:translate ,pred-name)
             (:note "defaulted")
             (:generator ,cost
               (test-type value temp target not-p ,type-codes :value-tn-ref args)))))))

(define-type-vop unbound-marker-p (unbound-marker-widetag))

(define-type-vop characterp (character-widetag))

(define-type-vop single-float-p (single-float-widetag))

(define-type-vop fixnump #.fixnum-lowtags)

(define-type-vop functionp (fun-pointer-lowtag))

(define-type-vop listp (list-pointer-lowtag))

(define-type-vop %instancep (instance-pointer-lowtag))

(define-type-vop %other-pointer-p (other-pointer-lowtag))

(define-type-vop non-null-symbol-p (symbol-widetag))

(define-type-vop closurep (closure-widetag))

(define-type-vop simple-fun-p (simple-fun-widetag))

(define-type-vop funcallable-instance-p (funcallable-instance-widetag))

(define-type-vop bignump (bignum-widetag))

(define-type-vop ratiop (ratio-widetag))

(define-type-vop complexp
  (complex-rational-widetag complex-single-float-widetag complex-double-float-widetag
                   #+long-float complex-long-float-widetag))

(define-type-vop complex-rational-p (complex-rational-widetag))

(define-type-vop complex-float-p
  (complex-single-float-widetag complex-double-float-widetag
                                #+long-float complex-long-float-widetag))

(define-type-vop complex-single-float-p (complex-single-float-widetag))

(define-type-vop complex-double-float-p (complex-double-float-widetag))

(define-type-vop double-float-p (double-float-widetag))

(define-type-vop simple-string-p
  (#+sb-unicode simple-character-string-widetag
   simple-base-string-widetag))

(macrolet
    ((define-simple-array-type-vops ()
         `(progn
           ,@(map 'list
                  (lambda (saetp)
                    (let ((primtype (saetp-primitive-type-name saetp)))
                    `(define-type-vop
                      ,(symbolicate primtype "-P")
                      (,(saetp-typecode saetp)))))
                  *specialized-array-element-type-properties*))))
  (define-simple-array-type-vops))

;;; Stupid name for this because SIMPLE-VECTOR means something else.
(define-type-vop simple-rank-1-array-*-p #.+simple-rank-1-array-widetags+)

(define-type-vop system-area-pointer-p (sap-widetag))

(define-type-vop weak-pointer-p (weak-pointer-widetag))

(define-type-vop code-component-p (code-header-widetag))

#-(or x86 x86-64 arm64 riscv)
(define-type-vop lra-p (return-pc-widetag))

(define-type-vop fdefn-p (fdefn-widetag))

(define-type-vop array-header-p
  (simple-array-widetag
   #+sb-unicode complex-character-string-widetag
   complex-base-string-widetag complex-bit-vector-widetag
   complex-vector-widetag complex-array-widetag))

(define-type-vop simple-array-header-p
  (simple-array-widetag))

(define-type-vop stringp #.+string-widetags+)

(define-type-vop base-string-p
  (simple-base-string-widetag complex-base-string-widetag))

(define-type-vop bit-vector-p
  (simple-bit-vector-widetag complex-bit-vector-widetag))

#+sb-unicode
(define-type-vop character-string-p
  (simple-character-string-widetag complex-character-string-widetag))

(define-type-vop vectorp #.+vector-widetags+)

;;; Note that this "type VOP" is sort of an oddball; it doesn't so
;;; much test for a Lisp-level type as just expose a low-level type
;;; code at the Lisp level. It is used as a building block to help us
;;; to express things like the test for (TYPEP FOO '(VECTOR T))
;;; efficiently in Lisp code, but it doesn't correspond to any type
;;; expression which would actually occur in reasonable application
;;; code. (Common Lisp doesn't have any natural way of expressing this
;;; type.) Thus, there's no point in building up the full machinery of
;;; associated backend type predicates and so forth as we do for
;;; ordinary type VOPs.
(define-type-vop complex-vector-p (complex-vector-widetag))

(define-type-vop simple-array-p #.+simple-array-widetags+)

(define-type-vop arrayp #.+array-widetags+)

(define-type-vop numberp
  (bignum-widetag
   ratio-widetag
   single-float-widetag
   double-float-widetag
   #+long-float long-float-widetag
   complex-rational-widetag
   complex-single-float-widetag
   complex-double-float-widetag
   #+long-float complex-long-float-widetag
   . #.fixnum-lowtags))

(define-type-vop rationalp
  (ratio-widetag bignum-widetag . #.fixnum-lowtags))

(define-type-vop integerp
  (bignum-widetag . #.fixnum-lowtags))

(define-type-vop floatp
  (single-float-widetag double-float-widetag #+long-float long-float-widetag))

(define-type-vop realp
  (ratio-widetag
   bignum-widetag
   single-float-widetag
   double-float-widetag
   #+long-float long-float-widetag
   . #.fixnum-lowtags))

#+sb-simd-pack
(define-type-vop simd-pack-p (simd-pack-widetag))
#+sb-simd-pack-256
(define-type-vop simd-pack-256-p (simd-pack-256-widetag))

;;; Not type vops, but generic over all backends
(macrolet ((def (name lowtag)
             `(define-vop ()
                (:translate ,name)
                (:policy :fast-safe)
                (:args (x :scs (descriptor-reg)))
                (:results (res :scs (unsigned-reg)))
                (:result-types unsigned-num)
                (:generator 2 (loadw res x 0 ,lowtag)))))
  (def function-header-word fun-pointer-lowtag)
  (def instance-header-word instance-pointer-lowtag))
