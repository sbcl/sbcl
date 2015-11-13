;;;; generic type testing and checking VOPs

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.
(in-package "SB!VM")


(define-vop (type-predicate)
  (:args (value :scs (any-reg descriptor-reg)))
  (:temporary #!+(or x86 x86-64)
              (:sc unsigned-reg :offset eax-offset)
              #!-(or x86 x86-64)
              (:sc non-descriptor-reg)
              temp)
  #!+(or x86 x86-64)
  (:ignore temp)
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe))

#!+(or x86 x86-64)
(define-vop (simple-type-predicate)
  (:args (value :scs (any-reg descriptor-reg control-stack)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe))

(defmacro !define-type-vop (pred-name type-codes
                             &optional (inherit 'type-predicate))
  (let ((cost (+ (* 2 (length type-codes))
                 (if (> (reduce #'max type-codes :key #'eval) lowtag-limit)
                     7
                     2))))
    `(define-vop (,pred-name ,inherit)
       (:translate ,pred-name)
       (:generator ,cost
         (test-type value target not-p ,type-codes
                    #!-(or x86-64 x86) :temp #!-(or x86-64 x86) temp)))))

(!define-type-vop fixnump
  #.fixnum-lowtags
  #!+(or x86-64 x86) simple-type-predicate) ;; save a register

(!define-type-vop functionp (fun-pointer-lowtag))

(!define-type-vop listp (list-pointer-lowtag))

(!define-type-vop %instancep (instance-pointer-lowtag))

(!define-type-vop %other-pointer-p (other-pointer-lowtag))

(!define-type-vop bignump (bignum-widetag))

(!define-type-vop ratiop (ratio-widetag))

(!define-type-vop complexp
  (complex-widetag complex-single-float-widetag complex-double-float-widetag
                   #!+long-float complex-long-float-widetag))

(!define-type-vop complex-rational-p (complex-widetag))

(!define-type-vop complex-float-p
  (complex-single-float-widetag complex-double-float-widetag
                                #!+long-float complex-long-float-widetag))

(!define-type-vop complex-single-float-p (complex-single-float-widetag))

(!define-type-vop complex-double-float-p (complex-double-float-widetag))

(!define-type-vop single-float-p (single-float-widetag))

(!define-type-vop double-float-p (double-float-widetag))

(!define-type-vop simple-string-p
  (#!+sb-unicode simple-character-string-widetag
   simple-base-string-widetag simple-array-nil-widetag))

(macrolet
    ((define-simple-array-type-vops ()
         `(progn
           ,@(map 'list
                  (lambda (saetp)
                    (let ((primtype (saetp-primitive-type-name saetp)))
                    `(!define-type-vop
                      ,(symbolicate primtype "-P")
                      (,(saetp-typecode saetp)))))
                  *specialized-array-element-type-properties*))))
  (define-simple-array-type-vops))

(macrolet
    ((def ()
       `(!define-type-vop simple-rank-1-array-*-p
         ,(map 'list #'saetp-typecode
               *specialized-array-element-type-properties*))))
  (def)) ; simple-rank-1-array-*-p

(!define-type-vop characterp (character-widetag))

(!define-type-vop system-area-pointer-p (sap-widetag))

(!define-type-vop weak-pointer-p (weak-pointer-widetag))

(!define-type-vop code-component-p (code-header-widetag))

(!define-type-vop lra-p (return-pc-header-widetag))

(!define-type-vop fdefn-p (fdefn-widetag))

(!define-type-vop closurep (closure-header-widetag))

(!define-type-vop simple-fun-p (simple-fun-header-widetag))

(!define-type-vop funcallable-instance-p (funcallable-instance-header-widetag))

(!define-type-vop array-header-p
  (simple-array-widetag
   #!+sb-unicode complex-character-string-widetag
   complex-base-string-widetag complex-bit-vector-widetag
   complex-vector-widetag complex-array-widetag complex-vector-nil-widetag))

(!define-type-vop stringp
  (#!+sb-unicode simple-character-string-widetag
   #!+sb-unicode complex-character-string-widetag
   simple-base-string-widetag complex-base-string-widetag
   simple-array-nil-widetag complex-vector-nil-widetag))

(!define-type-vop base-string-p
  (simple-base-string-widetag complex-base-string-widetag))

(!define-type-vop bit-vector-p
  (simple-bit-vector-widetag complex-bit-vector-widetag))

(!define-type-vop vector-nil-p
  (simple-array-nil-widetag complex-vector-nil-widetag))

#!+sb-unicode
(!define-type-vop character-string-p
  (simple-character-string-widetag complex-character-string-widetag))

(!define-type-vop vectorp
  (complex-vector-widetag .
   #.(append
      (map 'list
           #'saetp-typecode
           *specialized-array-element-type-properties*)
      (mapcan (lambda (saetp)
                (when (saetp-complex-typecode saetp)
                  (list (saetp-complex-typecode saetp))))
              (coerce *specialized-array-element-type-properties* 'list)))))

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
(!define-type-vop complex-vector-p (complex-vector-widetag))

(!define-type-vop simple-array-p
  (simple-array-widetag .
   #.(map 'list
          #'saetp-typecode
          *specialized-array-element-type-properties*)))

(!define-type-vop arrayp
  (simple-array-widetag
   complex-array-widetag
   complex-vector-widetag .
   #.(append
      (map 'list
           #'saetp-typecode
           *specialized-array-element-type-properties*)
      (mapcan (lambda (saetp)
                (when (saetp-complex-typecode saetp)
                  (list (saetp-complex-typecode saetp))))
              (coerce *specialized-array-element-type-properties* 'list)))))

(!define-type-vop numberp
  (bignum-widetag
   ratio-widetag
   single-float-widetag
   double-float-widetag
   #!+long-float long-float-widetag
   complex-widetag
   complex-single-float-widetag
   complex-double-float-widetag
   #!+long-float complex-long-float-widetag
   . #.fixnum-lowtags))

(!define-type-vop rationalp
  (ratio-widetag bignum-widetag . #.fixnum-lowtags))

(!define-type-vop integerp
  (bignum-widetag . #.fixnum-lowtags))

(!define-type-vop floatp
  (single-float-widetag double-float-widetag #!+long-float long-float-widetag))

(!define-type-vop realp
  (ratio-widetag
   bignum-widetag
   single-float-widetag
   double-float-widetag
   #!+long-float long-float-widetag
   . #.fixnum-lowtags))

#!+sb-simd-pack
(!define-type-vop simd-pack-p (simd-pack-widetag))
