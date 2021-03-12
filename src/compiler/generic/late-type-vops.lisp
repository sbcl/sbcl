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


;;; FIXME: backend-specific junk doesn't belong in compiler/generic.

#+(or x86 x86-64)
(progn
(define-vop (type-predicate)
  (:args (value :scs (any-reg descriptor-reg)))
  ;; x86 code has to avoid 'esi' and 'edi' for the temp
  ;; since they can't be accessed as an 8-bit byte.
  ;; x86-64 being more regular, any reg can serve as the temp.
  ;; In all likelihood, it'll get rax anyway just because.
  (:temporary (:sc unsigned-reg #+x86 :offset #+x86 eax-offset) temp)
  (:conditional)
  (:info target not-p)
  (:args-var args)
  (:policy :fast-safe))
(define-vop (simple-type-predicate)
  (:args (value :scs (any-reg descriptor-reg control-stack)))
  (:conditional)
  (:info target not-p)
  (:args-var args)
  (:policy :fast-safe))
;; A vop that accepts a computed set of widetags.
(define-vop (%other-pointer-subtype-p type-predicate)
  (:translate %other-pointer-subtype-p)
  (:info target not-p widetags)
  (:arg-types * (:constant t)) ; voodoo - 'target' and 'not-p' are absent
  (:generator 15 ; arbitrary
    (multiple-value-bind (headers except) (canonicalize-widetags+exceptions widetags)
      (%test-headers value temp target not-p nil headers :except except
                                                         :value-tn-ref args)))))

#-(or x86 x86-64)
(progn
(define-vop (type-predicate)
  (:args (value :scs (any-reg descriptor-reg)))
  (:temporary (:sc non-descriptor-reg) temp)
  (:conditional)
  (:info target not-p)
  (:args-var args)
  (:policy :fast-safe))
;; A vop that accepts a computed set of widetags.
(define-vop (%other-pointer-subtype-p type-predicate)
  (:translate %other-pointer-subtype-p)
  (:info target not-p widetags)
  (:arg-types * (:constant t)) ; voodoo - 'target' and 'not-p' are absent
  (:args-var args)
  (:generator 15 ; arbitrary
    (%test-headers value temp target not-p nil (canonicalize-widetags widetags)
                   :value-tn-ref args))))

(defmacro define-type-vop (pred-name type-codes
                             &optional (inherit 'type-predicate))
  (let ((cost (if (> (reduce #'max type-codes :key #'eval) lowtag-limit)
                  7
                  4)))
    `(define-vop (,pred-name ,inherit)
       (:translate ,pred-name)
       (:generator ,cost
         (test-type value
                    ,(if (eq inherit 'simple-type-predicate) nil 'temp)
                    target not-p ,type-codes
                    :value-tn-ref args)))))

#-x86-64 ; defined in compiler/x86-64/type-vops for x86-64
(progn
(define-type-vop unbound-marker-p (unbound-marker-widetag))

(define-type-vop characterp (character-widetag))

(define-type-vop single-float-p (single-float-widetag))

(define-type-vop fixnump
  #.fixnum-lowtags
  #+(or x86) simple-type-predicate) ;; save a register

(define-type-vop functionp (fun-pointer-lowtag))

(define-type-vop listp (list-pointer-lowtag))

(define-type-vop %instancep (instance-pointer-lowtag))

(define-type-vop %other-pointer-p (other-pointer-lowtag))

(define-type-vop non-null-symbol-p (symbol-widetag))

(define-type-vop closurep (closure-widetag))

(define-type-vop simple-fun-p (simple-fun-widetag))

(define-type-vop funcallable-instance-p (funcallable-instance-widetag))
) ; end PROGN

(define-type-vop bignump (bignum-widetag))

(define-type-vop ratiop (ratio-widetag))

(define-type-vop complexp
  (complex-widetag complex-single-float-widetag complex-double-float-widetag
                   #+long-float complex-long-float-widetag))

(define-type-vop complex-rational-p (complex-widetag))

(define-type-vop complex-float-p
  (complex-single-float-widetag complex-double-float-widetag
                                #+long-float complex-long-float-widetag))

(define-type-vop complex-single-float-p (complex-single-float-widetag))

(define-type-vop complex-double-float-p (complex-double-float-widetag))

(define-type-vop double-float-p (double-float-widetag))

(define-type-vop simple-string-p
  (#+sb-unicode simple-character-string-widetag
   simple-base-string-widetag))

#-x86-64
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

(macrolet
    ((def ()
       `(define-type-vop simple-rank-1-array-*-p
         ,(map 'list #'saetp-typecode
               *specialized-array-element-type-properties*))))
  (def)) ; simple-rank-1-array-*-p

(define-type-vop system-area-pointer-p (sap-widetag))

(define-type-vop weak-pointer-p (weak-pointer-widetag))

(define-type-vop code-component-p (code-header-widetag))

#-(or x86 x86-64) (define-type-vop lra-p (return-pc-widetag))

(define-type-vop fdefn-p (fdefn-widetag))

(define-type-vop array-header-p
  (simple-array-widetag
   #+sb-unicode complex-character-string-widetag
   complex-base-string-widetag complex-bit-vector-widetag
   complex-vector-widetag complex-array-widetag))

(define-type-vop simple-array-header-p
  (simple-array-widetag))

(define-type-vop stringp
  (#+sb-unicode simple-character-string-widetag
   #+sb-unicode complex-character-string-widetag
   simple-base-string-widetag complex-base-string-widetag))

(define-type-vop base-string-p
  (simple-base-string-widetag complex-base-string-widetag))

(define-type-vop bit-vector-p
  (simple-bit-vector-widetag complex-bit-vector-widetag))

#+sb-unicode
(define-type-vop character-string-p
  (simple-character-string-widetag complex-character-string-widetag))

(define-type-vop vectorp
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
(define-type-vop complex-vector-p (complex-vector-widetag))

(define-type-vop simple-array-p
  (simple-array-widetag .
   #.(map 'list
          #'saetp-typecode
          *specialized-array-element-type-properties*)))

(define-type-vop arrayp
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

(define-type-vop numberp
  (bignum-widetag
   ratio-widetag
   single-float-widetag
   double-float-widetag
   #+long-float long-float-widetag
   complex-widetag
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
