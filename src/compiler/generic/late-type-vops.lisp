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

(!define-type-vops fixnump check-fixnum fixnum object-not-fixnum-error
  (even-fixnum-lowtag odd-fixnum-lowtag)
  ;; we can save a register on the x86.
  :variant simple
  ;; we can save a couple of instructions and a branch on the ppc.
  :mask fixnum-tag-mask)

(!define-type-vops functionp check-fun function object-not-fun-error
  (fun-pointer-lowtag)
  :mask lowtag-mask)

(!define-type-vops listp check-list list object-not-list-error
  (list-pointer-lowtag)
  :mask lowtag-mask)

(!define-type-vops %instancep check-instance instance object-not-instance-error
  (instance-pointer-lowtag)
  :mask lowtag-mask)

(!define-type-vops %other-pointer-p nil nil nil
  (other-pointer-lowtag)
  :mask lowtag-mask)

(!define-type-vops bignump check-bignum bignum object-not-bignum-error
  (bignum-widetag))

(!define-type-vops ratiop check-ratio ratio object-not-ratio-error
  (ratio-widetag))

(!define-type-vops complexp check-complex complex object-not-complex-error
  (complex-widetag complex-single-float-widetag complex-double-float-widetag
                   #!+long-float complex-long-float-widetag))

(!define-type-vops complex-rational-p check-complex-rational nil
    object-not-complex-rational-error
  (complex-widetag))

(!define-type-vops complex-float-p check-complex-float nil
    object-not-complex-float-error
  (complex-single-float-widetag complex-double-float-widetag
                                #!+long-float complex-long-float-widetag))

(!define-type-vops complex-single-float-p check-complex-single-float complex-single-float
    object-not-complex-single-float-error
  (complex-single-float-widetag))

(!define-type-vops complex-double-float-p check-complex-double-float complex-double-float
    object-not-complex-double-float-error
  (complex-double-float-widetag))

(!define-type-vops single-float-p check-single-float single-float
    object-not-single-float-error
  (single-float-widetag))

(!define-type-vops double-float-p check-double-float double-float
    object-not-double-float-error
  (double-float-widetag))

(!define-type-vops simple-string-p check-simple-string nil
    object-not-simple-string-error
  (#!+sb-unicode simple-character-string-widetag
   simple-base-string-widetag simple-array-nil-widetag))

(macrolet
    ((define-simple-array-type-vops ()
         `(progn
           ,@(map 'list
                  (lambda (saetp)
                    (let ((primtype (saetp-primitive-type-name saetp)))
                    `(!define-type-vops
                      ,(symbolicate primtype "-P")
                      ,(symbolicate "CHECK-" primtype)
                      ,primtype
                      ,(symbolicate "OBJECT-NOT-" primtype "-ERROR")
                      (,(saetp-typecode saetp)))))
                  *specialized-array-element-type-properties*))))
  (define-simple-array-type-vops))

(!define-type-vops characterp check-character character
    object-not-character-error
  (character-widetag))

(!define-type-vops system-area-pointer-p check-system-area-pointer
      system-area-pointer
    object-not-sap-error
  (sap-widetag))

(!define-type-vops weak-pointer-p check-weak-pointer weak-pointer
    object-not-weak-pointer-error
  (weak-pointer-widetag))

(!define-type-vops code-component-p nil nil nil
  (code-header-widetag))

(!define-type-vops lra-p nil nil nil
  (return-pc-header-widetag))

(!define-type-vops fdefn-p nil nil nil
  (fdefn-widetag))

#!+(and sb-thread sb-lutex)
(!define-type-vops lutexp nil nil nil
  (lutex-widetag))

(!define-type-vops funcallable-instance-p nil nil nil
  (funcallable-instance-header-widetag))

(!define-type-vops array-header-p nil nil nil
  (simple-array-widetag
   #!+sb-unicode complex-character-string-widetag
   complex-base-string-widetag complex-bit-vector-widetag
   complex-vector-widetag complex-array-widetag complex-vector-nil-widetag))

(!define-type-vops stringp check-string nil object-not-string-error
  (#!+sb-unicode simple-character-string-widetag
   #!+sb-unicode complex-character-string-widetag
   simple-base-string-widetag complex-base-string-widetag
   simple-array-nil-widetag complex-vector-nil-widetag))

(!define-type-vops base-string-p check-base-string nil object-not-base-string-error
  (simple-base-string-widetag complex-base-string-widetag))

(!define-type-vops bit-vector-p check-bit-vector nil
    object-not-bit-vector-error
  (simple-bit-vector-widetag complex-bit-vector-widetag))

(!define-type-vops vector-nil-p check-vector-nil nil
    object-not-vector-nil-error
  (simple-array-nil-widetag complex-vector-nil-widetag))

#!+sb-unicode
(!define-type-vops character-string-p check-character-string nil
    object-not-character-string-error
  (simple-character-string-widetag complex-character-string-widetag))

(!define-type-vops vectorp check-vector nil object-not-vector-error
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
(!define-type-vops complex-vector-p check-complex-vector nil
    object-not-complex-vector-error
  (complex-vector-widetag))

(!define-type-vops simple-array-p check-simple-array nil
    object-not-simple-array-error
  (simple-array-widetag .
   #.(map 'list
          #'saetp-typecode
          *specialized-array-element-type-properties*)))

(!define-type-vops arrayp check-array nil object-not-array-error
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

(!define-type-vops numberp check-number nil object-not-number-error
  (even-fixnum-lowtag
   odd-fixnum-lowtag
   bignum-widetag
   ratio-widetag
   single-float-widetag
   double-float-widetag
   #!+long-float long-float-widetag
   complex-widetag
   complex-single-float-widetag
   complex-double-float-widetag
   #!+long-float complex-long-float-widetag))

(!define-type-vops rationalp check-rational nil object-not-rational-error
  (even-fixnum-lowtag odd-fixnum-lowtag ratio-widetag bignum-widetag))

(!define-type-vops integerp check-integer nil object-not-integer-error
  (even-fixnum-lowtag odd-fixnum-lowtag bignum-widetag))

(!define-type-vops floatp check-float nil object-not-float-error
  (single-float-widetag double-float-widetag #!+long-float long-float-widetag))

(!define-type-vops realp check-real nil object-not-real-error
  (even-fixnum-lowtag
   odd-fixnum-lowtag
   ratio-widetag
   bignum-widetag
   single-float-widetag
   double-float-widetag
   #!+long-float long-float-widetag))
