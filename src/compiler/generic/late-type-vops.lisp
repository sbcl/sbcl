(in-package "SB!VM")

(!define-type-vops fixnump check-fixnum fixnum object-not-fixnum-error
  (even-fixnum-lowtag odd-fixnum-lowtag)
  ;; we can save a register on the x86.
  :variant simple
  ;; we can save a couple of instructions and a branch on the ppc.
  ;; FIXME: make this be FIXNUM-MASK
  :mask 3)

(!define-type-vops functionp check-fun function object-not-fun-error
  (fun-pointer-lowtag)
  :mask lowtag-mask)

(!define-type-vops listp check-list list object-not-list-error
  (list-pointer-lowtag)
  :mask lowtag-mask)

(!define-type-vops %instancep check-instance instance object-not-instance-error
  (instance-pointer-lowtag)
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

#!+long-float
(!define-type-vops complex-long-float-p check-complex-long-float complex-long-float
    object-not-complex-long-float-error
  (complex-long-float-widetag))

(!define-type-vops single-float-p check-single-float single-float
    object-not-single-float-error
  (single-float-widetag))

(!define-type-vops double-float-p check-double-float double-float
    object-not-double-float-error
  (double-float-widetag))

#!+long-float
(!define-type-vops long-float-p check-long-float long-float
    object-not-long-float-error
  (long-float-widetag))

(!define-type-vops simple-string-p check-simple-string simple-string
    object-not-simple-string-error
  (simple-string-widetag))

(!define-type-vops simple-bit-vector-p check-simple-bit-vector simple-bit-vector
    object-not-simple-bit-vector-error
  (simple-bit-vector-widetag))

(!define-type-vops simple-vector-p check-simple-vector simple-vector
    object-not-simple-vector-error
  (simple-vector-widetag))

(!define-type-vops simple-array-unsigned-byte-2-p
      check-simple-array-unsigned-byte-2
      simple-array-unsigned-byte-2
    object-not-simple-array-unsigned-byte-2-error
  (simple-array-unsigned-byte-2-widetag))

(!define-type-vops simple-array-unsigned-byte-4-p
      check-simple-array-unsigned-byte-4
      simple-array-unsigned-byte-4
    object-not-simple-array-unsigned-byte-4-error
  (simple-array-unsigned-byte-4-widetag))

(!define-type-vops simple-array-unsigned-byte-8-p
      check-simple-array-unsigned-byte-8
      simple-array-unsigned-byte-8
    object-not-simple-array-unsigned-byte-8-error
  (simple-array-unsigned-byte-8-widetag))

(!define-type-vops simple-array-unsigned-byte-16-p
      check-simple-array-unsigned-byte-16
      simple-array-unsigned-byte-16
    object-not-simple-array-unsigned-byte-16-error
  (simple-array-unsigned-byte-16-widetag))

(!define-type-vops simple-array-unsigned-byte-32-p
      check-simple-array-unsigned-byte-32
      simple-array-unsigned-byte-32
    object-not-simple-array-unsigned-byte-32-error
  (simple-array-unsigned-byte-32-widetag))

(!define-type-vops simple-array-signed-byte-8-p
      check-simple-array-signed-byte-8
      simple-array-signed-byte-8
    object-not-simple-array-signed-byte-8-error
  (simple-array-signed-byte-8-widetag))

(!define-type-vops simple-array-signed-byte-16-p
      check-simple-array-signed-byte-16
      simple-array-signed-byte-16
    object-not-simple-array-signed-byte-16-error
  (simple-array-signed-byte-16-widetag))

(!define-type-vops simple-array-signed-byte-30-p
      check-simple-array-signed-byte-30
      simple-array-signed-byte-30
    object-not-simple-array-signed-byte-30-error
  (simple-array-signed-byte-30-widetag))

(!define-type-vops simple-array-signed-byte-32-p
      check-simple-array-signed-byte-32
      simple-array-signed-byte-32
    object-not-simple-array-signed-byte-32-error
  (simple-array-signed-byte-32-widetag))

(!define-type-vops simple-array-single-float-p check-simple-array-single-float
      simple-array-single-float
    object-not-simple-array-single-float-error
  (simple-array-single-float-widetag))

(!define-type-vops simple-array-double-float-p check-simple-array-double-float
      simple-array-double-float
    object-not-simple-array-double-float-error
  (simple-array-double-float-widetag))

#!+long-float
(!define-type-vops simple-array-long-float-p check-simple-array-long-float
      simple-array-long-float
    object-not-simple-array-long-float-error
  (simple-array-long-float-widetag))

(!define-type-vops simple-array-complex-single-float-p
      check-simple-array-complex-single-float
      simple-array-complex-single-float
    object-not-simple-array-complex-single-float-error
  (simple-array-complex-single-float-widetag))

(!define-type-vops simple-array-complex-double-float-p
      check-simple-array-complex-double-float
      simple-array-complex-double-float
    object-not-simple-array-complex-double-float-error
  (simple-array-complex-double-float-widetag))

#!+long-float
(!define-type-vops simple-array-complex-long-float-p
      check-simple-array-complex-long-float
      simple-array-complex-long-float
    object-not-simple-array-complex-long-float-error
  (simple-array-complex-long-float-widetag))

(!define-type-vops base-char-p check-base-char base-char
    object-not-base-char-error
  (base-char-widetag))

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

(!define-type-vops funcallable-instance-p nil nil nil
  (funcallable-instance-header-widetag))

(!define-type-vops array-header-p nil nil nil
  (simple-array-widetag complex-string-widetag complex-bit-vector-widetag
			complex-vector-widetag complex-array-widetag))

(!define-type-vops stringp check-string nil object-not-string-error
  (simple-string-widetag complex-string-widetag))

(!define-type-vops bit-vector-p check-bit-vector nil
    object-not-bit-vector-error
  (simple-bit-vector-widetag complex-bit-vector-widetag))

(!define-type-vops vectorp check-vector nil object-not-vector-error
  (simple-string-widetag
   simple-bit-vector-widetag
   simple-vector-widetag
   simple-array-unsigned-byte-2-widetag
   simple-array-unsigned-byte-4-widetag
   simple-array-unsigned-byte-8-widetag
   simple-array-unsigned-byte-16-widetag
   simple-array-unsigned-byte-32-widetag
   simple-array-signed-byte-8-widetag
   simple-array-signed-byte-16-widetag
   simple-array-signed-byte-30-widetag
   simple-array-signed-byte-32-widetag
   simple-array-single-float-widetag
   simple-array-double-float-widetag
   #!+long-float simple-array-long-float-widetag
   simple-array-complex-single-float-widetag
   simple-array-complex-double-float-widetag
   #!+long-float simple-array-complex-long-float-widetag
   complex-string-widetag
   complex-bit-vector-widetag
   complex-vector-widetag))

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
  (simple-array-widetag
   simple-string-widetag
   simple-bit-vector-widetag
   simple-vector-widetag
   simple-array-unsigned-byte-2-widetag
   simple-array-unsigned-byte-4-widetag
   simple-array-unsigned-byte-8-widetag
   simple-array-unsigned-byte-16-widetag
   simple-array-unsigned-byte-32-widetag
   simple-array-signed-byte-8-widetag
   simple-array-signed-byte-16-widetag
   simple-array-signed-byte-30-widetag
   simple-array-signed-byte-32-widetag
   simple-array-single-float-widetag
   simple-array-double-float-widetag
   #!+long-float simple-array-long-float-widetag
   simple-array-complex-single-float-widetag
   simple-array-complex-double-float-widetag
   #!+long-float simple-array-complex-long-float-widetag))

(!define-type-vops arrayp check-array nil object-not-array-error
  (simple-array-widetag
   simple-string-widetag
   simple-bit-vector-widetag
   simple-vector-widetag
   simple-array-unsigned-byte-2-widetag
   simple-array-unsigned-byte-4-widetag
   simple-array-unsigned-byte-8-widetag
   simple-array-unsigned-byte-16-widetag
   simple-array-unsigned-byte-32-widetag
   simple-array-signed-byte-8-widetag
   simple-array-signed-byte-16-widetag
   simple-array-signed-byte-30-widetag
   simple-array-signed-byte-32-widetag
   simple-array-single-float-widetag
   simple-array-double-float-widetag
   #!+long-float simple-array-long-float-widetag
   simple-array-complex-single-float-widetag
   simple-array-complex-double-float-widetag
   #!+long-float simple-array-complex-long-float-widetag
   complex-string-widetag
   complex-bit-vector-widetag
   complex-vector-widetag
   complex-array-widetag))

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
