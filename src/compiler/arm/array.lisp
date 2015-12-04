;;;; array operations for the ARM VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")


;;;; Allocator for the array header.

(define-vop (make-array-header)
  (:translate make-array-header)
  (:policy :fast-safe)
  (:args (type :scs (any-reg))
         (rank :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:temporary (:scs (descriptor-reg) :to (:result 0) :target result) header)
  (:temporary (:sc non-descriptor-reg :offset ocfp-offset) pa-flag)
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:results (result :scs (descriptor-reg)))
  (:generator 5
    ;; Compute the allocation size.
    (inst add ndescr rank (+ (* array-dimensions-offset n-word-bytes)
                             lowtag-mask))
    (inst bic ndescr ndescr lowtag-mask)
    (pseudo-atomic (pa-flag)
      (allocation header ndescr other-pointer-lowtag :flag-tn pa-flag)
      ;; Now that we have the space allocated, compute the header
      ;; value.
      (inst add ndescr rank (fixnumize (1- array-dimensions-offset)))
      (inst mov ndescr (lsl ndescr (- n-widetag-bits n-fixnum-tag-bits)))
      (inst orr ndescr ndescr (lsr type n-fixnum-tag-bits))
      ;; And store the header value.
      (storew ndescr header 0 other-pointer-lowtag))
    (move result header)))

(define-vop (make-array-header/c)
  (:translate make-array-header)
  (:policy :fast-safe)
  (:arg-types (:constant t) (:constant t))
  (:info type rank)
  (:temporary (:scs (descriptor-reg) :to (:result 0) :target result) header)
  (:temporary (:sc non-descriptor-reg :offset ocfp-offset) pa-flag)
  (:results (result :scs (descriptor-reg)))
  (:generator 4
    (let* ((header-size (+ rank
                           (1- array-dimensions-offset)))
           (bytes (logandc2 (+ (* (1+ header-size) n-word-bytes)
                               lowtag-mask)
                            lowtag-mask))
           (header-bits (logior (ash header-size
                                     n-widetag-bits)
                                type)))
      (pseudo-atomic (pa-flag)
        (allocation header bytes other-pointer-lowtag :flag-tn pa-flag)
        (load-immediate-word pa-flag header-bits)
        (storew pa-flag header 0 other-pointer-lowtag)))
    (move result header)))

;;;; Additional accessors and setters for the array header.
(define-full-reffer %array-dimension *
  array-dimensions-offset other-pointer-lowtag
  (any-reg) positive-fixnum sb!kernel:%array-dimension)

(define-full-setter %set-array-dimension *
  array-dimensions-offset other-pointer-lowtag
  (any-reg) positive-fixnum sb!kernel:%set-array-dimension)

(define-vop (array-rank-vop)
  (:translate sb!kernel:%array-rank)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 6
    (loadw temp x 0 other-pointer-lowtag)
    (inst mov temp (asr temp n-widetag-bits))
    (inst sub temp temp (1- array-dimensions-offset))
    (inst mov res (lsl temp n-fixnum-tag-bits))))

;;;; Bounds checking routine.
(define-vop (check-bound)
  (:translate %check-bound)
  (:policy :fast-safe)
  (:args (array :scs (descriptor-reg))
         (bound :scs (any-reg descriptor-reg))
         (index :scs (any-reg descriptor-reg)))
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 5
    (let ((error (generate-error-code vop 'invalid-array-index-error array bound index)))
      (%test-fixnum index error t)
      (inst cmp index bound)
      (inst b :hs error))))

;;;; Accessors/Setters

;;; Variants built on top of word-index-ref, etc.  I.e. those vectors whos
;;; elements are represented in integer registers and are built out of
;;; 8, 16, or 32 bit elements.
(macrolet ((def-full-data-vector-frobs (type element-type &rest scs)
  `(progn
     (define-full-reffer ,(symbolicate "DATA-VECTOR-REF/" type) ,type
       vector-data-offset other-pointer-lowtag
       ,(remove-if #'(lambda (x) (member x '(null))) scs)
       ,element-type
       data-vector-ref)
     (define-full-setter ,(symbolicate "DATA-VECTOR-SET/" type) ,type
       vector-data-offset other-pointer-lowtag ,scs ,element-type
       data-vector-set)))

           (def-partial-data-vector-frobs (type element-type size signed &rest scs)
  `(progn
     (define-partial-reffer ,(symbolicate "DATA-VECTOR-REF/" type) ,type
       ,size ,signed vector-data-offset other-pointer-lowtag ,scs
       ,element-type data-vector-ref)
     (define-partial-setter ,(symbolicate "DATA-VECTOR-SET/" type) ,type
       ,size vector-data-offset other-pointer-lowtag ,scs
       ,element-type data-vector-set))))

  (def-full-data-vector-frobs simple-vector *
    descriptor-reg any-reg null)

  (def-partial-data-vector-frobs simple-base-string character
    :byte nil character-reg)
  #!+sb-unicode
  (def-full-data-vector-frobs simple-character-string character character-reg)

  (def-partial-data-vector-frobs simple-array-unsigned-byte-7 positive-fixnum
    :byte nil unsigned-reg signed-reg)
  (def-partial-data-vector-frobs simple-array-unsigned-byte-8 positive-fixnum
    :byte nil unsigned-reg signed-reg)

  (def-partial-data-vector-frobs simple-array-unsigned-byte-15 positive-fixnum
    :short nil unsigned-reg signed-reg)
  (def-partial-data-vector-frobs simple-array-unsigned-byte-16 positive-fixnum
    :short nil unsigned-reg signed-reg)

  (def-full-data-vector-frobs simple-array-unsigned-byte-31 unsigned-num
    unsigned-reg)
  (def-full-data-vector-frobs simple-array-unsigned-byte-32 unsigned-num
    unsigned-reg)

  (def-partial-data-vector-frobs simple-array-signed-byte-8 tagged-num
    :byte t signed-reg)

  (def-partial-data-vector-frobs simple-array-signed-byte-16 tagged-num
    :short t signed-reg)

  (def-full-data-vector-frobs simple-array-unsigned-fixnum positive-fixnum
    any-reg)
  (def-full-data-vector-frobs simple-array-fixnum tagged-num
    any-reg)

  (def-full-data-vector-frobs simple-array-signed-byte-32 signed-num
    signed-reg))

;;; Integer vectors whose elements are smaller than a byte.  I.e. bit, 2-bit,
;;; and 4-bit vectors.
(macrolet ((def-small-data-vector-frobs (type bits)
  (let* ((elements-per-word (floor n-word-bits bits))
         (bit-shift (1- (integer-length elements-per-word))))
    `(progn
       (define-vop (,(symbolicate "DATA-VECTOR-REF/" type))
         (:note "inline array access")
         (:translate data-vector-ref)
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg))
                (index :scs (unsigned-reg)))
         (:arg-types ,type positive-fixnum)
         (:results (value :scs (any-reg)))
         (:result-types positive-fixnum)
         (:temporary (:scs (interior-reg)) lip)
         (:temporary (:scs (non-descriptor-reg) :to (:result 0)) temp result)
         (:generator 20
           ;; Compute the offset for the word we're interested in.
           (inst mov temp (lsr index ,bit-shift))
           ;; Load the word in question.
           (inst add lip object (lsl temp word-shift))
           (inst ldr result (@ lip
                               (- (* vector-data-offset n-word-bytes)
                                  other-pointer-lowtag)))
           ;; Compute the position of the bitfield we need.
           (inst and temp index ,(1- elements-per-word))
           ,@(when (eq *backend-byte-order* :big-endian)
               `((inst eor temp temp ,(1- elements-per-word))))
           ,@(unless (= bits 1)
               `((inst mov temp (lsl temp ,(1- (integer-length bits))))))
           ;; Shift the field we need to the low bits of RESULT.
           (inst mov result (lsr result temp))
           ;; Mask out the field we're interested in.
           (inst and result result ,(1- (ash 1 bits)))
           ;; And fixnum-tag the result.
           (inst mov value (lsl result n-fixnum-tag-bits))))
       (define-vop (,(symbolicate "DATA-VECTOR-SET/" type))
         (:note "inline array store")
         (:translate data-vector-set)
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg))
                (index :scs (unsigned-reg) :target shift)
                (value :scs (unsigned-reg immediate) :target result))
         (:arg-types ,type positive-fixnum positive-fixnum)
         (:results (result :scs (unsigned-reg)))
         (:result-types positive-fixnum)
         (:temporary (:scs (interior-reg)) lip)
         (:temporary (:scs (non-descriptor-reg)) temp old)
         (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) shift)
         (:generator 25
           ;; Compute the offset for the word we're interested in.
           (inst mov temp (lsr index ,bit-shift))
           (inst mov temp (lsl temp n-fixnum-tag-bits))
           ;; Load the word in question.
           (inst add lip object temp)
           (inst ldr old (@ lip
                            (- (* vector-data-offset n-word-bytes)
                               other-pointer-lowtag)))
           ;; Compute the position of the bitfield we need.
           (inst and shift index ,(1- elements-per-word))
           ,@(when (eq *backend-byte-order* :big-endian)
               `((inst eor shift ,(1- elements-per-word))))
           ,@(unless (= bits 1)
               `((inst mov shift  (lsl shift ,(1- (integer-length bits))))))
           ;; Clear the target bitfield.
           (unless (and (sc-is value immediate)
                        (= (tn-value value) ,(1- (ash 1 bits))))
             (inst mov temp ,(1- (ash 1 bits)))
             (inst bic old old (lsl temp shift)))
           ;; LOGIOR in the new value (shifted appropriatly).
           (sc-case value
             (immediate
              (inst mov temp (logand (tn-value value) ,(1- (ash 1 bits)))))
             (unsigned-reg
              (inst and temp value ,(1- (ash 1 bits)))))
           (inst orr old old (lsl temp shift))
           ;; Write the altered word back to the array.
           (inst str old (@ lip
                            (- (* vector-data-offset n-word-bytes)
                               other-pointer-lowtag)))
           ;; And present the result properly.
           (sc-case value
             (immediate
              (inst mov result (tn-value value)))
             (unsigned-reg
              (move result value)))))))))
  (def-small-data-vector-frobs simple-bit-vector 1)
  (def-small-data-vector-frobs simple-array-unsigned-byte-2 2)
  (def-small-data-vector-frobs simple-array-unsigned-byte-4 4))

;;; And the float variants.
(define-vop (data-vector-ref/simple-array-single-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:arg-types simple-array-single-float positive-fixnum)
  (:results (value :scs (single-reg)))
  (:temporary (:scs (interior-reg)) lip)
  (:result-types single-float)
  (:generator 5
    (inst add lip object (- (* vector-data-offset n-word-bytes)
                              other-pointer-lowtag))
    (inst add lip lip index)
    (inst flds value (@ lip))))


(define-vop (data-vector-set/simple-array-single-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (single-reg) :target result))
  (:arg-types simple-array-single-float positive-fixnum single-float)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 5
    (inst add lip object (- (* vector-data-offset n-word-bytes)
                            other-pointer-lowtag))
    (inst add lip lip index)
    (inst fsts value (@ lip))
    (unless (location= result value)
      (inst fcpys result value))))

(define-vop (data-vector-ref/simple-array-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:arg-types simple-array-double-float positive-fixnum)
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 7
    (inst add lip object (- (* vector-data-offset n-word-bytes)
                            other-pointer-lowtag))
    (inst add lip lip (lsl index 1))
    (inst fldd value (@ lip))))

(define-vop (data-vector-set/simple-array-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (double-reg) :target result))
  (:arg-types simple-array-double-float positive-fixnum double-float)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 20
    (inst add lip object (- (* vector-data-offset n-word-bytes)
                               other-pointer-lowtag))
    (inst add lip lip (lsl index 1))
    (inst fstd value (@ lip))
    (unless (location= result value)
      (inst fcpyd result value))))

;;; Complex float arrays.

(define-vop (data-vector-ref/simple-array-complex-single-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
         (index :scs (any-reg)))
  (:arg-types simple-array-complex-single-float positive-fixnum)
  (:results (value :scs (complex-single-reg)))
  (:temporary (:scs (interior-reg)) lip)
  (:result-types complex-single-float)
  (:generator 5
    (let ((real-tn (complex-single-reg-real-tn value)))
      (inst add lip object (- (* vector-data-offset n-word-bytes)
                              other-pointer-lowtag))
      (inst add lip lip (lsl index 1))
      (inst flds real-tn (@ lip)))
    (let ((imag-tn (complex-single-reg-imag-tn value)))
      (inst flds imag-tn (@ lip n-word-bytes)))))

(define-vop (data-vector-set/simple-array-complex-single-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
         (index :scs (any-reg))
         (value :scs (complex-single-reg) :target result))
  (:arg-types simple-array-complex-single-float positive-fixnum
              complex-single-float)
  (:results (result :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 5
    (let ((value-real (complex-single-reg-real-tn value))
          (result-real (complex-single-reg-real-tn result)))
      (inst add lip object (- (* vector-data-offset n-word-bytes)
                                 other-pointer-lowtag))
      (inst add lip lip (lsl index 1))
      (inst fsts value-real (@ lip))
      (unless (location= result-real value-real)
        (inst fcpys result-real value-real)))
    (let ((value-imag (complex-single-reg-imag-tn value))
          (result-imag (complex-single-reg-imag-tn result)))
      (inst fsts value-imag (@ lip n-word-bytes))
      (unless (location= result-imag value-imag)
        (inst fcpys result-imag value-imag)))))

(define-vop (data-vector-ref/simple-array-complex-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
         (index :scs (any-reg)))
  (:arg-types simple-array-complex-double-float positive-fixnum)
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 7
    (let ((real-tn (complex-double-reg-real-tn value)))
      (inst add lip object (- (* vector-data-offset n-word-bytes)
                              other-pointer-lowtag))
      (inst add lip lip (lsl index 2))
      (inst fldd real-tn (@ lip)))
    (let ((imag-tn (complex-double-reg-imag-tn value)))
      (inst fldd imag-tn (@ lip (* 2 n-word-bytes))))))

(define-vop (data-vector-set/simple-array-complex-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
         (index :scs (any-reg))
         (value :scs (complex-double-reg) :target result))
  (:arg-types simple-array-complex-double-float positive-fixnum
              complex-double-float)
  (:results (result :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 20
    (let ((value-real (complex-double-reg-real-tn value))
          (result-real (complex-double-reg-real-tn result)))
      (inst add lip object (- (* vector-data-offset n-word-bytes)
                              other-pointer-lowtag))
      (inst add lip lip (lsl index 2))
      (inst fstd value-real (@ lip))
      (unless (location= result-real value-real)
        (inst fcpyd result-real value-real)))
    (let ((value-imag (complex-double-reg-imag-tn value))
          (result-imag (complex-double-reg-imag-tn result)))
      (inst fstd value-imag (@ lip (* 2 n-word-bytes)))
      (unless (location= result-imag value-imag)
        (inst fcpyd result-imag value-imag)))))

;;; These vops are useful for accessing the bits of a vector irrespective of
;;; what type of vector it is.
(define-full-reffer vector-raw-bits * vector-data-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num %vector-raw-bits)
(define-full-setter set-vector-raw-bits * vector-data-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num %set-vector-raw-bits)
