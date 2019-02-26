;;;; array operations for the RV32 VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;;; Allocator for the array header.
(define-vop (make-array-header)
  (:policy :fast-safe)
  (:translate make-array-header)
  (:args (type :scs (any-reg))
         (rank :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:temporary (:scs (descriptor-reg) :to (:result 0) :target result) header)
  (:temporary (:sc non-descriptor-reg) pa-flag ndescr)
  (:results (result :scs (descriptor-reg)))
  (:generator 5
    ;; Compute the allocation size.
    (cond ((zerop (- word-shift n-fixnum-tag-bits))
           (inst addi ndescr rank (+ (* array-dimensions-offset n-word-bytes)
                                     lowtag-mask)))
          (t
           (inst slli ndescr rank (- word-shift n-fixnum-tag-bits))
           (inst addi ndescr ndescr (+ (* array-dimensions-offset n-word-bytes)
                                       lowtag-mask))))
    (inst andi ndescr ndescr (bic-mask lowtag-mask))
    (pseudo-atomic (pa-flag)
      (allocation header ndescr other-pointer-lowtag :flag-tn pa-flag)
      ;; Now that we have the space allocated, compute the header
      ;; value.
      (inst slli ndescr rank (- n-widetag-bits n-fixnum-tag-bits))
      (inst addi ndescr ndescr (ash (1- array-dimensions-offset) n-widetag-bits))
      (inst srli pa-flag type n-fixnum-tag-bits)
      (inst or ndescr ndescr pa-flag)
      ;; And store the header value.
      (storew ndescr header 0 other-pointer-lowtag))
    (move result header)))

(define-vop (make-array-header/c)
  (:translate make-array-header)
  (:policy :fast-safe)
  (:arg-types (:constant t) (:constant t))
  (:info type rank)
  (:temporary (:scs (descriptor-reg) :to (:result 0) :target result) header)
  (:temporary (:sc non-descriptor-reg) pa-flag)
  (:results (result :scs (descriptor-reg)))
  (:generator 4
    (let* ((header-size (+ rank (1- array-dimensions-offset)))
           (bytes (logandc2 (+ (* (1+ header-size) n-word-bytes)
                               lowtag-mask)
                            lowtag-mask))
           (header-bits (logior (ash header-size n-widetag-bits) type)))
      (pseudo-atomic (pa-flag)
        (allocation header bytes other-pointer-lowtag :flag-tn pa-flag)
        (inst li pa-flag header-bits)
        (storew pa-flag header 0 other-pointer-lowtag)))
    (move result header)))


;;;; Additional accessors and setters for the array header.
(define-full-reffer %array-dimension *
  array-dimensions-offset other-pointer-lowtag
  (any-reg) positive-fixnum sb-kernel:%array-dimension)

(define-full-setter %set-array-dimension *
  array-dimensions-offset other-pointer-lowtag
  (any-reg) positive-fixnum sb-kernel:%set-array-dimension)

(define-vop (array-rank-vop)
  (:translate sb-kernel:%array-rank)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 6
    (loadw temp x 0 other-pointer-lowtag)
    (inst srai temp temp n-widetag-bits)
    (inst subi temp temp (1- array-dimensions-offset))
    (inst slli res temp n-fixnum-tag-bits)))

;;;; Bounds checking routine.
(define-vop (check-bound)
  (:translate %check-bound)
  (:policy :fast-safe)
  (:args (array :scs (descriptor-reg))
         (bound :scs (any-reg descriptor-reg))
         (index :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 5
    (let ((error (generate-error-code vop 'invalid-array-index-error array bound index)))
      (%test-fixnum index temp error t)
      (inst bgeu index bound error))))

;;;; Accessors/Setters

;;; Variants built on top of word-index-ref, etc.  I.e. those vectors whos
;;; elements are represented in integer registers.
(macrolet
    ((def-full-data-vector-frobs (type element-type &rest scs)
       (let ((refname (symbolicate "DATA-VECTOR-REF/" type))
             (setname (symbolicate "DATA-VECTOR-SET/" type)))
         `(progn
            (define-full-reffer ,refname ,type
              vector-data-offset other-pointer-lowtag ,scs ,element-type data-vector-ref)
            (define-full-setter ,setname ,type
              vector-data-offset other-pointer-lowtag ,scs ,element-type data-vector-set))))
     (def-partial-data-vector-frobs (type element-type size signed &rest scs)
       (let ((refname (symbolicate "DATA-VECTOR-REF/" type))
             (setname (symbolicate "DATA-VECTOR-SET/" type)))
         `(progn
            (define-partial-reffer ,refname ,type ,size ,signed vector-data-offset other-pointer-lowtag ,scs ,element-type data-vector-ref)
            (define-partial-setter ,setname ,type
              ,size vector-data-offset other-pointer-lowtag ,scs
              ,element-type data-vector-set)))))
  (def-full-data-vector-frobs simple-vector * descriptor-reg any-reg)

  (def-partial-data-vector-frobs simple-base-string character 1 nil character-reg)
  #-64-bit
  (def-full-data-vector-frobs simple-character-string character character-reg)
  #+64-bit
  (def-partial-data-vector-frobs simple-character-string character
    4 nil character-reg)

  (def-partial-data-vector-frobs simple-array-unsigned-byte-7 positive-fixnum 1 nil unsigned-reg signed-reg)
  (def-partial-data-vector-frobs simple-array-signed-byte-8 tagged-num 1 t signed-reg)
  (def-partial-data-vector-frobs simple-array-unsigned-byte-8 positive-fixnum 1 nil unsigned-reg signed-reg)
  (def-partial-data-vector-frobs simple-array-unsigned-byte-15 positive-fixnum 2 nil unsigned-reg signed-reg)
  (def-partial-data-vector-frobs simple-array-signed-byte-16 tagged-num 2 t signed-reg)
  (def-partial-data-vector-frobs simple-array-unsigned-byte-16 positive-fixnum 2 nil unsigned-reg signed-reg)
  #-64-bit
  (progn
    (def-full-data-vector-frobs simple-array-unsigned-byte-31 unsigned-num unsigned-reg)
    (def-full-data-vector-frobs simple-array-signed-byte-32 signed-num signed-reg)
    (def-full-data-vector-frobs simple-array-unsigned-byte-32 unsigned-num unsigned-reg))
  #+64-bit
  (progn
    (def-partial-data-vector-frobs simple-array-unsigned-byte-31 positive-fixnum 4 nil unsigned-reg signed-reg)
    (def-partial-data-vector-frobs simple-array-signed-byte-32 tagged-num 4 t signed-reg)
    (def-partial-data-vector-frobs simple-array-unsigned-byte-32 positive-fixnum 4 nil unsigned-reg signed-reg)
    (def-full-data-vector-frobs simple-array-unsigned-byte-63 unsigned-num unsigned-reg)
    (def-full-data-vector-frobs simple-array-signed-byte-64 signed-num signed-reg)
    (def-full-data-vector-frobs simple-array-unsigned-byte-64 unsigned-num unsigned-reg))
  (def-full-data-vector-frobs simple-array-unsigned-fixnum positive-fixnum any-reg)
  (def-full-data-vector-frobs simple-array-fixnum tagged-num any-reg))

;;; Integer vectors whose elements are smaller than a byte.  I.e. bit, 2-bit,
;;; and 4-bit vectors.
(macrolet
    ((def-small-data-vector-frobs (type bits)
       (let* ((elements-per-word (floor n-word-bits bits))
              (bit-shift (1- (integer-length elements-per-word)))
              (refname (symbolicate "DATA-VECTOR-REF/" type))
              (setname (symbolicate "DATA-VECTOR-SET/" type)))
         `(progn
            (define-vop (,refname)
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
                (inst srli temp index ,bit-shift)
                ;; Load the word in question.
                (inst slli temp temp word-shift)
                (inst add lip object temp)
                (loadw result lip vector-data-offset other-pointer-lowtag)
                ;; Compute the position of the bitfield we need.
                (inst andi temp index ,(1- elements-per-word))
                ,@(unless (= bits 1)
                    `((inst slli temp temp ,(1- (integer-length bits)))))
                ;; Shift the field we need to the low bits of RESULT.
                (inst srl result result temp)
                ;; Mask out the field we're interested in.
                (inst andi result result ,(1- (ash 1 bits)))
                ;; And fixnum-tag the result.
                (inst slli value result n-fixnum-tag-bits)))
            (define-vop (,(symbolicate "DATA-VECTOR-REF-C/" type))
              (:translate data-vector-ref)
              (:policy :fast-safe)
              (:args (object :scs (descriptor-reg)))
              (:arg-types ,type
                          (:constant
                           (integer 0
                                    ,(1- (* (1+ (- (floor (+ #x7ff
                                                             other-pointer-lowtag)
                                                          n-word-bytes)
                                                   vector-data-offset))
                                            elements-per-word)))))
              (:info index)
              (:results (result :scs (unsigned-reg)))
              (:result-types positive-fixnum)
              (:generator 15
                (multiple-value-bind (word extra) (floor index ,elements-per-word)
                  (loadw result object (+ word vector-data-offset)
                         other-pointer-lowtag)
                  (unless (zerop extra)
                    (inst srli result result (* extra ,bits)))
                  (unless (= extra ,(1- elements-per-word))
                    (inst andi result result ,(1- (ash 1 bits)))))))
            (define-vop (,setname)
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
                (inst srli temp index ,bit-shift)
                ;; Load the word in question.
                (inst slli temp temp word-shift)
                (inst add lip object temp)
                (loadw old lip vector-data-offset other-pointer-lowtag)
                ;; Compute the position of the bitfield we need.
                (inst andi shift index ,(1- elements-per-word))
                ,@(unless (= bits 1)
                    `((inst slli shift shift ,(1- (integer-length bits)))))
                ;; Clear the target bitfield.
                (unless (and (sc-is value immediate)
                             (= (tn-value value) ,(1- (ash 1 bits))))
                  (inst li temp ,(1- (ash 1 bits)))
                  (inst sll temp temp shift)
                  (inst xori temp temp -1)
                  (inst and old old temp))
                ;; LOGIOR in the new value (shifted appropriatly).
                (sc-case value
                  (immediate
                   (inst li temp (logand (tn-value value) ,(1- (ash 1 bits)))))
                  (unsigned-reg
                   (inst andi temp value ,(1- (ash 1 bits)))))
                (inst sll temp temp shift)
                (inst or old old temp)
                ;; Write the altered word back to the array.
                (storew old lip vector-data-offset other-pointer-lowtag)
                (sc-case value
                  (immediate
                   (inst li result (tn-value value)))
                  (unsigned-reg
                   (move result value)))))))))
  (def-small-data-vector-frobs simple-bit-vector 1)
  (def-small-data-vector-frobs simple-array-unsigned-byte-2 2)
  (def-small-data-vector-frobs simple-array-unsigned-byte-4 4))

;;; And the float variants.
(macrolet ((frob ()
             `(define-vop (data-vector-ref/simple-array-float)
                (:note "inline array access")
                (:policy :fast-safe)
                (:args (object)
                       (index))
                (:results (value))
                (:temporary (:scs (interior-reg)) lip)
                ,@(unless (= word-shift n-fixnum-tag-bits)
                    `((:temporary (:sc non-descriptor-reg) temp)))
                (:variant-vars format)
                (:generator 20
                  ,@(cond ((= word-shift n-fixnum-tag-bits)
                           `((inst add lip object index)))
                          (t
                           `((inst slli temp index ,(- word-shift n-fixnum-tag-bits))
                             (inst add lip object temp))))
                  (inst fload format value lip  (- (* vector-data-offset n-word-bytes)
                                                   other-pointer-lowtag))))))
  (frob))

(define-vop (data-vector-ref/simple-array-single-float data-vector-ref/simple-array-float)
  (:translate data-vector-ref)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:arg-types simple-array-single-float positive-fixnum)
  (:results (value :scs (single-reg)))
  (:result-types single-float)
  (:variant :single))

(define-vop (data-vector-ref/simple-array-double-float data-vector-ref/simple-array-float)
  (:translate data-vector-ref)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:arg-types simple-array-double-float positive-fixnum)
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:variant :double))

(macrolet ((frob ()
             `(define-vop (data-vector-set/simple-array-float)
                (:note "inline array store")
                (:policy :fast-safe)
                (:args (object)
                       (index)
                       (value :target result))
                (:results (result))
                (:temporary (:scs (interior-reg)) lip)
                ,@(unless (= word-shift n-fixnum-tag-bits)
                    `((:temporary (:sc non-descriptor-reg) temp)))
                (:variant-vars format)
                (:generator 20
                  ,@(cond ((= word-shift n-fixnum-tag-bits)
                           `((inst add lip object index)))
                          (t
                           `((inst slli temp index ,(- word-shift n-fixnum-tag-bits))
                             (inst add lip object temp))))
                  (inst fstore format value lip (- (* vector-data-offset n-word-bytes)
                                                   other-pointer-lowtag))
                  (unless (location= result value)
                    (inst fmove format result value))))))
  (frob))

(define-vop (data-vector-set/simple-array-single-float data-vector-set/simple-array-float)
  (:translate data-vector-set)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (single-reg) :target result))
  (:arg-types simple-array-single-float positive-fixnum single-float)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:variant :single))

(define-vop (data-vector-set/simple-array-double-float data-vector-set/simple-array-float)
  (:translate data-vector-set)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (double-reg) :target result))
  (:arg-types simple-array-double-float positive-fixnum double-float)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:variant :double))

;;; Complex float arrays.
(define-vop (data-vector-ref/simple-array-complex-float)
  (:note "inline array access")
  (:policy :fast-safe)
  (:args (object)
         (index))
  (:results (value))
  (:temporary (:scs (interior-reg)) lip)
  (:temporary (:sc non-descriptor-reg) temp)
  (:variant-vars format size)
  (:generator 5
    (cond ((zerop (- word-shift n-fixnum-tag-bits))
           (inst add lip object index))
          (t
           (inst slli temp index (- word-shift n-fixnum-tag-bits))
           (inst add lip object temp)))
    (let ((real-tn (complex-reg-real-tn format value)))
      (inst fload format real-tn lip (- (* vector-data-offset size)
                                        other-pointer-lowtag)))
    (let ((imag-tn (complex-reg-imag-tn format value)))
      (inst fload format imag-tn lip (- (* (1+ vector-data-offset) size)
                                        other-pointer-lowtag)))))

(define-vop (data-vector-ref/simple-array-complex-single-float data-vector-ref/simple-array-complex-float)
  (:translate data-vector-ref)
  (:arg-types simple-array-complex-single-float positive-fixnum)
  (:results (value :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:variant :single 4))

(define-vop (data-vector-ref/simple-array-complex-double-float data-vector-ref/simple-array-complex-float)
  (:translate data-vector-ref)
  (:arg-types simple-array-complex-double-float positive-fixnum)
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:variant :double 8))

(define-vop (data-vector-set/simple-array-complex-float)
  (:note "inline array store")
  (:policy :fast-safe)
  (:args (object)
         (index)
         (value :target result))
  (:results (result))
  (:temporary (:scs (interior-reg)) lip)
  (:temporary (:sc non-descriptor-reg) temp)
  (:variant-vars format size)
  (:generator 5
    (cond ((zerop (- word-shift n-fixnum-tag-bits))
           (inst add lip object index))
          (t
           (inst slli temp index (- word-shift n-fixnum-tag-bits))
           (inst add lip object temp)))
    (let ((value-real (complex-reg-real-tn format value))
          (result-real (complex-reg-real-tn format result)))
      (inst fstore format value-real lip (- (* vector-data-offset size)
                                            other-pointer-lowtag))
      (unless (location= result-real value-real)
        (inst fmove format result-real value-real)))
    (let ((value-imag (complex-reg-imag-tn format value))
          (result-imag (complex-reg-imag-tn format result)))
      (inst fstore format value-imag lip (- (* (1+ vector-data-offset) size)
                                            other-pointer-lowtag))
      (unless (location= result-imag value-imag)
        (inst fmove format result-imag value-imag)))))

(define-vop (data-vector-set/simple-array-complex-single-float data-vector-set/simple-array-complex-float)
  (:translate data-vector-set)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (complex-single-reg) :target result))
  (:arg-types simple-array-complex-single-float positive-fixnum
              complex-single-float)
  (:results (result :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:variant :single 4))

(define-vop (data-vector-set/simple-array-complex-double-float data-vector-set/simple-array-complex-float)
  (:translate data-vector-set)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (complex-double-reg) :target result))
  (:arg-types simple-array-complex-double-float positive-fixnum
              complex-double-float)
  (:results (result :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:variant :double 8))


;;; These vops are useful for accessing the bits of a vector irrespective of
;;; what type of vector it is.
(define-full-reffer vector-raw-bits * vector-data-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num %vector-raw-bits)
(define-full-setter set-vector-raw-bits * vector-data-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num %set-vector-raw-bits)
