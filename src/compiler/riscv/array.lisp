;;;; array operations for the RISC-V VM

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
    (inst andi ndescr ndescr (lognot lowtag-mask))
    (pseudo-atomic (pa-flag)
      (allocation nil ndescr other-pointer-lowtag header :flag-tn pa-flag)
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
        (allocation nil bytes other-pointer-lowtag header :flag-tn pa-flag)
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
  (:variant-vars %test-fixnum)
  (:variant t)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 6
    (let ((error (generate-error-code vop 'invalid-array-index-error array bound index)))
      (when %test-fixnum
        (%test-fixnum index temp error t))
      (inst bgeu index bound error))))

(define-vop (check-bound/fast check-bound)
  (:policy :fast)
  (:variant nil)
  (:variant-cost 4))

(define-vop (check-bound/fixnum check-bound)
  (:args (array)
         (bound)
         (index :scs (any-reg)))
  (:arg-types * * tagged-num)
  (:variant nil)
  (:variant-cost 4))

(define-vop (check-bound/untagged check-bound)
  (:args (array)
         (bound :scs (unsigned-reg signed-reg))
         (index :scs (unsigned-reg signed-reg)))
  (:arg-types * (:or unsigned-num signed-num)
                (:or unsigned-num signed-num))
  (:variant nil)
  (:variant-cost 5))
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
            (define-partial-reffer ,refname ,type
              ,size ,signed vector-data-offset other-pointer-lowtag ,scs
              ,element-type data-vector-ref)
            (define-partial-setter ,setname ,type
              ,size vector-data-offset other-pointer-lowtag ,scs
              ,element-type data-vector-set))))
     (def-float-data-vector-frobs (type format element-type size complexp &rest scs)
       (let ((refname (symbolicate "DATA-VECTOR-REF/" type))
             (setname (symbolicate "DATA-VECTOR-SET/" type)))
         `(progn
            (,(if complexp
                  'define-complex-float-reffer
                  'define-float-reffer)
             ,refname ,type
             ,size ,format vector-data-offset other-pointer-lowtag ,scs
             ,element-type t "inline array access" data-vector-ref)
            (,(if complexp
                  'define-complex-float-setter
                  'define-float-setter)
             ,setname ,type
             ,size ,format vector-data-offset other-pointer-lowtag ,scs
             ,element-type t "inline array store" data-vector-set)))))
  (def-full-data-vector-frobs simple-vector * descriptor-reg any-reg)

  (def-partial-data-vector-frobs simple-base-string character 1 nil character-reg)
  #+(and sb-unicode (not 64-bit))
  (def-full-data-vector-frobs simple-character-string character character-reg)
  #+(and sb-unicode 64-bit)
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
  (def-full-data-vector-frobs simple-array-fixnum tagged-num any-reg)

  (def-float-data-vector-frobs simple-array-single-float :single single-float 4 nil single-reg)
  (def-float-data-vector-frobs simple-array-double-float :double double-float 8 nil double-reg)
  (def-float-data-vector-frobs simple-array-complex-single-float :single complex-single-float 4 t complex-single-reg)
  (def-float-data-vector-frobs simple-array-complex-double-float :double complex-double-float 8 t complex-double-reg))

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
                (:constant (integer 0
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
                   (move result value)))))
            (define-vop (,(symbolicate "DATA-VECTOR-SET-C/" type))
              (:translate data-vector-set)
              (:policy :fast-safe)
              (:args (object :scs (descriptor-reg))
                     (value :scs (unsigned-reg immediate) :target result))
              (:arg-types ,type
                          (:constant
                           (integer 0
                                    ,(1- (* (1+ (- (floor (+ #x7ff
                                                             other-pointer-lowtag)
                                                          n-word-bytes)
                                                   vector-data-offset))
                                            elements-per-word))))
                          positive-fixnum)
              (:info index)
              (:results (result :scs (unsigned-reg)))
              (:result-types positive-fixnum)
              (:temporary (:scs (non-descriptor-reg)) temp old)
              (:generator 20
                (multiple-value-bind (word extra) (floor index ,elements-per-word)
                  (loadw old object (+ word vector-data-offset) other-pointer-lowtag)
                  (unless (and (sc-is value immediate)
                               (= (tn-value value) ,(1- (ash 1 bits))))
                    (cond ((= extra ,(1- elements-per-word))
                           (inst slli old old ,bits)
                           (inst srli old old ,bits))
                          (t
                           (inst li temp
                                 (lognot (ash ,(1- (ash 1 bits)) (* extra ,bits))))
                           (inst and old old temp))))
                  (sc-case value
                    (immediate
                     (let ((value (ash (logand (tn-value value) ,(1- (ash 1 bits)))
                                       (* extra ,bits))))
                       (typecase value
                         (short-immediate
                          (inst ori old old value))
                         (t
                          (inst li temp value)
                          (inst or old old temp)))))
                    (unsigned-reg
                     (inst slli temp value (* extra ,bits))
                     (inst or old old temp)))
                  (storew old object (+ word vector-data-offset) other-pointer-lowtag)
                  (sc-case value
                    (immediate
                     (inst li result (tn-value value)))
                    (unsigned-reg
                     (move result value))))))))))
  (def-small-data-vector-frobs simple-bit-vector 1)
  (def-small-data-vector-frobs simple-array-unsigned-byte-2 2)
  (def-small-data-vector-frobs simple-array-unsigned-byte-4 4))


;;; These vops are useful for accessing the bits of a vector irrespective of
;;; what type of vector it is.
(define-full-reffer vector-raw-bits * vector-data-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num %vector-raw-bits)
(define-full-setter set-vector-raw-bits * vector-data-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num %set-vector-raw-bits)
