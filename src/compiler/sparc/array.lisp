;;;; the Sparc definitions for array operations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; allocator for the array header.
(define-vop (make-array-header)
  (:translate make-array-header)
  (:policy :fast-safe)
  (:args (type :scs (any-reg))
         (rank :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:temporary (:scs (descriptor-reg) :to (:result 0) :target result) header)
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:temporary (:scs (non-descriptor-reg)) gencgc-temp)
  (:results (result :scs (descriptor-reg)))
  (:generator 0
    (pseudo-atomic ()
      (inst add ndescr rank (+ (* array-dimensions-offset n-word-bytes)
                               lowtag-mask))
      (inst andn ndescr lowtag-mask)
      (allocation header ndescr other-pointer-lowtag :temp-tn gencgc-temp)
      (inst add ndescr rank (fixnumize (1- array-dimensions-offset)))
      (inst sll ndescr ndescr n-widetag-bits)
      (inst or ndescr ndescr type)
      ;; Remove the extraneous fixnum tag bits because TYPE and RANK
      ;; were fixnums
      (inst srl ndescr ndescr n-fixnum-tag-bits)
      (storew ndescr header 0 other-pointer-lowtag))
    (move result header)))

;;;; Additional accessors and setters for the array header.
(define-vop (%array-dimension word-index-ref)
  (:translate %array-dimension)
  (:policy :fast-safe)
  (:variant array-dimensions-offset other-pointer-lowtag))

(define-vop (%set-array-dimension word-index-set)
  (:translate %set-array-dimension)
  (:policy :fast-safe)
  (:variant array-dimensions-offset other-pointer-lowtag))

(define-vop (array-rank-vop)
  (:translate %array-rank)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 6
    (loadw temp x 0 other-pointer-lowtag)
    (inst sra temp n-widetag-bits)
    (inst sub temp (1- array-dimensions-offset))
    (inst sll res temp n-fixnum-tag-bits)))

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
    (let ((error (generate-error-code vop 'invalid-array-index-error
                                      array bound index)))
      (%test-fixnum index error t)
      (inst cmp index bound)
      (inst b :geu error)
      (inst nop))))

;;;; Accessors/Setters

;;; Variants built on top of word-index-ref, etc.  I.e. those vectors whos
;;; elements are represented in integer registers and are built out of
;;; 8, 16, or 32 bit elements.
(macrolet ((def-data-vector-frobs (type variant element-type &rest scs)
  `(progn
     (define-vop (,(symbolicate "DATA-VECTOR-REF/" (string type))
                  ,(symbolicate (string variant) "-REF"))
       (:note "inline array access")
       (:variant vector-data-offset other-pointer-lowtag)
       (:translate data-vector-ref)
       (:arg-types ,type positive-fixnum)
       (:results (value :scs ,scs))
       (:result-types ,element-type))
     (define-vop (,(symbolicate "DATA-VECTOR-SET/" (string type))
                  ,(symbolicate (string variant) "-SET"))
       (:note "inline array store")
       (:variant vector-data-offset other-pointer-lowtag)
       (:translate data-vector-set)
       (:arg-types ,type positive-fixnum ,element-type)
       (:args (object :scs (descriptor-reg))
              (index :scs (any-reg zero immediate))
              (value :scs ,scs))
       (:results (result :scs ,scs))
       (:result-types ,element-type)))))

  (def-data-vector-frobs simple-base-string byte-index
    character character-reg)
  #!+sb-unicode
  (def-data-vector-frobs simple-character-string word-index
    character character-reg)
  (def-data-vector-frobs simple-vector word-index
    * descriptor-reg any-reg)

  (def-data-vector-frobs simple-array-unsigned-byte-7 byte-index
    positive-fixnum unsigned-reg)
  (def-data-vector-frobs simple-array-unsigned-byte-8 byte-index
    positive-fixnum unsigned-reg)
  (def-data-vector-frobs simple-array-unsigned-byte-15 halfword-index
    positive-fixnum unsigned-reg)
  (def-data-vector-frobs simple-array-unsigned-byte-16 halfword-index
    positive-fixnum unsigned-reg)
  (def-data-vector-frobs simple-array-unsigned-byte-31 word-index
    unsigned-num unsigned-reg)
  (def-data-vector-frobs simple-array-unsigned-byte-32 word-index
    unsigned-num unsigned-reg)

  (def-data-vector-frobs simple-array-unsigned-fixnum word-index
    positive-fixnum any-reg)
  (def-data-vector-frobs simple-array-fixnum word-index
    tagged-num any-reg)
  (def-data-vector-frobs simple-array-signed-byte-32 word-index
    signed-num signed-reg))

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
         (:temporary (:scs (non-descriptor-reg) :to (:result 0)) temp result)
         (:generator 20
           (inst srl temp index ,bit-shift)
           (inst sll temp n-fixnum-tag-bits)
           (inst add temp (- (* vector-data-offset n-word-bytes)
                             other-pointer-lowtag))
           (inst ld result object temp)
           (inst and temp index ,(1- elements-per-word))
           (inst xor temp ,(1- elements-per-word))
           ,@(unless (= bits 1)
               `((inst sll temp ,(1- (integer-length bits)))))
           (inst srl result temp)
           (inst and result ,(1- (ash 1 bits)))
           (inst sll value result 2)))
       (define-vop (,(symbolicate "DATA-VECTOR-REF-C/" type))
         (:translate data-vector-ref)
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg)))
         (:arg-types ,type (:constant index))
         (:info index)
         (:results (result :scs (unsigned-reg)))
         (:result-types positive-fixnum)
         (:temporary (:scs (non-descriptor-reg)) temp)
         (:generator 15
           (multiple-value-bind (word extra)
               (floor index ,elements-per-word)
             (setf extra (logxor extra (1- ,elements-per-word)))
             (let ((offset (- (* (+ word vector-data-offset) n-word-bytes)
                              other-pointer-lowtag)))
               (cond ((typep offset '(signed-byte 13))
                      (inst ld result object offset))
                     (t
                      (inst li temp offset)
                      (inst ld result object temp))))
             (unless (zerop extra)
               (inst srl result (* extra ,bits)))
             (unless (= extra ,(1- elements-per-word))
               (inst and result ,(1- (ash 1 bits)))))))
       (define-vop (,(symbolicate "DATA-VECTOR-SET/" type))
         (:note "inline array store")
         (:translate data-vector-set)
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg))
                (index :scs (unsigned-reg) :target shift)
                (value :scs (unsigned-reg zero immediate) :target result))
         (:arg-types ,type positive-fixnum positive-fixnum)
         (:results (result :scs (unsigned-reg)))
         (:result-types positive-fixnum)
         (:temporary (:scs (non-descriptor-reg)) temp old offset)
         (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) shift)
         (:generator 25
           (inst srl offset index ,bit-shift)
           (inst sll offset n-fixnum-tag-bits)
           (inst add offset (- (* vector-data-offset n-word-bytes)
                               other-pointer-lowtag))
           (inst ld old object offset)
           (inst and shift index ,(1- elements-per-word))
           (inst xor shift ,(1- elements-per-word))
           ,@(unless (= bits 1)
               `((inst sll shift ,(1- (integer-length bits)))))
           (unless (and (sc-is value immediate)
                        (= (tn-value value) ,(1- (ash 1 bits))))
             (inst li temp ,(1- (ash 1 bits)))
             (inst sll temp shift)
             (inst not temp)
             (inst and old temp))
           (unless (sc-is value zero)
             (sc-case value
               (immediate
                (inst li temp (logand (tn-value value) ,(1- (ash 1 bits)))))
               (unsigned-reg
                (inst and temp value ,(1- (ash 1 bits)))))
             (inst sll temp shift)
             (inst or old temp))
           (inst st old object offset)
           (sc-case value
             (immediate
              (inst li result (tn-value value)))
             (t
              (move result value)))))
       (define-vop (,(symbolicate "DATA-VECTOR-SET-C/" type))
         (:translate data-vector-set)
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg))
                (value :scs (unsigned-reg zero immediate) :target result))
         (:arg-types ,type
                     (:constant index)
                     positive-fixnum)
         (:info index)
         (:results (result :scs (unsigned-reg)))
         (:result-types positive-fixnum)
         (:temporary (:scs (non-descriptor-reg)) offset-reg temp old)
         (:generator 20
           (multiple-value-bind (word extra) (floor index ,elements-per-word)
             (let ((offset (- (* (+ word vector-data-offset) n-word-bytes)
                              other-pointer-lowtag)))
               (cond ((typep offset '(signed-byte 13))
                      (inst ld old object offset))
                     (t
                      (inst li offset-reg offset)
                      (inst ld old object offset-reg)))
               (unless (and (sc-is value immediate)
                            (= (tn-value value) ,(1- (ash 1 bits))))
                 (cond ((zerop extra)
                        (inst sll old ,bits)
                        (inst srl old ,bits))
                       (t
                        (inst li temp
                              (lognot (ash ,(1- (ash 1 bits))
                                           (* (logxor extra
                                                      ,(1- elements-per-word))
                                              ,bits))))
                        (inst and old temp))))
               (sc-case value
                 (zero)
                 (immediate
                  (let ((value (ash (logand (tn-value value)
                                            ,(1- (ash 1 bits)))
                                    (* (logxor extra
                                               ,(1- elements-per-word))
                                       ,bits))))
                    (cond ((typep value '(signed-byte 13))
                           (inst or old value))
                          (t
                           (inst li temp value)
                           (inst or old temp)))))
                 (unsigned-reg
                  (inst sll temp value
                        (* (logxor extra ,(1- elements-per-word)) ,bits))
                  (inst or old temp)))
               (if (typep offset '(signed-byte 13))
                   (inst st old object offset)
                   (inst st old object offset-reg)))
             (sc-case value
               (immediate
                (inst li result (tn-value value)))
               (t
                (move result value))))))))))

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
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:result-types single-float)
  (:generator 5
    (inst add offset index (- (* vector-data-offset n-word-bytes)
                              other-pointer-lowtag))
    (inst ldf value object offset)))


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
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:generator 5
    (inst add offset index
          (- (* vector-data-offset n-word-bytes)
             other-pointer-lowtag))
    (inst stf value object offset)
    (unless (location= result value)
      (inst fmovs result value))))

(define-vop (data-vector-ref/simple-array-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:arg-types simple-array-double-float positive-fixnum)
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:generator 7
    (inst sll offset index 1)
    (inst add offset (- (* vector-data-offset n-word-bytes)
                        other-pointer-lowtag))
    (inst lddf value object offset)))

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
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:generator 20
    (inst sll offset index 1)
    (inst add offset (- (* vector-data-offset n-word-bytes)
                        other-pointer-lowtag))
    (inst stdf value object offset)
    (unless (location= result value)
      (move-double-reg result value))))

#!+long-float
(define-vop (data-vector-ref/simple-array-long-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:arg-types simple-array-long-float positive-fixnum)
  (:results (value :scs (long-reg)))
  (:result-types long-float)
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:generator 7
    (inst sll offset index 2)
    (inst add offset (- (* vector-data-offset n-word-bytes)
                        other-pointer-lowtag))
    (load-long-reg value object offset nil)))

#!+long-float
(define-vop (data-vector-set/simple-array-long-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (long-reg) :target result))
  (:arg-types simple-array-long-float positive-fixnum long-float)
  (:results (result :scs (long-reg)))
  (:result-types long-float)
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:generator 20
    (inst sll offset index 2)
    (inst add offset (- (* vector-data-offset n-word-bytes)
                        other-pointer-lowtag))
    (store-long-reg value object offset nil)
    (unless (location= result value)
      (move-long-reg result value))))


;;; XXX FIXME: Don't we have these above, in DEF-DATA-VECTOR-FROBS?
(define-vop (data-vector-ref/simple-array-signed-byte-8 signed-byte-index-ref)
  (:note "inline array access")
  (:variant vector-data-offset other-pointer-lowtag)
  (:translate data-vector-ref)
  (:arg-types simple-array-signed-byte-8 positive-fixnum)
  (:results (value :scs (signed-reg)))
  (:result-types tagged-num))

(define-vop (data-vector-set/simple-array-signed-byte-8 byte-index-set)
  (:note "inline array store")
  (:variant vector-data-offset other-pointer-lowtag)
  (:translate data-vector-set)
  (:arg-types simple-array-signed-byte-8 positive-fixnum tagged-num)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg zero immediate))
         (value :scs (signed-reg)))
  (:results (result :scs (signed-reg)))
  (:result-types tagged-num))


(define-vop (data-vector-ref/simple-array-signed-byte-16
             signed-halfword-index-ref)
  (:note "inline array access")
  (:variant vector-data-offset other-pointer-lowtag)
  (:translate data-vector-ref)
  (:arg-types simple-array-signed-byte-16 positive-fixnum)
  (:results (value :scs (signed-reg)))
  (:result-types tagged-num))

(define-vop (data-vector-set/simple-array-signed-byte-16 halfword-index-set)
  (:note "inline array store")
  (:variant vector-data-offset other-pointer-lowtag)
  (:translate data-vector-set)
  (:arg-types simple-array-signed-byte-16 positive-fixnum tagged-num)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg zero immediate))
         (value :scs (signed-reg)))
  (:results (result :scs (signed-reg)))
  (:result-types tagged-num))


;;; Complex float arrays.

(define-vop (data-vector-ref/simple-array-complex-single-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
         (index :scs (any-reg)))
  (:arg-types simple-array-complex-single-float positive-fixnum)
  (:results (value :scs (complex-single-reg)))
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:result-types complex-single-float)
  (:generator 5
    (let ((real-tn (complex-single-reg-real-tn value)))
      (inst sll offset index 1)
      (inst add offset (- (* vector-data-offset n-word-bytes)
                          other-pointer-lowtag))
      (inst ldf real-tn object offset))
    (let ((imag-tn (complex-single-reg-imag-tn value)))
      (inst add offset n-word-bytes)
      (inst ldf imag-tn object offset))))

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
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:generator 5
    (let ((value-real (complex-single-reg-real-tn value))
          (result-real (complex-single-reg-real-tn result)))
      (inst sll offset index 1)
      (inst add offset (- (* vector-data-offset n-word-bytes)
                          other-pointer-lowtag))
      (inst stf value-real object offset)
      (unless (location= result-real value-real)
        (inst fmovs result-real value-real)))
    (let ((value-imag (complex-single-reg-imag-tn value))
          (result-imag (complex-single-reg-imag-tn result)))
      (inst add offset n-word-bytes)
      (inst stf value-imag object offset)
      (unless (location= result-imag value-imag)
        (inst fmovs result-imag value-imag)))))

(define-vop (data-vector-ref/simple-array-complex-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
         (index :scs (any-reg)))
  (:arg-types simple-array-complex-double-float positive-fixnum)
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:generator 7
    (let ((real-tn (complex-double-reg-real-tn value)))
      (inst sll offset index 2)
      (inst add offset (- (* vector-data-offset n-word-bytes)
                          other-pointer-lowtag))
      (inst lddf real-tn object offset))
    (let ((imag-tn (complex-double-reg-imag-tn value)))
      (inst add offset (* 2 n-word-bytes))
      (inst lddf imag-tn object offset))))

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
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:generator 20
    (let ((value-real (complex-double-reg-real-tn value))
          (result-real (complex-double-reg-real-tn result)))
      (inst sll offset index 2)
      (inst add offset (- (* vector-data-offset n-word-bytes)
                          other-pointer-lowtag))
      (inst stdf value-real object offset)
      (unless (location= result-real value-real)
        (move-double-reg result-real value-real)))
    (let ((value-imag (complex-double-reg-imag-tn value))
          (result-imag (complex-double-reg-imag-tn result)))
      (inst add offset (* 2 n-word-bytes))
      (inst stdf value-imag object offset)
      (unless (location= result-imag value-imag)
        (move-double-reg result-imag value-imag)))))

#!+long-float
(define-vop (data-vector-ref/simple-array-complex-long-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
         (index :scs (any-reg)))
  (:arg-types simple-array-complex-long-float positive-fixnum)
  (:results (value :scs (complex-long-reg)))
  (:result-types complex-long-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:generator 7
    (let ((real-tn (complex-long-reg-real-tn value)))
      (inst sll offset index 3)
      (inst add offset (- (* vector-data-offset n-word-bytes)
                          other-pointer-lowtag))
      (load-long-reg real-tn object offset nil))
    (let ((imag-tn (complex-long-reg-imag-tn value)))
      (inst add offset (* 4 n-word-bytes))
      (load-long-reg imag-tn object offset nil))))

#!+long-float
(define-vop (data-vector-set/simple-array-complex-long-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
         (index :scs (any-reg))
         (value :scs (complex-long-reg) :target result))
  (:arg-types simple-array-complex-long-float positive-fixnum
              complex-long-float)
  (:results (result :scs (complex-long-reg)))
  (:result-types complex-long-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:generator 20
    (let ((value-real (complex-long-reg-real-tn value))
          (result-real (complex-long-reg-real-tn result)))
      (inst sll offset index 3)
      (inst add offset (- (* vector-data-offset n-word-bytes)
                          other-pointer-lowtag))
      (store-long-reg value-real object offset nil)
      (unless (location= result-real value-real)
        (move-long-reg result-real value-real)))
    (let ((value-imag (complex-long-reg-imag-tn value))
          (result-imag (complex-long-reg-imag-tn result)))
      (inst add offset (* 4 n-word-bytes))
      (store-long-reg value-imag object offset nil)
      (unless (location= result-imag value-imag)
        (move-long-reg result-imag value-imag)))))


;;; These vops are useful for accessing the bits of a vector irrespective of
;;; what type of vector it is.
(define-vop (vector-raw-bits word-index-ref)
  (:note "vector-raw-bits VOP")
  (:translate %vector-raw-bits)
  (:results (value :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:variant vector-data-offset other-pointer-lowtag))

(define-vop (set-vector-raw-bits word-index-set)
  (:note "setf vector-raw-bits VOP")
  (:translate %set-vector-raw-bits)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg zero immediate))
         (value :scs (unsigned-reg)))
  (:arg-types * tagged-num unsigned-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:variant vector-data-offset other-pointer-lowtag))
