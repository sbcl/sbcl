;;;; the MIPS definitions for array operations

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
  (:arg-types positive-fixnum positive-fixnum)
  (:temporary (:scs (non-descriptor-reg)) bytes header temp)
  (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:results (result :scs (descriptor-reg)))
  (:generator 13
    (inst addu bytes rank (+ (* array-dimensions-offset n-word-bytes)
                             lowtag-mask))
    (inst srl bytes n-lowtag-bits)
    (inst sll bytes n-lowtag-bits)
    ;; Compute the encoded rank. See ENCODE-ARRAY-RANK.
    (inst addu header rank (fixnumize -1))
    (inst and header header (fixnumize array-rank-mask))
    (inst sll header header array-rank-position)
    (inst or header type)
    ;; Remove the extraneous fixnum tag bits because TYPE and RANK
    ;; were fixnums
    (inst srl header n-fixnum-tag-bits)
    (pseudo-atomic (pa-flag)
      (allocation type bytes result other-pointer-lowtag `(,pa-flag ,temp))
      (storew header result 0 other-pointer-lowtag))))

;;;; Additional accessors and setters for the array header.
(define-full-reffer %array-dimension *
  array-dimensions-offset other-pointer-lowtag
  (any-reg) positive-fixnum %array-dimension)

(define-full-setter %set-array-dimension *
  array-dimensions-offset other-pointer-lowtag
  (any-reg) positive-fixnum %set-array-dimension)

(define-vop ()
  (:translate %array-rank)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    ;; convert ARRAY-RANK-POSITION to byte index and compensate for endianness
    ;; ASSUMPTION: n-widetag-bits = 8 and rank is adjacent to widetag
    (inst lbu res x #+little-endian (- 1 other-pointer-lowtag)
                    #+big-endian    (- 2 other-pointer-lowtag))
    (inst nop)
    (inst addu res 1)
    (inst and res array-rank-mask)))

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
    (let ((error (generate-error-code vop 'invalid-array-index-error
                                      array bound index)))
      (%test-fixnum index temp error t)
      (inst sltu temp index bound)
      (inst beq temp error)
      (inst nop))))

;;;; Accessors/Setters

;;; Variants built on top of word-index-ref, etc.  I.e. those vectors whos
;;; elements are represented in integer registers and are built out of
;;; 8, 16, or 32 bit elements.
(macrolet ((def-full-data-vector-frobs (type element-type &rest scs)
             `(progn (define-full-reffer ,(symbolicate "DATA-VECTOR-REF/" type) ,type
                       vector-data-offset other-pointer-lowtag
                       ,(remove-if #'(lambda (x) (member x '(null zero))) scs)
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

  ;; SIMPLE-VECTOR
  (define-full-reffer data-vector-ref/simple-vector simple-vector
    vector-data-offset other-pointer-lowtag (descriptor-reg any-reg) * data-vector-ref)
  (define-vop (data-vector-set/simple-vector)
    (:translate data-vector-set)
    (:policy :fast-safe)
    (:args (object :scs (descriptor-reg)) (index :scs (any-reg))
           (value :scs (descriptor-reg any-reg null zero)))
    (:arg-types simple-vector tagged-num *)
    (:temporary (:scs (non-descriptor-reg)) ea temp)
    (:vop-var vop)
    (:generator 6
      ;; We could potentially eliminate the ADDIU by ensuring that a simple-vector
      ;; never starts 2 words before the end of a card.
      ;; However, that's tricky to reason about and I don't care to do it.
      ;; (and also it's maybe not correct for card-spanning vectors)
      (inst addu ea object index)
      (inst addu ea ea (- (ash vector-data-offset word-shift) other-pointer-lowtag))
      (without-scheduling ()
        (emit-gengc-barrier object ea temp (vop-nth-arg 2 vop))
        (storew value ea 0 0))))
  (define-vop (data-vector-set/simple-vector-c)
    (:translate data-vector-set)
    (:policy :fast-safe)
    (:args (object :scs (descriptor-reg)) (value :scs (descriptor-reg any-reg null zero)))
    (:temporary (:scs (non-descriptor-reg)) ea temp)
    (:info index)
    ;; not sure if the load/store-index is off by something now
    (:arg-types simple-vector (:constant (load/store-index 4 7 2)) *)
    (:vop-var vop)
    (:generator 5
      (inst addu ea object (- (ash (+ vector-data-offset index) word-shift) other-pointer-lowtag))
      (without-scheduling ()
        (emit-gengc-barrier object ea temp (vop-nth-arg 1 vop))
        (storew value object (+ vector-data-offset index) other-pointer-lowtag))))

  (def-partial-data-vector-frobs simple-base-string character
    :byte nil character-reg)
  #+sb-unicode
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
           (inst srl temp index ,bit-shift)
           (inst sll temp n-fixnum-tag-bits)
           (inst addu lip object temp)
           (inst lw result lip
                 (- (* vector-data-offset n-word-bytes)
                    other-pointer-lowtag))
           (inst and temp index ,(1- elements-per-word))
           ,@(when (eq *backend-byte-order* :big-endian)
               `((inst xor temp ,(1- elements-per-word))))
           ,@(unless (= bits 1)
               `((inst sll temp ,(1- (integer-length bits)))))
           (inst srl result temp)
           (inst and result ,(1- (ash 1 bits)))
           (inst sll value result n-fixnum-tag-bits)))
       (define-vop (,(symbolicate "DATA-VECTOR-REF-C/" type))
         (:translate data-vector-ref)
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg)))
         (:arg-types ,type
                     (:constant
                      (integer 0
                               ,(1- (* (1+ (- (floor (+ #x7fff
                                                        other-pointer-lowtag)
                                                     n-word-bytes)
                                              vector-data-offset))
                                       elements-per-word)))))
         (:info index)
         (:results (result :scs (unsigned-reg)))
         (:result-types positive-fixnum)
         (:generator 15
           (multiple-value-bind (word extra) (floor index ,elements-per-word)
             ,@(when (eq *backend-byte-order* :big-endian)
                 `((setf extra (logxor extra (1- ,elements-per-word)))))
             (loadw result object (+ word vector-data-offset)
                    other-pointer-lowtag)
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
                (value :scs (unsigned-reg zero immediate)))
         (:arg-types ,type positive-fixnum positive-fixnum)
         (:temporary (:scs (interior-reg)) lip)
         (:temporary (:scs (non-descriptor-reg)) temp old)
         (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) shift)
         (:generator 25
           (inst srl temp index ,bit-shift)
           (inst sll temp n-fixnum-tag-bits)
           (inst addu lip object temp)
           (inst lw old lip
                 (- (* vector-data-offset n-word-bytes)
                    other-pointer-lowtag))
           (inst and shift index ,(1- elements-per-word))
           ,@(when (eq *backend-byte-order* :big-endian)
               `((inst xor shift ,(1- elements-per-word))))
           ,@(unless (= bits 1)
               `((inst sll shift ,(1- (integer-length bits)))))
           (unless (and (sc-is value immediate)
                        (= (tn-value value) ,(1- (ash 1 bits))))
             (inst li temp ,(1- (ash 1 bits)))
             (inst sll temp shift)
             (inst nor temp temp zero-tn)
             (inst and old temp))
           (unless (sc-is value zero)
             (sc-case value
               (immediate
                (inst li temp (logand (tn-value value) ,(1- (ash 1 bits)))))
               (unsigned-reg
                (inst and temp value ,(1- (ash 1 bits)))))
             (inst sll temp shift)
             (inst or old temp))
           (inst sw old lip
                 (- (* vector-data-offset n-word-bytes)
                    other-pointer-lowtag))))
       (define-vop (,(symbolicate "DATA-VECTOR-SET-C/" type))
         (:translate data-vector-set)
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg))
                (value :scs (unsigned-reg zero immediate)))
         (:arg-types ,type
                     (:constant
                      (integer 0
                               ,(1- (* (1+ (- (floor (+ #x7fff
                                                        other-pointer-lowtag)
                                                     n-word-bytes)
                                              vector-data-offset))
                                       elements-per-word))))
                     positive-fixnum)
         (:info index)
         (:temporary (:scs (non-descriptor-reg)) temp old)
         (:generator 20
           (multiple-value-bind (word extra) (floor index ,elements-per-word)
             ,@(when (eq *backend-byte-order* :big-endian)
                 `((setf extra (logxor extra (1- ,elements-per-word)))))
             (inst lw old object
                   (- (* (+ word vector-data-offset) n-word-bytes)
                      other-pointer-lowtag))
             (unless (and (sc-is value immediate)
                          (= (tn-value value) ,(1- (ash 1 bits))))
               (cond ((= extra ,(1- elements-per-word))
                      (inst sll old ,bits)
                      (inst srl old ,bits))
                     (t
                      (inst li temp
                            (lognot (ash ,(1- (ash 1 bits)) (* extra ,bits))))
                      (inst and old temp))))
             (sc-case value
               (zero)
               (immediate
                (let ((value (ash (logand (tn-value value) ,(1- (ash 1 bits)))
                                  (* extra ,bits))))
                  (cond ((< value #x10000)
                         (inst or old value))
                        (t
                         (inst li temp value)
                         (inst or old temp)))))
               (unsigned-reg
                (inst sll temp value (* extra ,bits))
                (inst or old temp)))
             (inst sw old object
                   (- (* (+ word vector-data-offset) n-word-bytes)
                      other-pointer-lowtag)))))))))
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
  (:result-types single-float)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 20
    (inst addu lip object index)
    (inst lwc1 value lip
          (- (* vector-data-offset n-word-bytes)
             other-pointer-lowtag))
    (inst nop)))

(define-vop (data-vector-set/simple-array-single-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (single-reg)))
  (:arg-types simple-array-single-float positive-fixnum single-float)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 20
    (inst addu lip object index)
    (inst swc1 value lip
          (- (* vector-data-offset n-word-bytes)
             other-pointer-lowtag))))

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
  (:generator 20
    (inst addu lip object index)
    (inst addu lip index)
    (ecase *backend-byte-order*
      (:big-endian
       (inst lwc1 value lip
             (+ (- (* vector-data-offset n-word-bytes)
                   other-pointer-lowtag)
                n-word-bytes))
       (inst lwc1-odd value lip
             (- (* vector-data-offset n-word-bytes)
                other-pointer-lowtag)))
      (:little-endian
       (inst lwc1 value lip
             (- (* vector-data-offset n-word-bytes)
                other-pointer-lowtag))
       (inst lwc1-odd value lip
             (+ (- (* vector-data-offset n-word-bytes)
                   other-pointer-lowtag)
                n-word-bytes))))
    (inst nop)))

(define-vop (data-vector-set/simple-array-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (double-reg)))
  (:arg-types simple-array-double-float positive-fixnum double-float)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 20
    (inst addu lip object index)
    (inst addu lip index)
    (ecase *backend-byte-order*
      (:big-endian
       (inst swc1 value lip
             (+ (- (* vector-data-offset n-word-bytes)
                   other-pointer-lowtag)
                n-word-bytes))
       (inst swc1-odd value lip
             (- (* vector-data-offset n-word-bytes)
                other-pointer-lowtag)))
      (:little-endian
       (inst swc1 value lip
             (- (* vector-data-offset n-word-bytes)
                other-pointer-lowtag))
       (inst swc1-odd value lip
             (+ (- (* vector-data-offset n-word-bytes)
                   other-pointer-lowtag)
                n-word-bytes))))))

;;; Complex float arrays.
(define-vop (data-vector-ref/simple-array-complex-single-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:arg-types simple-array-complex-single-float positive-fixnum)
  (:results (value :scs (complex-single-reg)))
  (:temporary (:scs (interior-reg)) lip)
  (:result-types complex-single-float)
  (:generator 5
    (inst addu lip object index)
    (inst addu lip index)
    (let ((real-tn (complex-single-reg-real-tn value)))
      (inst lwc1 real-tn lip (- (* vector-data-offset n-word-bytes)
                                other-pointer-lowtag)))
    (let ((imag-tn (complex-single-reg-imag-tn value)))
      (inst lwc1 imag-tn lip (- (* (1+ vector-data-offset) n-word-bytes)
                                other-pointer-lowtag)))
    (inst nop)))

(define-vop (data-vector-set/simple-array-complex-single-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (complex-single-reg)))
  (:arg-types simple-array-complex-single-float positive-fixnum
              complex-single-float)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 5
    (inst addu lip object index)
    (inst addu lip index)
    (let ((value-real (complex-single-reg-real-tn value)))
      (inst swc1 value-real lip (- (* vector-data-offset n-word-bytes)
                                   other-pointer-lowtag)))
    (let ((value-imag (complex-single-reg-imag-tn value)))
      (inst swc1 value-imag lip (- (* (1+ vector-data-offset) n-word-bytes)
                                   other-pointer-lowtag)))))

(define-vop (data-vector-ref/simple-array-complex-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg) :target shift))
  (:arg-types simple-array-complex-double-float positive-fixnum)
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:temporary (:scs (interior-reg)) lip)
  (:temporary (:scs (any-reg) :from (:argument 1)) shift)
  (:generator 6
    (inst sll shift index n-fixnum-tag-bits)
    (inst addu lip object shift)
    (let ((real-tn (complex-double-reg-real-tn value)))
      (ld-double real-tn lip (- (* vector-data-offset n-word-bytes)
                                other-pointer-lowtag)))
    (let ((imag-tn (complex-double-reg-imag-tn value)))
      (ld-double imag-tn lip (- (* (+ vector-data-offset 2) n-word-bytes)
                                other-pointer-lowtag)))
    (inst nop)))

(define-vop (data-vector-set/simple-array-complex-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg) :target shift)
         (value :scs (complex-double-reg)))
  (:arg-types simple-array-complex-double-float positive-fixnum
              complex-double-float)
  (:temporary (:scs (interior-reg)) lip)
  (:temporary (:scs (any-reg) :from (:argument 1)) shift)
  (:generator 6
    (inst sll shift index n-fixnum-tag-bits)
    (inst addu lip object shift)
    (let ((value-real (complex-double-reg-real-tn value)))
      (str-double value-real lip (- (* vector-data-offset n-word-bytes)
                                    other-pointer-lowtag)))
    (let ((value-imag (complex-double-reg-imag-tn value)))
      (str-double value-imag lip (- (* (+ vector-data-offset 2) n-word-bytes)
                                    other-pointer-lowtag)))))


;;; These vops are useful for accessing the bits of a vector irrespective of
;;; what type of vector it is.
(define-full-reffer vector-raw-bits * vector-data-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num %vector-raw-bits)
(define-full-setter set-vector-raw-bits * vector-data-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num %set-vector-raw-bits)
;;; Weak vectors
(define-full-reffer %weakvec-ref * vector-data-offset other-pointer-lowtag
  (any-reg descriptor-reg) * %weakvec-ref)
(define-full-setter %weakvec-set * vector-data-offset other-pointer-lowtag
  (any-reg descriptor-reg) * %weakvec-set)
