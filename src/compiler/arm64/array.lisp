;;;; array operations for the ARM VM

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
  (:translate make-array-header)
  (:policy :fast-safe)
  (:args (type :scs (any-reg))
         (rank :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:temporary (:scs (descriptor-reg) :to (:result 0) :target result) header)
  (:temporary (:sc non-descriptor-reg) ndescr)
  (:temporary (:scs (non-descriptor-reg) :offset lr-offset) lr)
  (:results (result :scs (descriptor-reg)))
  (:generator 5
    ;; Compute the allocation size.
    (inst lsl ndescr rank (- word-shift n-fixnum-tag-bits))
    (inst add ndescr ndescr (+ (* array-dimensions-offset n-word-bytes)
                               lowtag-mask))
    (inst and ndescr ndescr (bic-mask lowtag-mask))
    (pseudo-atomic (lr)
      (allocation nil ndescr other-pointer-lowtag header :flag-tn lr)
      ;; Now that we have the space allocated, compute the header
      ;; value.
      ;; See ENCODE-ARRAY-RANK.
      (inst sub ndescr rank (fixnumize 1))
      (inst and ndescr ndescr (fixnumize array-rank-mask))
      (inst orr ndescr type (lsl ndescr array-rank-position))
      (inst lsr ndescr ndescr n-fixnum-tag-bits)
      ;; And store the header value.
      (storew ndescr header 0 other-pointer-lowtag))
    (move result header)))

;;;; Additional accessors and setters for the array header.
(define-full-reffer %array-dimension *
  array-dimensions-offset other-pointer-lowtag
  (any-reg) positive-fixnum sb-kernel:%array-dimension)

(define-full-setter %set-array-dimension *
  array-dimensions-offset other-pointer-lowtag
  (any-reg) positive-fixnum sb-kernel:%set-array-dimension)

(define-vop ()
  (:translate %array-rank)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (inst ldrsb res (@ x (- (/ array-rank-position n-byte-bits)
                          other-pointer-lowtag)))
    (inst add res res 1)))

(define-vop ()
  (:translate %array-rank=)
  (:policy :fast-safe)
  (:args (array :scs (descriptor-reg)))
  (:temporary (:scs (unsigned-reg)) x)
  (:info rank)
  (:arg-types * (:constant t))
  (:conditional :eq)
  (:generator 2
    (inst ldrb x (@ array (- (/ array-rank-position n-byte-bits)
                            other-pointer-lowtag)))
    (inst cmp x (add-sub-immediate (encode-array-rank rank)))))

(define-vop (simple-array-header-of-rank-p type-predicate)
  (:translate sb-c::simple-array-header-of-rank-p)
  (:info target not-p rank)
  (:arg-types * (:constant t))
  (:generator 2
    (unless (other-pointer-tn-ref-p args)
      (%test-lowtag value temp (if not-p
                                   target
                                   drop-through)
                    t other-pointer-lowtag))
    (inst ldrh temp (@ value (- other-pointer-lowtag)))
    (inst cmp temp (add-sub-immediate
                     (dpb (encode-array-rank rank)
                       (byte 8 array-rank-position)
                       simple-array-widetag)))
    (inst b (if not-p
                :ne
                :eq)
      target)
    drop-through))

(defun vector-length-p (node)
  (destructuring-bind (array bound index) (sb-c::combination-args node)
    (declare (ignore index))
    (let ((ref (sb-c::principal-lvar-ref array))
          (bound-use (sb-c::principal-lvar-use bound)))
      (when (and ref
                 (sb-c::combination-p bound-use)
                 (eq (sb-c::combination-fun-source-name bound-use nil) 'sb-c::vector-length))
        (let ((ref2 (sb-c::principal-lvar-ref (car (sb-c::combination-args bound-use)))))
          (and ref
               (eq (sb-c::ref-leaf ref)
                   (sb-c::ref-leaf ref2))))))))

(defun power-of-two-limit-p (x)
  (and (fixnump x)
       (= (logcount (1+ x)) 1)))

;;;; Bounds checking routine.
(define-vop (check-bound)
  (:translate %check-bound)
  (:policy :fast-safe)
  (:args (array :scs (descriptor-reg constant))
         (bound :scs (any-reg descriptor-reg)
                :load-if (not (and (sc-is bound immediate)
                                   (not (sc-is index immediate))
                                   (typep (tn-value bound)
                                          '(and sc-offset
                                            (satisfies fixnum-add-sub-immediate-p))))))
         (index :scs (any-reg descriptor-reg)
                :load-if (not (and (sc-is index immediate)
                                   (typep (tn-value index)
                                          '(and sc-offset
                                            (satisfies fixnum-add-sub-immediate-p)))))))
  (:variant-vars %test-fixnum)
  (:variant t)
  (:vop-var vop)
  (:node-var node)
  (:save-p :compute-only)
  (:generator 6
    (let ((error (if (vector-length-p node)
                     (generate-error-code vop 'sb-kernel::invalid-vector-index-error
                                          array index)
                     (generate-error-code vop 'invalid-array-index-error
                                          array bound index)))
          (bound (if (sc-is bound immediate)
                     (let ((value (tn-value bound)))
                       (cond ((and %test-fixnum
                                   (power-of-two-limit-p (1- value)))
                              (lognot (fixnumize (1- value))))
                             ((sc-is index any-reg descriptor-reg)
                              (fixnumize value))
                             (t
                              value)))
                     bound))
          (index (if (sc-is index immediate)
                     (let ((value (tn-value index)))
                       (if (sc-is bound any-reg descriptor-reg)
                           (fixnumize value)
                           value))
                     index)))
      (cond ((eql bound -1)
             (inst cbnz index error))
            ((typep bound '(integer * -1))
             ;; Power of two bound, can be checked for fixnumness at
             ;; the same time as it always occupies a consecutive bit
             ;; range, everything else, including the tag, has to be
             ;; zero.
             (inst tst index bound)
             (inst b :ne error))
            (t
             (when (and %test-fixnum (not (integerp index)))
               (%test-fixnum index nil error t))
             (cond ((integerp bound)
                    (inst cmp index bound)
                    (inst b :hs error))
                   ((eql index 0)
                    (inst cbz bound error))
                   ((and (tn-p index)
                         (sc-is index unsigned-reg signed-reg))
                    (inst cmp index (lsr bound 1))
                    (inst b :hs error))
                   (t
                    (inst cmp bound index)
                    (inst b :ls error))))))))

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
         (bound)
         (index :scs (unsigned-reg signed-reg)))
  (:arg-types * *
              (:or unsigned-num signed-num))
  (:variant nil)
  (:variant-cost 5))
;;;; Accessors/Setters

;;; Variants built on top of word-index-ref, etc.  I.e. those vectors whos
;;; elements are represented in integer registers and are built out of
;;; 8, 16, 32, or 64 bit elements.
(macrolet ((full (type element-type &rest scs)
             `(progn
                (define-full-reffer ,(symbolicate "DATA-VECTOR-REF/" type) ,type
                  vector-data-offset other-pointer-lowtag
                  ,scs
                  ,element-type
                  data-vector-ref)
                (define-full-setter ,(symbolicate "DATA-VECTOR-SET/" type) ,type
                  vector-data-offset other-pointer-lowtag ,scs ,element-type
                  data-vector-set)))

           (partial (type element-type size signed &rest scs)
             `(progn
                (define-partial-reffer ,(symbolicate "DATA-VECTOR-REF/" type) ,type
                  ,size ,signed vector-data-offset other-pointer-lowtag ,scs
                  ,element-type data-vector-ref)
                (define-partial-setter ,(symbolicate "DATA-VECTOR-SET/" type) ,type
                  ,size vector-data-offset other-pointer-lowtag ,scs
                  ,element-type data-vector-set))))

  (full simple-vector * descriptor-reg any-reg)
  (full simple-array-unsigned-byte-63 unsigned-num unsigned-reg)
  (full simple-array-unsigned-byte-64 unsigned-num unsigned-reg)
  (full simple-array-unsigned-fixnum positive-fixnum any-reg)
  (full simple-array-fixnum tagged-num any-reg)
  (full simple-array-signed-byte-64 signed-num signed-reg)

  (partial simple-base-string character :byte nil character-reg)
  #+sb-unicode
  (partial simple-character-string character :word nil character-reg)

  (partial simple-array-unsigned-byte-7 positive-fixnum :byte nil unsigned-reg signed-reg)
  (partial simple-array-unsigned-byte-8 positive-fixnum :byte nil unsigned-reg signed-reg)

  (partial simple-array-unsigned-byte-15 positive-fixnum :short nil unsigned-reg signed-reg)
  (partial simple-array-unsigned-byte-16 positive-fixnum :short nil unsigned-reg signed-reg)

  (partial simple-array-unsigned-byte-31 positive-fixnum :word nil unsigned-reg signed-reg)
  (partial simple-array-unsigned-byte-32 positive-fixnum :word nil unsigned-reg signed-reg)

  (partial simple-array-signed-byte-8 tagged-num :byte t signed-reg)
  (partial simple-array-signed-byte-16 tagged-num :short t signed-reg)
  (partial simple-array-signed-byte-32 tagged-num :word t signed-reg)

  (partial simple-array-single-float single-float :single-float nil single-reg)
  (full simple-array-double-float double-float double-reg))

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
                (index :scs (unsigned-reg) :to :save))
         (:arg-types ,type positive-fixnum)
         (:results (result :scs (unsigned-reg)))
         (:result-types positive-fixnum)
         (:temporary (:scs (non-descriptor-reg)) temp)
         (:generator 20
           ;; Compute the offset for the word we're interested in.
           (inst lsr temp index ,bit-shift)
           ;; Load the word in question.
           (inst add temp object (lsl temp word-shift))
           (inst ldr result (@ temp
                               (- (* vector-data-offset n-word-bytes)
                                  other-pointer-lowtag)))
           ;; Compute the position of the bitfield we need.
           ,(if (= bits 1)
                `(setf temp index)
                `(inst and temp index ,(1- elements-per-word)))
           ,@(when (eq *backend-byte-order* :big-endian)
               `((inst eor temp temp ,(1- elements-per-word))))
           ,@(unless (= bits 1)
               `((inst lsl temp temp ,(1- (integer-length bits)))))
           ;; Shift the field we need to the low bits of RESULT.
           (inst lsr result result temp)
           ;; Mask out the field we're interested in.
           (inst and result result ,(1- (ash 1 bits)))))
       (define-vop (,(symbolicate "DATA-VECTOR-REF/" type "-C"))
         (:note "inline array access")
         (:translate data-vector-ref)
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg)))
         (:info index)
         (:arg-types ,type (:constant index))
         (:results (result :scs (unsigned-reg)))
         (:result-types positive-fixnum)
         (:generator 15
           (multiple-value-bind (index bit) (floor index ,elements-per-word)
             (inst ldr result (@ object
                                 (load-store-offset
                                  (+ (* index n-word-bytes)
                                     (- (* vector-data-offset n-word-bytes)
                                        other-pointer-lowtag)))))
             (inst ubfm result result (* bit ,bits) (+ (* bit ,bits) (1- ,bits))))))
       (define-vop (,(symbolicate "DATA-VECTOR-SET/" type "-C"))
         (:note "inline array store")
         (:translate data-vector-set)
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg))
                (value :scs (unsigned-reg)
                       :load-if (not (and (sc-is value immediate)
                                          (memq (tn-value value)
                                                '(0 ,(1- (ash 1 bits))))))))
         (:info index)
         (:arg-types ,type (:constant index) positive-fixnum)
         (:temporary (:scs (non-descriptor-reg)) old)
         (:generator 20
            (multiple-value-bind (index bit) (floor index ,elements-per-word)
              (inst ldr old (@ object
                               (load-store-offset
                                (+ (* index n-word-bytes)
                                   (- (* vector-data-offset n-word-bytes)
                                      other-pointer-lowtag)))))
              (cond ((not (sc-is value immediate))
                     (inst bfm old value (mod (- n-word-bits (* bit ,bits)) n-word-bits) (1- ,bits)))
                    ((zerop (tn-value value))
                     (inst and old old (lognot (ash (1- (ash 1 ,bits)) (* bit ,bits)))))
                    (t
                     (inst orr old old (ash (1- (ash 1 ,bits)) (* bit ,bits)))))
              (inst str old (@ object
                               (load-store-offset
                                (+ (* index n-word-bytes)
                                   (- (* vector-data-offset n-word-bytes)
                                      other-pointer-lowtag))))))))
       (define-vop (,(symbolicate "DATA-VECTOR-SET/" type))
         (:note "inline array store")
         (:translate data-vector-set)
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg))
                (index :scs (unsigned-reg)
                       ,@(unless (= bits 1)
                           '(:target shift)))
                (value :scs (unsigned-reg immediate)))
         (:arg-types ,type positive-fixnum positive-fixnum)
         (:temporary (:scs (non-descriptor-reg)) lip)
         (:temporary (:scs (non-descriptor-reg)) temp old)
         ,@(unless (= bits 1)
             '((:temporary (:scs (non-descriptor-reg) :from (:argument 1)) shift)))
         (:generator 25
           (let ((shift ,(if (= bits 1)
                             'index
                             'shift)))
             ;; Compute the offset for the word we're interested in.
             (inst lsr temp index ,bit-shift)
             ;; Load the word in question.
             (inst add lip object (lsl temp word-shift))
             (inst ldr old (@ lip
                              (- (* vector-data-offset n-word-bytes)
                                 other-pointer-lowtag)))
             ;; Compute the position of the bitfield we need.
             ,@(unless (= bits 1)
                 `((inst and shift index ,(1- elements-per-word))))
             ,@(when (eq *backend-byte-order* :big-endian)
                 `((inst eor shift ,(1- elements-per-word))))
             ,@(unless (= bits 1)
                 `((inst lsl shift shift ,(1- (integer-length bits)))))
             ;; Clear the target bitfield.
             (unless (and (sc-is value immediate)
                          (= (tn-value value) ,(1- (ash 1 bits))))
               (inst mov temp ,(1- (ash 1 bits)))
               (inst lsl temp temp shift)
               (inst bic old old temp))
             (unless (and (sc-is value immediate)
                          (= (tn-value value) 0))
               ;; LOGIOR in the new value (shifted appropriately).
               (sc-case value
                 (immediate
                  (inst mov temp (logand (tn-value value) ,(1- (ash 1 bits))))
                  (inst lsl temp temp shift))
                 (unsigned-reg
                  (inst lsl temp value shift)))
               (inst orr old old temp))
             ;; Write the altered word back to the array.
             (inst str old (@ lip
                              (- (* vector-data-offset n-word-bytes)
                                 other-pointer-lowtag))))))))))
  (def-small-data-vector-frobs simple-bit-vector 1)
  (def-small-data-vector-frobs simple-array-unsigned-byte-2 2)
  (def-small-data-vector-frobs simple-array-unsigned-byte-4 4))

(define-vop (data-vector-ref/simple-bit-vector-eq)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)) (index :scs (unsigned-reg)))
  (:arg-types simple-bit-vector positive-fixnum)
  (:conditional :eq)
  (:temporary (:scs (non-descriptor-reg)) temp x)
  (:generator 20
    (inst lsr temp index 6)
    (inst add temp object (lsl temp word-shift))
    (inst ldr x (@ temp (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
    (inst lsr x x index)
    (inst tst x 1)))

(define-vop (data-vector-ref/simple-bit-vector-c-eq)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types simple-bit-vector (:constant index))
  (:conditional :eq)
  (:temporary (:scs (non-descriptor-reg)) x)
  (:generator 15
    (multiple-value-bind (index bit)
        (floor index 64)
      (inst ldr x (@ object (load-store-offset (+ (* index n-word-bytes)
                                                  (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))))
      (inst tst x (ash 1 bit)))))


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
    (inst lsl offset index (- word-shift n-fixnum-tag-bits))
    (inst add offset offset (- (* vector-data-offset n-word-bytes)
                              other-pointer-lowtag))
    (inst ldr value (@ object offset))))

(define-vop (data-vector-set/simple-array-complex-single-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
         (index :scs (any-reg))
         (value :scs (complex-single-reg)))
  (:arg-types simple-array-complex-single-float positive-fixnum
              complex-single-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:generator 5
    (inst lsl offset index (- word-shift n-fixnum-tag-bits))
    (inst add offset offset (- (* vector-data-offset n-word-bytes)
                               other-pointer-lowtag))
    (inst str value (@ object offset))))

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
    (inst lsl offset index (1+ (- word-shift n-fixnum-tag-bits)))
    (inst add offset offset (- (* vector-data-offset n-word-bytes)
                               other-pointer-lowtag))
    (inst ldr value (@ object offset))))

(define-vop (data-vector-set/simple-array-complex-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
         (index :scs (any-reg))
         (value :scs (complex-double-reg)))
  (:arg-types simple-array-complex-double-float positive-fixnum
              complex-double-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:generator 5
    (inst lsl offset index (1+ (- word-shift n-fixnum-tag-bits)))
    (inst add offset offset (- (* vector-data-offset n-word-bytes)
                               other-pointer-lowtag))
    (inst str value (@ object offset))))

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

(define-vop (%compare-and-swap-svref word-index-cas)
  (:note "inline array compare-and-swap")
  (:policy :fast-safe)
  (:variant vector-data-offset other-pointer-lowtag)
  (:translate %compare-and-swap-svref)
  (:arg-types simple-vector positive-fixnum * *))

(define-vop (%compare-and-swap-svref-v8.1 word-index-cas-v8.1)
  (:note "inline array compare-and-swap")
  (:policy :fast-safe)
  (:variant vector-data-offset other-pointer-lowtag)
  (:translate %compare-and-swap-svref)
  (:arg-types simple-vector positive-fixnum * *))

(define-vop (array-atomic-incf/word)
  (:translate %array-atomic-incf/word)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg) :target offset)
         (diff :scs (unsigned-reg)))
  (:arg-types * positive-fixnum unsigned-num)
  (:results (result :scs (unsigned-reg) :from :load))
  (:result-types unsigned-num)
  (:temporary (:sc unsigned-reg) offset)
  (:temporary (:sc non-descriptor-reg) sum)
  (:temporary (:sc non-descriptor-reg) lip)
  (:generator 4
    (inst lsl offset index (- word-shift n-fixnum-tag-bits))
    (inst add offset offset (- (* vector-data-offset n-word-bytes)
                               other-pointer-lowtag))
    (inst add lip object offset)

    (inst dsb)
    LOOP
    (inst ldxr result lip)
    (inst add sum result diff)
    (inst stlxr tmp-tn sum lip)
    (inst cbnz tmp-tn LOOP)
    (inst dmb)))

(define-vop (array-atomic-incf/word-v8.1)
  (:translate %array-atomic-incf/word)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg) :target offset)
         (diff :scs (unsigned-reg)))
  (:arg-types * positive-fixnum unsigned-num)
  (:results (result :scs (unsigned-reg) :from :load))
  (:result-types unsigned-num)
  (:temporary (:sc unsigned-reg) offset)
  (:temporary (:sc non-descriptor-reg) lip)
  (:guard (member :arm-v8.1 *backend-subfeatures*))
  (:generator 3
    (inst lsl offset index (- word-shift n-fixnum-tag-bits))
    (inst add offset offset (- (* vector-data-offset n-word-bytes)
                               other-pointer-lowtag))
    (inst add lip object offset)
    (inst ldaddal diff result lip)))

