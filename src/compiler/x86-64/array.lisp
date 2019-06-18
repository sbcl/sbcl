;;;; array operations for the x86 VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")


;; For use in constant indexing; we can't use INDEX since the displacement
;; field of an EA can't contain 64 bit values.
(sb-xc:deftype low-index () '(signed-byte 29))

;;;; allocator for the array header

(define-vop (make-array-header)
  (:translate make-array-header)
  (:policy :fast-safe)
  (:args (type :scs (any-reg))
         (rank :scs (any-reg)))
  (:arg-types positive-fixnum positive-fixnum)
  (:temporary (:sc any-reg :to :eval) bytes)
  (:temporary (:sc any-reg :to :result) header)
  (:results (result :scs (descriptor-reg) :from :eval))
  (:node-var node)
  (:generator 13
    (inst lea :dword bytes
          (ea (+ (* array-dimensions-offset n-word-bytes) lowtag-mask)
              nil rank (ash 1 (- word-shift n-fixnum-tag-bits))))
    (inst and :dword bytes (lognot lowtag-mask))
    (inst lea :dword header (ea (fixnumize (1- array-dimensions-offset)) rank))
    (inst shl :dword header n-widetag-bits)
    (inst or  :dword header type)
    (inst shr :dword header n-fixnum-tag-bits)
    (instrument-alloc bytes node)
    (pseudo-atomic ()
     (allocation result bytes node nil 0)
     (storew header result 0 0)
     (inst or :byte result other-pointer-lowtag))))

(define-vop (make-array-header/c)
  (:translate make-array-header)
  (:policy :fast-safe)
  (:arg-types (:constant t) (:constant t))
  (:info type rank)
  (:results (result :scs (descriptor-reg) :from :eval))
  (:node-var node)
  (:generator 12
    (let* ((header-size (+ rank
                           (1- array-dimensions-offset)))
           (bytes (* (align-up (1+ header-size) 2) n-word-bytes))
           (header (logior (ash header-size
                                n-widetag-bits)
                           type)))
     (instrument-alloc bytes node)
     (pseudo-atomic ()
      (allocation result bytes node nil 0)
      (storew* header result 0 0 t)
      (inst or :byte result other-pointer-lowtag)))))


;;;; additional accessors and setters for the array header
(define-full-reffer %array-dimension *
  array-dimensions-offset other-pointer-lowtag
  (any-reg) positive-fixnum %array-dimension)

(define-full-setter %set-array-dimension *
  array-dimensions-offset other-pointer-lowtag
  (any-reg) positive-fixnum %set-array-dimension)

(define-vop (array-rank-vop)
  (:translate %array-rank)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 3
    ;; An unaligned dword read not spanning a 16-byte boundary is as fast as
    ;; and shorter by 5 bytes than a qword read and right-shift by 8.
    (inst mov :dword res (ea (1+ (- other-pointer-lowtag)) x))
    (inst sub :dword res (1- array-dimensions-offset))))

(define-vop (array-rank-vop=>fixnum)
  (:translate %array-rank)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 2
    (inst mov :dword res (ea (1+ (- other-pointer-lowtag)) x))
    (inst lea :dword res
          (let ((scale (ash 1 n-fixnum-tag-bits)))
            ;; Compute [res*N-disp]. for N=2 use [res+res-disp]
            (ea (- (* scale (1- array-dimensions-offset)))
                (if (= scale 2) res nil) res (if (= scale 2) 1 scale))))))

(define-vop (array-rank=)
  (:translate %array-rank=)
  (:policy :fast-safe)
  (:args (array :scs (descriptor-reg)))
  (:info rank)
  (:arg-types * (:constant t))
  (:conditional :e)
  (:generator 2
    (inst cmp :dword
          (ea (1+ (- other-pointer-lowtag)) array)
          (+ rank
             (1- array-dimensions-offset)))))

;;;; bounds checking routine
(define-vop (check-bound)
  (:translate %check-bound)
  (:policy :fast-safe)
  (:args (array :scs (descriptor-reg constant))
         (bound :scs (any-reg descriptor-reg)
                :load-if (not (and (sc-is bound immediate)
                                   (typep (tn-value bound)
                                          'sc-offset)
                                   (not (sc-is index immediate)))))
         (index :scs (any-reg descriptor-reg)
                :load-if (not (and (sc-is index immediate)
                                   (typep (tn-value index)
                                          'sc-offset)))))
  (:variant-vars %test-fixnum)
  (:variant t)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 6
    (let ((error (generate-error-code vop 'invalid-array-index-error
                                      array bound index))
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
      (cond ((typep bound '(integer * -1))
             ;; Power of two bound, can be checked for fixnumness at
             ;; the same time as it always occupies a consecutive bit
             ;; range, everything else, including the tag, has to be
             ;; zero.
             (inst test index (if (eql bound -1)
                                  index ;; zero?
                                  bound))
             (inst jmp :ne error))
            (t
             (when (and %test-fixnum (not (integerp index)))
               (%test-fixnum index nil error t))
             (cond ((integerp bound)
                    (inst cmp index bound)
                    (inst jmp :nb error))
                   (t
                    (if (eql index 0)
                        (inst test bound bound)
                        (inst cmp bound index))
                    (inst jmp :be error))))))))
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

;;;; accessors/setters

;;; variants built on top of WORD-INDEX-REF, etc. I.e., those vectors
;;; whose elements are represented in integer registers and are built
;;; out of 8, 16, or 32 bit elements.
(macrolet ((def-full-data-vector-frobs (type element-type &rest scs)
             `(progn
                (define-full-reffer+offset
                  ,(symbolicate "DATA-VECTOR-REF-WITH-OFFSET/" type)
                  ,type vector-data-offset other-pointer-lowtag ,scs
                  ,element-type data-vector-ref-with-offset)
                (define-full-setter+offset
                  ,(symbolicate "DATA-VECTOR-SET-WITH-OFFSET/" type)
                  ,type vector-data-offset other-pointer-lowtag ,scs
                  ,element-type data-vector-set-with-offset))))
  (def-full-data-vector-frobs simple-vector * descriptor-reg any-reg immediate)
  (def-full-data-vector-frobs simple-array-unsigned-byte-64 unsigned-num
    unsigned-reg)
  (def-full-data-vector-frobs simple-array-fixnum tagged-num any-reg)
  (def-full-data-vector-frobs simple-array-unsigned-fixnum
      positive-fixnum any-reg)
  (def-full-data-vector-frobs simple-array-signed-byte-64
      signed-num signed-reg)
  (def-full-data-vector-frobs simple-array-unsigned-byte-63 unsigned-num
    unsigned-reg))

(define-full-compare-and-swap %compare-and-swap-svref simple-vector
  vector-data-offset other-pointer-lowtag
  (descriptor-reg any-reg) *
  %compare-and-swap-svref)

;;;; integer vectors whose elements are smaller than a byte, i.e.,
;;;; bit, 2-bit, and 4-bit vectors

(define-vop (data-vector-ref-with-offset/simple-bit-vector-c)
  (:translate data-vector-ref-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:arg-types simple-bit-vector
              ;; this constant is possibly off by something
              ;; but (sbit n <huge-constant>) is unlikely to appear in code
              (:constant (integer 0 #x3ffffffff)) (:constant (integer 0 0)))
  (:info index offset)
  (:ignore offset)
  (:results (result :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 3
    ;; using 32-bit operand size might elide the REX prefix on mov + shift
    (multiple-value-bind (dword-index bit) (floor index 32)
      (inst mov :dword result
                (ea (+ (* dword-index 4)
                       (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
                    object))
      (let ((right-shift (- bit n-fixnum-tag-bits)))
        (cond ((plusp right-shift)
               (inst shr :dword result right-shift))
              ((minusp right-shift) ; = left shift
               (inst shl :dword result (- right-shift))))))
    (inst and :dword result (fixnumize 1))))

(define-vop (data-vector-ref-with-offset/simple-bit-vector)
  (:translate data-vector-ref-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (unsigned-reg)))
  (:info offset)
  (:ignore offset)
  (:arg-types simple-bit-vector positive-fixnum (:constant (integer 0 0)))
  (:results (result :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 4
    (inst bt (ea (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)
                 object) index)
    (inst sbb :dword result result)
    (inst and :dword result (fixnumize 1))))

(macrolet ((def-small-data-vector-frobs (type bits)
             (let* ((elements-per-word (floor n-word-bits bits))
                    (bit-shift (1- (integer-length elements-per-word))))
    `(progn
      ,@(unless (= bits 1)
       `((define-vop (,(symbolicate 'data-vector-ref-with-offset/ type))
         (:note "inline array access")
         (:translate data-vector-ref-with-offset)
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg))
                (index :scs (unsigned-reg)))
         (:info offset)
         (:arg-types ,type positive-fixnum (:constant (integer 0 0)))
         (:results (result :scs (unsigned-reg) :from (:argument 0)))
         (:result-types positive-fixnum)
         (:temporary (:sc unsigned-reg :offset ecx-offset) ecx)
         (:generator 20
           (aver (zerop offset))
           (move ecx index)
           (inst shr ecx ,bit-shift)
           (inst mov result
                 (ea (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)
                     object ecx n-word-bytes))
           (move ecx index)
           ;; We used to mask ECX for all values of BITS, but since
           ;; Intel's documentation says that the chip will mask shift
           ;; and rotate counts by 63 automatically, we can safely move
           ;; the masking operation under the protection of this UNLESS
           ;; in the bit-vector case.  --njf, 2006-07-14
           ,@(unless (= bits 1)
               `((inst and ecx ,(1- elements-per-word))
                 (inst shl ecx ,(1- (integer-length bits)))))
           (inst shr result :cl)
           (inst and result ,(1- (ash 1 bits)))))
       (define-vop (,(symbolicate 'data-vector-ref-with-offset/ type "-C"))
         (:translate data-vector-ref-with-offset)
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg)))
         (:arg-types ,type (:constant low-index) (:constant (integer 0 0)))
         (:info index offset)
         (:results (result :scs (unsigned-reg)))
         (:result-types positive-fixnum)
         (:generator 15
           (aver (zerop offset))
           (multiple-value-bind (word extra) (floor index ,elements-per-word)
             (loadw result object (+ word vector-data-offset)
                    other-pointer-lowtag)
             (unless (zerop extra)
               (inst shr result (* extra ,bits)))
             (unless (= extra ,(1- elements-per-word))
               (inst and result ,(1- (ash 1 bits)))))))))
       (define-vop (,(symbolicate 'data-vector-set-with-offset/ type))
         (:note "inline array store")
         (:translate data-vector-set-with-offset)
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg))
                (index :scs (unsigned-reg) :target ecx)
                (value :scs (unsigned-reg immediate) :target result))
         (:info offset)
         (:arg-types ,type positive-fixnum (:constant (integer 0 0))
                     positive-fixnum)
         (:results (result :scs (unsigned-reg)))
         (:result-types positive-fixnum)
         (:temporary (:sc unsigned-reg) word-index)
         (:temporary (:sc unsigned-reg) old)
         (:temporary (:sc unsigned-reg :offset ecx-offset) ecx)
         (:generator 25
           (aver (zerop offset))
           (move word-index index)
           (inst shr word-index ,bit-shift)
           (inst mov old
                 (ea (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)
                     object word-index n-word-bytes))
           (move ecx index)
           ;; We used to mask ECX for all values of BITS, but since
           ;; Intel's documentation says that the chip will mask shift
           ;; and rotate counts by 63 automatically, we can safely move
           ;; the masking operation under the protection of this UNLESS
           ;; in the bit-vector case.  --njf, 2006-07-14
           ,@(unless (= bits 1)
               `((inst and ecx ,(1- elements-per-word))
                 (inst shl ecx ,(1- (integer-length bits)))))
           (inst ror old :cl)
           (unless (and (sc-is value immediate)
                        (= (tn-value value) ,(1- (ash 1 bits))))
             (inst and old ,(lognot (1- (ash 1 bits)))))
           (sc-case value
             (immediate
              (unless (zerop (tn-value value))
                (inst or old (logand (tn-value value) ,(1- (ash 1 bits))))))
             (unsigned-reg
              (inst or old value)))
           (inst rol old :cl)
           (inst mov (ea (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)
                         object word-index n-word-bytes)
                 old)
           (sc-case value
             (immediate
              (inst mov result (tn-value value)))
             (unsigned-reg
              (move result value)))))
       (define-vop (,(symbolicate 'data-vector-set-with-offset/ type "-C"))
         (:translate data-vector-set-with-offset)
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg))
                (value :scs (unsigned-reg immediate) :target result))
         (:arg-types ,type (:constant low-index)
                     (:constant (integer 0 0)) positive-fixnum)
         (:temporary (:sc unsigned-reg) mask-tn)
         (:info index offset)
         (:results (result :scs (unsigned-reg)))
         (:result-types positive-fixnum)
         (:temporary (:sc unsigned-reg :to (:result 0)) old)
         (:generator 20
           (aver (zerop offset))
           (multiple-value-bind (word extra) (floor index ,elements-per-word)
             (inst mov old
                   (ea (- (* (+ word vector-data-offset) n-word-bytes)
                          other-pointer-lowtag)
                       object))
             (sc-case value
               (immediate
                (let* ((value (tn-value value))
                       (mask ,(1- (ash 1 bits)))
                       (shift (* extra ,bits)))
                  (unless (= value mask)
                    (inst mov mask-tn (ldb (byte 64 0)
                                           (lognot (ash mask shift))))
                    (inst and old mask-tn))
                  (unless (zerop value)
                    (inst mov mask-tn (ash value shift))
                    (inst or old mask-tn))))
               (unsigned-reg
                (let ((shift (* extra ,bits)))
                  (unless (zerop shift)
                    (inst ror old shift))
                  (inst mov mask-tn (lognot ,(1- (ash 1 bits))))
                  (inst and old mask-tn)
                  (inst or old value)
                  (unless (zerop shift)
                    (inst rol old shift)))))
             (inst mov (ea (- (* (+ word vector-data-offset) n-word-bytes)
                              other-pointer-lowtag)
                           object)
                   old)
             (sc-case value
               (immediate
                (inst mov result (tn-value value)))
               (unsigned-reg
                (move result value))))))))))
  (def-small-data-vector-frobs simple-bit-vector 1)
  (def-small-data-vector-frobs simple-array-unsigned-byte-2 2)
  (def-small-data-vector-frobs simple-array-unsigned-byte-4 4))
;;; And the float variants.

(defun make-ea-for-float-ref (object index offset element-size
                              &key (scale 1) (complex-offset 0))
  (etypecase index
      (integer
       (ea (- (+ (* vector-data-offset n-word-bytes)
                 (* (+ index offset) element-size)
                 complex-offset)
              other-pointer-lowtag)
           object))
      (tn
       (ea (- (+ (* vector-data-offset n-word-bytes)
                 (* offset element-size)
                 complex-offset)
              other-pointer-lowtag)
           object index scale))))

#.
(let ((use-temp (<= word-shift n-fixnum-tag-bits)))
  `(define-vop (data-vector-ref-with-offset/simple-array-single-float)
     (:note "inline array access")
     (:translate data-vector-ref-with-offset)
     (:policy :fast-safe)
     (:args (object :scs (descriptor-reg))
            (index :scs (any-reg)))
     (:info offset)
     (:arg-types simple-array-single-float tagged-num
                 (:constant (constant-displacement other-pointer-lowtag
                                                   4 vector-data-offset)))
     ,@(when use-temp '((:temporary (:sc unsigned-reg) dword-index)))
     (:results (value :scs (single-reg)))
     (:result-types single-float)
     (:generator 5
      ,@(if use-temp
            '((move dword-index index)
              (inst shr dword-index (1+ (- n-fixnum-tag-bits word-shift)))
              (inst movss value (make-ea-for-float-ref object dword-index offset 4)))
            '((inst movss value (make-ea-for-float-ref object index offset 4
                                 :scale (ash 4 (- n-fixnum-tag-bits)))))))))

(define-vop (data-vector-ref-with-offset/simple-array-single-float-c)
  (:note "inline array access")
  (:translate data-vector-ref-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index offset)
  (:arg-types simple-array-single-float (:constant low-index)
              (:constant (constant-displacement other-pointer-lowtag
                                                4 vector-data-offset)))
  (:results (value :scs (single-reg)))
  (:result-types single-float)
  (:generator 4
   (inst movss value (make-ea-for-float-ref object index offset 4))))

#.
(let ((use-temp (<= word-shift n-fixnum-tag-bits)))
  `(define-vop (data-vector-set-with-offset/simple-array-single-float)
     (:note "inline array store")
     (:translate data-vector-set-with-offset)
     (:policy :fast-safe)
     (:args (object :scs (descriptor-reg))
            (index :scs (any-reg))
            (value :scs (single-reg) :target result))
     (:info offset)
     (:arg-types simple-array-single-float tagged-num
                 (:constant (constant-displacement other-pointer-lowtag
                                                   4 vector-data-offset))
                  single-float)
     ,@(when use-temp '((:temporary (:sc unsigned-reg) dword-index)))
     (:results (result :scs (single-reg)))
     (:result-types single-float)
     (:generator 5
      ,@(if use-temp
            '((move dword-index index)
              (inst shr dword-index (1+ (- n-fixnum-tag-bits word-shift)))
              (inst movss (make-ea-for-float-ref object dword-index offset 4) value))
            '((inst movss (make-ea-for-float-ref object index offset 4
                           :scale (ash 4 (- n-fixnum-tag-bits))) value)))
      (move result value))))

(define-vop (data-vector-set-with-offset/simple-array-single-float-c)
  (:note "inline array store")
  (:translate data-vector-set-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (value :scs (single-reg) :target result))
  (:info index offset)
  (:arg-types simple-array-single-float (:constant low-index)
              (:constant (constant-displacement other-pointer-lowtag
                                                4 vector-data-offset))
              single-float)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:generator 4
   (inst movss (make-ea-for-float-ref object index offset 4) value)
   (move result value)))

(define-vop (data-vector-ref-with-offset/simple-array-double-float)
  (:note "inline array access")
  (:translate data-vector-ref-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:info offset)
  (:arg-types simple-array-double-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag
                                                8 vector-data-offset)))
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:generator 7
   (inst movsd value (make-ea-for-float-ref object index offset 8
                                            :scale (ash 1 (- word-shift n-fixnum-tag-bits))))))

(define-vop (data-vector-ref-c/simple-array-double-float)
  (:note "inline array access")
  (:translate data-vector-ref-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index offset)
  (:arg-types simple-array-double-float (:constant low-index)
              (:constant (constant-displacement other-pointer-lowtag
                                                8 vector-data-offset)))
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:generator 6
   (inst movsd value (make-ea-for-float-ref object index offset 8))))

(define-vop (data-vector-set-with-offset/simple-array-double-float)
  (:note "inline array store")
  (:translate data-vector-set-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (double-reg) :target result))
  (:info offset)
  (:arg-types simple-array-double-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag
                                                8 vector-data-offset))
              double-float)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:generator 20
   (inst movsd (make-ea-for-float-ref object index offset 8
                                      :scale (ash 1 (- word-shift n-fixnum-tag-bits)))
         value)
   (move result value)))

(define-vop (data-vector-set-with-offset/simple-array-double-float-c)
  (:note "inline array store")
  (:translate data-vector-set-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (value :scs (double-reg) :target result))
  (:info index offset)
  (:arg-types simple-array-double-float (:constant low-index)
              (:constant (constant-displacement other-pointer-lowtag
                                                8 vector-data-offset))
              double-float)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:generator 19
   (inst movsd (make-ea-for-float-ref object index offset 8) value)
   (move result value)))


;;; complex float variants

(define-vop (data-vector-ref-with-offset/simple-array-complex-single-float)
  (:note "inline array access")
  (:translate data-vector-ref-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:info offset)
  (:arg-types simple-array-complex-single-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag
                                                8 vector-data-offset)))
  (:results (value :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 5
    (inst movq value (make-ea-for-float-ref object index offset 8
                                            :scale (ash 1 (- word-shift n-fixnum-tag-bits))))))

(define-vop (data-vector-ref-with-offset/simple-array-complex-single-float-c)
  (:note "inline array access")
  (:translate data-vector-ref-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index offset)
  (:arg-types simple-array-complex-single-float (:constant low-index)
              (:constant (constant-displacement other-pointer-lowtag
                                                8 vector-data-offset)))
  (:results (value :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 4
    (inst movq value (make-ea-for-float-ref object index offset 8))))

(define-vop (data-vector-set-with-offset/simple-array-complex-single-float)
  (:note "inline array store")
  (:translate data-vector-set-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (complex-single-reg) :target result))
  (:info offset)
  (:arg-types simple-array-complex-single-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag
                                                8 vector-data-offset))
              complex-single-float)
  (:results (result :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 5
    (move result value)
    (inst movq (make-ea-for-float-ref object index offset 8
                                      :scale (ash 1 (- word-shift n-fixnum-tag-bits)))
          value)))

(define-vop (data-vector-set-with-offset/simple-array-complex-single-float-c)
  (:note "inline array store")
  (:translate data-vector-set-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (value :scs (complex-single-reg) :target result))
  (:info index offset)
  (:arg-types simple-array-complex-single-float (:constant low-index)
              (:constant (constant-displacement other-pointer-lowtag
                                                8 vector-data-offset))
              complex-single-float)
  (:results (result :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 4
    (move result value)
    (inst movq (make-ea-for-float-ref object index offset 8) value)))

(define-vop (data-vector-ref-with-offset/simple-array-complex-double-float)
  (:note "inline array access")
  (:translate data-vector-ref-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:info offset)
  (:arg-types simple-array-complex-double-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag
                                                16 vector-data-offset)))
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 7
    (inst movapd value (make-ea-for-float-ref object index offset 16
                                              :scale (ash 2 (- word-shift n-fixnum-tag-bits))))))

(define-vop (data-vector-ref-with-offset/simple-array-complex-double-float-c)
  (:note "inline array access")
  (:translate data-vector-ref-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index offset)
  (:arg-types simple-array-complex-double-float (:constant low-index)
              (:constant (constant-displacement other-pointer-lowtag
                                                16 vector-data-offset)))
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 6
    (inst movapd value (make-ea-for-float-ref object index offset 16))))

(define-vop (data-vector-set-with-offset/simple-array-complex-double-float)
  (:note "inline array store")
  (:translate data-vector-set-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (complex-double-reg) :target result))
  (:info offset)
  (:arg-types simple-array-complex-double-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag
                                                16 vector-data-offset))
              complex-double-float)
  (:results (result :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 20
    (inst movapd (make-ea-for-float-ref object index offset 16
                                        :scale (ash 2 (- word-shift n-fixnum-tag-bits)))
          value)
    (move result value)))

(define-vop (data-vector-set-with-offset/simple-array-complex-double-float-c)
  (:note "inline array store")
  (:translate data-vector-set-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (value :scs (complex-double-reg) :target result))
  (:info index offset)
  (:arg-types simple-array-complex-double-float (:constant low-index)
              (:constant (constant-displacement other-pointer-lowtag
                                                16 vector-data-offset))
              complex-double-float)
  (:results (result :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 19
    (inst movapd (make-ea-for-float-ref object index offset 16) value)
    (move result value)))



;;; {un,}signed-byte-{8,16,32} and characters
(macrolet ((define-data-vector-frobs (ptype mov-inst operand-size
                                            type &rest scs)
  (binding* ((opcode-modifier (if (eq mov-inst 'mov)
                                  operand-size
                                  `(,operand-size :qword)))
             (n-bytes (the (member 1 2 4) (size-nbyte operand-size)))
             ((index-sc scale)
              (if (>= n-bytes (ash 1 n-fixnum-tag-bits))
                  (values 'any-reg (ash n-bytes (- n-fixnum-tag-bits)))
                  (values 'signed-reg n-bytes)))
             (ea-expr `(ea (+ (* vector-data-offset n-word-bytes)
                              (* offset ,n-bytes)
                              (- other-pointer-lowtag))
                           object index ,scale))
             (ea-expr-const `(ea (+ (* vector-data-offset n-word-bytes)
                                    (* ,n-bytes (+ index offset))
                                    (- other-pointer-lowtag))
                                 object)))
    `(progn
         (define-vop (,(symbolicate "DATA-VECTOR-REF-WITH-OFFSET/" ptype))
           (:translate data-vector-ref-with-offset)
           (:policy :fast-safe)
           (:args (object :scs (descriptor-reg))
                  (index :scs (,index-sc)))
           (:info offset)
           (:arg-types ,ptype tagged-num
                       (:constant (constant-displacement other-pointer-lowtag
                                                         ,n-bytes vector-data-offset)))
           (:results (value :scs ,scs))
           (:result-types ,type)
           (:generator 5 (inst ,mov-inst ',opcode-modifier value ,ea-expr)))
         (define-vop (,(symbolicate "DATA-VECTOR-REF-WITH-OFFSET/" ptype "-C"))
           (:translate data-vector-ref-with-offset)
           (:policy :fast-safe)
           (:args (object :scs (descriptor-reg)))
           (:info index offset)
           (:arg-types ,ptype (:constant low-index)
                       (:constant (constant-displacement other-pointer-lowtag
                                                         ,n-bytes vector-data-offset)))
           (:results (value :scs ,scs))
           (:result-types ,type)
           (:generator 4 (inst ,mov-inst ',opcode-modifier value ,ea-expr-const)))
         (define-vop (,(symbolicate "DATA-VECTOR-SET-WITH-OFFSET/" ptype))
           (:translate data-vector-set-with-offset)
           (:policy :fast-safe)
           (:args (object :scs (descriptor-reg) :to (:eval 0))
                  (index :scs (,index-sc) :to (:eval 0))
                  (value :scs ,scs :target result))
           (:info offset)
           (:arg-types ,ptype tagged-num
                       (:constant (constant-displacement other-pointer-lowtag
                                                         ,n-bytes vector-data-offset))
                       ,type)
           (:results (result :scs ,scs))
           (:result-types ,type)
           (:generator 5
             (inst mov ,operand-size ,ea-expr value)
             (move result value)))
         (define-vop (,(symbolicate "DATA-VECTOR-SET-WITH-OFFSET/" ptype "-C"))
           (:translate data-vector-set-with-offset)
           (:policy :fast-safe)
           (:args (object :scs (descriptor-reg) :to (:eval 0))
                  (value :scs ,scs :target result))
           (:info index offset)
           (:arg-types ,ptype (:constant low-index)
                       (:constant (constant-displacement other-pointer-lowtag
                                                         ,n-bytes vector-data-offset))
                       ,type)
           (:results (result :scs ,scs))
           (:result-types ,type)
           (:generator 4
             (inst mov ,operand-size ,ea-expr-const value)
             (move result value)))))))
  (define-data-vector-frobs simple-array-unsigned-byte-7 movzx :byte
    positive-fixnum unsigned-reg signed-reg)
  (define-data-vector-frobs simple-array-unsigned-byte-8 movzx :byte
    positive-fixnum unsigned-reg signed-reg)
  (define-data-vector-frobs simple-array-signed-byte-8 movsx :byte
    tagged-num signed-reg)
  (define-data-vector-frobs simple-base-string movzx :byte
     character character-reg)
  (define-data-vector-frobs simple-array-unsigned-byte-15 movzx :word
    positive-fixnum unsigned-reg signed-reg)
  (define-data-vector-frobs simple-array-unsigned-byte-16 movzx :word
    positive-fixnum unsigned-reg signed-reg)
  (define-data-vector-frobs simple-array-signed-byte-16 movsx :word
    tagged-num signed-reg)
  (define-data-vector-frobs simple-array-unsigned-byte-32 movzx :dword
    positive-fixnum unsigned-reg signed-reg)
  (define-data-vector-frobs simple-array-unsigned-byte-31 movzx :dword
    positive-fixnum unsigned-reg signed-reg)
  (define-data-vector-frobs simple-array-signed-byte-32 movsx :dword
    tagged-num signed-reg)
  #+sb-unicode
  (define-data-vector-frobs simple-character-string movzx :dword
    character character-reg))


;;; These vops are useful for accessing the bits of a vector
;;; irrespective of what type of vector it is.
(define-full-reffer vector-raw-bits * vector-data-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num %vector-raw-bits)
(define-full-setter set-vector-raw-bits * vector-data-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num %set-vector-raw-bits)

;;;; ATOMIC-INCF for arrays

(define-vop (array-atomic-incf/word)
  (:translate %array-atomic-incf/word)
  (:policy :fast-safe)
  (:args (array :scs (descriptor-reg))
         (index :scs (any-reg))
         (diff :scs (unsigned-reg) :target result))
  (:arg-types * positive-fixnum unsigned-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 4
    (inst xadd (ea (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)
                   array index (ash 1 (- word-shift n-fixnum-tag-bits)))
          diff :lock)
    (move result diff)))
