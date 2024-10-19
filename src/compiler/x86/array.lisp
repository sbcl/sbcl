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
    (inst lea bytes
          (make-ea :dword :base rank
                   :disp (+ (* array-dimensions-offset n-word-bytes)
                            lowtag-mask)))
    (inst and bytes (lognot lowtag-mask))
    ;; rank 1 is stored as 0, 2 is stored as 1, ...
    (inst lea header (make-ea :dword :disp (fixnumize -1) :base rank))
    (inst and header (fixnumize array-rank-mask))
    (inst shl header array-rank-position)
    (inst or  header type)
    (inst shr header n-fixnum-tag-bits)
    (pseudo-atomic ()
     (allocation nil bytes other-pointer-lowtag node nil result)
     (storew header result 0 other-pointer-lowtag))))

;;;; additional accessors and setters for the array header
(define-full-reffer %array-dimension *
  array-dimensions-offset other-pointer-lowtag
  (any-reg) positive-fixnum %array-dimension)

(define-full-setter %set-array-dimension *
  array-dimensions-offset other-pointer-lowtag
  (any-reg) positive-fixnum %set-array-dimension)

(symbol-macrolet ((rank-disp
                    (- (/ array-rank-position n-byte-bits) other-pointer-lowtag)))
(define-vop ()
  (:translate %array-rank)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (inst movzx res (make-ea :byte :disp rank-disp :base x))
    ;; not all registers have an addressable low byte, so the simple trick
    ;; used on x86-64 of adding 1 to the 8-bit register won't work.
    (inst inc res)
    (inst and res array-rank-mask)))

(define-vop ()
  (:translate %array-rank=)
  (:policy :fast-safe)
  (:args (array :scs (descriptor-reg)))
  (:info rank)
  (:arg-types * (:constant t))
  (:conditional :e)
  (:generator 2
    (inst cmp (make-ea :byte :disp rank-disp :base array) (encode-array-rank rank)))))


(defun power-of-two-limit-p (x)
  (and (fixnump x)
       (= (logcount (1+ x)) 1)))

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
                    (inst cmp bound index)
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

;;; Ancestors
(define-vop (dvref)
  (:translate data-vector-ref-with-offset)
  (:policy :fast-safe))
(define-vop (dvset)
  (:translate data-vector-set-with-offset)
  (:policy :fast-safe))

;;; variants which affect an entire lispword-sized value.
(defmacro define-full-setter+addend (name type offset lowtag scs el-type)
  `(progn
     (define-vop (,name dvset)
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
              (index :scs (any-reg immediate))
              (value :scs ,scs))
       (:info addend)
       (:arg-types ,type tagged-num
                   (:constant (constant-displacement ,lowtag n-word-bytes ,offset)) ,el-type)
       (:generator 4
         (sc-case index
           (immediate
            (inst mov (make-ea :dword :base object
                               :disp (- (* (+ ,offset (tn-value index) addend)
                                           n-word-bytes)
                                        ,lowtag))
                  value))
           (t
            (inst mov (make-ea :dword :base object :index index
                               :disp (- (* (+ ,offset addend)
                                           n-word-bytes) ,lowtag))
                  value)))))))
(macrolet ((def-full-data-vector-frobs (type element-type &rest scs)
             `(progn
                (define-full-reffer+addend ,(symbolicate "DATA-VECTOR-REF-WITH-OFFSET/" type)
                  ,type vector-data-offset other-pointer-lowtag ,scs
                  ,element-type data-vector-ref-with-offset)
                (define-full-setter+addend ,(symbolicate "DATA-VECTOR-SET-WITH-OFFSET/" type)
                  ,type vector-data-offset other-pointer-lowtag ,scs
                  ,element-type))))
  (def-full-data-vector-frobs simple-vector * descriptor-reg any-reg)
  (def-full-data-vector-frobs simple-array-unsigned-byte-32 unsigned-num
    unsigned-reg)
  (def-full-data-vector-frobs simple-array-fixnum tagged-num any-reg)
  (def-full-data-vector-frobs simple-array-unsigned-fixnum positive-fixnum any-reg)
  (def-full-data-vector-frobs simple-array-signed-byte-32 signed-num
    signed-reg)
  (def-full-data-vector-frobs simple-array-unsigned-byte-31 unsigned-num
    unsigned-reg)
  #+sb-unicode
  (def-full-data-vector-frobs simple-character-string character character-reg))

(define-full-compare-and-swap %compare-and-swap-svref simple-vector
  vector-data-offset other-pointer-lowtag
  (descriptor-reg any-reg) *
  %compare-and-swap-svref)

;;;; integer vectors whose elements are smaller than a byte, i.e.,
;;;; bit, 2-bit, and 4-bit vectors

(define-vop (data-vector-ref-with-offset/simple-bit-vector-c dvref)
  (:args (object :scs (descriptor-reg)))
  (:arg-types simple-bit-vector
              (:constant (integer 0 #x7fffffff)) (:constant (integer 0 0)))
  (:info index addend)
  (:ignore addend)
  (:results (result :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 3
    (multiple-value-bind (dword-index bit) (floor index 32)
      (inst mov result
                (make-ea :dword :base object
                         :disp (+ (* dword-index 4)
                                  (- (* vector-data-offset n-word-bytes)
                                     other-pointer-lowtag))))
      (let ((right-shift (- bit n-fixnum-tag-bits)))
        (cond ((plusp right-shift)
               (inst shr result right-shift))
              ((minusp right-shift) ; = left shift
               (inst shl result (- right-shift))))))
    (inst and result (fixnumize 1))))

(define-vop (data-vector-ref-with-offset/simple-bit-vector dvref)
  (:args (object :scs (descriptor-reg))
         (index :scs (signed-reg unsigned-reg)))
  (:info addend)
  (:ignore addend)
  (:arg-types simple-bit-vector tagged-num (:constant (integer 0 0)))
  (:results (result :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 4
    (inst bt (make-ea :dword :base object
                      :disp (- (* vector-data-offset n-word-bytes)
                               other-pointer-lowtag))
             index)
    (inst sbb result result)
    (inst and result (fixnumize 1))))

(macrolet ((def-small-data-vector-frobs (type bits)
             (let* ((elements-per-word (floor n-word-bits bits))
                    (bit-shift (1- (integer-length elements-per-word))))
    `(progn
      ,@(unless (= bits 1)
       `((define-vop (,(symbolicate 'data-vector-ref-with-offset/ type) dvref)
         (:args (object :scs (descriptor-reg))
                (index :scs (signed-reg unsigned-reg)))
         (:info addend)
         (:ignore addend)
         (:arg-types ,type tagged-num (:constant (integer 0 0)))
         (:results (result :scs (unsigned-reg) :from (:argument 0)))
         (:result-types positive-fixnum)
         (:temporary (:sc unsigned-reg :offset ecx-offset) ecx)
         (:generator 20
           (move ecx index)
           (inst shr ecx ,bit-shift)
           (inst mov result (make-ea-for-vector-data object :index ecx))
           (move ecx index)
           ;; We used to mask ECX for all values of ELEMENT-PER-WORD,
           ;; but since Intel's documentation says that the chip will
           ;; mask shift and rotate counts by 31 automatically, we can
           ;; safely move the masking operation under the protection of
           ;; this UNLESS in the bit-vector case.  --njf, 2006-07-14
           ,@(unless (= elements-per-word n-word-bits)
               `((inst and ecx ,(1- elements-per-word))
                 (inst shl ecx ,(1- (integer-length bits)))))
           (inst shr result :cl)
           (inst and result ,(1- (ash 1 bits)))))
       (define-vop (,(symbolicate 'data-vector-ref-with-offset/ type "-C") dvref)
         (:args (object :scs (descriptor-reg)))
         (:arg-types ,type (:constant index) (:constant (integer 0 0)))
         (:info index addend)
         (:ignore addend)
         (:results (result :scs (unsigned-reg)))
         (:result-types positive-fixnum)
         (:generator 15
           (multiple-value-bind (word extra) (floor index ,elements-per-word)
             (loadw result object (+ word vector-data-offset)
                    other-pointer-lowtag)
             (unless (zerop extra)
               (inst shr result (* extra ,bits)))
             (unless (= extra ,(1- elements-per-word))
               (inst and result ,(1- (ash 1 bits)))))))))
       (define-vop (,(symbolicate 'data-vector-set-with-offset/ type) dvset)
         (:args (object :scs (descriptor-reg) :to (:argument 2))
                (index :scs (signed-reg unsigned-reg) :target ecx)
                (value :scs (unsigned-reg immediate)))
         (:info addend)
         (:ignore addend)
         (:arg-types ,type tagged-num (:constant (integer 0 0))
                     positive-fixnum)
         (:temporary (:sc unsigned-reg) word-index)
         (:temporary (:sc unsigned-reg) old)
         (:temporary (:sc unsigned-reg :offset ecx-offset :from (:argument 1)) ecx)
         (:generator 25
           (move word-index index)
           (inst shr word-index ,bit-shift)
           (inst mov old (make-ea-for-vector-data object :index word-index))
           (move ecx index)
           ;; We used to mask ECX for all values of ELEMENT-PER-WORD,
           ;; but since Intel's documentation says that the chip will
           ;; mask shift and rotate counts by 31 automatically, we can
           ;; safely move the masking operation under the protection of
           ;; this UNLESS in the bit-vector case.  --njf, 2006-07-14
           ,@(unless (= elements-per-word n-word-bits)
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
           (inst mov (make-ea-for-vector-data object :index word-index)
                 old)))
       (define-vop (,(symbolicate 'data-vector-set-with-offset/ type "-C") dvset)
         (:args (object :scs (descriptor-reg))
                (value :scs (unsigned-reg immediate)))
         (:arg-types ,type (:constant index) (:constant (integer 0 0))
                     positive-fixnum)
         (:info index addend)
         (:ignore addend)
         (:temporary (:sc unsigned-reg :to (:result 0)) old)
         (:generator 20
           (multiple-value-bind (word extra) (floor index ,elements-per-word)
             (loadw old object (+ word vector-data-offset) other-pointer-lowtag)
             (sc-case value
               (immediate
                (let* ((value (tn-value value))
                       (mask ,(1- (ash 1 bits)))
                       (shift (* extra ,bits)))
                  (unless (= value mask)
                    (inst and old (ldb (byte n-word-bits 0)
                                       (lognot (ash mask shift)))))
                  (unless (zerop value)
                    (inst or old (ash value shift)))))
               (unsigned-reg
                (let ((shift (* extra ,bits)))
                  (unless (zerop shift)
                    (inst ror old shift))
                  (inst and old (lognot ,(1- (ash 1 bits))))
                  (inst or old value)
                  (unless (zerop shift)
                    (inst rol old shift)))))
             (storew old object (+ word vector-data-offset) other-pointer-lowtag))))))))
  (def-small-data-vector-frobs simple-bit-vector 1)
  (def-small-data-vector-frobs simple-array-unsigned-byte-2 2)
  (def-small-data-vector-frobs simple-array-unsigned-byte-4 4))

;;; And the float variants.

(defun float-ref-ea (object index offset element-size
                              &key (scale 1) (complex-offset 0))
  (sc-case index
    (immediate
     (make-ea :dword :base object
              :disp (- (+ (* vector-data-offset n-word-bytes)
                          (* element-size (+ offset (tn-value index)))
                          complex-offset)
                       other-pointer-lowtag)))
    (t
     (make-ea :dword :base object :index index :scale scale
              :disp (- (+ (* vector-data-offset n-word-bytes)
                          (* element-size offset)
                          complex-offset)
                       other-pointer-lowtag)))))

(define-vop (data-vector-ref-with-offset/simple-array-single-float dvref)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg immediate)))
  (:info addend)
  (:arg-types simple-array-single-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag
                                                4 vector-data-offset)))
  (:results (value :scs (single-reg)))
  (:result-types single-float)
  (:generator 5
   (with-empty-tn@fp-top(value)
     (inst fld (float-ref-ea object index addend 4)))))

(define-vop (data-vector-set-with-offset/simple-array-single-float dvset)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg immediate))
         (value :scs (single-reg)))
  (:info addend)
  (:arg-types simple-array-single-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag
                                                4 vector-data-offset))
              single-float)
  (:generator 5
    (cond ((zerop (tn-offset value))
           ;; Value is in ST0.
           (inst fst (float-ref-ea object index addend 4)))
          (t
           ;; Value is not in ST0.
           (inst fxch value)
           (inst fst (float-ref-ea object index addend 4))
           (inst fxch value)))))

(define-vop (data-vector-ref-with-offset/simple-array-double-float dvref)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg immediate)))
  (:info addend)
  (:arg-types simple-array-double-float
              tagged-num
              (:constant (constant-displacement other-pointer-lowtag
                                                8 vector-data-offset)))
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:generator 7
   (with-empty-tn@fp-top(value)
     (inst fldd (float-ref-ea object index addend 8 :scale 2)))))

(define-vop (data-vector-set-with-offset/simple-array-double-float dvset)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg immediate))
         (value :scs (double-reg)))
  (:info addend)
  (:arg-types simple-array-double-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag
                                                8 vector-data-offset))
              double-float)
  (:generator 20
    (cond ((zerop (tn-offset value))
           ;; Value is in ST0.
           (inst fstd (float-ref-ea object index addend 8 :scale 2)))
          (t
           ;; Value is not in ST0.
           (inst fxch value)
           (inst fstd (float-ref-ea object index addend 8 :scale 2))
           (inst fxch value)))))

;;; complex float variants

(define-vop (data-vector-ref-with-offset/simple-array-complex-single-float dvref)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg immediate)))
  (:info addend)
  (:arg-types simple-array-complex-single-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag
                                                8 vector-data-offset)))
  (:results (value :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 5
    (let ((real-tn (complex-single-reg-real-tn value)))
      (with-empty-tn@fp-top (real-tn)
        (inst fld (float-ref-ea object index addend 8 :scale 2))))
    (let ((imag-tn (complex-single-reg-imag-tn value)))
      (with-empty-tn@fp-top (imag-tn)
        ;; FIXME
        (inst fld (float-ref-ea object index addend 8
                                         :scale 2 :complex-offset 4))))))

(define-vop (data-vector-set-with-offset/simple-array-complex-single-float dvset)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg immediate))
         (value :scs (complex-single-reg)))
  (:info addend)
  (:arg-types simple-array-complex-single-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag
                                                8 vector-data-offset))
              complex-single-float)
  (:generator 5
    (let ((value-real (complex-single-reg-real-tn value)))
      (cond ((zerop (tn-offset value-real))
             ;; Value is in ST0.
             (inst fst (float-ref-ea object index addend 8 :scale 2)))
            (t
             ;; Value is not in ST0.
             (inst fxch value-real)
             (inst fst (float-ref-ea object index addend 8 :scale 2))
             (inst fxch value-real))))
    (let ((value-imag (complex-single-reg-imag-tn value)))
      (inst fxch value-imag)
      (inst fst (float-ref-ea object index addend 8
                                       :scale 2 :complex-offset 4))
      (inst fxch value-imag))))

(define-vop (data-vector-ref-with-offset/simple-array-complex-double-float dvref)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg immediate)))
  (:info addend)
  (:arg-types simple-array-complex-double-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag
                                                16 vector-data-offset)))
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 7
    (let ((real-tn (complex-double-reg-real-tn value)))
      (with-empty-tn@fp-top (real-tn)
        (inst fldd (float-ref-ea object index addend 16 :scale 4)))
    (let ((imag-tn (complex-double-reg-imag-tn value)))
      (with-empty-tn@fp-top (imag-tn)
        (inst fldd (float-ref-ea object index addend 16
                                          :scale 4 :complex-offset 8)))))))

(define-vop (data-vector-set-with-offset/simple-array-complex-double-float dvset)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg immediate))
         (value :scs (complex-double-reg)))
  (:info addend)
  (:arg-types simple-array-complex-double-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag
                                                16 vector-data-offset))
              complex-double-float)
  (:generator 20
    (let ((value-real (complex-double-reg-real-tn value)))
      (cond ((zerop (tn-offset value-real))
             ;; Value is in ST0.
             (inst fstd (float-ref-ea object index addend 16
                                               :scale 4)))
            (t
             ;; Value is not in ST0.
             (inst fxch value-real)
             (inst fstd (float-ref-ea object index addend 16
                                               :scale 4))
             (inst fxch value-real))))
    (let ((value-imag (complex-double-reg-imag-tn value)))
      (inst fxch value-imag)
      (inst fstd (float-ref-ea object index addend 16
                                        :scale 4 :complex-offset 8))
      (inst fxch value-imag))))


;;; {un,}signed-byte-8, simple-base-string

(macrolet ((define-data-vector-frobs (ptype element-type ref-inst
                                            8-bit-tns-p &rest scs)
  `(progn
    (define-vop (,(symbolicate "DATA-VECTOR-REF-WITH-OFFSET/" ptype) dvref)
      (:args (object :scs (descriptor-reg))
             (index :scs (signed-reg immediate)))
      (:info addend)
      (:arg-types ,ptype tagged-num
                  (:constant (constant-displacement other-pointer-lowtag
                                                    1 vector-data-offset)))
      (:results (value :scs ,scs))
      (:result-types ,element-type)
      (:generator 5
        (sc-case index
          (immediate
           (inst ,ref-inst value (make-ea-for-vector-data
                                  object :size :byte
                                  :offset (+ (tn-value index) addend))))
          (t
           (inst ,ref-inst value
                 (make-ea-for-vector-data object :size :byte
                                          :index index :offset addend))))))
    (define-vop (,(symbolicate "DATA-VECTOR-SET-WITH-OFFSET/" ptype) dvset)
      (:args (object :scs (descriptor-reg) :to (:eval 0))
             (index :scs (signed-reg immediate) :to (:eval 0))
             (value :scs ,scs ,@(unless 8-bit-tns-p
                                  '(:target eax))))
      (:info addend)
      (:arg-types ,ptype tagged-num
                  (:constant (constant-displacement other-pointer-lowtag
                                                    1 vector-data-offset))
                  ,element-type)
      ,@(unless 8-bit-tns-p
         '((:temporary (:sc unsigned-reg :offset eax-offset
                        :from (:argument 2) :to (:result 0))
            eax)))
      (:generator 5
        ,@(unless 8-bit-tns-p
           '((move eax value)))
        (sc-case index
          (immediate
           (inst mov (make-ea-for-vector-data
                      object :size :byte :offset (+ (tn-value index) addend))
                 ,(if 8-bit-tns-p
                      'value
                      'al-tn)))
          (t
           (inst mov (make-ea-for-vector-data object :size :byte
                                              :index index :offset addend)
                 ,(if 8-bit-tns-p
                      'value
                      'al-tn)))))))))
  (define-data-vector-frobs simple-array-unsigned-byte-7 positive-fixnum
    movzx nil unsigned-reg signed-reg)
  (define-data-vector-frobs simple-array-unsigned-byte-8 positive-fixnum
    movzx nil unsigned-reg signed-reg)
  (define-data-vector-frobs simple-array-signed-byte-8 tagged-num
    movsx nil signed-reg)
  (define-data-vector-frobs simple-base-string character
                            #+sb-unicode movzx #-sb-unicode mov
                            #+sb-unicode nil #-sb-unicode t character-reg))

;;; {un,}signed-byte-16
(macrolet ((define-data-vector-frobs (ptype element-type ref-inst &rest scs)
    `(progn
      (define-vop (,(symbolicate "DATA-VECTOR-REF-WITH-OFFSET/" ptype) dvref)
        (:args (object :scs (descriptor-reg))
               (index :scs (signed-reg immediate)))
        (:info addend)
        (:arg-types ,ptype tagged-num
                    (:constant (constant-displacement other-pointer-lowtag
                                                      2 vector-data-offset)))
        (:results (value :scs ,scs))
        (:result-types ,element-type)
        (:generator 5
          (sc-case index
            (immediate
             (inst ,ref-inst value
                   (make-ea-for-vector-data object :size :word
                                            :offset (+ (tn-value index) addend))))
            (t
             (inst ,ref-inst value
                   (make-ea-for-vector-data object :size :word
                                            :index index :offset addend))))))
      (define-vop (,(symbolicate "DATA-VECTOR-SET-WITH-OFFSET/" ptype) dvset)
        (:args (object :scs (descriptor-reg) :to (:eval 0))
               (index :scs (signed-reg immediate) :to (:eval 0))
               (value :scs ,scs))
        (:info addend)
        (:arg-types ,ptype tagged-num
                    (:constant (constant-displacement other-pointer-lowtag
                                                      2 vector-data-offset))
                    ,element-type)
        (:temporary (:sc unsigned-reg :offset eax-offset
                         :from (:argument 2) :to (:result 0))
                    eax)
        (:generator 5
          (move eax value)
          (sc-case index
            (immediate
             (inst mov (make-ea-for-vector-data
                        object :size :word :offset (+ (tn-value index) addend))
                   ax-tn))
            (t
             (inst mov (make-ea-for-vector-data object :size :word
                                                :index index :offset addend)
                   ax-tn))))))))
  (define-data-vector-frobs simple-array-unsigned-byte-15 positive-fixnum
    movzx unsigned-reg signed-reg)
  (define-data-vector-frobs simple-array-unsigned-byte-16 positive-fixnum
    movzx unsigned-reg signed-reg)
  (define-data-vector-frobs simple-array-signed-byte-16 tagged-num
    movsx signed-reg))


;;; These vops are useful for accessing the bits of a vector
;;; irrespective of what type of vector it is.
(define-full-reffer vector-raw-bits * vector-data-offset other-pointer-lowtag
 (unsigned-reg) unsigned-num %vector-raw-bits)
(define-full-setter set-vector-raw-bits * vector-data-offset other-pointer-lowtag
 (unsigned-reg) unsigned-num %set-vector-raw-bits)

;;; Weak vectors
(define-full-reffer %weakvec-ref * vector-data-offset other-pointer-lowtag
  (any-reg descriptor-reg) * %weakvec-ref)
(define-full-setter %weakvec-set * vector-data-offset other-pointer-lowtag
  (any-reg descriptor-reg) * %weakvec-set)

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
    (inst xadd (make-ea :dword :base array :index index
                        :disp (- (* vector-data-offset n-word-bytes)
                                 other-pointer-lowtag))
          diff :lock)
    (move result diff)))
