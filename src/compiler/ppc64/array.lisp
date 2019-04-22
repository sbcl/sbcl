;;;; array operations for the PPC VM

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
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:temporary (:scs (non-descriptor-reg)) gc-temp)
  #-gencgc (:ignore gc-temp)
  (:results (result :scs (descriptor-reg)))
  (:generator 0
    (pseudo-atomic (pa-flag)
      (inst addi ndescr rank (+ (* array-dimensions-offset n-word-bytes)
                                lowtag-mask))
      (inst clrrdi ndescr ndescr n-lowtag-bits) ; or CLRRWI
      (allocation header ndescr other-pointer-lowtag
                  :temp-tn gc-temp
                  :flag-tn pa-flag)
      (inst addi ndescr rank (fixnumize (1- array-dimensions-offset)))
      (inst sldi ndescr ndescr n-widetag-bits) ; or SLWI
      (inst or ndescr ndescr type)
      (inst srdi ndescr ndescr n-fixnum-tag-bits) ; or SRDI
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
    (inst sradi temp temp n-widetag-bits) ; or SRAWI
    (inst subi temp temp (1- array-dimensions-offset))
    (inst sldi res temp n-fixnum-tag-bits))) ; or SLWI

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
      (inst cmpld index bound)
      (inst bge error))))


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
  #+sb-unicode
  (def-data-vector-frobs simple-character-string word-index
    character character-reg)
  (def-data-vector-frobs simple-vector word-index
    * descriptor-reg any-reg)
  (def-data-vector-frobs simple-array-unsigned-byte-7 byte-index
    positive-fixnum unsigned-reg)
  (def-data-vector-frobs simple-array-unsigned-byte-8 byte-index
    positive-fixnum unsigned-reg)
  (def-data-vector-frobs simple-array-unsigned-byte-15 16-bits-index
    positive-fixnum unsigned-reg)
  (def-data-vector-frobs simple-array-unsigned-byte-16 16-bits-index
    positive-fixnum unsigned-reg)
  (def-data-vector-frobs simple-array-unsigned-byte-31 32-bits-index
    unsigned-num unsigned-reg)
  (def-data-vector-frobs simple-array-unsigned-byte-32 32-bits-index
    unsigned-num unsigned-reg)
  (def-data-vector-frobs simple-array-unsigned-byte-63 word-index
    unsigned-num unsigned-reg)
  (def-data-vector-frobs simple-array-unsigned-byte-64 word-index
    unsigned-num unsigned-reg)
  (def-data-vector-frobs simple-array-unsigned-fixnum word-index
    positive-fixnum any-reg)
  (def-data-vector-frobs simple-array-fixnum word-index
    tagged-num any-reg)
  (def-data-vector-frobs simple-array-signed-byte-32 32-bits-index
    signed-num signed-reg)
  (def-data-vector-frobs simple-array-signed-byte-64 word-index
    signed-num signed-reg))

(define-vop (%compare-and-swap-svref word-index-cas)
  (:note "inline array compare-and-swap")
  (:policy :fast-safe)
  (:variant vector-data-offset other-pointer-lowtag)
  (:translate %compare-and-swap-svref)
  (:arg-types simple-vector positive-fixnum * *))

;;; Integer vectors whose elements are smaller than a byte.  I.e. bit, 2-bit,
;;; and 4-bit vectors.
;;;

(macrolet ((def-small-data-vector-frobs (type bits)
  ;; We're treating the read/writable quantum as 32 bits here
  ;; rather than 64 so that the code is the same as for 32-bit ppc.
  ;; It looks like we can get the single bit read access
  ;; down a few instructions.
  ;; This C code to access char array 'b' as bits compiles to 5 instructions:
  ;;  { int index = bit >> shift; return (int8_t)(b[index]<<(bit & mask)) < 0; }
  ;;        srdi 9,3,3
  ;;        rlwinm 3,3,0,29,31
  ;;        lbzx 9,4,9
  ;;        slw 3,9,3
  ;;        rldicl 3,3,57,63
  ;; We of course have to add in vector-data-offset, and fixnumize the result.
  (let* ((elements-per-word (floor 32 bits))
         (bit-shift (1- (integer-length elements-per-word))))
    `(progn
       (define-vop (,(symbolicate 'data-vector-ref/ type))
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
           ;; temp = (index >> bit-shift) << 2)
           (inst rlwinm temp index ,(- 32 (- bit-shift 2)) ,(- bit-shift 2) 29)
           (inst addi temp temp (- (* vector-data-offset n-word-bytes)
                                   other-pointer-lowtag))
           (inst lwzx result object temp)
           (inst andi. temp index ,(1- elements-per-word))
           (inst xori temp temp ,(1- elements-per-word))
           ,@(unless (= bits 1)
               `((inst slwi temp temp ,(1- (integer-length bits)))))
           (inst srw result result temp)
           (inst andi. result result ,(1- (ash 1 bits)))
           (inst slwi value result n-fixnum-tag-bits)))
       (define-vop (,(symbolicate 'data-vector-ref-c/ type))
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
             (let ((offset (- (* (+ word vector-data-offset)
                                 n-word-bytes)
                              other-pointer-lowtag)))
               (cond ((typep offset '(signed-byte 16))
                      (inst lwz result object offset))
                     (t
                      (inst lr temp offset)
                      (inst lwzx result object temp))))
             (unless (zerop extra)
               (inst srwi result result (* ,bits extra)))
             (unless (= extra ,(1- elements-per-word))
               (inst andi. result result ,(1- (ash 1 bits)))))))
       (define-vop (,(symbolicate 'data-vector-set/ type))
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
           ;; offset = (index >> bit-shift) << 2)
           (inst rlwinm offset index ,(- 32 (- bit-shift 2)) ,(- bit-shift 2) 29)
           (inst addi offset offset (- (* vector-data-offset n-word-bytes)
                                       other-pointer-lowtag))
           (inst lwzx old object offset)
           (inst andi. shift index ,(1- elements-per-word))
           (inst xori shift shift ,(1- elements-per-word))
           ,@(unless (= bits 1)
               `((inst slwi shift shift ,(1- (integer-length bits)))))
           (unless (and (sc-is value immediate)
                        (= (tn-value value) ,(1- (ash 1 bits))))
             (inst lr temp ,(1- (ash 1 bits)))
             (inst slw temp temp shift)
             (inst andc old old temp))
           (unless (sc-is value zero)
             (sc-case value
               (immediate
                (inst lr temp (logand (tn-value value) ,(1- (ash 1 bits)))))
               (unsigned-reg
                (inst andi. temp value ,(1- (ash 1 bits)))))
             (inst slw temp temp shift)
             (inst or old old temp))
           (inst stwx old object offset)
           (sc-case value
             (immediate
              (inst lr result (tn-value value)))
             (t
              (move result value)))))
       (define-vop (,(symbolicate 'data-vector-set-c/ type))
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
               (cond ((typep offset '(signed-byte 16))
                      (inst lwz old object offset))
                     (t
                      (inst lr offset-reg offset)
                      (inst lwzx old object offset-reg)))
               (unless (and (sc-is value immediate)
                            (= (tn-value value) ,(1- (ash 1 bits))))
                 (cond ((zerop extra)
                        (inst clrlwi old old ,bits))
                       (t
                        (inst lr temp
                              (lognot (ash ,(1- (ash 1 bits))
                                           (* (logxor extra
                                                      ,(1- elements-per-word))
                                              ,bits))))
                        (inst and old old temp))))
               (sc-case value
                 (zero)
                 (immediate
                  (let ((value (ash (logand (tn-value value)
                                            ,(1- (ash 1 bits)))
                                    (* (logxor extra
                                               ,(1- elements-per-word))
                                       ,bits))))
                    (cond ((typep value '(unsigned-byte 16))
                           (inst ori old old value))
                          (t
                           (inst lr temp value)
                           (inst or old old temp)))))
                 (unsigned-reg
                  (inst slwi temp value
                        (* (logxor extra ,(1- elements-per-word)) ,bits))
                  (inst or old old temp)))
               (if (typep offset '(signed-byte 16))
                   (inst stw old object offset)
                   (inst stwx old object offset-reg)))
             (sc-case value
               (immediate
                (inst lr result (tn-value value)))
               (t
                (move result value))))))))))
  (def-small-data-vector-frobs simple-bit-vector 1)
  (def-small-data-vector-frobs simple-array-unsigned-byte-2 2)
  (def-small-data-vector-frobs simple-array-unsigned-byte-4 4))


;;; And the float variants.
;;;

(defmacro compute-lispword-offset () ; for {tagged word, double float, complex single}
  '(progn
     (unless (= word-shift n-fixnum-tag-bits)
       (inst sldi offset index (- word-shift n-fixnum-tag-bits)))
     (inst addi offset index (- (* vector-data-offset n-word-bytes)
                                other-pointer-lowtag))))

(macrolet ((compute-sfloat-offset ()
             '(progn
                (case n-fixnum-tag-bits
                  (1 (inst sldi offset index 1))
                  (3 (inst srdi offset index 1)))
                (inst addi offset index (- (* vector-data-offset n-word-bytes)
                                           other-pointer-lowtag))))
           (compute-cplx-dfloat-offset ()
             '(progn
                (unless (= (1+ word-shift) n-fixnum-tag-bits)
                  (inst sldi offset index (- (1+ word-shift) n-fixnum-tag-bits)))
                (inst addi offset index (- (* vector-data-offset n-word-bytes)
                                           other-pointer-lowtag)))))

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
    (compute-sfloat-offset)
    (inst lfsx value object offset)))

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
    (compute-sfloat-offset)
    (inst stfsx value object offset)
    (unless (location= result value)
      (inst frsp result value))))

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
    (compute-lispword-offset)
    (inst lfdx value object offset)))

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
    (compute-lispword-offset)
    (inst stfdx value object offset)
    (unless (location= result value)
      (inst fmr result value))))


;;; Complex float arrays.

(define-vop (data-vector-ref/simple-array-complex-single-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:arg-types simple-array-complex-single-float positive-fixnum)
  (:results (value :scs (complex-single-reg)))
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:result-types complex-single-float)
  (:generator 5
    (compute-lispword-offset)
    (inst lfsx (complex-single-reg-real-tn value) object offset)
    (inst addi offset offset 4)
    (inst lfsx (complex-single-reg-imag-tn value) object offset)))

(define-vop (data-vector-set/simple-array-complex-single-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (complex-single-reg) :target result))
  (:arg-types simple-array-complex-single-float positive-fixnum
              complex-single-float)
  (:results (result :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:generator 5
    (compute-lispword-offset)
    (let ((value-real (complex-single-reg-real-tn value))
          (result-real (complex-single-reg-real-tn result)))
      (inst stfsx value-real object offset)
      (unless (location= result-real value-real)
        (inst frsp result-real value-real)))
    (let ((value-imag (complex-single-reg-imag-tn value))
          (result-imag (complex-single-reg-imag-tn result)))
      (inst addi offset offset 4)
      (inst stfsx value-imag object offset)
      (unless (location= result-imag value-imag)
        (inst frsp result-imag value-imag)))))

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
    (compute-cplx-dfloat-offset)
    (inst lfdx (complex-double-reg-real-tn value) object offset)
    (inst addi offset offset 8)
    (inst lfdx (complex-double-reg-imag-tn value) object offset)))

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
    (compute-cplx-dfloat-offset)
    (let ((value-real (complex-double-reg-real-tn value))
          (result-real (complex-double-reg-real-tn result)))
      (inst stfdx value-real object offset)
      (unless (location= result-real value-real)
        (inst fmr result-real value-real)))
    (let ((value-imag (complex-double-reg-imag-tn value))
          (result-imag (complex-double-reg-imag-tn result)))
      (inst addi offset offset 8)
      (inst stfdx value-imag object offset)
      (unless (location= result-imag value-imag)
        (inst fmr result-imag value-imag))))))


;;; These vops are useful for accessing the bits of a vector irrespective of
;;; what type of vector it is.
;;;

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
  (:arg-types * positive-fixnum unsigned-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:variant vector-data-offset other-pointer-lowtag))

;;;

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
             signed-16-bits-index-ref)
  (:note "inline array access")
  (:variant vector-data-offset other-pointer-lowtag)
  (:translate data-vector-ref)
  (:arg-types simple-array-signed-byte-16 positive-fixnum)
  (:results (value :scs (signed-reg)))
  (:result-types tagged-num))

(define-vop (data-vector-set/simple-array-signed-byte-16 16-bits-index-set)
  (:note "inline array store")
  (:variant vector-data-offset other-pointer-lowtag)
  (:translate data-vector-set)
  (:arg-types simple-array-signed-byte-16 positive-fixnum tagged-num)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg zero immediate))
         (value :scs (signed-reg)))
  (:results (result :scs (signed-reg)))
  (:result-types tagged-num))

;;;; ATOMIC-INCF for arrays

(define-vop (array-atomic-incf/word)
  (:translate %array-atomic-incf/word)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg) :target offset)
         (diff :scs (unsigned-reg)))
  (:arg-types * positive-fixnum unsigned-num)
  (:results (result :scs (unsigned-reg) :from :load))
  (:result-types unsigned-num)
  (:temporary (:sc unsigned-reg :from (:argument 1)) offset)
  (:temporary (:sc non-descriptor-reg) sum)
  (:generator 4
    (compute-lispword-offset)e
    ;; load the slot value, add DIFF, write the sum back, and return
    ;; the original slot value, atomically, and include a memory
    ;; barrier.
    (inst sync)
    LOOP
    (inst ldarx result offset object)
    (inst add sum result diff)
    (inst stdcx. sum offset object)
    (inst bne LOOP)
    (inst isync)))
