;;;; the HPPA definitions for array operations

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
  (:arg-types positive-fixnum positive-fixnum)
  (:temporary (:scs (any-reg)) bytes)
  (:temporary (:scs (non-descriptor-reg)) header)
  (:results (result :scs (descriptor-reg)))
  (:generator 13
    ;; Note: Cant use addi, the immediate is too large
    (inst li (+ (* (1+ array-dimensions-offset) n-word-bytes)
                lowtag-mask) header)
    (inst add header rank bytes)
    (inst li (lognot lowtag-mask) header)
    (inst and bytes header bytes)
    (inst addi (fixnumize (1- array-dimensions-offset)) rank header)
    (inst sll header n-widetag-bits header)
    (inst or header type header)
    (inst srl header n-fixnum-tag-bits header)
    (pseudo-atomic ()
      (set-lowtag other-pointer-lowtag alloc-tn result)
      (storew header result 0 other-pointer-lowtag)
      (inst add bytes alloc-tn alloc-tn))))


;;;; Additional accessors and setters for the array header.
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
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 6
    (loadw res x 0 other-pointer-lowtag)
    (inst sra res n-widetag-bits res)
    (inst addi (- (1- array-dimensions-offset)) res res)
    (inst sll res n-fixnum-tag-bits res)))

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
      (inst bc :>>= nil index bound error))))


;;;; Accessors/Setters

;;; Variants built on top of word-index-ref, etc.  I.e. those vectors whos
;;; elements are represented in integer registers and are built out of
;;; 8, 16, or 32 bit elements.
(macrolet ((def-full-data-vector-frobs (type element-type &rest scs)
  `(progn
     (define-full-reffer ,(symbolicate "DATA-VECTOR-REF/" type) ,type
       vector-data-offset other-pointer-lowtag
       ,(remove-if (lambda (x) (member x '(null zero))) scs)
       ,element-type
       data-vector-ref)
     (define-full-setter ,(symbolicate "DATA-VECTOR-SET/" type) ,type
       vector-data-offset other-pointer-lowtag ,scs ,element-type
       data-vector-set)))

           (def-partial-data-vector-frobs
             (type element-type size signed &rest scs)
  `(progn
     (define-partial-reffer ,(symbolicate "DATA-VECTOR-REF/" type) ,type
       ,size ,signed vector-data-offset other-pointer-lowtag ,scs
       ,element-type data-vector-ref)
     (define-partial-setter ,(symbolicate "DATA-VECTOR-SET/" type) ,type
       ,size vector-data-offset other-pointer-lowtag ,scs
       ,element-type data-vector-set))))

  (def-full-data-vector-frobs simple-vector *
                              descriptor-reg any-reg null zero)

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
       (define-vop (,(symbolicate 'data-vector-ref/ type))
         (:translate data-vector-ref)
         (:note "inline array access")
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg))
                (index :scs (unsigned-reg)))
         (:arg-types ,type positive-fixnum)
         (:results (result :scs (any-reg)))
         (:result-types positive-fixnum)
         (:temporary (:scs (interior-reg)) lip)
         (:temporary (:scs (non-descriptor-reg) :to (:result 0)) temp)
         (:generator 20
           (inst srl index ,bit-shift temp)
           (inst sh2add temp object lip)
           (inst zdep index ,(- 32 (integer-length bits)) ,bit-shift temp)
           ,@(unless (= bits 1)
               `((inst addi ,(1- bits) temp temp)))
           (inst mtctl temp :sar)
           (loadw result lip vector-data-offset other-pointer-lowtag)
           (inst extru result :variable ,bits result)
           (inst sll result n-fixnum-tag-bits result)))
       (define-vop (,(symbolicate 'data-vector-ref-c/ type))
         (:translate data-vector-ref)
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg)))
         (:arg-types ,type (:constant index))
         (:info index)
         (:results (result :scs (unsigned-reg)))
         (:result-types positive-fixnum)
         (:temporary (:scs (non-descriptor-reg) :to (:result 0)) temp)
         (:generator 15
           (multiple-value-bind (word extra) (floor index ,elements-per-word)
             (let ((offset (- (* (+ word vector-data-offset) n-word-bytes)
                              other-pointer-lowtag)))
               (cond ((typep offset '(signed-byte 14))
                      (inst ldw offset object result))
                     (t
                      (inst li offset temp)
                      (inst ldwx object temp result))))
             (inst extru result (+ (* extra ,bits) ,(1- bits)) ,bits result))))
       (define-vop (,(symbolicate 'data-vector-set/ type))
         (:note "inline array store")
         (:translate data-vector-set)
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg))
                (index :scs (unsigned-reg))
                (value :scs (unsigned-reg zero immediate) :target result))
         (:arg-types ,type positive-fixnum positive-fixnum)
         (:results (result :scs (unsigned-reg)))
         (:result-types positive-fixnum)
         (:temporary (:scs (non-descriptor-reg) :to (:result 0)) temp)
         (:temporary (:scs (non-descriptor-reg)) old)
         (:temporary (:scs (interior-reg)) lip)
         (:generator 25
           (inst srl index ,bit-shift temp)
           (inst sh2add temp object lip)
           (inst zdep index ,(- 32 (integer-length bits)) ,bit-shift temp)
           ,@(unless (= bits 1)
               `((inst addi ,(1- bits) temp temp)))
           (inst mtctl temp :sar)
           (loadw old lip vector-data-offset other-pointer-lowtag)
           (inst dep (sc-case value (immediate (tn-value value)) (t value))
                 :variable ,bits old)
           (storew old lip vector-data-offset other-pointer-lowtag)
           (sc-case value
             (immediate
              (inst li (tn-value value) result))
             (t
              (move value result)))))
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
         (:temporary (:scs (non-descriptor-reg)) old)
         (:temporary (:scs (interior-reg)) lip)
         (:generator 20
           (multiple-value-bind (word extra) (floor index ,elements-per-word)
             (let ((offset (- (* (+ word vector-data-offset) n-word-bytes)
                              other-pointer-lowtag)))
               (cond ((typep offset '(signed-byte 14))
                      (inst ldw offset object old))
                     (t
                      (inst li offset lip)
                      (inst add object lip lip)
                      (inst ldw 0 lip old)))
               (inst dep (sc-case value
                           (immediate (tn-value value))
                           (t value))
                     (+ (* extra ,bits) ,(1- bits))
                     ,bits
                     old)
               (if (typep offset '(signed-byte 14))
                   (inst stw old offset object)
                   (inst stw old (ldb (byte 11 0) offset) lip)))
             (sc-case value
               (immediate
                (inst li (tn-value value) result))
               (t
                (move value result))))))))))
  (def-small-data-vector-frobs simple-bit-vector 1)
  (def-small-data-vector-frobs simple-array-unsigned-byte-2 2)
  (def-small-data-vector-frobs simple-array-unsigned-byte-4 4))

;;; And the float variants.
(macrolet
  ((data-vector ((type set cost) &body body)
     (let* ((typen (case type (single 'single-float)
                              (double 'double-float)
                              (t type)))
            (name (symbolicate "DATA-VECTOR-" (if set "SET" "REF")
                               "/SIMPLE-ARRAY-" typen))
            (reg-type (symbolicate type "-REG")))
       `(define-vop (,name)
          (:translate ,(symbolicate "DATA-VECTOR-" (if set "SET" "REF")))
          (:note ,(concatenate 'string "inline array "
                 (if set "store" "access")))
          (:policy :fast-safe)
          (:args (object :scs (descriptor-reg) :to (:argument 1))
                 (index :scs (any-reg) :to (:argument 0) :target offset)
                 ,@(if set `((value :scs (,reg-type) :target result))))
          (:arg-types ,(symbolicate "SIMPLE-ARRAY-" typen) positive-fixnum
                      ,@(if set `(,typen)))
          (:results (,(if set 'result 'value) :scs (,reg-type)))
          (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) offset)
          (:result-types ,typen)
          (:generator ,cost
            ,@body)))))
  (data-vector (single nil 5)
    (inst addi (- (* vector-data-offset n-word-bytes)
                  other-pointer-lowtag)
          index offset)
    (inst fldx offset object value))
  (data-vector (single t 5)
    (inst addi (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)
          index offset)
    (inst fstx value offset object)
    (unless (location= result value)
      (inst funop :copy value result)))
  (data-vector (double nil 7)
    (inst sll index 1 offset)
    (inst addi (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)
          offset offset)
    (inst fldx offset object value))
  (data-vector (double t 7)
    (inst sll index 1 offset)
    (inst addi (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)
          offset offset)
    (inst fstx value offset object)
    (unless (location= result value)
      (inst funop :copy value result))))

(macrolet
  ((data-vector ((type set cost) &body body)
     (let* ((typen (case type (complex-single 'complex-single-float)
                              (complex-double 'complex-double-float)
                              (t type)))
            (name (symbolicate "DATA-VECTOR-" (if set "SET" "REF")
                               "/SIMPLE-ARRAY-" typen))
            (reg-type (symbolicate type "-REG")))
       `(define-vop (,name)
          (:translate ,(symbolicate "DATA-VECTOR-" (if set "SET" "REF")))
          (:note ,(concatenate 'string "inline array "
                 (if set "store" "access")))
          (:policy :fast-safe)
          (:args (object :scs (descriptor-reg) :to :result)
                 (index :scs (any-reg))
                 ,@(if set `((value :scs (,reg-type) :target result))))
          (:arg-types ,(symbolicate "SIMPLE-ARRAY-" typen) positive-fixnum
                      ,@(if set `(,typen)))
          (:results (,(if set 'result 'value) :scs (,reg-type)))
          (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
          (:result-types ,typen)
          (:generator ,cost
            ,@body)))))
  (data-vector (complex-single nil 5)
    (inst sll index 1 offset)
    (inst addi (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)
          offset offset)
    (let ((real-tn (complex-single-reg-real-tn value)))
      (inst fldx offset object real-tn))
    (let ((imag-tn (complex-single-reg-imag-tn value)))
      (inst addi n-word-bytes offset offset)
      (inst fldx offset object imag-tn)))
  (data-vector (complex-single t 5)
    (inst sll index 1 offset)
    (inst addi (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)
          offset offset)
    (let ((value-real (complex-single-reg-real-tn value))
          (result-real (complex-single-reg-real-tn result)))
      (inst fstx value-real offset object)
      (unless (location= result-real value-real)
        (inst funop :copy value-real result-real)))
    (let ((value-imag (complex-single-reg-imag-tn value))
          (result-imag (complex-single-reg-imag-tn result)))
      (inst addi n-word-bytes offset offset)
      (inst fstx value-imag offset object)
      (unless (location= result-imag value-imag)
        (inst funop :copy value-imag result-imag))))
  (data-vector (complex-double nil 7)
    (inst sll index 2 offset)
    (inst addi (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)
          offset offset)
    (let ((real-tn (complex-double-reg-real-tn value)))
      (inst fldx offset object real-tn))
    (let ((imag-tn (complex-double-reg-imag-tn value)))
      (inst addi (* 2 n-word-bytes) offset offset)
      (inst fldx offset object imag-tn)))
  (data-vector (complex-double t 20)
    (inst sll index 2 offset)
    (inst addi (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)
          offset offset)
    (let ((value-real (complex-double-reg-real-tn value))
          (result-real (complex-double-reg-real-tn result)))
      (inst fstx value-real offset object)
      (unless (location= result-real value-real)
        (inst funop :copy value-real result-real)))
    (let ((value-imag (complex-double-reg-imag-tn value))
          (result-imag (complex-double-reg-imag-tn result)))
      (inst addi (* 2 n-word-bytes) offset offset)
      (inst fstx value-imag offset object)
      (unless (location= result-imag value-imag)
        (inst funop :copy value-imag result-imag)))))


;;; These vops are useful for accessing the bits of a vector irrespective of
;;; what type of vector it is.
(define-full-reffer vector-raw-bits * vector-data-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num %vector-raw-bits)
(define-full-setter set-vector-raw-bits * vector-data-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num %set-vector-raw-bits)
