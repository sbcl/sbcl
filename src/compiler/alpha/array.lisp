;;; -*- Package: ALPHA -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;

;;;
;;; **********************************************************************
;;;
;;;    This file contains the Alpha definitions for array operations.
;;;
;;; Written by William Lott
;;; Conversion by Sean Hallgren
;;; Complex-float support by Douglas Crosher 1998.
;;;
(in-package "SB!VM")



;;;; Allocator for the array header.

(define-vop (make-array-header)
  (:policy :fast-safe)
  (:translate make-array-header)
  (:args (type :scs (any-reg))
	 (rank :scs (any-reg)))
  (:arg-types positive-fixnum positive-fixnum)
  (:temporary (:scs (any-reg)) bytes)
  (:temporary (:scs (non-descriptor-reg)) header)
  (:results (result :scs (descriptor-reg)))
  (:generator 13
    (inst addq rank (+ (* array-dimensions-offset word-bytes)
		       lowtag-mask)
	  bytes)
    (inst li (lognot lowtag-mask) header)
    (inst and bytes header bytes)
    (inst addq rank (fixnumize (1- array-dimensions-offset)) header)
    (inst sll header type-bits header)
    (inst bis header type header)
    (inst srl header 2 header)
    (pseudo-atomic ()
      (inst bis alloc-tn other-pointer-type result)
      (storew header result 0 other-pointer-type)
      (inst addq alloc-tn bytes alloc-tn))))



;;;; Additional accessors and setters for the array header.

(defknown sb!impl::%array-dimension (t index) index
  (flushable))
(defknown sb!impl::%set-array-dimension (t index index) index
  ())

(define-full-reffer %array-dimension *
  array-dimensions-offset other-pointer-type
  (any-reg) positive-fixnum sb!impl::%array-dimension)

(define-full-setter %set-array-dimension *
  array-dimensions-offset other-pointer-type
  (any-reg) positive-fixnum sb!impl::%set-array-dimension #+gengc nil)


(defknown sb!impl::%array-rank (t) index (flushable))

(define-vop (array-rank-vop)
  (:translate sb!impl::%array-rank)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 6
    (loadw temp x 0 other-pointer-type)
    (inst sra temp type-bits temp)
    (inst subq temp (1- array-dimensions-offset) temp)
    (inst sll temp 2 res)))



;;;; Bounds checking routine.


(define-vop (check-bound)
  (:translate %check-bound)
  (:policy :fast-safe)
  (:args (array :scs (descriptor-reg))
	 (bound :scs (any-reg descriptor-reg))
	 (index :scs (any-reg descriptor-reg) :target result))
  (:results (result :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 5
    (let ((error (generate-error-code vop invalid-array-index-error
				      array bound index)))
      (inst cmpult index bound temp)
      (inst beq temp error)
      (move index result))))



;;;; Accessors/Setters

;;; Variants built on top of word-index-ref, etc.  I.e. those vectors whos
;;; elements are represented in integer registers and are built out of
;;; 8, 16, or 32 bit elements.

(macrolet ((def-full-data-vector-frobs (type element-type &rest scs)
             `(progn
                (define-full-reffer ,(symbolicate "DATA-VECTOR-REF/" type) ,type
                  vector-data-offset other-pointer-type
                  ,(remove-if #'(lambda (x) (member x '(null zero))) scs)
                  ,element-type
                  data-vector-ref)
                (define-full-setter ,(symbolicate "DATA-VECTOR-SET/" type) ,type
                  vector-data-offset other-pointer-type ,scs ,element-type
                  data-vector-set #+gengc ,(if (member 'descriptor-reg scs) t nil))))

           (def-partial-data-vector-frobs
             (type element-type size signed &rest scs)
             `(progn
                (define-partial-reffer ,(symbolicate "DATA-VECTOR-REF/" type) ,type
                  ,size ,signed vector-data-offset other-pointer-type ,scs
                  ,element-type data-vector-ref)
                (define-partial-setter ,(symbolicate "DATA-VECTOR-SET/" type) ,type
                  ,size vector-data-offset other-pointer-type ,scs
                  ,element-type data-vector-set)))
           (def-small-data-vector-frobs (type bits)
             (let* ((elements-per-word (floor word-bits bits))
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
                    (:temporary (:scs (interior-reg)) lip)
                    (:temporary (:scs (non-descriptor-reg) :to (:result 0)) temp result)
                    (:generator 20
                                (inst srl index ,bit-shift temp)
                                (inst sll temp 2 temp)
                                (inst addq object temp lip)
                                (inst ldl result
                                      (- (* vector-data-offset word-bytes)
                                         other-pointer-type)
                                      lip)
                                (inst and index ,(1- elements-per-word) temp)
                                ,@(unless (= bits 1)
                                    `((inst sll temp ,(1- (integer-length bits)) temp)))
                                (inst srl result temp result)
                                (inst and result ,(1- (ash 1 bits)) result)
                                (inst sll result 2 value)))
                  (define-vop (,(symbolicate 'data-vector-ref-c/ type))
                    (:translate data-vector-ref)
                    (:policy :fast-safe)
                    (:args (object :scs (descriptor-reg)))
                    (:arg-types ,type
                                (:constant
                                 (integer 0
                                          ,(1- (* (1+ (- (floor (+ #x7fff
                                                                   other-pointer-type)
                                                                word-bytes)
                                                         vector-data-offset))
                                                  elements-per-word)))))
                    (:info index)
                    (:results (result :scs (unsigned-reg)))
                    (:result-types positive-fixnum)
                    (:generator 15
                                (multiple-value-bind (word extra) (floor index ,elements-per-word)
                                  (loadw result object (+ word vector-data-offset) 
                                         other-pointer-type)
                                  (unless (zerop extra)
                                    (inst srl result (* extra ,bits) result))
                                  (unless (= extra ,(1- elements-per-word))
                                    (inst and result ,(1- (ash 1 bits)) result)))))
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
                    (:temporary (:scs (interior-reg)) lip)
                    (:temporary (:scs (non-descriptor-reg)) temp old)
                    (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) shift)
                    (:generator 25
                                (inst srl index ,bit-shift temp)
                                (inst sll temp 2 temp)
                                (inst addq object temp lip)
                                (inst ldl old
                                      (- (* vector-data-offset word-bytes)
                                         other-pointer-type)
                                      lip)
                                (inst and index ,(1- elements-per-word) shift)
                                ,@(unless (= bits 1)
                                    `((inst sll shift ,(1- (integer-length bits)) shift)))
                                (unless (and (sc-is value immediate)
                                             (= (tn-value value) ,(1- (ash 1 bits))))
                                  (inst li ,(1- (ash 1 bits)) temp)
                                  (inst sll temp shift temp)
                                  (inst not temp temp)
                                  (inst and old temp old))
                                (unless (sc-is value zero)
                                  (sc-case value
                                           (immediate
                                            (inst li (logand (tn-value value) ,(1- (ash 1 bits))) temp))
                                           (unsigned-reg
                                            (inst and value ,(1- (ash 1 bits)) temp)))
                                  (inst sll temp shift temp)
                                  (inst bis old temp old))
                                (inst stl old
                                      (- (* vector-data-offset word-bytes)
                                         other-pointer-type)
                                      lip)
                                (sc-case value
                                         (immediate
                                          (inst li (tn-value value) result))
                                         (zero
                                          (move zero-tn result))
                                         (unsigned-reg
                                          (move value result)))))
                  (define-vop (,(symbolicate 'data-vector-set-c/ type))
                    (:translate data-vector-set)
                    (:policy :fast-safe)
                    (:args (object :scs (descriptor-reg))
                           (value :scs (unsigned-reg zero immediate) :target result))
                    (:arg-types ,type
                                (:constant
                                 (integer 0
                                          ,(1- (* (1+ (- (floor (+ #x7fff
                                                                   other-pointer-type)
                                                                word-bytes)
                                                         vector-data-offset))
                                                  elements-per-word))))
                                positive-fixnum)
                    (:info index)
                    (:results (result :scs (unsigned-reg)))
                    (:result-types positive-fixnum)
                    (:temporary (:scs (non-descriptor-reg)) temp old)
                    (:generator 20
                                (multiple-value-bind (word extra) (floor index ,elements-per-word)
                                  (inst ldl object
                                        (- (* (+ word vector-data-offset) word-bytes)
                                           other-pointer-type)
                                        old)
                                  (unless (and (sc-is value immediate)
                                               (= (tn-value value) ,(1- (ash 1 bits))))
                                    (cond ((= extra ,(1- elements-per-word))
                                           (inst sll old ,bits old)
                                           (inst srl old ,bits old))
                                          (t
                                           (inst li
                                                 (lognot (ash ,(1- (ash 1 bits)) (* extra ,bits)))
                                                 temp)
                                           (inst and old temp old))))
                                  (sc-case value
                                           (zero)
                                           (immediate
                                            (let ((value (ash (logand (tn-value value) ,(1- (ash 1 bits)))
                                                              (* extra ,bits))))
                                              (cond ((< value #x10000)
                                                     (inst bis old value old))
                                                    (t
                                                     (inst li value temp)
                                                     (inst bis old temp old)))))
                                           (unsigned-reg
                                            (inst sll value (* extra ,bits) temp)
                                            (inst bis old temp old)))
                                  (inst stl old
                                        (- (* (+ word vector-data-offset) word-bytes)
                                           other-pointer-type)
                                        object)
                                  (sc-case value
                                           (immediate
                                            (inst li (tn-value value) result))
                                           (zero
                                            (move zero-tn result))
                                           (unsigned-reg
                                            (move value result))))))))))
  (def-full-data-vector-frobs simple-vector *
    descriptor-reg any-reg null zero)
  
  (def-partial-data-vector-frobs simple-string base-char :byte nil
    base-char-reg)
  
  (def-partial-data-vector-frobs simple-array-unsigned-byte-8 positive-fixnum
    :byte nil unsigned-reg signed-reg)
  
  (def-partial-data-vector-frobs simple-array-unsigned-byte-16 positive-fixnum
    :short nil unsigned-reg signed-reg)
  
  (def-full-data-vector-frobs simple-array-unsigned-byte-32 unsigned-num
    unsigned-reg)
  
  (def-partial-data-vector-frobs simple-array-signed-byte-8 tagged-num
    :byte t signed-reg)
  
  (def-partial-data-vector-frobs simple-array-signed-byte-16 tagged-num
    :short t signed-reg)
  
  (def-full-data-vector-frobs simple-array-signed-byte-30 tagged-num any-reg)
  
  (def-full-data-vector-frobs simple-array-signed-byte-32 signed-num signed-reg)
  
  ;; Integer vectors whos elements are smaller than a byte.  I.e. bit, 2-bit,
  ;; and 4-bit vectors.
  ;;

  (def-small-data-vector-frobs simple-bit-vector 1)
  (def-small-data-vector-frobs simple-array-unsigned-byte-2 2)
  (def-small-data-vector-frobs simple-array-unsigned-byte-4 4))
  

;;; And the float variants.
;;; 

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
    (inst addq object index lip)
    (inst lds value
	  (- (* vector-data-offset word-bytes)
	     other-pointer-type)
	   lip)))

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
  (:generator 20
    (inst addq object index lip)
    (inst sts value
	  (- (* vector-data-offset word-bytes)
	     other-pointer-type)
	  lip)
    (unless (location= result value)
      (inst fmove value result))))

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
    (inst addq object index lip)
    (inst addq lip index lip)
    (inst ldt value
	  (- (* vector-data-offset word-bytes)
	     other-pointer-type)
	  lip)))

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
    (inst addq object index lip)
    (inst addq lip index lip)
    (inst stt value
	  (- (* vector-data-offset word-bytes)
	     other-pointer-type) lip)
    (unless (location= result value)
      (inst fmove value result))))


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
    (let ((real-tn (complex-single-reg-real-tn value)))
      (inst addq object index lip)
      (inst addq lip index lip)
      (inst lds real-tn
	    (- (* vector-data-offset word-bytes) other-pointer-type)
	    lip))
    (let ((imag-tn (complex-single-reg-imag-tn value)))
      (inst lds imag-tn
	    (- (* (1+ vector-data-offset) word-bytes) other-pointer-type)
	    lip))))

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
  (:temporary (:scs (interior-reg)) lip)
  (:generator 5
    (let ((value-real (complex-single-reg-real-tn value))
	  (result-real (complex-single-reg-real-tn result)))
      (inst addq object index lip)
      (inst addq lip index lip)
      (inst sts value-real
	    (- (* vector-data-offset word-bytes) other-pointer-type)
	    lip)
      (unless (location= result-real value-real)
	(inst fmove value-real result-real)))
    (let ((value-imag (complex-single-reg-imag-tn value))
	  (result-imag (complex-single-reg-imag-tn result)))
      (inst sts value-imag
	    (- (* (1+ vector-data-offset) word-bytes) other-pointer-type)
	    lip)
      (unless (location= result-imag value-imag)
	(inst fmove value-imag result-imag)))))

(define-vop (data-vector-ref/simple-array-complex-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg)))
  (:arg-types simple-array-complex-double-float positive-fixnum)
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 7
    (let ((real-tn (complex-double-reg-real-tn value)))
      (inst addq object index lip)
      (inst addq lip index lip)
      (inst addq lip index lip)
      (inst addq lip index lip)
      (inst ldt real-tn
	    (- (* vector-data-offset word-bytes) other-pointer-type)
	    lip))
    (let ((imag-tn (complex-double-reg-imag-tn value)))
      (inst ldt imag-tn
	    (- (* (+ vector-data-offset 2) word-bytes) other-pointer-type)
	    lip))))

(define-vop (data-vector-set/simple-array-complex-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
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
      (inst addq object index lip)
      (inst addq lip index lip)
      (inst addq lip index lip)
      (inst addq lip index lip)
      (inst stt value-real
	    (- (* vector-data-offset word-bytes) other-pointer-type)
	    lip)
      (unless (location= result-real value-real)
	(inst fmove value-real result-real)))
    (let ((value-imag (complex-double-reg-imag-tn value))
	  (result-imag (complex-double-reg-imag-tn result)))
      (inst stt value-imag
	    (- (* (+ vector-data-offset 2) word-bytes) other-pointer-type)
	    lip)
      (unless (location= result-imag value-imag)
	(inst fmove value-imag result-imag)))))


;;; These VOPs are used for implementing float slots in structures (whose raw
;;; data is an unsigned-32 vector.
;;;
(define-vop (raw-ref-single data-vector-ref/simple-array-single-float)
  (:translate %raw-ref-single)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum))
;;;
(define-vop (raw-set-single data-vector-set/simple-array-single-float)
  (:translate %raw-set-single)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum single-float))
;;;
(define-vop (raw-ref-double data-vector-ref/simple-array-double-float)
  (:translate %raw-ref-double)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum))
;;;
(define-vop (raw-set-double data-vector-set/simple-array-double-float)
  (:translate %raw-set-double)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum double-float))

(define-vop (raw-ref-complex-single
	     data-vector-ref/simple-array-complex-single-float)
  (:translate %raw-ref-complex-single)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum))
;;;
(define-vop (raw-set-complex-single
	     data-vector-set/simple-array-complex-single-float)
  (:translate %raw-set-complex-single)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum
	      complex-single-float))
;;;
(define-vop (raw-ref-complex-double
	     data-vector-ref/simple-array-complex-double-float)
  (:translate %raw-ref-complex-double)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum))
;;;
(define-vop (raw-set-complex-double
	     data-vector-set/simple-array-complex-double-float)
  (:translate %raw-set-complex-double)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum
	      complex-double-float))


;;; These vops are useful for accessing the bits of a vector irrespective of
;;; what type of vector it is.
;;; 

(define-full-reffer raw-bits * 0 other-pointer-type (unsigned-reg) unsigned-num
  %raw-bits)
(define-full-setter set-raw-bits * 0 other-pointer-type (unsigned-reg)
  unsigned-num %set-raw-bits #+gengc nil)



;;;; Misc. Array VOPs.

(define-vop (get-vector-subtype get-header-data))
(define-vop (set-vector-subtype set-header-data))
