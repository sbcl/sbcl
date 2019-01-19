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
  (:temporary (:scs (non-descriptor-reg)) bytes header)
  (:temporary (:sc non-descriptor-reg :offset pa-flag-offset) pa-flag)
  (:results (result :scs (descriptor-reg)))
  (:generator 13
    (style-warn "unimplemented make-array-header")))


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
    (style-warn "unimmplemented array-rank-vop")))

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
  (def-full-data-vector-frobs simple-character-string character character-reg)

  (def-partial-data-vector-frobs simple-array-unsigned-byte-7 positive-fixnum 1 nil unsigned-reg signed-reg)
  (def-partial-data-vector-frobs simple-array-signed-byte-8 tagged-num 1 t signed-reg)
  (def-partial-data-vector-frobs simple-array-unsigned-byte-8 positive-fixnum 1 nil unsigned-reg signed-reg)
  (def-partial-data-vector-frobs simple-array-unsigned-byte-15 positive-fixnum 2 nil unsigned-reg signed-reg)
  (def-partial-data-vector-frobs simple-array-signed-byte-16 tagged-num 2 t signed-reg)
  (def-partial-data-vector-frobs simple-array-unsigned-byte-16 positive-fixnum 2 nil unsigned-reg signed-reg)
  (def-full-data-vector-frobs simple-array-unsigned-fixnum positive-fixnum any-reg)
  (def-full-data-vector-frobs simple-array-fixnum tagged-num any-reg)
  (def-full-data-vector-frobs simple-array-unsigned-byte-31 unsigned-num unsigned-reg)
  (def-full-data-vector-frobs simple-array-signed-byte-32 signed-num signed-reg)
  (def-full-data-vector-frobs simple-array-unsigned-byte-32 unsigned-num unsigned-reg))

;;; Integer vectors whose elements are smaller than a byte.  I.e. bit, 2-bit,
;;; and 4-bit vectors.
(macrolet
    ((def-small-data-vector-frobs (type bits)
       (let* ((refname (symbolicate "DATA-VECTOR-REF/" type))
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
              (:generator 20))
            (define-vop (,setname)
              (:note "inline array store")
              (:translate data-vector-set)
              (:policy :fast-safe)
              (:args (object :scs (descriptor-reg))
                     (index :scs (unsigned-reg))
                     (value :scs (unsigned-reg)))
              (:arg-types ,type positive-fixnum positive-fixnum)
              (:results (result :scs (unsigned-reg)))
              (:result-types positive-fixnum)
              (:generator 25))))))
  (def-small-data-vector-frobs simple-bit-vector 1)
  (def-small-data-vector-frobs simple-array-unsigned-byte-2 2)
  (def-small-data-vector-frobs simple-array-unsigned-byte-4 4))

;;; And the float variants.
(macrolet ((def-float-vector-frobs (type eltype sc)
             (let ((refname (symbolicate "DATA-VECTOR-REF/" type))
                   (setname (symbolicate "DATA-VECTOR-SET/" type)))
               `(progn
                  (define-vop (,refname)
                    (:note "inline array access")
                    (:translate data-vector-ref)
                    (:policy :fast-safe)
                    (:args (object :scs (descriptor-reg))
                           (index :scs (any-reg)))
                    (:arg-types ,type positive-fixnum)
                    (:results (value :scs (,sc)))
                    (:result-types ,eltype)
                    (:generator 20))
                  (define-vop (,setname)
                    (:note "inline array store")
                    (:translate data-vector-set)
                    (:policy :fast-safe)
                    (:args (object :scs (descriptor-reg))
                           (index :scs (unsigned-reg))
                           (value :scs (,sc)))
                    (:arg-types ,type positive-fixnum ,eltype)
                    (:results (result :scs (,sc)))
                    (:result-types ,eltype)
                    (:generator 5))))))
  (def-float-vector-frobs simple-array-single-float single-float single-reg)
  (def-float-vector-frobs simple-array-double-float double-float double-reg)
  (def-float-vector-frobs simple-array-complex-single-float complex-single-float complex-single-reg)
  (def-float-vector-frobs simple-array-complex-double-float complex-double-float complex-double-reg))


;;; These vops are useful for accessing the bits of a vector irrespective of
;;; what type of vector it is.
(define-full-reffer vector-raw-bits * vector-data-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num %vector-raw-bits)
(define-full-setter set-vector-raw-bits * vector-data-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num %set-vector-raw-bits)
