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
       (let ((setname (symbolicate "DATA-VECTOR-SET/" type)))
         `(progn
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

(macrolet
    ((def-small-data-vector-frobs (type bits)
       (let ((setname (symbolicate "DATA-VECTOR-SET/" type)))
         `(progn
            (define-vop (,setname)
              (:translate data-vector-set)
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

(define-vop (data-vector-set/simple-array-single-float)
  (:translate data-vector-set)
  (:args (object :scs (descriptor-reg))
         (index :scs (unsigned-reg))
         (value :scs (single-reg)))
  (:arg-types simple-array-single-float positive-fixnum single-float)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:generator 5))

(define-vop (data-vector-set/simple-array-double-float)
  (:translate data-vector-set)
  (:args (object :scs (descriptor-reg))
         (index :scs (unsigned-reg))
         (value :scs (double-reg)))
  (:arg-types simple-array-double-float positive-fixnum double-float)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:generator 5))

(define-vop (data-vector-set/simple-array-complex-single-float)
  (:translate data-vector-set)
  (:args (object :scs (descriptor-reg))
         (index :scs (unsigned-reg))
         (value :scs (complex-single-reg)))
  (:arg-types simple-array-complex-single-float positive-fixnum complex-single-float)
  (:results (result :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 5))

(define-vop (data-vector-set/simple-array-complex-double-float)
  (:translate data-vector-set)
  (:args (object :scs (descriptor-reg))
         (index :scs (unsigned-reg))
         (value :scs (complex-double-reg)))
  (:arg-types simple-array-complex-double-float positive-fixnum complex-double-float)
  (:results (result :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 5))
