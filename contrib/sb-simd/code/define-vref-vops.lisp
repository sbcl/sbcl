(in-package #:sb-vm)

;;; Both load- and store VOPs are augmented with an auxiliary last argument
;;; that is a constant addend for the address calculation.  This addend is
;;; zero by default, but we can sometimes transform the code for the index
;;; calculation such that we have a nonzero addend.  We also generate two
;;; variants of the VOP - one for the general case, and one for the case
;;; where the index is a compile-time constant.

(macrolet
    ((define-vref-vop (vref-record-name)
       (with-accessors ((name sb-simd-internals:vref-record-name)
                        (vop sb-simd-internals:vref-record-vop)
                        (vop-c sb-simd-internals:vref-record-vop-c)
                        (mnemonic sb-simd-internals:vref-record-mnemonic)
                        (value-record sb-simd-internals:vref-record-value-record)
                        (vector-record sb-simd-internals:vref-record-vector-record)
                        (store sb-simd-internals:store-record-p))
           (sb-simd-internals:find-function-record vref-record-name)
         (let* ((vector-type (sb-simd-internals:value-record-type vector-record))
                (vector-primitive-type (sb-simd-internals:value-record-primitive-type vector-record))
                (value-scs (sb-simd-internals:value-record-scs value-record))
                (value-type (sb-simd-internals:value-record-type value-record))
                (value-primitive-type (sb-simd-internals:value-record-primitive-type value-record))
                (scalar-record
                  (etypecase value-record
                    (sb-simd-internals:simd-record (sb-simd-internals:simd-record-scalar-record value-record))
                    (sb-simd-internals:value-record value-record)))
                (bits-per-element (sb-simd-internals:value-record-bits scalar-record))
                (bytes-per-element (ceiling bits-per-element 8))
                (displacement
                  (multiple-value-bind (lo hi)
                      (displacement-bounds other-pointer-lowtag bits-per-element vector-data-offset)
                    `(integer ,lo ,hi))))
           (multiple-value-bind (index-scs scale)
               (if (>= bytes-per-element (ash 1 n-fixnum-tag-bits))
                   (values '(any-reg signed-reg unsigned-reg) `(index-scale ,bytes-per-element index))
                   (values '(signed-reg unsigned-reg) bytes-per-element))
             `(progn
                (defknown ,vop (,@(when store `(,value-type)) ,vector-type index ,displacement)
                    (values ,value-type &optional)
                    (always-translatable)
                  :overwrite-fndb-silently t)
                (define-vop (,vop)
                  (:translate ,vop)
                  (:policy :fast-safe)
                  (:args
                   ,@(when store `((value :scs ,value-scs :target result)))
                   (vector :scs (descriptor-reg))
                   (index :scs ,index-scs))
                  (:info addend)
                  (:arg-types
                   ,@(when store `(,value-primitive-type))
                   ,vector-primitive-type
                   positive-fixnum
                   (:constant ,displacement))
                  (:results (result :scs ,value-scs))
                  (:result-types ,value-primitive-type)
                  (:generator
                   2
                   ,@(let ((ea `(ea (+ (* vector-data-offset n-word-bytes)
                                       (* addend ,bytes-per-element)
                                       (- other-pointer-lowtag))
                                    vector index ,scale)))
                       (if store
                           `((inst ,mnemonic ,ea value)
                             (move result value))
                           `((inst ,mnemonic result ,ea))))))
                (define-vop (,vop-c)
                  (:translate ,vop)
                  (:policy :fast-safe)
                  (:args ,@(when store `((value :scs ,value-scs :target result)))
                         (vector :scs (descriptor-reg)))
                  (:info index addend)
                  (:arg-types ,@(when store `(,value-primitive-type))
                              ,vector-primitive-type
                              (:constant low-index)
                              (:constant ,displacement))
                  (:results (result :scs ,value-scs))
                  (:result-types ,value-primitive-type)
                  (:generator
                   1
                   ,@(let ((ea `(ea (+ (* vector-data-offset n-word-bytes)
                                       (* ,bytes-per-element (+ index addend))
                                       (- other-pointer-lowtag))
                                    vector)))
                       (if store
                           `((inst ,mnemonic ,ea value)
                             (move result value))
                           `((inst ,mnemonic result ,ea)))))))))))
     (define-vref-vops ()
       `(progn
          ,@(loop for vref-record
                    in (sb-simd-internals:filter-available-function-records
                        #'sb-simd-internals:vref-record-p)
                  collect `(define-vref-vop ,(sb-simd-internals:vref-record-name vref-record))))))
  (define-vref-vops))
