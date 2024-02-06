(in-package #:sb-vm)

(macrolet
    ((define-instruction-vop (instruction-record-name)
       (with-accessors ((name sb-simd-internals:instruction-record-name)
                        (vop sb-simd-internals:instruction-record-vop)
                        (mnemonic sb-simd-internals:instruction-record-mnemonic)
                        (argument-records sb-simd-internals:instruction-record-argument-records)
                        (result-records sb-simd-internals:instruction-record-result-records)
                        (cost sb-simd-internals:instruction-record-cost)
                        (pure sb-simd-internals:instruction-record-pure)
                        (always-translatable sb-simd-internals:instruction-record-always-translatable)
                        (associative sb-simd-internals:instruction-record-associative)
                        (prefix sb-simd-internals:instruction-record-prefix)
                        (suffix sb-simd-internals:instruction-record-suffix)
                        (encoding sb-simd-internals:instruction-record-encoding))
           (sb-simd-internals:find-function-record instruction-record-name)
         (let* ((asyms (sb-simd-internals:prefixed-symbols "A" (length argument-records)))
                (rsyms (sb-simd-internals:prefixed-symbols "R" (length result-records)))
                (defknown
                    `(defknown ,vop
                         (,@(mapcar #'sb-simd-internals:value-record-name argument-records))
                         (values ,@(mapcar #'sb-simd-internals:value-record-name result-records) &optional)
                         (,@(when (and always-translatable (not (eq encoding :fake-vop)))
                              '(always-translatable))
                          ,@(when pure '(foldable flushable movable)))
                       :overwrite-fndb-silently t))
                (arg-types
                  (mapcar #'sb-simd-internals:value-record-primitive-type argument-records))
                (result-types
                  (mapcar #'sb-simd-internals:value-record-primitive-type result-records))
                (args
                  (loop for asym in asyms
                        for argument-record in argument-records
                        when (symbolp (sb-simd-internals:value-record-primitive-type argument-record))
                          collect `(,asym :scs ,(sb-simd-internals:value-record-scs argument-record))))
                (info
                  (loop for asym in asyms
                        for argument-record in argument-records
                        unless (symbolp (sb-simd-internals:value-record-primitive-type argument-record))
                          collect asym))
                (results
                  (loop for rsym in rsyms
                        for result-record in result-records
                        collect `(,rsym :scs ,(sb-simd-internals:value-record-scs result-record)))))
           (ecase encoding
             ((:fake-vop :custom)
              `(progn ,defknown))
             (:standard
              (assert mnemonic)
              `(progn
                 ,defknown
                 (define-vop (,vop)
                   (:translate ,vop)
                   (:policy :fast-safe)
                   (:args ,@args)
                   (:info ,@info)
                   (:results ,@results)
                   (:arg-types ,@arg-types)
                   (:result-types ,@result-types)
                   (:generator
                    ,cost
                    (inst ,mnemonic ,@prefix ,@rsyms ,@asyms ,@suffix)))))
             (:move
              (assert mnemonic)
              (let ((src (first asyms))
                    (dst (first rsyms)))
                `(progn
                   ,defknown
                   (define-vop (,vop)
                     (:translate ,vop)
                     (:policy :fast-safe)
                     (:args (,@(first args) :target ,dst) ,@(rest args))
                     (:info ,@info)
                     (:results ,@results)
                     (:arg-types ,@arg-types)
                     (:result-types ,@result-types)
                     (:generator
                      ,cost
                      (unless (location= ,dst ,src)
                        (inst ,mnemonic ,@prefix ,@rsyms ,@asyms ,@suffix)))))))
             (:sse
              (assert mnemonic)
              (let ((x (first asyms))
                    (y (second asyms))
                    (rest (rest (rest asyms)))
                    (r (first rsyms)))
                `(progn
                   ,defknown
                   (define-vop (,vop)
                     (:translate ,vop)
                     (:policy :fast-safe)
                     (:args (,@(first args) :target ,r) ,@(rest args))
                     (:temporary (:sc ,(first (sb-simd-internals:value-record-scs (first argument-records)))) tmp)
                     (:info ,@info)
                     (:results ,@results)
                     (:arg-types ,@arg-types)
                     (:result-types ,@result-types)
                     (:generator
                      ,cost
                      (cond ((location= ,x ,r)
                             (inst ,mnemonic ,@prefix ,r ,y ,@rest ,@suffix))
                            ((or (not (tn-p ,y))
                                 (not (location= ,y ,r)))
                             (move ,r ,x)
                             (inst ,mnemonic ,@prefix ,r ,y ,@rest ,@suffix))
                            (t
                             (move tmp ,x)
                             (inst ,mnemonic ,@prefix tmp ,y ,@rest ,@suffix)
                             (move ,r tmp))))))))
             (:sse+xmm0
              (assert mnemonic)
              (let ((x (first asyms))
                    (y (second asyms))
                    (z (third asyms))
                    (r (first rsyms)))
                `(progn
                   ,defknown
                   (define-vop (,vop)
                     (:translate ,vop)
                     (:policy :fast-safe)
                     (:args (,@(first args) :target ,r) (,@(second args) :to :save) (,@(third args) :target xmm0))
                     (:temporary (:sc ,(first (sb-simd-internals:value-record-scs (second argument-records)))
                                  :from (:argument 2) :to :save :offset 0) xmm0)
                     (:info ,@info)
                     (:results ,@results)
                     (:arg-types ,@arg-types)
                     (:result-types ,@result-types)
                     (:generator
                       ,cost
                       (move xmm0 ,z)
                       (move ,r ,x)
                       (inst ,mnemonic ,@prefix ,r ,y xmm0 ,@suffix))))))
             (:fma
              (assert mnemonic)
              (let ((x (first asyms))
                    (y (second asyms))
                    (z (third asyms))
                    (rest (rest (rest (rest asyms))))
                    (r (first rsyms)))
                `(progn
                   ,defknown
                   (define-vop (,vop)
                     (:translate ,vop)
                     (:policy :fast-safe)
                     (:args (,@(first args) :target ,r) ,@(rest args))
                     (:temporary (:sc ,(first (sb-simd-internals:value-record-scs (first argument-records)))) tmp)
                     (:info ,@info)
                     (:results ,@results)
                     (:arg-types ,@arg-types)
                     (:result-types ,@result-types)
                     (:generator
                      ,cost
                      (cond ((location= ,x ,r)
                             (inst ,mnemonic ,@prefix ,r ,y ,z ,@rest ,@suffix))
                            ((and (or (not (tn-p ,y))
                                      (not (location= ,y ,r)))
                                  (or (not (tn-p ,z))
                                      (not (location= ,z ,r))))
                             (move ,r ,x)
                             (inst ,mnemonic ,@prefix ,r ,y ,z ,@rest ,@suffix))
                            (t
                             (move tmp ,x)
                             (inst ,mnemonic ,@prefix tmp ,y ,z ,@rest ,@suffix)
                             (move ,r tmp))))))))))))
     (define-instruction-vops ()
       `(progn
          ,@(loop for instruction-record
                    in (sb-simd-internals:filter-available-function-records
                        #'sb-simd-internals:instruction-record-p)
                  collect `(define-instruction-vop ,(sb-simd-internals:instruction-record-name instruction-record))))))
  (define-instruction-vops))
