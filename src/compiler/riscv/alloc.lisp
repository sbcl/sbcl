;;;; allocation VOPs for the RISC-V

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")


(define-vop (list)
  (:args (things :more t))
  (:temporary (:scs (descriptor-reg)) ptr)
  (:temporary (:scs (descriptor-reg) :to (:result 0) :target result)
              res)
  (:temporary (:scs (any-reg)) temp)
  (:temporary (:sc non-descriptor-reg) pa-flag)
  (:info star cons-cells)
  (:results (result :scs (descriptor-reg)))
  (:node-var node)
  (:generator 0
    (flet ((maybe-load (tn)
             (sc-case tn
              ((any-reg descriptor-reg) tn)
              (control-stack
               (load-stack-tn temp tn)
               temp))))
      (let ((alloc (* (pad-data-block cons-size) cons-cells)))
        (pseudo-atomic (pa-flag)
                   (allocation 'list alloc list-pointer-lowtag res
                               :flag-tn pa-flag
                               :stack-allocate-p (node-stack-allocate-p node)
                               :temp-tn temp)
                   (move ptr res)
                   (dotimes (i (1- cons-cells))
                     (storew (maybe-load (tn-ref-tn things)) ptr
                             cons-car-slot list-pointer-lowtag)
                     (setf things (tn-ref-across things))
                     (inst addi ptr ptr (pad-data-block cons-size))
                     (storew ptr ptr (- cons-cdr-slot cons-size)
                             list-pointer-lowtag))
                   (storew (maybe-load (tn-ref-tn things)) ptr
                     cons-car-slot list-pointer-lowtag)
                   (storew (if star
                               (maybe-load (tn-ref-tn (tn-ref-across things)))
                               null-tn)
                       ptr cons-cdr-slot list-pointer-lowtag))
        (move result res)))))

;;;; Special purpose inline allocators.

(define-vop (make-fdefn)
  (:args (name :scs (descriptor-reg) :to :eval))
  (:temporary (:sc non-descriptor-reg) pa-flag temp)
  (:results (result :scs (descriptor-reg) :from :argument))
  (:policy :fast-safe)
  (:translate make-fdefn)
  (:generator 37
    (with-fixed-allocation (result pa-flag fdefn-widetag fdefn-size)
      (inst li temp (make-fixup 'undefined-tramp :assembly-routine))
      (storew name result fdefn-name-slot other-pointer-lowtag)
      (storew null-tn result fdefn-fun-slot other-pointer-lowtag)
      (storew temp result fdefn-raw-addr-slot other-pointer-lowtag))))

;;; ALLOCATE-VECTOR
(define-vop (allocate-vector-on-heap)
  (:args (type :scs (unsigned-reg))
         (length :scs (any-reg))
         (words :scs (any-reg)))
  (:arg-types positive-fixnum
              positive-fixnum
              positive-fixnum)
  (:temporary (:sc non-descriptor-reg) bytes pa-flag)
  (:results (result :scs (descriptor-reg) :from :load))
  (:policy :fast-safe)
  (:generator 100
    (pseudo-atomic (pa-flag)
      (with-fixnum-as-word-index (words bytes)
        (inst addi bytes words (* (1+ vector-data-offset) n-word-bytes)))
      (inst andi bytes bytes (lognot lowtag-mask))
      (allocation nil bytes other-pointer-lowtag result :flag-tn pa-flag)
      (storew type result 0 other-pointer-lowtag)
      (storew length result vector-length-slot other-pointer-lowtag))))

(define-vop (allocate-vector-on-stack)
  (:args (type :scs (unsigned-reg))
         (length :scs (any-reg))
         (words :scs (any-reg)))
  (:arg-types positive-fixnum
              positive-fixnum
              positive-fixnum)
  (:temporary (:sc non-descriptor-reg) bytes temp)
  (:temporary (:sc non-descriptor-reg) pa-flag)
  (:results (result :scs (descriptor-reg) :from :load))
  (:policy :fast-safe)
  (:generator 100
    (pseudo-atomic (pa-flag)
      (with-fixnum-as-word-index (words bytes)
        (inst addi bytes words (* (1+ vector-data-offset) n-word-bytes)))
      (inst andi bytes bytes (lognot lowtag-mask))

      ;; FIXME: It would be good to check for stack overflow here.
      (allocation nil bytes other-pointer-lowtag result
                  :flag-tn pa-flag :stack-allocate-p t)

      (storew type result 0 other-pointer-lowtag)

      ;; The header word has already been set, skip it.
      (inst addi temp result (- (* vector-data-offset n-word-bytes)
                                other-pointer-lowtag))
      ;; Zero fill
      (let ((loop (gen-label)))
        (emit-label loop)
        (storew zero-tn temp 0)
        (inst addi temp temp n-word-bytes)
        ;; FIXME: This breaks the allocation abstraction a little.
        (inst blt temp csp-tn loop))
      ;; Our zero-fill loop always executes at least one store, so to
      ;; ensure that there is at least one slot available to be
      ;; clobbered, we defer setting the vector-length slot until now.
      (storew length result vector-length-slot other-pointer-lowtag))))

(define-vop (make-closure)
  (:args (function :to :save :scs (descriptor-reg)))
  (:info label length stack-allocate-p)
  (:ignore label)
  (:temporary (:sc non-descriptor-reg) pa-flag)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (let* ((size (+ length closure-info-offset))
           (alloc-size (pad-data-block size)))
      (pseudo-atomic (pa-flag)
        (allocation nil alloc-size fun-pointer-lowtag result
                    :flag-tn pa-flag
                    :stack-allocate-p stack-allocate-p)
        (inst li pa-flag (logior (ash (1- size) n-widetag-bits)
                                 closure-widetag))
        (storew pa-flag result 0 fun-pointer-lowtag)
        (storew function result closure-fun-slot fun-pointer-lowtag)))))

;;; The compiler likes to be able to directly make value cells.
(define-vop (make-value-cell)
  (:args (value :to :save :scs (descriptor-reg any-reg)))
  (:temporary (:sc non-descriptor-reg) pa-flag)
  (:results (result :scs (descriptor-reg)))
  (:info stack-allocate-p)
  (:generator 10
    (with-fixed-allocation (result pa-flag value-cell-widetag value-cell-size
                            :stack-allocate-p stack-allocate-p)
      (storew value result value-cell-value-slot other-pointer-lowtag))))

;;;; Automatic allocators for primitive objects.

(define-vop (make-unbound-marker)
  (:args)
  (:results (result :scs (descriptor-reg any-reg)))
  (:generator 1
    (inst li result unbound-marker-widetag)))

(define-vop (make-funcallable-instance-tramp)
  (:args)
  (:results (result :scs (any-reg)))
  (:generator 1
    (inst li result (make-fixup 'funcallable-instance-tramp :assembly-routine))))

(define-vop (fixed-alloc)
  (:args)
  (:info name words type lowtag stack-allocate-p)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc non-descriptor-reg) pa-flag)
  (:generator 4
    (with-fixed-allocation (result pa-flag type words :lowtag lowtag
                            :stack-allocate-p stack-allocate-p))))

(define-vop (var-alloc)
  (:args (extra :scs (any-reg)))
  (:arg-types positive-fixnum)
  (:info name words type lowtag stack-allocate-p)
  (:ignore name stack-allocate-p)
  (:temporary (:scs (any-reg)) bytes)
  (:temporary (:sc non-descriptor-reg) pa-flag)
  (:temporary (:sc non-descriptor-reg) header)
  (:results (result :scs (descriptor-reg)))
  (:generator 6
    (with-fixnum-as-word-index (extra bytes)
      (inst addi bytes extra (* (1- words) n-word-bytes)))
    (inst slli header bytes (- (length-field-shift type) word-shift))
    (inst addi header header type)
    ;; Add the object header to the allocation size and round up to
    ;; the allocation granularity.
    ;; The specified EXTRA value is the exact value placed in the
    ;; header as the word count when allocating code.
    (unless #-64-bit (= type code-header-widetag) #+64-bit nil
      (inst addi bytes bytes (* 2 n-word-bytes)))
    (inst andi bytes bytes (lognot lowtag-mask))
    (pseudo-atomic (pa-flag)
      (allocation nil bytes lowtag result :flag-tn pa-flag)
      (storew header result 0 lowtag))))
