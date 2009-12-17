;;;; allocation VOPs for Mips

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; LIST and LIST*
(define-vop (list-or-list*)
  (:args (things :more t))
  (:temporary (:scs (descriptor-reg) :type list) ptr)
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg) :type list :to (:result 0) :target result)
              res)
  (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:info num)
  (:results (result :scs (descriptor-reg)))
  (:variant-vars star)
  (:policy :safe)
  (:node-var node)
  (:generator 0
    (cond ((zerop num)
           (move result null-tn))
          ((and star (= num 1))
           (move result (tn-ref-tn things)))
          (t
           (macrolet
               ((store-car (tn list &optional (slot cons-car-slot))
                  `(let ((reg
                          (sc-case ,tn
                            ((any-reg descriptor-reg zero null)
                             ,tn)
                            (control-stack
                             (load-stack-tn temp ,tn)
                             temp))))
                     (storew reg ,list ,slot list-pointer-lowtag))))
             (let* ((dx-p (node-stack-allocate-p node))
                    (cons-cells (if star (1- num) num))
                    (alloc (* (pad-data-block cons-size) cons-cells)))
               (pseudo-atomic (pa-flag :extra (if dx-p 0 alloc))
                 (when dx-p
                   (align-csp res))
                 (inst srl res (if dx-p csp-tn alloc-tn) n-lowtag-bits)
                 (inst sll res n-lowtag-bits)
                 (inst or res list-pointer-lowtag)
                 (when dx-p
                   (inst addu csp-tn alloc))
                 (move ptr res)
                 (dotimes (i (1- cons-cells))
                   (store-car (tn-ref-tn things) ptr)
                   (setf things (tn-ref-across things))
                   (inst addu ptr ptr (pad-data-block cons-size))
                   (storew ptr ptr
                           (- cons-cdr-slot cons-size)
                           list-pointer-lowtag))
                 (store-car (tn-ref-tn things) ptr)
                 (cond (star
                        (setf things (tn-ref-across things))
                        (store-car (tn-ref-tn things) ptr cons-cdr-slot))
                       (t
                        (storew null-tn ptr
                                cons-cdr-slot list-pointer-lowtag)))
                 (aver (null (tn-ref-across things)))
                 (move result res))))))))

(define-vop (list list-or-list*)
  (:variant nil))

(define-vop (list* list-or-list*)
  (:variant t))

;;;; Special purpose inline allocators.

;;; ALLOCATE-VECTOR
(define-vop (allocate-vector-on-heap)
  (:args (type :scs (unsigned-reg))
         (length :scs (any-reg))
         (words :scs (any-reg)))
  (:arg-types positive-fixnum
              positive-fixnum
              positive-fixnum)
  (:temporary (:sc non-descriptor-reg) bytes)
  (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:results (result :scs (descriptor-reg) :from :load))
  (:policy :fast-safe)
  (:generator 100
    (inst addu bytes words (+ lowtag-mask
                              (* vector-data-offset n-word-bytes)))
    (inst srl bytes n-lowtag-bits)
    (inst sll bytes n-lowtag-bits)
    (pseudo-atomic (pa-flag)
      (inst or result alloc-tn other-pointer-lowtag)
      (inst addu alloc-tn bytes)
      (storew type result 0 other-pointer-lowtag)
      (storew length result vector-length-slot other-pointer-lowtag))))

(define-vop (allocate-vector-on-stack)
  (:args (type :scs (unsigned-reg))
         (length :scs (any-reg))
         (words :scs (any-reg)))
  (:arg-types positive-fixnum
              positive-fixnum
              positive-fixnum)
  (:temporary (:sc non-descriptor-reg) bytes)
  (:temporary (:sc non-descriptor-reg) temp)
  (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:results (result :scs (descriptor-reg) :from :load))
  (:policy :fast-safe)
  (:generator 100
    (inst addu bytes words (+ lowtag-mask
                              (* vector-data-offset n-word-bytes)))
    (inst srl bytes n-lowtag-bits)
    (inst sll bytes n-lowtag-bits)
    ;; FIXME: It would be good to check for stack overflow here.
    (pseudo-atomic (pa-flag)
      (align-csp temp)
      (inst or result csp-tn other-pointer-lowtag)
      (inst addu temp csp-tn (* vector-data-offset n-word-bytes))
      (inst addu csp-tn bytes)
      (storew type result 0 other-pointer-lowtag)
      (storew length result vector-length-slot other-pointer-lowtag)
      (let ((loop (gen-label)))
        (emit-label loop)
        (storew zero-tn temp 0)
        (inst bne temp csp-tn loop)
        (inst addu temp n-word-bytes))
      (align-csp temp))))

(define-vop (allocate-code-object)
  (:args (boxed-arg :scs (any-reg))
         (unboxed-arg :scs (any-reg)))
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:temporary (:scs (any-reg) :from (:argument 0)) boxed)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) unboxed)
  (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:generator 100
    (inst li ndescr (lognot lowtag-mask))
    (inst addu boxed boxed-arg
          (fixnumize (1+ code-trace-table-offset-slot)))
    (inst and boxed ndescr)
    (inst srl unboxed unboxed-arg word-shift)
    (inst addu unboxed unboxed lowtag-mask)
    (inst and unboxed ndescr)
    (inst sll ndescr boxed (- n-widetag-bits word-shift))
    (inst or ndescr code-header-widetag)

    (pseudo-atomic (pa-flag)
      (inst or result alloc-tn other-pointer-lowtag)
      (inst addu alloc-tn boxed)
      (storew ndescr result 0 other-pointer-lowtag)
      (storew unboxed result code-code-size-slot other-pointer-lowtag)
      (inst addu alloc-tn unboxed)
      (storew null-tn result code-entry-points-slot other-pointer-lowtag)
      (storew null-tn result code-debug-info-slot other-pointer-lowtag))))

(define-vop (make-fdefn)
  (:policy :fast-safe)
  (:translate make-fdefn)
  (:args (name :scs (descriptor-reg) :to :eval))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:results (result :scs (descriptor-reg) :from :argument))
  (:generator 37
    (with-fixed-allocation (result pa-flag temp fdefn-widetag fdefn-size nil)
      (inst li temp (make-fixup "undefined_tramp" :foreign))
      (storew name result fdefn-name-slot other-pointer-lowtag)
      (storew null-tn result fdefn-fun-slot other-pointer-lowtag)
      (storew temp result fdefn-raw-addr-slot other-pointer-lowtag))))

(define-vop (make-closure)
  (:args (function :to :save :scs (descriptor-reg)))
  (:info length stack-allocate-p)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (let* ((size (+ length closure-info-offset))
           (alloc-size (pad-data-block size)))
      (pseudo-atomic (pa-flag :extra (if stack-allocate-p 0 alloc-size))
        (cond (stack-allocate-p
               (align-csp result)
               (inst srl result csp-tn n-lowtag-bits)
               (inst addu csp-tn alloc-size))
              (t
               (inst srl result alloc-tn n-lowtag-bits)))
        (inst sll result n-lowtag-bits)
        (inst or result fun-pointer-lowtag)
        (inst li temp (logior (ash (1- size) n-widetag-bits)
                              closure-header-widetag))
        (storew temp result 0 fun-pointer-lowtag)
        (storew function result closure-fun-slot fun-pointer-lowtag)))))

;;; The compiler likes to be able to directly make value cells.
(define-vop (make-value-cell)
  (:args (value :to :save :scs (descriptor-reg any-reg null zero)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:info stack-allocate-p)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (with-fixed-allocation (result pa-flag temp value-cell-header-widetag
                            value-cell-size stack-allocate-p)
      (storew value result value-cell-value-slot other-pointer-lowtag))))

;;;; Automatic allocators for primitive objects.

(define-vop (make-unbound-marker)
  (:args)
  (:results (result :scs (any-reg)))
  (:generator 1
    (inst li result unbound-marker-widetag)))

(define-vop (make-funcallable-instance-tramp)
  (:args)
  (:results (result :scs (any-reg)))
  (:generator 1
    (inst li result (make-fixup "funcallable_instance_tramp" :foreign))))

(define-vop (fixed-alloc)
  (:args)
  (:info name words type lowtag stack-allocate-p)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:generator 4
    (pseudo-atomic (pa-flag :extra (if stack-allocate-p
                                       0
                                       (pad-data-block words)))
      (cond (stack-allocate-p
             (align-csp result)
             (inst or result csp-tn lowtag)
             (inst addu csp-tn (pad-data-block words)))
            (t
             ;; The pseudo-atomic bit in alloc-tn is set.  If the
             ;; lowtag also has a 1 bit in the same position, we're all
             ;; set.  Otherwise, we need to subtract the pseudo-atomic
             ;; bit.
             (inst or result alloc-tn (if (logbitp 0 lowtag) lowtag
                                                             (1- lowtag)))))
      (when type
        (inst li temp (logior (ash (1- words) n-widetag-bits) type))
        (storew temp result 0 lowtag)))))

(define-vop (var-alloc)
  (:args (extra :scs (any-reg)))
  (:arg-types positive-fixnum)
  (:info name words type lowtag)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (any-reg)) bytes)
  (:temporary (:scs (non-descriptor-reg)) header)
  (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:generator 6
    (inst addu bytes extra (* (1+ words) n-word-bytes))
    (inst sll header bytes (- n-widetag-bits n-fixnum-tag-bits))
    (inst addu header header (+ (ash -2 n-widetag-bits) type))
    (inst srl bytes bytes n-lowtag-bits)
    (inst sll bytes bytes n-lowtag-bits)
    (pseudo-atomic (pa-flag)
      (inst or result alloc-tn lowtag)
      (storew header result 0 lowtag)
      (inst addu alloc-tn alloc-tn bytes))))

