;;;; allocation VOPs for the Alpha port

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
  (:info num)
  (:results (result :scs (descriptor-reg)))
  (:variant-vars star)
  (:policy :safe)
  (:node-var node)
  (:generator 0
    (cond ((zerop num)
           (move null-tn result))
          ((and star (= num 1))
           (move (tn-ref-tn things) result))
          (t
           (macrolet
               ((store-car (tn list &optional (slot cons-car-slot))
                  `(let ((reg
                          (sc-case ,tn
                            ((any-reg descriptor-reg) ,tn)
                            (zero zero-tn)
                            (null null-tn)
                            (control-stack
                             (load-stack-tn temp ,tn)
                             temp))))
                     (storew reg ,list ,slot list-pointer-lowtag))))
             (let* ((dx-p (node-stack-allocate-p node))
                    (cons-cells (if star (1- num) num))
                    (space (* (pad-data-block cons-size) cons-cells)))
               (pseudo-atomic (:extra (if dx-p 0 space))
                 (cond (dx-p
                        (align-csp res)
                        (inst bis csp-tn list-pointer-lowtag res)
                        (inst lda csp-tn space csp-tn))
                       (t
                        (inst bis alloc-tn list-pointer-lowtag res)))
                 (move res ptr)
                 (dotimes (i (1- cons-cells))
                   (store-car (tn-ref-tn things) ptr)
                   (setf things (tn-ref-across things))
                   (inst lda ptr (pad-data-block cons-size) ptr)
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
                 (move res result))))))))

(define-vop (list list-or-list*)
  (:variant nil))

(define-vop (list* list-or-list*)
  (:variant t))

;;;; special purpose inline allocators

(define-vop (allocate-code-object)
  (:args (boxed-arg :scs (any-reg))
         (unboxed-arg :scs (any-reg)))
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:temporary (:scs (any-reg) :from (:argument 0)) boxed)
  (:temporary (:scs (non-descriptor-reg)) unboxed)
  (:generator 100
    (inst li (lognot lowtag-mask) ndescr)
    (inst lda boxed (fixnumize (1+ code-constants-offset)) boxed-arg)
    (inst and boxed ndescr boxed)
    (inst srl unboxed-arg word-shift unboxed)
    (inst lda unboxed lowtag-mask unboxed)
    (inst and unboxed ndescr unboxed)
    (inst sll boxed (- n-widetag-bits word-shift) ndescr)
    (inst bis ndescr code-header-widetag ndescr)

    (pseudo-atomic ()
      (inst bis alloc-tn other-pointer-lowtag result)
      (storew ndescr result 0 other-pointer-lowtag)
      (storew unboxed-arg result code-code-size-slot other-pointer-lowtag)
      (storew null-tn result code-entry-points-slot other-pointer-lowtag)
      (inst addq alloc-tn boxed alloc-tn)
      (inst addq alloc-tn unboxed alloc-tn))

    (storew null-tn result code-debug-info-slot other-pointer-lowtag)))

(define-vop (make-fdefn)
  (:policy :fast-safe)
  (:translate make-fdefn)
  (:args (name :scs (descriptor-reg) :to :eval))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (result :scs (descriptor-reg) :from :argument))
  (:generator 37
    (with-fixed-allocation (result temp fdefn-widetag fdefn-size)
      (storew name result fdefn-name-slot other-pointer-lowtag)
      (storew null-tn result fdefn-fun-slot other-pointer-lowtag)
      (inst li (make-fixup "undefined_tramp" :foreign) temp)
      (storew temp result fdefn-raw-addr-slot other-pointer-lowtag))))

(define-vop (make-closure)
  (:args (function :to :save :scs (descriptor-reg)))
  (:info length stack-allocate-p)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (let* ((size (+ length closure-info-offset))
           (alloc-size (pad-data-block size)))
      (inst li
            (logior (ash (1- size) n-widetag-bits) closure-header-widetag)
            temp)
      (pseudo-atomic (:extra (if stack-allocate-p 0 alloc-size))
        (cond (stack-allocate-p
               (align-csp result)
               (inst bis csp-tn fun-pointer-lowtag result)
               (inst lda csp-tn alloc-size csp-tn))
              (t
               (inst bis alloc-tn fun-pointer-lowtag result)))
        (storew temp result 0 fun-pointer-lowtag)
        (storew function result closure-fun-slot fun-pointer-lowtag)))))

;;; The compiler likes to be able to directly make value cells.
(define-vop (make-value-cell)
  (:args (value :to :save :scs (descriptor-reg any-reg null zero)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:info stack-allocate-p)
  (:ignore stack-allocate-p)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (with-fixed-allocation
        (result temp value-cell-header-widetag value-cell-size)
      (storew value result value-cell-value-slot other-pointer-lowtag))))

;;;; automatic allocators for primitive objects

(define-vop (make-unbound-marker)
  (:args)
  (:results (result :scs (descriptor-reg any-reg)))
  (:generator 1
    (inst li unbound-marker-widetag result)))

(define-vop (make-funcallable-instance-tramp)
  (:args)
  (:results (result :scs (any-reg)))
  (:generator 1
    (inst li (make-fixup "funcallable_instance_tramp" :foreign) result)))

(define-vop (fixed-alloc)
  (:args)
  (:info name words type lowtag stack-allocate-p)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 4
     (pseudo-atomic (:extra (if stack-allocate-p
                                0
                                (pad-data-block words)))
      (cond (stack-allocate-p
             (align-csp result)
             (inst bis csp-tn lowtag result)
             (inst addq csp-tn (pad-data-block words) csp-tn))
            (t
             (inst bis alloc-tn lowtag result)))
      (when type
        (inst li (logior (ash (1- words) n-widetag-bits) type) temp)
        (storew temp result 0 lowtag)))))

(define-vop (var-alloc)
  (:args (extra :scs (any-reg)))
  (:arg-types positive-fixnum)
  (:info name words type lowtag)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) header)
  (:temporary (:scs (non-descriptor-reg)) bytes)
  (:generator 6
    (inst lda bytes (* (1+ words) n-word-bytes) extra)
    (inst sll bytes (- n-widetag-bits 2) header)
    (inst lda header (+ (ash -2 n-widetag-bits) type) header)
    (inst srl bytes n-lowtag-bits bytes)
    (inst sll bytes n-lowtag-bits bytes)
    (pseudo-atomic ()
      (inst bis alloc-tn lowtag result)
      (storew header result 0 lowtag)
      (inst addq alloc-tn bytes alloc-tn))))
