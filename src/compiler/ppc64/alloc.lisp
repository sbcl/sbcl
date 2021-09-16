;;;; allocation VOPs for the PPC

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;;; LIST and LIST*
(define-vop (list)
  (:args (things :more t))
  (:temporary (:scs (descriptor-reg)) ptr)
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg) :to (:result 0) :target result)
              res)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:temporary (:scs (non-descriptor-reg)) alloc-temp)
  (:info star cons-cells)
  (:results (result :scs (descriptor-reg)))
  (:node-var node)
  #-gencgc (:ignore alloc-temp)
  (:generator 0
    (macrolet ((maybe-load (tn)
                  (once-only ((tn tn))
                    `(sc-case ,tn
                       ((any-reg descriptor-reg null)
                        ,tn)
                       (control-stack
                        (load-stack-tn temp ,tn)
                        temp)))))
      (let ((dx-p (node-stack-allocate-p node))
            (alloc (* (pad-data-block cons-size) cons-cells)))
        (pseudo-atomic (pa-flag :sync nil)
                 (if dx-p
                     (progn
                       (align-csp res)
                       (inst clrrdi res csp-tn n-lowtag-bits)
                       (inst ori res res list-pointer-lowtag)
                       (inst addi csp-tn csp-tn alloc))
                     (allocation 'list alloc list-pointer-lowtag res
                                 :temp-tn alloc-temp
                                 :flag-tn pa-flag))
                 (move ptr res)
                 (dotimes (i (1- cons-cells))
                   (storew (maybe-load (tn-ref-tn things)) ptr
                           cons-car-slot list-pointer-lowtag)
                   (setf things (tn-ref-across things))
                   (inst addi ptr ptr (pad-data-block cons-size))
                   (storew ptr ptr
                           (- cons-cdr-slot cons-size)
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
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:results (result :scs (descriptor-reg) :from :argument))
  (:policy :fast-safe)
  (:translate make-fdefn)
  (:generator 37
    (with-fixed-allocation (result pa-flag temp fdefn-widetag fdefn-size)
      (inst addi temp null-tn (make-fixup 'undefined-tramp :asm-routine-nil-offset))
      (storew name result fdefn-name-slot other-pointer-lowtag)
      (storew null-tn result fdefn-fun-slot other-pointer-lowtag)
      (storew temp result fdefn-raw-addr-slot other-pointer-lowtag))))

(define-vop (make-closure)
  (:args (function :to :save :scs (descriptor-reg)))
  (:info label length stack-allocate-p)
  (:ignore label)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (let* ((size (+ length closure-info-offset))
           (alloc-size (pad-data-block size)))
      (pseudo-atomic (pa-flag)
        (if stack-allocate-p
            (progn
              (align-csp result)
              (inst clrrdi result csp-tn n-lowtag-bits)
              (inst addi csp-tn csp-tn alloc-size)
              (inst ori result result fun-pointer-lowtag)
              (inst lr temp (logior (ash (1- size) n-widetag-bits) closure-widetag)))
            (progn
              (allocation nil (pad-data-block size) fun-pointer-lowtag result
                          :temp-tn temp :flag-tn pa-flag)
              (inst lr temp (logior (ash (1- size) n-widetag-bits) closure-widetag))))
        (storew temp result 0 fun-pointer-lowtag)
        (storew function result closure-fun-slot fun-pointer-lowtag)))))

;;; The compiler likes to be able to directly make value cells.
;;;
(define-vop (make-value-cell)
  (:args (value :to :save :scs (descriptor-reg any-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:info stack-allocate-p)
  (:ignore stack-allocate-p)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (with-fixed-allocation (result pa-flag temp value-cell-widetag value-cell-size)
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
    (inst addi result null-tn (make-fixup 'funcallable-instance-tramp :asm-routine-nil-offset))))

(define-vop (fixed-alloc)
  (:args)
  (:info name words type lowtag stack-allocate-p)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:generator 4
    (with-fixed-allocation (result pa-flag temp type words
                                   :lowtag lowtag
                                   :stack-allocate-p stack-allocate-p)
      )))

(define-vop (var-alloc)
  (:args (extra :scs (any-reg)))
  (:arg-types positive-fixnum)
  (:info name words type lowtag stack-allocate-p)
  (:ignore name #-gencgc temp stack-allocate-p)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (any-reg)) bytes)
  (:temporary (:scs (non-descriptor-reg)) header)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:generator 6
    ;; 'extra' is a tagged fixnum expressing the word count
    (cond ((= word-shift n-fixnum-tag-bits)
           ;; 'extra' already looks like an unsigned-reg expressing a byte count
           (inst addi bytes extra (* (1+ words) n-word-bytes)))
          (t
           ;; scale 'extra' to make it into an unsigned-reg byte count
           (inst sldi bytes extra (- word-shift n-fixnum-tag-bits))
           (inst addi bytes bytes (* (1+ words) n-word-bytes))))
    ;; store 1+nwords into header-data, downscaling bytes to words
    (inst sldi header bytes (- (length-field-shift type) word-shift))
    ;; subtract the excess length and add in the widetag
    (inst addi header header (+ (ash -2 (length-field-shift type)) type))
    (inst clrrdi bytes bytes n-lowtag-bits) ; round down to even
    (pseudo-atomic (pa-flag)
      (allocation nil bytes lowtag result :temp-tn temp :flag-tn pa-flag)
      (storew header result 0 lowtag))))
