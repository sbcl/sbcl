;;;; allocation VOPs for the HPPA

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
                `(let ((reg (sc-case ,tn
                              ((any-reg descriptor-reg zero null) ,tn)
                              (control-stack
                                (load-stack-tn temp ,tn)
                                temp))))
                   (storew reg ,list ,slot list-pointer-lowtag))))
             (let* ((dx-p (node-stack-allocate-p node))
                    (cons-cells (if star (1- num) num))
                    (alloc (* (pad-data-block cons-size) cons-cells)))
               (pseudo-atomic (:extra (if dx-p 0 alloc))
                 (when dx-p
                   (align-csp res))
                 (set-lowtag list-pointer-lowtag (if dx-p csp-tn alloc-tn) res)
                 (when dx-p
                   (if (typep alloc '(signed-byte 14))
                       (inst ldo alloc csp-tn csp-tn)
                       ;; FIXME: We have TEMP available, so can do an
                       ;; LIDL / LDO / ADD sequence here instead of
                       ;; punting.
                       (error "VOP LIST-OR-LIST* can't stack-allocate more than 511 CONSes at once")))
                 (move res ptr)
                 (dotimes (i (1- cons-cells))
                   (store-car (tn-ref-tn things) ptr)
                   (setf things (tn-ref-across things))
                   (inst addi (pad-data-block cons-size) ptr ptr)
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
  (:results (result :scs (descriptor-reg) :from :load))
  #!-stack-allocatable-vectors
  (:translate allocate-vector)
  (:policy :fast-safe)
  (:generator 100
    (inst addi (+ lowtag-mask
                  (* vector-data-offset n-word-bytes)) words bytes)
    (inst dep 0 31 n-lowtag-bits bytes)
    (pseudo-atomic ()
      (set-lowtag other-pointer-lowtag alloc-tn result)
      (inst add bytes alloc-tn alloc-tn)
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
  (:results (result :scs (descriptor-reg) :from :load))
  (:policy :fast-safe)
  (:generator 100
    (inst addi (+ lowtag-mask
                  (* vector-data-offset n-word-bytes)) words bytes)
    (inst dep 0 31 n-lowtag-bits bytes)
    ;; FIXME: It would be good to check for stack overflow here.
    (pseudo-atomic ()
      (align-csp temp)
      (set-lowtag other-pointer-lowtag csp-tn result)
      (inst addi (* vector-data-offset n-word-bytes) csp-tn temp)
      (inst add bytes csp-tn csp-tn)
      (storew type result 0 other-pointer-lowtag)
      (storew length result vector-length-slot other-pointer-lowtag)
      (let ((loop (gen-label)))
        (emit-label loop)
        (inst comb :<> temp csp-tn loop :nullify t)
        (inst stwm zero-tn n-word-bytes temp)))))

(define-vop (allocate-code-object)
  (:args (boxed-arg :scs (any-reg))
         (unboxed-arg :scs (any-reg)))
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:temporary (:scs (any-reg) :from (:argument 0)) boxed)
  (:temporary (:scs (non-descriptor-reg)) unboxed)
  (:generator 100
    (inst addi (fixnumize (1+ code-constants-offset)) boxed-arg boxed)
    (inst dep 0 31 n-lowtag-bits boxed)
    (inst srl unboxed-arg word-shift unboxed)
    (inst addi lowtag-mask unboxed unboxed)
    (inst dep 0 31 n-lowtag-bits unboxed)
    (inst sll boxed (- n-widetag-bits word-shift) ndescr)
    (inst addi code-header-widetag ndescr ndescr)
    (pseudo-atomic ()
      (set-lowtag other-pointer-lowtag alloc-tn result)
      (inst add alloc-tn boxed alloc-tn)
      (inst add alloc-tn unboxed alloc-tn)
      (storew ndescr result 0 other-pointer-lowtag)
      (storew unboxed-arg result code-code-size-slot other-pointer-lowtag)
      (storew null-tn result code-entry-points-slot other-pointer-lowtag)
      (storew null-tn result code-debug-info-slot other-pointer-lowtag))))

(define-vop (make-fdefn)
  (:translate make-fdefn)
  (:policy :fast-safe)
  (:args (name :scs (descriptor-reg) :to :eval))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (result :scs (descriptor-reg) :from :argument))
  (:generator 37
    (with-fixed-allocation (result nil temp fdefn-widetag fdefn-size nil)
      (inst li (make-fixup "undefined_tramp" :foreign) temp)
      (storew name result fdefn-name-slot other-pointer-lowtag)
      (storew null-tn result fdefn-fun-slot other-pointer-lowtag)
      (storew temp result fdefn-raw-addr-slot other-pointer-lowtag))))

(define-vop (make-closure)
  (:args (function :to :save :scs (descriptor-reg)))
  (:info length stack-allocate-p)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (with-fixed-allocation
        (result nil temp closure-header-widetag
         (+ length closure-info-offset)
         stack-allocate-p :lowtag fun-pointer-lowtag)
      (storew function result closure-fun-slot fun-pointer-lowtag))))

;;; The compiler likes to be able to directly make value cells.
(define-vop (make-value-cell)
  (:args (value :to :save :scs (descriptor-reg any-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (result :scs (descriptor-reg)))
  (:info stack-allocate-p)
  (:generator 10
    (with-fixed-allocation
        (result nil temp value-cell-header-widetag value-cell-size stack-allocate-p)
      (storew value result value-cell-value-slot other-pointer-lowtag))))

;;;; Automatic allocators for primitive objects.

(define-vop (make-unbound-marker)
  (:args)
  (:results (result :scs (descriptor-reg any-reg)))
  (:generator 1
    (inst li unbound-marker-widetag result)))

(define-vop (make-funcallable-instance-tramp)
  (:args)
  (:results (result :scs (any-reg)))
  (:generator 1
    (inst li (make-fixup 'funcallable-instance-tramp :assembly-routine)
          result)))

(define-vop (fixed-alloc)
  (:args)
  (:info name words type lowtag stack-allocate-p)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 4
    (with-fixed-allocation
      (result nil temp type words stack-allocate-p
       :lowtag lowtag :maybe-write t))))

(define-vop (var-alloc)
  (:args (extra :scs (any-reg)))
  (:arg-types positive-fixnum)
  (:info name words type lowtag)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (any-reg)) bytes)
  (:temporary (:scs (non-descriptor-reg)) header)
  (:generator 6
    (inst addi (* (1+ words) n-word-bytes) extra bytes)
    (inst sll bytes (- n-widetag-bits 2) header)
    (inst addi (+ (ash -2 n-widetag-bits) type) header header)
    (inst dep 0 31 n-lowtag-bits bytes)
    (pseudo-atomic ()
      (set-lowtag lowtag alloc-tn result)
      (storew header result 0 lowtag)
      (inst add alloc-tn bytes alloc-tn))))

