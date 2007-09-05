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
  (:generator 0
    (cond
     ((zerop num)
      (move null-tn result))
     ((and star (= num 1))
      (move (tn-ref-tn things) result))
     (t
      (macrolet
          ((maybe-load (tn)
             (once-only ((tn tn))
               `(sc-case ,tn
                  ((any-reg descriptor-reg zero null)
                   ,tn)
                  (control-stack
                   (load-stack-tn temp ,tn)
                   temp)))))
        (let* ((cons-cells (if star (1- num) num))
               (alloc (* (pad-data-block cons-size) cons-cells)))
          (pseudo-atomic (:extra alloc)
            (move alloc-tn res)
            (inst dep list-pointer-lowtag 31 3 res)
            (move res ptr)
            (dotimes (i (1- cons-cells))
              (storew (maybe-load (tn-ref-tn things)) ptr
                      cons-car-slot list-pointer-lowtag)
              (setf things (tn-ref-across things))
              (inst addi (pad-data-block cons-size) ptr ptr)
              (storew ptr ptr
                      (- cons-cdr-slot cons-size)
                      list-pointer-lowtag))
            (storew (maybe-load (tn-ref-tn things)) ptr
                    cons-car-slot list-pointer-lowtag)
            (storew (if star
                        (maybe-load (tn-ref-tn (tn-ref-across things)))
                        null-tn)
                    ptr cons-cdr-slot list-pointer-lowtag))
          (move res result)))))))


(define-vop (list list-or-list*)
  (:variant nil))

(define-vop (list* list-or-list*)
  (:variant t))


;;;; Special purpose inline allocators.

(define-vop (allocate-code-object)
  (:args (boxed-arg :scs (any-reg))
         (unboxed-arg :scs (any-reg)))
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:temporary (:scs (any-reg) :from (:argument 0)) boxed)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) unboxed)
  (:generator 100
    (inst addi (fixnumize (1+ code-trace-table-offset-slot)) boxed-arg boxed)
    (inst dep 0 31 3 boxed)
    (inst srl unboxed-arg word-shift unboxed)
    (inst addi lowtag-mask unboxed unboxed)
    (inst dep 0 31 3 unboxed)
    (pseudo-atomic ()
      ;; Note: we don't have to subtract off the 4 that was added by
      ;; pseudo-atomic, because depositing other-pointer-lowtag just adds
      ;; it right back.
      (inst move alloc-tn result)
      (inst dep other-pointer-lowtag 31 3 result)
      (inst add alloc-tn boxed alloc-tn)
      (inst add alloc-tn unboxed alloc-tn)
      (inst sll boxed (- n-widetag-bits word-shift) ndescr)
      (inst addi code-header-widetag ndescr ndescr)
      (storew ndescr result 0 other-pointer-lowtag)
      (storew unboxed result code-code-size-slot other-pointer-lowtag)
      (storew null-tn result code-entry-points-slot other-pointer-lowtag)
      (storew null-tn result code-debug-info-slot other-pointer-lowtag))))

(define-vop (make-fdefn)
  (:args (name :scs (descriptor-reg) :to :eval))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (result :scs (descriptor-reg) :from :argument))
  (:policy :fast-safe)
  (:translate make-fdefn)
  (:generator 37
    (with-fixed-allocation (result temp fdefn-widetag fdefn-size)
      (inst li (make-fixup "undefined_tramp" :foreign) temp)
      (storew name result fdefn-name-slot other-pointer-lowtag)
      (storew null-tn result fdefn-fun-slot other-pointer-lowtag)
      (storew temp result fdefn-raw-addr-slot other-pointer-lowtag))))

(define-vop (make-closure)
  (:args (function :to :save :scs (descriptor-reg)))
  (:info length stack-allocate-p)
  (:ignore stack-allocate-p)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (let ((size (+ length closure-info-offset)))
      (pseudo-atomic (:extra (pad-data-block size))
        (inst move alloc-tn result)
        (inst dep fun-pointer-lowtag 31 3 result)
        (inst li (logior (ash (1- size) n-widetag-bits) closure-header-widetag) temp)
        (storew temp result 0 fun-pointer-lowtag)
        (storew function result closure-fun-slot fun-pointer-lowtag)))))

;;; The compiler likes to be able to directly make value cells.
(define-vop (make-value-cell)
  (:args (value :to :save :scs (descriptor-reg any-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (result :scs (descriptor-reg)))
  (:info stack-allocate-p)
  (:ignore stack-allocate-p)
  (:generator 10
    (with-fixed-allocation
        (result temp value-cell-header-widetag value-cell-size))
    (storew value result value-cell-value-slot other-pointer-lowtag)))



;;;; Automatic allocators for primitive objects.

(define-vop (make-unbound-marker)
  (:args)
  (:results (result :scs (any-reg)))
  (:generator 1
    (inst li unbound-marker-widetag result)))

(define-vop (make-funcallable-instance-tramp)
  (:args)
  (:results (result :scs (any-reg)))
  (:generator 1
    (inst li (make-fixup "funcallable_instance_tramp" :foreign) result)))

(define-vop (fixed-alloc)
  (:args)
  (:info name words type lowtag)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 4
    (pseudo-atomic (:extra (pad-data-block words))
      (inst move alloc-tn result)
      (inst dep lowtag 31 3 result)
      (when type
        (inst li (logior (ash (1- words) n-widetag-bits) type) temp)
        (storew temp result 0 lowtag)))))

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
    (inst dep 0 31 3 bytes)
    (pseudo-atomic ()
      (inst move alloc-tn result)
      (inst dep lowtag 31 3 result)
      (storew header result 0 lowtag)
      (inst add alloc-tn bytes alloc-tn))))
