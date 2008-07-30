;;;; allocation VOPs for the Sparc port

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
           (move result null-tn))
          ((and star (= num 1))
           (move result (tn-ref-tn things)))
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
             (let* ((dx-p (node-stack-allocate-p node))
                    (cons-cells (if star (1- num) num))
                    (alloc (* (pad-data-block cons-size) cons-cells)))
               (pseudo-atomic (:extra (if dx-p 0 alloc))
                 (let ((allocation-area-tn (if dx-p csp-tn alloc-tn)))
                   (when dx-p
                     (align-csp res))
                   (inst andn res allocation-area-tn lowtag-mask)
                   (inst or res list-pointer-lowtag)
                   (when dx-p
                     (inst add csp-tn csp-tn alloc)))
                 (move ptr res)
                 (dotimes (i (1- cons-cells))
                   (storew (maybe-load (tn-ref-tn things)) ptr
                           cons-car-slot list-pointer-lowtag)
                   (setf things (tn-ref-across things))
                   (inst add ptr ptr (pad-data-block cons-size))
                   (storew ptr ptr
                           (- cons-cdr-slot cons-size)
                           list-pointer-lowtag))
                 (storew (maybe-load (tn-ref-tn things)) ptr
                         cons-car-slot list-pointer-lowtag)
                 (storew (if star
                             (maybe-load (tn-ref-tn (tn-ref-across things)))
                             null-tn)
                         ptr cons-cdr-slot list-pointer-lowtag))
               (move result res)))))))

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
    (inst add boxed boxed-arg (fixnumize (1+ code-trace-table-offset-slot)))
    (inst and boxed (lognot lowtag-mask))
    (inst srl unboxed unboxed-arg word-shift)
    (inst add unboxed lowtag-mask)
    (inst and unboxed (lognot lowtag-mask))
    (pseudo-atomic ()
      ;; CMUCL Comment:
      ;; Note: we don't have to subtract off the 4 that was added by
      ;; pseudo-atomic, because oring in other-pointer-lowtag just adds
      ;; it right back.
      ;;
      ;; This looks like another dreadful type pun. CSR - 2002-02-06
      (inst or result alloc-tn other-pointer-lowtag)
      (inst add alloc-tn boxed)
      (inst add alloc-tn unboxed)
      (inst sll ndescr boxed (- n-widetag-bits word-shift))
      (inst or ndescr code-header-widetag)
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
      (inst li temp (make-fixup "undefined_tramp" :foreign))
      (storew name result fdefn-name-slot other-pointer-lowtag)
      (storew null-tn result fdefn-fun-slot other-pointer-lowtag)
      (storew temp result fdefn-raw-addr-slot other-pointer-lowtag))))


(define-vop (make-closure)
  (:args (function :to :save :scs (descriptor-reg)))
  (:info length stack-allocate-p)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (let* ((size (+ length closure-info-offset))
           (alloc-size (pad-data-block size)))
      (pseudo-atomic (:extra (if stack-allocate-p 0 alloc-size))
        (cond (stack-allocate-p
               (align-csp temp)
               (inst andn result csp-tn lowtag-mask)
               (inst or result fun-pointer-lowtag)
               (inst add csp-tn alloc-size))
              (t
               (inst andn result alloc-tn lowtag-mask)
               (inst or result fun-pointer-lowtag)))
        (inst li temp (logior (ash (1- size) n-widetag-bits) closure-header-widetag))
        (storew temp result 0 fun-pointer-lowtag)
        (storew function result closure-fun-slot fun-pointer-lowtag)))))

;;; The compiler likes to be able to directly make value cells.
(define-vop (make-value-cell)
  (:args (value :to :save :scs (descriptor-reg any-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:info stack-allocate-p)
  (:ignore stack-allocate-p)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (with-fixed-allocation
        (result temp value-cell-header-widetag value-cell-size)
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
  (:ignore name stack-allocate-p)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 4
    (pseudo-atomic (:extra (pad-data-block words))
      (cond ((logbitp (1- n-lowtag-bits) lowtag)
             (inst or result alloc-tn lowtag))
            (t
             (inst andn result alloc-tn lowtag-mask)
             (inst or result lowtag)))
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
  (:generator 6
    (inst add bytes extra (* (1+ words) n-word-bytes))
    (inst sll header bytes (- n-widetag-bits 2))
    (inst add header header (+ (ash -2 n-widetag-bits) type))
    (inst and bytes (lognot lowtag-mask))
    (pseudo-atomic ()
      ;; Need to be careful if the lowtag and the pseudo-atomic flag
      ;; are not compatible.
      (cond ((logbitp (1- n-lowtag-bits) lowtag)
             (inst or result alloc-tn lowtag))
            (t
             (inst andn result alloc-tn lowtag-mask)
             (inst or result lowtag)))
      (storew header result 0 lowtag)
      (inst add alloc-tn alloc-tn bytes))))
