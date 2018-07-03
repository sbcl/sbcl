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
  (:temporary (:scs (descriptor-reg)) ptr)
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg) :to (:result 0) :target result)
              res)
  (:temporary (:scs (non-descriptor-reg)) alloc-temp)
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
               (pseudo-atomic ()
                 (allocation res alloc list-pointer-lowtag
                             :stack-p dx-p
                             :temp-tn alloc-temp)
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
  ;; BOXED is a count of words as a fixnum; it is therefore also a byte count
  ;; as a raw value because n-fixnum-tag-bits = word-shift.
  (:args (boxed :scs (any-reg) :to :save)
         (unboxed-arg :scs (any-reg) :to :save))
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:temporary (:scs (non-descriptor-reg)) size)
  (:temporary (:scs (non-descriptor-reg)) unboxed)
  (:generator 100
    (inst srl unboxed unboxed-arg word-shift)
    (inst add unboxed lowtag-mask)
    (inst and unboxed (lognot lowtag-mask))
    (pseudo-atomic ()
      ;;
      ;; This looks like another dreadful type pun. CSR - 2002-02-06
      ;;
      ;; Not any more, or not in that sense at least, because the
      ;; "p/a bit is also the highest lowtag bit" assumption is now hidden
      ;; in the allocation macro.  DFL - 2012-10-01
      ;;
      ;; Figure out how much space we really need and allocate it.
      (inst add size boxed unboxed)
      (allocation result size other-pointer-lowtag :temp-tn ndescr)
      (inst sll ndescr boxed (- n-widetag-bits word-shift))
      (inst or ndescr code-header-widetag)
      (storew ndescr result 0 other-pointer-lowtag)
      (storew unboxed-arg result code-code-size-slot other-pointer-lowtag)
      (storew null-tn result code-debug-info-slot other-pointer-lowtag))))

(define-vop (make-fdefn)
  (:args (name :scs (descriptor-reg) :to :eval))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (result :scs (descriptor-reg) :from :argument))
  (:policy :fast-safe)
  (:translate make-fdefn)
  (:generator 37
    (with-fixed-allocation (result temp fdefn-widetag fdefn-size)
      (inst li temp (make-fixup 'undefined-tramp :assembly-routine))
      (storew name result fdefn-name-slot other-pointer-lowtag)
      (storew null-tn result fdefn-fun-slot other-pointer-lowtag)
      (storew temp result fdefn-raw-addr-slot other-pointer-lowtag))))


(define-vop (make-closure)
  (:args (function :to :save :scs (descriptor-reg)))
  (:info label length stack-allocate-p)
  (:ignore label)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (let* ((size (+ length closure-info-offset))
           (alloc-size (pad-data-block size)))
      (pseudo-atomic ()
        (allocation result alloc-size fun-pointer-lowtag
                    :stack-p stack-allocate-p
                    :temp-tn temp)
        (inst li temp (logior (ash (1- size) n-widetag-bits) closure-widetag))
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
        (result temp value-cell-widetag value-cell-size)
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
  (:ignore name stack-allocate-p)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 4
    (pseudo-atomic ()
      (allocation result (pad-data-block words) lowtag :temp-tn temp)
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
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 6
    (inst add bytes extra (* (1+ words) n-word-bytes))
    (inst sll header bytes (- n-widetag-bits 2))
    (inst add header header (+ (ash -2 n-widetag-bits) type))
    (inst and bytes (lognot lowtag-mask))
    (pseudo-atomic ()
      (allocation result bytes lowtag :temp-tn temp)
      (storew header result 0 lowtag))))
