;;;; allocation VOPs for the ARM

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
  (:args (things :more t :scs (descriptor-reg any-reg control-stack constant immediate)))
  (:temporary (:scs (descriptor-reg)) ptr temp)
  (:temporary (:scs (descriptor-reg) :to (:result 0) :target result)
              res)
  (:temporary (:scs (non-descriptor-reg) :offset lr-offset) lr)
  (:info star cons-cells)
  (:results (result :scs (descriptor-reg)))
  (:node-var node)
  (:vop-var vop)
  (:generator 0
    (let ((alloc (* (pad-data-block cons-size) cons-cells))
          (dx (node-stack-allocate-p node))
          (prev-constant))
      (cond ((and (= cons-cells 1)
                  (aligned-stack-p dx))
             (inst stp (maybe-load (tn-ref-tn things))
                   (if star
                       (maybe-load (tn-ref-tn (tn-ref-across things)) result)
                       null-tn)
                   (@ csp-tn (* cons-size n-word-bytes) :post-index))
             (inst add-sub result csp-tn (- list-pointer-lowtag (* cons-size n-word-bytes))))
            (t
             (pseudo-atomic (lr :sync nil :elide-if dx)
               (allocation 'list alloc list-pointer-lowtag res
                           :flag-tn lr
                           :stack-allocate-p dx)
               (cond ((= cons-cells 1)
                      (inst stp (maybe-load (tn-ref-tn things))
                            (if star
                                (maybe-load (tn-ref-tn (tn-ref-across things)) ptr)
                                null-tn)
                            (@ tmp-tn)))
                     (t
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
                          ptr cons-cdr-slot list-pointer-lowtag))))
             (move result res))))))

(define-vop ()
  (:translate unaligned-dx-cons)
  (:args (car))
  (:results (result :scs (descriptor-reg)))
  (:ignore car)
  (:policy :fast-safe)
  (:generator 0
    (inst str null-tn (@ csp-tn n-word-bytes :post-index))
    (inst add-sub result csp-tn (- list-pointer-lowtag (* cons-size n-word-bytes)))))

;;;; Special purpose inline allocators.

(define-vop (make-fdefn)
  (:args (name :scs (descriptor-reg) :to :eval))
  (:temporary (:sc non-descriptor-reg) temp)
  (:temporary (:scs (non-descriptor-reg) :offset lr-offset) lr)
  (:results (result :scs (descriptor-reg) :from :argument))
  (:policy :fast-safe)
  (:translate make-fdefn)
  (:generator 37
    (with-fixed-allocation (result lr fdefn-widetag fdefn-size)
      (load-asm-routine temp 'undefined-tramp)
      (storew name result fdefn-name-slot other-pointer-lowtag)
      (storew null-tn result fdefn-fun-slot other-pointer-lowtag)
      (storew temp result fdefn-raw-addr-slot other-pointer-lowtag))))

(define-vop (make-closure)
  (:info label length stack-allocate-p)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:scs (non-descriptor-reg) :offset lr-offset) lr)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (let* ((size (+ length closure-info-offset))
           (alloc-size (pad-data-block size)))
      (pseudo-atomic (lr :elide-if stack-allocate-p)
        (allocation nil alloc-size fun-pointer-lowtag result
                    :flag-tn lr
                    :stack-allocate-p stack-allocate-p)
        (load-immediate-word temp
                             (logior (ash (1- size) n-widetag-bits) closure-widetag))
        (inst adr lr label (ash simple-fun-insts-offset word-shift))
        (storew-pair temp 0 lr closure-fun-slot tmp-tn)))))

(define-vop (reference-closure)
  (:info label)
  (:results (result :scs (descriptor-reg)))
  (:generator 1
    (inst adr result label fun-pointer-lowtag)))

;;; The compiler likes to be able to directly make value cells.
;;;
(define-vop (make-value-cell)
  (:args (value :to :save :scs (descriptor-reg any-reg zero)))
  (:temporary (:scs (non-descriptor-reg) :offset lr-offset) lr)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (with-fixed-allocation (result lr value-cell-widetag
                            value-cell-size :store-type-code nil)
      (storew-pair lr 0 value value-cell-value-slot tmp-tn))))

;;;; Automatic allocators for primitive objects.

(define-vop (make-unbound-marker)
  (:args)
  (:results (result :scs (descriptor-reg any-reg)))
  (:generator 1
    (inst mov result unbound-marker-widetag)))

(define-vop (fixed-alloc)
  (:args)
  (:info name words type lowtag stack-allocate-p)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg) :offset lr-offset) lr)
  (:generator 4
    (with-fixed-allocation (result lr type words
                            :lowtag lowtag
                            :stack-allocate-p stack-allocate-p))))

(define-vop (var-alloc)
  (:args (extra :scs (any-reg) :target bytes))
  (:arg-types positive-fixnum)
  (:info name words type lowtag stack-allocate-p)
  (:ignore name stack-allocate-p)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (any-reg) :from :argument) bytes)
  (:temporary (:sc non-descriptor-reg) header)
  (:temporary (:scs (non-descriptor-reg) :offset lr-offset) lr)
  (:generator 6
    ;; Build the object header, assuming that the header was in WORDS
    ;; but should not be in the header
    (inst lsl bytes extra (- word-shift n-fixnum-tag-bits))
    (let ((words (add-sub-immediate (* (1- words) n-word-bytes))))
      (unless (eql words 0)
        (inst add bytes bytes words)))
    (inst lsl header bytes (- (length-field-shift type) word-shift))
    (inst add header header type)
    ;; Add the object header to the allocation size and round up to
    ;; the allocation granularity
    (inst add bytes bytes (* 2 n-word-bytes))
    (inst and bytes bytes (bic-mask lowtag-mask))
    ;; Allocate the object and set its header
    (pseudo-atomic (lr)
      (allocation nil bytes lowtag result :flag-tn lr)
      (storew header result 0 lowtag))))

#+(and sb-xc-host immobile-space)
(define-vop (alloc-immobile-fixedobj)
  (:args (size-class :scs (any-reg) :target c-arg1)
         (nwords :scs (any-reg) :target c-arg2)
         (header :scs (any-reg) :target c-arg3))
  (:results (result :scs (descriptor-reg)))
  (:save-p t)
  (:temporary (:sc unsigned-reg :from (:argument 0) :to :eval :offset nl0-offset) c-arg1)
  (:temporary (:sc unsigned-reg :from (:argument 1) :to :eval :offset nl1-offset) c-arg2)
  (:temporary (:sc unsigned-reg :from (:argument 2) :to :eval :offset nl2-offset) c-arg3)
  (:temporary (:sc non-descriptor-reg :offset lr-offset) lr)
  (:temporary (:sc any-reg :offset r9-offset) cfunc)
  (:temporary (:sc unsigned-reg :from :eval :to (:result 0) :offset nl0-offset) nl0)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:vop-var vop)
  (:generator 50
   (let ((cur-nfp (current-nfp-tn vop)))
     (when cur-nfp
       (store-stack-tn nfp-save cur-nfp))
     (move c-arg1 size-class)
     (move c-arg2 nwords)
     (move c-arg3 header)
     (load-foreign-symbol cfunc "alloc_immobile_fixedobj")
     (pseudo-atomic (lr)
       (invoke-foreign-routine "call_into_c" lr)
       (when cur-nfp
         (load-stack-tn cur-nfp nfp-save))
       (move result nl0)))))
