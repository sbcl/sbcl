;;;; allocation VOPs for the x86-64

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; CONS, LIST and LIST*
(define-vop (list-or-list*)
  (:args (things :more t))
  (:temporary (:sc unsigned-reg) ptr temp)
  (:temporary (:sc unsigned-reg :to (:result 0) :target result) res)
  (:info num)
  (:results (result :scs (descriptor-reg)))
  (:variant-vars star)
  (:policy :safe)
  (:node-var node)
  (:generator 0
    (cond ((zerop num)
           ;; (move result nil-value)
           (inst mov result nil-value))
          ((and star (= num 1))
           (move result (tn-ref-tn things)))
          (t
           (macrolet
               ((store-car (tn list &optional (slot cons-car-slot))
                  `(let ((reg
                          (sc-case ,tn
                            ((any-reg descriptor-reg) ,tn)
                            ((control-stack)
                             (move temp ,tn)
                             temp))))
                     (storew reg ,list ,slot list-pointer-lowtag))))
             (let ((cons-cells (if star (1- num) num))
                   (stack-allocate-p (awhen (sb!c::node-lvar node)
                                       (sb!c::lvar-dynamic-extent it))))
               (maybe-pseudo-atomic stack-allocate-p
                (allocation res (* (pad-data-block cons-size) cons-cells) node
                            stack-allocate-p list-pointer-lowtag)
                (move ptr res)
                (dotimes (i (1- cons-cells))
                  (store-car (tn-ref-tn things) ptr)
                  (setf things (tn-ref-across things))
                  (inst add ptr (pad-data-block cons-size))
                  (storew ptr ptr (- cons-cdr-slot cons-size)
                          list-pointer-lowtag))
                (store-car (tn-ref-tn things) ptr)
                (cond (star
                       (setf things (tn-ref-across things))
                       (store-car (tn-ref-tn things) ptr cons-cdr-slot))
                      (t
                       (storew nil-value ptr cons-cdr-slot
                               list-pointer-lowtag)))
                (aver (null (tn-ref-across things)))))
             (move result res))))))

(define-vop (list list-or-list*)
  (:variant nil))

(define-vop (list* list-or-list*)
  (:variant t))

;;;; special-purpose inline allocators

;;; ALLOCATE-VECTOR
(define-vop (allocate-vector-on-heap)
  (:args (type :scs (unsigned-reg))
         (length :scs (any-reg))
         (words :scs (any-reg)))
  (:results (result :scs (descriptor-reg) :from :load))
  (:arg-types positive-fixnum
              positive-fixnum
              positive-fixnum)
  (:policy :fast-safe)
  (:generator 100
    (inst lea result (make-ea :byte :base words :disp
                              (+ (1- (ash 1 n-lowtag-bits))
                                 (* vector-data-offset n-word-bytes))))
    (inst and result (lognot lowtag-mask))
    (pseudo-atomic
      (allocation result result)
      (inst lea result (make-ea :byte :base result :disp other-pointer-lowtag))
      (storew type result 0 other-pointer-lowtag)
      (storew length result vector-length-slot other-pointer-lowtag))))

(define-vop (allocate-vector-on-stack)
  (:args (type :scs (unsigned-reg) :to :save)
         (length :scs (any-reg) :to :eval :target zero)
         (words :scs (any-reg) :target ecx))
  (:temporary (:sc any-reg :offset ecx-offset :from (:argument 2)) ecx)
  (:temporary (:sc any-reg :offset eax-offset :from :eval) zero)
  (:temporary (:sc any-reg :offset edi-offset) res)
  (:results (result :scs (descriptor-reg) :from :load))
  (:arg-types positive-fixnum
              positive-fixnum
              positive-fixnum)
  (:translate allocate-vector)
  (:policy :fast-safe)
  (:node-var node)
  (:generator 100
    (inst lea result (make-ea :byte :base words :disp
                              (+ (1- (ash 1 n-lowtag-bits))
                                 (* vector-data-offset n-word-bytes))))
    (inst and result (lognot lowtag-mask))
    ;; FIXME: It would be good to check for stack overflow here.
    (move ecx words)
    (inst shr ecx n-fixnum-tag-bits)
    (allocation result result node t other-pointer-lowtag)
    (inst cld)
    (inst lea res
          (make-ea :byte :base result :disp (- (* vector-data-offset n-word-bytes)
                                               other-pointer-lowtag)))
    (storew type result 0 other-pointer-lowtag)
    (storew length result vector-length-slot other-pointer-lowtag)
    (zeroize zero)
    (inst rep)
    (inst stos zero)))


(define-vop (make-fdefn)
  (:policy :fast-safe)
  (:translate make-fdefn)
  (:args (name :scs (descriptor-reg) :to :eval))
  (:results (result :scs (descriptor-reg) :from :argument))
  (:node-var node)
  (:generator 37
    (with-fixed-allocation (result fdefn-widetag fdefn-size node)
      (storew name result fdefn-name-slot other-pointer-lowtag)
      (storew nil-value result fdefn-fun-slot other-pointer-lowtag)
      (storew (make-fixup "undefined_tramp" :foreign)
              result fdefn-raw-addr-slot other-pointer-lowtag))))

(define-vop (make-closure)
  (:args (function :to :save :scs (descriptor-reg)))
  (:info length stack-allocate-p)
  (:temporary (:sc any-reg) temp)
  (:results (result :scs (descriptor-reg)))
  (:node-var node)
  (:generator 10
   (maybe-pseudo-atomic stack-allocate-p
    (let ((size (+ length closure-info-offset)))
      (allocation result (pad-data-block size) node stack-allocate-p
                  fun-pointer-lowtag)
      (storew (logior (ash (1- size) n-widetag-bits) closure-header-widetag)
              result 0 fun-pointer-lowtag))
    (loadw temp function closure-fun-slot fun-pointer-lowtag)
    (storew temp result closure-fun-slot fun-pointer-lowtag))))

;;; The compiler likes to be able to directly make value cells.
(define-vop (make-value-cell)
  (:args (value :scs (descriptor-reg any-reg) :to :result))
  (:results (result :scs (descriptor-reg) :from :eval))
  (:info stack-allocate-p)
  (:node-var node)
  (:generator 10
    (with-fixed-allocation
        (result value-cell-header-widetag value-cell-size node stack-allocate-p)
      (storew value result value-cell-value-slot other-pointer-lowtag))))

;;;; automatic allocators for primitive objects

(define-vop (make-unbound-marker)
  (:args)
  (:results (result :scs (any-reg)))
  (:generator 1
    (inst mov result unbound-marker-widetag)))

(define-vop (make-funcallable-instance-tramp)
  (:args)
  (:results (result :scs (any-reg)))
  (:generator 1
    (inst lea result (make-fixup "funcallable_instance_tramp" :foreign))))

(define-vop (fixed-alloc)
  (:args)
  (:info name words type lowtag stack-allocate-p)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:node-var node)
  (:generator 50
    (maybe-pseudo-atomic stack-allocate-p
     (allocation result (pad-data-block words) node stack-allocate-p lowtag)
     (when type
       (storew (logior (ash (1- words) n-widetag-bits) type)
               result
               0
               lowtag)))))

(define-vop (var-alloc)
  (:args (extra :scs (any-reg)))
  (:arg-types positive-fixnum)
  (:info name words type lowtag)
  (:ignore name)
  (:results (result :scs (descriptor-reg) :from (:eval 1)))
  (:temporary (:sc any-reg :from :eval :to (:eval 1)) bytes)
  (:temporary (:sc any-reg :from :eval :to :result) header)
  (:node-var node)
  (:generator 50
    (inst lea bytes
          (make-ea :qword :base extra :disp (* (1+ words) n-word-bytes)))
    (inst mov header bytes)
    (inst shl header (- n-widetag-bits 3)) ; w+1 to length field
    (inst lea header                    ; (w-1 << 8) | type
          (make-ea :qword :base header :disp (+ (ash -2 n-widetag-bits) type)))
    (inst and bytes (lognot lowtag-mask))
    (pseudo-atomic
     (allocation result bytes node)
     (inst lea result (make-ea :byte :base result :disp lowtag))
     (storew header result 0 lowtag))))

(define-vop (%make-symbol)
  (:policy :fast-safe)
  (:translate %make-symbol)
  (:args (name :scs (descriptor-reg) :to :eval))
  (:temporary (:sc unsigned-reg :from :eval) temp)
  (:results (result :scs (descriptor-reg) :from :argument))
  (:node-var node)
  (:generator 37
    (with-fixed-allocation (result symbol-header-widetag symbol-size node)
      (storew name result symbol-name-slot other-pointer-lowtag)
      (storew unbound-marker-widetag
              result
              symbol-value-slot
              other-pointer-lowtag)
      ;; Set up a random hash value for the symbol. Perhaps the object
      ;; address could be used for even faster and smaller code!
      ;; FIXME: We don't mind the symbol hash not being repeatable, so
      ;; we might as well add in the object address here, too. (Adding entropy
      ;; is good, even if ANSI doesn't understand that.)
      (inst imul temp
            (make-fixup "fast_random_state" :foreign)
            1103515245)
      (inst add temp 12345)
      (inst mov (make-fixup "fast_random_state" :foreign)
            temp)
      ;; We want a positive fixnum for the hash value, so discard the LS bits.
      ;;
      ;; FIXME: OK, who wants to tell me (CSR) why these two
      ;; instructions aren't replaced by (INST AND TEMP #x8FFFFFFC)?
      ;; Are the following two instructions actually faster?  Does the
      ;; difference in behaviour really matter?
      (inst shr temp 1)
      (inst and temp #xfffffffc)
      (storew temp result symbol-hash-slot other-pointer-lowtag)
      (storew nil-value result symbol-plist-slot other-pointer-lowtag)
      (storew nil-value result symbol-package-slot other-pointer-lowtag))))
