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
(macrolet ((calc-size-in-bytes (n-words result-tn)
             `(cond ((sc-is ,n-words immediate)
                     (pad-data-block (+ (tn-value ,n-words) vector-data-offset)))
                    (t
                     (inst lea ,result-tn
                           (make-ea :byte :index ,n-words
                                          :scale (ash 1 (- word-shift n-fixnum-tag-bits))
                                          :disp (+ lowtag-mask
                                                   (* vector-data-offset n-word-bytes))))
                     (inst and ,result-tn (lognot lowtag-mask))
                     ,result-tn)))
           (put-header (vector-tn type length)
             `(progn (storew (if (sc-is ,type immediate) (tn-value ,type) ,type)
                             ,vector-tn 0 other-pointer-lowtag)
                     (storew (if (sc-is ,length immediate)
                                 (fixnumize (tn-value ,length))
                                 ,length)
                             ,vector-tn vector-length-slot other-pointer-lowtag))))

  (define-vop (allocate-vector-on-heap)
    (:args (type :scs (unsigned-reg immediate))
           (length :scs (any-reg immediate))
           (words :scs (any-reg immediate)))
    (:results (result :scs (descriptor-reg) :from :load))
    (:arg-types positive-fixnum positive-fixnum positive-fixnum)
    (:policy :fast-safe)
    (:generator 100
      ;; The LET generates instructions that needn't be pseudoatomic
      ;; so don't move it inside.
      (let ((size (calc-size-in-bytes words result)))
        (pseudo-atomic
         (allocation result size nil nil other-pointer-lowtag)
         (put-header result type length)))))

  (define-vop (allocate-vector-on-stack)
    (:args (type :scs (unsigned-reg immediate) :to :save)
           (length :scs (any-reg immediate) :to :eval :target rax)
           (words :scs (any-reg immediate) :target rcx))
    (:temporary (:sc any-reg :offset ecx-offset :from (:argument 2)) rcx)
    (:temporary (:sc any-reg :offset eax-offset :from :eval) rax)
    (:temporary (:sc any-reg :offset edi-offset) rdi)
    (:results (result :scs (descriptor-reg) :from :load))
    (:arg-types positive-fixnum positive-fixnum positive-fixnum)
    (:translate allocate-vector)
    (:policy :fast-safe)
    (:node-var node)
    (:generator 100
      (let ((size (calc-size-in-bytes words result)))
        (allocation result size node t other-pointer-lowtag)
        (put-header result type length)
    ;; FIXME: It would be good to check for stack overflow here.
    ;; It would also be good to skip zero-fill of specialized vectors
    ;; perhaps in a policy-dependent way. At worst you'd see random
    ;; bits, and CLHS says consequences are undefined.
        (let ((data-addr
               (make-ea :qword :base result
                               :disp (- (* vector-data-offset n-word-bytes)
                                        other-pointer-lowtag))))
          (block zero-fill
            (if (sc-is words immediate)
                (let ((n (tn-value words)))
                  (if (> n 6)
                      (inst mov rcx (tn-value words))
                      (let ((zero (if (<= n 2) ; do imm-to-mem moves
                                      0
                                      (progn (zeroize rax) rax))))
                        (dotimes (i n (return-from zero-fill))
                          (inst mov data-addr zero)
                          (setq data-addr (copy-structure data-addr))
                          (incf (ea-disp data-addr) n-word-bytes)))))
                (progn (move rcx words)
                       (inst shr rcx n-fixnum-tag-bits)))
            (inst lea rdi data-addr)
            (inst cld)
            (zeroize rax)
            (inst rep)
            (inst stos rax)))))))

;;; ALLOCATE-LIST
(macrolet ((calc-size-in-bytes (length answer)
             `(cond ((sc-is ,length immediate)
                     (aver (/= (tn-value ,length) 0))
                     (* (tn-value ,length) n-word-bytes 2))
                    (t
                     (inst mov result nil-value)
                     (inst test ,length ,length)
                     (inst jmp :z done)
                     (inst lea ,answer
                           (make-ea :byte :base nil :index ,length
                                    :scale (ash 1 (1+ (- word-shift
                                                         n-fixnum-tag-bits)))))
                     ,answer)))
           (compute-end ()
             `(let ((size (cond ((or (not (fixnump size))
                                     (immediate32-p size))
                                 size)
                                (t
                                 (inst mov limit size)
                                 limit))))
                (inst lea limit
                      (make-ea :qword :base result
                                      :index (if (fixnump size) nil size)
                                      :disp (if (fixnump size) size 0))))))

  (define-vop (allocate-list-on-stack)
    (:args (length :scs (any-reg immediate))
           (element :scs (any-reg descriptor-reg)))
    (:results (result :scs (descriptor-reg) :from :load))
    (:arg-types positive-fixnum *)
    (:policy :fast-safe)
    (:node-var node)
    (:temporary (:sc descriptor-reg) tail next limit)
    (:node-var node)
    (:generator 20
      (let ((size (calc-size-in-bytes length next))
            (loop (gen-label)))
        (allocation result size node t list-pointer-lowtag)
        (compute-end)
        (inst mov next result)
        (emit-label LOOP)
        (inst mov tail next)
        (inst add next (* 2 n-word-bytes))
        (storew element tail cons-car-slot list-pointer-lowtag)
        ;; Store the CDR even if it will be smashed to nil.
        (storew next tail cons-cdr-slot list-pointer-lowtag)
        (inst cmp next limit)
        (inst jmp :ne loop)
        (storew nil-value tail cons-cdr-slot list-pointer-lowtag))
      done))

  (define-vop (allocate-list-on-heap)
    (:args (length :scs (any-reg immediate))
           (element :scs (any-reg descriptor-reg)
                    :load-if (not (and (sc-is element immediate)
                                       (eql (tn-value element) 0)))))
    (:results (result :scs (descriptor-reg) :from :load))
    (:arg-types positive-fixnum *)
    (:policy :fast-safe)
    (:node-var node)
    (:temporary (:sc descriptor-reg) tail next limit)
    (:generator 20
      (let ((size (calc-size-in-bytes length next))
            (entry (gen-label))
            (loop (gen-label))
            (no-init
             (and (sc-is element immediate) (eql (tn-value element) 0))))
        (pseudo-atomic
         (allocation result size node nil list-pointer-lowtag)
         (compute-end)
         (inst mov next result)
         (inst jmp entry)
         (emit-label LOOP)
         (storew next tail cons-cdr-slot list-pointer-lowtag)
         (emit-label ENTRY)
         (inst mov tail next)
         (inst add next (* 2 n-word-bytes))
         (unless no-init ; don't bother writing zeros in the CARs
           (storew element tail cons-car-slot list-pointer-lowtag))
         (inst cmp next limit)
         (inst jmp :ne loop))
        (storew nil-value tail cons-cdr-slot list-pointer-lowtag))
      done)))

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
  (:results (result :scs (descriptor-reg any-reg)))
  (:generator 1
    (inst mov result unbound-marker-widetag)))

(define-vop (make-funcallable-instance-tramp)
  (:args)
  (:results (result :scs (any-reg)))
  (:generator 1
    (inst mov result (make-fixup "funcallable_instance_tramp" :foreign))))

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
          (make-ea :qword :disp (* (1+ words) n-word-bytes) :index extra
                   :scale (ash 1 (- word-shift n-fixnum-tag-bits))))
    (inst mov header bytes)
    (inst shl header (- n-widetag-bits word-shift)) ; w+1 to length field
    (inst lea header                    ; (w-1 << 8) | type
          (make-ea :qword :base header
                   :disp (+ (ash -2 n-widetag-bits) type)))
    (inst and bytes (lognot lowtag-mask))
    (pseudo-atomic
     (allocation result bytes node)
     (inst lea result (make-ea :byte :base result :disp lowtag))
     (storew header result 0 lowtag))))
