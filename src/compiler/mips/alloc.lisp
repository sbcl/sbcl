;;;; allocation VOPs for Mips

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(defun allocation (type size result lowtag temps &key stackp)
  (cond (stackp
         (align-csp (car temps) (cadr temps))
         (inst or result csp-tn lowtag)
         (inst add csp-tn size))
        (t
         (let ((end-addr (car temps))
               (new-freeptr (cadr temps))
               (region-offset (if (eq type 'list)
                                  cons-region-offset
                                  mixed-region-offset)))
           (without-scheduling ()
             (inst lw result null-tn (- region-offset nil-value-offset))
             (inst lw end-addr null-tn (+ 4 (- region-offset nil-value-offset)))
             (inst add new-freeptr result size)
             (inst tltu end-addr new-freeptr
                   (logior (if (eq type 'list) #x100 0) (reg-tn-encoding result)))
             (inst sw new-freeptr null-tn (- region-offset nil-value-offset))
             (unless (= lowtag 0)
               (inst or result lowtag)))))))

;;;; LIST and LIST*
(define-vop (list)
  (:args (things :more t :scs (any-reg descriptor-reg zero null control-stack)))
  (:temporary (:scs (descriptor-reg)) ptr)
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg) :to (:result 0) :target result)
              res)
  (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:info star cons-cells)
  (:results (result :scs (descriptor-reg)))
  (:node-var node)
  (:generator 0
    (macrolet ((store-car (tn list &optional (slot cons-car-slot))
                  `(let ((reg
                          (sc-case ,tn
                            ((any-reg descriptor-reg zero null)
                             ,tn)
                            (control-stack
                             (load-stack-tn temp ,tn)
                             temp))))
                     (storew reg ,list ,slot list-pointer-lowtag))))
      (let ((dx-p (node-stack-allocate-p node))
            (alloc (* (pad-data-block cons-size) cons-cells)))
        (pseudo-atomic (pa-flag :elide-if dx-p)
                 (allocation 'list alloc res list-pointer-lowtag
                             `(,pa-flag ,temp) :stackp dx-p)
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
                 (move result res))))))

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
  (:temporary (:sc non-descriptor-reg) temp)
  (:results (result :scs (descriptor-reg) :from :load))
  (:policy :fast-safe)
  (:generator 100
    (inst addu bytes words (+ lowtag-mask
                              (* vector-data-offset n-word-bytes)))
    (inst srl bytes n-lowtag-bits)
    (inst sll bytes n-lowtag-bits)
    (pseudo-atomic (pa-flag)
      (allocation type bytes result other-pointer-lowtag `(,pa-flag ,temp))
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
  (:vop-var vop)
  (:node-var node)
  (:policy :fast-safe)
  (:generator 100
    (inst addu bytes words (+ lowtag-mask
                              (* vector-data-offset n-word-bytes)))
    (inst srl bytes n-lowtag-bits)
    (inst sll bytes n-lowtag-bits)
    (when (sb-c::make-vector-check-overflow-p node)
      (generate-stack-overflow-check vop bytes temp))
    (pseudo-atomic (pa-flag)      ; FIXME: why pseudo-atomic on stack?
      (align-csp temp pa-flag)
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
      (align-csp temp pa-flag)))) ; why do it again???

(define-vop (make-fdefn)
  (:policy :fast-safe)
  (:translate make-fdefn)
  (:args (name :scs (descriptor-reg) :to :eval))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:results (result :scs (descriptor-reg) :from :argument))
  (:generator 37
    (with-fixed-allocation (result pa-flag temp fdefn-widetag fdefn-size)
      (inst li temp (make-fixup 'undefined-tramp :assembly-routine))
      (storew name result fdefn-name-slot other-pointer-lowtag)
      (storew null-tn result fdefn-fun-slot other-pointer-lowtag)
      (storew temp result fdefn-raw-addr-slot other-pointer-lowtag))))

(define-vop (make-closure)
  (:args (function :to :save :scs (descriptor-reg)))
  (:info label length stack-allocate-p)
  (:ignore label)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (let* ((size (+ length closure-info-offset))
           (alloc-size (pad-data-block size)))
      (pseudo-atomic (pa-flag :elide-if stack-allocate-p)
        (allocation closure-widetag alloc-size result fun-pointer-lowtag
                    `(,pa-flag ,temp) :stackp stack-allocate-p)
        (inst li temp (logior (ash (1- size) n-widetag-bits)
                              closure-widetag))
        (storew temp result 0 fun-pointer-lowtag)
        (storew function result closure-fun-slot fun-pointer-lowtag)))))

;;; The compiler likes to be able to directly make value cells.
(define-vop (make-value-cell)
  (:args (value :to :save :scs (descriptor-reg any-reg null zero)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (with-fixed-allocation (result pa-flag temp value-cell-widetag
                            value-cell-size)
      (storew value result value-cell-value-slot other-pointer-lowtag))))

;;;; Automatic allocators for primitive objects.

(define-vop (make-unbound-marker)
  (:args)
  (:results (result :scs (descriptor-reg any-reg)))
  (:generator 1
    (inst li result unbound-marker-widetag)))

(define-vop (fixed-alloc)
  (:args)
  (:info name words type lowtag stack-allocate-p)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:generator 4
    (pseudo-atomic (pa-flag)
      (cond (stack-allocate-p
             (align-csp result pa-flag)
             (inst or result csp-tn lowtag)
             (inst addu csp-tn (pad-data-block words)))
            (t
             (allocation type (pad-data-block words) result lowtag `(,pa-flag ,temp))))
      (inst li temp (compute-object-header words type))
      (storew temp result 0 lowtag))))

(define-vop (var-alloc)
  (:args (extra :scs (any-reg)))
  (:arg-types positive-fixnum)
  (:info name words type lowtag stack-allocate-p)
  (:ignore name stack-allocate-p)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (any-reg)) bytes)
  (:temporary (:scs (non-descriptor-reg)) header temp)
  (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:generator 6
    (inst addu bytes extra (* (1+ words) n-word-bytes))
    (inst sll header bytes (- (length-field-shift type) n-fixnum-tag-bits))
    ;; The specified EXTRA value is the exact value placed in the header
    ;; as the word count when allocating code.
    (cond ((= type code-header-widetag)
           (inst addu header header type))
          (t
           (inst addu header header (+ (ash -2 (length-field-shift type)) type))
           (inst srl bytes bytes n-lowtag-bits)
           (inst sll bytes bytes n-lowtag-bits)))
    (pseudo-atomic (pa-flag)
      (allocation type bytes result lowtag `(,pa-flag ,temp))
      (storew header result 0 lowtag))))
