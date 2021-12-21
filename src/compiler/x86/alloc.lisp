;;;; allocation VOPs for the x86

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;;; allocation helpers

;;; Allocation within alloc_region (which is thread local) can be done
;;; inline.  If the alloc_region is overflown allocation is done by
;;; calling the C alloc() function.

;;; C calls for allocation don't /seem/ to make an awful lot of
;;; difference to speed. On pure consing it's about a 25%
;;; gain. Guessing from historical context, it looks like inline
;;; allocation was introduced before pseudo-atomic, at which time all
;;; calls to alloc() would have needed a syscall to mask signals for
;;; the duration.  Now we have pseudoatomic there's no need for that
;;; overhead.

(defun stack-allocation (alloc-tn size lowtag)
  (aver (not (location= alloc-tn esp-tn)))
  (inst sub esp-tn size)
  ;; FIXME: SIZE _should_ be double-word aligned (suggested but
  ;; unfortunately not enforced by PAD-DATA-BLOCK and
  ;; FIXED-ALLOC), so that ESP is always divisible by 8 (for
  ;; 32-bit lispobjs).  In that case, this AND instruction is
  ;; unnecessary and could be removed.  If not, explain why.  -- CSR,
  ;; 2004-03-30
  (inst and esp-tn (lognot lowtag-mask))
  (inst lea alloc-tn (make-ea :byte :base esp-tn :disp lowtag))
  (values))

(defun allocation-notinline (type alloc-tn size)
  (let* ((alloc-tn-offset (tn-offset alloc-tn))
         ;; C call to allocate via dispatch routines. Each
         ;; destination has a special entry point. The size may be a
         ;; register or a constant.
         (tn-name (ecase alloc-tn-offset
                    (#.eax-offset 'eax)
                    (#.ecx-offset 'ecx)
                    (#.edx-offset 'edx)
                    (#.ebx-offset 'ebx)
                    (#.esi-offset 'esi)
                    (#.edi-offset 'edi)))
         (size-text
           (case size
             (8  "8-")
             (16 "16-")
             (t  (if (eq type 'list) "LIST-" "")))))
    (unless (or (eql size 8) (eql size 16))
      (unless (and (tn-p size) (location= alloc-tn size))
        (inst mov alloc-tn size)))
    (let ((routine (package-symbolicate (sb-xc:symbol-package tn-name)
                                        "ALLOC-" size-text "TO-" tn-name)))
      (inst call (make-fixup routine :assembly-routine)))))

(defun allocation-inline (type alloc-tn size)
  (let* ((ok (gen-label))
         (done (gen-label))
         #+(and sb-thread win32)
         (scratch-tns (loop for my-tn in `(,eax-tn ,ebx-tn ,edx-tn ,ecx-tn)
                            when (and (not (location= alloc-tn my-tn))
                                      (or (not (tn-p size))
                                          (not (location= size my-tn))))
                            collect my-tn))
         (tls-prefix #+sb-thread :fs)
         #+(and sb-thread win32) (scratch-tn (pop scratch-tns))
         #+(and sb-thread win32) (swap-tn (pop scratch-tns))
         (free-pointer
           ;; thread->mixed_tlab.free_pointer
           (make-ea :dword
                    :base (or #+(and sb-thread win32)
                              scratch-tn)
                    :disp
                    #+sb-thread (* n-word-bytes thread-mixed-tlab-slot)
                    #-sb-thread mixed-region))
         (end-addr
            ;; thread->mixed_tlab.end_addr
           (make-ea :dword
                    :base (or #+(and sb-thread win32)
                              scratch-tn)
                    :disp
                    #+sb-thread (* n-word-bytes (1+ thread-mixed-tlab-slot))
                    #-sb-thread (+ mixed-region n-word-bytes))))
    (unless (and (tn-p size) (location= alloc-tn size))
      (inst mov alloc-tn size))
    #+(and sb-thread win32)
    (progn
      (inst push scratch-tn)
      (inst push swap-tn)
      (inst mov scratch-tn
            (make-ea :dword :disp
                     +win32-tib-arbitrary-field-offset+) tls-prefix)
      (setf tls-prefix nil))
    (inst add alloc-tn free-pointer tls-prefix)
    (inst cmp alloc-tn end-addr tls-prefix)
    (inst jmp :be ok)
    (let ((dst (if (eq type 'list)
                   (ecase (tn-offset alloc-tn)
                     (#.eax-offset 'alloc-list-overflow-eax)
                     (#.ecx-offset 'alloc-list-overflow-ecx)
                     (#.edx-offset 'alloc-list-overflow-edx)
                     (#.ebx-offset 'alloc-list-overflow-ebx)
                     (#.esi-offset 'alloc-list-overflow-esi)
                     (#.edi-offset 'alloc-list-overflow-edi))
                   (ecase (tn-offset alloc-tn)
                     (#.eax-offset 'alloc-overflow-eax)
                     (#.ecx-offset 'alloc-overflow-ecx)
                     (#.edx-offset 'alloc-overflow-edx)
                     (#.ebx-offset 'alloc-overflow-ebx)
                     (#.esi-offset 'alloc-overflow-esi)
                     (#.edi-offset 'alloc-overflow-edi)))))
      (inst call (make-fixup dst :assembly-routine)))
    (inst jmp done)
    (emit-label ok)
    ;; Swap ALLOC-TN and FREE-POINTER
    (cond ((and (tn-p size) (location= alloc-tn size))
           ;; XCHG is extremely slow, use the xor swap trick
           #-(and sb-thread win32)
           (progn
             (inst xor alloc-tn free-pointer tls-prefix)
             (inst xor free-pointer alloc-tn tls-prefix)
             (inst xor alloc-tn free-pointer tls-prefix))
           #+(and sb-thread win32)
           (progn
             (inst mov swap-tn free-pointer tls-prefix)
             (inst mov free-pointer alloc-tn tls-prefix)
             (inst mov alloc-tn swap-tn)))
          (t
           ;; It's easier if SIZE is still available.
           (inst mov free-pointer alloc-tn tls-prefix)
           (inst sub alloc-tn size)))
    (emit-label done)
    #+(and sb-thread win32)
    (progn
      (inst pop swap-tn)
      (inst pop scratch-tn))
    (values)))

;;; Emit code to allocate an object with a size in bytes given by
;;; SIZE.  The size may be an integer or a TN. If Inline is a VOP
;;; node-var then it is used to make an appropriate speed vs size
;;; decision.

;;; Allocation should only be used inside a pseudo-atomic section, which
;;; should also cover subsequent initialization of the object.

;;; (FIXME: so why aren't we asserting this?)

;;; A mnemonic device for the argument pattern here:
;;; 1. what to allocate: type, size, lowtag describe the object
;;; 2. how to allocate it: policy and how to invoke the trampoline
;;; 3. where to put the result
(defun allocation (type size lowtag node dynamic-extent alloc-tn)
  (declare (ignorable node))
  (cond
    (dynamic-extent
     (stack-allocation alloc-tn size lowtag))
    ((or (null node) (policy node (>= speed space)))
     (allocation-inline type alloc-tn size))
    (t
     (allocation-notinline type alloc-tn size)))
  (when (and lowtag (/= lowtag 0) (not dynamic-extent))
    ;; This is dumb, it should be an ADD or an OR, but a better solution
    ;; would be to pass lowtag into the allocation-inline function so that
    ;; for a fixed size we don't emit code such as "SUB r, 8 ; ADD r, 3".
    (inst lea alloc-tn (make-ea :byte :base alloc-tn :disp lowtag)))
  (values))

;;; Allocate an other-pointer object of fixed SIZE with a single word
;;; header having the specified WIDETAG value. The result is placed in
;;; RESULT-TN.
(defun alloc-other (result-tn widetag size node &optional stack-allocate-p)
  (pseudo-atomic (:elide-if stack-allocate-p)
      (allocation nil (* (pad-data-block size)
                         #+bignum-assertions (if (eql widetag bignum-widetag) 2 1))
                  other-pointer-lowtag
                  node stack-allocate-p result-tn)
      (storew (compute-object-header size widetag)
              result-tn 0 other-pointer-lowtag)))

;;;; CONS, LIST and LIST*
(define-vop (list)
  (:args (things :more t))
  (:temporary (:sc unsigned-reg) ptr temp)
  (:temporary (:sc unsigned-reg :to (:result 0) :target result) res)
  (:info star cons-cells)
  (:results (result :scs (descriptor-reg)))
  (:node-var node)
  (:generator 0
    (macrolet ((store-car (tn list &optional (slot cons-car-slot))
                  `(let ((reg
                          (sc-case ,tn
                            ((any-reg descriptor-reg) ,tn)
                            ((control-stack)
                             (move temp ,tn)
                             temp))))
                     (storew reg ,list ,slot list-pointer-lowtag))))
      (let ((stack-allocate-p (node-stack-allocate-p node)))
        (pseudo-atomic (:elide-if stack-allocate-p)
                (allocation 'list (* (pad-data-block cons-size) cons-cells)
                            list-pointer-lowtag node stack-allocate-p res)
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
      (move result res))))

;;;; special-purpose inline allocators

;;; ALLOCATE-VECTOR
(define-vop (allocate-vector-on-heap)
  (:args (type :scs (unsigned-reg immediate))
         (length :scs (any-reg immediate))
         (words :scs (any-reg immediate)))
  (:results (result :scs (descriptor-reg) :from :load))
  (:arg-types positive-fixnum positive-fixnum positive-fixnum)
  (:policy :fast-safe)
  (:node-var node)
  (:generator 100
   (flet ((store-widetag (value ptr slot lowtag)
             (inst mov (object-slot-ea
                        ptr slot lowtag
                        (typecase value
                          ((unsigned-byte 8) :byte)
                          ((unsigned-byte 16) :word)
                          (t :dword)))
                   value)))
    (let ((size (sc-case words
                  (immediate
                   (logandc2 (+ (fixnumize (tn-value words))
                                (+ (1- (ash 1 n-lowtag-bits))
                                   (* vector-data-offset n-word-bytes)))
                             lowtag-mask))
                  (t
                   (inst lea result (make-ea :byte :base words :disp
                                             (+ (1- (ash 1 n-lowtag-bits))
                                                (* vector-data-offset
                                                   n-word-bytes))))
                   (inst and result (lognot lowtag-mask))
                   result))))
      (pseudo-atomic ()
       (allocation nil size other-pointer-lowtag node nil result)
       (sc-case type
         (immediate
          (store-widetag (tn-value type) result 0 other-pointer-lowtag))
         (t
          (storew type result 0 other-pointer-lowtag)))
       (sc-case length
         (immediate
          (let ((fixnum-length (fixnumize (tn-value length))))
            (typecase fixnum-length
              ((unsigned-byte 8)
               (store-widetag fixnum-length result
                       vector-length-slot other-pointer-lowtag))
              (t
               (storew fixnum-length result
                       vector-length-slot other-pointer-lowtag)))))
         (t
          (storew length result vector-length-slot other-pointer-lowtag))))))))

(define-vop (allocate-vector-on-stack)
  (:args (type :scs (unsigned-reg immediate) :to :save)
         (length :scs (any-reg) :to :eval :target zero)
         (words :scs (any-reg) :target ecx))
  (:temporary (:sc any-reg :offset ecx-offset :from (:argument 2)) ecx)
  (:temporary (:sc any-reg :offset eax-offset :from :eval) zero)
  (:temporary (:sc any-reg :offset edi-offset) res)
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
    ;; FIXME: It would be good to check for stack overflow here.
    (move ecx words)
    (inst shr ecx n-fixnum-tag-bits)
    (stack-allocation result result other-pointer-lowtag)
    (inst cld)
    (inst lea res
          (make-ea :byte :base result :disp (- (* vector-data-offset n-word-bytes)
                                               other-pointer-lowtag)))
    (sc-case type
      (immediate
       (aver (typep (tn-value type) '(unsigned-byte 8)))
       (storew (tn-value type) result 0 other-pointer-lowtag))
      (t
       (storew type result 0 other-pointer-lowtag)))
    (storew length result vector-length-slot other-pointer-lowtag)
    (inst xor zero zero)
    (inst rep)
    (inst stos zero)))


(define-vop (make-fdefn)
  (:policy :fast-safe)
  (:translate make-fdefn)
  (:args (name :scs (descriptor-reg) :to :eval))
  (:results (result :scs (descriptor-reg) :from :argument))
  (:node-var node)
  (:generator 37
    (alloc-other result fdefn-widetag fdefn-size node)
    (storew name result fdefn-name-slot other-pointer-lowtag)
    (storew nil-value result fdefn-fun-slot other-pointer-lowtag)
    (storew (make-fixup 'undefined-tramp :assembly-routine)
            result fdefn-raw-addr-slot other-pointer-lowtag)))

(define-vop (make-closure)
  (:args (function :to :save :scs (descriptor-reg)))
  (:info label length stack-allocate-p)
  (:ignore label)
  (:temporary (:sc any-reg) temp)
  (:results (result :scs (descriptor-reg)))
  (:node-var node)
  (:generator 10
   (pseudo-atomic (:elide-if stack-allocate-p)
     (let ((size (+ length closure-info-offset)))
       (allocation nil (pad-data-block size) fun-pointer-lowtag
                   node stack-allocate-p result)
       (storew (logior (ash (1- size) n-widetag-bits) closure-widetag)
               result 0 fun-pointer-lowtag)))
   ;; Done with pseudo-atomic
   (inst lea temp (object-slot-ea function simple-fun-insts-offset
                                           fun-pointer-lowtag))
   (storew temp result closure-fun-slot fun-pointer-lowtag)))

;;; The compiler likes to be able to directly make value cells.
(define-vop (make-value-cell)
  (:args (value :scs (descriptor-reg any-reg) :to :result))
  (:results (result :scs (descriptor-reg) :from :eval))
  (:info stack-allocate-p)
  (:node-var node)
  (:generator 10
    (alloc-other result value-cell-widetag value-cell-size node stack-allocate-p)
    (storew value result value-cell-value-slot other-pointer-lowtag)))

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
    (inst mov result (make-fixup 'funcallable-instance-tramp :assembly-routine))))

(define-vop (fixed-alloc)
  (:info name words type lowtag stack-allocate-p)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:node-var node)
  (:generator 50
    (pseudo-atomic (:elide-if stack-allocate-p)
      (let ((nbytes (* (pad-data-block words)
                       #+bignum-assertions (if (eql type bignum-widetag) 2 1))))
        (allocation nil nbytes lowtag node stack-allocate-p result))
      (storew (compute-object-header words type) result 0 lowtag))))

(define-vop (var-alloc)
  (:args (extra :scs (any-reg)))
  (:arg-types positive-fixnum)
  (:info name words type lowtag stack-allocate-p)
  (:ignore name)
  (:results (result :scs (descriptor-reg) :from (:eval 1)))
  (:temporary (:sc any-reg :from :eval :to (:eval 1)) bytes)
  (:temporary (:sc any-reg :from :eval :to :result) header)
  (:node-var node)
  (:generator 50
    (inst lea bytes
          (make-ea :dword :base extra :disp (* (1+ words) n-word-bytes)))
    (inst mov header bytes)
    (inst shl header (- (length-field-shift type) 2)) ; w+1 to length field
    (inst lea header                    ; (w-1 << 8) | type
          (make-ea :dword :base header
                          :disp (+ (ash -2 (length-field-shift type)) type)))
    (inst and bytes (lognot lowtag-mask))
    #+bignum-assertions (when (eql type bignum-widetag) (inst shl bytes 1)) ; use 2x space
    (pseudo-atomic (:elide-if stack-allocate-p)
     (allocation nil bytes lowtag node stack-allocate-p result)
     (storew header result 0 lowtag))))
