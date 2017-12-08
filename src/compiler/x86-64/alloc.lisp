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
                   (stack-allocate-p (node-stack-allocate-p node)))
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

;;; Special variant of 'storew' which might have a shorter encoding
;;; when storing to the heap (which starts out zero-filled).
(defun storew* (word object slot lowtag zeroed)
  (if (or (not zeroed) (not (typep word '(signed-byte 32))))
      (storew word object slot lowtag) ; Possibly use temp-reg-tn
      (inst mov
            (make-ea (cond ((typep word '(unsigned-byte 8)) :byte)
                           ((and (not (logtest word #xff))
                                 (typep (ash word -8) '(unsigned-byte 8)))
                            ;; Array lengths 128 to 16384 which are multiples of 128
                            (setq word (ash word -8))
                            (decf lowtag 1) ; increment address by 1
                            :byte)
                           ((and (not (logtest word #xffff))
                                 (typep (ash word -16) '(unsigned-byte 8)))
                            ;; etc
                            (setq word (ash word -16))
                            (decf lowtag 2) ; increment address by 2
                            :byte)
                           ((typep word '(unsigned-byte 16)) :word)
                           ;; Definitely a (signed-byte 32) due to pre-test.
                           (t :dword))
                     :base object
                     :disp (- (* slot n-word-bytes) lowtag))
            word)))

;; from 'llvm/projects/compiler-rt/lib/msan/msan.h':
;;  "#define MEM_TO_SHADOW(mem) (((uptr)(mem)) ^ 0x500000000000ULL)"
#!+linux ; shadow space differs by OS
(defconstant msan-mem-to-shadow-xor-const #x500000000000)

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
           (put-header (vector-tn type length zeroed)
             `(progn (storew* (if (sc-is ,type immediate) (tn-value ,type) ,type)
                              ,vector-tn 0 other-pointer-lowtag ,zeroed)
                     (storew* (if (sc-is ,length immediate)
                                  (fixnumize (tn-value ,length))
                                  ,length)
                              ,vector-tn vector-length-slot other-pointer-lowtag
                              ,zeroed))))

  (define-vop (allocate-vector-on-heap)
    (:args (type :scs (unsigned-reg immediate))
           (length :scs (any-reg immediate))
           (words :scs (any-reg immediate)))
    ;; Result is live from the beginning, like a temp, because we use it as such
    ;; in 'calc-size-in-bytes'
    (:results (result :scs (descriptor-reg) :from :load))
    (:arg-types positive-fixnum positive-fixnum positive-fixnum)
    (:policy :fast-safe)
    (:generator 100
      ;; The LET generates instructions that needn't be pseudoatomic
      ;; so don't move it inside.
      (let ((size (calc-size-in-bytes words result)))
        (pseudo-atomic
         (allocation result size nil nil other-pointer-lowtag)
         (put-header result type length t)))))

  (define-vop (allocate-vector-on-stack)
    (:args (type :scs (unsigned-reg immediate))
           (length :scs (any-reg immediate))
           (words :scs (any-reg immediate)))
    (:results (result :scs (descriptor-reg) :from :load))
    (:temporary (:sc any-reg :offset ecx-offset :from :eval) rcx)
    (:temporary (:sc any-reg :offset eax-offset :from :eval) rax)
    (:temporary (:sc any-reg :offset edi-offset :from :eval) rdi)
    (:temporary (:sc complex-double-reg) zero)
    (:arg-types positive-fixnum positive-fixnum positive-fixnum)
    (:translate allocate-vector)
    (:policy :fast-safe)
    (:node-var node)
    (:generator 100
      (let ((size (calc-size-in-bytes words result))
            (rax-zeroed))
        (allocation result size node t other-pointer-lowtag)
        (put-header result type length nil)
        ;; FIXME: It would be good to check for stack overflow here.
        ;; It would also be good to skip zero-fill of specialized vectors
        ;; perhaps in a policy-dependent way. At worst you'd see random
        ;; bits, and CLHS says consequences are undefined.
        (when sb!c::*msan-compatible-stack-unpoison*
          ;; Unpoison all DX vectors regardless of widetag.
          ;; Mark the header and length as valid, not just the payload.
          #!+linux ; unimplemented for others
          (let ((words-savep
                 ;; 'words' might be co-located with any of the temps
                 (or (location= words rdi) (location= words rcx) (location= words rax)))
                (rax rax))
            (setq rax-zeroed (not (location= words rax)))
            (when words-savep ; use 'result' to save 'words'
              (inst mov result words))
            (cond ((sc-is words immediate)
                   (inst mov rcx (+ (tn-value words) vector-data-offset)))
                  (t
                   (inst lea rcx
                         (make-ea :qword :base words
                                  :disp (ash vector-data-offset n-fixnum-tag-bits)))
                   (if (= n-fixnum-tag-bits 1)
                       (setq rax (reg-in-size rax :dword)) ; don't bother shifting rcx
                       (inst shr rcx n-fixnum-tag-bits))))
            (inst mov rdi msan-mem-to-shadow-xor-const)
            (inst xor rdi rsp-tn) ; compute shadow address
            (zeroize rax)
            (inst rep)
            (inst stos rax)
            (when words-savep
              (inst mov words result) ; restore 'words'
              (inst lea result ; recompute the tagged pointer
                    (make-ea :byte :base rsp-tn :disp other-pointer-lowtag)))))
        (let ((data-addr
                (make-ea :qword :base result
                                :disp (- (* vector-data-offset n-word-bytes)
                                         other-pointer-lowtag))))
          (block zero-fill
            (cond ((sc-is words immediate)
                   (let ((n (tn-value words)))
                     (cond ((> n 8)
                            (inst mov rcx (tn-value words)))
                           ((= n 1)
                            (inst mov data-addr 0)
                            (return-from zero-fill))
                           (t
                            (multiple-value-bind (double single) (truncate n 2)
                              (inst xorpd zero zero)
                              (dotimes (i double)
                                (inst movapd data-addr zero)
                                (setf data-addr (copy-structure data-addr))
                                (incf (ea-disp data-addr) (* n-word-bytes 2)))
                              (unless (zerop single)
                                (inst movaps data-addr zero))
                              (return-from zero-fill))))))
                  (t
                   (move rcx words)
                   (inst shr rcx n-fixnum-tag-bits)))
            (inst lea rdi data-addr)
            (unless rax-zeroed (zeroize rax))
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

#!-immobile-space
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
      (storew (make-fixup 'undefined-tramp :assembly-routine)
              result fdefn-raw-addr-slot other-pointer-lowtag))))

(define-vop (make-closure)
  ; (:args (function :to :save :scs (descriptor-reg)))
  (:info label length stack-allocate-p)
  (:temporary (:sc any-reg) temp)
  (:results (result :scs (descriptor-reg)))
  (:node-var node)
  (:generator 10
   (maybe-pseudo-atomic stack-allocate-p
    (let* ((size (+ length closure-info-offset))
           (header (logior (ash (1- size) n-widetag-bits) closure-widetag)))
      (allocation result (pad-data-block size) node stack-allocate-p
                  fun-pointer-lowtag)
      (storew* #!-immobile-space header ; write the widetag and size
               #!+immobile-space        ; ... plus the layout pointer
               (progn (inst mov temp header)
                      (inst or temp (static-symbol-value-ea 'function-layout))
                      temp)
               result 0 fun-pointer-lowtag (not stack-allocate-p)))
    ;; These two instructions are within the scope of PSEUDO-ATOMIC.
    ;; This is due to scav_closure() assuming that it can always subtract
    ;; FUN_RAW_ADDR_OFFSET from closure->fun to obtain a Lisp object,
    ;; without any precheck for whether that word is currently 0.
    (inst lea temp (make-fixup nil :closure label))
    (storew temp result closure-fun-slot fun-pointer-lowtag))))

;;; The compiler likes to be able to directly make value cells.
(define-vop (make-value-cell)
  (:args (value :scs (descriptor-reg any-reg) :to :result))
  (:results (result :scs (descriptor-reg) :from :eval))
  (:info stack-allocate-p)
  (:node-var node)
  (:generator 10
    (with-fixed-allocation
        (result value-cell-widetag value-cell-size node stack-allocate-p)
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
    (let ((tramp (make-fixup 'funcallable-instance-tramp :assembly-routine)))
      (cond #!+immobile-code
            (sb!c::*code-is-immobile*
             (inst lea result tramp))
            (t
             (inst mov result tramp))))))

(define-vop (fixed-alloc)
  (:args)
  (:info name words type lowtag stack-allocate-p)
  (:results (result :scs (descriptor-reg)))
  (:node-var node)
  (:generator 50
    (progn name) ; possibly not used
    (maybe-pseudo-atomic stack-allocate-p
     (allocation result (pad-data-block words) node stack-allocate-p lowtag)
     (when type
       (let* ((widetag (if (typep type 'layout) instance-widetag type))
              (header (logior (ash (1- words) n-widetag-bits) widetag)))
         (if (or #!+compact-instance-header
                 (and (eq name '%make-structure-instance) stack-allocate-p))
             ;; Write a :DWORD, not a :QWORD, because the high half will be
             ;; filled in when the layout is stored. Can't use STOREW* though,
             ;; because it tries to store as few bytes as possible,
             ;; where this instruction must write exactly 4 bytes.
             (inst mov (make-ea :dword :base result :disp (- lowtag)) header)
             (storew* header result 0 lowtag (not stack-allocate-p)))
         (unless (eq type widetag) ; TYPE is actually a LAYOUT
           (inst mov (make-ea :dword :base result :disp (+ 4 (- lowtag)))
                 ;; XXX: should layout fixups use a name, not a layout object?
                 (make-fixup type :layout))))))))

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

#!+immobile-space
(define-vop (alloc-fixedobj)
  (:info lowtag size word0 word1)
  (:temporary (:sc unsigned-reg :to :eval :offset rdi-offset) c-arg1)
  (:temporary (:sc unsigned-reg :to :eval :offset rsi-offset) c-arg2)
  (:temporary (:sc unsigned-reg :from :eval :to (:result 0) :offset rax-offset) c-result)
  (:results (result :scs (descriptor-reg)))
  (:generator 50
   (inst mov c-arg1 size)
   (inst mov c-arg2 word0)
   ;; RSP needn't be restored because the allocators all return immediately
   ;; which has that effect
   (inst and rsp-tn -16)
   (inst mov temp-reg-tn (make-fixup "alloc_fixedobj" :foreign))
   (pseudo-atomic
     (inst call temp-reg-tn)
     (inst lea result (make-ea :qword :base c-result :disp lowtag))
     ;; If code, the next word must be set within the P-A
     ;; otherwise the GC would compute the wrong object size.
     (when word1
       (inst mov (make-ea :qword :base result :disp (- n-word-bytes lowtag)) word1)))))

#!+immobile-space
(macrolet ((def (lisp-name c-name arg-scs &body stuff
                           &aux (argc (length arg-scs)))
             `(define-vop (,lisp-name)
                (:args ,@(if (>= argc 1) `((arg1 :scs ,(first arg-scs) :target c-arg1)))
                       ,@(if (>= argc 2) `((arg2 :scs ,(second arg-scs) :target c-arg2))))
                ,@(if (>= argc 1)
                      '((:temporary (:sc unsigned-reg :from (:argument 0)
                                     :to :eval :offset rdi-offset) c-arg1)))
                ,@(if (>= argc 2)
                      '((:temporary (:sc unsigned-reg :from (:argument 1)
                                     :to :eval :offset rsi-offset) c-arg2)))
                (:temporary (:sc unsigned-reg :from :eval :to (:result 0)
                             :offset rax-offset) c-result)
                (:results (result :scs (descriptor-reg)))
                (:generator 50
                 (pseudo-atomic
                  ,@(if (>= argc 1) '((move c-arg1 arg1)))
                  ,@(if (>= argc 2) '((move c-arg2 arg2)))
                  (inst and rsp-tn -16)
                  (inst mov temp-reg-tn (make-fixup ,c-name :foreign))
                  (inst call temp-reg-tn)
                  ,@stuff
                  (move result c-result))))))
  (def alloc-immobile-layout "alloc_layout" ; MAKE-LAYOUT
       ((descriptor-reg))))

