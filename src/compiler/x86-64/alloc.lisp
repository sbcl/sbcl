;;;; allocation VOPs for the x86-64

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

;;; Most allocation is done by inline code with sometimes help
;;; from the C alloc() function by way of the alloc-tramp
;;; assembly routine.

(defun tagify (result base lowtag)
  (if (eql lowtag 0)
      (inst mov result base)
      (inst lea result (ea lowtag base))))

(defun stack-allocation (size lowtag alloc-tn &optional known-alignedp)
  (aver (not (location= alloc-tn rsp-tn)))
  (inst sub rsp-tn size)
  ;; see comment in x86/macros.lisp implementation of this
  ;; However that comment seems inapplicable here because:
  ;; - PAD-DATA-BLOCK quite clearly enforces double-word alignment,
  ;;   contradicting "... unfortunately not enforced by ..."
  ;; - It's not the job of FIXED-ALLOC to realign anything.
  ;; - The real issue is that it's not obvious that the stack is
  ;;   16-byte-aligned at *all* times. Maybe it is, maybe it isn't.
  (unless known-alignedp ; can skip this AND if we're all good
    (inst and rsp-tn #.(lognot lowtag-mask)))
  (tagify alloc-tn rsp-tn lowtag)
  (values))

#+nil ; was #+avx2
(defun avx-registers-used-p ()
  (or (when (and #+sb-xc-host (boundp '*component-being-compiled*))
        (let ((comp (component-info *component-being-compiled*)))
          (or (sb-c::ir2-component-avx2-used-p comp)
              (flet ((used-p (tn)
                       (do ((tn tn (sb-c::tn-next tn)))
                           ((null tn))
                         (when (sc-is tn ymm-reg
                                      int-avx2-reg
                                      double-avx2-reg single-avx2-reg)
                           (return-from avx-registers-used-p
                             (setf (sb-c::ir2-component-avx2-used-p comp) t))))))
                (used-p (sb-c::ir2-component-normal-tns comp))
                (used-p (sb-c::ir2-component-wired-tns comp))))))))

(defun alloc-unboxed-p (type)
  (case type
    ((unboxed-array
      #.bignum-widetag
      #.sap-widetag
      #.double-float-widetag
      #.complex-single-float-widetag
      #.complex-double-float-widetag)
     t)))

;;; Insert allocation profiler instrumentation
(eval-when (:compile-toplevel)
  (aver (= thread-tot-bytes-alloc-unboxed-slot
           (1+ thread-tot-bytes-alloc-boxed-slot))))

;;; the #+allocator metrics histogram contains an exact count
;;; for all sizes up to (* cons-size n-word-bytes histogram-small-bins).
;;; Larger allocations are grouped by the binary log of the size.
;;; It seems that 99.5% of all allocations are less than the small bucket limit,
;;; making the histogram fairly exact except for the tail.
(defparameter *consing-histo* nil)
(defconstant non-small-bucket-offset
  (+ histogram-small-bins
     (- (integer-length (* sb-vm::histogram-small-bins
                           sb-vm:cons-size sb-vm:n-word-bytes)))))

;;; Emit counter increments for SB-APROF. SCRATCH-REGISTERS is either a TN
;;; or list of TNs that can be used to store into the profiling data.
;;; We pick one of the available TNs to use for addressing the data buffer.
;;; The TN that we pick can't be R12 because encoding it into an instruction
;;; always requires a SIB byte, which doesn't fit in the reserved bytes
;;; of the instruction stream where hot patching occurs.
(defun instrument-alloc (type size node scratch-registers
                         &optional thread-temp
                         &aux (temp
                               (if (listp scratch-registers)
                                   (dolist (reg scratch-registers
                                                (first scratch-registers))
                                     (unless (location= reg r12-tn) (return reg)))
                                   scratch-registers)))
  (declare (ignorable type thread-temp))
  (aver (not (location= temp r12-tn)))
  ;; Each allocation sequence has to call INSTRUMENT-ALLOC,
  ;; so we may as well take advantage of this fact to load the temp reg
  ;; here, if provided, rather than spewing more #+gs-seg tests around.
  #+gs-seg (when thread-temp (inst rdgsbase thread-temp))
  #+allocator-metrics
  (let ((use-size-temp (not (typep size '(or (signed-byte 32) tn))))
        (tally (gen-label))
        (inexact (gen-label)))
    (cond ((tn-p type) ; from ALLOCATE-VECTOR-ON-HEAP
           ;; Constant huge size + unknown type can't occur.
           (aver (not use-size-temp))
           (inst cmp :byte type simple-vector-widetag)
           (inst set :ne temp)
           (inst and :dword temp 1)
           (inst add :qword
                 (ea thread-segment-reg
                     (ash thread-tot-bytes-alloc-boxed-slot word-shift)
                     thread-tn temp 8)
                 size))
          (t
           (inst add :qword
                 (thread-slot-ea (if (alloc-unboxed-p type)
                                     thread-tot-bytes-alloc-unboxed-slot
                                     thread-tot-bytes-alloc-boxed-slot))
                 (cond (use-size-temp (inst mov temp size) temp)
                       (t size)))))
    (cond ((tn-p size)
           (inst cmp size (* histogram-small-bins 16))
           (inst jmp :g inexact)
           (inst mov :dword temp size)
           (inst shr :dword temp (1+ word-shift))
           (inst dec :dword temp)
           (inst jmp tally)
           (emit-label inexact)
           (inst bsr temp size)
           ;; bsr returns 1 less than INTEGER-LENGTH
           (inst add :dword temp (1+ non-small-bucket-offset))
           (emit-label tally)
           (inst inc :qword (ea thread-segment-reg
                                (ash thread-obj-size-histo-slot word-shift)
                                thread-tn temp 8)))
          (t
           (let* ((n-conses (/ size (* sb-vm:cons-size sb-vm:n-word-bytes)))
                  (bucket (if (<= n-conses histogram-small-bins)
                              (1- n-conses)
                              (+ (integer-length size)
                                 non-small-bucket-offset))))
             (inst inc :qword
                   (thread-slot-ea (+ thread-obj-size-histo-slot bucket)))))))
  (when (policy node (> sb-c::instrument-consing 1))
    (when (tn-p size)
      (aver (not (location= size temp))))
    (let ((data temp)
          (patch-loc (gen-label))
          (skip-instrumentation (gen-label)))
      (inst mov data (thread-slot-ea thread-profile-data-slot thread-temp))
      (inst test data data)
      ;; This instruction is modified to "JMP :z" when profiling is
      ;; partially enabled. After the buffer is assigned, it becomes
      ;; fully enabled. The unconditional jmp gives minimal performance
      ;; loss if the profiler is statically disabled. (one memory
      ;; read and a test whose result is never used, which the CPU
      ;; is good at ignoring as far as instruction prefetch goes)
      (emit-label patch-loc)
      (push patch-loc (sb-assem::asmstream-alloc-points sb-assem:*asmstream*))
      (inst jmp skip-instrumentation)
      (emit-alignment 3 :long-nop)
      (let ((helper (if (integerp size)
                        'enable-alloc-counter
                        'enable-sized-alloc-counter)))
        ;; This jump is always encoded as 5 bytes
        (inst call (if (or (not node) ; assembly routine
                           (sb-c::code-immobile-p node))
                       (make-fixup helper :assembly-routine)
                       (uniquify-fixup helper))))
      (inst nop)
      ;; Emit "TEST AL, imm" where the immediate value
      ;; encodes the the data buffer base reg and size reg numbers.
      (inst byte #xA8) ; "TEST AL,imm"
      (cond ((integerp size)
             (inst byte (tn-offset data)))
            (t
             (inst byte (logior (tn-offset data) (ash (tn-offset size) 4)))
             (inst .skip 8 :long-nop)))
      (emit-label skip-instrumentation))))

;;; Emit code to allocate an object with a size in bytes given by
;;; SIZE into ALLOC-TN. The size may be an integer of a TN.
;;; NODE may be used to make policy-based decisions.
;;; This function should only be used inside a pseudo-atomic section,
;;; which to the degree needed should also cover subsequent initialization.
;;; CONSP says whether we're allocating conses. But we also need LOWTAG
;;; because the vop could want the CONS to have 0 or list-pointer lowtag.
;;;
;;; A mnemonic device for the argument pattern here:
;;; 1. what to allocate: type, size, lowtag describe the object
;;; 2. where to put the result
;;; 3. node (for determining immobile-space-p) and a scratch register or two
(defun allocation (type size lowtag alloc-tn node temp thread-temp)
  (declare (ignorable thread-temp))
  (flet ((fallback (size)
           ;; Call an allocator trampoline and get the result in the proper register.
           ;; There are 2 choices of trampoline to invoke alloc() or alloc_list()
           ;; in C. This is chosen by the name of the asm routine.
           (cond ((typep size '(and integer (not (signed-byte 32))))
                  ;; MOV accepts large immediate operands, PUSH does not
                  (inst mov alloc-tn size)
                  (inst push alloc-tn))
                 (t
                  (inst push size)))
           (invoke-asm-routine 'call (if (eq type 'list) 'list-alloc-tramp 'alloc-tramp) node t)
           (inst pop alloc-tn)))
    (let* ((NOT-INLINE (gen-label))
           (DONE (gen-label))
           (free-pointer #+sb-thread (thread-slot-ea thread-mixed-tlab-slot #+gs-seg thread-temp)
                         #-sb-thread (ea mixed-region))
           (end-addr (ea (sb-x86-64-asm::ea-segment free-pointer)
                         (+ n-word-bytes (ea-disp free-pointer))
                         (ea-base free-pointer))))
      (cond ((typep size `(integer ,large-object-size))
             ;; large objects will never be made in a per-thread region
             (fallback size)
             (when (/= lowtag 0) (inst or :byte alloc-tn lowtag)))
            ((and (tn-p size) (location= size alloc-tn))
             (aver (and temp (not (location= temp size))))
             (inst mov temp free-pointer)
             ;; alloc-tn <- old free ptr and temp <- new free ptr
             (inst xadd temp alloc-tn)
             (inst cmp temp end-addr)
             (inst jmp :a NOT-INLINE)
             (inst mov free-pointer temp)
             (emit-label DONE)
             (when (/= lowtag 0) (inst or :byte alloc-tn lowtag))
             (assemble (:elsewhere)
               (emit-label NOT-INLINE)
               (inst sub temp alloc-tn) ; new-free-ptr - old-free-ptr = size
               (fallback temp)
               (inst jmp DONE)))
            (t
             ;; fixed-size allocation whose size fits in an imm32 can be done
             ;; with only one register, the ALLOC-TN. If it doesn't fit in imm32,
             ;; it would get the first branch of the COND, for large objects.
             (inst mov alloc-tn free-pointer)
             (cond (temp
                    (when (tn-p size) (aver (not (location= size temp))))
                    (inst lea temp (ea size alloc-tn))
                    (inst cmp temp end-addr)
                    (inst jmp :a NOT-INLINE)
                    (inst mov free-pointer temp)
                    (emit-label DONE)
                    (when (/= lowtag 0) (inst or :byte alloc-tn lowtag)))
                   (t
                    (inst add alloc-tn size)
                    (inst cmp alloc-tn end-addr)
                    (inst jmp :a NOT-INLINE)
                    (inst mov free-pointer alloc-tn)
                    (cond ((tn-p size)
                           (inst sub alloc-tn size)
                           (emit-label DONE)
                           (when (/= lowtag 0) (inst or :byte alloc-tn lowtag)))
                          (t
                           ;; SUB can compute the result and tagify it.
                           ;; The fallback also has to tagify.
                           (inst add alloc-tn (+ (- size) lowtag))
                           (emit-label DONE)))))
             (assemble (:elsewhere)
               (emit-label NOT-INLINE)
               (fallback size)
               (when (and (/= lowtag 0) (not temp) (not (tn-p size)))
                 (inst or :byte alloc-tn lowtag))
               (inst jmp DONE))))))
  t)

;;; Allocate an other-pointer object of fixed NWORDS with a single-word
;;; header having the specified WIDETAG value. The result is placed in
;;; RESULT-TN.  NWORDS counts the header word.
(defun alloc-other (widetag nwords result-tn node alloc-temp thread-temp
                    &aux (bytes (pad-data-block nwords)))
  (declare (ignorable thread-temp))
  (let ((header (compute-object-header nwords widetag)))
    #+bignum-assertions
    (when (= widetag bignum-widetag) (setq bytes (* bytes 2))) ; use 2x the space
    (cond ((and (not alloc-temp) (location= result-tn r12-tn))
           ;; this is the problematic case for INSTRUMENT-ALLOC.
           ;; Therefore, allocate into RAX but save it in RESULT-TN
           ;; and then switch them back.
           (inst mov result-tn rax-tn)
           (instrument-alloc widetag bytes node rax-tn thread-temp)
           (pseudo-atomic (:thread-tn thread-temp)
             (allocation nil bytes other-pointer-lowtag rax-tn node nil thread-temp)
             (storew* header rax-tn 0 other-pointer-lowtag t))
           (inst xchg rax-tn result-tn))
          (t
           (instrument-alloc widetag bytes node result-tn thread-temp)
           (pseudo-atomic ()
             (cond (alloc-temp
                    (allocation nil bytes 0 result-tn node alloc-temp thread-temp)
                    (storew* header result-tn 0 0 t)
                    (inst or :byte result-tn other-pointer-lowtag))
                   (t
                    (allocation nil bytes other-pointer-lowtag result-tn node nil thread-temp)
                    (storew* header result-tn 0 other-pointer-lowtag t))))))))

;;;; CONS, ACONS, LIST and LIST*
(macrolet ((store-slot (tn list &optional (slot cons-car-slot)
                                          (lowtag list-pointer-lowtag))
             ;; TODO: gencgc does not need EMIT-GC-STORE-BARRIER here,
             ;; but other other GC strategies might.
             `(let ((reg
                     ;; FIXME: single-float gets placed in the boxed header
                     ;; rather than just doing an immediate store.
                     (sc-case ,tn
                      ((control-stack constant)
                       (move temp ,tn)
                       temp)
                      (t
                       (encode-value-if-immediate ,tn)))))
                (storew* reg ,list ,slot ,lowtag (not stack-allocate-p) temp))))

(define-vop (cons)
  (:args (car :scs (any-reg descriptor-reg constant immediate))
         (cdr :scs (any-reg descriptor-reg constant immediate)))
  (:temporary (:sc unsigned-reg :to (:result 0) :target result) alloc)
  (:temporary (:sc unsigned-reg :to (:result 0)) temp)
  (:results (result :scs (descriptor-reg)))
  #+gs-seg (:temporary (:sc unsigned-reg :offset 15) thread-tn)
  (:node-var node)
  (:generator 10
    (let ((stack-allocate-p (node-stack-allocate-p node))
          (nbytes (* cons-size n-word-bytes)))
      (unless stack-allocate-p
        (instrument-alloc 'list nbytes node (list temp alloc) thread-tn))
      (pseudo-atomic (:elide-if stack-allocate-p :thread-tn thread-tn)
        (if stack-allocate-p
            (stack-allocation nbytes 0 alloc)
            (allocation 'list nbytes 0 alloc node temp thread-tn))
        (store-slot car alloc cons-car-slot 0)
        (store-slot cdr alloc cons-cdr-slot 0)
        (if (location= alloc result)
            (inst or :byte alloc list-pointer-lowtag)
            (inst lea result (ea list-pointer-lowtag alloc)))))))

(define-vop (acons)
  (:args (key :scs (any-reg descriptor-reg constant immediate))
         (val :scs (any-reg descriptor-reg constant immediate))
         (tail :scs (any-reg descriptor-reg constant immediate)))
  (:temporary (:sc unsigned-reg :to (:result 0)) alloc)
  (:temporary (:sc unsigned-reg :to (:result 0) :target result) temp)
  (:results (result :scs (descriptor-reg)))
  #+gs-seg (:temporary (:sc unsigned-reg :offset 15) thread-tn)
  (:node-var node)
  (:translate acons)
  (:policy :fast-safe)
  (:generator 10
    (let ((stack-allocate-p nil)
          (nbytes (* cons-size 2 n-word-bytes)))
      (instrument-alloc 'list nbytes node (list temp alloc) thread-tn)
      (pseudo-atomic (:thread-tn thread-tn)
        (allocation 'list nbytes 0 alloc node temp thread-tn)
        (store-slot tail alloc cons-cdr-slot 0)
        (inst lea temp (ea (+ 16 list-pointer-lowtag) alloc))
        (store-slot temp alloc cons-car-slot 0)
        (let ((pair temp) (temp alloc)) ; give STORE-SLOT the ALLOC as its TEMP
          (store-slot key pair)
          (store-slot val pair cons-cdr-slot))
        ;; ALLOC could have been clobbered by using it as a temp for
        ;; loading a constant.
        (if (location= temp result)
            (inst sub result 16) ; TEMP is ALLOC+16+lowtag, so just subtract 16
            (inst lea result (ea (- 16) temp)))))))

;;; CONS-2 is similar to ACONS, except that instead of producing
;;;  ((X . Y) . Z) it produces (X Y . Z)
(define-vop (cons-2)
  (:args (car :scs (any-reg descriptor-reg constant immediate))
         (cadr :scs (any-reg descriptor-reg constant immediate))
         (cddr :scs (any-reg descriptor-reg constant immediate)))
  (:temporary (:sc unsigned-reg :to (:result 0) :target result) alloc)
  (:temporary (:sc unsigned-reg :to (:result 0)) temp)
  (:results (result :scs (descriptor-reg)))
  #+gs-seg (:temporary (:sc unsigned-reg :offset 15) thread-tn)
  (:node-var node)
  (:generator 10
    (let ((stack-allocate-p (node-stack-allocate-p node))
          (nbytes (* cons-size 2 n-word-bytes)))
      (unless stack-allocate-p
        (instrument-alloc 'list nbytes node (list temp alloc) thread-tn))
      (pseudo-atomic (:elide-if stack-allocate-p :thread-tn thread-tn)
        (if stack-allocate-p
            (stack-allocation nbytes 0 alloc)
            (allocation 'list nbytes 0 alloc node temp thread-tn))
        (store-slot car alloc cons-car-slot 0)
        (store-slot cadr alloc (+ 2 cons-car-slot) 0)
        (store-slot cddr alloc (+ 2 cons-cdr-slot) 0)
        (inst lea temp (ea (+ 16 list-pointer-lowtag) alloc))
        (store-slot temp alloc cons-cdr-slot 0)
        (if (location= alloc result)
            (inst or :byte alloc list-pointer-lowtag)
            (inst lea result (ea list-pointer-lowtag alloc)))))))

(define-vop (list)
  (:args (things :more t :scs (descriptor-reg constant immediate)))
  (:temporary (:sc unsigned-reg) ptr temp)
  (:temporary (:sc unsigned-reg :to (:result 0) :target result) res)
  #+gs-seg (:temporary (:sc unsigned-reg :offset 15) thread-tn)
  (:info star cons-cells)
  (:results (result :scs (descriptor-reg)))
  (:node-var node)
  (:generator 0
    (aver (>= cons-cells 3)) ; prevent regressions in ir2tran's vop selection
    (let ((stack-allocate-p (node-stack-allocate-p node))
          (size (* (pad-data-block cons-size) cons-cells)))
      (unless stack-allocate-p
        (instrument-alloc 'list size node (list ptr temp) thread-tn))
      (pseudo-atomic (:elide-if stack-allocate-p :thread-tn thread-tn)
        (if stack-allocate-p
            (stack-allocation size list-pointer-lowtag res)
            (allocation 'list size list-pointer-lowtag res node temp thread-tn))
        (move ptr res)
        (dotimes (i (1- cons-cells))
          (store-slot (tn-ref-tn things) ptr)
          (setf things (tn-ref-across things))
          (inst add ptr (pad-data-block cons-size))
          (storew ptr ptr (- cons-cdr-slot cons-size) list-pointer-lowtag))
        (store-slot (tn-ref-tn things) ptr cons-car-slot list-pointer-lowtag)
        (cond (star
               (setf things (tn-ref-across things))
               (store-slot (tn-ref-tn things) ptr cons-cdr-slot list-pointer-lowtag))
              (t
               (storew* nil-value ptr cons-cdr-slot list-pointer-lowtag
                        (not stack-allocate-p))))))
    (aver (null (tn-ref-across things)))
    (move result res)))
)

;;;; special-purpose inline allocators

;;; Special variant of 'storew' which might have a shorter encoding
;;; when storing to the heap (which starts out zero-filled).
;;; This will always write 8 bytes if WORD is a negative number.
(defun storew* (word object slot lowtag zeroed &optional temp)
  (cond
    ((or (not zeroed) (not (typep word '(unsigned-byte 31))))
     ;; Will use temp reg if WORD can't be encoded as an imm32
    (storew word object slot lowtag temp))
   ((/= word 0)
    (let ((size
           (cond ((typep word '(unsigned-byte 8))
                  :byte)
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
                 ((typep word '(unsigned-byte 16))
                  :word)
                 (t ; must be an (unsigned-byte 31)
                  :dword))))
      (inst mov size (ea (- (* slot n-word-bytes) lowtag) object) word)))))

;;; ALLOCATE-VECTOR
(defun store-string-trailing-null (vector type length words)
  ;; BASE-STRING needs to have a null terminator. The byte is inaccessible
  ;; to lisp, so clear it now.
  (cond ((and (sc-is type immediate)
              (/= (tn-value type) sb-vm:simple-base-string-widetag))) ; do nothing
        ((and (sc-is type immediate)
              (= (tn-value type) sb-vm:simple-base-string-widetag)
              (sc-is length immediate))
         (inst mov :byte (ea (- (+ (ash vector-data-offset word-shift)
                                   (tn-value length))
                                other-pointer-lowtag)
                             vector)
               0))
        ;; Zeroizing the entire final word is easier than using LENGTH now.
        ((sc-is words immediate)
         ;; I am not convinced that this case is reachable -
         ;; we won't DXify a vector of unknown type.
         (inst mov :qword
               ;; Given N data words, write to word N-1
               (ea (- (ash (+ (tn-value words) vector-data-offset -1)
                           word-shift)
                      other-pointer-lowtag)
                   vector)
               0))
        (t
         ;; This final case is ok with 0 data words - it might clobber the LENGTH
         ;; slot, but subsequently we rewrite that slot.
         ;; But strings always have at least 1 word, so no worries either way.
         (inst mov :qword
               (ea (- (ash (1- vector-data-offset) word-shift)
                      other-pointer-lowtag)
                   vector
                   words (ash 1 (- word-shift n-fixnum-tag-bits)))
               0))))

(macrolet ((calc-size-in-bytes (n-words size-tn)
             `(cond ((sc-is ,n-words immediate)
                     (pad-data-block (+ (tn-value ,n-words) vector-data-offset)))
                    (t
                     (inst lea ,size-tn
                           (ea (+ lowtag-mask (* vector-data-offset n-word-bytes))
                               nil ,n-words (ash 1 (- word-shift n-fixnum-tag-bits))))
                     (inst and ,size-tn (lognot lowtag-mask))
                     ,size-tn)))
           (put-header (vector-tn lowtag type len zeroed temp)
             `(let ((len (if (sc-is ,len immediate) (fixnumize (tn-value ,len)) ,len))
                    (type (if (sc-is ,type immediate) (tn-value ,type) ,type)))
                (storew* type ,vector-tn 0 ,lowtag ,zeroed ,temp)
                #+ubsan (inst mov :dword (vector-len-ea ,vector-tn ,lowtag) len)
                #-ubsan (storew* len ,vector-tn vector-length-slot
                                       ,lowtag ,zeroed ,temp)))
           (want-shadow-bits ()
             `(and poisoned
                   (if (sc-is type immediate)
                       (/= (tn-value type) simple-vector-widetag)
                       :maybe)))
           (calc-shadow-bits-size (reg)
             `(cond ((sc-is length immediate)
                     ;; Calculate number of dualwords (as 128 bits per dualword)
                     ;; and multiply by 16 to get number of bytes. Also add the 2 header words.
                     (* 16 (+ 2 (ceiling (tn-value length) 128))))
                    (t
                     ;; Compute (CEILING length 128) by adding 127, then truncating divide
                     ;; by 128, and untag as part of the divide step.
                     ;; Account for the two fixed words by adding in 128 more bits initially.
                     (inst lea :dword ,reg (ea (fixnumize (+ 128 127)) length))
                     (inst shr :dword ,reg 8) ; divide by 128 and untag as one operation
                     (inst shl :dword ,reg 4) ; multiply by 16 bytes per dualword
                     ,reg)))
           (store-originating-pc (vector)
             ;; Put the current program-counter into the length slot of the shadow bits
             ;; so that we can ascribe blame to the array's creator.
             `(let ((here (gen-label)))
                (emit-label here)
                (inst lea temp (rip-relative-ea here))
                (inst shl temp 4)
                (inst mov (ea (- 8 other-pointer-lowtag) ,vector) temp))))

  (define-vop (allocate-vector-on-heap)
    #+ubsan (:info poisoned)
    (:args (type :scs (unsigned-reg immediate))
           (length :scs (any-reg immediate))
           (words :scs (any-reg immediate)))
    ;; Result is live from the beginning, like a temp, because we use it as such
    ;; in 'calc-size-in-bytes'
    (:results (result :scs (descriptor-reg) :from :load))
    (:arg-types #+ubsan (:constant t)
                positive-fixnum positive-fixnum positive-fixnum)
    (:temporary (:sc unsigned-reg) temp)
    #+gs-seg (:temporary (:sc unsigned-reg :offset 15) thread-tn)
    (:policy :fast-safe)
    (:node-var node)
    (:generator 100
      #+ubsan
      (when (want-shadow-bits)
        ;; allocate a vector of "written" bits unless the vector is simple-vector-T,
        ;; which can use unbound-marker as a poison value on reads.
        (when (sc-is type unsigned-reg)
          (inst cmp :byte type simple-vector-widetag)
          (inst push 0)
          (inst jmp :e NO-SHADOW-BITS))
        ;; It would be possible to do this and the array proper
        ;; in a single pseudo-atomic section, but I don't care to do that.
        (let ((nbytes (calc-shadow-bits-size result)))
          (pseudo-atomic ()
            ;; Allocate the bits into RESULT
            (allocation nil nbytes 0 result node temp nil)
            (inst mov :byte (ea result) simple-bit-vector-widetag)
            (inst mov :dword (vector-len-ea result 0)
                  (if (sc-is length immediate) (fixnumize (tn-value length)) length))
            (inst or :byte result other-pointer-lowtag)))
        (store-originating-pc result)
        (inst push result)) ; save the pointer to the shadow bits
      NO-SHADOW-BITS
      ;; The LET generates instructions that needn't be pseudoatomic
      ;; so don't move it inside.
      ;; There are 3 possibilities for correctness of INSTRUMENT-ALLOC:
      ;; * If WORDS is not immediate, and ALLOC-TEMP is R12, then compute size
      ;;   into ALLOC-TEMP, use RESULT as the instrumentation temp.
      ;;   ALLOCATION receives: input = ALLOC-TEMP, output = RESULT, and no other temp
      ;; * If WORDS is not immediate and ALLOC-TEMP is not R12, then compute size
      ;;   into RESULT, use ALLOC-TEMP as the instrumentation temp.
      ;;   ALLOCATION receives: input = RESULT, output = RESULT, temp = ALLOC-TEMP.
      (multiple-value-bind (size-tn instrumentation-temp alloc-temp)
          (cond ((sc-is words immediate)
                 ;; If WORDS is immediate, then let INSTRUMENT-ALLOC choose its temp
                 (values (calc-size-in-bytes words nil) (list result temp) temp))
                ((location= temp r12-tn)
                 ;; Compute the size into TEMP, use RESULT for instrumentation.
                 ;; Don't give another temp to ALLOCATION, because its SIZE and temp
                 ;; can not be in the same register (which it AVERs).
                 (values (calc-size-in-bytes words temp) result nil))
                (t
                 ;; Compute the size into RESULT, use TEMP for instrumentation.
                 ;; ALLOCATION needs the temp register in this case,
                 ;; because input and output are in the same register.
                 (values (calc-size-in-bytes words result) temp temp)))
        (instrument-alloc (if (sc-is type immediate)
                              (case (tn-value type)
                                (#.simple-vector-widetag 'simple-vector)
                                (t 'unboxed-array))
                              type)
                          size-tn node instrumentation-temp thread-tn)
        (pseudo-atomic (:thread-tn thread-tn)
         (allocation nil size-tn 0 result node alloc-temp thread-tn)
         (put-header result 0 type length t alloc-temp)
         (inst or :byte result other-pointer-lowtag)))
      #+ubsan
      (cond ((want-shadow-bits)
             (inst pop temp-reg-tn) ; restore shadow bits
             (inst mov (object-slot-ea result 1 other-pointer-lowtag) temp-reg-tn))
            (poisoned ; uninitialized SIMPLE-VECTOR
             (store-originating-pc result)))))

  (define-vop (allocate-vector-on-stack)
    #+ubsan (:info poisoned)
    (:args (type :scs (unsigned-reg immediate))
           (length :scs (any-reg immediate))
           (words :scs (any-reg immediate)))
    (:results (result :scs (descriptor-reg) :from :load))
    (:vop-var vop)
    (:arg-types #+ubsan (:constant t)
                positive-fixnum positive-fixnum positive-fixnum)
    #+ubsan (:temporary (:sc any-reg :offset rax-offset) rax)
    #+ubsan (:temporary (:sc any-reg :offset rcx-offset) rcx)
    #+ubsan (:temporary (:sc any-reg :offset rdi-offset) rdi)
    (:policy :fast-safe)
    (:generator 10
      #+ubsan
      (when (want-shadow-bits)
        ;; allocate a vector of "written" bits unless the vector is simple-vector-T,
        ;; which can use unbound-marker as a poison value on reads.
        (when (sc-is type unsigned-reg) (bug "vector-on-stack: unknown type"))
        (zeroize rax)
        (let ((nbytes (calc-shadow-bits-size rcx)))
          (stack-allocation nbytes 0 rdi)
          (when (sc-is length immediate) (inst mov rcx nbytes)))
        (inst rep)
        (inst stos :byte) ; RAX was zeroed
        (inst lea rax (ea other-pointer-lowtag rsp-tn))
        (inst mov :dword (ea (- other-pointer-lowtag) rax) simple-bit-vector-widetag)
        (inst mov :dword (vector-len-ea rax)
              (if (sc-is length immediate) (fixnumize (tn-value length)) length))
        (store-originating-pc rax))
      (let ((size (calc-size-in-bytes words result)))
        ;; Compute tagged pointer sooner than later since access off RSP
        ;; requires an extra byte in the encoding anyway.
        (stack-allocation size other-pointer-lowtag result
                          ;; If already aligned RSP, don't need to do it again.
                          #+ubsan (want-shadow-bits))
        ;; NB: store the trailing null BEFORE storing the header,
        ;; in case the length in words is 0, which stores into the LENGTH slot
        ;; as if it were element -1 of data (which probably can't happen).
        (store-string-trailing-null result type length words)
        ;; FIXME: It would be good to check for stack overflow here.
        (put-header result other-pointer-lowtag type length nil nil)
        )
      #+ubsan
      (cond ((want-shadow-bits)
             (inst mov (ea (- (ash vector-length-slot word-shift) other-pointer-lowtag)
                           result)
                   rax))
            (poisoned ; uninitialized SIMPLE-VECTOR
             (store-originating-pc result)))))

  #+linux ; unimplemented for others
  (define-vop (allocate-vector-on-stack+msan-unpoison)
    #+ubsan (:info poisoned)
    #+ubsan (:ignore poisoned)
    (:args (type :scs (unsigned-reg immediate))
           (length :scs (any-reg immediate))
           (words :scs (any-reg immediate)))
    (:results (result :scs (descriptor-reg) :from :load))
    (:arg-types #+ubsan (:constant t)
                positive-fixnum positive-fixnum positive-fixnum)
    ;; This is a separate vop because it needs more temps.
    (:temporary (:sc any-reg :offset rcx-offset) rcx)
    (:temporary (:sc any-reg :offset rax-offset) rax)
    (:temporary (:sc any-reg :offset rdi-offset) rdi)
    (:policy :fast-safe)
    (:generator 10
      (let ((size (calc-size-in-bytes words result)))
        ;; Compute tagged pointer sooner than later since access off RSP
        ;; requires an extra byte in the encoding anyway.
        (stack-allocation size other-pointer-lowtag result)
        (store-string-trailing-null result type length words)
        ;; FIXME: It would be good to check for stack overflow here.
        (put-header result other-pointer-lowtag type length nil nil)
        (cond ((sc-is words immediate)
               (inst mov rcx (+ (tn-value words) vector-data-offset)))
              (t
               (inst lea rcx (ea (ash vector-data-offset n-fixnum-tag-bits) words))
               (inst shr rcx n-fixnum-tag-bits)))
        (inst mov rdi msan-mem-to-shadow-xor-const)
        (inst xor rdi rsp-tn) ; compute shadow address
        (zeroize rax)
        (inst rep)
        (inst stos :qword)))))

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
                           (ea nil ,length
                               (ash 1 (1+ (- word-shift n-fixnum-tag-bits)))))
                     ,answer)))
           (compute-end ()
             `(let ((size (cond ((typep size '(or (signed-byte 32) tn))
                                 size)
                                (t
                                 (inst mov limit size)
                                 limit))))
                (inst lea limit
                      (ea (if (fixnump size) size 0) result
                          (if (fixnump size) nil size))))))

  (define-vop (allocate-list-on-stack)
    (:args (length :scs (any-reg immediate))
           (element :scs (any-reg descriptor-reg)))
    (:results (result :scs (descriptor-reg) :from :load))
    (:arg-types positive-fixnum *)
    (:policy :fast-safe)
    (:temporary (:sc descriptor-reg) tail next limit)
    (:generator 20
      (let ((size (calc-size-in-bytes length next))
            (loop (gen-label)))
        (stack-allocation size list-pointer-lowtag result)
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
    #+gs-seg (:temporary (:sc unsigned-reg :offset 15) thread-tn)
    (:generator 20
      (let ((size (calc-size-in-bytes length tail))
            (entry (gen-label))
            (loop (gen-label))
            (no-init
             (and (sc-is element immediate) (eql (tn-value element) 0))))
        (instrument-alloc 'list size node (list next limit) thread-tn)
        (pseudo-atomic (:thread-tn thread-tn)
         (allocation 'list size list-pointer-lowtag result node limit thread-tn)
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

#-immobile-space
(define-vop (make-fdefn)
  (:policy :fast-safe)
  (:translate make-fdefn)
  (:args (name :scs (descriptor-reg) :to :eval))
  (:results (result :scs (descriptor-reg) :from :argument))
  #+gs-seg (:temporary (:sc unsigned-reg :offset 15) thread-tn)
  (:node-var node)
  (:generator 37
    (alloc-other fdefn-widetag fdefn-size result node nil thread-tn)
    (storew name result fdefn-name-slot other-pointer-lowtag)
    (storew nil-value result fdefn-fun-slot other-pointer-lowtag)
    (storew (make-fixup 'undefined-tramp :assembly-routine)
            result fdefn-raw-addr-slot other-pointer-lowtag)))

(define-vop (make-closure)
  (:info label length stack-allocate-p)
  (:temporary (:sc any-reg) temp)
  #+gs-seg (:temporary (:sc unsigned-reg :offset 15) thread-tn)
  (:results (result :scs (descriptor-reg)))
  (:node-var node)
  (:generator 10
    (let* ((words (+ length closure-info-offset)) ; including header
           (bytes (pad-data-block words))
           (header (logior (ash (1- words) n-widetag-bits) closure-widetag)))
      (unless stack-allocate-p
        (instrument-alloc closure-widetag bytes node (list result temp) thread-tn))
      (pseudo-atomic (:elide-if stack-allocate-p :thread-tn thread-tn)
        (if stack-allocate-p
            (stack-allocation bytes fun-pointer-lowtag result)
            (allocation nil bytes fun-pointer-lowtag result node temp thread-tn))
        (storew* #-immobile-space header ; write the widetag and size
                 #+immobile-space        ; ... plus the layout pointer
                 (progn (inst mov temp header)
                        (inst or temp #-sb-thread (static-symbol-value-ea 'function-layout)
                                      #+sb-thread
                                      (thread-slot-ea thread-function-layout-slot))
                        temp)
                 result 0 fun-pointer-lowtag (not stack-allocate-p)))
      ;; Finished with the pseudo-atomic instructions
      ;; TODO: gencgc does not need EMIT-GC-STORE-BARRIER here, but other other GC strategies might.
      (inst lea temp (rip-relative-ea label (ash simple-fun-insts-offset word-shift)))
      (storew temp result closure-fun-slot fun-pointer-lowtag)
      #+metaspace
      (let ((origin (sb-assem::asmstream-data-origin-label sb-assem:*asmstream*)))
        (inst lea temp (rip-relative-ea origin :code))
        (storew temp result closure-code-slot fun-pointer-lowtag)))))

;;; The compiler likes to be able to directly make value cells.
(define-vop (make-value-cell)
  (:args (value :scs (descriptor-reg any-reg) :to :result))
  (:results (result :scs (descriptor-reg) :from :eval))
  #+gs-seg (:temporary (:sc unsigned-reg :offset 15) thread-tn)
  (:info stack-allocate-p)
  (:node-var node)
  (:generator 10
    (cond (stack-allocate-p
           (stack-allocation (pad-data-block value-cell-size) other-pointer-lowtag result)
           (let ((header (compute-object-header value-cell-size value-cell-widetag)))
             (storew header result 0 other-pointer-lowtag)))
          (t
           (alloc-other value-cell-widetag value-cell-size result node nil thread-tn)))
    ;; TODO: gencgc does not need EMIT-GC-STORE-BARRIER here, but other other GC strategies might.
    (storew value result value-cell-value-slot other-pointer-lowtag)))

;;;; automatic allocators for primitive objects

(define-vop (make-funcallable-instance-tramp)
  (:args)
  (:results (result :scs (any-reg)))
  (:vop-var vop)
  (:generator 1
    (let ((tramp (make-fixup 'funcallable-instance-tramp :assembly-routine)))
      (if (sb-c::code-immobile-p vop)
          (inst lea result (ea tramp rip-tn))
          (inst mov result tramp)))))

(define-vop (fixed-alloc)
  (:args)
  (:info name words type lowtag stack-allocate-p)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg) alloc-temp)
  #+gs-seg (:temporary (:sc unsigned-reg :offset 15) thread-tn)
  (:node-var node)
  (:generator 50
   (let* ((instancep (typep type 'wrapper)) ; is this an instance type?
          (bytes (pad-data-block words)))
    #+bignum-assertions
    (when (eq type bignum-widetag) (setq bytes (* bytes 2))) ; use 2x the space
    (progn name) ; possibly not used
    (unless stack-allocate-p
      (instrument-alloc type bytes node (list result alloc-temp) thread-tn))
    (pseudo-atomic (:elide-if stack-allocate-p :thread-tn thread-tn)
      ;; If storing a header word, defer ORing in the lowtag until after
      ;; the header is written so that displacement can be 0.
      (if stack-allocate-p
          (stack-allocation bytes (if type 0 lowtag) result)
          (allocation nil bytes (if type 0 lowtag) result node alloc-temp thread-tn))
      (let* ((widetag (if instancep instance-widetag type))
             (header (compute-object-header words widetag)))
        (cond #+compact-instance-header
              ((and (eq name '%make-structure-instance) stack-allocate-p)
              ;; Write a :DWORD, not a :QWORD, because the high half will be
              ;; filled in when the layout is stored. Can't use STOREW* though,
              ;; because it tries to store as few bytes as possible,
              ;; where this instruction must write exactly 4 bytes.
               (inst mov :dword (ea 0 result) header))
              (t
               (storew* header result 0 0 (not stack-allocate-p)))))
      ;; GC can make the best choice about placement if it has a layout.
      ;; Of course with conservative GC the object will be pinned anyway,
      ;; but still, always having a layout is a good thing.
      (when instancep ; store its layout, while still in pseudo-atomic
        (inst mov :dword (ea 4 result) (make-fixup type :layout)))
      (inst or :byte result lowtag)))))

;;; Allocate a non-vector variable-length object.
;;; Exactly 4 allocators are rendered via this vop:
;;;  BIGNUM               (%ALLOCATE-BIGNUM)
;;;  FUNCALLABLE-INSTANCE (%MAKE-FUNCALLABLE-INSTANCE)
;;;  CLOSURE              (%ALLOC-CLOSURE)
;;;  INSTANCE             (%MAKE-INSTANCE)
;;; WORDS accounts for the mandatory slots *including* the header.
;;; EXTRA is the variable payload, also measured in words.
(define-vop (var-alloc)
  (:args (extra :scs (any-reg)))
  (:arg-types positive-fixnum)
  (:info name words type lowtag stack-allocate-p)
  (:ignore name)
  (:results (result :scs (descriptor-reg) :from (:eval 1)))
  (:temporary (:sc unsigned-reg :from :eval :to (:eval 1)) bytes)
  (:temporary (:sc unsigned-reg :from :eval :to :result) header)
  ;; KLUDGE: wire to RAX so that it doesn't get R12
  (:temporary (:sc unsigned-reg :offset 0) alloc-temp)
  #+gs-seg (:temporary (:sc unsigned-reg :offset 15) thread-tn)
  (:node-var node)
  (:generator 50
   ;; With the exception of bignums, these objects have effectively
   ;; 32-bit headers because the high 4 byes contain a layout pointer.
   (let ((operand-size (if (= type bignum-widetag) :qword :dword)))
      (inst lea operand-size bytes
            (ea (* (1+ words) n-word-bytes) nil
                extra (ash 1 (- word-shift n-fixnum-tag-bits))))
      (inst mov operand-size header bytes)
      (inst shl operand-size header (- (length-field-shift type) word-shift)) ; w+1 to length field
      (inst lea operand-size header                    ; (w-1 << 8) | type
            (ea (+ (ash -2 (length-field-shift type)) type) header))
      (inst and operand-size bytes (lognot lowtag-mask)))
   #+bignum-assertions
   (when (= type bignum-widetag) (inst shl :dword bytes 1)) ; use 2x the space
   (cond (stack-allocate-p
             (stack-allocation bytes lowtag result)
             (storew header result 0 lowtag))
         (t
             ;; can't pass RESULT as a possible choice of scratch register
             ;; because it might be in the same physical reg as BYTES.
             ;; Yup, the lifetime specs in this vop are pretty confusing.
             (instrument-alloc type bytes node alloc-temp thread-tn)
             (pseudo-atomic (:thread-tn thread-tn)
              (allocation nil bytes lowtag result node alloc-temp thread-tn)
              (storew header result 0 lowtag))))))

(macrolet ((c-call (name)
             `(let ((c-fun (make-fixup ,name :foreign)))
                (inst call (cond ((sb-c::code-immobile-p node) c-fun)
                                 (t (progn (inst mov rax c-fun) rax)))))))
#+immobile-space
(define-vop (alloc-immobile-fixedobj)
  (:args (size-class :scs (any-reg) :target c-arg1)
         (nwords :scs (any-reg) :target c-arg2)
         (header :scs (any-reg) :target c-arg3))
  (:temporary (:sc unsigned-reg :from (:argument 0) :to :eval :offset rdi-offset) c-arg1)
  (:temporary (:sc unsigned-reg :from (:argument 1) :to :eval :offset rsi-offset) c-arg2)
  (:temporary (:sc unsigned-reg :from (:argument 2) :to :eval :offset rdx-offset) c-arg3)
  (:temporary (:sc unsigned-reg :from :eval :to (:result 0) :offset rax-offset) rax)
  (:results (result :scs (descriptor-reg)))
  (:node-var node)
  (:generator 50
   (inst mov c-arg1 size-class)
   (inst mov c-arg2 nwords)
   (inst mov c-arg3 header)
   ;; RSP needn't be restored because the allocators all return immediately
   ;; which has that effect
   (inst and rsp-tn -16)
   (pseudo-atomic ()
     (c-call "alloc_immobile_fixedobj")
     (move result rax))))

) ; end MACROLET
