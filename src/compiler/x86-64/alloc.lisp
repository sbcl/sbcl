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

(defun stack-allocation (alloc-tn size lowtag &optional known-alignedp)
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

;;; For assemfile
#+(and avx2 sb-xc-host)
(defvar *avx-registers-used-p* nil)

#+avx2
(defun avx-registers-used-p ()
  (or #+sb-xc-host *avx-registers-used-p*
      (when (and #+sb-xc-host (boundp '*component-being-compiled*))
        (let ((comp (component-info *component-being-compiled*)))
          (or (sb-c::ir2-component-avx2-used-p comp)
              (flet ((used-p (tn)
                       (do ((tn tn (sb-c::tn-next tn)))
                           ((null tn))
                         (when (sc-is tn avx2-reg
                                      int-avx2-reg
                                      double-avx2-reg single-avx2-reg)
                           (return-from avx-registers-used-p
                             (setf (sb-c::ir2-component-avx2-used-p comp) t))))))
                (used-p (sb-c::ir2-component-normal-tns comp))
                (used-p (sb-c::ir2-component-wired-tns comp))))))))

;;; Call an allocator trampoline and get the result in the proper register.
;;; There are 2x2x2 choices of trampoline:
;;;  - invoke alloc() or alloc_list() in C
;;;  - place result into R11, or leave it on the stack
;;;  - preserve YMM registers around the call, or don't
;;; Rather than have 8 different DEFINE-ASSEMBLY-ROUTINEs, there are only 4,
;;; and each has 2 entry points. The earlier entry adds 1 more instruction
;;; to set the non-cons bit (the first of the binary choices mentioned above).
;;; Most of the time, the inline allocation sequence wants to use the trampoline
;;; that returns a result in TEMP-REG-TN (R11) which saves one move and
;;; clears the size argument from the stack in the RET instruction.
;;; If the result is returned on the stack, then we pop it below.
(defun %alloc-tramp (type node result-tn size lowtag)
  (let ((consp (eq type 'list))
        (to-r11 (location= result-tn r11-tn)))
    (when (typep size 'integer)
      (aver (= (align-up size (* 2 n-word-bytes)) size))
      (when (neq type 'list)
        (incf size) ; the low bit means we're allocating a non-cons object
        ;; Jump into the cons entry point which saves one instruction because why not.
        (setq consp t)))
    (cond ((typep size '(and integer (not (signed-byte 32))))
           ;; MOV accepts large immediate operands, PUSH does not
           (inst mov result-tn size)
           (inst push result-tn))
          (t
           (inst push size)))
    (invoke-asm-routine
     'call
     (cond #+avx2
           ((avx-registers-used-p)
            (if to-r11
                (if consp 'cons->r11.avx2 'alloc->r11.avx2)
                (if consp 'cons->rnn.avx2 'alloc->rnn.avx2)))
           (t
            (if to-r11
                (if consp 'cons->r11 'alloc->r11)
                (if consp 'cons->rnn 'alloc->rnn))))
     node)
    (unless to-r11
      (inst pop result-tn)))
  (unless (eql lowtag 0)
    (inst or :byte result-tn lowtag)))

;;; Insert allocation profiler instrumentation
(defun instrument-alloc (size node)
  (when (policy node (> sb-c::instrument-consing 1))
    (let ((skip-instrumentation (gen-label)))
      (inst mov temp-reg-tn (thread-slot-ea thread-profile-data-slot))
      (inst test temp-reg-tn temp-reg-tn)
      ;; This instruction is modified to "JMP :z" when profiling is
      ;; partially enabled. After the buffer is assigned, it becomes
      ;; fully enabled. The unconditional jmp gives minimal performance
      ;; loss if the profiler is statically disabled. (one memory
      ;; read and a test whose result is never used, which the CPU
      ;; is good at ignoring as far as instruction prefetch goes)
      (inst jmp skip-instrumentation)
      (emit-alignment 3 :long-nop)
      (let ((helper (if (integerp size)
                        'enable-alloc-counter
                        'enable-sized-alloc-counter)))
        (cond ((or (not node) ; assembly routine
                   (sb-c::code-immobile-p node))
               (inst call (make-fixup helper :assembly-routine)) ; 5 bytes
               (emit-alignment 3 :long-nop))
              (t
               (inst call (ea (make-fixup helper :assembly-routine*))) ; 7 bytes
               (inst nop))) ; align
        (unless (integerp size)
          ;; This TEST instruction is never executed- it informs the profiler
          ;; which register holds SIZE.
          (inst test size size) ; 3 bytes
          (emit-alignment 3 :long-nop)))
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
;;; 2. how to allocate it: policy and how to invoke the trampoline
;;; 3. where to put the result
(defun allocation (type size lowtag node dynamic-extent alloc-tn)
  (when dynamic-extent
    (stack-allocation alloc-tn size lowtag)
    (return-from allocation (values)))
  (aver (and (not (location= alloc-tn temp-reg-tn))
             (or (integerp size) (not (location= size temp-reg-tn)))))

  (aver (not (sb-assem::assembling-to-elsewhere-p)))
  ;; Otherwise do the normal inline allocation thing
  (let ((NOT-INLINE (gen-label))
        (DONE (gen-label))
        ;; thread->alloc_region.free_pointer
        (free-pointer
         #+sb-thread (thread-slot-ea thread-alloc-region-slot)
         #-sb-thread (ea boxed-region))
        ;; thread->alloc_region.end_addr
        (end-addr
         #+sb-thread (thread-slot-ea (1+ thread-alloc-region-slot))
         #-sb-thread (ea (+ boxed-region n-word-bytes))))

    (cond ((typep size `(integer , large-object-size))
           ;; large objects will never be made in a per-thread region
           (%alloc-tramp type node alloc-tn size lowtag))
          ((eql lowtag 0)
           (cond ((and (tn-p size) (location= size alloc-tn))
                  (inst mov temp-reg-tn free-pointer)
                  ;; alloc-tn <- old free ptr and temp <- new free ptr
                  (inst xadd temp-reg-tn alloc-tn))
                 (t
                  (inst mov alloc-tn free-pointer)
                  (inst lea temp-reg-tn (ea size alloc-tn))))
           (inst cmp temp-reg-tn end-addr)
           (inst jmp :a NOT-INLINE)
           (inst mov free-pointer temp-reg-tn)
           (emit-label DONE)
           (assemble (:elsewhere)
             (emit-label NOT-INLINE)
             (let ((size (cond ((and (tn-p size) (location= size alloc-tn))
                                (inst sub temp-reg-tn alloc-tn)
                                temp-reg-tn)
                               (t
                                size))))
               (%alloc-tramp type node alloc-tn size 0))
             (inst jmp DONE)))
          (t
           (inst mov temp-reg-tn free-pointer)
           (cond ((integerp size)
                  (inst lea alloc-tn (ea size temp-reg-tn)))
                 ((location= alloc-tn size)
                  (inst add alloc-tn temp-reg-tn))
                 (t
                  (inst lea alloc-tn (ea temp-reg-tn size))))
           (inst cmp alloc-tn end-addr)
           (inst jmp :a NOT-INLINE)
           (inst mov free-pointer alloc-tn)
           (emit-label DONE)
           (tagify alloc-tn temp-reg-tn lowtag)
           (assemble (:elsewhere)
             (emit-label NOT-INLINE)
             (cond ((and (tn-p size) (location= size alloc-tn)) ; recover SIZE
                    (inst sub alloc-tn temp-reg-tn)
                    (%alloc-tramp type node temp-reg-tn alloc-tn 0))
                   (t ; SIZE is intact
                    (%alloc-tramp type node temp-reg-tn size 0)))
             (inst jmp DONE))))
    (values)))

;;; Allocate an other-pointer object of fixed SIZE with a single word
;;; header having the specified WIDETAG value. The result is placed in
;;; RESULT-TN.
(defun alloc-other (result-tn widetag size node &optional stack-allocate-p
                    &aux (bytes (pad-data-block size)))
  (let ((header (compute-object-header size widetag)))
    (cond (stack-allocate-p
           (allocation nil bytes other-pointer-lowtag node t result-tn)
           (storew header result-tn 0 other-pointer-lowtag))
          (t
           (instrument-alloc bytes node)
           (pseudo-atomic ()
             (allocation nil bytes 0 node nil result-tn)
             (storew* header result-tn 0 0 t)
             (inst or :byte result-tn other-pointer-lowtag))))))

;;;; CONS, LIST and LIST*
(define-vop (list-or-list*)
  (:args (things :more t :scs (descriptor-reg constant immediate)))
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
               ((store-slot (tn list &optional (slot cons-car-slot)
                                               (lowtag list-pointer-lowtag))
                  `(let ((reg
                          ;; FIXME: single-float gets placed in the boxed header
                          ;; rather than just doing an immediate store.
                          (sc-case ,tn
                            ((control-stack constant)
                             (move temp ,tn)
                             temp)
                            (t
                             (encode-value-if-immediate ,tn)))))
                     (storew* reg ,list ,slot ,lowtag (not stack-allocate-p)))))
             (let* ((cons-cells (if star (1- num) num))
                    (stack-allocate-p (node-stack-allocate-p node))
                    (size (* (pad-data-block cons-size) cons-cells)))
               (unless stack-allocate-p
                 (instrument-alloc size node))
               (pseudo-atomic (:elide-if stack-allocate-p)
                (allocation 'list size (if (= cons-cells 2) 0 list-pointer-lowtag)
                            node stack-allocate-p res)
                (multiple-value-bind (last-base-reg lowtag car cdr)
                    (cond
                      ((= cons-cells 2)
                       ;; Note that this does not use the 'ptr' register at all.
                       ;; It would require a different vop to free that register up.
                       (store-slot (tn-ref-tn things) res cons-car-slot 0)
                       (setf things (tn-ref-across things))
                       (inst lea temp (ea (+ (* cons-size n-word-bytes) list-pointer-lowtag) res))
                       (store-slot temp res cons-cdr-slot 0)
                       (values res 0 (+ cons-size cons-car-slot) (+ cons-size cons-cdr-slot)))
                      ;; 1 cons IR1-transforms to CONS which IR2-converts as FIXED-ALLOC.
                      ((= cons-cells 1) (bug "Why?")) ; shoulda been CONS
                      (t
                       (move ptr res)
                       (dotimes (i (1- cons-cells))
                         (store-slot (tn-ref-tn things) ptr)
                         (setf things (tn-ref-across things))
                         (inst add ptr (pad-data-block cons-size))
                         (storew ptr ptr (- cons-cdr-slot cons-size)
                                 list-pointer-lowtag))
                       (values ptr list-pointer-lowtag cons-car-slot cons-cdr-slot)))
                  (store-slot (tn-ref-tn things) last-base-reg car lowtag)
                  (cond (star
                         (setf things (tn-ref-across things))
                         (store-slot (tn-ref-tn things) last-base-reg cdr lowtag))
                        (t
                         (storew* nil-value last-base-reg cdr lowtag
                                  (not stack-allocate-p))))
                  (cond ((= cons-cells 2)
                         (if (location= result res)
                             (inst or :byte result list-pointer-lowtag)
                             (inst lea result (ea list-pointer-lowtag res))))
                        (t
                         (move result res)))))))
           (aver (null (tn-ref-across things)))))))

(define-vop (list list-or-list*)
  (:variant nil))

(define-vop (list* list-or-list*)
  (:variant t))

;;;; special-purpose inline allocators

;;; Special variant of 'storew' which might have a shorter encoding
;;; when storing to the heap (which starts out zero-filled).
;;; This will always write 8 bytes if WORD is a negative number.
(defun storew* (word object slot lowtag zeroed)
  (cond
   ((or (not zeroed) (not (typep word '(unsigned-byte 31))))
    (storew word object slot lowtag)) ; Possibly use temp-reg-tn
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

(macrolet ((calc-size-in-bytes (n-words result-tn)
             `(cond ((sc-is ,n-words immediate)
                     (pad-data-block (+ (tn-value ,n-words) vector-data-offset)))
                    (t
                     (inst lea ,result-tn
                           (ea (+ lowtag-mask (* vector-data-offset n-word-bytes))
                               nil ,n-words (ash 1 (- word-shift n-fixnum-tag-bits))))
                     (inst and ,result-tn (lognot lowtag-mask))
                     ,result-tn)))
           (put-header (vector-tn lowtag type len zeroed)
             `(let ((len (if (sc-is ,len immediate) (fixnumize (tn-value ,len)) ,len))
                    (type (if (sc-is ,type immediate) (tn-value ,type) ,type)))
                (storew* type ,vector-tn 0 ,lowtag ,zeroed)
                #+ubsan (inst mov :dword (vector-len-ea ,vector-tn ,lowtag) len)
                #-ubsan (storew* len ,vector-tn vector-length-slot
                                       ,lowtag ,zeroed)))
           (require-shadow-bits ()
             `(and poisoned
                   (sb-c::policy node (> sb-c::aref-poison-detect 0))
                   (if (sc-is length immediate) (> (tn-value length) 0) :maybe)
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
           (alloc-origin-tracker (tn lowtag)
             `(let ((origin (sb-assem::asmstream-data-origin-label sb-assem:*asmstream*)))
                ;; store CONS-CAR-SLOT
                (inst mov :qword (ea (- ,lowtag) ,tn) (make-fixup nil :pc-offset-as-fixnum))
                (inst lea temp-reg-tn (rip-relative-ea origin :code))
                ;; store CONS-CDR-SLOT
                (inst mov (ea (- 8 ,lowtag) ,tn) temp-reg-tn)
                ;; OR in a list-pointer tag if it was 0
                ,(if (eq lowtag 0) `(inst or :byte ,tn list-pointer-lowtag)))))

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
    #+ubsan (:temporary (:sc unsigned-reg) shadow)
    (:policy :fast-safe)
    (:node-var node)
    (:generator 100
      #+ubsan
      (when (require-shadow-bits)
        (zeroize shadow)
        ;; allocate a shadow vector unless this vector is simple-vector-T,
        ;; which can use unbound-marker as a poison value
        (when (sc-is type unsigned-reg)
          (inst cmp :byte type simple-vector-widetag)
          (inst jmp :e NO-SHADOW-BITS))
        (when (sc-is length any-reg) ; empty array needs no shadow bits
          (inst test :dword length length)
          (inst jmp :z NO-SHADOW-BITS))
        (let ((nbytes (calc-shadow-bits-size shadow)))
          (pseudo-atomic ()
            ;; Allocate a cons into RESULT temporarily to store the code and offset
            (allocation nil (* cons-size n-word-bytes) 0 node nil result)
            (alloc-origin-tracker result 0)
            ;; Allocate the shadow bits into SHADOW
            (allocation nil nbytes 0 node nil shadow)
            (inst mov :byte (ea shadow) simple-bit-vector-widetag)
            (inst mov :dword (vector-len-ea shadow 0)
                  (if (sc-is length immediate) (fixnumize (tn-value length)) length))
            ;; Store RESULT (the cons) into the extra slot of SHADOW
            (storew result shadow vector-length-slot 0)
            (inst or :byte shadow other-pointer-lowtag))))
      NO-SHADOW-BITS
      ;; The LET generates instructions that needn't be pseudoatomic
      ;; so don't move it inside.
      (let ((size (calc-size-in-bytes words result)))
        (instrument-alloc size node)
        (pseudo-atomic ()
         (allocation nil size 0 node nil result)
         (put-header result 0 type length t)
         (inst or :byte result other-pointer-lowtag)
         ;; Don't leave PA section until shadow bits are assigned,
         ;; so that GC can decide whether the result can be
         ;; relocated to an unbxoxed vs boxed page.
         #+ubsan
         (cond ((require-shadow-bits)
                ;; slight bug - if this was ":maybe" and then the vector is
                ;; simple-vector but compile-time-unknown, this will take the jump
                ;; to NO-SHADOW-BITS and then store a 0 without storing the PC.
                ;; But that's OK because it can be compile-time-unknown only if
                ;; called from %MAKE-ARRAY and a few other places which try to
                ;; insert the origin location their own by looking up the caller
                ;; of the current frame.
                (storew shadow result vector-length-slot other-pointer-lowtag))
               (poisoned ; uninitialized SIMPLE-VECTOR
                (allocation nil (* cons-size n-word-bytes) 0 node nil shadow)
                (alloc-origin-tracker shadow 0)
                ;; Store the cons into the spare slot of result
                (storew shadow result vector-length-slot other-pointer-lowtag)))))))

 (define-vop (allocate-vector-on-stack)
    #+ubsan (:info poisoned)
    (:args (type :scs (unsigned-reg immediate))
           (length :scs (any-reg immediate))
           (words :scs (any-reg immediate)))
    (:results (result :scs (descriptor-reg) :from :load))
    (:vop-var vop)
    (:node-var node)
    (:arg-types #+ubsan (:constant t)
                positive-fixnum positive-fixnum positive-fixnum)
    #+ubsan (:temporary (:sc any-reg :offset rax-offset) rax)
    #+ubsan (:temporary (:sc any-reg :offset rcx-offset) rcx)
    #+ubsan (:temporary (:sc any-reg :offset rdi-offset) rdi)
    (:policy :fast-safe)
    (:generator 10
      #+ubsan
      (cond
        ((require-shadow-bits)
         ;; allocate a vector of "written" bits unless the vector is simple-vector-T,
         ;; which can use unbound-marker as a poison value on reads.
         (when (sc-is type unsigned-reg) (bug "vector-on-stack: unknown type"))
         (zeroize rax)
         ;; Allocate a cons into RESULT temporarily to store the code and offset
         (stack-allocation result (* cons-size n-word-bytes) list-pointer-lowtag)
         (alloc-origin-tracker result list-pointer-lowtag)
         (let ((nbytes (calc-shadow-bits-size rcx)))
           (stack-allocation rdi nbytes 0 t)
           (when (sc-is length immediate) (inst mov rcx nbytes)))
         (inst rep)
         (inst stos :byte) ; RAX was zeroed
         (inst lea rax (ea other-pointer-lowtag rsp-tn))
         (inst mov :byte (ea (- other-pointer-lowtag) rax) simple-bit-vector-widetag)
         (inst mov :dword (vector-len-ea rax)
               (if (sc-is length immediate) (fixnumize (tn-value length)) length))
         ;; Store the origin in the shadow vector
         (storew result rax vector-length-slot other-pointer-lowtag))
        (poisoned
         ;; Allocate a cons into RAX to store the code and offset
         (stack-allocation rax (* cons-size n-word-bytes) list-pointer-lowtag)
         (alloc-origin-tracker rax list-pointer-lowtag))
        (t
         ;; Always need tostore something in the spare slot, otherwise it could
         ;; have a random value that looks like a pointer to shadow bits.
         (zeroize rax)))
      (let ((size (calc-size-in-bytes words result)))
        ;; Compute tagged pointer sooner than later since access off RSP
        ;; requires an extra byte in the encoding anyway.
        (stack-allocation result size other-pointer-lowtag
                          ;; If already aligned RSP, don't need to do it again.
                          #+ubsan (require-shadow-bits))
        ;; NB: store the trailing null BEFORE storing the header,
        ;; in case the length in words is 0, which stores into the LENGTH slot
        ;; as if it were element -1 of data (which probably can't happen).
        (store-string-trailing-null result type length words)
        ;; FIXME: It would be good to check for stack overflow here.
        (put-header result other-pointer-lowtag type length nil))
      #+ubsan
      (storew rax result vector-length-slot other-pointer-lowtag)))

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
        (stack-allocation result size other-pointer-lowtag)
        (store-string-trailing-null result type length words)
        ;; FIXME: It would be good to check for stack overflow here.
        (put-header result other-pointer-lowtag type length nil)
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
        (stack-allocation result size list-pointer-lowtag)
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
        (instrument-alloc size node)
        (pseudo-atomic ()
         (allocation 'list size list-pointer-lowtag node nil result)
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
  (:node-var node)
  (:generator 37
    (alloc-other result fdefn-widetag fdefn-size node)
    (storew name result fdefn-name-slot other-pointer-lowtag)
    (storew nil-value result fdefn-fun-slot other-pointer-lowtag)
    (storew (make-fixup 'undefined-tramp :assembly-routine)
            result fdefn-raw-addr-slot other-pointer-lowtag)))

(define-vop (make-closure)
  ; (:args (function :to :save :scs (descriptor-reg)))
  (:info label length stack-allocate-p)
  (:temporary (:sc any-reg) temp)
  (:results (result :scs (descriptor-reg)))
  (:node-var node)
  (:generator 10
   (let* ((words (+ length closure-info-offset)) ; including header
          (bytes (pad-data-block words))
          (header (logior (ash (1- words) n-widetag-bits) closure-widetag)))
     (unless stack-allocate-p
       (instrument-alloc bytes node))
     (pseudo-atomic (:elide-if stack-allocate-p)
       (allocation nil bytes fun-pointer-lowtag node stack-allocate-p result)
       (storew* #-immobile-space header ; write the widetag and size
                #+immobile-space        ; ... plus the layout pointer
                (progn (inst mov temp header)
                       (inst or temp #-sb-thread (static-symbol-value-ea 'function-layout)
                                     #+sb-thread
                                     (thread-slot-ea thread-function-layout-slot))
                       temp)
                result 0 fun-pointer-lowtag (not stack-allocate-p)))
     ;; Finished with the pseudo-atomic instructions
     (when label
       (inst lea temp (rip-relative-ea label (ash simple-fun-insts-offset word-shift)))
       (storew temp result closure-fun-slot fun-pointer-lowtag)
       #+metaspace
       (let ((origin (sb-assem::asmstream-data-origin-label sb-assem:*asmstream*)))
         (inst lea temp (rip-relative-ea origin :code))
         (storew temp result closure-code-slot fun-pointer-lowtag))))))

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
  (:node-var node)
  (:generator 50
   (let* ((instancep (typep type 'wrapper)) ; is this an instance type?
          (bytes (pad-data-block words)))
    (progn name) ; possibly not used
    (unless stack-allocate-p
      (instrument-alloc bytes node))
    (pseudo-atomic (:elide-if stack-allocate-p)
      ;; If storing a header word, defer ORing in the lowtag until after
      ;; the header is written so that displacement can be 0.
      (allocation nil bytes (if type 0 lowtag) node stack-allocate-p result)
      (when type
        (let* ((widetag (if instancep instance-widetag type))
               (header (compute-object-header words widetag)))
          (if (or #+compact-instance-header
                  (and (eq name '%make-structure-instance) stack-allocate-p))
              ;; Write a :DWORD, not a :QWORD, because the high half will be
              ;; filled in when the layout is stored. Can't use STOREW* though,
              ;; because it tries to store as few bytes as possible,
              ;; where this instruction must write exactly 4 bytes.
              (inst mov :dword (ea 0 result) header)
              (storew* header result 0 0 (not stack-allocate-p)))
          (inst or :byte result lowtag))))
    (when instancep ; store its layout
      (inst mov :dword (ea (+ 4 (- lowtag)) result)
            (make-fixup type :layout))))))

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
      (cond (stack-allocate-p
             (stack-allocation result bytes lowtag)
             (storew header result 0 lowtag))
            (t
             (instrument-alloc bytes node)
             (pseudo-atomic ()
              (allocation nil bytes lowtag node nil result)
              (storew header result 0 lowtag))))))

(macrolet ((c-call (name)
             `(let ((c-fun (make-fixup ,name :foreign)))
                (inst call (cond ((sb-c::code-immobile-p node) c-fun)
                                 (t (progn (inst mov temp-reg-tn c-fun)
                                           temp-reg-tn)))))))
#+immobile-space
(define-vop (alloc-immobile-fixedobj)
  (:args (size-class :scs (any-reg) :target c-arg1)
         (nwords :scs (any-reg) :target c-arg2)
         (header :scs (any-reg) :target c-arg3))
  (:temporary (:sc unsigned-reg :from (:argument 0) :to :eval :offset rdi-offset) c-arg1)
  (:temporary (:sc unsigned-reg :from (:argument 1) :to :eval :offset rsi-offset) c-arg2)
  (:temporary (:sc unsigned-reg :from (:argument 2) :to :eval :offset rdx-offset) c-arg3)
  (:temporary (:sc unsigned-reg :from :eval :to (:result 0) :offset rax-offset)
              c-result)
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
     (move result c-result))))

(define-vop (alloc-dynamic-space-code)
  (:args (total-words :scs (signed-reg) :target c-arg1))
  (:temporary (:sc unsigned-reg :from (:argument 0) :to :eval :offset rdi-offset) c-arg1)
  (:temporary (:sc unsigned-reg :from :eval :to (:result 0) :offset rax-offset)
              c-result)
  (:results (result :scs (descriptor-reg)))
  (:node-var node)
  (:generator 50
   (inst mov c-arg1 total-words)
   ;; RSP needn't be restored because the allocators all return immediately
   ;; which has that effect
   (inst and rsp-tn -16)
   (pseudo-atomic () (c-call "alloc_code_object"))
   ;; C-RESULT is a tagged ptr. MOV doesn't need to be inside the PSEUDO-ATOMIC.
   (inst mov result c-result)))

) ; end MACROLET
