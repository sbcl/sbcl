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

#+nil
(defun dontuse-test-should-use-smlgc (type)
  (let ((bit (type-to-bit type)))
    (unless bit
      (error "No bit for ~x~%" type))
    (inst test :dword (static-symbol-value-ea '*use-smlgc*) (ash 1 (1+ bit)))))

(defmacro jump-if-not-bitmap-alloc (label)
  `(progn
     (inst cmp :dword (thread-slot-ea (1+ thread-ap4-slot)) -1)
     (inst jmp :e ,label)))
(defmacro jump-if-bitmap-alloc (label)
  `(progn
     (inst cmp :dword (thread-slot-ea (1+ thread-ap4-slot)) -1)
     (inst jmp :ne ,label)))

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

;;; Emit counter increments for SB-APROF. SCRATCH-REGISTERS is either a TN
;;; or list of TNs that can be used to store into the profiling data.
;;; We pick one of the available TNs to use for addressing the data buffer.
;;; The TN that we pick can't be R12 because encoding it into an instruction
;;; always requires a SIB byte, which doesn't fit in the reserved bytes
;;; of the instruction stream where hot patching occurs.
;;; Compare:
;;;        F048FF4018       LOCK INC QWORD PTR [RAX+24]
;;;        F049FF442418     LOCK INC QWORD PTR [R12+24]
(defun instrument-alloc (type size node scratch-registers
                         &optional thread-temp
                         &aux (temp
                               (if (listp scratch-registers)
                                   (dolist (reg scratch-registers
                                                (first scratch-registers))
                                     (unless (location= reg r12-tn) (return reg)))
                                   scratch-registers)))
  (declare (ignorable type thread-temp))
  ;; Each allocation sequence has to call INSTRUMENT-ALLOC,
  ;; so we may as well take advantage of this fact to load the temp reg
  ;; here, if provided, rather than spewing more #+gs-seg tests around.
  #+gs-seg (when thread-temp (inst rdgsbase thread-temp))
  #+smlgc-telemetry (inst inc :qword (thread-slot-ea thread-ct-new-objects-slot)) ; TELEMETRY
  (when (member :allocation-size-histogram sb-xc:*features*)
    (let ((use-size-temp (not (typep size '(or (signed-byte 32) tn)))))
      ;; Sum up the sizes of boxed vs unboxed allocations.
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
             (assemble ()
               ;; optimistically assume it's a small object, so just divide
               ;; the size by the size of a cons to get a (1-based) index.
               (inst mov :dword temp size)
               (inst shr :dword temp (1+ word-shift))
               ;; now see if the computed index is in range
               (inst cmp size (* n-histogram-bins-small 16))
               (inst jmp :le OK)
               ;; oversized. Compute the log2 of the size
               (inst bsr :dword temp size)
               ;; array of counts ... | array of sizes ...
               (inst add :qword (ea (ash (+ thread-allocator-histogram-slot
                                            1
                                            (- first-large-histogram-bin-log2size)
                                            n-histogram-bins-small
                                            n-histogram-bins-large)
                                         word-shift)
                                    thread-tn temp 8)
                     size)
               ;; not sure why this is "2" and not "1" in the fudge factor!!
               ;; (but the assertions come out right)
               (inst add :dword temp
                     (+ (- first-large-histogram-bin-log2size) n-histogram-bins-small 2))
               OK
               (inst inc :qword (ea thread-segment-reg
                                    (ash (1- thread-allocator-histogram-slot) word-shift)
                                    thread-tn temp 8))))
            ((<= size (* sb-vm:cons-size sb-vm:n-word-bytes n-histogram-bins-small))
             (let ((index (1- (/ size (* sb-vm:cons-size sb-vm:n-word-bytes)))))
               (inst inc :qword (thread-slot-ea (+ thread-allocator-histogram-slot index)))))
            (t
             (let ((index (- (integer-length size) first-large-histogram-bin-log2size)))
               (inst add :qword (thread-slot-ea (+ thread-allocator-histogram-slot
                                                   n-histogram-bins-small
                                                   n-histogram-bins-large index))
                     size)
               (inst inc :qword (thread-slot-ea (+ thread-allocator-histogram-slot
                                                   n-histogram-bins-small index))))))))
  (when (policy node (> sb-c::instrument-consing 1))
    (when (tn-p size)
      (aver (not (location= size temp))))
    ;; CAUTION: the logic for RAX-SAVE is entirely untested
    ;; as it never gets exercised, and can not be, until R12 ceases
    ;; to have its wired use as the GC card table base register.
    (binding* (((data rax-save) (if (location= temp r12-tn)
                                    (values rax-tn t)
                                    (values temp nil)))
               (patch-loc (gen-label))
               (skip-instrumentation (gen-label)))
      ;; Don't count allocations to the arena
      (inst cmp :qword (thread-slot-ea thread-arena-slot) 0)
      (inst jmp :nz skip-instrumentation)
      (when rax-save (inst push rax-tn))
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
      (let ((helper
             (if (integerp size) 'enable-alloc-counter 'enable-sized-alloc-counter)))
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
      (when rax-save (inst pop rax-tn))
      (emit-label skip-instrumentation))))

;;; An arbitrary marker for the cons primitive-type, not to be confused
;;; with the CONS-TYPE in our type-algebraic sense. Mostly just informs
;;; the allocator to use cons_tlab.
(defconstant +cons-primtype+ list-pointer-lowtag)
;(defvar *cons-n-histo* #x501009B0)

;;; For async GC based on SML#
;;; See 'struct alloc_ptr' in heap_concurrent
;;; and 'struct sml_bitptr' in smlsharp.h
(defmacro with-bitmap-ap ((allocptr &optional thread-slot) &body body)
  (cond ((eq allocptr :thread)
         (aver thread-slot)
         ;; DISP has to remain as an expression so that the returned form
         ;; is toplevelish.
         (let ((disp `(ash ,thread-slot word-shift)))
           `(macrolet ((freebit.ptr () '(ea ,disp thread-tn))
                       (freebit.mask () '(ea (+ ,disp 8) thread-tn))
                       ;; the most-significant byte of the mask for CONS special cases
                       (freebit.mask-byte3 () '(ea (+ ,disp 11) thread-tn))
                       (freeptr () '(ea (+ ,disp 16) thread-tn))
                       (freeptr-fetch-and-add (result nbytes scratch)
                         `(progn
                            ;; XADD would do this, but C compilers don't use it
                            ;; except for an atomic fetch-and-add.
                            ;; the Agner Fog instruction latency tables seem to support that.
                            (inst mov ,result (freeptr))
                            (inst lea ,scratch (ea ,nbytes ,result))
                            (inst mov (freeptr) ,scratch))))
              ,@body)))
        (t
         (aver (not thread-slot))
         `(macrolet ((freebit.ptr () '(ea 0 ,allocptr))
                     (freebit.mask () '(ea 8 ,allocptr))
                     (freeptr () '(ea 16 ,allocptr))
                     (segment.blocksize () '(ea 24 ,allocptr)))
            ,@body))))

(defconstant unused-word-pattern #xffffffffdeadbeef)
(defun assert-word-unused (ea &aux (ok (gen-label)))
  (declare (ignore ea ok))
  #+nil
  (when t
    (inst cmp :qword ea unused-word-pattern)
    (inst jmp :eq OK)
    (inst break halt-trap)
    (emit-label OK)))
(defun check-alivep (obj lowtag &aux (ok (gen-label)))
  (declare (ignore obj lowtag ok))
  #+nil
  (unless (constant-tn-p obj)
    (inst cmp :qword (ea (- lowtag) obj) unused-word-pattern)
    (inst jmp :ne OK)
    (inst break halt-trap)
    (emit-label OK)))

(defmacro count-alloc (name)
  (declare (ignorable name))
  #+nil
  (let ((index
         (the (not null)
              (position name
                        '(:cons1 :cons1-slow
                          :cons2 :cons2-slow
                          :cons3 :cons3-slow
                          :cons4 :cons4-slow
                          :cons5 :cons5-slow
                          :cons6+ :dummy ; always "slow"
                          :barrier-store :barrier-cmpxchg
                          )))))
    ;; The first 16 elements are taken up by a histogram of lengths
    ;; of &REST args that we see - count and count-slow for each length
    ;; 1 through 15 and a catch-all bin.
    ;; But to make things more confusing, this is a vector of SB-VM:WORD
    ;; though &REST lists treat it as a vector of UNSIGNED-BYTE-32
    `(inst inc :lock :qword
           (ea ,(+ (cons-stats-v) (ash 16 word-shift) (* index 8))))))

(with-bitmap-ap (:thread thread-ap4-slot)
(defun bitmap-inline-alloc-list (num-conses alloc-tn temp done-label fallback)
  #+nil
  (inst inc :lock :qword
        (ea (+ (cons-stats-v) (ash 16 word-shift)
               ;; Use pairs of elements for consN, consN-slow. See above
               (* (1- num-conses) 16))))
  (let ((bmwordptr alloc-tn)
        (mask temp)
        (unavailable (gen-label)))
    ;; >1 cons has a pre-test for UNAVAILABLE in case the current mask + NUM-CONSES
    ;; would exceed the current bitmap word
    (ecase num-conses
      (1
       (inst mov bmwordptr (freebit.ptr))
       (inst mov :dword mask (freebit.mask))) ; MASK has the bit we want next
      (2
       (inst mov bmwordptr (freebit.ptr))
       (inst mov :dword mask (freebit.mask))
       (inst test :dword mask mask)
       ;; Fail if mask bit 31 is 1. We'd need to test bit 0 of the next bitmap word too,
       ;; so just use the slow path. Fast path occurs 96% of the time if bitmap word = 0.
       (inst jmp :s UNAVAILABLE) ; MASK*3 will overflow a :DWORD is sign bit is on
       (inst lea :dword mask (ea mask mask 2))) ; MASK = MASK * 3 (setting 2 bits)
      (3
       (inst mov :dword bmwordptr (freebit.mask))
       ;; Fail if bit 30 or 31 is 1.  Fast path occurs 93% of the time if bitmap word = 0.
       (inst test :dword bmwordptr #xC0000000)
       (inst jmp :nz UNAVAILABLE) ; MASK*7 would overflow :DWORD
       ;; The highest bit that could be on is bit index 29. The highest affected bit
       ;; could be index 32 after multiplying by 8. (Thus we need :QWORD operations)
       (inst lea mask (ea nil bmwordptr 8))
       (inst sub mask bmwordptr)) ; Set 3 bits by computing mask*7 as (8 x mask) - mask
      (4
       (inst mov :dword mask (freebit.mask))
       ;; Fail if bit 29, 30 or 31 is 1.  Fast path occurs 90% of the time if bitmap word = 0.
       (inst test :dword mask #xE0000000)
       (inst jmp :nz UNAVAILABLE) ; MASK*15 would overflow :DWORD
       ;; The highest bit that could be on is bit index 28
       (inst mov :dword bmwordptr mask)
       (inst shl mask 4)
       (inst sub mask bmwordptr)) ; Set 4 bits by computing mask*15 as (16 x mask) - mask
      (5
       (inst mov :dword mask (freebit.mask))
       (inst test :dword mask #xF0000000)
       (inst jmp :nz UNAVAILABLE) ; MASK*31 would overflow :DWORD
       ;; The highest bit that could be on is bit index 27
       (inst mov :dword bmwordptr mask)
       (inst shl mask 5)
       (inst sub mask bmwordptr))) ; Set 5 bits by computing mask*31 as (32 x mask) - mask
    ;;
    (when (> num-conses 2) (inst mov bmwordptr (freebit.ptr)))
    (inst test :dword (ea bmwordptr) mask)
    (inst jmp :nz UNAVAILABLE)
    ;; advance the bit
    (case num-conses
      (1
       (inst rol :dword mask 1)
       (inst mov :dword (freebit.mask) mask)) ; writeback
      (t ; DO NOT write back from the mask register, as it contains > 1 set bit
       (inst rol :dword (freebit.mask) num-conses)))
    ;; I tested whether branchless logic for incrementing freebit.ptr is preferable
    ;; to jumping over an add if the carry is clear, and it definitely is,
    ;; because the branch is not very predictable.
    (inst sbb :dword temp temp) ; broadcast the carry into low 32 bits
    (inst and :dword temp 4) ; = 0 or 4 depending on CF prior to SBB
    (inst add :qword (freebit.ptr) temp)
#|
    ;; Jmp over add is worse
    (inst jmp :nc done-label)
    (inst add :qword (freebit.ptr) 4)
|#
    (freeptr-fetch-and-add alloc-tn (* num-conses 16) temp)
    (dotimes (i num-conses) (assert-word-unused (ea (ash i word-shift) alloc-tn)))
    (inst jmp DONE-LABEL) ; success
    (emit-label unavailable)
    (inst cmp :dword (if (= num-conses 1) mask (freebit.mask)) -1)
    (inst jmp :ne FALLBACK) ; call into C for help with bitmap allocation
    ;; falllthrough goes to the gencgc allocator
    )))

(defun bitmap-call-alloc-list (things alloc-tn node num-conses star)
  (macrolet ((pop-arg () `(prog1 (tn-ref-tn things) (setf things (tn-ref-across things)))))
    (loop (list-ctor-push-elt (pop-arg) alloc-tn)
          (unless things (return))))
  (cond ((<= num-conses 5)
         (let ((fallback
                (aref (if star
                          #(bitmap-list*4-fallback bitmap-list*5-fallback bitmap-list*6-fallback)
                          #(bitmap-list3-fallback  bitmap-list4-fallback  bitmap-list5-fallback))
                      (- num-conses 3))))
           (invoke-asm-routine 'call fallback node)))
        (t
         (inst lea rax-tn (ea (* (+ num-conses (if star 0 -1)) n-word-bytes) rsp-tn))
         (inst mov rcx-tn num-conses)
         (invoke-asm-routine 'call (if star 'bitmap-listify* 'bitmap-listify) node)
         (inst add rsp-tn (* (+ num-conses (if star 1 0)) n-word-bytes))))
  (move alloc-tn rax-tn))

(defun bitmap-alloc (nbytes result-tn temps node done-label)
  (aver (<= 1 (length temps) 2))
  (let* ((log2size (the (integer 4 12) (integer-length (1- nbytes))))
         ;; each alloc_ptr consumes 4 words consecutively from the AP4 slot
         (thread-slot (+ thread-ap4-slot (* 4 (- log2size 4))))
         (fallback (gen-label))
         (unavailable (gen-label))
         (temp (car temps)))
    (with-bitmap-ap (:thread thread-slot)
      (inst mov temp (freebit.ptr))
      (inst mov :dword result-tn (freebit.mask))
      (inst test :dword (ea temp) result-tn) ; result-tn holds the desired bit
      (inst jmp :nz UNAVAILABLE) ; block is not available
      (inst rol :dword (freebit.mask) 1)
      (inst sbb :dword result-tn result-tn) ; broadcast the carry into low 32 bits
      (inst and :dword result-tn 4) ; = 0 or 4 depending on CF prior to SBB
      (inst add (freebit.ptr) result-tn)
      (freeptr-fetch-and-add result-tn (ash 1 log2size) temp)
      (inst jmp DONE-LABEL)
      (emit-label UNAVAILABLE)
      ;; if mask is all 1s then we are NOT using the bitmap allocator
      ;; if mask is NOT all 1s then we ARE using the bitmap allocator
      (inst cmp :dword result-tn -1) ; freebit.mask is in RESULT-TN
      (inst jmp :ne fallback))
    ;; fallthrough
    (assemble (:elsewhere)
      (emit-label fallback)
      (case log2size
        (4 (call-reg-specific-asm-routine node "" result-tn "-ALLOC16-FALLBACK"))
        (5 (call-reg-specific-asm-routine node "" result-tn "-ALLOC32-FALLBACK"))
        (6 (call-reg-specific-asm-routine node "" result-tn "-ALLOC64-FALLBACK"))
        ;; other sizes use a generalized fallback
        (t (inst lea result-tn (thread-slot-ea thread-slot))
           (call-reg-specific-asm-routine node "" result-tn "-ALLOC-FALLBACK")))
      (inst jmp DONE-LABEL))))

(define-vop (sb-c::end-pseudo-atomic)
  (:generator 1 (emit-end-pseudo-atomic)))

;;; Emit code to allocate an object with a size in bytes given by
;;; SIZE into ALLOC-TN. The size may be an integer of a TN.
;;; NODE may be used to make policy-based decisions.
;;; This function should only be used inside a pseudo-atomic section,
;;; which to the degree needed should also cover subsequent initialization.
;;;
;;; A mnemonic device for the argument pattern here:
;;; 1. what to allocate: type, size, lowtag describe the object
;;; 2. where to put the result
;;; 3. node (for determining immobile-space-p) and a scratch register or two
(defun gengc-alloc (type size lowtag alloc-tn node temp thread-temp
                    &key overflow
                    &aux (systemp (system-tlab-p type node)))
  (declare (ignorable thread-temp))
  (when overflow
    (aver (eql type +cons-primtype+)))
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
           (invoke-asm-routine
            'call
            (if systemp
                (if (eql type +cons-primtype+) 'sys-list-alloc-tramp 'sys-alloc-tramp)
                (if (eql type +cons-primtype+) 'list-alloc-tramp 'alloc-tramp))
            node)
           (inst pop alloc-tn)))
    (let* ((NOT-INLINE (gen-label))
           (DONE (gen-label))
           (free-pointer #+sb-thread
                         (let ((slot (if systemp
                                         (if (eql type +cons-primtype+)
                                             thread-sys-cons-tlab-slot
                                             thread-sys-mixed-tlab-slot)
                                         (if (eql type +cons-primtype+)
                                             thread-cons-tlab-slot
                                             thread-mixed-tlab-slot))))
                           (thread-slot-ea slot #+gs-seg thread-temp))
                         #-sb-thread
                         (ea (+ static-space-start
                                (if (eql type +cons-primtype+)
                                    cons-region-offset
                                    mixed-region-offset))))
           (end-addr (ea (sb-x86-64-asm::ea-segment free-pointer)
                         (+ n-word-bytes (ea-disp free-pointer))
                         (ea-base free-pointer))))
      (cond ((typep size `(integer ,large-object-size))
             ;; large objects will never be made in a per-thread region
             (cond (overflow (funcall overflow))
                   (t (fallback size)
                      (when (/= lowtag 0) (inst or :byte alloc-tn lowtag)))))
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
               (cond (overflow (funcall overflow))
                     (t (fallback temp)
                        (inst jmp DONE)))))
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
                           (let ((bias (+ (- size) lowtag)))
                             (if (= bias -1) (inst dec alloc-tn) (inst add alloc-tn bias)))
                           (emit-label DONE)))))
             (assemble (:elsewhere)
               (emit-label NOT-INLINE)
               (cond (overflow (funcall overflow))
                     (t
                      (fallback size)
                      (when (and (/= lowtag 0) (not temp) (not (tn-p size)))
                        (inst or :byte alloc-tn lowtag))
                      (inst jmp DONE))))))))

  t)

(defun allocation (type size lowtag alloc-tn node temps thread-temp
                   &key (try-bitmap-alloc t)
                   &aux (saved-reg
                         (when (and try-bitmap-alloc (not temps))
                           (if (location= alloc-tn rax-tn) rcx-tn rax-tn)))
                        (temps (ensure-list temps)))
  ;; Conses and variable-sized or large allocations are
  ;; handled outside of this function if using SML# allocator
  (when saved-reg
    (inst push saved-reg))
  (assemble ()
    (when try-bitmap-alloc
      (bitmap-alloc size alloc-tn (or temps (list saved-reg)) node DONE))
    ;; fallthrough
    (gengc-alloc type size lowtag alloc-tn node (or (first temps) saved-reg)
                 thread-temp)
    DONE
    ;; FIXME: there's redundancy here, because a single OR instruction
    ;; should be part of both code flows.
    (unless (= lowtag 0) (inst or :byte alloc-tn lowtag)))
  (when saved-reg
    (inst pop saved-reg)))

;;; Allocate an other-pointer object of fixed NWORDS with a single-word
;;; header having the specified WIDETAG value. The result is placed in
;;; RESULT-TN.  NWORDS counts the header word.
(defun alloc-other (widetag nwords result-tn node alloc-temps thread-temp
                    &optional init
                    &aux (bytes (pad-data-block nwords)))
  (declare (ignorable thread-temp))
  (declare (dynamic-extent init))
  #+bignum-assertions
  (when (= widetag bignum-widetag) (setq bytes (* bytes 2))) ; use 2x the space
  (aver (<= bytes smlgc-blocksize-max))
  (instrument-alloc widetag bytes node (cons result-tn (ensure-list alloc-temps)) thread-temp)
  (let ((header (compute-object-header nwords widetag)))
    ;; if the object is cons-sized (2 words) then the page fill byte is 0xff
    ;; so we have to write the entire header word. If more than 2 words, the fill byte is 0.
    (pseudo-atomic ()
      (cond (alloc-temps
             (allocation widetag bytes 0 result-tn node alloc-temps thread-temp)
             (storew* header result-tn 0 0 (> nwords 2))
             (inst or :byte result-tn other-pointer-lowtag))
            (t
             (allocation widetag bytes other-pointer-lowtag result-tn node nil thread-temp)
             (storew* header result-tn 0 other-pointer-lowtag (> nwords 2))))
      (when init
        (funcall init)))))

(defun list-ctor-push-elt (x scratch)
  (inst push (if (sc-is x immediate)
                 (let ((bits (encode-value-if-immediate x)))
                   (or (plausible-signed-imm32-operand-p bits)
                       (progn (inst mov scratch bits) scratch)))
                 x)))

;;;; CONS, ACONS, LIST and LIST*
(macrolet ((pop-arg (ref)
             `(prog1 (tn-ref-tn ,ref) (setf ,ref (tn-ref-across ,ref))))
           (store-slot (arg list &optional (slot cons-car-slot)
                                           (lowtag list-pointer-lowtag))
             ;; TODO: gencgc does not need EMIT-GC-STORE-BARRIER here,
             ;; but other other GC strategies might.
             `(let* (immediate-value
                     (tn ,arg)
                     (reg
                       (sc-case tn
                         (constant
                          (unless (and (constant-tn-p tn)
                                       (eql prev-constant (tn-value tn)))
                            (setf prev-constant (if (constant-tn-p tn)
                                                    (tn-value tn)
                                                    temp))
                            (move temp tn))
                          temp)
                         (immediate
                          (if (eql prev-constant (setf immediate-value (tn-value tn)))
                              temp
                              (encode-value-if-immediate tn)))
                         (control-stack
                          (setf prev-constant temp) ;; a non-eq initial value
                          (move temp tn)
                          temp)
                         (t
                          tn))))
                (when (eq (storew reg ,list ,slot ,lowtag temp)
                          temp)
                  (setf prev-constant immediate-value)))))

(define-vop (cons)
  (:args (car :scs (any-reg descriptor-reg constant immediate control-stack))
         (cdr :scs (any-reg descriptor-reg constant immediate control-stack)))
  (:temporary (:sc unsigned-reg :to (:result 0) :target result) alloc)
  ;; TEMP does not have to be wired, because the fallback does not destroy
  ;; any registers
  (:temporary (:sc unsigned-reg :to (:result 0)
               :unused-if (node-stack-allocate-p (sb-c::vop-node vop)))
              temp)
  (:results (result :scs (descriptor-reg)))
  #+gs-seg (:temporary (:sc unsigned-reg :offset 15) thread-tn)
  (:vop-var vop)
  (:node-var node)
  (:generator 10
    (cond
      ((node-stack-allocate-p node)
       (inst and rsp-tn (lognot lowtag-mask))
       (cond ((and (sc-is car immediate) (sc-is cdr immediate)
                   (typep (encode-value-if-immediate car) '(signed-byte 8))
                   (typep (encode-value-if-immediate cdr) '(signed-byte 8)))
              ;; (CONS 0 0) takes just 4 bytes to encode the PUSHes (for example)
              (inst push (encode-value-if-immediate cdr))
              (inst push (encode-value-if-immediate car)))
             ((and (sc-is car immediate) (sc-is cdr immediate)
                   (eql (encode-value-if-immediate car) (encode-value-if-immediate cdr)))
              (inst mov alloc (encode-value-if-immediate cdr))
              (inst push alloc)
              (inst push alloc))
             (t
              (list-ctor-push-elt cdr alloc)
              (list-ctor-push-elt car alloc)))
       (inst lea result (ea list-pointer-lowtag rsp-tn)))
      (t
       (let ((nbytes (* cons-size n-word-bytes))
             (prev-constant temp) ;; a non-eq initial value
             (fallback (gen-label)))
         (instrument-alloc +cons-primtype+ nbytes node (list temp alloc) thread-tn)
         (pseudo-atomic (:thread-tn thread-tn)
           (bitmap-inline-alloc-list 1 alloc temp CONTINUE FALLBACK)
           (assemble (:elsewhere)
             (emit-label fallback)
             (call-reg-specific-asm-routine node "BITMAP-CONS-TO-" alloc "-FALLBACK")
             (inst jmp CONTINUE))
           ;; pointer bump allocation
           (gengc-alloc +cons-primtype+ nbytes 0 alloc node temp thread-tn)
           CONTINUE
           (store-slot car alloc cons-car-slot 0)
           (store-slot cdr alloc cons-cdr-slot 0)
           (if (location= alloc result)
               (inst or :byte alloc list-pointer-lowtag)
               (inst lea result (ea list-pointer-lowtag alloc)))))))))

#+nil
(define-vop (acons)
  (:args (key :scs (any-reg descriptor-reg constant immediate control-stack))
         (val :scs (any-reg descriptor-reg constant immediate control-stack))
         (tail :scs (any-reg descriptor-reg constant immediate control-stack)))
  (:temporary (:sc unsigned-reg :to (:result 0)) alloc)
  (:temporary (:sc unsigned-reg :to (:result 0) :target result) temp)
  (:results (result :scs (descriptor-reg)))
  #+gs-seg (:temporary (:sc unsigned-reg :offset 15) thread-tn)
  (:node-var node)
  (:translate acons)
  (:policy :fast-safe)
  (:generator 10
    (let ((nbytes (* cons-size 2 n-word-bytes))
          (prev-constant temp))
      (instrument-alloc +cons-primtype+ nbytes node (list temp alloc) thread-tn)
      (pseudo-atomic (:thread-tn thread-tn)
        (gengc-alloc +cons-primtype+ nbytes 0 alloc node temp thread-tn)
        (store-slot tail alloc cons-cdr-slot 0)
        (inst lea temp (ea (+ 16 list-pointer-lowtag) alloc))
        (store-slot temp alloc cons-car-slot 0)
        (setf prev-constant temp)
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
  (:args (car :scs (any-reg descriptor-reg constant immediate control-stack))
         (cadr :scs (any-reg descriptor-reg constant immediate control-stack))
         (cddr :scs (any-reg descriptor-reg constant immediate control-stack)))
  (:temporary (:sc unsigned-reg :to (:result 0) :target result) alloc)
  ;; TEMP has to be wired because the fallback returns a value in RAX
  ;; (can we return it on the stack instead?)
  (:temporary (:sc unsigned-reg :to (:result 0) :offset rax-offset
               :unused-if (node-stack-allocate-p (sb-c::vop-node vop)))
              temp)
  (:results (result :scs (descriptor-reg)))
  #+gs-seg (:temporary (:sc unsigned-reg :offset 15) thread-tn)
  (:vop-var vop)
  (:node-var node)
  (:generator 10
    (cond
      ((node-stack-allocate-p node)
       (inst and rsp-tn (lognot lowtag-mask))
       (list-ctor-push-elt cddr alloc)
       (list-ctor-push-elt cadr alloc)
       (inst lea alloc (ea list-pointer-lowtag rsp-tn))
       (inst push alloc) ; cdr of the first cons
       (list-ctor-push-elt car alloc)
       (inst lea result (ea list-pointer-lowtag rsp-tn)))
      (t
       (let ((nbytes (* 2 cons-size n-word-bytes))
             (prev-constant temp)
             (fallback (gen-label)))
         (instrument-alloc +cons-primtype+ nbytes node (list temp alloc) thread-tn)
         (pseudo-atomic (:thread-tn thread-tn)
           (bitmap-inline-alloc-list 2 alloc temp CONTINUE FALLBACK)
           (assemble (:elsewhere)
             (emit-label fallback)
             (list-ctor-push-elt car temp)
             (list-ctor-push-elt cadr temp)
             (cond ((and (constant-tn-p cddr) (eq (tn-value cddr) nil))
                    (invoke-asm-routine 'call 'bitmap-list2-fallback node))
                   (t
                    (list-ctor-push-elt cddr temp)
                    (invoke-asm-routine 'call 'bitmap-list*3-fallback node)))
             (move result temp)
             (inst jmp done))
           ;; pointer bump allocation
           (gengc-alloc +cons-primtype+ nbytes 0 alloc node temp thread-tn)
           CONTINUE
           (store-slot car alloc cons-car-slot 0)
           (store-slot cadr alloc (+ 2 cons-car-slot) 0)
           (store-slot cddr alloc (+ 2 cons-cdr-slot) 0)
           (inst lea temp (ea (+ 16 list-pointer-lowtag) alloc))
           (store-slot temp alloc cons-cdr-slot 0)
           (if (location= alloc result)
               (inst or :byte alloc list-pointer-lowtag)
               (inst lea result (ea list-pointer-lowtag alloc)))
           DONE))))))

(define-vop (list)
  (:args (things :more t :scs (descriptor-reg any-reg constant immediate)))
  (:temporary (:sc unsigned-reg :offset rcx-offset) ptr)
  (:temporary (:sc unsigned-reg :offset rax-offset) temp)
  (:temporary (:sc unsigned-reg :to (:result 0) :target result) res)
  #+gs-seg (:temporary (:sc unsigned-reg :offset 15) thread-tn)
  (:info star cons-cells)
  (:results (result :scs (descriptor-reg)))
  (:node-var node)
  (:generator 0
    (aver (>= cons-cells 3)) ; prevent regressions in ir2tran's vop selection
    (let* ((stack-allocate-p (node-stack-allocate-p node))
           (size (* (pad-data-block cons-size) cons-cells))
           (prev-constant temp))
      (unless stack-allocate-p
        (instrument-alloc +cons-primtype+ size node (list ptr temp) thread-tn))
      (pseudo-atomic (:elide-if stack-allocate-p :thread-tn thread-tn)
        (cond
          (stack-allocate-p
           (stack-allocation size list-pointer-lowtag res))
          (t
           (let ((linear-alloc (gen-label)) (fallback (gen-label)))
             (cond
               ((> cons-cells 5)
                (jump-if-not-bitmap-alloc LINEAR-ALLOC)
                ;; Don't bother trying to get contiguous bits- success rate is too low
                (count-alloc :cons6+)
                (bitmap-call-alloc-list things res node cons-cells star)
                (inst jmp DONE))
               (t
                ;; emit the fallback first because THINGS gets popped later
                (assemble (:elsewhere)
                  (emit-label fallback)
                  (bitmap-call-alloc-list things res node cons-cells star)
                  (inst jmp DONE))
                (bitmap-inline-alloc-list cons-cells res temp CONTINUE FALLBACK)))
             (emit-label LINEAR-ALLOC)
             (gengc-alloc +cons-primtype+ size 0 res node temp thread-tn))))
        CONTINUE
        (unless stack-allocate-p (inst or :byte res list-pointer-lowtag))
        (move ptr res)
        (dotimes (i (1- cons-cells))
          (store-slot (pop-arg things) ptr)
          (inst add ptr (pad-data-block cons-size))
          (storew ptr ptr (- cons-cdr-slot cons-size) list-pointer-lowtag))
        (store-slot (pop-arg things) ptr cons-car-slot list-pointer-lowtag)
        (if star
            (store-slot (pop-arg things) ptr cons-cdr-slot list-pointer-lowtag)
            (storew nil-value ptr cons-cdr-slot list-pointer-lowtag))
        DONE))
    (aver (null things))
    (move result res)))
)

;;;; special-purpose inline allocators

;;; Zeroed storew - possibly use a shorter encoding if storing
;;; to a word that was initialized with 0-fill.
;;; This will always write 8 bytes if WORD is a negative number.
(defun storew* (word object slot lowtag zeroed &optional temp)
  (setq zeroed nil)
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
             (declare (ignore zeroed))
             `(let ((len (if (sc-is ,len immediate) (fixnumize (tn-value ,len)) ,len))
                    (type (if (sc-is ,type immediate) (tn-value ,type) ,type)))
                (storew* type ,vector-tn 0 ,lowtag #|,zeroed|# nil ,temp)
                #+ubsan (inst mov :dword (vector-len-ea ,vector-tn ,lowtag) len)
                #-ubsan (storew* len ,vector-tn vector-length-slot
                                       ,lowtag #|,zeroed|# nil ,temp)))
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
    ;; Wiring RAX as the temp is for the bitmap allocator call
    (:temporary (:sc unsigned-reg :offset rax-offset) temp)
    (:temporary (:sc complex-double-reg :offset 7) header-temp)
    #+gs-seg (:temporary (:sc unsigned-reg :offset 15) thread-tn)
    (:policy :fast-safe)
    (:node-var node)
    (:arg-refs args-ref)
    (:ignore header-temp)
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
            (allocation simple-bit-vector-widetag nbytes 0 result node temp nil)
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
      (binding*
          ((fixed-size (if (sc-is words immediate)
                           (pad-data-block (+ (tn-value words) vector-data-offset))))
           (maybe-large (or (not fixed-size) (> fixed-size smlgc-blocksize-max)))
           ((size-tn instrumentation-temp alloc-temp)
            (cond
                (fixed-size
                 ;; If WORDS is immediate, then let INSTRUMENT-ALLOC choose its temp
                 (values fixed-size (list result temp) temp))
                ((location= temp r12-tn)
                 ;; Compute the size into TEMP, use RESULT for instrumentation.
                 ;; Don't give another temp to ALLOCATION, because its SIZE and temp
                 ;; can not be in the same register (which it AVERs).
                 (values (calc-size-in-bytes words temp) result nil))
                (t
                 ;; Compute the size into RESULT, use TEMP for instrumentation.
                 ;; ALLOCATION needs the temp register in this case,
                 ;; because input and output are in the same register.
                 (values (calc-size-in-bytes words result) temp temp)))))
        (instrument-alloc (if (sc-is type immediate)
                              (case (tn-value type)
                                (#.simple-vector-widetag 'simple-vector)
                                (t 'unboxed-array))
                              type)
                          size-tn node instrumentation-temp thread-tn)
        ;; Same concept as in var-alloc
        (when maybe-large
          (assemble ()
            (jump-if-not-bitmap-alloc LINEAR-ALLOC)
            ;; MOVDQU can load and store the object header from these 2 words
            ;; I pity the fool whose uses an immediate so large
            ;; that's it not encodable as a PUSH operand.
            (inst push (encode-value-if-immediate length))
            (inst push (encode-value-if-immediate type nil)) ; untagged
            (inst mov temp size-tn) ; TEMP = RAX
            (invoke-asm-routine 'call 'bitmap-vect-alloc node)
            (move result temp)
            (inst jmp DONE)
            LINEAR-ALLOC))
        (pseudo-atomic (:thread-tn thread-tn)
         (allocation (if (sc-is type immediate) (tn-value type) 'vector)
                     size-tn 0 result node (list alloc-temp) thread-tn
                     :try-bitmap-alloc (not maybe-large))
         (put-header result 0 type length t alloc-temp)
         (inst or :byte result other-pointer-lowtag)))
      DONE
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
    (:node-var node)
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
        (when (sb-c::make-vector-check-overflow-p node)
          (let ((overflow (generate-error-code vop 'stack-allocated-object-overflows-stack-error size)))
            (inst sub rsp-tn size)
            (inst cmp :qword rsp-tn (thread-slot-ea thread-control-stack-start-slot))
            ;; avoid clearing condition codes
            (inst lea rsp-tn (if (integerp size)
                                 (ea size rsp-tn)
                                 (ea rsp-tn size)))
            (inst jmp :be overflow)))
        ;; Compute tagged pointer sooner than later since access off RSP
        ;; requires an extra byte in the encoding anyway.
        (stack-allocation size other-pointer-lowtag result
                          ;; If already aligned RSP, don't need to do it again.
                          #+ubsan (want-shadow-bits))
        ;; NB: store the trailing null BEFORE storing the header,
        ;; in case the length in words is 0, which stores into the LENGTH slot
        ;; as if it were element -1 of data (which probably can't happen).
        (store-string-trailing-null result type length words)
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
#+nil
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
    (:node-var node)
    (:vop-var vop)
    (:temporary (:sc descriptor-reg) tail next limit)
    (:generator 20
      (let ((size (calc-size-in-bytes length next))
            (loop (gen-label)))
        (when (sb-c::make-list-check-overflow-p node)
          (let ((overflow (generate-error-code vop 'stack-allocated-object-overflows-stack-error size)))
            (inst sub rsp-tn size)
            (inst cmp :qword rsp-tn (thread-slot-ea thread-control-stack-start-slot))
            ;; avoid clearing condition codes
            (inst lea rsp-tn (ea rsp-tn size))
            (inst jmp :be overflow)))
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

  #+nil ; FIXME
  (define-vop (allocate-list-on-heap)
    (:args (length :scs (any-reg immediate))
           ;; Too bad we don't have an SC that implies actually a CPU immediate
           ;; i.e. fits in an imm32 operand
           (element :scs (any-reg descriptor-reg)))
    (:results (result :scs (descriptor-reg) :from :load))
    (:arg-types positive-fixnum *)
    (:policy :fast-safe)
    (:node-var node)
    ;; These are need for the bitmap allocator
    (:temporary (:sc unsigned-reg :offset rcx-offset :from (:argument 0)) rcx)
    (:temporary (:sc unsigned-reg :offset rax-offset :from (:argument 1) :to :result) rax)
    ;; Too many temps. Oh well.
    (:temporary (:sc descriptor-reg) tail next limit)
    #+gs-seg (:temporary (:sc unsigned-reg :offset 15) thread-tn)
    (:generator 20
      (unless (sc-is length immediate)
        (inst test length length)
        (inst jmp :nz continue)
        (inst mov result nil-value)
        (inst jmp out))
      continue
      (unless (sc-is length immediate) (move rcx length))
      (move rax element)
      (let ((nbytes (cond ((sc-is length immediate)
                           (* (tn-value ,length) n-word-bytes 2))
                          (t
                           (inst shl rcx (1+ (- word-shift n-fixnum-tag-bits)))
                           rcx))))
        (instrument-alloc +cons-primtype+ nbytes node (list next limit) thread-tn))
      (let ((size (calc-size-in-bytes length tail))
            (entry (gen-label))
            (loop (gen-label))
            (native (gen-label))
            (leave-pa (gen-label)))
        (instrument-alloc +cons-primtype+ size node (list next limit) thread-tn)
        (pseudo-atomic (:thread-tn thread-tn)
         (test-should-use-smlgc :make-list)
         (inst jmp :z native)
         ;; SML# allocator, cons-at-a-time
         (emit-label native)
         (gengc-alloc +cons-primtype+ size list-pointer-lowtag result node
                     limit thread-tn
                     :overflow
                     (lambda ()
                       ;; Push C call args right-to-left
                       (inst push (if (integerp size) (constantize size) size))
                       (inst push (if (sc-is element immediate) (tn-value element) element))
                       (invoke-asm-routine
                        'call (if (system-tlab-p 0 node) 'sys-make-list 'make-list) node)
                       (inst pop result)
                       (inst jmp leave-pa)))
         (compute-end)
         (inst mov next result)
         (inst jmp entry)
         (emit-label LOOP)
         (storew next tail cons-cdr-slot list-pointer-lowtag)
         (emit-label ENTRY)
         (inst mov tail next)
         (inst add next (* 2 n-word-bytes))
         (storew element tail cons-car-slot list-pointer-lowtag)
         (inst cmp next limit)
         (inst jmp :ne loop)
         ;; still pseudo-atomic
         (storew nil-value tail cons-cdr-slot list-pointer-lowtag)
         (emit-label leave-pa)))
      done))) ; label needed by calc-size-in-bytes

#-immobile-space
(define-vop (make-fdefn)
  (:policy :fast-safe)
  (:translate make-fdefn)
  (:args (name :scs (descriptor-reg) :to :eval))
  (:results (result :scs (descriptor-reg) :from :argument))
  #+gs-seg (:temporary (:sc unsigned-reg :offset 15) thread-tn)
  (:node-var node)
  (:temporary (:sc unsigned-reg) temp temp2)
  (:generator 37
    (alloc-other fdefn-widetag fdefn-size result node (list temp temp2) thread-tn
      (lambda ()
        (storew name result fdefn-name-slot other-pointer-lowtag)
        (storew nil-value result fdefn-fun-slot other-pointer-lowtag)
        (storew (make-fixup 'undefined-tramp :assembly-routine)
                result fdefn-raw-addr-slot other-pointer-lowtag)))))

(define-vop (make-closure)
  (:info label length stack-allocate-p)
  (:temporary (:sc any-reg) temp)
  #+gs-seg (:temporary (:sc unsigned-reg :offset 15) thread-tn)
  (:results (result :scs (descriptor-reg)))
  (:node-var node)
  (:vop-var vop)
  (:generator 10
    (aver (>= length 1))
    (let* ((words (+ length closure-info-offset)) ; including header
           (bytes (pad-data-block words))
           (header (logior (ash (1- words) n-widetag-bits) closure-widetag))
           (remain-pseudo-atomic
            (eq (car (last (vop-codegen-info vop))) :pseudo-atomic)))
      (unless stack-allocate-p
        (instrument-alloc closure-widetag bytes node (list result temp) thread-tn))
      (pseudo-atomic (:default-exit (not remain-pseudo-atomic)
                      :elide-if stack-allocate-p :thread-tn thread-tn)
        (if stack-allocate-p
            (stack-allocation bytes fun-pointer-lowtag result)
            (allocation closure-widetag bytes fun-pointer-lowtag result node (list temp) thread-tn))
        (storew* #-compact-instance-header header ; write the widetag and size
                 #+compact-instance-header        ; ... plus the layout pointer
                 (let ((layout #-sb-thread (static-symbol-value-ea 'function-layout)
                               #+sb-thread (thread-slot-ea thread-function-layout-slot)))
                   (cond ((typep header '(unsigned-byte 16))
                          (inst mov temp layout)
                          ;; emit a 2-byte constant, the low 4 of TEMP were zeroed
                          (inst mov :word temp header))
                         (t
                          (inst mov temp header)
                          (inst or temp layout)))
                   temp)
                 result 0 fun-pointer-lowtag (not stack-allocate-p))
        (inst lea temp (rip-relative-ea label (ash simple-fun-insts-offset word-shift)))
        (storew temp result closure-fun-slot fun-pointer-lowtag)))))

;;; The compiler likes to be able to directly make value cells.
(define-vop (make-value-cell)
  (:args (value :scs (descriptor-reg any-reg immediate constant) :to :result))
  (:results (result :scs (descriptor-reg) :from :eval))
  #+gs-seg (:temporary (:sc unsigned-reg :offset 15) thread-tn)
  (:info stack-allocate-p)
  (:node-var node)
  (:temporary (:sc unsigned-reg) temp temp2)
  (:generator 10
    (let ((data (if (sc-is value immediate)
                    (let ((bits (encode-value-if-immediate value)))
                      (if (integerp bits)
                          (constantize bits)
                          bits)) ; could be a fixup
                    value)))
      (cond (stack-allocate-p
             ;; No regression test got here. Therefore I think there's no such thing as a
             ;; dynamic-extent value cell. It makes sense that there isn't: DX closures
             ;; would just reference their frame, wouldn't they?
             (inst and rsp-tn (lognot lowtag-mask)) ; align
             (inst push data)
             (inst push (compute-object-header value-cell-size value-cell-widetag))
             (inst lea result (ea other-pointer-lowtag rsp-tn)))
            (t
             (alloc-other
              value-cell-widetag value-cell-size result node
              (list temp temp2) thread-tn
              (lambda ()
                ;; FIXME: use the temp instead of push/pop
                (if (sc-case value
                     (immediate
                      (unless (integerp data) (inst push data) t))
                     (constant
                      (inst push value) t)
                     (t nil))
                    (inst pop (object-slot-ea result value-cell-value-slot other-pointer-lowtag))
                    (storew data result value-cell-value-slot other-pointer-lowtag)))))))))

;;;; automatic allocators for primitive objects

(define-vop (make-funcallable-instance-tramp)
  (:args)
  (:results (result :scs (any-reg)))
  (:vop-var vop)
  (:generator 1
    ;; gets "... is not the name of a defined VOP." if not defined at all
    #+compact-instance-header (bug "Shouldn't get here")
    (inst mov result (make-fixup 'funcallable-instance-tramp :assembly-routine))))

(flet
  ((alloc (vop name words type lowtag stack-allocate-p result
                    &optional temps node
                    &aux (bytes (pad-data-block words))
                         (remain-pseudo-atomic
                          (eq (car (last (vop-codegen-info vop))) :pseudo-atomic)))
    #+bignum-assertions
    (when (eq type bignum-widetag) (setq bytes (* bytes 2))) ; use 2x the space
    (aver (<= bytes smlgc-blocksize-max))
    (progn name) ; possibly not used
    (unless stack-allocate-p
      (instrument-alloc type bytes node (cons result temps) thread-tn))
    (pseudo-atomic (:default-exit (not remain-pseudo-atomic)
                    :elide-if stack-allocate-p :thread-tn thread-tn)
      ;; If storing a header word, defer ORing in the lowtag until after
      ;; the header is written so that displacement can be 0.
      (cond (stack-allocate-p
             (stack-allocation bytes (if type 0 lowtag) result))
            ((eql type funcallable-instance-widetag)
             (bitmap-alloc bytes result temps node ALLOCATED)
             (inst push bytes)
             (invoke-asm-routine 'call 'alloc-funinstance vop)
             (inst pop result))
            (t
             (allocation type bytes (if type 0 lowtag) result node temps thread-tn)))
      ALLOCATED
      (let ((header (compute-object-header words type)))
        (cond #+compact-instance-header
              ((and (eq name '%make-structure-instance) stack-allocate-p)
              ;; Write a :DWORD, not a :QWORD, because the high half will be
              ;; filled in when the layout is stored. Can't use STOREW* though,
              ;; because it tries to store as few bytes as possible,
              ;; where this instruction must write exactly 4 bytes.
               (inst mov :dword (ea 0 result) header))
              (t
               (storew* header result 0 0 (and (not stack-allocate-p)
                                               (> words 2))))))
      ;; GC can make the best choice about placement if it has a layout.
      ;; Of course with conservative GC the object will be pinned anyway,
      ;; but still, always having a layout is a good thing.
      (when (typep type 'layout) ; store its layout, while still in pseudo-atomic
        (inst mov :dword (ea 4 result) (make-fixup type :layout)))
      (inst or :byte result lowtag))))
  ;; DX is strictly redundant in these 2 vops, but they're written this way
  ;; so that backends can choose to use a single vop for both.
  (define-vop (fixed-alloc)
    (:info name words type lowtag dx)
    (:results (result :scs (descriptor-reg)))
    (:temporary (:sc unsigned-reg) temp1 temp2)
    #+gs-seg (:temporary (:sc unsigned-reg :offset 15) thread-tn)
    (:vop-var vop)
    (:node-var node)
    (:generator 50 (alloc vop name words type lowtag dx result (list temp1 temp2) node)))
  (define-vop (sb-c::fixed-alloc-to-stack)
    (:info name words type lowtag dx)
    (:results (result :scs (descriptor-reg)))
    (:vop-var vop)
    (:generator 50 (alloc vop name words type lowtag dx result))))

;;; Allocate a non-vector variable-length object.
;;; Exactly 4 allocators are rendered via this vop:
;;;  BIGNUM               (%ALLOCATE-BIGNUM)
;;;  FUNCALLABLE-INSTANCE (%MAKE-FUNCALLABLE-INSTANCE)
;;;  CLOSURE              (%ALLOC-CLOSURE)
;;;  INSTANCE             (%MAKE-INSTANCE,%MAKE-INSTANCE/MIXED)
;;; WORDS accounts for the mandatory slots *including* the header.
;;; EXTRA is the variable payload, also measured in words.
(define-vop (var-alloc)
  (:args (extra :scs (any-reg)))
  (:arg-types positive-fixnum)
  (:info name words type lowtag stack-allocate-p)
  (:results (result :scs (descriptor-reg) :from (:eval 1)))
  (:temporary (:sc unsigned-reg :from :eval :to (:eval 1)) bytes)
  (:temporary (:sc unsigned-reg :from :eval :to :result) header)
  ;; alloc-temp is a passing register for the ASM routine
  (:temporary (:sc unsigned-reg :offset rax-offset) alloc-temp)
  #+gs-seg (:temporary (:sc unsigned-reg :offset 15) thread-tn)
  (:node-var node)
  (:vop-var vop)
  (:generator 50
   (when (eq name '%make-funcallable-instance)
     ;; %MAKE-FUNCALLABLE-INSTANCE needs to allocate to pages of code,
     ;; which it failed to do if the var-alloc translation was invoked.
     ;; But it seems we never need this! (so is it FIXME or isn't it?)
     (error "can't %MAKE-FUNCALLABLE-INSTANCE of unknown length"))
   (progn ; let ((remain-pseudo-atomic (eq (car (last (vop-codegen-info vop))) :pseudo-atomic)))
     ; (declare (ignore remain-pseudo-atomic))
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
            (assemble ()
             ;; can't pass RESULT as a possible choice of scratch register
             ;; because it might be in the same physical reg as BYTES.
             ;; Yup, the lifetime specs in this vop are pretty confusing.
             (instrument-alloc type bytes node alloc-temp thread-tn)
             (jump-if-not-bitmap-alloc LINEAR-ALLOC)
             (inst push lowtag)
             (inst push header)
             (move alloc-temp bytes) ; RAX passes the size
             (invoke-asm-routine 'call 'bitmap-var-alloc node)
             (move result alloc-temp)
             (inst jmp done)
             LINEAR-ALLOC
             (pseudo-atomic (:thread-tn thread-tn)
              (gengc-alloc type bytes lowtag result node alloc-temp thread-tn)
              (storew header result 0 lowtag))
             DONE))))))

#+sb-xc-host
(progn
(define-vop (alloc-code)
  (:args (total-words :scs (unsigned-reg))
         (boxed-words :scs (unsigned-reg)))
  (:temporary (:sc unsigned-reg :offset rdi-offset :from (:argument 0)) rdi)
  (:temporary (:sc unsigned-reg :offset rsi-offset) rsi)
  (:temporary (:sc unsigned-reg :offset rax-offset) rax)
  (:temporary (:sc unsigned-reg :offset r15-offset) frame)
  (:results (res :scs (descriptor-reg)))
  (:node-var node)
  (:ignore frame)
  (:generator 1
    (move rdi total-words) ; C arg 1
    (move rsi boxed-words) ; C arg 2
    (jump-if-not-bitmap-alloc LINEAR-ALLOC)

    ;; bitmap allocator
    (inst lea rax (ea nil rdi n-word-bytes)) ; RAX = nbytes
    ;(inst cmp rax smlgc-blocksize-max)
    ;(inst jmp :be OK)
    ;(inst break halt-trap) ; can't use this vop for large code
    ;OK
    (inst shl rdi 32) ; CODE-HEADER-SIZE-SHIFT
    (inst or :byte rdi code-header-widetag)
    (inst push other-pointer-lowtag)
    (inst push rdi) ; header word
    (invoke-asm-routine 'call 'bitmap-var-alloc node)
    ;; store the boxed size in bytes. BITMAP-VAR-ALLOC preserves RSI
    (inst shl rsi word-shift) ; words to bytes
    (move res rax)
    (storew rsi res code-boxed-size-slot other-pointer-lowtag)
    (inst jmp done)

    LINEAR-ALLOC
    (with-registers-preserved (c :except rsi :frame-reg r15)
      (pseudo-atomic ()
        (inst call (ea (make-fixup "alloc_code_object" :foreign 8))))
      (move rsi rax-tn))
    (move res rsi)

    DONE))

;;; Place a large codeblob under GC control.
;;; The header words haven't been filled in yet because we have to deal with a subtle
;;; timing issue: what happens if, one instruction after exiting pseudo-atomic, this
;;; thread is asked by GC to publish roots. Do we see a properly tagged pointer to a
;;; known good object; while prior to calling into C we do NOT see it as being a good
;;; object? So the header can be filled in only while pseudo-atomic.
;;; Other threads may observe the object in the balanced binary tree but ignore it
;;; as if it were filler. (See also alloc_large in "lispobj.c")
#+nil
(define-vop (manage-large-codeblob)
  (:policy :fast-safe)
  (:arg-types positive-fixnum t)
  (:args (header :scs (unsigned-reg) :target rbx)
         (mseg :scs (sap-reg) :target rdi))
  (:temporary (:sc unsigned-reg :offset rbx-offset :from (:argument 0)) rbx)
  (:temporary (:sc unsigned-reg :offset rdi-offset :from (:argument 1) :to :result) rdi)
  (:results (res :scs (descriptor-reg)))
  (:generator 1
    (move rbx header)
    (move rdi mseg) ; C call arg
    ;; Don't need to save any FPRs because this vop is invoked only by a Lisp function
    ;; that does not use floating-point. Even if the C side clobbers every FPR
    ;; (which it doesn't) the Lisp call convention has no callee-saved FPRs.
    (with-registers-preserved (c :except (rdi :fprs))
      (pseudo-atomic ()
        (inst call (ea (make-fixup "manage_large_object" :foreign 8)))
        ;; Compute the tagged pointer to the code blob given the 'mseg' pointer
        ;; which was conveniently returned from C.
        (inst lea rdi (ea (+ other-pointer-lowtag smlgc-mseg-overhead-bytes) rax-tn))
        ;; Store the header word
        (storew header rdi 0 other-pointer-lowtag)))
    (move res rdi)))
) ; end PROGN

#|
#+sb-xc-host
(define-vop (alloc-code)
  (:args (total-words :scs (unsigned-reg) :target c-arg-1)
         (boxed-words :scs (unsigned-reg) :target c-arg-2))
  (:temporary (:sc unsigned-reg
               :offset #.(first *c-call-register-arg-offsets*)
               :from (:argument 0) :to :result) c-arg-1)
  (:temporary (:sc unsigned-reg :offset rsi-offset
               :offset #.(second *c-call-register-arg-offsets*)
               :from (:argument 1) :to :result) c-arg-2)
  (:temporary (:sc unsigned-reg :offset r15-offset) dummy)
  (:results (res :scs (descriptor-reg)))
  (:ignore dummy)
  (:generator 1
    (move c-arg-1 total-words)
    (move c-arg-2 boxed-words)
    (with-registers-preserved (c :except #-win32 rdi #+win32 rcx :frame-reg r15)
      (pseudo-atomic ()
        (call-c
         #-immobile-code (ea (make-fixup "alloc_code_object" :foreign 8))
         #+immobile-code (make-fixup "alloc_code_object" :foreign)))
      (move c-arg-1 rax-tn))
    (move res c-arg-1)))
|#

#+immobile-space
(macrolet ((c-fun (name)
             `(let ((c-fun (make-fixup ,name :foreign)))
                (cond ((sb-c::code-immobile-p node) c-fun)
                                 (t (progn (inst mov rax c-fun) rax))))))
(define-vop (!alloc-immobile-fixedobj)
  (:args (size-class :scs (any-reg) :target c-arg1)
         (nwords :scs (any-reg) :target c-arg2)
         (header :scs (any-reg) :target c-arg3))
  (:temporary (:sc unsigned-reg :from (:argument 0) :to :eval
               :offset #.(first *c-call-register-arg-offsets*)) c-arg1)
  (:temporary (:sc unsigned-reg :from (:argument 1) :to :eval
               :offset #.(second *c-call-register-arg-offsets*)) c-arg2)
  (:temporary (:sc unsigned-reg :from (:argument 2) :to :eval
               :offset #.(third *c-call-register-arg-offsets*)) c-arg3)
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
     (call-c (c-fun "alloc_immobile_fixedobj"))
     (move result rax))))

;;; Timing test:
;;; Dynamic-space allocation:
;;; * (defun f (n) (dotimes (i n) (make-symbol "b")))
;;; * (time (f 500000))
;;; Evaluation took: 0.004 seconds of real time
;;; Immobile-space:
;;; * (defun f (n) (dotimes (i n) (sb-vm::make-immobile-symbol "b")))
;;; Evaluation took: 0.043 seconds of real time
;;; With vop:  0.028 seconds of real time

(eval-when (:compile-toplevel)
  (aver (evenp symbol-size))) ; assumptions in the code below

(define-vop (!fast-alloc-immobile-symbol)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg :offset rax-offset) rax)
  (:temporary (:sc unsigned-reg :offset rbx-offset) rbx)
  (:temporary (:sc unsigned-reg :offset rcx-offset) rcx)
  (:temporary (:sc unsigned-reg) header)
  (:generator 1
    ;; fixedobj_pages linkage entry: 1 PTE per page, 12-byte struct
    (inst mov rbx (rip-relative-ea (make-fixup "fixedobj_pages" :foreign-dataref)))
    ;; fixedobj_page_hint: 1 hint per sizeclass. C type = uint32_t
    (inst mov rax (rip-relative-ea (make-fixup "fixedobj_page_hint" :foreign-dataref)))
    (inst mov rbx (ea rbx)) ; get the base of the fixedobj_pages array
    ;; This has to be pseudoatomic as soon as the page hint is loaded.
    ;; Consider the situation where there is exactly one symbol on a page that
    ;; is almost all free, and the symbol page hint points to that page.
    ;; If GC occurs, it might dispose that symbol, resetting the page attributes
    ;; and page hint. It would be an error to allocate to that page
    ;; because it is no longer a page of symbols but rather a free page.
    ;; There is no way to inform GC that we are currently looking at a page
    ;; in anticipation of allocating to it.
    (pseudo-atomic ()
       (inst mov :dword rax (ea 4 rax)) ; rax := fixedobj_page_hint[1] (sizeclass=SYMBOL)
       (inst test :dword rax rax)
       (inst jmp :z FAIL) ; fail if hint page is 0
       (inst lea rbx (ea rbx rax 8))  ; rbx := &fixedobj_pages[hint].free_index
       ;; compute fixedobj_page_address(hint) into RAX
       (inst mov rcx (rip-relative-ea (make-fixup "FIXEDOBJ_SPACE_START" :foreign-dataref)))
       (inst shl rax (integer-length (1- immobile-card-bytes)))
       (inst add rax (ea rcx))
       ;; load the page's free pointer
       (inst mov :dword rcx (ea rbx)) ; rcx := fixedobj_pages[hint].free_index
       ;; fail if allocation would overrun the page
       (inst cmp :dword rcx (- immobile-card-bytes (* symbol-size n-word-bytes)))
       (inst jmp :a FAIL)
       ;; compute address of the allegedly free memory block into RESULT
       (inst lea result (ea rcx rax)) ; free_index + page_base
       ;; read the potential symbol header
       (inst mov rax (ea result))
       (inst test :dword rax 1)
       (inst jmp :nz FAIL) ; not a fixnum implies already taken
       ;; try to claim this word of memory
       (inst mov header (logior (ash (1- symbol-size) n-widetag-bits) symbol-widetag))
       (inst cmpxchg :lock (ea result) header)
       (inst jmp :ne FAIL) ; already taken
       ;; compute new free_index = spacing + old header + free_index
       (inst lea :dword rax (ea (* symbol-size n-word-bytes) rax rcx))
       (inst mov :dword (ea rbx) rax) ; store new free_index
       ;; set the low bit of the 'gens' field
       (inst or :lock :byte (ea 7 rbx) 1) ; 7+rbx = &fixedobj_pages[i].attr.parts.gens_
       (inst or :byte result other-pointer-lowtag) ; make_lispobj()
       (inst jmp OUT)
       FAIL
       (inst mov result nil-value)
       OUT)))

) ; end MACROLET

(define-vop (new-make-list)
  (:args (length :scs (signed-reg))
         (element :scs (any-reg descriptor-reg)))
  (:temporary (:sc unsigned-reg :offset rcx-offset :from (:argument 0)) rcx)
  (:temporary (:sc unsigned-reg :offset rax-offset :from (:argument 1) :to :result) rax)
  (:results (res :scs (descriptor-reg)))
  (:generator 10
    (inst test length length)
    (inst jmp :nz callout)
    (inst mov res nil-value)
    (inst jmp done)
    callout
    (move rcx length)
    (move rax element)
    (inst call (ea (make-fixup 'make-list-helper :assembly-routine*)))
    (move res rax)
    done))
