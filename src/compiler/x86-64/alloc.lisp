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

(defun stack-allocation (alloc-tn size lowtag)
  (aver (not (location= alloc-tn rsp-tn)))
  (inst sub rsp-tn size)
  ;; see comment in x86/macros.lisp implementation of this
  ;; However that comment seems inapplicable here because:
  ;; - PAD-DATA-BLOCK quite clearly enforces double-word alignment,
  ;;   contradicting "... unfortunately not enforced by ..."
  ;; - It's not the job of FIXED-ALLOC to realign anything.
  ;; - The real issue is that it's not obvious that the stack is
  ;;   16-byte-aligned at *all* times. Maybe it is, maybe it isn't.
  (inst and rsp-tn #.(lognot lowtag-mask))
  (tagify alloc-tn rsp-tn lowtag)
  (values))

(defun %alloc-tramp (node result-tn size lowtag)
  (cond ((typep size '(and integer (not (signed-byte 32))))
         ;; MOV accepts large immediate operands, PUSH does not
         (inst mov result-tn size)
         (inst push result-tn))
        (t
         (inst push size)))
  ;; This really would be better if it recognized TEMP-REG-TN as the "good" case
  ;; rather than specializing on R11, which just happens to be the temp reg.
  ;; But the assembly routine is hand-written, not generated, and it has to match,
  ;; so there's not much that can be done to generalize it.
  (let ((to-r11 (location= result-tn r11-tn)))
    (invoke-asm-routine 'call (if to-r11 'alloc-tramp-r11 'alloc-tramp) node)
    (unless to-r11
      (inst pop result-tn)))
  (unless (eql lowtag 0)
    (inst or :byte result-tn lowtag))
  (values))

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
(defun allocation (alloc-tn size node &optional dynamic-extent lowtag)
  (when dynamic-extent
    (stack-allocation alloc-tn size lowtag)
    (return-from allocation (values)))
  (aver (and (not (location= alloc-tn temp-reg-tn))
             (or (integerp size) (not (location= size temp-reg-tn)))))

  #+(and (not sb-thread) sb-dynamic-core)
  ;; We'd need a spare reg in which to load boxed_region from the linkage table.
  ;; Could push/pop any random register on the stack and own it temporarily,
  ;; but seeing as nobody cared about this, just punt.
  (%alloc-tramp node alloc-tn size lowtag)

  #-(and (not sb-thread) sb-dynamic-core)
  ;; Otherwise do the normal inline allocation thing
  (let ((NOT-INLINE (gen-label))
        (DONE (gen-label))
        ;; Yuck.
        (in-elsewhere (sb-assem::assembling-to-elsewhere-p))
        ;; thread->alloc_region.free_pointer
        (free-pointer
         #+sb-thread (thread-slot-ea thread-alloc-region-slot)
         #-sb-thread (ea (make-fixup "gc_alloc_region" :foreign)))
        ;; thread->alloc_region.end_addr
        (end-addr
         #+sb-thread (thread-slot-ea (1+ thread-alloc-region-slot))
         #-sb-thread (ea (make-fixup "gc_alloc_region" :foreign 8))))

    (cond ((or in-elsewhere
               ;; large objects will never be made in a per-thread region
               (and (integerp size)
                    (>= size large-object-size)))
           (%alloc-tramp node alloc-tn size lowtag))
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
               (%alloc-tramp node alloc-tn size 0))
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
                    (inst sub alloc-tn free-pointer)
                    (%alloc-tramp node temp-reg-tn alloc-tn 0))
                   (t ; SIZE is intact
                    (%alloc-tramp node temp-reg-tn size 0)))
             (inst jmp DONE))))
    (values)))

;;; Allocate an other-pointer object of fixed SIZE with a single word
;;; header having the specified WIDETAG value. The result is placed in
;;; RESULT-TN.
(defun alloc-other (result-tn widetag size node &optional stack-allocate-p
                    &aux (bytes (pad-data-block size)))
  (let ((header (compute-object-header size widetag)))
    (cond (stack-allocate-p
           (allocation result-tn bytes node t other-pointer-lowtag)
           (storew header result-tn 0 other-pointer-lowtag))
          (t
           (instrument-alloc bytes node)
           (pseudo-atomic ()
             (allocation result-tn bytes node nil 0)
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
                (allocation res size node stack-allocate-p
                            (if (= cons-cells 2) 0 list-pointer-lowtag))
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
(macrolet ((calc-size-in-bytes (n-words result-tn)
             `(cond ((sc-is ,n-words immediate)
                     (pad-data-block (+ (tn-value ,n-words) vector-data-offset)))
                    (t
                     (inst lea ,result-tn
                           (ea (+ lowtag-mask (* vector-data-offset n-word-bytes))
                               nil ,n-words (ash 1 (- word-shift n-fixnum-tag-bits))))
                     (inst and ,result-tn (lognot lowtag-mask))
                     ,result-tn)))
           (put-header (vector-tn lowtag type length zeroed)
             `(progn (storew* (if (sc-is ,type immediate) (tn-value ,type) ,type)
                              ,vector-tn 0 ,lowtag ,zeroed)
                     (storew* (if (sc-is ,length immediate)
                                  (fixnumize (tn-value ,length))
                                  ,length)
                              ,vector-tn vector-length-slot ,lowtag ,zeroed))))

  (define-vop (allocate-vector-on-heap)
    (:args (type :scs (unsigned-reg immediate))
           (length :scs (any-reg immediate))
           (words :scs (any-reg immediate)))
    ;; Result is live from the beginning, like a temp, because we use it as such
    ;; in 'calc-size-in-bytes'
    (:results (result :scs (descriptor-reg) :from :load))
    (:arg-types positive-fixnum positive-fixnum positive-fixnum)
    (:policy :fast-safe)
    (:node-var node)
    (:generator 100
      ;; The LET generates instructions that needn't be pseudoatomic
      ;; so don't move it inside.
      (let ((size (calc-size-in-bytes words result)))
        (instrument-alloc size node)
        (pseudo-atomic ()
         (allocation result size node nil 0)
         (put-header result 0 type length t)
         (inst or :byte result other-pointer-lowtag)))))

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
        ;; Compute tagged pointer sooner than later since access off RSP
        ;; requires an extra byte in the encoding anyway.
        (stack-allocation result size other-pointer-lowtag)
        (put-header result other-pointer-lowtag type length nil)
        ;; FIXME: It would be good to check for stack overflow here.
        ;; It would also be good to skip zero-fill of specialized vectors
        ;; perhaps in a policy-dependent way. At worst you'd see random
        ;; bits, and CLHS says consequences are undefined.
        (when (sb-c:msan-unpoison sb-c:*compilation*)
          ;; Unpoison all DX vectors regardless of widetag.
          ;; Mark the header and length as valid, not just the payload.
          #+linux ; unimplemented for others
          (let ((words-savep
                 ;; 'words' might be co-located with any of the temps
                 (or (location= words rdi) (location= words rcx) (location= words rax))))
            (setq rax-zeroed (not (location= words rax)))
            (when words-savep ; use 'result' to save 'words'
              (inst mov result words))
            (cond ((sc-is words immediate)
                   (inst mov rcx (+ (tn-value words) vector-data-offset)))
                  (t
                   (inst lea rcx
                         (ea (ash vector-data-offset n-fixnum-tag-bits) words))
                   (inst shr rcx n-fixnum-tag-bits)))
            (inst mov rdi msan-mem-to-shadow-xor-const)
            (inst xor rdi rsp-tn) ; compute shadow address
            (zeroize rax)
            (inst rep)
            (inst stos rax)
            (when words-savep
              (inst mov words result) ; restore 'words'
              (inst lea result ; recompute the tagged pointer
                    (ea other-pointer-lowtag rsp-tn)))))
        (unless (sb-c::vector-initialized-p node)
          (let ((data-addr
                  (ea (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)
                      result)))
            (block zero-fill
              (cond ((sc-is words immediate)
                     (let ((n (tn-value words)))
                       (cond ((> n 8)
                              (inst mov rcx (tn-value words)))
                             ((= n 1)
                              (inst mov :qword data-addr 0)
                              (return-from zero-fill))
                             (t
                              (multiple-value-bind (double single) (truncate n 2)
                                (inst xorpd zero zero)
                                (dotimes (i double)
                                  (inst movapd data-addr zero)
                                  (setf data-addr
                                        (ea (+ (ea-disp data-addr) (* n-word-bytes 2))
                                            (ea-base data-addr))))
                                (unless (zerop single)
                                  (inst movaps data-addr zero))
                                (return-from zero-fill))))))
                    (t
                     (move rcx words)
                     (inst shr rcx n-fixnum-tag-bits)))
              (inst lea rdi data-addr)
              (unless rax-zeroed (zeroize rax))
              (inst rep)
              (inst stos rax))))))))

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
             `(let ((size (cond ((or (not (fixnump size))
                                     (plausible-signed-imm32-operand-p size))
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
       (allocation result bytes node stack-allocate-p fun-pointer-lowtag)
       (storew* #-immobile-space header ; write the widetag and size
                #+immobile-space        ; ... plus the layout pointer
                (progn (inst mov temp header)
                       (inst or temp #-sb-thread (static-symbol-value-ea 'function-layout)
                                     #+sb-thread
                                     (thread-slot-ea thread-function-layout-slot))
                       temp)
                result 0 fun-pointer-lowtag (not stack-allocate-p)))
     ;; Done with pseudo-atomic
     (inst lea temp (rip-relative-ea label (ash simple-fun-insts-offset word-shift)))
     (storew temp result closure-fun-slot fun-pointer-lowtag))))

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
   (let* ((instancep (typep type 'layout)) ; is this any instance?
          (layoutp #+immobile-space
                   (eq type #.(find-layout 'layout))) ; " a new LAYOUT instance?
          (bytes (pad-data-block words)))
    (progn name) ; possibly not used
    (unless (or stack-allocate-p layoutp)
      (instrument-alloc bytes node))
    (pseudo-atomic (:elide-if stack-allocate-p)
     (cond (layoutp
            (invoke-asm-routine 'call 'alloc-layout node)
            (inst mov result r11-tn))
           (t
            ;; If storing a header word, defer ORing in the lowtag until after
            ;; the header is written so that displacement can be 0.
            (allocation result bytes node stack-allocate-p (if type 0 lowtag))
            (when type
              (let* ((widetag (if instancep instance-widetag type))
                     (header (logior (ash (1- words) n-widetag-bits) widetag)))
                (if (or #+compact-instance-header
                        (and (eq name '%make-structure-instance) stack-allocate-p))
                    ;; Write a :DWORD, not a :QWORD, because the high half will be
                    ;; filled in when the layout is stored. Can't use STOREW* though,
                    ;; because it tries to store as few bytes as possible,
                    ;; where this instruction must write exactly 4 bytes.
                    (inst mov :dword (ea 0 result) header)
                    (storew* header result 0 0 (not stack-allocate-p)))
                (inst or :byte result lowtag))))))
    (when instancep ; store its layout
      (inst mov :dword (ea (+ 4 (- lowtag)) result)
            (make-fixup type :layout))))))

;;; Allocate a non-vector variable-length object.
;;; Exactly 4 allocators are rendered via this vop:
;;;  BIGNUM               (%ALLOCATE-BIGNUM)
;;;  FUNCALLABLE-INSTANCE (%MAKE-FUNCALLABLE-INSTANCE)
;;;  CLOSURE              (%COPY-CLOSURE)
;;;  INSTANCE             (%MAKE-INSTANCE)
;;; WORDS accounts for the mandatory slots *including* the header.
;;; EXTRA is the variable payload, also measured in words.
(define-vop (var-alloc)
  (:args (extra :scs (any-reg)))
  (:arg-types positive-fixnum)
  (:info name words type lowtag)
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
      (inst shl operand-size header (- n-widetag-bits word-shift)) ; w+1 to length field
      (inst lea operand-size header                    ; (w-1 << 8) | type
            (ea (+ (ash -2 n-widetag-bits) type) header))
      (inst and operand-size bytes (lognot lowtag-mask)))
      (instrument-alloc bytes node)
      (pseudo-atomic ()
       (allocation result bytes node nil lowtag)
       (storew header result 0 lowtag))))

(macrolet ((c-call (name)
             `(let ((c-fun (make-fixup ,name :foreign)))
                (inst call (cond ((sb-c::code-immobile-p node) c-fun)
                                 (t (progn (inst mov temp-reg-tn c-fun)
                                           temp-reg-tn)))))))
#+immobile-space
(define-vop (alloc-immobile-fixedobj)
  (:info lowtag size header)
  (:temporary (:sc unsigned-reg :to :eval :offset rdi-offset) c-arg1)
  (:temporary (:sc unsigned-reg :to :eval :offset rsi-offset) c-arg2)
  (:temporary (:sc unsigned-reg :from :eval :to (:result 0) :offset rax-offset)
              c-result)
  (:results (result :scs (descriptor-reg)))
  (:node-var node)
  (:generator 50
   (inst mov c-arg1 size)
   (inst mov c-arg2 header)
   ;; RSP needn't be restored because the allocators all return immediately
   ;; which has that effect
   (inst and rsp-tn -16)
   (pseudo-atomic ()
     (c-call "alloc_fixedobj")
     (inst lea result (ea lowtag c-result)))))

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
   ;; C-RESULT is a tagged ptr. MOV doesn't need to be pseudo-atomic.
   (inst mov result c-result)))

) ; end MACROLET
