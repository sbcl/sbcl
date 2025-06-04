;;;; a bunch of handy macros for x86-64

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;;; instruction-like macros

;;; This used to be a macro (and still is on the other platforms) but
;;; the support for SC-dependent move instructions needed here makes
;;; that expand into so large an expression that the resulting code
;;; bloat is not justifiable.
(defun move (dst src &optional size)
  "Move SRC into DST unless they are location=."
  (unless (location= dst src)
    (sc-case dst
      ((single-reg complex-single-reg)
       (aver (xmm-tn-p src))
       (inst movaps dst src))
      ((double-reg complex-double-reg)
       (aver (xmm-tn-p src))
       (inst movapd dst src))
      #+sb-simd-pack
      ((int-sse-reg sse-reg)
       (aver (xmm-tn-p src))
       (inst movdqa dst src))
      #+sb-simd-pack
      ((single-sse-reg double-sse-reg)
       (aver (xmm-tn-p src))
       (inst movaps dst src))
      #+sb-simd-pack-256
      ((ymm-reg int-avx2-reg)
       (aver (xmm-tn-p src))
       (inst vmovdqa dst src))
      #+sb-simd-pack-256
      ((single-avx2-reg double-avx2-reg)
       (aver (xmm-tn-p src))
       (inst vmovaps dst src))
      (t
       (if size
           (inst mov size dst src)
           (inst mov dst src))))))

(defmacro object-slot-ea (ptr slot lowtag)
  `(ea (- (* ,slot n-word-bytes) ,lowtag) ,ptr))
(defmacro tls-index-of (sym)
  `(ea (+ 4 (- other-pointer-lowtag)) ,sym))

(defmacro loadw (value ptr &optional (slot 0) (lowtag 0))
  `(inst mov ,value (object-slot-ea ,ptr ,slot ,lowtag)))

(defun storew (value ptr &optional (slot 0) (lowtag 0) temp)
  (let* ((size (if (tn-p value)
                   (sc-operand-size (tn-sc value))
                   :qword))
         (ea (ea (- (* slot n-word-bytes) lowtag) ptr)))
    (aver (eq size :qword))
    (cond ((or (typep value '(and integer (not (signed-byte 32))))
               (nil-relative-p value))
           (cond (temp
                  (if (nil-relative-p value)
                      (move-immediate temp value)
                      (inst mov temp value))
                  (inst mov ea temp)
                  ;; uhh, why does this clause return TEMP but the
                  ;; T clause returns nothing in particular?
                  temp)
                 (t
                  (bug "need temp reg for STOREW of immediate operand"))))
          (t
           (inst mov :qword ea value)))))

(defmacro pushw (ptr &optional (slot 0) (lowtag 0))
  `(inst push (object-slot-ea ,ptr ,slot ,lowtag)))

(defmacro popw (ptr &optional (slot 0) (lowtag 0))
  `(inst pop (object-slot-ea ,ptr ,slot ,lowtag)))


;;;; macros to generate useful values

(defmacro load-symbol (reg symbol)
  (cond (symbol `(inst lea ,reg (ea ,(static-symbol-offset symbol) null-tn)))
        (t `(inst mov ,reg null-tn))))

;;; Access a thread slot at a fixed index. If GPR-TN is provided,
;;; then it points to 'struct thread', which is relevant only if #+gs-seg.
(defun thread-slot-ea (slot-index &optional gpr-tn)
  (declare (type (signed-byte 16) slot-index)) ; arbitrary
  (if gpr-tn
      (ea (ash slot-index word-shift) gpr-tn)
      ;; Otherwise do something depending on #[-+]gs-seg
      (let (#+gs-seg (thread-tn nil))
        (ea thread-segment-reg (ash slot-index word-shift) thread-tn))))

;;; Similar to thread-slot-ea, but INDEX in this case does not signify the Nth slot {0,1,2,..}
;;; but rather the displacement into the thread's storage, added to the thread base address.
(defun thread-tls-ea (index)
  #+gs-seg (ea :gs index) ; INDEX is either a DISP or a BASE of the EA
  ;; Whether index is an an integer or a register, the EA constructor
  ;; call is the same.
  ;; Due to an encoding peculiarity, using thread-base-reg as the index register
  ;; is better when index is non-constant.
  ;; Base of r13 is reg=5 in ModRegRM, so if mod were 0, it would imply
  ;; RIP-relative addressing. (And attempting to encode an index is illegal)
  ;; So the 'mod' bits must be nonzero, which mandates encoding of an
  ;; explicit displacement of 0.  Using INDEX as base avoids the extra byte.
  #-gs-seg (ea index thread-tn))

(defmacro static-constant-ea (name &optional (extra 0)) ; EXTRA is for byte-sized access
  (declare (notinline position))
  ;; 6 = number of words between native ptr to NIL-as-list and 1st trailer constant
  (let ((n (+ 6 (position name +static-space-trailer-constants+))))
    `(ea (+ ,extra ,(- (ash n word-shift) list-pointer-lowtag)) null-tn)))

;;; assert that alloc-region->free_pointer and ->end_addr can be accessed
;;; using a single byte displacement from thread-tn
(eval-when (:compile-toplevel)
  (aver (<= (1+ thread-boxed-tlab-slot) 15))
  (aver (<= (1+ thread-mixed-tlab-slot) 15))
  (aver (<= (1+ thread-cons-tlab-slot) 15)))

(defmacro load-tl-symbol-value (reg symbol)
  `(inst mov ,reg (thread-slot-ea ,(symbol-thread-slot symbol))))

(defmacro store-tl-symbol-value (reg symbol)
  `(inst mov (thread-slot-ea ,(symbol-thread-slot symbol)) ,reg))

(defmacro load-binding-stack-pointer (reg)
  `(load-tl-symbol-value ,reg *binding-stack-pointer*))

(defmacro store-binding-stack-pointer (reg)
  `(store-tl-symbol-value ,reg *binding-stack-pointer*))

;;;; error code
(defun emit-error-break (vop kind code values)
  (assemble ()
    (inst break)
    ;; The return PC points here; note the location for the debugger.
    (when vop
      (note-this-location vop :internal-error))
    (if (= kind invalid-arg-count-trap) ; there is no "payload" in this trap kind
        (inst byte kind)
        (emit-internal-error kind code values))))

(defun generate-error-code (vop error-code &rest values)
  (apply #'generate-error-code+ nil vop error-code values))

(defun generate-error-code+ (preamble-emitter vop error-code &rest values)
  "Generate-Error-Code Error-code Value*
  Emit code for an error with the specified Error-Code and context Values."
  (assemble (:elsewhere)
    START
    (when preamble-emitter
      (funcall preamble-emitter))
    (emit-error-break vop
                      (case error-code ; should be named ERROR-SYMBOL really
                        (invalid-arg-count-error invalid-arg-count-trap)
                        (t error-trap))
                      (error-number-or-lose error-code)
                      values)
    (values start))) ; prevent START from being seen as a label defn


;;;; PSEUDO-ATOMIC

;;; This is used to wrap operations which leave untagged memory lying
;;; around.  It's an operation which the AOP weenies would describe as
;;; having "cross-cutting concerns", meaning it appears all over the
;;; place and there's no logical single place to attach documentation.
;;; grep (mostly in src/runtime) is your friend

;;; Unsafely clear pa flags so that the image can properly lose in a
;;; pa section.
#+sb-thread
(defmacro %clear-pseudo-atomic ()
  '(inst mov :qword (thread-slot-ea thread-pseudo-atomic-bits-slot) 0))

#+sb-safepoint
(defun emit-safepoint ()
  ;; FIXME: need to get the node and policy to decide not to emit this safepoint.
  ;; Also, it would be good to emit only the last of consecutive safepoints in
  ;; straight-line code, e.g. (LIST (LIST X Y) (LIST Z W)) should emit 1 safepoint
  ;; not 3, even if we consider it 3 separate pointer bumps.
  ;; (Ideally we'd only do 1 pointer bump, but that's a separate issue)
  (inst test :byte rax-tn (ea nil-static-space-end-offs null-tn)))

(macrolet ((pa-bits-ea ()
             `(thread-slot-ea thread-pseudo-atomic-bits-slot
                              #+gs-seg ,@(if thread (list thread))))
           (nonzero-bits ()
             ;; reg-mem move is allegedly faster than imm-mem according to
             ;; someone at some point. Whether that's true or not, it is what it is.
             ;; THREAD-TN is a better choice than RBP-TN since it's constant.
             #+(and sb-thread (not gs-seg)) 'thread-tn
             #-(and sb-thread (not gs-seg)) 'rbp-tn))
  (defun emit-begin-pseudo-atomic ()
    #-sb-safepoint (inst mov (pa-bits-ea) (nonzero-bits)))
  (defun emit-end-pseudo-atomic ()
    #+sb-safepoint (emit-safepoint)
    #-sb-safepoint
    (assemble ()
      (inst xor (pa-bits-ea) (nonzero-bits))
      (inst jmp :z OUT)
      ;; if PAI was set, interrupts were disabled at the same time
      ;; using the process signal mask.
      #+partial-sw-int-avoidance (inst call (ea (make-fixup 'synchronous-trap :assembly-routine)))
      #-partial-sw-int-avoidance (progn
      #+int1-breakpoints (inst icebp)
      #-int1-breakpoints (inst break pending-interrupt-trap))
      OUT)))

(defmacro define-allocator (name &body body &aux (g (cdr (assoc :generator body))))
  `(define-vop ,name
     #+gs-seg (:temporary (:sc unsigned-reg :offset 15) thread-tn)
     ,@(remove :generator body :key 'car)
     (:node-var node)
     (:generator ,(car g) ; cost
       (macrolet
           ((instrument-alloc (&rest args) `(emit-instrument-alloc node thread-tn ,@args))
            (allocation (&rest args) `(emit-allocation node thread-tn ,@args))
            (alloc-other (&rest args) `(emit-alloc-other node thread-tn ,@args)))
         (assemble () ,@(cdr g)))))) ; forms

(defmacro pseudo-atomic ((&key elide-if (default-exit t))
                         &body forms)
  `(macrolet ((exit-pseudo-atomic () '(emit-end-pseudo-atomic)))
     (unless ,elide-if
       (emit-begin-pseudo-atomic))
     (assemble () ,@forms)
     (when (and ,default-exit (not ,elide-if))
       (exit-pseudo-atomic))))

;;; For now, ALLOCATING is synonymous with PSEUDO-ATOMIC, however there are
;;; a few distinguishing factors rendering it not exactly the same:
;;; 1. It could automatically insert a binding of a thread temp register, should
;;;    we decide that R13 is not permanently wired to the current thread.
;;; 2. It will have a yielding behavior that is neither like sb-safepoint
;;;    nor the current pseudo-atomic, when new-and-improved yieldpoint patches
;;;    (work in progress) are completed.
;;; 3. It separates pseudo-atomicity for the purpose of object creation from
;;;    any other reason which it might be required. Consider that for allocation
;;;    a behavior might be: if there is room in the TLAB, then fulfill the request;
;;;    otherwise, induce GC to run immediately and then resume the request.
;;; [Like sb-safepoint, yieldpoints avoid using flag bits in the thread struct,
;;; but unlike sb-safepoint, ALLOCATING will not emit a trapping instruction
;;; at the end of every consing operation. Instead, if and only if it calls
;;; into C due to TLAB overflow might it yield. This makes sense because we
;;; give GC the opportunity to run exactly there if the region is exhausted,
;;; or else there will necessarily be a later yieldpoint based on control flow.
;;; Note that this is not simply a re-statement of point 3 above which suggests
;;; that it _will_ cause GC to run; but in general ALLOCATION _may_ let GC run.
;;; Whereas pseudo-atomic never permits GC to run.
;;; Also note that "yieldpoints" and "safepoints" are more-or-less the same,
;;; but a new approach to whatever the concept is demands a different name]
(defmacro allocating (options &body body)
  `(pseudo-atomic ,options ,@body))

;;;; indexed references

(defun index-scale (element-size index-tn)
  (if (sc-is index-tn immediate)
      1
      (ash element-size
           (if (sc-is index-tn any-reg) (- n-fixnum-tag-bits) 0))))

(sb-xc:deftype load/store-index (scale lowtag min-offset
                                 &optional (max-offset min-offset))
  `(integer ,(- (truncate (+ (ash 1 16)
                             (* min-offset sb-vm:n-word-bytes)
                             (- lowtag))
                          scale))
            ,(truncate (- (+ (1- (ash 1 16)) lowtag)
                          (* max-offset sb-vm:n-word-bytes))
                       scale)))

(defmacro define-full-compare-and-swap
    (name type offset lowtag scs el-type &optional translate)
  `(progn
     (define-vop (,name)
       (:translate ,translate)
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg) :to :eval)
              (index :scs (any-reg signed-reg unsigned-reg
                                   (immediate
                                    (typep (- (* (+ (tn-value tn) ,offset) n-word-bytes) ,lowtag) '(signed-byte 32))))
                     :to :eval)
              (old-value :scs (,@scs immediate) #|:target rax|#)
              (new-value :scs ,scs))
       (:vop-var vop)
       (:arg-types ,type tagged-num ,el-type ,el-type)
       ;; if OLD-VALUE were LOCATION= to RAX then we'd clobber it
       ;; while computing the EA for the barrier, or else we could use
       ;; a separate temp.
       (:temporary (:sc descriptor-reg :offset rax-offset
                        #|:from (:argument 2)|# :to :result :target value)  rax)
       (:results (value :scs ,scs))
       (:result-types ,el-type)
       (:generator 5
         (let ((ea (ea (- (* (+ (if (sc-is index immediate) (tn-value index) 0) ,offset)
                         n-word-bytes)
                      ,lowtag)
                   object
                   (unless (sc-is index immediate) index)
                   (index-scale n-word-bytes index))))
           ,@(ecase name
               (%compare-and-swap-svref
                ;; store barrier needs the EA of the affected element
                '((emit-gengc-barrier object ea rax (vop-nth-arg 3 vop))))
               (%instance-cas
                ;; store barrier affects only the object's base address
                '((emit-gengc-barrier object nil rax (vop-nth-arg 3 vop))))
               ((%raw-instance-cas/word %raw-instance-cas/signed-word)))
           (move-immediate rax (encode-value-if-immediate old-value ,(and (memq 'any-reg scs) t)))
           (inst cmpxchg :lock ea new-value)
           (move value rax))))))

(defun bignum-index-check (bignum index addend vop)
  (declare (ignore bignum index addend vop))
  ;; Conditionally compile this in to sanity-check the bignum logic
  #+nil
  (let ((ok (gen-label)))
    (cond ((and (tn-p index) (not (constant-tn-p index)))
           (aver (sc-is index any-reg))
           (inst lea :dword temp-reg-tn (ea (fixnumize addend) index))
           (inst shr :dword temp-reg-tn n-fixnum-tag-bits))
          (t
           (inst mov temp-reg-tn (+ (if (tn-p index) (tn-value index) index) addend))))
    (inst cmp :dword temp-reg-tn (ea (- 1 other-pointer-lowtag) bignum))
    (inst jmp :b ok)
    (inst break halt-trap)
    (emit-label ok)))

(defmacro define-full-reffer (name type offset lowtag scs el-type &optional translate)
  `(progn
     (define-vop (,name)
       (:translate ,translate)
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
              (index :scs (any-reg signed-reg unsigned-reg)))
       (:arg-types ,type tagged-num)
       (:results (value :scs ,scs))
       (:result-types ,el-type)
       (:vop-var vop)
       (:generator 3                    ; pw was 5
         ,@(when (eq translate 'sb-bignum:%bignum-ref)
             '((bignum-index-check object index 0 vop)))
         (inst mov value (ea (- (* ,offset n-word-bytes) ,lowtag)
                             object index (index-scale n-word-bytes index)))))
     (define-vop (,(symbolicate name "-C"))
       (:translate ,translate)
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg)))
       (:info index)
       (:arg-types ,type
                   (:constant (load/store-index ,n-word-bytes ,(eval lowtag)
                                                ,(eval offset))))
       (:results (value :scs ,scs))
       (:result-types ,el-type)
       (:vop-var vop)
       (:generator 2                    ; pw was 5
         ,@(when (eq translate 'sb-bignum:%bignum-ref)
             '((bignum-index-check object index 0 vop)))
         (inst mov value (ea (- (* (+ ,offset index) n-word-bytes) ,lowtag)
                             object))))))

(defmacro define-full-reffer+addend (name type offset lowtag scs el-type &optional translate)
  (flet ((trap (index-to-encode)
           (declare (ignorable index-to-encode))
           #+ubsan
           ;; It's OK that the cell is read twice when testing for a trap value.
           ;; The value should only change from trapping to non-trapping, so if we loaded
           ;; a trap, and then one instruction later the data is valid (due to being
           ;; stored in another thread), then it's a false positive that is indicative
           ;; of a race. A false negative (failure to signal on a trap value) can not
           ;; occur unless unsafely using REPLACE into this vector.
           (when (memq name '(data-vector-ref-with-offset/simple-vector
                              data-vector-ref-with-offset/simple-vector-c))
             `((when (sb-c::policy (sb-c::vop-node vop) (> sb-c::aref-trapping 0))
                 (inst cmp :byte ea unwritten-vector-element-marker)
                 (inst jmp :e (generate-error-code
                               vop 'uninitialized-element-error object
                               ,index-to-encode)))))))
  `(progn
     (define-vop (,name)
       (:translate ,translate)
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
              (index :scs (any-reg signed-reg unsigned-reg)))
       (:info addend)
       (:arg-types ,type tagged-num
                   (:constant (constant-displacement other-pointer-lowtag
                                                     n-word-bytes vector-data-offset)))
       (:results (value :scs ,scs))
       (:result-types ,el-type)
       (:vop-var vop)
       (:generator 3
         ,@(when (eq translate 'sb-bignum:%bignum-ref-with-offset)
             '((bignum-index-check object index addend vop)))
         (let ((ea (ea (- (* (+ ,offset addend) n-word-bytes) ,lowtag)
                       object index (index-scale n-word-bytes index))))
           ,@(trap 'index)
           (inst mov value ea))))
     ;; This vop is really not ideal to have.  Couldn't we recombine two constants
     ;; and use a vop that only takes the object and just ONE index?
     (define-vop (,(symbolicate name "-C"))
       (:translate ,translate)
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg)))
       (:info index addend)
       (:arg-types ,type
                   (:constant (load/store-index ,n-word-bytes ,(eval lowtag)
                                                ,(eval offset)))
                   (:constant (constant-displacement other-pointer-lowtag
                                                     n-word-bytes vector-data-offset)))
       (:results (value :scs ,scs))
       (:result-types ,el-type)
       (:vop-var vop)
       (:generator 2
         ,@(when (eq translate 'sb-bignum:%bignum-ref-with-offset)
             '((bignum-index-check object index addend vop)))
         (let ((ea (ea (- (* (+ ,offset index addend) n-word-bytes) ,lowtag) object)))
           ,@(trap '(emit-constant (+ index addend)))
           (inst mov value ea)))))))

;;; used for: INSTANCE-INDEX-SET %CLOSURE-INDEX-SET
;;;           SB-BIGNUM:%BIGNUM-SET %SET-ARRAY-DIMENSION %SET-VECTOR-RAW-BITS
(defmacro define-full-setter (name type offset lowtag scs el-type translate)
  (let ((tagged (and (member 'any-reg scs)
                     t))
        (barrier (member name '(instance-index-set %closure-index-set %weakvec-set))))
    `(define-vop (,name)
       (:translate ,translate)
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
              (index :scs (any-reg signed-reg unsigned-reg
                                   (immediate
                                    (typep (- (* (+ ,offset (tn-value tn)) n-word-bytes) ,lowtag)
                                           '(signed-byte 32)))))
              (value :scs (,@scs
                           ,(if barrier ;; will use value-temp anyway
                                'immediate
                                `(immediate (let ((value (tn-value tn)))
                                              (and (integerp value)
                                                   (plausible-signed-imm32-operand-p (,(if tagged 'fixnumize 'progn) value)))))))))
       (:arg-types ,type tagged-num ,el-type)
       (:arg-refs obj-ref ind-ref val-ref)
       (:vop-var vop)
       ,@(and barrier
              `((:gc-barrier 0 2)
                (:info barrier)
                (:temporary (:sc unsigned-reg) val-temp)))
       (:generator 4
         #+permgen
         ,@(when (string= name 'instance-index-set)
             `((when (and (eq (tn-ref-type obj-ref) (specifier-type 'layout))
                          ;; since ANY-REG is non-pointer, OBJECT doesn't need remembering
                          (not (sc-is value any-reg)))
                 (inst push object)
                 (invoke-asm-routine 'call 'gc-remember-layout vop))))
         ,@(when (eq translate 'sb-bignum:%bignum-set)
             '((bignum-index-check object index 0 vop)))
         (let ((ea (if (sc-is index immediate)
                       (ea (- (* (+ ,offset (tn-value index)) n-word-bytes) ,lowtag)
                           object)
                       (ea (- (* ,offset n-word-bytes) ,lowtag)
                           object index (index-scale n-word-bytes index)))))
           ,@(if barrier
                 `((when barrier
                     (emit-gengc-barrier object nil val-temp t))
                   (emit-store ea value val-temp))
                 `((inst mov :qword ea (encode-value-if-immediate value ,tagged)))))))))

;;; This is not "very" arch-specific apart from use of the EA macro
(defmacro mutex-slot (base-reg slot-name)
  (let* ((slots (dd-slots (find-defstruct-description 'sb-thread:mutex)))
         (slot (find slot-name slots :key #'dsd-name :test #'string=))
         (word-index (+ instance-slots-offset (dsd-index slot))))
    `(ea ,(- (ash word-index word-shift) instance-pointer-lowtag)
         ,base-reg)))
