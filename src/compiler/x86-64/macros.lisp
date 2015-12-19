;;;; a bunch of handy macros for x86-64

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; instruction-like macros

;;; This used to be a macro (and still is on the other platforms) but
;;; the support for SC-dependent move instructions needed here makes
;;; that expand into so large an expression that the resulting code
;;; bloat is not justifiable.
(defun move (dst src)
  #!+sb-doc
  "Move SRC into DST unless they are location=."
  (unless (location= dst src)
    (sc-case dst
      ((single-reg complex-single-reg)
       (aver (xmm-register-p src))
       (inst movaps dst src))
      ((double-reg complex-double-reg)
       (aver (xmm-register-p src))
       (inst movapd dst src))
      #!+sb-simd-pack
      ((int-sse-reg sse-reg)
       (aver (xmm-register-p src))
       (inst movdqa dst src))
      #!+sb-simd-pack
      ((single-sse-reg double-sse-reg)
       (aver (xmm-register-p src))
       (inst movaps dst src))
      (t
       (inst mov dst src)))))

(defmacro make-ea-for-object-slot (ptr slot lowtag)
  `(make-ea :qword :base ,ptr :disp (- (* ,slot n-word-bytes) ,lowtag)))
(defmacro make-ea-for-object-slot-half (ptr slot lowtag)
  `(make-ea :dword :base ,ptr :disp (- (* ,slot n-word-bytes) ,lowtag)))
(defmacro tls-index-of (sym)
  `(make-ea :dword :base ,sym :disp (+ 4 (- other-pointer-lowtag))))

(defmacro loadw (value ptr &optional (slot 0) (lowtag 0))
  `(inst mov ,value (make-ea-for-object-slot ,ptr ,slot ,lowtag)))

(defmacro storew (value ptr &optional (slot 0) (lowtag 0))
  (once-only ((value value))
    `(cond ((and (integerp ,value)
                 (not (typep ,value '(signed-byte 32))))
            (inst mov temp-reg-tn ,value)
            (inst mov (make-ea-for-object-slot ,ptr ,slot ,lowtag) temp-reg-tn))
           (t
            (inst mov (make-ea-for-object-slot ,ptr ,slot ,lowtag) ,value)))))

(defmacro pushw (ptr &optional (slot 0) (lowtag 0))
  `(inst push (make-ea-for-object-slot ,ptr ,slot ,lowtag)))

(defmacro popw (ptr &optional (slot 0) (lowtag 0))
  `(inst pop (make-ea-for-object-slot ,ptr ,slot ,lowtag)))

(defun call-indirect (offset)
  (typecase offset
    ((signed-byte 32)
     (inst call (make-ea :qword :disp offset)))
    (t
     (inst mov temp-reg-tn offset)
     (inst call (make-ea :qword :base temp-reg-tn)))))

;;;; macros to generate useful values

(defmacro load-symbol (reg symbol)
  `(inst mov ,reg (+ nil-value (static-symbol-offset ,symbol))))

(defmacro make-ea-for-symbol-value (symbol)
  `(make-ea :qword
    :disp (+ nil-value
           (static-symbol-offset ',symbol)
           (ash symbol-value-slot word-shift)
           (- other-pointer-lowtag))))

(defmacro load-symbol-value (reg symbol)
  `(inst mov ,reg (make-ea-for-symbol-value ,symbol)))

(defmacro store-symbol-value (reg symbol)
  `(inst mov (make-ea-for-symbol-value ,symbol) ,reg))

;; Return the effective address of the value slot of static SYMBOL.
(defun static-symbol-value-ea (symbol)
   (make-ea :qword
            :disp (+ nil-value
                     (static-symbol-offset symbol)
                     (ash symbol-value-slot word-shift)
                     (- other-pointer-lowtag))))

#!+sb-thread
(progn
  ;; Return an EA for the TLS of SYMBOL, or die.
  (defun symbol-known-tls-cell (symbol)
    (let ((index (info :variable :wired-tls symbol)))
      (aver (integerp index))
      (make-ea :qword :base thread-base-tn :disp index)))

  ;; LOAD/STORE-TL-SYMBOL-VALUE macros are ad-hoc (ugly) emulations
  ;; of (INFO :VARIABLE :WIRED-TLS) = :ALWAYS-THREAD-LOCAL
  (defmacro load-tl-symbol-value (reg symbol)
    `(inst mov ,reg (symbol-known-tls-cell ',symbol)))

  (defmacro store-tl-symbol-value (reg symbol)
    `(inst mov (symbol-known-tls-cell ',symbol) ,reg)))

#!-sb-thread
(progn
  (defmacro load-tl-symbol-value (reg symbol)
    `(load-symbol-value ,reg ,symbol))
  (defmacro store-tl-symbol-value (reg symbol)
    `(store-symbol-value ,reg ,symbol)))

(defmacro load-binding-stack-pointer (reg)
  #!+sb-thread `(inst mov ,reg (symbol-known-tls-cell '*binding-stack-pointer*))
  #!-sb-thread `(load-symbol-value ,reg *binding-stack-pointer*))

(defmacro store-binding-stack-pointer (reg)
  #!+sb-thread `(inst mov (symbol-known-tls-cell '*binding-stack-pointer*) ,reg)
  #!-sb-thread `(store-symbol-value ,reg *binding-stack-pointer*))

(defmacro load-type (target source &optional (offset 0))
  #!+sb-doc
  "Loads the type bits of a pointer into target independent of
   byte-ordering issues."
  (once-only ((n-target target)
              (n-source source)
              (n-offset offset))
    (ecase *backend-byte-order*
      (:little-endian
       `(inst movzx ,n-target
              (make-ea :byte :base ,n-source :disp ,n-offset)))
      (:big-endian
       `(inst movzx ,n-target
              (make-ea :byte :base ,n-source
                             :disp (+ ,n-offset (1- n-word-bytes))))))))

;;;; allocation helpers

;;; All allocation is done by calls to assembler routines that
;;; eventually invoke the C alloc() function.

;;; Emit code to allocate an object with a size in bytes given by
;;; Size. The size may be an integer of a TN. If Inline is a VOP
;;; node-var then it is used to make an appropriate speed vs size
;;; decision.

(defun allocation-dynamic-extent (alloc-tn size lowtag)
  (inst sub rsp-tn size)
  ;; see comment in x86/macros.lisp implementation of this
  ;; However that comment seems inapplicable here because:
  ;; - PAD-DATA-BLOCK quite clearly enforces double-word alignment,
  ;;   contradicting "... unfortunately not enforced by ..."
  ;; - It's not the job of WITH-FIXED-ALLOCATION to realign anything.
  ;; - The real issue is that it's not obvious that the stack is
  ;;   16-byte-aligned at *all* times. Maybe it is, maybe it isn't.
  (inst and rsp-tn #.(lognot lowtag-mask))
  (aver (not (location= alloc-tn rsp-tn)))
  (inst lea alloc-tn (make-ea :byte :base rsp-tn :disp lowtag))
  (values))

;;; This macro should only be used inside a pseudo-atomic section,
;;; which should also cover subsequent initialization of the
;;; object.
(defun allocation-tramp (alloc-tn size lowtag)
  (cond ((typep size '(and integer (not (signed-byte 32))))
         ;; MOV accepts large immediate operands, PUSH does not
         (inst mov alloc-tn size)
         (inst push alloc-tn))
        (t
         (inst push size)))
  (inst mov alloc-tn (make-fixup "alloc_tramp" :foreign))
  (inst call alloc-tn)
  (inst pop alloc-tn)
  (when lowtag
    (inst or (reg-in-size alloc-tn :byte) lowtag))
  (values))

(defun allocation (alloc-tn size &optional ignored dynamic-extent lowtag)
  (declare (ignore ignored))
  (when dynamic-extent
    (allocation-dynamic-extent alloc-tn size lowtag)
    (return-from allocation (values)))
  (let ((NOT-INLINE (gen-label))
        (DONE (gen-label))
        ;; Yuck.
        (in-elsewhere (eq *elsewhere* sb!assem::**current-segment**))
        ;; thread->alloc_region.free_pointer
        (free-pointer
         #!+sb-thread
         (make-ea :qword
                  :base thread-base-tn :scale 1
                  :disp (* n-word-bytes thread-alloc-region-slot))
         #!-sb-thread
         (make-ea :qword
                  :scale 1 :disp
                  (make-fixup "boxed_region" :foreign)))
        ;; thread->alloc_region.end_addr
        (end-addr
         #!+sb-thread
         (make-ea :qword
                  :base thread-base-tn :scale 1
                  :disp (* n-word-bytes (1+ thread-alloc-region-slot)))
         #!-sb-thread
         (make-ea :qword
                  :scale 1 :disp
                  (make-fixup "boxed_region" :foreign 8))))
    (cond ((or in-elsewhere
               #!+gencgc
               ;; large objects will never be made in a per-thread region
               (and (integerp size)
                    (>= size large-object-size)))
           (allocation-tramp alloc-tn size lowtag))
          (t
           (inst mov temp-reg-tn free-pointer)
           (cond ((tn-p size)
                  (if (location= alloc-tn size)
                      (inst add alloc-tn temp-reg-tn)
                      (inst lea alloc-tn
                            (make-ea :qword :base temp-reg-tn :index size))))
                 ((typep size '(signed-byte 31))
                  (inst lea alloc-tn
                        (make-ea :qword :base temp-reg-tn :disp size)))
                 (t ; a doozy - 'disp' in an EA is too small for this size
                  (inst mov alloc-tn temp-reg-tn)
                  (inst add alloc-tn (constantize size))))
           (inst cmp alloc-tn end-addr)
           (inst jmp :a NOT-INLINE)
           (inst mov free-pointer alloc-tn)
           (if lowtag
               (inst lea alloc-tn (make-ea :byte :base temp-reg-tn :disp lowtag))
               (inst mov alloc-tn temp-reg-tn))
           (emit-label DONE)
           (assemble (*elsewhere*)
             (emit-label NOT-INLINE)
             (cond ((numberp size)
                    (allocation-tramp alloc-tn size lowtag))
                   (t
                    (inst sub alloc-tn free-pointer)
                    (allocation-tramp alloc-tn alloc-tn lowtag)))
             (inst jmp DONE))))
    (values)))

;;; Allocate an other-pointer object of fixed SIZE with a single word
;;; header having the specified WIDETAG value. The result is placed in
;;; RESULT-TN.
(defmacro with-fixed-allocation ((result-tn widetag size &optional inline stack-allocate-p)
                                 &body forms)
  (unless forms
    (bug "empty &body in WITH-FIXED-ALLOCATION"))
  (once-only ((result-tn result-tn) (size size) (stack-allocate-p stack-allocate-p))
    `(maybe-pseudo-atomic ,stack-allocate-p
      (allocation ,result-tn (pad-data-block ,size) ,inline ,stack-allocate-p
                  other-pointer-lowtag)
      (storew (logior (ash (1- ,size) n-widetag-bits) ,widetag)
              ,result-tn 0 other-pointer-lowtag)
      ,@forms)))

;;;; error code
(defun emit-error-break (vop kind code values)
  (assemble ()
    #!-ud2-breakpoints
    (inst int 3)                  ; i386 breakpoint instruction
    ;; On Darwin, we need to use #x0b0f instead of int3 in order
    ;; to generate a SIGILL instead of a SIGTRAP as darwin/x86
    ;; doesn't seem to be reliably firing SIGTRAP
    ;; handlers. Hopefully this will be fixed by Apple at a
    ;; later date. Do the same on x86-64 as we do on x86 until this gets
    ;; sorted out.
    #!+ud2-breakpoints
    (inst word #x0b0f)
    ;; The return PC points here; note the location for the debugger.
    (when vop
      (note-this-location vop :internal-error))
    (inst byte kind)                       ; eg trap_Xyyy
    (case kind
      (#.invalid-arg-count-trap) ; there is no "payload" in this trap kind
      (t
       (with-adjustable-vector (vector)       ; interr arguments
         (write-var-integer code vector)
         (dolist (tn values)
        ;; classic CMU CL comment:
        ;;   zzzzz jrd here. tn-offset is zero for constant
        ;;   tns.
           (write-var-integer (make-sc-offset (sc-number (tn-sc tn))
                                              (or (tn-offset tn) 0))
                              vector))
         (inst byte (length vector))
         (dotimes (i (length vector))
           (inst byte (aref vector i))))))))

(defun error-call (vop error-code &rest values)
  #!+sb-doc
  "Cause an error. ERROR-CODE is the error to cause."
  (emit-error-break vop error-trap (error-number-or-lose error-code) values))

(defun generate-error-code (vop error-code &rest values)
  #!+sb-doc
  "Generate-Error-Code Error-code Value*
  Emit code for an error with the specified Error-Code and context Values."
  (assemble (*elsewhere*)
    (let ((start-lab (gen-label)))
      (emit-label start-lab)
      (emit-error-break vop
                        (case error-code ; should be named ERROR-SYMBOL really
                          (invalid-arg-count-error invalid-arg-count-trap)
                          (t error-trap))
                        (error-number-or-lose error-code)
                        values)
      start-lab)))


;;;; PSEUDO-ATOMIC

;;; This is used to wrap operations which leave untagged memory lying
;;; around.  It's an operation which the AOP weenies would describe as
;;; having "cross-cutting concerns", meaning it appears all over the
;;; place and there's no logical single place to attach documentation.
;;; grep (mostly in src/runtime) is your friend

(defmacro maybe-pseudo-atomic (not-really-p &body body)
  `(if ,not-really-p
       (progn ,@body)
       (pseudo-atomic ,@body)))

;;; Unsafely clear pa flags so that the image can properly lose in a
;;; pa section.
#!+sb-thread
(defmacro %clear-pseudo-atomic ()
  '(inst mov (make-ea :qword :base thread-base-tn
              :disp (* n-word-bytes thread-pseudo-atomic-bits-slot))
    0))

#!+sb-safepoint
(defun emit-safepoint ()
  (inst test al-tn (make-ea :byte :disp gc-safepoint-page-addr)))

#!+sb-thread
(defmacro pseudo-atomic (&rest forms)
  #!+sb-safepoint-strictly
  `(progn ,@forms (emit-safepoint))
  #!-sb-safepoint-strictly
  (with-unique-names (label)
    `(let ((,label (gen-label)))
       (inst mov (make-ea :qword
                          :base thread-base-tn
                          :disp (* n-word-bytes thread-pseudo-atomic-bits-slot))
             rbp-tn)
       ,@forms
       (inst xor (make-ea :qword
                          :base thread-base-tn
                          :disp (* n-word-bytes thread-pseudo-atomic-bits-slot))
             rbp-tn)
       (inst jmp :z ,label)
       ;; if PAI was set, interrupts were disabled at the same time
       ;; using the process signal mask.
       (inst break pending-interrupt-trap)
       (emit-label ,label)
       #!+sb-safepoint
       ;; In this case, when allocation thinks a GC should be done, it
       ;; does not mark PA as interrupted, but schedules a safepoint
       ;; trap instead.  Let's take the opportunity to trigger that
       ;; safepoint right now.
       (emit-safepoint))))


#!-sb-thread
(defmacro pseudo-atomic (&rest forms)
  (with-unique-names (label)
    `(let ((,label (gen-label)))
       ;; FIXME: The MAKE-EA noise should become a MACROLET macro or
       ;; something. (perhaps SVLB, for static variable low byte)
       (inst mov (make-ea :qword :disp (+ nil-value
                                          (static-symbol-offset
                                           '*pseudo-atomic-bits*)
                                          (ash symbol-value-slot word-shift)
                                          (- other-pointer-lowtag)))
             rbp-tn)
       ,@forms
       (inst xor (make-ea :qword :disp (+ nil-value
                                          (static-symbol-offset
                                           '*pseudo-atomic-bits*)
                                          (ash symbol-value-slot word-shift)
                                          (- other-pointer-lowtag)))
             rbp-tn)
       (inst jmp :z ,label)
       ;; if PAI was set, interrupts were disabled at the same time
       ;; using the process signal mask.
       (inst break pending-interrupt-trap)
       (emit-label ,label))))

;;;; indexed references

(defmacro define-full-compare-and-swap
    (name type offset lowtag scs el-type &optional translate)
  `(progn
     (define-vop (,name)
         ,@(when translate `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg) :to :eval)
              (index :scs (any-reg) :to :result)
              (old-value :scs ,scs :target rax)
              (new-value :scs ,scs))
       (:arg-types ,type tagged-num ,el-type ,el-type)
       (:temporary (:sc descriptor-reg :offset rax-offset
                        :from (:argument 2) :to :result :target value)  rax)
       (:results (value :scs ,scs))
       (:result-types ,el-type)
       (:generator 5
         (move rax old-value)
         (inst cmpxchg (make-ea :qword :base object :index index
                                :scale (ash 1 (- word-shift n-fixnum-tag-bits))
                                :disp (- (* ,offset n-word-bytes) ,lowtag))
               new-value :lock)
         (move value rax)))))

(defmacro define-full-reffer (name type offset lowtag scs el-type &optional translate)
  `(progn
     (define-vop (,name)
       ,@(when translate
           `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
              (index :scs (any-reg)))
       (:arg-types ,type tagged-num)
       (:results (value :scs ,scs))
       (:result-types ,el-type)
       (:generator 3                    ; pw was 5
         (inst mov value (make-ea :qword :base object :index index
                                  :scale (ash 1 (- word-shift n-fixnum-tag-bits))
                                  :disp (- (* ,offset n-word-bytes)
                                           ,lowtag)))))
     (define-vop (,(symbolicate name "-C"))
       ,@(when translate
           `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg)))
       (:info index)
       (:arg-types ,type
                   (:constant (load/store-index ,n-word-bytes ,(eval lowtag)
                                                ,(eval offset))))
       (:results (value :scs ,scs))
       (:result-types ,el-type)
       (:generator 2                    ; pw was 5
         (inst mov value (make-ea :qword :base object
                                  :disp (- (* (+ ,offset index) n-word-bytes)
                                           ,lowtag)))))))

(defmacro define-full-reffer+offset (name type offset lowtag scs el-type &optional translate)
  `(progn
     (define-vop (,name)
       ,@(when translate
           `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
              (index :scs (any-reg)))
       (:info offset)
       (:arg-types ,type tagged-num
                   (:constant (constant-displacement other-pointer-lowtag
                                                     n-word-bytes vector-data-offset)))
       (:results (value :scs ,scs))
       (:result-types ,el-type)
       (:generator 3                    ; pw was 5
         (inst mov value (make-ea :qword :base object :index index
                                  :scale (ash 1 (- word-shift n-fixnum-tag-bits))
                                  :disp (- (* (+ ,offset offset) n-word-bytes)
                                           ,lowtag)))))
     (define-vop (,(symbolicate name "-C"))
       ,@(when translate
           `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg)))
       (:info index offset)
       (:arg-types ,type
                   (:constant (load/store-index ,n-word-bytes ,(eval lowtag)
                                                ,(eval offset)))
                   (:constant (constant-displacement other-pointer-lowtag
                                                     n-word-bytes vector-data-offset)))
       (:results (value :scs ,scs))
       (:result-types ,el-type)
       (:generator 2                    ; pw was 5
         (inst mov value (make-ea :qword :base object
                                  :disp (- (* (+ ,offset index offset) n-word-bytes)
                                           ,lowtag)))))))

(defmacro define-full-setter (name type offset lowtag scs el-type &optional translate)
  `(progn
     (define-vop (,name)
       ,@(when translate
           `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
              (index :scs (any-reg))
              (value :scs ,scs :target result))
       (:arg-types ,type tagged-num ,el-type)
       (:results (result :scs ,scs))
       (:result-types ,el-type)
       (:generator 4                    ; was 5
         (inst mov (make-ea :qword :base object :index index
                            :scale (ash 1 (- word-shift n-fixnum-tag-bits))
                            :disp (- (* ,offset n-word-bytes) ,lowtag))
               value)
         (move result value)))
     (define-vop (,(symbolicate name "-C"))
       ,@(when translate
           `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
              (value :scs ,scs :target result))
       (:info index)
       (:arg-types ,type
                   (:constant (load/store-index ,n-word-bytes ,(eval lowtag)
                                                ,(eval offset)))
                   ,el-type)
       (:results (result :scs ,scs))
       (:result-types ,el-type)
       (:generator 3                    ; was 5
         (inst mov (make-ea :qword :base object
                            :disp (- (* (+ ,offset index) n-word-bytes)
                                     ,lowtag))
               value)
         (move result value)))))

(defmacro define-full-setter+offset (name type offset lowtag scs el-type &optional translate)
  `(progn
     (define-vop (,name)
       ,@(when translate
           `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
              (index :scs (any-reg))
              (value :scs ,scs :target result))
       (:info offset)
       (:arg-types ,type tagged-num
                   (:constant (constant-displacement other-pointer-lowtag
                                                     n-word-bytes
                                                     vector-data-offset))
                   ,el-type)
       (:results (result :scs ,scs))
       (:result-types ,el-type)
       (:generator 4                    ; was 5
         (inst mov (make-ea :qword :base object :index index
                            :scale (ash 1 (- word-shift n-fixnum-tag-bits))
                            :disp (- (* (+ ,offset offset) n-word-bytes) ,lowtag))
               value)
         (move result value)))
     (define-vop (,(symbolicate name "-C"))
       ,@(when translate
           `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
              (value :scs ,scs :target result))
       (:info index offset)
       (:arg-types ,type
                   (:constant (load/store-index ,n-word-bytes ,(eval lowtag)
                                                ,(eval offset)))
                   (:constant (constant-displacement other-pointer-lowtag
                                                     n-word-bytes
                                                     vector-data-offset))
                   ,el-type)
       (:results (result :scs ,scs))
       (:result-types ,el-type)
       (:generator 3                    ; was 5
         (inst mov (make-ea :qword :base object
                            :disp (- (* (+ ,offset index offset) n-word-bytes)
                                     ,lowtag))
               value)
         (move result value)))))

;;; helper for alien stuff.

(def!macro with-pinned-objects ((&rest objects) &body body)
  #!+sb-doc
  "Arrange with the garbage collector that the pages occupied by
OBJECTS will not be moved in memory for the duration of BODY.
Useful for e.g. foreign calls where another thread may trigger
collection."
  (if objects
      (let ((pins (make-gensym-list (length objects)))
            (wpo (sb!xc:gensym "WITH-PINNED-OBJECTS-THUNK")))
        ;; BODY is stuffed in a function to preserve the lexical
        ;; environment.
        `(flet ((,wpo () (progn ,@body)))
           ;; The cross-compiler prints either "unknown type: COMPILER-NOTE" at
           ;; each use of W-P-O prior to 'ir1report' being compiled, or else
           ;; "could not stack allocate". Kill it with fire :-(
           (declare (muffle-conditions #+sb-xc compiler-note #-sb-xc t))
           ;; PINS are dx-allocated in case the compiler for some
           ;; unfathomable reason decides to allocate value-cells
           ;; for them -- since we have DX value-cells on x86oid
           ;; platforms this still forces them on the stack.
           (dx-let ,(mapcar #'list pins objects)
             (multiple-value-prog1 (,wpo)
               ;; TOUCH-OBJECT has a VOP with an empty body: compiler
               ;; thinks we're using the argument and doesn't flush
               ;; the variable, but we don't have to pay any extra
               ;; beyond that -- and MULTIPLE-VALUE-PROG1 keeps them
               ;; live till the body has finished. *whew*
               ,@(mapcar (lambda (pin)
                           `(touch-object ,pin))
                         pins)))))
      `(progn ,@body)))

;;; Emit the most compact form of the test immediate instruction,
;;; using an 8 bit test when the immediate is only 8 bits and the
;;; value is one of the four low registers (rax, rbx, rcx, rdx) or the
;;; control stack.
(defun emit-optimized-test-inst (x y)
  (typecase y
    ((unsigned-byte 7)
     ;; If we knew that the sign bit would not be tested, this could
     ;; handle (unsigned-byte 8) constants. But since we don't know,
     ;; we assume that it's not ok to change the test such that the S flag
     ;; comes out possibly differently.
     (let ((offset (tn-offset x)))
       (cond ((and (sc-is x any-reg descriptor-reg signed-reg unsigned-reg)
                   (or (= offset rax-offset) (= offset rbx-offset)
                       (= offset rcx-offset) (= offset rdx-offset)))
              (inst test (reg-in-size x :byte) y))
             ((sc-is x control-stack)
              (inst test (make-ea :byte :base rbp-tn
                                  :disp (frame-byte-offset offset))
                    y))
             (t
              (inst test x y)))))
    (t
     (inst test x y))))
