;;;; a bunch of handy macros for the x86

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;; We can load/store into fp registers through the top of stack
;;; %st(0) (fr0 here). Loads imply a push to an empty register which
;;; then changes all the reg numbers. These macros help manage that.

;;; Use this when we don't have to load anything. It preserves old tos
;;; value, but probably destroys tn with operation.
(defmacro with-tn@fp-top((tn) &body body)
  `(progn
    (unless (zerop (tn-offset ,tn))
      (inst fxch ,tn))
    ,@body
    (unless (zerop (tn-offset ,tn))
      (inst fxch ,tn))))

;;; Use this to prepare for load of new value from memory. This
;;; changes the register numbering so the next instruction had better
;;; be a FP load from memory; a register load from another register
;;; will probably be loading the wrong register!
(defmacro with-empty-tn@fp-top((tn) &body body)
  `(progn
     (inst fstp ,tn)
     ,@body
     (unless (zerop (tn-offset ,tn))
       (inst fxch ,tn))))                ; save into new dest and restore st(0)

;;;; instruction-like macros

(defmacro move (dst src)
  #!+sb-doc
  "Move SRC into DST unless they are location=."
  (once-only ((n-dst dst)
              (n-src src))
    `(unless (location= ,n-dst ,n-src)
       (inst mov ,n-dst ,n-src))))

(defmacro align-stack-pointer (tn)
  #!-darwin (declare (ignore tn))
  #!+darwin
  ;; 16 byte alignment.
  `(inst and ,tn #xfffffff0))

(defmacro make-ea-for-object-slot (ptr slot lowtag &optional (size :dword))
  `(make-ea ,size :base ,ptr :disp (- (* ,slot n-word-bytes) ,lowtag)))

(defmacro loadw (value ptr &optional (slot 0) (lowtag 0))
  `(inst mov ,value (make-ea-for-object-slot ,ptr ,slot ,lowtag)))

(defmacro storew (value ptr &optional (slot 0) (lowtag 0))
  (once-only ((value value))
    `(inst mov (make-ea-for-object-slot ,ptr ,slot ,lowtag) ,value)))

;;; A handy macro for storing widetags.
(defmacro storeb (value ptr &optional (slot 0) (lowtag 0))
  (once-only ((value value))
    `(inst mov (make-ea-for-object-slot ,ptr ,slot ,lowtag :byte) ,value)))

(defmacro pushw (ptr &optional (slot 0) (lowtag 0))
  `(inst push (make-ea-for-object-slot ,ptr ,slot ,lowtag)))

(defmacro popw (ptr &optional (slot 0) (lowtag 0))
  `(inst pop (make-ea-for-object-slot ,ptr ,slot ,lowtag)))

(defmacro make-ea-for-vector-data (object &key (size :dword) (offset 0)
                                   index (scale (ash (width-bits size) -3)))
  `(make-ea ,size :base ,object :index ,index :scale ,scale
            :disp (- (+ (* vector-data-offset n-word-bytes)
                        (* ,offset ,scale))
                     other-pointer-lowtag)))

;;;; macros to generate useful values

(defmacro load-symbol (reg symbol)
  `(inst mov ,reg (+ nil-value (static-symbol-offset ,symbol))))

(defmacro make-ea-for-symbol-value (symbol &optional (width :dword))
  (declare (type symbol symbol))
  `(make-ea ,width
    :disp (+ nil-value
           (static-symbol-offset ',symbol)
           (ash symbol-value-slot word-shift)
           (- other-pointer-lowtag))))

(defmacro load-symbol-value (reg symbol)
  `(inst mov ,reg (make-ea-for-symbol-value ,symbol)))

(defmacro store-symbol-value (reg symbol)
  `(inst mov (make-ea-for-symbol-value ,symbol) ,reg))

#!+sb-thread
(defmacro make-ea-for-symbol-tls-index (symbol)
  (declare (type symbol symbol))
  `(make-ea :dword
    :disp (+ nil-value
           (static-symbol-offset ',symbol)
           (ash symbol-tls-index-slot word-shift)
           (- other-pointer-lowtag))))

#!+sb-thread
(defmacro load-tl-symbol-value (reg symbol)
  `(progn
    (inst mov ,reg (make-ea-for-symbol-tls-index ,symbol))
    (inst mov ,reg (make-ea :dword :base ,reg) :fs)))
#!-sb-thread
(defmacro load-tl-symbol-value (reg symbol) `(load-symbol-value ,reg ,symbol))

#!+sb-thread
(defmacro store-tl-symbol-value (reg symbol temp)
  `(progn
    (inst mov ,temp (make-ea-for-symbol-tls-index ,symbol))
    (inst mov (make-ea :dword :base ,temp) ,reg :fs)))
#!-sb-thread
(defmacro store-tl-symbol-value (reg symbol temp)
  (declare (ignore temp))
  `(store-symbol-value ,reg ,symbol))

(defmacro load-binding-stack-pointer (reg)
  #!+sb-thread
  `(progn
     (inst mov ,reg (make-ea :dword
                             :disp (* 4 thread-binding-stack-pointer-slot))
           :fs))
  #!-sb-thread
  `(load-symbol-value ,reg *binding-stack-pointer*))

(defmacro store-binding-stack-pointer (reg)
  #!+sb-thread
  `(progn
     (inst mov (make-ea :dword
                        :disp (* 4 thread-binding-stack-pointer-slot))
           ,reg :fs))
  #!-sb-thread
  `(store-symbol-value ,reg *binding-stack-pointer*))

(defmacro load-type (target source &optional (offset 0))
  #!+sb-doc
  "Loads the type bits of a pointer into target independent of
   byte-ordering issues."
  (once-only ((n-target target)
              (n-source source)
              (n-offset offset))
    (ecase *backend-byte-order*
      (:little-endian
       `(inst mov ,n-target
              (make-ea :byte :base ,n-source :disp ,n-offset)))
      (:big-endian
       `(inst mov ,n-target
              (make-ea :byte :base ,n-source
                             :disp (+ ,n-offset (1- n-word-bytes))))))))

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

(defun allocation-dynamic-extent (alloc-tn size lowtag)
  (inst sub esp-tn size)
  ;; FIXME: SIZE _should_ be double-word aligned (suggested but
  ;; unfortunately not enforced by PAD-DATA-BLOCK and
  ;; WITH-FIXED-ALLOCATION), so that ESP is always divisible by 8 (for
  ;; 32-bit lispobjs).  In that case, this AND instruction is
  ;; unneccessary and could be removed.  If not, explain why.  -- CSR,
  ;; 2004-03-30
  (inst and esp-tn (lognot lowtag-mask))
  (aver (not (location= alloc-tn esp-tn)))
  (inst lea alloc-tn (make-ea :byte :base esp-tn :disp lowtag))
  (values))

(defun allocation-notinline (alloc-tn size)
  (let* ((alloc-tn-offset (tn-offset alloc-tn))
         ;; C call to allocate via dispatch routines. Each
         ;; destination has a special entry point. The size may be a
         ;; register or a constant.
         (tn-text (ecase alloc-tn-offset
                    (#.eax-offset "eax")
                    (#.ecx-offset "ecx")
                    (#.edx-offset "edx")
                    (#.ebx-offset "ebx")
                    (#.esi-offset "esi")
                    (#.edi-offset "edi")))
         (size-text (case size (8 "8_") (16 "16_") (t ""))))
    (unless (or (eql size 8) (eql size 16))
      (unless (and (tn-p size) (location= alloc-tn size))
        (inst mov alloc-tn size)))
    (inst call (make-fixup (concatenate 'string
                                         "alloc_" size-text
                                         "to_" tn-text)
                           :foreign))))

(defun allocation-inline (alloc-tn size)
  (let ((ok (gen-label))
        (done (gen-label))
        (free-pointer
         (make-ea :dword :disp
                  #!+sb-thread (* n-word-bytes thread-alloc-region-slot)
                  #!-sb-thread (make-fixup "boxed_region" :foreign)
                  :scale 1)) ; thread->alloc_region.free_pointer
        (end-addr
         (make-ea :dword :disp
                  #!+sb-thread (* n-word-bytes (1+ thread-alloc-region-slot))
                  #!-sb-thread (make-fixup "boxed_region" :foreign 4)
                  :scale 1)))   ; thread->alloc_region.end_addr
    (unless (and (tn-p size) (location= alloc-tn size))
      (inst mov alloc-tn size))
    (inst add alloc-tn free-pointer #!+sb-thread :fs)
    (inst cmp alloc-tn end-addr #!+sb-thread :fs)
    (inst jmp :be ok)
    (let ((dst (ecase (tn-offset alloc-tn)
                 (#.eax-offset "alloc_overflow_eax")
                 (#.ecx-offset "alloc_overflow_ecx")
                 (#.edx-offset "alloc_overflow_edx")
                 (#.ebx-offset "alloc_overflow_ebx")
                 (#.esi-offset "alloc_overflow_esi")
                 (#.edi-offset "alloc_overflow_edi"))))
      (inst call (make-fixup dst :foreign)))
    (inst jmp-short done)
    (emit-label ok)
    ;; Swap ALLOC-TN and FREE-POINTER
    (cond ((and (tn-p size) (location= alloc-tn size))
           ;; XCHG is extremely slow, use the xor swap trick
           (inst xor alloc-tn free-pointer #!+sb-thread :fs)
           (inst xor free-pointer alloc-tn #!+sb-thread :fs)
           (inst xor alloc-tn free-pointer #!+sb-thread :fs))
          (t
           ;; It's easier if SIZE is still available.
           (inst mov free-pointer alloc-tn #!+sb-thread :fs)
           (inst sub alloc-tn size)))
    (emit-label done))
  (values))


;;; Emit code to allocate an object with a size in bytes given by
;;; SIZE.  The size may be an integer or a TN. If Inline is a VOP
;;; node-var then it is used to make an appropriate speed vs size
;;; decision.

;;; Allocation should only be used inside a pseudo-atomic section, which
;;; should also cover subsequent initialization of the object.

;;; (FIXME: so why aren't we asserting this?)

(defun allocation (alloc-tn size &optional inline dynamic-extent lowtag)
  (cond
    (dynamic-extent
     (allocation-dynamic-extent alloc-tn size lowtag))
    ((or (null inline) (policy inline (>= speed space)))
     (allocation-inline alloc-tn size))
    (t
     (allocation-notinline alloc-tn size)))
  (when (and lowtag (not dynamic-extent))
    (inst lea alloc-tn (make-ea :byte :base alloc-tn :disp lowtag)))
  (values))

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
    (inst int 3)                        ; i386 breakpoint instruction
    ;; CLH 20060314
    ;; On Darwin, we need to use #x0b0f instead of int3 in order
    ;; to generate a SIGILL instead of a SIGTRAP as darwin/x86
    ;; doesn't seem to be reliably firing SIGTRAP
    ;; handlers. Hopefully this will be fixed by Apple at a
    ;; later date.
    #!+ud2-breakpoints
    (inst word #x0b0f)
    ;; The return PC points here; note the location for the debugger.
    (when vop
      (note-this-location vop :internal-error))
    (inst byte kind)                    ; e.g. trap_xyyy
    (with-adjustable-vector (vector)    ; interr arguments
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
        (inst byte (aref vector i))))))

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
      (emit-error-break vop error-trap (error-number-or-lose error-code) values)
      start-lab)))


;;;; PSEUDO-ATOMIC

;;; This is used to wrap operations which leave untagged memory lying
;;; around.  It's an operation which the AOP weenies would describe as
;;; having "cross-cutting concerns", meaning it appears all over the
;;; place and there's no logical single place to attach documentation.
;;; grep (mostly in src/runtime) is your friend

;;; KLUDGE: since the stack on the x86 is treated conservatively, it
;;; does not matter whether a signal occurs during construction of a
;;; dynamic-extent object, as the half-finished construction of the
;;; object will not cause any difficulty.  We can therefore elide
(defmacro maybe-pseudo-atomic (not-really-p &body forms)
  `(if ,not-really-p
       (progn ,@forms)
       (pseudo-atomic ,@forms)))

;;; Unsafely clear pa flags so that the image can properly lose in a
;;; pa section.
#!+sb-thread
(defmacro %clear-pseudo-atomic ()
  '(inst mov (make-ea :dword :disp (* 4 thread-pseudo-atomic-bits-slot)) 0 :fs))

#!+sb-thread
(defmacro pseudo-atomic (&rest forms)
  (with-unique-names (label)
    `(let ((,label (gen-label)))
       (inst mov (make-ea :dword :disp (* 4 thread-pseudo-atomic-bits-slot))
             ebp-tn :fs)
       ,@forms
       (inst xor (make-ea :dword :disp (* 4 thread-pseudo-atomic-bits-slot))
             ebp-tn :fs)
       (inst jmp :z ,label)
       ;; if PAI was set, interrupts were disabled at the same time
       ;; using the process signal mask.
       (inst break pending-interrupt-trap)
       (emit-label ,label))))

#!-sb-thread
(defmacro pseudo-atomic (&rest forms)
  (with-unique-names (label)
    `(let ((,label (gen-label)))
       (inst mov (make-ea-for-symbol-value *pseudo-atomic-bits* :dword)
             ebp-tn)
       ,@forms
       (inst xor (make-ea-for-symbol-value *pseudo-atomic-bits* :dword)
             ebp-tn)
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
              (index :scs (any-reg immediate unsigned-reg) :to :result)
              (old-value :scs ,scs :target eax)
              (new-value :scs ,scs))
       (:arg-types ,type tagged-num ,el-type ,el-type)
       (:temporary (:sc descriptor-reg :offset eax-offset
                        :from (:argument 2) :to :result :target value)  eax)
       (:results (value :scs ,scs))
       (:result-types ,el-type)
       (:generator 5
         (move eax old-value)
         (let ((ea (sc-case index
                     (immediate
                      (make-ea :dword :base object
                               :disp (- (* (+ ,offset (tn-value index))
                                           n-word-bytes)
                                        ,lowtag)))
                     (unsigned-reg
                      (make-ea :dword :base object :index index :scale 4
                               :disp (- (* ,offset n-word-bytes)
                                        ,lowtag)))
                     (t
                      (make-ea :dword :base object :index index
                               :disp (- (* ,offset n-word-bytes)
                                        ,lowtag))))))
           (inst cmpxchg ea new-value :lock))
         (move value eax)))))

(defmacro define-full-reffer (name type offset lowtag scs el-type &optional translate)
  `(progn
     (define-vop (,name)
       ,@(when translate
           `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
              (index :scs (any-reg immediate unsigned-reg)))
       (:arg-types ,type tagged-num)
       (:results (value :scs ,scs))
       (:result-types ,el-type)
       (:generator 3                    ; pw was 5
         (sc-case index
           (immediate
            (inst mov value (make-ea :dword :base object
                                     :disp (- (* (+ ,offset (tn-value index))
                                                 n-word-bytes)
                                              ,lowtag))))
           (unsigned-reg
            (inst mov value (make-ea :dword :base object :index index :scale 4
                                     :disp (- (* ,offset n-word-bytes)
                                              ,lowtag))))
           (t
            (inst mov value (make-ea :dword :base object :index index
                                     :disp (- (* ,offset n-word-bytes)
                                              ,lowtag)))))))))

(defmacro define-full-reffer+offset (name type offset lowtag scs el-type &optional translate)
  `(progn
     (define-vop (,name)
       ,@(when translate
           `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
              (index :scs (any-reg immediate unsigned-reg)))
       (:arg-types ,type tagged-num
                   (:constant (constant-displacement ,lowtag sb!vm:n-word-bytes ,offset)))
       (:info offset)
       (:results (value :scs ,scs))
       (:result-types ,el-type)
       (:generator 3                    ; pw was 5
         (sc-case index
           (immediate
            (inst mov value (make-ea :dword :base object
                                     :disp (- (* (+ ,offset
                                                    (tn-value index)
                                                    offset)
                                                 n-word-bytes)
                                              ,lowtag))))
           (unsigned-reg
            (inst mov value (make-ea :dword :base object :index index :scale 4
                                     :disp (- (* (+ ,offset offset)
                                                 n-word-bytes)
                                              ,lowtag))))
           (t
            (inst mov value (make-ea :dword :base object :index index
                                     :disp (- (* (+ ,offset offset)
                                                 n-word-bytes)
                                              ,lowtag)))))))))

(defmacro define-full-setter (name type offset lowtag scs el-type &optional translate)
  `(progn
     (define-vop (,name)
       ,@(when translate
           `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
              (index :scs (any-reg immediate))
              (value :scs ,scs :target result))
       (:arg-types ,type tagged-num ,el-type)
       (:results (result :scs ,scs))
       (:result-types ,el-type)
       (:generator 4                    ; was 5
         (sc-case index
           (immediate
            (inst mov (make-ea :dword :base object
                               :disp (- (* (+ ,offset (tn-value index))
                                           n-word-bytes)
                                        ,lowtag))
                  value))
           (t
            (inst mov (make-ea :dword :base object :index index
                               :disp (- (* ,offset n-word-bytes) ,lowtag))
                  value)))
        (move result value)))))

(defmacro define-full-setter+offset (name type offset lowtag scs el-type &optional translate)
  `(progn
     (define-vop (,name)
       ,@(when translate
           `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
              (index :scs (any-reg immediate))
              (value :scs ,scs :target result))
       (:info offset)
       (:arg-types ,type tagged-num
                   (:constant (constant-displacement ,lowtag sb!vm:n-word-bytes ,offset)) ,el-type)
       (:results (result :scs ,scs))
       (:result-types ,el-type)
       (:generator 4                    ; was 5
         (sc-case index
           (immediate
            (inst mov (make-ea :dword :base object
                               :disp (- (* (+ ,offset (tn-value index) offset)
                                           n-word-bytes)
                                        ,lowtag))
                  value))
           (t
            (inst mov (make-ea :dword :base object :index index
                               :disp (- (* (+ ,offset offset)
                                           n-word-bytes) ,lowtag))
                  value)))
        (move result value)))))

;;; helper for alien stuff.

(def!macro with-pinned-objects ((&rest objects) &body body)
  "Arrange with the garbage collector that the pages occupied by
OBJECTS will not be moved in memory for the duration of BODY.
Useful for e.g. foreign calls where another thread may trigger
collection."
  (if objects
      (let ((pins (make-gensym-list (length objects)))
            (wpo (block-gensym "WPO")))
        ;; BODY is stuffed in a function to preserve the lexical
        ;; environment.
        `(flet ((,wpo () (progn ,@body)))
           (declare (muffle-conditions compiler-note))
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
