;;;; a bunch of handy macros for the ARM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;; Instruction-like macros.

(defmacro move (dst src &optional (predicate :al))
  "Move SRC into DST unless they are location=."
  (once-only ((n-dst dst)
              (n-src src))
    `(unless (location= ,n-dst ,n-src)
       (inst mov ,predicate ,n-dst ,n-src))))

(macrolet
    ((def (type inst)
       (let ((real-tn-fn (symbolicate 'complex- type '-reg-real-tn))
             (imag-tn-fn (symbolicate 'complex- type '-reg-imag-tn)))
         `(progn
            (defmacro ,(symbolicate 'move- type)
                (dst src &optional (predicate :al))
              (once-only ((n-dst dst)
                          (n-src src))
                `(unless (location= ,n-dst ,n-src)
                   (inst ,',inst ,predicate ,n-dst ,n-src))))
            (defmacro ,(symbolicate 'move-complex- type)
                (dst src &optional (predicate :al))
              (once-only ((n-dst dst)
                          (n-src src))
                `(unless (location= ,n-dst ,n-src)
                   ;; Note that the complex (single and double) float
                   ;; registers are aligned to paired underlying
                   ;; (single and double) registers, so there is no
                   ;; need to worry about overlap.
                   (let ((src-real (,',real-tn-fn ,n-src))
                         (dst-real (,',real-tn-fn ,n-dst)))
                     (inst ,',inst ,predicate dst-real src-real))
                   (let ((src-imag (,',imag-tn-fn ,n-src))
                         (dst-imag (,',imag-tn-fn ,n-dst)))
                     (inst ,', inst ,predicate dst-imag src-imag)))))))))
  (def single fcpys)
  (def double fcpyd))

(macrolet
    ((def (op inst shift)
       `(defmacro ,op (object base
                       &optional (offset 0) (lowtag 0) (predicate :al))
          `(inst ,',inst ,predicate ,object
                 (@ ,base (- (ash ,offset ,,shift) ,lowtag))))))
  (def loadw ldr word-shift)
  (def storew str word-shift))

(defmacro load-symbol (reg symbol)
  (once-only ((reg reg) (symbol symbol))
    `(progn
       (composite-immediate-instruction add ,reg null-tn (static-symbol-offset ,symbol)))))

(defmacro load-symbol-value (reg symbol &optional (predicate :al))
  `(inst ldr ,predicate ,reg
         (@ null-tn
            (+ (static-symbol-offset ',symbol)
               (ash symbol-value-slot word-shift)
               (- other-pointer-lowtag)))))

(defmacro store-symbol-value (reg symbol &optional (predicate :al))
  `(inst str ,predicate ,reg
         (@ null-tn
            (+ (static-symbol-offset ',symbol)
               (ash symbol-value-slot word-shift)
               (- other-pointer-lowtag)))))

(defmacro load-type (target source &optional (offset 0) (predicate :al))
  "Loads the type bits of a pointer into target independent of
  byte-ordering issues."
  (once-only ((n-target target)
              (n-source source)
              (n-offset offset))
    (let ((target-offset (ecase *backend-byte-order*
                           (:little-endian n-offset)
                           (:big-endian `(+ ,n-offset (1- n-word-bytes))))))
      `(inst ldrb ,predicate ,n-target (@ ,n-source ,target-offset)))))

;;; Macros to handle the fact that our stack pointer isn't actually in
;;; a register (or won't be, by the time we're done).

(defmacro load-csp (target &optional (predicate :al))
  `(load-symbol-value ,target *control-stack-pointer* ,predicate))

(defmacro store-csp (source &optional (predicate :al))
  `(store-symbol-value ,source *control-stack-pointer* ,predicate))

;;; Macros to handle the fact that we cannot use the machine native call and
;;; return instructions.

(defmacro lisp-jump (function)
  "Jump to the lisp function FUNCTION."
  `(inst add pc-tn ,function
         (- (ash simple-fun-insts-offset word-shift)
            fun-pointer-lowtag)))

(defmacro lisp-return (return-pc return-style)
  "Return to RETURN-PC."
  `(progn
     ;; Indicate a single-valued return by clearing all of the status
     ;; flags, or a multiple-valued return by setting all of the status
     ;; flags.
     ,(ecase return-style
             (:single-value '(inst msr (cpsr :f) 0))
             (:multiple-values '(inst msr (cpsr :f) #xf0000000))
             (:known))
     #+(or) ;; Doesn't work, can't have a negative immediate value.
     (inst add pc-tn ,return-pc (- 4 other-pointer-lowtag))
     (inst sub pc-tn ,return-pc (- other-pointer-lowtag 4))))

(defmacro emit-return-pc (label)
  "Emit a return-pc header word.  LABEL is the label to use for this return-pc."
  `(progn
     (emit-alignment n-lowtag-bits)
     (emit-label ,label)
     (inst lra-header-word)))


;;;; Stack TN's

;;; Move a stack TN to a register and vice-versa.
(defun load-stack-offset (reg stack stack-tn &optional (predicate :al))
  (let ((offset (tn-byte-offset stack-tn)))
    (cond ((or (tn-p offset)
               (typep offset '(unsigned-byte 12)))
           (inst ldr predicate reg (@ stack offset)))
          (t
           (load-immediate-word reg offset)
           (inst ldr predicate reg (@ stack reg))))))

(defmacro load-stack-tn (reg stack &optional (predicate :al))
  `(let ((reg ,reg)
         (stack ,stack))
     (sc-case stack
       ((control-stack)
        (load-stack-offset reg cfp-tn stack ,predicate)))))

(defun store-stack-offset (reg stack stack-tn &optional (predicate :al))
  (let ((offset (tn-byte-offset stack-tn)))
    (cond ((or (typep offset '(unsigned-byte 12))
               (tn-p offset))
           (inst str predicate reg (@ stack offset)))
          (t
           (let ((low (ldb (byte 12 0) offset))
                 (high (mask-field (byte 20 12) offset)))
             ;; KLUDGE:
             ;; Have to do this because it is used in move vops
             ;; which do not have temporary registers.
             ;; The debugger will be not happy.
             (composite-immediate-instruction add stack stack high)
             (inst str predicate reg (@ stack low))
             (composite-immediate-instruction sub stack stack high))))))

(defmacro store-stack-tn (stack reg &optional (predicate :al))
  `(let ((stack ,stack)
         (reg ,reg))
     (sc-case stack
       ((control-stack)
        (store-stack-offset reg cfp-tn stack ,predicate)))))

(defmacro maybe-load-stack-tn (reg reg-or-stack)
  "Move the TN Reg-Or-Stack into Reg if it isn't already there."
  (once-only ((n-reg reg)
              (n-stack reg-or-stack))
    `(sc-case ,n-reg
       ((any-reg descriptor-reg)
        (sc-case ,n-stack
          ((any-reg descriptor-reg)
           (move ,n-reg ,n-stack))
          ((control-stack)
           (load-stack-offset ,n-reg cfp-tn ,n-stack)))))))

;;;; Storage allocation:


;;; This is the main mechanism for allocating memory in the lisp heap.
;;;
;;; The allocated space is stored in RESULT-TN with the lowtag LOWTAG
;;; applied.  The amount of space to be allocated is SIZE bytes (which
;;; must be a multiple of the lisp object size).
;;;
;;; Each platform seems to have its own slightly different way to do
;;; heap allocation, taking various different options as parameters.
;;; For ARM, we take the bare minimum parameters, RESULT-TN, SIZE, and
;;; LOWTAG, and we require a single temporary register called FLAG-TN
;;; to emphasize the parallelism with PSEUDO-ATOMIC (which must
;;; surround a call to ALLOCATION anyway), and to indicate that the
;;; P-A FLAG-TN is also acceptable here.

#+gencgc
(defun allocation-tramp (type alloc-tn size back-label)
  (when (integerp size)
    (load-immediate-word alloc-tn size))
  ;; Using the native stack is OK - the register values have fixnum nature.
  (inst word (logior #xe92d0000 ; PUSH {rN, lr}
                     (ash 1 (if (integerp size) (tn-offset alloc-tn) (tn-offset size)))
                     (ash 1 (tn-offset lr-tn))))
  ;; select the C function index as per *LINKAGE-SPACE-PREDEFINED-ENTRIES*
  (let ((index (if (eq type 'list) 1 0)))
    (inst ldr alloc-tn (@ null-tn (- (linkage-table-entry-address index)
                                     nil-value))))
  (inst blx alloc-tn)
  (inst word (logior #xe8bd0000 ; POP {rN, lr}
                     (ash 1 (tn-offset alloc-tn))
                     (ash 1 (tn-offset lr-tn))))
  (inst b back-label))

(defun allocation (type size lowtag result-tn &key flag-tn stack-allocate-p)
  #+cheneygc (declare (ignore type))
  ;; Normal allocation to the heap.
  (cond (stack-allocate-p
         (load-csp result-tn)
         (inst tst result-tn lowtag-mask)
         (inst add :ne result-tn result-tn n-word-bytes)
         (if (integerp size)
             (composite-immediate-instruction add flag-tn result-tn size)
             (inst add flag-tn result-tn size))
         (store-csp flag-tn)
         ;; :ne is from TST above, this needs to be done after the
         ;; stack pointer has been stored.
         (storew null-tn result-tn -1 0 :ne)
         (inst orr result-tn result-tn lowtag))
        #-gencgc
        (t
         (load-symbol-value flag-tn *allocation-pointer*)
         (inst add result-tn flag-tn lowtag)
         (if (integerp size)
             (composite-immediate-instruction add flag-tn flag-tn size)
             (inst add flag-tn flag-tn size))
         (store-symbol-value flag-tn *allocation-pointer*))
        #+gencgc
        (t
         (let ((region-disp (- mixed-region nil-value))
               (alloc (gen-label))
               (back-from-alloc (gen-label)))
           (inst ldr result-tn (@ null-tn region-disp)) ; free ptr
           (inst ldr flag-tn (@ null-tn (+ region-disp n-word-bytes))) ; end ptr
           (if (integerp size)
               (composite-immediate-instruction add result-tn result-tn size)
               (inst add result-tn result-tn size))
           (inst cmp result-tn flag-tn)
           (inst b :hi ALLOC)
           (inst str result-tn (@ null-tn region-disp)) ; free ptr

           (if (integerp size)
               (composite-immediate-instruction sub result-tn result-tn size)
               (inst sub result-tn result-tn size))

           (emit-label BACK-FROM-ALLOC)
           (when lowtag
             (inst orr result-tn result-tn lowtag))

           (assemble (:elsewhere)
             (emit-label ALLOC)
             (allocation-tramp type result-tn size BACK-FROM-ALLOC))))))

(defmacro with-fixed-allocation ((result-tn flag-tn type-code size
                                            &key (lowtag other-pointer-lowtag)
                                                 stack-allocate-p)
                                 &body body)
  "Do stuff to allocate an other-pointer object of fixed Size with a single
  word header having the specified Type-Code.  The result is placed in
  Result-TN, and Temp-TN is a non-descriptor temp (which may be randomly used
  by the body.)  The body is placed inside the PSEUDO-ATOMIC, and presumably
  initializes the object."
  (once-only ((result-tn result-tn) (flag-tn flag-tn)
              (type-code type-code) (size size) (lowtag lowtag))
    `(pseudo-atomic (,flag-tn)
       (allocation nil (pad-data-block ,size) ,lowtag ,result-tn
                   :flag-tn ,flag-tn
                   :stack-allocate-p ,stack-allocate-p)
       (load-immediate-word ,flag-tn (compute-object-header ,size ,type-code))
       (storew ,flag-tn ,result-tn 0 ,lowtag)
       ,@body)))

;;;; Error Code
(defun emit-error-break (vop kind code values)
  (assemble ()
    (when vop
      (note-this-location vop :internal-error))
    ;; Use the magic officially-undefined instruction that Linux
    ;; treats as generating SIGTRAP.
    (inst debug-trap)
    (emit-internal-error kind code values)
    (emit-alignment word-shift)))

(defun generate-error-code (vop error-code &rest values)
  "Generate-Error-Code Error-code Value*
  Emit code for an error with the specified Error-Code and context Values."
  (assemble (:elsewhere)
    (let ((start-lab (gen-label)))
      (emit-label start-lab)
      (emit-error-break vop error-trap (error-number-or-lose error-code) values)
      start-lab)))

;;;; PSEUDO-ATOMIC


;;; handy macro for making sequences look atomic

;;; With LINK being NIL this doesn't store the next PC in LR when
;;; calling do_pending_interrupt.
;;; This used by allocate-vector-on-heap, there's a comment explaining
;;; why it needs that.
(defmacro pseudo-atomic ((flag-tn &key (link t)) &body forms)
  `(progn
     (without-scheduling ()
       (store-symbol-value pc-tn *pseudo-atomic-atomic*))
     (assemble ()
       ,@forms)
     (without-scheduling ()
       (store-symbol-value null-tn *pseudo-atomic-atomic*)
       (load-symbol-value ,flag-tn *pseudo-atomic-interrupted*)
       ;; When *pseudo-atomic-interrupted* is not 0 it contains the address of
       ;; do_pending_interrupt
       (inst cmp ,flag-tn 0)
       ,(if link
            `(inst blx :ne ,flag-tn)
            `(inst bx :ne ,flag-tn)))))

;;;; memory accessor vop generators

(defmacro define-full-reffer (name type offset lowtag scs el-type
                              &optional translate)
  `(define-vop (,name)
     ,@(when translate
             `((:translate ,translate)))
     (:policy :fast-safe)
     (:args (object :scs (descriptor-reg))
            (index :scs (any-reg)))
     (:arg-types ,type tagged-num)
     (:temporary (:scs (interior-reg)) lip)
     (:results (value :scs ,scs))
     (:result-types ,el-type)
     (:generator 5
       (inst add lip object index)
       (loadw value lip ,offset ,lowtag))))

(defmacro define-full-setter (name type offset lowtag scs el-type
                              &optional translate)
  `(define-vop (,name)
     ,@(when translate
             `((:translate ,translate)))
     (:policy :fast-safe)
     (:args (object :scs (descriptor-reg))
            (index :scs (any-reg))
            (value :scs ,scs))
     (:arg-types ,type tagged-num ,el-type)
     (:temporary (:scs (interior-reg)) lip)
     (:generator 2
       (inst add lip object index)
       (storew value lip ,offset ,lowtag))))

(defmacro define-partial-reffer (name type size signed offset lowtag scs
                                 el-type &optional translate)
  `(define-vop (,name)
     ,@(when translate
             `((:translate ,translate)))
     (:policy :fast-safe)
     (:args (object :scs (descriptor-reg))
            (index :scs (unsigned-reg)))
     (:arg-types ,type positive-fixnum)
     (:results (value :scs ,scs))
     (:result-types ,el-type)
     (:temporary (:scs (interior-reg)) lip)
     (:generator 5
       ,(if (eq size :byte)
            '(inst add lip object index)
            '(inst add lip object (lsl index 1)))
       (inst ,(ecase size
                     (:byte (if signed 'ldrsb 'ldrb))
                     (:short (if signed 'ldrsh 'ldrh)))
             value (@ lip (- (* ,offset n-word-bytes) ,lowtag))))))

(defmacro define-partial-setter (name type size offset lowtag scs el-type
                                 &optional translate)
  `(define-vop (,name)
     ,@(when translate
             `((:translate ,translate)))
     (:policy :fast-safe)
     (:args (object :scs (descriptor-reg))
            (index :scs (unsigned-reg))
            (value :scs ,scs))
     (:arg-types ,type positive-fixnum ,el-type)
     (:temporary (:scs (interior-reg)) lip)
     (:generator 5
       ,(if (eq size :byte)
            '(inst add lip object index)
            '(inst add lip object (lsl index 1)))
       (inst ,(ecase size (:byte 'strb) (:short 'strh))
             value (@ lip (- (* ,offset n-word-bytes) ,lowtag))))))
