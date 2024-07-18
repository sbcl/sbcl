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

(defmacro move (dst src)
  "Move SRC into DST unless they are location=."
  (once-only ((n-dst dst)
              (n-src src))
    `(unless (location= ,n-dst ,n-src)
       (inst mov ,n-dst ,n-src))))

(defmacro move-float (dst src)
  (once-only ((n-dst dst)
              (n-src src))
    `(unless (location= ,n-dst ,n-src)
       (inst fmov ,n-dst ,n-src))))

(defmacro move-complex-double (dst src)
  (once-only ((n-dst dst)
              (n-src src))
    `(unless (location= ,n-dst ,n-src)
       (inst s-mov ,n-dst ,n-src))))

(defun logical-mask (x)
  (cond ((encode-logical-immediate x)
         x)
        (t
         (load-immediate-word tmp-tn x)
         tmp-tn)))

(defun load-store-offset (offset &optional (temp tmp-tn) (size 64))
  (cond ((ldr-str-offset-encodable offset size)
         offset)
        (t
         (load-immediate-word temp offset)
         temp)))

(macrolet
    ((def (op inst shift)
       `(defmacro ,op (object base
                       &optional (offset 0) (lowtag 0))
          `(inst ,',inst ,object
                 (@ ,base (load-store-offset (- (ash ,offset ,,shift) ,lowtag)))))))
  (def loadw ldr word-shift)
  (def storew str word-shift))

(defmacro storew-pair (object1 offset1 object2 offset2
                       base)
  (assert (= (eval offset1) (1- (eval offset2))))
  `(inst stp ,object1 ,object2 (@ ,base (* ,(eval offset1) n-word-bytes))))

(defmacro loadw-pair (object1 offset1 object2 offset2
                       base)
  (assert (= (eval offset1) (1- (eval offset2))))
  `(inst ldp ,object1 ,object2 (@ ,base (* ,(eval offset1) n-word-bytes))))

(defmacro load-symbol (reg symbol)
  (once-only ((reg reg) (symbol symbol))
    `(inst add ,reg null-tn (add-sub-immediate  (static-symbol-offset ,symbol)))))

(defmacro load-symbol-value (reg symbol)
  `(inst ldr ,reg (@ null-tn (load-store-offset (+ (static-symbol-offset ',symbol)
                                                   (ash symbol-value-slot word-shift)
                                                   (- other-pointer-lowtag))))))

(defmacro store-symbol-value (reg symbol)
  `(inst str ,reg
         (@ null-tn (load-store-offset (+ (static-symbol-offset ',symbol)
                                          (ash symbol-value-slot word-shift)
                                          (- other-pointer-lowtag))))))

(defmacro load-type (target source &optional (offset 0))
  "Loads the type bits of a pointer into target independent of
  byte-ordering issues."
  (once-only ((n-target target)
              (n-source source)
              (n-offset offset))
    (let ((target-offset #+little-endian n-offset
                         #+big-endian `(+ ,n-offset (1- n-word-bytes))))
      `(inst ldrb ,n-target (@ ,n-source ,target-offset)))))

(defmacro lisp-jump (lr)
  `(progn
     (inst add ,lr ,lr 4)
     (inst br ,lr)))

(defmacro lisp-return (lr return-style)
  "Return to RETURN-PC."
  `(progn
     ;; Indicate a single-valued return by clearing the Z flag
     ,@(ecase return-style
         (:single-value '((inst cmp null-tn 0)))
         (:multiple-values '((inst cmp zr-tn zr-tn)))
         (:known))
     (inst ret ,lr)))

;;;; Stack TN's

;;; Move a stack TN to a register and vice-versa.
(defun load-stack-offset (reg stack stack-tn)
  (inst ldr reg (@ stack (load-store-offset (tn-byte-offset stack-tn)))))

(defmacro load-stack-tn (reg stack)
  `(let ((reg ,reg)
         (stack ,stack))
     (sc-case stack
       ((control-stack)
        (load-stack-offset reg cfp-tn stack)))))

(defun store-stack-offset (reg stack stack-tn)
  (let ((offset (tn-byte-offset stack-tn)))
    (inst str reg (@ stack (load-store-offset offset)))))

(defmacro store-stack-tn (stack reg)
  `(let ((stack ,stack)
         (reg ,reg))
     (sc-case stack
       ((control-stack)
        (store-stack-offset reg cfp-tn stack)))))

(defmacro maybe-load-stack-tn (reg reg-or-stack)
  "Move the TN Reg-Or-Stack into Reg if it isn't already there."
  (once-only ((n-reg reg)
              (n-stack reg-or-stack))
    `(sc-case ,n-reg
       ((any-reg descriptor-reg non-descriptor-reg)
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

#+generational
(defun allocation-tramp (type alloc-tn size back-label systemp)
  (if (integerp size)
      (load-immediate-word tmp-tn size)
      (inst mov tmp-tn size))
  (let ((asm-routine (if systemp
                         (if (eq type 'list) 'sys-list-alloc-tramp 'sys-alloc-tramp)
                         (if (eq type 'list) 'list-alloc-tramp 'alloc-tramp))))
    (invoke-asm-routine asm-routine alloc-tn))
  (inst b back-label))

;;; Leaves the untagged pointer in TMP-TN,
;;; Allowing it to be used with STP later.
(defun allocation (type size lowtag result-tn
                   &key flag-tn
                        stack-allocate-p
                        overflow
                        (systemp (system-tlab-p type (sb-c::vop-node sb-assem::*current-vop*))))
  (declare (ignorable type))
  ;; Normal allocation to the heap.
  (if stack-allocate-p
      (assemble ()
        (cond ((aligned-stack-p stack-allocate-p)
               (assemble ()
                 (inst tst csp-tn lowtag-mask)
                 (inst b :eq skip)
                 (inst add csp-tn csp-tn 8)
                 (error-call nil 'sb-kernel::unreachable-error)
                 skip)
               (move tmp-tn csp-tn)
               (inst add csp-tn csp-tn (add-sub-immediate size result-tn))
               (inst add result-tn tmp-tn lowtag))
              (t
               (inst add tmp-tn csp-tn lowtag-mask)
               (inst and tmp-tn tmp-tn (lognot lowtag-mask))
               (inst add csp-tn tmp-tn (add-sub-immediate size result-tn))
               (inst add result-tn tmp-tn lowtag))))
      (let ((alloc (gen-label))
            #+sb-thread (tlab (if systemp
                                  (if (eq type 'list) thread-sys-cons-tlab-slot thread-sys-mixed-tlab-slot)
                                  (if (eq type 'list) thread-cons-tlab-slot thread-mixed-tlab-slot)))
            #-sb-thread (region-offset (if (eq type 'list)
                                           cons-region-offset
                                           mixed-region-offset))
            (back-from-alloc (gen-label)))
        #-sb-thread
        (progn
          (loadw result-tn null-tn 0 (- nil-value-offset region-offset))
          (loadw flag-tn null-tn 1 (- nil-value-offset region-offset)))
        #+sb-thread
        (inst ldp tmp-tn flag-tn (@ thread-tn (* n-word-bytes tlab)))
        (inst add result-tn tmp-tn (add-sub-immediate size result-tn))
        (inst cmp result-tn flag-tn)
        (inst b :hi ALLOC)
        #-sb-thread (inst str result-tn (@ null-tn (load-store-offset (- region-offset nil-value-offset))))
        #+sb-thread (storew result-tn thread-tn tlab)

        (emit-label BACK-FROM-ALLOC)
        (inst add result-tn tmp-tn lowtag)
        (assemble (:elsewhere)
          (emit-label ALLOC)
          (if overflow
              (funcall overflow)
              (allocation-tramp type
                                result-tn
                                size
                                BACK-FROM-ALLOC
                                systemp))))))

(defmacro with-fixed-allocation ((result-tn flag-tn type-code size
                                            &key (lowtag other-pointer-lowtag)
                                                 stack-allocate-p
                                                 (store-type-code t))
                                 &body body)
  "Do stuff to allocate an other-pointer object of fixed Size with a single
  word header having the specified Type-Code.  The result is placed in
  Result-TN, and Temp-TN is a non-descriptor temp (which may be randomly used
  by the body.)  The body is placed inside the PSEUDO-ATOMIC, and presumably
  initializes the object."
  (once-only ((result-tn result-tn) (flag-tn flag-tn)
              (type-code type-code) (size size) (lowtag lowtag)
              (stack-allocate-p stack-allocate-p))
    `(pseudo-atomic (,flag-tn :sync ,type-code
                     :elide-if ,stack-allocate-p)
       (allocation nil (pad-data-block ,size) ,lowtag ,result-tn
                   :flag-tn ,flag-tn
                   :stack-allocate-p ,stack-allocate-p)
       (when ,type-code
         (load-immediate-word ,flag-tn (compute-object-header ,size ,type-code))
         ,@(and store-type-code
                `((storew ,flag-tn ,result-tn 0 ,lowtag))))
       ,@body)))

;;;; Error Code
;;;; BRK accepts a 16-bit immediate
;;;; Encode the error kind in the first byte.
;;;; If KIND is ERROR-TRAP, then add CODE to that first byte.
;;;; Otherwise CODE goes into the byte following BRK.
;;;; The arguments are normally encoded by ENCODE-INTERNAL-ERROR-ARGS,
;;;; except for the first argument if it's a descriptor-reg or
;;;; any-reg, then it goes into the second byte of the BRK instruction
;;;; immediate.
;;;; Otherwise that second byte is 31 (the ZR register).
(defun emit-error-break (vop kind code values)
  (assemble ()
    (when vop
      (note-this-location vop :internal-error))
    (cond ((= kind invalid-arg-count-trap)
           ;; NARGS is implicitly assumed for invalid-arg-count
           (inst brk kind)
           (return-from emit-error-break))
          (t
           (let ((first-value (car values)))
             (inst brk (dpb (cond ((and (tn-p first-value)
                                        (sc-is first-value descriptor-reg any-reg unsigned-reg signed-reg))
                                   (pop values)
                                   (dpb (sc-case first-value
                                          (unsigned-reg 1)
                                          (signed-reg 2)
                                          (t 0))
                                        (byte 2 5)
                                        (tn-offset first-value)))
                                  (t
                                   zr-offset))
                            (byte 8 8)
                            (if (= kind error-trap)
                                (+ kind code)
                                kind)))
             (unless (= kind error-trap)
               (inst byte code)))))
    (encode-internal-error-args values)
    (emit-alignment 2)))

(defun generate-error-code (vop error-code &rest values)
  (apply #'generate-error-code+ nil vop error-code values))

(defun generate-error-code+ (preamble-emitter vop error-code &rest values)
  "Generate-Error-Code Error-code Value*
  Emit code for an error with the specified Error-Code and context Values."
  (assemble (:elsewhere)
    (let ((start-lab (gen-label)))
      (emit-label start-lab)
      (when preamble-emitter
        (funcall preamble-emitter))
      (emit-error-break vop
                        (if (eq error-code 'invalid-arg-count-error)
                            invalid-arg-count-trap
                            error-trap)
                        (error-number-or-lose error-code) values)
      start-lab)))

;;;; PSEUDO-ATOMIC

#+sb-safepoint
(defun emit-safepoint ()
  (inst ldr zr-tn (@ null-tn
                     (load-store-offset
                      (- (+ nil-value-offset gc-safepoint-trap-offset))))))

;;; handy macro for making sequences look atomic
(defmacro pseudo-atomic ((flag-tn &key elide-if (sync t)) &body forms)
  (declare (ignorable sync))
  #+sb-safepoint
  `(progn ,flag-tn (assemble () ,@forms) (unless ,elide-if (emit-safepoint)))
  #-sb-safepoint
  `(progn
     (unless ,elide-if
       (without-scheduling ()
         #-sb-thread
         (store-symbol-value csp-tn *pseudo-atomic-atomic*)
         #+sb-thread
         (inst str (32-bit-reg null-tn)
               (@ thread-tn
                  (* n-word-bytes thread-pseudo-atomic-bits-slot)))))
     (assemble ()
       ,@forms)
     (unless ,elide-if
       (without-scheduling ()
         #-sb-thread
         (progn
           (store-symbol-value null-tn *pseudo-atomic-atomic*)
           (load-symbol-value ,flag-tn *pseudo-atomic-interrupted*))
         #+sb-thread
         (progn
           (when ,sync
            (inst dmb :ishst))
           (inst str (32-bit-reg zr-tn)
                 (@ thread-tn
                    (* n-word-bytes thread-pseudo-atomic-bits-slot)))
           (inst ldr (32-bit-reg ,flag-tn)
                 (@ thread-tn
                    (+ (* n-word-bytes thread-pseudo-atomic-bits-slot) 4))))
         (let ((not-interrputed (gen-label)))
           (inst cbz ,flag-tn not-interrputed)
           (inst brk pending-interrupt-trap)
           (emit-label not-interrputed))))))

;;;; memory accessor vop generators

(defmacro define-full-reffer (name type offset lowtag scs el-type
                              &optional translate)
  `(define-vop (,name)
     ,@(when translate
         `((:translate ,translate)))
     (:policy :fast-safe)
     (:args (object :scs (descriptor-reg))
            (index :scs (any-reg unsigned-reg signed-reg immediate)))
     (:arg-types ,type tagged-num)
     (:results (value :scs ,scs))
     (:result-types ,el-type)
     (:generator 5
       (sc-case index
         (immediate
          (inst ldr value (@ object (load-store-offset
                                     (- (ash (+ ,offset (tn-value index)) word-shift)
                                        ,lowtag)))))
         (t
          (inst add tmp-tn object (lsl index (- word-shift (if (sc-is index any-reg)
                                                               n-fixnum-tag-bits
                                                               0))))
          (loadw value tmp-tn ,offset ,lowtag))))))

(defmacro define-full-setter (name type offset lowtag scs el-type
                              &optional translate
                              &aux (barrierp (member 'descriptor-reg scs)))
  (case name
    ((data-vector-set/simple-vector %weakvec-set)
     `(define-vop (,name)
        (:translate ,translate)
        (:policy :fast-safe)
        (:args (object :scs (descriptor-reg))
               (index :scs (any-reg unsigned-reg signed-reg immediate))
               (value :scs (,@scs zero)))
        (:arg-types ,type tagged-num ,el-type)
        (:temporary (:sc non-descriptor-reg) ea)
        (:vop-var vop)
        (:generator 2
          (cond ((require-gengc-barrier-p object (vop-nth-arg 2 vop) value)
                 (sc-case index
                   (immediate
                    (inst add ea object (load-store-offset
                                         (- (ash (+ ,offset (tn-value index)) word-shift)
                                            ,lowtag))))
                   (t
                    ;; Scale the index
                    (let ((unshift (if (sc-is index any-reg) n-fixnum-tag-bits 0)))
                      (inst add ea object (lsl index (- word-shift unshift))))
                    ;; Calculate the exact cell address to ensure the right card is marked
                    (inst add ea ea (- (ash vector-data-offset word-shift) other-pointer-lowtag))))
                 (emit-gengc-barrier object ea tmp-tn t)
                 (storew value ea 0 0))
                (t
                 (sc-case index
                   (immediate
                    (inst str value (@ object (load-store-offset
                                               (- (ash (+ ,offset (tn-value index)) word-shift)
                                                  ,lowtag)))))
                   (t
                    (let ((unshift (if (sc-is index any-reg) n-fixnum-tag-bits 0)))
                      (inst add tmp-tn object (lsl index (- word-shift unshift))))
                    (storew value tmp-tn ,offset ,lowtag))))))))
    (t
     `(define-vop (,name)
        (:translate ,translate)
        (:policy :fast-safe)
        (:args (object :scs (descriptor-reg))
               (index :scs (any-reg unsigned-reg signed-reg immediate))
               (value :scs (,@scs zero)))
        (:arg-types ,type tagged-num ,el-type)
        (:vop-var vop)
        (:generator 2
          ,@(if barrierp '((emit-gengc-barrier object nil tmp-tn (vop-nth-arg 2 vop))))
          (sc-case index
            (immediate
             (inst str value (@ object (load-store-offset
                                        (- (ash (+ ,offset (tn-value index)) word-shift)
                                           ,lowtag)))))
            (t
             (let ((unshift (if (sc-is index any-reg) n-fixnum-tag-bits 0)))
               (inst add tmp-tn object (lsl index (- word-shift unshift))))
             (storew value tmp-tn ,offset ,lowtag))))))))

(defmacro define-partial-reffer (name type size signed offset lowtag scs
                                 el-type &optional translate)
  `(define-vop (,name)
     ,@(when translate
         `((:translate ,translate)))
     (:policy :fast-safe)
     (:args (object :scs (descriptor-reg))
            (index :scs (any-reg unsigned-reg signed-reg immediate)))
     (:arg-types ,type tagged-num)
     (:results (value :scs ,scs))
     (:result-types ,el-type)
     (:generator 5
       ,@(multiple-value-bind (op shift)
             (ecase size
               (:byte
                (values (if signed 'ldrsb 'ldrb) 0))
               (:short
                (values (if signed 'ldrsh 'ldrh) 1))
               (:word
                (values (if signed 'ldrsw 'ldr) 2))
               (:single-float
                (values 'ldr 2)))
           (let ((value (if (and (eq size :word)
                                 (not signed))
                            '(32-bit-reg value)
                            'value)))
             `((sc-case index
                 (immediate
                  (inst ,op ,value (@ object (load-store-offset
                                              (+
                                               (ash (tn-value index) ,shift)
                                               (- (* ,offset n-word-bytes) ,lowtag))))))
                 (t
                  (let ((shift ,shift))
                    (when (sc-is index any-reg)
                      (decf shift n-fixnum-tag-bits))
                    (inst add tmp-tn object (if (minusp shift)
                                                (asr index (- shift))
                                                (lsl index shift)))
                    (inst ,op
                          ,value (@ tmp-tn (- (* ,offset n-word-bytes) ,lowtag))))))))))))

(defmacro define-partial-setter (name type size offset lowtag scs el-type
                                 &optional translate)
  (let ((value `((value :scs ,scs
                             :load-if (not (and (sc-is value immediate)
                                                (eql (tn-value value) 0))))))
        (setf-p (typep translate '(cons (eql setf)))))
   `(define-vop (,name)
      ,@(when translate
          `((:translate ,translate)))
      (:policy :fast-safe)
      (:args ,@(when setf-p
                 value)
             (object :scs (descriptor-reg))
             (index :scs (any-reg unsigned-reg signed-reg immediate))
             ,@(unless setf-p
                 value))
      (:arg-types ,@(when setf-p
                      `(,el-type))
                  ,type
                  tagged-num
                  ,@(unless setf-p
                      `(,el-type)))
      (:generator 5
        (when (sc-is value immediate)
          (setf value zr-tn))
        ,@(multiple-value-bind (op shift)
              (ecase size
                (:byte
                 (values 'strb 0))
                (:short
                 (values 'strh 1))
                ((:word :single-float)
                 (values 'str 2)))
            (let ((value (if (eq size :word)
                             '(32-bit-reg value)
                             'value)))
              `((sc-case index
                  (immediate
                   (inst ,op ,value (@ object (load-store-offset
                                               (+
                                                (ash (tn-value index) ,shift)
                                                (- (* ,offset n-word-bytes) ,lowtag))))))
                  (t
                   (let ((shift ,shift))
                     (when (sc-is index any-reg)
                       (decf shift n-fixnum-tag-bits))
                     (inst add tmp-tn object (if (minusp shift)
                                                 (asr index (- shift))
                                                 (lsl index shift)))
                     (inst ,op
                           ,value (@ tmp-tn (- (* ,offset n-word-bytes) ,lowtag)))))))))))))

(defun load-inline-constant (dst &rest constant-descriptor)
  (inst load-from-label dst (cdr (apply #'register-inline-constant constant-descriptor))))

;;;

(defmacro load-binding-stack-pointer (reg)
  #+sb-thread `(loadw ,reg thread-tn thread-binding-stack-pointer-slot)
  #-sb-thread `(load-symbol-value ,reg *binding-stack-pointer*))

(defmacro store-binding-stack-pointer (reg)
  #+sb-thread `(storew ,reg thread-tn thread-binding-stack-pointer-slot)
  #-sb-thread `(store-symbol-value ,reg *binding-stack-pointer*))

#+sb-thread
(defmacro tls-index-of (sym)
  `(@ ,sym (- #+little-endian 4 other-pointer-lowtag)))

(defmacro load-tl-symbol-value (reg symbol)
  #+sb-thread
  `(inst ldr ,reg (@ thread-tn ,(info :variable :wired-tls symbol)))
  #-sb-thread
  `(load-symbol-value ,reg ,symbol))

(defmacro store-tl-symbol-value (reg symbol)
  #+sb-thread
  `(inst str ,reg (@ thread-tn ,(info :variable :wired-tls symbol)))
  #-sb-thread
  `(store-symbol-value ,reg ,symbol))

;;; Load constants, stack-values, reusing when possible
(defmacro maybe-load (tn &optional (temp 'temp))
  (once-only ((tn tn))
    `(sc-case ,tn
       ((any-reg descriptor-reg)
        ,tn)
       ((immediate constant)
        (cond ((not (sb-c::tn-leaf ,tn))
               (load-constant vop ,tn ,temp)
               ,temp)
              ((eql (tn-value ,tn) 0)
               zr-tn)
              ((or (eql prev-constant (tn-value ,tn))
                   (progn
                     (setf prev-constant (tn-value ,tn))
                     nil))
               temp)
              ((sc-is ,tn constant)
               (load-constant vop ,tn ,temp)
               ,temp)
              (t
               (load-immediate vop ,tn ,temp)
               ,temp)))
       (control-stack
        (setf prev-constant nil)
        (load-stack-tn ,temp ,tn)
        ,temp))))
