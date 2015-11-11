;;;; a bunch of handy macros for the ARM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

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
    (let ((target-offset (ecase *backend-byte-order*
                           (:little-endian n-offset)
                           (:big-endian `(+ ,n-offset (1- n-word-bytes))))))
      `(inst ldrb ,n-target (@ ,n-source ,target-offset)))))

;;; Macros to handle the fact that our stack pointer isn't actually in
;;; a register (or won't be, by the time we're done).

;;; Macros to handle the fact that we cannot use the machine native call and
;;; return instructions.

(defmacro lisp-jump (function lip)
  "Jump to the lisp lip LIP."
  `(let ((function ,function)
         (lip ,lip))
     (assert (sc-is lip interior-reg))
     (inst add lip function
           (- (ash simple-fun-code-offset word-shift)
              fun-pointer-lowtag))
     (inst br lip)))

(defmacro lisp-return (function lip return-style)
  "Return to RETURN-PC."
  `(let* ((function ,function)
          (lip ,lip))
     ;; Indicate a single-valued return by clearing all of the status
     ;; flags, or a multiple-valued return by setting all of the status
     ;; flags.
     (assert (sc-is lip interior-reg))
     ,@(ecase return-style
         (:single-value '((inst msr :nzcv zr-tn)))
         (:multiple-values '((inst orr tmp-tn zr-tn #xf0000000)
                             (inst msr :nzcv tmp-tn)))
         (:known))
     (inst sub lip function (- other-pointer-lowtag 8))
     (inst ret lip)))

(defmacro emit-return-pc (label)
  "Emit a return-pc header word.  LABEL is the label to use for this return-pc."
  `(progn
     (emit-alignment n-lowtag-bits)
     (emit-label ,label)
     (inst lra-header-word)))


;;;; Stack TN's

;;; Move a stack TN to a register and vice-versa.
(defun load-stack-offset (reg stack stack-tn)
  (inst ldr reg (@ stack (load-store-offset (* (tn-offset stack-tn) n-word-bytes)))))

(defmacro load-stack-tn (reg stack)
  `(let ((reg ,reg)
         (stack ,stack))
     (sc-case stack
       ((control-stack)
        (load-stack-offset reg cfp-tn stack)))))

(defun store-stack-offset (reg stack stack-tn)
  (let ((offset (* (tn-offset stack-tn) n-word-bytes)))
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

#!+gencgc
(defun allocation-tramp (alloc-tn size back-label return-in-tmp lip)
  (unless (eq size tmp-tn)
    (inst mov tmp-tn size))
  (load-inline-constant alloc-tn '(:fixup "alloc_tramp" :foreign) lip)
  (inst blr alloc-tn)
  (unless return-in-tmp
    (move alloc-tn tmp-tn))
  (inst b back-label))

(defmacro allocation (result-tn size lowtag &key flag-tn
                                                 stack-allocate-p
                                                 (lip (if stack-allocate-p
                                                          nil
                                                          (missing-arg))))
  ;; Normal allocation to the heap.
  (once-only ((result-tn result-tn)
              (size size)
              (lowtag lowtag)
              (flag-tn flag-tn)
              (stack-allocate-p stack-allocate-p)
              (lip lip))
    `(cond (,stack-allocate-p
            (assemble ()
              (move ,result-tn csp-tn)
              (inst tst ,result-tn lowtag-mask)
              (inst b :eq ALIGNED)
              (inst add ,result-tn ,result-tn n-word-bytes)
              ALIGNED
              (inst add csp-tn ,result-tn (add-sub-immediate ,size))
              ;; :ne is from TST above, this needs to be done after the
              ;; stack pointer has been stored.
              (inst b :eq ALIGNED2)
              (storew zr-tn ,result-tn -1 0)
              ALIGNED2
              (when ,lowtag
                (inst add ,result-tn ,result-tn ,lowtag))))
           #!-gencgc
           (t
            (load-symbol-value ,flag-tn *allocation-pointer*)
            (inst add ,result-tn ,flag-tn ,lowtag)
            (inst add ,flag-tn ,flag-tn (add-sub-immediate ,size))
            (store-symbol-value ,flag-tn *allocation-pointer*))
           #!+gencgc
           (t
            (let ((alloc (gen-label))
                  (back-from-alloc (gen-label))
                  size)
              #!-sb-thread
              (progn
                (load-inline-constant ,flag-tn '(:fixup "boxed_region" :foreign) ,lip)
                (inst ldp ,result-tn ,flag-tn (@ ,flag-tn)))
              #!+sb-thread
              (inst ldp ,result-tn ,flag-tn (@ thread-tn
                                               (* n-word-bytes thread-alloc-region-slot)))
              (setf size (add-sub-immediate ,size))
              (inst add ,result-tn ,result-tn size)
              (inst cmp ,result-tn ,flag-tn)
              (inst b :hi ALLOC)
              #!-sb-thread
              (progn
                (load-inline-constant ,flag-tn '(:fixup "boxed_region" :foreign) ,lip)
                (storew ,result-tn ,flag-tn))
              #!+sb-thread
              (storew ,result-tn thread-tn thread-alloc-region-slot)

              ;; alloc_tramp uses tmp-tn for returning the result,
              ;; save on a move when possible
              (inst sub (if ,lowtag
                            tmp-tn
                            ,result-tn) ,result-tn size)

              (emit-label BACK-FROM-ALLOC)
              (when ,lowtag
                (inst add ,result-tn tmp-tn ,lowtag))

              (assemble (*elsewhere*)
                (emit-label ALLOC)
                (allocation-tramp ,result-tn
                                  ,size BACK-FROM-ALLOC
                                  ;; see the comment above aboout alloc_tramp
                                  (and ,lowtag t)
                                  ,lip)))))))

(defmacro with-fixed-allocation ((result-tn flag-tn type-code size
                                            &key (lowtag other-pointer-lowtag)
                                                 stack-allocate-p
                                                 (lip (missing-arg)))
                                 &body body)
  "Do stuff to allocate an other-pointer object of fixed Size with a single
  word header having the specified Type-Code.  The result is placed in
  Result-TN, and Temp-TN is a non-descriptor temp (which may be randomly used
  by the body.)  The body is placed inside the PSEUDO-ATOMIC, and presumably
  initializes the object."
  (once-only ((result-tn result-tn) (flag-tn flag-tn)
              (type-code type-code) (size size) (lowtag lowtag)
              (stack-allocate-p stack-allocate-p)
              (lip lip))
    `(pseudo-atomic (,flag-tn)
       (allocation ,result-tn (pad-data-block ,size) ,lowtag
                   :flag-tn ,flag-tn
                   :stack-allocate-p ,stack-allocate-p
                   :lip ,lip)
       (when ,type-code
         (inst mov ,flag-tn (ash (1- ,size) n-widetag-bits))
         (inst add ,flag-tn ,flag-tn ,type-code)
         (storew ,flag-tn ,result-tn 0 ,lowtag))
       ,@body)))

;;;; Error Code
(defun emit-error-break (vop kind code values)
  (assemble ()
    (when vop
      (note-this-location vop :internal-error))
    ;; Encode both kind and code as an argument to BRK
    (inst brk (dpb code (byte 8 8) kind))
    ;; NARGS is implicitely assumed for invalid-arg-count
    (unless (= kind invalid-arg-count-trap)
     (with-adjustable-vector (vector)
       (dolist (tn values)
         (write-var-integer (make-sc-offset (sc-number (tn-sc tn))
                                            (or (tn-offset tn) 0))
                            vector))
       (inst byte (length vector))
       (dotimes (i (length vector))
         (inst byte (aref vector i)))
       (emit-alignment 2)))))

(defun error-call (vop error-code &rest values)
  #!+sb-doc
  "Cause an error.  ERROR-CODE is the error to cause."
  (emit-error-break vop error-trap (error-number-or-lose error-code) values))

(defun generate-error-code (vop error-code &rest values)
  #!+sb-doc
  "Generate-Error-Code Error-code Value*
  Emit code for an error with the specified Error-Code and context Values."
  (assemble (*elsewhere*)
    (let ((start-lab (gen-label)))
      (emit-label start-lab)
      (emit-error-break vop
                        (if (eq error-code 'invalid-arg-count-error)
                            invalid-arg-count-trap
                            error-trap)
                        (error-number-or-lose error-code) values)
      start-lab)))

;;;; PSEUDO-ATOMIC


;;; handy macro for making sequences look atomic
(defmacro pseudo-atomic ((flag-tn) &body forms)
  `(progn
     (without-scheduling ()
       #!-sb-thread
       (store-symbol-value csp-tn *pseudo-atomic-atomic*)
       #!+sb-thread
       (inst str (32-bit-reg null-tn)
             (@ thread-tn
                (* n-word-bytes thread-pseudo-atomic-bits-slot))))
     (assemble ()
       ,@forms)
     (without-scheduling ()
       #!-sb-thread
       (progn
         (store-symbol-value null-tn *pseudo-atomic-atomic*)
         (load-symbol-value ,flag-tn *pseudo-atomic-interrupted*))
       #!+sb-thread
       (progn
         (inst dmb)
         (inst str (32-bit-reg zr-tn)
               (@ thread-tn
                  (* n-word-bytes thread-pseudo-atomic-bits-slot)))
         (inst ldr (32-bit-reg ,flag-tn)
               (@ thread-tn
                  (+ (* n-word-bytes thread-pseudo-atomic-bits-slot) 4))))
       ;; When *pseudo-atomic-interrupted* is not 0 it contains the address of
       ;; do_pending_interrupt
       (let ((not-interrputed (gen-label)))
         (inst cbz ,flag-tn not-interrputed)
         (inst brk pending-interrupt-trap)
         (emit-label not-interrputed)))))

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
       (inst add lip object (lsl index (- word-shift n-fixnum-tag-bits)))
       (loadw value lip ,offset ,lowtag))))

(defmacro define-full-setter (name type offset lowtag scs el-type
                              &optional translate)
  `(define-vop (,name)
     ,@(when translate
             `((:translate ,translate)))
     (:policy :fast-safe)
     (:args (object :scs (descriptor-reg))
            (index :scs (any-reg))
            (value :scs ,scs :target result))
     (:arg-types ,type tagged-num ,el-type)
     (:temporary (:scs (interior-reg)) lip)
     (:results (result :scs ,scs))
     (:result-types ,el-type)
     (:generator 2
       (inst add lip object (lsl index (- word-shift n-fixnum-tag-bits)))
       (storew value lip ,offset ,lowtag)
       (move result value))))

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
       ,@(ecase size (eq size :byte)
               (:byte
                `((inst add lip object index)
                  (inst ,(if signed 'ldrsb 'ldrb)
                        value (@ lip (- (* ,offset n-word-bytes) ,lowtag)))))
               (:short
                `((inst add lip object (lsl index 1))
                  (inst ,(if signed 'ldrsh 'ldrh)
                        value (@ lip (- (* ,offset n-word-bytes) ,lowtag)))))
               (:word
                `((inst add lip object (lsl index 2))
                  (inst ,(if signed 'ldrsw 'ldr) (32-bit-reg value)
                        (@ lip (- (* ,offset n-word-bytes) ,lowtag)))))))))

(defmacro define-partial-setter (name type size offset lowtag scs el-type
                                 &optional translate)
  `(define-vop (,name)
     ,@(when translate
             `((:translate ,translate)))
     (:policy :fast-safe)
     (:args (object :scs (descriptor-reg))
            (index :scs (unsigned-reg))
            (value :scs ,scs :target result))
     (:arg-types ,type positive-fixnum ,el-type)
     (:temporary (:scs (interior-reg)) lip)
     (:results (result :scs ,scs))
     (:result-types ,el-type)
     (:generator 5
       ,@(ecase size (eq size :byte)
                (:byte
                 `((inst add lip object index)
                   (inst strb value (@ lip (- (* ,offset n-word-bytes) ,lowtag)))))
                (:short
                 `((inst add lip object (lsl index 1))
                   (inst strh value (@ lip (- (* ,offset n-word-bytes) ,lowtag)))))
                (:word
                 `((inst add lip object (lsl index 2))
                   (inst str (32-bit-reg value) (@ lip (- (* ,offset n-word-bytes) ,lowtag))))))
       (move result value))))

(def!macro with-pinned-objects ((&rest objects) &body body)
  "Arrange with the garbage collector that the pages occupied by
OBJECTS will not be moved in memory for the duration of BODY.
Useful for e.g. foreign calls where another thread may trigger
garbage collection.  This is currently implemented by disabling GC"
  #!-gencgc
  (declare (ignore objects))            ; should we eval these for side-effect?
  #!-gencgc
  `(without-gcing
    ,@body)
  #!+gencgc
  `(let ((*pinned-objects* (list* ,@objects *pinned-objects*)))
     (declare (truly-dynamic-extent *pinned-objects*))
     ,@body))

(defun load-inline-constant (dst value &optional lip)
  (destructuring-bind (size . label) (register-inline-constant value)
    (ecase size
      (:qword
       (inst load-from-label dst label lip)))))

;;;

(defmacro load-binding-stack-pointer (reg)
  #!+sb-thread `(loadw ,reg thread-tn thread-binding-stack-pointer-slot)
  #!-sb-thread `(load-symbol-value ,reg *binding-stack-pointer*))

(defmacro store-binding-stack-pointer (reg)
  #!+sb-thread `(storew ,reg thread-tn thread-binding-stack-pointer-slot)
  #!-sb-thread `(store-symbol-value ,reg *binding-stack-pointer*))

#!+sb-thread
(defmacro tls-index-of (sym)
  `(@ ,sym (- #!+little-endian 4 other-pointer-lowtag)))

(defmacro load-tl-symbol-value (reg symbol)
  #!+sb-thread
  `(let ((reg ,reg))
     (load-symbol tmp-tn ',symbol)
     (inst ldr tmp-tn (tls-index-of tmp-tn))
     (inst ldr reg (@ thread-tn tmp-tn)))
  #!-sb-thread
  `(load-symbol-value ,reg ,symbol))

(defmacro store-tl-symbol-value (reg symbol)
  #!+sb-thread
  `(let ((reg ,reg))
     (load-symbol tmp-tn ',symbol)
     (inst ldr tmp-tn (tls-index-of tmp-tn))
     (inst str reg (@ thread-tn tmp-tn)))
  #!-sb-thread
  `(store-symbol-value ,reg ,symbol))
