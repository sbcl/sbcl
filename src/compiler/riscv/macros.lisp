;;;; various useful macros for generating RISC-V code

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
  "Move SRC into DST (unless they are location=."
  (once-only ((n-dst dst)
              (n-src src))
    `(unless (location= ,n-dst ,n-src)
       (inst addi ,n-dst ,n-src 0))))

;;; Add a possibly-too-large immediate value using TEMP. VOP-NAME is merely
;;; to track where the temp is needed because I might have overdone use of it
;;; and could potentially remove some uses of this. (Not sure)
(defun add-imm (result source imm-operand vop-name tmp)
  (declare (ignorable vop-name))
  (cond ((typep imm-operand 'short-immediate)
         (inst addi result source imm-operand))
        (t
         (inst li tmp imm-operand)
         (inst add result source tmp))))

(macrolet ((def-mem-op (op inst shift)
             `(defmacro ,op (object base &optional (offset 0) (lowtag 0))
                `(inst ,',inst ,object ,base (- (ash ,offset ,,shift) ,lowtag)))))
  (def-mem-op loadw #-64-bit lw #+64-bit ld word-shift)
  (def-mem-op storew #-64-bit sw #+64-bit sd word-shift))

(defun load-frame-word (object base wordindex vop-name tmp)
  (declare (ignore vop-name))
  (let ((imm (ash wordindex word-shift)))
    (cond ((typep imm 'short-immediate)
           (loadw object base wordindex))
          (t
           (unless tmp
             ;; check that it's OK to temporarily mess up OBJECT
             (aver (not (location= object base)))
             (setq tmp object)) ; use OBJECT for pointer arithmetic
           ;; TOOD: there's a way to do this using only 2 instructions if the
           ;; immediate is only a little too large: first, "addi TEMP base k"
           ;; where K gets us nearer to the desired address; then store based off
           ;; TMP using a smaller immediate index.
           ;; For now, 3 instructions it shall be.
           (inst li tmp imm)
           (inst add tmp base tmp)
           (inst #-64-bit lw #+64-bit ld object tmp 0)))))
(defun store-frame-word (object base wordindex vop-name tmp)
  (declare (ignore vop-name))
  (let ((imm (ash wordindex word-shift)))
    (cond ((typep imm 'short-immediate)
           (storew object base wordindex))
          (tmp
           (inst li tmp imm)
           (inst add tmp base tmp)
           (inst #-64-bit sw #+64-bit sd object tmp 0))
          (t
           ;; Lacking a scratch register, I don't know what else to do but modify
           ;; the BASE register.
           ;; FIXME: POSSIBLY DANGEROUS! if a stop-for-GC occurs and the frame-pointer is off
           ;; from what it should be, the set of values below the frame-pointer omits many
           ;; words. Maybe it's OK because the Lisp stack grows upward so we only care that
           ;; the stack is bounded by the stack-pointer and not the frame-pointer?
           (do ((max-short-imm #x7F0)
                (adjusted-imm imm)
                (n-adjustments 0))
               ((typep adjusted-imm 'short-immediate)
                (inst #-64-bit sw #+64-bit sd object base adjusted-imm)
                (dotimes (i n-adjustments)
                  (inst subi base base max-short-imm)))
             (inst addi base base max-short-imm)
             (decf adjusted-imm max-short-imm)
             (incf n-adjustments))))))

(macrolet ((def-coerce-op (name inst)
             `(defmacro ,name ((reg temp-reg) &body body)
                `(progn
                   ,@(if (= word-shift n-fixnum-tag-bits)
                         body
                         `((inst ,',inst ,temp-reg ,reg ,(- word-shift n-fixnum-tag-bits))
                           ,(when body
                              `(let ((,reg ,temp-reg))
                                 ,@body))))))))
  (def-coerce-op with-word-index-as-fixnum srai)
  (def-coerce-op with-fixnum-as-word-index slli))

(defconstant fixnum-as-word-index-needs-temp
  (cl:/= sb-vm:word-shift sb-vm:n-fixnum-tag-bits))

(defun load-symbol (reg symbol)
  (inst addi reg null-tn (static-symbol-offset symbol)))

#+sb-thread
(progn
  (defun load-tls-index (reg symbol)
    #-64-bit
    (loadw reg symbol symbol-tls-index-slot other-pointer-lowtag)
    #+64-bit
    (inst lwu reg symbol (- 4 other-pointer-lowtag)))

  (defun store-tls-index (reg symbol)
    #-64-bit
    (storew reg symbol symbol-tls-index-slot other-pointer-lowtag)
    #+64-bit
    (inst sw reg symbol (- 4 other-pointer-lowtag))))

(defmacro load-symbol-value (reg symbol)
  `(inst #-64-bit lw #+64-bit ld ,reg null-tn
         (+ (static-symbol-offset ',symbol)
            (ash symbol-value-slot word-shift)
            (- other-pointer-lowtag))))

(defmacro store-symbol-value (reg symbol)
  `(inst #-64-bit sw #+64-bit sd ,reg null-tn
         (+ (static-symbol-offset ',symbol)
            (ash symbol-value-slot word-shift)
            (- other-pointer-lowtag))))

(macrolet ((define-tls-accessors (reader setter slot-offset variable)
             (declare (ignore #-sb-thread slot-offset
                              #+sb-thread variable))
             `(progn
                (defun ,reader (reg)
                  #+sb-thread
                  (loadw reg thread-base-tn ,slot-offset)
                  #-sb-thread
                  (load-symbol-value reg ,variable))
                (defun ,setter (reg)
                  #+sb-thread
                  (storew reg thread-base-tn ,slot-offset)
                  #-sb-thread
                  (store-symbol-value reg ,variable)))))
  (define-tls-accessors load-binding-stack-pointer store-binding-stack-pointer
    thread-binding-stack-pointer-slot *binding-stack-pointer*)
  (define-tls-accessors load-current-catch-block store-current-catch-block
    thread-current-catch-block-slot *current-catch-block*)
  (define-tls-accessors load-current-unwind-protect-block store-current-unwind-protect-block
    thread-current-unwind-protect-block-slot *current-unwind-protect-block*)
  (define-tls-accessors load-stepping store-stepping
    thread-stepping-slot sb-impl::*stepping*))

;;; TODO: these two macros would benefit from alien-linkage-table space being
;;; located below static space with linkage entries allocated downward
;;; from the end. Then the sequence would reduce to 2 instructions:
;;;    lw temp (k)$NULL
;;;    lw dest, (0)$TEMP
(defun load-foreign-symbol-value (dest symbol temp)
  (aver (string/= symbol "foreign_function_call_active"))
  (let ((fixup (make-fixup symbol :foreign-dataref)))
    (inst lui temp fixup)
    (inst #-64-bit lw #+64-bit ld temp temp fixup)
    (inst #-64-bit lw #+64-bit ld dest temp 0)))

(defun store-foreign-symbol-value (src symbol temp)
  (let ((fixup (make-fixup symbol :foreign-dataref))
        ;; see comment in globals.c
        (op (cond #+64-bit ((string/= symbol "foreign_function_call_active") 'sd)
                  (t 'sw))))
    (inst lui temp fixup)
    (inst #-64-bit lw #+64-bit ld temp temp fixup)
    (inst* op src temp 0)))

(defmacro load-type (target source &optional (offset 0))
  "Loads the type bits of a pointer into target independent of
byte-ordering issues."
  `(inst lbu ,target ,source ,offset))


;;;; Three Way Comparison
(defun three-way-comparison (x y condition flavor not-p target)
  (ecase condition
    (:eq (if not-p
             (inst bne x y target)
             (inst beq x y target)))
    ((:lt :gt)
     (when (eq condition :gt)
       (rotatef x y))
     (ecase flavor
       (:unsigned (if not-p
                      (inst bgeu x y target)
                      (inst bltu x y target)))
       (:signed (if not-p
                    (inst bge x y target)
                    (inst blt x y target)))))))


(defun emit-error-break (vop kind code values)
  (assemble ()
    (when vop
      (note-this-location vop :internal-error))
    (inst ebreak)
    (if (= kind invalid-arg-count-trap) ; there is no "payload" in this trap kind
        (inst byte kind)
        (emit-internal-error kind code values))
    (emit-alignment 2)))

(defun generate-error-code (vop error-code &rest values)
  "Generate-Error-Code Error-code Value*
  Emit code for an error with the specified Error-Code and context Values."
  (assemble (:elsewhere)
    (let ((start-lab (gen-label)))
      (emit-label start-lab)
      (emit-error-break vop
                        (if (eq error-code 'invalid-arg-count-error)
                            invalid-arg-count-trap
                            error-trap)
                        (error-number-or-lose error-code) values)
      start-lab)))

;;;; PSEUDO-ATOMIC

(defun set-pseudo-atomic-bit ()
  #-sb-thread
  (store-symbol-value csp-tn *pseudo-atomic-atomic*)
  #+sb-thread
  (inst sh null-tn thread-base-tn
        (* thread-pseudo-atomic-bits-slot n-word-bytes)))

(defun clear-pseudo-atomic-bit ()
  #-sb-thread
  (store-symbol-value null-tn *pseudo-atomic-atomic*)
  #+sb-thread
  (inst sh zero-tn thread-base-tn (* thread-pseudo-atomic-bits-slot n-word-bytes)))

(defun load-pseudo-atomic-interrupted (reg)
  #-sb-thread
  (load-symbol-value reg *pseudo-atomic-interrupted*)
  #+sb-thread
  (inst lh reg thread-base-tn
        (+ (* thread-pseudo-atomic-bits-slot n-word-bytes) 2)))

;;; handy macro for making sequences look atomic
(defmacro pseudo-atomic ((flag-tn) &body forms)
  `(progn
     (without-scheduling ()
       (set-pseudo-atomic-bit))
     (assemble ()
       ,@forms)
     #+sb-thread
     (inst fence :rw :w)
     (without-scheduling ()
       (clear-pseudo-atomic-bit)
       (load-pseudo-atomic-interrupted ,flag-tn)
       (let ((not-interrupted (gen-label)))
         (inst beq ,flag-tn zero-tn not-interrupted)
         (inst ebreak pending-interrupt-trap)
         (emit-alignment 2)
         (emit-label not-interrupted)))))

#|
If we are doing [reg+offset*n-word-bytes-lowtag+index*scale]
and

-2^11 <= offset*n-word-bytes - lowtag + index*scale < 2^11
-2^11 <= offset*n-word-bytes - lowtag + index*scale <= 2^11-1
-2^11 + lowtag -offset*n-word-bytes <= index*scale <= 2^11-1 + lowtag - offset*n-word-bytes
|#
(sb-xc:deftype load/store-index (scale lowtag offset)
  (let* ((encodable (list (- (ash 1 11)) (1- (ash 1 11))))
         (add-lowtag (mapcar (lambda (x) (+ x lowtag)) encodable))
         (sub-offset (mapcar (lambda (x) (- x (* offset n-word-bytes))) add-lowtag))
         (truncated (mapcar (lambda (x) (truncate x scale)) sub-offset)))
    `(integer ,(first truncated) ,(second truncated))))

(defmacro define-full-reffer (name type offset lowtag scs eltype &optional translate)
  `(progn
     (define-vop (,name)
       ,@(when translate `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
              (index :scs (any-reg)))
       (:arg-types ,type tagged-num)
       (:temporary (:scs (interior-reg)) lip)
       ,@(when fixnum-as-word-index-needs-temp
           `((:temporary (:sc non-descriptor-reg) temp)))
       (:results (value :scs ,scs))
       (:result-types ,eltype)
       (:generator 5
         (with-fixnum-as-word-index (index temp)
           (inst add lip object index))
         (loadw value lip ,offset ,lowtag)))
     (define-vop (,(symbolicate name "-C"))
       ,@(when translate `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg)))
       (:info index)
       (:arg-types ,type
         (:constant (load/store-index #.n-word-bytes ,(eval lowtag) ,(eval offset))))
       (:results (value :scs ,scs))
       (:result-types ,eltype)
       (:generator 4
         (loadw value object (+ ,offset index) ,lowtag)))))

(defmacro define-full-setter (name type offset lowtag scs eltype &optional translate)
  `(progn
     (define-vop (,name)
       ,@(when translate `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
              (index :scs (any-reg))
              (value :scs ,scs))
       (:arg-types ,type tagged-num ,eltype)
       (:temporary (:scs (interior-reg)) lip)
       ,@(when fixnum-as-word-index-needs-temp
           `((:temporary (:sc non-descriptor-reg) temp)))
       (:generator 3
         (with-fixnum-as-word-index (index temp)
           (inst add lip object index))
         (storew value lip ,offset ,lowtag)))
     (define-vop (,(symbolicate name "-C"))
       ,@(when translate
           `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
              (value :scs ,scs))
       (:info index)
       (:arg-types ,type
         (:constant (load/store-index #.n-word-bytes ,(eval lowtag) ,(eval offset)))
         ,eltype)
       (:generator 1
         (storew value object (+ ,offset index) ,lowtag)))))

(defmacro define-partial-reffer (name type size signed offset lowtag scs eltype &optional translate)
  (let ((shift (- (integer-length size) n-fixnum-tag-bits 1)))
    `(progn
       (define-vop (,name)
         ,@(when translate `((:translate ,translate)))
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg)) (index :scs (any-reg)))
         (:arg-types ,type positive-fixnum)
         (:temporary (:scs (interior-reg)) lip)
         ,@(unless (zerop shift)
             `((:temporary (:sc non-descriptor-reg) temp)))
         (:results (value :scs ,scs))
         (:result-types ,eltype)
         (:generator 5
           ,@(cond ((zerop shift)
                    `((inst add lip object index)))
                   (t
                    `(,(if (minusp shift)
                           `(inst srai temp index ,(- shift))
                           `(inst slli temp index ,shift))
                      (inst add lip object temp))))
           (inst ,(ecase size
                    (1 (if signed 'lb 'lbu))
                    (2 (if signed 'lh 'lhu))
                    (4 (if signed 'lw 'lwu)))
                 value lip (- (* ,offset n-word-bytes) ,lowtag))))
       (define-vop (,(symbolicate name "-C"))
         ,@(when translate
             `((:translate ,translate)))
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg)))
         (:info index)
         (:arg-types ,type
           (:constant (load/store-index ,(eval size) ,(eval lowtag) ,(eval offset))))
         (:results (value :scs ,scs))
         (:result-types ,eltype)
         (:generator 4
           (inst ,(ecase size
                    (1 (if signed 'lb 'lbu))
                    (2 (if signed 'lh 'lhu))
                    (4 (if signed 'lw 'lwu)))
                 value object
                 (- (+ (* ,offset n-word-bytes) (* index ,size)) ,lowtag)))))))

(defmacro define-partial-setter (name type size offset lowtag scs eltype &optional translate)
  (let ((shift (- (integer-length size) n-fixnum-tag-bits 1)))
    `(progn
       (define-vop (,name)
         ,@(when translate `((:translate ,translate)))
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg))
                (index :scs (any-reg))
                (value :scs ,scs))
         (:arg-types ,type positive-fixnum ,eltype)
         (:temporary (:scs (interior-reg)) lip)
         ,@(unless (zerop shift)
             `((:temporary (:sc non-descriptor-reg) temp)))
         (:generator 5
           ,@(cond ((zerop shift)
                    '((inst add lip object index)))
                   (t
                    `(,(if (minusp shift)
                           `(inst srai temp index ,(- shift))
                           `(inst slli temp index ,shift))
                      (inst add lip object temp))))
           (inst ,(ecase size (1 'sb) (2 'sh) (4 'sw))
                 value lip (- (* ,offset n-word-bytes) ,lowtag))))
       (define-vop (,(symbolicate name "-C"))
         ,@(when translate
             `((:translate ,translate)))
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg))
                (value :scs ,scs))
         (:info index)
         (:arg-types ,type
           (:constant (load/store-index ,(eval size) ,(eval lowtag) ,(eval offset)))
           ,eltype)
         (:generator 4
           (inst ,(ecase size (1 'sb) (2 'sh) (4 'sw))
                 value object
                 (- (+ (* ,offset n-word-bytes) (* index ,size)) ,lowtag)))))))

(defmacro define-float-reffer (name type size format offset lowtag scs eltype &optional arrayp note translate)
  (let ((shift (if arrayp
                   (- (integer-length size) n-fixnum-tag-bits 1)
                   (- word-shift n-fixnum-tag-bits))))
    `(progn
       (define-vop (,name)
         (:note ,note)
         ,@(when translate `((:translate ,translate)))
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg))
                (index :scs (any-reg)))
         (:arg-types ,type tagged-num)
         (:temporary (:scs (interior-reg)) lip)
         ,@(unless (zerop shift)
             `((:temporary (:sc non-descriptor-reg) temp)))
         (:results (value :scs ,scs))
         (:result-types ,eltype)
         (:generator 5
           ,@(cond ((zerop shift)
                    `((inst add lip object index)))
                   (t
                    `((inst slli temp index ,shift)
                      (inst add lip object temp))))
           (inst fload ,format value lip (- (* ,offset n-word-bytes) ,lowtag))))
       (define-vop (,(symbolicate name "-C"))
         (:note ,note)
         ,@(when translate `((:translate ,translate)))
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg)))
         (:info index)
         (:arg-types ,type
           (:constant (load/store-index ,(if arrayp size n-word-bytes) ,(eval lowtag) ,(eval offset))))
         (:results (value :scs ,scs))
         (:result-types ,eltype)
         (:generator 4
           (inst fload ,format value object (- (+ (* ,offset n-word-bytes) (* (* index ,(if arrayp size n-word-bytes)))) ,lowtag)))))))

(defmacro define-float-setter (name type size format offset lowtag scs eltype &optional arrayp note translate)
  (let ((shift (if arrayp
                   (- (integer-length size) n-fixnum-tag-bits 1)
                   (- word-shift n-fixnum-tag-bits)))
        (resultp nil))
    `(progn
       (define-vop (,name)
         (:note ,note)
         ,@(when translate `((:translate ,translate)))
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg))
                (index :scs (any-reg))
                (value :scs ,scs ,@(when resultp '(:target result))))
         (:arg-types ,type tagged-num ,eltype)
         (:temporary (:scs (interior-reg)) lip)
         ,@(unless (zerop shift)
             `((:temporary (:sc non-descriptor-reg) temp)))
         ,@(when resultp `((:results (result :scs ,scs)) (:result-types ,eltype)))
         (:generator 5
           ,@(cond ((zerop shift)
                    `((inst add lip object index)))
                   (t
                    `((inst slli temp index ,shift)
                      (inst add lip object temp))))
           (inst fstore ,format value lip (- (* ,offset n-word-bytes) ,lowtag))
           ,@(when resultp
               `((unless (location= result value) (inst fmove ,format result value))))))
       (define-vop (,(symbolicate name "-C"))
         (:note ,note)
         ,@(when translate `((:translate ,translate)))
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg))
                (value :scs ,scs ,@(when resultp '(:target result))))
         (:info index)
         (:arg-types ,type
           (:constant (load/store-index ,(if arrayp size n-word-bytes) ,(eval lowtag) ,(eval offset)))
           ,eltype)
         ,@(when resultp `((:results (result :scs ,scs)) (:result-types ,eltype)))
         (:generator 4
           (inst fstore ,format value object (- (+ (* ,offset n-word-bytes) (* index ,(if arrayp size n-word-bytes))) ,lowtag))
           ,@(when resultp
               `((unless (location= result value) (inst fmove ,format result value)))))))))

;; FIXME: constant arg VOPs missing.
(defmacro define-complex-float-reffer (name type size format offset lowtag scs eltype &optional arrayp note translate)
  (let ((shift (if arrayp
                   (- (integer-length size) n-fixnum-tag-bits)
                   (- word-shift n-fixnum-tag-bits))))
    `(define-vop (,name)
       (:note ,note)
       ,@(when translate `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
              (index :scs (any-reg)))
       (:arg-types ,type tagged-num)
       (:temporary (:scs (interior-reg)) lip)
       ,@(unless (zerop shift)
           `((:temporary (:sc non-descriptor-reg) temp)))
       (:results (value :scs ,scs))
       (:result-types ,eltype)
       (:generator 6
         ,@(cond ((zerop shift)
                  `((inst add lip object index)))
                 (t
                  `((inst slli temp index ,shift)
                    (inst add lip object temp))))
         ,(ecase format
            #+64-bit
            (:single
             `(inst fload :double value lip (- (* ,offset n-word-bytes) ,lowtag)))
            ((#-64-bit :single :double)
             `(progn
                (let ((real-tn (complex-reg-real-tn ,format value)))
                  (inst fload ,format real-tn lip (- (* ,offset n-word-bytes) ,lowtag)))
                (let ((imag-tn (complex-reg-imag-tn ,format value)))
                  (inst fload ,format imag-tn lip (- (+ (* ,offset n-word-bytes) ,size) ,lowtag))))))))))

(defmacro define-complex-float-setter (name type size format offset lowtag scs eltype &optional arrayp note translate)
  (let ((shift (if arrayp
                   (- (integer-length size) n-fixnum-tag-bits)
                   (- word-shift n-fixnum-tag-bits)))
        (resultp nil))
    `(define-vop (,name)
       (:note ,note)
       ,@(when translate `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
              (index :scs (any-reg))
              (value :scs ,scs ,@(when resultp '(:target result))))
       (:arg-types ,type tagged-num ,eltype)
       (:temporary (:scs (interior-reg)) lip)
       ,@(unless (zerop shift)
           `((:temporary (:sc non-descriptor-reg) temp)))
       ,@(when resultp `((:results (result :scs ,scs)) (:result-types ,eltype)))
       (:generator 6
         ,@(cond ((zerop shift)
                  `((inst add lip object index)))
                 (t
                  `((inst slli temp index ,shift)
                    (inst add lip object temp))))
         ,(ecase format
            #+64-bit
            (:single
             `(inst fstore :double value lip (- (* ,offset n-word-bytes) ,lowtag)))
            ((#-64-bit :single :double)
             `(progn
                (let ((real-tn (complex-reg-real-tn ,format value)))
                  (inst fstore ,format real-tn lip (- (* ,offset n-word-bytes) ,lowtag)))
                (let ((imag-tn (complex-reg-imag-tn ,format value)))
                  (inst fstore ,format imag-tn lip (- (+ (* ,offset n-word-bytes) ,size) ,lowtag))))))
         ,@(when resultp `((move-complex ,format result value)))))))

(defmacro define-full-casser (name type offset lowtag scs eltype &optional translate)
  `(define-vop (,name)
     ,@(when translate `((:translate ,translate)))
     (:policy :fast-safe)
     (:args (object :scs (descriptor-reg))
            (index :scs (any-reg) :target temp)
            (old-value :scs ,scs)
            (new-value :scs ,scs))
     (:arg-types ,type positive-fixnum ,eltype ,eltype)
     (:temporary (:scs (interior-reg)) lip)
     (:temporary (:sc non-descriptor-reg) temp)
     (:results (result :scs ,scs :from :load))
     (:result-types ,eltype)
     (:generator 5
       (with-fixnum-as-word-index (index temp)
         (inst add lip object index))
       (inst addi lip lip (- (* ,offset n-word-bytes) ,lowtag))
       LOOP
       (inst lr result lip :aq)
       (inst bne result old-value EXIT)
       (inst sc temp new-value lip :aq :rl)
       (inst bne temp zero-tn LOOP)
       EXIT)))

(defmacro define-atomic-frobber (name op type offset lowtag scs eltype &optional translate)
  `(define-vop (,name)
     ,@(when translate `((:translate ,translate)))
     (:policy :fast-safe)
     (:args (object :scs (descriptor-reg))
            (index :scs (any-reg)
                   ,@(when fixnum-as-word-index-needs-temp
                       '(:target temp)))
            (operand :scs ,scs :target result))
     (:arg-types ,type positive-fixnum ,eltype)
     (:results (result :scs ,scs))
     (:result-types unsigned-num)
     (:temporary (:sc interior-reg) lip)
     ,@(when fixnum-as-word-index-needs-temp
         '((:temporary (:sc any-reg) temp)))
     (:generator 3
       (with-fixnum-as-word-index (index temp)
         (inst add lip object index))
       (inst addi lip lip (- (* ,offset n-word-bytes) ,lowtag))
       (inst ,op result operand lip :aq :rl))))


;;;; Stack TN's

;;; Move a stack TN to a register and vice-versa.
(defmacro load-stack-tn (reg stack)
  `(let ((reg ,reg)
         (stack ,stack))
     (let ((offset (tn-offset stack)))
       (sc-case stack
         ((control-stack)
          (load-frame-word reg cfp-tn offset 'load-stack-tn nil))))))

(defmacro store-stack-tn (stack reg)
  `(let ((stack ,stack)
         (reg ,reg))
     (let ((offset (tn-offset stack)))
       (sc-case stack
         ((control-stack)
          (store-frame-word reg cfp-tn offset 'store-stack-tn nil))))))

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
           (load-frame-word ,n-reg cfp-tn (tn-offset ,n-stack) 'maybe-load-stack-tn nil)))))))

(defun align-csp (temp)
  (let ((aligned (gen-label)))
    (inst andi temp csp-tn lowtag-mask)
    (inst beq temp zero-tn aligned)
    (inst addi csp-tn csp-tn n-word-bytes)
    (storew zero-tn csp-tn -1)
    (emit-label aligned)))


;;;; Storage allocation:

(defun generate-stack-overflow-check (vop size temp)
  (let ((overflow (generate-error-code vop
                                       'stack-allocated-object-overflows-stack-error
                                       size)))
    #-sb-thread
    (load-symbol-value temp *control-stack-end*)
    #+sb-thread
    (loadw temp thread-base-tn thread-control-stack-end-slot)
    (inst sub temp temp csp-tn)
    (inst bge size temp overflow)))

#+gencgc
(defun alloc-tramp-stub-name (tn-offset type)
  (declare (type (unsigned-byte 5) tn-offset))
  (aref (load-time-value
         (let ((a (make-array 64)))
           (dotimes (i 32 a)
             (let ((r (write-to-string i)))
               (setf (aref a i)  (package-symbolicate "SB-VM" "ALLOC-LIST-TO-R" r)
                     (aref a (+ i 32)) (package-symbolicate "SB-VM" "ALLOC-TO-R" r)))))
         t)
        (if (eq type 'list) tn-offset (+ tn-offset 32))))

(defun load-alloc-free-pointer (reg)
  #-sb-thread
  (loadw reg null-tn 0 (- nil-value-offset mixed-region-offset))
  #+sb-thread
  (loadw reg thread-base-tn thread-mixed-tlab-slot))

(defun load-alloc-end-addr (reg)
  #-sb-thread
  (loadw reg null-tn 1 (- nil-value-offset mixed-region-offset))
  #+sb-thread
  (loadw reg thread-base-tn (+ thread-mixed-tlab-slot 1)))

(defun store-alloc-free-pointer (reg)
  #-sb-thread
  (storew reg null-tn 0 (- nil-value-offset mixed-region-offset))
  #+sb-thread
  (storew reg thread-base-tn thread-mixed-tlab-slot))

;;; This is the main mechanism for allocating memory in the lisp heap.
;;;
;;; The allocated space is stored in RESULT-TN with the lowtag LOWTAG
;;; applied.  The amount of space to be allocated is SIZE bytes (which
;;; must be a multiple of the lisp object size).
;;;
;;; Each platform seems to have its own slightly different way to do
;;; heap allocation, taking various different options as parameters.
;;; For RISC-V, we take the bare minimum parameters, RESULT-TN, SIZE,
;;; and LOWTAG, and we require a single temporary register called
;;; FLAG-TN to emphasize the parallelism with PSEUDO-ATOMIC (which
;;; must surround a call to ALLOCATION anyway), and to indicate that
;;; the P-A FLAG-TN is also acceptable here.
(defun allocation (type size lowtag result-tn &key flag-tn
                                                   stack-allocate-p
                                                   temp-tn)
  (declare (ignorable type))
  (cond (stack-allocate-p
         ;; Stack allocation
         ;;
         ;; The control stack grows up, so round up CSP to a
         ;; multiple of the lispobj size.  Use that as the
         ;; allocation pointer.  Then add SIZE bytes to the
         ;; allocation and set CSP to that, so we have the desired
         ;; space.
         (align-csp flag-tn)
         (inst ori result-tn csp-tn lowtag)
         (etypecase size
           (short-immediate
            (inst addi csp-tn csp-tn size))
           (u+i-immediate
            (inst li flag-tn size)
            (inst add csp-tn csp-tn flag-tn))
           (tn
            (inst add csp-tn csp-tn size))))
        ;; Normal allocation to the heap.
        (t
         (let ((alloc (gen-label))
               (back-from-alloc (gen-label)))
           (load-alloc-free-pointer result-tn)
           (etypecase size
             (short-immediate
              (inst addi result-tn result-tn size))
             (u+i-immediate
              (inst li temp-tn size)
              (inst add result-tn result-tn temp-tn))
             (tn
              (inst add result-tn result-tn size)))
           (load-alloc-end-addr flag-tn)
           (inst blt flag-tn result-tn alloc)
           (store-alloc-free-pointer result-tn)
           (emit-label back-from-alloc)
           ;; Compute the base pointer and add the lowtag.
           (etypecase size
             (short-immediate
              (inst subi result-tn result-tn (- size lowtag)))
             (u+i-immediate
              (inst sub result-tn result-tn temp-tn)
              (inst ori result-tn result-tn lowtag))
             (tn
              (inst sub result-tn result-tn size)
              (inst ori result-tn result-tn lowtag)))
           (assemble (:elsewhere)
             (emit-label alloc)
             (inst jal lip-tn (make-fixup (alloc-tramp-stub-name (tn-offset result-tn) type) :assembly-routine))
             (inst j back-from-alloc))))))

(defmacro with-fixed-allocation ((result-tn flag-tn type-code size
                                  &key (lowtag other-pointer-lowtag)
                                       stack-allocate-p
                                       temp-tn)
                                 &body body)
  "Do stuff to allocate an other-pointer object of fixed Size with a single
  word header having the specified Type-Code.  The result is placed in
  Result-TN, and Temp-TN is a non-descriptor temp (which may be randomly used
  by the body.)  The body is placed inside the PSEUDO-ATOMIC, and presumably
  initializes the object."
  (once-only ((result-tn result-tn) (flag-tn flag-tn)
              (type-code type-code) (size size)
              (stack-allocate-p stack-allocate-p)
              (lowtag lowtag))
    `(pseudo-atomic (,flag-tn)
       (allocation nil (pad-data-block ,size) ,lowtag ,result-tn
                   :flag-tn ,flag-tn
                   :stack-allocate-p ,stack-allocate-p
                   ,@(when temp-tn `(:temp-tn ,temp-tn)))
       (inst li ,flag-tn (compute-object-header ,size ,type-code))
       (storew ,flag-tn ,result-tn 0 ,lowtag)
       ,@body)))
