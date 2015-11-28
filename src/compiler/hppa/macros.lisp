;;;; various useful macros for generating HPPA code

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.
(in-package "SB!VM")



(defmacro expand (expr)
  (let ((gensym (gensym)))
    `(macrolet
       ((,gensym ()
           ,expr))
       (,gensym))))

;;; Instruction-like macros.
;;; FIXME-lav: add if always-emit-code-p is :e= then error if location=
(defmacro move (src dst &optional always-emit-code-p)
  #!+sb-doc
  "Move SRC into DST (unless they are location= and ALWAYS-EMIT-CODE-P is nil)."
  (once-only ((n-src src)
              (n-dst dst))
    `(if (location= ,n-dst ,n-src)
       (when ,always-emit-code-p
         (inst nop))
       (inst move ,n-src ,n-dst))))

(defmacro loadw (result base &optional (offset 0) (lowtag 0))
  (once-only ((result result) (base base))
    `(inst ldw (- (ash ,offset word-shift) ,lowtag) ,base ,result)))

(defmacro storew (value base &optional (offset 0) (lowtag 0))
  (once-only ((value value) (base base) (offset offset) (lowtag lowtag))
    `(inst stw ,value (- (ash ,offset word-shift) ,lowtag) ,base)))

(defmacro load-symbol (reg symbol)
  (once-only ((reg reg) (symbol symbol))
    `(let ((offset (static-symbol-offset ,symbol)))
       (cond
         ((typep offset '(signed-byte 11))
           (inst addi offset null-tn ,reg))
         (t
           (inst ldil offset ,reg)
           (inst ldo offset null-tn ,reg :unsigned t))))))

(defmacro load-symbol-value (reg symbol)
  `(inst ldw
         (+ (static-symbol-offset ',symbol)
            (ash symbol-value-slot word-shift)
            (- other-pointer-lowtag))
         null-tn ,reg))

(defmacro store-symbol-value (reg symbol)
  `(inst stw ,reg (+ (static-symbol-offset ',symbol)
                     (ash symbol-value-slot word-shift)
                     (- other-pointer-lowtag))
         null-tn))

(defmacro load-type (target source &optional (offset 0))
  #!+sb-doc
  "Loads the type bits of a pointer into target independent of
byte-ordering issues."
  (once-only ((n-target target)
              (n-source source)
              (n-offset offset))
    (ecase *backend-byte-order*
      (:little-endian
       `(inst ldb ,n-offset ,n-source ,n-target))
      (:big-endian
       `(inst ldb (+ ,n-offset (1- n-word-bytes)) ,n-source ,n-target)))))

(defmacro set-lowtag (tag src dst)
  `(progn
     (inst move ,src ,dst)
     (inst dep ,tag 31 n-lowtag-bits ,dst)))

;;; Macros to handle the fact that we cannot use the machine native call and
;;; return instructions.

(defmacro lisp-jump (function)
  #!+sb-doc
  "Jump to the lisp function FUNCTION."
  `(progn
     (inst addi (- (ash simple-fun-code-offset word-shift)
                   fun-pointer-lowtag) ,function lip-tn)
     (inst bv lip-tn)
     (move ,function code-tn t)))

(defmacro lisp-return (return-pc &key (offset 0) (frob-code t))
  #!+sb-doc
  "Return to RETURN-PC."
  `(progn
     (inst addi (- (* (1+ ,offset) n-word-bytes) other-pointer-lowtag)
           ,return-pc lip-tn)
     (inst bv lip-tn ,@(unless frob-code '(:nullify t)))
     ,@(if frob-code
         `((move ,return-pc code-tn t)))))

(defmacro emit-return-pc (label)
  #!+sb-doc
  "Emit a return-pc header word.  LABEL is the label to use for this
   return-pc."
  `(progn
     ;; alignment causes the return point to land on two address,
     ;; where the first must be nop pad.
     (emit-alignment n-lowtag-bits)
     (emit-label ,label)
     (inst lra-header-word)))


;;;; Stack TN's

;;; Move a stack TN to a register and vice-versa.
(defmacro load-stack-tn (reg stack)
  `(let ((reg ,reg)
         (stack ,stack))
     (let ((offset (tn-offset stack)))
       (sc-case stack
         ((control-stack)
          (loadw reg cfp-tn offset))))))

(defmacro store-stack-tn (stack reg)
  `(let ((stack ,stack)
         (reg ,reg))
     (let ((offset (tn-offset stack)))
       (sc-case stack
         ((control-stack)
          (storew reg cfp-tn offset))))))

(defmacro maybe-load-stack-tn (reg reg-or-stack)
  #!+sb-doc
  "Move the TN Reg-Or-Stack into Reg if it isn't already there."
  (once-only ((n-reg reg)
              (n-stack reg-or-stack))
    `(sc-case ,n-reg
       ((any-reg descriptor-reg)
        (sc-case ,n-stack
          ((any-reg descriptor-reg)
           (move ,n-stack ,n-reg))
          ((control-stack)
           (loadw ,n-reg cfp-tn (tn-offset ,n-stack))))))))


;;;; Storage allocation:

(defmacro with-fixed-allocation ((result-tn flag-tn temp-tn type-code
                                  size dynamic-extent-p
                                  &key (lowtag other-pointer-lowtag)
                                       maybe-write)
                                 &body body)
  #!+sb-doc
  "Do stuff to allocate an other-pointer object of fixed Size with a single
word header having the specified Type-Code.  The result is placed in
Result-TN, and Temp-TN is a non-descriptor temp (which may be randomly used
by the body.)  The body is placed inside the PSEUDO-ATOMIC, and presumably
initializes the object."
  (declare (ignore flag-tn))
  (once-only ((result-tn result-tn) (temp-tn temp-tn)
              (type-code type-code) (size size)
              (lowtag lowtag))
    (let ((write-body `((inst li (logior (ash (1- ,size) n-widetag-bits) ,type-code) ,temp-tn)
                        (storew ,temp-tn ,result-tn 0 ,lowtag))))
      `(if ,dynamic-extent-p
         (pseudo-atomic ()
           (align-csp ,temp-tn)
           (set-lowtag ,lowtag csp-tn ,result-tn)
           (inst addi (pad-data-block ,size) csp-tn csp-tn)
           ,@(if maybe-write
               `((when ,type-code ,@write-body))
               write-body)
           ,@body)
         (pseudo-atomic (:extra (pad-data-block ,size))
           (set-lowtag ,lowtag alloc-tn ,result-tn)
           ,@(if maybe-write
               `((when ,type-code ,@write-body))
               write-body)
           ,@body)))))

;;; is used for stack allocation of dynamic-extent objects
;;; FIXME-lav, if using defun, atleast surround in assembly-form ? macro better ?
(defun align-csp (temp)
  (declare (ignore temp))
  (let ((aligned (gen-label)))
    (inst extru csp-tn 31 n-lowtag-bits zero-tn :<>)
    (inst b aligned :nullify t)
    (inst addi n-word-bytes csp-tn csp-tn)
    (storew zero-tn csp-tn -1)
    (emit-label aligned)))


;;;; Error Code
(defun emit-error-break (vop kind code values)
  (assemble ()
    (when vop
      (note-this-location vop :internal-error))
    (inst break kind)
    (with-adjustable-vector (vector)
      (write-var-integer code vector)
      (dolist (tn values)
        (write-var-integer (make-sc-offset (sc-number
                                            (tn-sc tn))
                                           (tn-offset tn))
                           vector))
      (inst byte (length vector))
      (dotimes (i (length vector))
        (inst byte (aref vector i))))
    (emit-alignment word-shift)))

(defun error-call (vop error-code &rest values)
  #!+sb-doc
  "Cause an error.  ERROR-CODE is the error to cause."
  (emit-error-break vop error-trap (error-number-or-lose error-code) values))


(defun cerror-call (vop label error-code &rest values)
  #!+sb-doc
  "Cause a continuable error.  If the error is continued, execution resumes at
  LABEL."
  (without-scheduling ()
    (inst b label)
    (emit-error-break vop cerror-trap (error-number-or-lose error-code) values)))

(defun generate-error-code (vop error-code &rest values)
  #!+sb-doc
  "Generate-Error-Code Error-code Value*
  Emit code for an error with the specified Error-Code and context Values."
  (assemble (*elsewhere*)
    (let ((start-lab (gen-label)))
      (emit-label start-lab)
      (apply #'error-call vop error-code values)
      start-lab)))

(defmacro generate-cerror-code (vop error-code &rest values)
  #!+sb-doc
  "Generate-CError-Code Error-code Value*
  Emit code for a continuable error with the specified Error-Code and
  context Values.  If the error is continued, execution resumes after
  the GENERATE-CERROR-CODE form."
  (assemble ()
    (let ((continue (gen-label)))
      (emit-label continue)
      (assemble (*elsewhere*)
        (let ((error (gen-label)))
          (emit-label error)
          (apply #'cerror-call vop continue error-code values)
          error)))))

;;;; PSEUDO-ATOMIC

;;; handy macro for making sequences look atomic
(defmacro pseudo-atomic ((&key (extra 0)) &rest forms)
  (let ((n-extra (gensym)))
    `(let ((,n-extra ,extra))
       (inst addi 4 alloc-tn alloc-tn)
       ,@forms
       (cond
         ((typep ,n-extra '(signed-byte 11))
          (inst addit (- ,n-extra 4) alloc-tn alloc-tn :od))
         ((typep ,n-extra '(signed-byte 14))
          (inst ldo ,n-extra alloc-tn alloc-tn)
          (inst addit -4 alloc-tn alloc-tn :od))
         (t
          ;; FIXME: Make this case work, somehow
          (error "EXTRA out-of-range in PSEUDO-ATOMIC"))))))

;;;; indexed references

(deftype load/store-index (scale lowtag min-offset
                                 &optional (max-offset min-offset))
  `(integer ,(- (truncate (+ (ash 1 14)
                             (* min-offset n-word-bytes)
                             (- lowtag))
                          scale))
            ,(truncate (- (+ (1- (ash 1 14)) lowtag)
                          (* max-offset n-word-bytes))
                       scale)))

(defmacro define-full-reffer (name type offset lowtag scs el-type
                                   &optional translate)
  `(progn
     (define-vop (,name)
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
         (inst add object index lip)
         (loadw value lip ,offset ,lowtag)))
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
       (:generator 4
         (loadw value object (+ ,offset index) ,lowtag)))))

(defmacro define-full-setter (name type offset lowtag scs el-type
                                   &optional translate)
  `(progn
     (define-vop (,name)
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
         (inst add object index lip)
         (storew value lip ,offset ,lowtag)
         (move value result)))
     (define-vop (,(symbolicate name "-C"))
       ,@(when translate
           `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
              (value :scs ,scs))
       (:info index)
       (:arg-types ,type
                   (:constant (load/store-index ,n-word-bytes ,(eval lowtag)
                                                ,(eval offset)))
                   ,el-type)
       (:results (result :scs ,scs))
       (:result-types ,el-type)
       (:generator 1
         (storew value object (+ ,offset index) ,lowtag)
         (move value result)))))


(defmacro define-partial-reffer (name type size signed offset lowtag scs
                                      el-type &optional translate)
  (let ((scale (ecase size (:byte 1) (:short 2))))
    `(progn
       (define-vop (,name)
         ,@(when translate
             `((:translate ,translate)))
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg) :to (:eval 0))
                (index :scs (unsigned-reg)))
         (:arg-types ,type positive-fixnum)
         (:results (value :scs ,scs))
         (:result-types ,el-type)
         (:temporary (:scs (interior-reg)) lip)
         (:generator 5
           (inst ,(ecase size (:byte 'add) (:short 'sh1add))
                 index object lip)
           (inst ,(ecase size (:byte 'ldb) (:short 'ldh))
                 (- (* ,offset n-word-bytes) ,lowtag) lip value)
           ,@(when signed
               `((inst extrs value 31 ,(* scale n-byte-bits) value)))))
       (define-vop (,(symbolicate name "-C"))
         ,@(when translate
             `((:translate ,translate)))
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg)))
         (:info index)
         (:arg-types ,type
                     (:constant (load/store-index ,scale
                                                  ,(eval lowtag)
                                                  ,(eval offset))))
         (:results (value :scs ,scs))
         (:result-types ,el-type)
         (:generator 5
           (inst ,(ecase size (:byte 'ldb) (:short 'ldh))
                 (- (+ (* ,offset n-word-bytes) (* index ,scale)) ,lowtag)
                 object value)
           ,@(when signed
               `((inst extrs value 31 ,(* scale n-byte-bits) value))))))))

(defmacro define-partial-setter (name type size offset lowtag scs el-type
                                      &optional translate)
  (let ((scale (ecase size (:byte 1) (:short 2))))
    `(progn
       (define-vop (,name)
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
           (inst ,(ecase size (:byte 'add) (:short 'sh1add))
                 index object lip)
           (inst ,(ecase size (:byte 'stb) (:short 'sth))
                 value (- (* ,offset n-word-bytes) ,lowtag) lip)
           (move value result)))
       (define-vop (,(symbolicate name "-C"))
         ,@(when translate
             `((:translate ,translate)))
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg))
                (value :scs ,scs :target result))
         (:info index)
         (:arg-types ,type
                     (:constant (load/store-index ,scale
                                                  ,(eval lowtag)
                                                  ,(eval offset)))
                     ,el-type)
         (:results (result :scs ,scs))
         (:result-types ,el-type)
         (:generator 5
           (inst ,(ecase size (:byte 'stb) (:short 'sth))
                 value
                 (- (+ (* ,offset n-word-bytes) (* index ,scale)) ,lowtag)
                 object)
           (move value result))))))


(def!macro with-pinned-objects ((&rest objects) &body body)
  "Arrange with the garbage collector that the pages occupied by
OBJECTS will not be moved in memory for the duration of BODY.
Useful for e.g. foreign calls where another thread may trigger
garbage collection.  This is currently implemented by disabling GC"
  (declare (ignore objects))            ;should we eval these for side-effect?
  `(without-gcing
    ,@body))

