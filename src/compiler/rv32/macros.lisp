;;;; various useful macros for generating RV32 code

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

(defmacro move (dst src &optional (always-emit-code-p nil))
  "Move SRC into DST (unless they are location=."
  (once-only ((n-dst dst)
              (n-src src))
    `(unless (location= ,n-dst ,n-src)
       ;; annoying hack with the null-tn, but it has to be done.
       (inst addi ,n-dst ,n-src 0))))

(defmacro def-mem-op (op inst shift load)
  `(defmacro ,op (object base &optional (offset 0) (lowtag 0))
     `(inst ,',inst ,object ,base (- (ash ,offset ,,shift) ,lowtag))))
;;;
(def-mem-op loadw lw word-shift t)
(def-mem-op storew sw word-shift nil)


;;;; Three Way Comparison
(defun three-way-comparison (x y condition flavor not-p target)
  (ecase condition
    (:eq (if not-p
             (inst bne x y target)
             (inst beq x y target)))
    ((:lt :gt)
     (when (eq flavor :gt)
       (rotatef x y))
     (ecase flavor
       (:unsigned (if not-p
                      (inst bltu x y target)
                      (inst bgeu x y target)))
       (:signed (if not-p
                    (inst blt x y target)
                    (inst bge x y target)))))))


(defun emit-error-break (vop kind code values)
  (assemble ()
    (when vop (note-this-location vop :internal-error))
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

#|
If we are doing [reg+offset*n-word-bytes-lowtag+index*scale]
and

-2^11 ≤ offset*n-word-bytes - lowtag + index*scale < 2^11
-2^11 ≤ offset*n-word-bytes - lowtag + index*scale ≤ 2^11-1
-2^11 + lowtag -offset*n-word-bytes ≤ index*scale ≤ 2^11-1 + lowtag - offset*n-word-bytes
|#
(deftype load/store-index (scale lowtag offset)
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
       (:args (object :scs (descriptor-reg)) (index :scs (any-reg)))
       (:arg-types ,type tagged-num)
       (:temporary (:scs (interior-reg)) lip)
       (:results (value :scs ,scs))
       (:result-types ,eltype)
       (:generator 5
         (inst add lip object index)
         (loadw value lip ,offset ,lowtag)))
     (define-vop (,(symbolicate name "-C"))
       ,@(when translate `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg)))
       (:info index)
       (:arg-types ,type
                   (:constant
                    (load/store-index #.n-word-bytes ,(eval lowtag) ,(eval offset))))
       (:results (value :scs ,scs))
       (:result-types ,eltype)
       (:generator 4
         (loadw value object (+ ,offset index) ,lowtag)))))

(defmacro define-full-setter (name type offset lowtag scs eltype &optional translate)
  `(progn
     (define-vop (,name)
       ,@(when translate `((:translate ,translate)))
       (:args (object :scs (descriptor-reg)) (index :scs (any-reg)) (value :scs ,scs))
       (:arg-types ,type tagged-num ,eltype)
       (:results (result :scs ,scs))
       (:result-types ,eltype)
       (:generator 5))))

(defmacro define-partial-reffer (name type size signed offset lowtag scs eltype &optional translate)
  `(progn
     (define-vop (,name)
       ,@(when translate `((:translate ,translate)))
       (:args (object :scs (descriptor-reg)) (index :scs (any-reg)))
       (:arg-types ,type tagged-num)
       (:results (value :scs ,scs))
       (:result-types ,eltype)
       (:generator 5))))

(defmacro define-partial-setter (name type size offset lowtag scs eltype &optional translate)
  `(progn
     (define-vop (,name)
       ,@(when translate `((:translate ,translate)))
       (:args (object :scs (descriptor-reg)) (index :scs (any-reg)) (value :scs ,scs))
       (:arg-types ,type tagged-num ,eltype)
       (:results (result :scs ,scs))
       (:result-types ,eltype)
       (:generator 5))))


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
  "Move the TN Reg-Or-Stack into Reg if it isn't already there."
  (once-only ((n-reg reg)
              (n-stack reg-or-stack))
    `(sc-case ,n-reg
       ((any-reg descriptor-reg)
        (sc-case ,n-stack
          ((any-reg descriptor-reg)
           (move ,n-reg ,n-stack))
          ((control-stack)
           (loadw ,n-reg cfp-tn (tn-offset ,n-stack))))))))
