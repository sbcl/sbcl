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

(defun emit-error-break (vop kind code values)
  (assemble ()
    (when vop (note-this-location vop :internal-error))
    (emit-internal-error kind code values)
    (emit-alignment word-shift)))

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
         (inst lw value lip (- (ash ,offset word-shift) ,lowtag))))
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
         (inst lw value object (- (ash (+ ,offset index) word-shift) ,lowtag))))))

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
