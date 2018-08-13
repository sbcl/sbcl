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

(defmacro define-full-reffer (name type offset lowtag scs eltype &optional translate)
  `(progn
     (define-vop (,name)
       ,@(when translate `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg)) (index :scs (any-reg)))
       (:arg-types ,type tagged-num)
       (:results (value :scs ,scs))
       (:result-types ,eltype)
       (:generator 5))))

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
