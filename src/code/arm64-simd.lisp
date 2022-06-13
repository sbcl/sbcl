;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-vop ()
    (:translate vector-ref-128)
    (:args (vector :scs (descriptor-reg) :to :result)
           (index :scs (any-reg)))
    (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
    (:arg-types * tagged-num)
    (:results (res :scs (complex-double-reg)))
    (:result-types complex-double-float)
    (:policy :fast-safe)
    (:generator 3
      (inst lsl offset index (1+ (- word-shift n-fixnum-tag-bits)))
      (inst add offset offset (- (* vector-data-offset n-word-bytes)
                                other-pointer-lowtag))
      (inst ldr res (@ vector offset))))

  (define-vop (set-vector-ref-128)
    (:translate (setf vector-ref-128))
    (:args (value :scs (complex-double-reg))
           (vector :scs (descriptor-reg) :to :result)
           (index :scs (any-reg)))
    (:temporary (:scs (non-descriptor-reg) :from (:argument 2)) offset)
    (:arg-types complex-double-float * tagged-num)
    (:policy :fast-safe)
    (:generator 3
      (inst lsl offset index (1+ (- word-shift n-fixnum-tag-bits)))
      (inst add offset offset (- (* vector-data-offset n-word-bytes)
                                other-pointer-lowtag))
      (inst str value (@ vector offset)))))

(defmacro simd-mask (value)
  `(inline-vop
       (((:info value) ,(logior value
                                (ash value 32)
                                (ash value 64)
                                (ash value 96))))
       ((res complex-double-reg complex-double-float))
     (load-inline-constant res :oword value)))

(defmacro simd-string-case (a source destination index fallback)
  `(let ((ascii-p (simd-mask 192))
         (a-mask (simd-mask ,(char-code a)))
         (z-mask (simd-mask 25))
         (flip (simd-mask #x20)))
     (declare (optimize sb-c::preserve-single-use-debug-variables))
     (loop for ,index below (ceiling length (/ 128 32))
           do
           (let ((bits (vector-ref-128 ,source ,index)))
             (unless (zerop (inline-vop
                                (((bits complex-double-reg complex-double-float :target temp) bits)
                                 ((ascii-p complex-double-reg complex-double-float :to :save) ascii-p)
                                 ((temp)))
                                ((res unsigned-reg unsigned-num))
                              (inst cmgt temp bits ascii-p :4s)
                              (inst umaxv temp temp :4s)
                              (inst umov res temp 0 :s)))
               (return ,fallback))
             (setf (vector-ref-128 ,destination ,index)
                   (inline-vop (((bits complex-double-reg complex-double-float :target res) bits)
                                ((a-mask complex-double-reg complex-double-float :to :save) a-mask)
                                ((z-mask) z-mask)
                                ((flip) flip)
                                ((temp)))
                       ((res complex-double-reg complex-double-float))
                     (inst s-sub temp bits a-mask :4s)
                     (inst cmhs temp z-mask temp :4s)
                     (inst s-and temp temp flip)
                     (inst s-eor res bits temp)))))))
