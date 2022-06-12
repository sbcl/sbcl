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
    (:args (vector :scs (descriptor-reg))
           (index :scs (any-reg)))
    (:arg-types * tagged-num)
    (:results (res :scs (int-sse-reg)))
    (:result-types simd-pack-ub32)
    (:policy :fast-safe)
    (:generator 3
      (inst movdqa res
        (ea (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)
          vector
          index
          (index-scale (* n-word-bytes 2) index)))))

  (define-vop (set-vector-ref-128)
    (:translate (setf vector-ref-128))
    (:args (value :scs (int-sse-reg))
           (vector :scs (descriptor-reg))
           (index :scs (any-reg)))
    (:arg-types simd-pack-ub32 * tagged-num)
    (:policy :fast-safe)
    (:generator 3
      (inst movdqa (ea (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)
                     vector
                     index
                     (index-scale (* n-word-bytes 2) index))
        value))))

;;; Cross compilation doesn't understand simd-pack constants.
(defmacro simd-mask (value)
  `(inline-vop
       (((:info value) ,(logior value
                                (ash value 32)
                                (ash value 64)
                                (ash value 96))))
       ((res int-sse-reg simd-pack-ub32))
     (inst movdqa res (register-inline-constant :sse value))))

(defmacro simd-case (a source destination index fallback)
  `(macrolet ((mask (ub32)
                `(%make-simd-pack-ub32 ,ub32 ,ub32 ,ub32 ,ub32)))
     (let ((ascii-p (simd-mask 192))
           (a-mask (simd-mask ,(+ (expt 2 31) (char-code a))))
           (z-mask (simd-mask ,(+ (expt 2 31) 25)))
           (flip (simd-mask #x20)))
       (declare (optimize speed
                          sb-c::preserve-single-use-debug-variables))
       (loop for ,index below (ceiling length (/ 128 32))
             do
             (let ((bits (vector-ref-128 ,source ,index)))
               (unless (zerop (inline-vop
                                  (((bits int-sse-reg simd-pack-ub32 :target temp) bits)
                                   ((ascii-p int-sse-reg simd-pack-ub32 :to :save) ascii-p)
                                   ((temp)))
                                  ((res unsigned-reg unsigned-num))
                                (move temp bits)
                                (inst pcmpgtd temp ascii-p)
                                (inst pmovmskb res temp)))
                 (return ,fallback))
               (setf (vector-ref-128 ,destination ,index)
                     (inline-vop
                         (((bits int-sse-reg simd-pack-ub32 :target res) bits)
                          ((a-mask int-sse-reg simd-pack-ub32 :to :save) a-mask)
                          ((z-mask) z-mask)
                          ((flip) flip)
                          ((temp)))
                         ((res int-sse-reg simd-pack-ub32))
                       (move temp bits)
                       (inst psubd temp a-mask)
                       (inst pcmpgtd temp z-mask)
                       (inst pandn temp flip)
                       (move res bits)
                       (inst pxor res temp))))))))
