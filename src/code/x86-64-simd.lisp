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

(defmacro simd-string-case (a source destination index fallback)
  `(let ((ascii-p (simd-mask 192))
         (a-mask (simd-mask ,(+ (expt 2 31) (char-code a))))
         (z-mask (simd-mask ,(+ (expt 2 31) 25)))
         (flip (simd-mask #x20)))
     (declare (optimize sb-c::preserve-single-use-debug-variables))
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
                     (inst pxor res temp)))))))

(defun simd-nreverse8 (vector start end)
  (declare ((simple-array * (*)) vector)
           (fixnum start end)
           (optimize speed (safety 0)))
  (let ((sap (vector-sap vector)))
    (inline-vop (((left sap-reg t) sap)
                 ((start any-reg tagged-num) start)
                 ((end) end)
                 ((right signed-reg signed-num))
                 ((l))
                 ((r)))
        ()
      (inst shr end 1)
      (inst shr start 1)
      (inst lea right (ea left end))
      (inst add left start)
      (inst mov l right)
      (inst sub l left)
      (inst cmp l 16)
      (inst jmp :b BYTE)
      (inst sub right 8)

      LOOP
      (inst mov l (ea left))
      (inst mov r (ea right))
      (inst bswap l)
      (inst bswap r)
      (inst mov (ea left) r)
      (inst mov (ea right) l)
      (inst add left 8)
      (inst sub right 8)
      (inst cmp left right)
      (inst jmp :b LOOP)

      (inst add right 8)

      BYTE
      (inst sub right 1)
      (inst cmp right left)
      (inst jmp :b DONE)

      ;; After the 16-element copy above there are at most 15
      ;; elements, have to swap 14 elements with one staying in the
      ;; middle.
      (loop repeat 7
            do
            (inst mov :byte l (ea left))
            (inst mov :byte r (ea right))
            (inst mov :byte (ea left) r)
            (inst mov :byte (ea right) l)
            (inst add left 1)
            (inst sub right 1)
            (inst cmp left right)
            (inst jmp :ge DONE))
      DONE))
  vector)

(defun simd-nreverse32 (vector start end)
  (declare ((simple-array * (*)) vector)
           (fixnum start end)
           (optimize speed (safety 0)))
  (let ((sap (vector-sap vector)))
    (inline-vop (((left sap-reg t) sap)
                 ((start any-reg tagged-num) start)
                 ((end) end)
                 ((right signed-reg signed-num))
                 ((l))
                 ((r)))
        ()
      (inst shl end 1)
      (inst shl start 1)
      (inst lea right (ea left end))
      (inst add left start)
      (inst mov l right)
      (inst sub l left)
      (inst cmp l 16)
      (inst jmp :b SCALAR)
      (inst sub right 8)

      LOOP
      (inst mov l (ea left))
      (inst mov r (ea right))
      (inst ror l 32)
      (inst ror r 32)
      (inst mov (ea left) r)
      (inst mov (ea right) l)
      (inst add left 8)
      (inst sub right 8)
      (inst cmp left right)
      (inst jmp :b LOOP)

      (inst add right 8)

      SCALAR
      (inst sub right 4)
      (inst cmp right left)
      (inst jmp :b DONE)

      (inst mov :dword l (ea left))
      (inst mov :dword r (ea right))
      (inst mov :dword (ea left) r)
      (inst mov :dword (ea right) l)
      DONE))
  vector)

(defun simd-reverse8 (source start length target)
  (declare ((simple-array * (*)) vector target)
           (fixnum start length)
           (optimize speed (safety 0)))
  (let ((source (vector-sap source))
        (target (vector-sap target)))
    (inline-vop (((source sap-reg t) source)
                 ((target sap-reg t) target)
                 ((start any-reg tagged-num) start)
                 ((length) length)
                 ((s-i signed-reg signed-num))
                 ((t-i))
                 ((g)))
        ()
      (inst shr start 1)
      (inst add source start)
      (zeroize t-i)
      (inst mov s-i length)
      (inst shr s-i 1)
      (inst cmp s-i 8)
      (inst jmp :b SCALAR)
      (inst sub s-i 8)

      LOOP
      (inst mov g (ea source s-i))
      (inst bswap g)
      (inst mov (ea target t-i) g)

      (inst add t-i 8)
      (inst sub s-i 8)
      (inst jmp :ge LOOP)
      (inst add s-i 8)

      SCALAR
      (inst sub s-i 1)
      (inst jmp :b DONE)

      (loop repeat 7
            do
            (inst mov :byte g (ea source s-i))
            (inst mov :byte (ea target t-i) g)
            (inst add t-i 1)
            (inst sub s-i 1)
            (inst jmp :b DONE))
      DONE))
  target)
