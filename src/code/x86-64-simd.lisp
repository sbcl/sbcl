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
  `(let ((ascii-p (simd-mask ,(if (char= a #\a)
                                  223
                                  191)))
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

(defun simd-nreverse8 (result vector start end)
  (declare ((simple-array * (*)) vector)
           (fixnum start end)
           (optimize speed (safety 0)))
  (with-pinned-objects (vector)
    (inline-vop (((left sap-reg t) (vector-sap vector))
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
      (inst dec right)
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
            (inst inc left)
            (inst dec right)
            (inst cmp left right)
            (inst jmp :ge DONE))
      DONE))
  result)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun concat-ub8 (ub8s)
    (let ((result 0))
      (loop for ub8 in ub8s
            do (setf result (logior (ash result 8) ub8)))
      result))

  (defun concat-ub32 (ub32s)
    (let ((result 0))
      (loop for ub32 in ub32s
            do (setf result (logior (ash result 32) ub32)))
      result)))

(def-variant simd-nreverse8 :avx2 (result vector start end)
  (declare (optimize speed (safety 0)))
  (with-pinned-objects (vector)
    (inline-vop (((left sap-reg t) (vector-sap vector))
                 ((start any-reg tagged-num) start)
                 ((end) end)
                 ((right signed-reg signed-num))
                 ((l))
                 ((r))
                 ((vl int-avx2-reg))
                 ((vr))
                 ((reverse-mask))
                 ((reverse-mask-xmm int-sse-reg))
                 ((vl-xmm))
                 ((vr-xmm)))
        ()
      (let ((reverse-mask-c (register-inline-constant :avx2 (concat-ub8 (loop for i below 32 collect i)))))
        (assemble ()
          (inst shr end 1)
          (inst shr start 1)
          (inst lea right (ea left end))
          (inst add left start)
          (inst mov l right)
          (inst sub l left)
          (inst cmp l 64)
          (inst jmp :b XMM)
          (inst sub right 32)

          (inst vmovdqu reverse-mask reverse-mask-c)
          LOOP
          (inst vmovdqu vl (ea left))
          (inst vmovdqu vr (ea right))
          (inst vperm2i128 vl vl vl 1)
          (inst vperm2i128 vr vr vr 1)
          (inst vpshufb vl vl reverse-mask)
          (inst vpshufb vr vr reverse-mask)
          (inst vmovdqu (ea left) vr)
          (inst vmovdqu (ea right) vl)
          (inst add left 32)
          (inst sub right 32)
          (inst cmp left right)
          (inst jmp :b LOOP)

          (inst vzeroupper)
          (inst add right 32)

          (inst mov l right)
          (inst sub l left)
          XMM
          (inst cmp l 32)
          (inst jmp :l WORD)
          (inst sub right 16)
          (inst vmovdqu reverse-mask-xmm reverse-mask-c)
          (inst vmovdqu vl-xmm (ea left))
          (inst vmovdqu vr-xmm (ea right))
          (inst vpshufb vl-xmm vl-xmm reverse-mask-c)
          (inst vpshufb vr-xmm vr-xmm reverse-mask-c)
          (inst vmovdqu (ea left) vr-xmm)
          (inst vmovdqu (ea right) vl-xmm)
          (inst add left 16)


          (inst mov l right)
          (inst sub l left)
          WORD

          (inst cmp l 16)
          (inst jmp :l BYTE)

          (inst sub right 8)
          (inst mov l (ea left))
          (inst mov r (ea right))
          (inst bswap l)
          (inst bswap r)
          (inst mov (ea left) r)
          (inst mov (ea right) l)
          (inst add left 8)

          BYTE
          (inst dec right)
          (inst cmp right left)
          (inst jmp :b DONE)

          (loop repeat 7
                do
                (inst mov :byte l (ea left))
                (inst mov :byte r (ea right))
                (inst mov :byte (ea left) r)
                (inst mov :byte (ea right) l)
                (inst inc left)
                (inst dec right)
                (inst cmp left right)
                (inst jmp :ge DONE))

          DONE))))
  result)

(defun simd-nreverse32 (result vector start end)
  (declare (optimize speed (safety 0)))
  (with-pinned-objects (vector)
    (inline-vop (((left sap-reg t) (vector-sap vector))
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
  result)

(def-variant simd-nreverse32 :avx2 (result vector start end)
  (declare (optimize speed (safety 0)))
  (with-pinned-objects (vector)
    (inline-vop (((left sap-reg t) (vector-sap vector))
                 ((start any-reg tagged-num) start)
                 ((end) end)
                 ((right signed-reg signed-num))
                 ((l))
                 ((r))
                 ((vl int-avx2-reg))
                 ((vr))
                 ((reverse-mask))
                 ((vl-xmm int-sse-reg))
                 ((vr-xmm)))
        ()
      (inst shl end 1)
      (inst shl start 1)
      (inst lea right (ea left end))
      (inst add left start)
      (inst mov l right)
      (inst sub l left)
      (inst cmp l 64)
      (inst jmp :b XMM)
      (inst sub right 32)

      (inst vmovdqu reverse-mask (register-inline-constant :avx2 (concat-ub32 (loop for i to 7 collect i))))
      LOOP
      (inst vmovdqu vl (ea left))
      (inst vmovdqu vr (ea right))
      (inst vpermd vl reverse-mask vl)
      (inst vpermd vr reverse-mask vr)

      (inst vmovdqu (ea left) vr)
      (inst vmovdqu (ea right) vl)
      (inst add left 32)
      (inst sub right 32)
      (inst cmp left right)
      (inst jmp :b LOOP)

      (inst vzeroupper)
      (inst add right 32)

      (inst mov l right)
      (inst sub l left)
      XMM
      (inst cmp l 32)
      (inst jmp :l WORD)

      (inst sub right 16)
      (inst vmovdqu vl-xmm (ea left))
      (inst vmovdqu vr-xmm (ea right))
      (inst vpshufd vl-xmm vl-xmm 27)
      (inst vpshufd vr-xmm vr-xmm 27)
      (inst vmovdqu (ea left) vr-xmm)
      (inst vmovdqu (ea right) vl-xmm)
      (inst add left 16)

      (inst mov l right)
      (inst sub l left)
      WORD
      (inst cmp l 16)
      (inst jmp :l SCALAR)

      (inst sub right 8)
      (inst mov l (ea left))
      (inst mov r (ea right))
      (inst ror l 32)
      (inst ror r 32)
      (inst mov (ea left) r)
      (inst mov (ea right) l)
      (inst add left 8)

      SCALAR
      (inst sub right 4)
      (inst cmp right left)
      (inst jmp :b DONE)

      (inst mov :dword l (ea left))
      (inst mov :dword r (ea right))
      (inst mov :dword (ea left) r)
      (inst mov :dword (ea right) l)
      DONE))
  result)

(defun simd-reverse8 (target source start length)
  (declare (optimize speed (safety 0)))
  (with-pinned-objects (target source)
    (inline-vop (((source sap-reg t) (vector-sap source))
                 ((target sap-reg t) (vector-sap target))
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
            (inst inc t-i)
            (inst sub s-i 1)
            (inst jmp :b DONE))
      DONE))
  target)

(def-variant simd-reverse8 :avx2 (target source start length)
  (declare (optimize speed (safety 0)))
  (with-pinned-objects (target source)
    (inline-vop (((source sap-reg t) (vector-sap source))
                 ((target sap-reg t) (vector-sap target))
                 ((start any-reg tagged-num) start)
                 ((length) length)
                 ((s-i signed-reg signed-num))
                 ((t-i))
                 ((g))
                 ((v int-avx2-reg))
                 ((reverse-mask))
                 ((reverse-mask-xmm int-sse-reg))
                 ((v-xmm)))
        ()
      (let ((reverse-mask-c (register-inline-constant :avx2 (concat-ub8 (loop for i below 32 collect i)))))
        (assemble ()
          (inst shr start 1)
          (inst add source start)
          (zeroize t-i)
          (inst mov s-i length)
          (inst shr s-i 1)
          (inst cmp s-i 32)
          (inst jmp :b XMM)
          (inst sub s-i 32)

          (inst vmovdqu reverse-mask reverse-mask-c)
          LOOP
          (inst vmovdqu v (ea source s-i))
          (inst vperm2i128 v v v 1)
          (inst vpshufb v v reverse-mask)
          (inst vmovdqu (ea target t-i) v)
          (inst add t-i 32)
          (inst sub s-i 32)
          (inst jmp :ge LOOP)
          (inst vzeroupper)
          (inst add s-i 32)

          XMM
          (inst cmp s-i 16)
          (inst jmp :b WORD)

          (inst sub s-i 16)
          (inst vmovdqu reverse-mask-xmm reverse-mask-c)
          (inst vmovdqu v-xmm (ea source s-i))
          (inst vpshufb v-xmm v-xmm reverse-mask-c)
          (inst vmovdqu (ea target t-i) v-xmm)
          (inst add t-i 16)

          WORD
          (inst cmp s-i 8)
          (inst jmp :b BYTE)

          (inst sub s-i 8)
          (inst mov g (ea source s-i))
          (inst bswap g)
          (inst mov (ea target t-i) g)
          (inst add t-i 8)

          BYTE
          (inst sub s-i 1)
          (inst jmp :b DONE)

          (loop repeat 7
                do
                (inst mov :byte g (ea source s-i))
                (inst mov :byte (ea target t-i) g)
                (inst inc t-i)
                (inst sub s-i 1)
                (inst jmp :b DONE))
          DONE))))
  target)

(defun simd-reverse32 (target source start length)
  (declare (optimize speed (safety 0)))
  (with-pinned-objects (target source)
    (inline-vop (((source sap-reg t) (vector-sap source))
                 ((target sap-reg t) (vector-sap target))
                 ((start any-reg tagged-num) start)
                 ((length) length)
                 ((s-i signed-reg signed-num))
                 ((t-i))
                 ((g)))
        ()
      (inst shl start 1)
      (inst add source start)
      (zeroize t-i)
      (inst mov s-i length)
      (inst shl s-i 1)
      (inst cmp s-i 8)
      (inst jmp :b SCALAR)
      (inst sub s-i 8)

      LOOP
      (inst mov g (ea source s-i))
      (inst ror g 32)
      (inst mov (ea target t-i) g)

      (inst add t-i 8)
      (inst sub s-i 8)
      (inst jmp :ge LOOP)
      (inst add s-i 8)

      SCALAR
      (inst sub s-i 4)
      (inst jmp :b DONE)

      (inst mov :dword g (ea source s-i))
      (inst mov :dword (ea target t-i) g)
      DONE))
  target)

(def-variant simd-reverse32 :avx2 (target source start length)
  (declare (optimize speed (safety 0)))
  (with-pinned-objects (source target)
    (inline-vop (((source sap-reg t) (vector-sap source))
                 ((target sap-reg t) (vector-sap target))
                 ((start any-reg tagged-num) start)
                 ((length) length)
                 ((s-i signed-reg signed-num))
                 ((t-i))
                 ((g))
                 ((v int-avx2-reg))
                 ((reverse-mask))
                 ((v-xmm int-sse-reg)))
        ()
      (inst shl start 1)
      (inst add source start)
      (zeroize t-i)
      (inst mov s-i length)
      (inst shl s-i 1)
      (inst cmp s-i 32)
      (inst jmp :b XMM)
      (inst sub s-i 32)

      (inst vmovdqu reverse-mask (register-inline-constant :avx2 (concat-ub32 (loop for i to 7 collect i))))
      LOOP
      (inst vmovdqu v (ea source s-i))
      (inst vpermd v reverse-mask v)
      (inst vmovdqu (ea target t-i) v)
      (inst add t-i 32)
      (inst sub s-i 32)
      (inst jmp :ge LOOP)
      (inst vzeroupper)
      (inst add s-i 32)

      XMM
      (inst cmp s-i 16)
      (inst jmp :b WORD)

      (inst sub s-i 16)
      (inst vmovdqu v-xmm (ea source s-i))
      (inst vpshufd v-xmm v-xmm 27)
      (inst vmovdqu (ea target t-i) v-xmm)
      (inst add t-i 16)

      WORD
      (inst cmp s-i 8)
      (inst jmp :b SCALAR)

      (inst sub s-i 8)
      (inst mov g (ea source s-i))
      (inst ror g 32)
      (inst mov (ea target t-i) g)
      (inst add t-i 8)

      SCALAR
      (inst sub s-i 4)
      (inst jmp :b DONE)

      (inst mov :dword g (ea source s-i))
      (inst mov :dword (ea target t-i) g)
      DONE))
  target)

(defun simd-cmp-8-32 (byte-array 32-bit-array length)
  (declare (optimize speed (safety 0)))
  (with-pinned-objects (byte-array 32-bit-array)
    (inline-vop (((byte-array sap-reg t) (vector-sap byte-array))
                 ((32-bit-array sap-reg t) (vector-sap 32-bit-array))
                 ((end))
                 ((length any-reg) length)
                 ((cmp unsigned-reg))
                 ((bytes int-sse-reg))
                 ((32-bits))
                 ((zero)))
        ((res descriptor-reg t :from :load))
      (load-symbol res t)
      (inst test length length)
      (inst jmp :z DONE)
      (inst pxor zero zero)
      (inst shr length 1)
      (inst lea end (ea length byte-array))

      LOOP
      (inst movd bytes (ea byte-array))
      (inst movdqa 32-bits (ea 32-bit-array))
      (inst add byte-array 4)
      (inst add 32-bit-array 16)
      ;; Interleaving with zero widens bytes to halfwords, and then
      ;; again to words.
      ;; PMOVZXBD from SSE4.1 doesn't seem to be faster.
      (inst punpcklbw bytes zero)
      (inst punpcklwd bytes zero)
      (inst pcmpeqd bytes 32-bits)
      (inst pmovmskb cmp bytes)
      (inst cmp :dword cmp #xFFFF)
      (inst jmp :ne FALSE)

      (inst cmp byte-array end)
      (inst jmp :b LOOP)
      (inst jmp DONE)

      FALSE
      (load-symbol res nil)
      DONE)))

(defun simd-copy-utf8-bytes-to-character-string (requested total-copied start string ibuf)
  (declare (type index start requested total-copied)
           (optimize speed (safety 0)))
  (with-pinned-objects (string)
    (let* ((head (sb-impl::buffer-head ibuf))
           (tail (sb-impl::buffer-tail ibuf))
           (left (- requested total-copied))
           (result-characters (logand left -16))
           (string-bytes (logand (- tail head) -16))
           (n (min result-characters string-bytes))
           (string-start (truly-the fixnum (* (+ start total-copied) 4)))
           (copied
             (inline-vop (((byte-array* sap-reg t) (sb-impl::buffer-sap ibuf))
                          ((byte-array sap-reg t))
                          ((32-bit-array sap-reg t) (vector-sap string))
                          ((ascii-mask int-sse-reg))
                          ((bytes int-sse-reg))
                          ((16-bits int-sse-reg))
                          ((32-bits int-sse-reg))
                          ((32-bits-2 int-sse-reg))
                          ((string-start unsigned-reg) string-start)
                          ((end unsigned-reg))
                          ((head unsigned-reg) head)
                          ((n unsigned-reg) n)
                          ((32-bits-4 int-sse-reg))
                          ((temp int-sse-reg))
                          ((zero)))
                 ((res unsigned-reg unsigned-num))
               (let ((16-bits-2 bytes)
                     (32-bits-3 bytes))
                (assemble ()
                  (inst movdqa ascii-mask (register-inline-constant :sse (concat-ub8 (loop for i below 16 collect 128))))

                  (inst add byte-array* head)
                  (inst mov byte-array byte-array*)
                  (inst lea end (ea byte-array* n))
                  (inst add 32-bit-array string-start)
                  (inst pxor zero zero)
                  (inst jmp start)


                  LOOP
                  (inst movdqu bytes (ea byte-array))
                  (move temp bytes)
                  (inst pand temp ascii-mask)
                  (inst pmovmskb head temp)
                  (inst test head head)
                  (inst jmp :nz done)
                  (inst add byte-array 16)
                  (move 16-bits bytes)
                  (inst punpcklbw 16-bits zero)
                  (move 32-bits-2 16-bits)


                  (move 32-bits 16-bits)
                  (inst punpcklwd 32-bits zero)

                  (inst movdqu (ea 32-bit-array) 32-bits)
                  (inst psrldq 32-bits-2 8)
                  (inst punpcklwd 32-bits-2 zero)

                  (inst movdqu (ea 16 32-bit-array) 32-bits-2)

                  (move 16-bits-2 bytes)

                  (inst psrldq 16-bits-2 8)
                  (inst punpcklbw 16-bits-2 zero)
                  (move 32-bits-4 16-bits-2)
                  (move 32-bits-3 16-bits-2)

                  (inst punpcklwd 32-bits-3 zero)

                  (inst movdqu (ea 32 32-bit-array) 32-bits-3)
                  (inst psrldq 32-bits-4 8)
                  (inst punpcklwd 32-bits-4 zero)

                  (inst movdqu (ea 48 32-bit-array) 32-bits-4)

                  (inst add 32-bit-array (* 16 4))


                  start
                  (inst cmp byte-array end)
                  (inst jmp :l LOOP)

                  DONE))

               (inst sub byte-array byte-array*)
               (move res byte-array))))
      (setf (sb-impl::buffer-head ibuf) (+ head copied))
      (incf total-copied copied))))
