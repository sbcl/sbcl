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
  (defun concat-ub (size ubs)
    (let ((result 0))
      (loop for ub in ubs
            do (setf result (logior (ash result size)
                                    (ldb (byte size 0) ub))))
      result))
  (defun reg-in-sc (tn sc)
    (make-random-tn (sc-or-lose sc) (tn-offset tn))))

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
      (let ((reverse-mask-c (register-inline-constant :avx2 (concat-ub 8 (loop for i below 32 collect i)))))
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

      (inst vmovdqu reverse-mask (register-inline-constant :avx2
                                                           (concat-ub 32 (loop for i to 7 collect i))))
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
      (let ((reverse-mask-c (register-inline-constant :avx2 (concat-ub 8 (loop for i below 32 collect i)))))
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

      (inst vmovdqu reverse-mask (register-inline-constant :avx2
                                                           (concat-ub 32 (loop for i to 7 collect i))))
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

#+sb-unicode
(defun utf8-to-character-string (start end string ibuf)
  (declare (type index start end)
           (optimize speed (safety 0)))
  (with-pinned-objects (string)
    (let* ((head (sb-impl::buffer-head ibuf))
           (tail (sb-impl::buffer-tail ibuf))
           (left (- end start))
           (result-characters (logand left -16))
           (string-bytes (logand (- tail head) -16))
           (n (min result-characters string-bytes))
           (string-start (truly-the fixnum (* start 4)))
           (copied
             (inline-vop (((byte-array* sap-reg t) (sb-impl::buffer-sap ibuf))
                          ((byte-array sap-reg t))
                          ((32-bit-array sap-reg t) (vector-sap string))
                          ((bytes int-sse-reg))
                          ((16-bits int-sse-reg))
                          ((32-bits int-sse-reg))
                          ((32-bits-2 int-sse-reg))
                          ((string-start unsigned-reg) string-start)
                          ((end unsigned-reg))
                          ((head unsigned-reg) head)
                          ((n unsigned-reg) n)
                          ((32-bits-4 int-sse-reg))
                          ((zero)))
                 ((res unsigned-reg unsigned-num))
               (let ((16-bits-2 bytes)
                     (32-bits-3 bytes))
                (assemble ()
                  (inst add byte-array* head)
                  (inst mov byte-array byte-array*)
                  (inst lea end (ea byte-array* n))
                  (inst add 32-bit-array string-start)
                  (inst pxor zero zero)
                  (inst jmp start)


                  LOOP
                  (inst movdqu bytes (ea byte-array))
                  (inst pmovmskb head bytes) ;; any high bit set? not ascii
                  (inst test :dword head head)
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

                  START
                  (inst cmp byte-array end)
                  (inst jmp :l LOOP)

                  DONE))

               (inst sub byte-array byte-array*)
               (move res byte-array))))
      (setf (sb-impl::buffer-head ibuf) (+ head copied))
      (+ start copied))))

#+sb-unicode
(defun ascii-sap-to-character-string (sap string length)
  (declare (optimize speed (safety 0))
           (system-area-pointer sap)
           (index length))
  (let ((n (logand length -16)))
    (with-pinned-objects (string)
      (inline-vop (((byte-array* sap-reg t) sap)
                   ((byte-array sap-reg t))
                   ((32-bit-array sap-reg t) (vector-sap string))
                   ((bytes int-sse-reg))
                   ((16-bits int-sse-reg))
                   ((32-bits int-sse-reg))
                   ((32-bits-2 int-sse-reg))
                   ((end unsigned-reg))
                   ((n unsigned-reg) n)
                   ((32-bits-4 int-sse-reg))
                   ((zero)))
          ((res unsigned-reg unsigned-num))
        (let ((16-bits-2 bytes)
              (32-bits-3 bytes))
          (assemble ()
            (inst mov byte-array byte-array*)
            (inst lea end (ea byte-array* n))
            (inst pxor zero zero)
            (inst jmp start)


            LOOP
            (inst movdqu bytes (ea byte-array))
            (inst add byte-array 16)

            (move 16-bits bytes)
            (inst punpcklbw 16-bits zero)
            (move 32-bits-2 16-bits)


            (move 32-bits 16-bits)
            (inst punpcklwd 32-bits zero)

            (inst movdqa (ea 32-bit-array) 32-bits)
            (inst psrldq 32-bits-2 8)
            (inst punpcklwd 32-bits-2 zero)

            (inst movdqa (ea 16 32-bit-array) 32-bits-2)

            (move 16-bits-2 bytes)

            (inst psrldq 16-bits-2 8)
            (inst punpcklbw 16-bits-2 zero)
            (move 32-bits-4 16-bits-2)
            (move 32-bits-3 16-bits-2)

            (inst punpcklwd 32-bits-3 zero)

            (inst movdqa (ea 32 32-bit-array) 32-bits-3)
            (inst psrldq 32-bits-4 8)
            (inst punpcklwd 32-bits-4 zero)

            (inst movdqa (ea 48 32-bit-array) 32-bits-4)

            (inst add 32-bit-array (* 16 4))

            START
            (inst cmp byte-array end)
            (inst jmp :l LOOP)

            DONE))

        (inst sub byte-array byte-array*)
        (move res byte-array)))
    (loop for i from n below length
          do (setf (aref string i)
                   (code-char (sap-ref-8 sap i))))))

(def-variant character-string-to-ascii-byte-array :avx2 (byte-array string length)
  (declare (index length)
           (simple-character-string string)
           ((simple-array (unsigned-byte 8) (*)) byte-array)
           (optimize speed (safety 0)))
  (with-pinned-objects (string byte-array)
    (inline-vop (((byte-array sap-reg t) (vector-sap byte-array))
                 ((32-bit-array sap-reg t) (vector-sap string))
                 ((n unsigned-reg) (logand (+ (* length 4) 15) -16))
                 ((bytes1 int-avx2-reg))
                 ((bytes2 int-avx2-reg)))
        ()
      (inst sub n 64)
      (inst jmp :b TAIL)

      LOOP
      (inst vmovdqu bytes1 (ea 32-bit-array))

      (inst vpackusdw bytes1 bytes1 (ea 32 32-bit-array))
      (inst vpermq bytes1 bytes1 216)
      (inst vpackuswb bytes1 bytes1 bytes1)
      (inst vpermq bytes1 bytes1 216)

      (inst add 32-bit-array 64)

      (inst vmovdqa (ea byte-array) (reg-in-sc bytes1 'int-sse-reg))
      (inst add byte-array 16)
      (inst sub n 64)
      (inst jmp :ae LOOP)

      TAIL
      (inst add :dword n 64)
      (inst jmp :z DONE)
      (inst vpxor bytes2 bytes2 bytes2)

      (inst cmp :dword n 32)
      (inst jmp :l ONE)
      (inst vmovdqu bytes1 (ea 32-bit-array))
      (inst jmp :e NARROW)
      (inst vmovdqa (reg-in-sc bytes2 'int-sse-reg) (ea 32 32-bit-array))
      (inst jmp NARROW)
      ONE
      (inst vmovdqa (reg-in-sc bytes1 'int-sse-reg) (ea 32-bit-array))

      NARROW
      (inst vpackusdw bytes1 bytes1 bytes2)
      (inst vpermq bytes1 bytes1 216)
      (inst vpackuswb bytes1 bytes1 bytes1)
      (inst vpermq bytes1 bytes1 216)
      (inst vmovdqa (ea byte-array) (reg-in-sc bytes1 'int-sse-reg))

      DONE
      (inst vzeroupper))))

#+sb-unicode
(defun utf8-to-base-string (start end string ibuf)
  (declare (type index start end)
           (optimize speed (safety 0)))
  (with-pinned-objects (string)
    (let* ((head (sb-impl::buffer-head ibuf))
           (tail (sb-impl::buffer-tail ibuf))
           (n (logand (min (- end start)
                           (- tail head))
                      -16))
           (copied
             (inline-vop (((byte-array* sap-reg t) (sb-impl::buffer-sap ibuf))
                          ((byte-array sap-reg t))
                          ((32-bit-array sap-reg t) (vector-sap string))
                          ((bytes int-sse-reg))
                          ((string-start unsigned-reg) start)
                          ((end unsigned-reg))
                          ((head unsigned-reg) head)
                          ((n unsigned-reg) n))
                 ((res unsigned-reg unsigned-num))
               (inst add byte-array* head)
               (inst mov byte-array byte-array*)
               (inst lea end (ea byte-array* n))
               (inst add 32-bit-array string-start)
               (inst jmp start)

               LOOP
               (inst movdqu bytes (ea byte-array))
               (inst pmovmskb head bytes) ;; any high bit set? not ascii
               (inst test :dword head head)
               (inst jmp :nz done)
               (inst movdqu (ea 32-bit-array) bytes)
               (inst add byte-array 16)
               (inst add 32-bit-array 16)

               START
               (inst cmp byte-array end)
               (inst jmp :l LOOP)

               DONE

               (inst sub byte-array byte-array*)
               (move res byte-array))))
      (setf (sb-impl::buffer-head ibuf) (+ head copied))
      (+ start copied))))

#+sb-unicode
(def-variant utf8-crlf-to-base-string :ssse3+popcnt (start end string ibuf)
  (declare (type index start end)
           (optimize speed (safety 0)))
  (let* ((head (sb-impl::buffer-head ibuf))
         (tail (sb-impl::buffer-tail ibuf))
         (n (logand (min (- end start)
                         (- (- tail head) 16)) ;; read one more chunk
                    (- 16)))
         (shuffle-table (load-time-value (let ((table (make-array (* 256 8) :element-type '(unsigned-byte 8))))
                                           (loop for row below 256
                                                 do (loop with indexes = (loop for i below 8
                                                                               unless (logbitp i row)
                                                                               collect i)
                                                          for column below 8
                                                          for index = (or (pop indexes)
                                                                          0)
                                                          do
                                                          (setf (aref table (+ (* row 8) column))
                                                                index)))
                                           table))))

    (if (<= n 0)
        start
        (with-pinned-objects (string)
          (multiple-value-bind (new-head copied)
              (inline-vop (((byte-array* sap-reg t) (sb-impl::buffer-sap ibuf))
                           ((byte-array sap-reg t))
                           ((char-array* sap-reg t) (vector-sap string))
                           ((char-array sap-reg t))
                           ((crlf-mask complex-double-reg))
                           ((bytes complex-double-reg))
                           ((next-bytes complex-double-reg))
                           ((shifted complex-double-reg))
                           ((temp complex-double-reg))
                           ((string-start unsigned-reg) start)
                           ((end unsigned-reg) n)
                           ((head unsigned-reg) head)
                           ((shuffle-table sap-reg) (vector-sap shuffle-table))
                           ((shuffle-mask complex-double-reg))
                           ((shuffle-mask2 complex-double-reg)))
                  ((new-head unsigned-reg positive-fixnum :from :load)
                   (copied unsigned-reg positive-fixnum :from :load))
                (inst movdqa crlf-mask (register-inline-constant :sse (concat-ub 8 (loop for i below 8
                                                                                        collect #x0A
                                                                                        collect #x0D))))
                (inst lea byte-array (ea head byte-array*))
                (inst add end byte-array)

                (inst add char-array* string-start)
                (inst mov char-array char-array*)


                LOOP
                (inst movdqu bytes (ea byte-array))
                ;; Shift bytes right to find CRLF starting at odd indexes
                ;; grab the first byte from the next vector to check if it
                ;; it's an LF
                (inst movdqu shifted (ea 1 byte-array))

                (inst pmovmskb head bytes) ;; any high bit set? not ascii
                (inst test :dword head head)
                (inst jmp :nz done)


                ;; Compare both variants
                (move temp bytes)
                (inst pcmpeqw temp crlf-mask)
                (inst pcmpeqw shifted crlf-mask)
                ;; pcmpeqw will have FFFF, shifting in different directions and then combining
                ;; will have FF in the right places for CR in the original chunk.
                (inst psrlw temp 8)
                (inst psllw shifted 8)
                (inst por temp shifted)

                ;; Get a 16-bit mask
                (inst pmovmskb copied temp)

                ;; Split into two 8-bit parts
                (inst mov :byte head copied)
                (inst shr :dword copied 8)

                (move next-bytes bytes)
                (inst punpckhqdq next-bytes next-bytes)
                (inst movq shuffle-mask (ea shuffle-table head 8))
                (inst movq shuffle-mask2 (ea shuffle-table copied 8))
                (inst pshufb bytes shuffle-mask)
                (inst pshufb next-bytes shuffle-mask2)

                (inst movq (ea char-array) bytes)
                (inst add char-array 8)
                (inst popcnt :dword head head)
                (inst sub char-array head)

                (inst movq (ea char-array) next-bytes)
                (inst add char-array 8)
                (inst popcnt :dword copied copied)
                (inst sub char-array copied)
                (inst add byte-array 16)

                (inst cmp byte-array end)
                (inst jmp :l LOOP)

                DONE
                (inst mov copied char-array)
                (inst sub copied char-array*)
                (inst mov new-head byte-array)
                (inst sub new-head byte-array*))
            (setf (sb-impl::buffer-head ibuf) new-head)
            (truly-the index (+ start copied)))))))

#+sb-unicode
(def-variant utf8-crlf-to-character-string :ssse3+popcnt (start end string ibuf)
  (declare (type index start end)
           (optimize speed (safety 0)))
  (let* ((head (sb-impl::buffer-head ibuf))
         (tail (sb-impl::buffer-tail ibuf))
         (n (logand (min (- end start)
                         (- (- tail head) 16)) ;; read one more chunk
                    (- 16)))
         (string-start (* start 4))
         (shuffle-table (load-time-value (let ((table (make-array (* 256 8) :element-type '(unsigned-byte 8))))
                                           (loop for row below 256
                                                 do (loop with indexes = (loop for i below 8
                                                                               unless (logbitp i row)
                                                                               collect i)
                                                          for column below 8
                                                          for index = (or (pop indexes)
                                                                          0)
                                                          do
                                                          (setf (aref table (+ (* row 8) column))
                                                                index)))
                                           table))))
    (if (<= n 0)
        start
        (with-pinned-objects (string)
          (multiple-value-bind (new-head copied)
              (inline-vop (((byte-array* sap-reg t) (sb-impl::buffer-sap ibuf))
                           ((byte-array sap-reg t))
                           ((char-array* sap-reg t) (vector-sap string))
                           ((char-array sap-reg t))
                           ((crlf-mask complex-double-reg))
                           ((bytes complex-double-reg))
                           ((next-bytes complex-double-reg))
                           ((shifted complex-double-reg))
                           ((temp complex-double-reg))
                           ((string-start unsigned-reg) string-start)
                           ((end unsigned-reg) n)
                           ((head unsigned-reg) head)
                           ((shuffle-table sap-reg) (vector-sap shuffle-table))
                           ((shuffle-mask complex-double-reg))
                           ((shuffle-mask2 complex-double-reg))
                           ((32-bits-2 int-sse-reg))
                           ((zero int-sse-reg)))
                  ((new-head unsigned-reg positive-fixnum :from :load)
                   (copied unsigned-reg positive-fixnum :from :load))
                (inst movdqa crlf-mask (register-inline-constant :sse
                                                                 (concat-ub 8 (loop for i below 8
                                                                                    collect #x0A
                                                                                    collect #x0D))))
                (inst pxor zero zero)
                (inst lea byte-array (ea head byte-array*))
                (inst add end byte-array)

                (inst add char-array* string-start)
                (inst mov char-array char-array*)


                LOOP
                (inst movdqu bytes (ea byte-array))
                ;; Shift bytes right to find CRLF starting at odd indexes
                ;; grab the first byte from the next vector to check if it
                ;; it's an LF
                (inst movdqu shifted (ea 1 byte-array))

                (inst pmovmskb head bytes) ;; any high bit set? not ascii
                (inst test :dword head head)
                (inst jmp :nz done)


                ;; Compare both variants
                (move temp bytes)
                (inst pcmpeqw temp crlf-mask)
                (inst pcmpeqw shifted crlf-mask)
                ;; pcmpeqw will have FFFF, shifting in different directions and then combining
                ;; will have FF in the right places for CR in the original chunk.
                (inst psrlw temp 8)
                (inst psllw shifted 8)
                (inst por temp shifted)

                ;; Get a 16-bit mask
                (inst pmovmskb copied temp)

                ;; Split into two 8-bit parts
                (inst mov :byte head copied)
                (inst shr :dword copied 8)

                (move next-bytes bytes)
                (inst punpckhqdq next-bytes next-bytes)
                (inst movq shuffle-mask (ea shuffle-table head 8))
                (inst movq shuffle-mask2 (ea shuffle-table copied 8))
                (inst pshufb bytes shuffle-mask)
                (inst pshufb next-bytes shuffle-mask2)

                ;; Widen
                (inst punpcklbw bytes zero)
                (move 32-bits-2 bytes)

                (inst punpcklwd bytes zero)

                (inst psrldq 32-bits-2 8)
                (inst punpcklwd 32-bits-2 zero)

                (inst movdqu (ea char-array) bytes)
                (inst movdqu (ea 16 char-array) 32-bits-2)

                (inst add char-array 32)
                (inst popcnt :dword head head)
                (inst shl head 2)
                (inst sub char-array head)

                (inst punpcklbw next-bytes zero)
                (move 32-bits-2 next-bytes)

                (inst punpcklwd next-bytes zero)

                (inst psrldq 32-bits-2 8)
                (inst punpcklwd 32-bits-2 zero)

                (inst movdqu (ea char-array) next-bytes)
                (inst movdqu (ea 16 char-array) 32-bits-2)

                (inst add char-array 32)
                (inst popcnt :dword copied copied)
                (inst shl copied 2)
                (inst sub char-array copied)

                (inst add byte-array 16)

                (inst cmp byte-array end)
                (inst jmp :l LOOP)

                DONE
                (inst mov copied char-array)
                (inst sub copied char-array*)
                (inst mov new-head byte-array)
                (inst sub new-head byte-array*))
            (setf (sb-impl::buffer-head ibuf) new-head)
            (truly-the index (+ start (truncate copied 4))))))))

(defun character-string-to-utf8 (start end string obuf)
  (declare (type index start end)
           (optimize speed (safety 0)))
  (with-pinned-objects (string)
    (let* ((tail (sb-impl::buffer-tail obuf))
           (buffer-left (- (sb-impl::buffer-length obuf) tail))
           (string-left (- end start))
           (n (logand (min buffer-left string-left) -16))
           (string-start (truly-the fixnum (* start 4))))
      (multiple-value-bind (copied last-newline)
          (inline-vop (((byte-array* sap-reg t) (sb-impl::buffer-sap obuf))
                       ((32-bit-array sap-reg t) (vector-sap string))
                       ((string-start unsigned-reg) string-start)
                       ((end unsigned-reg) n)
                       ((tail unsigned-reg) tail)
                       ((ascii-mask complex-double-reg))
                       ((newlines complex-double-reg))
                       ((bytes1 complex-double-reg))
                       ((bytes2 complex-double-reg))
                       ((bytes3 complex-double-reg))
                       ((bytes4 complex-double-reg))
                       ((temp1))
                       ((temp complex-double-reg))
                       ((indexes))
                       ((increment))
                       ((last-newlines)))
              ((byte-array unsigned-reg unsigned-num :from :load)
               (last-newline signed-reg signed-num))

            (inst movdqa ascii-mask (register-inline-constant :sse
                                                              (concat-ub 32 (loop repeat 4
                                                                                  collect 127))))
            (inst movdqa newlines (register-inline-constant :sse
                                                            (concat-ub 32 (loop repeat 4
                                                                                collect 10))))
            (inst movdqa increment (register-inline-constant :sse
                                                             (concat-ub 32 (loop repeat 4
                                                                                 collect 4))))
            (inst movdqa indexes (register-inline-constant :sse
                                                           (concat-ub 32 '(3 2 1 0))))
            (inst pcmpeqb last-newlines last-newlines) ;; #xFF....

            (inst add byte-array* tail)
            (move byte-array byte-array*)
            (inst add end byte-array*)
            (inst add 32-bit-array string-start)
            (inst jmp start)

            LOOP
            (inst movdqu bytes1 (ea 32-bit-array))
            (inst movdqu bytes2 (ea 16 32-bit-array))
            (inst movdqu bytes3 (ea 32 32-bit-array))
            (inst movdqu bytes4 (ea 48 32-bit-array))

            (inst movdqa temp bytes1)
            (inst por temp bytes2)
            (inst por temp bytes3)
            (inst por temp bytes4)
            (inst pcmpgtd temp ascii-mask)
            (inst pmovmskb tail temp)
            (inst test :dword tail tail)
            (inst jmp :nz done)

            (loop for bytes in (list bytes1 bytes2 bytes3 bytes4)
                  do
                  (inst movdqa temp bytes)
                  (inst pcmpeqd temp newlines)
                  (inst movdqa temp1 indexes)
                  (inst pand temp1 temp)
                  (inst pandn temp last-newlines)
                  (inst movdqa last-newlines temp)
                  (inst por last-newlines temp1)
                  (inst paddd indexes increment))

            (inst add 32-bit-array 64)

            (inst movdqa    temp1 bytes1)
            (inst punpcklwd bytes1 bytes2)
            (inst punpckhwd temp1 bytes2)

            (inst movdqa    bytes2 bytes1)
            (inst punpckhwd bytes2 temp1)
            (inst punpcklwd bytes1 temp1)
            (inst punpcklwd bytes1 bytes2)

            (inst movdqa    bytes2 bytes3)
            (inst punpcklwd bytes3 bytes4)
            (inst punpckhwd bytes2 bytes4)
            (inst movdqa    bytes4 bytes3)
            (inst punpckhwd bytes4 bytes2)
            (inst punpcklwd bytes3 bytes2)
            (inst punpcklwd bytes3 bytes4)
            (inst packuswb bytes1 bytes3)

            (inst movdqu (ea byte-array) bytes1)
            (inst add byte-array 16)

            start
            (inst cmp byte-array end)
            (inst jmp :l LOOP)

            DONE

            ;; Find the max last-newline...
            (progn
              (inst movdqa  temp last-newlines)
              (inst psrldq  temp 8)
              (inst movdqa  temp1 temp)
              (inst pcmpgtd temp1 last-newlines)
              (inst pand    temp temp1)
              (inst pandn   temp1 last-newlines)
              (inst por     temp1 temp)
              (inst movdqa  last-newlines temp1)
              (inst psrldq  last-newlines 4)
              (inst movdqa  temp last-newlines)
              (inst pcmpgtd temp temp1)
              (inst pand    last-newlines temp)
              (inst pandn   temp temp1)
              (inst por     temp last-newlines)
              (inst movd    last-newline temp)
              (inst movsx '(:dword :qword) last-newline last-newline))
            (inst sub byte-array byte-array*))
        (setf (sb-impl::buffer-tail obuf) (+ tail copied))
        (values (+ start copied)
                (if (>= last-newline 0)
                    (truly-the index (+ start last-newline))
                    -1))))))

(def-variant character-string-to-utf8 :avx2 (start end string obuf)
  (declare (type index start end)
           (optimize speed (safety 0)))
  (with-pinned-objects (string)
    (let* ((tail (sb-impl::buffer-tail obuf))
           (buffer-left (- (sb-impl::buffer-length obuf) tail))
           (string-left (- end start))
           (n (logand (min buffer-left string-left) -16))
           (string-start (truly-the fixnum (* start 4))))
      (multiple-value-bind (copied last-newline)
          (inline-vop (((byte-array* sap-reg t) (sb-impl::buffer-sap obuf))
                       ((32-bit-array sap-reg t) (vector-sap string))
                       ((string-start unsigned-reg) string-start)
                       ((end unsigned-reg) n)
                       ((tail unsigned-reg) tail)
                       ((ascii-mask int-avx2-reg))
                       ((newlines int-avx2-reg))
                       ((bytes1 int-avx2-reg))
                       ((bytes2 int-avx2-reg))
                       ((temp int-avx2-reg))
                       ((indexes))
                       ((increment))
                       ((last-newlines)))
              ((byte-array unsigned-reg unsigned-num :from :load)
               (last-newline signed-reg signed-num))
            (inst vmovdqu ascii-mask (register-inline-constant :avx2
                                                               (concat-ub 32 (loop repeat 8
                                                                                   collect (ldb (byte 32 0) (lognot 127))))))
            (inst vmovdqu newlines (register-inline-constant :avx2
                                                            (concat-ub 32 (loop repeat 8
                                                                                collect 10))))
            (inst vmovdqu increment (register-inline-constant :avx2
                                                             (concat-ub 32 (loop repeat 8
                                                                                 collect 8))))
            (inst vmovdqu indexes (register-inline-constant :avx2
                                                           (concat-ub 32 '(7 6 5 4 3 2 1 0))))
            (inst vpcmpeqb last-newlines last-newlines last-newlines) ;; #xFF....

            (inst add byte-array* tail)
            (move byte-array byte-array*)
            (inst add end byte-array*)
            (inst add 32-bit-array string-start)

            (inst jmp start)

            LOOP
            (inst vmovdqu bytes1 (ea 32-bit-array))
            (inst vmovdqu bytes2 (ea 32 32-bit-array))

            (inst vpor temp bytes1 bytes2)
            (inst vptest temp ascii-mask)
            (inst jmp :nz done)

            (loop for bytes in (list bytes1 bytes2)
                  do
                  (inst vpcmpeqd temp bytes newlines)
                  (inst vpblendvb last-newlines last-newlines indexes temp)
                  (inst vpaddd indexes indexes increment))

            (inst vpackusdw bytes1 bytes1 bytes2)
            (inst vpermq bytes1 bytes1 216)
            (inst vpackuswb bytes1 bytes1 bytes1)
            (inst vpermq bytes1 bytes1 216)

            (inst add 32-bit-array 64)

            (inst vmovdqu (ea byte-array) (reg-in-sc bytes1 'int-sse-reg))
            (inst add byte-array 16)

            start
            (inst cmp byte-array end)
            (inst jmp :l LOOP)

            DONE
            (let ((xlast-newlines (reg-in-sc last-newlines 'int-sse-reg))
                  (temp (reg-in-sc temp 'int-sse-reg)))
              (inst vextracti128 temp last-newlines 1)
              (inst vzeroupper)
              (inst vpmaxsd xlast-newlines temp xlast-newlines)
              (inst vpsrldq temp xlast-newlines 8)
              (inst vpmaxsd xlast-newlines xlast-newlines temp)
              (inst vpsrldq temp xlast-newlines 4)
              (inst vpmaxsd xlast-newlines xlast-newlines temp)
              (inst vmovd  last-newline xlast-newlines))

            (inst movsx '(:dword :qword) last-newline last-newline)
            (inst sub byte-array byte-array*))
        (setf (sb-impl::buffer-tail obuf) (+ tail copied))
        (values (+ start copied)
                (if (>= last-newline 0)
                    (truly-the index (+ start last-newline))
                    -1))))))

(defun simd-position8 (element vector start end)
  (declare (type index start end)
           (optimize speed (safety 0)))
  (with-pinned-objects (vector)
    (inline-vop (((byte-array* sap-reg t) (vector-sap vector))
                 ((start unsigned-reg) start)
                 ((end unsigned-reg) end)
                 ((element unsigned-reg) element)
                 ((left))
                 ((byte-array sap-reg t))
                 ((bytes complex-double-reg))
                 ((search)))
        ((res descriptor-reg t :from :load))
      (inst mov res null-tn)

      (inst lea byte-array (ea start byte-array*))
      (inst add end byte-array*)

      (move left end)
      (inst sub left byte-array)

      (inst cmp left 16)
      (inst jmp :l SCALAR)

      (inst movd search element)
      (inst punpcklbw search search)
      (inst punpcklwd search search)
      (inst punpckldq search search)
      (inst punpcklqdq search search)

      LOOP
      (inst movdqu bytes (ea byte-array))
      (inst pcmpeqb bytes search)
      (inst pmovmskb left bytes)
      (inst test :dword left left)
      (inst jmp :nz FOUND)

      (inst add byte-array 16)
      (move left end)
      (inst sub left byte-array)
      (inst cmp left 16)
      (inst jmp :ge LOOP)


      SCALAR
      (loop repeat 15
            do (inst cmp byte-array end)
               (inst jmp :ge DONE)
               (inst cmp :byte element (ea byte-array))
               (inst jmp :e FOUND-SCALAR)
               (inst inc byte-array))
      (inst jmp DONE)

      FOUND
      (inst bsf :dword left left)
      (inst add byte-array left)

      FOUND-SCALAR
      (inst sub byte-array byte-array*)
      (move res byte-array)
      (inst shl res n-fixnum-tag-bits)
      DONE)))

(def-variant simd-position8 :avx2 (element vector start end)
  (declare (type index start end)
           (optimize speed (safety 0)))
  (with-pinned-objects (vector)
    (inline-vop (((byte-array* sap-reg t) (vector-sap vector))
                 ((start unsigned-reg) start)
                 ((end unsigned-reg) end)
                 ((element unsigned-reg) element)
                 ((left))
                 ((byte-array sap-reg t))
                 ((bytes int-avx2-reg))
                 ((search)))
        ((res descriptor-reg t :from :load))
      (inst mov res null-tn)

      (inst lea byte-array (ea start byte-array*))
      (inst add end byte-array*)

      (move left end)
      (inst sub left byte-array)

      (inst cmp left 32)
      (inst jmp :l SSE)

      (inst vmovd search element)
      (inst vpbroadcastb search search)

      LOOP
      (inst vpcmpeqb bytes search (ea byte-array))
      (inst vpmovmskb left bytes)
      (inst test :dword left left)
      (inst jmp :nz FOUND)

      (inst add byte-array 32)
      (move left end)
      (inst sub left byte-array)
      (inst cmp left 32)
      (inst jmp :ge LOOP)
      (inst vzeroupper)

      SSE
      (inst cmp left 16)
      (inst jmp :l SCALAR)

      (let ((bytes (reg-in-sc bytes 'complex-double-reg))
            (search (reg-in-sc search 'complex-double-reg)))
        (inst vmovd search element)
        (inst vpbroadcastb search search)
        (inst vpcmpeqb bytes search (ea byte-array))
        (inst vpmovmskb left bytes)
        (inst test :dword left left)
        (inst jmp :nz FOUND)

        (inst add byte-array 16))

      SCALAR
      (loop repeat 15
            do (inst cmp byte-array end)
               (inst jmp :ge DONE)
               (inst cmp :byte element (ea byte-array))
               (inst jmp :e FOUND-SCALAR)
               (inst inc byte-array))
      (inst jmp DONE)

      FOUND
      (inst vzeroupper)
      (inst bsf :dword left left)
      (inst add byte-array left)

      FOUND-SCALAR
      (inst sub byte-array byte-array*)
      (move res byte-array)
      (inst shl res n-fixnum-tag-bits)
      DONE)))

(defun simd-position8-from-end (element vector start end)
  (declare (type index start end)
           (optimize speed (safety 0)))
  (with-pinned-objects (vector)
    (inline-vop (((byte-array* sap-reg t) (vector-sap vector))
                 ((start unsigned-reg) start)
                 ((end unsigned-reg) end)
                 ((element unsigned-reg) element)
                 ((left))
                 ((byte-array sap-reg t))
                 ((bytes complex-double-reg))
                 ((search)))
        ((res descriptor-reg t :from :load))
      (inst mov res null-tn)

      (inst lea byte-array (ea end byte-array*))
      (inst add start byte-array*)

      (move left byte-array)
      (inst sub left start)

      (inst cmp left 16)
      (inst jmp :l SCALAR)

      (inst movd search element)
      (inst punpcklbw search search)
      (inst punpcklwd search search)
      (inst punpckldq search search)
      (inst punpcklqdq search search)

      LOOP
      (inst sub byte-array 16)
      (inst movdqu bytes (ea byte-array))
      (inst pcmpeqb bytes search)
      (inst pmovmskb left bytes)
      (inst test :dword left left)
      (inst jmp :nz FOUND)

      (move left byte-array)
      (inst sub left start)
      (inst cmp left 16)
      (inst jmp :ge LOOP)


      SCALAR
      (loop repeat 15
            do (inst cmp byte-array start)
               (inst jmp :le DONE)
               (inst dec byte-array)
               (inst cmp :byte element (ea byte-array))
               (inst jmp :e FOUND-SCALAR))
      (inst jmp DONE)

      FOUND
      (inst bsr :dword left left)
      (inst add byte-array left)

      FOUND-SCALAR
      (inst sub byte-array byte-array*)
      (move res byte-array)
      (inst shl res n-fixnum-tag-bits)
      DONE)))

(def-variant simd-position8-from-end :avx2 (element vector start end)
  (declare (type index start end)
           (optimize speed (safety 0)))
  (with-pinned-objects (vector)
    (inline-vop (((byte-array* sap-reg t) (vector-sap vector))
                 ((start unsigned-reg) start)
                 ((end unsigned-reg) end)
                 ((element unsigned-reg) element)
                 ((left))
                 ((byte-array sap-reg t))
                 ((bytes int-avx2-reg))
                 ((search)))
        ((res descriptor-reg t :from :load))
      (inst mov res null-tn)

      (inst lea byte-array (ea end byte-array*))
      (inst add start byte-array*)

      (move left byte-array)
      (inst sub left start)

      (inst cmp left 32)
      (inst jmp :l SSE)

      (inst vmovd search element)
      (inst vpbroadcastb search search)


      LOOP
      (inst sub byte-array 32)
      (inst vpcmpeqb bytes search (ea byte-array))
      (inst vpmovmskb left bytes)
      (inst test :dword left left)
      (inst jmp :nz FOUND)

      (move left byte-array)
      (inst sub left start)
      (inst cmp left 32)
      (inst jmp :ge LOOP)
      (inst vzeroupper)

      SSE
      (inst cmp left 16)
      (inst jmp :l SCALAR)

      (let ((bytes (reg-in-sc bytes 'complex-double-reg))
            (search (reg-in-sc search 'complex-double-reg)))
        (inst vmovd search element)
        (inst vpbroadcastb search search)
        (inst sub byte-array 16)
        (inst vpcmpeqb bytes search (ea byte-array))
        (inst vpmovmskb left bytes)
        (inst test :dword left left)
        (inst jmp :nz FOUND))

      SCALAR
      (loop repeat 15
            do (inst cmp byte-array start)
               (inst jmp :le DONE)
               (inst dec byte-array)
               (inst cmp :byte element (ea byte-array))
               (inst jmp :e FOUND-SCALAR))
      (inst jmp DONE)

      FOUND
      (inst vzeroupper)
      (inst bsr :dword left left)
      (inst add byte-array left)

      FOUND-SCALAR
      (inst sub byte-array byte-array*)
      (move res byte-array)
      (inst shl res n-fixnum-tag-bits)
      DONE)))


(defun simd-position32 (element vector start end)
  (declare (type index start end)
           (optimize speed (safety 0)))
  (with-pinned-objects (vector)
    (inline-vop (((32-bit-array* sap-reg t) (vector-sap vector))
                 ((start any-reg) start)
                 ((end any-reg) end)
                 ((element unsigned-reg) element)
                 ((left))
                 ((32-bit-array sap-reg t))
                 ((bytes complex-double-reg))
                 ((search)))
        ((res descriptor-reg t :from :load))
      (inst mov res null-tn)
      (inst shl end 1)
      (inst lea 32-bit-array (ea 32-bit-array* start 2))

      (inst add end 32-bit-array*)

      (move left end)
      (inst sub left 32-bit-array)

      (inst cmp left 16)
      (inst jmp :l SCALAR)

      (inst movd search element)
      (inst pshufd search search 0)

      LOOP
      (inst movdqu bytes (ea 32-bit-array))
      (inst pcmpeqd bytes search)
      (inst movmskps left bytes)
      (inst test :dword left left)
      (inst jmp :nz FOUND)

      (inst add 32-bit-array 16)
      (move left end)
      (inst sub left 32-bit-array)
      (inst cmp left 16)
      (inst jmp :ge LOOP)


      SCALAR
      (loop repeat 15
            do (inst cmp 32-bit-array end)
               (inst jmp :ge DONE)
               (inst cmp :dword element (ea 32-bit-array))
               (inst jmp :e FOUND-SCALAR)
               (inst add 32-bit-array 4))
      (inst jmp DONE)

      FOUND
      (inst bsf :dword left left)
      (inst shl left 2)
      (inst add 32-bit-array left)

      FOUND-SCALAR
      (inst sub 32-bit-array 32-bit-array*)
      (move res 32-bit-array)
      (inst shr res 1)
      DONE)))

(def-variant simd-position32 :avx2 (element vector start end)
  (declare (type index start end)
           (optimize speed (safety 0)))
  (with-pinned-objects (vector)
    (inline-vop (((32-bit-array* sap-reg t) (vector-sap vector))
                 ((start any-reg) start)
                 ((end any-reg) end)
                 ((element unsigned-reg) element)
                 ((left))
                 ((32-bit-array sap-reg t))
                 ((bytes int-avx2-reg))
                 ((search)))
        ((res descriptor-reg t :from :load))
      (inst mov res null-tn)
      (inst shl end 1)
      (inst lea 32-bit-array (ea 32-bit-array* start 2))

      (inst add end 32-bit-array*)

      (move left end)
      (inst sub left 32-bit-array)

      (inst cmp left 32)
      (inst jmp :l SSE)

      (inst vmovd search element)
      (inst vpbroadcastd search search)

      LOOP
      (inst vpcmpeqd bytes search (ea 32-bit-array))
      (inst vmovmskps left bytes)
      (inst test :dword left left)
      (inst jmp :nz FOUND)

      (inst add 32-bit-array 32)
      (move left end)
      (inst sub left 32-bit-array)
      (inst cmp left 32)
      (inst jmp :ge LOOP)
      (inst vzeroupper)

      SSE
      (inst cmp left 16)
      (inst jmp :l SCALAR)

      (let ((bytes (reg-in-sc bytes 'complex-double-reg))
            (search (reg-in-sc search 'complex-double-reg)))
        (inst vmovd search element)
        (inst vpbroadcastd search search)
        (inst vpcmpeqd bytes search (ea 32-bit-array))
        (inst vmovmskps left bytes)
        (inst test :dword left left)
        (inst jmp :nz FOUND)

        (inst add 32-bit-array 16))

      SCALAR
      (loop repeat 15
            do (inst cmp 32-bit-array end)
               (inst jmp :ge DONE)
               (inst cmp :dword element (ea 32-bit-array))
               (inst jmp :e FOUND-SCALAR)
               (inst add 32-bit-array 4))
      (inst jmp DONE)

      FOUND
      (inst bsf :dword left left)
      (inst shl left 2)
      (inst add 32-bit-array left)

      FOUND-SCALAR
      (inst sub 32-bit-array 32-bit-array*)
      (move res 32-bit-array)
      (inst shr res 1)
      DONE)))

(defun simd-position32-from-end (element vector start end)
  (declare (type index start end)
           (optimize speed (safety 0)))
  (with-pinned-objects (vector)
    (inline-vop (((32-bit-array* sap-reg t) (vector-sap vector))
                 ((start any-reg) start)
                 ((end any-reg) end)
                 ((element unsigned-reg) element)
                 ((left))
                 ((32-bit-array sap-reg t))
                 ((bytes complex-double-reg))
                 ((search)))
        ((res descriptor-reg t :from :load))
      (inst mov res null-tn)

      (inst shl start 1)
      (inst lea 32-bit-array (ea 32-bit-array* end 2))
      (inst add start 32-bit-array*)

      (move left 32-bit-array)
      (inst sub left start)

      (inst cmp left 16)
      (inst jmp :l SCALAR)

      (inst movd search element)
      (inst pshufd search search 0)

      LOOP
      (inst sub 32-bit-array 16)
      (inst movdqu bytes (ea 32-bit-array))
      (inst pcmpeqd bytes search)
      (inst movmskps left bytes)
      (inst test :dword left left)
      (inst jmp :nz FOUND)

      (move left 32-bit-array)
      (inst sub left start)
      (inst cmp left 16)
      (inst jmp :ge LOOP)


      SCALAR
      (loop repeat 15
            do (inst cmp 32-bit-array start)
               (inst jmp :le DONE)
               (inst sub 32-bit-array 4)
               (inst cmp :byte element (ea 32-bit-array))
               (inst jmp :e FOUND-SCALAR))
      (inst jmp DONE)

      FOUND
      (inst bsr :dword left left)
      (inst shl left 2)
      (inst add 32-bit-array left)

      FOUND-SCALAR
      (inst sub 32-bit-array 32-bit-array*)
      (move res 32-bit-array)
      (inst shr res 1)
      DONE)))

(def-variant simd-position32-from-end :avx2 (element vector start end)
  (declare (type index start end)
           (optimize speed (safety 0)))
  (with-pinned-objects (vector)
    (inline-vop (((32-bit-array* sap-reg t) (vector-sap vector))
                 ((start any-reg) start)
                 ((end any-reg) end)
                 ((element unsigned-reg) element)
                 ((left))
                 ((32-bit-array sap-reg t))
                 ((bytes int-avx2-reg))
                 ((search)))
        ((res descriptor-reg t :from :load))
      (inst mov res null-tn)

      (inst shl start 1)
      (inst lea 32-bit-array (ea 32-bit-array* end 2))
      (inst add start 32-bit-array*)

      (move left 32-bit-array)
      (inst sub left start)

      (inst cmp left 32)
      (inst jmp :l SSE)

      (inst vmovd search element)
      (inst vpbroadcastd search search)


      LOOP
      (inst sub 32-bit-array 32)
      (inst vpcmpeqd bytes search (ea 32-bit-array))
      (inst vmovmskps left bytes)
      (inst test :dword left left)
      (inst jmp :nz FOUND)

      (move left 32-bit-array)
      (inst sub left start)
      (inst cmp left 32)
      (inst jmp :ge LOOP)
      (inst vzeroupper)

      SSE
      (inst cmp left 16)
      (inst jmp :l SCALAR)

      (let ((bytes (reg-in-sc bytes 'complex-double-reg))
            (search (reg-in-sc search 'complex-double-reg)))
        (inst vmovd search element)
        (inst vpbroadcastd search search)
        (inst sub 32-bit-array 16)
        (inst vpcmpeqd bytes search (ea 32-bit-array))
        (inst vmovmskps left bytes)
        (inst test :dword left left)
        (inst jmp :nz FOUND))

      SCALAR
      (loop repeat 15
            do (inst cmp 32-bit-array start)
               (inst jmp :le DONE)
               (inst sub 32-bit-array 4)
               (inst cmp :byte element (ea 32-bit-array))
               (inst jmp :e FOUND-SCALAR))
      (inst jmp DONE)

      FOUND
      (inst vzeroupper)
      (inst bsr :dword left left)
      (inst shl left 2)
      (inst add 32-bit-array left)

      FOUND-SCALAR
      (inst sub 32-bit-array 32-bit-array*)
      (move res 32-bit-array)
      (inst shr res 1)
      DONE)))

(def-variant utf8-strlen :avx2 (sap)
  (declare (system-area-pointer sap)
           (optimize speed (safety 0)))
  (inline-vop
      (((bytes        sap-reg t) sap)
       ((ptr          sap-reg t))

       ((total-conts  unsigned-reg))
       ((tmp          unsigned-reg))

       ((tbl1         int-avx2-reg))
       ((tbl2         int-avx2-reg))
       ((tbl3         int-avx2-reg))
       ((tbl4         int-avx2-reg))

       ((mask-0f      int-avx2-reg))
       ((mask-c0      int-avx2-reg))
       ((zeros        int-avx2-reg))

       ((errors       int-avx2-reg))
       ((prev         int-avx2-reg))
       ((prev-len     int-avx2-reg))

       ((current      int-avx2-reg))
       ((tmp1         int-avx2-reg))
       ((tmp2         int-avx2-reg))
       ((tmp3         int-avx2-reg))
       ((tmp4         int-avx2-reg))
       ((total-conts-vec int-avx2-reg)))

      ((char-length descriptor-reg t :from :load)
       (byte-length unsigned-reg positive-fixnum :from :load)
       (all-ascii descriptor-reg t))
    (flet ((validate (&optional last)
             (assemble ()
               ;; Skip an all-ASCII block
               (inst vpmaxub tmp2 current prev)
               (inst vpmovmskb tmp tmp2)
               (inst test tmp tmp)
               (inst jmp :z VALIDATED)

               (inst vpsubusb tmp2 tmp2 (register-inline-constant
                                         :avx2
                                         #xDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDF))
               (inst vptest tmp2 tmp2)
               (inst jmp :nz full)

               ;; 1/2 bytes
               (inst vpcmpgtb tmp2 zeros current) ;; non-ascii

               (inst vpcmpgtb tmp3 mask-c0 current) ;; continuations
               ;; 2-byte leading bytes
               (inst vpcmpgtb tmp4 current (register-inline-constant
                                            :avx2
                                            #xC1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1))
               (inst vpand tmp4 tmp4 tmp2) ;; it's a signed comparison, remove ascii

               ;; Find #xC0 or #xC1, which are overlong
               (inst vpandn tmp1 tmp3 tmp2) ;; neither ascii or continuations
               (inst vpxor tmp1 tmp1 tmp4) ;; nor a valid leading byte

               ;; Continuations must follow leading bytes,
               ;; they must align with the shifted input

               (inst vpcmpgtb tmp2 prev-len zeros)

               ;; Identify leading non-ascii bytes, shifted left by
               ;; one byte, with the previous byte shifted in
               (inst vperm2i128 tmp2 tmp2 tmp4 #x21)
               (inst vpalignr tmp2 tmp4 tmp2 15)
               (inst vpxor tmp2 tmp2 tmp3)

               (inst vpor tmp1 tmp1 tmp2)
               (inst vpor errors errors tmp1)

               (inst vpsubb tmp2 zeros tmp3)
               (inst vpsadbw tmp2 tmp2 zeros)
               (inst vpaddq total-conts-vec total-conts-vec tmp2)
               (unless last
                 (inst vpsubb prev-len zeros tmp4)) ;; set to 1
               (inst jmp VALIDATED)
               FULL
               ;; The Keiser, Lemire algorithm
               (inst vperm2i128 tmp1 prev current #x21)
               (inst vpalignr tmp1 current tmp1 15)

               (inst vpsrlw tmp2 tmp1 4)
               (inst vpand tmp2 tmp2 mask-0f)

               (inst vpand tmp3 tmp1 mask-0f)

               (inst vpsrlw tmp1 current 4)
               (inst vpand tmp1 tmp1 mask-0f)

               (inst vpshufb tmp2 tbl1 tmp2)
               (inst vpshufb tmp3 tbl2 tmp3)
               (inst vpand tmp2 tmp2 tmp3)
               (inst vpshufb tmp3 tbl3 tmp1)
               (inst vpand tmp2 tmp2 tmp3)
               (inst vpor errors errors tmp2)

               (inst vpshufb tmp1 tbl4 tmp1)

               (inst vperm2i128 tmp2 prev-len tmp1 #x21)

               (inst vpalignr tmp4 tmp1 tmp2 13)
               (inst vpalignr tmp3 tmp1 tmp2 14)
               (inst vpalignr tmp2 tmp1 tmp2 15)
               (unless last
                 (inst vmovdqa prev-len tmp1))

               (inst vpcmpeqb tmp1 tmp1 tmp1)
               (inst vpaddb tmp3 tmp3 tmp1)
               (inst vpaddb tmp1 tmp1 tmp1)
               (inst vpaddb tmp4 tmp4 tmp1)

               (inst vpcmpgtb tmp2 tmp2 zeros)
               (inst vpcmpgtb tmp3 tmp3 zeros)
               (inst vpcmpgtb tmp4 tmp4 zeros)

               (inst vpor tmp2 tmp2 tmp3)
               (inst vpor tmp2 tmp2 tmp4)

               (inst vpcmpgtb tmp3 mask-c0 current)

               (inst vpxor tmp4 tmp3 tmp2)
               (inst vpor errors errors tmp4)

               ;; Subtract continuations
               (inst vpsubb tmp4 zeros tmp3)
               (inst vpsadbw tmp4 tmp4 zeros)
               (inst vpaddq total-conts-vec total-conts-vec tmp4)
               VALIDATED)))
      (assemble ()
        ;; Align the start and then mask off the extra bits
        (inst mov ptr bytes)
        (inst and ptr -32)
        (inst mov byte-length bytes)
        (inst sub byte-length ptr)

        (inst vmovdqu tmp2 (register-inline-constant :avx2 #x1F1E1D1C1B1A191817161514131211100F0E0D0C0B0A09080706050403020100))

        (inst vmovq tmp1 byte-length)
        (inst vpbroadcastb tmp1 tmp1)
        (inst vpcmpgtb tmp1 tmp1 tmp2)

        ;; Replace the aligned bits with ones, avoiding null termination
        (inst vmovdqa current (ea ptr))
        (inst vpandn current tmp1 current)
        (inst vpsubb current current tmp1)

        (inst vpxor zeros zeros zeros)
        ASCII
        (inst vpcmpeqb tmp1 current zeros)
        (inst vpmovmskb tmp tmp1)
        (inst test tmp tmp)
        (inst jmp :nz ASCII-TAIL)

        (inst vpmovmskb tmp current)
        (inst test tmp tmp)
        (inst jmp :nz NON-ASCII)

        (inst add ptr 32)
        (inst vmovdqa current (ea ptr))
        (inst jmp ASCII)

        ASCII-TAIL
        (inst bsf tmp tmp)

        (inst vpmovmskb byte-length current)
        (inst test byte-length byte-length)
        (inst jmp :z ALL-ASCII-DONE)

        (inst bsf byte-length byte-length)
        (inst cmp byte-length tmp)
        (inst jmp :b NON-ASCII)

        ALL-ASCII-DONE
        (inst add ptr tmp)
        (inst sub ptr bytes)
        (inst mov byte-length ptr)
        (inst mov char-length byte-length)
        (inst shl char-length 1)
        (load-symbol all-ascii t)
        (inst jmp DONE)

        NON-ASCII
        (inst mov char-length null-tn)
        (zeroize total-conts)
        (inst vpxor total-conts-vec total-conts-vec total-conts-vec)

        (inst vmovdqu tbl1 (register-inline-constant
                            :avx2
                            #x3806000100000000000000000000000038060001000000000000000000000000))
        (inst vmovdqu tbl2 (register-inline-constant
                            :avx2
                            #x2020242020202020202020100000010B2020242020202020202020100000010B))
        (inst vmovdqu tbl3 (register-inline-constant
                            :avx2
                            #x202020203535332B2020202020202020202020203535332B2020202020202020))
        (inst vmovdqu tbl4 (register-inline-constant
                            :avx2
                            #x0302010100000000000000000000000003020101000000000000000000000000))

        (inst vmovdqu mask-0f (register-inline-constant
                               :avx2
                               #x0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F))
        (inst vmovdqu mask-c0 (register-inline-constant
                               :avx2
                               #xC0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0))

        (inst vpxor errors errors errors)
        (inst vpxor prev prev prev)
        (inst vpxor prev-len prev-len prev-len)

        (inst jmp START)

        LOOP
        (inst vmovdqa current (ea ptr))

        START
        (inst vpcmpeqb tmp1 current zeros)
        (inst vpmovmskb tmp tmp1)
        (inst test tmp tmp)
        (inst jmp :nz TAIL)

        (validate)

        (inst vmovdqa prev current)
        (inst add ptr 32)
        (inst jmp LOOP)

        TAIL
        (inst bsf tmp tmp)

        (inst vmovdqu tmp3 (register-inline-constant
                            :avx2
                            #x1F1E1D1C1B1A191817161514131211100F0E0D0C0B0A09080706050403020100))
        (inst vmovq tmp2 tmp)
        (inst vpbroadcastb tmp2 tmp2)

        (inst vpcmpgtb tmp1 tmp2 tmp3)
        (inst vpand current current tmp1)

        (inst add ptr tmp)

        (validate t)

        (inst sub ptr bytes)
        (inst mov byte-length ptr)

        (inst vptest errors errors)
        (inst jmp :nz ERROR)

        (inst vextracti128 tmp1 total-conts-vec 1)
        (inst vpaddq tmp1 tmp1 total-conts-vec)
        (inst vpunpckhqdq tmp2 tmp1 tmp1)
        (inst vpaddq tmp1 tmp1 tmp2)
        (inst vmovq tmp tmp1)
        (inst add total-conts tmp)

        (inst mov char-length byte-length)
        (inst sub char-length total-conts)
        (inst shl char-length 1)
        ERROR
        (inst mov all-ascii null-tn)
        DONE
        (inst vzeroupper)))))

(def-variant sb-impl::character-string-utf8-length :avx2 (string)
  (with-pinned-objects (string)
    (inline-vop
        (((ptr sap-reg t) (vector-sap string))
         ((length any-reg t) (length string))

         ((chars-left   unsigned-reg))
         ((tmp          unsigned-reg))

         ;; 128-bit registers because strings are double-word aligned
         ((c-7f         int-sse-reg))
         ((c-7ff        int-sse-reg))
         ((c-ffff       int-sse-reg))
         ((c-1b         int-sse-reg))

         ((extra-len    int-sse-reg))
         ((errors       int-sse-reg))

         ((current      int-sse-reg))
         ((tmp1         int-sse-reg))
         ((mask         int-sse-reg)))

        ((res descriptor-reg t :from :load)
         (all-ascii descriptor-reg t :from :load))

      (inst mov res length)
      (load-symbol all-ascii t)

      (inst test length length)
      (inst jmp :z DONE)

      (inst mov chars-left length)
      (inst shr chars-left n-fixnum-tag-bits)

      (inst vmovdqa c-7f (register-inline-constant
                          :sse (concat-ub 32 (loop repeat 4 collect #x7F))))

      ASCII-LOOP
      (inst vmovdqa current (ea ptr))
      (inst add ptr 16)

      (inst vpcmpgtd tmp1 current c-7f)
      (inst vptest tmp1 tmp1)
      (inst jmp :nz NON-ASCII)

      (inst sub chars-left 4)
      (inst jmp :g ASCII-LOOP)
      (inst jmp DONE)

      NON-ASCII
      (inst mov res null-tn)
      (inst mov all-ascii null-tn)

      (inst vpxor extra-len extra-len extra-len)
      (inst vpxor errors errors errors)

      (inst vmovdqa c-7ff (register-inline-constant
                           :sse (concat-ub 32 (loop repeat 4 collect #x7FF))))
      (inst vmovdqa c-ffff (register-inline-constant
                            :sse (concat-ub 32 (loop repeat 4 collect #xFFFF))))
      (inst vmovdqa c-1b (register-inline-constant
                          :sse (concat-ub 32 (loop repeat 4 collect #x1B))))

      (inst jmp START)

      LOOP
      (inst sub chars-left 4)
      (inst jmp :le EXIT)
      (inst vmovdqa current (ea ptr))
      (inst add ptr 16)

      ;; ASCII fast path
      (inst vpcmpgtd tmp1 current c-7f)
      (inst vptest tmp1 tmp1)
      (inst jmp :z LOOP)

      START
      ;; Check for surrogates #xD800-#xDFFF
      (inst vpsrld tmp1 current 11)
      (inst vpcmpeqd tmp1 tmp1 c-1b)
      (inst vpor errors errors tmp1)

      (inst vpcmpgtd tmp1 current c-7f)

      (inst vpcmpgtd mask current c-7ff)
      (inst vpaddd tmp1 tmp1 mask)

      (inst vpcmpgtd mask current c-ffff)
      (inst vpaddd tmp1 tmp1 mask)

      (inst vpsubd extra-len extra-len tmp1)

      (inst jmp LOOP)

      EXIT
      (inst vptest errors errors)
      (inst jmp :nz DONE)

      (inst vpshufd tmp1 extra-len #b01001110)
      (inst vpaddd tmp1 tmp1 extra-len)
      (inst vpshufd mask tmp1 #b10110001)
      (inst vpaddd tmp1 tmp1 mask)

      (inst vmovd tmp tmp1)

      (inst shl tmp n-fixnum-tag-bits)
      (inst add tmp length)
      (inst mov res tmp)

      DONE)))

(def-variant utf8-sap-to-character-string :avx2 (sap string byte-array-length)
  (declare (index byte-array-length)
           (system-area-pointer sap)
           (simple-character-string string)
           (optimize speed (safety 0)))
  (with-pinned-objects (string)
    (multiple-value-bind (byte-index char-index)
        (inline-vop (((byte-array* sap-reg t :target byte-array) sap)
                     ((string sap-reg t) (vector-sap string))
                     ((table unsigned-reg t))
                     ((full-table unsigned-reg t))
                     ((string-length unsigned-reg) (logand (+ (length string) 3) -4))
                     ((byte-array sap-reg t :from (:argument 0)))
                     ((byte-array-length unsigned-reg) byte-array-length)
                     ((index unsigned-reg))
                     ((tmp2 unsigned-reg))
                     ((produced unsigned-reg))
                     ((tmp unsigned-reg))
                     ((current complex-double-reg t))
                     ((next complex-double-reg t))
                     ((x2 complex-double-reg t))
                     ((x3 complex-double-reg t))
                     ((x4 complex-double-reg t))
                     ((c-c0 complex-double-reg t))
                     ((c-df complex-double-reg t))
                     ((c-bf complex-double-reg t))
                     ((c-0f complex-double-reg t))
                     ((c-00ff int-avx2-reg t))
                     ((c-shift int-avx2-reg t))
                     ((c-3080 complex-double-reg t))
                     ((tag-clear complex-double-reg t)))
            ((byte-index unsigned-reg positive-fixnum :from :load)
             (char-index unsigned-reg positive-fixnum :from :load))

          (assemble ()
            (move byte-array byte-array*)
            (inst lea table
                  (register-inline-constant
                   (let ((table (make-array (* #b10101011 16) :element-type '(unsigned-byte 8)
                                                              :initial-element #xFF)))
                     (loop for row to #b10101010 ;; highest possible inverted index for compressing 1/2 bytes
                           do (loop with indexes = (loop for i below 8
                                                         unless (logbitp i row)
                                                         collect (* i 2)
                                                         and
                                                         collect (1+ (* i 2)))
                                    for column below 16
                                    for index = (pop indexes)
                                    when index
                                    do
                                    (setf (aref table (+ (* row 16) column)) index)))
                     table)))
            (inst mov tmp #xC0)
            (inst vmovd x2 tmp)
            (inst vpbroadcastb c-c0 x2)

            (inst mov tmp #xDF)
            (inst vmovd x2 tmp)
            (inst vpbroadcastb c-df x2)

            (inst mov tmp #x3080)
            (inst vmovd x2 tmp)
            (inst vpbroadcastw c-3080 x2)

            (inst mov tmp #xBF)
            (inst vmovd x2 tmp)
            (inst vpbroadcastw c-bf x2)
            (zeroize byte-index)
            (zeroize char-index)

            (flet ((convert-1-2 (full)
                     (assemble ()
                       LOOP
                       (move tmp byte-array-length)
                       (inst sub tmp byte-index)
                       (inst cmp tmp 9)
                       (inst jmp :l DONE)

                       (inst vmovq current (ea byte-array byte-index))

                       ;; Check for 3 or 4 bytes
                       (inst vpsubusb x2 current c-df)
                       (inst vptest x2 x2)
                       (inst jmp :nz full)

                       ;; Build a bit pattern of non-continuation bytes
                       ;; suitable for the lookup table
                       (inst vpcmpgtb x3 c-c0 current)
                       (inst vpmovmskb tmp2 x3)
                       (inst shl :dword tmp2 4)

                       ;; Widen to 16-bits
                       (inst vpmovzxbw x3 current)
                       (inst vpmovzxbw next (ea 1 byte-array byte-index))

                       ;; next is shifted by one,
                       ;; construct a codepoint from two overlapping bytes,
                       ;; i.e. (dpb b0 (byte 5 6) b1)
                       (inst vpsllw x4 x3 6)
                       (inst vpxor x4 x4 next)
                       (inst vpxor x4 x4 c-3080)

                       ;; Select either the x4 two bytes or one ascii byte
                       (inst vpcmpgtw next x3 c-bf)
                       (inst vpblendvb x3 x3 x4 next)

                       ;; Remove the gaps left over from using two bytes as one codepoint
                       (inst vpshufb x3 x3 (ea table tmp2))
                       (inst xor :dword tmp2 #xFF0) ;; Count non-continuation bytes
                       (move tmp string-length)
                       (inst sub tmp char-index)
                       (inst cmp tmp 8)

                       (inst jmp :l TAIL-16)

                       (inst popcnt :dword tmp2 tmp2)

                       ;; Widen
                       (let ((ymm-x4 (reg-in-sc x4 'int-avx2-reg)))
                         (inst vpmovzxwd ymm-x4 x3)
                         (inst vmovdqu (ea string char-index 4) ymm-x4))

                       (inst add byte-index 8)
                       (inst add char-index tmp2)

                       (inst jmp LOOP))))
              (assemble ()
                (convert-1-2 START-FULL)

                START-FULL

                (inst mov tmp2 #xFF)
                (inst vmovd c-00ff tmp2)
                (inst vpbroadcastw c-00ff c-00ff)

                (inst mov tmp2 #x0F)
                (inst vmovd c-0f tmp2)
                (inst vpbroadcastb c-0f c-0f)

                (inst mov tmp2 #x10000001)
                (inst vmovd c-shift tmp2)
                (inst vpbroadcastd c-shift c-shift)

                (inst lea full-table
                      (register-inline-constant
                       (coerce (loop for index below (ash 1 10)
                                     for low-index = (ldb (byte 8 0) index)
                                     for tmp2 = (ldb (byte 2 8) index)
                                     append (let ((starts (loop for i to 7
                                                                when (logbitp i low-index)
                                                                collect i)))
                                              (loop for lane below 8
                                                    for start = (pop starts)
                                                    for next = (car starts)
                                                    for sources = (when start
                                                                    (loop for i from (1- (or next (+ tmp2 8))) downto start
                                                                          collect i))
                                                    append (loop for byte below 4
                                                                 collect (or (pop sources) #xFF)))))
                               '(vector (unsigned-byte 8)))))
                (inst vmovdqa tag-clear (register-inline-constant
                                         :sse #x070F1F1F3F3F3F3F7F7F7F7F7F7F7F7F))

                FULL-LOOP
                (move tmp byte-array-length)
                (inst sub tmp byte-index)

                FULL-LOOP-LENGTH-COMPUTED
                (inst cmp tmp 16)
                (inst jmp :l DONE)

                (move tmp string-length)
                (inst sub tmp char-index)
                (inst cmp tmp 8)
                (inst jmp :l DONE)

                ;; Process the leading bytes in the first 8 bytes, loading 16 bytes
                ;; so that the last leading byte might drag in 3 more bytes
                (inst vmovdqu current (ea byte-array byte-index))

                ;; Identify leading bytes
                (inst vpcmpgtb x2 current c-c0)
                ;; Turn them into an 8 bit index
                (inst vpmovmskb tmp x2)
                (inst movzx '(:byte :dword) index tmp)
                (inst popcnt :dword produced index)

                ;; Count the number of bytes to the next leading byte, turning it into a 2 bit suffix
                (inst shr :dword tmp 8)
                (inst tzcnt :dword tmp tmp)
                (inst shl :dword tmp 8)
                (inst or :dword index tmp)

                (inst shl :dword index 5)

                ;; Use the high 4 bits of each byte to get an and-mask that
                ;; will clear their tags
                (inst vpsrlw x4 current 4)
                (inst vpand x4 x4 c-0f)

                (inst vpshufb x4 tag-clear x4)
                (inst vpand current current x4)

                (let ((current (reg-in-sc current 'int-avx2-reg))
                      (x2 (reg-in-sc x2 'int-avx2-reg))
                      (x3 (reg-in-sc x3 'int-avx2-reg)))
                  ;; Duplicate the low bits, for vpshufb
                  (inst vinserti128 current current current 1)

                  ;; Shuffle the bytes into 4-byte lanes
                  (inst vpshufb current current (ea full-table index))

                  ;; Perform
                  ;; A + B<<6 + C<<12 + D<<18
                  (inst vpand x2 c-00ff current)
                  (inst vpandn x3 c-00ff current)
                  (inst vpsrlw x3 x3 2)
                  (inst vpaddw x2 x2 x3)
                  (inst vpmaddwd current x2 c-shift)

                  (inst vmovdqu (ea string char-index 4) current))

                (inst add byte-index 8)
                (inst add char-index produced)

                ;; Can't re-enter the 1-2 loop if there were
                ;; continuation bytes into the next word, (and can't
                ;; add suffix to byte-index, as it will kill out of
                ;; order execution)
                (inst test :dword tmp tmp)
                (inst jmp :nz FULL-LOOP)
                (convert-1-2 FULL-LOOP-LENGTH-COMPUTED)))


            TAIL-16
            (inst cmp :dword tmp 4)
            (inst jmp :l DONE)
            (inst and :dword tmp2 #xF0)
            (inst popcnt :dword tmp2 tmp2)

            ;; Widen
            (inst vpmovzxwd x4 x3)
            (inst vmovdqu (ea string char-index 4) x4)

            (inst add byte-index 4)
            (inst add char-index tmp2)

            DONE
            (inst vzeroupper)))

      (loop while (and (< byte-index byte-array-length)
                       (<= #x80 (sap-ref-8 sap byte-index) #xbf))
            ;; Remove any continuations consumed by the above loop
            do (incf byte-index))
      (loop while (< byte-index byte-array-length)
            do
            (let ((b0 (sap-ref-8 sap byte-index)))
              (cond
                ((< b0 #x80)
                 (setf (schar string char-index) (code-char b0))
                 (incf byte-index))
                ((< b0 #xE0)
                 (let ((b1 (sap-ref-8 sap (+ byte-index 1))))
                   (setf (schar string char-index)
                         (code-char (dpb b0 (byte 5 6) b1)))
                   (incf byte-index 2)))
                ((< b0 #xF0)
                 (let ((b1 (sap-ref-8 sap (+ byte-index 1)))
                       (b2 (sap-ref-8 sap (+ byte-index 2))))
                   (setf (schar string char-index)
                         (code-char (dpb b0 (byte 4 12)
                                         (dpb b1 (byte 6 6) b2))))
                   (incf byte-index 3)))
                (t
                 (let ((b1 (sap-ref-8 sap (+ byte-index 1)))
                       (b2 (sap-ref-8 sap (+ byte-index 2)))
                       (b3 (sap-ref-8 sap (+ byte-index 3))))
                   (setf (schar string char-index)
                         (code-char (dpb b0 (byte 3 18)
                                         (dpb b1 (byte 6 12)
                                              (dpb b2 (byte 6 6) b3)))))
                   (incf byte-index 4))))
              (incf char-index))))))

(def-variant character-string-to-utf8-byte-array :avx2 (byte-array string byte-array-length)
  (declare (index byte-array-length)
           (simple-character-string string)
           ((simple-array (unsigned-byte 8) (*)) byte-array)
           (optimize speed (safety 0)))
  (let ((length (length string)))
    (with-pinned-objects (string byte-array)
      (multiple-value-bind (byte-index char-index)
          (inline-vop (((byte-array sap-reg t) (vector-sap byte-array))
                       ((string sap-reg t) (vector-sap string))
                       ((n signed-reg) (logand (+ (* length 4) 15) -16))
                       ((byte-array-length unsigned-reg) (logand (+ byte-array-length 15) -16))
                       ((table sap-reg t))
                       ((full-table sap-reg t))
                       ((tmp unsigned-reg))
                       ((multiplier unsigned-reg))
                       ((temp complex-double-reg))
                       ((bytes complex-double-reg))
                       ((c-3f complex-double-reg))
                       ((c-80 complex-double-reg))
                       ((c-7ff int-avx2-reg))
                       ((c-7f complex-double-reg))
                       ((c-ffff complex-double-reg))
                       ((low-bytes complex-double-reg))
                       ((high-bytes complex-double-reg))
                       ((utf8-mask complex-double-reg))
                       ((zero complex-double-reg))
                       ((ascii complex-double-reg)))
              ((byte-index unsigned-reg positive-fixnum :from :load)
               (char-index unsigned-reg positive-fixnum :from :load))

            (inst lea table (register-inline-constant
                             (let ((table (make-array (* 256 16) :element-type '(unsigned-byte 8)
                                                                 :initial-element #xFF)))
                               (loop for row below 256
                                     do (loop with indexes = (loop for i below 8
                                                                   collect (* i 2)
                                                                   unless (logbitp i row)
                                                                   collect (1+ (* i 2)))
                                              for column below 16
                                              for index = (pop indexes)
                                              when index
                                              do
                                              (setf (aref table (+ (* row 16) column)) index)))
                               table)))
            (inst mov tmp #x3F)
            (inst vmovd temp tmp)
            (inst vpbroadcastw c-3f temp)

            (inst mov tmp #b1000000011000000)
            (inst vmovd temp tmp)
            (inst vpbroadcastw utf8-mask temp)

            (inst mov tmp #x80)
            (inst vmovd temp tmp)
            (inst vpbroadcastw c-80 temp)

            (inst mov tmp #x7ff)
            (inst vmovd temp tmp)
            (inst vpbroadcastd c-7ff temp)

            (inst vpxor zero zero zero)

            (flet ((make-full-table ()
                     (let* ((table-size 256)
                            (row-size 64)
                            (table (make-array (* table-size row-size) :element-type '(unsigned-byte 8)
                                                                       :initial-element 0)))
                       (loop for row below table-size
                             for dest-index = 0
                             do (loop
                                  for lane below 4
                                  for bytes = (1+ (ldb (byte 2 (* lane 2)) row))
                                  do (loop for b below bytes
                                           for src-index = (+ (* lane 4) (- bytes 1 b))
                                           for lead-p = (= b 0)
                                           for and-mask = (if lead-p
                                                              (case bytes
                                                                (1 #x7F) (2 #x1F) (3 #x0F) (4 #x07))
                                                              #x3F)
                                           for orr-mask = (if lead-p
                                                              (case bytes
                                                                (1 #x00) (2 #xC0) (3 #xE0) (4 #xF0))
                                                              #x80)
                                           do (setf (aref table (+ (* row row-size) dest-index)) src-index)
                                              (setf (aref table (+ (* row row-size) 16 dest-index)) and-mask)
                                              (setf (aref table (+ (* row row-size) 32 dest-index)) orr-mask)
                                              (incf dest-index)))
                                (loop for i from dest-index below 16
                                      do (setf (aref table (+ (* row row-size) i)) #xFF)
                                         (setf (aref table (+ (* row row-size) 16 i)) 0)
                                         (setf (aref table (+ (* row row-size) 32 i)) 0))
                                (setf (aref table (+ (* row row-size) 48)) dest-index))
                       table))

                   (convert (size full)
                     (inst cmp byte-array-length (/ size 2))
                     (inst jmp :l DONE)
                     (let* ((sc (ecase size
                                  (32 'int-avx2-reg)
                                  (16 'int-sse-reg)))
                            (bytes (reg-in-sc bytes sc))
                            (temp (reg-in-sc temp sc)))
                       (inst vmovdqu bytes (ea string char-index))
                       ;; Stop if anything is 3-4 bytes in utf8
                       (inst vpcmpgtd temp bytes c-7ff)
                       (inst vptest temp temp)
                       (inst jmp :nz full)
                       ;; Narrow to 16 bits
                       (cond ((eq size 32)
                              (inst vpackusdw bytes bytes bytes)
                              (inst vpermq bytes bytes 216))
                             (t
                              (inst vpackusdw bytes bytes zero))))

                     (inst vpcmpgtw ascii c-80 bytes)

                     ;; Construct
                     ;; (logior
                     ;;  #x80C0
                     ;;  (dpb (ldb (byte 6 0) bits)
                     ;;       (byte 8 8)
                     ;;       (ldb (byte 5 6) bits)))
                     ;; For each 16-bits
                     (inst vpsrlw low-bytes bytes 6)
                     (inst vpor low-bytes low-bytes utf8-mask)
                     (inst vpand high-bytes c-3f bytes)
                     (inst vpsllw high-bytes high-bytes 8)
                     (inst vpor high-bytes high-bytes low-bytes)

                     ;; Either select two bytes or one byte
                     (inst vpblendvb bytes high-bytes bytes ascii)
                     ;; Shrink the mask from 16 bits to 8 bits
                     (inst vpacksswb ascii ascii zero)
                     ;; Remove the zero second byte from ascii words
                     (inst vpmovmskb tmp ascii)

                     (inst shl :dword tmp 4)
                     (inst vpshufb bytes bytes (ea table tmp))
                     (if (eq size 32)
                         (inst vmovdqu (ea byte-index byte-array) bytes)
                         (inst vmovq (ea byte-index byte-array) bytes))
                     (inst add char-index size)
                     (when (eq size 32)
                       (inst popcnt :dword tmp tmp)
                       (inst add byte-index 16)
                       (inst sub byte-index tmp)
                       (inst sub byte-array-length 16)
                       (inst add byte-array-length tmp)
                       (inst sub n size)))
                   (convert-full ()
                     (inst cmp byte-array-length 16)
                     (inst jmp :l DONE)
                     (symbol-macrolet ((t1 low-bytes)
                                       (t2 high-bytes)
                                       (t3 ascii))

                       ;; Compute utf8 lengths -1
                       (inst vpcmpgtd t1 bytes c-7f)
                       (inst vpcmpgtd t2 bytes c-7ff)
                       (inst vpcmpgtd t3 bytes c-ffff)

                       (inst vpaddd temp t1 t2)
                       (inst vpaddd temp temp t3)

                       ;; Negate
                       (inst vpsubd temp zero temp)

                       ;; Build an 8-bit index mask
                       ;; Narrow to 16 bits, making a 64-bit mask
                       (inst vpackusdw temp temp temp)
                       (inst vmovq tmp temp)

                       ;; Multiplying by 1 + 2^6 + 2^12 + 2^18
                       ;; shifts two bits per byte into the upper byte
                       (inst imul tmp multiplier)
                       (inst shr tmp (- 56 6)) ;; shift left 6 for the table entry size

                       ;; Spread the character to all 4 bytes
                       (inst vpslld t1 bytes 6)
                       (inst vpslld t2 bytes 4)
                       (inst vpslld t3 bytes 2)

                       (inst vpand t1 t1 (register-inline-constant :oword #xFF000000FF000000FF000000FF000000))
                       (inst vpand t2 t2 (register-inline-constant :oword #x00FF000000FF000000FF000000FF0000))
                       (inst vpand t3 t3 (register-inline-constant :oword #x0000FF000000FF000000FF000000FF00))
                       (inst vpand bytes bytes (register-inline-constant :oword #x000000FF000000FF000000FF000000FF))

                       (inst vpor t2 t2 t3)
                       (inst vpor bytes bytes t1)
                       (inst vpor bytes bytes t2)

                       ;; Shuffle the bytes into place
                       (inst vpshufb bytes bytes (ea 0 full-table tmp))
                       (inst vpand bytes bytes (ea 16 full-table tmp))
                       (inst vpor bytes bytes (ea 32 full-table tmp))
                       (inst movzx '(:byte :dword) tmp (ea 48 full-table tmp)) ;; number of produced bytes

                       (inst vmovdqu (ea byte-index byte-array) bytes)

                       (inst add byte-index tmp)
                       (inst sub byte-array-length tmp)
                       (inst add char-index 16)
                       (inst sub n 16))))

              (assemble ()
                (zeroize byte-index)
                (zeroize char-index)

                (inst cmp n 32)
                (inst jmp :l TAIL)
                LOOP
                (convert 32 START-FULL-LENGTH)
                (inst cmp n 32)
                (inst jmp :ge LOOP)

                TAIL
                (inst test n n)
                (inst jmp :z DONE)

                (convert 16 START-FULL-LENGTH)
                (inst jmp DONE)

                START-FULL-LENGTH
                (inst lea full-table (register-inline-constant (make-full-table)))

                (inst mov tmp #xFFFF)
                (inst vmovd temp tmp)
                (inst vpbroadcastd c-ffff temp)

                (inst mov tmp #x7f)
                (inst vmovd temp tmp)
                (inst vpbroadcastd c-7f temp)
                (inst mov multiplier #x0100040010004000)


                FULL-LENGTH
                (convert-full)

                (inst cmp n 32)
                (inst jmp :l TAIL2)
                LOOP2
                (convert 32 FULL-LENGTH)
                (inst cmp n 32)
                (inst jmp :ge LOOP2)

                TAIL2
                (inst test n n)
                (inst jmp :z DONE)
                (convert 16 FULL-LENGTH)))
            DONE
            (inst vzeroupper))
        (setf char-index (truncate char-index 4))
        (let ((sap (vector-sap byte-array)))
          (loop while (< char-index length)
                do
                (let ((bits (char-code (char string char-index))))
                  (cond ((< bits 128)
                         (setf (aref byte-array byte-index) bits)
                         (incf byte-index))
                        ((< bits 2048)
                         (setf (sap-ref-16 sap byte-index)
                               (logior
                                #x80C0
                                (dpb (ldb (byte 6 0) bits)
                                     (byte 8 8)
                                     (ldb (byte 5 6) bits))))
                         (incf byte-index 2))
                        ((< bits 65536)
                         (setf (sap-ref-16 sap (1+ byte-index))
                               (logior
                                #x8080
                                (dpb (ldb (byte 6 0) bits)
                                     (byte 8 8)
                                     (ldb (byte 6 6) bits))))
                         (setf (aref byte-array byte-index) (logior 224 (ldb (byte 4 12) bits)))
                         (incf byte-index 3))
                        (t
                         (setf (sap-ref-32 sap byte-index)
                               (logior
                                #x808080F0
                                (dpb (ldb (byte 6 0) bits)
                                     (byte 8 24)
                                     (dpb (ldb (byte 6 6) bits)
                                          (byte 8 16)
                                          (dpb (ldb (byte 6 12) bits)
                                               (byte 8 8)
                                               (ldb (byte 3 18) bits))))))
                         (incf byte-index 4)))
                  (incf char-index))))))))
