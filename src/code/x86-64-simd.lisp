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
    (make-random-tn :kind :normal
                    :sc (sc-or-lose sc)
                    :offset (tn-offset tn))))

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
(defun simd-copy-utf8-to-character-string (start end string ibuf)
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
(defun simd-copy-utf8-to-base-string (start end string ibuf)
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
(def-variant simd-copy-utf8-crlf-to-base-string :ssse3+popcnt (start end string ibuf)
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
(def-variant simd-copy-utf8-crlf-to-character-string :ssse3+popcnt (start end string ibuf)
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

(defun simd-copy-character-string-to-utf8 (start end string obuf)
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
            (inst movdqa last-newlines (register-inline-constant :sse
                                                                 (concat-ub 32 (loop repeat 4
                                                                                     collect -1))))
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

            (loop for bytes in (list bytes1 bytes2 bytes3 bytes4)
                  do
                  (inst movdqa temp bytes)
                  (inst pcmpgtd temp ascii-mask)
                  (inst pmovmskb tail temp)
                  (inst test :dword tail tail)
                  (inst jmp :nz done))

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

(def-variant simd-copy-character-string-to-utf8 :avx2 (start end string obuf)
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
                                                                                   collect 127))))
            (inst vmovdqu newlines (register-inline-constant :avx2
                                                            (concat-ub 32 (loop repeat 8
                                                                                collect 10))))
            (inst vmovdqu increment (register-inline-constant :avx2
                                                             (concat-ub 32 (loop repeat 8
                                                                                 collect 8))))
            (inst vmovdqu indexes (register-inline-constant :avx2
                                                           (concat-ub 32 '(7 6 5 4 3 2 1 0))))
            (inst vmovdqu last-newlines (register-inline-constant :avx2
                                                                 (concat-ub 32 (loop repeat 8
                                                                                     collect -1))))

            (inst add byte-array* tail)
            (move byte-array byte-array*)
            (inst add end byte-array*)
            (inst add 32-bit-array string-start)

            (inst jmp start)

            LOOP
            (inst vmovdqu bytes1 (ea 32-bit-array))
            (inst vmovdqu bytes2 (ea 32 32-bit-array))

            (loop for bytes in (list bytes1 bytes2)
                  do
                  (inst vpcmpgtd temp bytes ascii-mask)
                  (inst vpmovmskb tail temp)
                  (inst test :dword tail tail)
                  (inst jmp :nz done))

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
      (inst mov res nil-value)

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
               (inst jmp :eq FOUND-SCALAR)
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
      (inst mov res nil-value)

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
               (inst jmp :eq FOUND-SCALAR)
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
      (inst mov res nil-value)

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
               (inst jmp :eq FOUND-SCALAR))
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
      (inst mov res nil-value)

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
               (inst jmp :eq FOUND-SCALAR))
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
      (inst mov res nil-value)
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
               (inst jmp :eq FOUND-SCALAR)
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
      (inst mov res nil-value)
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
               (inst jmp :eq FOUND-SCALAR)
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
      (inst mov res nil-value)

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
               (inst jmp :eq FOUND-SCALAR))
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
      (inst mov res nil-value)

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
               (inst jmp :eq FOUND-SCALAR))
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
