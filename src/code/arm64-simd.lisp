;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

#+sb-unicode
(assert (= base-char-code-limit 128))

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
      (inst str value (@ vector offset))))

  (defun reg-in-sc (tn sc)
    (make-random-tn :kind :normal
                    :sc (sc-or-lose sc)
                    :offset (tn-offset tn))))

(defmacro simd-mask-32 (value)
  `(inline-vop
       (((:info value) ,value))
       ((res complex-double-reg complex-double-float))
     (inst movi res value :4s)))

(defmacro simd-mask-8 (value)
  `(inline-vop
       (((:info value) ,value))
       ((res complex-double-reg complex-double-float))
     (inst movi res value :16b)))

;;; Basically like the x86 with-pinned-object,
;;; but here only the boxed registers are pinned.
;;; This doesn't prevent the var from going to the stack, but none of
;;; the routines should do that.
(defmacro with-pinned-objects-in-registers (vars &body body)
  `(multiple-value-prog1 ,@body
     ,@(loop for var in vars
             collect `(touch-object ,var))))

(defmacro simd-string-case (a source destination index fallback)
  `(let ((ascii-p (simd-mask-32 ,(if (char= a #\a)
                                     223
                                     191)))
         (a-mask (simd-mask-32 ,(char-code a)))
         (z-mask (simd-mask-32 25))
         (flip (simd-mask-32 #x20)))
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

(defun simd-nreverse8 (result vector start end)
  (declare (optimize speed (safety 0)))
  (with-pinned-objects-in-registers (vector)
    (inline-vop (((left sap-reg t) (vector-sap vector))
                 ((start any-reg tagged-num) start)
                 ((end) end)
                 ((right signed-reg signed-num))
                 ((gl))
                 ((gr))
                 ((vl complex-double-reg complex-double-float))
                 ((vr)))
        ()
      (inst add right left (lsr end 1))
      (inst add left left (lsr start 1))
      (inst sub gl right left)
      (inst cmp gl 32)
      (inst b :lt WORD)
      (inst sub right right 16)

      LOOP
      (inst ldr vl (@ left))
      (inst ldr vr (@ right))

      (inst rev64 vl vl)
      (inst ext vl vl vl 8)
      (inst rev64 vr vr)
      (inst ext vr vr vr 8)

      (inst str vr (@ left 16 :post-index))
      (inst str vl (@ right -16 :post-index))
      (inst cmp left right)
      (inst b :lt loop)

      (inst add right right 16)

      (inst sub gl right left)
      WORD
      (inst cmp gl 16)
      (inst b :lt BYTE)

      (inst ldr gl (@ left))
      (inst ldr gr (@ right -8 :pre-index))
      (inst rev gl gl)
      (inst rev gr gr)
      (inst str gr (@ left 8 :post-index))
      (inst str gl (@ right))

      BYTE
      (inst sub right right 1)
      (inst cmp right left)
      (inst b :lt DONE)

      ;; After the 16-element copy above there are at most 15
      ;; elements, have to swap 14 elements with one staying in the
      ;; middle.
      (loop repeat 7
            do
            (inst ldrb gl (@ left))
            (inst ldrb gr (@ right))
            (inst strb gr (@ left 1 :post-index))
            (inst strb gl (@ right -1 :post-index))
            (inst cmp left right)
            (inst b :ge DONE))
      DONE))
  result)

(defun simd-nreverse32 (result vector start end)
  (declare (optimize speed (safety 0)))
  (with-pinned-objects-in-registers (vector)
    (inline-vop (((left sap-reg t) (vector-sap vector))
                 ((start any-reg tagged-num) start)
                 ((end) end)
                 ((right signed-reg signed-num))
                 ((gl))
                 ((gr))
                 ((vl complex-double-reg complex-double-float))
                 ((vr)))
        ()
      (inst add right left (lsl end 1))
      (inst add left left (lsl start 1))
      (inst sub gl right left)
      (inst cmp gl 32)
      (inst b :lt SCALAR)
      (inst sub right right 16)

      LOOP
      (inst ldr vl (@ left))
      (inst ldr vr (@ right))

      (inst rev64 vl vl :4s)
      (inst ext vl vl vl 8)
      (inst rev64 vr vr :4s)
      (inst ext vr vr vr 8)

      (inst str vr (@ left 16 :post-index))
      (inst str vl (@ right -16 :post-index))
      (inst cmp left right)
      (inst b :lt loop)

      (inst add right right 16)

      SCALAR
      (inst sub right right 4)
      (inst cmp right left)
      (inst b :lt DONE)
      (setf gl (32-bit-reg gl)
            gr (32-bit-reg gr))

      (loop repeat 3
            do
            (inst ldr gl (@ left))
            (inst ldr gr (@ right))
            (inst str gr (@ left 4 :post-index))
            (inst str gl (@ right -4 :post-index))
            (inst cmp left right)
            (inst b :ge DONE))
      DONE))
  result)

(defun simd-reverse8 (target source start length)
  (declare (optimize speed (safety 0)))
  (with-pinned-objects-in-registers (target source)
    (inline-vop (((source sap-reg t) (vector-sap source))
                 ((target sap-reg t) (vector-sap target))
                 ((start any-reg tagged-num) start)
                 ((length) length)
                 ((s-i signed-reg signed-num))
                 ((t-i))
                 ((g))
                 ((v complex-double-reg complex-double-float)))
        ()
      (inst add source source (lsr start 1))
      (inst mov t-i 0)
      (inst lsr s-i length 1)
      (inst cmp s-i 16)
      (inst b :lt WORD)
      (inst sub s-i s-i 16)

      LOOP
      (inst ldr v (@ source s-i))

      (inst rev64 v v)
      (inst ext v v v 8)
      (inst str v (@ target t-i))

      (inst add t-i t-i 16)
      (inst subs s-i s-i 16)
      (inst b :ge LOOP)
      (inst add s-i s-i 16)

      WORD
      (inst cmp s-i 7)
      (inst b :lt BYTE)
      (inst sub s-i s-i 8)
      (inst ldr g (@ source s-i))
      (inst rev g g)
      (inst str g (@ target t-i))
      (inst add t-i t-i 8)

      BYTE
      (inst subs s-i s-i 1)
      (inst b :lt DONE)

      (loop repeat 7
            do
            (inst ldrb g (@ source s-i))
            (inst strb g (@ target t-i))
            (inst add t-i t-i 1)
            (inst subs s-i s-i 1)
            (inst b :lt DONE))
      DONE))
  target)

(defun simd-reverse32 (target source start length)
  (declare (optimize speed (safety 0)))
  (with-pinned-objects-in-registers (target source)
    (inline-vop (((source sap-reg t) (vector-sap source))
                 ((target sap-reg t) (vector-sap target))
                 ((start any-reg tagged-num) start)
                 ((length) length)
                 ((s-i signed-reg signed-num))
                 ((t-i))
                 ((g))
                 ((v complex-double-reg complex-double-float)))
        ()
      (inst add source source (lsl start 1))
      (inst mov t-i 0)
      (inst lsl s-i length 1)
      (inst cmp s-i 16)
      (inst b :lt SCALAR)
      (inst sub s-i s-i 16)

      LOOP
      (inst ldr v (@ source s-i))

      (inst rev64 v v :4s)
      (inst ext v v v 8)
      (inst str v (@ target t-i))

      (inst add t-i t-i 16)
      (inst subs s-i s-i 16)
      (inst b :ge LOOP)
      (inst add s-i s-i 16)

      SCALAR
      (inst subs s-i s-i 4)
      (inst b :lt DONE)

      (setf g (32-bit-reg g))
      (loop repeat 3
            do
            (inst ldr g (@ source s-i))
            (inst str g (@ target t-i))
            (inst add t-i t-i 4)
            (inst subs s-i s-i 4)
            (inst b :lt DONE))
      DONE))
  target)

(defun simd-cmp-8-32 (byte-array 32-bit-array length)
  (declare (optimize speed (safety 0)))
  (with-pinned-objects-in-registers (byte-array 32-bit-array)
    (inline-vop (((byte-array sap-reg t) (vector-sap byte-array))
                 ((32-bit-array sap-reg t) (vector-sap 32-bit-array))
                 ((length any-reg) length)
                 ((32-bits complex-double-reg))
                 ((bytes single-reg))
                 ((cmp unsigned-reg))
                 ((end)))
        ((res descriptor-reg t :from :load))
      (load-symbol res t)
      (inst cbz length DONE)
      (inst add end byte-array (lsr length 1))
      LOOP
      (inst ldr bytes (@ byte-array 4 :post-index))
      (inst ldr 32-bits (@ 32-bit-array 16 :post-index))
      (setf bytes (reg-in-sc bytes 'complex-double-reg))

      (inst ushll bytes :8h bytes :8b 0)
      (inst ushll bytes :4s bytes :4h 0)

      (inst cmeq bytes bytes 32-bits :4s)
      (inst uminv bytes bytes :4s)
      (inst umov cmp bytes 0 :s)
      (inst cbz cmp FALSE)
      (inst cmp byte-array end)
      (inst b :lt LOOP)
      (inst b DONE)
      FALSE
      (inst mov res null-tn)
      DONE)))

(defun simd-cmp-8-8 (a b length)
  (declare (optimize speed (safety 0)))
  (with-pinned-objects-in-registers (a b)
    (inline-vop (((a-array sap-reg t) (vector-sap a))
                 ((b-array sap-reg t) (vector-sap b))
                 ((length any-reg) length)
                 ((a complex-double-reg))
                 ((b))
                 ((cmp unsigned-reg))
                 ((end)))
        ((res descriptor-reg t :from :load))
      (load-symbol res t)
      (inst cbz length DONE)
      (inst add end a-array (lsr length 1))
      LOOP
      (inst ldr a (@ a-array 16 :post-index))
      (inst ldr b (@ b-array 16 :post-index))
      (inst cmeq a a b :16b)
      (inst uminv a a :16b)
      (inst umov cmp a 0 :b)
      (inst cbz cmp FALSE)
      (inst cmp a-array end)
      (inst b :lt LOOP)
      (inst b DONE)
      FALSE
      (inst mov res null-tn)
      DONE)))

(defun simd-cmp-32-32 (a b length)
  (declare (optimize speed (safety 0)))
  (with-pinned-objects-in-registers (a b)
    (inline-vop (((a-array sap-reg t) (vector-sap a))
                 ((b-array sap-reg t) (vector-sap b))
                 ((length any-reg) length)
                 ((a complex-double-reg))
                 ((b))
                 ((cmp unsigned-reg))
                 ((end)))
        ((res descriptor-reg t :from :load))
      (load-symbol res t)
      (inst cbz length DONE)
      (inst add end a-array (lsl length 1))
      LOOP
      (inst ldr a (@ a-array 16 :post-index))
      (inst ldr b (@ b-array 16 :post-index))
      (inst cmeq a a b :4s)
      (inst uminv a a :4s)
      (inst umov cmp a 0 :s)
      (inst cbz cmp FALSE)
      (inst cmp a-array end)
      (inst b :lt LOOP)
      (inst b DONE)
      FALSE
      (inst mov res null-tn)
      DONE)))

(defun simd-base-string-equal (a b length)
  (declare (simple-base-string a b)
           (optimize speed (safety 0)))
  (with-pinned-objects-in-registers (a b)
    (inline-vop (((a-array sap-reg t) (vector-sap a))
                 ((b-array sap-reg t) (vector-sap b))
                 ((length any-reg) length)
                 ((a complex-double-reg))
                 ((b))
                 ((cmp unsigned-reg))
                 ((end))
                 ((a-mask complex-double-reg complex-double-float))
                 ((z-mask))
                 ((flip))
                 ((temp))
                 ((temp2)))
        ((res descriptor-reg t :from :load))
      (load-symbol res t)
      (inst cbz length DONE)
      (inst add end a-array (lsr length 1))
      (inst movi a-mask (char-code #\a) :16b)
      (inst movi z-mask 25 :16b)
      (inst movi flip #x20 :16b)
      LOOP
      (inst ldr a (@ a-array 16 :post-index))
      (inst ldr b (@ b-array 16 :post-index))

      ;; Upcase a
      (inst s-sub temp a a-mask)
      (inst cmhs temp z-mask temp)
      (inst s-and temp temp flip)
      (inst s-eor a a temp)

      ;; Upcase b
      (inst s-sub temp2 b a-mask)
      (inst cmhs temp2 z-mask temp2)
      (inst s-and temp2 temp2 flip)
      (inst s-eor b b temp2)

      (inst cmeq a a b)
      (inst uminv a a :16b)
      (inst umov cmp a 0 :b)
      (inst cbz cmp FALSE)

      (inst cmp a-array end)
      (inst b :lt LOOP)
      (inst b DONE)
      FALSE
      (inst mov res null-tn)
      DONE)))

#+sb-unicode
(defun simd-base-character-string-equal (base-string character-string length)
  (declare (optimize speed (safety 0)))
  (with-pinned-objects-in-registers (base-string character-string)
    (inline-vop (((base-string sap-reg t) (vector-sap base-string))
                 ((character-string sap-reg t) (vector-sap character-string))
                 ((length any-reg) length)
                 ((characters complex-double-reg))
                 ((base-chars single-reg))
                 ((cmp unsigned-reg))
                 ((end))
                 ((a-mask complex-double-reg complex-double-float))
                 ((z-mask))
                 ((flip))
                 ((temp))
                 ((temp2)))
        ((res descriptor-reg t :from :load))
      (load-symbol res t)
      (inst cbz length DONE)
      (inst add end base-string (lsr length 1))
      (inst movi a-mask (char-code #\a) :4s)
      (inst movi z-mask 25 :4s)
      (inst movi flip #x20 :4s)
      LOOP
      (inst ldr base-chars (@ base-string 4 :post-index))
      (inst ldr characters (@ character-string 16 :post-index))
      (setf base-chars (reg-in-sc base-chars 'complex-double-reg))

      ;; Upcase 32-bit wide characters
      (inst s-sub temp characters a-mask :4s)
      (inst cmhs temp z-mask temp :4s)
      (inst s-and temp temp flip)
      (inst s-eor characters characters temp)

      ;; Widen 8-bit wide characters to 32-bits
      (inst ushll base-chars :8h base-chars :8b 0)
      (inst ushll base-chars :4s base-chars :4h 0)

      ;; And upcase them too
      (inst s-sub temp2 base-chars a-mask :4s)
      (inst cmhs temp2 z-mask temp2 :4s)
      (inst s-and temp2 temp2 flip)
      (inst s-eor base-chars base-chars temp2)

      (inst cmeq base-chars base-chars characters :4s)
      (inst uminv base-chars base-chars :4s)
      (inst umov cmp base-chars 0 :s)
      (inst cbz cmp FALSE)

      (inst cmp base-string end)
      (inst b :lt LOOP)
      (inst b DONE)
      FALSE
      (inst mov res null-tn)
      DONE)))

(defun simd-copy-utf8-to-character-string (start end string ibuf)
  (declare (type index start end)
           (optimize speed (safety 0)))
  (with-pinned-objects-in-registers (string)
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
                          ((string-start any-reg) string-start)
                          ((end unsigned-reg))
                          ((head any-reg) head)
                          ((n any-reg) n)
                          ((ascii-mask complex-double-reg))
                          ((bytes complex-double-reg))
                          ((16-bits complex-double-reg))
                          ((16-bits-2 complex-double-reg))
                          ((32-bits complex-double-reg))
                          ((32-bits-2 complex-double-reg))
                          ((32-bits-3 complex-double-reg))
                          ((32-bits-4 complex-double-reg))
                          ((temp complex-double-reg)))
                 ((res unsigned-reg unsigned-num))
               (inst movi ascii-mask 128 :16b)
               (inst add byte-array* byte-array* (lsr head 1))
               (inst mov byte-array byte-array*)
               (inst add end byte-array* (lsr n 1))
               (inst add 32-bit-array 32-bit-array (lsr string-start 1))
               (inst b start)

               LOOP
               (inst ldr bytes (@ byte-array))
               (inst s-and temp bytes ascii-mask)
               (inst umaxv temp temp :4s)
               (inst umov tmp-tn temp 0 :s)
               (inst cbnz tmp-tn DONE)
               (inst add byte-array byte-array 16)

               (inst ushll 16-bits :8h bytes :8b)
               (inst ushll 32-bits :4s 16-bits :4h)

               (inst ushll2 16-bits-2 :8h bytes :16b)
               (inst ushll2 32-bits-2 :4s 16-bits :8h)

               (inst ushll 32-bits-3 :4s 16-bits-2 :4h)
               (inst ushll2 32-bits-4 :4s 16-bits-2 :8h)
               (inst stp 32-bits 32-bits-2 (@ 32-bit-array 64 :post-index))
               (inst stp 32-bits-3 32-bits-4 (@ 32-bit-array -32))

               start
               (inst cmp byte-array end)
               (inst b :lt LOOP)

               DONE
               (inst sub res byte-array byte-array*))))
      (setf (sb-impl::buffer-head ibuf) (+ head copied))
      (+ start copied))))

(defun simd-copy-utf8-to-base-string (start end string ibuf)
  (declare (type index start end)
           (optimize speed (safety 0)))
  (with-pinned-objects-in-registers (string)
    (let* ((head (sb-impl::buffer-head ibuf))
           (tail (sb-impl::buffer-tail ibuf))
           (n (logand (min (- end start)
                           (- tail head))
                      -16))
           (copied
             (inline-vop (((byte-array* sap-reg t) (sb-impl::buffer-sap ibuf))
                          ((byte-array sap-reg t))
                          ((char-array sap-reg t) (vector-sap string))
                          ((ascii-mask complex-double-reg))
                          ((bytes complex-double-reg))
                          ((string-start any-reg) start)
                          ((end unsigned-reg))
                          ((head any-reg) head)
                          ((n any-reg) n)
                          ((temp complex-double-reg)))
                 ((res unsigned-reg unsigned-num))
               (inst movi ascii-mask 128 :16b)
               (inst add byte-array* byte-array* (lsr head 1))
               (inst mov byte-array byte-array*)
               (inst add end byte-array* (lsr n 1))
               (inst add char-array char-array (lsr string-start 1))
               (inst b start)

               LOOP
               (inst ldr bytes (@ byte-array))
               (inst s-and temp bytes ascii-mask)
               (inst umaxv temp temp :4s)
               (inst umov tmp-tn temp 0 :s)
               (inst cbnz tmp-tn DONE)
               (inst add byte-array byte-array 16)
               (inst str bytes (@ char-array 16 :post-index))

               start
               (inst cmp byte-array end)
               (inst b :lt LOOP)

               DONE
               (inst sub res byte-array byte-array*))))
      (setf (sb-impl::buffer-head ibuf) (+ head copied))
      (+ start copied))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun concat-ub8 (ub8s)
    (let ((result 0))
      (loop for ub8 in ub8s
            do (setf result (logior (ash result 8) ub8)))
      result)))

(defun simd-copy-utf8-crlf-to-base-string (start end string ibuf)
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
        (with-pinned-objects-in-registers (string)
          (multiple-value-bind (new-head copied)
              (inline-vop (((byte-array* sap-reg t) (sb-impl::buffer-sap ibuf))
                           ((byte-array sap-reg t))
                           ((char-array* sap-reg t) (vector-sap string))
                           ((char-array sap-reg t))
                           ((ascii-mask complex-double-reg))
                           ((crlf-mask complex-double-reg))
                           ((bytes complex-double-reg))
                           ((next-bytes complex-double-reg))
                           ((shifted complex-double-reg))
                           ((temp complex-double-reg))
                           ((temp2 complex-double-reg))
                           ((temp3 complex-double-reg))
                           ((string-start any-reg) start)
                           ((end any-reg) n)
                           ((head any-reg) head)
                           ((bit-mask complex-double-reg))
                           ((shuffle-table sap-reg) (vector-sap shuffle-table))
                           ((shuffle-mask complex-double-reg))
                           ((shuffle-mask2 complex-double-reg))
                           ((count unsigned-reg)))
                  ((new-head unsigned-reg positive-fixnum :from :load)
                   (copied unsigned-reg positive-fixnum :from :load))
                (inst movi ascii-mask 128 :16b)
                (inst mov tmp-tn #x0A0D)
                (inst dup crlf-mask tmp-tn :8h)

                (inst add byte-array byte-array* (lsr head 1))
                (inst add end byte-array (lsr end 1))

                (inst add char-array* char-array* (lsr string-start 1))
                (inst mov char-array char-array*)

                (load-inline-constant bit-mask :oword (concat-ub8 (append (loop for i downfrom 7 to 0
                                                                                collect (ash 1 i))
                                                                          (loop for i downfrom 7 to 0
                                                                                collect (ash 1 i)))))
                (inst ldr next-bytes (@ byte-array))
                (inst s-and temp next-bytes ascii-mask)
                (inst umaxv temp temp :4s)
                (inst umov tmp-tn temp 0 :s)
                (inst cbnz tmp-tn DONE)

                LOOP
                (inst add byte-array byte-array 16)
                (inst s-mov bytes next-bytes)
                (inst ldr next-bytes (@ byte-array))

                (inst s-and temp next-bytes ascii-mask)
                (inst umaxv temp temp :4s)
                (inst umov tmp-tn temp 0 :s)
                (inst cbnz tmp-tn DONE)

                ;; Shift bytes right to find CRLF starting at odd indexes
                ;; and grab the first byte from the next vector to check if it
                ;; it's an LF
                (inst ext shifted bytes next-bytes 1 :16b)
                ;; Compare both variants
                (inst cmeq temp bytes crlf-mask :8h)
                (inst cmeq temp2 shifted crlf-mask :8h)


                ;; SLI retains the destination parts, matching elements
                ;; will have FFFF, shifting and inserting will combine
                ;; them with zeros producing just one FF
                (inst sli temp temp2 :8h 8)

                ;; Count matches
                (inst ushr temp3 temp :16b 7)
                (inst addv temp2 temp3 :8b)
                (inst fmov count (reg-in-sc temp2 'single-reg))

                ;; bit-mask has powers of two for each byte index,
                ;; adding them together will produce an 8-bit mask.
                (inst s-and temp2 temp bit-mask)

                (inst addv temp temp2 :8b)
                (inst fmov tmp-tn (reg-in-sc temp 'single-reg))
                (inst ldr (reg-in-sc shuffle-mask 'double-reg) (@ shuffle-table (extend tmp-tn :lsl 3)))
                (inst tbl temp (list bytes) shuffle-mask :8b)
                (inst str (reg-in-sc temp 'double-reg) (@ char-array 8 :post-index))
                (inst sub char-array char-array count)

                ;; Second half

                ;; Count matches
                (inst ins temp3 0 temp3 1 :d)
                (inst addv temp3 temp3 :8b)
                (inst fmov count (reg-in-sc temp3 'single-reg))

                (inst ins temp2 0 temp2 1 :d)
                (inst addv temp2 temp2 :8b)
                (inst fmov tmp-tn (reg-in-sc temp2 'single-reg))

                (inst ldr (reg-in-sc shuffle-mask2 'double-reg) (@ shuffle-table (extend tmp-tn :lsl 3)))
                (inst ins bytes 0 bytes 1 :d)
                (inst tbl temp (list bytes) shuffle-mask2 :8b)
                (inst str (reg-in-sc temp 'double-reg) (@ char-array 8 :post-index))
                (inst sub char-array char-array count)

                (inst cmp byte-array end)
                (inst b :lt LOOP)

                DONE
                (inst sub copied char-array char-array*)
                (inst sub new-head byte-array byte-array*))
            (setf (sb-impl::buffer-head ibuf) new-head)
            (truly-the index (+ start copied)))))))

(defun simd-copy-utf8-crlf-to-character-string (start end string ibuf)
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
        (with-pinned-objects-in-registers (string)
          (multiple-value-bind (new-head copied)
              (inline-vop (((byte-array* sap-reg t) (sb-impl::buffer-sap ibuf))
                           ((byte-array sap-reg t))
                           ((char-array* sap-reg t) (vector-sap string))
                           ((char-array sap-reg t))
                           ((ascii-mask complex-double-reg))
                           ((crlf-mask complex-double-reg))
                           ((bytes complex-double-reg))
                           ((next-bytes complex-double-reg))
                           ((shifted complex-double-reg))
                           ((temp complex-double-reg))
                           ((temp2 complex-double-reg))
                           ((temp3 complex-double-reg))
                           ((string-start any-reg) start)
                           ((end any-reg) n)
                           ((head any-reg) head)
                           ((bit-mask complex-double-reg))
                           ((shuffle-table sap-reg) (vector-sap shuffle-table))
                           ((shuffle-mask complex-double-reg))
                           ((shuffle-mask2 complex-double-reg))
                           ((16-bits complex-double-reg))
                           ((16-bits-2 complex-double-reg))
                           ((32-bits complex-double-reg))
                           ((32-bits-2 complex-double-reg))
                           ((count unsigned-reg)))
                  ((new-head unsigned-reg positive-fixnum :from :load)
                   (copied unsigned-reg positive-fixnum :from :load))
                (inst movi ascii-mask 128 :16b)
                (inst mov tmp-tn #x0A0D)
                (inst dup crlf-mask tmp-tn :8h)

                (inst add byte-array byte-array* (lsr head 1))
                (inst add end byte-array (lsr end 1))

                (inst add char-array* char-array* (lsl string-start (- 2 1)))
                (inst mov char-array char-array*)

                (load-inline-constant bit-mask :oword (concat-ub8 (append (loop for i downfrom 7 to 0
                                                                                collect (ash 1 i))
                                                                          (loop for i downfrom 7 to 0
                                                                                collect (ash 1 i)))))
                (inst ldr next-bytes (@ byte-array))
                (inst s-and temp next-bytes ascii-mask)
                (inst umaxv temp temp :4s)
                (inst umov tmp-tn temp 0 :s)
                (inst cbnz tmp-tn DONE)

                LOOP
                (inst add byte-array byte-array 16)
                (inst s-mov bytes next-bytes)
                (inst ldr next-bytes (@ byte-array))

                (inst s-and temp next-bytes ascii-mask)
                (inst umaxv temp temp :4s)
                (inst umov tmp-tn temp 0 :s)
                (inst cbnz tmp-tn DONE)

                ;; Shift bytes right to find CRLF starting at odd indexes
                ;; and grab the first byte from the next vector to check if it
                ;; it's an LF
                (inst ext shifted bytes next-bytes 1 :16b)
                ;; Compare both variants
                (inst cmeq temp bytes crlf-mask :8h)
                (inst cmeq temp2 shifted crlf-mask :8h)


                ;; SLI retains the destination parts, matching elements
                ;; will have FFFF, shifting and inserting will combine
                ;; them with zeros producing just one FF
                (inst sli temp temp2 :8h 8)

                ;; Count matches
                (inst ushr temp3 temp :16b 7)
                (inst addv temp2 temp3 :8b)
                (inst fmov count (reg-in-sc temp2 'single-reg))

                ;; bit-mask has powers of two for each byte index,
                ;; adding them together will produce an 8-bit mask.
                (inst s-and temp2 temp bit-mask)

                (inst addv temp temp2 :8b)
                (inst fmov tmp-tn (reg-in-sc temp 'single-reg))
                (inst ldr (reg-in-sc shuffle-mask 'double-reg) (@ shuffle-table (extend tmp-tn :lsl 3)))
                (inst tbl temp (list bytes) shuffle-mask :8b)

                ;; Widen
                (inst ushll 16-bits :8h temp :8b)
                (inst ushll 32-bits :4s 16-bits :4h)
                (inst ushll2 16-bits-2 :8h temp :16b)
                (inst ushll2 32-bits-2 :4s 16-bits :8h)
                (inst stp 32-bits 32-bits-2 (@ char-array 32 :post-index))
                (inst sub char-array char-array (lsl count 2))

                ;; Second half

                ;; Count matches
                (inst ins temp3 0 temp3 1 :d)
                (inst addv temp3 temp3 :8b)
                (inst fmov count (reg-in-sc temp3 'single-reg))

                (inst ins temp2 0 temp2 1 :d)
                (inst addv temp2 temp2 :8b)
                (inst fmov tmp-tn (reg-in-sc temp2 'single-reg))

                (inst ldr (reg-in-sc shuffle-mask2 'double-reg) (@ shuffle-table (extend tmp-tn :lsl 3)))
                (inst ins bytes 0 bytes 1 :d)
                (inst tbl temp (list bytes) shuffle-mask2 :8b)

                (inst ushll 16-bits :8h temp :8b)
                (inst ushll 32-bits :4s 16-bits :4h)
                (inst ushll2 16-bits-2 :8h temp :16b)
                (inst ushll2 32-bits-2 :4s 16-bits :8h)
                (inst stp 32-bits 32-bits-2 (@ char-array 32 :post-index))
                (inst sub char-array char-array (lsl count 2))

                (inst cmp byte-array end)
                (inst b :lt LOOP)

                DONE
                (inst sub copied char-array char-array*)
                (inst sub new-head byte-array byte-array*))
            (setf (sb-impl::buffer-head ibuf) new-head)
            (truly-the index (+ start (ash copied -2))))))))
