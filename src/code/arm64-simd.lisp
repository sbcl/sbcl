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
    (make-random-tn (sc-or-lose sc) (tn-offset tn))))

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
  `(multiple-value-prog1 (progn ,@body)
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
                     (inst sub temp bits a-mask :4s)
                     (inst cmhs temp z-mask temp :4s)
                     (inst and temp temp flip :16b)
                     (inst eor res bits temp :16b)))))))

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

      (inst rev64 vl vl :16b)
      (inst ext vl vl vl 8 :16b)
      (inst rev64 vr vr :16b)
      (inst ext vr vr vr 8 :16b)

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
      (inst ext vl vl vl 8 :16b)
      (inst rev64 vr vr :4s)
      (inst ext vr vr vr 8 :16b)

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

      (inst rev64 v v :16b)
      (inst ext v v v 8 :16b)
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
      (inst ext v v v 8 :16b)
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
      (inst sub temp a a-mask :16b)
      (inst cmhs temp z-mask temp :16b)
      (inst and temp temp flip :16b)
      (inst eor a a temp :16b)

      ;; Upcase b
      (inst sub temp2 b a-mask :16b)
      (inst cmhs temp2 z-mask temp2 :16b)
      (inst and temp2 temp2 flip :16b)
      (inst eor b b temp2 :16b)

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
      (inst sub temp characters a-mask :4s)
      (inst cmhs temp z-mask temp :4s)
      (inst and temp temp flip :16b)
      (inst eor characters characters temp :16b)

      ;; Widen 8-bit wide characters to 32-bits
      (inst ushll base-chars :8h base-chars :8b 0)
      (inst ushll base-chars :4s base-chars :4h 0)

      ;; And upcase them too
      (inst sub temp2 base-chars a-mask :4s)
      (inst cmhs temp2 z-mask temp2 :4s)
      (inst and temp2 temp2 flip :16b)
      (inst eor base-chars base-chars temp2 :16b)

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun check-ascii (input temp not-ascii-label &optional (size 1))
    ;; Check for ASCII by looking at the largest byte
    (multiple-value-bind (v-size size) (ecase size
                                         (1 (values :16b :b))
                                         (4 (values :4s :s)))
      (inst umaxv temp input v-size)
      (inst umov tmp-tn temp 0 size)
      (inst cmp tmp-tn 127)
      (inst b :hi not-ascii-label))))

(defun utf8-to-character-string (start end string ibuf)
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
                          ((bytes complex-double-reg))
                          ((16-bits complex-double-reg))
                          ((16-bits-2 complex-double-reg))
                          ((32-bits complex-double-reg))
                          ((32-bits-2 complex-double-reg))
                          ((32-bits-3 complex-double-reg))
                          ((32-bits-4 complex-double-reg))
                          ((temp complex-double-reg)))
                 ((res unsigned-reg unsigned-num))
               (inst add byte-array* byte-array* (lsr head 1))
               (inst mov byte-array byte-array*)
               (inst add end byte-array* (lsr n 1))
               (inst add 32-bit-array 32-bit-array (lsr string-start 1))
               (inst b start)

               LOOP
               (inst ldr bytes (@ byte-array))
               (check-ascii bytes temp DONE)

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

#+sb-unicode
(defun ascii-sap-to-character-string (sap string length)
  (declare (optimize speed (safety 0))
           (system-area-pointer sap)
           (index length))
  (let ((n (logand length -16)))
    (with-pinned-objects-in-registers (string)
      (inline-vop (((byte-array* sap-reg t) sap)
                   ((byte-array sap-reg t))
                   ((32-bit-array sap-reg t) (vector-sap string))
                   ((end unsigned-reg))
                   ((n any-reg) n)
                   ((bytes complex-double-reg))
                   ((16-bits complex-double-reg))
                   ((16-bits-2 complex-double-reg))
                   ((32-bits complex-double-reg t :offset 4))
                   ((32-bits-2 complex-double-reg t :offset 5))
                   ((32-bits-3 complex-double-reg t :offset 6))
                   ((32-bits-4 complex-double-reg t :offset 7)))
          ()
        (inst add end byte-array* (lsr n 1))
        (inst mov byte-array byte-array*)
        (inst b start)

        LOOP
        (inst ldr bytes (@ byte-array 16 :post-index))

        (inst ushll 16-bits :8h bytes :8b)
        (inst ushll 32-bits :4s 16-bits :4h)

        (inst ushll2 16-bits-2 :8h bytes :16b)
        (inst ushll2 32-bits-2 :4s 16-bits :8h)

        (inst ushll 32-bits-3 :4s 16-bits-2 :4h)
        (inst ushll2 32-bits-4 :4s 16-bits-2 :8h)
        (inst st1 (list 32-bits 32-bits-2 32-bits-3 32-bits-4) (@ 32-bit-array 64 :post-index) :4s)

        start
        (inst cmp byte-array end)
        (inst b :lt LOOP)

        DONE))
    (loop for i from n below length
          do (setf (aref string i)
                   (code-char (sap-ref-8 sap i))))))

(defun character-string-to-ascii-byte-array (byte-array string length)
  (declare (index length)
           (simple-character-string string)
           ((simple-array (unsigned-byte 8) (*)) byte-array)
           (optimize speed (safety 0)))
  (with-pinned-objects-in-registers (string byte-array)
    (inline-vop (((byte-array sap-reg t) (vector-sap byte-array))
                 ((32-bit-array sap-reg t) (vector-sap string))
                 ((n unsigned-reg) (logand (+ (* length 4) 15) -16))
                 ((bytes complex-double-reg t :offset 1))
                 ((bytes2 complex-double-reg t :offset 2))
                 ((bytes3 complex-double-reg t :offset 3))
                 ((bytes4 complex-double-reg t :offset 4)))
        ()
      (inst cmp n 64)
      (inst b :lt TAIL)

      LOOP

      (inst ld1 (list bytes bytes2 bytes3 bytes4) (@ 32-bit-array 64 :post-index) :4s)

      (inst uzp1 bytes2 bytes bytes2 :8h)
      (inst uzp1 bytes4 bytes3 bytes4 :8h)
      (inst uzp1 bytes4 bytes2 bytes4 :16b)
      (inst str  bytes4 (@ byte-array 16 :post-index))
      (inst sub n n 64)
      (inst cmp n 64)
      (inst b :ge LOOP)

      TAIL
      (inst cbz n DONE)
      (inst movi bytes2 0 :2d)
      (inst movi bytes3 0 :2d)

      (inst tbz n 5 ONE)
      (inst tbz n 4 TWO)

      (inst ldr bytes3 (@ 32-bit-array 32))
      (inst uzp1 bytes3 bytes3 bytes2 :8h)
      TWO
      (inst ldr bytes2 (@ 32-bit-array 16))
      ONE
      (inst ldr bytes (@ 32-bit-array))

      (inst uzp1 bytes2 bytes bytes2 :8h)
      (inst uzp1 bytes3 bytes2 bytes3 :16b)
      (inst str  bytes3 (@ byte-array))

      DONE))
  byte-array)

(defun utf8-to-base-string (start end string ibuf)
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
                          ((bytes complex-double-reg))
                          ((string-start any-reg) start)
                          ((end unsigned-reg))
                          ((head any-reg) head)
                          ((n any-reg) n)
                          ((temp complex-double-reg)))
                 ((res unsigned-reg unsigned-num))
               (inst add byte-array* byte-array* (lsr head 1))
               (inst mov byte-array byte-array*)
               (inst add end byte-array* (lsr n 1))
               (inst add char-array char-array (lsr string-start 1))
               (inst b start)

               LOOP
               (inst ldr bytes (@ byte-array))
               (check-ascii bytes temp DONE)
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
  (defun concat-ub (size ubs)
    (let ((result 0))
      (loop for ub in ubs
            do (setf result (logior (ash result size) ub)))
      result)))

(defun utf8-crlf-to-base-string (start end string ibuf)
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
        (with-pinned-objects-in-registers (string shuffle-table)
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
                (inst mov tmp-tn #x0A0D)
                (inst dup crlf-mask tmp-tn :8h)

                (inst add byte-array byte-array* (lsr head 1))
                (inst add end byte-array (lsr end 1))

                (inst add char-array* char-array* (lsr string-start 1))
                (inst mov char-array char-array*)

                (load-inline-constant bit-mask :oword
                                      (concat-ub 8 (append (loop for i downfrom 7 to 0
                                                                 collect (ash 1 i))
                                                           (loop for i downfrom 7 to 0
                                                                 collect (ash 1 i)))))
                (inst ldr next-bytes (@ byte-array))
                (check-ascii next-bytes temp DONE)

                LOOP
                (inst mov bytes next-bytes :16b)
                (inst ldr next-bytes (@ byte-array 16))
                (check-ascii next-bytes temp DONE)

                (inst add byte-array byte-array 16)


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
                (inst sli temp temp2 8 :8h)

                ;; Count matches
                (inst ushr temp3 temp 7 :16b)
                (inst addv temp2 temp3 :8b)
                (inst fmov count (reg-in-sc temp2 'single-reg))

                ;; bit-mask has powers of two for each byte index,
                ;; adding them together will produce an 8-bit mask.
                (inst and temp2 temp bit-mask :16b)

                (inst addv temp temp2 :8b)
                (inst fmov tmp-tn (reg-in-sc temp 'single-reg))
                (inst ldr shuffle-mask (@ shuffle-table (lsl tmp-tn 3)) :d)
                (inst tbl temp (list bytes) shuffle-mask :8b)
                (inst str temp (@ char-array 8 :post-index) :d)
                (inst sub char-array char-array count)

                ;; Second half

                ;; Count matches
                (inst ins temp3 0 temp3 1 :d)
                (inst addv temp3 temp3 :8b)
                (inst fmov count (reg-in-sc temp3 'single-reg))

                (inst ins temp2 0 temp2 1 :d)
                (inst addv temp2 temp2 :8b)
                (inst fmov tmp-tn (reg-in-sc temp2 'single-reg))

                (inst ldr shuffle-mask2 (@ shuffle-table (lsl tmp-tn 3)) :d)
                (inst ins bytes 0 bytes 1 :d)
                (inst tbl temp (list bytes) shuffle-mask2 :8b)
                (inst str temp (@ char-array 8 :post-index) :d)
                (inst sub char-array char-array count)

                (inst cmp byte-array end)
                (inst b :lt LOOP)

                DONE
                (inst sub copied char-array char-array*)
                (inst sub new-head byte-array byte-array*))
            (setf (sb-impl::buffer-head ibuf) new-head)
            (truly-the index (+ start copied)))))))

(defun utf8-crlf-to-character-string (start end string ibuf)
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
        (with-pinned-objects-in-registers (string shuffle-table)
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
                           ((32-bits complex-double-reg))
                           ((32-bits-2 complex-double-reg))
                           ((count unsigned-reg)))
                  ((new-head unsigned-reg positive-fixnum :from :load)
                   (copied unsigned-reg positive-fixnum :from :load))
                (inst mov tmp-tn #x0A0D)
                (inst dup crlf-mask tmp-tn :8h)

                (inst add byte-array byte-array* (lsr head 1))
                (inst add end byte-array (lsr end 1))

                (inst add char-array* char-array* (lsl string-start (- 2 1)))
                (inst mov char-array char-array*)

                (load-inline-constant bit-mask :oword
                                      (concat-ub 8 (append (loop for i downfrom 7 to 0
                                                                 collect (ash 1 i))
                                                           (loop for i downfrom 7 to 0
                                                                 collect (ash 1 i)))))
                (inst ldr next-bytes (@ byte-array))
                (check-ascii next-bytes temp DONE)

                LOOP
                (inst mov bytes next-bytes :16b)
                (inst ldr next-bytes (@ byte-array 16))
                (check-ascii next-bytes temp DONE)
                (inst add byte-array byte-array 16)

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
                (inst sli temp temp2 8 :8h)

                ;; Count matches
                (inst ushr temp3 temp 7 :16b)
                (inst addv temp2 temp3 :8b)
                (inst fmov count (reg-in-sc temp2 'single-reg))

                ;; bit-mask has powers of two for each byte index,
                ;; adding them together will produce an 8-bit mask.
                (inst and temp2 temp bit-mask :16b)

                (inst addv temp temp2 :8b)
                (inst fmov tmp-tn (reg-in-sc temp 'single-reg))
                (inst ldr shuffle-mask (@ shuffle-table (lsl tmp-tn 3)) :d)
                (inst tbl temp (list bytes) shuffle-mask :8b)

                ;; Widen
                (inst ushll 16-bits :8h temp :8b)
                (inst ushll 32-bits :4s 16-bits :4h)
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

                (inst ldr shuffle-mask2 (@ shuffle-table (lsl tmp-tn 3)) :d)
                (inst ins bytes 0 bytes 1 :d)
                (inst tbl temp (list bytes) shuffle-mask2 :8b)

                (inst ushll 16-bits :8h temp :8b)
                (inst ushll 32-bits :4s 16-bits :4h)
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

(defun character-string-to-utf8 (start end string obuf)
  (declare (type index start end)
           (optimize speed (safety 0)))
  (with-pinned-objects-in-registers (string)
    (let* ((tail (sb-impl::buffer-tail obuf))
           (buffer-left (- (sb-impl::buffer-length obuf) tail))
           (string-left (- end start))
           (n (logand (min buffer-left string-left) -16))
           (string-start (truly-the fixnum (* start 4))))
      (multiple-value-bind (copied last-newline)
          (inline-vop (((byte-array* sap-reg t) (sb-impl::buffer-sap obuf))
                       ((byte-array sap-reg t))
                       ((32-bit-array sap-reg t) (vector-sap string))
                       ((string-start any-reg) string-start)
                       ((end unsigned-reg))
                       ((tail any-reg) tail)
                       ((n any-reg) n)
                       ((newlines complex-double-reg))
                       ((bytes complex-double-reg))
                       ((bytes2 complex-double-reg))
                       ((bytes3 complex-double-reg))
                       ((bytes4 complex-double-reg))
                       ((temp complex-double-reg))
                       ((temp2 complex-double-reg))
                       ((indexes))
                       ((increment))
                       ((last-newlines)))
              ((res unsigned-reg unsigned-num)
               (last-newline signed-reg signed-num))
            (inst movi newlines 10 :4s)
            (inst movi increment 4 :4s)
            (inst mvni last-newlines 0 :4s)
            (load-inline-constant indexes :oword (concat-ub 32 '(3 2 1 0)))
            (inst add byte-array* byte-array* (lsr tail 1))
            (inst mov byte-array byte-array*)
            (inst add end byte-array* (lsr n 1))
            (inst add 32-bit-array 32-bit-array (lsr string-start 1))
            (inst b start)

            LOOP
            (inst ldp bytes bytes2 (@ 32-bit-array))
            (inst ldp bytes3 bytes4 (@ 32-bit-array 32))

            (inst orr temp bytes bytes2 :16b)
            (inst orr temp2 bytes3 bytes4 :16b)
            (inst orr temp temp temp2 :16b)
            (check-ascii temp temp DONE 4)

            ;; Find newlines
            (loop for bytes in (list bytes bytes2 bytes3 bytes4)
                  do
                  (inst cmeq temp bytes newlines :4s)
                  (inst bit last-newlines indexes temp :16b)
                  (inst add indexes indexes increment :4s))

            (inst add 32-bit-array 32-bit-array 64)

            (inst uzp1 bytes2 bytes bytes2 :8h)
            (inst uzp1 bytes4 bytes3 bytes4 :8h)
            (inst uzp1 bytes4 bytes2 bytes4 :16b)
            (inst str  bytes4 (@ byte-array 16 :post-index))
            start
            (inst cmp byte-array end)
            (inst b :lt LOOP)

            DONE
            (inst sub res byte-array byte-array*)
            (inst smaxv temp last-newlines :4s)
            (inst smov last-newline temp 0 :s))
        (setf (sb-impl::buffer-tail obuf) (+ tail copied))
        (values (+ start copied)
                (if (>= last-newline 0)
                    (truly-the index (+ start last-newline))
                    -1))))))

(defun simd-position8 (element vector start end)
  (declare (type index start end)
           (optimize speed (safety 0)))
  (with-pinned-objects-in-registers (vector)
    (inline-vop (((vector* sap-reg t) (vector-sap vector))
                 ((start any-reg) start)
                 ((end* any-reg) end)
                 ((element unsigned-reg) element)
                 ((length))
                 ((diff signed-reg))
                 ((end))
                 ((vector sap-reg t))
                 ((vector-start))
                 ((bytes complex-double-reg))
                 ((cmp complex-double-reg))
                 ((search)))
        ((res descriptor-reg t :from :load))
      (inst mov res null-tn)
      (inst dup search element :16b)
      (inst add vector vector* (lsr start 1))
      (inst add end vector* (lsr end* 1))

      (inst sub length end vector)
      (inst cbz length done)

      ;; Align up
      (inst add diff length 15)
      (inst and diff diff -16)

      ;; How much to read before the start.
      ;; Vector header and length are 16-byte long, so it's always safe.
      (inst sub diff diff length)
      (inst sub vector-start vector diff)

      (inst ldr bytes (@ vector-start))
      (inst cmeq cmp bytes search :16b)
      (inst shrn cmp cmp 4 :8b)

      (inst fmov length cmp)

      ;; Discard the matching padding bits, multiplied by 4 because
      ;; each matching byte is 4-bit long after shrn.
      (inst lsl tmp-tn diff 2)
      (inst lsr length length tmp-tn)

      (inst cbnz length FOUND)

      (inst add vector vector-start 16)

      LOOP
      (inst cmp vector end)
      (inst b :eq DONE)
      (inst ldr bytes (@ vector))
      (inst cmeq cmp bytes search :16b)
      (inst shrn cmp cmp 4 :8b)
      (inst fmov length cmp)
      (inst cbnz length FOUND)
      (inst add vector vector 16)
      (inst b LOOP)

      FOUND
      (inst rbit length length)
      (inst clz length length)
      (inst add vector vector (lsr length 2))

      (inst sub length vector vector*)
      (inst lsl res length n-fixnum-tag-bits)
      DONE)))

(defun simd-position8-from-end (element vector start end)
  (declare (type index start end)
           (optimize speed (safety 0)))
  (with-pinned-objects-in-registers (vector)
    (inline-vop (((vector* sap-reg t) (vector-sap vector))
                 ((start* any-reg) start)
                 ((end any-reg) end)
                 ((element unsigned-reg) element)
                 ((length))
                 ((padded-length))
                 ((start))
                 ((padded))
                 ((found-bits))
                 ((vector sap-reg t))
                 ((bytes complex-double-reg))
                 ((cmp complex-double-reg))
                 ((search)))
        ((res descriptor-reg t :from :load))
      (inst mov res null-tn)
      (inst dup search element :16b)
      (inst add vector vector* (lsr end 1))

      (inst add start vector* (lsr start* 1))

      (inst sub length vector start)
      (inst cbz length done)

      ;; Align the start to 16-bytes and then process the tail.
      (inst add padded-length length 15)
      (inst and padded-length padded-length -16)
      (inst sub padded vector padded-length)

      LOOP
      (inst sub vector vector 16)
      (inst cmp vector padded)
      (inst b :le TAIL)

      (inst ldr bytes (@ vector))
      (inst cmeq cmp bytes search :16b)
      (inst shrn cmp cmp 4 :8b)
      (inst fmov found-bits cmp)
      (inst cbnz found-bits FOUND)

      (inst b LOOP)

      TAIL
      ;; Read past the start if needed.
      ;; Vector header and length are 16-byte long, making it safe.
      (inst ldr bytes (@ vector))
      (inst cmeq cmp bytes search :16b)
      (inst shrn cmp cmp 4 :8b)

      ;; Clear the extra bits
      (inst sub padded padded-length length)

      (inst add padded padded 16)
      (inst lsl padded padded 2)

      (inst mov length -1)
      (inst lsl padded length padded)

      (inst fmov found-bits cmp)
      (inst and found-bits found-bits padded)
      (inst cbz found-bits DONE)


      FOUND
      (inst clz found-bits found-bits)
      (inst eor found-bits found-bits 63)
      (inst add vector vector (lsr found-bits 2))

      (inst sub found-bits vector vector*)
      (inst lsl res found-bits n-fixnum-tag-bits)
      DONE)))

(defun simd-position32 (element vector start end)
  (declare (type index start end)
           (optimize speed (safety 0)))
  (with-pinned-objects-in-registers (vector)
    (inline-vop (((vector* sap-reg t) (vector-sap vector))
                 ((start any-reg) start)
                 ((end* any-reg) end)
                 ((element unsigned-reg) element)
                 ((length))
                 ((diff signed-reg))
                 ((end))
                 ((vector sap-reg t))
                 ((vector-start))
                 ((bytes complex-double-reg))
                 ((cmp complex-double-reg))
                 ((search)))
        ((res descriptor-reg t :from :load))
      (inst mov res null-tn)
      (inst dup search element :4s)
      (inst add vector vector* (lsl start 1))
      (inst add end vector* (lsl end* 1))

      (inst sub length end vector)
      (inst cbz length done)

      ;; Align up
      (inst add diff length 15)
      (inst and diff diff -16)

      ;; How much to read before the start.
      ;; Vector header and length are 16-byte long, so it's always safe.
      (inst sub diff diff length)
      (inst sub vector-start vector diff)

      (inst ldr bytes (@ vector-start))
      (inst cmeq cmp bytes search :4s)
      (inst shrn cmp cmp 4 :8b)

      (inst fmov length cmp)

      ;; Discard the matching padding bits, multiplied by 4 because
      ;; each matching byte is 4-bit long after shrn.
      (inst lsl tmp-tn diff 2)
      (inst lsr length length tmp-tn)

      (inst cbnz length FOUND)

      (inst add vector vector-start 16)

      LOOP
      (inst cmp vector end)
      (inst b :eq DONE)
      (inst ldr bytes (@ vector))
      (inst cmeq cmp bytes search :4s)
      (inst shrn cmp cmp 4 :8b)
      (inst fmov length cmp)
      (inst cbnz length FOUND)
      (inst add vector vector 16)
      (inst b LOOP)

      FOUND
      (inst rbit length length)
      (inst clz length length)
      (inst add vector vector (lsr length 2))

      (inst sub length vector vector*)
      (inst lsr res length 1)
      DONE)))

(defun simd-position32-from-end (element vector start end)
  (declare (type index start end)
           (optimize speed (safety 0)))
  (with-pinned-objects-in-registers (vector)
    (inline-vop (((vector* sap-reg t) (vector-sap vector))
                 ((start* any-reg) start)
                 ((end any-reg) end)
                 ((element unsigned-reg) element)
                 ((found-bits))
                 ((start))
                 ((length))
                 ((padded))
                 ((padded-length))
                 ((vector sap-reg t))
                 ((bytes complex-double-reg))
                 ((cmp complex-double-reg))
                 ((search)))
        ((res descriptor-reg t :from :load))
      (inst mov res null-tn)
      (inst dup search element :4s)
      (inst add vector vector* (lsl end 1))

      (inst add start vector* (lsl start* 1))

      (inst sub length vector start)
      (inst cbz length done)

      ;; Align the start to 16-bytes and then process the tail.
      (inst add padded-length length 15)
      (inst and padded-length padded-length -16)
      (inst sub padded vector padded-length)

      LOOP
      (inst sub vector vector 16)
      (inst cmp vector padded)
      (inst b :le TAIL)
      (inst ldr bytes (@ vector))
      (inst cmeq cmp bytes search :4s)
      (inst shrn cmp cmp 4 :8b)
      (inst fmov found-bits cmp)
      (inst cbnz found-bits FOUND)
      (inst b LOOP)

      TAIL
      ;; Read past the start if needed.
      ;; Vector header and length are 16-byte long, making it safe.
      (inst ldr bytes (@ vector))
      (inst cmeq cmp bytes search :4s)
      (inst shrn cmp cmp 4 :8b)

      ;; Clear the extra bits
      (inst sub padded padded-length length)

      (inst add padded padded 16)
      (inst lsl padded padded 2)

      (inst mov length -1)
      (inst lsl padded length padded)

      (inst fmov found-bits cmp)
      (inst and found-bits found-bits padded)
      (inst cbz found-bits DONE)



      FOUND
      (inst lsr found-bits found-bits 15)
      (inst clz found-bits found-bits)
      (inst eor found-bits found-bits 63)

      (inst add vector vector (lsr found-bits 2))

      (inst sub found-bits vector vector*)
      (inst lsr res found-bits 1)
      DONE)))

(defun utf8-strlen (sap)
  (declare (system-area-pointer sap)
           (optimize speed (safety 0)))
  (inline-vop
      (((bytes        sap-reg t) sap)
       ((ptr          sap-reg t))

       ((total-conts  unsigned-reg))
       ((tmp          unsigned-reg))

       ((tbl1         complex-double-reg))
       ((tbl2         complex-double-reg))
       ((tbl3         complex-double-reg))
       ((tbl4         complex-double-reg))

       ((nibble-mask  complex-double-reg))
       ((twos         complex-double-reg))
       ((indexes      complex-double-reg))
       ((c-c1         complex-double-reg))
       ((c-c0         complex-double-reg))
       ((errors       complex-double-reg))
       ((prev         complex-double-reg))
       ((prev-len     complex-double-reg))

       ((current      complex-double-reg))
       ((tmp1         complex-double-reg))
       ((tmp2         complex-double-reg))
       ((tmp3         complex-double-reg))
       ((tmp4         complex-double-reg)))

      ((char-length descriptor-reg t :from :load)
       (byte-length unsigned-reg positive-fixnum :from :load)
       (all-ascii descriptor-reg))
    (flet ((validate (&optional last)
             (assemble ()
               ;; Skip an all-ASCII block
               (inst umax tmp2 current prev :16b)
               (inst umaxv tmp2 tmp2 :16b)
               (inst umov tmp tmp2 0 :b)
               (inst tbz tmp 7 VALIDATED)

               (inst ext tmp1 prev current 15 :16b)

               (inst tbnz tmp 5 full)

;;; 1/2 bytes

               ;; Identify continuations
               (inst cmgt tmp3 c-c0 current :16b)

               ;; Identify leading non-ascii bytes, shifted left by
               ;; one byte, with the previous byte shifted in
               (inst cmhi tmp1 tmp1 c-c1 :16b)

               (inst cmhi tmp4 current c-c1 :16b)

               ;; Continuations must follow leading bytes,
               ;; they must align with the shifted input
               (inst eor tmp1 tmp1 tmp3 :16b) ;; errors 1

               ;; Find #xC0 or #xC1, which are overlong
               (inst cmhs tmp2 current c-c0 :16b) ;; >= c0
               (inst bic tmp2 tmp2 tmp4 :16b) ;; tmp4 has a mask for > c1

               (inst orr tmp1 tmp1 tmp2 :16b) ;; combine errors
               (inst orr errors errors tmp1 :16b)

               (inst addv tmp3 tmp3 :16b)
               (inst smov tmp tmp3 0 :b)
               (inst sub total-conts total-conts tmp)
               (inst and prev-len tmp4 twos :16b)

               (inst b validated)

               FULL
               ;; The Keiser, Lemire algorithm
               (inst ushr tmp2 tmp1 4 :16b)
               (inst and tmp3 tmp1 nibble-mask :16b)
               (inst ushr tmp4 current 4 :16b)

               (inst tbl tmp2 (list tbl1) tmp2 :16b)
               (inst tbl tmp3 (list tbl2) tmp3 :16b)
               (inst tbl tmp4 (list tbl3) tmp4 :16b)

               (inst and tmp2 tmp2 tmp3 :16b)
               (inst and tmp2 tmp2 tmp4 :16b)
               (inst orr errors errors tmp2 :16b)

               (inst ushr tmp1 current 4 :16b)
               (inst tbl tmp1 (list tbl4) tmp1 :16b)

               (inst ext tmp2 prev-len tmp1 15 :16b)
               (inst ext tmp3 prev-len tmp1 14 :16b)
               (inst ext tmp4 prev-len tmp1 13 :16b)

               (inst ushr tmp2 tmp2 1 :16b)
               (inst ushr tmp3 tmp3 2 :16b)
               (inst ushr tmp4 tmp4 3 :16b)

               (inst orr tmp2 tmp2 tmp3 :16b)
               (inst orr tmp2 tmp2 tmp4 :16b)

               (inst ushr tmp3 current 6 :16b)
               (inst cmeq tmp3 tmp3 twos :16b)

               (inst cmtst tmp4 tmp2 tmp2 :16b)

               (inst eor tmp4 tmp3 tmp4 :16b)
               (inst orr errors errors tmp4 :16b)

               ;; Subtract continuations
               (inst addv tmp4 tmp3 :16b)
               (inst smov tmp tmp4 0 :b)
               (inst sub total-conts total-conts tmp)
               (unless last
                 (inst mov prev-len tmp1 :16b))
               VALIDATED)))
      (assemble ()
        ;; Align the start and then mask off the extra bits
        (inst and ptr bytes -16)
        (inst sub byte-length bytes ptr)

        (inst ldr current (@ ptr))

        (load-inline-constant indexes :oword #x0F0E0D0C0B0A09080706050403020100)

        ;; Replace the aligned bits with ones, avoiding null termination
        (inst dup tmp1 byte-length :16b)
        (inst cmhi tmp1 tmp1 indexes :16b)
        (inst bic current current tmp1 :16b)
        (inst sub current current tmp1 :16b)

        ASCII
        (inst uminv tmp1 current :16b)
        (inst fmov tmp (reg-in-sc tmp1 'single-reg))
        (inst cbz tmp ASCII-TAIL)
        (inst umaxv tmp1 current :16b)
        (inst fmov tmp (reg-in-sc tmp1 'single-reg))
        (inst tbnz tmp 7 NON-ASCII)


        (inst ldr current (@ ptr 16 :pre-index))
        (inst b ASCII)

        ASCII-TAIL

        ;; Find the first zero
        (inst cmtst tmp1 current current :16b)
        (inst orr tmp1 tmp1 indexes :16b)
        (inst uminv tmp1 tmp1 :16b)
        (inst fmov tmp (reg-in-sc tmp1 'single-reg))

        ;; Zero out the bytes after the first zero
        (inst dup tmp1 tmp :16b)
        (inst cmhi tmp1 tmp1 indexes :16b)
        (inst and current current tmp1 :16b)

        (inst sminv tmp1 current :16b)
        (inst fmov byte-length (reg-in-sc tmp1 'single-reg))
        (inst tbnz byte-length 7 NON-ASCII)

        (inst add ptr ptr tmp)
        (inst sub byte-length ptr bytes)
        (load-symbol all-ascii t)
        (inst mov char-length (lsl byte-length 1))
        (inst b DONE)

        NON-ASCII
        (inst mov char-length null-tn)
        (inst movi nibble-mask #x0f :16b)
        (inst movi twos 2 :16b)
        (inst mov total-conts 0)

        (load-inline-constant tbl1 :oword #x38060001000000000000000000000000)
        (load-inline-constant tbl2 :oword #x2020242020202020202020100000010B)
        (load-inline-constant tbl3 :oword #x202020203535332B2020202020202020)
        (load-inline-constant tbl4 :oword #x08040202000000000000000000000000)

        (inst movi errors   0 :16b)
        (inst movi prev     0 :16b)
        (inst movi prev-len 0 :16b)
        (inst movi c-c0 #xc0 :16b)
        (inst movi c-c1 #xc1 :16b)

        (inst b START)

        LOOP
        (inst ldr current (@ ptr))

        START
        (inst uminv tmp1 current :16b)
        (inst fmov tmp (reg-in-sc tmp1 'single-reg))
        (inst cbz tmp TAIL)

        (validate)

        (inst mov prev current :16b)

        (inst add ptr ptr 16)
        (inst b LOOP)

        TAIL
        ;; Find the first zero
        (inst cmtst tmp1 current current :16b)
        (inst orr tmp1 tmp1 indexes :16b)
        (inst uminv tmp1 tmp1 :16b)
        (inst fmov tmp (reg-in-sc tmp1 'single-reg))

        ;; Zero out the bytes after the first zero
        (inst dup tmp1 tmp :16b)
        (inst cmhi tmp1 tmp1 indexes :16b)
        (inst and current current tmp1 :16b)
        (inst add ptr ptr tmp)

        (validate t)

        (inst sub byte-length ptr bytes)
        (inst mov all-ascii null-tn)

        (inst umaxv errors errors :16b)
        (inst fmov tmp (reg-in-sc errors 'single-reg))
        (inst cbnz tmp DONE)

        (inst sub tmp-tn byte-length total-conts)
        (inst lsl char-length tmp-tn n-fixnum-tag-bits)
        DONE))))

(defun sb-impl::character-string-utf8-length (string)
  (declare (optimize speed (safety 0)))
  (with-pinned-objects-in-registers (string)
    (inline-vop
        (((ptr sap-reg t) (vector-sap string))
         ((length any-reg t) (length string))

         ((chars-left   unsigned-reg))
         ((tmp          unsigned-reg))

         ((c-7f         complex-double-reg))
         ((c-7ff        complex-double-reg))
         ((c-ffff       complex-double-reg))
         ((c-1b         complex-double-reg))

         ((extra-len    complex-double-reg))
         ((errors       complex-double-reg))

         ((current      complex-double-reg))
         ((tmp1         complex-double-reg))
         ((mask         complex-double-reg)))

        ((res descriptor-reg t :from :load)
         (all-ascii descriptor-reg t :from :load))
      (inst mov res length)

      (load-symbol all-ascii t)

      (inst cbz length DONE)

      (inst lsr chars-left length n-fixnum-tag-bits)

      ASCII-LOOP
      (inst ldr current (@ ptr 16 :post-index))

      (inst umaxv tmp1 current :4s)
      (inst fmov tmp (reg-in-sc tmp1 'single-reg))
      (inst cmp tmp 127)

      (inst b :hi NON-ASCII)
      (inst subs chars-left chars-left 4)
      (inst b :gt ASCII-LOOP)
      (inst b DONE)

      NON-ASCII
      (inst mov res null-tn)
      (inst mov all-ascii null-tn)
      (inst movi extra-len 0 :16b)
      (inst movi errors 0 :16b)

      (inst movi c-7f  #x7F :4s)
      (inst movi c-7ff #x7FF :4s)
      (inst movi c-ffff #x0000ffff0000ffff :2d)
      (inst movi c-1b  #x1b :4s)

      (inst b START)

      LOOP
      (inst subs chars-left chars-left 4)
      (inst b :le EXIT)
      (inst ldr current (@ ptr 16 :post-index))

      ;; ASCII fast path
      (inst umaxv tmp1 current :4s)
      (inst fmov tmp (reg-in-sc tmp1 'single-reg))
      (inst cmp tmp 127)
      (inst b :le LOOP)

      START
      ;; Check for surrogates #xD800-#xDFFF
      (inst ushr tmp1 current 11 :4s)
      (inst cmeq tmp1 tmp1 c-1b :4s)
      (inst orr errors errors tmp1 :16b)

      (inst cmhi tmp1 current c-7f :4s)

      (inst cmhi mask current c-7ff :4s)
      (inst add tmp1 tmp1 mask :4s)

      (inst cmhi mask current c-ffff :4s)
      (inst add tmp1 tmp1 mask :4s)

      (inst saddlp tmp1 tmp1 :4s)
      (inst sub extra-len extra-len tmp1 :2d)

      (inst b LOOP)

      EXIT
      (inst umaxv tmp1 errors :4s)
      (inst fmov tmp (reg-in-sc tmp1 'single-reg))
      (inst cbnz tmp DONE)

      (inst addp tmp1 extra-len extra-len :2d)
      (inst fmov tmp tmp1)

      (inst add res length (lsl tmp n-fixnum-tag-bits))
      DONE)))

(defun utf8-sap-to-character-string (sap string byte-array-length)
  (declare (index byte-array-length)
           (system-area-pointer sap)
           (simple-character-string string)
           (optimize speed (safety 0)))
  (with-pinned-objects-in-registers (string )
    (multiple-value-bind (byte-index char-index)
        (inline-vop (((byte-array* sap-reg t :target byte-array) sap)
                     ((string sap-reg t) (vector-sap string))
                     ((table any-reg t))
                     ((full-table any-reg t))
                     ((string-length unsigned-reg) (logand (+ (length string) 3) -4))
                     ((byte-array sap-reg t :from (:argument 0)))
                     ((byte-array-length unsigned-reg) byte-array-length)
                     ((index unsigned-reg))
                     ((suffix unsigned-reg))
                     ((ptr any-reg))
                     ((bytes complex-double-reg t))
                     ((bytes16 complex-double-reg t))
                     ((next complex-double-reg t))
                     ((temp complex-double-reg))
                     ((temp2 complex-double-reg t :offset 11))
                     ((temp3 complex-double-reg t :offset 12))
                     ((c-c0 complex-double-reg))
                     ((c-bf complex-double-reg))
                     ((powers double-reg))
                     ((c-ff complex-double-reg))
                     ((c-4 complex-double-reg))
                     ((shuf-low complex-double-reg t :offset 1))
                     ((shuf-high complex-double-reg t :offset 2))
                     ((chars-low complex-double-reg t :offset 5))
                     ((chars-high complex-double-reg t :offset 6))
                     ((s1 complex-double-reg t :offset 8))
                     ((s2 complex-double-reg t :offset 9))
                     ((tag-clear complex-double-reg))
                     ((continuations complex-double-reg))
                     ((starts complex-double-reg))
                     ((combined complex-double-reg))
                     ((is-lead16 complex-double-reg))
                     ((shuf complex-double-reg))
                     ((packed complex-double-reg))
                     ((count complex-double-reg)))
            ((byte-index unsigned-reg positive-fixnum :from :load)
             (char-index unsigned-reg positive-fixnum :from :load))

          (assemble ()
            (move byte-array byte-array*)
            (inst movi c-c0 #xC0 :16b)
            (inst mov byte-index 0)
            (inst mov char-index 0)
            (load-inline-constant powers :qword #x8040201008040201)
            (inst movi c-bf #xBF :8h)
            (load-inline-constant table
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
                                    table))
            (flet ((convert-1-2 (full)
                     (assemble ()
                       LOOP
                       (inst sub tmp-tn byte-array-length byte-index)
                       (inst cmp tmp-tn 9)
                       (inst b :lt DONE)

                       (inst add ptr byte-array byte-index)
                       (inst ldr bytes (@ ptr) :d)


                       (inst umaxv temp bytes :8b)
                       (inst umov index temp 0 :b)
                       (inst cmp index #xE0) ;; 3 or 4 bytes
                       (inst b :ge full)

                       (inst ldr next (@ ptr 1) :d)

                       ;; Build a bit pattern of non-continuation bytes
                       ;; suitable for the lookup table
                       (inst cmge continuations c-c0 bytes :8b)
                       (inst and starts powers continuations :8b)
                       (inst addv starts starts :8b)
                       (inst umov tmp-tn starts 0 :b)

                       (inst ushll bytes16 :8h bytes :8b 0)
                       (inst ushll combined :8h next :8b 0)

                       ;; next is shifted by one,
                       ;; construct a codepoint from two overlapping bytes,
                       ;; i.e. (dpb b0 (byte 5 6) b1)
                       (inst sli combined bytes16 6 :8h)
                       (inst bic combined #xF800 :8h)

                       ;; Select either the combined two bytes or one ascii byte
                       (inst cmhi is-lead16 bytes16 c-bf :8h)
                       (inst bsl is-lead16 combined bytes16 :16b)

                       ;; Remove the gaps left over from using two bytes as one codepoint
                       (inst ldr shuf (@ table (lsl tmp-tn 4)))
                       (inst tbl packed (list is-lead16) shuf :16b)

                       ;; Widen
                       (inst ushll s1 :4s packed :4h 0)
                       (inst add ptr string (lsl char-index 2))

                       (inst sub tmp-tn string-length char-index)
                       (inst cmp tmp-tn 8)
                       (inst b :lt tail-16)

                       (inst ushll2 s2 :4s packed :8h 0)

                       (inst addv count continuations :8b)
                       (inst stp s1 s2 (@ ptr))
                       (inst smov tmp-tn count 0 :b)
                       (inst add byte-index byte-index 8)
                       (inst add char-index char-index 8)
                       (inst add char-index char-index tmp-tn) ;; subtract continuations

                       (inst b LOOP))))
              (assemble ()
                (convert-1-2 START-FULL)

                START-FULL
                (inst movi c-ff #xFF :8h)
                (inst movi c-4 4 :4s)
                (load-inline-constant tag-clear :oword #x070F1F1F3F3F3F3F7F7F7F7F7F7F7F7F)
                (load-inline-constant full-table (coerce (loop for index below (ash 1 10)
                                                               for low-index = (ldb (byte 8 0) index)
                                                               for suffix = (ldb (byte 2 8) index)
                                                               append (let ((starts (loop for i to 7
                                                                                          when (logbitp i low-index)
                                                                                          collect i)))
                                                                        (loop for lane below 8
                                                                              for start = (pop starts)
                                                                              for next = (car starts)

                                                                              for sources = (when start
                                                                                              (loop for i from (1- (or next
                                                                                                                       (+ suffix 8))) downto start
                                                                                                    collect i))
                                                                              append (loop for byte below 4
                                                                                           collect (or (pop sources) #xFF)))))
                                                         '(vector (unsigned-byte 8))))

                FULL-LOOP
                (inst sub tmp-tn byte-array-length byte-index)
                FULL-LOOP-LENGTH-COMPUTED
                (inst cmp tmp-tn 16)
                (inst b :lt DONE)

                (inst sub tmp-tn string-length char-index)
                (inst cmp tmp-tn 8)
                (inst b :lt DONE)



                ;; Process the leading bytes in the first 8 bytes, loading 16 bytes
                ;; so that the last leading byte might drag in 3 more bytes
                (inst ldr bytes (@ byte-array byte-index))

                ;; Identify leading bytes
                (inst cmge temp bytes c-c0 :16b)

                ;; Turn them into an 8 bit index
                (inst and temp2 temp powers :8b)
                (inst addv temp3 temp2 :8b)
                (inst umov index temp3 0 :b)

                ;; Need to know where the last leading byte ends
                (inst umov suffix temp 1 :d)
                ;; Count the number of bytes to the next leading byte, turning it into a 2 bit suffix
                (inst rbit suffix suffix)
                (inst clz suffix suffix)
                ;; Bytes to bits
                (inst lsr suffix suffix 3)

                ;; Add the size of the last character, ensuring that only 2 bits are added
                (inst bfm index suffix 56 1)

                (inst add ptr full-table (lsl index 5))
                (inst ld1 (list shuf-low shuf-high) (@ ptr) :16b)

                (inst addv temp2 temp :8b)
                ;; A negated number of produced characters
                (inst smov suffix temp2 0 :b)

                ;; Use the high 4 bits of each byte to get an and-mask that
                ;; will clear their tags
                (inst ushr temp bytes 4 :16b)
                (inst tbl temp (list tag-clear) temp :16b)
                (inst and bytes bytes temp :16b)

                ;; Shuffle the bytes into 4-byte lanes
                (inst tbl chars-low (list bytes) shuf-low :16b)
                (inst tbl chars-high (list bytes) shuf-high :16b)

                (flet ((decode-lane (chars)
                         ;; Perform
                         ;; A + B<<6 + C<<12 + D<<18
                         ;; =>
                         ;; A + B<<6 + ((C + D<<6) << 12)

                         ;; [D, 0, B, 0]
                         (inst bic s1 chars c-ff :16b)
                         ;; [0, C, 0, A]
                         (inst and s2 chars c-ff :16b)
                         ;; Combine into two 12-bit blocks per 32-bit lane: [L1, L0]
                         (inst usra s2 s1 2 :8h)
                         ;; Shift L0 left by 4, leave L1 alone: [L1, L0 << 4]
                         (inst ushl s2 s2 c-4 :8h)
                         ;; Shift each 32-bit lane right by 4
                         (inst ushr chars s2 4 :4s)))

                  (decode-lane chars-low)
                  (decode-lane chars-high)
                  (inst add ptr string (lsl char-index 2))
                  (inst st1 (list chars-low chars-high) (@ ptr) :16b))


                (inst add byte-index byte-index 8)
                (inst sub char-index char-index suffix)
                ;; Can't re-enter the 1-2 loop if there were
                ;; continuation bytes into the next word, (and can't
                ;; add suffix to byte-index, as it will kill out of
                ;; order execution)
                (inst cbnz suffix full-loop)
                (convert-1-2 FULL-LOOP-LENGTH-COMPUTED)))
            TAIL-16
            (inst cmp tmp-tn 8)
            (inst b :lt DONE)
            (inst movi temp #xFFFFFFFF)
            (inst and continuations continuations temp :8b)
            (inst addv count continuations :8b)
            (inst smov tmp-tn count 0 :b)
            (inst str s1 (@ ptr))
            (inst add byte-index byte-index 4)
            (inst add char-index char-index 4)
            (inst add char-index char-index tmp-tn)
            DONE))
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

(defun character-string-to-utf8-byte-array (byte-array string byte-array-length)
  (declare (index byte-array-length)
           (simple-character-string string)
           ((simple-array (unsigned-byte 8) (*)) byte-array)
           (optimize speed (safety 0)))
  (let* ((length (length string)))
    (with-pinned-objects-in-registers (string byte-array)
      (multiple-value-bind (byte-index char-index)
          (inline-vop (((32-bit-array* sap-reg t :target 32-bit-array) (vector-sap string))
                       ((byte-array sap-reg t) (vector-sap byte-array))
                       ((n signed-reg) (logand (+ (* length 4) 15) -16))
                       ((byte-array-length unsigned-reg) (logand (+ byte-array-length 15) -16))
                       ((table any-reg t))
                       ((full-table any-reg t))
                       ((32-bit-array sap-reg t :from (:argument 0)))
                       ((tmp unsigned-reg))
                       ((ptr unsigned-reg))
                       ((temp complex-double-reg))
                       ((c-80 complex-double-reg))
                       ((2byte-mask-8h complex-double-reg))
                       ((shuf complex-double-reg))
                       ((ascii complex-double-reg))
                       ((powers double-reg))
                       ((ascii-count complex-double-reg))
                       ((f1 complex-double-reg t :offset 1))
                       ((f2 complex-double-reg t :offset 2))
                       ((f3 complex-double-reg t :offset 3))
                       ((bytes complex-double-reg t :offset 4))
                       ((bytes2 complex-double-reg t :offset 5))
                       ((r4 complex-double-reg t))
                       ((length1 complex-double-reg t :offset 10))
                       ((length2 complex-double-reg t :offset 11))
                       ((shuf-mask complex-double-reg t :offset 6))
                       ((and-mask complex-double-reg t :offset 7))
                       ((orr-mask complex-double-reg t :offset 8))
                       ((shift-mask complex-double-reg t)))
              ((byte-index unsigned-reg positive-fixnum :from :load)
               (char-index unsigned-reg positive-fixnum :from :load))
            (inst movi c-80 #x80 :8h)
            (move 32-bit-array 32-bit-array*)
            (load-inline-constant 2byte-mask-8h :oword #x80C080C080C080C080C080C080C080C0)
            (load-inline-constant powers :qword (concat-ub 8 '(128 64 32 16 8 4 2 1)))
            (flet ((make-table ()
                     (let* ((table-size 256)
                            (table (make-array (* table-size 16) :element-type '(unsigned-byte 8)
                                                                 :initial-element #xFF)))
                       ;; A table for selecting 1 or 2 byte utf8
                       ;; indexed by an 8 bit mask each bit with a set bit representing 1-byte characters
                       (loop for row below table-size
                             do (loop with dest-index = 0
                                      for lane below 8
                                      do
                                      (cond ((logbitp lane row)
                                             (setf (aref table (+ (* row 16) dest-index)) (* lane 2))
                                             (incf dest-index))
                                            ((setf (aref table (+ (* row 16) dest-index)) (+ 16 (* lane 2)))
                                             (setf (aref table (+ (* row 16) (1+ dest-index))) (+ 16 1 (* lane 2)))
                                             (incf dest-index 2)))))
                       table))
                   (make-full-table ()
                     (let* ((table-size 256)
                            (row-size (* 16 3))
                            (table (make-array (* table-size row-size) :element-type '(unsigned-byte 8)
                                                                       :initial-element 0)))

                       ;; A table with three masks per entry
                       ;; indexed by 4x2 bits representing the number of utf8 bytes for character - 1
                       (loop for i below (* table-size row-size)
                             when (< (mod i row-size) 16)
                             ;; fill the TBL part with an out of bounds index to get back zeros
                             do (setf (aref table i) #xFF))
                       (loop for row below table-size
                             do (loop with dest-index = 0
                                      for lane below 4
                                      for bytes = (1+ (ldb (byte 2 (* lane 2)) row))
                                      for zeros = (- 4 bytes)
                                      do (loop for b below bytes
                                               for reg-index = (+ zeros b)
                                               for src-index = (+ (* reg-index 16) (* lane 4))
                                               for lead-p = (= b 0)
                                               for and-mask = (if lead-p
                                                                  (case bytes
                                                                    (1 #x7F)
                                                                    (2 #x1F)
                                                                    (3 #x0F)
                                                                    (4 #x07))
                                                                  #x3F)
                                               for orr-mask = (if lead-p
                                                                  (case bytes
                                                                    (1 #x00)
                                                                    (2 #xC0)
                                                                    (3 #xE0)
                                                                    (4 #xF0))
                                                                  #x80)
                                               do (setf (aref table (+ (* row row-size) dest-index)) src-index) ;; tbl
                                                  (setf (aref table (+ (* row row-size) 16 dest-index)) and-mask) ;; and
                                                  (setf (aref table (+ (* row row-size) 32 dest-index)) orr-mask) ;; orr
                                                  (incf dest-index))))
                       table))
                   (convert (size full-length)
                     (assemble ()
                       (multiple-value-bind (h-size b-size)
                           (ecase size
                             (32
                              (inst ld1 (list bytes bytes2) (@ 32-bit-array) :4s)
                              ;; Stop if anything is 3-4 bytes in utf8
                              (inst orr temp bytes bytes2 :4s)
                              (inst umaxv temp temp :4s)
                              (inst umov tmp temp 0 :s)
                              (inst cmp tmp #x800)
                              (inst b :ge full-length)
                              (inst add 32-bit-array 32-bit-array 32)
                              ;; Narrow to 16 bits
                              (inst uzp1 bytes bytes bytes2 :8h)
                              (values :8h :16b))
                             (16
                              (inst ldr bytes (@ 32-bit-array))
                              ;; Stop if anything is 3-4 bytes in utf8
                              (inst umaxv temp bytes :4s)
                              (inst umov tmp temp 0 :s)
                              (inst cmp tmp #x800)
                              (inst b :ge full-length)
                              ;; Narrow to 16 bits
                              (inst xtn bytes bytes :4h)
                              (values :4h :8b)))
                         (inst cmp byte-array-length (/ size 2))
                         (inst b :lt DONE)

                         ;; Construct
                         ;; (logior
                         ;;  #x80C0
                         ;;  (dpb (ldb (byte 6 0) bits)
                         ;;       (byte 8 8)
                         ;;       (ldb (byte 5 6) bits)))
                         (inst ushr bytes2 bytes 6 h-size)
                         (inst sli bytes2 bytes 8 h-size)
                         (inst bic bytes2 #xC000 h-size)
                         (inst orr bytes2 bytes2 2byte-mask-8h h-size)

                         (inst cmhi ascii c-80 bytes h-size)

                         ;; Shrink the mask from 16 bits to 8 bits
                         (inst xtn ascii ascii :8b)
                         (inst addv ascii-count ascii :8b)
                         (inst and ascii powers ascii :8b)

                         (inst addv ascii ascii :8b)
                         (inst umov tmp ascii 0 :b)

                         (inst ldr shuf (@ table (lsl tmp 4)))
                         (inst tbl bytes (list bytes bytes2) shuf b-size)

                         (inst str bytes (@ byte-array byte-index) (when (eq size 16)
                                                                     :d))
                         (inst add char-index char-index size)
                         (case size
                           (32
                            (inst smov tmp ascii-count 0 :b)
                            (inst add byte-index byte-index 16)
                            (inst add byte-index byte-index tmp)
                            (inst sub byte-array-length byte-array-length 16)
                            (inst sub byte-array-length byte-array-length tmp)
                            (inst sub n n 32))))))
                   (convert-full ()
                     (inst cmp byte-array-length 16)
                     (inst b :lt DONE)

                     ;; Remove the low bits for each of the possible 4 resulting bytes
                     (inst ushr f1 bytes 18 :4s)
                     (inst ushr f2 bytes 12 :4s)
                     (inst ushr f3 bytes 6 :4s)

                     ;; Compute utf8 lengths - 1
                     (inst clz temp bytes :4s)
                     ;; Map leading zeros to utf8 lengths
                     (inst tbl temp (list length1 length2) temp :16b)
                     (inst addv r4 temp :4s) ;; total length in utf-8 bytes - 4

                     ;; Shift by 0 2 4 6
                     (inst ushl temp temp shift-mask :4s)
                     ;; combine into an 8-bit mask, 2 bits per lane
                     (inst addv temp temp :4s)
                     (inst umov tmp temp 0 :b)

                     ;; Multiply by 48 (3 * 16)
                     (inst add tmp tmp (lsl tmp 1))
                     (inst add ptr full-table (lsl tmp 4))

                     (inst ld1 (list shuf-mask and-mask orr-mask) (@ ptr) :16b)

                     (inst umov tmp-tn r4 0 :b)

                     (inst tbl bytes (list f1 f2 f3 bytes) shuf-mask :16b)

                     (inst and bytes bytes and-mask :16b)
                     (inst orr bytes bytes orr-mask :16b)

                     (inst str bytes (@ byte-array byte-index))

                     (inst add byte-index byte-index 4)
                     (inst add byte-index byte-index tmp-tn)
                     (inst sub byte-array-length byte-array-length 4)
                     (inst sub byte-array-length byte-array-length tmp-tn)
                     (inst add 32-bit-array 32-bit-array 16)
                     (inst add char-index char-index 16)
                     (inst sub n n 16)))
              (assemble ()
                (load-inline-constant table (make-table))
                (inst mov byte-index 0)
                (inst mov char-index 0)

                (inst cmp n 32)
                (inst b :lt TAIL)
                LOOP
                (convert 32 START-FULL-LENGTH)
                (inst cmp n 32)
                (inst b :ge LOOP)

                TAIL
                (inst cbz n DONE)

                (convert 16 START-FULL-LENGTH)
                (inst b DONE)

                START-FULL-LENGTH
                (load-inline-constant shift-mask :oword #x6000000040000000200000000)
                (load-inline-constant full-table (make-full-table))
                (inst movi length1 3 :16b)
                (load-inline-constant length2 :oword #x00000000000000010101010202020202)
                FULL-LENGTH
                (convert-full)

                (inst cmp n 32)
                (inst b :lt TAIL2)
                LOOP2
                (convert 32 FULL-LENGTH)
                (inst cmp n 32)
                (inst b :ge LOOP2)

                TAIL2
                (inst cbz n DONE)
                (convert 16 FULL-LENGTH)))
            DONE)
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
