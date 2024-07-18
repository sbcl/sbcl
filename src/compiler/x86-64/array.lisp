;;;; array operations for the x86 VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")


;; For use in constant indexing; we can't use INDEX since the displacement
;; field of an EA can't contain 64 bit values.
(sb-xc:deftype low-index () '(signed-byte 29))

(defun unpoison-element (array index &optional (addend 0))
  #-ubsan (declare (ignore array index addend))
  #+ubsan
  (let ((no-bits (gen-label)))
    (aver (= addend 0))
    (inst mov temp-reg-tn (object-slot-ea array 1 other-pointer-lowtag))
    ;; See if the ancillary slot holds a bit-vector and not a list.
    ;; A list will denote the stack trace at the creation site
    ;; rather than shadow bits.
    (inst test :byte temp-reg-tn #b1000)
    (inst jmp :z no-bits)
    (flet ((constant-index (index)
             (multiple-value-bind (dword-index bit) (floor index 32) dword-index bit
               (inst bts :lock :dword (ea (bit-base dword-index) temp-reg-tn) bit))))
      (if (integerp index)
          (constant-index index)
          (sc-case index
           (immediate (constant-index (tn-value index)))
           ((signed-reg unsigned-reg)
            (inst bts :lock (ea (bit-base 0) temp-reg-tn) index))
           (t
            (aver (sc-is index any-reg))
            (inst shr :dword index 1) ; untag it
            (inst bts :lock (ea (bit-base 0) temp-reg-tn) index)
            (inst shl :dword index 1)))))
    (emit-label no-bits)))

(defun test-poisoned (vop temp array index &optional (addend 0))
  (declare (ignore vop temp array index addend))
  #+nil
  (unless (sb-c::policy (sb-c::vop-node vop) (= safety 0))
    (let ((ok (gen-label)))
      (when (integerp index) (setq index (emit-constant index)))
      (inst mov temp (object-slot-ea object 1 other-pointer-lowtag)) ; shadow bits
      (inst test :byte temp temp)
      (inst jmp :z ok) ; no shadow bits
      (if (and (eql addend 0) (eql (tn-sc index unsigned-reg)))
          (inst bt (ea (bit-base 0) temp) index)
          (error "Unhandled SCs in test-poisoned"))
      (inst jmp :nc (generate-error-code vop 'uninitialized-element-error
                                         object index addend))
      (emit-label ok))))

;;;; allocator for the array header

(define-vop (make-array-header)
  (:translate make-array-header)
  (:policy :fast-safe)
  (:args (type :scs (any-reg))
         (rank :scs (any-reg)))
  (:arg-types positive-fixnum positive-fixnum)
  (:temporary (:sc any-reg :to :eval) bytes)
  (:temporary (:sc any-reg :to :result) header)
  (:temporary (:sc unsigned-reg) temp)
  #+gs-seg (:temporary (:sc unsigned-reg :offset 15) thread-tn)
  (:results (result :scs (descriptor-reg) :from :eval))
  (:node-var node)
  (:generator 13
    (inst lea :dword bytes
          (ea (+ (* array-dimensions-offset n-word-bytes) lowtag-mask)
              nil rank (ash 1 (- word-shift n-fixnum-tag-bits))))
    (inst and :dword bytes (lognot lowtag-mask))
    ;; rank 1 is stored as 0, 2 is stored as 1, ...
    (inst lea :dword header (ea (fixnumize -1) rank))
    (inst and :dword header (fixnumize array-rank-mask))
    (inst shl :dword header array-rank-position)
    (inst or  :dword header type)
    (inst shr :dword header n-fixnum-tag-bits)
    (instrument-alloc nil bytes node temp thread-tn)
    (pseudo-atomic (:thread-tn thread-tn)
     (allocation type bytes 0 result node temp thread-tn)
     (storew header result 0 0)
     (inst or :byte result other-pointer-lowtag))))

;;;; additional accessors and setters for the array header
(define-full-reffer %array-dimension *
  array-dimensions-offset other-pointer-lowtag
  (any-reg) positive-fixnum %array-dimension)

(define-full-setter %set-array-dimension *
  array-dimensions-offset other-pointer-lowtag
  (any-reg) positive-fixnum %set-array-dimension)

(symbol-macrolet ((rank-disp
                    (- (/ array-rank-position n-byte-bits) other-pointer-lowtag)))
(define-vop ()
  (:translate %array-rank)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 3
    (inst movzx '(:byte :dword) res (ea rank-disp x))
    (inst inc :byte res)))

(define-vop ()
  (:translate %array-rank=)
  (:policy :fast-safe)
  (:args (array :scs (descriptor-reg)))
  (:info rank)
  (:arg-types * (:constant t))
  (:conditional :e)
  (:generator 2
    (inst cmp :byte (ea rank-disp array) (encode-array-rank rank))))

(define-vop (array-vectorp simple-type-predicate)
  ;; SIMPLE-TYPE-PREDICATE says that it takes stack locations, but that's no good.
  (:args (array :scs (any-reg descriptor-reg)))
  (:translate vectorp)
  (:conditional :z)
  (:info)
  (:guard (lambda (node)
            (let ((arg (car (sb-c::combination-args node))))
              (csubtypep (sb-c::lvar-type arg) (specifier-type 'array)))))
  (:generator 1
    (inst cmp :byte (ea rank-disp array) (encode-array-rank 1)))))

(define-vop (simple-array-header-of-rank-p type-predicate)
  (:translate sb-c::simple-array-header-of-rank-p)
  (:info rank)
  (:conditional :e)
  (:arg-types * (:constant t))
  (:generator 2
    (let ((c (dpb (encode-array-rank rank) (byte 8 array-rank-position)
                  simple-array-widetag)))
      (cond ((other-pointer-tn-ref-p args t)
             (inst cmp :word (ea (- other-pointer-lowtag) value) c))
            (t
             (%lea-for-lowtag-test temp value other-pointer-lowtag :qword)
             (inst test :byte temp lowtag-mask)
             (inst jmp :ne OUT)
             (inst cmp :word (ea temp) c))))
    OUT))


(defun power-of-two-limit-p (x)
  (and (fixnump x)
       (= (logcount (1+ x)) 1)))

;;;; bounds checking routine
(defun emit-bounds-check (vop %test-fixnum array index limit)
  (let*  ((use-length-p (null limit))
          (error
            (if use-length-p
                (generate-error-code vop 'sb-kernel::invalid-vector-index-error
                                     array index)
                (generate-error-code vop 'invalid-array-index-error array limit
                                     index)))
          (bound (if (and (tn-p limit) (sc-is limit immediate))
                     (let ((value (tn-value limit)))
                       (cond ((and %test-fixnum
                                   (power-of-two-limit-p (1- value)))
                              (lognot (fixnumize (1- value))))
                             ((sc-is index any-reg descriptor-reg)
                              (fixnumize value))
                             (t
                              value)))
                     limit))
          (index (if (sc-is index immediate)
                     (let ((value (tn-value index)))
                       (if (or (null bound) ; from array header
                               (sc-is bound any-reg descriptor-reg))
                           (fixnumize value)
                           value))
                     index)))
    (cond ((typep bound '(integer * -1))
           ;; Power of two bound, can be checked for fixnumness at
           ;; the same time as it always occupies a consecutive bit
           ;; range, everything else, including the tag, has to be
           ;; zero.
           (inst test index (if (eql bound -1)
                                index ;; zero?
                                bound))
           (inst jmp :ne error))
          (t
           (when (and %test-fixnum (not (integerp index)))
             (%test-fixnum index nil error t))
           (cond (use-length-p
                  (let ((len (vector-len-ea array)))
                    (cond ((integerp index)
                           (inst cmp vector-len-op-size len index)
                           (inst jmp :be error))
                          (t
                           (inst cmp vector-len-op-size index len)
                           (inst jmp :nb error)))))
                 ((integerp bound)
                  (inst cmp index bound)
                  (inst jmp :nb error))
                 (t
                  (if (eql index 0)
                      (inst test bound bound)
                      (inst cmp bound index))
                  (inst jmp :be error)))))))

(define-vop (check-bound)
  (:translate %check-bound)
  (:policy :fast-safe)
  (:args (array :scs (descriptor-reg constant))
         (bound :scs (any-reg descriptor-reg)
                :load-if (not (and (sc-is bound immediate)
                                   (typep (tn-value bound)
                                          'sc-offset)
                                   (not (sc-is index immediate)))))
         (index :scs (any-reg descriptor-reg)
                :load-if (not (and (sc-is index immediate)
                                   (typep (tn-value index)
                                          'sc-offset)))))
  (:variant-vars %test-fixnum)
  (:variant t)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 6 (emit-bounds-check vop %test-fixnum array index bound)))

(define-vop (check-bound/fast check-bound)
  (:policy :fast)
  (:variant nil)
  (:variant-cost 4))

(define-vop (check-bound/fixnum check-bound)
  (:args (array)
         (bound)
         (index :scs (any-reg)))
  (:arg-types * * tagged-num)
  (:variant nil)
  (:variant-cost 4))

(define-vop (check-bound/untagged check-bound)
  (:args (array)
         (bound :scs (unsigned-reg signed-reg))
         (index :scs (unsigned-reg signed-reg)))
  (:arg-types * (:or unsigned-num signed-num)
                (:or unsigned-num signed-num))
  (:variant nil)
  (:variant-cost 5))

(define-vop (check-vector-bound)
  (:args (array :scs (descriptor-reg)) ; no constant sc allowed
         (index :scs (any-reg descriptor-reg)
                :load-if (not (and (sc-is index immediate)
                                   (typep (tn-value index)
                                          'sc-offset)))))
  (:variant-vars %test-fixnum)
  (:variant t)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 5 (emit-bounds-check vop %test-fixnum array index nil)))

(define-vop (check-vector-bound/fast check-vector-bound)
  (:policy :fast)
  (:variant nil)
  (:variant-cost 3))

(define-vop (check-vector-bound/fixnum check-vector-bound)
  (:args (array)
         (index :scs (any-reg)))
  (:arg-types * tagged-num)
  (:variant nil)
  (:variant-cost 3))

(flet ((try-absorb-load (vop replacement)
         (let ((prev (sb-c::vop-prev vop)))
           ;; If the 2nd arg is the result of VECTOR-LENGTH of the 1st arg, then
           ;; don't load the length; instead absorb it into a CMP instruction.
           (when (and prev
                      (eq (vop-name prev) 'slot)
                      (eq (car (vop-codegen-info prev)) 'sb-c::vector-length)
                      (eq (tn-ref-tn (vop-args prev))
                          (tn-ref-tn (vop-args vop))))
             (let* ((args (vop-args vop))
                    (array (tn-ref-tn args))
                    (limit (tn-ref-tn (tn-ref-across args)))
                    (index (tn-ref-tn (tn-ref-across (tn-ref-across args)))))
               ;; Don't eliminate VECTOR-LENGTH if it has more than one read ref
               (unless (tn-ref-next (sb-c::tn-reads limit))
                 (let ((new (sb-c::emit-and-insert-vop
                             (sb-c::vop-node vop) (vop-block vop)
                             (template-or-lose replacement)
                             (sb-c:reference-tn-list (list array index) nil)
                             nil vop)))
                   (sb-c::delete-vop prev)
                   (sb-c::delete-vop vop)
                   new)))))))
  (setf (sb-c::vop-info-optimizer (template-or-lose 'check-bound))
        (lambda (vop) (try-absorb-load vop 'check-vector-bound))
        (sb-c::vop-info-optimizer (template-or-lose 'check-bound/fast))
        (lambda (vop) (try-absorb-load vop 'check-vector-bound/fast))
        (sb-c::vop-info-optimizer (template-or-lose 'check-bound/fixnum))
        (lambda (vop) (try-absorb-load vop 'check-vector-bound/fixnum))))

;;;; accessors/setters

;;; Ancestors
(define-vop (dvref)
  ;; I don't think we should print these notes even if we could, which we can't.
  ;; We do, however, seem to print
  ;;  "The first argument is a (SIMPLE-ARRAY * (1)), not a SIMPLE-STRING."
  ;; for pretty much any general type-unknown AREF.
  ;; (:note "inline array access")
  (:translate data-vector-ref-with-offset)
  (:policy :fast-safe))
(define-vop (dvset)
  ;; (:note "inline array store")
  (:translate data-vector-set-with-offset)
  (:policy :fast-safe))

;;; variants which affect an entire lispword-sized value.
;;; Toplevel macro for ease of viewing the expansion.
(defmacro define-full-setter+addend (name type offset lowtag scs el-type)
  (let ((scs (adjoin 'immediate scs)))
   `(progn
     (define-vop (,name dvset)
       (:args (object :scs (descriptor-reg))
              (index :scs (any-reg signed-reg unsigned-reg))
              (value :scs ,scs))
       (:info addend)
       (:arg-types ,type tagged-num
                   (:constant (constant-displacement other-pointer-lowtag
                                                     n-word-bytes
                                                     vector-data-offset))
                   ,el-type)
       (:temporary (:sc unsigned-reg) val-temp)
       (:vop-var vop)
       (:generator 4
         ;; XXX: Is this good - we unpoison first, and then store? It seems wrong.
         ,@(unless (eq type 'simple-vector) '((unpoison-element object index addend)))
         (let ((ea (ea (- (* (+ ,offset addend) n-word-bytes) ,lowtag)
                           object index (index-scale n-word-bytes index))))
           ,@(when (eq type 'simple-vector)
               '((emit-gengc-barrier object ea val-temp (vop-nth-arg 2 vop))))
           (emit-store ea value val-temp
                       ,(not (intersection '(signed-reg unsigned-reg) scs))))))
     (define-vop (,(symbolicate name "-C") dvset)
       (:args (object :scs (descriptor-reg))
              (value :scs ,scs))
       (:info index addend)
       (:arg-types ,type
                   (:constant (load/store-index ,n-word-bytes ,(eval lowtag)
                                                ,(eval offset)))
                   (:constant (constant-displacement other-pointer-lowtag
                                                     n-word-bytes
                                                     vector-data-offset))
                   ,el-type)
       (:temporary (:sc unsigned-reg) val-temp)
       (:vop-var vop)
       (:generator 3
         ;; XXX: Is this good - we unpoison first, and then store? It seems wrong.
         ,@(unless (eq type 'simple-vector) '((unpoison-element object (+ index addend))))
         (let ((ea (ea (- (* (+ ,offset index addend) n-word-bytes) ,lowtag) object)))
           ,@(when (eq type 'simple-vector)
               '((emit-gengc-barrier object ea val-temp (vop-nth-arg 1 vop))))
           (emit-store ea value val-temp
                       ,(not (intersection '(signed-reg unsigned-reg) scs)))))))))
(defmacro def-full-data-vector-frobs (type element-type &rest scs)
  `(progn
     (define-full-reffer+addend ,(symbolicate "DATA-VECTOR-REF-WITH-OFFSET/" type)
       ,type vector-data-offset other-pointer-lowtag ,scs
       ,element-type data-vector-ref-with-offset)
     (define-full-setter+addend ,(symbolicate "DATA-VECTOR-SET-WITH-OFFSET/" type)
       ,type vector-data-offset other-pointer-lowtag ,scs
       ,element-type)))
(progn
  (def-full-data-vector-frobs simple-vector * descriptor-reg any-reg immediate constant)
  (def-full-data-vector-frobs simple-array-unsigned-byte-64 unsigned-num
    unsigned-reg)
  (def-full-data-vector-frobs simple-array-fixnum tagged-num any-reg)
  (def-full-data-vector-frobs simple-array-unsigned-fixnum
      positive-fixnum any-reg)
  (def-full-data-vector-frobs simple-array-signed-byte-64
      signed-num signed-reg)
  (def-full-data-vector-frobs simple-array-unsigned-byte-63 unsigned-num
    unsigned-reg))

;;; Try to combine DATA-VECTOR-REF/SIMPLE-VECTOR + IF-EQ.
;;; But never do it under ubsan.
#-ubsan
(defoptimizer (sb-c::vop-optimize data-vector-ref-with-offset/simple-vector) (vop)
  (let ((next (vop-next vop)))
    (when (and next
               (eq (vop-name next) 'if-eq)
               ;; Prevent this optimization in safe code
               (sb-c::policy (sb-c::vop-node vop) (< safety 3)))
      (let* ((result-tn (tn-ref-tn (vop-results vop)))
             (next-args (vop-args next))
             (left (tn-ref-tn next-args))
             (right (tn-ref-tn (tn-ref-across next-args)))
             (comparand (if (eq result-tn left) right left)))
        ;; Since we're only looking at the :Z flag it does not matter
        ;; if the array ref is the left or right operand of CMP.
        (when (and (sb-c::very-temporary-p result-tn)
                   (or (eq result-tn left) (eq result-tn right))
                   (or (not (constant-tn-p comparand))
                       (and (tn-sc comparand)
                            (plausible-signed-imm32-operand-p
                             (encode-value-if-immediate comparand)))))
          (let* ((new-args (sb-c:reference-tn-list
                            (list (tn-ref-tn (vop-args vop))
                                  (tn-ref-tn (tn-ref-across (vop-args vop)))
                                  comparand)
                            nil))
                 (new (sb-c::emit-and-insert-vop
                       (sb-c::vop-node vop) (vop-block vop)
                       (template-or-lose 'svref-with-addend+if-eq)
                       new-args nil vop (append (vop-codegen-info vop)
                                                (vop-codegen-info next)))))
            (sb-c::delete-vop vop)
            (sb-c::delete-vop next)
            new))))))

(define-vop (svref-with-addend+if-eq)
   (:args (object :scs (descriptor-reg))
          (index :scs (any-reg signed-reg unsigned-reg))
          (comparand :scs (any-reg descriptor-reg immediate)))
   (:info addend)
   (:arg-types simple-vector tagged-num * (:constant integer))
   (:conditional :z)
   (:generator 1
    (inst cmp :qword
          (ea (- (* (+ vector-data-offset addend) n-word-bytes) other-pointer-lowtag)
              object index
              (index-scale n-word-bytes index))
          (encode-value-if-immediate comparand))))

(define-vop (data-vector-ref-with-offset/constant-simple-vector)
  (:policy :fast-safe)
  (:args (object :scs (constant))
         (index :scs (any-reg)))
  (:temporary (:sc unsigned-reg) array)
  (:info addend)
  (:arg-types simple-vector tagged-num (:constant fixnum))
  (:results (value :scs (descriptor-reg any-reg immediate)))
  (:result-types *)
  (:generator 3
    (inst lea array object)
    (inst mov value (ea (* addend n-word-bytes) array
                        index (ash 1 (- word-shift n-fixnum-tag-bits))))))

;;; TODO: check for uninitialized element if safety = 3
(define-full-compare-and-swap %compare-and-swap-svref simple-vector
  vector-data-offset other-pointer-lowtag
  (descriptor-reg any-reg) *
  %compare-and-swap-svref)

;;; SIMPLE-BIT-VECTOR
(defun bit-base (dword-index)
  (+ (* dword-index 4)
     (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))

(defun emit-sbit-op (inst bv index &optional word temp)
  (cond ((sc-is index immediate)
         (multiple-value-bind (dword-index bit) (floor (tn-value index) 32)
           (let ((disp (bit-base dword-index)))
             (inst* inst :dword (ea disp bv) bit))))
        (t
         ;; mem/reg BT[SR] are really slow.
         (inst mov word index)
         (inst shr word (integer-length (1- n-word-bits)))
         (inst mov temp (ea (bit-base 0) bv word n-word-bytes))
         (if (functionp inst)
             (funcall inst)
             (inst* inst temp index))
         (inst mov (ea (bit-base 0) bv word n-word-bytes) temp))))

(define-vop (data-vector-set-with-offset/simple-bit-vector)
  (:translate data-vector-set-with-offset)
  (:policy :fast-safe)
  ;; Arg order is (VECTOR INDEX ADDEND VALUE)
  (:arg-types simple-bit-vector positive-fixnum (:constant (eql 0)) positive-fixnum)
  (:args (bv :scs (descriptor-reg))
         (index :scs (unsigned-reg (immediate
                                    (typep (bit-base (floor (tn-value tn) 32)) '(signed-byte 32)))))
         (value :scs (immediate any-reg signed-reg unsigned-reg control-stack
                                signed-stack unsigned-stack)))
  (:temporary (:sc unsigned-reg) word temp)
  (:info addend)
  (:ignore addend)
  (:generator 6
    (unpoison-element bv index)
    (cond ((sc-is value immediate)
           (ecase (tn-value value)
             (1 (emit-sbit-op 'bts bv index word temp))
             (0 (emit-sbit-op 'btr bv index word temp))))
          ((sc-is index immediate)
           (assemble ()
             (inst test :byte value
                   (if (sc-is value control-stack signed-stack unsigned-stack) #xff value))
             (inst jmp :z ZERO)
             (emit-sbit-op 'bts bv index)
             (inst jmp OUT)
             ZERO
             (emit-sbit-op 'btr bv index)
             OUT))
          (t
           (emit-sbit-op (lambda ()
                           (assemble ()
                             (inst test :byte value
                                   (if (sc-is value control-stack signed-stack unsigned-stack) #xff value))
                             (inst jmp :z ZERO)
                             (inst bts temp index)
                             (inst jmp OUT)
                             ZERO
                             (inst btr temp index)
                             OUT))
                         bv index word temp)))))

(define-vop (data-vector-ref-with-offset/simple-bit-vector-c dvref)
  (:args (object :scs (descriptor-reg)))
  (:arg-types simple-bit-vector
              ;; this constant is possibly off by something
              ;; but (sbit n <huge-constant>) is unlikely to appear in code
              (:constant (integer 0 #x3ffffffff)) (:constant (integer 0 0)))
  (:info index addend)
  (:ignore addend)
  (:results (result :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 3
    ;; using 32-bit operand size might elide the REX prefix on mov + shift
    (multiple-value-bind (dword-index bit) (floor index 32)
      (inst mov :dword result (ea (bit-base dword-index) object))
      (let ((right-shift (- bit n-fixnum-tag-bits)))
        (cond ((plusp right-shift)
               (inst shr :dword result right-shift))
              ((minusp right-shift)     ; = left shift
               (inst shl :dword result (- right-shift))))))
    (inst and :dword result (fixnumize 1))))

(define-vop (data-vector-ref-with-offset/simple-bit-vector dvref)
  (:args (object :scs (descriptor-reg))
         (index :scs (unsigned-reg)))
  (:info addend)
  (:ignore addend)
  (:arg-types simple-bit-vector positive-fixnum (:constant (integer 0 0)))
  (:temporary (:sc unsigned-reg) temp)
  (:results (result :scs (any-reg)))
  (:result-types positive-fixnum)
  (:vop-var vop)
  (:generator 4
    ;; mem/reg BT is really slow.
    (inst mov temp index)
    (inst shr temp (integer-length (1- n-word-bits)))
    (inst mov temp (ea (bit-base 0) object temp n-word-bytes))
    (inst bt temp index)
    (inst sbb :dword result result)
    (inst and :dword result (fixnumize 1))))

(define-vop (data-vector-ref-with-offset/simple-bit-vector-c-eq)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:arg-types simple-bit-vector
              (:constant (integer 0 #x3ffffffff)) (:constant (integer 0 0)))
  (:info index addend)
  (:ignore addend)
  (:conditional :eq)
  (:generator 3
    (multiple-value-bind (byte-index bit) (floor index 8)
      (inst test :byte (ea (+ byte-index
                              (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
                           object)
            (ash 1 bit)))))

(define-vop (data-vector-ref-with-offset/simple-bit-vector-eq)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (unsigned-reg)))
  (:info addend)
  (:ignore addend)
  (:arg-types simple-bit-vector positive-fixnum (:constant (integer 0 0)))
  (:temporary (:sc unsigned-reg) word)
  (:conditional :nc)
  (:vop-var vop)
  (:generator 4
    ;; mem/reg BT is really slow.
    (inst mov word index)
    (inst shr word (integer-length (1- n-word-bits)))
    (inst mov word (ea (bit-base 0) object word n-word-bytes))
    (inst bt word index)))

;;;; vectors whose elements are 2 or 4 bits each
(macrolet ((def-small-data-vector-frobs (type bits)
             (let* ((elements-per-word (floor n-word-bits bits))
                    (bit-shift (1- (integer-length elements-per-word))))
               `(progn
                  (define-vop (,(symbolicate 'data-vector-ref-with-offset/ type) dvref)
                    (:args (object :scs (descriptor-reg))
                           (index :scs (unsigned-reg)))
                    (:info addend)
                    (:ignore addend)
                    (:arg-types ,type positive-fixnum (:constant (integer 0 0)))
                    (:results (result :scs (unsigned-reg) :from (:argument 0)))
                    (:result-types positive-fixnum)
                    (:temporary (:sc unsigned-reg :offset rcx-offset) ecx)
                    (:generator 20
                      (move ecx index)
                      (inst shr ecx ,bit-shift)
                      (inst mov result
                        (ea (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)
                          object ecx n-word-bytes))
                      (move ecx index)
                      ;; We used to mask ECX for all values of BITS, but since
                      ;; Intel's documentation says that the chip will mask shift
                      ;; and rotate counts by 63 automatically, we can safely move
                      ;; the masking operation under the protection of this UNLESS
                      ;; in the bit-vector case.  --njf, 2006-07-14
                      ,@(unless (= bits 1)
                          `((inst and ecx ,(1- elements-per-word))
                            (inst shl ecx ,(1- (integer-length bits)))))
                      (inst shr result :cl)
                      (inst and result ,(1- (ash 1 bits)))))
                  (define-vop (,(symbolicate 'data-vector-ref-with-offset/ type "-C") dvref)
                    (:args (object :scs (descriptor-reg)))
                    (:arg-types ,type (:constant low-index) (:constant (integer 0 0)))
                    (:info index addend)
                    (:ignore addend)
                    (:results (result :scs (unsigned-reg)))
                    (:result-types positive-fixnum)
                    (:generator 15
                      (multiple-value-bind (word extra) (floor index ,elements-per-word)
                        (loadw result object (+ word vector-data-offset)
                            other-pointer-lowtag)
                        (unless (zerop extra)
                          (inst shr result (* extra ,bits)))
                        (unless (= extra ,(1- elements-per-word))
                          (inst and result ,(1- (ash 1 bits)))))))
                  (define-vop (,(symbolicate 'data-vector-set-with-offset/ type) dvset)
                    (:args (object :scs (descriptor-reg))
                           (index :scs (unsigned-reg) :target ecx)
                           (value :scs (unsigned-reg immediate)))
                    (:info addend)
                    (:ignore addend)
                    (:arg-types ,type positive-fixnum (:constant (integer 0 0))
                                positive-fixnum)
                    (:temporary (:sc unsigned-reg) word-index)
                    (:temporary (:sc unsigned-reg) old)
                    (:temporary (:sc unsigned-reg :offset rcx-offset) ecx)
                    (:generator 25
                      (unpoison-element object index)
                      (move word-index index)
                      (inst shr word-index ,bit-shift)
                      (inst mov old
                        (ea (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)
                          object word-index n-word-bytes))
                      (move ecx index)
                      ;; We used to mask ECX for all values of BITS, but since
                      ;; Intel's documentation says that the chip will mask shift
                      ;; and rotate counts by 63 automatically, we can safely move
                      ;; the masking operation under the protection of this UNLESS
                      ;; in the bit-vector case.  --njf, 2006-07-14
                      ,@(unless (= bits 1)
                          `((inst and ecx ,(1- elements-per-word))
                            (inst shl ecx ,(1- (integer-length bits)))))
                      (inst ror old :cl)
                      (unless (and (sc-is value immediate)
                                   (= (tn-value value) ,(1- (ash 1 bits))))
                        (inst and old ,(lognot (1- (ash 1 bits)))))
                      (sc-case value
                        (immediate
                         (unless (zerop (tn-value value))
                           (inst or old (logand (tn-value value) ,(1- (ash 1 bits))))))
                        (unsigned-reg
                         (inst or old value)))
                      (inst rol old :cl)
                      (inst mov (ea (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)
                                  object word-index n-word-bytes)
                        old)))
                  (define-vop (,(symbolicate 'data-vector-set-with-offset/ type "-C") dvset)
                    (:args (object :scs (descriptor-reg))
                           (value :scs (unsigned-reg immediate)))
                    (:arg-types ,type (:constant low-index)
                                (:constant (integer 0 0)) positive-fixnum)
                    (:temporary (:sc unsigned-reg) mask-tn)
                    (:info index addend)
                    (:ignore addend)
                    (:temporary (:sc unsigned-reg :to (:result 0)) old)
                    (:generator 20
                      (unpoison-element object index)
                      (multiple-value-bind (word extra) (floor index ,elements-per-word)
                        (inst mov old
                              (ea (- (* (+ word vector-data-offset) n-word-bytes)
                                     other-pointer-lowtag)
                                  object))
                        (sc-case value
                          (immediate
                           (let* ((value (tn-value value))
                                  (mask ,(1- (ash 1 bits)))
                                  (shift (* extra ,bits)))
                             (unless (= value mask)
                               (inst mov mask-tn (ldb (byte 64 0)
                                                      (lognot (ash mask shift))))
                               (inst and old mask-tn))
                             (unless (zerop value)
                               (inst mov mask-tn (ash value shift))
                               (inst or old mask-tn))))
                          (unsigned-reg
                           (let ((shift (* extra ,bits)))
                             (unless (zerop shift)
                               (inst ror old shift))
                             (inst mov mask-tn (lognot ,(1- (ash 1 bits))))
                             (inst and old mask-tn)
                             (inst or old value)
                             (unless (zerop shift)
                               (inst rol old shift)))))
                        (inst mov (ea (- (* (+ word vector-data-offset) n-word-bytes)
                                         other-pointer-lowtag)
                                      object)
                              old))))))))
  (def-small-data-vector-frobs simple-array-unsigned-byte-2 2)
  (def-small-data-vector-frobs simple-array-unsigned-byte-4 4))
;;; And the float variants.

(defun float-ref-ea (object index addend element-size
                              &key (scale 1) (complex-offset 0))
  (etypecase index
      (integer
       (ea (- (+ (* vector-data-offset n-word-bytes)
                 (* (+ index addend) element-size)
                 complex-offset)
              other-pointer-lowtag)
           object))
      (tn
       (ea (- (+ (* vector-data-offset n-word-bytes)
                 (* addend element-size)
                 complex-offset)
              other-pointer-lowtag)
           object index scale))))

#.
(let ((use-temp (<= word-shift n-fixnum-tag-bits)))
  `(define-vop (data-vector-ref-with-offset/simple-array-single-float dvref)
     (:args (object :scs (descriptor-reg))
            (index :scs (any-reg)))
     (:info addend)
     (:arg-types simple-array-single-float tagged-num
                 (:constant (constant-displacement other-pointer-lowtag
                                                   4 vector-data-offset)))
     ,@(when use-temp '((:temporary (:sc unsigned-reg) dword-index)))
     (:results (value :scs (single-reg)))
     (:result-types single-float)
     (:generator 5
      ,@(if use-temp
            '((move dword-index index)
              (inst shr dword-index (1+ (- n-fixnum-tag-bits word-shift)))
              (inst movss value (float-ref-ea object dword-index addend 4)))
            '((inst movss value (float-ref-ea object index addend 4
                                 :scale (ash 4 (- n-fixnum-tag-bits)))))))))

(define-vop (data-vector-ref-with-offset/simple-array-single-float-c dvref)
  (:args (object :scs (descriptor-reg)))
  (:info index addend)
  (:arg-types simple-array-single-float (:constant low-index)
              (:constant (constant-displacement other-pointer-lowtag
                                                4 vector-data-offset)))
  (:results (value :scs (single-reg)))
  (:result-types single-float)
  (:generator 4
   (inst movss value (float-ref-ea object index addend 4))))

#.
(let ((use-temp (<= word-shift n-fixnum-tag-bits)))
  `(define-vop (data-vector-set-with-offset/simple-array-single-float dvset)
     (:args (object :scs (descriptor-reg))
            (index :scs (any-reg))
            (value :scs (single-reg)))
     (:info addend)
     (:arg-types simple-array-single-float tagged-num
                 (:constant (constant-displacement other-pointer-lowtag
                                                   4 vector-data-offset))
                  single-float)
     ,@(when use-temp '((:temporary (:sc unsigned-reg) dword-index)))
     (:generator 5
      (unpoison-element object index addend)
      ;; NOTE: this can not possibly work with n-fixnum-tag-bits = 3
      ;; because (ash 4 -3) is 0
      ,@(if use-temp
            '((move dword-index index)
              (inst shr dword-index (1+ (- n-fixnum-tag-bits word-shift)))
              (inst movss (float-ref-ea object dword-index addend 4) value))
            '((inst movss (float-ref-ea object index addend 4
                           :scale (ash 4 (- n-fixnum-tag-bits))) value))))))

(define-vop (data-vector-set-with-offset/simple-array-single-float-c dvset)
  (:args (object :scs (descriptor-reg))
         (value :scs (single-reg)))
  (:info index addend)
  (:arg-types simple-array-single-float (:constant low-index)
              (:constant (constant-displacement other-pointer-lowtag
                                                4 vector-data-offset))
              single-float)
  (:generator 4
   (unpoison-element object (+ index addend))
   (inst movss (float-ref-ea object index addend 4) value)))

(define-vop (data-vector-ref-with-offset/simple-array-double-float dvref)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg signed-reg unsigned-reg)))
  (:info addend)
  (:arg-types simple-array-double-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag
                                                8 vector-data-offset)))
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:generator 7
   (inst movsd value (float-ref-ea object index addend 8 :scale (index-scale 8 index)))))

(define-vop (data-vector-ref-c/simple-array-double-float dvref)
  (:args (object :scs (descriptor-reg)))
  (:info index addend)
  (:arg-types simple-array-double-float (:constant low-index)
              (:constant (constant-displacement other-pointer-lowtag
                                                8 vector-data-offset)))
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:generator 6
   (inst movsd value (float-ref-ea object index addend 8))))

(define-vop (data-vector-set-with-offset/simple-array-double-float dvset)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg signed-reg unsigned-reg))
         (value :scs (double-reg)))
  (:info addend)
  (:arg-types simple-array-double-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag
                                                8 vector-data-offset))
              double-float)
  (:generator 20
   (unpoison-element object index addend)
   (inst movsd (float-ref-ea object index addend 8 :scale (index-scale 8 index)) value)))

(define-vop (data-vector-set-with-offset/simple-array-double-float-c dvset)
  (:args (object :scs (descriptor-reg))
         (value :scs (double-reg)))
  (:info index addend)
  (:arg-types simple-array-double-float (:constant low-index)
              (:constant (constant-displacement other-pointer-lowtag
                                                8 vector-data-offset))
              double-float)
  (:generator 19
   (unpoison-element object (+ index addend))
   (inst movsd (float-ref-ea object index addend 8) value)))

;;; complex float variants

(define-vop (data-vector-ref-with-offset/simple-array-complex-single-float dvref)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:info addend)
  (:arg-types simple-array-complex-single-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag
                                                8 vector-data-offset)))
  (:results (value :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 5
    (inst movq value (float-ref-ea object index addend 8
                                            :scale (ash 1 (- word-shift n-fixnum-tag-bits))))))

(define-vop (data-vector-ref-with-offset/simple-array-complex-single-float-c dvref)
  (:args (object :scs (descriptor-reg)))
  (:info index addend)
  (:arg-types simple-array-complex-single-float (:constant low-index)
              (:constant (constant-displacement other-pointer-lowtag
                                                8 vector-data-offset)))
  (:results (value :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 4
    (inst movq value (float-ref-ea object index addend 8))))

(define-vop (data-vector-set-with-offset/simple-array-complex-single-float dvset)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (complex-single-reg)))
  (:info addend)
  (:arg-types simple-array-complex-single-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag
                                                8 vector-data-offset))
              complex-single-float)
  (:generator 5
    (unpoison-element object index addend)
    (inst movq (float-ref-ea object index addend 8
                                      :scale (ash 1 (- word-shift n-fixnum-tag-bits)))
          value)))

(define-vop (data-vector-set-with-offset/simple-array-complex-single-float-c dvset)
  (:args (object :scs (descriptor-reg))
         (value :scs (complex-single-reg)))
  (:info index addend)
  (:arg-types simple-array-complex-single-float (:constant low-index)
              (:constant (constant-displacement other-pointer-lowtag
                                                8 vector-data-offset))
              complex-single-float)
  (:generator 4
    (unpoison-element object (+ index addend))
    (inst movq (float-ref-ea object index addend 8) value)))

(define-vop (data-vector-ref-with-offset/simple-array-complex-double-float dvref)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:info addend)
  (:arg-types simple-array-complex-double-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag
                                                16 vector-data-offset)))
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 7
    (inst movapd value (float-ref-ea object index addend 16
                                              :scale (ash 2 (- word-shift n-fixnum-tag-bits))))))

(define-vop (data-vector-ref-with-offset/simple-array-complex-double-float-c dvref)
  (:args (object :scs (descriptor-reg)))
  (:info index addend)
  (:arg-types simple-array-complex-double-float (:constant low-index)
              (:constant (constant-displacement other-pointer-lowtag
                                                16 vector-data-offset)))
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 6
    (inst movapd value (float-ref-ea object index addend 16))))

(define-vop (data-vector-set-with-offset/simple-array-complex-double-float dvset)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (complex-double-reg)))
  (:info addend)
  (:arg-types simple-array-complex-double-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag
                                                16 vector-data-offset))
              complex-double-float)
  (:generator 20
    (unpoison-element object index addend)
    (inst movapd (float-ref-ea object index addend 16
                                        :scale (ash 2 (- word-shift n-fixnum-tag-bits)))
          value)))

(define-vop (data-vector-set-with-offset/simple-array-complex-double-float-c dvset)
  (:args (object :scs (descriptor-reg))
         (value :scs (complex-double-reg)))
  (:info index addend)
  (:arg-types simple-array-complex-double-float (:constant low-index)
              (:constant (constant-displacement other-pointer-lowtag
                                                16 vector-data-offset))
              complex-double-float)
  (:generator 19
    (unpoison-element object (+ index addend))
    (inst movapd (float-ref-ea object index addend 16) value)))



;;; {un,}signed-byte-{8,16,32} and characters
(macrolet ((define-data-vector-frobs (ptype mov-inst operand-size
                                            type &rest scs)
  (binding* ((opcode-modifier (if (eq mov-inst 'mov)
                                  operand-size
                                  `(,operand-size ,(if (eq mov-inst 'movzx) :dword :qword))))
             (n-bytes (the (member 1 2 4) (size-nbyte operand-size)))
             ((index-scs scale)
              (if (>= n-bytes (ash 1 n-fixnum-tag-bits))
                  (values '(any-reg signed-reg unsigned-reg) `(index-scale ,n-bytes index))
                  (values '(signed-reg unsigned-reg) n-bytes)))
             (ea-expr `(ea (+ (* vector-data-offset n-word-bytes)
                              (* addend ,n-bytes)
                              (- other-pointer-lowtag))
                           object index ,scale))
             (ea-expr-const `(ea (+ (* vector-data-offset n-word-bytes)
                                    (* ,n-bytes (+ index addend))
                                    (- other-pointer-lowtag))
                                 object)))
    `(progn
         (define-vop (,(symbolicate "DATA-VECTOR-REF-WITH-OFFSET/" ptype) dvref)
           (:args (object :scs (descriptor-reg))
                  (index :scs ,index-scs))
           (:info addend)
           (:arg-types ,ptype tagged-num
                       (:constant (constant-displacement other-pointer-lowtag
                                                         ,n-bytes vector-data-offset)))
           (:results (value :scs ,scs))
           (:result-types ,type)
           (:generator 5 (inst ,mov-inst ',opcode-modifier value ,ea-expr)))
         (define-vop (,(symbolicate "DATA-VECTOR-REF-WITH-OFFSET/" ptype "-C") dvref)
           (:args (object :scs (descriptor-reg)))
           (:info index addend)
           (:arg-types ,ptype (:constant low-index)
                       (:constant (constant-displacement other-pointer-lowtag
                                                         ,n-bytes vector-data-offset)))
           (:results (value :scs ,scs))
           (:result-types ,type)
           (:generator 4 (inst ,mov-inst ',opcode-modifier value ,ea-expr-const)))
         (define-vop (,(symbolicate "DATA-VECTOR-SET-WITH-OFFSET/" ptype) dvset)
           (:args (object :scs (descriptor-reg) :to (:eval 0))
                  (index :scs ,index-scs :to (:eval 0))
                  (value :scs (,@scs immediate)))
           (:info addend)
           (:arg-types ,ptype tagged-num
                       (:constant (constant-displacement other-pointer-lowtag
                                                         ,n-bytes vector-data-offset))
                       ,type)
           (:generator 5
            (unpoison-element object index addend)
            (inst mov ,operand-size ,ea-expr (encode-value-if-immediate value nil))))
         (define-vop (,(symbolicate "DATA-VECTOR-SET-WITH-OFFSET/" ptype "-C") dvset)
           (:args (object :scs (descriptor-reg) :to (:eval 0))
                  (value :scs (,@scs immediate)))
           (:info index addend)
           (:arg-types ,ptype (:constant low-index)
                       (:constant (constant-displacement other-pointer-lowtag
                                                         ,n-bytes vector-data-offset))
                       ,type)
           (:generator 4
            (unpoison-element object (+ index addend))
            (inst mov ,operand-size ,ea-expr-const (encode-value-if-immediate value nil))))))))
  (define-data-vector-frobs simple-array-unsigned-byte-7 movzx :byte
    positive-fixnum unsigned-reg signed-reg)
  (define-data-vector-frobs simple-array-unsigned-byte-8 movzx :byte
    positive-fixnum unsigned-reg signed-reg)
  (define-data-vector-frobs simple-array-signed-byte-8 movsx :byte
    tagged-num signed-reg)
  (define-data-vector-frobs simple-base-string movzx :byte
     character character-reg)
  (define-data-vector-frobs simple-array-unsigned-byte-15 movzx :word
    positive-fixnum unsigned-reg signed-reg)
  (define-data-vector-frobs simple-array-unsigned-byte-16 movzx :word
    positive-fixnum unsigned-reg signed-reg)
  (define-data-vector-frobs simple-array-signed-byte-16 movsx :word
    tagged-num signed-reg)
  (define-data-vector-frobs simple-array-unsigned-byte-32 mov :dword
    positive-fixnum unsigned-reg signed-reg)
  (define-data-vector-frobs simple-array-unsigned-byte-31 mov :dword
    positive-fixnum unsigned-reg signed-reg)
  (define-data-vector-frobs simple-array-signed-byte-32 movsx :dword
    tagged-num signed-reg)
  #+sb-unicode
  (define-data-vector-frobs simple-character-string mov :dword
    character character-reg))


;;; These vops are useful for accessing the bits of a vector
;;; irrespective of what type of vector it is.
(define-full-reffer vector-raw-bits * vector-data-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num %vector-raw-bits)
(define-full-setter set-vector-raw-bits * vector-data-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num %set-vector-raw-bits)

;;; Weak vectors
(define-full-reffer %weakvec-ref * vector-data-offset other-pointer-lowtag
  (any-reg descriptor-reg) * %weakvec-ref)
(define-full-setter %weakvec-set * vector-data-offset other-pointer-lowtag
  (any-reg descriptor-reg) * %weakvec-set)

;;;; ATOMIC-INCF for arrays

(define-vop (array-atomic-incf/word)
  (:translate %array-atomic-incf/word)
  (:policy :fast-safe)
  (:args (array :scs (descriptor-reg))
         (index :scs (any-reg))
         (diff :scs (unsigned-reg) :target result))
  (:arg-types * positive-fixnum unsigned-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 4
    (inst xadd :lock
          (ea (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)
              array index (ash 1 (- word-shift n-fixnum-tag-bits)))
          diff)
    (move result diff)))
