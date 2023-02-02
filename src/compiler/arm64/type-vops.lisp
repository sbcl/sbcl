;;;; type testing and checking VOPs for the ARM VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(defun %test-fixnum (value temp target not-p)
  (declare (ignore temp))
  (assemble ()
    #.(assert (= fixnum-tag-mask 1))
    (if not-p
        (inst tbnz* value 0 target)
        (inst tbz* value 0 target))))

(defun %test-fixnum-immediate-and-headers (value temp target not-p immediate
                                           headers &key value-tn-ref)
  (let ((drop-through (gen-label)))
    #.(assert (= fixnum-tag-mask 1))
    (inst tbz* value 0 (if not-p drop-through target))
    (%test-immediate-and-headers value temp target not-p immediate headers
                                 :drop-through drop-through
                                 :value-tn-ref value-tn-ref)))

(defun %test-immediate-and-headers (value temp target not-p immediate headers
                                    &key (drop-through (gen-label))
                                         value-tn-ref)
  (cond ((= immediate single-float-widetag)
         (inst cmp (32-bit-reg value) single-float-widetag))
        (t
         (inst mov temp immediate)
         (inst cmp temp (extend value :uxtb))))
  (inst b :eq (if not-p drop-through target))
  (%test-headers value temp target not-p nil headers
                 :drop-through drop-through
                 :value-tn-ref value-tn-ref))

(defun %test-fixnum-and-headers (value temp target not-p headers
                                 &key value-tn-ref)
  (let ((drop-through (gen-label)))
    (assemble ()
      #.(assert (= fixnum-tag-mask 1))
      (inst tbz* value 0 (if not-p drop-through target)))
    (%test-headers value temp target not-p nil headers
                   :drop-through drop-through
                   :value-tn-ref value-tn-ref)))

(defun %test-immediate (value temp target not-p immediate)
  (cond ((= immediate unbound-marker-widetag)
         (inst cmp value immediate))
        (t
         (inst and temp value widetag-mask)
         (inst cmp temp immediate)))
  (inst b (if not-p :ne :eq) target))

(defun %test-lowtag (value temp target not-p lowtag)
  (assemble ()
    (inst and temp value lowtag-mask)
    (inst cmp temp lowtag)
    (inst b (if not-p :ne :eq) target)))

(defun %test-headers (value widetag target not-p function-p headers
                      &key (drop-through (gen-label))
                           value-tn-ref
                           (nil-in-other-pointers t))
  (let ((lowtag (if function-p fun-pointer-lowtag other-pointer-lowtag))
        (temp widetag))
    (flet ((%logical-mask (x)
             (cond ((encode-logical-immediate x)
                    x)
                   ;; The remaining bits of the widetag are zero, no
                   ;; need to mask them off. Possible to encode more
                   ;; values that way.
                   ((let ((extend (dpb x (byte 8 0) most-positive-word)))
                      (and (encode-logical-immediate extend)
                           extend)))
                   (t
                    (logical-mask x)))))
      (multiple-value-bind (when-true when-false)
          (if not-p
              (values drop-through target)
              (values target drop-through))
        (assemble ()
          (cond (widetag
                 (unless (and value-tn-ref
                              (eq lowtag other-pointer-lowtag)
                              (other-pointer-tn-ref-p value-tn-ref nil-in-other-pointers))
                   (%test-lowtag value widetag when-false t lowtag))
                 (load-type widetag value (- lowtag)))
                (t
                 (setf widetag value
                       temp tmp-tn)))
          (do ((remaining headers (cdr remaining)))
              ((null remaining))
            (let ((header (car remaining))
                  (last (null (cdr remaining))))
              (cond
                ((atom header)
                 (cond
                   ((and (not last) (null (cddr remaining))
                         (atom (cadr remaining))
                         (= (logcount (logxor header (cadr remaining))) 1))
                    (inst and temp widetag (%logical-mask
                                         (ldb (byte 8 0) (logeqv header (cadr remaining)))))
                    (inst cmp temp (ldb (byte 8 0) (logand header (cadr remaining))))
                    (inst b (if not-p :ne :eq) target)
                    (return))
                   (t
                    (inst cmp widetag header)
                    (if last
                        (inst b (if not-p :ne :eq) target)
                        (inst b :eq when-true)))))
                (t
                 (let ((start (car header))
                       (end (cdr header)))
                   (cond
                     ((and last (not (= start bignum-widetag))
                           (= (+ start 4) end)
                           (= (logcount (logxor start end)) 1))
                      (inst and temp widetag (%logical-mask
                                           (ldb (byte 8 0) (logeqv start end))))
                      (inst cmp temp (ldb (byte 8 0) (logand start end)))
                      (inst b (if not-p :ne :eq) target))
                     ((and (not last) (null (cddr remaining))
                           (= (+ start 4) end) (= (logcount (logxor start end)) 1)
                           (listp (cadr remaining))
                           (= (+ (caadr remaining) 4) (cdadr remaining))
                           (= (logcount (logxor (caadr remaining) (cdadr remaining))) 1)
                           (= (logcount (logxor (caadr remaining) start)) 1))
                      (inst and temp widetag (ldb (byte 8 0) (logeqv start (cdadr remaining))))
                      (inst cmp temp (ldb (byte 8 0) (logand start (cdadr remaining))))
                      (inst b (if not-p :ne :eq) target)
                      (return))
                     ((and last
                           (/= start bignum-widetag)
                           (/= end complex-array-widetag))
                      (inst sub temp widetag start)
                      (inst cmp temp (- end start))
                      (inst b (if not-p :hi :ls) target))
                     (t
                      (unless (= start bignum-widetag)
                        (inst cmp widetag start)
                        (if (= end complex-array-widetag)
                            (progn
                              (aver last)
                              (inst b (if not-p :lt :ge) target))
                            (inst b :lt when-false)))
                      (unless (= end complex-array-widetag)
                        (inst cmp widetag end)
                        (if last
                            (inst b (if not-p :gt :le) target)
                            (inst b :le when-true))))))))))
          (emit-label drop-through))))))

;;;; Other integer ranges.

;;; A (signed-byte 64) can be represented with either fixnum or a bignum with
;;; exactly one digit.
(define-vop (signed-byte-64-p type-predicate)
  (:translate signed-byte-64-p)
  (:generator 10
    (let ((fixnum-p (types-equal-or-intersect (tn-ref-type args) (specifier-type 'fixnum))))
      (multiple-value-bind (yep nope)
          (if not-p
              (values not-target target)
              (values target not-target))
        (assemble ()
          (when fixnum-p
            (inst tbz* value 0 yep))
          (unless (fixnum-or-other-pointer-tn-ref-p args t)
            (test-type value temp nope t (other-pointer-lowtag)))
          (loadw temp value 0 other-pointer-lowtag)
          (inst cmp temp (+ (ash 1 n-widetag-bits) bignum-widetag))
          (inst b (if not-p :ne :eq) target))))
    not-target))

(define-vop (signed-byte-64-p/unsigned)
  (:args (value :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:conditional :eq)
  (:policy :fast-safe)
  (:translate signed-byte-64-p)
  (:generator 5
    (inst tst value (ash 1 (1- n-word-bits)))))

;;; An (UNSIGNED-BYTE 64) can be represented with either a positive
;;; fixnum, a bignum with exactly one positive digit, or a bignum with
;;; exactly two digits and the second digit all zeros.
(define-vop (unsigned-byte-64-p type-predicate)
  (:translate unsigned-byte-64-p)
  (:generator 10
    (let ((fixnum-p (types-equal-or-intersect (tn-ref-type args) (specifier-type 'fixnum)))
          (other-pointer-p (fixnum-or-other-pointer-tn-ref-p args t)))
      (multiple-value-bind (yep nope)
          (if not-p
              (values not-target target)
              (values target not-target))
        (assemble ()
          (cond ((not other-pointer-p)
                 ;; Move to a temporary and mask off the lowtag,
                 ;; but leave the sign bit for testing for positive fixnums.
                 ;; When using 32-bit registers that bit will not be visible.
                 (inst and temp value (logior (ash 1 (1- n-word-bits)) lowtag-mask)))
                (fixnum-p
                 (move temp value)))
          (when fixnum-p
            (%test-fixnum temp nil fixnum nil))
          (unless other-pointer-p
            (inst cmp (32-bit-reg temp) other-pointer-lowtag)
            (inst b :ne nope))
          ;; Get the header.
          (loadw temp value 0 other-pointer-lowtag)
          ;; Is it one?
          (inst cmp temp (+ (ash 1 n-widetag-bits) bignum-widetag))
          (inst b :eq single-word)
          ;; If it's other than two, it can't be an (unsigned-byte 64)
          (inst cmp temp (+ (ash 2 n-widetag-bits) bignum-widetag))
          (inst b :ne nope)
          ;; Get the second digit.
          (loadw temp value (1+ bignum-digits-offset) other-pointer-lowtag)
          ;; All zeros, it's an (unsigned-byte 64).
          (inst cbz temp yep)
          (inst b nope)

          single-word
          ;; Get the single digit.
          (loadw temp value bignum-digits-offset other-pointer-lowtag)

          ;; positive implies (unsigned-byte 64).
          fixnum
          (if not-p
              (inst tbnz* temp (1- n-word-bits) target)
              (inst tbz* temp (1- n-word-bits) target))))
      (values))
    NOT-TARGET))

(define-vop (fixnump/unsigned)
  (:policy :fast-safe)
  (:args (value :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:translate fixnump)
  (:conditional :eq)
  (:generator 3
    (inst tst value (ash (1- (ash 1 (- n-word-bits
                                   n-positive-fixnum-bits)))
                     n-positive-fixnum-bits))))

(define-vop (fixnump/signed)
  (:args (value :scs (signed-reg)))
  (:policy :fast-safe)
  (:conditional :vc)
  (:info)
  (:arg-types signed-num)
  (:translate fixnump)
  (:generator 3
    (inst adds zr-tn value value)))

(progn
  (define-vop (>-integer-fixnum)
    (:translate >)
    (:args (integer :scs (descriptor-reg))
           (fixnum :scs (immediate any-reg)))
    (:arg-types (:or integer bignum) tagged-num)
    (:temporary (:sc non-descriptor-reg) temp)
    (:conditional)
    (:info target not-p)
    (:arg-refs integer-ref)
    (:policy :fast-safe)
    (:variant-vars comparison)
    (:variant :gt)
    (:generator 10
      (let* ((integer-p (csubtypep (tn-ref-type integer-ref) (specifier-type 'integer)))
             (other-pointer-p (fixnum-or-other-pointer-tn-ref-p integer-ref t))
             negative-p
             (fixnum-value (and (sc-is fixnum immediate)
                                (tn-value fixnum))))
        (flet ((load-fixnum ()
                 (when (integerp fixnum-value)
                   (setf fixnum
                         (let* ((fixnum (fixnumize fixnum-value))
                                (abs (abs fixnum)))
                           (cond ((add-sub-immediate-p abs)
                                  (when (minusp fixnum)
                                    (setf negative-p t))
                                  abs)
                                 (t
                                  (add-sub-immediate fixnum))))))))
         (multiple-value-bind (yep nope)
             (if not-p
                 (values not-target target)
                 (values target not-target))
           (assemble ()
             (when (types-equal-or-intersect (tn-ref-type integer-ref) (specifier-type 'fixnum))
               (cond ((and (not other-pointer-p)
                           (and (eql fixnum-value 0)
                                (eq comparison :ge)))
                      (inst tst integer (lognot (fixnumize most-positive-fixnum)))
                      (inst b :eq yep))
                     ((and (eql fixnum-value most-positive-fixnum)
                           (case comparison
                             (:gt
                              (inst tbz* integer 0 nope)
                              t)
                             (:le
                              (inst tbz* integer 0 yep)
                              t))))
                     ((and (eql fixnum-value most-negative-fixnum)
                           (case comparison
                             (:lt
                              (inst tbz* integer 0 nope)
                              t)
                             (:ge
                              (inst tbz* integer 0 yep)
                              t))))
                     (t
                      (inst tbnz integer 0 bignum)
                      (load-fixnum)
                      (cond
                        ((and (eql fixnum 0)
                              (case comparison
                                (:lt
                                 (inst tbnz* integer 63 yep)
                                 t)
                                (:ge
                                 (inst tbz* integer 63 yep)
                                 t))))
                        ((and (eql fixnum-value -1)
                              (case comparison
                                (:le
                                 (inst tbnz* integer 63 yep)
                                 t)
                                (:gt
                                 (inst tbz* integer 63 yep)
                                 t))))
                        (negative-p
                         (inst cmn integer fixnum)
                         (inst b comparison yep))
                        (t
                         (inst cmp integer fixnum)
                         (inst b comparison yep)))
                      (inst b nope))))
             bignum
             (unless other-pointer-p
               (test-type integer temp nope t (other-pointer-lowtag)))
             (loadw temp integer 0 other-pointer-lowtag)
             (unless integer-p
               (inst and tmp-tn temp widetag-mask)
               (inst cmp tmp-tn bignum-widetag)
               (inst b :ne nope))
             #.(assert (= (integer-length bignum-widetag) 5))
             (inst add temp integer (lsr temp 5))
             (inst ldr temp (@ temp (- other-pointer-lowtag)))
             (if (case comparison
                   ((:gt :ge) not-p)
                   (t (not not-p)))
                 (inst tbnz* temp (1- n-word-bits) target)
                 (inst tbz* temp (1- n-word-bits) target))))))
      not-target))

  (define-vop (<-integer-fixnum >-integer-fixnum)
    (:translate <)
    (:variant :lt))

  (define-vop (>-fixnum-integer >-integer-fixnum)
    (:translate >)
    (:args (fixnum :scs (immediate any-reg))
           (integer :scs (descriptor-reg)))
    (:arg-types tagged-num (:or integer bignum))
    (:arg-refs nil integer-ref)
    (:variant :lt))

  (define-vop (<-fixnum-integer >-fixnum-integer)
    (:translate <)
    (:variant :gt))

;;; For integerp+cmp
  (define-vop (<=-integer-fixnum >-integer-fixnum)
    (:translate)
    (:variant :le))
  (define-vop (>=-integer-fixnum <-integer-fixnum)
    (:translate)
    (:variant :ge))
  (define-vop (<=-fixnum-integer >-fixnum-integer)
    (:translate)
    (:variant :ge))
  (define-vop (>=-fixnum-integer <-fixnum-integer)
    (:translate)
    (:variant :le)))


;;; MOD type checks
(defun power-of-two-limit-p (x)
  (and (fixnump x)
       (= (logcount (1+ x)) 1)))

(define-vop (test-fixnum-mod-power-of-two)
  (:args (value :scs (any-reg descriptor-reg
                      unsigned-reg signed-reg)))
  (:arg-types *
              (:constant (satisfies power-of-two-limit-p)))
  (:translate fixnum-mod-p)
  (:conditional :eq)
  (:info hi)
  (:policy :fast-safe)
  (:generator 2
     (let* ((fixnum-hi (if (sc-is value unsigned-reg signed-reg)
                           hi
                           (fixnumize hi))))
       (inst tst value (lognot fixnum-hi)))))

(defun add-sub-immediate-p+1 (x)
  (add-sub-immediate-p (1+ x)))

(defun fixnum-add-sub-immediate-p+1 (x)
  (fixnum-add-sub-immediate-p (1+ x)))

(defun fixnum-add-sub-immediate-p/+1 (x)
  (or (fixnum-add-sub-immediate-p x)
      (fixnum-add-sub-immediate-p (1+ x))))

(define-vop (test-fixnum-mod-signed-unsigned-imm)
  (:args (value :scs (unsigned-reg signed-reg)))
  (:arg-types (:or unsigned-num signed-num)
              (:constant (satisfies add-sub-immediate-p)))
  (:translate fixnum-mod-p)
  (:conditional :ls)
  (:info hi)
  (:policy :fast-safe)
  (:generator 3
    (inst cmp value hi)))

(define-vop (test-fixnum-mod-signed-unsigned-imm+1 test-fixnum-mod-signed-unsigned-imm)
  (:arg-types (:or unsigned-num signed-num)
              (:constant (satisfies add-sub-immediate-p+1)))
  (:conditional :lo)
  (:generator 3
    (inst cmp value (1+ hi))))

(define-vop (test-fixnum-mod-tagged-imm)
  (:args (value :scs (any-reg)))
  (:arg-types tagged-num
              (:constant (satisfies fixnum-add-sub-immediate-p)))
  (:translate fixnum-mod-p)
  (:conditional :ls)
  (:info hi)
  (:policy :fast-safe)
  (:generator 3
    (inst cmp value (fixnumize hi))))

(define-vop (test-fixnum-mod-tagged-imm+1 test-fixnum-mod-tagged-imm)
  (:arg-types tagged-num
              (:constant (satisfies fixnum-add-sub-immediate-p+1)))
  (:conditional :lo)
  (:generator 3
    (inst cmp value (fixnumize (1+ hi)))))

(define-vop (test-fixnum-mod-signed-unsigned-imm)
  (:args (value :scs (unsigned-reg signed-reg)))
  (:arg-types (:or unsigned-num signed-num)
              (:constant (satisfies add-sub-immediate-p)))
  (:translate fixnum-mod-p)
  (:conditional :ls)
  (:info hi)
  (:policy :fast-safe)
  (:generator 3
    (inst cmp value hi)))

(define-vop (test-fixnum-mod-signed-unsigned-imm+1 test-fixnum-mod-signed-unsigned-imm)
  (:arg-types (:or unsigned-num signed-num)
              (:constant (satisfies add-sub-immediate-p+1)))
  (:conditional :lo)
  (:generator 3
    (inst cmp value (1+ hi))))

(define-vop (test-fixnum-mod-tagged-imm)
  (:args (value :scs (any-reg)))
  (:arg-types tagged-num
              (:constant (satisfies fixnum-add-sub-immediate-p)))
  (:translate fixnum-mod-p)
  (:conditional :ls)
  (:info hi)
  (:policy :fast-safe)
  (:generator 3
    (inst cmp value (fixnumize hi))))

(define-vop (test-fixnum-mod-tagged-imm+1 test-fixnum-mod-tagged-imm)
  (:arg-types tagged-num
              (:constant (satisfies fixnum-add-sub-immediate-p+1)))
  (:conditional :lo)
  (:generator 3
    (inst cmp value (fixnumize (1+ hi)))))

(define-vop (test-fixnum-mod-tagged-unsigned)
  (:args (value :scs (any-reg unsigned-reg signed-reg)))
  (:arg-types (:or tagged-num unsigned-num signed-num)
              (:constant fixnum))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:translate fixnum-mod-p)
  (:conditional :ls)
  (:info hi)
  (:policy :fast-safe)
  (:generator 4
    (let ((fixnum-hi (if (sc-is value unsigned-reg signed-reg)
                         hi
                         (fixnumize hi))))
      (load-immediate-word temp fixnum-hi)
      (inst cmp value temp))))

(define-vop (test-fixnum-mod-*-imm)
  (:args (value :scs (any-reg descriptor-reg)))
  (:arg-types * (:constant (satisfies fixnum-add-sub-immediate-p/+1)))
  (:translate fixnum-mod-p)
  (:conditional)
  (:info target not-p hi)
  (:policy :fast-safe)
  (:generator 5
    (let* ((1+ (not (fixnum-add-sub-immediate-p hi)))
           (fixnum-hi (fixnumize (if 1+
                                     (1+ hi)
                                     hi))))
      #.(assert (= fixnum-tag-mask 1))
      (inst tbnz* value 0 (if not-p target skip))
      (inst cmp value fixnum-hi)
      (inst b (if not-p
                  (if 1+ :hs :hi)
                  (if 1+ :lo :ls))
            target))
    skip))

(define-vop (test-fixnum-mod-*)
  (:args (value :scs (any-reg descriptor-reg)))
  (:arg-types * (:constant fixnum))
  (:translate fixnum-mod-p)
  (:temporary (:scs (any-reg)) temp)
  (:conditional)
  (:info target not-p hi)
  (:policy :fast-safe)
  (:generator 6
    #.(assert (= fixnum-tag-mask 1))
    (inst tbnz* value 0 (if not-p target skip))
    (let ((condition (if not-p :hi :ls)))
      (load-immediate-word temp (fixnumize hi))
      (inst cmp value temp)
      (inst b condition target))
    SKIP))

;;;; List/symbol types:
;;;
;;; symbolp (or symbol (eq nil))
;;; consp (and list (not (eq nil)))

(define-vop (symbolp type-predicate)
  (:translate symbolp)
  (:generator 12
    (unless (other-pointer-tn-ref-p args t)
      (inst cmp value null-tn)
      (inst b :eq (if not-p drop-thru target)))
    (test-type value temp target not-p (symbol-widetag)
               :value-tn-ref args)
    drop-thru))

(define-vop (non-null-symbol-p type-predicate)
  (:translate non-null-symbol-p)
  (:generator 7
    (when (types-equal-or-intersect (tn-ref-type args) (specifier-type 'null))
      (inst cmp value null-tn)
      (inst b :eq (if not-p target drop-thru)))
    (test-type value temp target not-p (symbol-widetag) :value-tn-ref args)
    drop-thru))

(define-vop (consp type-predicate)
  (:translate consp)
  (:conditional :ne)
  (:info)
  (:generator 8
    (inst and temp value lowtag-mask)
    (inst cmp temp list-pointer-lowtag)
    (inst ccmp value null-tn :eq 4)))

(define-vop (single-float-p)
  (:args (value :scs (any-reg descriptor-reg)))
  (:conditional :eq)
  (:policy :fast-safe)
  (:translate single-float-p)
  (:generator 7
    (inst cmp (32-bit-reg value) single-float-widetag)))

(define-vop (load-other-pointer-widetag)
  (:args (value :scs (any-reg descriptor-reg)))
  (:arg-refs args)
  (:info not-other-pointer-label null-label)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (unless (other-pointer-tn-ref-p args)
      (when null-label
        (inst cmp value null-tn)
        (inst b :eq null-label))
      (inst and r value lowtag-mask)
      (inst cmp r other-pointer-lowtag)
      (inst b :ne not-other-pointer-label))
    (load-type r value (- other-pointer-lowtag))))

(define-vop (test-widetag)
  (:args (value :scs (unsigned-reg)))
  (:info target not-p type-codes)
  (:generator 1
    (%test-headers value nil target not-p nil
      (if (every #'integerp type-codes)
          (canonicalize-widetags type-codes)
          type-codes))))

(define-vop (load-instance-layout)
  (:args (object :scs (any-reg descriptor-reg)))
  (:arg-refs args)
  (:info not-instance)
  (:results (r :scs (descriptor-reg)))
  (:generator 1
    (unless (instance-tn-ref-p args)
      (inst and tmp-tn object lowtag-mask)
      (inst cmp tmp-tn instance-pointer-lowtag)
      (inst b :ne not-instance))
    (loadw r object instance-slots-offset instance-pointer-lowtag)))

(defun structure-is-a (layout temp this-id test-layout &optional desc-temp target not-p done)
  (cond ((integerp test-layout)
         (inst ldrsw temp
               (@ layout
                  (- (ash (+ instance-slots-offset
                             (get-dsd-index layout sb-kernel::flags))
                          word-shift)
                     instance-pointer-lowtag)))
         (inst tst temp test-layout))
        ((and desc-temp
              (neq (tn-kind desc-temp) :unused))
         (inst load-constant desc-temp
               (tn-byte-offset (emit-constant test-layout)))
         (inst cmp layout desc-temp))
        (t
         (let* ((test-id (layout-id test-layout))
                (depthoid (wrapper-depthoid test-layout))
                (offset (+ (id-bits-offset)
                           (ash (- depthoid 2) 2)
                           (- instance-pointer-lowtag))))
           (when (and target
                      (> depthoid sb-kernel::layout-id-vector-fixed-capacity))
             (inst ldrsw temp
                   (@ layout
                      (- (+ #+little-endian 4
                            (ash (+ instance-slots-offset
                                    (get-dsd-index layout sb-kernel::flags))
                                 word-shift))
                         instance-pointer-lowtag)))
             (inst cmp temp (add-sub-immediate (fixnumize depthoid)))
             (inst b :lt (if not-p target done)))
           (inst ldr (32-bit-reg this-id) (@ layout offset))
           ;; 8-bit IDs are permanently assigned, so no fixup ever needed for those.
           (cond ((typep test-id '(and (signed-byte 8) (not (eql 0))))
                  (if (minusp test-id)
                      (inst cmn (32-bit-reg this-id) (- test-id))
                      (inst cmp (32-bit-reg this-id) test-id)))
                 (t
                  (destructuring-bind (size . label)
                      ;; This uses the bogus definition of :dword, the one which
                      ;; emits 4 bytes. _technically_ dword should be 8 bytes.
                      (register-inline-constant :dword `(:layout-id ,test-layout))
                    (declare (ignore size))
                    (inst load-from-label (32-bit-reg temp) label))
                  (inst cmp (32-bit-reg this-id) (32-bit-reg temp))))))))

;;; This could be split into two vops to avoid wasting an allocation for 'temp'
;;; when the immediate form is used.
(define-vop ()
  (:translate sb-c::%structure-is-a)
  (:args (x :scs (descriptor-reg)))
  (:arg-types * (:constant t))
  (:policy :fast-safe)
  (:conditional :eq)
  (:info test-layout)
  (:temporary (:sc unsigned-reg) this-id temp)
  (:generator 4
    (structure-is-a x temp this-id test-layout)))

(define-vop ()
  (:translate sb-c::structure-typep)
  (:args (object :scs (descriptor-reg)))
  (:arg-types * (:constant t))
  (:arg-refs args)
  (:policy :fast-safe)
  (:conditional)
  (:info target not-p test-layout)
  (:temporary (:sc descriptor-reg) layout)
  (:temporary (:sc unsigned-reg
               :unused-if
               (and (instance-tn-ref-p args)
                    #1=(and (not (integerp test-layout))
                            (let ((classoid (wrapper-classoid test-layout)))
                              (and (eq (classoid-state classoid) :sealed)
                                   (not (classoid-subclasses classoid)))))))
              temp)
  (:temporary (:sc unsigned-reg
               :unused-if (or (integerp test-layout)
                              #1#))
              this-id)
  (:temporary (:sc descriptor-reg
               :unused-if (not #1#))
              desc-temp)
  (:generator 4
    (unless (instance-tn-ref-p args)
      (inst and temp object lowtag-mask)
      (inst cmp temp instance-pointer-lowtag)
      (inst b :ne (if not-p target done)))
    (loadw layout object instance-slots-offset instance-pointer-lowtag)
    (structure-is-a layout temp this-id test-layout desc-temp target not-p done)
    (inst b (if (if (integerp test-layout)
                    (not not-p)
                    not-p)
                :ne :eq) target)
    done))

(define-vop (structure-typep*)
  (:args (layout :scs (descriptor-reg)))
  (:arg-types * (:constant t))
  (:policy :fast-safe)
  (:conditional)
  (:info target not-p test-layout)
  (:temporary (:sc unsigned-reg
               :unused-if
               #1=(and (not (integerp test-layout))
                       (let ((classoid (wrapper-classoid test-layout)))
                         (and (eq (classoid-state classoid) :sealed)
                              (not (classoid-subclasses classoid))))))
              temp)
  (:temporary (:sc unsigned-reg
               :unused-if (or (integerp test-layout)
                              #1#))
              this-id)
  (:temporary (:sc descriptor-reg
               :unused-if (not #1#))
              desc-temp)
  (:generator 4
    (structure-is-a layout temp this-id test-layout desc-temp target not-p done)
    (inst b (if (if (integerp test-layout)
                    (not not-p)
                    not-p)
                :ne :eq) target)
    done))

(define-vop (keywordp type-predicate)
  (:translate keywordp)
  (:generator 3
    #.(assert (= sb-impl::package-id-bits 16))
    (unless (csubtypep (tn-ref-type args) (specifier-type 'symbol))
      (test-type value temp (if not-p target not-target) t (symbol-widetag)
                 :value-tn-ref args))
    (inst ldrh temp (@ value (+ (ash symbol-name-slot word-shift)
                               (- other-pointer-lowtag)
                               6)))
    (inst cmp temp sb-impl::+package-id-keyword+)
    (inst b (if not-p :ne :eq) target)
    not-target))
