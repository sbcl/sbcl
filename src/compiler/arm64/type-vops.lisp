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
                                           headers &key value-tn-ref immediate-tested)
  (let ((drop-through (gen-label)))
    #.(assert (= fixnum-tag-mask 1))
    (when (types-equal-or-intersect (tn-ref-type value-tn-ref)
                                    (specifier-type 'fixnum))
      (inst tbz* value 0 (if not-p drop-through target)))
    (%test-immediate-and-headers value temp target not-p immediate headers
                                 :drop-through drop-through
                                 :value-tn-ref value-tn-ref
                                 :immediate-tested immediate-tested)))

(defun %test-immediate-and-headers (value temp target not-p immediate headers
                                    &key (drop-through (gen-label))
                                         value-tn-ref
                                         immediate-tested)
  (multiple-value-bind (bit set) (tn-ref-lowtag-bit immediate value-tn-ref)
    (case set
      (1
       (inst tbnz* value bit (if not-p drop-through target)))
      (0
       (inst tbz* value bit (if not-p drop-through target)))
      (t
       (cond ((= immediate single-float-widetag)
              (when (types-equal-or-intersect (tn-ref-type value-tn-ref)
                                              (specifier-type 'single-float))
                (inst cmp (32-bit-reg value) single-float-widetag)
                (inst b :eq (if not-p drop-through target))))
             (t
              (inst mov temp immediate)
              (inst cmp temp (extend value :uxtb))
              (inst b :eq (if not-p drop-through target)))))))
  (%test-headers value temp target not-p nil headers
                 :drop-through drop-through
                 :value-tn-ref value-tn-ref
                 :immediate-tested immediate-tested))

(defun %test-fixnum-and-headers (value temp target not-p headers
                                 &key value-tn-ref
                                      immediate-tested)
  (let ((drop-through (gen-label)))
    (assemble ()
      #.(assert (= fixnum-tag-mask 1))
      (when (types-equal-or-intersect (tn-ref-type value-tn-ref)
                                      (specifier-type 'fixnum))
        (inst tbz* value 0 (if not-p drop-through target))))
    (%test-headers value temp target not-p nil headers
                   :drop-through drop-through
                   :value-tn-ref value-tn-ref
                   :immediate-tested immediate-tested)))

(defun %test-immediate (value temp target not-p immediate &key value-tn-ref)
  (multiple-value-bind (bit set) (tn-ref-lowtag-bit immediate value-tn-ref)
    (case set
      (1
       (if not-p
           (inst tbz* value bit target)
           (inst tbnz* value bit target)))
      (0
       (if not-p
           (inst tbnz* value bit target)
           (inst tbz* value bit target)))
      (t
       (cond ((= immediate unbound-marker-widetag)
              (inst cmp value immediate))
             (t
              (inst and temp value widetag-mask)
              (inst cmp temp immediate)))
       (inst b (if not-p :ne :eq) target)))))

(defun %test-lowtag (value temp target not-p lowtag &key value-tn-ref)
  (multiple-value-bind (bit set) (tn-ref-lowtag-bit lowtag value-tn-ref)
    (case set
      (1
       (if not-p
           (inst tbz* value bit target)
           (inst tbnz* value bit target)))
      (0
       (if not-p
           (inst tbnz* value bit target)
           (inst tbz* value bit target)))
      (t
       (inst and temp value lowtag-mask)
       (inst cmp temp lowtag)
       (inst b (if not-p :ne :eq) target)))))

(defun %test-headers (value widetag target not-p function-p headers
                      &key (drop-through (gen-label))
                           value-tn-ref
                           immediate-tested
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
          (flet ((check-widetag (target not-p)
                   (unless (and value-tn-ref
                                (= lowtag other-pointer-lowtag)
                                (other-pointer-tn-ref-p value-tn-ref nil-in-other-pointers immediate-tested))
                     (multiple-value-bind (bit set) (tn-ref-lowtag-bit lowtag value-tn-ref
                                                                       nil-in-other-pointers immediate-tested)
                       (when (and set (not not-p))
                         (setf set (logxor set 1)))
                       (case set
                         (1
                          (inst tbz* value bit target))
                         (0
                          (inst tbnz* value bit target))
                         (t
                          (%test-lowtag value widetag target not-p lowtag))))
                     t)))
            (cond
              ((and value-tn-ref
                    ;; Is testing only the lowtag enough?
                    (eq lowtag other-pointer-lowtag)
                    (let ((widetags (sb-c::type-other-pointer-widetags (tn-ref-type value-tn-ref))))
                      (when widetags
                        (loop for widetag in widetags
                              always
                              (loop for header in headers
                                    thereis (if (consp header)
                                                (<= (car header) widetag (cdr header))
                                                (eql widetag header)))))))
               (or (check-widetag target not-p)
                   (unless not-p
                     (inst b target))))
              (t
               (cond (widetag
                      (check-widetag when-false t)
                      (load-type widetag value (- lowtag)))
                     (t
                      (setf widetag value
                            temp tmp-tn)))
               (do ((remaining headers (cdr remaining)))
                   ((null remaining))
                 (let ((header (car remaining))
                       (last (null (cdr remaining))))
                   (cond
                     ((and (eql header simple-array-widetag)
                           value-tn-ref
                           (csubtypep (tn-ref-type value-tn-ref) (specifier-type 'string))))
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
                                value-tn-ref
                                (csubtypep (tn-ref-type value-tn-ref) (specifier-type 'array))
                                (= start simple-array-widetag))
                           (inst cmp widetag end)
                           (inst b (if not-p :gt :le) target))
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
                                 (inst b :le when-true)))))))))))))
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

(define-vop (signed-byte-64-p-move-to-word signed-byte-64-p)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:translate)
  (:generator 10
    (let ((fixnum-p (types-equal-or-intersect (tn-ref-type args) (specifier-type 'fixnum))))
      (multiple-value-bind (yep nope)
          (if not-p
              (values not-target target)
              (values target not-target))
        (assemble ()
          (inst asr r value n-fixnum-tag-bits)
          (when fixnum-p
            (inst tbz* value 0 yep))
          (unless (fixnum-or-other-pointer-tn-ref-p args t)
            (test-type value temp nope t (other-pointer-lowtag)))
          (loadw temp value 0 other-pointer-lowtag)
          (inst cmp temp (+ (ash 1 n-widetag-bits) bignum-widetag))
          (loadw r value bignum-digits-offset other-pointer-lowtag)
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
    (let* ((fixnum-p (types-equal-or-intersect (tn-ref-type args) (specifier-type 'fixnum)))
           (other-pointer-p (fixnum-or-other-pointer-tn-ref-p args t))
           (not-signed-byte-64-p (not (types-equal-or-intersect (tn-ref-type args) (specifier-type 'signed-word))))
           (unsigned-p (or not-signed-byte-64-p
                           (not (types-equal-or-intersect (tn-ref-type args) (specifier-type '(integer * -1)))))))
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
            (%test-fixnum temp nil (if unsigned-p
                                       yep
                                       fixnum) nil))
          (unless other-pointer-p
            (inst cmp (32-bit-reg temp) other-pointer-lowtag)
            (inst b :ne nope))
          ;; Get the header.
          (loadw temp value 0 other-pointer-lowtag)
          (unless not-signed-byte-64-p
            ;; Is it one?
            (inst cmp temp (+ (ash 1 n-widetag-bits) bignum-widetag))
            (inst b :eq (if unsigned-p
                            yep
                            single-word)))
          ;; If it's other than two, it can't be an (unsigned-byte 64)
          (inst cmp temp (+ (ash 2 n-widetag-bits) bignum-widetag))
          (inst b :ne nope)
          ;; Get the second digit.
          (loadw temp value (1+ bignum-digits-offset) other-pointer-lowtag)
          ;; All zeros, it's an (unsigned-byte 64).
          (cond (unsigned-p
                 (if not-p
                     (inst cbnz temp target)
                     (inst cbz temp target)))
                (t
                 (inst cbz temp yep)
                 (inst b nope)))

          single-word
          (unless unsigned-p
            ;; Get the single digit.
            (loadw temp value bignum-digits-offset other-pointer-lowtag))

          ;; positive implies (unsigned-byte 64).
          fixnum
          (unless unsigned-p
            (if not-p
                (inst tbnz* temp (1- n-word-bits) target)
                (inst tbz* temp (1- n-word-bits) target)))))
      (values))
    NOT-TARGET))

(define-vop (unsigned-byte-x-p type-predicate)
  (:arg-types * (:constant (integer 1)))
  (:translate sb-c::unsigned-byte-x-p)
  (:info target not-p x)
  (:temporary (:sc unsigned-reg) last-digit)
  (:generator 10
    (let* ((type (tn-ref-type args))
           (fixnum-p (types-equal-or-intersect type (specifier-type 'fixnum)))
           (integer-p (csubtypep type (specifier-type 'integer)))
           (other-pointer-p (fixnum-or-other-pointer-tn-ref-p args t))
           (unsigned-p (not (types-equal-or-intersect type (specifier-type '(integer * -1))))))
      (multiple-value-bind (yep nope)
          (if not-p
              (values not-target target)
              (values target not-target))
        (assemble ()
          (cond ((not other-pointer-p)
                 ;; Move to a temporary and mask off the lowtag,
                 ;; but leave the sign bit for testing for positive fixnums.
                 ;; When using 32-bit registers that bit will not be visible.
                 (inst and last-digit value (logior (ash 1 (1- n-word-bits)) lowtag-mask)))
                (fixnum-p
                 (move last-digit value)))
          (when fixnum-p
            (%test-fixnum last-digit nil (if unsigned-p
                                             yep
                                             fixnum) nil))
          (unless other-pointer-p
            (inst cmp (32-bit-reg last-digit) other-pointer-lowtag)
            (inst b :ne nope))
          ;; Get the header.
          (loadw temp value 0 other-pointer-lowtag)
          (unless integer-p
            (inst and tmp-tn temp widetag-mask)
            (inst cmp tmp-tn bignum-widetag)
            (inst b :ne nope))
          #.(assert (= (integer-length bignum-widetag) 5))
          (inst add last-digit value (lsr temp 5))
          (inst ldr last-digit (@ last-digit (- other-pointer-lowtag)))
          (inst lsr temp temp n-widetag-bits)
          (inst cmp temp (add-sub-immediate (1+ (/ x n-word-bits))))
          (inst b :gt nope)
          (inst b :lt (if unsigned-p
                          yep
                          fixnum))
          ;; Is it a sign-extended sign bit
          (cond ((not unsigned-p)
                 (inst cbnz last-digit nope))
                (not-p
                 (inst cbnz last-digit target))
                (t
                 (inst cbz last-digit target)))

          fixnum
          (unless unsigned-p
            (if not-p
                (inst tbnz* last-digit (1- n-word-bits) target)
                (inst tbz* last-digit (1- n-word-bits) target))))))
    NOT-TARGET))

(define-vop (unsigned-byte-64-p-move-to-word unsigned-byte-64-p)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate)
  (:generator 10
    (let* ((fixnum-p (types-equal-or-intersect (tn-ref-type args) (specifier-type 'fixnum)))
           (other-pointer-p (fixnum-or-other-pointer-tn-ref-p args t))
           (not-signed-byte-64-p (not (types-equal-or-intersect (tn-ref-type args) (specifier-type 'signed-word))))
           (unsigned-p (or not-signed-byte-64-p
                           (not (types-equal-or-intersect (tn-ref-type args) (specifier-type '(integer * -1)))))))
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
            (inst asr r value n-fixnum-tag-bits)
            (%test-fixnum temp nil (if unsigned-p
                                       yep
                                       fixnum) nil))
          (unless other-pointer-p
            (inst cmp (32-bit-reg temp) other-pointer-lowtag)
            (inst b :ne nope))
          ;; Get the header.
          (loadw temp value 0 other-pointer-lowtag)
          (loadw r value bignum-digits-offset other-pointer-lowtag)
          (unless not-signed-byte-64-p
            ;; Is it one?
            (inst cmp temp (+ (ash 1 n-widetag-bits) bignum-widetag))
            (inst b :eq (if unsigned-p
                            yep
                            single-word)))
          ;; If it's other than two, it can't be an (unsigned-byte 64)
          (inst cmp temp (+ (ash 2 n-widetag-bits) bignum-widetag))
          (inst b :ne nope)
          ;; Get the second digit.
          (loadw temp value (1+ bignum-digits-offset) other-pointer-lowtag)
          ;; All zeros, it's an (unsigned-byte 64).
          (cond (unsigned-p
                 (if not-p
                     (inst cbnz temp target)
                     (inst cbz temp target)))
                (t
                 (inst cbz temp yep)
                 (inst b nope)))

          single-word

          ;; positive implies (unsigned-byte 64).
          fixnum
          (unless unsigned-p
            (if not-p
                (inst tbnz* r (1- n-word-bits) target)
                (inst tbz* r (1- n-word-bits) target)))))
      (values))
    NOT-TARGET))

(define-vop (un/signed-byte-64-p-move-to-word signed-byte-64-p)
  ;; Ideally, this would use a single register but the rest of the stuff
  ;; depends on their storage class.
  (:results (rs :scs (signed-reg))
            (ru :scs (unsigned-reg)))
  (:info target not-p target-unsigned not-p-unsigned unsigned-fall-through)
  (:result-types signed-num unsigned-num)
  (:translate)
  (:generator 10
    (let ((fixnum-p (types-equal-or-intersect (tn-ref-type args) (specifier-type 'fixnum))))
      (assemble ()
        (when fixnum-p
          (inst asr rs value n-fixnum-tag-bits)
          (inst tbz* value 0 (if not-p
                                 not-target
                                 target)))
        (unless (fixnum-or-other-pointer-tn-ref-p args t)
          (test-type value temp
              (cond (not-p-unsigned
                     unsigned-fall-through)
                    (t
                     not-target))
              t (other-pointer-lowtag)))
        (loadw temp value 0 other-pointer-lowtag)
        (inst cmp temp (+ (ash 1 n-widetag-bits) bignum-widetag))
        (loadw rs value bignum-digits-offset other-pointer-lowtag)
        ;; Single digit bignum is always signed-byte-64
        (inst b :eq (if not-p
                        not-target
                        target))
        (inst cmp temp (+ (ash 2 n-widetag-bits) bignum-widetag))
        (inst b :ne (cond (not-p-unsigned
                           unsigned-fall-through)
                          (t
                           not-target)))
        (move ru rs)
        ;; If the second digit is zero then this is an unsigned-byte-64
        (loadw temp value (1+ bignum-digits-offset) other-pointer-lowtag)
        (cond (not-p-unsigned
               (inst cbnz temp target-unsigned))
              (t
               (inst cbz temp target-unsigned)))
        (inst b unsigned-fall-through)))
    not-target))

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
               (cond ((and (eql fixnum-value 0)
                           (memq comparison '(:ge :lt))
                           (csubtypep (type-intersection (tn-ref-type integer-ref)
                                                         (specifier-type 'fixnum))
                                      (specifier-type '(integer * -1))))
                      (inst tbz* integer 0 (if (eq comparison :lt)
                                               yep
                                               nope)))
                     ((and (eql fixnum-value 0)
                           (memq comparison '(:ge :lt))
                           (csubtypep (type-intersection (tn-ref-type integer-ref)
                                                         (specifier-type 'fixnum))
                                      (specifier-type '(integer 0))))
                      (inst tbz* integer 0 (if (eq comparison :lt)
                                               nope
                                               yep)))
                     ((and (not other-pointer-p)
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
    (test-type value temp target not-p (symbol-widetag) :value-tn-ref args
               :nil-in-other-pointers nil)))

(define-vop (consp type-predicate)
  (:translate consp)
  (:conditional :ne)
  (:info)
  (:generator 8
    (inst and temp value lowtag-mask)
    (inst cmp temp list-pointer-lowtag)
    (inst ccmp value null-tn :eq 4)))

(define-vop (pointerp consp)
  (:translate pointerp)
  (:conditional :eq)
  (:generator 3
    (inst and temp value 3)
    (inst cmp temp 3)))

(define-vop (single-float-p)
  (:args (value :scs (any-reg descriptor-reg)))
  (:arg-refs value-ref)
  (:conditional :eq)
  (:policy :fast-safe)
  (:translate single-float-p)
  (:vop-var vop)
  (:generator 7
    (multiple-value-bind (bit set) (tn-ref-lowtag-bit single-float-widetag value-ref)
      (case set
        (1
         (change-vop-flags vop '(:ne))
         (inst tst value (ash 1 bit)))
        (0
         (inst tst value (ash 1 bit)))
        (t
         (inst cmp (32-bit-reg value) single-float-widetag))))))

(define-vop (load-other-pointer-widetag)
  (:args (value :scs (any-reg descriptor-reg)))
  (:arg-refs args)
  (:info not-other-pointer-label null-label)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (when null-label
      (inst cmp value null-tn)
      (inst b :eq null-label))
    (unless (other-pointer-tn-ref-p args t)
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
  (let ((test-layout (case (layout-classoid-name test-layout)
                       (condition +condition-layout-flag+)
                       (pathname  +pathname-layout-flag+)
                       (structure-object +structure-layout-flag+)
                       (t test-layout))))
   (cond ((integerp test-layout)
          (inst ldrsw temp
                (@ layout
                   (- (ash (+ instance-slots-offset
                              (get-dsd-index layout sb-kernel::flags))
                           word-shift)
                      instance-pointer-lowtag)))
          (inst tst temp test-layout)
          )
         ((and desc-temp
               (neq (tn-kind desc-temp) :unused))
          (inst load-constant desc-temp
                (tn-byte-offset (emit-constant test-layout)))
          (inst cmp layout desc-temp)
          nil)
         (t
          (let* ((test-id (layout-id test-layout))
                 (depthoid (layout-depthoid test-layout))
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
                   (inst cmp (32-bit-reg this-id) (32-bit-reg temp)))))
          nil))))

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
                    #1=(and (not (memq (layout-classoid-name test-layout)
                                       '(condition pathname structure-object)))
                            (let ((classoid (layout-classoid test-layout)))
                              (and (eq (classoid-state classoid) :sealed)
                                   (not (classoid-subclasses classoid)))))))
              temp)
  (:temporary (:sc unsigned-reg
               :unused-if (or (memq (layout-classoid-name test-layout)
                                    '(condition pathname structure-object))
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

    (inst b (if (if (structure-is-a layout temp this-id test-layout desc-temp target not-p done)
                    (not not-p)
                    not-p)
                :ne :eq) target)
    done))

(define-vop (structure-typep*)
  (:args (layout :scs (descriptor-reg)))
  (:arg-types * (:constant t))
  (:policy :fast-safe)
  (:info target not-p test-layout)
  (:temporary (:sc unsigned-reg
               :unused-if
               #1=(and (not (memq (layout-classoid-name test-layout)
                                  '(condition pathname structure-object)))
                       (let ((classoid (layout-classoid test-layout)))
                         (and (eq (classoid-state classoid) :sealed)
                              (not (classoid-subclasses classoid))))))
              temp)
  (:temporary (:sc unsigned-reg
               :unused-if (or (memq (layout-classoid-name test-layout)
                                    '(condition pathname structure-object))
                              #1#))
              this-id)
  (:temporary (:sc descriptor-reg
               :unused-if (not #1#))
              desc-temp)
  (:generator 4
    (inst b (if (if (structure-is-a layout temp this-id test-layout desc-temp target not-p done)
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
    (inst ldrh temp (@ value (- 2 other-pointer-lowtag)))
    (inst cmp temp sb-impl::+package-id-keyword+)
    (inst b (if not-p :ne :eq) target)
    not-target))
