;;;; type testing and checking VOPs for the ARM VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

(defun %test-fixnum (value target not-p &key temp)
  (declare (ignore temp))
  (assemble ()
    (inst tst value fixnum-tag-mask)
    (inst b (if not-p :ne :eq) target)))

(defun %test-fixnum-immediate-and-headers (value target not-p immediate
                                           headers &key temp)
  (let ((drop-through (gen-label)))
    (inst tst value fixnum-tag-mask)
    (inst b :eq (if not-p drop-through target))
    (%test-immediate-and-headers value target not-p immediate headers
                                 :drop-through drop-through :temp temp)))

(defun %test-immediate-and-headers (value target not-p immediate headers
                                    &key (drop-through (gen-label)) temp)

  (inst mov temp immediate)
  (inst cmp temp (extend value :uxtb))
  (inst b :eq (if not-p drop-through target))
  (%test-headers value target not-p nil headers :drop-through drop-through
                                                :temp temp))

(defun %test-fixnum-and-headers (value target not-p headers &key temp)
  (let ((drop-through (gen-label)))
    (assemble ()
      (inst ands temp value fixnum-tag-mask)
      (inst b :eq (if not-p drop-through target)))
    (%test-headers value target not-p nil headers
                   :drop-through drop-through :temp temp)))

(defun %test-immediate (value target not-p immediate &key temp)
  (assemble ()
    (inst and temp value widetag-mask)
    (inst cmp temp immediate)
    (inst b (if not-p :ne :eq) target)))

(defun %test-lowtag (value target not-p lowtag &key temp)
  (assemble ()
    (inst and temp value lowtag-mask)
    (inst cmp temp lowtag)
    (inst b (if not-p :ne :eq) target)))

(defun %test-headers (value target not-p function-p headers
                      &key temp (drop-through (gen-label)))
    (let ((lowtag (if function-p fun-pointer-lowtag other-pointer-lowtag)))
    (multiple-value-bind (when-true when-false)
        (if not-p
            (values drop-through target)
            (values target drop-through))
      (assemble ()
        (%test-lowtag value when-false t lowtag :temp temp)
        (load-type temp value (- lowtag))
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
                  (inst and temp temp (logical-mask
                                       (ldb (byte 8 0) (logeqv header (cadr remaining)))))
                  (inst cmp temp (ldb (byte 8 0) (logand header (cadr remaining))))
                  (inst b (if not-p :ne :eq) target)
                  (return))
                 (t
                  (inst cmp temp header)
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
                    (inst and temp temp (logical-mask
                                         (ldb (byte 8 0) (logeqv start end))))
                    (inst cmp temp (ldb (byte 8 0) (logand start end)))
                    (inst b (if not-p :ne :eq) target))
                   ((and (not last) (null (cddr remaining))
                         (= (+ start 4) end) (= (logcount (logxor start end)) 1)
                         (listp (cadr remaining))
                         (= (+ (caadr remaining) 4) (cdadr remaining))
                         (= (logcount (logxor (caadr remaining) (cdadr remaining))) 1)
                         (= (logcount (logxor (caadr remaining) start)) 1))
                    (inst and temp temp (ldb (byte 8 0) (logeqv start (cdadr remaining))))
                    (inst cmp temp (ldb (byte 8 0) (logand start (cdadr remaining))))
                    (inst b (if not-p :ne :eq) target)
                    (return))
                   (t
                    (unless (= start bignum-widetag)
                      (inst cmp temp start)
                      (if (= end complex-array-widetag)
                          (progn
                            (aver last)
                            (inst b (if not-p :lt :ge) target))
                          (inst b :lt when-false)))
                    (unless (= end complex-array-widetag)
                      (inst cmp temp end)
                      (if last
                          (inst b (if not-p :gt :le) target)
                          (inst b :le when-true))))))))))
        (emit-label drop-through)))))

;;;; Other integer ranges.

;;; A (signed-byte 64) can be represented with either fixnum or a bignum with
;;; exactly one digit.
(define-vop (signed-byte-64-p type-predicate)
  (:translate signed-byte-64-p)
  (:generator 45
    (multiple-value-bind (yep nope)
        (if not-p
            (values not-target target)
            (values target not-target))
      (assemble ()
        (inst ands temp value fixnum-tag-mask)
        (inst b :eq yep)
        (test-type value nope t (other-pointer-lowtag) :temp temp)
        (loadw temp value 0 other-pointer-lowtag)
        (load-immediate-word tmp-tn (+ (ash 1 n-widetag-bits) bignum-widetag))
        (inst eor temp temp tmp-tn)
        (inst tst temp temp)
        (inst b (if not-p :ne :eq) target)))
    not-target))

;;; An (UNSIGNED-BYTE 64) can be represented with either a positive
;;; fixnum, a bignum with exactly one positive digit, or a bignum with
;;; exactly two digits and the second digit all zeros.
(define-vop (unsigned-byte-64-p type-predicate)
  (:translate unsigned-byte-64-p)
  (:generator 45
   (let ((single-word (gen-label))
         (fixnum (gen-label)))
     (multiple-value-bind (yep nope)
         (if not-p
             (values not-target target)
             (values target not-target))
       (assemble ()
         ;; Is it a fixnum?
         (move temp value)
         (%test-fixnum temp fixnum nil)

         ;; If not, is it an other pointer?
         (test-type value nope t (other-pointer-lowtag) :temp temp)
         ;; Get the header.
         (loadw temp value 0 other-pointer-lowtag)
         ;; Is it one?
         (load-immediate-word tmp-tn (+ (ash 1 n-widetag-bits) bignum-widetag))
         (inst eor temp temp tmp-tn)
         (inst tst temp temp)
         (inst b :eq single-word)
         ;; If it's other than two, we can't be an (unsigned-byte 64)
         (inst eor temp temp (logxor (+ (ash 1 n-widetag-bits) bignum-widetag)
                                     (+ (ash 2 n-widetag-bits) bignum-widetag)))
         (inst tst temp temp)
         (inst b :ne nope)
         ;; Get the second digit.
         (loadw temp value (1+ bignum-digits-offset) other-pointer-lowtag)
         ;; All zeros, its an (unsigned-byte 64).
         (inst cbz temp yep)
         (inst b nope)

         (emit-label single-word)
         ;; Get the single digit.
         (loadw temp value bignum-digits-offset other-pointer-lowtag)

         ;; positive implies (unsigned-byte 64).
         (emit-label fixnum)
         (inst cmp temp 0)
         (if not-p
             (inst b :lt target)
             (inst b :ge target))))
     (values))
   NOT-TARGET))

(define-vop (fixnump/unsigned-byte-64)
  (:policy :fast-safe)
  (:args (value :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:translate fixnump)
  (:conditional :eq)
  (:generator 5
    (inst tst value (ash (1- (ash 1 (- n-word-bits
                                   n-positive-fixnum-bits)))
                     n-positive-fixnum-bits))))

(define-vop (fixnump/signed-byte-64 type-predicate)
  (:args (value :scs (signed-reg)))
  (:conditional :vc)
  (:info)
  (:arg-types signed-num)
  (:translate fixnump)
  (:generator 5
    (inst adds temp value value)))

;;; MOD type checks
(defun power-of-two-limit-p (x)
  (and (fixnump x)
       (= (logcount (1+ x)) 1)))

(define-vop (test-fixnum-mod-power-of-two)
  (:args (value :scs (any-reg descriptor-reg
                              unsigned-reg signed-reg
                              immediate)))
  (:arg-types *
              (:constant (satisfies power-of-two-limit-p)))
  (:translate fixnum-mod-p)
  (:conditional :eq)
  (:info hi)
  (:save-p :compute-only)
  (:policy :fast-safe)
  (:generator 2
     (aver (not (sc-is value immediate)))
     (let* ((fixnum-hi (if (sc-is value unsigned-reg signed-reg)
                           hi
                           (fixnumize hi))))
       (inst tst value (lognot fixnum-hi)))))

(define-vop (test-fixnum-mod-tagged-unsigned-imm)
  (:args (value :scs (any-reg descriptor-reg
                              unsigned-reg signed-reg
                              immediate)))
  (:arg-types (:or tagged-num unsigned-num signed-num)
              (:constant (satisfies add-sub-immediate-p)))
  (:translate fixnum-mod-p)
  (:conditional :ls)
  (:info hi)
  (:save-p :compute-only)
  (:policy :fast-safe)
  (:generator 3
     (aver (not (sc-is value immediate)))
     (let ((fixnum-hi (if (sc-is value unsigned-reg signed-reg)
                          hi
                          (fixnumize hi))))
       (inst cmp value fixnum-hi))))

(defun add-sub-immediate+1-p (x)
  (add-sub-immediate-p (1+ (fixnumize x))))

;;; Adding 1 and changing the codntions from <= to < allows to encode
;;; more immediates.
(define-vop (test-fixnum-mod-tagged-unsigned-imm+1)
  (:args (value :scs (any-reg descriptor-reg
                              unsigned-reg signed-reg
                              immediate)))
  (:arg-types (:or tagged-num unsigned-num signed-num)
              (:constant (satisfies add-sub-immediate+1-p)))
  (:translate fixnum-mod-p)
  (:conditional :cc)
  (:info hi)
  (:save-p :compute-only)
  (:policy :fast-safe)
  (:generator 3
     (aver (not (sc-is value immediate)))
     (let ((fixnum-hi (if (sc-is value unsigned-reg signed-reg)
                          (1+ hi)
                          (fixnumize (1+ hi)))))
       (inst cmp value fixnum-hi))))

(define-vop (test-fixnum-mod-tagged-unsigned)
  (:args (value :scs (any-reg descriptor-reg
                              unsigned-reg signed-reg
                              immediate)))
  (:arg-types (:or tagged-num unsigned-num signed-num)
              (:constant fixnum))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:translate fixnum-mod-p)
  (:conditional :ls)
  (:info hi)
  (:save-p :compute-only)
  (:policy :fast-safe)
  (:generator 4
     (aver (not (sc-is value immediate)))
     (let ((fixnum-hi (if (sc-is value unsigned-reg signed-reg)
                          hi
                          (fixnumize hi))))
       (load-immediate-word temp fixnum-hi)
       (inst cmp value temp))))

(defun add-sub-immediate/+1-p (x)
  (let ((x (fixnumize x)))
    (or (add-sub-immediate-p x)
        (add-sub-immediate-p (1+ x)))))

(define-vop (test-fixnum-mod-*-imm)
  (:args (value :scs (any-reg descriptor-reg)))
  (:arg-types * (:constant (satisfies add-sub-immediate/+1-p)))
  (:translate fixnum-mod-p)
  (:conditional)
  (:info target not-p hi)
  (:save-p :compute-only)
  (:policy :fast-safe)
  (:generator 5
    (let* ((1+ (not (add-sub-immediate-p (fixnumize hi))))
           (fixnum-hi (fixnumize (if 1+
                                     (1+ hi)
                                     hi)))
           (skip (gen-label)))
      (inst tst value fixnum-tag-mask)
      (inst b :ne (if not-p target skip))
      (inst cmp value fixnum-hi)
      (inst b (if not-p
                  (if 1+ :cs :hi)
                  (if 1+ :cc :ls))
            target)
      (emit-label SKIP))))

(define-vop (test-fixnum-mod-*)
  (:args (value :scs (any-reg descriptor-reg)))
  (:arg-types * (:constant fixnum))
  (:translate fixnum-mod-p)
  (:temporary (:scs (any-reg)) temp)
  (:conditional)
  (:info target not-p hi)
  (:save-p :compute-only)
  (:policy :fast-safe)
  (:generator 6
    (inst tst value fixnum-tag-mask)
    (inst b :ne (if not-p target skip))
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
    (let* ((drop-thru (gen-label))
           (is-symbol-label (if not-p drop-thru target)))
      (inst cmp value null-tn)
      (inst b :eq is-symbol-label)
      (test-type value target not-p (symbol-header-widetag) :temp temp)
      (emit-label drop-thru))))

(define-vop (consp type-predicate)
  (:translate consp)
  (:generator 8
    (let* ((drop-thru (gen-label))
           (is-not-cons-label (if not-p target drop-thru)))
      (inst cmp value null-tn)
      (inst b :eq is-not-cons-label)
      (test-type value target not-p (list-pointer-lowtag) :temp temp)
      (emit-label drop-thru))))
