;;;; type testing and checking VOPs for the x86 VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;;; test generation utilities

(defun generate-fixnum-test (value)
  (emit-optimized-test-inst value fixnum-tag-mask))

(defun %test-fixnum (value temp target not-p)
  (declare (ignore temp))
  (generate-fixnum-test value)
  (inst jmp (if not-p :nz :z) target))

(defun %test-fixnum-and-headers (value temp target not-p headers &key value-tn-ref)
  (let ((drop-through (gen-label)))
    (generate-fixnum-test value)
    (inst jmp :z (if not-p drop-through target))
    (%test-headers value temp target not-p nil headers
                   :drop-through drop-through :value-tn-ref value-tn-ref)))

(defun %test-immediate (value temp target not-p immediate)
  (declare (ignore temp))
  ;; Code a single instruction byte test if possible.
  (let ((offset (tn-offset value)))
    (cond ((and (sc-is value any-reg descriptor-reg)
                (or (= offset eax-offset) (= offset ebx-offset)
                    (= offset ecx-offset) (= offset edx-offset)))
           (inst cmp (make-random-tn :kind :normal
                                     :sc (sc-or-lose 'byte-reg)
                                     :offset offset)
                 immediate))
          (t
           (move eax-tn value)
           (inst cmp al-tn immediate))))
  (inst jmp (if not-p :ne :e) target))

(defun %test-lowtag (value temp target not-p lowtag)
  (declare (ignore temp))
  (inst lea eax-tn (make-ea :dword :base value :disp (- lowtag)))
  (inst test al-tn lowtag-mask)
  ;; FIXME: another 'optimization' which doesn't appear to work:
  ;; prefetching the hypothetically pointed-to version should help,
  ;; but this is in fact non-ideal in plenty of ways: we emit way too
  ;; many of these prefetch instructions; pointed-to objects are very
  ;; often in the cache anyway; etc. etc.  Still, as proof-of-concept,
  ;; not too bad.  -- CSR, 2004-07-27
  (when (member :prefetch *backend-subfeatures*)
    (inst prefetchnta (make-ea :byte :base value :disp (- lowtag))))
  (inst jmp (if not-p :ne :e) target))

(defun %test-headers (value temp target not-p function-p headers
                      &key except (drop-through (gen-label)) value-tn-ref)
  (let ((lowtag (if function-p fun-pointer-lowtag other-pointer-lowtag)))
    (multiple-value-bind (equal less-or-equal greater-or-equal when-true when-false)
        ;; EQUAL, LESS-OR-EQUAL and GREATER-OR-EQUAL are the conditions for
        ;; branching to TARGET.  WHEN-TRUE and WHEN-FALSE are the
        ;; labels to branch to when we know it's true and when we know
        ;; it's false respectively.
        (if not-p
            (values :ne :a :b drop-through target)
            (values :e :na :nb target drop-through))
      (unless (and value-tn-ref
                   (eq lowtag other-pointer-lowtag)
                   (other-pointer-tn-ref-p value-tn-ref))
        (%test-lowtag value temp when-false t lowtag))
      (cond
        ((and (null (cdr headers))
              (not except)
              (numberp (car headers)))
         ;; Optimize the common case: referencing the value from memory
         ;; is slightly smaller than loading it and then doing the
         ;; comparison.  Doing this for other cases (e.g. range of
         ;; [BIGNUM-WIDETAG..FOO-WIDETAG]) is also possible, but such
         ;; opportunities don't come up very often and the code would
         ;; get pretty hairy...
         (inst cmp (make-ea :byte :base value :disp (- lowtag)) (car headers))
         (inst jmp equal target))
        (t
         (inst mov al-tn (make-ea :byte :base value :disp (- lowtag)))
         (dolist (widetag except)
           (inst cmp al-tn widetag)
           (inst jmp :e when-false))
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
                   ;; FIXME: (VECTOR T) does not and could not admit this hack.
                   ;; The others could but are broken except for BIT-VECTOR.
                   ;; BASE-STRING, (VECTOR NIL), BIT-VECTOR, (VECTOR T)
                   (inst and al-tn (ldb (byte 8 0) (logeqv header (cadr remaining))))
                   (inst cmp al-tn (ldb (byte 8 0) (logand header (cadr remaining))))
                   (inst jmp equal target)
                   (return))
                  (t
                   (inst cmp al-tn header)
                   (if last
                       (inst jmp equal target)
                       (inst jmp :e when-true)))))
               (t
                (let ((start (car header))
                      (end (cdr header)))
                  (cond
                    ;; LAST = don't need al-tn later
                    ((and last (not (= start bignum-widetag))
                          (= (+ start 4) end) (= (logcount (logxor start end)) 1))
                     ;; SIMPLE-STRING
                     (inst and al-tn (ldb (byte 8 0) (logeqv start end)))
                     (inst cmp al-tn (ldb (byte 8 0) (logand start end)))
                     (inst jmp equal target))
                    ((and (not last) (null (cddr remaining))
                          (= (+ start 4) end) (= (logcount (logxor start end)) 1)
                          (listp (cadr remaining))
                          (= (+ (caadr remaining) 4) (cdadr remaining))
                          (= (logcount (logxor (caadr remaining) (cdadr remaining))) 1)
                          (= (logcount (logxor (caadr remaining) start)) 1))
                     ;; STRING
                     (inst and al-tn (ldb (byte 8 0) (logeqv start (cdadr remaining))))
                     (inst cmp al-tn (ldb (byte 8 0) (logand start (cdadr remaining))))
                     (inst jmp equal target)
                     ;; we've shortcircuited the DO, so we must return.
                     ;; It's OK to do so, because (NULL (CDDR REMAINING))
                     ;; was true.
                     (return))
                    (t
                     (cond
                       ((= start bignum-widetag)
                        (inst cmp al-tn end)
                        (if last
                            (inst jmp less-or-equal target)
                            (inst jmp :be when-true)))
                       ((= end complex-array-widetag)
                        (inst cmp al-tn start)
                        (if last
                            (inst jmp greater-or-equal target)
                            (inst jmp :b when-false)))
                       ((not last)
                        (inst cmp al-tn start)
                        (inst jmp :b when-false)
                        (inst cmp al-tn end)
                        (inst jmp :be when-true))
                       (t
                        (inst sub al-tn start)
                        (inst cmp al-tn (- end start))
                        (inst jmp less-or-equal target))))))))))))
      (emit-label drop-through))))

;;; simpler VOP that don't need a temporary register
(define-vop (simple-type-predicate)
  (:args (value :scs (any-reg descriptor-reg control-stack)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe))

;;;; other integer ranges

(define-vop (fixnump/unsigned-byte-32 simple-type-predicate)
  (:args (value :scs (unsigned-reg)))
  (:info)
  (:conditional :be)
  (:arg-types unsigned-num)
  (:translate fixnump)
  (:generator 3
    ;; We could encode this with :Z and SHR, analogously to the signed-byte-32
    ;; case below -- as we do on x86-64 -- but that costs us an extra
    ;; register. Compromises...
    (inst cmp value #.most-positive-fixnum)))

(define-vop (fixnump/signed-byte-32 type-predicate)
  (:args (value :scs (signed-reg)))
  (:info)
  (:conditional :z)
  (:arg-types signed-num)
  (:translate fixnump)
  (:ignore temp)
  (:generator 3
    ;; Hackers Delight, p. 53: signed
    ;;    a <= x <= a + 2^n - 1
    ;; is equivalent to unsigned
    ;;    ((x-a) >> n) = 0
    (inst mov eax-tn value)
    (inst sub eax-tn #.most-negative-fixnum)
    (inst shr eax-tn #.(integer-length (- most-positive-fixnum
                                          most-negative-fixnum)))))

;;; A (SIGNED-BYTE 32) can be represented with either fixnum or a bignum with
;;; exactly one digit.

(define-vop (signed-byte-32-p type-predicate)
  (:translate signed-byte-32-p)
  (:ignore temp)
  (:generator 45
    (multiple-value-bind (yep nope)
        (if not-p
            (values not-target target)
            (values target not-target))
      (generate-fixnum-test value)
      (inst jmp :e yep)
      (inst lea eax-tn (make-ea :dword :base value
                                :disp (- other-pointer-lowtag)))
      (inst test al-tn lowtag-mask)
      (inst jmp :ne nope)
      (inst cmp (object-slot-ea value 0 other-pointer-lowtag)
            (+ (ash 1 n-widetag-bits) bignum-widetag))
      (inst jmp (if not-p :ne :e) target))
    NOT-TARGET))

;;; An (unsigned-byte 32) can be represented with either a positive
;;; fixnum, a bignum with exactly one positive digit, or a bignum with
;;; exactly two digits and the second digit all zeros.
(define-vop (unsigned-byte-32-p type-predicate)
  (:translate unsigned-byte-32-p)
  (:ignore temp)
  (:generator 45
    (let ((not-target (gen-label))
          (single-word (gen-label))
          (fixnum (gen-label)))
      (multiple-value-bind (yep nope)
          (if not-p
              (values not-target target)
              (values target not-target))
        ;; Is it a fixnum?
        (move eax-tn value)
        (inst test al-tn fixnum-tag-mask)
        (inst jmp :e fixnum)

        ;; If not, is it an other pointer?
        (inst and al-tn lowtag-mask)
        (inst cmp al-tn other-pointer-lowtag)
        (inst jmp :ne nope)
        ;; Get the header.
        (loadw eax-tn value 0 other-pointer-lowtag)
        ;; Is it one?
        (inst cmp eax-tn (+ (ash 1 n-widetag-bits) bignum-widetag))
        (inst jmp :e single-word)
        ;; If it's other than two, we can't be an (unsigned-byte 32)
        ;: Leave EAX holding 0 in the affirmative case.
        (inst sub eax-tn (+ (ash 2 n-widetag-bits) bignum-widetag))
        (inst jmp :ne nope)
        ;; Compare the second digit to zero (in EAX).
        (inst cmp (object-slot-ea value (1+ bignum-digits-offset)
                                           other-pointer-lowtag) eax-tn)
        (inst jmp :z yep) ; All zeros, its an (unsigned-byte 32).
        (inst jmp nope)

        (emit-label single-word)
        ;; Get the single digit.
        (loadw eax-tn value bignum-digits-offset other-pointer-lowtag)

        ;; positive implies (unsigned-byte 32).
        (emit-label fixnum)
        (inst test eax-tn eax-tn)
        (inst jmp (if not-p :s :ns) target)

        (emit-label not-target)))))

(defun power-of-two-limit-p (x)
  (and (fixnump x)
       (= (logcount (1+ x)) 1)))

(define-vop (test-fixnum-mod-power-of-two)
  (:args (value :scs (any-reg descriptor-reg
                      unsigned-reg signed-reg)))
  (:arg-types *
              (:constant (satisfies power-of-two-limit-p)))
  (:translate sb-c::fixnum-mod-p)
  (:conditional :e)
  (:info hi)
  (:policy :fast-safe)
  (:generator 4
     (let* ((fixnum-hi (if (sc-is value unsigned-reg signed-reg)
                           hi
                           (fixnumize hi))))
       (inst test value (lognot fixnum-hi)))))

(define-vop (test-fixnum-mod-tagged-unsigned)
  (:args (value :scs (any-reg unsigned-reg signed-reg)))
  (:arg-types (:or tagged-num unsigned-num signed-num)
              (:constant fixnum))
  (:translate sb-c::fixnum-mod-p)
  (:conditional :be)
  (:info hi)
  (:policy :fast-safe)
  (:generator 5
     (let ((fixnum-hi (if (sc-is value unsigned-reg signed-reg)
                          hi
                          (fixnumize hi))))
       (inst cmp value fixnum-hi))))

(define-vop (test-fixnum-mod-*)
  (:args (value :scs (any-reg descriptor-reg)))
  (:arg-types * (:constant fixnum))
  (:translate sb-c::fixnum-mod-p)
  (:conditional)
  (:info target not-p hi)
  (:policy :fast-safe)
  (:generator 6
     (let* ((fixnum-hi (fixnumize hi))
            (skip (gen-label)))
       (generate-fixnum-test value)
       (inst jmp :ne (if not-p target skip))
       (inst cmp value fixnum-hi)
       (inst jmp (if not-p :a :be) target)
       (emit-label skip))))


;;;; list/symbol types
;;;
;;; symbolp (or symbol (eq nil))
;;; consp (and list (not (eq nil)))

(define-vop (symbolp type-predicate)
  (:translate symbolp)
  (:ignore temp)
  (:generator 12
    (let ((is-symbol-label (if not-p DROP-THRU target))
          (widetag-tn (make-ea :byte :base value :disp (- other-pointer-lowtag))))
      ;; It could have been done with just TEST-TYPE, but using CMP on
      ;; EAX saves one byte, this basically unrolls TEST-TYPE and
      ;; inserts a comparison to NIL in the middle.
      (inst lea eax-tn widetag-tn)
      (inst cmp eax-tn (- nil-value other-pointer-lowtag))
      (inst jmp :e is-symbol-label)
      (inst test al-tn other-pointer-lowtag)
      (inst jmp :nz (if not-p target drop-thru))
      (inst cmp widetag-tn symbol-widetag)
      (inst jmp (if not-p :ne :e) target))
    DROP-THRU))

(define-vop (consp type-predicate)
  (:translate consp)
  (:ignore temp)
  (:generator 8
     (let ((is-not-cons-label (if not-p target drop-thru)))
       ;; It could have been done with just TEST-TYPE, but using CMP on
       ;; EAX saves one byte, this basically unrolls TEST-TYPE and
       ;; inserts a comparison to NIL in the middle.
       (inst lea eax-tn (make-ea :dword :base value :disp (- list-pointer-lowtag)))
       (inst cmp eax-tn (- nil-value list-pointer-lowtag))
       (inst jmp :e is-not-cons-label)
       (inst test al-tn other-pointer-lowtag)
       (inst jmp (if not-p :nz :z) target))
    DROP-THRU))
