;;;; type testing and checking VOPs for the x86 VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; test generation utilities

(defun generate-fixnum-test (value)
  (emit-optimized-test-inst value 3))

(defun %test-fixnum (value target not-p)
  (generate-fixnum-test value)
  (inst jmp (if not-p :nz :z) target))

(defun %test-fixnum-and-headers (value target not-p headers)
  (let ((drop-through (gen-label)))
    (generate-fixnum-test value)
    (inst jmp :z (if not-p drop-through target))
    (%test-headers value target not-p nil headers drop-through)))

(defun %test-immediate (value target not-p immediate)
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

(defun %test-lowtag (value target not-p lowtag)
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

(defun %test-headers (value target not-p function-p headers
                            &optional (drop-through (gen-label)))
  (let ((lowtag (if function-p fun-pointer-lowtag other-pointer-lowtag)))
    (multiple-value-bind (equal less-or-equal greater-or-equal when-true when-false)
        ;; EQUAL, LESS-OR-EQUAL and GREATER-OR-EQUAL are the conditions for
        ;; branching to TARGET.  WHEN-TRUE and WHEN-FALSE are the
        ;; labels to branch to when we know it's true and when we know
        ;; it's false respectively.
        (if not-p
            (values :ne :a :b drop-through target)
            (values :e :na :nb target drop-through))
      (%test-lowtag value when-false t lowtag)
      (cond
        ((and (null (cdr headers))
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
                        (if last
                            (inst jmp less-or-equal target)
                            (inst jmp :be when-true)))
                       (t
                        (inst sub al-tn start)
                        (inst cmp al-tn (- end start))
                        (inst jmp less-or-equal target))))))))))))
      (emit-label drop-through))))

;;;; type checking and testing

(define-vop (check-type)
  (:args (value :target result :scs (any-reg descriptor-reg)))
  (:results (result :scs (any-reg descriptor-reg)))
  (:temporary (:sc unsigned-reg :offset eax-offset :to (:result 0)) eax)
  (:ignore eax)
  (:vop-var vop)
  (:save-p :compute-only))

(define-vop (type-predicate)
  (:args (value :scs (any-reg descriptor-reg)))
  (:temporary (:sc unsigned-reg :offset eax-offset) eax)
  (:ignore eax)
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe))

;;; simpler VOP that don't need a temporary register
(define-vop (simple-check-type)
  (:args (value :target result :scs (any-reg descriptor-reg)))
  (:results (result :scs (any-reg descriptor-reg)
                    :load-if (not (and (sc-is value any-reg descriptor-reg)
                                       (sc-is result control-stack)))))
  (:vop-var vop)
  (:save-p :compute-only))

(define-vop (simple-type-predicate)
  (:args (value :scs (any-reg descriptor-reg control-stack)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe))

(defun cost-to-test-types (type-codes)
  (+ (* 2 (length type-codes))
     (if (> (apply #'max type-codes) lowtag-limit) 7 2)))

(defmacro !define-type-vops (pred-name check-name ptype error-code
                             (&rest type-codes)
                             &key (variant nil variant-p) &allow-other-keys)
  ;; KLUDGE: UGH. Why do we need this eval? Can't we put this in the
  ;; expansion?
  (let* ((cost (cost-to-test-types (mapcar #'eval type-codes)))
         (prefix (if variant-p
                     (concatenate 'string (string variant) "-")
                     "")))
    `(progn
       ,@(when pred-name
           `((define-vop (,pred-name ,(intern (concatenate 'string prefix "TYPE-PREDICATE")))
               (:translate ,pred-name)
               (:generator ,cost
                 (test-type value target not-p (,@type-codes))))))
       ,@(when check-name
           `((define-vop (,check-name ,(intern (concatenate 'string prefix "CHECK-TYPE")))
               (:generator ,cost
                 (let ((err-lab
                        (generate-error-code vop ',error-code value)))
                   (test-type value err-lab t (,@type-codes))
                   (move result value))))))
       ,@(when ptype
           `((primitive-type-vop ,check-name (:check) ,ptype))))))

;;;; other integer ranges

(define-vop (fixnump/unsigned-byte-32 simple-type-predicate)
  (:args (value :scs (unsigned-reg)))
  (:info)
  (:conditional :be)
  (:arg-types unsigned-num)
  (:translate fixnump)
  (:generator 5
    ;; We could encode this with :Z and SHR, analogously to the signed-byte-32
    ;; case below -- as we do on x86-64 -- but that costs us an extra
    ;; register. Compromises...
    (inst cmp value #.sb!xc:most-positive-fixnum)))

(define-vop (fixnump/signed-byte-32 type-predicate)
  (:args (value :scs (signed-reg)))
  (:info)
  (:conditional :z)
  (:arg-types signed-num)
  (:translate fixnump)
  (:generator 5
    ;; Hackers Delight, p. 53: signed
    ;;    a <= x <= a + 2^n - 1
    ;; is equivalent to unsigned
    ;;    ((x-a) >> n) = 0
    (inst mov eax-tn value)
    (inst sub eax-tn #.sb!xc:most-negative-fixnum)
    (inst shr eax-tn #.(integer-length (- sb!xc:most-positive-fixnum
                                          sb!xc:most-negative-fixnum)))))

;;; A (SIGNED-BYTE 32) can be represented with either fixnum or a bignum with
;;; exactly one digit.

(define-vop (signed-byte-32-p type-predicate)
  (:translate signed-byte-32-p)
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
      (inst cmp (make-ea-for-object-slot value 0 other-pointer-lowtag)
            (+ (ash 1 n-widetag-bits) bignum-widetag))
      (inst jmp (if not-p :ne :e) target))
    NOT-TARGET))

(define-vop (check-signed-byte-32 check-type)
  (:generator 45
    (let ((nope (generate-error-code vop
                                     'object-not-signed-byte-32-error
                                     value)))
      (generate-fixnum-test value)
      (inst jmp :e yep)
      (inst lea eax-tn (make-ea :dword :base value
                                :disp (- other-pointer-lowtag)))
      (inst test al-tn lowtag-mask)
      (inst jmp :ne nope)
      (inst cmp (make-ea-for-object-slot value 0 other-pointer-lowtag)
            (+ (ash 1 n-widetag-bits) bignum-widetag))
      (inst jmp :ne nope))
    YEP
    (move result value)))

;;; An (unsigned-byte 32) can be represented with either a positive
;;; fixnum, a bignum with exactly one positive digit, or a bignum with
;;; exactly two digits and the second digit all zeros.
(define-vop (unsigned-byte-32-p type-predicate)
  (:translate unsigned-byte-32-p)
  (:generator 45
    (let ((not-target (gen-label))
          (single-word (gen-label))
          (fixnum (gen-label)))
      (multiple-value-bind (yep nope)
          (if not-p
              (values not-target target)
              (values target not-target))
        ;; Is it a fixnum?
        (generate-fixnum-test value)
        (move eax-tn value)
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
        (inst cmp eax-tn (+ (ash 2 n-widetag-bits) bignum-widetag))
        (inst jmp :ne nope)
        ;; Get the second digit.
        (loadw eax-tn value (1+ bignum-digits-offset) other-pointer-lowtag)
        ;; All zeros, its an (unsigned-byte 32).
        (inst or eax-tn eax-tn)
        (inst jmp :z yep)
        (inst jmp nope)

        (emit-label single-word)
        ;; Get the single digit.
        (loadw eax-tn value bignum-digits-offset other-pointer-lowtag)

        ;; positive implies (unsigned-byte 32).
        (emit-label fixnum)
        (inst or eax-tn eax-tn)
        (inst jmp (if not-p :s :ns) target)

        (emit-label not-target)))))

(define-vop (check-unsigned-byte-32 check-type)
  (:generator 45
    (let ((nope
           (generate-error-code vop 'object-not-unsigned-byte-32-error value))
          (yep (gen-label))
          (fixnum (gen-label))
          (single-word (gen-label)))

      ;; Is it a fixnum?
      (generate-fixnum-test value)
      (move eax-tn value)
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
      (inst cmp eax-tn (+ (ash 2 n-widetag-bits) bignum-widetag))
      (inst jmp :ne nope)
      ;; Get the second digit.
      (loadw eax-tn value (1+ bignum-digits-offset) other-pointer-lowtag)
      ;; All zeros, its an (unsigned-byte 32).
      (inst or eax-tn eax-tn)
      (inst jmp :z yep)
      (inst jmp nope)

      (emit-label single-word)
      ;; Get the single digit.
      (loadw eax-tn value bignum-digits-offset other-pointer-lowtag)

      ;; positive implies (unsigned-byte 32).
      (emit-label fixnum)
      (inst or eax-tn eax-tn)
      (inst jmp :s nope)

      (emit-label yep)
      (move result value))))

;;;; list/symbol types
;;;
;;; symbolp (or symbol (eq nil))
;;; consp (and list (not (eq nil)))

(define-vop (symbolp type-predicate)
  (:translate symbolp)
  (:generator 12
    (let ((is-symbol-label (if not-p drop-thru target)))
      (inst cmp value nil-value)
      (inst jmp :e is-symbol-label)
      (test-type value target not-p (symbol-header-widetag)))
    DROP-THRU))

(define-vop (check-symbol check-type)
  (:generator 12
    (let ((error (generate-error-code vop 'object-not-symbol-error value)))
      (inst cmp value nil-value)
      (inst jmp :e drop-thru)
      (test-type value error t (symbol-header-widetag)))
    DROP-THRU
    (move result value)))

(define-vop (consp type-predicate)
  (:translate consp)
  (:generator 8
    (let ((is-not-cons-label (if not-p target drop-thru)))
      (inst cmp value nil-value)
      (inst jmp :e is-not-cons-label)
      (test-type value target not-p (list-pointer-lowtag)))
    DROP-THRU))

(define-vop (check-cons check-type)
  (:generator 8
    (let ((error (generate-error-code vop 'object-not-cons-error value)))
      (inst cmp value nil-value)
      (inst jmp :e error)
      (test-type value error t (list-pointer-lowtag))
      (move result value))))
