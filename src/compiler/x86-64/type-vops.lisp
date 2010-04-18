;;;; type testing and checking VOPs for the x86-64 VM

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
  "zero flag set if VALUE is fixnum"
  (inst test
        (cond ((sc-is value any-reg descriptor-reg)
               (make-byte-tn value))
              ((sc-is value control-stack)
               (make-ea :byte :base rbp-tn
                        :disp (frame-byte-offset (tn-offset value))))
              (t
               value))
        sb!vm::fixnum-tag-mask))

(defun %test-fixnum (value target not-p)
  (generate-fixnum-test value)
  (inst jmp (if not-p :nz :z) target))

(defun %test-fixnum-and-headers (value target not-p headers)
  (let ((drop-through (gen-label)))
    (generate-fixnum-test value)
    (inst jmp :z (if not-p drop-through target))
    (%test-headers value target not-p nil headers drop-through)))

(defun %test-fixnum-and-immediate (value target not-p immediate)
  (let ((drop-through (gen-label)))
    (generate-fixnum-test value)
    (inst jmp :z (if not-p drop-through target))
    (%test-immediate value target not-p immediate drop-through)))

(defun %test-fixnum-immediate-and-headers (value target not-p immediate
                                           headers)
  (let ((drop-through (gen-label)))
    (generate-fixnum-test value)
    (inst jmp :z (if not-p drop-through target))
    (%test-immediate-and-headers value target not-p immediate headers
                                 drop-through)))

(defun %test-immediate (value target not-p immediate
                        &optional (drop-through (gen-label)))
  ;; Code a single instruction byte test if possible.
  (cond ((sc-is value any-reg descriptor-reg)
         (inst cmp (make-byte-tn value) immediate))
        (t
         (move rax-tn value)
         (inst cmp al-tn immediate)))
  (inst jmp (if not-p :ne :e) target)
  (emit-label drop-through))

(defun %test-immediate-and-headers (value target not-p immediate headers
                                    &optional (drop-through (gen-label)))
  ;; Code a single instruction byte test if possible.
  (cond ((sc-is value any-reg descriptor-reg)
         (inst cmp (make-byte-tn value) immediate))
        (t
         (move rax-tn value)
         (inst cmp al-tn immediate)))
  (inst jmp :e (if not-p drop-through target))
  (%test-headers value target not-p nil headers drop-through))

(defun %test-lowtag (value target not-p lowtag)
  (if (and (sc-is value any-reg descriptor-reg)
           (< (tn-offset value) r8-offset))
      (move eax-tn (make-dword-tn value)) ; shorter encoding (no REX prefix)
      (move rax-tn value))
  (inst and al-tn lowtag-mask)
  (inst cmp al-tn lowtag)
  (inst jmp (if not-p :ne :e) target))

(defun %test-headers (value target not-p function-p headers
                            &optional (drop-through (gen-label)))
  (let ((lowtag (if function-p fun-pointer-lowtag other-pointer-lowtag)))
    (multiple-value-bind (equal less-or-equal greater-or-equal when-true when-false)
        ;; EQUAL, LESS-OR-EQUAL, and GREATER-OR-EQUAL are the conditions
        ;; for branching to TARGET.  WHEN-TRUE and WHEN-FALSE are the
        ;; labels to branch to when we know it's true and when we know
        ;; it's false respectively.
        (if not-p
            (values :ne :a :b drop-through target)
            (values :e :na :nb target drop-through))
      (%test-lowtag value when-false t lowtag)
      (inst mov al-tn (make-ea :byte :base value :disp (- lowtag)))
      (do ((remaining headers (cdr remaining)))
          ((null remaining))
        (let ((header (car remaining))
              (last (null (cdr remaining))))
          (cond
           ((atom header)
            (inst cmp al-tn header)
            (if last
                (inst jmp equal target)
                (inst jmp :e when-true)))
           (t
             (let ((start (car header))
                   (end (cdr header)))
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
                  (inst jmp less-or-equal target))))))))
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

(define-vop (fixnump/unsigned-byte-64 simple-type-predicate)
  (:args (value :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:translate fixnump)
  (:temporary (:sc unsigned-reg) tmp)
  (:info)
  (:conditional :z)
  (:generator 5
    (inst mov tmp value)
    (inst shr tmp n-positive-fixnum-bits)))

(define-vop (fixnump/signed-byte-64 type-predicate)
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
    (inst mov rax-tn #.(- sb!xc:most-negative-fixnum))
    (inst add rax-tn value)
    (inst shr rax-tn #.(integer-length (- sb!xc:most-positive-fixnum
                                          sb!xc:most-negative-fixnum)))))

;;; A (SIGNED-BYTE 64) can be represented with either fixnum or a bignum with
;;; exactly one digit.

(define-vop (signed-byte-64-p type-predicate)
  (:translate signed-byte-64-p)
  (:generator 45
    (multiple-value-bind (yep nope)
        (if not-p
            (values not-target target)
            (values target not-target))
      (generate-fixnum-test value)
      (inst jmp :e yep)
      (move rax-tn value)
      (inst and al-tn lowtag-mask)
      (inst cmp al-tn other-pointer-lowtag)
      (inst jmp :ne nope)
      (inst cmp (make-ea-for-object-slot value 0 other-pointer-lowtag)
            (+ (ash 1 n-widetag-bits) bignum-widetag))
      (inst jmp (if not-p :ne :e) target))
    NOT-TARGET))

(define-vop (check-signed-byte-64 check-type)
  (:generator 45
    (let ((nope (generate-error-code vop
                                     'object-not-signed-byte-64-error
                                     value)))
      (generate-fixnum-test value)
      (inst jmp :e yep)
      (move rax-tn value)
      (inst and al-tn lowtag-mask)
      (inst cmp al-tn other-pointer-lowtag)
      (inst jmp :ne nope)
      (inst cmp (make-ea-for-object-slot value 0 other-pointer-lowtag)
            (+ (ash 1 n-widetag-bits) bignum-widetag))
      (inst jmp :ne nope))
    YEP
    (move result value)))

;;; An (unsigned-byte 64) can be represented with either a positive
;;; fixnum, a bignum with exactly one positive digit, or a bignum with
;;; exactly two digits and the second digit all zeros.
(define-vop (unsigned-byte-64-p type-predicate)
  (:translate unsigned-byte-64-p)
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
        (move rax-tn value)
        (inst jmp :e fixnum)

        ;; If not, is it an other pointer?
        (inst and rax-tn lowtag-mask)
        (inst cmp rax-tn other-pointer-lowtag)
        (inst jmp :ne nope)
        ;; Get the header.
        (loadw rax-tn value 0 other-pointer-lowtag)
        ;; Is it one?
        (inst cmp rax-tn (+ (ash 1 n-widetag-bits) bignum-widetag))
        (inst jmp :e single-word)
        ;; If it's other than two, we can't be an (unsigned-byte 64)
        (inst cmp rax-tn (+ (ash 2 n-widetag-bits) bignum-widetag))
        (inst jmp :ne nope)
        ;; Get the second digit.
        (loadw rax-tn value (1+ bignum-digits-offset) other-pointer-lowtag)
        ;; All zeros, its an (unsigned-byte 64).
        (inst or rax-tn rax-tn)
        (inst jmp :z yep)
        (inst jmp nope)

        (emit-label single-word)
        ;; Get the single digit.
        (loadw rax-tn value bignum-digits-offset other-pointer-lowtag)

        ;; positive implies (unsigned-byte 64).
        (emit-label fixnum)
        (inst or rax-tn rax-tn)
        (inst jmp (if not-p :s :ns) target)

        (emit-label not-target)))))

(define-vop (check-unsigned-byte-64 check-type)
  (:generator 45
    (let ((nope
           (generate-error-code vop 'object-not-unsigned-byte-64-error value))
          (yep (gen-label))
          (fixnum (gen-label))
          (single-word (gen-label)))

      ;; Is it a fixnum?
      (generate-fixnum-test value)
      (move rax-tn value)
      (inst jmp :e fixnum)

      ;; If not, is it an other pointer?
      (inst and rax-tn lowtag-mask)
      (inst cmp rax-tn other-pointer-lowtag)
      (inst jmp :ne nope)
      ;; Get the header.
      (loadw rax-tn value 0 other-pointer-lowtag)
      ;; Is it one?
      (inst cmp rax-tn (+ (ash 1 n-widetag-bits) bignum-widetag))
      (inst jmp :e single-word)
      ;; If it's other than two, we can't be an (unsigned-byte 64)
      (inst cmp rax-tn (+ (ash 2 n-widetag-bits) bignum-widetag))
      (inst jmp :ne nope)
      ;; Get the second digit.
      (loadw rax-tn value (1+ bignum-digits-offset) other-pointer-lowtag)
      ;; All zeros, its an (unsigned-byte 64).
      (inst or rax-tn rax-tn)
      (inst jmp :z yep)
      (inst jmp nope)

      (emit-label single-word)
      ;; Get the single digit.
      (loadw rax-tn value bignum-digits-offset other-pointer-lowtag)

      ;; positive implies (unsigned-byte 64).
      (emit-label fixnum)
      (inst or rax-tn rax-tn)
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
    (let ((is-symbol-label (if not-p DROP-THRU target)))
      (inst cmp value nil-value)
      (inst jmp :e is-symbol-label)
      (test-type value target not-p (symbol-header-widetag)))
    DROP-THRU))

(define-vop (check-symbol check-type)
  (:generator 12
    (let ((error (generate-error-code vop 'object-not-symbol-error value)))
      (inst cmp value nil-value)
      (inst jmp :e DROP-THRU)
      (test-type value error t (symbol-header-widetag)))
    DROP-THRU
    (move result value)))

(define-vop (consp type-predicate)
  (:translate consp)
  (:generator 8
    (let ((is-not-cons-label (if not-p target DROP-THRU)))
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
