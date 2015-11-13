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

;;; Optimize the case of moving a 64-bit value into RAX when not caring
;;; about the upper 32 bits: often the REX prefix can be spared.
(defun move-qword-to-eax (value)
  (if (and (sc-is value any-reg descriptor-reg)
           (< (tn-offset value) r8-offset))
      (move eax-tn (reg-in-size value :dword))
      (move rax-tn value)))

(defun generate-fixnum-test (value)
  #!+sb-doc
  "Set the Z flag if VALUE is fixnum"
  (inst test
        (cond ((sc-is value any-reg descriptor-reg)
               (reg-in-size value :byte))
              ;; This is hooey. None of the type-vops presently allow
              ;; control-stack as a storage class.
              ((sc-is value control-stack)
               (make-ea :byte :base rbp-tn
                        :disp (frame-byte-offset (tn-offset value))))
              (t
               value))
        fixnum-tag-mask))

(defun %test-fixnum (value target not-p)
  (generate-fixnum-test value)
  (inst jmp (if not-p :nz :z) target))

;;; General FIXME: it's fine that we wire these to use rAX which has
;;; the shortest encoding, but for goodness sake can we pass the TN
;;; from the VOP like every other backend does? Freely referencing the
;;; permanent globals RAX-TN,EAX-TN,AL-TN is a bad way to go about it.

(defun %lea-for-lowtag-test (target value lowtag)
  (inst lea target (make-ea :dword :base value :disp (- lowtag))))

;; Numerics including fixnum, excluding short-float. (INTEGER,RATIONAL)
(defun %test-fixnum-and-headers (value target not-p headers)
  (let ((drop-through (gen-label)))
    (case n-fixnum-tag-bits
     (1 (%lea-for-lowtag-test eax-tn value other-pointer-lowtag)
        (inst test al-tn 1)
        (inst jmp :nz (if not-p drop-through target)) ; inverted
        (%test-headers value target not-p nil headers
                       :drop-through drop-through :compute-eax nil))
     (t
      (generate-fixnum-test value)
      (inst jmp :z (if not-p drop-through target))
      (%test-headers value target not-p nil headers
                     :drop-through drop-through)))))

;; I can see no reason this would ever be used.
;; (or fixnum character|unbound-marker) is implausible.
(defun %test-fixnum-and-immediate (value target not-p immediate)
  (let ((drop-through (gen-label)))
    (generate-fixnum-test value)
    (inst jmp :z (if not-p drop-through target))
    (%test-immediate value target not-p immediate drop-through)))

;; Numerics
(defun %test-fixnum-immediate-and-headers (value target not-p immediate
                                           headers)
  (let ((drop-through (gen-label)))
    (case n-fixnum-tag-bits
     (1 (%lea-for-lowtag-test eax-tn value other-pointer-lowtag)
        (inst test al-tn 1)
        (inst jmp :nz (if not-p drop-through target)) ; inverted
        (inst cmp al-tn (- immediate other-pointer-lowtag))
        (inst jmp :e (if not-p drop-through target))
        (%test-headers value target not-p nil headers
                       :drop-through drop-through :compute-eax nil))
     (t (generate-fixnum-test value)
        (inst jmp :z (if not-p drop-through target))
        (%test-immediate-and-headers value target not-p immediate headers
                                     drop-through)))))

(defun %test-immediate (value target not-p immediate
                        &optional (drop-through (gen-label)))
  ;; Code a single instruction byte test if possible.
  (cond ((sc-is value any-reg descriptor-reg)
         (inst cmp (reg-in-size value :byte) immediate))
        (t
         (move rax-tn value)
         (inst cmp al-tn immediate)))
  (inst jmp (if not-p :ne :e) target)
  (emit-label drop-through))

;; Numerics including short-float, excluding fixnum
(defun %test-immediate-and-headers (value target not-p immediate headers
                                    &optional (drop-through (gen-label)))
  ;; Code a single instruction byte test if possible.
  (cond ((sc-is value any-reg descriptor-reg)
         (inst cmp (reg-in-size value :byte) immediate))
        (t
         (move rax-tn value)
         (inst cmp al-tn immediate)))
  (inst jmp :e (if not-p drop-through target))
  (%test-headers value target not-p nil headers :drop-through drop-through))

(defun %test-lowtag (value target not-p lowtag)
  (%lea-for-lowtag-test eax-tn value lowtag)
  (inst test al-tn lowtag-mask)
  (inst jmp (if not-p :nz :z) target))

(defun %test-headers (value target not-p function-p headers
                      &key except
                           (drop-through (gen-label))
                           (compute-eax t))
  (let ((lowtag (if function-p fun-pointer-lowtag other-pointer-lowtag)))
    (multiple-value-bind (equal less-or-equal greater-or-equal when-true
                                when-false)
        ;; EQUAL, LESS-OR-EQUAL, and GREATER-OR-EQUAL are the conditions
        ;; for branching to TARGET.  WHEN-TRUE and WHEN-FALSE are the
        ;; labels to branch to when we know it's true and when we know
        ;; it's false respectively.
        (if not-p
            (values :ne :a :b drop-through target)
            (values :e :na :nb target drop-through))
      (when compute-eax
        (%lea-for-lowtag-test eax-tn value lowtag))
      (inst test al-tn lowtag-mask)
      (inst jmp :nz when-false)
      ;; FIXME: this backend seems to be missing the special logic for
      ;;        testing exactly two widetags differing only in a single bit,
      ;;        which through evolution is almost totally unworkable anyway...
      (do ((remaining headers (cdr remaining))
           ;; It is preferable (smaller and faster code) to directly
           ;; compare the value in memory instead of loading it into
           ;; a register first. Find out if this is possible and set
           ;; WIDETAG-TN accordingly. If impossible, generate the
           ;; register load.
           ;; Compared to x86 we additionally optimize the cases of a
           ;; range starting with BIGNUM-WIDETAG (= min widetag)
           ;; or ending with COMPLEX-ARRAY-WIDETAG (= max widetag)
           (widetag-tn (if (and (null (cdr headers))
                                (not except)
                                (or (atom (car headers))
                                    (= (caar headers) bignum-widetag)
                                    (= (cdar headers) complex-array-widetag)))
                           (make-ea :byte :base value :disp (- lowtag))
                           (progn
                             (inst mov eax-tn (make-ea :dword :base value
                                                       :disp (- lowtag)))
                             al-tn))))
          ((null remaining))
        (dolist (widetag except) ; only after loading widetag-tn
          (inst cmp al-tn widetag)
          (inst jmp :e when-false))
        (setq except nil)
        (let ((header (car remaining))
              (last (null (cdr remaining))))
          (cond
           ((atom header)
            (inst cmp widetag-tn header)
            (if last
                (inst jmp equal target)
                (inst jmp :e when-true)))
           (t
             (let ((start (car header))
                   (end (cdr header)))
               (cond
                 ((= start bignum-widetag)
                  (inst cmp widetag-tn end)
                  (if last
                      (inst jmp less-or-equal target)
                      (inst jmp :be when-true)))
                 ((= end complex-array-widetag)
                  (inst cmp widetag-tn start)
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
                  (inst jmp less-or-equal target))))))))
      (emit-label drop-through))))

;;;; other integer ranges

(define-vop (fixnump/unsigned-byte-64 simple-type-predicate)
  (:args (value :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:translate fixnump)
  (:temporary (:sc unsigned-reg :from (:argument 0)) tmp)
  (:info)
  (:conditional :z)
  (:generator 5
    (move tmp value)
    (inst shr tmp n-positive-fixnum-bits)))

#-#.(cl:if (cl:= sb!vm:n-fixnum-tag-bits 1) '(:and) '(:or))
(define-vop (fixnump/signed-byte-64 simple-type-predicate)
  (:args (value :scs (signed-reg)))
  (:info)
  (:conditional :z)
  (:temporary (:sc unsigned-reg) temp)
  (:arg-types signed-num)
  (:translate fixnump)
  (:generator 5
    ;; Hackers Delight, p. 53: signed
    ;;    a <= x <= a + 2^n - 1
    ;; is equivalent to unsigned
    ;;    ((x-a) >> n) = 0
    (inst mov temp #.(- sb!xc:most-negative-fixnum))
    (inst add temp value)
    (inst shr temp n-fixnum-bits)))

#+#.(cl:if (cl:= sb!vm:n-fixnum-tag-bits 1) '(:and) '(:or))
(define-vop (fixnump/signed-byte-64 simple-type-predicate)
  (:args (value :scs (signed-reg) :target temp))
  (:info)
  (:conditional :no)
  (:temporary (:sc unsigned-reg :from (:argument 0)) temp)
  (:arg-types signed-num)
  (:translate fixnump)
  (:generator 5
    (move temp value)
    ;; The overflow flag will be set if the reg's sign bit changes.
    (inst shl temp 1)))

;;; A (SIGNED-BYTE 64) can be represented with either fixnum or a bignum with
;;; exactly one digit.

(define-vop (signed-byte-64-p type-predicate)
  (:translate signed-byte-64-p)
  (:generator 45
    (multiple-value-bind (yep nope)
        (if not-p
            (values not-target target)
            (values target not-target))
      #.(case n-fixnum-tag-bits
          (1 '(progn
               (%lea-for-lowtag-test eax-tn value other-pointer-lowtag)
               (inst test al-tn fixnum-tag-mask) ; 0th bit = 1 => fixnum
               (inst jmp :nz yep)
               (inst test al-tn lowtag-mask)))
          (t '(progn
               (move-qword-to-eax value)
               (inst test al-tn fixnum-tag-mask)
               (inst jmp :e yep)
               (inst and al-tn lowtag-mask)
               (inst cmp al-tn other-pointer-lowtag))))
      (inst jmp :ne nope)
      (inst cmp (make-ea-for-object-slot value 0 other-pointer-lowtag)
            (+ (ash 1 n-widetag-bits) bignum-widetag))
      (inst jmp (if not-p :ne :e) target))
    NOT-TARGET))

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
        (move rax-tn value)
        (inst test al-tn fixnum-tag-mask)
        (inst jmp :e fixnum)

        ;; If not, is it an other pointer?
        (inst and al-tn lowtag-mask)
        (inst cmp al-tn other-pointer-lowtag)
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
        (inst test rax-tn rax-tn)
        (inst jmp :z yep)
        (inst jmp nope)

        (emit-label single-word)
        ;; Get the single digit.
        (loadw rax-tn value bignum-digits-offset other-pointer-lowtag)

        ;; positive implies (unsigned-byte 64).
        (emit-label fixnum)
        (inst test rax-tn rax-tn)
        (inst jmp (if not-p :s :ns) target)

        (emit-label not-target)))))

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
  (:conditional :e)
  (:info hi)
  (:save-p :compute-only)
  (:policy :fast-safe)
  (:generator 4
     (aver (not (sc-is value immediate)))
     (let* ((fixnum-hi (if (sc-is value unsigned-reg signed-reg)
                           hi
                           (fixnumize hi))))
       (inst test value (constantize (lognot fixnum-hi))))))

(define-vop (test-fixnum-mod-tagged-unsigned)
  (:args (value :scs (any-reg descriptor-reg
                              unsigned-reg signed-reg
                              immediate)))
  (:arg-types (:or tagged-num unsigned-num signed-num)
              (:constant fixnum))
  (:translate fixnum-mod-p)
  (:conditional :be)
  (:info hi)
  (:save-p :compute-only)
  (:policy :fast-safe)
  (:generator 5
     (aver (not (sc-is value immediate)))
     (let ((fixnum-hi (if (sc-is value unsigned-reg signed-reg)
                          hi
                          (fixnumize hi))))
       (inst cmp value (constantize fixnum-hi)))))

(define-vop (test-fixnum-mod-*)
  (:args (value :scs (any-reg descriptor-reg)))
  (:arg-types * (:constant fixnum))
  (:translate fixnum-mod-p)
  (:conditional)
  (:info target not-p hi)
  (:save-p :compute-only)
  (:policy :fast-safe)
  (:generator 6
     (let* ((fixnum-hi (fixnumize hi))
            (skip (gen-label)))
       (generate-fixnum-test value)
       (inst jmp :ne (if not-p target skip))
       (inst cmp value (constantize fixnum-hi))
       (inst jmp (if not-p :a :be) target)
       (emit-label skip))))

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

(define-vop (consp type-predicate)
  (:translate consp)
  (:generator 8
    (let ((is-not-cons-label (if not-p target DROP-THRU)))
      (inst cmp value nil-value)
      (inst jmp :e is-not-cons-label)
      (test-type value target not-p (list-pointer-lowtag)))
    DROP-THRU))

;; A vop that accepts a computed set of widetags.
(define-vop (%other-pointer-subtype-p type-predicate)
  (:translate %other-pointer-subtype-p)
  (:info target not-p widetags)
  (:arg-types * (:constant t)) ; voodoo - 'target' and 'not-p' are absent
  (:generator 15 ; arbitrary
    (multiple-value-bind (headers exceptions)
        (canonicalize-headers-and-exceptions widetags)
      (%test-headers value target not-p nil headers
                     :except exceptions))))
