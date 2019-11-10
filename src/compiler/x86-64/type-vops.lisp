;;;; type testing and checking VOPs for the x86-64 VM

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
  "Set the Z flag if VALUE is fixnum"
  (inst test :byte
        (cond ;; This is hooey. None of the type-vops presently allow
              ;; control-stack as a storage class.
              ((sc-is value control-stack)
               (ea (frame-byte-offset (tn-offset value)) rbp-tn))
              (t
               ;; Don't check for (ANY-REG DESCRIPTOR-REG) because VALUE
               ;; can have a "weird" SC that would not have tag bits.
               ;; Sometimes a vop temp is specified as UNSIGNED-REG, e.g.
               value))
        fixnum-tag-mask))

(defun %test-fixnum (value temp target not-p)
  (declare (ignore temp))
  (generate-fixnum-test value)
  (inst jmp (if not-p :nz :z) target))

(defun %lea-for-lowtag-test (temp value lowtag)
  (inst lea :dword temp (ea (- lowtag) value)))

;; Numerics including fixnum, excluding short-float. (INTEGER,RATIONAL)
(defun %test-fixnum-and-headers (value temp target not-p headers
                                 &key value-tn-ref)
  (let ((drop-through (gen-label)))
    (case n-fixnum-tag-bits
     (1 (%lea-for-lowtag-test temp value other-pointer-lowtag)
        (inst test :byte temp 1)
        (inst jmp :nz (if not-p drop-through target)) ; inverted
        (%test-headers value temp target not-p nil headers
                       :drop-through drop-through :compute-temp nil
                       :value-tn-ref value-tn-ref))
     (t
      (generate-fixnum-test value)
      (inst jmp :z (if not-p drop-through target))
      (%test-headers value temp target not-p nil headers
                     :drop-through drop-through)))))

;; I can see no reason this would ever be used.
;; (or fixnum character|unbound-marker) is implausible.
(defun %test-fixnum-and-immediate (value target not-p immediate)
  (error "WAT")
  (let ((drop-through (gen-label)))
    (generate-fixnum-test value)
    (inst jmp :z (if not-p drop-through target))
    (%test-immediate value target not-p immediate drop-through)))

;; Numerics
(defun %test-fixnum-immediate-and-headers (value temp target not-p immediate headers
                                           &key value-tn-ref)
  (let ((drop-through (gen-label)))
    (case n-fixnum-tag-bits
     (1 (%lea-for-lowtag-test temp value other-pointer-lowtag)
        (inst test :byte temp 1)
        (inst jmp :nz (if not-p drop-through target)) ; inverted
        (inst cmp :byte temp (- immediate other-pointer-lowtag))
        (inst jmp :e (if not-p drop-through target))
        (%test-headers value temp target not-p nil headers
                       :drop-through drop-through :compute-temp nil
                       :value-tn-ref value-tn-ref))
     (t (generate-fixnum-test value)
        (inst jmp :z (if not-p drop-through target))
        (%test-immediate-and-headers value temp target not-p immediate headers
                                     :drop-through drop-through)))))

(defun %test-immediate (value temp target not-p immediate
                        &optional (drop-through (gen-label)))
  ;; Code a single instruction byte test if possible.
  (cond ((sc-is value any-reg descriptor-reg)
         (inst cmp :byte value immediate))
        (t
         (move temp value) ; FIXME - why load?
         (inst cmp :byte temp immediate)))
  (inst jmp (if not-p :ne :e) target)
  (emit-label drop-through))

;; Numerics including short-float, excluding fixnum
(defun %test-immediate-and-headers (value temp target not-p immediate headers
                                    &key (drop-through (gen-label))
                                         value-tn-ref)
  ;; Code a single instruction byte test if possible.
  (cond ((sc-is value any-reg descriptor-reg)
         (inst cmp :byte value immediate))
        (t
         (move temp value) ; FIXME - why load?
         (inst cmp :byte temp immediate)))
  (inst jmp :e (if not-p drop-through target))
  (%test-headers value temp target not-p nil headers
                 :drop-through drop-through
                 :value-tn-ref value-tn-ref))

(defun %test-lowtag (value temp target not-p lowtag)
  (%lea-for-lowtag-test temp value lowtag)
  (inst test :byte temp lowtag-mask)
  (inst jmp (if not-p :nz :z) target))

(defun %test-headers (value temp target not-p function-p headers
                      &key except
                           (drop-through (gen-label))
                           (compute-temp t)
                           value-tn-ref)
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

      (unless (and value-tn-ref
                   (eq lowtag other-pointer-lowtag)
                   (other-pointer-tn-ref-p value-tn-ref))
        (when compute-temp
          (%lea-for-lowtag-test temp value lowtag))
        (inst test :byte temp lowtag-mask)
        (inst jmp :nz when-false))
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
                           (ea (- lowtag) value)
                           (progn (inst mov :dword temp (ea (- lowtag) value))
                                  temp))))
          ((null remaining))
        (dolist (widetag except) ; only after loading widetag-tn
          (inst cmp :byte temp widetag)
          (inst jmp :e when-false))
        (setq except nil)
        (let ((header (car remaining))
              (last (null (cdr remaining))))
          (cond
           ((atom header)
            (inst cmp :byte widetag-tn header)
            (if last
                (inst jmp equal target)
                (inst jmp :e when-true)))
           (t
             (let ((start (car header))
                   (end (cdr header)))
               (cond
                 ((= start bignum-widetag)
                  (inst cmp :byte widetag-tn end)
                  (if last
                      (inst jmp less-or-equal target)
                      (inst jmp :be when-true)))
                 ((= end complex-array-widetag)
                  (inst cmp :byte widetag-tn start)
                  (if last
                      (inst jmp greater-or-equal target)
                      (inst jmp :b when-false)))
                 ((not last)
                  (inst cmp :byte temp start)
                  (inst jmp :b when-false)
                  (inst cmp :byte temp end)
                  (inst jmp :be when-true))
                 (t
                  (inst sub :byte temp start)
                  (inst cmp :byte temp (- end start))
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
  (:generator 3
    (move tmp value)
    (inst shr tmp n-positive-fixnum-bits)))

#-#.(cl:if (cl:= sb-vm:n-fixnum-tag-bits 1) '(:and) '(:or))
(define-vop (fixnump/signed-byte-64 simple-type-predicate)
  (:args (value :scs (signed-reg)))
  (:info)
  (:conditional :z)
  (:temporary (:sc unsigned-reg) temp)
  (:arg-types signed-num)
  (:translate fixnump)
  (:generator 3
    ;; Hackers Delight, p. 53: signed
    ;;    a <= x <= a + 2^n - 1
    ;; is equivalent to unsigned
    ;;    ((x-a) >> n) = 0
    (inst mov temp #.(- sb-xc:most-negative-fixnum))
    (inst add temp value)
    (inst shr temp n-fixnum-bits)))

#+#.(cl:if (cl:= sb-vm:n-fixnum-tag-bits 1) '(:and) '(:or))
(define-vop (fixnump/signed-byte-64 simple-type-predicate)
  (:args (value :scs (signed-reg) :target temp))
  (:info)
  (:conditional :no)
  (:temporary (:sc unsigned-reg :from (:argument 0)) temp)
  (:arg-types signed-num)
  (:translate fixnump)
  (:generator 3
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
      (case n-fixnum-tag-bits
        (1 (%lea-for-lowtag-test temp value other-pointer-lowtag)
           (inst test :byte temp fixnum-tag-mask) ; 0th bit = 1 => fixnum
           (inst jmp :nz yep)
           (inst test :byte temp lowtag-mask))
        (t ;; we'll only examine 1 byte, but moving a :dword is preferable
           ;; as it doesn't require the CPU to retain the other 7 bytes.
           (inst mov :dword temp value)
           (inst test :byte temp fixnum-tag-mask)
           (inst jmp :e yep)
           (inst and :byte temp lowtag-mask)
           (inst cmp :byte temp other-pointer-lowtag)))
      (inst jmp :ne nope)
      (inst cmp :qword (ea (- other-pointer-lowtag) value)
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
        (inst mov temp value)
        (inst test :byte temp fixnum-tag-mask)
        (inst jmp :e fixnum)

        ;; If not, is it an other pointer?
        (inst and :byte temp lowtag-mask)
        (inst cmp :byte temp other-pointer-lowtag)
        (inst jmp :ne nope)
        ;; Get the header.
        (loadw temp value 0 other-pointer-lowtag)
        ;; Is it one?
        (inst cmp temp (+ (ash 1 n-widetag-bits) bignum-widetag))
        (inst jmp :e single-word)
        ;; If it's other than two, we can't be an (unsigned-byte 64)
        ;: Leave TEMP holding 0 in the affirmative case.
        (inst sub temp (+ (ash 2 n-widetag-bits) bignum-widetag))
        (inst jmp :ne nope)
        ;; Compare the second digit to zero (in TEMP).
        (inst cmp (object-slot-ea value (1+ bignum-digits-offset) other-pointer-lowtag)
              temp)
        (inst jmp :z yep) ; All zeros, its an (unsigned-byte 64).
        (inst jmp nope)

        (emit-label single-word)
        ;; Get the single digit.
        (loadw temp value bignum-digits-offset other-pointer-lowtag)

        ;; positive implies (unsigned-byte 64).
        (emit-label fixnum)
        (inst test temp temp)
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
  (:translate fixnum-mod-p)
  (:conditional :e)
  (:info hi)
  (:policy :fast-safe)
  (:generator 4
     (let* ((fixnum-hi (if (sc-is value unsigned-reg signed-reg)
                           hi
                           (fixnumize hi))))
       (inst test value (constantize (lognot fixnum-hi))))))

(define-vop (test-fixnum-mod-tagged-unsigned)
  (:args (value :scs (any-reg unsigned-reg signed-reg)))
  (:arg-types (:or tagged-num unsigned-num signed-num)
              (:constant fixnum))
  (:translate fixnum-mod-p)
  (:conditional :be)
  (:info hi)
  (:policy :fast-safe)
  (:generator 5
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
  (:policy :fast-safe)
  (:generator 6
     (let* ((fixnum-hi (fixnumize hi))
            (skip (gen-label)))
       (generate-fixnum-test value)
       (inst jmp :ne (if not-p target skip))
       (inst cmp value (constantize fixnum-hi))
       (inst jmp (if not-p :a :be) target)
       (emit-label skip))))

(define-vop (pointerp)
  (:args (value :scs (any-reg descriptor-reg) :target temp))
  (:temporary (:sc unsigned-reg :from (:argument 0)) temp)
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:translate pointerp)
  (:generator 3
    (inst lea :dword temp (ea -3 value))
    (inst test :byte temp #b11)
    (inst jmp (if not-p :nz :z) target)))

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
      (test-type value temp target not-p (symbol-widetag)))
    DROP-THRU))

(define-vop (consp type-predicate)
  (:translate consp)
  (:generator 8
    (let ((is-not-cons-label (if not-p target DROP-THRU)))
      (inst cmp value nil-value)
      (inst jmp :e is-not-cons-label)
      (test-type value temp target not-p (list-pointer-lowtag)))
    DROP-THRU))

(define-vop (widetag=)
  (:translate widetag=)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:info widetag)
  (:arg-types * (:constant t))
  (:conditional :e)
  (:args-var args)
  (:generator 2
   (inst cmp :byte (ea (- other-pointer-lowtag) x) widetag)))

;;; TRANSFORM-INSTANCE-TYPEP checks for this vop by name and will try to use it,
;;; so don't define it if inapplicable.
#+compact-instance-header
(progn
(defknown layout-eq (instance t) boolean (flushable))
(define-vop (layout-eq)
  (:translate layout-eq)
  (:policy :fast-safe)
  (:conditional :e)
  (:args (instance :scs (descriptor-reg))
         (layout :scs (descriptor-reg immediate)))
  (:generator 1
    (inst cmp :dword
          (ea (- 4 instance-pointer-lowtag) instance)
          (if (sc-is layout immediate)
              (make-fixup (tn-value layout) :layout)
              layout)))))

(defknown layout-inherits-ref-eq (simple-vector index t) boolean (flushable))
(define-vop (layout-inherits-ref-eq)
  (:translate layout-inherits-ref-eq)
  (:policy :fast-safe)
  (:conditional :e)
  (:args (vector :scs (descriptor-reg))
         (index :scs (any-reg descriptor-reg immediate))
         (thing :scs (descriptor-reg immediate)))
  (:generator 1
    (inst cmp :dword
          (if (sc-is index immediate)
              (ea (+ (- other-pointer-lowtag)
                     (ash (+ vector-data-offset (tn-value index)) word-shift))
                  vector)
              (ea (+ (- other-pointer-lowtag)
                     (ash vector-data-offset word-shift))
                  vector index (ash 1 (- word-shift n-fixnum-tag-bits))))
          (if (sc-is thing immediate)
              (make-fixup (tn-value thing) :layout)
              thing))))
