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
        (cond ((ea-p value) value) ; merged a memory load + fixnump vop
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

(defun %lea-for-lowtag-test (temp value lowtag &optional (operand-size :dword))
  ;; If OPERAND-SIZE is :QWORD, then instead of discarding the upper 32 bits
  ;; in a lowtag test, preserve the full word which becomes an untagged pointer.
  ;; The REX byte can't be avoided if VALUE is R8 or higher, so it costs nothing
  ;; to widen the LEA operand. In this way, we avoid emitting a displacement to
  ;; load the widetag. The total number of bytes emitted is reduced by 1,
  ;; or by nothing. ASSUMPTION: tagged pointer causes conservative pinning,
  ;; hence an untagged pointer remains valid.
  (inst lea operand-size temp (ea (- lowtag) value)))

;; Numerics including fixnum, excluding short-float. (INTEGER,RATIONAL)
(defun %test-fixnum-and-headers (value temp target not-p headers
                                 &key value-tn-ref)
  (let ((drop-through (gen-label)))
    (case n-fixnum-tag-bits
     (1 (%lea-for-lowtag-test temp value other-pointer-lowtag :qword)
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
     (1 (%lea-for-lowtag-test temp value other-pointer-lowtag :qword)
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
  (declare (ignore temp))
  (inst cmp :byte value immediate)
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
  (let* ((lowtag (if function-p fun-pointer-lowtag other-pointer-lowtag))
         ;; It is preferable (smaller and faster code) to directly
         ;; compare the value in memory instead of loading it into
         ;; a register first. Find out if this is possible and set
         ;; WIDETAG-TN accordingly. If impossible, generate the
         ;; register load.
         (widetag-tn (if (and (null (cdr headers))
                              (not except)
                              (or (atom (car headers))
                                  (= (caar headers) bignum-widetag)
                                  (= (cdar headers) complex-array-widetag)))
                         (ea (- lowtag) value)
                         temp))
         (first (car headers))
         (second (cadr headers))
         (untagged))

    (multiple-value-bind (equal less-or-equal greater-or-equal when-true
                                when-false)
        ;; EQUAL, LESS-OR-EQUAL, and GREATER-OR-EQUAL are the conditions
        ;; for branching to TARGET.  WHEN-TRUE and WHEN-FALSE are the
        ;; labels to branch to when we know it's true and when we know
        ;; it's false respectively.
        (if not-p
            (values :ne :a :b drop-through target)
            (values :e :na :nb target drop-through))

      (cond ((and value-tn-ref
                  (eq lowtag other-pointer-lowtag)
                  (other-pointer-tn-ref-p value-tn-ref))) ; best case: lowtag is right
            ((and value-tn-ref
                  ;; If HEADERS contains a range, then list pointers have to be
                  ;; disallowed - consider a list whose CAR has a fixnum that
                  ;; spuriously matches the range test.
                  (if (some #'listp headers)
                      (headered-object-pointer-tn-ref-p value-tn-ref)
                      (pointer-tn-ref-p value-tn-ref)))
             ;; Emit one fewer conditional jump than the general case,
             (inst mov temp value)
             (inst and temp (lognot lowtag-mask))
             (if (ea-p widetag-tn)
                 (setq widetag-tn (ea temp))
                 (setq untagged (ea temp))))
            (t
             ;; Regardless of whether :COMPUTE-TEMP is T or NIL, it will hold
             ;; an untagged ptr to VALUE if the lowtag test passes.
             (setq untagged (ea temp))
             (when (ea-p widetag-tn)
               (setq widetag-tn untagged))
             (when compute-temp
               (%lea-for-lowtag-test temp value lowtag :qword))
             (inst test :byte temp lowtag-mask)
             (inst jmp :nz when-false)))

      (when (eq widetag-tn temp)
        (inst mov :dword temp (or untagged (ea (- lowtag) value))))
      (dolist (widetag except)
        (inst cmp :byte temp widetag)
        (inst jmp :e when-false))

      (cond
       ((and (fixnump first)
             (fixnump second)
             (not (cddr headers))
             (= (logcount (logxor first second)) 1))
        ;; Two widetags differing at one bit. Use one cmp and branch.
        ;; Start by ORing in the bit that they differ on.
        (let ((diff-bit (logxor first second)))
          (aver (not (ea-p widetag-tn))) ; can't clobber a header
          (inst or :byte widetag-tn diff-bit)
          (inst cmp :byte widetag-tn (logior first diff-bit))
          (if not-p (inst jmp :ne target) (inst jmp :eq target))))
       (t
      ;; Compared to x86 we additionally optimize the cases of a
      ;; range starting with BIGNUM-WIDETAG (= min widetag)
      ;; or ending with COMPLEX-ARRAY-WIDETAG (= max widetag)
        (do ((remaining headers (cdr remaining)))
            ((null remaining))
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
                    (inst jmp less-or-equal target))))))))))

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
    (inst mov temp #.(- most-negative-fixnum))
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

(define-vop (pointerp)
  (:args (value :scs (any-reg descriptor-reg)))
  (:temporary (:sc unsigned-reg :from (:argument 0)) temp)
  (:conditional :z)
  (:policy :fast-safe)
  (:translate pointerp)
  (:generator 3
    (if (location= temp value) (inst sub :dword value 3) (inst lea :dword temp (ea -3 value)))
    (inst test :byte temp #b11)))

;; A fixnum or single-digit bignum satisfies signed-byte-64-p
(define-vop (signed-byte-64-p pointerp)
  (:translate signed-byte-64-p)
  (:conditional :z)
  (:args-var arg-ref)
  (:generator 6
    (when (types-equal-or-intersect (tn-ref-type arg-ref) (specifier-type 'fixnum))
      (inst test :byte value fixnum-tag-mask)
      (inst jmp :z out)) ; good
    (let ((ea (cond ((fixnum-or-other-pointer-tn-ref-p arg-ref)
                     (ea (- other-pointer-lowtag) value))
                    (t
                     (%lea-for-lowtag-test temp value other-pointer-lowtag :qword)
                     (inst test :byte temp lowtag-mask)
                     (inst jmp :nz out)
                     (ea temp)))))
      (inst cmp :qword ea (bignum-header-for-length 1)))
    OUT))

(define-vop (signed-byte-64-p/unsigned)
  (:args (value :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:conditional :ns)
  (:policy :fast-safe)
  (:translate signed-byte-64-p)
  (:generator 5
    (inst test value value)))

(macrolet ((define (name src-size)
             `(progn
                (define-vop (,name)
                  (:translate ,name)
                  (:args (value :scs (any-reg descriptor-reg)))
                  (:conditional :z)
                  (:args-var arg-ref)
                  (:policy :fast-safe)
                  (:temporary (:sc unsigned-reg) temp temp2)
                  (:generator 6
                    ;; Optimistically assume that the argument is a fixnum.
                    ;; If it is, then we're shifting out the tag bit, otherwise
                    ;; putting random bits in the low byte, which is harmless.
                    (move temp value)
                    (inst sar temp n-fixnum-tag-bits)
                    (inst movsx '(,src-size :qword) temp2 temp)
                    (inst cmp temp2 temp)
                    ;; If the input wasn't known to be a fixnum, and it passed
                    ;; the sign-extension test, then check FIXNUMP now.
                    (unless (csubtypep (tn-ref-type arg-ref) (specifier-type 'fixnum))
                      (inst jmp :nz OUT) ; did NOT pass the sign-extension test
                      (inst test :byte value fixnum-tag-mask))
                    OUT))
                (define-vop (,(symbolicate name "/SIGNED"))
                  (:translate ,name)
                  (:args (value :scs (signed-reg)))
                  (:arg-types signed-num)
                  (:conditional :z)
                  (:policy :fast-safe)
                  (:temporary (:sc unsigned-reg) temp)
                  (:generator 2
                    (inst movsx '(,src-size :qword) temp value)
                    (inst cmp temp value))))))
  (define signed-byte-8-p :byte)
  (define signed-byte-16-p :word)
  (define signed-byte-32-p :dword))

;;; Sign bit and fixnum tag bit.
(defconstant non-negative-fixnum-mask-constant
  #x8000000000000001)
(defconstant non-negative-fixnum-mask-constant-wired-address
  (+ static-space-start (* 12 n-word-bytes)))
;; the preceding constant is embedded in an array,
;; the header of which must not overlap the static alloc regions
#-sb-thread
(aver (>= (- non-negative-fixnum-mask-constant-wired-address (* 2 n-word-bytes))
          (+ (max boxed-region cons-region mixed-region) (* 3 n-word-bytes))))

;;; An (unsigned-byte 64) can be represented with either a positive
;;; fixnum, a bignum with exactly one positive digit, or a bignum with
;;; exactly two digits and the second digit all zeros.
(define-vop (unsigned-byte-64-p type-predicate)
  (:translate unsigned-byte-64-p)
  (:generator 10
    (let ((not-target (gen-label))
          (single-word (gen-label))
          (fixnum-p (types-equal-or-intersect (tn-ref-type args) (specifier-type 'fixnum))))
      (multiple-value-bind (yep nope)
          (if not-p
              (values not-target target)
              (values target not-target))
        (when fixnum-p
          ;; Is it a fixnum with the sign bit clear?
          (inst test (ea non-negative-fixnum-mask-constant-wired-address) value)
          (inst jmp :z yep))
        (cond ((fixnum-or-other-pointer-tn-ref-p args)
               (when fixnum-p
                 (inst test :byte value fixnum-tag-mask)
                 (inst jmp :z nope)))
              (t
               (%lea-for-lowtag-test temp value other-pointer-lowtag)
               (inst test :byte temp lowtag-mask)
               (inst jmp :ne nope)))
        ;; Get the header.
        (loadw temp value 0 other-pointer-lowtag)
        ;; Is it one?
        (inst cmp temp (bignum-header-for-length 1))
        (inst jmp :e single-word)
        ;; If it's other than two, we can't be an (unsigned-byte 64)
        ;: Leave TEMP holding 0 in the affirmative case.
        (inst sub temp (bignum-header-for-length 2))
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
                           (fixnumize hi)))
            (mask (lognot fixnum-hi))
            (constant
             (if (= (ldb (byte 64 0) mask) non-negative-fixnum-mask-constant)
                 (ea non-negative-fixnum-mask-constant-wired-address)
                 (constantize mask))))
       (inst test value constant))))

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

;;; SINGLE-FLOAT-P, CHARACTERP, UNBOUND-MARKER-P produce a flag result
;;; and never need a temporary.
(macrolet ((define (name widetag)
             `(define-vop (,name simple-type-predicate)
                (:translate ,name)
                (:info)
                (:conditional :z)
                (:generator 1 (inst cmp :byte value ,widetag)))))
  (define single-float-p single-float-widetag)
  (define characterp character-widetag)
  (define unbound-marker-p unbound-marker-widetag))

;;; FUNCTIONP, LISTP, %INSTANCEP, %OTHER-POINTER-P produce a flag result
(macrolet ((define (name lowtag)
             `(define-vop (,name pointerp)
                (:translate ,name)
                (:generator 2
                  (if (location= temp value)
                      (inst sub :dword value ,lowtag)
                      (inst lea :dword temp (ea (- ,lowtag) value)))
                  (inst test :byte temp lowtag-mask)))))
  (define functionp fun-pointer-lowtag)
  (define listp list-pointer-lowtag)
  (define %instancep instance-pointer-lowtag)
  (define %other-pointer-p other-pointer-lowtag))

(macrolet ((define (name widetag)
             `(define-vop (,name type-predicate)
                (:translate ,name)
                (:info)
                (:conditional :z)
                (:generator 4
                  (inst lea temp (ea (- fun-pointer-lowtag) value))
                  (inst test :byte temp lowtag-mask)
                  (inst jmp :ne out)
                  (inst cmp :byte (ea temp) ,widetag)
                  out))))
  (define closurep closure-widetag)
  (define simple-fun-p simple-fun-widetag)
  (define funcallable-instance-p funcallable-instance-widetag))

;;;; list/symbol types
;;;
;;; symbolp (or symbol (eq nil))
;;; consp (and list (not (eq nil)))

(defun test-other-ptr (arg arg-ref widetag temp label)
  (inst cmp :byte
        (cond ((other-pointer-tn-ref-p arg-ref)
               (ea (- other-pointer-lowtag) arg))
              (t
               (%lea-for-lowtag-test temp arg other-pointer-lowtag :qword)
               (inst test :byte temp lowtag-mask)
               (inst jmp :ne label)
               (ea temp)))
        widetag))

(define-vop (symbolp type-predicate)
  (:translate symbolp)
  (:info)
  (:conditional :z)
  (:generator 5
    ;; SYMBOLP would have been IR1-transformed to NON-NULL-SYMBOL-P when possible
    (inst cmp value nil-value)
    (inst jmp :e out)
    (test-other-ptr value args symbol-widetag temp out)
    out))

(define-vop (non-null-symbol-p symbolp)
  (:translate non-null-symbol-p)
  (:generator 3 (test-other-ptr value args symbol-widetag temp out) out))

;;; It would be far better if we could recognize the IR1 for
;;;  (AND (CONSP X) (EQ (CAR X) 'FOO))
;;; rather than treating (TYPEP X '(CONS (EQL FOO))) as a special case,
;;; but hey at least this provides the IR2 support for it.
(define-vop (car-eq-if-listp)
  (:args (value :scs (descriptor-reg))
         (obj :scs (immediate any-reg descriptor-reg)))
  (:temporary (:sc unsigned-reg) temp)
  (:conditional :z)
  (:policy :fast-safe)
  (:translate car-eq-if-listp)
  (:generator 3
    (inst lea temp (ea (- list-pointer-lowtag) value))
    (inst test :byte temp lowtag-mask)
    (inst jmp :nz out)
    (inst cmp :qword (ea temp) (encode-value-if-immediate obj))
    out))

(eval-when (:compile-toplevel) (aver (= sb-impl::package-id-bits 16)))
(define-vop (keywordp symbolp)
  (:translate keywordp)
  (:generator 3
    (inst lea temp (ea (- other-pointer-lowtag) value))
    (inst test :byte temp lowtag-mask)
    (inst jmp :ne out)
    (inst cmp :byte (ea temp) symbol-widetag)
    (inst jmp :ne out)
    (inst cmp :word (ea (+ (ash symbol-name-slot word-shift) 6) temp) sb-impl::+package-id-keyword+)
    out))

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

#+compact-instance-header
(progn
 (define-vop ()
   (:translate %instance-layout)
   (:policy :fast-safe)
   (:args (object :scs (descriptor-reg)))
   (:results (res :scs (descriptor-reg)))
   (:variant-vars lowtag)
   (:variant instance-pointer-lowtag)
   (:generator 1
    (inst mov :dword res (ea (- 4 lowtag) object))))
 (define-vop ()
   (:translate %set-instance-layout)
   (:policy :fast-safe)
   (:args (object :scs (descriptor-reg))
          (value :scs (any-reg descriptor-reg)))
   (:vop-var vop)
   (:temporary (:sc unsigned-reg) temp)
   (:node-var node)
   (:generator 1
     (unless (sb-c::set-slot-old-p node)
       (emit-gc-store-barrier object nil temp (vop-nth-arg 1 vop) value))
     (inst mov :dword (ea (- 4 instance-pointer-lowtag) object) value)))
 (define-vop (%fun-layout %instance-layout)
   (:translate %fun-layout)
   (:variant fun-pointer-lowtag))
 (define-vop (%set-fun-layout %set-instance-layout)
   (:translate %set-fun-layout)
   (:generator 1
     (unless (sb-c::set-slot-old-p node)
       (emit-gc-store-barrier object nil temp (vop-nth-arg 1 vop) value))
     (inst mov :dword (ea (- 4 fun-pointer-lowtag) object) value)))
 (define-vop ()
  (:translate sb-c::layout-eq)
  (:policy :fast-safe)
  (:conditional :e)
  (:args (object :scs (descriptor-reg))
         (layout :scs (descriptor-reg immediate #+metaspace constant)))
  (:arg-types * * (:constant t))
  (:info lowtag)
  (:generator 1
    ;; With metaspace, the layout argument is actually a #<WRAPPER>
    ;; which does not have IMMEDIATE sc, but rather CONSTANT sc.
    ;; But we use a layout fixup which stuffs in the pointer to the layout.
    (inst cmp :dword (ea (- 4 lowtag) object)
          (if (sc-is layout immediate constant)
              (make-fixup (tn-value layout) :layout)
              layout)))))

;;; Return the DISP part of an EA based on MEM-OP,
;;; which is a memory access vop such as INSTANCE-REF.
;;; Return NIL if the access can't be absorbed into a following instruction.
(defun valid-memref-byte-disp (mem-op &aux (info (vop-codegen-info mem-op)))
  (ecase (vop-name mem-op)
    (instance-index-ref-c
     ;; for historical reasons, this has a "-C" variant which takes an info arg
     (destructuring-bind (index) info
       (- (ash (+ index instance-slots-offset) word-shift)
          instance-pointer-lowtag)))
    ((%raw-instance-ref/word %raw-instance-ref/signed-word)
     ;; raw slot vops accept an immediate TN, not a codegen arg
     (let ((index (tn-ref-tn (tn-ref-across (vop-args mem-op)))))
       (when (sc-is index immediate)
         (- (ash (+ (tn-value index) instance-slots-offset) word-shift)
            instance-pointer-lowtag))))
    (slot
     (destructuring-bind (name index lowtag) info
       (declare (ignore name))
       (- (ash index word-shift) lowtag)))
    (data-vector-ref-with-offset/simple-vector-c
     (destructuring-bind (index offset) info
       (let ((disp (- (ash (+ vector-data-offset index offset) word-shift)
                      other-pointer-lowtag)))
         (if (typep disp '(signed-byte 32)) disp))))))

(define-vop (fixnump simple-type-predicate)
  (:translate fixnump)
  (:args-var arg-ref)
  (:args (value :scs (any-reg descriptor-reg) :load-if (tn-ref-memory-access arg-ref)))
  (:info)
  (:conditional :z)
  ;; the compiler is very sensitive to this cost here as regards boxing. DON'T TOUCH !!!
  (:generator 3
   (awhen (tn-ref-memory-access arg-ref)
     (setq value (ea (cdr it) value)))
   (generate-fixnum-test value)))

(macrolet
    ((define-simple-array-type-vops ()
         `(progn
           ,@(map 'list
                  (lambda (saetp &aux (primtype (saetp-primitive-type-name saetp))
                                      (name (symbolicate primtype "-P")))
                    `(define-vop (,name symbolp)
                       (:translate ,name)
                       (:generator 4
                        (test-other-ptr value args ,(saetp-typecode saetp) temp out)
                        out)))
                  *specialized-array-element-type-properties*))))
  (define-simple-array-type-vops))

;;; Try to absorb a memory load into FIXNUMP.
(defoptimizer (sb-c::vop-optimize fixnump) (vop)
  ;; Ensure that the fixnump vop does not try to absorb more than one memref.
  ;; That is, if the initial IR2 matches (fixnump (memref (memref))) which is simplified
  ;; to (memref+fixnump (memref x)), it would seem to allow matching of the pattern
  ;; again if this optimizer is reapplied, because the "new" fixnump vop is superficially
  ;; the same, except for the attachment of extra data to its input.
  (unless (tn-ref-memory-access (vop-args vop))
    (let ((prev (sb-c::previous-vop-is
                 vop
                 '(instance-index-ref-c slot
                   ;; FIXME: could we also handle the non "-C" vop?
                   ;; The problem is that because FIXNUMP only takes one arg,
                   ;; an additional TN would have to be grafted into the data flow
                   ;; to convey the INDEX arg. So it's not the same fixnump vop
                   ;; any more - it's like a two-arg variant of fixnump.
                   data-vector-ref-with-offset/simple-vector-c))))
      ;; Inhibit the optimization on simple-vector if we need to trap uninitialized reads.
      ;; #+ubsan always inhibits, otherwise it's policy-based
      (when (and prev
                 (eq (vop-name prev) 'data-vector-ref-with-offset/simple-vector-c)
                 #-ubsan (sb-c::policy (sb-c::vop-node vop) (= safety 3)))
        (return-from vop-optimize-fixnump-optimizer nil))
      (aver (not (vop-results vop))) ; is a :CONDITIONAL vop
      (when (and prev (eq (vop-block prev) (vop-block vop)))
        (let ((arg (vop-args vop)))
          (when (and (eq (tn-ref-tn (vop-results prev)) (tn-ref-tn arg))
                     (sb-c::very-temporary-p (tn-ref-tn arg)))
            (binding* ((disp (valid-memref-byte-disp prev) :exit-if-null)
                       (arg-ref
                        (sb-c:reference-tn (tn-ref-tn (vop-args prev)) nil))
                       (new (sb-c::emit-and-insert-vop
                             (sb-c::vop-node vop) (vop-block vop) (sb-c::vop-info vop)
                             arg-ref nil prev (vop-codegen-info vop))))
              (setf (tn-ref-memory-access arg-ref) `(:read . ,disp))
              (sb-c::delete-vop prev)
              (sb-c::delete-vop vop)
              new)))))))

(define-vop (>-integer-fixnum)
  (:translate >)
  (:args (integer :scs (descriptor-reg))
         (fixnum :scs (immediate any-reg)))
  (:arg-types (:or integer bignum) tagged-num)
  (:temporary (:sc unsigned-reg) temp)
  (:conditional)
  (:info target not-p)
  (:args-var args)
  (:policy :fast-safe)
  (:variant-vars comparison)
  (:variant :g)
  (:generator 8
    (unless (sc-is (tn-ref-tn args) descriptor-reg control-stack)
      (setf args (tn-ref-across args)))
    (let* ((integer-p (csubtypep (tn-ref-type args) (specifier-type 'integer)))
           (fixnum (if (sc-is fixnum immediate)
                       (let* ((value (fixnumize (tn-value fixnum))))
                         (cond ((plausible-signed-imm32-operand-p value)
                                value)
                               (t
                                (inst mov temp value)
                                temp)))
                       fixnum)))
      (multiple-value-bind (yep nope)
          (if not-p
              (values not-target target)
              (values target not-target))
        (assemble ()
          (when (types-equal-or-intersect (tn-ref-type args) (specifier-type 'fixnum))
            (generate-fixnum-test integer)
            (inst jmp :nz BIGNUM)
            (if (eql fixnum 0)
                (inst test integer integer)
                (inst cmp integer fixnum))
            (inst jmp comparison yep)
            (inst jmp nope))
          bignum
          (unless (fixnum-or-other-pointer-tn-ref-p args)
            (test-type integer temp nope t (other-pointer-lowtag)))
          (loadw temp integer 0 other-pointer-lowtag)
          (unless integer-p
            (inst cmp :byte temp bignum-widetag)
            (inst jmp :ne nope))
          #.(assert (= (integer-length bignum-widetag) 5))
          (inst shr temp 5)
          (inst cmp :qword (ea (- other-pointer-lowtag) integer temp) 0)
          (inst jmp (case comparison
                      ((:l :le) (if not-p :ge :l))
                      (t (if not-p :l :ge)))
                target))))
    not-target))

(define-vop (<-integer-fixnum >-integer-fixnum)
  (:translate <)
  (:variant :l))

(define-vop (>-fixnum-integer >-integer-fixnum)
  (:translate >)
  (:args (fixnum :scs (immediate any-reg))
         (integer :scs (descriptor-reg)))
  (:arg-types tagged-num (:or integer bignum))
  (:variant :l))

(define-vop (<-fixnum-integer >-fixnum-integer)
  (:translate <)
  (:variant :g))

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
  (:variant :le))
