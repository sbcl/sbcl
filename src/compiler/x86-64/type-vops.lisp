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
                                 &key value-tn-ref immediate-tested)
  (let ((drop-through (gen-label)))
    (case n-fixnum-tag-bits
     (1
      (%lea-for-lowtag-test temp value other-pointer-lowtag :qword)
      (when (types-equal-or-intersect (tn-ref-type value-tn-ref)
                                      (specifier-type 'fixnum))
        (inst test :byte temp 1)
        (inst jmp :nz (if not-p drop-through target))) ; inverted
      (%test-headers value temp target not-p nil headers
                     :drop-through drop-through :compute-temp nil
                     :value-tn-ref value-tn-ref
                     :immediate-tested immediate-tested))
     (t
      (generate-fixnum-test value)
      (inst jmp :z (if not-p drop-through target))
      (%test-headers value temp target not-p nil headers
                     :drop-through drop-through
                     :immediate-tested immediate-tested)))))

;; Numerics
(defun %test-fixnum-immediate-and-headers (value temp target not-p immediate headers
                                           &key value-tn-ref immediate-tested)
  (let ((drop-through (gen-label)))
    (case n-fixnum-tag-bits
     (1
      (%lea-for-lowtag-test temp value other-pointer-lowtag :qword)
      (cond ((and (eq immediate single-float-widetag)
                  (number-or-other-pointer-tn-ref-p value-tn-ref))
             (inst test :byte temp lowtag-mask)
             (inst jmp :nz (if not-p drop-through target)))
            (t
             (when (types-equal-or-intersect (tn-ref-type value-tn-ref)
                                             (specifier-type 'fixnum))
               (inst test :byte temp 1)
               (inst jmp :nz (if not-p drop-through target))) ; inverted
             (when (or (/= immediate single-float-widetag)
                       (types-equal-or-intersect (tn-ref-type value-tn-ref)
                                                 (specifier-type 'single-float)))
               (inst cmp :byte temp (- immediate other-pointer-lowtag))
               (inst jmp :e (if not-p drop-through target)))))

      (%test-headers value temp target not-p nil headers
                     :drop-through drop-through :compute-temp nil
                     :value-tn-ref value-tn-ref
                     :immediate-tested immediate-tested))
     (t (generate-fixnum-test value)
        (inst jmp :z (if not-p drop-through target))
        (%test-immediate-and-headers value temp target not-p immediate headers
                                     :drop-through drop-through
                                     :immediate-tested immediate-tested)))))

(defun %test-immediate (value temp target not-p immediate
                        &key value-tn-ref)
  (declare (ignore temp value-tn-ref))
  (inst cmp :byte value immediate)
  (inst jmp (if not-p :ne :e) target))

;; Numerics including short-float, excluding fixnum
(defun %test-immediate-and-headers (value temp target not-p immediate headers
                                    &key (drop-through (gen-label))
                                         value-tn-ref
                                         immediate-tested)
  ;; Code a single instruction byte test if possible.
  (cond ((sc-is value any-reg descriptor-reg)
         (inst cmp :byte value immediate))
        (t
         (move temp value) ; FIXME - why load?
         (inst cmp :byte temp immediate)))
  (inst jmp :e (if not-p drop-through target))
  (%test-headers value temp target not-p nil headers
                 :drop-through drop-through
                 :value-tn-ref value-tn-ref
                 :immediate-tested immediate-tested))

(defun %test-lowtag (value temp target not-p lowtag &key value-tn-ref)
  (declare (ignore value-tn-ref))
  (%lea-for-lowtag-test temp value lowtag)
  (inst test :byte temp lowtag-mask)
  (inst jmp (if not-p :nz :z) target))

(defun %test-headers (value temp target not-p function-p headers
                      &key except
                           (drop-through (gen-label))
                           (load-widetag t)
                           (compute-temp load-widetag)
                           value-tn-ref
                           immediate-tested)
  (let* ((lowtag (if function-p fun-pointer-lowtag other-pointer-lowtag))
         ;; It is preferable (smaller and faster code) to directly
         ;; compare the value in memory instead of loading it into
         ;; a register first. Find out if this is possible and set
         ;; WIDETAG-TN accordingly. If impossible, generate the
         ;; register load.
         (widetag-tn (if (and load-widetag
                              (null (cdr headers))
                              (not except)
                              (or (atom (car headers))
                                  (= (caar headers) bignum-widetag)
                                  (= (cdar headers) complex-array-widetag)
                                  (and value-tn-ref
                                       (= (caar headers) simple-array-widetag)
                                       (csubtypep (tn-ref-type value-tn-ref) (specifier-type 'array)))))
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
      (flet ((test-lowtag (target not-p &optional test)
               (cond ((not load-widetag)
                      nil)
                     ((and value-tn-ref
                           (eq lowtag other-pointer-lowtag)
                           (other-pointer-tn-ref-p value-tn-ref t immediate-tested))
                      nil) ; best case: lowtag is right
                     ((and value-tn-ref
                           (not test)
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
                          (setq untagged (ea temp)))
                      nil)
                     (t
                      ;; Regardless of whether :COMPUTE-TEMP is T or NIL, it will hold
                      ;; an untagged ptr to VALUE if the lowtag test passes.
                      (setq untagged (ea temp))
                      (when (ea-p widetag-tn)
                        (setq widetag-tn untagged))
                      (when compute-temp
                        (%lea-for-lowtag-test temp value lowtag :qword))
                      (inst test :byte temp lowtag-mask)
                      (inst jmp (if not-p :nz :z) target)
                      t))))
        (cond
          ((and value-tn-ref
                (not except)
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
           (or (test-lowtag target not-p t)
               (unless not-p
                 (inst jmp target))))
          (t
           (test-lowtag when-false t)
           (when (and load-widetag
                      (eq widetag-tn temp))
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
                    ((and (eql header simple-array-widetag)
                          value-tn-ref
                          (csubtypep (tn-ref-type value-tn-ref) (specifier-type 'string))))
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
                          (inst jmp less-or-equal target)))))))))))))

      (emit-label drop-through))))

;;;; other integer ranges

(define-vop (simple-type-predicate)
  (:args (value :scs (any-reg descriptor-reg control-stack)))
  (:conditional)
  (:arg-refs args)
  (:policy :fast-safe))

(define-vop (fixnump/unsigned-byte-64 simple-type-predicate)
  (:args (value :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:translate fixnump)
  (:temporary (:sc unsigned-reg :from (:argument 0)) tmp)
  (:conditional :z)
  (:generator 3
    (move tmp value)
    (inst shr tmp n-positive-fixnum-bits)))

#-#.(cl:if (cl:= sb-vm:n-fixnum-tag-bits 1) '(:and) '(:or))
(define-vop (fixnump/signed-byte-64 simple-type-predicate)
  (:args (value :scs (signed-reg)))
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
  (:arg-refs arg-ref)
  (:generator 6
    (when (types-equal-or-intersect (tn-ref-type arg-ref) (specifier-type 'fixnum))
      (inst test :byte value fixnum-tag-mask)
      (inst jmp :z out)) ; good
    (let ((ea (cond ((fixnum-or-other-pointer-tn-ref-p arg-ref t)
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
             `(define-vop (,(symbolicate name "/SIGNED"))
                (:translate ,name)
                (:args (value :scs (signed-reg)))
                (:arg-types signed-num)
                (:conditional :z)
                (:policy :fast-safe)
                (:temporary (:sc unsigned-reg) temp)
                (:generator 2
                  (inst movsx '(,src-size :qword) temp value)
                  (inst cmp temp value)))))
  (define signed-byte-8-p :byte)
  (define signed-byte-16-p :word)
  (define signed-byte-32-p :dword))

(define-vop (signed-byte-8-p)
  (:translate signed-byte-8-p)
  (:args (value :scs (any-reg descriptor-reg)))
  (:conditional :z)
  (:arg-refs arg-ref)
  (:policy :fast-safe)
  (:temporary (:sc unsigned-reg) temp)
  (:generator 6
    (inst lea temp (ea (ash (expt 2 7) n-fixnum-tag-bits) value))
    (inst test temp (lognot (fixnumize (1- (expt 2 8)))))))

(define-vop (signed-byte-16-p)
  (:translate signed-byte-16-p)
  (:args (value :scs (any-reg descriptor-reg)))
  (:conditional :z)
  (:arg-refs arg-ref)
  (:policy :fast-safe)
  (:temporary (:sc unsigned-reg) temp)
  (:generator 6
    (inst lea temp (ea (ash (expt 2 15) n-fixnum-tag-bits) value))
    (inst test temp (lognot (fixnumize (1- (expt 2 16)))))))

(define-vop (signed-byte-32-p)
  (:translate signed-byte-32-p)
  (:args (value :scs (any-reg descriptor-reg)))
  (:conditional :z)
  (:arg-refs arg-ref)
  (:policy :fast-safe)
  (:temporary (:sc unsigned-reg) temp temp2)
  (:generator 6
    (move temp value)
    (inst sar temp n-fixnum-tag-bits)
    (inst movsx '(:dword :qword) temp2 temp)
    (inst cmp temp2 temp)
    (unless (csubtypep (tn-ref-type arg-ref) (specifier-type 'fixnum))
      (inst jmp :nz out)
      (inst test :byte value fixnum-tag-mask))
    out))

;;; An (unsigned-byte 64) can be represented with either a positive
;;; fixnum, a bignum with exactly one positive digit, or a bignum with
;;; exactly two digits and the second digit all zeros.
(define-vop (unsigned-byte-64-p type-predicate)
  (:translate unsigned-byte-64-p)
  (:generator 10
    (let* ((fixnum-p (types-equal-or-intersect (tn-ref-type args) (specifier-type 'fixnum)))
           (not-signed-byte-64-p (not (types-equal-or-intersect (tn-ref-type args) (specifier-type 'signed-word))))
           (unsigned-p (or not-signed-byte-64-p
                           (not (types-equal-or-intersect (tn-ref-type args) (specifier-type '(integer * -1)))))))
      (multiple-value-bind (yep nope)
          (if not-p
              (values not-target target)
              (values target not-target))
        (assemble ()
          (cond ((fixnum-or-other-pointer-tn-ref-p args t)
                 (when fixnum-p
                   (inst test :byte value fixnum-tag-mask)
                   (cond (unsigned-p
                          (inst jmp :z yep))
                         (t
                          (inst jmp :nz bignum)
                          (inst test value value)
                          (inst jmp :ns yep)
                          (inst jmp nope)))))
                (t
                 (when fixnum-p
                   (cond (unsigned-p
                          (inst test :byte value fixnum-tag-mask)
                          (inst jmp :z yep))
                         (t ;; Is it a fixnum with the sign bit clear?
                          (inst test (constantize non-negative-fixnum-mask) value)
                          (inst jmp :z yep))))
                 (%lea-for-lowtag-test temp value other-pointer-lowtag)
                 (inst test :byte temp lowtag-mask)
                 (inst jmp :ne nope)))
          ;; Get the header.
          bignum
          (loadw temp value 0 other-pointer-lowtag)

          (unless not-signed-byte-64-p
            ;; Is it one?
            (inst cmp temp (bignum-header-for-length 1))
            (cond (unsigned-p
                   (inst jmp :e yep))
                  (t
                   (inst jmp :ne two-word)
                   ;; is it positive?
                   (inst cmp :byte (ea (+ (- (* bignum-digits-offset n-word-bytes) other-pointer-lowtag)
                                          (1- n-word-bytes))
                                       value) 0)
                   (inst jmp :ns yep))))

          two-word
          ;; If it's other than two, we can't be an (unsigned-byte 64)
          ;; Leave TEMP holding 0 in the affirmative case.
          (inst sub temp (bignum-header-for-length 2))
          (inst jmp :ne nope)
          ;; Compare the second digit to zero (in TEMP).
          (inst cmp (object-slot-ea value (1+ bignum-digits-offset) other-pointer-lowtag) temp)
          (inst jmp (if not-p :nz :z) target))))
    not-target))

(define-vop (unsigned-byte-x-p type-predicate)
  (:arg-types * (:constant (integer 1)))
  (:translate sb-c::unsigned-byte-x-p)
  (:info target not-p x)
  (:temporary (:sc unsigned-reg) last-digit)
  (:generator 10
    (let* ((type (tn-ref-type args))
           (fixnum-p (types-equal-or-intersect type (specifier-type 'fixnum)))
           (integer-p (csubtypep type (specifier-type 'integer)))
           (unsigned-p (not (types-equal-or-intersect type (specifier-type '(integer * -1))))))
      (multiple-value-bind (yep nope)
          (if not-p
              (values not-target target)
              (values target not-target))
        (assemble ()
          (when fixnum-p
            (cond (unsigned-p
                   (inst test :byte value fixnum-tag-mask)
                   (inst jmp :z yep))
                  (t ;; Is it a fixnum with the sign bit clear?
                   (inst test (constantize non-negative-fixnum-mask) value)
                   (inst jmp :z yep))))
          (cond ((fixnum-or-other-pointer-tn-ref-p args t)
                 (when (and fixnum-p
                            (not unsigned-p))
                   (inst test :byte value fixnum-tag-mask)
                   (inst jmp :z nope)))
                (t
                 (%lea-for-lowtag-test temp value other-pointer-lowtag)
                 (inst test :byte temp lowtag-mask)
                 (inst jmp :ne nope)))
          ;; Get the header.
          (loadw temp value 0 other-pointer-lowtag)
          (unless integer-p
            (inst cmp :byte temp bignum-widetag)
            (inst jmp :ne nope))
          (inst shr temp n-widetag-bits)
          (inst cmp :dword temp (1+ (/ x n-word-bits)))
          (inst jmp :g nope)
          ;; Is it a sign-extended sign bit
          (cond (unsigned-p
                 (inst jmp :l yep)
                 (inst cmp :dword (ea (+ (- other-pointer-lowtag) (/ n-word-bytes 2))
                                      value temp n-word-bytes)
                       0)
                 (inst jmp (if not-p :nz :z) target))
                (t
                 (inst mov last-digit (ea (- other-pointer-lowtag) value temp n-word-bytes))
                 (inst jmp :l fixnum)
                 (inst test last-digit last-digit)
                 (inst jmp :nz nope)))
          fixnum
          (unless unsigned-p
            (inst test last-digit last-digit)
            (inst jmp (if not-p :s :ns) target)))))
    not-target))

;;; SINGLE-FLOAT-P, CHARACTERP, UNBOUND-MARKER-P produce a flag result
;;; and never need a temporary.
(macrolet ((define (name widetag)
             `(define-vop (,name simple-type-predicate)
                (:translate ,name)
                (:conditional :z)
                (:generator 1 (inst cmp :byte value ,widetag)))))
  (define single-float-p single-float-widetag)
  (define characterp character-widetag)
  (define unbound-marker-p unbound-marker-widetag))

;;; FUNCTIONP, LISTP, %INSTANCEP, %OTHER-POINTER-P produce a flag result
(macrolet ((define (name lowtag)
             `(define-vop (,name pointerp)
                (:translate ,name)
                (:arg-refs value-ref)
                (:vop-var vop)
                (:generator 2
                  (multiple-value-bind (bit set) (tn-ref-lowtag-bit ,lowtag value-ref)
                    (cond
                      (bit
                       (inst test :byte value (ash 1 bit))
                       (when (eq set 1)
                         (change-vop-flags vop '(:nz))))
                      (t
                       (if (location= temp value)
                           (inst sub :dword value ,lowtag)
                           (inst lea :dword temp (ea (- ,lowtag) value)))
                       (inst test :byte temp lowtag-mask))))))))
  (define functionp fun-pointer-lowtag)
  (define listp list-pointer-lowtag)
  (define %instancep instance-pointer-lowtag)
  (define %other-pointer-p other-pointer-lowtag))

;;; Function subtypes produce a flag result
(macrolet ((define (name widetag)
             `(define-vop (,name type-predicate)
                (:translate ,name)
                (:info) ; nullify the info
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
;;; Various OTHER-POINTER objects produce a flag result.
;;; The parens around widetag are from copy&paste of generic/type-vops
(macrolet ((define (name (widetag))
             `(define-vop (,name type-predicate)
                (:translate ,name)
                (:info) ; nullify the info
                (:conditional :z)
                (:generator 4
                  (test-other-ptr value args ,widetag temp out)
                  out))))
  (define bignump (bignum-widetag))
  (define ratiop (ratio-widetag))
  (define complex-rational-p (complex-rational-widetag))
  (define complex-single-float-p (complex-single-float-widetag))
  (define complex-double-float-p (complex-double-float-widetag))
  (define double-float-p (double-float-widetag))
  (define system-area-pointer-p (sap-widetag))
  (define weak-pointer-p (weak-pointer-widetag))
  (define code-component-p (code-header-widetag))
  (define fdefn-p (fdefn-widetag))
  (define simple-array-header-p (simple-array-widetag))
  (define complex-vector-p (complex-vector-widetag)))

(macrolet ((fail-if-not-otherptr ()
             `(cond ((other-pointer-tn-ref-p value-tn-ref t)
                     (inst mov :byte temp (ea (- other-pointer-lowtag) value)))
                    (t
                     (inst lea temp (ea (- other-pointer-lowtag) value))
                     (inst test :byte temp lowtag-mask)
                     ;; TEST clears the Carry, so if this jump occurs,
                     ;; it returns the correct answer for the vops that return
                     ;; their result in Z or C.
                     (inst jmp :nz out)
                     (inst mov :byte temp (ea temp))))))
  #+sb-unicode
  (macrolet ((define (name (simple nonsimple))
               (aver (= (logior 8 (symbol-value simple)) (symbol-value nonsimple)))
               `(define-vop (,name type-predicate)
                  (:translate ,name)
                  (:info) ; nullify the info
                  (:conditional :z)
                  (:arg-refs value-tn-ref)
                  (:generator 4
                    (fail-if-not-otherptr)
                    (inst or :byte temp 8)
                    (inst cmp :byte temp ,nonsimple)
                    out))))
    (define base-string-p (simple-base-string-widetag complex-base-string-widetag))
    (define character-string-p
        (simple-character-string-widetag complex-character-string-widetag)))
  (macrolet ((define (name widetags)
               (let* ((widetags (symbol-value widetags))
                      (min (reduce #'min widetags))
                      (max (reduce #'max widetags)))
                 `(define-vop (,name type-predicate)
                    (:translate ,name)
                    (:info)
                    (:conditional :c) ; Carry flag = "below" (unsigned)
                    (:arg-refs value-tn-ref)
                    (:vop-var vop)
                    (:generator 4
                      (cond ((and (eq ,min simple-array-widetag)
                                  (csubtypep (tn-ref-type value-tn-ref) (specifier-type 'array)))
                             (change-vop-flags vop '(:le))
                             (inst cmp :byte (ea (- other-pointer-lowtag) value)
                                   ,max))
                            (t
                             (fail-if-not-otherptr)
                             (inst sub :byte temp ,min)
                             (inst cmp :byte temp ,(1+ (- max min)))))
                      OUT)))))
    (define simple-rank-1-array-*-p +simple-rank-1-array-widetags+)
    (define vectorp +vector-widetags+)
    (define simple-array-p +simple-array-widetags+)
    #+sb-unicode (define stringp +string-widetags+)))

;;;; list/symbol types
;;;
;;; symbolp (or symbol (eq nil))
;;; consp (and list (not (eq nil)))

;;; Test whether ARG is an other-pointer to WIDETAG, setting the Z flag if so
(defun test-other-ptr (arg arg-ref widetag temp label &optional (permit-nil t))
  (inst cmp :byte
        (cond ((other-pointer-tn-ref-p arg-ref permit-nil)
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
    ;; If the VALUE were known to be an OTHER-POINTER, the IR1 lvar type
    ;; can't intersect NULL. So the IR1 should have been transformed to NON-NULL-SYMBOL-P.
    ;; Hence this must *not* be known to be an OTHER-POINTER.
    (aver (not (other-pointer-tn-ref-p args)))
    (if (other-pointer-tn-ref-p args t) ; allow NIL
        (inst cmp :byte (ea (- other-pointer-lowtag) value) symbol-widetag)
        (assemble ()
          (%lea-for-lowtag-test temp value other-pointer-lowtag :qword)
          (inst test :byte temp lowtag-mask)
          (inst jmp :e compare-widetag)
          (inst cmp value null-tn)
          (inst jmp out)
          compare-widetag
          (inst cmp :byte (ea temp) symbol-widetag)))
    out))

(define-vop (non-null-symbol-p symbolp)
  (:translate non-null-symbol-p)
  (:generator 3
    (test-other-ptr value args symbol-widetag temp out nil)
    out))

;;; It would be far better if we could recognize the IR1 for
;;;  (AND (CONSP X) (EQ (CAR X) 'FOO))
;;; rather than treating (TYPEP X '(CONS (EQL FOO))) as a special case,
;;; but hey at least this provides the IR2 support for it.
(define-vop (car-eq-if-listp)
  (:args (value :scs (descriptor-reg))
         (obj :scs (any-reg descriptor-reg
                            (immediate (reg-or-legal-imm32-p tn)))))
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
    (cond ((csubtypep (tn-ref-type args) (specifier-type 'symbol))
           (inst cmp :word (ea (- 1 other-pointer-lowtag) value)
                 sb-impl::+package-id-keyword+))
          (t
           (cond ((other-pointer-tn-ref-p args t)
                  (inst mov :dword temp (object-slot-ea value 0 other-pointer-lowtag)))
                 (t
                  (inst lea temp (ea (- other-pointer-lowtag) value))
                  (inst test :byte temp lowtag-mask)
                  (inst jmp :ne out)
                  (inst mov :dword temp (ea temp))))
           (inst shl :dword temp 8) ; zeroize flag/generation bits
           (inst cmp :dword temp
                 (ash (logior (ash sb-impl::+package-id-keyword+ 8) symbol-widetag) 8))))
    out))

(define-vop (consp type-predicate)
  (:translate consp)
  (:generator 8
    (let ((is-not-cons-label (if not-p target DROP-THRU)))
      (inst cmp value null-tn)
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
  (:arg-refs args)
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
   (:generator 1
     (emit-gengc-barrier object nil temp (vop-nth-arg 1 vop))
     (inst mov :dword (ea (- 4 instance-pointer-lowtag) object) value)))
 (define-vop (%fun-layout %instance-layout)
   (:translate %fun-layout)
   (:variant fun-pointer-lowtag))
 (define-vop (%set-fun-layout %set-instance-layout)
   (:translate %set-fun-layout)
   (:generator 1
     (emit-gengc-barrier object nil temp (vop-nth-arg 1 vop))
     (inst mov :dword (ea (- 4 fun-pointer-lowtag) object) value)))
 (define-vop ()
  (:translate sb-c::layout-eq)
  (:policy :fast-safe)
  (:conditional :e)
  (:args (object :scs (descriptor-reg))
         (layout :scs (descriptor-reg immediate)))
  (:arg-types * * (:constant t))
  (:info lowtag)
  (:generator 1
    (inst cmp :dword (ea (- 4 lowtag) object)
          (if (sc-is layout immediate)
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
  (:arg-refs arg-ref)
  (:args (value :scs (any-reg descriptor-reg)))
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
  (:arg-refs integer-ref)
  (:policy :fast-safe)
  (:variant-vars comparison)
  (:variant :g)
  (:generator 8
    (let* ((integer-p (csubtypep (tn-ref-type integer-ref) (specifier-type 'integer)))
           (fixnum (if (sc-is fixnum immediate)
                       (let* ((value (fixnumize (tn-value fixnum)))
                              (one (fixnumize 1)))
                         (cond ((plausible-signed-imm32-operand-p value)
                                value)
                               ((and (plausible-signed-imm32-operand-p (+ value one))
                                     (or (and (eql comparison :le)
                                              (setf comparison :l))
                                         (and (eql comparison :g)
                                              (setf comparison :ge))))
                                (setf value (+ value one)))
                               ((and (plausible-signed-imm32-operand-p (- value one))
                                     (or (and (eql comparison :ge)
                                              (setf comparison :g))
                                         (and (eql comparison :l)
                                              (setf comparison :le))))
                                (setf value (- value one)))
                               (t
                                (inst mov temp value)
                                temp)))
                       fixnum)))
      (multiple-value-bind (yep nope)
          (if not-p
              (values not-target target)
              (values target not-target))
        (assemble ()
          (when (types-equal-or-intersect (tn-ref-type integer-ref) (specifier-type 'fixnum))
            (generate-fixnum-test integer)
            (cond
              ((and (eql fixnum 0)
                    (memq comparison '(:ge :l))
                    (csubtypep (type-intersection (tn-ref-type integer-ref)
                                                  (specifier-type 'fixnum))
                               (specifier-type '(integer * -1))))
               (inst jmp :z (if (eq comparison :l)
                                yep
                                nope)))
              ((and (eql fixnum 0)
                    (memq comparison '(:ge :l))
                    (csubtypep (type-intersection (tn-ref-type integer-ref)
                                                  (specifier-type 'fixnum))
                               (specifier-type '(integer 0))))
               (inst jmp :z (if (eq comparison :l)
                                nope
                                yep)))
              (t
               (inst jmp :nz BIGNUM)
               (cond ((eql fixnum 0)
                      (inst test integer integer))
                     ((or (and (eql fixnum #.(fixnumize -1))
                               (or (and (eql comparison :le)
                                        (setf comparison :l))
                                   (and (eql comparison :g)
                                        (setf comparison :ge))))
                          (and (eql fixnum #.(fixnumize 1))
                               (or (and (eql comparison :ge)
                                        (setf comparison :g))
                                   (and (eql comparison :l)
                                        (setf comparison :le)))))
                      (inst test integer integer))
                     (t
                      (inst cmp integer fixnum)))
               (inst jmp comparison yep)
               (inst jmp nope))))
          bignum
          (unless (fixnum-or-other-pointer-tn-ref-p integer-ref t)
            (test-type integer temp nope t (other-pointer-lowtag)))
          (loadw temp integer 0 other-pointer-lowtag)
          (unless integer-p
            (inst cmp :byte temp bignum-widetag)
            (inst jmp :ne nope))
          #.(assert (= (integer-length bignum-widetag) 5))
          (inst shr temp 5)
          (inst cmp :dword (ea (+ (- other-pointer-lowtag) (/ n-word-bytes 2)) integer temp) 0)
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
  (:arg-refs nil integer-ref)
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

(define-vop (load-other-pointer-widetag)
  (:args (value :scs (any-reg descriptor-reg)))
  (:arg-refs value-ref)
  (:info not-other-pointer-label null-label zero-extend)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (when null-label
      ;; Since the comparison against a register, not an imm8 or imm32 any more,
      ;; there is probably no reason to distinguish the two cases here.
      ;; The only conceivable reason would be if null-tn were in a low register
      ;; then it's theoretically possible that a REX prefix could be avoided.
      (if (types-equal-or-intersect
           (type-difference (tn-ref-type value-ref) (specifier-type 'null))
           (specifier-type 'cons))
          (inst cmp value null-tn)
          (inst cmp :byte value null-tn)) ; was: (logand nil-value #xff)
      (inst jmp :e null-label))
    (cond ((other-pointer-tn-ref-p value-ref t)
           (if zero-extend
               (inst movzx '(:byte :dword) r (ea (- other-pointer-lowtag) value))
               (inst mov :byte r (ea (- other-pointer-lowtag) value))))
          (t
           (%lea-for-lowtag-test r value other-pointer-lowtag :qword)
           (inst test :byte r lowtag-mask)
           (inst jmp :nz not-other-pointer-label)
           (if zero-extend
               (inst movzx '(:byte :dword) r (ea r))
               (inst mov :byte r (ea r)))))))

(define-vop (test-widetag)
  (:args (value :scs (unsigned-reg) :target temp))
  (:temporary (:sc unsigned-reg :from (:argument 1)) temp)
  (:info target not-p type-codes)
  (:generator 1
    (move temp value :dword)
    (%test-headers nil temp target not-p nil
      (if (every #'integerp type-codes)
          (canonicalize-widetags type-codes)
          type-codes)
      :load-widetag nil)))

(macrolet ((read-depthoid ()
             `(ea (- (+ 4 (ash (+ instance-slots-offset
                                  (get-dsd-index layout sb-kernel::flags))
                               word-shift))
                     instance-pointer-lowtag)
                  layout)))
  (define-vop ()
    (:translate layout-depthoid)
    (:policy :fast-safe)
    (:args (layout :scs (descriptor-reg)))
    (:results (res :scs (any-reg)))
    (:result-types fixnum)
    (:generator 1
      (inst movsx '(:dword :qword) res (read-depthoid))))
  (define-vop ()
    (:translate sb-c::layout-depthoid-ge)
    (:policy :fast-safe)
    (:args (layout :scs (descriptor-reg)))
    (:info k)
    (:arg-types * (:constant (unsigned-byte 16)))
    (:conditional :ge)
    (:generator 1
      (inst cmp :dword (read-depthoid) (fixnumize k))))

  (defun structure-is-a (layout test-layout &optional target not-p done)
    (let ((test-layout
            (case (layout-classoid-name test-layout)
                       (condition +condition-layout-flag+)
                       (pathname  +pathname-layout-flag+)
                       (structure-object +structure-layout-flag+)
                       (t test-layout))))
     (cond ((integerp test-layout)
            (inst test
                  (if (typep test-layout '(unsigned-byte 8))
                      :byte
                      :dword)
                  (ea (- (ash (+ instance-slots-offset
                                 (get-dsd-index layout sb-kernel::flags))
                              word-shift)
                         instance-pointer-lowtag)
                      layout)
                  test-layout))
           ((let ((classoid (layout-classoid test-layout)))
              (and (eq (classoid-state classoid) :sealed)
                   (not (classoid-subclasses classoid))))
            (emit-constant test-layout)
            #+compact-instance-header
            (inst cmp :dword
                  layout (make-fixup test-layout :layout))
            #-compact-instance-header
            (inst cmp (emit-constant test-layout) layout))

           (t
            (let* ((depthoid (layout-depthoid test-layout))
                   (offset (+ (id-bits-offset)
                              (ash (- depthoid 2) 2)
                              (- instance-pointer-lowtag))))
              (when (and target
                         (> depthoid sb-kernel::layout-id-vector-fixed-capacity))
                (inst cmp :dword (read-depthoid) (fixnumize depthoid))
                (inst jmp :l (if not-p target done)))
              (inst cmp :dword
                    (ea offset layout)
                    ;; Small layout-ids can only occur for layouts made in genesis.
                    ;; Therefore if the compile-time value of the ID is small,
                    ;; it is permanently assigned to that type.
                    ;; Otherwise, we allow for the possibility that the compile-time ID
                    ;; is not the same as the load-time ID.
                    ;; I don't think layout-id 0 can get here, but be sure to exclude it.
                    (cond ((or (typep (layout-id test-layout) '(and (signed-byte 8) (not (eql 0))))
                               (not (sb-c::producing-fasl-file)))
                           (layout-id test-layout))
                          (t
                           (make-fixup test-layout :layout-id))))))))))

(define-vop ()
  (:translate sb-c::%structure-is-a)
  (:args (x :scs (descriptor-reg)))
  (:arg-types * (:constant t))
  (:info test)
  (:policy :fast-safe)
  (:conditional :e)
  (:generator 1
    (structure-is-a x test)))

(define-vop ()
  (:translate sb-c::structure-typep)
  (:args (object :scs (descriptor-reg)))
  (:arg-types * (:constant t))
  (:arg-refs args)
  (:policy :fast-safe)
  (:conditional)
  (:info target not-p test-layout)
  (:temporary (:sc descriptor-reg) layout)
  (:generator 4
    (unless (instance-tn-ref-p args)
      (%test-lowtag object layout (if not-p target done) t instance-pointer-lowtag))

    (cond ((and (not (memq (layout-classoid-name test-layout)
                           '(condition pathname structure-object)))
                (let ((classoid (layout-classoid test-layout)))
                  (and (eq (classoid-state classoid) :sealed)
                       (not (classoid-subclasses classoid)))))
           (emit-constant test-layout)
           #+compact-instance-header
           (inst cmp :dword (ea (- 4 instance-pointer-lowtag) object)
                 (make-fixup test-layout :layout))
           #-compact-instance-header
           (progn
             (inst mov layout (emit-constant test-layout))
             (inst cmp (object-slot-ea object instance-slots-offset instance-pointer-lowtag)
                   layout)))
          (t
           #+compact-instance-header
           (inst mov :dword layout (ea (- 4 instance-pointer-lowtag) object))
           #-compact-instance-header
           (loadw layout object instance-slots-offset instance-pointer-lowtag)
           (structure-is-a layout test-layout target not-p done)))
    (inst jmp (if (if  (memq (layout-classoid-name test-layout)
                             '(condition pathname structure-object))
                      (not not-p)
                      not-p)
                  :ne :e) target)
    done))

(define-vop (structure-typep*)
  (:args (layout :scs (descriptor-reg)))
  (:arg-types * (:constant t))
  (:policy :fast-safe)
  (:info target not-p test-layout)
  (:generator 4
    (structure-is-a layout test-layout target not-p done)
    (inst jmp (if (if  (memq (layout-classoid-name test-layout)
                             '(condition pathname structure-object))
                      (not not-p)
                      not-p)
                  :ne :e) target)
    done))

(define-vop (load-instance-layout)
  (:args (object :scs (any-reg descriptor-reg)))
  (:arg-refs args)
  (:info not-instance)
  (:temporary (:sc unsigned-reg) temp)
  (:results (r :scs (descriptor-reg)))
  (:generator 1
    (unless (instance-tn-ref-p args)
      (%test-lowtag object temp not-instance t instance-pointer-lowtag))
    #+compact-instance-header
    (inst mov :dword r (ea (- 4 instance-pointer-lowtag) object))
    #-compact-instance-header
    (loadw r object instance-slots-offset instance-pointer-lowtag)))
