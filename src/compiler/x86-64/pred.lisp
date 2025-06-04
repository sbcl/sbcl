;;;; predicate VOPs for the x86 VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;;; the branch VOP

;;; The unconditional branch, emitted when we can't drop through to the desired
;;; destination. Dest is the continuation we transfer control to.
(define-vop (branch)
  (:info dest)
  (:generator 5
    (inst jmp dest)))


;;;; Generic conditional VOPs

;;; The generic conditional branch, emitted immediately after test
;;; VOPs that only set flags.
;;;
;;; FLAGS is a list of condition descriptors. If the first descriptor
;;; is CL:NOT, the test was true if all the remaining conditions are
;;; false. Otherwise, the test was true if any of the conditions is.
;;;
;;; NOT-P flips the meaning of the test, as with regular :CONDITIONAL
;;; VOP. If NOT-P is true, the code must branch to dest if the test was
;;; false. Otherwise, the code must branch to dest if the test was true.

(define-vop (branch-if)
  (:info dest not-p flags)
  (:vop-var vop)
  (:generator 0
    (let ((flags (conditional-flags-flags flags)))
      (when (cdr flags)
        ;; Specifying multiple flags is an extremely confusing convention that supports
        ;; floating-point inequality tests utilizing the flag register in an unusual way,
        ;; as documented in the COMISS and COMISD instructions:
        ;; "the ZF, PF, and CF flags in the EFLAGS register according
        ;;  to the result (unordered, greater than, less than, or equal)"
        ;; I think it would have been better if, instead of allowing more than one flag,
        ;; we passed in a pseudo-condition code such as ':sf<=' and deferred to this vop
        ;; to interpret the abstract condition in terms of how to CPU sets the bits
        ;; in those particular instructions.
        ;; Note that other architectures allow only 1 flag in branch-if, if it works at all.
        ;; Enable this assertion if you need to sanity-check the preceding claim.
        ;; There's really no "dynamic" reason for it to fail.
        #+nil
        (aver (memq (vop-name (sb-c::vop-prev vop))
                    '(<single-float <double-float <=single-float <=double-float
                      >single-float >double-float >=single-float >=double-float
                      =/single-float =/double-float))))
      (when (eq (car flags) 'not)
        (pop flags)
        (setf not-p (not not-p)))
      (cond ((null (rest flags))
             (inst jmp
                   (if not-p
                       (negate-condition (first flags))
                       (first flags))
                   dest))
            (not-p
             (let ((not-lab (gen-label))
                   (last    (car (last flags))))
               (dolist (flag (butlast flags))
                 (inst jmp flag not-lab))
               (inst jmp (negate-condition last) dest)
               (emit-label not-lab)))
            (t
             (dolist (flag flags)
               (inst jmp flag dest)))))))

(define-vop (jump-table)
  (:args (index :scs (signed-reg unsigned-reg any-reg)
                :target offset))
  (:info targets otherwise min max)
  (:temporary (:sc any-reg :from (:argument 0)) offset)
  (:temporary (:sc unsigned-reg) table)
  (:generator 0
    (let ((fixnump (sc-is index any-reg)))
      (flet ((fix (x)
               (if fixnump
                   (fixnumize x)
                   x)))
        (let (disp)
          (unless (zerop min)
            (let ((byte-disp (- (* min n-word-bytes))))
              (if (and (not otherwise)
                       (typep byte-disp '(signed-byte 32)))
                  (setf disp byte-disp)
                  (let ((diff (- (fix min))))
                    (unless (typep diff '(signed-byte 32))
                      (inst mov table diff)
                      (setf diff table))
                    (cond ((location= offset index)
                           (inst add offset diff))
                          (t
                           (inst lea offset (ea diff index))
                           (setf index offset)))))))
          (when otherwise
            (inst cmp index (fix (- max min)))
            (inst jmp :a otherwise))
          (inst lea table (register-inline-constant :jump-table targets))
          (inst jmp (if disp
                        (ea disp table index (if fixnump 4 8))
                        (ea table index (if fixnump 4 8)))))))))

(defun convert-conditional-move-p (dst-tn)
  (sc-case dst-tn
    ((descriptor-reg any-reg)
     'move-if/t)
    (unsigned-reg
     'move-if/unsigned)
    (signed-reg
     'move-if/signed)
    ;; FIXME: Can't use CMOV with byte registers, and characters live
    ;; in such outside of unicode builds. A better solution then just
    ;; disabling MOVE-IF/CHAR should be possible, though.
    #+sb-unicode
    (character-reg
     'move-if/char)
    (sap-reg
     'move-if/sap)
    (t)))

(define-vop (move-if)
  (:args (then) (else))
  (:results (res))
  (:info flags)
  (:temporary (:sc unsigned-reg) temp)
  (:generator 0
    (let* ((flags (conditional-flags-flags flags))
           (not-p (eq (first flags) 'not))
           (size))
      (when not-p (pop flags))
      (when (location= res then)
        (rotatef then else)
        (setf not-p (not not-p)))
      (flet ((load-immediate (dst constant-tn
                              &optional (sc-reg dst))
               (let ((bits (immediate-tn-repr constant-tn
                                              (sc-is sc-reg any-reg descriptor-reg))))
                 (if (typep bits '(unsigned-byte 31))
                     (unless size
                       (setf size :dword))
                     (setf size :qword))
                 (if (nil-relative-p bits)
                     (move-immediate dst bits)
                 ;; Can't use ZEROIZE, since XOR will affect the flags.
                     (inst mov dst bits)))))
        (cond ((null (rest flags))
               (cond ((sc-is else immediate)
                      (load-immediate res else))
                     (t
                      (setf size :qword)
                      (move res else)))
               (cond ((sc-is then immediate)
                      (load-immediate temp then res)
                      (setf then temp))
                     (t
                      (setf size :qword)))
               (inst cmov size (if not-p
                                   (negate-condition (first flags))
                                   (first flags))
                     res
                     then))
              (not-p
               (cond ((sc-is then immediate)
                      (when (location= else res)
                        (inst mov temp else)
                        (setf else temp))
                      (load-immediate res then))
                     ((location= else res)
                      (move temp else)
                      (move res then)
                      (setf else temp))
                     (t
                      (move res then)))
               (when (sc-is else immediate)
                 (load-immediate temp else res)
                 (setf else temp))
               (dolist (flag flags)
                 (inst cmov flag res else)))
              (t
               (if (sc-is else immediate)
                   (load-immediate res else)
                   (move res else))
               (when (sc-is then immediate)
                 (load-immediate temp then res)
                 (setf then temp))
               (dolist (flag flags)
                 (inst cmov flag res then))))))))

(macrolet ((def-move-if (name type reg stack)
             `(define-vop (,name move-if)
                (:args (then :scs (immediate ,@(ensure-list reg) ,stack) :to :eval
                             :load-if (not (or (sc-is then immediate)
                                               (and (sc-is then ,stack)
                                                    (not (location= else res))))))
                       (else :scs (immediate ,@(ensure-list reg) ,stack) :target res
                             :load-if (not (sc-is else immediate ,stack))))
                (:arg-types ,type ,type)
                (:results (res :scs ,(ensure-list reg)))
                (:result-types ,type))))
  (def-move-if move-if/t t (descriptor-reg any-reg) control-stack)
  (def-move-if move-if/unsigned unsigned-num unsigned-reg unsigned-stack)
  (def-move-if move-if/signed signed-num signed-reg signed-stack)
  ;; FIXME: See convert-conditional-move-p above.
  #+sb-unicode
  (def-move-if move-if/char character character-reg character-stack)
  (def-move-if move-if/sap system-area-pointer sap-reg sap-stack))

;;; Return a hint about how to calculate the answer from X,Y and flags.
;;; Return NIL to give up.
(defun computable-from-flags-p (res x y flags)
  ;; TODO: handle unsigned-reg
  (unless (and (singleton-p (conditional-flags-flags flags))
               (sc-is res sb-vm::any-reg sb-vm::descriptor-reg))
    (return-from computable-from-flags-p nil))
  ;; There are plenty more algebraic transforms possible,
  ;; but this picks off some very common cases.
  (flet ((try-shift (x y)
           (and (eql x 0)
                (typep y '(and fixnum unsigned-byte))
                (= (logcount y) 1)
                'shl))
         (try-add (x y) ; commutative
           ;; (signed-byte 32) is gonna work for sure.
           ;; Other things might too, but "perfect is the enemy of good".
           ;; The constant in LEA is pre-fixnumized.
           ;; Post-fixnumizing instead would open up a few more possibilities.
           (and (fixnump x)
                (fixnump y)
                (typep (fixnumize x) '(signed-byte 32))
                (typep (fixnumize y) '(signed-byte 32))
                (member (abs (fixnumize (- x y))) '(2 4 8))
                'add)))
    ;; FIXME: the BOOLEAN case has little benefit, except that converting to
    ;; a CMOV in the general way unnecessarily loads both inputs even when
    ;; one of them is NIL, e.g. (lambda (x) (eql x 1)) becomes
    ;;   CMP RSI, 2
    ;;   MOV RDX, R12 ; <-- this is completely superfluous
    ;;   LEA RAX, [R12-56]
    ;;   CMOVEQ RDX, RAX
    (or (or (and (eq x t) (eq y nil) 'boolean)
            (and (eq x nil) (eq y t) 'boolean))
        (try-shift x y)
        (try-shift y x)
        (try-add x y))))

(define-vop (compute-from-flags)
  (:args (x-tn :scs (immediate constant))
         (y-tn :scs (immediate constant)))
  (:results (res :scs (any-reg descriptor-reg)))
  (:info flags)
  (:generator 3
    (let* ((x (tn-value x-tn))
           (y (tn-value y-tn))
           (hint (computable-from-flags-p res x y flags))
           (flag (car (conditional-flags-flags flags))))
      (ecase hint
        (boolean
         (when (eql x t) (setq flag (negate-condition flag)))
         (load-symbol res t) ; doesn't mess up flags
         (inst cmov flag res null-tn))
        (shl
         (when (eql x 0)
           (setq flag (negate-condition flag)))
         (let ((bit (1- (integer-length (fixnumize (logior x y))))))
           (inst set flag res)
           (inst movzx '(:byte :dword) res res)
           (inst shl (if (> bit 31) :qword :dword) res bit)))
        (add
         (let* ((x (fixnumize x))
                (y (fixnumize y))
                (min (min x y))
                (delta (abs (- x y)))
                ;; [RES+RES+n] encodes more compactly than [RES*2+n]
                (ea (if (= delta 2) (ea min res res) (ea min nil res delta))))
           (when (eql x min)
             (setq flag (negate-condition flag)))
           (inst set flag res)
           (inst movzx '(:byte :dword) res res)
           ;; Retain bit 63... if either is negative
           (inst lea (if (or (minusp x) (minusp y)) :qword :dword) res ea)))))))

;;;; conditional VOPs

;;; Note: a constant-tn is allowed in CMP; it uses an EA displacement,
;;; not immediate data.
(define-vop (if-eq)
  (:args (x :scs (any-reg descriptor-reg control-stack))
         (y :scs (any-reg descriptor-reg control-stack immediate constant)))
  (:conditional :e)
  (:policy :fast-safe)
  (:translate eq)
  (:arg-refs x-tn-ref)
  (:temporary (:sc unsigned-reg) temp) ; TODO: add :unused-if
  (:generator 6
    (cond
      ((sc-is y constant)
       (inst cmp x (cond ((sc-is x descriptor-reg any-reg) y)
                         (t (inst mov temp y) temp))))
      ((and (sc-is y immediate) (nil-relative-p (immediate-tn-repr y)))
       (inst cmp x (move-immediate temp (immediate-tn-repr y))))
      ((sc-is y immediate)
       (let* ((value (encode-value-if-immediate y))
              (immediate (plausible-signed-imm32-operand-p value)))
         (when (and (null (tn-value y)) (tn-ref-type x-tn-ref))
           ;; if the complement of X's type with respect to type NULL can't
           ;; be a cons, then we don't need a 4-byte comparison against NIL.
           ;; It suffices to test the low byte. Similar logic could pertain to many
           ;; other type tests, e.g. STRINGP on known (OR INSTANCE STRING)
           ;; could skip the widetag test.
           ;; I'm starting to wonder if it would be better to expose the lowtag/widetag
           ;; tests in IR1 as an AND expression so that type inference can remove what's
           ;; possible to deduce. The we just need a way to efficiently recombine
           ;; the AND back to one vop where we can. "selection DAG, anyone?"
           (when (not (types-equal-or-intersect
                       (type-difference (tn-ref-type x-tn-ref) (specifier-type 'null))
                       (specifier-type 'cons)))
             (inst cmp :byte x null-tn)
             (return-from if-eq)))
         (cond ((or (fixup-p value) (tn-p value)) ; immobile object or NIL
                (inst cmp x value))
               ((and (zerop value) (sc-is x any-reg descriptor-reg))
                (inst test x x))
               (immediate
                (inst cmp x immediate))
               ((not (sc-is x control-stack))
                (inst cmp x (constantize value)))
               (t
                (inst mov temp value)
                (inst cmp x temp)))))
      ((and (sc-is x control-stack) (sc-is y control-stack))
       (inst mov temp x)
       (inst cmp temp y))
      (t
       (inst cmp x y)))))

;; The template above is a very good fallback for the generic
;; case.  However, it is sometimes possible to perform unboxed
;; comparisons.  Repurpose char= and eql templates here, instead
;; of forcing values to be boxed and then compared.
;;
;; We only weaken EQL => EQ for characters and fixnums, and detect
;; when types definitely mismatch.  No need to import other EQL
;; VOPs (e.g. floats).
(macrolet ((def (eq-name eql-name cost)
             `(define-vop (,eq-name ,eql-name)
                (:translate eq)
                (:variant-cost ,cost))))
  (def fast-if-eq-character fast-char=/character 3)
  (def fast-if-eq-character/c fast-char=/character/c 2)
  (def fast-if-eq-fixnum fast-eql/fixnum 3)
  (def fast-if-eq-fixnum/c fast-eql-c/fixnum 2)
  (def fast-if-eq-signed fast-if-eql/signed 5)
  (def fast-if-eq-signed/c fast-if-eql-c/signed 4)
  (def fast-if-eq-unsigned fast-if-eql/unsigned 5)
  (def fast-if-eq-unsigned/c fast-if-eql-c/unsigned 4))

(define-vop (%instance-ref-eq)
  (:args (instance :scs (descriptor-reg))
         (x :scs (descriptor-reg any-reg)
            :load-if (or (not (sc-is x immediate))
                         (typep (tn-value x)
                                '(and integer
                                  (not (signed-byte #.(- 32 n-fixnum-tag-bits)))))
                         (nil-relative-p (immediate-tn-repr x)))))
  (:arg-types * (:constant (unsigned-byte 16)) *)
  (:info slot)
  (:translate %instance-ref-eq)
  (:conditional :e)
  (:policy :fast-safe)
  (:generator 1
   (inst cmp :qword
         (ea (+ (- instance-pointer-lowtag)
                (ash (+ slot instance-slots-offset) word-shift))
             instance)
         (encode-value-if-immediate x))))

;;; See comment below about ASSUMPTIONS
(eval-when (:compile-toplevel)
  (assert (eql other-pointer-lowtag #b1111))
  ;; This is also assumed in src/runtime/x86-64-assem.S
  (assert (eql (min bignum-widetag ratio-widetag single-float-widetag double-float-widetag
                    complex-rational-widetag complex-single-float-widetag complex-double-float-widetag)
               bignum-widetag))
  (assert (eql (max bignum-widetag ratio-widetag single-float-widetag double-float-widetag
                    complex-rational-widetag complex-single-float-widetag complex-double-float-widetag)
               complex-double-float-widetag)))

;;; Most uses of EQL are transformed into a non-generic form, but when we need
;;; the general form, it's possible to make it nearly as efficient as EQ.
;;; I think it's worth the extra 25 bytes or so per call site versus just
;;; punting to an assembly routine always.
(define-vop (if-eql)
  (:args (x :scs (any-reg descriptor-reg) :target rdi)
         (y :scs (any-reg descriptor-reg) :target rsi))
  (:arg-refs x-ref y-ref)
  (:conditional :e)
  (:policy :fast-safe)
  (:translate eql)
  (:temporary (:sc unsigned-reg :offset rdi-offset :from (:argument 0)) rdi)
  (:temporary (:sc unsigned-reg :offset rsi-offset :from (:argument 1)) rsi)
  (:temporary (:sc unsigned-reg :offset rax-offset) rax)
  (:temporary (:sc unsigned-reg :offset r11-offset) asm-temp)
  (:vop-var vop)
  (:ignore asm-temp)
  (:generator 15
    (inst cmp x y)
    (inst jmp :e done)                  ; affirmative
    (let ((x-ratiop (csubtypep (tn-ref-type x-ref) (specifier-type 'ratio)))
          (y-ratiop (csubtypep (tn-ref-type y-ref) (specifier-type 'ratio)))
          (routine 'generic-eql))
      (cond ((and x-ratiop y-ratiop)
             (move rdi x)
             (move rsi y)
             (invoke-asm-routine 'call 'eql-ratio vop))
            ((or x-ratiop y-ratiop)
             (let ((check (if x-ratiop
                              y
                              x)))
               (%lea-for-lowtag-test rax check other-pointer-lowtag)
               (inst test :byte rax other-pointer-lowtag)
               (inst jmp :ne done)
               (inst cmp :byte (ea -15 check) ratio-widetag)
               (inst jmp :ne done))
             (move rdi x)
             (move rsi y)
             (invoke-asm-routine 'call 'eql-ratio vop))
            (t
             ;; If they are not both OTHER-POINTER objects, return false.
             ;; ASSUMPTION: other-pointer-lowtag = #b1111
             ;; This ANDing trick would be wrong if, e.g., the OTHER-POINTER tag
             ;; were #b0011 and the two inputs had lowtags #b0111 and #b1011
             ;; which when ANDed look like #b0011.
             ;; AND EAX, ESI is more compact than AND AL, SIL
             (inst mov :dword rax x)
             (inst and :dword rax y) ; now AL = #x_F only if both lowtags were #xF
             (inst not :dword rax) ; now AL = #x_0 only if it was #x_F
             (inst test :byte rax #b00001111) ; will be all 0 if ok
             (inst jmp :ne done)              ; negative

             ;; If the widetags are not the same, return false.
             ;; Using a :dword compare gets us the bignum length check almost for free
             ;; unless the length's representation requires more 4 bytes.
             ;; I bet nobody would mind if MAXIMUM-BIGNUM-LENGTH were #xFFFFFF.
             (inst mov :dword rax (ea (- other-pointer-lowtag) x))
             (inst cmp :dword rax (ea (- other-pointer-lowtag) y))
             (inst jmp :ne done)        ; negative
             (unless (or (csubtypep (tn-ref-type x-ref) (specifier-type 'number))
                         (csubtypep (tn-ref-type y-ref) (specifier-type 'number)))
               ;; If not a numeric widetag, return false. See ASSUMPTIONS re widetag order.
               (inst sub :byte rax bignum-widetag)
               (inst cmp :byte rax (- complex-double-float-widetag bignum-widetag))
               (setf routine 'generic-eql*) ;; expects widetag-bignum in AL
               ;; "above" means CF=0 and ZF=0 so we're returning the right thing here
               (inst jmp :a done))
             ;; The hand-written assembly code receives args in the C arg registers.
             ;; Anything else it needs will be callee-saved.
             (move rdi x)               ; load the C call args
             (move rsi y)
             (invoke-asm-routine 'call routine vop))))
    DONE))
