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
  (:generator 0
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
                (inst jmp flag dest))))))

(define-vop (multiway-branch-if-eq)
  ;; TODO: also accept signed-reg, unsigned-reg, character-reg
  ;; also, could probably tighten up the TN lifetime to avoid a move
  (:args (x :scs (any-reg descriptor-reg)))
  (:info labels otherwise key-type keys test-vop-name)
  (:temporary (:sc unsigned-reg) table)
  (:args-var x-tn-ref)
  (:generator 10
    (let* ((key-derived-type (tn-ref-type x-tn-ref))
           (ea)
           (min (car keys)) ; keys are sorted
           (max (car (last keys)))
           (vector (make-array (1+ (- max min)) :initial-element otherwise))
           ;; This fixnumize won't overflow because ir2opt won't use
           ;; a multiway-branch unless all keys are char or fixnum.
           ;; But what if MIN is MOST-NEGATIVE-FIXNUM ????
           (-min (fixnumize (- min))))
      (mapc (lambda (key label) (setf (aref vector (- key min)) label))
            keys labels)
      (ecase key-type
        (fixnum
           (cond
             ((and (typep (* min (- sb-vm:n-word-bytes)) '(signed-byte 32))
                   (typep key-derived-type 'numeric-type)
                   (csubtypep key-derived-type (specifier-type 'fixnum))
                   ;; There could be some dead code if the ranges don't line up.
                   (>= (numeric-type-low key-derived-type) min)
                   (<= (numeric-type-high key-derived-type) max))
              (setq ea (ea (* min (- sb-vm:n-word-bytes)) table x 4)))
             (t
              ;; First exclude out-of-bounds values because there's no harm
              ;; in doing that up front regardless of the argument's lisp type.
              (typecase -min
                ;; TODO: if min is 0, use X directly, don't move into temp-reg-tn
                ((eql 0) (move temp-reg-tn x))
                ((signed-byte 32) (inst lea temp-reg-tn (ea -min x)))
                (t (inst mov temp-reg-tn x)
                   (inst add :qword temp-reg-tn (constantize -min))))
              (inst cmp temp-reg-tn (constantize (fixnumize (- max min))))
              (inst jmp :a otherwise)
              ;; We have to check the type here because a chain of EQ tests
              ;; does not impose a type constraint.
              ;; If type of X was derived as fixnum, then elide this test.
              (unless (eq test-vop-name 'sb-vm::fast-if-eq-fixnum/c)
                (inst test :byte x fixnum-tag-mask)
                (inst jmp :ne otherwise))
              (setq ea (ea table temp-reg-tn 4))))
            (inst lea table (register-inline-constant :jump-table vector))
            (inst jmp ea))
        (character
           ;; Same as above, but test the widetag before shifting it out.
           (unless (member test-vop-name '(fast-char=/character/c
                                           fast-if-eq-character/c))
             (inst cmp :byte x character-widetag)
             (inst jmp :ne otherwise))
           (inst mov :dword temp-reg-tn x)
           (inst shr :dword temp-reg-tn n-widetag-bits)
           (unless (= min 0)
             (inst sub :dword temp-reg-tn min))
           (inst cmp temp-reg-tn (- max min))
           (inst jmp :a otherwise)
           (inst lea table (register-inline-constant :jump-table vector))
           (inst jmp (ea table temp-reg-tn 8)))))))

(define-load-time-global *cmov-ptype-representation-vop*
  (mapcan (lambda (entry)
            (destructuring-bind (ptypes &optional sc vop)
                entry
              (mapcar (if (and vop sc)
                          (lambda (ptype)
                            (list ptype sc vop))
                          #'list)
                      (ensure-list ptypes))))
          '((t descriptor-reg move-if/t)

            ((fixnum positive-fixnum)
             any-reg move-if/fx)
            ((unsigned-byte-64 unsigned-byte-63)
             unsigned-reg move-if/unsigned)
            (signed-byte-64 signed-reg move-if/signed)
            ;; FIXME: Can't use CMOV with byte registers, and characters live
            ;; in such outside of unicode builds. A better solution then just
            ;; disabling MOVE-IF/CHAR should be possible, though.
            #+sb-unicode
            (character character-reg move-if/char)

            ((single-float complex-single-float
              double-float complex-double-float))

            (system-area-pointer sap-reg move-if/sap)))
  "Alist of primitive type -> (storage-class-name VOP-name)
   if values of such a type should be cmoved, and NIL otherwise.

   storage-class-name is the name of the storage class to use for
   the values, and VOP-name the name of the VOP that will be used
   to execute the conditional move.")

(defun convert-conditional-move-p (node dst-tn x-tn y-tn)
  (declare (ignore node))
  (let* ((ptype (sb-c::tn-primitive-type dst-tn))
         (name  (sb-c:primitive-type-name ptype))
         (param (cdr (or (assoc name *cmov-ptype-representation-vop*)
                         '(t descriptor-reg move-if/t)))))
    (when param
      (destructuring-bind (representation vop) param
        (let ((scn (sc-number-or-lose representation)))
          (labels ((make-tn ()
                     (make-representation-tn ptype scn))
                   (frob-tn (tn)
                     ;; Careful not to load constants which require boxing
                     ;; and may overwrite the flags.
                     ;; Representation selection should avoid that.
                     (if (eq (tn-kind tn) :constant)
                         tn
                         (make-tn))))
            (values vop
                    (frob-tn x-tn) (frob-tn y-tn)
                    (make-tn)
                    nil)))))))

(define-vop (move-if)
  (:args (then) (else))
  (:results (res))
  (:info flags)
  (:generator 0
     (let ((not-p (eq (first flags) 'not)))
       (when not-p (pop flags))
       (flet ((load-immediate (dst constant-tn
                               &optional (sc (sc-name (tn-sc dst))))
                ;; Can't use ZEROIZE, since XOR will affect the flags.
                (inst mov dst
                      (encode-value-if-immediate constant-tn
                                                 (memq sc '(any-reg descriptor-reg))))))
         (cond ((null (rest flags))
                (if (sc-is else immediate)
                    (load-immediate res else)
                    (move res else))
                (when (sc-is then immediate)
                  (load-immediate temp-reg-tn then (sc-name (tn-sc res)))
                  (setf then temp-reg-tn))
                (inst cmov (if not-p
                               (negate-condition (first flags))
                               (first flags))
                      res
                      then))
               (not-p
                (cond ((sc-is then immediate)
                       (when (location= else res)
                         (inst mov temp-reg-tn else)
                         (setf else temp-reg-tn))
                       (load-immediate res then))
                      ((location= else res)
                       (inst xchg else then)
                       (rotatef else then))
                      (t
                       (move res then)))
                (when (sc-is else immediate)
                  (load-immediate temp-reg-tn else (sc-name (tn-sc res)))
                  (setf else temp-reg-tn))
                (dolist (flag flags)
                  (inst cmov flag res else)))
               (t
                (if (sc-is else immediate)
                    (load-immediate res else)
                    (move res else))
                (when (sc-is then immediate)
                  (load-immediate temp-reg-tn then (sc-name (tn-sc res)))
                  (setf then temp-reg-tn))
                (dolist (flag flags)
                  (inst cmov flag res then))))))))

(macrolet ((def-move-if (name type reg stack)
             `(define-vop (,name move-if)
                (:args (then :scs (immediate ,reg ,stack) :to :eval
                             :load-if (not (or (sc-is then immediate)
                                               (and (sc-is then ,stack)
                                                    (not (location= else res))))))
                       (else :scs (immediate ,reg ,stack) :target res
                             :load-if (not (sc-is else immediate ,stack))))
                (:arg-types ,type ,type)
                (:results (res :scs (,reg)
                               :from (:argument 1)))
                (:result-types ,type))))
  (def-move-if move-if/t t descriptor-reg control-stack)
  (def-move-if move-if/fx tagged-num any-reg control-stack)
  (def-move-if move-if/unsigned unsigned-num unsigned-reg unsigned-stack)
  (def-move-if move-if/signed signed-num signed-reg signed-stack)
  ;; FIXME: See *CMOV-PTYPE-REPRESENTATION-VOP* above.
  #+sb-unicode
  (def-move-if move-if/char character character-reg character-stack)
  (def-move-if move-if/sap system-area-pointer sap-reg sap-stack))

;;; Return a hint about how to calculate the answer from X,Y and flags.
;;; Return NIL to give up.
(defun computable-from-flags-p (res x y flags)
  ;; TODO: handle unsigned-reg
  (unless (and (singleton-p flags)
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
    (or #+sb-thread (or (and (eq x t) (eq y nil) 'boolean)
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
           (flag (car flags)))
      (ecase hint
        (boolean
         ;; FIXNUMP -> {T,NIL} could be special-cased, reducing the instruction count by
         ;; 1 or 2 depending on whether the argument and result are in the same register.
         ;; Best case would be "AND :dword res, arg, 1 ; MOV res, [ea]".
         (when (eql x t)
           ;; T is at the lower address, so to pick it out we need index=0
           ;; which makes the condition in (IF BIT T NIL) often flipped.
           (setq flag (negate-condition flag)))
         (inst set res flag)
         (inst movzx '(:byte :dword) res res)
         (inst mov :dword res
               (ea (ash thread-t-nil-constants-slot word-shift) thread-base-tn res 4)))
        (shl
         (when (eql x 0)
           (setq flag (negate-condition flag)))
         (let ((bit (1- (integer-length (fixnumize (logior x y))))))
           (inst set res flag)
           (inst movzx '(:byte :dword) res res)
           (inst shl (if (> bit 31) :qword :dword) res bit)))
        (add
         (let* ((x (fixnumize x))
                (y (fixnumize y))
                (min (min x y))
                (delta (abs (- x y)))
                (ea (ea min nil res delta)))
           (when (eql x min)
             (setq flag (negate-condition flag)))
           (inst set res flag)
           (inst movzx '(:byte :dword) res res)
           ;; Retain bit 63... if either is negative
           (inst lea (if (or (minusp x) (minusp y)) :qword :dword) res ea)))))))

;;;; conditional VOPs

;;; Note: a constant-tn is allowed in CMP; it uses an EA displacement,
;;; not immediate data.
(define-vop (if-eq)
  (:args (x :scs (any-reg descriptor-reg control-stack))
         (y :scs (any-reg descriptor-reg immediate)
            :load-if (and (sc-is x control-stack)
                          (not (sc-is y any-reg descriptor-reg immediate)))))
  (:conditional :e)
  (:policy :fast-safe)
  (:translate eq)
  (:args-var x-tn-ref)
  (:generator 6
    (cond
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
             (inst cmp :byte x (logand nil-value #xff))
             (return-from if-eq)))
         (cond ((fixup-p value) ; immobile object
                (inst cmp x value))
               ((and (zerop value) (sc-is x any-reg descriptor-reg))
                (inst test x x))
               (immediate
                (inst cmp x immediate))
               ((not (sc-is x control-stack))
                (inst cmp x (constantize value)))
               (t
                (inst mov temp-reg-tn value)
                (inst cmp x temp-reg-tn)))))
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
                                  (not (signed-byte #.(- 32 n-fixnum-tag-bits))))))))
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
                    complex-widetag complex-single-float-widetag complex-double-float-widetag)
               bignum-widetag))
  (assert (eql (max bignum-widetag ratio-widetag single-float-widetag double-float-widetag
                    complex-widetag complex-single-float-widetag complex-double-float-widetag)
               complex-double-float-widetag)))

;;; Most uses of EQL are transformed into a non-generic form, but when we need
;;; the general form, it's possible to make it nearly as efficient as EQ.
;;; I think it's worth the extra 25 bytes or so per call site versus just
;;; punting to an assembly routine always.
(define-vop (if-eql)
  (:args (x :scs (any-reg descriptor-reg) :target rdi)
         (y :scs (any-reg descriptor-reg) :target rsi))
  (:conditional :e)
  (:policy :fast-safe)
  (:translate eql)
  (:temporary (:sc unsigned-reg :offset rdi-offset :from (:argument 0)) rdi)
  (:temporary (:sc unsigned-reg :offset rsi-offset :from (:argument 1)) rsi)
  (:temporary (:sc unsigned-reg :offset rax-offset) rax)
  (:vop-var vop)
  (:generator 15
    (inst cmp x y)
    (inst jmp :e done) ; affirmative

    ;; If they are not both OTHER-POINTER objects, return false.
    ;; ASSUMPTION: other-pointer-lowtag = #b1111
    ;; This ANDing trick would be wrong if, e.g., the OTHER-POINTER tag
    ;; were #b0011 and the two inputs had lowtags #b0111 and #b1011
    ;; which when ANDed look like #b0011.
    ;; We use :BYTE rather than :DWORD here because byte-sized
    ;; operations on the accumulator encode more compactly.
    (inst mov :byte rax x)
    (inst and :byte rax y) ; now AL = #x_F only if both lowtags were #xF
    (inst not :byte rax)   ; now AL = #x_0 only if it was #x_F
    (inst and :byte rax #b00001111) ; will be all 0 if ok
    (inst jmp :ne done) ; negative

    ;; If the widetags are not the same, return false.
    ;; Using a :dword compare gets us the bignum length check almost for free
    ;; unless the length's representation requires more than 3 bytes.
    ;; It sounds like a :qword compare would be the right thing, but remember
    ;; one header bit acts as a concurrent GC mark bit in all headered objects,
    ;; though we're not really using it yet. (We are, but not concurrently)
    (inst mov :dword rax (ea (- other-pointer-lowtag) x))
    (inst cmp :dword rax (ea (- other-pointer-lowtag) y))
    (inst jmp :ne done) ; negative

    ;; If not a numeric widetag, return false. See ASSUMPTIONS re widetag order.
    (inst sub :byte rax bignum-widetag)
    (inst cmp :byte rax (- complex-double-float-widetag bignum-widetag))
    ;; "above" means CF=0 and ZF=0 so we're returning the right thing here
    (inst jmp :a done)

    ;; The hand-written assembly code receives args in the C arg registers.
    ;; It also receives AL holding the biased down widetag.
    ;; Anything else it needs will be callee-saved.
    (move rdi x) ; load the C call args
    (move rsi y)
    (invoke-asm-routine 'call 'generic-eql vop)
    DONE))
