;;;; various array operations that are too expensive (in space) to do
;;;; inline

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;; Fill 'vector' with 'item', unrolling the loop, and taking care
;;; to deal with pre- and post-loop pieces for proper alignment.
;;; Alternatively, if the CPU has the enhanced MOVSB feature, use REP STOS
;;; depending on the number of elements to be written.
(symbol-macrolet
    ((disp (- (ash vector-data-offset word-shift) other-pointer-lowtag))
     (scale (ash 1 (- word-shift n-fixnum-tag-bits)))
     (card-index scratch) ; alias for RSI
     (end-card-index item) ; alias for RAX
     (count end)) ; alias for RCX
(define-assembly-routine (vector-fill/t ; <-- this could work on raw bits too
                          (:translate vector-fill/t)
                          (:policy :fast-safe))
                         ((:arg  vector (descriptor-reg) (:lisp-reg 0))
                          (:arg  item   (any-reg descriptor-reg) rax-offset)
                          (:arg  start  (any-reg descriptor-reg) (:lisp-reg 1))
                          (:arg  end    (any-reg descriptor-reg) rcx-offset)
                          (:res  res    (descriptor-reg) (:lisp-reg 0))
                          (:temp scratch unsigned-reg rbx-offset)
                          ;; storage class doesn't matter since all float regs
                          ;; and sse regs map to the same storage base.
                          (:temp wordpair double-reg 0))
  (move res vector) ; to "use" res

  (inst cmp start end)
  (inst jmp :ge DONE)
  ;; stash ITEM safely away, exactly where we usually need it
  (inst movq wordpair item)
  ;; Mark each GC card of the vector unless ITEM is not a pointer
  ;; (NIL is non-pointer) or the COUNT is 0.
  ;; TODO: Skipping marking whenever VECTOR is younger than ITEM would yield a nice
  ;; savings especially for things like (fill x :EMPTY) because keywords are nearly
  ;; always immortal, at least for any image that was saved to disk. That being so,
  ;; could we hoist marking into the IR1 representation so that transforms can
  ;; decide whether to mark at all, and separate out the actual fill routine?
  #+soft-card-marks
  (assemble ()
    (inst lea :dword card-index (ea -3 item)) ; same as POINTERP (see type-vops)
    (inst test :byte card-index #b11)
    (inst jmp :nz DONE-CARD-MARKING)
    (inst cmp item null-tn)
    (inst jmp :e DONE-CARD-MARKING)

    ;; Compute EA of starting and ending (inclusive) indices
    (inst lea card-index (ea disp vector start scale))
    (inst shr card-index gencgc-card-shift)
    (inst and :dword card-index card-index-mask)
    ;; Compute the modularly post-incremented end-card-mask so that our backward branch
    ;; in the loop can use a not-equal test. (We can't use any branch on inequality
    ;; because modular math screws it up)
    (inst lea end-card-index (ea (- disp n-word-bytes) vector end scale))
    (inst shr end-card-index gencgc-card-shift)
    (inst inc :dword end-card-index)
    (inst and :dword end-card-index card-index-mask)
    LOOP
    ;; Were we to use MARK-GC-CARD which is technically the right abstraction,
    ;; it emits different encodings of the 'disp 'field for #+/-sb-safepoint
    ;; because in one case the disp is imm8 and the other imm32.
    ;; Then we would have an annoyance in the C runtime of having to #ifdef
    ;; the hot-patching of the "JMP UNROLL" below - its address would change.
    (inst mov :byte (ea :disp32 nil-cardtable-disp null-tn card-index) CARD-MARKED)
    (inst inc :dword card-index)
    (inst and :dword card-index card-index-mask)
    (inst cmp :dword card-index end-card-index)
    (inst jmp :ne LOOP))

  DONE-CARD-MARKING
  ;; restore ITEM from its saved location. Needed for the STOS instruction
  ;; and for the final elements after loop unrolling.
  (inst movq item wordpair)
  ;; compute number of elements as a fixnum
  (inst sub count start)
  ;; 'start' is an interior pointer to 'vector',
  ;; but 'vector' is pinned because it's in a register, so this is ok.
  (inst lea start (ea (- (ash vector-data-offset word-shift) other-pointer-lowtag)
                      vector start (ash 1 (- word-shift n-fixnum-tag-bits))))
  ;; REP STOS has a fixed cost that makes it suboptimal below
  ;; a certain fairly high threshold - about 350 objects in my testing.
  (inst cmp count (fixnumize 350))

  ;; *** tune_asm_routines_for_microarch() will replace this unconditional
  ;;     "JMP UNROLL" with "JL UNROLL" after the core file is parsed,
  ;;     if STOS is deemed to be preferable on this cpu.
  ;;     Otherwise we'll always jump over the REP STOS instruction.
  ;;     The preceding CMP is pointless in that case, but harmless.
  (inst jmp unroll)

  (inst shr count n-fixnum-tag-bits)
  (move scratch vector) ; save it for the return value
  (move rdi-tn start) ; implicit operand to STOS
  (inst rep)
  (inst stos :qword)
  (move rdi-tn scratch) ; restore the return value
  DONE
  (inst ret)
  UNROLL
  (inst lea scratch (ea start count scale)) ; compute end pointer
  ;; if address ends in 8, we must write 1 word before using MOVDQA
  (inst test :byte start #b1000)
  (inst jmp :z SETUP)
  (inst mov (ea start) item)
  (inst add start n-word-bytes)
  (inst sub count (fixnumize 1))
  SETUP
  ;; Compute (FLOOR COUNT 8) to get the number of fast iterations.
  ;; We can untagify and divide by 8 in the same operation
  (inst shr count (+ 3 n-fixnum-tag-bits)) ; It's a native integer now.
  ;; For a very small number of elements, the unrolled loop won't execute.
  (inst jmp :z FINISH)
  ;; WORDPAIR already holds ITEM in its low 64 bits
  (inst pshufd wordpair wordpair #b01000100)
  ;; Multiply count by 64 (= 8 lisp objects) and add to 'start'
  ;; to get the upper limit of the loop.
  (inst shl count 6)
  (inst add count start) ; remember: COUNT and END are the same register!
  ;; MOVNTDQ is supposedly faster, but would require a trailing SFENCE
  ;; which measurably harms performance on a small number of iterations.
  UNROLL-LOOP ; Write 4 double-quads = 8 lisp objects
  (inst movdqa (ea  0 start) wordpair)
  (inst movdqa (ea 16 start) wordpair)
  (inst movdqa (ea 32 start) wordpair)
  (inst movdqa (ea 48 start) wordpair)
  (inst add start (* 8 n-word-bytes))
  (inst cmp start end)
  (inst jmp :b UNROLL-LOOP)
  FINISH
  ;; Now we're going to complete the fill with no looping by jumping
  ;; to the middle of a sequence of stores.
  ;; SCRATCH is the ending byte address. Bytes remaining is (SCRATCH - START)
  ;; There are at most 7 more stores to do. Each takes 4 bytes to encode
  ;; including the last one whose EA displacement is 0.
  ;; Therefore divide scratch by 2 to get the number of bytes to execute.
  (inst sub scratch start)
  (inst shr :dword scratch 1)
  ;; Subtract from TAIL to get the entry point without using a lookup table
  (inst lea end (rip-relative-ea TAIL))
  (inst sub end scratch)
  (inst jmp end)
  ;; entry points
  (inst mov (ea 48 start) item)
  (inst mov (ea 40 start) item)
  (inst mov (ea 32 start) item)
  (inst mov (ea 24 start) item)
  (inst mov (ea 16 start) item)
  (inst mov (ea  8 start) item)
  (inst mov (ea :disp8 0 start) item)
  TAIL))

(define-assembly-routine (%data-vector-and-index
                          (:translate %data-vector-and-index)
                          (:policy :fast-safe)
                          (:arg-types t positive-fixnum)
                          (:result-types t positive-fixnum))
    ((:arg array descriptor-reg (:lisp-reg 0))
     (:arg index any-reg (:lisp-reg 1))
     (:temp temp unsigned-reg rcx-offset)
     (:res result descriptor-reg (:lisp-reg 0))
     (:res offset any-reg (:lisp-reg 1)))
  (declare (ignore result offset))
  LOOP
  (inst mov :byte temp (ea (- other-pointer-lowtag) array))

  (inst cmp :byte temp simple-array-widetag)
  (inst jmp :e SKIP)
  (inst cmp :byte temp complex-base-string-widetag)
  (inst jmp :l DONE)
  SKIP

  (inst add index (object-slot-ea array array-displacement-slot other-pointer-lowtag))
  (loadw array array array-data-slot other-pointer-lowtag)
  (inst jmp LOOP)
  DONE)


(define-assembly-routine (%data-vector-and-index/check-bound
                          (:translate %data-vector-and-index/check-bound)
                          (:policy :fast-safe)
                          (:arg-types t positive-fixnum)
                          (:result-types t positive-fixnum)
                          (:save-p :compute-only))
    ((:arg array descriptor-reg (:lisp-reg 0))
     (:arg index any-reg (:lisp-reg 1))
     (:temp temp any-reg rcx-offset)
     (:res result descriptor-reg (:lisp-reg 0))
     (:res offset any-reg (:lisp-reg 1)))
  (declare (ignore result offset))
  (let ((error (generate-error-code nil 'invalid-array-index-error array temp index)))
    (assemble ()
      (inst mov :byte temp (ea (- other-pointer-lowtag) array))
      (inst cmp :byte temp simple-array-widetag)
      (inst jmp :e HEADER)
      (inst cmp :byte temp complex-base-string-widetag)
      (inst jmp :ge HEADER)

      (loadw temp array array-fill-pointer-slot other-pointer-lowtag)
      (inst cmp temp index)
      (inst jmp :be error)
      (inst jmp DONE)

      HEADER
      (loadw temp array array-elements-slot other-pointer-lowtag)
      (inst cmp temp index)
      (inst jmp :be error)

      LOOP
      (inst add index (object-slot-ea array array-displacement-slot other-pointer-lowtag))
      (loadw array array array-data-slot other-pointer-lowtag)

      (inst mov :byte temp (ea (- other-pointer-lowtag) array))
      (inst cmp :byte temp simple-array-widetag)
      (inst jmp :e LOOP)
      (inst cmp :byte temp complex-base-string-widetag)
      (inst jmp :ge LOOP)

      DONE)))

(define-assembly-routine (%data-vector-pop
                          (:translate %data-vector-pop)
                          (:policy :fast-safe)
                          (:arg-types t)
                          (:result-types t positive-fixnum)
                          (:save-p :compute-only)
                          (:check-type t))
    ((:arg array descriptor-reg (:lisp-reg 0))
     (:temp temp any-reg rcx-offset)
     (:res result descriptor-reg (:lisp-reg 0))
     (:res offset any-reg (:lisp-reg 1)))
  (declare (ignore result))
  (let ((error (generate-error-code nil 'fill-pointer-error array)))
    (assemble ()

      (multiple-value-bind (imm8 shift) (header-byte-imm8 (ash +array-fill-pointer-p+ array-flags-data-position))
        (inst test :byte (ea (- (1+ shift) other-pointer-lowtag) array) imm8))
      (inst jmp :z ERROR)

      (loadw offset array array-fill-pointer-slot other-pointer-lowtag)
      (inst test offset offset)
      (inst jmp :z ERROR)
      (inst sub offset (fixnumize 1))
      (storew offset array array-fill-pointer-slot other-pointer-lowtag)


      LOOP
      (inst add offset (object-slot-ea array array-displacement-slot other-pointer-lowtag))
      (loadw array array array-data-slot other-pointer-lowtag)

      (inst mov :byte temp (ea (- other-pointer-lowtag) array))
      (inst cmp :byte temp simple-array-widetag)
      (inst jmp :e LOOP)
      (inst cmp :byte temp complex-base-string-widetag)
      (inst jmp :ge LOOP)

      DONE)))

(define-assembly-routine (%data-vector-push
                          (:translate %data-vector-push)
                          (:policy :fast-safe)
                          (:arg-types t)
                          (:result-types t t)
                          (:save-p :compute-only)
                          (:check-type t))
    ((:arg array descriptor-reg (:lisp-reg 0))
     (:temp temp any-reg rcx-offset)
     (:res result descriptor-reg (:lisp-reg 0))
     (:res offset descriptor-reg (:lisp-reg 1)))
  (declare (ignore result))
  (let ((error (generate-error-code nil 'fill-pointer-error array)))
    (assemble ()
      (multiple-value-bind (imm8 shift) (header-byte-imm8 (ash +array-fill-pointer-p+ array-flags-data-position))
        (inst test :byte (ea (- (1+ shift) other-pointer-lowtag) array) imm8))
      (inst jmp :z ERROR)

      (loadw offset array array-fill-pointer-slot other-pointer-lowtag)
      (loadw temp array array-elements-slot other-pointer-lowtag)
      (inst cmp temp offset)
      (inst jmp :ne SKIP)
      (inst mov offset null-tn)
      (inst ret)

      SKIP
      (inst lea temp (ea (fixnumize 1) offset))
      (storew temp array array-fill-pointer-slot other-pointer-lowtag)

      LOOP
      (inst add offset (object-slot-ea array array-displacement-slot other-pointer-lowtag))
      (loadw array array array-data-slot other-pointer-lowtag)

      (inst mov :byte temp (ea (- other-pointer-lowtag) array))
      (inst cmp :byte temp simple-array-widetag)
      (inst jmp :e LOOP)
      (inst cmp :byte temp complex-base-string-widetag)
      (inst jmp :ge LOOP)

      DONE)))
