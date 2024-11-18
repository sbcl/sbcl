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
(define-assembly-routine (vector-fill/t ; <-- this could work on raw bits too
                          (:translate vector-fill/t)
                          (:policy :fast-safe))
                         ((:arg  vector (descriptor-reg) rdx-offset)
                          (:arg  item   (any-reg descriptor-reg) rax-offset)
                          (:arg  start  (any-reg descriptor-reg) rdi-offset)
                          (:arg  end    (any-reg descriptor-reg) rsi-offset)
                          (:res  res    (descriptor-reg) rdx-offset)
                          (:temp count unsigned-reg rcx-offset)
                          (:temp end-card-index unsigned-reg rbx-offset)
                          ;; storage class doesn't matter since all float regs
                          ;; and sse regs map to the same storage base.
                          (:temp wordpair double-reg float0-offset))
  (progn end-card-index)
  (move res vector) ; to "use" res

  ;; Mark each GC card of the vector unless ITEM is not a pointer
  ;; (NIL is non-pointer) or the COUNT is 0.
  #+soft-card-marks
  (progn
  (inst cmp start end)
  (inst jmp :ge DONE)
  (inst lea :dword count (ea -3 item)) ; same as POINTERP (see type-vops)
  (inst test :byte count #b11)
  (inst jmp :nz DONE-CARD-MARKING)
  (inst cmp item nil-value)
  (inst jmp :e DONE-CARD-MARKING)

  (let ((disp (- (ash vector-data-offset word-shift) other-pointer-lowtag))
        (card-index count)
        (loop (gen-label)))
    ;; Compute EA of starting and ending (inclusive) indices
    (inst lea card-index (ea disp vector start (ash 1 (- word-shift n-fixnum-tag-bits))))
    (inst lea end-card-index (ea (- disp n-word-bytes)
                                 vector end (ash 1 (- word-shift n-fixnum-tag-bits))))
    (inst shr card-index gencgc-card-shift)
    (inst shr end-card-index gencgc-card-shift)
    (inst and :dword card-index card-index-mask)
    (inst and :dword end-card-index card-index-mask)
    (emit-label LOOP)
    (inst mov :byte (ea gc-card-table-reg-tn card-index) CARD-MARKED) ; mark one card
    (inst cmp card-index end-card-index)
    (inst jmp :e DONE-CARD-MARKING)
    (inst inc :dword card-index)
    (inst and :dword card-index card-index-mask)
    (inst jmp LOOP)))

  DONE-CARD-MARKING
  (move count end)
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
  (inst rep)
  (inst stos :qword)
  DONE
  (inst ret)
  UNROLL
  (inst test count count)
  (inst jmp :z DONE)
  ;; if address ends in 8, we must write 1 word before using MOVDQA
  (inst test :byte start #b1000)
  (inst jmp :z SETUP)
  (inst mov (ea start) item)
  (inst add start n-word-bytes)
  (inst sub count (fixnumize 1))
  SETUP
  ;; Compute (FLOOR COUNT 8) to compute the number of fast iterations.
  (inst shr count (+ 3 n-fixnum-tag-bits)) ; It's a native integer now.
  ;; For a very small number of elements, the unrolled loop won't execute.
  (inst jmp :z FINISH)
  ;; Load the xmm register.
  (inst movq wordpair item)
  (inst pshufd wordpair wordpair #b01000100)
  ;; Multiply count by 64 (= 8 lisp objects) and add to 'start'
  ;; to get the upper limit of the loop.
  (inst shl count 6)
  (inst add count start)
  ;; MOVNTDQ is supposedly faster, but would require a trailing SFENCE
  ;; which measurably harms performance on a small number of iterations.
  UNROLL-LOOP ; Write 4 double-quads = 8 lisp objects
  (inst movdqa (ea  0 start) wordpair)
  (inst movdqa (ea 16 start) wordpair)
  (inst movdqa (ea 32 start) wordpair)
  (inst movdqa (ea 48 start) wordpair)
  (inst add start (* 8 n-word-bytes))
  (inst cmp start count)
  (inst jmp :b UNROLL-LOOP)
  FINISH
  ;; Now recompute 'count' as the ending address
  (inst lea count (ea (- (ash vector-data-offset word-shift) other-pointer-lowtag)
                      vector
                      end (ash 1 (- word-shift n-fixnum-tag-bits))))
  (inst cmp start count)
  (inst jmp :ae DONE)
  FINAL-LOOP
  (inst mov (ea start) item)
  (inst add start n-word-bytes)
  (inst cmp start count)
  (inst jmp :b FINAL-LOOP))

(define-assembly-routine (%data-vector-and-index
                          (:translate %data-vector-and-index)
                          (:policy :fast-safe)
                          (:arg-types t positive-fixnum)
                          (:result-types t positive-fixnum))
    ((:arg array descriptor-reg rdx-offset)
     (:arg index any-reg rdi-offset)
     (:temp temp unsigned-reg rcx-offset)
     (:res result descriptor-reg rdx-offset)
     (:res offset any-reg rdi-offset))
  (declare (ignore result offset))
  LOOP
  (inst mov :byte temp (ea (- other-pointer-lowtag) array))

  (inst cmp :byte temp simple-array-widetag)
  (inst jmp :eq SKIP)
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
                          (:result-types t positive-fixnum))
    ((:arg array descriptor-reg rdx-offset)
     (:arg index any-reg rdi-offset)
     (:temp temp any-reg rcx-offset)
     (:res result descriptor-reg rdx-offset)
     (:res offset any-reg rdi-offset))
  (declare (ignore result offset))
  (let ((error
          (assemble (:elsewhere)
            error
            ;; Fake up a stack frame so that backtraces come out right.
            (inst push rbp-tn)
            (inst mov rbp-tn rsp-tn)
            (emit-error-break nil error-trap
                              (error-number-or-lose 'invalid-array-index-error)
                              (list array temp index))
            (progn error))))
    (assemble ()
      (inst mov :byte temp (ea (- other-pointer-lowtag) array))
      (inst cmp :byte temp simple-array-widetag)
      (inst jmp :eq HEADER)
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
      (inst jmp :eq LOOP)
      (inst cmp :byte temp complex-base-string-widetag)
      (inst jmp :ge LOOP)

      DONE)))

(define-assembly-routine (%data-vector-pop
                          (:translate %data-vector-pop)
                          (:policy :fast-safe)
                          (:arg-types t)
                          (:result-types t positive-fixnum))
    ((:arg array descriptor-reg rdx-offset)
     (:temp temp any-reg rcx-offset)
     (:res result descriptor-reg rdx-offset)
     (:res offset any-reg rdi-offset))
  (declare (ignore result))
  (let ((error
          (assemble (:elsewhere)
            error
            ;; Fake up a stack frame so that backtraces come out right.
            (inst push rbp-tn)
            (inst mov rbp-tn rsp-tn)
            (emit-error-break nil error-trap
                              (error-number-or-lose 'fill-pointer-error)
                              (list array))
            (progn error))))
    (assemble ()
      (inst mov :dword temp (ea (- other-pointer-lowtag) array))
      (inst cmp :byte temp complex-base-string-widetag)
      (inst jmp :l ERROR)

      (inst test :word temp (ash sb-vm:+array-fill-pointer-p+
                                 sb-vm:array-flags-data-position))
      (inst jmp :nz ERROR)

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
      (inst jmp :eq LOOP)
      (inst cmp :byte temp complex-base-string-widetag)
      (inst jmp :ge LOOP)

      DONE)))

(define-assembly-routine (%data-vector-push
                          (:translate %data-vector-push)
                          (:policy :fast-safe)
                          (:arg-types t)
                          (:result-types t t))
    ((:arg array descriptor-reg rdx-offset)
     (:temp temp any-reg rcx-offset)
     (:res result descriptor-reg rdx-offset)
     (:res offset descriptor-reg rdi-offset))
  (declare (ignore result))
  (let ((error
          (assemble (:elsewhere)
            error
            ;; Fake up a stack frame so that backtraces come out right.
            (inst push rbp-tn)
            (inst mov rbp-tn rsp-tn)
            (emit-error-break nil error-trap
                              (error-number-or-lose 'fill-pointer-error)
                              (list array))
            (progn error))))
    (assemble ()
      (inst mov :dword temp (ea (- other-pointer-lowtag) array))
      (inst cmp :byte temp complex-base-string-widetag)
      (inst jmp :l ERROR)

      (inst test :word temp (ash sb-vm:+array-fill-pointer-p+
                                 sb-vm:array-flags-data-position))
      (inst jmp :nz ERROR)

      (loadw offset array array-fill-pointer-slot other-pointer-lowtag)
      (loadw temp array array-elements-slot other-pointer-lowtag)
      (inst cmp temp offset)
      (inst jmp :ne SKIP)
      (inst mov offset nil-value)
      (inst ret)

      SKIP
      (inst lea temp (ea (fixnumize 1) offset))
      (storew temp array array-fill-pointer-slot other-pointer-lowtag)


      LOOP
      (inst add offset (object-slot-ea array array-displacement-slot other-pointer-lowtag))
      (loadw array array array-data-slot other-pointer-lowtag)

      (inst mov :byte temp (ea (- other-pointer-lowtag) array))
      (inst cmp :byte temp simple-array-widetag)
      (inst jmp :eq LOOP)
      (inst cmp :byte temp complex-base-string-widetag)
      (inst jmp :ge LOOP)

      DONE)))
