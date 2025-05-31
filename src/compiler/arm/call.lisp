;;;; the VM definition of function call for the ARM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(defconstant arg-count-sc (make-sc+offset immediate-arg-scn nargs-offset))
(defconstant closure-sc (make-sc+offset descriptor-reg-sc-number lexenv-offset))

(defconstant return-pc-passing-offset
  (make-sc+offset control-stack-sc-number lra-save-offset))

(defconstant old-fp-passing-offset
  (make-sc+offset control-stack-sc-number ocfp-save-offset))

;;; Make the TNs used to hold OLD-FP and RETURN-PC within the current
;;; function. We treat these specially so that the debugger can find
;;; them at a known location.
(defun make-old-fp-save-location ()
  ;; Unlike the other backends, ARM function calling is designed to
  ;; pass OLD-FP within the stack frame rather than in a register.  As
  ;; such, in order for lifetime analysis not to screw up, we need it
  ;; to be a stack TN wired to the save offset, not a normal TN with a
  ;; wired SAVE-TN.
  (let ((tn (make-wired-tn *fixnum-primitive-type*
                           control-stack-arg-scn
                           ocfp-save-offset)))
    (setf (tn-kind tn) :environment)
    tn))
(defun make-return-pc-save-location ()
  (let ((tn (make-wired-tn *backend-t-primitive-type* control-stack-sc-number
                           lra-save-offset)))
    (setf (tn-kind tn) :environment)
    tn))

;;; Make a TN for the standard argument count passing location.  We
;;; only need to make the standard location, since a count is never
;;; passed when we are using non-standard conventions.
(defun make-arg-count-location ()
  (make-wired-tn *fixnum-primitive-type* immediate-arg-scn nargs-offset))

;;;; Frame hackery:

;;; Return the number of bytes needed for the current non-descriptor
;;; stack frame.
(defun bytes-needed-for-non-descriptor-stack-frame ()
  (logandc2 (+ (* (sb-allocated-size 'non-descriptor-stack) n-word-bytes)
               +number-stack-alignment-mask+)
            +number-stack-alignment-mask+))

;;; Used for setting up the Old-FP in local call.
(define-vop (current-fp)
  (:results (val :scs (any-reg)))
  (:generator 1
    (move val cfp-tn)))

;;; Used for computing the caller's NFP for use in known-values return.  Only
;;; works assuming there is no variable size stuff on the nstack.
(define-vop (compute-old-nfp)
  (:results (val :scs (any-reg)))
  (:vop-var vop)
  (:generator 1
    (let ((nfp (current-nfp-tn vop)))
      (when nfp
        ;; FIXME-ARM: taken form MIPS is this correct? (phs)
        (inst add val nfp (bytes-needed-for-non-descriptor-stack-frame))))))

;;; Accessing a slot from an earlier stack frame is definite hackery.
(define-vop (ancestor-frame-ref)
  (:args (frame-pointer :scs (descriptor-reg))
         (variable-home-tn :load-if nil))
  (:results (value :scs (descriptor-reg any-reg)))
  (:policy :fast-safe)
  (:generator 4
    (aver (sc-is variable-home-tn control-stack))
    (load-stack-offset value frame-pointer variable-home-tn)))

(define-vop (ancestor-frame-set)
  (:args (frame-pointer :scs (descriptor-reg))
         (value :scs (descriptor-reg any-reg)))
  (:results (variable-home-tn :load-if nil))
  (:policy :fast-safe)
  (:generator 4
    (aver (sc-is variable-home-tn control-stack))
    (store-stack-offset value frame-pointer variable-home-tn)))

(define-vop (xep-allocate-frame)
  (:info start-lab)
  ;; KLUDGE: Specify an explicit offset for TEMP because NARGS is a
  ;; non-descriptor-reg, but is also live, yet the register allocator
  ;; does not know that it is, and if TEMP collides NARGS and
  ;; COMPUTE-CODE needs TEMP then we run into trouble very quickly.
  (:temporary (:scs (non-descriptor-reg) :offset ocfp-offset) temp)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 1
    ;; Make sure the function is aligned, and drop a label pointing to this
    ;; function header.
    (emit-alignment n-lowtag-bits)
    (emit-label start-lab)
    ;; Allocate function header.
    (inst simple-fun-header-word)
    (inst .skip (* (1- simple-fun-insts-offset) n-word-bytes))
    (inst compute-code code-tn lip start-lab temp)))

(define-vop (xep-setup-sp)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:vop-var vop)
  (:generator 1
    (composite-immediate-instruction
       add temp cfp-tn
       (* n-word-bytes (sb-allocated-size 'control-stack)))
    (store-csp temp)
    (let ((nfp-tn (current-nfp-tn vop)))
      (when nfp-tn
        (let ((nbytes (bytes-needed-for-non-descriptor-stack-frame)))
          (inst sub nfp-tn nsp-tn nbytes)
          (move nsp-tn nfp-tn))))))

(define-vop (allocate-frame)
  (:results (res :scs (any-reg))
            (nfp :scs (any-reg)))
  (:info callee)
  (:temporary (:sc any-reg) temp)
  (:generator 2
    (load-csp res)
    (composite-immediate-instruction
      add temp res
      (* (max 1 (sb-allocated-size 'control-stack))
        n-word-bytes))
    (store-csp temp)
    (when (ir2-environment-number-stack-p callee)
      (let ((nbytes (bytes-needed-for-non-descriptor-stack-frame)))
        (inst sub nfp nsp-tn nbytes)
        (inst mov nsp-tn nfp)))))

;;; Allocate a partial frame for passing stack arguments in a full call.  Nargs
;;; is the number of arguments passed.  If no stack arguments are passed, then
;;; we don't have to do anything.
(define-vop (allocate-full-call-frame)
  (:info nargs)
  (:results (res :scs (any-reg)))
  (:temporary (:sc any-reg) csp-temp)
  (:generator 2
    ;; Unlike most other backends, we store the "OCFP" at frame
    ;; allocation time rather than at function-entry time, largely due
    ;; to a lack of usable registers.
    (load-csp res)
    ;; Our minimum caller frame size is two words, one for the frame
    ;; link and one for the LRA.
    (composite-immediate-instruction
     add csp-temp res (* (max 2 nargs) n-word-bytes))
    (store-csp csp-temp)
    (storew cfp-tn res ocfp-save-offset)))

;;; Emit code needed at the return-point from an unknown-values call
;;; for a fixed number of values.  VALUES is the head of the TN-REF
;;; list for the locations that the values are to be received into.
;;; NVALS is the number of values that are to be received (should
;;; equal the length of Values).
;;;
;;; MOVE-TEMP is a DESCRIPTOR-REG TN used as a temporary.
;;;
;;; This code exploits the fact that in the unknown-values convention,
;;; a single value return returns with all of the condition flags
;;; clear, whereas a return of other than one value returns with the
;;; condition flags set.
;;;
;;; If 0 or 1 values are expected, then we just emit an instruction to
;;; reset the SP (which will only be executed when other than 1 value
;;; is returned.)
;;;
;;; In the general case, we have to do three things:
;;;  -- Default unsupplied register values.  This need only be done when a
;;;     single value is returned, since register values are defaulted by the
;;;     callee in the non-single case.
;;;  -- Default unsupplied stack values.  This needs to be done whenever there
;;;     are stack values.
;;;  -- Reset SP.  This must be done whenever other than 1 value is returned,
;;;     regardless of the number of values desired.

(defun default-unknown-values (vop values nvals move-temp temp lip lra-label)
  (declare (type (or tn-ref null) values)
           (type unsigned-byte nvals) (type tn move-temp temp))
  (let ((expecting-values-on-stack (> nvals register-arg-count))
        (values-on-stack temp))
    (note-this-location vop (if (<= nvals 1)
                                :single-value-return
                                :unknown-return))
    (inst compute-code code-tn lip lra-label temp)
    ;; Pick off the single-value case first.
    (sb-assem:without-scheduling ()

      ;; Default register values for single-value return case.
      ;; The callee returns with condition bits CLEAR in the
      ;; single-value case.
      (when values
        (do ((i 1 (1+ i))
             (val (tn-ref-across values) (tn-ref-across val)))
            ((= i (min nvals register-arg-count)))
          (inst mov :ne (tn-ref-tn val) null-tn)))

      ;; If we're not expecting values on the stack, all that
      ;; remains is to clear the stack frame (for the multiple-
      ;; value return case).
      (unless expecting-values-on-stack
        (store-csp ocfp-tn :eq))

      ;; If we ARE expecting values on the stack, we need to
      ;; either move them to their result location or to set their
      ;; result location to the default.
      (when expecting-values-on-stack

        ;; For the single-value return case, fake up NARGS and
        ;; OCFP so that we don't screw ourselves with the
        ;; defaulting and stack clearing logic.
        (load-csp ocfp-tn :ne)
        (inst mov :ne nargs-tn n-word-bytes)

        ;; Compute the number of stack values (may be negative if
        ;; not all of the register values are populated).
        (inst sub values-on-stack nargs-tn (fixnumize register-arg-count))

        ;; For each expected stack value...
        (do ((i register-arg-count (1+ i))
             (val (do ((i 0 (1+ i))
                       (val values (tn-ref-across val)))
                      ((= i register-arg-count) val))
                  (tn-ref-across val)))
            ((null val))

          ;; ... Load it if there is a stack value available, or
          ;; default it if there isn't.
          (inst subs values-on-stack values-on-stack 4)
          (loadw move-temp ocfp-tn i 0 :ge)
          (store-stack-tn (tn-ref-tn val) move-temp :ge)
          (store-stack-tn (tn-ref-tn val) null-tn :lt))

        ;; Deallocate the callee stack frame.
        (store-csp ocfp-tn))))
  (values))

;;;; Unknown values receiving:

;;;    Emit code needed at the return point for an unknown-values call for an
;;; arbitrary number of values.
;;;
;;;    We do the single and non-single cases with no shared code: there doesn't
;;; seem to be any potential overlap, and receiving a single value is more
;;; important efficiency-wise.
;;;
;;;    When there is a single value, we just push it on the stack, returning
;;; the old SP and 1.
;;;
;;;    When there is a variable number of values, we move all of the argument
;;; registers onto the stack, and return Args and Nargs.
;;;
;;;    Args and Nargs are TNs wired to the named locations.  We must
;;; explicitly allocate these TNs, since their lifetimes overlap with the
;;; results Start and Count (also, it's nice to be able to target them).
(defun receive-unknown-values (args nargs start count lra-label temp lip)
  (declare (type tn args nargs start count temp))
  (inst compute-code code-tn lip lra-label temp)
  (load-csp nargs :ne)
  (inst add :ne temp nargs n-word-bytes)
  (store-csp temp :ne)
  (inst str :ne (first *register-arg-tns*) (@ nargs))
  (inst mov :ne start nargs)
  (inst mov :ne count (fixnumize 1))
  (do ((arg *register-arg-tns* (rest arg))
       (i 0 (1+ i)))
      ((null arg))
    (storew (first arg) args i 0 :eq))
  (move start args :eq)
  (move count nargs :eq)
  (values))


;;; VOP that can be inherited by unknown values receivers.  The main
;;; thing this handles is allocation of the result temporaries.
(define-vop (unknown-values-receiver)
  (:results
   (start :scs (any-reg))
   (count :scs (any-reg)))
  (:temporary (:sc descriptor-reg :offset ocfp-offset
                   :from :eval :to (:result 0))
              values-start)
  (:temporary (:sc any-reg :offset nargs-offset
               :from :eval :to (:result 1))
              nvals))

;;; This hook in the codegen pass lets us insert code before fall-thru entry
;;; points, local-call entry points, and tail-call entry points.  The default
;;; does nothing.
(defun emit-block-header (start-label trampoline-label fall-thru-p alignp)
  (declare (ignore fall-thru-p alignp))
  (when trampoline-label
    (emit-label trampoline-label))
  (emit-label start-label))


;;;; XEP hackery:

;;; Get the lexical environment from its passing location.
(define-vop (setup-closure-environment)
  (:temporary (:sc descriptor-reg :offset lexenv-offset :target closure
               :to (:result 0))
              lexenv)
  (:results (closure :scs (descriptor-reg)))
  (:info label)
  (:ignore label)
  (:generator 6
    ;; Get result.
    (move closure lexenv)))

;;; Copy a more arg from the argument area to the end of the current frame.
;;; Fixed is the number of non-more arguments.
(define-vop (copy-more-arg)
  ;; The environment here-and-now is not properly initialized.  The
  ;; stack frame is not yet fully allocated, and even if it were most
  ;; of the slots have live data in them that PACK does not know
  ;; about, so we cannot afford a register spill.  As far as the boxed
  ;; registers go, the arg-passing registers (R0, R1, and R2) are
  ;; live, LEXENV is live, and LRA is live.  On the unboxed front,
  ;; NARGS is live.  FP has been set up by the caller, SP is
  ;; protecting our stack arguments, but is otherwise not set up.  NFP
  ;; is not yet set up.  CODE and NULL are set up.  SP and NFP must be
  ;; correctly set up by the time we're done, and OCFP and R8 are
  ;; available for use as temporaries.  If we were any more register
  ;; constrained, we'd be spilling registers manually (rather than
  ;; allowing PACK to do it for us).  -- AJB, 2012-Oct-30
  (:vop-var vop)
  ;; Pack COUNT and DEST into the same register, being careful to tell
  ;; PACK that their lifetimes do not overlap (we're lying to PACK, as
  ;; COUNT is live both before and after DEST, but not while DEST is
  ;; live).
  (:temporary (:sc any-reg :offset ocfp-offset :to :eval) count)
  (:temporary (:sc any-reg :offset ocfp-offset :from :eval) dest)
  (:temporary (:sc descriptor-reg :offset r8-offset) temp)
  (:info fixed)
  (:generator 20
    ;; We open up with a LET to obtain a TN for NFP.  We'll call it
    ;; RESULT, to distinguish it from NFP-as-NFP and to roughly
    ;; parallel the PPC implementation.  We can't use a :TEMPORARY
    ;; here because it would conflict with the existing NFP if there
    ;; is a number-stack frame in play, but we only use it prior to
    ;; actually setting up the "real" NFP.
    (let ((result (make-random-tn (sc-or-lose 'any-reg) nfp-offset)))
      ;; And we use ASSEMBLE here so that we get "implcit labels"
      ;; rather than having to use GEN-LABEL and EMIT-LABEL.
      (assemble ()
        ;; Compute the end of the fixed stack frame (start of the MORE
        ;; arg area) into RESULT.
        (composite-immediate-instruction
         add result cfp-tn (* n-word-bytes (sb-allocated-size 'control-stack)))
        ;; Compute the end of the MORE arg area (and our overall frame
        ;; allocation) into the stack pointer.
        (cond ((zerop fixed)
               (inst cmp nargs-tn 0)
               (inst add dest result nargs-tn)
               (store-csp dest)
               (inst b :eq DONE))
              (t
               (inst subs count nargs-tn (fixnumize fixed))
               (store-csp result :le)
               (inst b :le DONE)
               (inst add dest result count)
               (store-csp dest)))

        (when (< fixed register-arg-count)
          ;; We must stop when we run out of stack args, not when we
          ;; run out of more args.
          (inst add result result (fixnumize (- register-arg-count fixed))))

        ;; We are copying at most (- NARGS FIXED) values, from last to
        ;; first, in order to shift them out of the allocated part of
        ;; the stack frame.  The FIXED values remain where they are,
        ;; as they are part of the allocated stack frame.  Any
        ;; remaining values are being moved to just beyond the end of
        ;; the allocated stack frame, for a distance of (-
        ;; (sb-allocated-size 'control-stack) fixed) words.  There is
        ;; a constant displacement of a single word in the loop below,
        ;; because DEST points to the space AFTER the value being
        ;; moved.

        LOOP
        (let ((delta (- (sb-allocated-size 'control-stack) fixed)))
          (cond ((zerop delta)) ;; nothing to move
                ((plusp delta)  ;; copy backward
                 (inst cmp dest result)
                 (inst ldr :gt temp (@ dest (- (* (1+ delta) n-word-bytes))))
                 (inst str :gt temp (@ dest (- n-word-bytes) :pre-index))
                 (inst b :gt LOOP))
                (t ;; copy forward
                 (inst cmp dest result)
                 (inst ldr :gt temp (@ result (- (* delta n-word-bytes))))
                 (inst str :gt temp (@ result n-word-bytes :post-index))
                 (inst b :gt LOOP))))

        DO-REGS
        (when (< fixed register-arg-count)
          ;; Now we have to deposit any more args that showed up in registers.
          (inst subs count nargs-tn (fixnumize fixed))
          (do ((i fixed (1+ i)))
              ((>= i register-arg-count))
            ;; Don't deposit any more than there are.
            (inst b :eq DONE)
            (inst subs count count (fixnumize 1))
            ;; Store it into the space reserved to it, by displacement
            ;; from the frame pointer.
            (storew (nth i *register-arg-tns*)
                    cfp-tn (+ (sb-allocated-size 'control-stack)
                              (- i fixed)))))
        DONE

        ;; Now that we're done with the &MORE args, we can set up the
        ;; number stack frame.
        (let ((nfp-tn (current-nfp-tn vop)))
          (when nfp-tn
            (composite-immediate-instruction
             sub nfp-tn nsp-tn (bytes-needed-for-non-descriptor-stack-frame))
            (move nsp-tn nfp-tn)))))))

;;; More args are stored consecutively on the stack, starting
;;; immediately at the context pointer.  The context pointer is not
;;; typed, so the lowtag is 0.
(define-vop (more-arg)
  (:translate %more-arg)
  (:policy :fast-safe)
  (:args (context :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:arg-types * tagged-num)
  (:temporary (:scs (any-reg)) temp)
  (:results (value :scs (descriptor-reg any-reg)))
  (:result-types *)
  (:generator 5
    (inst add temp context index)
    (loadw value temp)))

(define-vop (more-arg-or-nil)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:result 1))
         (count :scs (any-reg) :to (:result 1)))
  (:arg-types * tagged-num)
  (:info index)
  (:temporary (:sc unsigned-reg) temp)
  (:results (value :scs (descriptor-reg any-reg)))
  (:result-types *)
  (:generator 3
    (flet ((maybe-load-immediate (x)
             (cond ((encodable-immediate x)
                    x)
                   (t
                    (load-immediate-word temp x)
                    temp))))
      (inst mov value null-tn)
      (inst cmp count (maybe-load-immediate (fixnumize index)))
      (inst b :le done)
      (inst ldr value
            (@ object
               (maybe-load-immediate (ash index word-shift)))))
    done))

;;; Turn more arg (context, count) into a list.
(define-vop ()
  (:args (context-arg :target context :scs (descriptor-reg))
         (count-arg :target count :scs (any-reg)))
  (:arg-types * tagged-num)
  (:temporary (:scs (any-reg) :from (:argument 0)) context)
  (:temporary (:scs (any-reg) :from (:argument 1)) count)
  (:temporary (:scs (descriptor-reg) :from :eval) temp)
  (:temporary (:scs (any-reg) :from :eval) dst)
  (:temporary (:sc non-descriptor-reg :offset ocfp-offset) pa-flag)
  (:results (result :scs (descriptor-reg)))
  (:translate %listify-rest-args)
  (:policy :safe)
  (:node-var node)
  (:generator 20
    (move context context-arg)
    (move count count-arg)
    ;; Check to see if there are any arguments.
    (inst cmp count 0)
    (move result null-tn)
    (inst b :eq DONE)

    ;; We need to do this atomically.
    (pseudo-atomic (pa-flag)
      ;; Allocate a cons (2 words) for each item.
      (let* ((dx-p (node-stack-allocate-p node))
             (size (cond (dx-p
                          (lsl count 1))
                         (t
                          (inst mov temp (lsl count 1))
                          temp))))
        (allocation 'list size list-pointer-lowtag dst
                    :flag-tn pa-flag
                    :stack-allocate-p dx-p))
      (move result dst)
      ;; FIXME: This entire loop is based on the PPC version, which is
      ;; a poor fit for the ARM instruction set.
      (inst b ENTER)

      ;; Compute the next cons and store it in the current one.
      LOOP
      (inst add dst dst (* 2 n-word-bytes))
      (storew dst dst -1 list-pointer-lowtag)

      ;; Grab one value.
      ENTER
      (loadw temp context)
      (inst add context context n-word-bytes)

      ;; Dec count, and if != zero, go back for more.
      (inst subs count count (fixnumize 1))
      ;; Store the value into the car of the current cons.
      (storew temp dst 0 list-pointer-lowtag)
      (inst b :gt LOOP)

      ;; NIL out the last cons.
      (storew null-tn dst 1 list-pointer-lowtag))
    DONE))

;;; Return the location and size of the more arg glob created by
;;; Copy-More-Arg.  Supplied is the total number of arguments supplied
;;; (originally passed in NARGS.)  Fixed is the number of non-rest
;;; arguments.
;;;
;;; We must duplicate some of the work done by Copy-More-Arg, since at
;;; that time the environment is in a pretty brain-damaged state,
;;; preventing this info from being returned as values.  What we do is
;;; compute supplied - fixed, and return a pointer that many words
;;; below the current stack top.
(define-vop ()
  (:policy :fast-safe)
  (:translate sb-c::%more-arg-context)
  (:args (supplied :scs (any-reg)))
  (:arg-types tagged-num (:constant fixnum))
  (:info fixed)
  (:results (context :scs (descriptor-reg))
            (count :scs (any-reg)))
  (:result-types t tagged-num)
  (:note "more-arg-context")
  (:generator 5
    (inst sub count supplied (fixnumize fixed))
    (load-csp context)
    (inst sub context context count)))

(define-vop (verify-arg-count)
  (:policy :fast-safe)
  (:args (nargs :scs (any-reg)))
  (:arg-types positive-fixnum (:constant t) (:constant t))
  (:temporary (:sc unsigned-reg :offset nl2-offset) temp)
  (:info min max)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 3
    (let ((err-lab
           (generate-error-code vop 'invalid-arg-count-error nargs)))
      (flet ((maybe-load-immediate (x)
               (let ((x (fixnumize x)))
                (cond ((encodable-immediate x)
                       x)
                      (t
                       (load-immediate-word temp x)
                       temp)))))
       (cond ((not min)
              (inst cmp nargs (maybe-load-immediate max))
              (inst b :ne err-lab))
             (max
              (when (plusp min)
                (inst cmp nargs (maybe-load-immediate min))
                (inst b :lo err-lab))
              (inst cmp nargs (maybe-load-immediate max))
              (inst b :hi err-lab))
             ((plusp min)
              (inst cmp nargs (maybe-load-immediate min))
              (inst b :lo err-lab)))))))

;;;; Local call with unknown values convention return:

;;; Non-TR local call for a fixed number of values passed according to the
;;; unknown values convention.
;;;
;;; Args are the argument passing locations, which are specified only to
;;; terminate their lifetimes in the caller.
;;;
;;; Values are the return value locations (wired to the standard passing
;;; locations).
;;;
;;; Save is the save info, which we can ignore since saving has been done.
;;; Return-PC is the TN that the return PC should be passed in.
;;; Target is a continuation pointing to the start of the called function.
;;; Nvals is the number of values received.
;;;
;;; Note: we can't use normal load-tn allocation for the fixed args, since all
;;; registers may be tied up by the more operand.  Instead, we use
;;; MAYBE-LOAD-STACK-TN.
(define-vop (call-local)
  (:args (fp)
         (nfp)
         (args :more t))
  (:results (values :more t))
  (:save-p t)
  (:move-args :local-call)
  (:info arg-locs callee target nvals)
  (:vop-var vop)
  (:temporary (:scs (descriptor-reg) :from (:eval 0)) move-temp)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:temporary (:sc any-reg :offset ocfp-offset :from (:eval 0)) ocfp)
  (:temporary (:scs (interior-reg)) lip)
  (:ignore arg-locs args ocfp)
  (:generator 5
    (let ((label (gen-label))
          (cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (store-stack-tn nfp-save cur-nfp))
      (let ((callee-nfp (callee-nfp-tn callee)))
        (when callee-nfp
          (maybe-load-stack-tn callee-nfp nfp)))
      (maybe-load-stack-tn cfp-tn fp)
      (inst compute-lra lip lip label)
      (store-stack-tn (callee-return-pc-tn callee) lip)
      (note-this-location vop :call-site)
      (inst b target)
      (emit-return-pc label)
      (default-unknown-values vop values nvals move-temp temp lip label)
      ;; alpha uses (maybe-load-stack-nfp-tn cur-nfp nfp-save temp)
      ;; instead of the clause below
      (when cur-nfp
        (load-stack-tn cur-nfp nfp-save)))))


;;; Non-TR local call for a variable number of return values passed according
;;; to the unknown values convention.  The results are the start of the values
;;; glob and the number of values received.
;;;
;;; Note: we can't use normal load-tn allocation for the fixed args, since all
;;; registers may be tied up by the more operand.  Instead, we use
;;; MAYBE-LOAD-STACK-TN.
(define-vop (multiple-call-local unknown-values-receiver)
  (:args (fp)
         (nfp)
         (args :more t))
  (:save-p t)
  (:move-args :local-call)
  (:info save callee target)
  (:ignore args save)
  (:vop-var vop)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 20
    (let ((label (gen-label))
          (cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (store-stack-tn nfp-save cur-nfp))
      (let ((callee-nfp (callee-nfp-tn callee)))
        ;; alpha doesn't test this before the maybe-load
        (when callee-nfp
          (maybe-load-stack-tn callee-nfp nfp)))
      (maybe-load-stack-tn cfp-tn fp)
      (inst compute-lra lip lip label)
      (store-stack-tn (callee-return-pc-tn callee) lip)
      (note-this-location vop :call-site)
      (inst b target)
      (emit-return-pc label)
      (note-this-location vop :unknown-return)
      (receive-unknown-values values-start nvals start count label temp lip)
      (when cur-nfp
        (load-stack-tn cur-nfp nfp-save)))))

;;;; Local call with known values return:

;;; Non-TR local call with known return locations.  Known-value return works
;;; just like argument passing in local call.
;;;
;;; Note: we can't use normal load-tn allocation for the fixed args, since all
;;; registers may be tied up by the more operand.  Instead, we use
;;; MAYBE-LOAD-STACK-TN.
(define-vop (known-call-local)
  (:args (fp)
         (nfp)
         (args :more t))
  (:results (res :more t))
  (:move-args :local-call)
  (:save-p t)
  (:info save callee target)
  (:ignore args res save)
  (:vop-var vop)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 5
    (let ((label (gen-label))
          (cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (store-stack-tn nfp-save cur-nfp))
      (let ((callee-nfp (callee-nfp-tn callee)))
        (when callee-nfp
          (maybe-load-stack-tn callee-nfp nfp)))
      (maybe-load-stack-tn cfp-tn fp)
      (inst compute-lra lip lip label)
      (store-stack-tn (callee-return-pc-tn callee) lip)
      (note-this-location vop :call-site)
      (inst b target)
      (emit-return-pc label)
      (note-this-location vop :known-return)
      (when cur-nfp
        (load-stack-tn cur-nfp nfp-save)))))

;;; Return from known values call.  We receive the return locations as
;;; arguments to terminate their lifetimes in the returning function.  We
;;; restore FP and CSP and jump to the Return-PC.
;;;
;;; Note: we can't use normal load-tn allocation for the fixed args, since all
;;; registers may be tied up by the more operand.  Instead, we use
;;; MAYBE-LOAD-STACK-TN.
(define-vop (known-return)
  (:args (old-fp :target old-fp-temp)
         (return-pc :target return-pc-temp)
         (vals :more t))
  (:temporary (:sc any-reg :from (:argument 0)) old-fp-temp)
  (:temporary (:sc descriptor-reg :from (:argument 1)) return-pc-temp)
  (:move-args :known-return)
  (:info val-locs)
  (:ignore val-locs vals)
  (:vop-var vop)
  (:generator 6
    (maybe-load-stack-tn old-fp-temp old-fp)
    (maybe-load-stack-tn return-pc-temp return-pc)
    (store-csp cfp-tn)
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (inst add cur-nfp cur-nfp (bytes-needed-for-non-descriptor-stack-frame))
        (move nsp-tn cur-nfp)))
    (move cfp-tn old-fp-temp)
    (lisp-return return-pc-temp :known)))

;;;; Full call:
;;;
;;; There is something of a cross-product effect with full calls.
;;; Different versions are used depending on whether we know the
;;; number of arguments or the name of the called function, and
;;; whether we want fixed values, unknown values, or a tail call.
;;;
;;; In full call, the arguments are passed creating a partial frame on
;;; the stack top and storing stack arguments into that frame.  On
;;; entry to the callee, this partial frame is pointed to by FP.  If
;;; there are no stack arguments, we don't bother allocating a partial
;;; frame, and instead set FP to SP just before the call.

;;; This macro helps in the definition of full call VOPs by avoiding code
;;; replication in defining the cross-product VOPs.
;;;
;;; Name is the name of the VOP to define.
;;;
;;; Named is true if the first argument is a symbol whose global function
;;; definition is to be called.
;;;
;;; Return is either :Fixed, :Unknown or :Tail:
;;; -- If :Fixed, then the call is for a fixed number of values, returned in
;;;    the standard passing locations (passed as result operands).
;;; -- If :Unknown, then the result values are pushed on the stack, and the
;;;    result values are specified by the Start and Count as in the
;;;    unknown-values continuation representation.
;;; -- If :Tail, then do a tail-recursive call.  No values are returned.
;;;    The Old-Fp and Return-PC are passed as the second and third arguments.
;;;
;;; In non-tail calls, the pointer to the stack arguments is passed as the last
;;; fixed argument.  If Variable is false, then the passing locations are
;;; passed as a more arg.  Variable is true if there are a variable number of
;;; arguments passed on the stack.  Variable cannot be specified with :Tail
;;; return.  TR variable argument call is implemented separately.
;;;
;;; In tail call with fixed arguments, the passing locations are passed as a
;;; more arg, but there is no new-FP, since the arguments have been set up in
;;; the current frame.
(defmacro define-full-call (name named return variable)
  (aver (not (and variable (eq return :tail))))
  `(define-vop (,name
                ,@(when (eq return :unknown)
                    '(unknown-values-receiver)))
     (:args
      ,@(unless (eq return :tail)
          '((new-fp :scs (any-reg) :to :eval)))
      ,@(case named
         ((nil)
          '((arg-fun :target lexenv)))
         (:direct)
         (t
          '((name :target name-pass))))

      ,@(when (eq return :tail)
          '((old-fp)
            (return-pc)))

      ,@(unless variable '((args :more t :scs (descriptor-reg control-stack)))))

     ,@(when (eq return :fixed)
         '((:results (values :more t))))

     (:save-p ,(if (eq return :tail) :compute-only t))

     ,@(unless (or (eq return :tail) variable)
         '((:move-args :full-call)))

     (:vop-var vop)
     (:info ,@(unless (or variable (eq return :tail)) '(arg-locs))
            ,@(unless variable '(nargs))
            ,@(when (eq named :direct) '(fun))
            ,@(when (eq return :fixed) '(nvals))
            step-instrumenting)

     (:ignore
      ,@(when (eq return :fixed) '(ocfp-temp))
      ,@(unless (or variable (eq return :tail)) '(arg-locs))
      ,@(unless variable '(args))
      ,@(when (eq return :tail) '(old-fp)))

     ,@(unless (eq named :direct)
         `((:temporary (:sc descriptor-reg :offset lexenv-offset
                        :from (:argument ,(if (eq return :tail) 0 1))
                        :to :eval)
                       ,(if named 'name-pass 'lexenv))))

     (:temporary (:scs (descriptor-reg) :from (:argument 0) :to :eval)
                 function)
     (:temporary (:sc any-reg :offset nargs-offset :to :eval)
                 nargs-pass)

     ,@(when variable
         (mapcar #'(lambda (name offset)
                     `(:temporary (:sc descriptor-reg
                                   :offset ,offset
                                   :to :eval)
                         ,name))
                 register-arg-names *register-arg-offsets*))
     ,@(when (eq return :fixed)
         '((:temporary (:scs (descriptor-reg) :from :eval) move-temp)
           (:temporary (:sc non-descriptor-reg :from :eval :offset ocfp-offset) ocfp-temp)))

     ,@(unless (eq return :tail)
         '((:temporary (:scs (non-descriptor-reg)) temp)
           (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)))

     (:temporary (:scs (interior-reg)) lip)

     (:generator ,(+ (if named 5 0)
                     (if variable 19 1)
                     (if (eq return :tail) 0 10)
                     15
                     (if (eq return :unknown) 25 0))
       (let* ((cur-nfp (current-nfp-tn vop))
              ,@(unless (eq return :tail)
                  '((lra-label (gen-label))))
              (filler
               (remove nil
                       (list :load-nargs
                             ,@(if (eq return :tail)
                                   '((unless (location= return-pc
                                                        (make-random-tn (sc-or-lose 'control-stack) lra-save-offset))
                                       :load-return-pc)
                                     (when cur-nfp
                                       :frob-nfp))
                                   '(:comp-lra
                                     (when cur-nfp
                                       :frob-nfp)
                                     :load-fp))))))
         (flet ((do-next-filler ()
                  (let* ((next (pop filler))
                         (what (if (consp next) (car next) next)))
                    (ecase what
                      (:load-nargs
                       ,@(if variable
                             `((load-csp nargs-pass)
                               ;; The variable args are on the stack
                               ;; and become the frame, but there may
                               ;; be <3 args and 2 stack slots are
                               ;; assumed allocate on the call. So
                               ;; need to ensure there are at least 2
                               ;; slots. This just adds 2 more.
                               (inst add r0 nargs-pass (* 2 n-word-bytes))
                               (inst sub nargs-pass nargs-pass new-fp)
                               (store-csp r0)
                               ,@(let ((index -1))
                                   (mapcar #'(lambda (name)
                                               `(loadw ,name new-fp
                                                    ,(incf index)))
                                           register-arg-names))
                               (storew cfp-tn new-fp ocfp-save-offset))
                             '((load-immediate-word nargs-pass (fixnumize nargs)))))
                      ,@(if (eq return :tail)
                            '((:load-return-pc
                               (error "RETURN-PC not in its passing location"))
                              (:frob-nfp
                               (inst add cur-nfp cur-nfp (bytes-needed-for-non-descriptor-stack-frame))
                               (move nsp-tn cur-nfp)))
                            `((:comp-lra
                               (inst compute-lra lip lip lra-label)
                               (inst str lip (@ new-fp (* lra-save-offset
                                                          n-word-bytes))))
                              (:frob-nfp
                               (store-stack-tn nfp-save cur-nfp))
                              (:load-fp
                               (move cfp-tn new-fp))))
                      ((nil)))))
                (insert-step-instrumenting (callable-tn)
                  ;; Conditionally insert a conditional trap:
                  (when step-instrumenting
                    (assemble ()
                      ;; Get the symbol-value of SB-IMPL::*STEPPING*
                      ;; KLUDGE: ... into LIP.  Either it's zero or it
                      ;; isn't, and even taking a stray interrupt and
                      ;; GC can't screw that up.
                      (load-symbol-value lip sb-impl::*stepping*)
                      (inst cmp lip 0)
                      ;; If it's not 0, trap.
                      (inst b :eq step-done-label)
                      ;; CONTEXT-PC will be pointing here when the
                      ;; interrupt is handled, not after the
                      ;; DEBUG-TRAP.
                      (note-this-location vop :internal-error)
                      ;; Best-guess at a usable trap.  x86oids don't
                      ;; have much more than this, SPARC, MIPS, PPC
                      ;; and HPPA encode (TN-OFFSET CALLABLE-TN),
                      ;; Alpha ignores stepping entirely.
                      (inst debug-trap)
                      (inst byte single-step-around-trap)
                      (inst byte (tn-offset callable-tn))
                      (emit-alignment word-shift)

                      STEP-DONE-LABEL))))
           (declare (ignorable #'insert-step-instrumenting))
           ,@(case named
               ((t)
                `((sc-case name
                    (descriptor-reg (move name-pass name))
                    (control-stack
                     (load-stack-tn name-pass name)
                     (do-next-filler))
                    (constant
                     (load-constant vop name name-pass)
                     (do-next-filler)))
                  (insert-step-instrumenting name-pass)
                  (loadw function name-pass fdefn-raw-addr-slot
                      other-pointer-lowtag)
                  (do-next-filler)))
               ((nil)
                `((sc-case arg-fun
                    (descriptor-reg (move lexenv arg-fun))
                    (control-stack
                     (load-stack-tn lexenv arg-fun)
                     (do-next-filler))
                    (constant
                     (load-constant vop arg-fun lexenv)
                     (do-next-filler)))
                  (loadw function lexenv closure-fun-slot
                      fun-pointer-lowtag)
                  (do-next-filler)
                  (insert-step-instrumenting function)))
               (:direct
                `((inst ldr function (@ null-tn (static-fun-offset fun))))))
           (loop
             (if filler
                 (do-next-filler)
                 (return)))

           (note-this-location vop :call-site)
           (lisp-jump function))

         ,@(ecase return
             (:fixed
              '((emit-return-pc lra-label)
                (default-unknown-values vop values nvals move-temp
                                        temp lip lra-label)
                (when cur-nfp
                  (load-stack-tn cur-nfp nfp-save))))
             (:unknown
              '((emit-return-pc lra-label)
                (note-this-location vop :unknown-return)
                (receive-unknown-values values-start nvals start count
                                        lra-label temp lip)
                (when cur-nfp
                  (load-stack-tn cur-nfp nfp-save))))
             (:tail))))))

(define-full-call call nil :fixed nil)
(define-full-call call-named t :fixed nil)
(define-full-call static-call-named :direct :fixed nil)
(define-full-call multiple-call nil :unknown nil)
(define-full-call multiple-call-named t :unknown nil)
(define-full-call static-multiple-call-named :direct :unknown nil)
(define-full-call tail-call nil :tail nil)
(define-full-call tail-call-named t :tail nil)
(define-full-call static-tail-call-named :direct :tail nil)

(define-full-call call-variable nil :fixed t)
(define-full-call multiple-call-variable nil :unknown t)

;;; Defined separately, since needs special code that BLT's the
;;; arguments down.
(define-vop (tail-call-variable)
  (:args
   (args-arg :scs (any-reg) :target args)
   (function-arg :scs (descriptor-reg) :target lexenv)
   (old-fp-arg :scs (any-reg) :load-if nil)
   (lra-arg :scs (descriptor-reg) :load-if nil))
  (:temporary (:sc any-reg :offset nl2-offset :from (:argument 0)) args)
  (:temporary (:sc any-reg :offset lexenv-offset :from (:argument 1)) lexenv)
  (:temporary (:sc interior-reg) lip)
  (:ignore old-fp-arg lra-arg)
  (:vop-var vop)
  (:generator 75
    ;; Move these into the passing locations if they are not already there.
    (move args args-arg)
    (move lexenv function-arg)
    ;; Clear the number stack if anything is there.
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (inst add cur-nfp cur-nfp (bytes-needed-for-non-descriptor-stack-frame))
        (move nsp-tn cur-nfp)))
    (let ((fixup-lab (gen-label)))
      (assemble (:elsewhere)
        (emit-label fixup-lab)
        (inst word (make-fixup 'tail-call-variable :assembly-routine)))
      (inst load-from-label pc-tn lip fixup-lab))))

;;;; Unknown values return:

;;; Return a single value using the unknown-values convention.
(define-vop (return-single)
  (:args (old-fp :scs (any-reg) :to :eval)
         (return-pc :scs (descriptor-reg))
         (value))
  (:ignore value)
  (:vop-var vop)
  (:generator 6
    ;; Clear the number stack.
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (inst add cur-nfp cur-nfp (bytes-needed-for-non-descriptor-stack-frame))
        (move nsp-tn cur-nfp)))
    ;; Clear the control stack, and restore the frame pointer.
    (store-csp cfp-tn)
    (move cfp-tn old-fp)
    ;; Out of here.
    (lisp-return return-pc :single-value)))

;;; Do unknown-values return of a fixed number of values.  The Values are
;;; required to be set up in the standard passing locations.  Nvals is the
;;; number of values returned.
;;;
;;; If returning a single value, then deallocate the current frame, restore
;;; FP and jump to the single-value entry at Return-PC + 8.
;;;
;;; If returning other than one value, then load the number of values returned,
;;; NIL out unsupplied values registers, restore FP and return at Return-PC.
;;; When there are stack values, we must initialize the argument pointer to
;;; point to the beginning of the values block (which is the beginning of the
;;; current frame.)
(define-vop (return)
  (:args
   (old-fp :scs (any-reg))
   (return-pc :scs (descriptor-reg) :to (:eval 1) :target lra)
   (values :more t))
  (:ignore values)
  (:info nvals)
  (:temporary (:sc descriptor-reg :offset r0-offset :from (:eval 0)) r0)
  (:temporary (:sc descriptor-reg :offset r1-offset :from (:eval 0)) r1)
  (:temporary (:sc descriptor-reg :offset r2-offset :from (:eval 0)) r2)
  (:temporary (:sc descriptor-reg :offset lexenv-offset :from (:eval 1)) lra)
  (:temporary (:sc any-reg :offset nargs-offset) nargs)
  (:temporary (:sc any-reg :offset ocfp-offset) val-ptr)
  (:vop-var vop)
  (:generator 6
    (move lra return-pc)
    ;; Clear the number stack.
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (composite-immediate-instruction
         add cur-nfp cur-nfp
         (bytes-needed-for-non-descriptor-stack-frame))
        (move nsp-tn cur-nfp)))
    (cond ((= nvals 1)
           ;; Clear the control stack, and restore the frame pointer.
           (store-csp cfp-tn)
           (move cfp-tn old-fp)
           ;; Out of here.
           (lisp-return lra :single-value))
          (t
           ;; Establish the values pointer.
           (move val-ptr cfp-tn)
           ;; restore the frame pointer and clear as much of the control
           ;; stack as possible.
           (move cfp-tn old-fp)
           (composite-immediate-instruction
            add nargs val-ptr (* nvals n-word-bytes))
           (store-csp nargs)
           ;; Establish the values count.
           (load-immediate-word nargs (fixnumize nvals))
           ;; pre-default any argument register that need it.
           (when (< nvals register-arg-count)
             (dolist (reg (subseq (list r0 r1 r2) nvals))
               (move reg null-tn)))
           ;; And away we go.
           (lisp-return lra :multiple-values)))))

;;; Do unknown-values return of an arbitrary number of values (passed
;;; on the stack.)  We check for the common case of a single return
;;; value, and do that inline using the normal single value return
;;; convention.  Otherwise, we branch off to code that calls an
;;; assembly-routine.
(define-vop (return-multiple)
  (:args
   (old-fp-arg :scs (any-reg) :to (:eval 1))
   (lra-arg :scs (descriptor-reg) :to (:eval 1))
   (vals-arg :scs (any-reg) :target vals)
   (nvals-arg :scs (any-reg) :target nvals))
  (:temporary (:sc any-reg :offset nl2-offset :from (:argument 0)) old-fp)
  (:temporary (:sc descriptor-reg :offset lexenv-offset :from (:argument 1)) lra)
  (:temporary (:sc any-reg :offset ocfp-offset :from (:argument 2)) vals)
  (:temporary (:sc any-reg :offset nargs-offset :from (:argument 3)) nvals)
  (:temporary (:sc descriptor-reg :offset r0-offset) r0)
  (:vop-var vop)
  (:generator 13
    (move lra lra-arg)
    ;; Clear the number stack.
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (composite-immediate-instruction
         add cur-nfp cur-nfp
         (bytes-needed-for-non-descriptor-stack-frame))
        (move nsp-tn cur-nfp)))

    ;; Check for the single case.
    (inst cmp nvals-arg (fixnumize 1))
    (inst b :ne NOT-SINGLE)

    ;; Return with one value.
    (inst ldr r0 (@ vals-arg))
    (store-csp cfp-tn)
    (move cfp-tn old-fp-arg)
    (lisp-return lra-arg :single-value)

    ;; Nope, not the single case.
    NOT-SINGLE
    (move old-fp old-fp-arg)
    (move vals vals-arg)
    (move nvals nvals-arg)
    (inst ldr pc-tn (@ fixup))
    FIXUP
    (inst word (make-fixup 'return-multiple :assembly-routine))))

;;; Single-stepping

(define-vop (step-instrument-before-vop)
  (:temporary (:scs (descriptor-reg)) stepping)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 3
    (load-symbol-value stepping sb-impl::*stepping*)
    ;; If it's not zero, trap.
    (inst cmp stepping 0)
    (inst b :eq DONE)
    ;; CONTEXT-PC will be pointing here when the interrupt is handled,
    ;; not after the BREAK.
    (note-this-location vop :internal-error)
    ;; A best-guess effort at a debug trap suitable for a
    ;; single-step-before-trap.
    (inst debug-trap)
    (inst byte single-step-before-trap)
    (emit-alignment word-shift)
    DONE))
