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
  (:temporary (:sc non-descriptor-reg :offset lr-offset) lr)
  (:info start-lab)
  (:generator 1
    ;; Make sure the function is aligned, and drop a label pointing to this
    ;; function header.
    (emit-alignment n-lowtag-bits)
    (emit-label start-lab)
    ;; Allocate function header.
    (inst simple-fun-header-word)
    (inst .skip (* (1- simple-fun-insts-offset) n-word-bytes))
    (inst str lr (@ cfp-tn (* lra-save-offset n-word-bytes)))))

(define-vop (xep-setup-sp)
  (:vop-var vop)
  (:generator 1
    (inst add csp-tn cfp-tn
          (add-sub-immediate (* n-word-bytes (sb-allocated-size 'control-stack))))
    (let ((nfp-tn (current-nfp-tn vop)))
      (when nfp-tn
        (let ((nbytes (bytes-needed-for-non-descriptor-stack-frame)))
          (inst sub nfp-tn nsp-tn nbytes)
          (inst mov-sp nsp-tn nfp-tn))))))

(define-vop (allocate-frame)
  (:results (res :scs (any-reg))
            (nfp :scs (any-reg)))
  (:info callee)
  (:generator 2
    (move res csp-tn)
    (let ((size (add-sub-immediate (* (max 1 (sb-allocated-size 'control-stack)) n-word-bytes))))
      (cond ((typep size '(signed-byte 9))
             (inst str cfp-tn (@ csp-tn size :post-index)))
            (t
             (inst add csp-tn csp-tn size)
             (storew cfp-tn res ocfp-save-offset))))
    (when (ir2-environment-number-stack-p callee)
      (inst sub nfp nsp-tn (add-sub-immediate
                            (bytes-needed-for-non-descriptor-stack-frame)))
      (inst mov-sp nsp-tn nfp))))

;;; Allocate a partial frame for passing stack arguments in a full call.  Nargs
;;; is the number of arguments passed.  If no stack arguments are passed, then
;;; we don't have to do anything.
;;; LR and CFP are always saved on the stack, but it's safe to have two words above CSP.
(define-vop (allocate-full-call-frame)
  (:info nargs)
  (:results (res :scs (any-reg)))
  (:generator 2
    (if (<= nargs register-arg-count)
        ;; Don't touch RES, the call vops would use CSP-TN in this case.
        (storew cfp-tn csp-tn ocfp-save-offset)
        (let ((size (add-sub-immediate (* nargs n-word-bytes))))
          (move res csp-tn)
          (cond ((typep size '(signed-byte 9))
                 (inst str cfp-tn (@ csp-tn size :post-index)))
                (t
                 (inst add csp-tn csp-tn size)
                 (storew cfp-tn res ocfp-save-offset)))))))

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

(defun default-unknown-values (vop values nvals move-temp node)
  (declare (type (or tn-ref null) values)
           (type unsigned-byte nvals) (type tn move-temp))
  (multiple-value-bind (type name leaf) (sb-c::lvar-fun-type (sb-c::basic-combination-fun node))
    (let* ((verify (and leaf
                        (policy node (and (>= safety 1)
                                          (= debug 3)))
                        (memq (sb-c::leaf-where-from leaf) '(:declared-verify :defined-here))))
           (type (if verify
                     (if (fun-type-p type)
                         (fun-type-returns type)
                         *wild-type*)
                     (sb-c::node-derived-type node)))
           (min-values (values-type-min-value-count type))
           (max-values (values-type-max-value-count type))
           (trust (or (and (= min-values 0)
                           (= max-values call-arguments-limit))
                      (not verify)))
           (expecting-values-on-stack (> nvals register-arg-count)))

      (note-this-location vop (if (<= nvals 1)
                                  :single-value-return
                                  :unknown-return))
      (flet ((check-nargs ()
               (assemble ()
                 (let* ((*location-context* (list* name
                                                   (type-specifier type)
                                                   (make-restart-location SKIP)))
                        (err-lab (generate-error-code vop 'invalid-arg-count-error))
                        (min min-values)
                        (max (and (< max-values call-arguments-limit)
                                  max-values)))
                   (labels ((load-immediate (x)
                              (add-sub-immediate (fixnumize x))))
                     (cond ((eql max 0)
                            (inst cbnz nargs-tn err-lab))
                           ((eql min max)
                            (inst cmp nargs-tn (load-immediate max))
                            (inst b :ne err-lab))
                           (max
                            (let ((nargs tmp-tn))
                              (if (zerop min)
                                  (setf nargs nargs-tn)
                                  (inst sub tmp-tn nargs-tn (load-immediate min)))
                              (inst cmp nargs (load-immediate (- max min))))
                            (inst b :hi err-lab))
                           (t
                            (cond ((= min 1)
                                   (inst cbz nargs-tn err-lab))
                                  ((plusp min)
                                   (inst cmp nargs-tn (load-immediate min))
                                   (inst b :lo err-lab)))))))
                 SKIP)))
        ;; Pick off the single-value case first.
        (assemble ()
          (sb-assem:without-scheduling ()

            ;; Default register values for single-value return case.
            ;; The callee returns with condition bits CLEAR in the
            ;; single-value case.
            (when values
              (do ((i 1 (1+ i))
                   (val (tn-ref-across values) (tn-ref-across val)))
                  ((= i (min nvals register-arg-count)))
                (unless (eq (tn-kind (tn-ref-tn val)) :unused)
                  (cond
                    ((and trust
                          (> min-values i)))
                    (t
                     (inst csel (tn-ref-tn val) null-tn (tn-ref-tn val) :ne))))))

            ;; If we're not expecting values on the stack, all that
            ;; remains is to clear the stack frame (for the multiple-
            ;; value return case).
            (unless (or expecting-values-on-stack
                        (and trust
                             (type-single-value-p type)))
              (cond ((or (not trust)
                         (values-type-may-be-single-value-p type))
                     (inst csel csp-tn ocfp-tn csp-tn :eq)
                     (unless trust
                       (inst mov tmp-tn (fixnumize 1))
                       (inst csel nargs-tn tmp-tn nargs-tn :ne)
                       (check-nargs)))
                    ((eq type *empty-type*))
                    (t
                     (inst mov csp-tn ocfp-tn))))
            (macrolet ((map-stack-values (&body body)
                         `(do ((i register-arg-count (1+ i))
                               (val (do ((i 0 (1+ i))
                                         (val values (tn-ref-across val)))
                                        ((= i register-arg-count) val))
                                    (tn-ref-across val)))
                              ((null val))
                            (let ((tn (tn-ref-tn val)))
                              ,@body))))
              ;; If we ARE expecting values on the stack, we need to
              ;; either move them to their result location or to set their
              ;; result location to the default.
              (when expecting-values-on-stack
                (let ((decrement (fixnumize (1+ register-arg-count)))
                      (stack-targets-p (map-stack-values
                                        (when (and (>= i min-values)
                                                   (neq (tn-kind tn) :unused)
                                                   (sc-is tn control-stack))
                                          (return t)))))
                  ;; If all destinations are registers move NIL into all
                  ;; of them before checking for single value return, that
                  ;; way it doesn't need to set up NARGS and OCFP.
                  (unless stack-targets-p
                    (map-stack-values
                     (when (and (>= i min-values)
                                (neq (tn-kind tn) :unused))
                       (inst mov tn null-tn))))
                  (cond ((and trust
                              (> min-values 1)))
                        ((or (not trust)
                             stack-targets-p)
                         (inst csel ocfp-tn csp-tn ocfp-tn :ne)
                         (inst mov tmp-tn (fixnumize 1))
                         (inst csel nargs-tn tmp-tn nargs-tn :ne)
                         (unless trust
                           (check-nargs)))
                        (t
                         (inst b :ne DONE)))
                  (map-stack-values
                   (cond ((eq (tn-kind tn) :unused)
                          (incf decrement (fixnumize 1)))
                         ((< i min-values)
                          (incf decrement (fixnumize 1))
                          (sc-case tn
                            (control-stack
                             (let* ((next (and (< (1+ i) min-values)
                                               (tn-ref-across val)))
                                    (next-tn (and next
                                                  (tn-ref-tn next))))
                               (cond ((and next-tn
                                           (not (sc-is next-tn control-stack))
                                           (neq (tn-kind next-tn) :unused)
                                           (ldp-stp-offset-p (* i n-word-bytes) n-word-bits))
                                      (inst ldp move-temp next-tn
                                            (@ ocfp-tn (* i n-word-bytes)))
                                      (store-stack-tn tn move-temp)
                                      (setf val next)
                                      (incf i)
                                      (incf decrement (fixnumize 1)))
                                     (t
                                      (loadw move-temp ocfp-tn i)
                                      (store-stack-tn tn move-temp)))))
                            (t
                             (let* ((next (and (< (1+ i) min-values)
                                               (tn-ref-across val)))
                                    (next-tn (and next
                                                  (tn-ref-tn next))))
                               (cond ((and next-tn
                                           (neq (tn-kind next-tn) :unused)
                                           (ldp-stp-offset-p (* i n-word-bytes) n-word-bits))
                                      (let ((stack (sc-is next-tn control-stack)))
                                        (inst ldp tn (if stack
                                                         move-temp
                                                         next-tn)
                                              (@ ocfp-tn (* i n-word-bytes)))
                                        (when stack
                                          (store-stack-tn next-tn move-temp)))
                                      (setf val next)
                                      (incf i)
                                      (incf decrement (fixnumize 1)))
                                     (t
                                      (loadw tn ocfp-tn i)))))))
                         (t
                          (let ((dst move-temp))
                            (assemble ()
                              ;; ... Load it if there is a stack value available, or
                              ;; default it if there isn't.
                              (inst subs nargs-tn nargs-tn decrement)
                              (setf decrement (fixnumize 1))
                              (unless (sc-is tn control-stack)
                                (setf dst tn))
                              (when stack-targets-p
                                (move dst null-tn))
                              (inst b :lt NONE)
                              (loadw dst ocfp-tn i)
                              NONE
                              (when (sc-is tn control-stack)
                                (store-stack-tn tn dst))))))))
                ;; Deallocate the callee stack frame.
                (move csp-tn ocfp-tn))))
          DONE))))
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
(defun receive-unknown-values (node args nargs start count)
  (declare (type tn args nargs start count))
  (let ((unused-count-p (eq (tn-kind count) :unused))
        (unused-start-p (eq (tn-kind start) :unused))
        (type (sb-c::node-derived-type node)))
    (if (type-single-value-p type)
        (assemble ()
          (unless unused-start-p
            (move start csp-tn))
          (unless unused-count-p
            (inst mov count (fixnumize 1)))
          (inst str (first *register-arg-tns*) (@ csp-tn n-word-bytes :post-index)))
        (assemble ()
          (inst b :eq MULTIPLE)
          (unless unused-start-p
            (move start csp-tn))
          (inst str (first *register-arg-tns*) (@ csp-tn n-word-bytes :post-index))
          (unless unused-count-p
            (inst mov count (fixnumize 1)))
          (inst b DONE)
          MULTIPLE
          #.(assert (evenp register-arg-count))
          (do ((arg *register-arg-tns* (cddr arg))
               (i 0 (+ i 2)))
              ((null arg))
            (inst stp (first arg) (second arg)
                  (@ args (* i n-word-bytes))))
          (unless unused-start-p
            (move start args))
          (unless unused-count-p
            (move count nargs))
          DONE))))

;;; VOP that can be inherited by unknown values receivers.  The main
;;; thing this handles is allocation of the result temporaries.
(define-vop (unknown-values-receiver)
  (:results
   (start :scs (any-reg))
   (count :scs (any-reg)))
  (:temporary (:sc any-reg :offset ocfp-offset :from :result) values-start)
  (:temporary (:sc any-reg :offset nargs-offset :from :result) nvals)
  ;; Avoid being clobbered by RECEIVE-UNKNOWN-VALUES
  (:temporary (:sc descriptor-reg :offset r0-offset :from :result) r0-temp))

;;; This hook in the codegen pass lets us insert code before fall-thru entry
;;; points, local-call entry points, and tail-call entry points.  The default
;;; does nothing.
(defun emit-block-header (start-label trampoline-label fall-thru-p alignp)
  (declare (ignore alignp))
  (when (and fall-thru-p trampoline-label)
    (inst b start-label))
  (when trampoline-label
    (emit-label trampoline-label)
    (inst str lr-tn (@ cfp-tn (* lra-save-offset n-word-bytes))))
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
  ;; correctly set up by the time we're done, and OCFP and R9 are
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
  (:temporary (:sc descriptor-reg :offset r9-offset) temp)
  (:info fixed)
  (:generator 20
    ;; We open up with a LET to obtain a TN for NFP.  We'll call it
    ;; RESULT, to distinguish it from NFP-as-NFP and to roughly
    ;; parallel the PPC implementation.  We can't use a :TEMPORARY
    ;; here because it would conflict with the existing NFP if there
    ;; is a number-stack frame in play, but we only use it prior to
    ;; actually setting up the "real" NFP.
    (let ((result (make-random-tn :kind :normal
                                  :sc (sc-or-lose 'any-reg)
                                  :offset nfp-offset))
          (delta (- (sb-allocated-size 'control-stack) fixed)))
      (assemble ()
        ;; Compute the end of the fixed stack frame (start of the MORE
        ;; arg area) into RESULT.
        (inst add result cfp-tn (add-sub-immediate
                                 (* n-word-bytes (sb-allocated-size 'control-stack))))
        ;; Compute the end of the MORE arg area (and our overall frame
        ;; allocation) into the stack pointer.
        (cond ((zerop fixed)
               (inst add dest result (lsl nargs-tn (- word-shift n-fixnum-tag-bits)))
               (move csp-tn dest)
               (inst cbz nargs-tn done))
              (t
               (inst subs count nargs-tn (fixnumize fixed))
               (inst csel csp-tn result csp-tn :le)
               (inst b :le DONE)
               (inst add dest result (lsl count (- word-shift n-fixnum-tag-bits)))
               ;; Don't leave the arguments unprotected when moving below the stack pointer
               (when (>= delta 0)
                 (move csp-tn dest))))

        (when (< fixed register-arg-count)
          ;; We must stop when we run out of stack args, not when we
          ;; run out of more args.
          (inst add result result (* (- register-arg-count fixed) n-word-bytes)))

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
        (cond ((zerop delta)) ;; nothing to move
              ((plusp delta)   ;; copy backward
               (inst cmp dest result)
               (inst b :le DO-REGS)
               (inst ldr temp (@ dest (load-store-offset
                                       (- (* (1+ delta) n-word-bytes)))))
               (inst str temp (@ dest (- n-word-bytes) :pre-index))
               (inst b LOOP))
              (t ;; copy forward
               (assemble ()
                 (inst cmp dest result)
                 (inst b :le INNER-LOOP-DONE)
                 (inst ldr temp (@ result (load-store-offset
                                           (- (* delta n-word-bytes)))))
                 (inst str temp (@ result n-word-bytes :post-index))
                 (inst b LOOP)
                 INNER-LOOP-DONE
                 (inst mov csp-tn dest))))

        DO-REGS
        (when (< fixed register-arg-count)
          ;; Now we have to deposit any more args that showed up in registers.
          (loop with i = fixed
                for offset = (+ delta i)
                do
                (cond ((and (< (1+ i) register-arg-count)
                            (ldp-stp-offset-p (* offset n-word-bytes) n-word-bits))
                       (inst stp
                             (nth i *register-arg-tns*)
                             (nth (1+ i) *register-arg-tns*)
                             (@ cfp-tn (* offset n-word-bytes)))
                       (incf i 2))
                      (t
                       (storew (nth i *register-arg-tns*) cfp-tn offset)
                       (incf i)))
                while (< i register-arg-count)))
        DONE

        ;; Now that we're done with the &MORE args, we can set up the
        ;; number stack frame.
        (let ((nfp-tn (current-nfp-tn vop)))
          (when nfp-tn
            (inst sub nfp-tn nsp-tn (add-sub-immediate (bytes-needed-for-non-descriptor-stack-frame)))
            (inst mov-sp nsp-tn nfp-tn)))))))

;;; More args are stored consecutively on the stack, starting
;;; immediately at the context pointer.  The context pointer is not
;;; typed, so the lowtag is 0.
(define-vop (more-arg)
  (:translate %more-arg)
  (:policy :fast-safe)
  (:args (context :scs (descriptor-reg))
         (index :scs (any-reg immediate)))
  (:arg-types * tagged-num)
  (:temporary (:scs (any-reg)) temp)
  (:results (value :scs (descriptor-reg any-reg)))
  (:result-types *)
  (:generator 5
    (sc-case index
      (immediate
       (inst ldr value
             (@ context
                (load-store-offset
                 (ash (tn-value index) word-shift)))))
      (t
       (inst add temp context (lsl index (- word-shift n-fixnum-tag-bits)))
       (loadw value temp)))))

(define-vop ()
  (:translate sb-c::%more-kw-arg)
  (:policy :fast-safe)
  (:args (context :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:arg-types * tagged-num)
  (:temporary (:scs (any-reg)) temp)
  (:results (value :scs (descriptor-reg any-reg))
            (keyword :scs (descriptor-reg any-reg)))
  (:result-types * *)
  (:generator 5
    (inst add temp context (lsl index (- word-shift n-fixnum-tag-bits)))
    (inst ldp keyword value (@ temp))))

(define-vop (more-arg-or-nil)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:result 1))
         (count :scs (any-reg) :to (:result 1)))
  (:arg-types * tagged-num)
  (:info index)
  (:results (value :scs (descriptor-reg any-reg)))
  (:result-types *)
  (:generator 3
    (inst mov value null-tn)
    (cond ((zerop index)
           (inst cbz count done))
          (t
           (inst cmp count (fixnumize index))
           (inst b :le done)))
    (inst ldr value
          (@ object
             (load-store-offset
              (ash index word-shift))))
    done))

;;; Turn more arg (context, count) into a list.
(define-vop ()
  (:translate %listify-rest-args)
  (:args (context-arg :target context :scs (descriptor-reg))
         (count-arg :target count :scs (any-reg)))
  (:arg-types * tagged-num)
  (:temporary (:scs (descriptor-reg) :from (:argument 0)) context)
  (:temporary (:scs (any-reg) :from (:argument 1)) count)
  (:temporary (:scs (descriptor-reg) :from :eval) temp)
  (:temporary (:scs (any-reg) :from :eval) dst)
  (:temporary (:sc non-descriptor-reg :offset lr-offset) lr)
  (:results (result :scs (descriptor-reg)))
  (:policy :safe)
  (:node-var node)
  (:generator 20
    (move context context-arg)
    (move count count-arg)
    ;; Check to see if there are any arguments.
    (move result null-tn)
    (inst cbz count DONE)

    (pseudo-atomic (lr :sync nil :elide-if (node-stack-allocate-p node))
      (assemble ()
        ;; Allocate a cons (2 words) for each item.
        (let* ((dx-p (node-stack-allocate-p node))
               (size (cond (dx-p
                            (lsl count (1+ (- word-shift n-fixnum-tag-bits))))
                           (t
                            (inst lsl temp count (1+ (- word-shift n-fixnum-tag-bits)))
                            temp))))
          (allocation 'list size list-pointer-lowtag dst
                      :flag-tn lr
                      :stack-allocate-p dx-p
                      :overflow
                      (lambda ()
                        ;; The size will be computed by subtracting from CSP
                        (inst mov tmp-tn context)
                        (invoke-asm-routine (if (system-tlab-p 0 node) 'sys-listify-&rest 'listify-&rest) lr)
                        (inst mov result tmp-tn)
                        (inst b ALLOC-DONE))))
        (move result dst)

        (inst b ENTER)

        ;; Compute the next cons and store it in the current one.
        LOOP
        (inst add dst dst (* 2 n-word-bytes))
        (storew dst dst -1 list-pointer-lowtag)

        ;; Grab one value.
        ENTER
        (inst ldr temp (@ context n-word-bytes :post-index))

        ;; Dec count, and if != zero, go back for more.
        (inst subs count count (fixnumize 1))
        ;; Store the value into the car of the current cons.
        (storew temp dst 0 list-pointer-lowtag)
        (inst b :gt LOOP)

        ;; NIL out the last cons.
        (storew null-tn dst 1 list-pointer-lowtag)
        ALLOC-DONE))
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
    (inst sub context csp-tn (lsl count (- word-shift n-fixnum-tag-bits)))))

(define-vop (verify-arg-count)
  (:policy :fast-safe)
  (:args (nargs :scs (any-reg)))
  (:arg-types positive-fixnum (:constant t) (:constant t))
  (:info min max)
  (:vop-var vop)
  (:temporary (:sc unsigned-reg :offset nl0-offset) temp)
  (:save-p :compute-only)
  (:generator 3
    (let ((err-lab
           (generate-error-code vop 'invalid-arg-count-error)))
      (labels ((load-immediate (x)
                 (add-sub-immediate (fixnumize x))))
        (cond ((eql max 0)
               (inst cbnz nargs err-lab))
              ((not min)
               (inst cmp nargs (load-immediate max))
               (inst b :ne err-lab))
              (max
               (if (zerop min)
                   (setf temp nargs)
                   (inst sub temp nargs (load-immediate min)))
               (inst cmp temp (load-immediate (- max min)))
               (inst b :hi err-lab))
              (t
               (cond ((= min 1)
                      (inst cbz nargs err-lab))
                     ((plusp min)
                      (inst cmp nargs (load-immediate min))
                      (inst b :lo err-lab)))))))))

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
  (:node-var node)
  (:temporary (:scs (descriptor-reg) :from (:eval 0)) move-temp)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:temporary (:sc any-reg :offset ocfp-offset :from (:eval 0)) ocfp)
  (:ignore arg-locs args ocfp)
  (:generator 5
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (store-stack-tn nfp-save cur-nfp))
      (let ((callee-nfp (callee-nfp-tn callee)))
        (when callee-nfp
          (maybe-load-stack-tn callee-nfp nfp)))
      (maybe-load-stack-tn cfp-tn fp)
      (note-this-location vop :call-site)
      (inst bl target)
      (default-unknown-values vop values nvals move-temp node)
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
  (:ignore args save r0-temp)
  (:vop-var vop)
  (:node-var node)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:generator 20
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (store-stack-tn nfp-save cur-nfp))
      (let ((callee-nfp (callee-nfp-tn callee)))
        ;; alpha doesn't test this before the maybe-load
        (when callee-nfp
          (maybe-load-stack-tn callee-nfp nfp)))
      (maybe-load-stack-tn cfp-tn fp)
      (note-this-location vop :call-site)
      (inst bl target)
      (note-this-location vop :unknown-return)
      (receive-unknown-values node values-start nvals start count)
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
  (:generator 5
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (store-stack-tn nfp-save cur-nfp))
      (let ((callee-nfp (callee-nfp-tn callee)))
        (when callee-nfp
          (maybe-load-stack-tn callee-nfp nfp)))
      (maybe-load-stack-tn cfp-tn fp)
      (note-this-location vop :call-site)
      (inst bl target)
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
  (:args (old-fp)
         (return-pc)
         (vals :more t))
  (:temporary (:sc non-descriptor-reg :offset lr-offset) lr)
  (:move-args :known-return)
  (:info val-locs)
  (:ignore old-fp return-pc val-locs vals)
  (:vop-var vop)
  (:generator 6
    (move csp-tn cfp-tn)
    (loadw-pair cfp-tn ocfp-save-offset lr lra-save-offset cfp-tn)
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (inst add nsp-tn cur-nfp (add-sub-immediate
                                  (bytes-needed-for-non-descriptor-stack-frame)))))
    (lisp-return lr :known)))

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
(defmacro define-full-call (name named return variable &optional args)
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

      ,@(unless variable `((args :more t ,@(unless (eq args :fixed)
                                             '(:scs (descriptor-reg control-stack)))))))

     ,@(when (memq return '(:fixed :unboxed))
         '((:results (values :more t))))

     (:save-p ,(if (eq return :tail) :compute-only t))

     ,@(unless (or (eq return :tail)
                   variable)
         `((:move-args ,(if (eq args :fixed)
                            :fixed
                            :full-call))))


     (:vop-var vop)
     (:node-var node)
     (:info ,@(unless (or variable (eq return :tail)) '(arg-locs))
            ,@(unless variable '(nargs))
            ,@(when (eq named :direct) '(fun))
            ,@(when (eq return :fixed) '(nvals))
            step-instrumenting
            ,@(unless named
                '(fun-type)))

     (:ignore
      ,@(unless (or variable (eq return :tail)) '(arg-locs))
      ,@(unless variable '(args))
      ,@(ecase return
          (:fixed '(ocfp-temp))
          (:unboxed '(ocfp-temp node values))
          (:tail '(old-fp return-pc node))
          (:unknown '(r0-temp))))

     ,@(unless (eq named :direct)
         `((:temporary (:sc descriptor-reg :offset lexenv-offset
                        :from (:argument ,(if (eq return :tail) 0 1))
                        :to :eval)
                       ,(if named 'name-pass 'lexenv))))

     (:temporary (:sc any-reg :offset nargs-offset :to
                      ,(if (eq return :fixed)
                           :save
                           :eval))
                 nargs-pass)

     ,@(when variable
         (mapcar #'(lambda (name offset)
                     `(:temporary (:sc descriptor-reg
                                   :offset ,offset
                                   :to :result)
                                  ,name))
                 *register-arg-names* *register-arg-offsets*))
     ,@(when (eq return :fixed)
         '((:temporary (:scs (descriptor-reg) :from :eval) move-temp)
           (:temporary (:sc any-reg :from :eval :offset ocfp-offset) ocfp-temp)))
     ,@(when (eq return :unboxed)
         '((:temporary (:sc any-reg :from :eval :offset ocfp-offset) ocfp-temp)))
     ,@(unless (eq return :tail)
         '((:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)))

     (:temporary (:sc non-descriptor-reg :offset lr-offset) lr)

     (:generator ,(+ (if named 5 0)
                    (if variable 19 1)
                    (if (eq return :tail) 0 10)
                    15
                    (if (eq return :unknown) 25 0))
       (let* ((cur-nfp (current-nfp-tn vop))
              (filler
                (remove nil
                        (list ,@(if (eq return :tail)
                                    '(:load-nargs
                                      (when cur-nfp
                                        :frob-nfp))
                                    '(:load-nargs
                                      (when cur-nfp
                                        :frob-nfp)
                                      :load-fp))))))
         (flet ((do-next-filler ()
                  (let* ((next (pop filler))
                         (what (if (consp next) (car next) next)))
                    (ecase what
                      (:load-nargs
                       ,@(if variable
                             `((inst sub nargs-pass csp-tn new-fp)
                               (inst asr nargs-pass nargs-pass (- word-shift n-fixnum-tag-bits))
                               ,@(do ((arg *register-arg-names* (cddr arg))
                                      (i 0 (+ i 2))
                                      (insts))
                                     ((null arg) (nreverse insts))
                                   #.(assert (evenp register-arg-count))
                                   (push `(inst ldp ,(first arg) ,(second arg)
                                                (@ new-fp ,(* i n-word-bytes)))
                                         insts))
                               (storew cfp-tn new-fp ocfp-save-offset))
                             '((unless (consp nargs)
                                 (load-immediate-word nargs-pass (fixnumize nargs))))))
                      ,@(if (eq return :tail)
                            '((:frob-nfp
                               (inst add nsp-tn cur-nfp (add-sub-immediate
                                                         (bytes-needed-for-non-descriptor-stack-frame)))))
                            `((:frob-nfp
                               (store-stack-tn nfp-save cur-nfp))
                              (:load-fp
                               (move cfp-tn (cond ,@(and
                                                     (not variable)
                                                     '(((<= (if (consp nargs)
                                                                (car nargs)
                                                                nargs) register-arg-count)
                                                        csp-tn)))
                                                  (t
                                                   new-fp))))))
                      ((nil)))))
                (insert-step-instrumenting ()
                  ;; Conditionally insert a conditional trap:
                  (when step-instrumenting
                    (assemble ()
                      #-sb-thread
                      (load-symbol-value tmp-tn sb-impl::*stepping*)
                      #+sb-thread
                      (loadw tmp-tn thread-tn thread-stepping-slot)
                      (inst cbz tmp-tn step-done-label)
                      ;; CONTEXT-PC will be pointing here when the
                      ;; interrupt is handled, not after the
                      ;; DEBUG-TRAP.
                      (note-this-location vop :internal-error)
                      (inst brk single-step-around-trap)
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
                  (do-next-filler)
                  (insert-step-instrumenting)))
               ((nil)
                `((sc-case arg-fun
                    (descriptor-reg (move lexenv arg-fun))
                    (control-stack
                     (load-stack-tn lexenv arg-fun)
                     (do-next-filler))
                    (constant
                     (load-constant vop arg-fun lexenv)
                     (do-next-filler)))
                  (insert-step-instrumenting)
                  (do-next-filler))))
           (loop
            (if filler
                (do-next-filler)
                (return)))
           ,@(case named
               ((t)
                `((loadw lr name-pass fdefn-raw-addr-slot other-pointer-lowtag)
                  ,(if (eq return :tail)
                       `(inst add lr lr 4))))
               (:direct
                `((inst ldr lr (@ null-tn (load-store-offset (static-fun-offset fun))))
                  ,(if (eq return :tail)
                       `(inst add lr lr 4)))))

           (note-this-location vop :call-site)

           ,(if named
                (if (eq return :tail)
                    `(inst br lr)
                    `(inst blr lr))
                (if (eq return :tail)
                    `(tail-call-unnamed lexenv lr fun-type)
                    `(call-unnamed lexenv lr fun-type))))

         ,@(ecase return
             (:fixed
              '((default-unknown-values vop values nvals move-temp node)
                (when cur-nfp
                  (load-stack-tn cur-nfp nfp-save))))
             (:unknown
              '((note-this-location vop :unknown-return)
                (receive-unknown-values node values-start nvals start count)
                (when cur-nfp
                  (load-stack-tn cur-nfp nfp-save))))
             ((:unboxed)
              '((when cur-nfp
                  (load-stack-tn cur-nfp nfp-save))))
             ((:tail)))))))

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

(define-full-call fixed-call-named t :fixed nil :fixed)
(define-full-call fixed-tail-call-named t :tail nil :fixed)

(define-full-call unboxed-call-named t :unboxed nil)
(define-full-call fixed-unboxed-call-named t :unboxed nil :fixed)

;;; Defined separately, since needs special code that BLT's the
;;; arguments down.
(define-vop (tail-call-variable)
  (:args
   (args-arg :scs (any-reg) :target args)
   (function-arg :scs (descriptor-reg) :target lexenv)
   (old-fp-arg :scs (any-reg) :load-if nil)
   (lra-arg :scs (descriptor-reg) :load-if nil))
  (:info fun-type)
  (:temporary (:sc any-reg :offset nl2-offset :from (:argument 0)) args)
  (:temporary (:sc descriptor-reg :offset lexenv-offset :from (:argument 1)) lexenv)
  (:ignore old-fp-arg lra-arg)
  (:vop-var vop)
  (:generator 75
    ;; Move these into the passing locations if they are not already there.
    (move args args-arg)
    (move lexenv function-arg)
    ;; Clear the number stack if anything is there.
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (inst add nsp-tn cur-nfp (add-sub-immediate
                                  (bytes-needed-for-non-descriptor-stack-frame)))))
    (invoke-asm-routine (if (eq fun-type :function)
                            'tail-call-variable
                            'tail-call-callable-variable)
                        tmp-tn
                        :tail t)))

;;; Invoke the function-designator FUN.
(defun tail-call-unnamed (lexenv lr type)
  (case type
    (:symbol
     (invoke-asm-routine 'tail-call-symbol tmp-tn :tail t))
    (t
     (assemble ()
       (when (eq type :designator)
         (inst and tmp-tn lexenv lowtag-mask)
         (inst cmp tmp-tn fun-pointer-lowtag)
         (inst b :eq call)
         (invoke-asm-routine 'tail-call-symbol tmp-tn :tail t))
       call
       (loadw lr lexenv closure-fun-slot fun-pointer-lowtag)
       (inst add lr lr 4)
       (inst br lr)))))

(defun call-unnamed (lexenv lr type)
  (case type
    (:symbol
     (invoke-asm-routine 'call-symbol tmp-tn))
    (t
     (assemble ()
       (when (eq type :designator)
         (inst and tmp-tn lexenv lowtag-mask)
         (inst cmp tmp-tn fun-pointer-lowtag)
         (inst b :eq call)
         (invoke-asm-routine 'call-symbol tmp-tn)
         (inst b ret))
       call
       (loadw lr lexenv closure-fun-slot fun-pointer-lowtag)
       (inst blr lr)
       ret))))

;;;; Unknown values return:

;;; Return a single value using the unknown-values convention.
(define-vop (return-single)
  (:args (old-fp)
         (return-pc)
         (value))
  (:temporary (:sc non-descriptor-reg :offset lr-offset) lr)
  (:ignore value old-fp return-pc)
  (:vop-var vop)
  (:generator 6
    ;; Clear the number stack.
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (inst add nsp-tn cur-nfp (add-sub-immediate
                                  (bytes-needed-for-non-descriptor-stack-frame)))))
    ;; Interrupts leave two words of space for the new frame, so it's safe
    ;; to deallocate the frame before accessing OCFP/LR.
    (move csp-tn cfp-tn)
    (loadw-pair cfp-tn ocfp-save-offset lr lra-save-offset cfp-tn)
    ;; Clear the control stack, and restore the frame pointer.

    ;; Out of here.
    (lisp-return lr :single-value)))

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
   (old-fp)
   (return-pc)
   (values :more t))
  (:ignore values old-fp return-pc)
  (:info nvals)
  (:temporary (:sc descriptor-reg :offset r0-offset :from (:eval 0)) r0)
  (:temporary (:sc descriptor-reg :offset r1-offset :from (:eval 0)) r1)
  (:temporary (:sc descriptor-reg :offset r2-offset :from (:eval 0)) r2)
  (:temporary (:sc descriptor-reg :offset r3-offset :from (:eval 0)) r3)
  (:temporary (:sc non-descriptor-reg :offset lr-offset) lr)
  (:temporary (:sc any-reg :offset nargs-offset) nargs)
  (:temporary (:sc any-reg :offset ocfp-offset) val-ptr)
  (:vop-var vop)
  (:generator 6
    ;; Clear the number stack.
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (inst add nsp-tn cur-nfp (add-sub-immediate
                                  (bytes-needed-for-non-descriptor-stack-frame)))))
    (cond ((= nvals 1)
           ;; Clear the control stack, and restore the frame pointer.
           (move csp-tn cfp-tn)
           (loadw-pair cfp-tn ocfp-save-offset lr lra-save-offset cfp-tn)
           ;; Out of here.
           (lisp-return lr :single-value))
          (t
           ;; Establish the values pointer.
           (move val-ptr cfp-tn)
           ;; restore the frame pointer and clear as much of the control
           ;; stack as possible.
           (loadw-pair cfp-tn ocfp-save-offset lr lra-save-offset cfp-tn)
           (inst add csp-tn val-ptr (add-sub-immediate (* nvals n-word-bytes)))
           ;; Establish the values count.
           (load-immediate-word nargs (fixnumize nvals))
           ;; pre-default any argument register that need it.
           (when (< nvals register-arg-count)
             (dolist (reg (subseq (list r0 r1 r2 r3) nvals))
               (move reg null-tn)))
           ;; And away we go.
           (lisp-return lr :multiple-values)))))

;;; Do unknown-values return of an arbitrary number of values (passed
;;; on the stack.)  We check for the common case of a single return
;;; value, and do that inline using the normal single value return
;;; convention.  Otherwise, we branch off to code that calls an
;;; assembly-routine.
(define-vop (return-multiple)
  (:args
   (old-fp-arg :scs (any-reg) :to (:eval 1))
   (lra-arg)
   (vals-arg :scs (any-reg) :target vals)
   (nvals-arg :scs (any-reg) :target nvals))
  (:temporary (:sc any-reg :offset nl2-offset :from (:argument 0)) old-fp)
  (:temporary (:sc any-reg :offset nl1-offset :from (:argument 2)) vals)
  (:temporary (:sc any-reg :offset nargs-offset :from (:argument 3)) nvals)
  (:temporary (:sc descriptor-reg :offset r0-offset) r0)
  (:temporary (:sc non-descriptor-reg :offset lr-offset) lr)
  (:vop-var vop)
  (:generator 13
    (maybe-load-stack-tn lr lra-arg)
    ;; Clear the number stack.
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (inst add nsp-tn cur-nfp (add-sub-immediate
                                  (bytes-needed-for-non-descriptor-stack-frame)))))

    ;; Check for the single case.
    (inst cmp nvals-arg (fixnumize 1))
    (inst b :ne NOT-SINGLE)

    ;; Return with one value.
    (inst ldr r0 (@ vals-arg))
    (move csp-tn cfp-tn)
    (move cfp-tn old-fp-arg)
    (lisp-return lr :single-value)

    NOT-SINGLE
    (move old-fp old-fp-arg)
    (move vals vals-arg)
    (move nvals nvals-arg)
    (invoke-asm-routine 'return-multiple tmp-tn :tail t)))

;;; Single-stepping

(define-vop (step-instrument-before-vop)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 3
    #-sb-thread
    (load-symbol-value tmp-tn sb-impl::*stepping*)
    #+sb-thread
    (loadw tmp-tn thread-tn thread-stepping-slot)
    (inst cbz tmp-tn DONE)
    ;; CONTEXT-PC will be pointing here when the interrupt is handled,
    ;; not after the BREAK.
    (note-this-location vop :internal-error)
    ;; A best-guess effort at a debug trap suitable for a
    ;; single-step-before-trap.
    (inst brk single-step-before-trap)
    DONE))

