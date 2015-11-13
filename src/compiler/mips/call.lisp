;;;; the VM definition of function call for MIPS

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

(defconstant arg-count-sc (make-sc-offset immediate-arg-scn nargs-offset))
(defconstant closure-sc (make-sc-offset descriptor-reg-sc-number lexenv-offset))

;;; Make a passing location TN for a local call return PC.  If standard is
;;; true, then use the standard (full call) location, otherwise use any legal
;;; location.  Even in the non-standard case, this may be restricted by a
;;; desire to use a subroutine call instruction.
(defun make-return-pc-passing-location (standard)
  (if standard
      (make-wired-tn *backend-t-primitive-type* descriptor-reg-sc-number lra-offset)
      (make-restricted-tn *backend-t-primitive-type* descriptor-reg-sc-number)))

;;; This is similar to MAKE-RETURN-PC-PASSING-LOCATION, but makes a
;;; location to pass OLD-FP in. This is (obviously) wired in the
;;; standard convention, but is totally unrestricted in non-standard
;;; conventions, since we can always fetch it off of the stack using
;;; the arg pointer.
(defun make-old-fp-passing-location (standard)
  (if standard
      (make-wired-tn *fixnum-primitive-type* immediate-arg-scn ocfp-offset)
      (make-normal-tn *fixnum-primitive-type*)))

;;; Make the TNs used to hold OLD-FP and RETURN-PC within the current
;;; function. We treat these specially so that the debugger can find
;;; them at a known location.
(defun make-old-fp-save-location (env)
  (specify-save-tn
   (physenv-debug-live-tn (make-normal-tn *fixnum-primitive-type*) env)
   (make-wired-tn *fixnum-primitive-type*
                  control-stack-arg-scn
                  ocfp-save-offset)))

(defun make-return-pc-save-location (env)
  (let ((ptype *backend-t-primitive-type*))
    (specify-save-tn
     (physenv-debug-live-tn (make-normal-tn ptype) env)
     (make-wired-tn ptype control-stack-arg-scn lra-save-offset))))

;;; Make a TN for the standard argument count passing location.  We only
;;; need to make the standard location, since a count is never passed when we
;;; are using non-standard conventions.
(defun make-arg-count-location ()
  (make-wired-tn *fixnum-primitive-type* immediate-arg-scn nargs-offset))

;;;; Frame hackery:

;;; BYTES-NEEDED-FOR-NON-DESCRIPTOR-STACK-FRAME -- internal
;;;
;;; Return the number of bytes needed for the current non-descriptor stack
;;; frame.  Non-descriptor stack frames must be multiples of 8 bytes on
;;; the PMAX.
;;;
(defun bytes-needed-for-non-descriptor-stack-frame ()
  (* (logandc2 (1+ (sb-allocated-size 'non-descriptor-stack)) 1)
     n-word-bytes))

;;; Used for setting up the Old-FP in local call.
;;;
(define-vop (current-fp)
  (:results (val :scs (any-reg)))
  (:generator 1
    (move val cfp-tn)))

;;; Used for computing the caller's NFP for use in known-values return.  Only
;;; works assuming there is no variable size stuff on the nstack.
;;;
(define-vop (compute-old-nfp)
  (:results (val :scs (any-reg)))
  (:vop-var vop)
  (:generator 1
    (let ((nfp (current-nfp-tn vop)))
      (when nfp
        (inst addu val nfp (bytes-needed-for-non-descriptor-stack-frame))))))

;;; Accessing a slot from an earlier stack frame is definite hackery.
(define-vop (ancestor-frame-ref)
  (:args (frame-pointer :scs (descriptor-reg))
         (variable-home-tn :load-if nil))
  (:results (value :scs (descriptor-reg any-reg)))
  (:policy :fast-safe)
  (:generator 4
    (aver (sc-is variable-home-tn control-stack))
    (loadw value frame-pointer (tn-offset variable-home-tn))))
(define-vop (ancestor-frame-set)
  (:args (frame-pointer :scs (descriptor-reg))
         (value :scs (descriptor-reg any-reg)))
  (:results (variable-home-tn :load-if nil))
  (:policy :fast-safe)
  (:generator 4
    (aver (sc-is variable-home-tn control-stack))
    (storew value frame-pointer (tn-offset variable-home-tn))))

(define-vop (xep-allocate-frame)
  (:info start-lab)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 1
    ;; Make sure the function is aligned, and drop a label pointing to this
    ;; function header.
    (emit-alignment n-lowtag-bits)
    (emit-label start-lab)
    ;; Allocate function header.
    (inst simple-fun-header-word)
    (dotimes (i (1- simple-fun-code-offset))
      (inst word 0))
    ;; The start of the actual code.
    ;; Compute CODE from the address of this entry point.
    (let ((entry-point (gen-label)))
      (emit-label entry-point)
      (inst compute-code-from-lip code-tn lip-tn entry-point temp)
      ;; ### We should also save it on the stack so that the garbage collector
      ;; won't forget about us if we call anyone else.
      )))

(define-vop (xep-setup-sp)
  (:vop-var vop)
  (:generator 1
    (inst addu csp-tn cfp-tn
          (* n-word-bytes (sb-allocated-size 'control-stack)))
    (let ((nfp (current-nfp-tn vop)))
      (when nfp
        (inst addu nsp-tn nsp-tn
              (- (bytes-needed-for-non-descriptor-stack-frame)))
        (move nfp nsp-tn)))))

(define-vop (allocate-frame)
  (:results (res :scs (any-reg))
            (nfp :scs (any-reg)))
  (:info callee)
  (:generator 2
    (move res csp-tn)
    (inst addu csp-tn csp-tn
          (* n-word-bytes (sb-allocated-size 'control-stack)))
    (when (ir2-physenv-number-stack-p callee)
      (inst addu nsp-tn nsp-tn
            (- (bytes-needed-for-non-descriptor-stack-frame)))
      (move nfp nsp-tn))))

;;; Allocate a partial frame for passing stack arguments in a full call.  Nargs
;;; is the number of arguments passed.  If no stack arguments are passed, then
;;; we don't have to do anything.
;;;
(define-vop (allocate-full-call-frame)
  (:info nargs)
  (:results (res :scs (any-reg)))
  (:generator 2
    (when (> nargs register-arg-count)
      (move res csp-tn)
      (inst addu csp-tn csp-tn (* nargs n-word-bytes)))))




;;; Emit code needed at the return-point from an unknown-values call for a
;;; fixed number of values.  Values is the head of the TN-Ref list for the
;;; locations that the values are to be received into.  Nvals is the number of
;;; values that are to be received (should equal the length of Values).
;;;
;;;    MOVE-TEMP is a DESCRIPTOR-REG TN used as a temporary.
;;;
;;;    This code exploits the fact that in the unknown-values convention, a
;;; single value return returns at the return PC + 8, whereas a return of other
;;; than one value returns directly at the return PC.
;;;
;;;    If 0 or 1 values are expected, then we just emit an instruction to reset
;;; the SP (which will only be executed when other than 1 value is returned.)
;;;
;;; In the general case, we have to do three things:
;;;  -- Default unsupplied register values.  This need only be done when a
;;;     single value is returned, since register values are defaulted by the
;;;     callee in the non-single case.
;;;  -- Default unsupplied stack values.  This needs to be done whenever there
;;;     are stack values.
;;;  -- Reset SP.  This must be done whenever other than 1 value is returned,
;;;     regardless of the number of values desired.
;;;
;;; The general-case code looks like this:
#|
        b regs-defaulted                ; Skip if MVs
        nop

        move a1 null-tn                 ; Default register values
        ...
        loadi nargs 1                   ; Force defaulting of stack values
        move ocfp csp                   ; Set up args for SP resetting

regs-defaulted
        subu temp nargs register-arg-count

        bltz temp default-value-7       ; jump to default code
        addu temp temp -1
        loadw move-temp ocfp-tn 6       ; Move value to correct location.
        store-stack-tn val4-tn move-temp

        bltz temp default-value-8
        addu temp temp -1
        loadw move-temp ocfp-tn 7
        store-stack-tn val5-tn move-temp

        ...

defaulting-done
        move csp ocfp                    ; Reset SP.
<end of code>

<elsewhere>
default-value-7
        store-stack-tn val4-tn null-tn  ; Nil out 7'th value. (first on stack)

default-value-8
        store-stack-tn val5-tn null-tn  ; Nil out 8'th value.

        ...

        br defaulting-done
        nop
|#
;;;
(defun default-unknown-values (vop values nvals move-temp temp lra-label)
  (declare (type (or tn-ref null) values)
           (type unsigned-byte nvals) (type tn move-temp temp))
  (if (<= nvals 1)
      (progn
        ;; Note that this is a single-value return point.  This is actually
        ;; the multiple-value entry point for a single desired value, but
        ;; the code location has to be here, or the debugger backtrace
        ;; gets confused.
        (without-scheduling ()
          (note-this-location vop :single-value-return)
          (move csp-tn ocfp-tn t)
          (inst nop))
        (when lra-label
          (inst compute-code-from-lra code-tn code-tn lra-label temp)))
      (let ((regs-defaulted (gen-label))
            (defaulting-done (gen-label))
            (default-stack-vals (gen-label)))
        (without-scheduling ()
          ;; Note that this is an unknown-values return point.
          (note-this-location vop :unknown-return)
          ;; Branch off to the MV case.
          (inst b regs-defaulted)
          ;; If there are no stack results, clear the stack now.
          (if (> nvals register-arg-count)
              (inst addu temp nargs-tn (fixnumize (- register-arg-count)))
              (move csp-tn ocfp-tn t)))

        ;; Do the single value case.
        (do ((i 1 (1+ i))
             (val (tn-ref-across values) (tn-ref-across val)))
            ((= i (min nvals register-arg-count)))
          (move (tn-ref-tn val) null-tn))
        (when (> nvals register-arg-count)
          (inst b default-stack-vals)
          (move ocfp-tn csp-tn t))

        (emit-label regs-defaulted)

        (when (> nvals register-arg-count)
          ;; If there are stack results, we have to default them
          ;; and clear the stack.
          (collect ((defaults))
            (do ((i register-arg-count (1+ i))
                 (val (do ((i 0 (1+ i))
                           (val values (tn-ref-across val)))
                          ((= i register-arg-count) val))
                      (tn-ref-across val)))
                ((null val))

              (let ((default-lab (gen-label))
                    (tn (tn-ref-tn val)))
                (defaults (cons default-lab tn))

                (inst blez temp default-lab)
                (inst lw move-temp ocfp-tn (* i n-word-bytes))
                (inst addu temp temp (fixnumize -1))
                (store-stack-tn tn move-temp)))

            (emit-label defaulting-done)
            (move csp-tn ocfp-tn)

            (let ((defaults (defaults)))
              (aver defaults)
              (assemble (*elsewhere*)
                (emit-label default-stack-vals)
                (do ((remaining defaults (cdr remaining)))
                    ((null remaining))
                  (let ((def (car remaining)))
                    (emit-label (car def))
                    (when (null (cdr remaining))
                      (inst b defaulting-done))
                    (store-stack-tn (cdr def) null-tn)))))))

        (when lra-label
          (inst compute-code-from-lra code-tn code-tn lra-label temp))))
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
;;;
(defun receive-unknown-values (args nargs start count lra-label temp)
  (declare (type tn args nargs start count temp))
  (let ((variable-values (gen-label))
        (done (gen-label)))
    (without-scheduling ()
      (inst b variable-values)
      (inst nop))

    (when lra-label
      (inst compute-code-from-lra code-tn code-tn lra-label temp))
    (inst addu csp-tn csp-tn n-word-bytes)
    (storew (first *register-arg-tns*) csp-tn -1)
    (inst addu start csp-tn (- n-word-bytes))
    (inst li count (fixnumize 1))

    (emit-label done)

    (assemble (*elsewhere*)
      (emit-label variable-values)
      (when lra-label
        (inst compute-code-from-lra code-tn code-tn lra-label temp))
      (do ((arg *register-arg-tns* (rest arg))
           (i 0 (1+ i)))
          ((null arg))
        (storew (first arg) args i))
      (move start args)
      (inst b done)
      (move count nargs t)))
  (values))


;;; VOP that can be inherited by unknown values receivers.  The main thing this
;;; handles is allocation of the result temporaries.
;;;
(define-vop (unknown-values-receiver)
  (:results
   (start :scs (any-reg))
   (count :scs (any-reg)))
  (:temporary (:sc descriptor-reg :offset ocfp-offset
                   :from :eval :to (:result 0))
              values-start)
  (:temporary (:sc any-reg :offset nargs-offset
               :from :eval :to (:result 1))
              nvals)
  (:temporary (:scs (non-descriptor-reg)) temp))


;;; This hook in the codegen pass lets us insert code before fall-thru entry
;;; points, local-call entry points, and tail-call entry points.  The default
;;; does nothing.
(defun emit-block-header (start-label trampoline-label fall-thru-p alignp)
  (declare (ignore fall-thru-p alignp))
  (when trampoline-label
    (emit-label trampoline-label))
  (emit-label start-label))


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
;;;
(define-vop (call-local)
  (:args (fp)
         (nfp)
         (args :more t))
  (:results (values :more t))
  (:save-p t)
  (:move-args :local-call)
  (:info arg-locs callee target nvals)
  (:vop-var vop)
  (:temporary (:scs (descriptor-reg) :from :eval) move-temp)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:temporary (:sc any-reg :offset ocfp-offset :from :eval) ocfp)
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
      (inst compute-lra-from-code
            (callee-return-pc-tn callee) code-tn label temp)
      (note-this-location vop :call-site)
      (inst b target)
      (inst nop)
      (emit-return-pc label)
      (default-unknown-values vop values nvals move-temp temp label)
      (when cur-nfp
        (load-stack-tn cur-nfp nfp-save)))))


;;; Non-TR local call for a variable number of return values passed according
;;; to the unknown values convention.  The results are the start of the values
;;; glob and the number of values received.
;;;
;;; Note: we can't use normal load-tn allocation for the fixed args, since all
;;; registers may be tied up by the more operand.  Instead, we use
;;; MAYBE-LOAD-STACK-TN.
;;;
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
  (:generator 20
    (let ((label (gen-label))
          (cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (store-stack-tn nfp-save cur-nfp))
      (let ((callee-nfp (callee-nfp-tn callee)))
        (when callee-nfp
          (maybe-load-stack-tn callee-nfp nfp)))
      (maybe-load-stack-tn cfp-tn fp)
      (inst compute-lra-from-code
            (callee-return-pc-tn callee) code-tn label temp)
      (note-this-location vop :call-site)
      (inst b target)
      (inst nop)
      (emit-return-pc label)
      (note-this-location vop :unknown-return)
      (receive-unknown-values values-start nvals start count label temp)
      (when cur-nfp
        (load-stack-tn cur-nfp nfp-save)))))


;;;; Local call with known values return:

;;; Non-TR local call with known return locations.  Known-value return works
;;; just like argument passing in local call.
;;;
;;; Note: we can't use normal load-tn allocation for the fixed args, since all
;;; registers may be tied up by the more operand.  Instead, we use
;;; MAYBE-LOAD-STACK-TN.
;;;
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
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 5
    (let ((label (gen-label))
          (cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (store-stack-tn nfp-save cur-nfp))
      (let ((callee-nfp (callee-nfp-tn callee)))
        (when callee-nfp
          (maybe-load-stack-tn callee-nfp nfp)))
      (maybe-load-stack-tn cfp-tn fp)
      (inst compute-lra-from-code
            (callee-return-pc-tn callee) code-tn label temp)
      (note-this-location vop :call-site)
      (inst b target)
      (inst nop)
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
;;;
(define-vop (known-return)
  (:args (ocfp :target ocfp-temp)
         (return-pc :target return-pc-temp)
         (vals :more t))
  (:temporary (:sc any-reg :from (:argument 0)) ocfp-temp)
  (:temporary (:sc descriptor-reg :from (:argument 1))
              return-pc-temp)
  (:temporary (:scs (interior-reg)) lip)
  (:move-args :known-return)
  (:info val-locs)
  (:ignore val-locs vals)
  (:vop-var vop)
  (:generator 6
    (maybe-load-stack-tn ocfp-temp ocfp)
    (maybe-load-stack-tn return-pc-temp return-pc)
    (move csp-tn cfp-tn)
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (inst addu nsp-tn cur-nfp
              (bytes-needed-for-non-descriptor-stack-frame))))
    (inst addu lip return-pc-temp (- n-word-bytes other-pointer-lowtag))
    (inst j lip)
    (move cfp-tn ocfp-temp t)))


;;;; Full call:
;;;
;;;    There is something of a cross-product effect with full calls.  Different
;;; versions are used depending on whether we know the number of arguments or
;;; the name of the called function, and whether we want fixed values, unknown
;;; values, or a tail call.
;;;
;;; In full call, the arguments are passed creating a partial frame on the
;;; stack top and storing stack arguments into that frame.  On entry to the
;;; callee, this partial frame is pointed to by FP.  If there are no stack
;;; arguments, we don't bother allocating a partial frame, and instead set FP
;;; to SP just before the call.

;;;    This macro helps in the definition of full call VOPs by avoiding code
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
;;;    The Ocfp and Return-PC are passed as the second and third arguments.
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
;;;
(defmacro define-full-call (name named return variable)
  (aver (not (and variable (eq return :tail))))
  `(define-vop (,name
                ,@(when (eq return :unknown)
                    '(unknown-values-receiver)))
     (:args
      ,@(unless (eq return :tail)
          '((new-fp :scs (any-reg) :to :eval)))

      ,(if named
           '(name :target name-pass)
           '(arg-fun :target lexenv))

      ,@(when (eq return :tail)
          '((ocfp :target ocfp-pass)
            (return-pc :target return-pc-pass)))

      ,@(unless variable '((args :more t :scs (descriptor-reg)))))

     ,@(when (eq return :fixed)
         '((:results (values :more t))))

     (:save-p ,(if (eq return :tail) :compute-only t))

     ,@(unless (or (eq return :tail) variable)
         '((:move-args :full-call)))

     (:vop-var vop)
     (:info ,@(unless (or variable (eq return :tail)) '(arg-locs))
            ,@(unless variable '(nargs))
            ,@(when (eq return :fixed) '(nvals))
            step-instrumenting)

     (:ignore ,@(unless (or variable (eq return :tail)) '(arg-locs))
              ,@(unless variable '(args)))

     (:temporary (:sc descriptor-reg
                  :offset ocfp-offset
                  :from (:argument 1)
                  ,@(unless (eq return :fixed)
                      '(:to :eval)))
                 ocfp-pass)

     (:temporary (:sc descriptor-reg
                  :offset lra-offset
                  :from (:argument ,(if (eq return :tail) 2 1))
                  :to :eval)
                 return-pc-pass)

     ,@(if named
         `((:temporary (:sc descriptor-reg :offset fdefn-offset
                        :from (:argument ,(if (eq return :tail) 0 1))
                        :to :eval)
                       name-pass))

         `((:temporary (:sc descriptor-reg :offset lexenv-offset
                        :from (:argument ,(if (eq return :tail) 0 1))
                        :to :eval)
                       lexenv)
           (:temporary (:scs (descriptor-reg) :from (:argument 0) :to :eval)
                       function)))

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
         '((:temporary (:scs (descriptor-reg) :from :eval) move-temp)))

     (:temporary (:scs (descriptor-reg) :to :eval) stepping)

     ,@(unless (eq return :tail)
         '((:temporary (:scs (non-descriptor-reg)) temp)
           (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)))

     (:temporary (:sc interior-reg) entry-point)

     (:generator ,(+ (if named 5 0)
                     (if variable 19 1)
                     (if (eq return :tail) 0 10)
                     15
                     (if (eq return :unknown) 25 0))
       (let* ((cur-nfp (current-nfp-tn vop))
              ,@(unless (eq return :tail)
                  '((lra-label (gen-label))))
              (step-done-label (gen-label))
              (filler
               (remove nil
                       (list :load-nargs
                             ,@(if (eq return :tail)
                                   '((unless (location= ocfp ocfp-pass)
                                       :load-ocfp)
                                     (unless (location= return-pc
                                                        return-pc-pass)
                                       :load-return-pc)
                                     (when cur-nfp
                                       :frob-nfp))
                                   '(:comp-lra
                                     (when cur-nfp
                                       :frob-nfp)
                                     :save-fp
                                     :load-fp))))))
         (flet ((do-next-filler ()
                  (let* ((next (pop filler))
                         (what (if (consp next) (car next) next)))
                    (ecase what
                      (:load-nargs
                       ,@(if variable
                             `((inst subu nargs-pass csp-tn new-fp)
                               ,@(let ((index -1))
                                   (mapcar #'(lambda (name)
                                               `(inst lw ,name new-fp
                                                      ,(ash (incf index)
                                                            word-shift)))
                                           register-arg-names)))
                             '((inst li nargs-pass (fixnumize nargs)))))
                      ,@(if (eq return :tail)
                            '((:load-ocfp
                               (sc-case ocfp
                                 (any-reg
                                  (move ocfp-pass ocfp t))
                                 (control-stack
                                  (inst lw ocfp-pass cfp-tn
                                        (ash (tn-offset ocfp)
                                             word-shift)))))
                              (:load-return-pc
                               (sc-case return-pc
                                 (descriptor-reg
                                  (move return-pc-pass return-pc t))
                                 (control-stack
                                  (inst lw return-pc-pass cfp-tn
                                        (ash (tn-offset return-pc)
                                             word-shift)))))
                              (:frob-nfp
                               (inst addu nsp-tn cur-nfp
                                     (bytes-needed-for-non-descriptor-stack-frame))))
                            `((:comp-lra
                               (inst compute-lra-from-code
                                     return-pc-pass code-tn lra-label temp))
                              (:frob-nfp
                               (store-stack-tn nfp-save cur-nfp))
                              (:save-fp
                               (move ocfp-pass cfp-tn t))
                              (:load-fp
                               ,(if variable
                                    '(move cfp-tn new-fp)
                                    '(if (> nargs register-arg-count)
                                         (move cfp-tn new-fp)
                                         (move cfp-tn csp-tn))))))
                      ((nil)
                       (inst nop)))))
                (insert-step-instrumenting (callable-tn)
                  ;; Conditionally insert a conditional trap:
                  (when step-instrumenting
                    (load-symbol-value stepping sb!impl::*stepping*)
                    ;; If it's not 0, trap.
                    (inst beq stepping STEP-DONE-LABEL)
                    (inst nop)
                    ;; CONTEXT-PC will be pointing here when the
                    ;; interrupt is handled, not after the BREAK.
                    (note-this-location vop :step-before-vop)
                    ;; Construct a trap code with the low bits from
                    ;; SINGLE-STEP-AROUND-TRAP and the high bits from
                    ;; the register number of CALLABLE-TN.
                    (inst break 0 (logior single-step-around-trap
                                          (ash (reg-tn-encoding callable-tn)
                                               5)))
                    (emit-label step-done-label))))

           ,@(if named
                 `((sc-case name
                     (descriptor-reg (move name-pass name))
                     (control-stack
                      (inst lw name-pass cfp-tn
                            (ash (tn-offset name) word-shift))
                      (do-next-filler))
                     (constant
                      (inst lw name-pass code-tn
                            (- (ash (tn-offset name) word-shift)
                               other-pointer-lowtag))
                      (do-next-filler)))
                   ;; The step instrumenting must be done after
                   ;; FUNCTION is loaded, but before ENTRY-POINT is
                   ;; calculated.
                   (insert-step-instrumenting name-pass)
                   (inst lw entry-point name-pass
                         (- (ash fdefn-raw-addr-slot word-shift)
                            other-pointer-lowtag))
                   (do-next-filler))
                 `((sc-case arg-fun
                     (descriptor-reg (move lexenv arg-fun))
                     (control-stack
                      (inst lw lexenv cfp-tn
                            (ash (tn-offset arg-fun) word-shift))
                      (do-next-filler))
                     (constant
                      (inst lw lexenv code-tn
                            (- (ash (tn-offset arg-fun) word-shift)
                               other-pointer-lowtag))
                      (do-next-filler)))
                   (inst lw function lexenv
                         (- (ash closure-fun-slot word-shift)
                            fun-pointer-lowtag))
                   (do-next-filler)
                   ;; The step instrumenting must be done before
                   ;; after FUNCTION is loaded, but before ENTRY-POINT
                   ;; is calculated.
                   (insert-step-instrumenting function)
                   (inst addu entry-point function
                         (- (ash simple-fun-code-offset word-shift)
                            fun-pointer-lowtag))))
           (loop
             (if (cdr filler)
                 (do-next-filler)
                 (return)))

           (do-next-filler)
           (note-this-location vop :call-site)
           (inst j entry-point)
           (inst nop))

         ,@(ecase return
             (:fixed
              '((emit-return-pc lra-label)
                (default-unknown-values vop values nvals
                 move-temp temp lra-label)
                (when cur-nfp
                  (load-stack-tn cur-nfp nfp-save))))
             (:unknown
              '((emit-return-pc lra-label)
                (note-this-location vop :unknown-return)
                (receive-unknown-values values-start nvals start count
                 lra-label temp)
                (when cur-nfp
                  (load-stack-tn cur-nfp nfp-save))))
             (:tail))))))


(define-full-call call nil :fixed nil)
(define-full-call call-named t :fixed nil)
(define-full-call multiple-call nil :unknown nil)
(define-full-call multiple-call-named t :unknown nil)
(define-full-call tail-call nil :tail nil)
(define-full-call tail-call-named t :tail nil)

(define-full-call call-variable nil :fixed t)
(define-full-call multiple-call-variable nil :unknown t)


;;; Defined separately, since needs special code that BLT's the arguments
;;; down.
;;;
(define-vop (tail-call-variable)
  (:args
   (args-arg :scs (any-reg) :target args)
   (function-arg :scs (descriptor-reg) :target lexenv)
   (ocfp-arg :scs (any-reg) :target ocfp)
   (lra-arg :scs (descriptor-reg) :target lra))

  (:temporary (:sc any-reg :offset nl0-offset :from (:argument 0)) args)
  (:temporary (:sc any-reg :offset lexenv-offset :from (:argument 1)) lexenv)
  (:temporary (:sc any-reg :offset ocfp-offset :from (:argument 2)) ocfp)
  (:temporary (:sc any-reg :offset lra-offset :from (:argument 3)) lra)

  (:vop-var vop)

  (:generator 75

    ;; Move these into the passing locations if they are not already there.
    (move args args-arg)
    (move lexenv function-arg)
    (move ocfp ocfp-arg)
    (move lra lra-arg)

    ;; Clear the number stack if anything is there and jump to the
    ;; assembly-routine that does the bliting.
    (inst j (make-fixup 'tail-call-variable :assembly-routine))
    (let ((cur-nfp (current-nfp-tn vop)))
      (if cur-nfp
        (inst addu nsp-tn cur-nfp
              (bytes-needed-for-non-descriptor-stack-frame))
        (inst nop)))))


;;;; Unknown values return:

;;; Return a single value using the unknown-values convention.
;;;
(define-vop (return-single)
  (:args (ocfp :scs (any-reg))
         (return-pc :scs (descriptor-reg))
         (value))
  (:ignore value)
  (:temporary (:scs (interior-reg)) lip)
  (:vop-var vop)
  (:generator 6
    ;; Clear the number stack.
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (inst addu nsp-tn cur-nfp
              (bytes-needed-for-non-descriptor-stack-frame))))
    ;; Clear the control stack, and restore the frame pointer.
    (move csp-tn cfp-tn)
    (move cfp-tn ocfp)
    ;; Out of here.
    (lisp-return return-pc lip :offset 2)))


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
;;;
(define-vop (return)
  (:args (ocfp :scs (any-reg))
         (return-pc :scs (descriptor-reg) :to (:eval 1))
         (values :more t))
  (:ignore values)
  (:info nvals)
  (:temporary (:sc descriptor-reg :offset a0-offset :from (:eval 0)) a0)
  (:temporary (:sc descriptor-reg :offset a1-offset :from (:eval 0)) a1)
  (:temporary (:sc descriptor-reg :offset a2-offset :from (:eval 0)) a2)
  (:temporary (:sc descriptor-reg :offset a3-offset :from (:eval 0)) a3)
  (:temporary (:sc descriptor-reg :offset a4-offset :from (:eval 0)) a4)
  (:temporary (:sc descriptor-reg :offset a5-offset :from (:eval 0)) a5)
  (:temporary (:sc any-reg :offset nargs-offset) nargs)
  (:temporary (:sc any-reg :offset ocfp-offset) val-ptr)
  (:temporary (:scs (interior-reg)) lip)
  (:vop-var vop)
  (:generator 6
    ;; Clear the number stack.
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (inst addu nsp-tn cur-nfp
              (bytes-needed-for-non-descriptor-stack-frame))))
    (cond ((= nvals 1)
           ;; Clear the control stack, and restore the frame pointer.
           (move csp-tn cfp-tn)
           (move cfp-tn ocfp)
           ;; Out of here.
           (lisp-return return-pc lip :offset 2))
          (t
           ;; Establish the values pointer and values count.
           (move val-ptr cfp-tn)
           (inst li nargs (fixnumize nvals))
           ;; restore the frame pointer and clear as much of the control
           ;; stack as possible.
           (move cfp-tn ocfp)
           (inst addu csp-tn val-ptr (* nvals n-word-bytes))
           ;; pre-default any argument register that need it.
           (when (< nvals register-arg-count)
             (dolist (reg (subseq (list a0 a1 a2 a3 a4 a5) nvals))
               (move reg null-tn)))
           ;; And away we go.
           (lisp-return return-pc lip)))))

;;; Do unknown-values return of an arbitrary number of values (passed on the
;;; stack.)  We check for the common case of a single return value, and do that
;;; inline using the normal single value return convention.  Otherwise, we
;;; branch off to code that calls an assembly-routine.
;;;
(define-vop (return-multiple)
  (:args (ocfp-arg :scs (any-reg) :target ocfp)
         (lra-arg :scs (descriptor-reg) :target lra)
         (vals-arg :scs (any-reg) :target vals)
         (nvals-arg :scs (any-reg) :target nvals))

  (:temporary (:sc any-reg :offset nl1-offset :from (:argument 0)) ocfp)
  (:temporary (:sc descriptor-reg :offset lra-offset :from (:argument 1)) lra)
  (:temporary (:sc any-reg :offset nl0-offset :from (:argument 2)) vals)
  (:temporary (:sc any-reg :offset nargs-offset :from (:argument 3)) nvals)
  (:temporary (:sc descriptor-reg :offset a0-offset) a0)
  (:temporary (:scs (interior-reg)) lip)

  (:vop-var vop)

  (:generator 13
    (let ((not-single (gen-label)))
      ;; Clear the number stack.
      (let ((cur-nfp (current-nfp-tn vop)))
        (when cur-nfp
          (inst addu nsp-tn cur-nfp
                (bytes-needed-for-non-descriptor-stack-frame))))

      ;; Check for the single case.
      (inst li a0 (fixnumize 1))
      (inst bne nvals-arg a0 not-single)
      (inst lw a0 vals-arg)

      ;; Return with one value.
      (move csp-tn cfp-tn)
      (move cfp-tn ocfp-arg)
      (lisp-return lra-arg lip :offset 2)

      ;; Nope, not the single case.
      (emit-label not-single)
      (move ocfp ocfp-arg)
      (move lra lra-arg)
      (move vals vals-arg)

      (inst j (make-fixup 'return-multiple :assembly-routine))
      (move nvals nvals-arg t))))

;;;; XEP hackery:

;;; Get the lexical environment from its passing location.
;;;
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
;;;
(define-vop (copy-more-arg)
  (:temporary (:sc any-reg :offset nl0-offset) result)
  (:temporary (:sc any-reg :offset nl1-offset) count)
  (:temporary (:sc any-reg :offset nl2-offset) src)
  (:temporary (:sc any-reg :offset nl3-offset) dst)
  (:temporary (:sc descriptor-reg :offset l0-offset) temp)
  (:info fixed)
  (:generator 20
    (let ((loop (gen-label))
          (do-regs (gen-label))
          (done (gen-label)))
      (when (< fixed register-arg-count)
        ;; Save a pointer to the results so we can fill in register args.
        ;; We don't need this if there are more fixed args than reg args.
        (move result csp-tn))
      ;; Allocate the space on the stack.
      (cond ((zerop fixed)
             (inst beq nargs-tn done)
             (inst addu csp-tn csp-tn nargs-tn))
            (t
             (inst addu count nargs-tn (fixnumize (- fixed)))
             (inst blez count done)
             (inst nop)
             (inst addu csp-tn csp-tn count)))
      (when (< fixed register-arg-count)
        ;; We must stop when we run out of stack args, not when we run out of
        ;; more args.
        (inst addu count nargs-tn (fixnumize (- register-arg-count))))
      ;; Everything of interest in registers.
      (inst blez count do-regs)
      ;; Initialize dst to be end of stack.
      (move dst csp-tn t)
      ;; Initialize src to be end of args.
      (inst addu src cfp-tn nargs-tn)

      (emit-label loop)
      ;; *--dst = *--src, --count
      (inst addu src src (- n-word-bytes))
      (inst addu count count (fixnumize -1))
      (loadw temp src)
      (inst addu dst dst (- n-word-bytes))
      (inst bgtz count loop)
      (storew temp dst)

      (emit-label do-regs)
      (when (< fixed register-arg-count)
        ;; Now we have to deposit any more args that showed up in registers.
        ;; We know there is at least one more arg, otherwise we would have
        ;; branched to done up at the top.
        (inst subu count nargs-tn (fixnumize (1+ fixed)))
        (do ((i fixed (1+ i)))
            ((>= i register-arg-count))
          ;; Is this the last one?
          (inst beq count done)
          ;; Store it relative to the pointer saved at the start.
          (storew (nth i *register-arg-tns*) result (- i fixed))
          ;; Decrement count.
          (inst subu count (fixnumize 1))))
      (emit-label done))))


;;; More args are stored consecutively on the stack, starting
;;; immediately at the context pointer.  The context pointer is not
;;; typed, so the lowtag is 0.
(define-full-reffer more-arg * 0 0 (descriptor-reg any-reg) * %more-arg)

;;; Turn more arg (context, count) into a list.
(define-vop (listify-rest-args)
  (:args (context-arg :target context :scs (descriptor-reg))
         (count-arg :target count :scs (any-reg)))
  (:arg-types * tagged-num)
  (:temporary (:scs (any-reg) :from (:argument 0)) context)
  (:temporary (:scs (any-reg) :from (:argument 1)) count)
  (:temporary (:scs (descriptor-reg) :from :eval) temp dst)
  (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:results (result :scs (descriptor-reg)))
  (:translate %listify-rest-args)
  (:policy :safe)
  (:node-var node)
  (:generator 20
    (let* ((enter (gen-label))
           (loop (gen-label))
           (done (gen-label))
           (dx-p (node-stack-allocate-p node))
           (alloc-area-tn (if dx-p csp-tn alloc-tn)))
      (move context context-arg)
      (move count count-arg)
      ;; Check to see if there are any arguments.
      (inst beq count done)
      (move result null-tn t)

      ;; We need to do this atomically.
      (pseudo-atomic (pa-flag)
        (when dx-p
          (align-csp temp))
        ;; Allocate a cons (2 words) for each item.
        (inst srl result alloc-area-tn n-lowtag-bits)
        (inst sll result n-lowtag-bits)
        (inst or result list-pointer-lowtag)
        (move dst result)
        (inst sll temp count 1)
        (inst b enter)
        (inst addu alloc-area-tn temp)

        ;; Store the current cons in the cdr of the previous cons.
        (emit-label loop)
        (inst addu dst dst (* 2 n-word-bytes))
        (storew dst dst -1 list-pointer-lowtag)

        (emit-label enter)
        ;; Grab one value.
        (loadw temp context)
        (inst addu context n-word-bytes)

        ;; Dec count, and if != zero, go back for more.
        (inst addu count count (fixnumize -1))
        (inst bne count loop)

        ;; Store the value in the car (in delay slot)
        (storew temp dst 0 list-pointer-lowtag)

        ;; NIL out the last cons.
        (storew null-tn dst 1 list-pointer-lowtag))
      (emit-label done))))

;;; Return the location and size of the more arg glob created by Copy-More-Arg.
;;; Supplied is the total number of arguments supplied (originally passed in
;;; NARGS.)  Fixed is the number of non-rest arguments.
;;;
;;; We must duplicate some of the work done by Copy-More-Arg, since at that
;;; time the environment is in a pretty brain-damaged state, preventing this
;;; info from being returned as values.  What we do is compute
;;; supplied - fixed, and return a pointer that many words below the current
;;; stack top.
;;;
(define-vop (more-arg-context)
  (:policy :fast-safe)
  (:translate sb!c::%more-arg-context)
  (:args (supplied :scs (any-reg)))
  (:arg-types tagged-num (:constant fixnum))
  (:info fixed)
  (:results (context :scs (descriptor-reg))
            (count :scs (any-reg)))
  (:result-types t tagged-num)
  (:note "more-arg-context")
  (:generator 5
    (inst addu count supplied (fixnumize (- fixed)))
    (inst subu context csp-tn count)))


;;; Signal wrong argument count error if Nargs isn't = to Count.
;;;
#!-precise-arg-count-error
(define-vop (verify-arg-count)
  (:policy :fast-safe)
  (:translate sb!c::%verify-arg-count)
  (:args (nargs :scs (any-reg)))
  (:arg-types positive-fixnum (:constant t))
  (:temporary (:scs (any-reg) :type fixnum) temp)
  (:info count)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 3
    (let ((err-lab
           (generate-error-code vop 'invalid-arg-count-error nargs)))
      (cond ((zerop count)
             (inst bne nargs err-lab)
             (inst nop))
            (t
             (inst li temp (fixnumize count))
             (inst bne nargs temp err-lab)
             (inst nop))))))

#!+precise-arg-count-error
(define-vop (verify-arg-count)
  (:policy :fast-safe)
  (:args (nargs :scs (any-reg)))
  (:temporary (:scs (any-reg) :type fixnum) temp)
  (:arg-types positive-fixnum (:constant t) (:constant t))
  (:info min max)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 3
              (let ((err-lab
                      (generate-error-code vop 'invalid-arg-count-error nargs)))
                (cond ((not min)
                       (cond ((zerop max)
                              (inst bne nargs err-lab))
                             (t
                              (inst li temp (fixnumize max))
                              (inst bne nargs temp err-lab)))
                       (inst nop))
                      (max
                       (when (plusp min)
                         (inst li temp (fixnumize min))
                         (inst sltu temp nargs temp)
                         (inst bne temp err-lab)
                         (inst nop))
                       (inst li temp (fixnumize max))
                       (inst sltu temp temp nargs)
                       (inst bne temp err-lab)
                       (inst nop))
                      ((plusp min)
                       (inst li temp (fixnumize min))
                       (inst sltu temp nargs temp)
                       (inst bne temp err-lab)
                       (inst nop))))))

;;; Single-stepping

(define-vop (step-instrument-before-vop)
  (:temporary (:scs (descriptor-reg)) stepping)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 3
    (load-symbol-value stepping sb!impl::*stepping*)
    ;; If it's not 0, trap.
    (inst beq stepping DONE)
    (inst nop)
    ;; CONTEXT-PC will be pointing here when the interrupt is handled,
    ;; not after the BREAK.
    (note-this-location vop :step-before-vop)
    ;; CALLEE-REGISTER-OFFSET isn't needed for before-traps, so we
    ;; can just use a bare SINGLE-STEP-BEFORE-TRAP as the code.
    (inst break 0 single-step-before-trap)
    DONE))
