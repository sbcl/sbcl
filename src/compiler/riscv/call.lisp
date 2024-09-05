;;;; function call for the RISC-V VM

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

;;; Make a passing location TN for a local call return PC.
(defun make-return-pc-passing-location (standard)
  (declare (ignore standard))
  (make-wired-tn *fixnum-primitive-type* immediate-arg-scn ra-offset))

;;; This is similar to MAKE-RETURN-PC-PASSING-LOCATION, but makes a
;;; location to pass OLD-FP in. This is (obviously) wired in the
;;; standard convention, but is totally unrestricted in non-standard
;;; conventions, since we can always fetch it off of the stack using
;;; the arg pointer.
(defun make-old-fp-passing-location ()
  (make-wired-tn *fixnum-primitive-type* immediate-arg-scn ocfp-offset))

(defconstant old-fp-passing-offset
  (make-sc+offset descriptor-reg-sc-number ocfp-offset))

;;; Make the TNs used to hold OLD-FP and RETURN-PC within the current
;;; function. We treat these specially so that the debugger can find
;;; them at a known location.
(defun make-old-fp-save-location (env)
  (specify-save-tn
   (environment-debug-live-tn (make-normal-tn *fixnum-primitive-type*) env)
   (make-wired-tn *fixnum-primitive-type* control-stack-arg-scn ocfp-save-offset)))

(defun make-return-pc-save-location (env)
  (let ((ptype *fixnum-primitive-type*))
    (specify-save-tn
     (environment-debug-live-tn (make-normal-tn ptype) env)
     (make-wired-tn ptype control-stack-arg-scn ra-save-offset))))

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

;;; This is used for setting up the Old-FP in local call.
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
        (inst addi val nfp (bytes-needed-for-non-descriptor-stack-frame))))))

;;; Accessing a slot from an earlier stack frame is definite hackery.
(define-vop (ancestor-frame-ref)
  (:args (frame-pointer :scs (descriptor-reg))
         (variable-home-tn :load-if nil))
  (:results (value :scs (descriptor-reg any-reg)))
  ;; :unused-if does not work because offsets haven't been assigned
  (:temporary (#|:unused-if (typep (ash (tn-offset variable-home-tn) word-shift) 'short-immediate)|#
               :sc unsigned-reg) tmp)
  (:policy :fast-safe)
  (:generator 4
    (aver (sc-is variable-home-tn control-stack))
    (load-frame-word value frame-pointer (tn-offset variable-home-tn) 'ancestor-frame-ref tmp)))

(define-vop (ancestor-frame-set)
  (:args (frame-pointer :scs (descriptor-reg))
         (value :scs (descriptor-reg any-reg)))
  (:results (variable-home-tn :load-if nil))
  ;; :unused-if does not work because offsets haven't been assigned
  (:temporary (#|:unused-if (typep (ash (tn-offset variable-home-tn) word-shift) 'short-immediate)|#
               :sc unsigned-reg) tmp)
  (:generator 4
    (aver (sc-is variable-home-tn control-stack))
    (store-frame-word value frame-pointer (tn-offset variable-home-tn) 'ancestor-frame-set tmp)))

(define-vop (xep-allocate-frame)
  (:info start-lab)
  (:temporary (:scs (interior-reg)) lip)
  (:temporary (:scs (descriptor-reg) :offset l0-offset) fn)
  (:generator 1
    ;; Make sure the function is aligned, and drop a label pointing to this
    ;; function header.
    (emit-alignment n-lowtag-bits)
    (emit-label start-lab)
    ;; Allocate function header.
    (inst simple-fun-header-word)
    (inst .skip (* (1- simple-fun-insts-offset) n-word-bytes))
    (inst compute-code-from-fn code-tn fn lip start-lab)))

(define-vop (xep-setup-sp)
  (:vop-var vop)
  (:temporary (:sc unsigned-reg) tmp) ; TODO- can :unused-if be based on frame size?
  (:generator 1
    (add-imm csp-tn cfp-tn (* n-word-bytes (sb-allocated-size 'control-stack)) 'xep-setup-sp tmp)
    (let ((nfp (current-nfp-tn vop)))
      (when nfp
        (inst subi nsp-tn nsp-tn (bytes-needed-for-non-descriptor-stack-frame))
        (move nfp nsp-tn)))))

(define-vop (allocate-frame)
  (:results (res :scs (any-reg))
            (nfp :scs (any-reg)))
  (:temporary (:sc unsigned-reg) tmp)
  (:info callee)
  (:generator 2
    (move res csp-tn)
    (add-imm csp-tn csp-tn (* n-word-bytes (sb-allocated-size 'control-stack)) 'allocate-frame tmp)
    (when (ir2-environment-number-stack-p callee)
      (inst addi nsp-tn nsp-tn (- (bytes-needed-for-non-descriptor-stack-frame)))
      (move nfp nsp-tn))))

;;; Allocate a partial frame for passing stack arguments in a full
;;; call.  NARGS is the number of arguments passed.  If no stack
;;; arguments are passed, then we don't have to do anything.
(define-vop (allocate-full-call-frame)
  (:info nargs)
  (:results (res :scs (any-reg)))
  (:generator 2
    (when (> nargs register-arg-count)
      (move res csp-tn)
      (inst addi csp-tn csp-tn (* nargs n-word-bytes)))))

;;; Emit code needed at the return-point from an unknown-values call
;;; for a fixed number of values.  VALUES is the head of the TN-REF
;;; list for the locations that the values are to be received into.
;;; NVALS is the number of values that are to be received (should
;;; equal the length of Values).
;;;
;;; MOVE-TEMP is a DESCRIPTOR-REG TN used as a temporary.
;;;
;;; This code exploits the fact that in the unknown-values convention,
;;; a single-value return is indicated with a negative number in NARGS
;;; and multiple-value return is indicated with a positive fixnum in
;;; NARGS.
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
(defun default-unknown-values (vop values nvals move-temp lip lra-label)
  (declare (type (or tn-ref null) values)
           (type unsigned-byte nvals) (type tn move-temp))
  (let ((expecting-values-on-stack (> nvals register-arg-count))
        (not-single (gen-label))
        (done (gen-label))
        defaulting-code-p)
    (note-this-location vop (if (<= nvals 1)
                                :single-value-return
                                :unknown-return))
    (inst compute-code-from-ra code-tn ra-tn lip lra-label)
    ;; Pick off the single-value case first.
    (sb-assem:without-scheduling ()

      ;; Default register values for single-value return case.  The
      ;; callee returns with negative NARGS for single value return.
      (when values
        (do ((i 1 (1+ i))
             (val (tn-ref-across values) (tn-ref-across val)))
            ((= i (min nvals register-arg-count)))
          (unless (eq (tn-kind (tn-ref-tn val)) :unused)
            (unless defaulting-code-p
              (inst bge nargs-tn zero-tn not-single)
              (setf defaulting-code-p t))
            (move (tn-ref-tn val) null-tn))))

      ;; If we're not expecting values on the stack, all that
      ;; remains is to clear the stack frame (for the multiple-
      ;; value return case).
      (unless expecting-values-on-stack
        (cond (defaulting-code-p
               (inst j done)
               (emit-label not-single))
              (t
               (inst blt nargs-tn zero-tn done)))
        (move csp-tn ocfp-tn)
        (emit-label done))

      ;; If we ARE expecting values on the stack, we need to
      ;; either move them to their result location or to set their
      ;; result location to the default.
      (when expecting-values-on-stack
        ;; For the single-value return case, fake up NARGS and
        ;; OCFP so that we don't screw ourselves with the
        ;; defaulting and stack clearing logic.
        (unless defaulting-code-p
          (inst bge nargs-tn zero-tn not-single))
        (move ocfp-tn csp-tn)
        (inst li nargs-tn (fixnumize 1))
        (emit-label not-single)

        ;; For each expected stack value..
        (do ((i register-arg-count (1+ i))
             (decrement (fixnumize (1+ register-arg-count)))
             (val (do ((i 0 (1+ i))
                       (val values (tn-ref-across val)))
                      ((= i register-arg-count) val))
                  (tn-ref-across val)))
            ((null val))
          (let ((tn (tn-ref-tn val)))
            (if (eq (tn-kind tn) :unused)
                (incf decrement (fixnumize 1))
                (assemble ()
                  ;; ... Load it if there is a stack value available, or
                  ;; default it if there isn't.
                  (inst subi nargs-tn nargs-tn decrement)
                  (setf decrement (fixnumize 1))
                  (inst blt nargs-tn zero-tn NONE)
                  (loadw move-temp ocfp-tn i)
                  (inst j DEFAULTED)
                  NONE
                  (move (sc-case tn
                          (control-stack move-temp)
                          (t tn))
                        null-tn)
                  DEFAULTED
                  (sc-case tn
                    (control-stack
                     (store-stack-tn tn move-temp))
                    (t))))))
        ;; Deallocate the callee stack frame.
        (move csp-tn ocfp-tn))))
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
(defun receive-unknown-values (args nargs start count label lip)
  (declare (type tn args nargs start count))
  (assemble ()
    (inst compute-code-from-ra code-tn ra-tn lip label)
    (inst bge nargs-tn zero-tn MULTIPLE)
    (move start csp-tn)
    (inst addi csp-tn csp-tn n-word-bytes)
    (storew (first *register-arg-tns*) csp-tn -1)
    (inst li count (fixnumize 1))

    (inst j DONE)
    MULTIPLE
    (do ((arg *register-arg-tns* (rest arg))
         (i 0 (1+ i)))
        ((null arg))
      (storew (first arg) args i))
    (move start args)
    (move count nargs)
    DONE))

;;; VOP that can be inherited by unknown values receivers.  The main
;;; thing this handles is allocation of the result temporaries.
(define-vop (unknown-values-receiver)
  (:results (start :scs (any-reg))
            (count :scs (any-reg)))
  (:temporary (:sc descriptor-reg :offset ocfp-offset
                   :from :eval :to (:result 0))
              values-start)
  (:temporary (:sc any-reg :offset nargs-offset
               :from :eval :to (:result 1))
              nvals))

;;; This hook in the codegen pass lets us insert code before fall-thru
;;; entry points, local-call entry points, and tail-call entry points.
;;; The default does nothing.
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
  (:vop-var vop)
  (:temporary (:scs (descriptor-reg) :from (:eval 0)) move-temp)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:temporary (:sc any-reg :offset ocfp-offset :from (:eval 0)) ocfp)
  (:temporary (:scs (interior-reg)) lip)
  (:ignore arg-locs args ocfp)
  (:info arg-locs callee target nvals)
  (:generator 5
    (let ((label (gen-label))
          (cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (store-stack-tn nfp-save cur-nfp))
      (let ((callee-nfp (callee-nfp-tn callee)))
        (when callee-nfp
          (maybe-load-stack-tn callee-nfp nfp)))
      (maybe-load-stack-tn cfp-tn fp)
      (note-this-location vop :call-site)
      (inst jal ra-tn target)
      (emit-label label)
      (default-unknown-values vop values nvals move-temp lip label)
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
  (:info save callee target)
  (:save-p t)
  (:move-args :local-call)
  (:ignore args save)
  (:vop-var vop)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
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
      (note-this-location vop :call-site)
      (inst jal ra-tn target)
      (emit-label label)
      (note-this-location vop :unknown-return)
      (receive-unknown-values values-start nvals start count label lip)
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
  (:info save callee target)
  (:ignore args res save)
  (:save-p t)
  (:move-args :local-call)
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
      (inst jal ra-tn target)
      (note-this-location vop :known-return)
      (when cur-nfp
        (load-stack-tn cur-nfp nfp-save)))))

;;; Return from known values call.  We receive the return locations as
;;; arguments to terminate their lifetimes in the returning function.
;;; We restore FP and CSP and jump to the Return-PC.
;;;
;;; Note: we can't use normal load-tn allocation for the fixed args,
;;; since all registers may be tied up by the more operand.  Instead,
;;; we use MAYBE-LOAD-STACK-TN.
(define-vop (known-return)
  (:args (old-fp :target old-fp-temp)
         (return-pc :target return-pc-temp)
         (values :more t))
  (:temporary (:sc any-reg :from (:argument 0)) old-fp-temp)
  (:temporary (:sc any-reg :from (:argument 1) :offset ra-offset) return-pc-temp)
  (:info val-locs)
  (:ignore val-locs values)
  (:move-args :known-return)
  (:vop-var vop)
  (:generator 6
    (maybe-load-stack-tn old-fp-temp old-fp)
    (maybe-load-stack-tn return-pc-temp return-pc)
    (move csp-tn cfp-tn)
    (clear-number-stack vop)
    (move cfp-tn old-fp-temp)
    (inst jalr zero-tn return-pc-temp 0)))


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
  (let ((register-arg-names (loop repeat register-arg-count
                                  collect (gensym))))
    `(define-vop (,name
                  ,@(when (eql return :unknown) '(unknown-values-receiver)))
       (:args
        ,@(unless (eq return :tail)
            '((new-fp :scs (any-reg) :to :eval)))
        ,@(case named
            ((nil)
             '((arg-fun :target lexenv)))
            (:direct)
            (t '((name :target name-pass))))
        ,@(when (eq return :tail)
            '((old-fp :target old-fp-pass)
              (return-pc :target return-pc-pass)))
        ,@(unless variable
            '((args :more t :scs (descriptor-reg control-stack)))))
       ,@(when (eq return :fixed)
           '((:results (values :more t))))
       (:save-p ,(if (eq return :tail) :compute-only t))
       ,@(unless (or (eq return :tail) variable)
           '((:move-args :full-call)))
       (:vop-var vop)
       (:info
        ,@(unless (or variable (eq return :tail)) '(arg-locs))
        ,@(unless variable '(nargs))
        ,@(when (eq named :direct) '(fun))
        ,@(when (eq return :fixed) '(nvals))
        step-instrumenting)
       (:ignore ,@(unless (eq return :tail) '(return-pc-pass))
                ,@(unless (or variable (eq return :tail)) '(arg-locs))
                ,@(unless variable '(args)))

       (:temporary (:sc descriptor-reg
                    :offset ocfp-offset
                    :from (:argument 1)
                    ,@(unless (eq return :fixed)
                        '(:to :eval)))
                   old-fp-pass)

       (:temporary (:sc any-reg
                    :offset ra-offset
                    :from (:argument ,(if (eq return :tail) 2 1))
                    :to :eval)
                   return-pc-pass)

       ,@(unless (eq named :direct)
         `((:temporary (:sc descriptor-reg :offset lexenv-offset
                        :from (:argument ,(if (eq return :tail) 0 1))
                        :to :eval)
                       ,(if named 'name-pass 'lexenv))))
       (:temporary (:scs (descriptor-reg) :offset l0-offset
                         :to :eval)
                   function)
       (:temporary (:sc any-reg :offset nargs-offset :to
                        ,(if (eq return :fixed)
                             :save
                             :eval))
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
           '((:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
             (:temporary (:scs (interior-reg)) lip)))

       (:generator ,(+ (if named 5 0)
                       (if variable 19 1)
                       (if (eq return :tail) 0 10)
                       15
                       (if (eq return :unknown) 25 0))
         (let* ((cur-nfp (current-nfp-tn vop))
                ,@(unless (eq return :tail)
                    '((label (gen-label))))
                (step-done-label (gen-label))
                (filler
                  (remove nil
                          (list :load-nargs
                                ,@(if (eq return :tail)
                                      '((unless (location= old-fp old-fp-pass)
                                          :load-ocfp)
                                        (unless (location= return-pc
                                                           return-pc-pass)
                                          :load-return-pc)
                                        (when cur-nfp
                                          :frob-nfp))
                                      '((when cur-nfp
                                          :frob-nfp)
                                        :save-fp
                                        :load-fp))))))
           (flet ((do-next-filler ()
                    (let* ((next (pop filler))
                           (what (if (consp next) (car next) next)))
                      (ecase what
                        (:load-nargs
                         ,@(if variable
                               `((inst sub nargs-pass csp-tn new-fp)
                                 (with-word-index-as-fixnum (nargs-pass nargs-pass))
                                 ,@(let ((index -1))
                                     (mapcar (lambda (name)
                                               `(loadw ,name new-fp ,(incf index)))
                                             register-arg-names)))
                               '((inst li nargs-pass (fixnumize nargs)))))
                        ,@(if (eq return :tail)
                              '((:load-ocfp
                                 (sc-case old-fp
                                   (any-reg
                                    (move old-fp-pass old-fp))
                                   (control-stack
                                    (load-stack-tn old-fp-pass old-fp))))
                                (:load-return-pc
                                 (sc-case return-pc
                                   (any-reg
                                    (move return-pc-pass return-pc))
                                   (control-stack
                                    (load-stack-tn return-pc-pass return-pc))))
                                (:frob-nfp
                                 (inst addi nsp-tn cur-nfp
                                  (bytes-needed-for-non-descriptor-stack-frame))))
                              `((:frob-nfp
                                 (store-stack-tn nfp-save cur-nfp))
                                (:save-fp
                                 (move old-fp-pass cfp-tn))
                                (:load-fp
                                 ,(if variable
                                      '(move cfp-tn new-fp)
                                      '(if (> nargs register-arg-count)
                                        (move cfp-tn new-fp)
                                        (move cfp-tn csp-tn))))))
                        ((nil)))))
                  (insert-step-instrumenting (callable-tn)
                    ;; Conditionally insert a conditional trap:
                    (when step-instrumenting
                      (load-stepping stepping)
                      ;; If it's not 0, trap.
                      (inst beq stepping zero-tn STEP-DONE-LABEL)
                      ;; CONTEXT-PC will be pointing here when the
                      ;; interrupt is handled, not after the EBREAK.
                      (note-this-location vop :internal-error)
                      (inst ebreak single-step-around-trap)
                      (inst byte (tn-offset callable-tn))
                      (emit-alignment 2)
                      (emit-label step-done-label))))
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
                  `((etypecase (static-fun-offset fun)
                      (short-immediate
                       (inst #-64-bit lw #+64-bit ld function null-tn (static-fun-offset fun)))
                      (u+i-immediate
                       (multiple-value-bind (u i)
                           (u-and-i-inst-immediate (static-fun-offset fun))
                         (inst lui function u)
                         (inst add function null-tn function)
                         (inst #-64-bit lw #+64-bit ld function function i)))))))
             (loop
               (if filler
                   (do-next-filler)
                   (return)))

             (note-this-location vop :call-site)
             (inst jalr ,(if (eq return :tail)
                             'zero-tn
                             'ra-tn)
                   function
                   (- (ash simple-fun-insts-offset word-shift)
                      fun-pointer-lowtag)))

           ,@(ecase return
               (:fixed
                '((emit-label label)
                  (default-unknown-values vop values nvals move-temp lip label)
                  (when cur-nfp
                    (load-stack-tn cur-nfp nfp-save))))
               (:unknown
                '((emit-label label)
                  (note-this-location vop :unknown-return)
                  (receive-unknown-values values-start nvals start count
                                          label lip)
                  (when cur-nfp
                    (load-stack-tn cur-nfp nfp-save))))
               (:tail)))))))

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
   (old-fp-arg :scs (any-reg) :target old-fp)
   (ra-arg :scs (any-reg) :target ra))
  (:temporary (:sc any-reg :offset nl0-offset :from (:argument 0)) args)
  (:temporary (:sc any-reg :offset lexenv-offset :from (:argument 1)) lexenv)
  (:temporary (:sc any-reg :offset ocfp-offset :from (:argument 2)) old-fp)
  (:temporary (:sc any-reg :offset ra-offset :from (:argument 3)) ra)
  (:vop-var vop)
  (:generator 75
    ;; Move these into the passing locations if they are not already there.
    (move args args-arg)
    (move lexenv function-arg)
    (move old-fp old-fp-arg)
    (move ra ra-arg)
    ;; Clear the number stack if anything is there.
    (clear-number-stack vop)
    ;; And jump to the assembly routine.
    (inst jal zero-tn (make-fixup 'tail-call-variable :assembly-routine))))


;;;; Unknown values return:

(defun clear-number-stack (vop)
  (let ((cur-nfp (current-nfp-tn vop)))
    (when cur-nfp
      (inst addi nsp-tn cur-nfp (bytes-needed-for-non-descriptor-stack-frame)))))

;;; Return a single value using the unknown-values convention.
(define-vop (return-single)
  (:args (old-fp :scs (any-reg))
         (return-pc :scs (any-reg) :target ra)
         (value))
  (:temporary (:sc any-reg :offset ra-offset :from (:argument 1)) ra)
  (:ignore value)
  (:vop-var vop)
  (:generator 6
    ;; Clear the number stack.
    (clear-number-stack vop)
    ;; Clear the control stack, and restore the frame pointer.
    (move csp-tn cfp-tn)
    (move cfp-tn old-fp)
    ;; Out of here.
    (inst li nargs-tn -1) ; mark single value return
    (move ra return-pc)
    (inst jalr zero-tn ra 0)))

;;; Do unknown-values return of a fixed number of values.  The Values
;;; are required to be set up in the standard passing locations.
;;; Nvals is the number of values returned.

;;; If returning other than one value, then load the number of values
;;; returned, NIL out unsupplied values registers, restore FP and
;;; return at Return-PC.  When there
;;; are stack values, we must initialize the argument pointer to point
;;; to the beginning of the values block (which is the beginning of
;;; the current frame.)
(macrolet ((frob ()
             (let ((a (loop repeat register-arg-count
                            collect (gensym))))
               `(define-vop (return)
                  (:args (old-fp :scs (any-reg))
                         (return-pc :scs (any-reg) :to (:eval 1) :target ra)
                         (values :more t))
                  (:ignore values)
                  ,@(loop for an-offset in *register-arg-offsets*
                          for an in a
                          collect `(:temporary (:sc descriptor-reg
                                                :offset ,an-offset
                                                :from :eval)
                                               ,an))
                  (:temporary (:sc any-reg :offset nargs-offset) nargs)
                  (:temporary (:sc any-reg :offset ocfp-offset) val-ptr)
                  (:temporary (:sc any-reg :offset ra-offset :from (:eval 1)) ra)
                  (:info nvals)
                  (:vop-var vop)
                  (:generator 6
                    (when (= nvals 1)
                      ;; This is handled in RETURN-SINGLE.
                      (error "nvalues is 1"))
                    ;; Clear the number stack.
                    (clear-number-stack vop)
                    ;; Establish the values pointer and values count.
                    (move val-ptr cfp-tn)
                    (inst li nargs (fixnumize nvals))
                    ;; restore the frame pointer and clear as much of
                    ;; the control stack as possible.
                    (move cfp-tn old-fp)
                    (inst addi csp-tn val-ptr (* nvals n-word-bytes))
                    ;; pre-default any argument register that need it.
                    (when (< nvals register-arg-count)
                      (dolist (reg (subseq (list ,@a) nvals))
                        (move reg null-tn)))
                    ;; And away we go.
                    (move ra return-pc)
                    (inst jalr zero-tn ra 0))))))
  (frob))

;;; Do unknown-values return of an arbitrary number of values (passed
;;; on the stack.)  We check for the common case of a single return
;;; value, and do that inline using the normal single value return
;;; convention.  Otherwise, we branch off to code that calls an
;;; assembly-routine.
(define-vop (return-multiple)
  (:args (old-fp-arg :scs (any-reg) :target old-fp)
         (return-pc :scs (any-reg) :target ra)
         (vals-arg :scs (any-reg) :target vals)
         (nvals-arg :scs (any-reg) :target nvals))
  (:temporary (:sc any-reg :offset nl1-offset :from (:argument 0)) old-fp)
  (:temporary (:sc any-reg :offset ra-offset :from (:argument 1)) ra)
  (:temporary (:sc any-reg :offset nl0-offset :from (:argument 2)) vals)
  (:temporary (:sc any-reg :offset nargs-offset :from (:argument 3)) nvals)
  (:temporary (:scs (descriptor-reg) :offset a0-offset) a0)
  (:temporary (:scs (descriptor-reg) :offset a1-offset) a1)
  (:vop-var vop)
  (:generator 13
    ;; Clear the number stack.
    (clear-number-stack vop)
    ;; Load the first argument. It's common to both cases.
    (loadw a0 vals-arg)
    (inst li a1 (fixnumize 1))
    ;; Check for the single case.
    (inst bne nvals-arg a1 NOT-SINGLE)

    ;; Return with one value.
    (move csp-tn cfp-tn)
    (move cfp-tn old-fp-arg)
    (inst li nargs-tn -1) ; mark single value return
    (move ra return-pc)
    (inst jalr zero-tn ra 0)

    NOT-SINGLE
    (move old-fp old-fp-arg)
    (move ra return-pc)
    (move vals vals-arg)
    (move nvals nvals-arg)

    (inst jal zero-tn (make-fixup 'return-multiple :assembly-routine))))


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


;;; More args are stored consecutively on the stack, starting
;;; immediately at the context pointer.  The context pointer is not
;;; typed, so the lowtag is 0.
(define-full-reffer more-arg * 0 0 (descriptor-reg any-reg) * %more-arg)

;;; Copy a more arg from the argument area to the end of the current frame.
;;; Fixed is the number of non-more arguments.
(define-vop (copy-more-arg)
  (:temporary (:sc any-reg :offset nl0-offset) result)
  (:temporary (:sc any-reg :offset nl1-offset) count)
  (:temporary (:sc any-reg :offset nl3-offset) dest)
  (:temporary (:sc descriptor-reg :offset l0-offset) temp)
  ;; EXTRA-TEMP can possibly be eliminated. But since TEMP is wired to L0, it is an
  ;; arg-passing reg which gets utilized in (NTH I *REGISTER-ARG-TNS*) and so
  ;; should not be clobbered with random data there. I think.
  (:temporary (:sc unsigned-reg) extra-temp)
  (:vop-var vop)
  (:info fixed)
  (:generator 20
    (let ((loop (gen-label))
          (do-regs (gen-label))
          (done (gen-label))
          (delta (- (sb-allocated-size 'control-stack) fixed)))
      ;; Compute the end of the fixed stack frame (start of the MORE arg
      ;; area) into RESULT.
      (add-imm result cfp-tn
            (* n-word-bytes (sb-allocated-size 'control-stack)) 'copy-more-arg extra-temp)
      ;; Compute the end of the MORE arg area (and our overall frame
      ;; allocation) into the stack pointer.
      (cond ((zerop fixed)
             (let ((nargs nargs-tn))
               (with-fixnum-as-word-index (nargs dest)
                 (inst add dest result nargs)))
             (move csp-tn dest)
             (inst beq nargs-tn zero-tn done))
            (t
             (inst subi count nargs-tn (fixnumize fixed))
             (let ((skip (gen-label)))
               (inst blt zero-tn count skip)
               (move csp-tn result)
               (inst j done)
               (emit-label skip))
             (with-fixnum-as-word-index (count dest)
               (inst add dest result count))
             ;; Don't leave the arguments unprotected when moving below the stack pointer
             (when (>= delta 0)
               (move csp-tn dest))))
      ;; Allocate the space on the stack.
      (when (< fixed register-arg-count)
        ;; We must stop when we run out of stack args, not when we run
        ;; out of more args.
        (inst addi result result (* (- register-arg-count fixed) n-word-bytes)))

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
      (emit-label loop)
      (let ((done (gen-label)))
        (cond ((zerop delta)
               ;; If DELTA is zero then all of the MORE args are in the
               ;; right place, so we don't really need to do anything.)
               )
              ((plusp delta)
               ;; If DELTA is positive then the allocated stack frame is
               ;; overlapping some of the MORE args, and we need to copy
               ;; the list starting from the end (so that we don't
               ;; overwrite any elements before they're copied).
               (inst bge result dest do-regs)
               (load-frame-word temp dest (- (1+ delta)) 'copy-more-arg extra-temp)
               (storew temp dest -1)
               (inst subi dest dest n-word-bytes)
               (inst j loop))
              (t
               ;; If DELTA is negative then the start of the MORE args
               ;; is beyond the end of the allocated stack frame, and we
               ;; need to copy the list from the start in order to close
               ;; the gap.
               (inst bge result dest done)
               ;; Unlike above, this LOADW can't have IMM that is too large
               (loadw temp result (- delta))
               (storew temp result 0)
               (inst addi result result n-word-bytes)
               (inst j loop)

               (emit-label done)
               (move csp-tn dest))))
      (emit-label do-regs)
      (when (< fixed register-arg-count)
        ;; Now we have to deposit any more args that showed up in registers.
        (when (zerop fixed)
          (move count nargs-tn))
        (do ((i fixed (1+ i)))
            ((>= i register-arg-count))
          ;; Don't deposit any more than there are.
          (inst beq count zero-tn done)
          (inst subi count count (fixnumize 1))
          ;; Store it into the space reserved to it, by displacement
          ;; from the frame pointer.
          (store-frame-word (nth i *register-arg-tns*) cfp-tn
                  (+ (sb-allocated-size 'control-stack)
                     (- i fixed))
                  'copy-more-arg extra-temp)))
      (emit-label done)
      ;; Now that we're done with the &MORE args, we can set up the
      ;; number stack frame.
      (let ((cur-nfp (current-nfp-tn vop)))
        (when cur-nfp
          (inst subi nsp-tn nsp-tn (bytes-needed-for-non-descriptor-stack-frame))
          (move cur-nfp nsp-tn))))))

(define-vop (more-arg-or-nil)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:result 1))
         (count :scs (any-reg) :to (:result 1)))
  (:temporary (:scs (any-reg)) index-temp)
  (:info index)
  (:results (value :scs (descriptor-reg any-reg)))
  (:result-types *)
  (:generator 3
    (move value null-tn)
    (cond ((zerop index)
           (inst beq count zero-tn done))
          (t
           (inst li index-temp (fixnumize index))
           (inst bge index-temp count done)))
    (loadw value object index)
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
  (:temporary (:sc non-descriptor-reg) pa-flag)
  (:results (result :scs (descriptor-reg)))
  (:translate %listify-rest-args)
  (:policy :safe)
  (:node-var node)
  (:generator 20
    (let* ((enter (gen-label))
           (loop (gen-label))
           (done (gen-label))
           (dx-p (node-stack-allocate-p node)))
      (move context context-arg)
      (move count count-arg)
      ;; Check to see if there are any arguments.
      (move result null-tn)
      (inst beq count zero-tn done)

      ;; We need to do this atomically.
      (pseudo-atomic (pa-flag)
        (inst slli temp count (1+ (- word-shift n-fixnum-tag-bits)))
        (allocation 'list temp list-pointer-lowtag dst
                    :flag-tn pa-flag
                    :stack-allocate-p dx-p)
        (move result dst)
        (inst j enter)

        ;; Compute the next cons and store it in the current one.
        (emit-label loop)
        (inst addi dst dst (* cons-size n-word-bytes))
        (storew dst dst -1 list-pointer-lowtag)

        (emit-label enter)
        ;; Grab one value
        (loadw temp context)
        (inst addi context context n-word-bytes)
        ;; Store the value into the car of the current cons.
        (storew temp dst 0 list-pointer-lowtag)
        ;; Dec count, and if != zero, go back for more.
        (inst subi count count (fixnumize 1))
        (inst bne count zero-tn loop)

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
(define-vop ()
  (:policy :fast-safe)
  (:translate sb-c::%more-arg-context)
  (:args (supplied :scs (any-reg) .
         #.(cl:when sb-vm::fixnum-as-word-index-needs-temp
             '(:target temp))))
  (:arg-types tagged-num (:constant fixnum))
  (:info fixed)
  (:results (context :scs (descriptor-reg))
            (count :scs (any-reg)))
  #+#.(cl:if sb-vm::fixnum-as-word-index-needs-temp '(and) '(or))
  (:temporary (:sc non-descriptor-reg) temp)
  (:result-types t tagged-num)
  (:note "more-arg-context")
  (:generator 5
    (inst subi count supplied (fixnumize fixed))
    (with-fixnum-as-word-index (count temp)
      (inst sub context csp-tn count))))

(define-vop (verify-arg-count)
  (:policy :fast-safe)
  (:args (nargs :scs (any-reg)))
  (:temporary (:scs (unsigned-reg)) temp)
  (:info min max)
  (:vop-var vop)
  (:arg-types positive-fixnum (:constant t) (:constant t))
  (:save-p :compute-only)
  (:generator 3
    (let ((err-lab
            (generate-error-code vop 'invalid-arg-count-error nargs)))
      (cond ((not min)
             (cond ((zerop max)
                    (inst bne nargs zero-tn err-lab))
                   (t
                    (inst li temp (fixnumize max))
                    (inst bne nargs temp err-lab))))
            (max
             (when (plusp min)
               (inst li temp (fixnumize min))
               (inst bltu nargs temp err-lab))
             (inst li temp (fixnumize max))
             (inst bltu temp nargs err-lab))
            ((plusp min)
             (inst li temp (fixnumize min))
             (inst bltu nargs temp err-lab))))))

(define-vop (step-instrument-before-vop)
  (:temporary (:scs (descriptor-reg)) stepping)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 3
    (load-stepping stepping)
    ;; If it's not 0, trap.
    (inst beq stepping zero-tn DONE)
    ;; CONTEXT-PC will be pointing here when the interrupt is handled,
    ;; not after the EBREAK.
    (note-this-location vop :internal-error)
    ;; CALLEE-REGISTER-OFFSET isn't needed for before-traps, so we
    ;; can just use a bare SINGLE-STEP-BEFORE-TRAP as the code.
    (inst ebreak single-step-before-trap)
    (emit-alignment 2)
    DONE))
