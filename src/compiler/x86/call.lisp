;;;; function call for the x86 VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; interfaces to IR2 conversion

;;; Return a wired TN describing the N'th full call argument passing
;;; location.
(def-vm-support-routine standard-argument-location (n)
  (declare (type unsigned-byte n))
  (if (< n register-arg-count)
      (make-wired-tn *backend-t-primitive-type* descriptor-reg-sc-number
		     (nth n *register-arg-offsets*))
      (make-wired-tn *backend-t-primitive-type* control-stack-sc-number n)))

;;; Make a passing location TN for a local call return PC.
;;;
;;; Always wire the return PC location to the stack in its standard
;;; location.
;;;
;;; No problems.
;#+nil
(def-vm-support-routine make-return-pc-passing-location (standard)
  (declare (ignore standard))
  (make-wired-tn (primitive-type-or-lose 'system-area-pointer)
		 sap-stack-sc-number return-pc-save-offset))
;;; If standard is true, then use the standard (full call) location,
;;; otherwise use any legal location.
;;;
;;; No problems.
#+nil
(def-vm-support-routine make-return-pc-passing-location (standard)
  (let ((ptype (primitive-type-or-lose 'system-area-pointer)))
    (if standard
	(make-wired-tn ptype sap-stack-sc-number return-pc-save-offset)
	(make-normal-tn ptype))))

;;; Similar to Make-Return-PC-Passing-Location, but makes a location to pass
;;; Old-FP in.
;;;
;;; This is wired in both the standard and the local-call
;;; conventions, because we want to be able to assume it's always there.
;;; Besides, the x86 doesn't have enough registers to really make it
;;; profitable to pass it in a register.
;;;
;;; No problems
;#+nil
(def-vm-support-routine make-old-fp-passing-location (standard)
  (declare (ignore standard))
  (make-wired-tn *fixnum-primitive-type* control-stack-sc-number
		 ocfp-save-offset))
;;; If standard is true, then use the standard (full call) location,
;;; otherwise use any legal location.
;;;
;;; No problems.
#+nil
(def-vm-support-routine make-old-fp-passing-location (standard)
  (if standard
      (make-wired-tn *fixnum-primitive-type* control-stack-sc-number
		     ocfp-save-offset)
      (make-normal-tn *fixnum-primitive-type*)))

;;; Make the TNs used to hold Old-FP and Return-PC within the current
;;; function. We treat these specially so that the debugger can find them at a
;;; known location.
;;;
;;; Without using a save-tn - which does not make much sense if it is
;;; wire to the stack? No problems.
(def-vm-support-routine make-old-fp-save-location (env)
  (environment-debug-live-tn (make-wired-tn *fixnum-primitive-type*
					    control-stack-sc-number
					    ocfp-save-offset)
			     env))
;;; Using a save-tn. No problems.
#+nil
(def-vm-support-routine make-old-fp-save-location (env)
  (specify-save-tn
   (environment-debug-live-tn (make-normal-tn *fixnum-primitive-type*) env)
   (make-wired-tn *fixnum-primitive-type* control-stack-sc-number
		  ocfp-save-offset)))

;;; Without using a save-tn - which does not make much sense if it is
;;; wire to the stack? No problems.
(def-vm-support-routine make-return-pc-save-location (env)
  (environment-debug-live-tn
   (make-wired-tn (primitive-type-or-lose 'system-area-pointer)
		  sap-stack-sc-number return-pc-save-offset)
   env))
;;; Using a save-tn. No problems.
#+nil
(def-vm-support-routine make-return-pc-save-location (env)
  (let ((ptype (primitive-type-or-lose 'system-area-pointer)))
    (specify-save-tn
     (environment-debug-live-tn (make-normal-tn ptype) env)
     (make-wired-tn ptype sap-stack-sc-number return-pc-save-offset))))

;;; Make a TN for the standard argument count passing location. We only
;;; need to make the standard location, since a count is never passed when we
;;; are using non-standard conventions.
(def-vm-support-routine make-argument-count-location ()
  (make-wired-tn *fixnum-primitive-type* any-reg-sc-number ecx-offset))


;;; Make a TN to hold the number-stack frame pointer. This is allocated
;;; once per component, and is component-live.
(def-vm-support-routine make-nfp-tn ()
  (make-restricted-tn *fixnum-primitive-type* ignore-me-sc-number))

(def-vm-support-routine make-stack-pointer-tn ()
  (make-normal-tn *fixnum-primitive-type*))

(def-vm-support-routine make-number-stack-pointer-tn ()
  (make-restricted-tn *fixnum-primitive-type* ignore-me-sc-number))

;;; Return a list of TNs that can be used to represent an unknown-values
;;; continuation within a function.
(def-vm-support-routine make-unknown-values-locations ()
  (list (make-stack-pointer-tn)
	(make-normal-tn *fixnum-primitive-type*)))


;;; This function is called by the Entry-Analyze phase, allowing
;;; VM-dependent initialization of the IR2-Component structure. We push
;;; placeholder entries in the Constants to leave room for additional
;;; noise in the code object header.
;;;
;;; For the x86 the first constant is a pointer to a list of fixups,
;;; or nil if the code object has none.
(def-vm-support-routine select-component-format (component)
  (declare (type component component))
  (dotimes (i (1+ code-constants-offset))
    (vector-push-extend nil
			(ir2-component-constants (component-info component))))
  (values))

;;;; frame hackery

;;; Used for setting up the Old-FP in local call.
(define-vop (current-fp)
  (:results (val :scs (any-reg control-stack)))
  (:generator 1
    (move val ebp-tn)))

;;; We don't have a separate NFP, so we don't need to do anything here.
(define-vop (compute-old-nfp)
  (:results (val))
  (:ignore val)
  (:generator 1
    nil))


(define-vop (xep-allocate-frame)
  (:info start-lab copy-more-arg-follows)
  (:vop-var vop)
  (:generator 1
    ;; Make sure the function is aligned, and drop a label pointing to this
    ;; function header.
    (align lowtag-bits)
    (trace-table-entry trace-table-function-prologue)
    (emit-label start-lab)
    ;; Skip space for the function header.
    (inst function-header-word)
    (dotimes (i (1- sb!vm:function-code-offset))
      (inst dword 0))

    ;; The start of the actual code.
    ;; Save the return-pc.
    (popw ebp-tn (- (1+ return-pc-save-offset)))

    ;; If copy-more-arg follows it will allocate the correct stack
    ;; size. The stack is not allocated first here as this may expose
    ;; args on the stack if they take up more space than the frame!
    (unless copy-more-arg-follows
      ;; The args fit within the frame so just allocate the frame.
      (inst lea esp-tn
	    (make-ea :dword :base ebp-tn
		     :disp (- (* sb!vm:word-bytes
				 (max 3 (sb-allocated-size 'stack)))))))

    (trace-table-entry trace-table-normal)))

;;; This is emitted directly before either a known-call-local, call-local,
;;; or a multiple-call-local. All it does is allocate stack space for the
;;; callee (who has the same size stack as us).
(define-vop (allocate-frame)
  (:results (res :scs (any-reg control-stack))
	    (nfp))
  (:info callee)
  (:ignore nfp callee)
  (:generator 2
    (move res esp-tn)
    (inst sub esp-tn (* sb!vm:word-bytes (sb-allocated-size 'stack)))))

;;; Allocate a partial frame for passing stack arguments in a full call. Nargs
;;; is the number of arguments passed. We allocate at least 3 slots, because
;;; the XEP noise is going to want to use them before it can extend the stack.
(define-vop (allocate-full-call-frame)
  (:info nargs)
  (:results (res :scs (any-reg control-stack)))
  (:generator 2
    (move res esp-tn)
    (inst sub esp-tn (* (max nargs 3) sb!vm:word-bytes))))



;;; Emit code needed at the return-point from an unknown-values call for a
;;; fixed number of values. Values is the head of the TN-Ref list for the
;;; locations that the values are to be received into. Nvals is the number of
;;; values that are to be received (should equal the length of Values).
;;;
;;; Move-Temp is a Descriptor-Reg TN used as a temporary.
;;;
;;; This code exploits the fact that in the unknown-values convention, a
;;; single value return returns at the return PC + 2, whereas a return of other
;;; than one value returns directly at the return PC.
;;;
;;; If 0 or 1 values are expected, then we just emit an instruction to reset
;;; the SP (which will only be executed when other than 1 value is returned.)
;;;
;;; In the general case we have to do three things:
;;;  -- Default unsupplied register values. This need only be done when a
;;;     single value is returned, since register values are defaulted by the
;;;     called in the non-single case.
;;;  -- Default unsupplied stack values. This needs to be done whenever there
;;;     are stack values.
;;;  -- Reset SP. This must be done whenever other than 1 value is returned,
;;;     regardless of the number of values desired.
(defun default-unknown-values (vop values nvals)
  (declare (type (or tn-ref null) values)
	   (type unsigned-byte nvals))
  (cond
   ((<= nvals 1)
    (note-this-location vop :single-value-return)
    (inst mov esp-tn ebx-tn))
   ((<= nvals register-arg-count)
    (let ((regs-defaulted (gen-label)))
      (note-this-location vop :unknown-return)
      (inst jmp-short regs-defaulted)
      ;; Default the unsuppled registers.
      (let* ((2nd-tn-ref (tn-ref-across values))
	     (2nd-tn (tn-ref-tn 2nd-tn-ref)))
	(inst mov 2nd-tn nil-value)
	(when (> nvals 2)
	  (loop
	    for tn-ref = (tn-ref-across 2nd-tn-ref)
	    then (tn-ref-across tn-ref)
	    for count from 2 below register-arg-count
	    do count (inst mov (tn-ref-tn tn-ref) 2nd-tn))))
      (inst mov ebx-tn esp-tn)
      (emit-label regs-defaulted)
      (inst mov esp-tn ebx-tn)))
   ((<= nvals 7)
    ;; Number of bytes depends on the relative jump instructions. Best
    ;; case is 31+(n-3)*14, worst case is 35+(n-3)*18. For nvals=6
    ;; that is 73/89 bytes, and for nvals=7 that is 87/107 bytes which
    ;; is likely better than using the blt below.
    (let ((regs-defaulted (gen-label))
	  (defaulting-done (gen-label))
	  (default-stack-slots (gen-label)))
      (note-this-location vop :unknown-return)
      ;; Branch off to the MV case.
      (inst jmp-short regs-defaulted)
      ;; Do the single value case.
      ;; Default the register args
      (inst mov eax-tn nil-value)
      (do ((i 1 (1+ i))
	   (val (tn-ref-across values) (tn-ref-across val)))
	  ((= i (min nvals register-arg-count)))
	(inst mov (tn-ref-tn val) eax-tn))

      ;; Fake other registers so it looks like we returned with all the
      ;; registers filled in.
      (move ebx-tn esp-tn)
      (inst push edx-tn)
      (inst jmp default-stack-slots)

      (emit-label regs-defaulted)

      (inst mov eax-tn nil-value)
      (storew edx-tn ebx-tn -1)
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

	    (inst cmp ecx-tn (fixnumize i))
	    (inst jmp :be default-lab)
	    (loadw edx-tn ebx-tn (- (1+ i)))
	    (inst mov tn edx-tn)))

	(emit-label defaulting-done)
	(loadw edx-tn ebx-tn -1)
	(move esp-tn ebx-tn)

	(let ((defaults (defaults)))
	  (when defaults
	    (assemble (*elsewhere*)
	      (trace-table-entry trace-table-function-prologue)
	      (emit-label default-stack-slots)
	      (dolist (default defaults)
		(emit-label (car default))
		(inst mov (cdr default) eax-tn))
	      (inst jmp defaulting-done)
	      (trace-table-entry trace-table-normal)))))))
   (t
    ;; 91 bytes for this branch.
    (let ((regs-defaulted (gen-label))
	  (restore-edi (gen-label))
	  (no-stack-args (gen-label))
	  (default-stack-vals (gen-label))
	  (count-okay (gen-label)))
      (note-this-location vop :unknown-return)
      ;; Branch off to the MV case.
      (inst jmp-short regs-defaulted)

      ;; Default the register args, and set up the stack as if we entered
      ;; the MV return point.
      (inst mov ebx-tn esp-tn)
      (inst push edx-tn)
      (inst mov edi-tn nil-value)
      (inst push edi-tn)
      (inst mov esi-tn edi-tn)
      ;; Compute a pointer to where to put the [defaulted] stack values.
      (emit-label no-stack-args)
      (inst lea edi-tn
	    (make-ea :dword :base ebp-tn
		     :disp (* (- (1+ register-arg-count)) word-bytes)))
      ;; Load EAX with NIL so we can quickly store it, and set up stuff
      ;; for the loop.
      (inst mov eax-tn nil-value)
      (inst std)
      (inst mov ecx-tn (- nvals register-arg-count))
      ;; Jump into the default loop.
      (inst jmp default-stack-vals)

      ;; The regs are defaulted. We need to copy any stack arguments,
      ;; and then default the remaining stack arguments.
      (emit-label regs-defaulted)
      ;; Save EDI.
      (storew edi-tn ebx-tn (- (1+ 1)))
      ;; Compute the number of stack arguments, and if it's zero or less,
      ;; don't copy any stack arguments.
      (inst sub ecx-tn (fixnumize register-arg-count))
      (inst jmp :le no-stack-args)

      ;; Throw away any unwanted args.
      (inst cmp ecx-tn (fixnumize (- nvals register-arg-count)))
      (inst jmp :be count-okay)
      (inst mov ecx-tn (fixnumize (- nvals register-arg-count)))
      (emit-label count-okay)
      ;; Save the number of stack values.
      (inst mov eax-tn ecx-tn)
      ;; Compute a pointer to where the stack args go.
      (inst lea edi-tn
	    (make-ea :dword :base ebp-tn
		     :disp (* (- (1+ register-arg-count)) word-bytes)))
      ;; Save ESI, and compute a pointer to where the args come from.
      (storew esi-tn ebx-tn (- (1+ 2)))
      (inst lea esi-tn
	    (make-ea :dword :base ebx-tn
		     :disp (* (- (1+ register-arg-count)) word-bytes)))
      ;; Do the copy.
      (inst shr ecx-tn word-shift)		; make word count
      (inst std)
      (inst rep)
      (inst movs :dword)
      ;; Restore ESI.
      (loadw esi-tn ebx-tn (- (1+ 2)))
      ;; Now we have to default the remaining args. Find out how many.
      (inst sub eax-tn (fixnumize (- nvals register-arg-count)))
      (inst neg eax-tn)
      ;; If none, then just blow out of here.
      (inst jmp :le restore-edi)
      (inst mov ecx-tn eax-tn)
      (inst shr ecx-tn word-shift)	; word count
      ;; Load EAX with NIL for fast storing.
      (inst mov eax-tn nil-value)
      ;; Do the store.
      (emit-label default-stack-vals)
      (inst rep)
      (inst stos eax-tn)
      ;; Restore EDI, and reset the stack.
      (emit-label restore-edi)
      (loadw edi-tn ebx-tn (- (1+ 1)))
      (inst mov esp-tn ebx-tn))))
  (values))

;;;; unknown values receiving

;;; Emit code needed at the return point for an unknown-values call for an
;;; arbitrary number of values.
;;;
;;; We do the single and non-single cases with no shared code: there doesn't
;;; seem to be any potential overlap, and receiving a single value is more
;;; important efficiency-wise.
;;;
;;; When there is a single value, we just push it on the stack, returning
;;; the old SP and 1.
;;;
;;; When there is a variable number of values, we move all of the argument
;;; registers onto the stack, and return Args and Nargs.
;;;
;;; Args and Nargs are TNs wired to the named locations. We must
;;; explicitly allocate these TNs, since their lifetimes overlap with the
;;; results Start and Count (also, it's nice to be able to target them).
(defun receive-unknown-values (args nargs start count)
  (declare (type tn args nargs start count))
  (let ((variable-values (gen-label))
	(done (gen-label)))
    (inst jmp-short variable-values)

    (inst mov start esp-tn)
    (inst push (first *register-arg-tns*))
    (inst mov count (fixnumize 1))
    (inst jmp done)

    (emit-label variable-values)
    ;; dtc: this writes the registers onto the stack even if they are
    ;; not needed, only the number specified in ecx are used and have
    ;; stack allocated to them. No harm is done.
    (loop
      for arg in *register-arg-tns*
      for i downfrom -1
      do (storew arg args i))
    (move start args)
    (move count nargs)

    (emit-label done))
  (values))

;;; VOP that can be inherited by unknown values receivers. The main thing this
;;; handles is allocation of the result temporaries.
(define-vop (unknown-values-receiver)
  (:temporary (:sc descriptor-reg :offset ebx-offset
		   :from :eval :to (:result 0))
	      values-start)
  (:temporary (:sc any-reg :offset ecx-offset
	       :from :eval :to (:result 1))
	      nvals)
  (:results (start :scs (any-reg control-stack))
	    (count :scs (any-reg control-stack))))

;;;; local call with unknown values convention return

;;; Non-TR local call for a fixed number of values passed according to the
;;; unknown values convention.
;;;
;;; FP is the frame pointer in install before doing the call.
;;;
;;; NFP would be the number-stack frame pointer if we had a separate number
;;; stack.
;;;
;;; Args are the argument passing locations, which are specified only to
;;; terminate their lifetimes in the caller.
;;;
;;; Values are the return value locations (wired to the standard passing
;;; locations).
;;; Nvals is the number of values received.
;;;
;;; Save is the save info, which we can ignore since saving has been done.
;;;
;;; Target is a continuation pointing to the start of the called function.
(define-vop (call-local)
  (:args (fp)
	 (nfp)
	 (args :more t))
  (:results (values :more t))
  (:save-p t)
  (:move-args :local-call)
  (:info arg-locs callee target nvals)
  (:vop-var vop)
  (:ignore nfp arg-locs args #+nil callee)
  (:generator 5
    (trace-table-entry trace-table-call-site)
    (move ebp-tn fp)

    (let ((ret-tn (callee-return-pc-tn callee)))
      #+nil
      (format t "*call-local ~S; tn-kind ~S; tn-save-tn ~S; its tn-kind ~S~%"
	      ret-tn (sb!c::tn-kind ret-tn) (sb!c::tn-save-tn ret-tn)
	      (sb!c::tn-kind (sb!c::tn-save-tn ret-tn)))

      ;; Is the return-pc on the stack or in a register?
      (sc-case ret-tn
	((sap-stack)
	 #+nil (format t "*call-local: ret-tn on stack; offset=~S~%"
		       (tn-offset ret-tn))
	 (storew (make-fixup nil :code-object return)
		 ebp-tn (- (1+ (tn-offset ret-tn)))))
	((sap-reg)
	 (inst lea ret-tn (make-fixup nil :code-object return)))))

    (note-this-location vop :call-site)
    (inst jmp target)
    RETURN
    (default-unknown-values vop values nvals)
    (trace-table-entry trace-table-normal)))

;;; Non-TR local call for a variable number of return values passed according
;;; to the unknown values convention. The results are the start of the values
;;; glob and the number of values received.
(define-vop (multiple-call-local unknown-values-receiver)
  (:args (fp)
	 (nfp)
	 (args :more t))
  (:save-p t)
  (:move-args :local-call)
  (:info save callee target)
  (:ignore args save nfp #+nil callee)
  (:vop-var vop)
  (:generator 20
    (trace-table-entry trace-table-call-site)
    (move ebp-tn fp)

    (let ((ret-tn (callee-return-pc-tn callee)))
      #+nil
      (format t "*multiple-call-local ~S; tn-kind ~S; tn-save-tn ~S; its tn-kind ~S~%"
	      ret-tn (sb!c::tn-kind ret-tn) (sb!c::tn-save-tn ret-tn)
	      (sb!c::tn-kind (sb!c::tn-save-tn ret-tn)))

      ;; Is the return-pc on the stack or in a register?
      (sc-case ret-tn
	((sap-stack)
	 #+nil (format t "*multiple-call-local: ret-tn on stack; offset=~S~%"
		       (tn-offset ret-tn))
	 ;; Stack
	 (storew (make-fixup nil :code-object return)
		 ebp-tn (- (1+ (tn-offset ret-tn)))))
	((sap-reg)
	 ;; Register
	 (inst lea ret-tn (make-fixup nil :code-object return)))))

    (note-this-location vop :call-site)
    (inst jmp target)
    RETURN
    (note-this-location vop :unknown-return)
    (receive-unknown-values values-start nvals start count)
    (trace-table-entry trace-table-normal)))

;;;; local call with known values return

;;; Non-TR local call with known return locations. Known-value return works
;;; just like argument passing in local call.
;;;
;;; Note: we can't use normal load-tn allocation for the fixed args, since all
;;; registers may be tied up by the more operand. Instead, we use
;;; MAYBE-LOAD-STACK-TN.
(define-vop (known-call-local)
  (:args (fp)
	 (nfp)
	 (args :more t))
  (:results (res :more t))
  (:move-args :local-call)
  (:save-p t)
  (:info save callee target)
  (:ignore args res save nfp #+nil callee)
  (:vop-var vop)
  (:generator 5
    (trace-table-entry trace-table-call-site)
    (move ebp-tn fp)

    (let ((ret-tn (callee-return-pc-tn callee)))

      #+nil
      (format t "*known-call-local ~S; tn-kind ~S; tn-save-tn ~S; its tn-kind ~S~%"
	      ret-tn (sb!c::tn-kind ret-tn) (sb!c::tn-save-tn ret-tn)
	      (sb!c::tn-kind (sb!c::tn-save-tn ret-tn)))

      ;; Is the return-pc on the stack or in a register?
      (sc-case ret-tn
	((sap-stack)
	 #+nil (format t "*known-call-local: ret-tn on stack; offset=~S~%"
		       (tn-offset ret-tn))
	 ;; Stack
	 (storew (make-fixup nil :code-object return)
		 ebp-tn (- (1+ (tn-offset ret-tn)))))
	((sap-reg)
	 ;; Register
	 (inst lea ret-tn (make-fixup nil :code-object return)))))

    (note-this-location vop :call-site)
    (inst jmp target)
    RETURN
    (note-this-location vop :known-return)
    (trace-table-entry trace-table-normal)))

;;; Return from known values call. We receive the return locations as
;;; arguments to terminate their lifetimes in the returning function. We
;;; restore FP and CSP and jump to the Return-PC.
;;;
;;; We can assume we know exactly where old-fp and return-pc are because
;;; make-old-fp-save-location and make-return-pc-save-location always
;;; return the same place.
#+nil
(define-vop (known-return)
  (:args (old-fp)
	 (return-pc :scs (any-reg immediate-stack) :target rpc)
	 (vals :more t))
  (:move-args :known-return)
  (:info val-locs)
  (:temporary (:sc unsigned-reg :from (:argument 1)) rpc)
  (:ignore val-locs vals)
  (:vop-var vop)
  (:generator 6
    (trace-table-entry trace-table-function-epilogue)
    ;; Save the return-pc in a register 'cause the frame-pointer is going away.
    ;; Note this not in the usual stack location so we can't use RET
    (move rpc return-pc)
    ;; Restore the stack.
    (move esp-tn ebp-tn)
    ;; Restore the old fp. We know OLD-FP is going to be in its stack
    ;; save slot, which is a different frame that than this one,
    ;; so we don't have to worry about having just cleared
    ;; most of the stack.
    (move ebp-tn old-fp)
    (inst jmp rpc)
    (trace-table-entry trace-table-normal)))

;;; From Douglas Crosher
;;; Return from known values call. We receive the return locations as
;;; arguments to terminate their lifetimes in the returning function. We
;;; restore FP and CSP and jump to the Return-PC.
;;;
;;; The old-fp may be either in a register or on the stack in its
;;; standard save locations - slot 0.
;;;
;;; The return-pc may be in a register or on the stack in any slot.
(define-vop (known-return)
  (:args (old-fp)
	 (return-pc)
	 (vals :more t))
  (:move-args :known-return)
  (:info val-locs)
  (:ignore val-locs vals)
  (:vop-var vop)
  (:generator 6
    (trace-table-entry trace-table-function-epilogue)

    #+nil (format t "*known-return: old-fp ~S, tn-kind ~S; ~S ~S~%"
		  old-fp (sb!c::tn-kind old-fp) (sb!c::tn-save-tn old-fp)
		  (sb!c::tn-kind (sb!c::tn-save-tn old-fp)))

    #+nil (format t "*known-return: return-pc ~S, tn-kind ~S; ~S ~S~%"
		  return-pc (sb!c::tn-kind return-pc) (sb!c::tn-save-tn return-pc)
		  (sb!c::tn-kind (sb!c::tn-save-tn return-pc)))

    ;; return-pc may be either in a register or on the stack.
    (sc-case return-pc
      ((sap-reg)
       (sc-case old-fp
	 ((control-stack)

	  #+nil (format t "*known-return: old-fp ~S on stack; offset=~S~%"
			old-fp (tn-offset old-fp))

	  (cond ((zerop (tn-offset old-fp))
		 ;; Zot all of the stack except for the old-fp.
		 (inst lea esp-tn (make-ea :dword :base ebp-tn
					   :disp (- (* (1+ ocfp-save-offset)
						       word-bytes))))
		 ;; Restore the old fp from its save location on the stack,
		 ;; and zot the stack.
		 (inst pop ebp-tn))

		(t
		 (cerror "Continue any-way"
			 "VOP return-local doesn't work if old-fp (in slot %s) is not in slot 0"
			 (tn-offset old-fp)))))

	 ((any-reg descriptor-reg)
	  ;; Zot all the stack.
	  (move esp-tn ebp-tn)
	  ;; Restore the old-fp.
	  (move ebp-tn old-fp)))

       ;; Return; return-pc is in a register.
       (inst jmp return-pc))

      ((sap-stack)

       #+nil (format t "*known-return: return-pc ~S on stack; offset=~S~%"
		     return-pc (tn-offset return-pc))

       ;; Zot all of the stack except for the old-fp and return-pc.
       (inst lea esp-tn
	     (make-ea :dword :base ebp-tn
		      :disp (- (* (1+ (tn-offset return-pc)) word-bytes))))
       ;; Restore the old fp. old-fp may be either on the stack in its
       ;; save location or in a register, in either case this restores it.
       (move ebp-tn old-fp)
       ;; The return pops the return address (4 bytes), then we need
       ;; to pop all the slots before the return-pc which includes the
       ;; 4 bytes for the old-fp.
       (inst ret (* (tn-offset return-pc) word-bytes))))

    (trace-table-entry trace-table-normal)))

;;;; full call
;;;
;;;    There is something of a cross-product effect with full calls. Different
;;; versions are used depending on whether we know the number of arguments or
;;; the name of the called function, and whether we want fixed values, unknown
;;; values, or a tail call.
;;;
;;; In full call, the arguments are passed creating a partial frame on the
;;; stack top and storing stack arguments into that frame. On entry to the
;;; callee, this partial frame is pointed to by FP.

;;; This macro helps in the definition of full call VOPs by avoiding code
;;; replication in defining the cross-product VOPs.
;;;
;;; Name is the name of the VOP to define.
;;;
;;; Named is true if the first argument is an fdefinition object whose
;;; definition is to be called.
;;;
;;; Return is either :Fixed, :Unknown or :Tail:
;;; -- If :Fixed, then the call is for a fixed number of values, returned in
;;;    the standard passing locations (passed as result operands).
;;; -- If :Unknown, then the result values are pushed on the stack, and the
;;;    result values are specified by the Start and Count as in the
;;;    unknown-values continuation representation.
;;; -- If :Tail, then do a tail-recursive call. No values are returned.
;;;    The Old-Fp and Return-PC are passed as the second and third arguments.
;;;
;;; In non-tail calls, the pointer to the stack arguments is passed as the last
;;; fixed argument. If Variable is false, then the passing locations are
;;; passed as a more arg. Variable is true if there are a variable number of
;;; arguments passed on the stack. Variable cannot be specified with :Tail
;;; return. TR variable argument call is implemented separately.
;;;
;;; In tail call with fixed arguments, the passing locations are passed as a
;;; more arg, but there is no new-FP, since the arguments have been set up in
;;; the current frame.
(macrolet ((define-full-call (name named return variable)
	    (assert (not (and variable (eq return :tail))))
	    `(define-vop (,name
			  ,@(when (eq return :unknown)
			      '(unknown-values-receiver)))
	       (:args
	       ,@(unless (eq return :tail)
		   '((new-fp :scs (any-reg) :to (:argument 1))))

	       (fun :scs (descriptor-reg control-stack)
		    :target eax :to (:argument 0))

	       ,@(when (eq return :tail)
		   '((old-fp)
		     (return-pc)))

	       ,@(unless variable '((args :more t :scs (descriptor-reg)))))

	       ,@(when (eq return :fixed)
	       '((:results (values :more t))))

	       (:save-p ,(if (eq return :tail) :compute-only t))

	       ,@(unless (or (eq return :tail) variable)
	       '((:move-args :full-call)))

	       (:vop-var vop)
	       (:info
	       ,@(unless (or variable (eq return :tail)) '(arg-locs))
	       ,@(unless variable '(nargs))
	       ,@(when (eq return :fixed) '(nvals)))

	       (:ignore
	       ,@(unless (or variable (eq return :tail)) '(arg-locs))
	       ,@(unless variable '(args)))

	       ;; We pass either the fdefn object (for named call) or the actual
	       ;; function object (for unnamed call) in EAX. With named call,
	       ;; closure-tramp will replace it with the real function and invoke
	       ;; the real function for closures. Non-closures do not need this
	       ;; value, so don't care what shows up in it.
	       (:temporary
	       (:sc descriptor-reg :offset eax-offset :from (:argument 0) :to :eval)
	       eax)

	       ;; We pass the number of arguments in ECX.
	       (:temporary (:sc unsigned-reg :offset ecx-offset :to :eval) ecx)

	       ;; With variable call, we have to load the register-args out
	       ;; of the (new) stack frame before doing the call. Therefore,
	       ;; we have to tell the lifetime stuff that we need to use them.
	       ,@(when variable
		   (mapcar #'(lambda (name offset)
			       `(:temporary (:sc descriptor-reg
						 :offset ,offset
						 :from (:argument 0)
						 :to :eval)
					    ,name))
			   *register-arg-names* *register-arg-offsets*))

	       ,@(when (eq return :tail)
		   '((:temporary (:sc unsigned-reg
				      :from (:argument 1)
				      :to (:argument 2))
				 old-fp-tmp)))

	       (:generator ,(+ (if named 5 0)
			       (if variable 19 1)
			       (if (eq return :tail) 0 10)
			       15
			       (if (eq return :unknown) 25 0))
	       (trace-table-entry trace-table-call-site)

	       ;; This has to be done before the frame pointer is changed!
	       ;; eax stores the 'lexical environment' needed for closures
	       (move eax fun)


	       ,@(if variable
		     ;; For variable call, compute the number of
		     ;; arguments and move some of the arguments to
		     ;; registers.
		     (collect ((noise))
			      ;; Compute the number of arguments.
			      (noise '(inst mov ecx new-fp))
			      (noise '(inst sub ecx esp-tn))
			      ;; Move the necessary args to registers,
			      ;; this moves them all even if they are
			      ;; not all needed.
			      (loop
			       for name in *register-arg-names*
			       for index downfrom -1
			       do (noise `(loadw ,name new-fp ,index)))
			      (noise))
		   '((if (zerop nargs)
			 (inst xor ecx ecx)
		       (inst mov ecx (fixnumize nargs)))))
	       ,@(cond ((eq return :tail)
			'(;; Python has figured out what frame we should
			  ;; return to so might as well use that clue.
			  ;; This seems really important to the
			  ;; implementation of things like
			  ;; (without-interrupts ...)
			  ;;
			  ;; dtc; Could be doing a tail call from a
			  ;; known-local-call etc in which the old-fp
			  ;; or ret-pc are in regs or in non-standard
			  ;; places. If the passing location were
			  ;; wired to the stack in standard locations
			  ;; then these moves will be un-necessary;
			  ;; this is probably best for the x86.
			  (sc-case old-fp
				   ((control-stack)
				    (unless (= ocfp-save-offset
					       (tn-offset old-fp))
				      ;; FIXME: FORMAT T for stale
				      ;; diagnostic output (several of
				      ;; them around here), ick
				      (format t "** tail-call old-fp not S0~%")
				      (move old-fp-tmp old-fp)
				      (storew old-fp-tmp
					      ebp-tn
					      (- (1+ ocfp-save-offset)))))
				   ((any-reg descriptor-reg)
				    (format t "** tail-call old-fp in reg not S0~%")
				    (storew old-fp
					    ebp-tn
					    (- (1+ ocfp-save-offset)))))

			  ;; For tail call, we have to push the
			  ;; return-pc so that it looks like we CALLed
			  ;; despite the fact that we are going to JMP.
			  (inst push return-pc)
			  ))
		       (t
			;; For non-tail call, we have to save our
			;; frame pointer and install the new frame
			;; pointer. We can't load stack tns after this
			;; point.
			`(;; Python doesn't seem to allocate a frame
			  ;; here which doesn't leave room for the
			  ;; ofp/ret stuff.
		
			  ;; The variable args are on the stack and
			  ;; become the frame, but there may be <3
			  ;; args and 3 stack slots are assumed
			  ;; allocate on the call. So need to ensure
			  ;; there are at least 3 slots. This hack
			  ;; just adds 3 more.
			  ,(if variable
			       '(inst sub esp-tn (fixnumize 3)))

			  ;; Save the fp
			  (storew ebp-tn new-fp (- (1+ ocfp-save-offset)))

			  (move ebp-tn new-fp) ; NB - now on new stack frame.
			  )))

	       (note-this-location vop :call-site)

	       (inst ,(if (eq return :tail) 'jmp 'call)
		     (make-ea :dword :base eax
			      :disp ,(if named
					 '(- (* fdefn-raw-addr-slot word-bytes)
					     other-pointer-type)
				       '(- (* closure-function-slot word-bytes)
					   function-pointer-type))))
	       ,@(ecase return
		   (:fixed
		    '((default-unknown-values vop values nvals)))
		   (:unknown
		    '((note-this-location vop :unknown-return)
		      (receive-unknown-values values-start nvals start count)))
		   (:tail))
	       (trace-table-entry trace-table-normal)))))

  (define-full-call call nil :fixed nil)
  (define-full-call call-named t :fixed nil)
  (define-full-call multiple-call nil :unknown nil)
  (define-full-call multiple-call-named t :unknown nil)
  (define-full-call tail-call nil :tail nil)
  (define-full-call tail-call-named t :tail nil)

  (define-full-call call-variable nil :fixed t)
  (define-full-call multiple-call-variable nil :unknown t))

;;; This is defined separately, since it needs special code that BLT's the
;;; arguments down. All the real work is done in the assembly routine. We just
;;; set things up so that it can find what it needs.
(define-vop (tail-call-variable)
  (:args (args :scs (any-reg control-stack) :target esi)
	 (function :scs (descriptor-reg control-stack) :target eax)
	 (old-fp)
	 (ret-addr))
  (:temporary (:sc unsigned-reg :offset esi-offset :from (:argument 0)) esi)
  (:temporary (:sc unsigned-reg :offset eax-offset :from (:argument 1)) eax)
;  (:ignore ret-addr old-fp)
  (:generator 75
    ;; Move these into the passing locations if they are not already there.
    (move esi args)
    (move eax function)

    ;; The following assumes that the return-pc and old-fp are on the
    ;; stack in their standard save locations - Check this.
    (unless (and (sc-is old-fp control-stack)
		 (= (tn-offset old-fp) ocfp-save-offset))
	    (error "tail-call-variable: ocfp not on stack in standard save location?"))
    (unless (and (sc-is ret-addr sap-stack)
		 (= (tn-offset ret-addr) return-pc-save-offset))
	    (error "tail-call-variable: ret-addr not on stack in standard save location?"))


    ;; And jump to the assembly routine.
    (inst jmp (make-fixup 'tail-call-variable :assembly-routine))))

;;;; unknown values return

;;; Return a single-value using the Unknown-Values convention. Specifically,
;;; we jump to clear the stack and jump to return-pc+2.
;;;
;;; We require old-fp to be in a register, because we want to reset ESP before
;;; restoring EBP. If old-fp were still on the stack, it could get clobbered
;;; by a signal.
;;;
;;; pfw--get wired-tn conflicts sometimes if register sc specd for args
;;; having problems targeting args to regs -- using temps instead.
(define-vop (return-single)
  (:args (old-fp)
	 (return-pc)
	 (value))
  (:temporary (:sc unsigned-reg) ofp)
  (:temporary (:sc unsigned-reg) ret)
  (:ignore value)
  (:generator 6
    (trace-table-entry trace-table-function-epilogue)
    (move ret return-pc)
    ;; Clear the control stack
    (move ofp old-fp)
    ;; Adjust the return address for the single value return.
    (inst add ret 2)
    ;; Restore the frame pointer.
    (move esp-tn ebp-tn)
    (move ebp-tn ofp)
    ;; Out of here.
    (inst jmp ret)))

;;; Do unknown-values return of a fixed (other than 1) number of values. The
;;; Values are required to be set up in the standard passing locations. Nvals
;;; is the number of values returned.
;;;
;;; Basically, we just load ECX with the number of values returned and EBX
;;; with a pointer to the values, set ESP to point to the end of the values,
;;; and jump directly to return-pc.
(define-vop (return)
  (:args (old-fp)
	 (return-pc :to (:eval 1))
	 (values :more t))
  (:ignore values)
  (:info nvals)

  ;; In the case of other than one value, we need these registers to tell
  ;; the caller where they are and how many there are.
  (:temporary (:sc unsigned-reg :offset ebx-offset) ebx)
  (:temporary (:sc unsigned-reg :offset ecx-offset) ecx)

  ;; We need to stretch the lifetime of return-pc past the argument
  ;; registers so that we can default the argument registers without
  ;; trashing return-pc.
  (:temporary (:sc unsigned-reg :offset (first *register-arg-offsets*)
		   :from :eval) a0)
  (:temporary (:sc unsigned-reg :offset (second *register-arg-offsets*)
		   :from :eval) a1)
  (:temporary (:sc unsigned-reg :offset (third *register-arg-offsets*)
		   :from :eval) a2)

  (:generator 6
    (trace-table-entry trace-table-function-epilogue)
    ;; Establish the values pointer and values count.
    (move ebx ebp-tn)
    (if (zerop nvals)
	(inst xor ecx ecx) ; smaller
      (inst mov ecx (fixnumize nvals)))
    ;; restore the frame pointer.
    (move ebp-tn old-fp)
    ;; clear as much of the stack as possible, but not past the return
    ;; address.
    (inst lea esp-tn (make-ea :dword :base ebx
			      :disp (- (* (max nvals 2) word-bytes))))
    ;; pre-default any argument register that need it.
    (when (< nvals register-arg-count)
      (let* ((arg-tns (nthcdr nvals (list a0 a1 a2)))
	     (first (first arg-tns)))
	(inst mov first nil-value)
	(dolist (tn (cdr arg-tns))
	  (inst mov tn first))))
    ;; And away we go. Except that return-pc is still on the
    ;; stack and we've changed the stack pointer. So we have to
    ;; tell it to index off of EBX instead of EBP.
    (cond ((zerop nvals)
	   ;; Return popping the return address and the OCFP.
	   (inst ret word-bytes))
	  ((= nvals 1)
	   ;; Return popping the return, leaving 1 slot. Can this
	   ;; happen, or is a single value return handled elsewhere?
	   (inst ret))
	  (t
	   (inst jmp (make-ea :dword :base ebx
			      :disp (- (* (1+ (tn-offset return-pc))
					  word-bytes))))))

    (trace-table-entry trace-table-normal)))

;;; Do unknown-values return of an arbitrary number of values (passed on the
;;; stack.)  We check for the common case of a single return value, and do that
;;; inline using the normal single value return convention. Otherwise, we
;;; branch off to code that calls an assembly-routine.
;;;
;;; The assembly routine takes the following args:
;;;  EAX -- the return-pc to finally jump to.
;;;  EBX -- pointer to where to put the values.
;;;  ECX -- number of values to find there.
;;;  ESI -- pointer to where to find the values.
(define-vop (return-multiple)
  (:args (old-fp :to (:eval 1) :target old-fp-temp)
	 (return-pc :target eax)
	 (vals :scs (any-reg) :target esi)
	 (nvals :scs (any-reg) :target ecx))

  (:temporary (:sc unsigned-reg :offset eax-offset :from (:argument 1)) eax)
  (:temporary (:sc unsigned-reg :offset esi-offset :from (:argument 2)) esi)
  (:temporary (:sc unsigned-reg :offset ecx-offset :from (:argument 3)) ecx)
  (:temporary (:sc unsigned-reg :offset ebx-offset :from (:eval 0)) ebx)
  (:temporary (:sc descriptor-reg :offset (first *register-arg-offsets*)
		   :from (:eval 0)) a0)
  (:temporary (:sc unsigned-reg :from (:eval 1)) old-fp-temp)
  (:node-var node)

  (:generator 13
    (trace-table-entry trace-table-function-epilogue)
    ;; Load the return-pc.
    (move eax return-pc)
    (unless (policy node (> space speed))
      ;; Check for the single case.
      (let ((not-single (gen-label)))
	(inst cmp nvals (fixnumize 1))
	(inst jmp :ne not-single)
	
	;; Return with one value.
	(loadw a0 vals -1)
	;; Clear the stack. We load old-fp into a register before clearing
	;; the stack.
	(move old-fp-temp old-fp)
	(move esp-tn ebp-tn)
	(move ebp-tn old-fp-temp)
	;; Fix the return-pc to point at the single-value entry point.
	(inst add eax 2)
	;; Out of here.
	(inst jmp eax)
	
	;; Nope, not the single case. Jump to the assembly routine.
	(emit-label not-single)))
    (move esi vals)
    (move ecx nvals)
    (move ebx ebp-tn)
    (move ebp-tn old-fp)
    (inst jmp (make-fixup 'return-multiple :assembly-routine))
    (trace-table-entry trace-table-normal)))

;;;; XEP hackery

;;; We don't need to do anything special for regular functions.
(define-vop (setup-environment)
  (:info label)
  (:ignore label)
  (:generator 0
    ;; Don't bother doing anything.
    nil))

;;; Get the lexical environment from its passing location.
(define-vop (setup-closure-environment)
  (:results (closure :scs (descriptor-reg)))
  (:info label)
  (:ignore label)
  (:generator 6
    ;; Get result.
    (move closure eax-tn)))

;;; Copy a more arg from the argument area to the end of the current frame.
;;; Fixed is the number of non-more arguments.
;;;
;;; The tricky part is doing this without trashing any of the calling
;;; convention registers that are still needed. This vop is emitted directly
;;; after the xep-allocate frame. That means the registers are in use as
;;; follows:
;;;
;;;  EAX -- The lexenv.
;;;  EBX -- Available.
;;;  ECX -- The total number of arguments.
;;;  EDX -- The first arg.
;;;  EDI -- The second arg.
;;;  ESI -- The third arg.
;;;
;;; So basically, we have one register available for our use: EBX.
;;;
;;; What we can do is push the other regs onto the stack, and then restore
;;; their values by looking directly below where we put the more-args.
(define-vop (copy-more-arg)
  (:info fixed)
  (:generator 20
    ;; Avoid the copy if there are no more args.
    (cond ((zerop fixed)
	   (inst jecxz just-alloc-frame))
	  (t
	   (inst cmp ecx-tn (fixnumize fixed))
	   (inst jmp :be just-alloc-frame)))

    ;; Allocate the space on the stack.
    ;; stack = ebp - (max 3 frame-size) - (nargs - fixed)
    (inst lea ebx-tn
	  (make-ea :dword :base ebp-tn
		   :disp (- (fixnumize fixed)
			    (* sb!vm:word-bytes
			       (max 3 (sb-allocated-size 'stack))))))
    (inst sub ebx-tn ecx-tn)  ; Got the new stack in ebx
    (inst mov esp-tn ebx-tn)

    ;; Now: nargs>=1 && nargs>fixed

    ;; Save the original count of args.
    (inst mov ebx-tn ecx-tn)

    (cond ((< fixed register-arg-count)
	   ;; We must stop when we run out of stack args, not when we
	   ;; run out of more args.
	   ;; Number to copy = nargs-3
	   (inst sub ecx-tn (fixnumize register-arg-count))
	   ;; Everything of interest in registers.
	   (inst jmp :be do-regs))
	  (t
	   ;; Number to copy = nargs-fixed
	   (inst sub ecx-tn (fixnumize fixed))))

    ;; Save edi and esi register args.
    (inst push edi-tn)
    (inst push esi-tn)
    ;; Okay, we have pushed the register args. We can trash them
    ;; now.

    ;; Initialize dst to be end of stack; skiping the values pushed
    ;; above.
    (inst lea edi-tn (make-ea :dword :base esp-tn :disp 8))

    ;; Initialize src to be end of args.
    (inst mov esi-tn ebp-tn)
    (inst sub esi-tn ebx-tn)

    (inst shr ecx-tn word-shift)	; make word count
    ;; And copy the args.
    (inst cld)				; auto-inc ESI and EDI.
    (inst rep)
    (inst movs :dword)

    ;; So now we need to restore EDI and ESI.
    (inst pop esi-tn)
    (inst pop edi-tn)

    DO-REGS

    ;; Restore ECX
    (inst mov ecx-tn ebx-tn)

    ;; Here: nargs>=1 && nargs>fixed
    (when (< fixed register-arg-count)
	  ;; Now we have to deposit any more args that showed up in
	  ;; registers.
	  (do ((i fixed))
	      ( nil )
	      ;; Store it relative to ebp
	      (inst mov (make-ea :dword :base ebp-tn
				 :disp (- (* 4
					     (+ 1 (- i fixed)
						(max 3 (sb-allocated-size 'stack))))))
		    (nth i *register-arg-tns*))

	      (incf i)
	      (when (>= i register-arg-count)
		    (return))

	      ;; Don't deposit any more than there are.
	      (if (zerop i)
		  (inst test ecx-tn ecx-tn)
		(inst cmp ecx-tn (fixnumize i)))
	      (inst jmp :eq done)))

    (inst jmp done)

    JUST-ALLOC-FRAME
    (inst lea esp-tn
	  (make-ea :dword :base ebp-tn
		   :disp (- (* sb!vm:word-bytes
			       (max 3 (sb-allocated-size 'stack))))))

    DONE))

;;; More args are stored contiguously on the stack, starting immediately at the
;;; context pointer. The context pointer is not typed, so the lowtag is 0.
(define-vop (more-arg)
  (:translate %more-arg)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg) :target temp))
  (:arg-types * tagged-num)
  (:temporary (:sc unsigned-reg :from (:argument 1) :to :result) temp)
  (:results (value :scs (any-reg descriptor-reg)))
  (:result-types *)
  (:generator 5
    (move temp index)
    (inst neg temp)
    (inst mov value (make-ea :dword :base object :index temp))))

(define-vop (more-arg-c)
  (:translate %more-arg)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types * (:constant (signed-byte 30)))
  (:results (value :scs (any-reg descriptor-reg)))
  (:result-types *)
  (:generator 4
   (inst mov value
	 (make-ea :dword :base object :disp (- (* index word-bytes))))))


;;; Turn more arg (context, count) into a list.
(define-vop (listify-rest-args)
  (:translate %listify-rest-args)
  (:policy :safe)
  (:args (context :scs (descriptor-reg) :target src)
	 (count :scs (any-reg) :target ecx))
  (:arg-types * tagged-num)
  (:temporary (:sc unsigned-reg :offset esi-offset :from (:argument 0)) src)
  (:temporary (:sc unsigned-reg :offset ecx-offset :from (:argument 1)) ecx)
  (:temporary (:sc unsigned-reg :offset eax-offset) eax)
  (:temporary (:sc unsigned-reg) dst)
  (:results (result :scs (descriptor-reg)))
  (:node-var node)
  (:generator 20
    (let ((enter (gen-label))
	  (loop (gen-label))
	  (done (gen-label)))
      (move src context)
      (move ecx count)
      ;; Check to see whether there are no args, and just return NIL if so.
      (inst mov result nil-value)
      (inst jecxz done)
      (inst lea dst (make-ea :dword :index ecx :scale 2))
      (pseudo-atomic
       (allocation dst dst node)
       (inst lea dst (make-ea :byte :base dst :disp list-pointer-type))
       ;; Convert the count into a raw value, so that we can use the LOOP inst.
       (inst shr ecx 2)
       ;; Set decrement mode (successive args at lower addresses)
       (inst std)
       ;; Set up the result.
       (move result dst)
       ;; Jump into the middle of the loop, 'cause that's were we want
       ;; to start.
       (inst jmp enter)
       (emit-label loop)
       ;; Compute a pointer to the next cons.
       (inst add dst (* cons-size word-bytes))
       ;; Store a pointer to this cons in the CDR of the previous cons.
       (storew dst dst -1 list-pointer-type)
       (emit-label enter)
       ;; Grab one value and stash it in the car of this cons.
       (inst lods eax)
       (storew eax dst 0 list-pointer-type)
       ;; Go back for more.
       (inst loop loop)
       ;; NIL out the last cons.
       (storew nil-value dst 1 sb!vm:list-pointer-type))
      (emit-label done))))

;;; Return the location and size of the more arg glob created by Copy-More-Arg.
;;; Supplied is the total number of arguments supplied (originally passed in
;;; ECX.)  Fixed is the number of non-rest arguments.
;;;
;;; We must duplicate some of the work done by Copy-More-Arg, since at that
;;; time the environment is in a pretty brain-damaged state, preventing this
;;; info from being returned as values. What we do is compute
;;; supplied - fixed, and return a pointer that many words below the current
;;; stack top.
(define-vop (more-arg-context)
  (:policy :fast-safe)
  (:translate sb!c::%more-arg-context)
  (:args (supplied :scs (any-reg) :target count))
  (:arg-types positive-fixnum (:constant fixnum))
  (:info fixed)
  (:results (context :scs (descriptor-reg))
	    (count :scs (any-reg)))
  (:result-types t tagged-num)
  (:note "more-arg-context")
  (:generator 5
    (move count supplied)
    ;; SP at this point points at the last arg pushed.
    ;; Point to the first more-arg, not above it.
    (inst lea context (make-ea :dword :base esp-tn
			       :index count :scale 1
			       :disp (- (+ (fixnumize fixed) 4))))
    (unless (zerop fixed)
      (inst sub count (fixnumize fixed)))))

;;; Signal wrong argument count error if Nargs isn't = to Count.
(define-vop (verify-argument-count)
  (:policy :fast-safe)
  (:translate sb!c::%verify-argument-count)
  (:args (nargs :scs (any-reg)))
  (:arg-types positive-fixnum (:constant t))
  (:info count)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 3
    (let ((err-lab
	   (generate-error-code vop invalid-argument-count-error nargs)))
      (if (zerop count)
	  (inst test nargs nargs)  ; smaller instruction
	(inst cmp nargs (fixnumize count)))
      (inst jmp :ne err-lab))))

;;; Various other error signallers.
(macrolet ((frob (name error translate &rest args)
	     `(define-vop (,name)
		,@(when translate
		    `((:policy :fast-safe)
		      (:translate ,translate)))
		(:args ,@(mapcar #'(lambda (arg)
				     `(,arg :scs (any-reg descriptor-reg)))
				 args))
		(:vop-var vop)
		(:save-p :compute-only)
		(:generator 1000
		  (error-call vop ,error ,@args)))))
  (frob argument-count-error invalid-argument-count-error
    sb!c::%argument-count-error nargs)
  (frob type-check-error object-not-type-error sb!c::%type-check-error
    object type)
  (frob layout-invalid-error layout-invalid-error sb!c::%layout-invalid-error
    object layout)
  (frob odd-keyword-arguments-error odd-keyword-arguments-error
    sb!c::%odd-keyword-arguments-error)
  (frob unknown-keyword-argument-error unknown-keyword-argument-error
    sb!c::%unknown-keyword-argument-error key)
  (frob nil-function-returned-error nil-function-returned-error nil fun))
