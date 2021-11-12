;;;; function call for the x86 VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(defconstant arg-count-sc (make-sc+offset any-reg-sc-number ecx-offset))
(defconstant closure-sc (make-sc+offset descriptor-reg-sc-number eax-offset))

(defconstant return-pc-passing-offset
  (make-sc+offset sap-stack-sc-number return-pc-save-offset))

(defconstant old-fp-passing-offset
  (make-sc+offset control-stack-sc-number ocfp-save-offset))

;;; Make the TNs used to hold OLD-FP and RETURN-PC within the current
;;; function. We treat these specially so that the debugger can find
;;; them at a known location.
;;;
;;; Without using a save-tn - which does not make much sense if it is
;;; wired to the stack?
(defun make-old-fp-save-location ()
  (let ((tn (make-wired-tn *fixnum-primitive-type*
                           control-stack-sc-number
                           ocfp-save-offset)))
    (setf (tn-kind tn) :environment)
    tn))
(defun make-return-pc-save-location ()
  (let ((tn (make-wired-tn (primitive-type-or-lose 'system-area-pointer)
                           sap-stack-sc-number return-pc-save-offset)))
    (setf (tn-kind tn) :environment)
    tn))

;;; Make a TN for the standard argument count passing location. We only
;;; need to make the standard location, since a count is never passed when we
;;; are using non-standard conventions.
(defun make-arg-count-location ()
  (make-wired-tn *fixnum-primitive-type* any-reg-sc-number ecx-offset))

;;;; frame hackery

;;; This is used for setting up the Old-FP in local call.
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

;;; Accessing a slot from an earlier stack frame is definite hackery.
(define-vop (ancestor-frame-ref)
  (:args (frame-pointer :scs (descriptor-reg))
         (variable-home-tn :load-if nil))
  (:results (value :scs (descriptor-reg any-reg)))
  (:policy :fast-safe)
  (:generator 4
    (aver (sc-is variable-home-tn control-stack))
    (loadw value frame-pointer
           (frame-word-offset (tn-offset variable-home-tn)))))
(define-vop (ancestor-frame-set)
  (:args (frame-pointer :scs (descriptor-reg))
         (value :scs (descriptor-reg any-reg)))
  (:results (variable-home-tn :load-if nil))
  (:policy :fast-safe)
  (:generator 4
    (aver (sc-is variable-home-tn control-stack))
    (storew value frame-pointer
            (frame-word-offset (tn-offset variable-home-tn)))))

(macrolet ((define-frame-op
               (suffix sc stack-sc instruction
                &optional (ea
                           `(make-ea :dword
                                     :base frame-pointer
                                     :disp (frame-byte-offset
                                            (tn-offset variable-home-tn)))))
               (let ((reffer (symbolicate 'ancestor-frame-ref '/ suffix))
                     (setter (symbolicate 'ancestor-frame-set '/ suffix)))
                 `(progn
                    (define-vop (,reffer ancestor-frame-ref)
                      (:results (value :scs (,sc)))
                      (:generator 4
                        (aver (sc-is variable-home-tn ,stack-sc))
                        (inst ,instruction value
                              ,ea)))
                    (define-vop (,setter ancestor-frame-set)
                      (:args (frame-pointer :scs (descriptor-reg))
                             (value :scs (,sc)))
                      (:generator 4
                        (aver (sc-is variable-home-tn ,stack-sc))
                        (inst ,instruction ,ea value))))))
           (define-x87-frame-op
               (suffix sc stack-sc (load set)
                &optional (ea
                           `(make-ea :dword
                                     :base frame-pointer
                                     :disp (frame-byte-offset
                                            (tn-offset variable-home-tn)))))
               (let ((reffer (symbolicate 'ancestor-frame-ref '/ suffix))
                     (setter (symbolicate 'ancestor-frame-set '/ suffix)))
                 `(progn
                    (define-vop (,reffer ancestor-frame-ref)
                      (:results (value :scs (,sc)))
                      (:generator 4
                        (aver (sc-is variable-home-tn ,stack-sc))
                        ,(if (symbolp load)
                             `(with-empty-tn@fp-top (value)
                                (inst ,load ,ea))
                             load)))
                    (define-vop (,setter ancestor-frame-set)
                      (:args (frame-pointer :scs (descriptor-reg))
                             (value :scs (,sc)))
                      (:generator 4
                        (aver (sc-is variable-home-tn ,stack-sc))
                        ,(if (symbolp set)
                             `(with-tn@fp-top (value)
                                (inst ,set ,ea))
                             set)))))))
  (define-frame-op signed-byte-32 signed-reg signed-stack mov)
  (define-frame-op unsigned-byte-32 unsigned-reg unsigned-stack mov)
  (define-frame-op system-area-pointer sap-reg sap-stack mov)

  (define-x87-frame-op double-float double-reg double-stack
    (fldd fstd) (make-ea :dword
                         :base frame-pointer
                         :disp (frame-byte-offset
                                (1+ (tn-offset variable-home-tn)))))
  (define-x87-frame-op single-float single-reg single-stack
    (fld fst))

  (define-x87-frame-op complex-double-float complex-double-reg
    complex-double-stack
    ((let ((real (complex-double-reg-real-tn value))
           (imag (complex-double-reg-imag-tn value)))
       (with-empty-tn@fp-top (real)
         (inst fldd (ea-for-cdf-real-stack variable-home-tn frame-pointer)))
       (with-empty-tn@fp-top (imag)
         (inst fldd (ea-for-cdf-imag-stack variable-home-tn frame-pointer))))
     (let ((real (complex-double-reg-real-tn value))
           (imag (complex-double-reg-imag-tn value)))
       (with-tn@fp-top (real)
         (inst fstd (ea-for-cdf-real-stack variable-home-tn frame-pointer)))
       (with-tn@fp-top (imag)
         (inst fstd (ea-for-cdf-imag-stack variable-home-tn frame-pointer))))))
  (define-x87-frame-op complex-single-float complex-single-reg
    complex-single-stack
    ((let ((real (complex-single-reg-real-tn value))
           (imag (complex-single-reg-imag-tn value)))
       (with-empty-tn@fp-top (real)
         (inst fld (ea-for-csf-real-stack variable-home-tn frame-pointer)))
       (with-empty-tn@fp-top (imag)
         (inst fld (ea-for-csf-imag-stack variable-home-tn frame-pointer))))
     (let ((real (complex-single-reg-real-tn value))
           (imag (complex-single-reg-imag-tn value)))
       (with-tn@fp-top (real)
         (inst fst (ea-for-csf-real-stack variable-home-tn frame-pointer)))
       (with-tn@fp-top (imag)
         (inst fst (ea-for-csf-imag-stack variable-home-tn frame-pointer)))))))

(defun primitive-type-indirect-cell-type (ptype)
  (declare (type primitive-type ptype))
  (macrolet ((foo (&body data)
                 `(case (primitive-type-name ptype)
                    ,@(loop for (name stack-sc ref set) in data
                            collect
                            `(,name
                               (load-time-value
                                (list (primitive-type-or-lose ',name)
                                      (sc-or-lose ',stack-sc)
                                      (lambda (node block fp value res)
                                        (sb-c::vop ,ref node block
                                                   fp value res))
                                      (lambda (node block fp new-val value)
                                        (sb-c::vop ,set node block
                                                   fp new-val value)))))))))
    (foo (double-float double-stack
                       ancestor-frame-ref/double-float
                       ancestor-frame-set/double-float)
         (single-float single-stack
                       ancestor-frame-ref/single-float
                       ancestor-frame-set/single-float)
         (complex-double-float complex-double-stack
                               ancestor-frame-ref/complex-double-float
                               ancestor-frame-set/complex-double-float)
         (complex-single-float complex-single-stack
                               ancestor-frame-ref/complex-single-float
                               ancestor-frame-set/complex-single-float)
         (signed-byte-32 signed-stack
                         ancestor-frame-ref/signed-byte-32
                         ancestor-frame-set/signed-byte-32)
         (unsigned-byte-32 unsigned-stack
                           ancestor-frame-ref/unsigned-byte-32
                           ancestor-frame-set/unsigned-byte-32)
         (unsigned-byte-31 unsigned-stack
                           ancestor-frame-ref/unsigned-byte-32
                           ancestor-frame-set/unsigned-byte-32)
         (system-area-pointer sap-stack
                              ancestor-frame-ref/system-area-pointer
                              ancestor-frame-set/system-area-pointer))))

(define-vop (xep-allocate-frame)
  (:info start-lab)
  (:generator 1
    (let ((nop-kind
           (shiftf (sb-assem::asmstream-inter-function-padding sb-assem:*asmstream*)
                   :nop)))
      (emit-alignment n-lowtag-bits (if (eq nop-kind :nop) #x90 0)))
    (emit-label start-lab)
    ;; Skip space for the function header.
    (inst simple-fun-header-word)
    (inst .skip (* (1- simple-fun-insts-offset) n-word-bytes))
    ;; The start of the actual code.
    ;; Save the return-pc.
    (popw ebp-tn (frame-word-offset return-pc-save-offset))))

(define-vop (xep-setup-sp)
  (:generator 1
    (inst lea esp-tn
            (make-ea :dword :base ebp-tn
                     :disp (- (* n-word-bytes
                                 (- (max 3 (sb-allocated-size 'stack))
                                    sp->fp-offset)))))))

;;; This is emitted directly before either a known-call-local, call-local,
;;; or a multiple-call-local. All it does is allocate stack space for the
;;; callee (who has the same size stack as us).
(define-vop (allocate-frame)
  (:results (res :scs (any-reg))
            (nfp))
  (:info callee)
  (:ignore nfp callee)
  (:generator 2
    (inst lea res (make-ea :dword :base esp-tn
                           :disp (- (* sp->fp-offset n-word-bytes))))
    (inst sub esp-tn (* n-word-bytes (sb-allocated-size 'stack)))))

;;; Allocate a partial frame for passing stack arguments in a full
;;; call. NARGS is the number of arguments passed. We allocate at
;;; least 3 slots, because the XEP noise is going to want to use them
;;; before it can extend the stack.
(define-vop (allocate-full-call-frame)
  (:info nargs)
  (:results (res :scs (any-reg)))
  (:generator 2
    (inst lea res (make-ea :dword :base esp-tn
                           :disp (- (* sp->fp-offset n-word-bytes))))
    (inst sub esp-tn (* (max nargs 3) n-word-bytes))))

;;; Emit code needed at the return-point from an unknown-values call
;;; for a fixed number of values. Values is the head of the TN-REF
;;; list for the locations that the values are to be received into.
;;; Nvals is the number of values that are to be received (should
;;; equal the length of Values).
;;;
;;; If 0 or 1 values are expected, then we just emit an instruction to
;;; reset the SP (which will only be executed when other than 1 value
;;; is returned.)
;;;
;;; In the general case we have to do three things:
;;;  -- Default unsupplied register values. This need only be done
;;;     when a single value is returned, since register values are
;;;     defaulted by the called in the non-single case.
;;;  -- Default unsupplied stack values. This needs to be done whenever
;;;     there are stack values.
;;;  -- Reset SP. This must be done whenever other than 1 value is
;;;     returned, regardless of the number of values desired.
(defun default-unknown-values (vop values nvals node)
  (declare (type (or tn-ref null) values)
           (type unsigned-byte nvals))
  (let ((type (sb-c::basic-combination-derived-type node)))
    (cond
      ((<= nvals 1)
       (note-this-location vop :single-value-return)
       (cond
         ((<= (values-type-max-value-count type)
              register-arg-count)
          (when (and (named-type-p type)
                     (eq nil (named-type-name type)))
            ;; The function never returns, it may happen that the code
            ;; ends right here leavig the :SINGLE-VALUE-RETURN note
            ;; dangling. Let's emit a NOP.
            (inst nop)))
         ((not (values-type-may-be-single-value-p type))
          (inst mov esp-tn ebx-tn))
         ((member :cmov *backend-subfeatures*)
          (inst cmov :c esp-tn ebx-tn))
         (t
          (let ((single-value (gen-label)))
            (inst jmp :nc single-value)
            (inst mov esp-tn ebx-tn)
            (emit-label single-value)))))
      ((<= nvals register-arg-count)
       (note-this-location vop :unknown-return)
       (when (values-type-may-be-single-value-p type)
         (let ((regs-defaulted (gen-label)))
           (inst jmp :c regs-defaulted)
           ;; Default the unsupplied registers.
           (let* ((2nd-tn-ref (tn-ref-across values))
                  (2nd-tn (tn-ref-tn 2nd-tn-ref)))
             (inst mov 2nd-tn nil-value)
             (when (> nvals 2)
               (loop
                for tn-ref = (tn-ref-across 2nd-tn-ref)
                then (tn-ref-across tn-ref)
                for count from 2 below register-arg-count
                do (inst mov (tn-ref-tn tn-ref) 2nd-tn))))
           (inst mov ebx-tn esp-tn)
           (emit-label regs-defaulted)))
       (when (< register-arg-count
                (values-type-max-value-count type))
         (inst mov esp-tn ebx-tn)))
      ((<= nvals 7)
       ;; The number of bytes depends on the relative jump instructions.
       ;; Best case is 31+(n-3)*14, worst case is 35+(n-3)*18. For
       ;; NVALS=6 that is 73/89 bytes, and for NVALS=7 that is 87/107
       ;; bytes which is likely better than using the blt below.
       (let ((regs-defaulted (gen-label))
             (defaulting-done (gen-label))
             (default-stack-slots (gen-label)))
         (note-this-location vop :unknown-return)
         ;; Branch off to the MV case.
         (inst jmp :c regs-defaulted)
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
         (inst jmp default-stack-slots)
         (emit-label regs-defaulted)
         (inst mov eax-tn nil-value)
         (collect ((defaults))
           (do ((i register-arg-count (1+ i))
                (val (do ((i 0 (1+ i))
                          (val values (tn-ref-across val)))
                         ((= i register-arg-count) val))
                     (tn-ref-across val)))
               ((null val))
             (let ((default-lab (gen-label))
                   (tn (tn-ref-tn val))
                   (first-stack-arg-p (= i register-arg-count)))
               (defaults (cons default-lab
                               (cons tn first-stack-arg-p)))
               (inst cmp ecx-tn (fixnumize i))
               (inst jmp :be default-lab)
               (when first-stack-arg-p
                 ;; There are stack args so the frame of the callee is
                 ;; still there, save EDX in its first slot temporalily.
                 (storew edx-tn ebx-tn (frame-word-offset sp->fp-offset)))
               (loadw edx-tn ebx-tn (frame-word-offset (+ sp->fp-offset i)))
               (inst mov tn edx-tn)))
           (emit-label defaulting-done)
           (loadw edx-tn ebx-tn (frame-word-offset sp->fp-offset))
           (move esp-tn ebx-tn)
           (let ((defaults (defaults)))
             (when defaults
               (assemble (:elsewhere)
                 (emit-label default-stack-slots)
                 (dolist (default defaults)
                   (emit-label (car default))
                   (when (cddr default)
                     ;; We are setting the first stack argument to NIL.
                     ;; The callee's stack frame is dead, save EDX by
                     ;; pushing it to the stack, it will end up at same
                     ;; place as in the (STOREW EDX-TN EBX-TN -1) case
                     ;; above.
                     (inst push edx-tn))
                   (inst mov (second default) eax-tn))
                 (inst jmp defaulting-done)))))))
      (t
       (let ((regs-defaulted (gen-label))
             (restore-edi (gen-label))
             (no-stack-args (gen-label))
             (default-stack-vals (gen-label))
             (count-okay (gen-label))
             (copy-loop (gen-label)))
         (note-this-location vop :unknown-return)
         ;; Branch off to the MV case.
         (inst jmp :c regs-defaulted)
         ;; Default the register args, and set up the stack as if we
         ;; entered the MV return point.
         (inst mov ebx-tn esp-tn)
         (inst mov edi-tn nil-value)
         (inst mov esi-tn edi-tn)
         ;; Compute a pointer to where to put the [defaulted] stack values.
         (emit-label no-stack-args)
         (inst push edx-tn)
         (inst push edi-tn)
         (inst lea edi-tn
               (make-ea :dword :base ebp-tn
                        :disp (frame-byte-offset register-arg-count)))
         ;; Load EAX with NIL so we can quickly store it, and set up
         ;; stuff for the loop.
         (inst mov eax-tn nil-value)
         (inst mov ecx-tn (fixnumize (- nvals register-arg-count)))
         ;; Jump into the default loop.
         (inst jmp default-stack-vals)
         ;; The regs are defaulted. We need to copy any stack arguments,
         ;; and then default the remaining stack arguments.
         (emit-label regs-defaulted)
         ;; Compute the number of stack arguments, and if it's zero or
         ;; less, don't copy any stack arguments.
         (inst sub ecx-tn (fixnumize register-arg-count))
         (inst jmp :le no-stack-args)
         ;; Save EDI.
         (storew edi-tn ebx-tn (frame-word-offset (+ sp->fp-offset 1)))
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
                        :disp (frame-byte-offset register-arg-count)))
         ;; Save ESI, and compute a pointer to where the args come from.
         (storew esi-tn ebx-tn (frame-word-offset (+ sp->fp-offset 2)))
         (inst lea esi-tn
               (make-ea :dword :base ebx-tn
                        :disp (frame-byte-offset
                               (+ sp->fp-offset register-arg-count))))
         ;; Do the copy.
         (inst push edx-tn)
         (emit-label copy-loop)
         (inst mov edx-tn (make-ea :dword :base esi-tn))
         (inst sub esi-tn n-word-bytes)
         (inst mov (make-ea :dword :base edi-tn) edx-tn)
         (inst sub edi-tn n-word-bytes)
         (inst sub ecx-tn (fixnumize 1))
         (inst jmp :nz copy-loop)
         (inst pop edx-tn)
         ;; Restore ESI.
         (loadw esi-tn ebx-tn (frame-word-offset (+ sp->fp-offset 2)))
         ;; Now we have to default the remaining args. Find out how many.
         (inst sub eax-tn (fixnumize (- nvals register-arg-count)))
         (inst neg eax-tn)
         ;; If none, then just blow out of here.
         (inst jmp :le restore-edi)
         (inst mov ecx-tn eax-tn)
         ;; Load EAX with NIL for fast storing.
         (inst mov eax-tn nil-value)
         ;; Do the store.
         (emit-label default-stack-vals)
         (inst mov (make-ea :dword :base edi-tn) eax-tn)
         (inst sub edi-tn n-word-bytes)
         (inst sub ecx-tn (fixnumize 1))
         (inst jmp :nz default-stack-vals)
         ;; Restore EDI, and reset the stack.
         (emit-label restore-edi)
         (loadw edi-tn ebx-tn (frame-word-offset (+ sp->fp-offset 1)))
         (inst mov esp-tn ebx-tn)))))
  (values))

;;;; unknown values receiving

;;; Emit code needed at the return point for an unknown-values call
;;; for an arbitrary number of values.
;;;
;;; We do the single and non-single cases with no shared code: there
;;; doesn't seem to be any potential overlap, and receiving a single
;;; value is more important efficiency-wise.
;;;
;;; When there is a single value, we just push it on the stack,
;;; returning the old SP and 1.
;;;
;;; When there is a variable number of values, we move all of the
;;; argument registers onto the stack, and return ARGS and NARGS.
;;;
;;; ARGS and NARGS are TNs wired to the named locations. We must
;;; explicitly allocate these TNs, since their lifetimes overlap with
;;; the results start and count. (Also, it's nice to be able to target
;;; them.)
(defun receive-unknown-values (args nargs start count node)
  (declare (type tn args nargs start count))
  (let ((type (sb-c::basic-combination-derived-type node))
        (variable-values (gen-label))
        (stack-values (gen-label))
        (done (gen-label)))
    (when (values-type-may-be-single-value-p type)
      (inst jmp :c variable-values)
      (cond ((location= start (first *register-arg-tns*))
             (inst push (first *register-arg-tns*))
             (inst lea start (make-ea :dword :base esp-tn :disp n-word-bytes)))
            (t (inst mov start esp-tn)
               (inst push (first *register-arg-tns*))))
      (inst mov count (fixnumize 1))
      (inst jmp done)
      (emit-label variable-values))
    ;; The stack frame is burnt and RETurned from if there are no
    ;; stack values. In this case quickly reallocate sufficient space.
    (when (<= (values-type-min-value-count type)
              register-arg-count)
      (inst cmp nargs (fixnumize register-arg-count))
      (inst jmp :g stack-values)
      (inst sub esp-tn nargs)
      (emit-label stack-values))
    ;; dtc: this writes the registers onto the stack even if they are
    ;; not needed, only the number specified in ecx are used and have
    ;; stack allocated to them. No harm is done.
    (loop
      for arg in *register-arg-tns*
      for i downfrom -1
      for j below (values-type-max-value-count type)
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

(defun check-ocfp-and-return-pc (old-fp return-pc)
  #+nil
  (format t "*known-return: old-fp ~S, tn-kind ~S; ~S ~S~%"
          old-fp (tn-kind old-fp) (sb-c::tn-save-tn old-fp)
          (tn-kind (sb-c::tn-save-tn old-fp)))
  #+nil
  (format t "*known-return: return-pc ~S, tn-kind ~S; ~S ~S~%"
          return-pc (tn-kind return-pc)
          (sb-c::tn-save-tn return-pc)
          (tn-kind (sb-c::tn-save-tn return-pc)))
  (unless (and (sc-is old-fp control-stack)
               (= (tn-offset old-fp) ocfp-save-offset))
    (error "ocfp not on stack in standard save location?"))
  (unless (and (sc-is return-pc sap-stack)
               (= (tn-offset return-pc) return-pc-save-offset))
    (error "return-pc not on stack in standard save location?")))

;;; The local call convention doesn't fit that well with x86-style
;;; calls. Emit a header for local calls to pop the return address
;;; in the right place.
(defun emit-block-header (start-label trampoline-label fall-thru-p alignp)
  (declare (ignore alignp))
  (when trampoline-label
    (when fall-thru-p
      (inst jmp start-label))
    (emit-label trampoline-label)
    (popw ebp-tn (frame-word-offset return-pc-save-offset)))
  (emit-label start-label))

;;; Non-TR local call for a fixed number of values passed according to
;;; the unknown values convention.
;;;
;;; FP is the frame pointer in install before doing the call.
;;;
;;; NFP would be the number-stack frame pointer if we had a separate
;;; number stack.
;;;
;;; Args are the argument passing locations, which are specified only
;;; to terminate their lifetimes in the caller.
;;;
;;; VALUES are the return value locations (wired to the standard
;;; passing locations). NVALS is the number of values received.
;;;
;;; Save is the save info, which we can ignore since saving has been
;;; done.
;;;
;;; TARGET is a continuation pointing to the start of the called
;;; function.
(define-vop (call-local)
  (:args (fp)
         (nfp)
         (args :more t))
  (:results (values :more t))
  (:save-p t)
  (:move-args :local-call)
  (:info arg-locs callee target nvals)
  (:vop-var vop)
  (:ignore nfp arg-locs args callee)
  (:node-var node)
  (:generator 5
    (move ebp-tn fp)
    (note-this-location vop :call-site)
    (inst call target)
    (default-unknown-values vop values nvals node)))

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
  (:ignore args save nfp callee)
  (:vop-var vop)
  (:node-var node)
  (:generator 20
    (move ebp-tn fp)
    (note-this-location vop :call-site)
    (inst call target)
    (note-this-location vop :unknown-return)
    (receive-unknown-values values-start nvals start count node)))

;;;; local call with known values return

;;; Non-TR local call with known return locations. Known-value return
;;; works just like argument passing in local call.
;;;
;;; Note: we can't use normal load-tn allocation for the fixed args,
;;; since all registers may be tied up by the more operand. Instead,
;;; we use MAYBE-LOAD-STACK-TN.
(define-vop (known-call-local)
  (:args (fp)
         (nfp)
         (args :more t))
  (:results (res :more t))
  (:move-args :local-call)
  (:save-p t)
  (:info save callee target)
  (:ignore args res save nfp callee)
  (:vop-var vop)
  (:generator 5
    (move ebp-tn fp)
    (note-this-location vop :call-site)
    (inst call target)
    (note-this-location vop :known-return)))

;;; From Douglas Crosher
;;; Return from known values call. We receive the return locations as
;;; arguments to terminate their lifetimes in the returning function. We
;;; restore FP and CSP and jump to the Return-PC.
(define-vop (known-return)
  (:args (old-fp)
         (return-pc)
         (vals :more t))
  (:move-args :known-return)
  (:info val-locs)
  (:ignore val-locs vals)
  (:vop-var vop)
  (:generator 6
    (check-ocfp-and-return-pc old-fp return-pc)
    ;; Zot all of the stack except for the old-fp and return-pc.
    (inst mov esp-tn ebp-tn)
    (inst pop ebp-tn)
    (inst ret)))

;;;; full call
;;;
;;; There is something of a cross-product effect with full calls.
;;; Different versions are used depending on whether we know the
;;; number of arguments or the name of the called function, and
;;; whether we want fixed values, unknown values, or a tail call.
;;;
;;; In full call, the arguments are passed creating a partial frame on
;;; the stack top and storing stack arguments into that frame. On
;;; entry to the callee, this partial frame is pointed to by FP.

;;; This macro helps in the definition of full call VOPs by avoiding
;;; code replication in defining the cross-product VOPs.
;;;
;;; NAME is the name of the VOP to define.
;;;
;;; NAMED is true if the first argument is an fdefinition object whose
;;; definition is to be called.
;;;
;;; RETURN is either :FIXED, :UNKNOWN or :TAIL:
;;; -- If :FIXED, then the call is for a fixed number of values, returned in
;;;    the standard passing locations (passed as result operands).
;;; -- If :UNKNOWN, then the result values are pushed on the stack, and the
;;;    result values are specified by the Start and Count as in the
;;;    unknown-values continuation representation.
;;; -- If :TAIL, then do a tail-recursive call. No values are returned.
;;;    The Old-Fp and Return-PC are passed as the second and third arguments.
;;;
;;; In non-tail calls, the pointer to the stack arguments is passed as
;;; the last fixed argument. If Variable is false, then the passing
;;; locations are passed as a more arg. Variable is true if there are
;;; a variable number of arguments passed on the stack. Variable
;;; cannot be specified with :TAIL return. TR variable argument call
;;; is implemented separately.
;;;
;;; In tail call with fixed arguments, the passing locations are
;;; passed as a more arg, but there is no new-FP, since the arguments
;;; have been set up in the current frame.
(macrolet ((define-full-call (name named return variable)
            (aver (not (and variable (eq return :tail))))
            `(define-vop (,name
                          ,@(when (eq return :unknown)
                              '(unknown-values-receiver)))
               (:args
               ,@(unless (eq return :tail)
                   '((new-fp :scs (any-reg) :to (:argument 1))))
               ,@(unless (eq named :direct)
                   '((fun :scs (descriptor-reg control-stack)
                          :target eax :to (:argument 0))))

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
               ,@(when (eq named :direct) '(fun))
               ,@(when (eq return :fixed) '(nvals))
               step-instrumenting)

               (:ignore
               ,@(unless (or variable (eq return :tail)) '(arg-locs))
               ,@(unless variable '(args))
               ,@(when (eq named :direct) '(step-instrumenting)))

               ;; We pass either the fdefn object (for named call) or
               ;; the actual function object (for unnamed call) in
               ;; EAX. With named call, closure-tramp will replace it
               ;; with the real function and invoke the real function
               ;; for closures. Non-closures do not need this value,
               ;; so don't care what shows up in it.
               ,@(unless (eq named :direct)
                   '((:temporary
                      (:sc descriptor-reg
                       :offset eax-offset
                       :from (:argument 0)
                       :to :eval)
                      eax)))

               ;; We pass the number of arguments in ECX.
               (:temporary (:sc unsigned-reg :offset ecx-offset :to :eval) ecx)

               ;; With variable call, we have to load the
               ;; register-args out of the (new) stack frame before
               ;; doing the call. Therefore, we have to tell the
               ;; lifetime stuff that we need to use them.
               ,@(when variable
                   (mapcar (lambda (name offset)
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
               ,@(unless (eq return :tail)
                   '((:node-var node)))

               (:generator ,(+ (if named 5 0)
                               (if variable 19 1)
                               (if (eq return :tail) 0 10)
                               15
                               (if (eq return :unknown) 25 0))

               ;; This has to be done before the frame pointer is
               ;; changed! EAX stores the 'lexical environment' needed
               ;; for closures.
               ,@(unless (eq named :direct)
                   '((move eax fun)))
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
                                      (error "** tail-call old-fp not S0~%")
                                      (move old-fp-tmp old-fp)
                                      (storew old-fp-tmp
                                              ebp-tn
                                              (frame-word-offset ocfp-save-offset))))
                                   ((any-reg descriptor-reg)
                                    (error "** tail-call old-fp in reg not S0~%")
                                    (storew old-fp
                                            ebp-tn
                                            (frame-word-offset ocfp-save-offset))))

                          ;; For tail call, we have to push the
                          ;; return-pc so that it looks like we CALLed
                          ;; despite the fact that we are going to JMP.
                          (inst push return-pc)))
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

                          ;; Bias the new-fp for use as an fp
                          ,(if variable
                               '(inst sub new-fp (fixnumize sp->fp-offset)))

                          ;; Save the fp
                          (storew ebp-tn new-fp
                                  (frame-word-offset ocfp-save-offset))

                          (move ebp-tn new-fp))))  ; NB - now on new stack frame.
               ,@(unless (eq named :direct)  ;; handle-single-step-around-trap can't handle it
                   `((when step-instrumenting
                       (emit-single-step-test)
                       (inst jmp :eq DONE)
                       (inst break single-step-around-trap))))
               DONE

               (note-this-location vop :call-site)

               (inst ,(if (eq return :tail) 'jmp 'call)
                     ,(case named
                        (:direct
                         '(make-ea :dword :disp
                           (+ nil-value (static-fun-offset fun))))
                        ((t)
                         '(object-slot-ea eax fdefn-raw-addr-slot
                           other-pointer-lowtag))
                        ((nil)
                         '(object-slot-ea eax closure-fun-slot
                           fun-pointer-lowtag))))
               ,@(ecase return
                   (:fixed
                    '((default-unknown-values vop values nvals node)))
                   (:unknown
                    '((note-this-location vop :unknown-return)
                      (receive-unknown-values values-start nvals start count
                                              node)))
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
  (define-full-call multiple-call-variable nil :unknown t))

;;; This is defined separately, since it needs special code that BLT's
;;; the arguments down. All the real work is done in the assembly
;;; routine. We just set things up so that it can find what it needs.
(define-vop (tail-call-variable)
  (:args (args :scs (any-reg control-stack) :target esi)
         (function :scs (descriptor-reg control-stack) :target eax)
         (old-fp)
         (return-pc))
  (:temporary (:sc unsigned-reg :offset esi-offset :from (:argument 0)) esi)
  (:temporary (:sc unsigned-reg :offset eax-offset :from (:argument 1)) eax)
  (:generator 75
    (check-ocfp-and-return-pc old-fp return-pc)
    ;; Move these into the passing locations if they are not already there.
    (move esi args)
    (move eax function)
    ;; And jump to the assembly routine.
    (inst jmp (make-fixup 'tail-call-variable :assembly-routine))))

;;;; unknown values return

;;; Return a single-value using the Unknown-Values convention.
;;;
;;; pfw--get wired-tn conflicts sometimes if register sc specd for args
;;; having problems targeting args to regs -- using temps instead.
;;;
;;; First off, modifying the return-pc defeats the branch-prediction
;;; optimizations on modern CPUs quite handily. Second, we can do all
;;; this without needing a temp register. Fixed the latter, at least.
;;; -- AB 2006/Feb/04
(define-vop (return-single)
  (:args (old-fp)
         (return-pc)
         (value))
  (:ignore value)
  (:generator 6
    (check-ocfp-and-return-pc old-fp return-pc)
    ;; Drop stack above old-fp
    (inst mov esp-tn ebp-tn)
    ;; Clear the multiple-value return flag
    (inst clc)
    ;; Restore the old frame pointer
    (inst pop ebp-tn)
    ;; And return.
    (inst ret)))

;;; Do unknown-values return of a fixed (other than 1) number of
;;; values. The VALUES are required to be set up in the standard
;;; passing locations. NVALS is the number of values returned.
;;;
;;; Basically, we just load ECX with the number of values returned and
;;; EBX with a pointer to the values, set ESP to point to the end of
;;; the values, and jump directly to return-pc.
(define-vop (return)
  (:args (old-fp)
         (return-pc :to (:eval 1))
         (values :more t))
  (:ignore values)
  (:info nvals)
  ;; In the case of other than one value, we need these registers to
  ;; tell the caller where they are and how many there are.
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
    (check-ocfp-and-return-pc old-fp return-pc)
    (when (= nvals 1)
      ;; This is handled in RETURN-SINGLE.
      (error "nvalues is 1"))
    ;; Establish the values pointer and values count.
    (inst lea ebx (make-ea :dword :base ebp-tn
                           :disp (* sp->fp-offset n-word-bytes)))
    (if (zerop nvals)
        (inst xor ecx ecx)              ; smaller
        (inst mov ecx (fixnumize nvals)))
    ;; Pre-default any argument register that need it.
    (when (< nvals register-arg-count)
      (let* ((arg-tns (nthcdr nvals (list a0 a1 a2)))
             (first (first arg-tns)))
        (inst mov first nil-value)
        (dolist (tn (cdr arg-tns))
          (inst mov tn first))))
    ;; Set the multiple value return flag.
    (inst stc)
    ;; And away we go. Except that return-pc is still on the
    ;; stack and we've changed the stack pointer. So we have to
    ;; tell it to index off of EBX instead of EBP.
    (cond ((<= nvals register-arg-count)
           (inst mov esp-tn ebp-tn)
           (inst pop ebp-tn)
           (inst ret))
          (t
           ;; Some values are on the stack after RETURN-PC and OLD-FP,
           ;; can't return normally and some slots of the frame will
           ;; be used as temporaries by the receiver.
           ;;
           ;; Clear as much of the stack as possible, but not past the
           ;; old frame address.
           (inst lea esp-tn
                 (make-ea :dword :base ebp-tn
                          :disp (frame-byte-offset (1- nvals))))
           (move ebp-tn old-fp)
           (inst push (make-ea :dword :base ebx
                               :disp (frame-byte-offset
                                      (+ sp->fp-offset
                                         (tn-offset return-pc)))))
           (inst ret)))))

;;; Do unknown-values return of an arbitrary number of values (passed
;;; on the stack.) We check for the common case of a single return
;;; value, and do that inline using the normal single value return
;;; convention. Otherwise, we branch off to code that calls an
;;; assembly-routine.
;;;
;;; The assembly routine takes the following args:
;;;  ECX -- number of values to find there.
;;;  ESI -- pointer to where to find the values.
(define-vop (return-multiple)
  (:args (old-fp)
         (return-pc)
         (vals :scs (any-reg) :target esi)
         (nvals :scs (any-reg) :target ecx))
  (:temporary (:sc unsigned-reg :offset esi-offset :from (:argument 2)) esi)
  (:temporary (:sc unsigned-reg :offset ecx-offset :from (:argument 3)) ecx)
  (:temporary (:sc descriptor-reg :offset (first *register-arg-offsets*)
                   :from (:eval 0)) a0)
  (:node-var node)
  (:generator 13
    (check-ocfp-and-return-pc old-fp return-pc)
    (unless (policy node (> space speed))
      ;; Check for the single case.
      (let ((not-single (gen-label)))
        (inst cmp nvals (fixnumize 1))
        (inst jmp :ne not-single)
        ;; Return with one value.
        (loadw a0 vals -1)
        ;; Clear the stack until ocfp.
        (inst mov esp-tn ebp-tn)
        ;; clear the multiple-value return flag
        (inst clc)
        ;; Out of here.
        (inst pop ebp-tn)
        (inst ret)
        ;; Nope, not the single case. Jump to the assembly routine.
        (emit-label not-single)))
    (move esi vals)
    (move ecx nvals)
    (inst jmp (make-fixup 'return-multiple :assembly-routine))))

;;;; XEP hackery

;;; Get the lexical environment from its passing location.
(define-vop (setup-closure-environment)
  (:results (closure :scs (descriptor-reg)))
  (:info label)
  (:ignore label)
  (:generator 6
    ;; Get result.
    (move closure eax-tn)))

;;; Copy a &MORE arg from the argument area to the end of the current
;;; frame. FIXED is the number of non-&MORE arguments.
;;;
;;; The tricky part is doing this without trashing any of the calling
;;; convention registers that are still needed. This vop is emitted
;;; directly after the xep-allocate frame. That means the registers
;;; are in use as follows:
;;;
;;;  EAX -- The lexenv.
;;;  EBX -- Available.
;;;  ECX -- The total number of arguments * N-WORD-BYTES.
;;;  EDX -- The first arg.
;;;  EDI -- The second arg.
;;;  ESI -- The third arg.
;;;
;;; So basically, we have one register available for our use: EBX.
;;;
;;; What we can do is push the other regs onto the stack, and then
;;; restore their values by looking directly below where we put the
;;; more-args.
(define-vop (copy-more-arg)
  (:info fixed)
  (:generator 20
    ;; Avoid the copy if there are no more args.
    (cond ((zerop fixed)
           (inst test ecx-tn ecx-tn)
           (inst jmp :z JUST-ALLOC-FRAME))
          (t
           (inst cmp ecx-tn (fixnumize fixed))
           (inst jmp :be JUST-ALLOC-FRAME)))

    ;; Allocate the space on the stack.
    ;; stack = ebp + sp->fp-offset - (max 3 frame-size) - (nargs - fixed)
    ;;
    ;; Problem: this might leave some &more args outside esp, so
    ;; clamp the movement for now.  If fixed > frame-size, reset
    ;; esp to the end of the current &more args (which *should*
    ;; be a noop?), and only set esp to its final value after the
    ;; stack-stack memmove loop.  Otherwise, an unlucky signal
    ;; could end up overwriting the &more arguments before they're
    ;; moved in their final place.
    (inst lea ebx-tn
          (make-ea :dword :base ebp-tn
                          :disp (* n-word-bytes
                                   (- sp->fp-offset
                                      (max 0
                                           (- (max 3 (sb-allocated-size 'stack))
                                              fixed))))))
    (inst sub ebx-tn ecx-tn)          ; Got the new stack in ebx
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
           (inst jmp :be DO-REGS))
          (t
           ;; Number to copy = nargs-fixed
           (inst sub ecx-tn (fixnumize fixed))))

    (let ((delta (* n-word-bytes
                    (- (max 3 (sb-allocated-size 'stack))
                       fixed)))
          (LOOP (gen-label)))
      (cond ((zerop delta)
             ;; nothing to move!
             )
            ((minusp delta)
             ;; stack frame smaller than fixed; moving args to higher
             ;; addresses (stack grows downard), so copy from the
             ;; end.  Moreover, because we'd have to shrink the frame,
             ;; esp currently points at the end of the source args.
             (inst push ebx-tn)

             (emit-label LOOP)
             (inst sub ecx-tn n-word-bytes)
             (inst mov ebx-tn (make-ea :dword
                                       :base esp-tn :index ecx-tn
                                       ;; compensate for PUSH above
                                       :disp n-word-bytes))
             (inst mov (make-ea :dword
                                :base esp-tn :index ecx-tn
                                ;; compensate for PUSH, and
                                ;; add (abs delta)
                                :disp (- n-word-bytes delta))
                   ebx-tn)
             (inst jmp :nz LOOP)

             (inst pop ebx-tn))
            ((plusp delta)
             ;; stack frame larger than fixed. Moving args to lower
             ;; addresses, so copy from the lowest address.  esp
             ;; already points to the lowest address of the destination.
             (inst push ebx-tn)
             (inst push esi-tn)

             (inst xor ebx-tn ebx-tn)
             (emit-label LOOP)
             (inst mov esi-tn (make-ea :dword
                                       :base esp-tn :index ebx-tn
                                       ;; PUSHed 2 words
                                       :disp (+ (* 2 n-word-bytes)
                                                delta)))
             (inst mov (make-ea :dword
                                :base esp-tn :index ebx-tn
                                :disp (* 2 n-word-bytes))
                   esi-tn)
             (inst add ebx-tn n-word-bytes)
             (inst sub ecx-tn n-word-bytes)
             (inst jmp :nz LOOP)

             (inst pop esi-tn)
             (inst pop ebx-tn))))
    DO-REGS
    ;; stack can now be set to its final size
    (when (< (max 3 (sb-allocated-size 'stack)) fixed)
      (inst add esp-tn (* n-word-bytes
                          (- fixed
                             (max 3 (sb-allocated-size 'stack))))))

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
                           :disp (* n-word-bytes
                                    (- sp->fp-offset
                                       (+ 1
                                          (- i fixed)
                                          (max 3 (sb-allocated-size
                                                  'stack))))))
              (nth i *register-arg-tns*))

        (incf i)
        (when (>= i register-arg-count)
          (return))

        ;; Don't deposit any more than there are.
        (if (zerop i)
            (inst test ecx-tn ecx-tn)
            (inst cmp ecx-tn (fixnumize i)))
        (inst jmp :eq DONE)))

    (inst jmp DONE)

    JUST-ALLOC-FRAME
    (inst lea esp-tn
          (make-ea :dword :base ebp-tn
                   :disp (* n-word-bytes
                            (- sp->fp-offset
                               (max 3 (sb-allocated-size 'stack))))))

    DONE))

(define-vop ()
  (:translate sb-c::%more-kw-arg)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:result 1))
         (index :scs (any-reg immediate) :to (:result 1) :target keyword))
  (:arg-types * tagged-num)
  (:results (value :scs (descriptor-reg any-reg))
            (keyword :scs (descriptor-reg any-reg)))
  (:result-types * *)
  (:generator 4
    (sc-case index
      (immediate
       (inst mov value (make-ea :dword :base object :disp (tn-value index)))
       (inst mov keyword (make-ea :dword :base object
                                  :disp (+ (tn-value index) n-word-bytes))))
      (t
       (inst mov value (make-ea :dword :base object :index index))
       (inst mov keyword (make-ea :dword :base object :index index
                                  :disp n-word-bytes))))))

(define-vop (more-arg/c)
  (:translate sb-c:%more-arg)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:result 1)))
  (:info index)
  (:arg-types * (:constant (signed-byte 32)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:result-types *)
  (:generator 3
    (inst mov value (make-ea :dword :base object
                                    :disp (- (* index n-word-bytes))))))

(define-vop (more-arg)
  (:translate sb-c:%more-arg)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:result 1))
         (index :scs (any-reg) :to (:result 1) :target value))
  (:arg-types * tagged-num)
  (:results (value :scs (descriptor-reg any-reg)))
  (:result-types *)
  (:generator 4
    (move value index)
    (inst neg value)
    (inst mov value (make-ea :dword :base object :index value))))

(define-vop (more-arg-or-nil)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:result 1))
         (count :scs (any-reg) :to (:result 1)))
  (:arg-types * tagged-num)
  (:info index)
  (:results (value :scs (descriptor-reg any-reg)))
  (:result-types *)
  (:generator 3
    (inst mov value nil-value)
    (inst cmp count (fixnumize index))
    (inst jmp :be done)
    (inst mov value (make-ea :dword :base object
                                    :disp (- (* index n-word-bytes))))
    done))

;;; Turn more arg (context, count) into a list.
(define-vop ()
  (:translate %listify-rest-args)
  (:policy :safe)
  ;; CONTEXT is used throughout the copying loop
  (:args (context :scs (descriptor-reg) :to :save)
         (count :scs (any-reg) :target ecx))
  (:arg-types * tagged-num)
  ;; The only advantage to specifying ECX here is that JECXZ can be used
  ;; in one place, and then only in the unlikely scenario that CONTEXT is not
  ;; in ECX. If it was, SHL sets/clears the Z flag, but LEA doesn't.
  ;; Not much of an advantage, but why not.
  (:temporary (:sc unsigned-reg :offset ecx-offset :from (:argument 1)) ecx)
  ;; Note that DST conflicts with RESULT because we use both as temps
  (:temporary (:sc unsigned-reg ) value dst)
  (:results (result :scs (descriptor-reg)))
  (:node-var node)
  (:generator 20
    (let ((loop (gen-label))
          (done (gen-label))
          (stack-allocate-p (node-stack-allocate-p node)))
      ;; Compute the number of bytes to allocate
      (let ((shift (- (1+ word-shift) n-fixnum-tag-bits)))
        (if (location= count ecx)
            (inst shl ecx shift)
            (inst lea ecx (make-ea :dword :index count :scale (ash 1 shift)))))
      ;; Setup for the CDR of the last cons (or the entire result) being NIL.
      (inst mov result nil-value)
      (inst jecxz DONE)
      (pseudo-atomic (:elide-if stack-allocate-p)
       ;; Produce an untagged pointer into DST
       (allocation 'list ecx 0 node stack-allocate-p dst)
       ;; Recalculate DST as a tagged pointer to the last cons
       (inst lea dst (make-ea :dword :disp (- list-pointer-lowtag (* cons-size n-word-bytes))
                              :base dst :index ecx))
       (inst shr ecx (1+ word-shift)) ; convert bytes to number of cells
       ;; The rightmost arguments are at lower addresses.
       ;; Start by indexing the last argument
       (inst neg ecx)
       (emit-label LOOP)
       ;; Grab one value and store into this cons. Use ECX as an index into the
       ;; vector of values in CONTEXT, but add 4 because CONTEXT points exactly at
       ;; the 0th value, which means that the index is 1 word too low.
       ;; (It's -1 if there is exactly 1 value, instead of 0, and so on)
       (inst mov value (make-ea :dword :disp 4 :base context :index ecx :scale 4))
       ;; RESULT began as NIL which gives the correct value for the CDR in the final cons.
       ;; Subsequently it points to each cons just populated, which is correct all the way
       ;; up to and including the final result.
       (storew result dst cons-cdr-slot list-pointer-lowtag)
       (storew value dst cons-car-slot list-pointer-lowtag)
       (inst mov result dst) ; preserve the value to put in the CDR of the preceding cons
       (inst sub dst (* cons-size n-word-bytes)) ; get the preceding cons
       (inst inc ecx)
       (inst jmp :nz loop))
      (emit-label done))))

;;; Return the location and size of the &MORE arg glob created by
;;; COPY-MORE-ARG. SUPPLIED is the total number of arguments supplied
;;; (originally passed in ECX). FIXED is the number of non-rest
;;; arguments.
;;;
;;; We must duplicate some of the work done by COPY-MORE-ARG, since at
;;; that time the environment is in a pretty brain-damaged state,
;;; preventing this info from being returned as values. What we do is
;;; compute supplied - fixed, and return a pointer that many words
;;; below the current stack top.
(define-vop ()
  (:policy :fast-safe)
  (:translate sb-c::%more-arg-context)
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
    (inst lea context (make-ea :dword :base esp-tn :index count
                               :disp (- (+ (fixnumize fixed) n-word-bytes))))
    (unless (zerop fixed)
      (inst sub count (fixnumize fixed)))))

;;; Signal wrong argument count error if NARGS isn't equal to COUNT.
(define-vop (verify-arg-count)
  (:policy :fast-safe)
  (:args (nargs :scs (any-reg)))
  (:arg-types positive-fixnum (:constant t) (:constant t))
  (:info min max)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 3
    (let ((err-lab
            (generate-error-code vop 'invalid-arg-count-error nargs)))
      (flet ((check-min ()
               (cond ((= min 1)
                      (inst test nargs nargs)
                      (inst jmp :e err-lab))
                     ((plusp min)
                      (inst cmp nargs (fixnumize min))
                      (inst jmp :b err-lab)))))
        (cond ((not min)
               (if (zerop max)
                   (inst test nargs nargs)
                   (inst cmp nargs (fixnumize max)))
               (inst jmp :ne err-lab))
              (max
               (check-min)
               (inst cmp nargs (fixnumize max))
               (inst jmp :a err-lab))
              (t
               (check-min)))))))

;;; Single-stepping
(defun emit-single-step-test ()
  ;; We use different ways of representing whether stepping is on on
  ;; +SB-THREAD / -SB-THREAD: on +SB-THREAD, we use a slot in the
  ;; thread structure. On -SB-THREAD we use the value of a static
  ;; symbol. Things are done this way, since reading a thread-local
  ;; slot from a symbol would require an extra register on +SB-THREAD,
  ;; and reading a slot from a thread structure would require an extra
  ;; register on -SB-THREAD.
  #+sb-thread
  (progn
    #+win32 (inst push eax-tn)
    (with-tls-ea (EA :base #+win32 eax-tn #-win32 :unused
                     :disp-type :constant
                     :disp (* thread-stepping-slot n-word-bytes))
      (inst cmp EA 0 :maybe-fs))
    #+win32 (inst pop eax-tn))
  #-sb-thread
  (inst cmp (make-ea-for-symbol-value sb-impl::*stepping*) 0))

(define-vop (step-instrument-before-vop)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 3
     (emit-single-step-test)
     (inst jmp :eq DONE)
     (inst break single-step-before-trap)
     DONE
     (note-this-location vop :internal-error)))
