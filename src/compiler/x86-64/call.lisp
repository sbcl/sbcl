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

(defconstant arg-count-sc (make-sc+offset any-reg-sc-number rcx-offset))
(defconstant closure-sc (make-sc+offset any-reg-sc-number rax-offset))

(defconstant return-pc-passing-offset
  (make-sc+offset sap-stack-sc-number return-pc-save-offset))

(defconstant old-fp-passing-offset
  (make-sc+offset control-stack-sc-number ocfp-save-offset))

(defun compute-linkage-cell (node name res)
  (cond ((sb-c::code-immobile-p node)
         (inst lea res (rip-relative-ea (make-fixup name :linkage-cell))))
        (t
         (inst mov res (thread-slot-ea sb-vm::thread-linkage-table-slot))
         (inst lea res (ea (make-fixup name :linkage-cell) res)))))

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
  (make-wired-tn *fixnum-primitive-type* any-reg-sc-number rcx-offset))

;;;; frame hackery

;;; This is used for setting up the Old-FP in local call.
(define-vop (current-fp)
  (:results (val :scs (any-reg control-stack)))
  (:generator 1
    (move val rbp-tn)))

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
                &optional (ea `(ea (frame-byte-offset (tn-offset variable-home-tn))
                                   frame-pointer)))
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
                        (inst ,instruction ,ea value)))))))
  (define-frame-op double-float double-reg double-stack movsd)
  (define-frame-op single-float single-reg single-stack movss)
  (define-frame-op complex-double-float complex-double-reg complex-double-stack
    movupd (ea-for-cdf-data-stack variable-home-tn frame-pointer))
  (define-frame-op complex-single-float complex-single-reg complex-single-stack
    movq   (ea-for-csf-data-stack variable-home-tn frame-pointer))
  (define-frame-op signed-byte-64 signed-reg signed-stack mov)
  (define-frame-op unsigned-byte-64 unsigned-reg unsigned-stack mov)
  (define-frame-op system-area-pointer sap-reg sap-stack mov))

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
         (signed-byte-64 signed-stack
                         ancestor-frame-ref/signed-byte-64
                         ancestor-frame-set/signed-byte-64)
         (unsigned-byte-64 unsigned-stack
                           ancestor-frame-ref/unsigned-byte-64
                           ancestor-frame-set/unsigned-byte-64)
         (unsigned-byte-63 unsigned-stack
                           ancestor-frame-ref/unsigned-byte-64
                           ancestor-frame-set/unsigned-byte-64)
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
    (popw rbp-tn (frame-word-offset return-pc-save-offset))))

(defun emit-lea (target source disp)
  (if (eql disp 0)
      (inst mov target source)
      (inst lea target (ea disp source))))

(define-vop (xep-setup-sp)
  (:generator 1
    (emit-lea rsp-tn rbp-tn     (- (* n-word-bytes
                                      (- (sb-allocated-size 'stack)
                                         sp->fp-offset))))))

;;; This is emitted directly before either a known-call-local, call-local,
;;; or a multiple-call-local. All it does is allocate stack space for the
;;; callee (who has the same size stack as us).
(define-vop (allocate-frame)
  (:results (res :scs (any-reg))
            (nfp))
  (:info callee)
  (:ignore nfp callee)
  (:generator 2
    (inst lea res (ea (- (* sp->fp-offset n-word-bytes)) rsp-tn))
    (inst sub rsp-tn (* n-word-bytes (sb-allocated-size 'stack)))))

(defun make-stack-pointer-tn (&optional nargs)
  ;; Avoid using a temporary register if the new frame pointer will be
  ;; at the same location as the new stack pointer
  (if (and nargs
           (= (* sp->fp-offset n-word-bytes)
              (* (max (if (> nargs register-arg-count)
                          nargs
                          0)
                      (sb-c::sb-size (sb-or-lose 'stack)))
                 n-word-bytes)))
      (make-wired-tn *fixnum-primitive-type* any-reg-sc-number rsp-offset)
      (make-normal-tn *fixnum-primitive-type*)))

;;; Allocate a partial frame for passing stack arguments in a full
;;; call. NARGS is the number of arguments passed. We allocate at
;;; least 2 slots, because the XEP noise is going to want to use them
;;; before it can extend the stack.
(define-vop (allocate-full-call-frame)
  (:info nargs)
  (:results (res :scs (any-reg)))
  (:generator 2
    (let ((fp-offset (* sp->fp-offset n-word-bytes))
          (stack-size (* (max (if (> nargs register-arg-count)
                                  nargs
                                  0)
                              (sb-c::sb-size (sb-or-lose 'stack)))
                         n-word-bytes)))
      (cond ((= fp-offset stack-size)
             (inst sub rsp-tn stack-size)
             (move res rsp-tn))
            (t
             (inst lea res (ea (- fp-offset) rsp-tn))
             (inst sub rsp-tn stack-size))))))

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
(defun default-unknown-values (vop values nvals node rbx move-temp)
  (declare (type (or tn-ref null) values)
           (type unsigned-byte nvals))
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
                      (not verify))))
     (flet ((check-nargs ()
              (assemble ()
                (let* ((*location-context* (list* name
                                                  (type-specifier type)
                                                  (make-restart-location SKIP)))
                       (err-lab (generate-error-code vop 'invalid-arg-count-error))
                       (min min-values)
                       (max (and (< max-values call-arguments-limit)
                                 max-values)))
                  (cond ((eql min max)
                         (if (zerop max)
                             (inst test :dword rcx-tn rcx-tn)
                             (inst cmp :dword rcx-tn (fixnumize max)))
                         (inst jmp :ne err-lab))
                        (max
                         (let ((nargs move-temp))
                          (if (zerop min)
                              (setf nargs rcx-tn)
                              (inst lea :dword move-temp (ea (fixnumize (- min)) rcx-tn)))
                          (inst cmp :dword nargs (fixnumize (- max min)))
                          (inst jmp :a err-lab)))
                        (t
                         (cond ((= min 1)
                                (inst test :dword rcx-tn rcx-tn)
                                (inst jmp :e err-lab))
                               ((plusp min)
                                (inst cmp :dword rcx-tn (fixnumize min))
                                (inst jmp :b err-lab))))))
                SKIP)))
       (cond
         ((<= nvals 1)
          (note-this-location vop :single-value-return)
          (cond
            ((and trust
                  (<= (sb-kernel:values-type-max-value-count type)
                      register-arg-count)))
            ((and trust
                  (not (sb-kernel:values-type-may-be-single-value-p type)))
             (inst mov rsp-tn rbx))
            (t
             (inst cmov :c rsp-tn rbx)
             (unless trust
               (inst mov move-temp (fixnumize 1))
               (inst cmov :nc rcx-tn move-temp)
               (check-nargs)))))
         ((<= nvals register-arg-count)
          (note-this-location vop :unknown-return)
          (when (or (not trust)
                    (sb-kernel:values-type-may-be-single-value-p type))
            (assemble ()
              (inst jmp :c regs-defaulted)
              ;; Default the unsupplied registers.
              (let* ((2nd-tn-ref (tn-ref-across values))
                     (2nd-tn (tn-ref-tn 2nd-tn-ref))
                     (2nd-tn-live (neq (tn-kind 2nd-tn) :unused)))
                (when 2nd-tn-live
                  (inst mov 2nd-tn nil-value))
                (when (> nvals 2)
                  (loop
                    for tn-ref = (tn-ref-across 2nd-tn-ref)
                    then (tn-ref-across tn-ref)
                    for count from 2 below register-arg-count
                    unless (eq (tn-kind (tn-ref-tn tn-ref)) :unused)
                    do
                    (inst mov :dword (tn-ref-tn tn-ref)
                          (if 2nd-tn-live 2nd-tn nil-value)))))
              (inst mov rbx rsp-tn)
              regs-defaulted))

          (when (or (not trust)
                    (< register-arg-count
                       (sb-kernel:values-type-max-value-count type)))
            (inst mov rsp-tn rbx))
          (unless trust
            (inst mov move-temp (fixnumize 1))
            (inst cmov :nc rcx-tn move-temp)
            (check-nargs)))
         (t
          (collect ((defaults))
            (let ((default-stack-slots (gen-label))
                  (used-registers
                    (loop for i from 1 below register-arg-count
                          for tn = (tn-ref-tn (setf values (tn-ref-across values)))
                          unless (eq (tn-kind tn) :unused)
                          collect tn
                          finally (setf values (tn-ref-across values))))
                  (used-stack-slots-p
                    (loop for ref = values then (tn-ref-across ref)
                          while ref
                          thereis (neq (tn-kind (tn-ref-tn ref)) :unused))))
              (assemble ()
                (note-this-location vop :unknown-return)
                (unless trust
                  (inst mov move-temp (fixnumize 1))
                  (inst cmov :nc rcx-tn move-temp))
                ;; If it returned exactly one value the registers and the
                ;; stack slots need to be filled with NIL.
                (cond ((and trust
                            (> min-values 1)))
                      (used-stack-slots-p
                       (inst jmp :nc default-stack-slots))
                      (t
                       (inst jmp :c regs-defaulted)
                       (loop for null = nil-value then (car used-registers)
                             for reg in used-registers
                             do (inst mov :dword reg null))
                       (inst jmp done)))
                REGS-DEFAULTED
                (do ((i register-arg-count (1+ i))
                     (val values (tn-ref-across val)))
                    ((null val))
                  (let ((tn (tn-ref-tn val)))
                    (unless (eq (tn-kind tn) :unused)
                      (when (or (not trust)
                                (>= i min-values))
                        (let ((default-lab (gen-label)))
                          (defaults (cons default-lab tn))
                          ;; Note that the max number of values received
                          ;; is assumed to fit in a :dword register.
                          (inst cmp :dword rcx-tn (fixnumize i))
                          (inst jmp :be default-lab)))
                      (sc-case tn
                        (control-stack
                         (loadw move-temp rbx (frame-word-offset (+ sp->fp-offset i)))
                         (inst mov tn move-temp))
                        (t
                         (loadw tn rbx (frame-word-offset (+ sp->fp-offset i))))))))
                DEFAULTING-DONE
                (move rsp-tn rbx)
                (unless trust
                  (check-nargs))
                DONE
                (let ((defaults (defaults)))
                  (when defaults
                    (assemble (:elsewhere)
                      (when (or (not trust)
                                (<= min-values 1))
                        (emit-label default-stack-slots)
                        (loop for null = nil-value then (car used-registers)
                              for reg in used-registers
                              do (inst mov :dword reg null))
                        (move rbx rsp-tn))
                      (dolist (default defaults)
                        (emit-label (car default))
                        (inst mov (cdr default) nil-value))
                      (inst jmp defaulting-done)))))))))))))

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
        (done (gen-label))
        (unused-count-p (eq (tn-kind count) :unused)))
    (when (sb-kernel:values-type-may-be-single-value-p type)
      (inst jmp :c variable-values)
      (cond ((eq (tn-kind start) :unused)
             (inst push (first *register-arg-tns*)))
            ((location= start (first *register-arg-tns*))
             (inst push (first *register-arg-tns*))
             (inst lea start (ea n-word-bytes rsp-tn)))
            (t (inst mov start rsp-tn)
               (inst push (first *register-arg-tns*))))
      (unless unused-count-p
        (inst mov count (fixnumize 1)))
      (inst jmp done)
      (emit-label variable-values))
    ;; The stack frame is burnt and RETurned from if there are no
    ;; stack values. In this case quickly reallocate sufficient space.
    (when (<= (sb-kernel:values-type-min-value-count type)
              register-arg-count)
      (inst cmp nargs (fixnumize register-arg-count))
      (inst jmp :g stack-values)
      #+#.(cl:if (cl:= sb-vm:word-shift sb-vm:n-fixnum-tag-bits) '(and) '(or))
      (inst sub rsp-tn nargs)
      #-#.(cl:if (cl:= sb-vm:word-shift sb-vm:n-fixnum-tag-bits) '(and) '(or))
      (let ((sub nargs))
        (unless unused-count-p
          (inst mov :dword (setf sub rax-tn) nargs))
        (inst shl :dword sub (- word-shift n-fixnum-tag-bits))
        (inst sub rsp-tn sub))
      (emit-label stack-values))
    ;; dtc: this writes the registers onto the stack even if they are
    ;; not needed, only the number specified in rcx are used and have
    ;; stack allocated to them. No harm is done.
    (loop
      for arg in *register-arg-tns*
      for i downfrom -1
      for j below (sb-kernel:values-type-max-value-count type)
      do (storew arg args i))
    (unless (eq (tn-kind start) :unused)
     (move start args))
    (unless unused-count-p
      (move count nargs))

    (emit-label done))
  (values))

;;; VOP that can be inherited by unknown values receivers. The main thing this
;;; handles is allocation of the result temporaries.
(define-vop (unknown-values-receiver)
  (:temporary (:sc descriptor-reg :offset rbx-offset
                   :from :eval :to (:result 0))
              values-start)
  (:temporary (:sc any-reg :offset rcx-offset
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
  (when (and fall-thru-p trampoline-label)
    (inst jmp start-label))
  (when trampoline-label
    (emit-label trampoline-label)
    (popw rbp-tn (frame-word-offset return-pc-save-offset)))
  (when alignp
    (emit-alignment n-lowtag-bits alignp))
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
  (:temporary (:sc any-reg) move-temp)
  (:generator 5
    (move rbp-tn fp)
    (note-this-location vop :call-site)
    (inst call target)
    (default-unknown-values vop values nvals node rbx-tn move-temp)))

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
    (move rbp-tn fp)
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
    (move rbp-tn fp)
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
    (inst leave)
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
(defmacro define-full-call (vop-name named return variable &optional args)
  (aver (not (and variable (eq return :tail))))
  `(define-vop (,vop-name ,@(when (eq return :unknown) '(unknown-values-receiver)))
     (:args    ,@(unless (eq return :tail)
                   '((new-fp :scs (any-reg) :to (:argument 1))))

               ,@(unless named ; FUN is an info argument for named call
                   '((fun :scs (descriptor-reg control-stack)
                          :target rax :to (:argument 0))))

               ,@(when (eq return :tail)
                   '((old-fp)
                     (return-pc)))

               ,@(unless variable
                   `((args :more t ,@(unless (eq args :fixed)
                                       '(:scs (descriptor-reg control-stack)))))))

     ,@(when (memq return '(:fixed :unboxed)) '((:results (values :more t))))

     (:save-p ,(if (eq return :tail) :compute-only t))

     ,@(unless (or (eq return :tail) variable)
         `((:move-args ,(if (eq args :fixed) :fixed :full-call))))

     (:vop-var vop)
     (:node-var node)
     (:info    ,@(unless (or variable (eq return :tail)) '(arg-locs))
               ,@(unless variable '(nargs))
               ;; Intuitively you might want FUN to be the first codegen arg,
               ;; but that won't work, because EMIT-ARG-MOVES wants the
               ;; passing locs in (FIRST (vop-codegen-info vop)).
               ,@(when named '(fun))
               ,@(when (eq return :fixed) '(nvals))
               step-instrumenting
               ,@(unless named '(fun-type)))

     (:ignore   ,@(unless (or variable (eq return :tail)) '(arg-locs))
                ,@(unless variable '(args))
                ,@(when (eq return :unboxed) '(values)))

     ;; For anonymous call, RAX is the function. For named call, RAX will be the linkage
     ;; table base if not stepping, or the linkage cell itself if stepping.
     ;; Calls from immobile-space without stepping avoid using RAX, and instead
     ;; access the linkage table relative to RIP.
     (:temporary (:sc descriptor-reg :offset rax-offset :from (:argument 0) :to :eval) rax)

     ;; We pass the number of arguments in RCX.
     (:temporary
      (:sc unsigned-reg :offset rcx-offset :to ,(if (eq return :fixed) :save :eval))
      rcx)

     ,@(when (eq return :fixed)
                   ;; Save it for DEFAULT-UNKNOWN-VALUES to work
         `((:temporary (:sc unsigned-reg :offset rbx-offset :from :result) rbx)
           (:temporary (:sc any-reg) move-temp)))

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
         '((:temporary (:sc unsigned-reg :from (:argument 1) :to (:argument 2))
            old-fp-tmp)))
     ,@(unless (eq return :tail) '((:node-var node)))

     (:generator ,(+ (if named 5 0)
                     (if variable 19 1)
                     (if (eq return :tail) 0 10)
                     15
                     (if (eq return :unknown) 25 0))

       (progn node) ; always "use" it

               ;; This has to be done before the frame pointer is
               ;; changed! RAX stores the 'lexical environment' needed
               ;; for closures.
       ,@(unless named '((move rax fun)))

       ,@(if variable
                     ;; For variable call, compute the number of
                     ;; arguments and move some of the arguments to
                     ;; registers.
             `((inst mov rcx new-fp)
               (inst sub rcx rsp-tn)
               (inst shr rcx ,(- word-shift n-fixnum-tag-bits))
                              ;; Move the necessary args to registers,
                              ;; this moves them all even if they are
                              ;; not all needed.
               ,@(loop for name in *register-arg-names*
                       for index downfrom -1
                       collect `(loadw ,name new-fp ,index)))
             '((cond ((listp nargs)) ;; no-verify-arg-count
                     ((zerop nargs)
                      (zeroize rcx))
                     (t
                      (inst mov rcx (fixnumize nargs))))))
       ,@(cond ((eq return :tail)
                '(        ;; Python has figured out what frame we should
                          ;; return to so might as well use that clue.
                          ;; This seems really important to the
                          ;; implementation of things like
                          ;; (without-interrupts ...)
                          ;;
                          ;; dtc; Could be doing a tail call from a
                          ;; known-call-local etc in which the old-fp
                          ;; or ret-pc are in regs or in non-standard
                          ;; places. If the passing location were
                          ;; wired to the stack in standard locations
                          ;; then these moves will be un-necessary;
                          ;; this is probably best for the x86.
                  (sc-case old-fp
                   ((control-stack)
                    (unless (= ocfp-save-offset (tn-offset old-fp))
                                      ;; FIXME: FORMAT T for stale
                                      ;; diagnostic output (several of
                                      ;; them around here), ick
                      (error "** tail-call old-fp not S0~%")
                      (move old-fp-tmp old-fp)
                      (storew old-fp-tmp rbp-tn (frame-word-offset ocfp-save-offset))))
                   ((any-reg descriptor-reg)
                    (error "** tail-call old-fp in reg not S0~%")
                    (storew old-fp rbp-tn (frame-word-offset ocfp-save-offset))))

                          ;; For tail call, we have to push the
                          ;; return-pc so that it looks like we CALLed
                          ;; despite the fact that we are going to JMP.
                  (inst push return-pc)))
               (t
                        ;; For non-tail call, we have to save our
                        ;; frame pointer and install the new frame
                        ;; pointer. We can't load stack tns after this
                        ;; point.
                `(        ;; Python doesn't seem to allocate a frame
                          ;; here which doesn't leave room for the
                          ;; ofp/ret stuff.

                          ;; The variable args are on the stack and
                          ;; become the frame, but there may be <3
                          ;; args and 3 stack slots are assumed
                          ;; allocate on the call. So need to ensure
                          ;; there are at least 3 slots. This hack
                          ;; just adds 3 more.
                  ,(if variable
                       '(inst sub rsp-tn (* 3 n-word-bytes)))

                          ;; Bias the new-fp for use as an fp
                   ,(if variable
                        '(inst sub new-fp (* sp->fp-offset n-word-bytes)))

                          ;; Save the fp
                   (storew rbp-tn new-fp (frame-word-offset ocfp-save-offset))
                   (move rbp-tn new-fp))))  ; NB - now on new stack frame.

       (when step-instrumenting
         ,@(when named '((compute-linkage-cell node fun rax)))
         (emit-single-step-test)
         (inst jmp :eq DONE)
         (inst break single-step-around-trap))
       DONE
       (note-this-location vop :call-site)
       ,(cond (named
               `(emit-direct-call fun ',(if (eq return :tail) 'jmp 'call)
                                  node step-instrumenting))
              ((eq return :tail)
               `(tail-call-unnamed rax fun-type vop))
              (t
               `(call-unnamed rax fun-type vop)))
       ,@(ecase return
           (:fixed '((default-unknown-values vop values nvals node rbx move-temp)))
           (:unknown
            '((note-this-location vop :unknown-return)
              (receive-unknown-values values-start nvals start count node)))
           ((:tail :unboxed))))))

(define-full-call call nil :fixed nil)
(define-full-call call-named t :fixed nil)
(define-full-call multiple-call nil :unknown nil)
(define-full-call multiple-call-named t :unknown nil)
(define-full-call tail-call nil :tail nil)
(define-full-call tail-call-named t :tail nil)

(define-full-call call-variable nil :fixed t)
(define-full-call multiple-call-variable nil :unknown t)
(define-full-call fixed-call-named t :fixed nil :fixed)
(define-full-call fixed-tail-call-named t :tail nil :fixed)

(define-full-call unboxed-call-named t :unboxed nil)
(define-full-call fixed-unboxed-call-named t :unboxed nil :fixed)

;;; Call NAME "directly" meaning in a single JMP or CALL instruction,
;;; if possible (without loading RAX)
(defun emit-direct-call (name instruction node step-instrumenting)
  (cond (step-instrumenting
         ;; If step-instrumenting, then RAX points to the linkage table cell
         (inst* instruction (ea rax-tn)))
        ((sb-c::code-immobile-p node)
         (inst* instruction (rip-relative-ea (make-fixup name :linkage-cell))))
        (t
         ;; get the linkage table base into RAX
         (inst mov rax-tn (thread-slot-ea sb-vm::thread-linkage-table-slot))
         (inst* instruction (ea (make-fixup name :linkage-cell) rax-tn)))))

;;; Invoke the function-designator FUN.
(defun tail-call-unnamed (fun type vop)
  (let ((relative-call (sb-c::code-immobile-p vop))
        (fun-ea (ea (- (* closure-fun-slot n-word-bytes) fun-pointer-lowtag)
                    fun)))
    (case type
      (:designator
       (assemble ()
         (%lea-for-lowtag-test rbx-tn fun fun-pointer-lowtag)
         (inst test :byte rbx-tn lowtag-mask)
         (inst jmp :nz (if relative-call
                           (make-fixup 'call-symbol :assembly-routine)
                           not-fun))
         (inst jmp fun-ea)
         not-fun
         (unless relative-call
           (invoke-asm-routine 'jmp 'call-symbol vop))))
      (:symbol
       (invoke-asm-routine 'jmp 'call-symbol vop))
      (t
       (inst jmp fun-ea)))))

(defun call-unnamed (fun type vop)
  (case type
    (:symbol
     (invoke-asm-routine 'call 'call-symbol vop))
    (t
     (assemble ()
       (when (eq type :designator)
         (%lea-for-lowtag-test rbx-tn fun fun-pointer-lowtag)
         (inst test :byte rbx-tn lowtag-mask)
         (inst jmp :z call)
         (invoke-asm-routine 'call 'call-symbol vop)
         (inst jmp ret))
       call
       (inst call (ea (- (* closure-fun-slot n-word-bytes) fun-pointer-lowtag)
                      fun))
       ret))))

;;; This is defined separately, since it needs special code that BLT's
;;; the arguments down. All the real work is done in the assembly
;;; routine. We just set things up so that it can find what it needs.
(define-vop (tail-call-variable)
  (:args (args :scs (any-reg control-stack) :target rsi)
         (function :scs (descriptor-reg control-stack) :target rax)
         (old-fp)
         (return-pc))
  (:info fun-type)
  (:temporary (:sc unsigned-reg :offset rsi-offset :from (:argument 0)) rsi)
  (:temporary (:sc unsigned-reg :offset rax-offset :from (:argument 1)) rax)
  (:vop-var vop)
  (:generator 75
    (check-ocfp-and-return-pc old-fp return-pc)
    ;; Move these into the passing locations if they are not already there.
    (move rsi args)
    (move rax function)
    ;; And jump to the assembly routine.
    (invoke-asm-routine 'jmp (if (eq fun-type :function)
                                 'tail-call-variable
                                 'tail-call-callable-variable)
                        vop)))

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
    ;; Drop stack above old-fp and restore old frame pointer
    (inst leave)
    ;; Clear the multiple-value return flag
    (inst clc)
    ;; And return.
    (inst ret)))

;;; Do unknown-values return of a fixed (other than 1) number of
;;; values. The VALUES are required to be set up in the standard
;;; passing locations. NVALS is the number of values returned.
;;;
;;; Basically, we just load RCX with the number of values returned and
;;; RBX with a pointer to the values, set RSP to point to the end of
;;; the values, and jump directly to return-pc.
(define-vop (return)
  (:args (old-fp)
         (return-pc :to (:eval 1))
         (values :more t))
  (:ignore values)
  (:info nvals)
  ;; In the case of other than one value, we need these registers to
  ;; tell the caller where they are and how many there are.
  (:temporary (:sc unsigned-reg :offset rbx-offset) rbx)
  (:temporary (:sc unsigned-reg :offset rcx-offset) rcx)
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
    (inst lea rbx (ea (* sp->fp-offset n-word-bytes) rbp-tn))
    (if (zerop nvals)
        (zeroize rcx) ; smaller
        (inst mov rcx (fixnumize nvals)))
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
    ;; tell it to index off of RBX instead of RBP.
    (cond ((<= nvals register-arg-count)
           (inst leave)
           (inst ret))
          (t
           ;; Some values are on the stack after RETURN-PC and OLD-FP,
           ;; can't return normally and some slots of the frame will
           ;; be used as temporaries by the receiver.
           ;;
           ;; Clear as much of the stack as possible, but not past the
           ;; old frame address.
           (inst lea rsp-tn
                 (ea (frame-byte-offset (1- nvals)) rbp-tn))
           (move rbp-tn old-fp)
           (inst push (ea (frame-byte-offset
                           (+ sp->fp-offset (tn-offset return-pc)))
                          rbx))
           (inst ret)))))

;;; Do unknown-values return of an arbitrary number of values (passed
;;; on the stack.) We check for the common case of a single return
;;; value, and do that inline using the normal single value return
;;; convention. Otherwise, we branch off to code that calls an
;;; assembly-routine.
;;;
;;; The assembly routine takes the following args:
;;;  RCX -- number of values to find there.
;;;  RSI -- pointer to where to find the values.
(define-vop (return-multiple)
  (:args (old-fp)
         (return-pc)
         (vals :scs (any-reg) :target rsi)
         (nvals :scs (any-reg) :target rcx))
  (:temporary (:sc unsigned-reg :offset rsi-offset :from (:argument 2)) rsi)
  (:temporary (:sc unsigned-reg :offset rcx-offset :from (:argument 3)) rcx)
  (:temporary (:sc descriptor-reg :offset (first *register-arg-offsets*)
                   :from (:eval 0)) a0)
  (:node-var node)
  (:vop-var vop)
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
        (inst leave)
        ;; clear the multiple-value return flag
        (inst clc)
        ;; Out of here.
        (inst ret)
        ;; Nope, not the single case. Jump to the assembly routine.
        (emit-label not-single)))
    (move rsi vals)
    (move rcx nvals)
    (invoke-asm-routine 'jmp 'return-multiple vop)))

;;;; XEP hackery

;;; Get the lexical environment from its passing location.
(define-vop (setup-closure-environment)
  (:results (closure :scs (descriptor-reg)))
  (:info label)
  (:ignore label)
  (:generator 6
    ;; Get result.
    (move closure rax-tn)))

;;; Copy a &MORE arg from the argument area to the end of the current
;;; frame. FIXED is the number of non-&MORE arguments.
(define-vop (copy-more-arg)
  (:temporary (:sc any-reg :offset r8-offset) copy-index)
  (:temporary (:sc any-reg :offset r9-offset) source)
  (:temporary (:sc descriptor-reg :offset r10-offset) temp)
  (:info fixed min-verified)
  (:generator 20
    ;; Avoid the copy if there are no more args.
    (cond ((zerop fixed)
           (inst test :dword rcx-tn rcx-tn)
           (inst jmp :z JUST-ALLOC-FRAME))
          ((and (eql min-verified fixed)
                (> fixed 1))
           ;; verify-arg-count will do a CMP
           (inst jmp :e JUST-ALLOC-FRAME))
          (t
           (inst cmp :dword rcx-tn (fixnumize fixed))
           (inst jmp :be JUST-ALLOC-FRAME)))

    ;; Create a negated copy of the number of arguments to allow us to
    ;; use EA calculations in order to do scaled subtraction.
    (inst mov :dword temp rcx-tn)
    (inst neg temp)

    ;; Allocate the space on the stack.
    ;; stack = rbp + sp->fp-offset - frame-size - (nargs - fixed)
    ;; if we'd move SP backward, swap the meaning of rsp and source;
    ;; otherwise, we'd be accessing values below SP, and that's no good
    ;; if a signal interrupts this code sequence.  In that case, store
    ;; the final value in rsp after the stack-stack memmove loop.
    (let* ((delta (- fixed (sb-allocated-size 'stack)))
           (loop (gen-label))
           (fixnum->word (ash 1 (- word-shift n-fixnum-tag-bits)))
           (below (plusp delta)))
      (inst lea (if below source rsp-tn)
            (ea (* n-word-bytes (+ sp->fp-offset delta))
                rbp-tn temp fixnum->word))

      ;; Now: nargs>=1 && nargs>fixed

      (cond ((< fixed register-arg-count)
             ;; the code above only moves the final value of rsp in
             ;; rsp directly if that condition is satisfied.  Currently,
             ;; r-a-c is 3, so the aver is OK. If the calling convention
             ;; ever changes, the logic above with LEA will have to be
             ;; adjusted.
             (aver (<= fixed (sb-allocated-size 'stack)))
             ;; We must stop when we run out of stack args, not when we
             ;; run out of more args.
             ;; Number to copy = nargs-3
             ;; Save the original count of args.
             (inst mov rbx-tn rcx-tn)
             (inst sub rbx-tn (fixnumize register-arg-count))
             ;; Everything of interest in registers.
             (inst jmp :be DO-REGS))
            (t
             ;; Number to copy = nargs-fixed
             (inst lea rbx-tn (ea (- (fixnumize fixed)) rcx-tn))))

      ;; Initialize R8 to be the end of args.
      ;; Swap with SP if necessary to mirror the previous condition
      (unless (zerop delta)
        (inst lea (if below rsp-tn source)
              (ea (* sp->fp-offset n-word-bytes)
                  rbp-tn temp fixnum->word)))

      ;; src: rbp + temp + sp->fp
      ;; dst: rbp + temp + sp->fp + (fixed - [stack-size])
      (cond ((zerop delta))             ; no-op move
            ((minusp delta)
             ;; dst is lower than src, copy forward
             (zeroize copy-index)
             ;; We used to use REP MOVS here, but on modern x86 it performs
             ;; much worse than an explicit loop for small blocks.

             (emit-label loop)
             (inst mov temp (ea source copy-index))
             (inst mov (ea rsp-tn copy-index) temp)
             (inst add copy-index n-word-bytes)
             (inst sub rbx-tn (fixnumize 1))
             (inst jmp :nz loop))
            ((plusp delta)
             ;; dst is higher than src; copy backward
             (emit-label loop)
             (inst sub rbx-tn (fixnumize 1))
             (inst mov temp (ea rsp-tn rbx-tn fixnum->word))
             (inst mov (ea source rbx-tn fixnum->word) temp)
             (inst jmp :nz loop)
             ;; done with the stack--stack copy. Reset RSP to its final
             ;; value
             (inst mov rsp-tn source))))
    DO-REGS

    ;; Here: nargs>=1 && nargs>fixed
    (when (< fixed register-arg-count)
      ;; Now we have to deposit any more args that showed up in
      ;; registers.
      (do ((i fixed))
          ( nil )
        ;; Store it relative to rbp
        (inst mov (ea (* n-word-bytes
                         (- sp->fp-offset
                            (+ 1 (- i fixed) (sb-allocated-size 'stack))))
                       rbp-tn)
              (nth i *register-arg-tns*))

        (incf i)
        (when (>= i register-arg-count)
          (return))

        ;; Don't deposit any more than there are.
        #.(assert (= register-arg-count 3))
        (cond ((> fixed 0)
               (inst cmp :dword rcx-tn (fixnumize i))
               (inst jmp :eq DONE))
              ;; Use a single comparison for 1 and 2
              ((= i 1)
               (inst cmp :dword rcx-tn (fixnumize 2))
               (inst jmp :l DONE))
              (t
               (inst jmp :eq DONE)))))

    (inst jmp DONE)

    JUST-ALLOC-FRAME
    (emit-lea rsp-tn rbp-tn
                         (* n-word-bytes
                            (- sp->fp-offset
                               (sb-allocated-size 'stack))))

    DONE))

(define-vop ()
  (:translate sb-c::%more-kw-arg)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:result 1))
         (index :scs (any-reg) :to (:result 1) :target keyword))
  (:arg-types * tagged-num)
  (:results (value :scs (descriptor-reg any-reg))
            (keyword :scs (descriptor-reg any-reg)))
  (:result-types * *)
  (:generator 4
     (inst mov value (ea object index (ash 1 (- word-shift n-fixnum-tag-bits))))
     (inst mov keyword (ea n-word-bytes object index
                           (ash 1 (- word-shift n-fixnum-tag-bits))))))

(define-vop (more-arg/c)
  (:translate sb-c:%more-arg)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:result 1)))
  (:info index)
  (:arg-types * (:constant (signed-byte #.(- 32 word-shift))))
  (:results (value :scs (descriptor-reg any-reg)))
  (:result-types *)
  (:generator 3
    (inst mov value (ea (- (* index n-word-bytes)) object))))

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
    (inst mov value (ea object value
                        (ash 1 (- word-shift n-fixnum-tag-bits))))))

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
    (inst mov value (ea (- (* index n-word-bytes)) object))
    done))

;;; Turn more arg (context, count) into a list.
;;; Cons cells will be filled in right-to-left.
;;; This has a slight advantage in code size, and eliminates an initial
;;; forward jump into the loop. it also admits an interesting possibility
;;; to reduce the scope of the pseudo-atomic section so as not to
;;; encompass construction of the list. To do that, we will need to invent
;;; a new widetag for "contiguous CONS block" which has a header conveying
;;; the total payload length. Initially we would store that into the CAR of the
;;; first cons cell. Upon seeing such header, GC shall treat that entire object
;;; as a boxed payload of specified length. It will be implicitly pinned
;;; (if conservative) or transported as a whole (if precise). Then when the CAR
;;; of the first cons is overwritten, the object changes to a linked list.
(define-vop ()
  (:translate %listify-rest-args)
  (:policy :safe)
  ;; CONTEXT is used throughout the copying loop
  (:args (context :scs (descriptor-reg) :to :save)
         (count :scs (any-reg) :target rcx))
  (:arg-types * tagged-num)
  ;; The only advantage to specifying RCX here is that JRCXZ can be used
  ;; in one place, and then only in the unlikely scenario that CONTEXT is not
  ;; in RCX. If it was, SHL sets/clears the Z flag, but LEA doesn't.
  ;; Not much of an advantage, but why not.
  (:temporary (:sc unsigned-reg :offset rcx-offset :from (:argument 1)) rcx)
  ;; Note that DST conflicts with RESULT because we use both as temps
  (:temporary (:sc unsigned-reg) value dst)
  #+gs-seg (:temporary (:sc unsigned-reg :offset 15) thread-tn)
  (:results (result :scs (descriptor-reg)))
  (:node-var node)
  (:generator 20
#|
    ;; TODO: if instrumenting, just revert to the older way of precomputing
    ;; a size rather than scaling by 8 in ALLOCATION so that we don't have
    ;; to scale and unscale.
    ;; Compute the number of bytes to allocate
    (let ((shift (- (1+ word-shift) n-fixnum-tag-bits)))
      (if (location= count rcx)
          (inst shl :dword rcx shift)
          (inst lea :dword rcx (ea nil count (ash 1 shift)))))
|#
    (move rcx count :dword)
    ;; Setup for the CDR of the last cons (or the entire result) being NIL.
    (inst mov result nil-value)
    (cond ((not (member :allocation-size-histogram sb-xc:*features*))
           (inst jrcxz DONE))
          (t ; jumps too far for JRCXZ sometimes
           (inst test rcx rcx)
           (inst jmp :z done)))
    (when (and (not (node-stack-allocate-p node)) (instrument-alloc-policy-p node))
      (inst shl :dword rcx word-shift) ; compute byte count
      (instrument-alloc +cons-primtype+ rcx node (list value dst) thread-tn)
      (inst shr :dword rcx word-shift)) ; undo the computation
    (pseudo-atomic (:elide-if (node-stack-allocate-p node) :thread-tn thread-tn)
       ;; Produce an untagged pointer into DST
      (let ((scale
             (cond ((node-stack-allocate-p node)
                    ;; LEA on RSP would be ok but we'd need to negate RCX first, then un-negate
                    ;; to compute the final cons, then negate again. So use SHL and SUB instead.
                    (inst shl :dword rcx word-shift)
                    (stack-allocation rcx 0 dst)
                    1)
                   (t
                    (allocation +cons-primtype+ rcx 0 dst node value thread-tn
                       :scale 8
                       :overflow
                       (lambda ()
                         (inst push rcx)
                         (inst push context)
                         (invoke-asm-routine
                          'call (if (system-tlab-p 0 node) 'sys-listify-&rest 'listify-&rest)
                          node)
                         (inst pop result)
                         (inst jmp alloc-done)))
                    8))))
       ;; Recalculate DST as a tagged pointer to the last cons
       (inst lea dst (ea (- list-pointer-lowtag (* cons-size n-word-bytes)) dst rcx scale))
       ;; scale=8 implies RCX counts ncells (as a fixnum) therefore just untag it.
       ;; scale=1 implies RCX counts nbytes therefore ncells = RCX/16
       (inst shr :dword rcx (if (= scale 8) n-fixnum-tag-bits (1+ word-shift))))
       ;; The rightmost arguments are at lower addresses.
       ;; Start by indexing the last argument
       (inst neg rcx) ; :QWORD because it's negative
       LOOP
       ;; Grab one value and store into this cons. Use RCX as an index into the
       ;; vector of values in CONTEXT, but add 8 because CONTEXT points exactly at
       ;; the 0th value, which means that the index is 1 word too low.
       ;; (It's -1 if there is exactly 1 value, instead of 0, and so on)
       (inst mov value (ea 8 context rcx 8))
       ;; RESULT began as NIL which gives the correct value for the CDR in the final cons.
       ;; Subsequently it points to each cons just populated, which is correct all the way
       ;; up to and including the final result.
       (storew result dst cons-cdr-slot list-pointer-lowtag)
       (storew value dst cons-car-slot list-pointer-lowtag)
       (inst mov result dst) ; preserve the value to put in the CDR of the preceding cons
       (inst sub dst (* cons-size n-word-bytes)) ; get the preceding cons
       (inst inc rcx) ; :QWORD because it's negative
       (inst jmp :nz loop)
       ALLOC-DONE)
    DONE))

;;; Return the location and size of the &MORE arg glob created by
;;; COPY-MORE-ARG. SUPPLIED is the total number of arguments supplied
;;; (originally passed in RCX). FIXED is the number of non-rest
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
    (inst lea context (ea (- (* (1+ fixed) n-word-bytes))
                          rsp-tn count
                          (ash 1 (- word-shift n-fixnum-tag-bits))))
    (unless (zerop fixed)
      (inst sub count (fixnumize fixed)))))

(define-vop (verify-arg-count)
  (:policy :fast-safe)
  (:args (nargs :scs (any-reg)))
  (:arg-types positive-fixnum (:constant t) (:constant t))
  (:temporary (:sc unsigned-reg :offset rbx-offset) temp)
  (:info min max)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 3
    ;; NOTE: copy-more-arg expects this to issue a CMP for min > 1
    (let ((err-lab
            (generate-error-code vop 'invalid-arg-count-error nargs)))
      (cond ((not min)
             (if (zerop max)
                 (inst test :dword nargs nargs)
                 (inst cmp :dword nargs (fixnumize max)))
             (inst jmp :ne err-lab))
            (max
             (if (zerop min)
                 (setf temp nargs)
                 (inst lea :dword temp (ea (fixnumize (- min)) nargs)))
             (inst cmp :dword temp (fixnumize (- max min)))
             (inst jmp :a err-lab))
            (t
             (cond ((= min 1)
                    (inst test :dword nargs nargs)
                    (inst jmp :e err-lab))
                   ((plusp min)
                    (inst cmp :dword nargs (fixnumize min))
                    (inst jmp :b err-lab))))))))
;;; Single-stepping

(defun emit-single-step-test ()
  ;; We use different ways of representing whether stepping is on on
  ;; +SB-THREAD / -SB-THREAD: on +SB-THREAD, we use a slot in the
  ;; thread structure. On -SB-THREAD we use the value of a static
  ;; symbol. Things are done this way, since reading a thread-local
  ;; slot from a symbol would require an extra register on +SB-THREAD,
  ;; and reading a slot from a thread structure would require an extra
  ;; register on -SB-THREAD. While this isn't critical for x86-64,
  ;; it's more serious for x86.
  #+sb-thread (inst cmp :byte (thread-slot-ea thread-stepping-slot) 0)
  #-sb-thread (inst cmp :byte (static-symbol-value-ea 'sb-impl::*stepping*) 0))

(define-vop (step-instrument-before-vop)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 3
     (emit-single-step-test)
     (inst jmp :eq DONE)
     (inst break single-step-before-trap)
     DONE
     (note-this-location vop :internal-error)))
