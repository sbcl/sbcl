;;;; Undefined-function and closure trampoline definitions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

(in-package "SB-VM")

(macrolet
    ((def ((name c-name pseudo-atomic &key do-not-preserve (stack-delta 0))
           move-arg
           move-result)
       `(define-assembly-routine
            (,name (:return-style :none))
            ()
          (macrolet ((map-registers (op)
                       (let ((registers (set-difference
                                         '(rax-tn rcx-tn rdx-tn rsi-tn rdi-tn
                                           r8-tn r9-tn r10-tn r11-tn)
                                         ',do-not-preserve)))
                         ;; Preserve alignment
                         (when (oddp (length registers))
                           (push (car registers) registers))
                         `(progn
                            ,@(loop for reg in (if (eq op 'pop)
                                                   (reverse registers)
                                                   registers)
                                    collect
                                    `(inst ,op ,reg)))))
                     (map-floats (op)
                       `(progn
                          ,@(loop for i by 16
                                  for float in
                                  '(float0-tn float1-tn float2-tn float3-tn
                                    float4-tn float5-tn float6-tn float7-tn
                                    float8-tn float9-tn float10-tn float11-tn
                                    float12-tn float13-tn float14-tn float15-tn)
                                  collect
                                  (if (eql op 'pop)
                                      `(inst movaps ,float (ea ,i rsp-tn))
                                      `(inst movaps (ea ,i rsp-tn) ,float))))))
            (inst cld)
            (inst push rbp-tn)
            (inst mov rbp-tn rsp-tn)
            (inst and rsp-tn (- (* n-word-bytes 2)))
            (inst sub rsp-tn (* 16 16))
            (map-floats push)
            (map-registers push)
            ,@move-arg
            (pseudo-atomic (:elide-if (not ,pseudo-atomic))
              ;; asm routines can always call foreign code with a relative operand
              (inst call (make-fixup ,c-name :foreign)))
            ,@move-result
            (map-registers pop)
            (map-floats pop)
            (inst mov rsp-tn rbp-tn)
            (inst pop rbp-tn)
            (inst ret ,stack-delta)))))

  (def (alloc-tramp "alloc" nil)
    ((inst mov rdi-tn (ea 16 rbp-tn))) ; arg
    ((inst mov (ea 16 rbp-tn) rax-tn))) ; result

  (def (alloc-tramp-r11 "alloc" nil
                        :do-not-preserve (r11-tn)
                        :stack-delta 8) ;; remove the size parameter
    ((inst mov rdi-tn (ea 16 rbp-tn))) ; arg
    ((inst mov r11-tn rax-tn))) ; result

  #+immobile-space
  (def (alloc-layout "alloc_layout" nil :do-not-preserve (r11-tn))
    () ; no arg
    ((inst mov r11-tn rax-tn))) ; result

  ;; These routines are for the deterministic allocation profiler.
  ;; The C support routine's argument is the return PC
  (def (enable-alloc-counter "allocation_tracker_counted" t)
    ((inst lea rdi-tn (ea 8 rbp-tn))) ; arg
    ()) ; result

  (def (enable-sized-alloc-counter "allocation_tracker_sized" t)
    ((inst lea rdi-tn (ea 8 rbp-tn))) ; arg
    ())) ; result

(define-assembly-routine (undefined-tramp (:return-style :none))
    ((:temp rax descriptor-reg rax-offset))
  (inst pop (ea n-word-bytes rbp-tn))
  (emit-error-break nil cerror-trap (error-number-or-lose 'undefined-fun-error) (list rax))
  (inst push (ea n-word-bytes rbp-tn))
  (inst jmp (ea (- (* closure-fun-slot n-word-bytes) fun-pointer-lowtag) rax)))

#-sb-dynamic-core
(define-assembly-routine
    (undefined-alien-tramp (:return-style :none))
    ()
  ;; Unlike in the above UNDEFINED-TRAMP, we *should* *not* issue "POP [RBP+8]"
  ;; because that would overwrite the PC to which the calling function is
  ;; supposed to return with the address from which this alien call was made
  ;; (a PC within that same function) since C convention does not arrange
  ;; for RBP to hold the new frame pointer prior to making a call.
  ;; This wouldn't matter much because the only restart available is to throw
  ;; to toplevel, so a lost frame is not hugely important, but it's annoying.
  (error-call nil 'undefined-alien-fun-error rbx-tn))

#+sb-dynamic-core
(define-assembly-routine
    (undefined-alien-tramp (:return-style :none))
    ()
  (inst push rax-tn) ; save registers in case we want to see the old values
  (inst push rbx-tn)
  ;; load RAX with the PC after the call site
  (inst mov rax-tn (ea 16 rsp-tn))
  ;; load RBX with the signed 32-bit immediate from the call instruction
  (inst movsx '(:dword :qword) rbx-tn (ea -4 rax-tn))
  ;; if at [PC-5] we see #x25 then it was a call with 32-bit mem addr
  ;; if ...              #xE8 then ...                32-bit offset
  (inst cmp :byte (ea -5 rax-tn) #x25)
  (inst jmp :e ABSOLUTE)
  (inst cmp :byte (ea -5 rax-tn) #xE8)
  (inst jmp :e RELATIVE)
  ;; failing those, assume RBX was valid. ("can't happen")
  (inst mov rbx-tn (ea rsp-tn)) ; restore pushed value of RBX
  (inst jmp trap)
  ABSOLUTE
  (inst lea rbx-tn (ea -8 rbx-tn))
  (inst jmp TRAP)
  RELATIVE
  (inst add rbx-tn rax-tn)
  TRAP
  (error-call nil 'undefined-alien-fun-error rbx-tn))

;;; the closure trampoline - entered when a global function is a closure
;;; and the function is called "by name" (normally, as when it is the
;;; head of a form) via an FDEFN. Register %RAX holds the fdefn address,
;;; but the simple-fun which underlies the closure expects %RAX to be the
;;; closure itself. So we grab the closure out of the fdefn pointed to,
;;; then jump to the simple-fun that the closure points to.
;;;
;;; Immobile code uses a different strategy to call a closure that has been
;;; installed as a globally named function. The fdefn contains a jump opcode
;;; to a tiny code component specific to the particular closure.
;;; The trampoline is responsible for loading RAX, since named calls don't.
#-immobile-code
(define-assembly-routine
    (closure-tramp (:return-style :none))
    ()
  (loadw rax-tn rax-tn fdefn-fun-slot other-pointer-lowtag)
  (inst jmp (make-ea-for-object-slot rax-tn closure-fun-slot fun-pointer-lowtag)))

(define-assembly-routine
    (funcallable-instance-tramp (:return-style :none))
    ()
  (loadw rax-tn rax-tn funcallable-instance-function-slot fun-pointer-lowtag)
  (inst jmp (make-ea-for-object-slot rax-tn closure-fun-slot fun-pointer-lowtag)))
