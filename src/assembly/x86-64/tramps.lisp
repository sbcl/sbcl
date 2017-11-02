;;;; Undefined-function and closure trampoline definitions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

(in-package "SB!VM")

(macrolet
    ((def ((name &key do-not-preserve (stack-delta 0))
           &body move-result)
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
                                      `(inst movaps ,float (make-ea :qword :base rsp-tn :disp ,i))
                                      `(inst movaps (make-ea :qword :base rsp-tn :disp ,i) ,float))))))
            (inst cld)
            (inst push rbp-tn)
            (inst mov rbp-tn rsp-tn)
            (inst and rsp-tn (- (* n-word-bytes 2)))
            (inst sub rsp-tn (* 16 16))
            (map-floats push)
            (map-registers push)
            (inst mov rdi-tn (make-ea :qword :base rbp-tn :disp 16))
            (inst mov rax-tn (make-fixup "alloc" :foreign))
            (inst call rax-tn)
            ,@move-result
            (map-registers pop)
            (map-floats pop)
            (inst mov rsp-tn rbp-tn)
            (inst pop rbp-tn)
            (inst ret ,stack-delta)))))
  (def (alloc-tramp)
    (inst mov (make-ea :qword :base rbp-tn :disp 16) rax-tn))
  (def (alloc-tramp-r11 :do-not-preserve (r11-tn)
                        :stack-delta 8) ;; remove the size parameter
    (inst mov r11-tn rax-tn)))

(define-assembly-routine
    (undefined-tramp (:return-style :none))
    ((:temp rax descriptor-reg rax-offset))
  #!+immobile-code
  (progn
    (inst pop rax) ; gets the address of the fdefn (plus some)
    (inst sub (reg-in-size rax :dword)
          ;; Subtract the length of the JMP instruction plus offset to the
          ;; raw-addr-slot, and add back the lowtag. Voila, a tagged descriptor.
          (+ 5 (ash fdefn-raw-addr-slot word-shift) (- other-pointer-lowtag))))
  (inst pop (make-ea :qword :base rbp-tn :disp n-word-bytes))
  (emit-error-break nil cerror-trap (error-number-or-lose 'undefined-fun-error) (list rax))
  (inst push (make-ea :qword :base rbp-tn :disp n-word-bytes))
  (inst jmp
        (make-ea :qword :base rax
                        :disp (- (* closure-fun-slot n-word-bytes)
                                 fun-pointer-lowtag))))

(define-assembly-routine
    (undefined-alien-tramp (:return-style :none))
    ()
  (inst pop (make-ea :qword :base rbp-tn :disp n-word-bytes))
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
#!-immobile-code
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
