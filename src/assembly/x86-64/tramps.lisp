;;;; Undefined-function and closure trampoline definitions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

(in-package "SB-VM")

(macrolet ((do-fprs (operation regset)
             (multiple-value-bind (mnemonic fpr-align)
                 (ecase regset
                   (:xmm (values 'movaps 16))
                   (:ymm (values 'vmovaps 32)))
               `(progn
                  ,@(loop for regno below 16
                       collect
                         (ecase operation
                           (push
                            `(inst ,mnemonic (ea ,(+ 8 (* regno fpr-align)) rsp-tn)
                                   (sb-x86-64-asm::get-fpr ,regset ,regno)))
                           (pop
                            `(inst ,mnemonic (sb-x86-64-asm::get-fpr ,regset ,regno)
                                   (ea ,(+ 8 (* regno fpr-align)) rsp-tn)))))))))
  ;; Caller will have allocated 512 bytes above the stack-pointer
  ;; prior to the CALL. Use that as the save area.
  (define-assembly-routine (save-ymm) () (do-fprs push :ymm))
  (define-assembly-routine (restore-ymm) () (do-fprs pop :ymm))
  ;; As above, but only 256 bytes of the save area are needed, the rest goes to waste.
  (define-assembly-routine (save-xmm (:export fpr-save)) ()
    fpr-save ; KLUDGE: this is element 4 of the entry point vector
    (do-fprs push :xmm))
  (define-assembly-routine (restore-xmm (:export fpr-restore)) ()
    fpr-restore ; KLUDGE: this is element 6 of the entry point vector
    (do-fprs pop :xmm)))

(define-assembly-routine (alloc-tramp) ()
  (with-registers-preserved (c)
    (inst mov rdi-tn (ea 16 rbp-tn))
    (inst call (make-fixup "alloc" :foreign))
    (inst mov (ea 16 rbp-tn) rax-tn))) ; result onto stack

(define-assembly-routine (list-alloc-tramp) ()
  (with-registers-preserved (c)
    (inst mov rdi-tn (ea 16 rbp-tn))
    (inst call (make-fixup "alloc_list" :foreign))
    (inst mov (ea 16 rbp-tn) rax-tn))) ; result onto stack

;;; These routines are for the deterministic consing profiler.
;;; The C support routine's argument is the return PC.
(define-assembly-routine (enable-alloc-counter) ()
  (with-registers-preserved (c)
    (inst lea rdi-tn (ea 8 rbp-tn))
    (pseudo-atomic () (inst call (make-fixup "allocation_tracker_counted" :foreign)))))

(define-assembly-routine (enable-sized-alloc-counter) ()
  (with-registers-preserved (c)
    (inst lea rdi-tn (ea 8 rbp-tn))
    (pseudo-atomic () (inst call (make-fixup "allocation_tracker_sized" :foreign)))))

(define-assembly-routine (undefined-tramp (:return-style :none))
    ((:temp rax descriptor-reg rax-offset))
  (inst pop (ea n-word-bytes rbp-tn))
  (emit-error-break nil cerror-trap (error-number-or-lose 'undefined-fun-error) (list rax))
  (inst push (ea n-word-bytes rbp-tn))
  (inst jmp (ea (- (* closure-fun-slot n-word-bytes) fun-pointer-lowtag) rax)))

#+win32
(define-assembly-routine
    (undefined-alien-tramp (:return-style :none))
    ()
  (error-call nil 'undefined-alien-fun-error rbx-tn))

#-win32
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
  (inst jmp (object-slot-ea rax-tn closure-fun-slot fun-pointer-lowtag)))

(define-assembly-routine
    (funcallable-instance-tramp (:return-style :none))
    ()
  (loadw rax-tn rax-tn funcallable-instance-function-slot fun-pointer-lowtag)
  (inst jmp (object-slot-ea rax-tn closure-fun-slot fun-pointer-lowtag)))

(define-assembly-routine (ensure-symbol-hash (:return-style :raw)) ()
  (with-registers-preserved (lisp)
    (inst mov rdx-tn (ea 16 rbp-tn)) ; arg
    (call-static-fun 'ensure-symbol-hash 1)
    (inst mov (ea 16 rbp-tn) rdx-tn))) ; result to arg passing loc
