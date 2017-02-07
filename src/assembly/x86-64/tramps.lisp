;;;; Undefined-function and closure trampoline definitions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

(in-package "SB!VM")

(define-assembly-routine
    (undefined-tramp (:return-style :none))
    ((:temp rax descriptor-reg rax-offset))
  #!+immobile-code
  (progn
    (inst pop rax) ; gets the address of the fdefn (plus some)
    (inst sub (reg-in-size rax :dword)
          (+ 5 (ash fdefn-raw-addr-slot word-shift)(- other-pointer-lowtag))))
  (inst pop (make-ea :qword :base rbp-tn :disp n-word-bytes))
  (emit-error-break nil cerror-trap (error-number-or-lose 'undefined-fun-error) (list rax))
  (inst push (make-ea :qword :base rbp-tn :disp n-word-bytes))
  #!-immobile-code
  (inst jmp
        (make-ea :qword :base rax
                        :disp (- (* fdefn-raw-addr-slot
                                    n-word-bytes)
                                 other-pointer-lowtag)))
  #!+immobile-code
  ;; No single instruction can jump to the raw function in an fdefn.
  ;; There are a couple ways to go about it: load the tagged function object
  ;; and call that, or circuitously "return" to an address that directs
  ;; control flow to the raw function. This logic opts for the latter,
  ;; on the grounds that it does not mutate the contents of RAX.
  ;; FIXME: Maybe it would be ok to just add 9 to rax and jump there,
  ;; but have to think about lifetime of the FDEFN if we have no pointer
  ;; to it. Or teach GC about interior pointers to fdefns. Or something.
  (progn (inst push rax)
         (inst add (make-ea :qword :base rsp-tn) 9)
         (inst ret)))

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
