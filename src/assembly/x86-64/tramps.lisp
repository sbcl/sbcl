;;;; Undefined-function and closure trampoline definitions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

(in-package "SB!VM")

;;; KLUDGE: this has to be the very first code component in immobile space
;;; due to hardcoding of the address in arch_os_link_runtime().
#!+immobile-space
(define-assembly-routine
    (alloc-tramp (:return-style :none))
    ()
  ;; This is a shell of a routine. We leave filler enough to write two JMP
  ;; instructions in the manner of linkage table entries. This is so that CALL
  ;; can be used with a rel32 operand from immobile code without assuming a fixed
  ;; difference between the immobile space and linkage table base addresses.
  ;; (Assembly routines are themselves in immobile space)
  ;; The entries are "alloc_tramp" and "alloc_to_r11", in that order.
  (dotimes (i (* 2 sb!vm:linkage-table-entry-size))
    (inst byte 0)))

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
