;;;; ARM VM definitions of various system hacking operations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; other miscellaneous VOPs

(define-vop (halt)
  (:temporary (:sc non-descriptor-reg :offset ocfp-offset) error-temp)
  (:generator 1
    ;; See macros.lisp, EMIT-ERROR-BREAK, for an explanation.
    (inst mov error-temp #x000f0000)
    (inst add error-temp error-temp 1)
    (inst swi 0)
    (inst byte halt-trap)
    ;; Re-align to the next instruction boundary.
    (emit-alignment word-shift)))
