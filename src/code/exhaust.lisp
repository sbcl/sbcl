;;;; detecting and handling exhaustion of memory (stack or heap)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;; FIXME: Even though this is only called when (> SAFETY (MAX SPEED SPACE))
;;; it's still annoyingly wasteful for it to be a full function call.
;;; It should probably be a VOP calling an assembly routine or something
;;; like that.
(defun %detect-stack-exhaustion ()
  ;; FIXME: Check the stack pointer against *STACK-EXHAUSTION*, and if
  ;; out of range signal an error (in a context where *S-E* has been
  ;; rebound to give some space to let error handling code do its
  ;; thing without new exhaustion problems).
  (values))
