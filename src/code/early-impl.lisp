;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;; entries in STATIC-SYMBOLS table, references to which can be compiled
;;; as though they're special variables
;;;
;;; FIXME: These should be listed once and only once, instead of
;;; listed here and then listed separately (and by now, 2001-06-06,
;;; slightly differently) elsewhere.
(declaim (special *posix-argv*
		  *!initial-fdefn-objects*
		  *read-only-space-free-pointer*
		  sb!vm:*static-space-free-pointer*
		  sb!vm:*initial-dynamic-space-free-pointer*
		  *current-catch-block*
		  *current-unwind-protect-block*
		  sb!vm::*alien-stack*
		  ;; FIXME: The pseudo-atomic variable stuff should be
		  ;; conditional on :SB-PSEUDO-ATOMIC-SYMBOLS, which
		  ;; should be conditional on :X86, instead of the
		  ;; pseudo-atomic stuff being directly conditional on
		  ;; :X86. (Note that non-X86 ports mention
		  ;; pseudo-atomicity too, but they handle it without
		  ;; messing with special variables.)
		  #!+x86 *pseudo-atomic-atomic*
		  #!+x86 *pseudo-atomic-interrupted*
		  sb!unix::*interrupts-enabled*
		  sb!unix::*interrupt-pending*
		  *free-interrupt-context-index*
		  sb!vm::*allocation-pointer*
		  sb!vm::*binding-stack-pointer*
		  sb!vm::*internal-gc-trigger*
		  sb!vm::*fp-constant-0d0*
		  sb!vm::*fp-constant-1d0*
		  sb!vm::*fp-constant-0f0*
		  sb!vm::*fp-constant-1f0*
		  sb!vm::*fp-constant-0l0*
		  sb!vm::*fp-constant-1l0*
		  sb!vm::*fp-constant-pi*
		  sb!vm::*fp-constant-l2t*
		  sb!vm::*fp-constant-l2e*
		  sb!vm::*fp-constant-lg2*
		  sb!vm::*fp-constant-ln2*
		  sb!pcl::..slot-unbound..))
