;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(file-comment
 "$Header$")

;;; entries in STATIC-SYMBOLS table, references to which can be compiled
;;; as though they're special variables
(declaim (special *posix-argv*
		  *!initial-fdefn-objects*
		  *read-only-space-free-pointer*
		  sb!vm:*static-space-free-pointer*
		  sb!vm:*initial-dynamic-space-free-pointer*
		  *current-catch-block*
		  *current-unwind-protect-block*
		  sb!c::*eval-stack-top*
		  sb!vm::*alien-stack*
		  ;; KLUDGE: I happened to notice that these should be #!+X86.
		  ;; There could easily be others in the list, too.
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
		  sb!vm::*fp-constant-0s0*
		  sb!vm::*fp-constant-1s0*
		  sb!vm::*fp-constant-0l0*
		  sb!vm::*fp-constant-1l0*
		  sb!vm::*fp-constant-pi*
		  sb!vm::*fp-constant-l2t*
		  sb!vm::*fp-constant-l2e*
		  sb!vm::*fp-constant-lg2*
		  sb!vm::*fp-constant-ln2*
		  sb!vm::*scavenge-read-only-space*
		  sb!vm::*control-stacks*
		  sb!pcl::..slot-unbound..
		  sb!vm::*x86-cgc-active-p*
		  sb!vm::*static-blue-bag*))
