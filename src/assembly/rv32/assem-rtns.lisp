;;;; the machine specific support routines needed by the file assembler

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(define-assembly-routine (throw (:return-style :none))
  ((:arg target (descriptor-reg any-reg) a0-offset)
   (:arg start (descriptor-reg any-reg) ocfp-offset)
   (:arg count (descriptor-reg any-reg) nargs-offset))
  (declare (ignore target start count)))

(define-assembly-routine (unwind (:return-style :none))
  ((:arg block (descriptor-reg any-reg) a0-offset)
   (:arg start (descriptor-reg any-reg) ocfp-offset)
   (:arg count (descriptor-reg any-reg) nargs-offset))
  (declare (ignore block start count)))
