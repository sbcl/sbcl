;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

(define-alien-routine ("purify" %purify) void
  (static-roots unsigned-long)
  (read-only-roots unsigned-long))

(defun purify (&key root-structures environment-name)
  "This function optimizes garbage collection by moving all currently live
   objects into non-collected storage. ROOT-STRUCTURES is an optional list of
   objects which should be copied first to maximize locality.

   DEFSTRUCT structures defined with the (:PURE T) option are moved into
   read-only storage, further reducing GC cost. List and vector slots of pure
   structures are also moved into read-only storage.

   ENVIRONMENT-NAME is unused.

   This function is a no-op on platforms using the generational garbage
   collector (x86, x86-64, ppc, arm, arm64)."
  (declare (ignore environment-name))
  (%purify (get-lisp-obj-address root-structures)
           (get-lisp-obj-address nil)))
