;;;; system memory allocation

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

(sb!alien:define-alien-routine ("os_allocate" allocate-system-memory)
                               system-area-pointer
  (bytes sb!alien:unsigned-long))

(sb!alien:define-alien-routine ("os_deallocate" deallocate-system-memory)
                               sb!alien:void
  (addr system-area-pointer)
  (bytes sb!alien:unsigned-long))
