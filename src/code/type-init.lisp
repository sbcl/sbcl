;;;; When this file's top level forms are run, it precomputes the
;;;; translations for commonly used type specifiers. This stuff is
;;;; split off from the other type stuff to get around problems with
;;;; everything needing to be loaded before everything else. This
;;;; stuff is also somewhat implementation-dependent in that
;;;; implementations may want to precompute other types which are
;;;; important to them.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;; numeric types
(/show0 "precomputing numeric types")
(precompute-types '((mod 2) (mod 4) (mod 16) (mod #x100) (mod #x10000)
                    (mod #x100000000)
                    (unsigned-byte 1) (unsigned-byte 2) (unsigned-byte 4)
                    (unsigned-byte 8) (unsigned-byte 16) (unsigned-byte 32)
                    (signed-byte 8) (signed-byte 16) (signed-byte 32)))

;;; built-in symbol type specifiers
(/show0 "precomputing built-in symbol type specifiers")
(precompute-types *!standard-type-names*)

#+sb-xc-host (setf *type-system-initialized* t)

(/show0 "done with type-init.lisp")
