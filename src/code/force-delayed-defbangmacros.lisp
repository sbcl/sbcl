;;;; Now that all the cross-compiler DEFMACRO machinery has been set up, we
;;;; can feed the stored DEF!MACRO argument lists to it.
;;;;
;;;; KLUDGE: There's no real reason for this to be in its own file, except
;;;; perhaps the parallelism with FORCE-DELAYED-DEF!STRUCTS (which does have a
;;;; good reason).

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

#+sb-xc-host (force-delayed-def!macros)
