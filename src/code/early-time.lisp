;;;; Time-related constants that are needed before unix.lisp / win32.lisp
;;;; can be built.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(defconstant sb!xc:internal-time-units-per-second 1000
  #!+sb-doc
  "The number of internal time units that fit into a second. See
GET-INTERNAL-REAL-TIME and GET-INTERNAL-RUN-TIME.")
