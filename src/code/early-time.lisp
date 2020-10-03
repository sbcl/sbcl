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

(in-package "SB-IMPL")

(defconstant internal-time-units-per-second
  #+64-bit 1000000 ; microseconds
  ;; 1 week in milliseconds is: (* 1000 60 60 24 7) = 604800000 which is
  ;; a 30-bit number, but there are only 29 bits in a positive fixnum.
  ;; It is left as an exercise to change the 32-bit code to use hundredths
  ;; instead of thousands of a second. The upside of so doing is that
  ;; GET-INTERNAL-REAL-TIME would be able to express more range without consing.
  ;; The downside is of course decreased resolution.
  #-64-bit 1000    ; milliseconds
  "The number of internal time units that fit into a second. See
GET-INTERNAL-REAL-TIME and GET-INTERNAL-RUN-TIME.")
