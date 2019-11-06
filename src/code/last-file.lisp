;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;; Verify on startup that some constants were dumped reflecting the correct
;;; action of our vanilla-host-compatible functions.
;;; For now, just SXHASH is checked.

;;; Parallelized build doesn't get the full set of data because the side effect
;;; of data recording when invoking compile-time functions aren't propagated
;;; back to process that forked the children doing the grunt work.
(defvar sb-c::*sxhash-crosscheck* '#.sb-c::*sxhash-crosscheck*)
(defun check-compile-time-sxhashes ()
  (loop for (object . hash) in sb-c::*sxhash-crosscheck*
        unless (= (sxhash object) hash)
        do (error "SB-XC:SXHASH computed wrong answer for ~S. Got ~x should be ~x"
                  object hash (sxhash object))))
(check-compile-time-sxhashes)
