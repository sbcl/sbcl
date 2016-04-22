;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;; the size of the chunks returned by RANDOM-CHUNK
(def!constant n-random-chunk-bits 32)

(sb!xc:defstruct (random-state (:constructor %make-random-state
                                   (state))
                               ;; Needed for reading #S(random-state)
                               (:constructor %%make-random-state)
                               (:copier nil)) ; since shallow copy is wrong
  (state (init-random-state)
   :type (simple-array (unsigned-byte 32) (627))
   :read-only t))

(declaim (freeze-type random-state))
