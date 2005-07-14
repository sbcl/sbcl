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
(def!constant random-chunk-length 32)

;;; the amount that we overlap chunks by when building a large integer
;;; to make up for the loss of randomness in the low bits
(def!constant random-integer-overlap 3)

;;; extra bits of randomness that we generate before taking the value MOD the
;;; limit, to avoid loss of randomness near the limit
(def!constant random-integer-extra-bits 10)

;;; the largest fixnum we can compute from one chunk of bits
(def!constant random-fixnum-max
  (1- (ash 1 (- random-chunk-length random-integer-extra-bits))))

(sb!xc:defstruct (random-state (:constructor %make-random-state)
                               (:copier nil)) ; since shallow copy is wrong
  (state (init-random-state) :type (simple-array (unsigned-byte 32) (627))))
