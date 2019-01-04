;;;; type testing and checking VOPs for the RV32 VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;; Test generation utilities.
(defun %test-fixnum (value temp target not-p))

(defun %test-fixnum-and-headers (value temp target not-p headers &key value-tn-ref))

(defun %test-headers (value temp target not-p function-p headers
                      &key (drop-through (gen-label)) value-tn-ref))

(defun %test-lowtag (value temp target not-p lowtag))
