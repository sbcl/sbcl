;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;; the maximum number of bytes per page on this system (used by GENESIS)
(defvar *backend-page-bytes* 0)
(declaim (type index *backend-page-bytes*))

#!+sb-thread
(progn
  (defglobal sb!vm::*free-tls-index* 0)
  #!-64-bit ; on 64-bit, a single lisp word holds the index and lock flag
  (!defglobal sb!vm::*tls-index-lock* 0))
