;;;; character implementation stuff which is to be visible at
;;;; build-the-cross-compiler time

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(def!constant sb!xc:char-code-limit #!-sb-unicode 256 #!+sb-unicode #x110000
  #!+sb-doc
  "the upper exclusive bound on values produced by CHAR-CODE")

(def!constant base-char-code-limit #!-sb-unicode 256 #!+sb-unicode 128)
