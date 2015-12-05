;;;; miscellaneous stuff about the ANSI standard

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;; Common Lisp special variables which have SB-XC versions
(proclaim '(special sb!xc:*macroexpand-hook* sb!xc:*gensym-counter*))

;;; the Common Lisp defined type spec symbols
(defparameter *!standard-type-names*
  '(array atom bignum bit bit-vector character compiled-function
    complex cons double-float extended-char fixnum float function
    hash-table integer keyword list long-float nil null number package
    pathname random-state ratio rational real readtable sequence
    short-float simple-array simple-bit-vector simple-string simple-vector
    single-float standard-char stream string base-char symbol t vector))

(defvar sb!sys::*software-version* nil)
