;;;; cross-compile-time-only replacements for unportable character
;;;; stuff

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

(let ((ascii-standard-chars " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"))
  (defun sb-xc:code-char (x)
    (declare (type (or (integer 10 10) (integer 32 126)) x))
    (if (= x 10)
        #\Newline
        (char ascii-standard-chars (- x 32))))
  (defun sb-xc:char-code (character)
    (declare (type standard-char character))
    (if (char= character #\Newline)
        10
        (+ (position character ascii-standard-chars) 32))))
