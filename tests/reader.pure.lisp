;;;; tests related to the Lisp reader

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;; 
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(in-package "CL-USER")

(assert (equal (symbol-name '#:|fd\sA|) "fdsA"))

;;; Prior to sbcl-0.7.2.10, SBCL disobeyed the ANSI requirements on
;;; returning NIL for unset dispatch-macro-character functions (bug
;;; 151, fixed by Alexey Dejenka sbcl-devel "bug 151" 2002-04-12)
(assert (not (get-dispatch-macro-character #\# #\{)))
(assert (not (get-dispatch-macro-character #\# #\0)))
;;; and we might as well test that we don't have any cross-compilation
;;; shebang residues left...
(assert (not (get-dispatch-macro-character #\# #\!)))
;;; also test that all the illegal sharp macro characters are
;;; recognized as being illegal.
(loop for char in '(#\Backspace #\Tab #\Newline #\Linefeed
                    #\Page #\Return #\Space #\) #\<)
   do (assert (get-dispatch-macro-character #\# char)))

(assert (not (ignore-errors (get-dispatch-macro-character #\! #\0)
                            t)))
