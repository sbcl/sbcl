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
;;; returning NIL for unset dispatch-macro-character functions. (bug
;;; 151, fixed by Alexey Dejenka sbcl-devel "bug 151" 2002-04-12)
(assert (not (get-dispatch-macro-character #\# #\{)))
(assert (not (get-dispatch-macro-character #\# #\0)))
;;; And we might as well test that we don't have any cross-compilation
;;; shebang residues left...
(assert (not (get-dispatch-macro-character #\# #\!)))
;;; Also test that all the illegal sharp macro characters are
;;; recognized as being illegal.
(loop for char in '(#\Backspace #\Tab #\Newline #\Linefeed
                    #\Page #\Return #\Space #\) #\<)
   do (assert (get-dispatch-macro-character #\# char)))

(assert (not (ignore-errors (get-dispatch-macro-character #\! #\0)
                            t)))

;;; In sbcl-0.7.3, GET-MACRO-CHARACTER and SET-MACRO-CHARACTER didn't
;;; use NIL to represent the no-macro-attached-to-this-character case
;;; as ANSI says they should. (This problem is parallel to the
;;; GET-DISPATCH-MACRO misbehavior fixed in sbcl-0.7.2.10, but
;;; was fixed a little later.)
(dolist (customizable-char
	 ;; According to ANSI "2.1.4 Character Syntax Types", these
	 ;; characters are reserved for the programmer.
	 '(#\? #\! #\[ #\] #\{ #\}))
  ;; So they should have no macro-characterness.
  (multiple-value-bind (macro-fun non-terminating-p)
      (get-macro-character customizable-char)
    (assert (null macro-fun))
    ;; Also, in a bit of ANSI weirdness, NON-TERMINATING-P can be
    ;; true only when MACRO-FUN is true. (When the character
    ;; is not a macro character, it can be embedded in a token,
    ;; so it'd be more logical for NON-TERMINATING-P to be T in
    ;; this case; but ANSI says it's NIL in this case.
    (assert (null non-terminating-p))))

;;; rudimentary test of SET-SYNTAX-FROM-CHAR, just to verify that it
;;; wasn't totally broken by the GET-MACRO-CHARACTER/SET-MACRO-CHARACTER
;;; fixes in 0.7.3.16
(assert (= 123579 (read-from-string "123579")))
(let ((*readtable* (copy-readtable)))
  (set-syntax-from-char #\7 #\;)
  (assert (= 1235 (read-from-string "123579"))))

;;; PARSE-INTEGER must signal an error of type PARSE-ERROR if it is
;;; unable to parse an integer and :JUNK-ALLOWED is NIL.
(macrolet ((assert-parse-error (form)
	     `(multiple-value-bind (val cond)
	          (ignore-errors ,form)
	        (assert (null val))
	        (assert (typep cond 'parse-error)))))
  (assert-parse-error (parse-integer "    "))
  (assert-parse-error (parse-integer "12 a"))
  (assert-parse-error (parse-integer "12a"))
  (assert-parse-error (parse-integer "a"))
  (assert (= (parse-integer "12") 12))
  (assert (= (parse-integer "   12   ") 12))
  (assert (= (parse-integer "   12asdb" :junk-allowed t) 12)))
