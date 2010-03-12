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

;;; #A notation enforces that once one 0 dimension has been found, all
;;; subsequent ones are also 0.
(assert (equal (array-dimensions (read-from-string "#3A()"))
               '(0 0 0)))
(assert (equal (array-dimensions (read-from-string "#3A(())"))
               '(1 0 0)))
(assert (equal (array-dimensions (read-from-string "#3A((() ()))"))
               '(1 2 0)))

;;; Bug reported by Nikodemus Siivola on sbcl-devel 2003-07-21:
;;; package misconfiguration
(assert (eq
         (handler-case (with-input-from-string (s "cl:") (read s))
           (end-of-file (c)
             'good))
         'good))

;;; Bugs found by Paul Dietz
(assert (equal (multiple-value-list
                (parse-integer "   123      "))
               '(123 12)))

(let* ((base "xxx 123  yyy")
       (intermediate (make-array 8 :element-type (array-element-type base)
                                 :displaced-to base
                                 :displaced-index-offset 2))
       (string (make-array 6 :element-type (array-element-type base)
                           :displaced-to intermediate
                           :displaced-index-offset 1)))
  (assert (equal (multiple-value-list
                  (parse-integer string))
                 '(123 6))))

(let ((*read-base* *read-base*))
  (dolist (float-string '(".9" ".9e9" ".9e+9" ".9e-9"
                          "-.9" "-.9e9" "-.9e+9" "-.9e-9"
                          "+.9" "+.9e9" "+.9e+9" "+.9e-9"
                          "0.9" "0.9e9" "0.9e+9" "0.9e-9"
                          "9.09" "9.09e9" "9.09e+9" "9.09e-9"
                          #|"9e9" could be integer|# "9e+9" "9e-9"))
    (loop for i from 2 to 36
          do (setq *read-base* i)
          do (assert (typep (read-from-string float-string)
                            *read-default-float-format*))
          do (assert (typep
                      (read-from-string (substitute #\E #\e float-string))
                      *read-default-float-format*))
          if (position #\e float-string)
          do (assert (typep
                      (read-from-string (substitute #\s #\e float-string))
                      'short-float))
          and do (assert (typep
                          (read-from-string (substitute #\S #\e float-string))
                          'short-float))
          and do (assert (typep
                          (read-from-string (substitute #\f #\e float-string))
                          'single-float))
          and do (assert (typep
                          (read-from-string (substitute #\F #\e float-string))
                          'single-float))
          and do (assert (typep
                          (read-from-string (substitute #\d #\e float-string))
                          'double-float))
          and do (assert (typep
                          (read-from-string (substitute #\D #\e float-string))
                          'double-float))
          and do (assert (typep
                          (read-from-string (substitute #\l #\e float-string))
                          'long-float))
          and do (assert (typep
                          (read-from-string (substitute #\L #\e float-string))
                          'long-float)))))

(let ((*read-base* *read-base*))
  (dolist (integer-string '("1." "2." "3." "4." "5." "6." "7." "8." "9." "0."))
    (loop for i from 2 to 36
          do (setq *read-base* i)
          do (assert (typep (read-from-string integer-string) 'integer)))))

(let ((*read-base* *read-base*))
  (dolist (symbol-string '("A." "a." "Z." "z."

                           "+.9eA" "+.9ea"

                           "0.A" "0.a" "0.Z" "0.z"

                           #|"9eA" "9ea"|# "9e+A" "9e+a" "9e-A" "9e-a"
                           #|"Ae9" "ae9"|# "Ae+9" "ae+9" "Ae-9" "ae-9"

                           "ee+9" "Ee+9" "eE+9" "EE+9"
                           "ee-9" "Ee-9" "eE-9" "EE-9"

                           "A.0" "A.0e10" "a.0" "a.0e10"

                           "1e1e+9"))
    (loop for i from 2 to 36
          do (setq *read-base* i)
          do (assert (typep (read-from-string symbol-string) 'symbol)))))

(let ((standard-chars " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~
")
      (standard-terminating-macro-chars "\"'(),;`")
      (standard-nonterminating-macro-chars "#"))
  (flet ((frob (char)
           (multiple-value-bind (fun non-terminating-p)
               (get-macro-character char)
             (cond
               ((find char standard-terminating-macro-chars)
                (unless (and fun (not non-terminating-p))
                  (list char)))
               ((find char standard-nonterminating-macro-chars)
                (unless (and fun non-terminating-p)
                  (list char)))
               (t (unless (and (not fun) (not non-terminating-p))
                    (list char)))))))
    (let ((*readtable* (copy-readtable nil)))
      (assert (null (loop for c across standard-chars append (frob c)))))))

(let ((standard-chars " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~
")
      (undefined-chars "!\"$%&,;>?@[]^_`~{}/dDeEfFgGhHiIjJkKlLmMnNqQtTuUvVwWyYzZ"))
  (flet ((frob (char)
           (let ((fun (get-dispatch-macro-character #\# char)))
             (cond
               ((find char undefined-chars)
                (when fun (list char)))
               ((digit-char-p char 10)
                (when fun (list char)))
               (t
                (unless fun (list char)))))))
    (let ((*readtable* (copy-readtable nil)))
      (assert (null (loop for c across standard-chars append (frob c)))))))

;;; All these must return a primary value of NIL when *read-suppress* is T
;;; Reported by Bruno Haible on cmucl-imp 2004-10-25.
(let ((*read-suppress* t))
  (assert (null (read-from-string "(1 2 3)")))
  (assert (null (with-input-from-string (s "abc xyz)")
                  (read-delimited-list #\) s))))
  (assert (null (with-input-from-string (s "(1 2 3)")
                  (read-preserving-whitespace s))))
  (assert (null (with-input-from-string (s "(1 2 3)")
                 (read s)))))

;;; EOF-ERROR-P defaults to true. Reported by Bruno Haible on
;;; cmucl-imp 2004-10-18.
(multiple-value-bind (res err) (ignore-errors (read-from-string ""))
  (assert (not res))
  (assert (typep err 'end-of-file)))

(assert (equal '((0 . "A") (1 . "B"))
               (coerce (read-from-string "#((0 . \"A\") (1 . \"B\"))")
                       'list)))

;;; parse-integer uses whitespace[1] not whitespace[2] as its
;;; definition of whitespace to skip.
(let ((*readtable* (copy-readtable)))
  (set-syntax-from-char #\7 #\Space)
  (assert (= 710 (parse-integer "710"))))

(let ((*readtable* (copy-readtable)))
  (set-syntax-from-char #\7 #\Space)
  (assert (string= (format nil "~7D" 1) "      1")))

(let ((symbol (find-symbol "DOES-NOT-EXIST" "CL-USER")))
  (assert (null symbol))
  (handler-case
      (read-from-string "CL-USER:DOES-NOT-EXIST")
    (reader-error (c)
      (princ c))))

;;; The GET-MACRO-CHARACTER in SBCL <= "1.0.34.2" bogusly computed its
;;; second return value relative to *READTABLE* rather than the passed
;;; readtable.
(let* ((*readtable* (copy-readtable nil)))
  (set-syntax-from-char #\" #\A)
  (multiple-value-bind (reader-fn non-terminating-p)
      (get-macro-character #\" (copy-readtable nil))
    (declare (ignore reader-fn))
    (assert (not non-terminating-p))))

(with-test (:name :bug-309093)
  (assert (eq :error
              (handler-case
                  (read-from-string "`#2A((,(1+ 0) 0) (0 0))")
                (reader-error ()
                  :error)))))
