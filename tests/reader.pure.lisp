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

(with-test (:name :random-describe)
  (describe sb-impl::*empty-extended-char-table* (make-broadcast-stream)))

(with-test (:name (:reader symbol :escape))
  (assert (equal (symbol-name '#:|fd\sA|) "fdsA")))

(with-test (:name (get-dispatch-macro-character :return-value))
  ;; Prior to sbcl-0.7.2.10, SBCL disobeyed the ANSI requirements on
  ;; returning NIL for unset dispatch-macro-character functions. (bug
  ;; 151, fixed by Alexey Dejenka sbcl-devel "bug 151" 2002-04-12)
  (assert (not (get-dispatch-macro-character #\# #\{)))
  (assert (not (get-dispatch-macro-character #\# #\0)))
  ;; And we might as well test that we don't have any cross-compilation
  ;; shebang residues left...
  (assert (not (get-dispatch-macro-character #\# #\!)))
  ;; Also test that all the illegal sharp macro characters are
  ;; recognized as being illegal.
  (loop for char in '(#\Backspace #\Tab #\Newline #\Linefeed
                      #\Page #\Return #\Space #\) #\<)
        do (assert (get-dispatch-macro-character #\# char)))

  (assert-error (get-dispatch-macro-character #\! #\0)))

(with-test (:name (get-macro-character :return-value))
  ;; In sbcl-0.7.3, GET-MACRO-CHARACTER and SET-MACRO-CHARACTER didn't
  ;; use NIL to represent the no-macro-attached-to-this-character case
  ;; as ANSI says they should. (This problem is parallel to the
  ;; GET-DISPATCH-MACRO misbehavior fixed in sbcl-0.7.2.10, but was
  ;; fixed a little later.)
  (dolist (customizable-char
           ;; According to ANSI "2.1.4 Character Syntax Types", these
           ;; characters are reserved for the programmer.
           '(#\? #\! #\[ #\] #\{ #\}))
    ;; So they should have no macro-characterness.
    (multiple-value-bind (macro-fun non-terminating-p)
        (get-macro-character customizable-char)
      (assert (null macro-fun))
      ;; Also, in a bit of ANSI weirdness, NON-TERMINATING-P can be
      ;; true only when MACRO-FUN is true. (When the character is not
      ;; a macro character, it can be embedded in a token, so it'd be
      ;; more logical for NON-TERMINATING-P to be T in this case; but
      ;; ANSI says it's NIL in this case.
      (assert (null non-terminating-p)))))

(with-test (:name (set-syntax-from-char :smoke))
  ;; rudimentary test of SET-SYNTAX-FROM-CHAR, just to verify that it
  ;; wasn't totally broken by the GET-MACRO-CHARACTER/SET-MACRO-CHARACTER
  ;; fixes in 0.7.3.16
  (assert (= 123579 (read-from-string "123579")))
  (let ((*readtable* (copy-readtable)))
    (set-syntax-from-char #\7 #\;)
    (assert (= 1235 (read-from-string "123579")))))

;;; PARSE-INTEGER must signal an error of type PARSE-ERROR if it is
;;; unable to parse an integer and :JUNK-ALLOWED is NIL.
(with-test (:name (parse-integer :smoke))
  (macrolet ((assert-parse-error (form)
               `(assert-error ,form parse-error)))
    (assert-parse-error (parse-integer "    "))
    (assert-parse-error (parse-integer "12 a"))
    (assert-parse-error (parse-integer "12a"))
    (assert-parse-error (parse-integer "a"))
    (assert (= (parse-integer "12") 12))
    (assert (= (parse-integer "   12   ") 12))
    (assert (= (parse-integer "   12asdb" :junk-allowed t) 12))))

;;; #A notation enforces that once one 0 dimension has been found, all
;;; subsequent ones are also 0.
(with-test (:name (:sharpsign-a-reader-macro :initial-elements))
  (assert (equal (array-dimensions (read-from-string "#3A()"))
                 '(0 0 0)))
  (assert (equal (array-dimensions (read-from-string "#3A(())"))
                 '(1 0 0)))
  (assert (equal (array-dimensions (read-from-string "#3A((() ()))"))
                 '(1 2 0))))

;;; Bug reported by Nikodemus Siivola on sbcl-devel 2003-07-21:
;;; package misconfiguration
(with-test (:name (with-input-from-string :package-misconfiguration))
  (assert-error (with-input-from-string (s "cl:") (read s)) end-of-file))

;;; Bugs found by Paul Dietz
(with-test (:name (parse-integer 1))
  (assert (equal (multiple-value-list
                  (parse-integer "   123      "))
                 '(123 12))))

(with-test (:name (parse-integer 2))
  (let* ((base "xxx 123  yyy")
         (intermediate (make-array 8 :element-type (array-element-type base)
                                     :displaced-to base
                                     :displaced-index-offset 2))
         (string (make-array 6 :element-type (array-element-type base)
                               :displaced-to intermediate
                               :displaced-index-offset 1)))
    (assert (equal (multiple-value-list
                    (parse-integer string))
                   '(123 6)))))

(with-test (:name (*read-base* 1))
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
                            'long-float))))))

(with-test (:name (*read-base* 2))
  (let ((*read-base* *read-base*))
    (dolist (integer-string '("1." "2." "3." "4." "5." "6." "7." "8." "9." "0."))
      (loop for i from 2 to 36
            do (setq *read-base* i)
            do (assert (typep (read-from-string integer-string) 'integer))))))

(with-test (:name (*read-base* 3))
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
            do (assert (typep (read-from-string symbol-string) 'symbol))))))

(with-test (:name (readtable :specified-macro-characters))
  (let ((standard-chars " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~
")
        (standard-terminating-macro-chars "\"'(),;`")
        (standard-nonterminating-macro-chars "#"))
    (flet ((frob (char)
             (multiple-value-bind (fun non-terminating-p)
                 (get-macro-character char)
               (cond ((find char standard-terminating-macro-chars)
                      (unless (and fun (not non-terminating-p))
                        (list char)))
                     ((find char standard-nonterminating-macro-chars)
                      (unless (and fun non-terminating-p)
                        (list char)))
                     (t (unless (and (not fun) (not non-terminating-p))
                          (list char)))))))
      (let ((*readtable* (copy-readtable nil)))
        (assert (null (loop for c across standard-chars append (frob c))))))))

(with-test (:name (readtable :specified-dispatch-macro-characters))
  (let ((standard-chars " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~
")
        (undefined-chars "!\"$%&,;>?@[]^_`~{}/dDeEfFgGhHiIjJkKlLmMnNqQtTuUvVwWyYzZ"))
    (flet ((frob (char)
             (let ((fun (get-dispatch-macro-character #\# char)))
               (cond ((find char undefined-chars)
                      (when fun (list char)))
                     ((digit-char-p char 10)
                      (when fun (list char)))
                     (t
                      (unless fun (list char)))))))
      (let ((*readtable* (copy-readtable nil)))
        (assert (null (loop for c across standard-chars append (frob c))))))))

(with-test (:name (copy-readtable :with-unicode-macro)
                  :skipped-on (not :sb-unicode))
  (let ((rt (copy-readtable)))
    (set-macro-character (code-char #x100fa) #'error nil rt)
    (assert (plusp (hash-table-count (sb-impl::extended-char-table rt))))
    (copy-readtable nil rt)
    (assert (null (get-macro-character #\UFC rt)))))

;;; All these must return a primary value of NIL when *read-suppress* is T
;;; Reported by Bruno Haible on cmucl-imp 2004-10-25.
(with-test (:name (*read-suppress* :char-macros))
  (let ((*read-suppress* t))
    (assert (null (read-from-string "(1 2 3)")))
    (assert (null (with-input-from-string (s "abc xyz)")
                    (read-delimited-list #\) s))))
    (assert (null (with-input-from-string (s "(1 2 3)")
                    (read-preserving-whitespace s))))
    (assert (null (with-input-from-string (s "(1 2 3)")
                    (read s))))
    ;; .. and it's better to avoid consing rather than produce an object and
    ;; throw it away, even though it's (mostly) indistinguishable to the user.
    (let ((input (make-string-input-stream "this-is-a-string! .")))
      (assert (string= (sb-impl::with-read-buffer ()
                         (sb-impl::read-string input #\!))
                       "")))))

;;; System code that asks whether %READ-PRESERVING-WHITESPACE hit EOF
;;; mistook NIL as an object returned normally for NIL the default eof mark.
(with-test (:name (read-preserving-whitespace file-position))
  (multiple-value-bind (obj pos1) (read-from-string "NIL A")
    (declare (ignore obj))
    (multiple-value-bind (obj pos2) (read-from-string "NNN A")
      (declare (ignore obj))
      (assert (= pos1 pos2 4))))
  ;; This also affected *READ-SUPPRESS*. The root cause is the same,
  ;; but the rationale for why the change is valid is slightly subtle
  ;; on account of the vague if not weird implication regarding READ
  ;; that there might actually be differences in non-preservation of
  ;; whitespace based on *READ-SUPPRESS*. CLHS entry for READ:
  ;;  "When *read-suppress* is false, read throws away the delimiting
  ;;   character required by certain printed representations if it is a
  ;;   whitespace[2] character; but read preserves the character (using
  ;;   unread-char) if it is syntactically meaningful, because it could
  ;;   be the start of the next expression."
  ;; Why would it mention *read-supress* at all, unless the expectation
  ;; is that when suppressing you might /not/ throw away a space?
  ;; But it isn't "when-and-only-when" so we're certainly legal to
  ;; make the behavior identical regardless of *read-suppress*.
  (dolist (test '("#-q 1 2" "#+sbcl 1 2")) ; Two tests from lp#327790.
    (flet ((try (*read-suppress*)
             (with-input-from-string (s test)
               (read s)
               (file-position s))))
      (assert (= (try nil) (try t)))))
  ;; Check that conversion from local eof-object to user-specified eof
  ;; object is nearly perfectly immune to false positives.
  ;; The only remaining confusion is that
  ;;   (read-from-string "#.(code-header-ref (fun-code-header #'read) 6)")
  ;; returns NIL instead of (NIL) [subject to change depending on
  ;; what 6 should be] but that is too ridiculous to worry about.
  (assert (eq (read-from-string "#.sb-impl::*eof-object*")
              sb-impl::*eof-object*)))

;;; EOF-ERROR-P defaults to true. Reported by Bruno Haible on
;;; cmucl-imp 2004-10-18.
(with-test (:name (read-from-string :eof-error))
  (assert-error (read-from-string "") end-of-file)
  (assert (equal '((0 . "A") (1 . "B"))
                 (coerce (read-from-string "#((0 . \"A\") (1 . \"B\"))")
                         'list))))

(with-test (:name (read-delimited-list :non-recursive :circularity))
  (let* ((stream (make-string-input-stream "#1=(nil) #1#)"))
         (result (read-delimited-list #\) stream)))
    (assert (eq (first result) (second result)))))

;;; parse-integer uses whitespace[1] not whitespace[2] as its
;;; definition of whitespace to skip.
(with-test (:name (parse-integer :whitespace-handling))
  (let ((*readtable* (copy-readtable)))
    (set-syntax-from-char #\7 #\Space)
    (assert (= 710 (parse-integer "710"))))

  (let ((*readtable* (copy-readtable)))
    (set-syntax-from-char #\7 #\Space)
    (assert (string= (format nil "~7D" 1) "      1"))))

(with-test (:name :report-reader-error)
  ;; Apparently this wants to test the printing of the error string
  ;; otherwise we'd just use ASSERT-SIGNAL.
  (let ((symbol (find-symbol "DOES-NOT-EXIST" "CL-USER")))
    (declare (optimize safety)) ; don't flush PRINC-TO-STRING
    (assert (null symbol))
    (handler-case (read-from-string "CL-USER:DOES-NOT-EXIST")
     (reader-error (c) (princ-to-string c)))))

;;; The GET-MACRO-CHARACTER in SBCL <= "1.0.34.2" bogusly computed its
;;; second return value relative to *READTABLE* rather than the passed
;;; readtable.
(with-test (:name (get-macro-character :argument))
  (let* ((*readtable* (copy-readtable nil)))
    (set-syntax-from-char #\" #\A)
    (multiple-value-bind (reader-fn non-terminating-p)
        (get-macro-character #\" (copy-readtable nil))
      (declare (ignore reader-fn))
      (assert (not non-terminating-p)))))

(with-test (:name :bug-309093)
  (assert (eq :error
              (handler-case
                  (read-from-string "`#2A((,(1+ 0) 0) (0 0))")
                (reader-error ()
                  :error)))))

(with-test (:name (set-syntax-from-char :dispatch-macro-char))
  (let ((rt (copy-readtable)))
    (make-dispatch-macro-character #\! nil rt)
    (set-dispatch-macro-character #\! #\! (constantly 'bang^2) rt)
    (flet ((maybe-bang ()
             (let ((*readtable* rt))
               (read-from-string "!!"))))
      (assert (eq 'bang^2 (maybe-bang)))
      (set-syntax-from-char #\! #\! rt)
      (assert (eq '!! (maybe-bang))))))

(with-test (:name :read-in-package-syntax :skipped-on :sb-devel)
  (assert (equal '(sb-c::a (sb-kernel::x sb-kernel::y) sb-c::b)
                 (read-from-string "sb-c::(a sb-kernel::(x y) b)")))
  (assert (equal '(cl-user::yes-this-is-sbcl)
                 (read-from-string "cl-user::(#+sbcl yes-this-is-sbcl)")))
  (assert (eq :violated!
              (handler-case
                  (read-from-string "cl::'foo")
                (package-lock-violation ()
                  :violated!)))))

(with-test (:name :bug-309070)
  (with-timeout 10
    (assert-error (read-from-string "10e10000000000000000000")
                  sb-kernel:reader-impossible-number-error)))

(with-test (:name :bug-1095918)
  (assert (= (length `#3(1)) 3)))

(with-test (:name :obscure-reader-package-usage)
  ;; commit 8fd604 cause a bug in reading "::(foo bar)" which tried
  ;; to treat the package-designator as a string, but in this case
  ;; it is hardcoded to *keyword-package*.
  (assert (equal (read-from-string "::(foo bar)") '(:foo :bar))))

;; I do not know the complete list of platforms for which this test
;; will not cons, but there were four different heap allocations
;; instead of using dx allocation or a recyclable resource:
;;  - most obviously, a 128-character buffer per invocation of READ
;;  - calling SUBSEQ for package names
;;  - multiple-value-call in WITH-CHAR-MACRO-RESULT
;;  - the initial cons cell in READ-LIST
(with-test (:name :read-does-not-cons-per-se :skipped-on :interpreter)
  (flet ((test-reading (string)
           (let ((s (make-string-input-stream string)))
             (read s) ; once outside the loop, to make A-SYMBOL
             (ctu:assert-no-consing
              (progn (file-position s 0)
                     (read s))
              40000))
           ;; WITH-INPUT-FROM-STRING doesn't heap-allocate a stream
           (ctu:assert-no-consing
            (with-input-from-string (s string)
              (opaque-identity s)))
           ;; READ-FROM-STRING doesn't heap-allocate a stream
           (ctu:assert-no-consing
            (read-from-string string))))
    ;; These each used to produce at least 20 MB of garbage,
    ;; a result of using 128-character (= 512 bytes for Unicode) buffers.
    ;; Now we use exactly one buffer, or maybe two for package + symbol-name.
    ;; There is no way to allow an allocation of precisely 512 bytes
    ;; without counting a whole allocation page against this test.
    ;; If you get unlucky, the tests might cons one SB-IMPL::TOKEN-BUFFER.
    ;; And if you get really unlucky, that might be the straw that breaks
    ;; the camel's back - forcing the use of a new GC page, which looks
    ;; like it consed 32768 bytes on the old page. Due to the allowable
    ;; tolerance in CHECK-CONSING, running the test more times than there
    ;; are bytes consed should pass for "no consing" because it's obviously
    ;; impossible to cons 1 byte per run.
    ;; If this still fails, it might be due to somebody changing the
    ;; backend-page-bytes to exceed 32KB. Not sure what to do about that.
    #+64-bit (test-reading "4.0s0")
    (test-reading "COMMON-LISP-USER::A-SYMBOL")
    (test-reading "()")
    (test-reading "#\\-") ; should not copy the token buffer
    ;; *READ-SUPPRESS* avoids creation of lists
    (test-reading "#-sbcl(a (b c (d (e) (f) g)) h i j . x . y baz) 5")
    ))

(with-test (:name :sharp-left-paren-empty-list)
  (assert (read-from-string "#0()")) ; edge case that works
  (assert (eq (handler-case (read-from-string "#3()")
                (sb-int:simple-reader-error () :win))
              :win)))

(with-test (:name :sharp-star-empty-multiple-escapes)
  (assert (eq (handler-case (read-from-string "#*101||1")
                (sb-int:simple-reader-error () :win))
              :win)))

(with-test (:name :sharp-star-default-fill :skipped-on :sbcl)
  ;; can't assert anything about bits beyond the supplied ones
  (let ((bv (opaque-identity #*11)))
    (assert (= (sb-kernel:%vector-raw-bits bv 0) 3))))

;;; The WITH-FAST-READ-BYTE macro accidentally left the package lock
;;; of FAST-READ-BYTE disabled during its body.
(with-test (:name :fast-read-byte-package-lock :skipped-on :sb-devel)
  (let ((fun
         ;; Suppress the compiler output to avoid noise when running the
         ;; test. (There are a warning and an error about the package
         ;; lock violation and a note about FAST-READ-BYTE being
         ;; unused.) It's easy and more precise to test for the error
         ;; that the compiled function signals when it is called.
         (let ((*error-output* (make-broadcast-stream)))
           (compile nil
                    '(lambda ()
                      (sb-int:with-fast-read-byte (t *standard-input*)
                        ;; Signal an error if the symbol is locked.
                        (declare (special sb-int:fast-read-byte))))))))
    (assert-error (funcall fun) program-error)))

(with-test (:name :sharp-plus-requires-subform)
  (assert-error (read-from-string "(let ((foo 3) #+sbcl) wat)"))
  (assert-error (read-from-string "(let ((foo 3) #-brand-x) wat)")))

;; Another test asserting that a signaled condition is printable
(with-test (:name :impossible-number-error)
  (locally
   (declare (optimize safety)) ; don't flush PRINC-TO-STRING
   (princ-to-string (nth-value 1 (ignore-errors (READ-FROM-STRING "1/0"))))))

(with-test (:name :read-from-string-compiler-macro)
  ;; evaluation order should be the customary one. In particular,
  ;; do not assume that EOF-ERROR-P and EOF-VALUE are constants.
  (sb-int:collect ((l))
    (read-from-string "a" (l 'first) (l 'second) :start (progn (l 'third) 0))
    (assert (equal (l) '(first second third)))))

(with-test (:name :sharp-star-reader-error)
  (assert-error (read-from-string (format nil "#~D*" (1+ most-positive-fixnum))) reader-error))

(defun test1 (print &optional expect)
  (let ((*readtable* (copy-readtable nil)))
    (when print
      (format t "READTABLE-CASE  Input   Symbol-name~@
                 ----------------------------------~%"))
    (dolist (readtable-case '(:upcase :downcase :preserve :invert))
      (setf (readtable-case *readtable*) readtable-case)
      (dolist (input '("ZEBRA" "Zebra" "zebra"))
        (if print
            (format t "~&:~A~16T~A~24T~A"
                    (string-upcase readtable-case)
                    input
                    (symbol-name (read-from-string input)))
            (assert (string= (symbol-name (read-from-string input))
                             (pop expect))))))))

(defun test2 (print &optional expect)
  (let ((*readtable* (copy-readtable nil)))
    (when print
      (format t "READTABLE-CASE *PRINT-CASE*  Symbol-name  Output  Princ~@
                 --------------------------------------------------------~%"))
    (dolist (readtable-case '(:upcase :downcase :preserve :invert))
      (setf (readtable-case *readtable*) readtable-case)
      (dolist (*print-case* '(:upcase :downcase :capitalize))
        (dolist (symbol '(|ZEBRA| |Zebra| |zebra|))
          (if print
              (format t "~&:~A~15T:~A~29T~A~42T~A~50T~A"
                      (string-upcase readtable-case)
                      (string-upcase *print-case*)
                      (symbol-name symbol)
                      (prin1-to-string symbol)
                      (princ-to-string symbol))
              (progn
                (assert (string= (prin1-to-string symbol) (pop expect)))
                (assert (string= (princ-to-string symbol) (pop expect))))))))))

(with-test (:name :readtable-cases)
  (test1 nil '("ZEBRA" "ZEBRA" "ZEBRA"
               "zebra" "zebra" "zebra"
               "ZEBRA" "Zebra" "zebra"
               "zebra" "Zebra" "ZEBRA"))
  (test2 nil '("ZEBRA"   "ZEBRA"
               "|Zebra|" "Zebra"
               "|zebra|" "zebra"
               "zebra"   "zebra"
               "|Zebra|" "zebra"
               "|zebra|" "zebra"
               "Zebra"   "Zebra"
               "|Zebra|" "Zebra"
               "|zebra|" "zebra"
               "|ZEBRA|" "ZEBRA"
               "|Zebra|" "ZEBRA"
               "ZEBRA"   "ZEBRA"
               "|ZEBRA|" "ZEBRA"
               "|Zebra|" "Zebra"
               "zebra"   "zebra"
               "|ZEBRA|" "ZEBRA"
               "|Zebra|" "Zebra"
               "Zebra"   "Zebra"
               "ZEBRA"   "ZEBRA"
               "Zebra"   "Zebra"
               "zebra"   "zebra"
               "ZEBRA"   "ZEBRA"
               "Zebra"   "Zebra"
               "zebra"   "zebra"
               "ZEBRA"   "ZEBRA"
               "Zebra"   "Zebra"
               "zebra"   "zebra"
               "zebra"   "zebra"
               "Zebra"   "Zebra"
               "ZEBRA"   "ZEBRA"
               "zebra"   "zebra"
               "Zebra"   "Zebra"
               "ZEBRA"   "ZEBRA"
               "zebra"   "zebra"
               "Zebra"   "Zebra"
               "ZEBRA"   "ZEBRA")))

#+sb-unicode
(with-test (:name :base-char-preference)
  (let* ((rt (copy-readtable))
         (*readtable* rt)
         (callcount 0))
    (flet ((expect (setting symbol-name-type string-type)
             (unless (eq setting :default)
               (setf (readtable-base-char-preference rt) setting))
             ;; Each test has to intern a new symbol of course.
             (let ((input (format nil "MAMALOOK~D" (incf callcount))))
               (assert (equal (type-of (symbol-name (read-from-string input)))
                              symbol-name-type))
               (assert (equal (type-of (read-from-string "\"Foobarbaz\""))
                              string-type)))
             ;; Also verify that COPY-READTABLE works
             (assert (eq (readtable-base-char-preference (copy-readtable rt))
                         (readtable-base-char-preference rt)))))
      ;; Verify correctness of the stated default as per the docstring
      (assert (eq (readtable-base-char-preference rt) :symbols))
      ;; Default: prefer base symbols, but CHARACTER strings.
      (expect :default '(simple-base-string 9) '(simple-array character (9)))
      ;; Prefer base strings, but CHARACTER strings for symbol names
      (expect :strings
              '(simple-array character (9))
              '(simple-base-string 9))
      ;; Prefer base-string for everything
      (expect :both '(simple-base-string 9) '(simple-base-string 9))
      ;; Prefer base-string for neither
      (expect nil
              '(simple-array character (9))
              '(simple-array character (9))))))

(with-test (:name :rational-rdff)
  (with-standard-io-syntax
    (let ((*read-default-float-format* 'rational))
      (macrolet ((assert-read-eqlity (x y)
                   `(assert (eql (read-from-string ,x) ,y)))
                 (assert-print-string= (x y)
                   `(assert (string= (prin1-to-string ,x) ,y))))
        (assert-read-eqlity "1.0" 1)
        (assert-read-eqlity "-1.0e0" -1)
        (assert-read-eqlity "0.1" 1/10)
        (assert-read-eqlity "-0.1e0" -1/10)
        (assert-read-eqlity "0.1d0" 0.1d0)
        (assert-read-eqlity "-0.1f0" -0.1f0)

        (assert-print-string= 1 "1")
        (assert-print-string= 1/10 "1/10")
        (assert-print-string= 0.1f0 "0.1f0")
        (assert-print-string= 0.1d0 "0.1d0")))))

(with-test (:name :rational-exponent)
  (with-standard-io-syntax
    (macrolet ((assert-read-eqlity (x y)
                 `(assert (eql (read-from-string ,x) ,y))))
      (assert-read-eqlity "1.0r0" 1)
      (assert-read-eqlity "1.0r5" 100000)
      (assert-read-eqlity "0.1r0" 1/10)
      (assert-read-eqlity "0.333R0" 333/1000)
      (assert-read-eqlity "1R-3" 1/1000)
      (assert-read-eqlity ".1R2" 10))))
