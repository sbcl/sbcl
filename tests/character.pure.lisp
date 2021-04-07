;;;; various CHARACTER tests without side effects

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

;;; ANSI's specification of #'CHAR-NAME imposes these constraints.
;;;
;;; (Obviously, the numeric values in this test implicitly assume
;;; we're using an ASCII-based character set.)
(dolist (i '(("Newline" 10)
             ;; (ANSI also imposes a constraint on the "semi-standard
             ;; character" "Linefeed", but in ASCII as interpreted by
             ;; Unix it's shadowed by "Newline" and so doesn't exist
             ;; as a separate character.)
             ("Space" 32)
             ("Tab" 9)
             ("Page" 12)
             ("Rubout" 127)
             ("Return" 13)
             ("Backspace" 8)))
  (destructuring-bind (name code) i
    (let ((named-char (name-char name))
          (coded-char (code-char code)))
      (assert (eql named-char coded-char))
      (assert (characterp named-char))
      (let ((coded-char-name (char-name coded-char)))
        (assert (string= name coded-char-name))))))

;;; Trivial tests for some unicode names
#+sb-unicode
(dolist (d '(("LATIN_CAPITAL_LETTER_A" 65)
             ("LATIN_SMALL_LETTER_A" 97)
             ("LATIN_SMALL_LETTER_CLOSED_OPEN_E" 666)
             ("DIGRAM_FOR_GREATER_YIN" 9871)))
  (destructuring-bind (name code) d
    (assert (eql (code-char code) (name-char (string-downcase name))))
    (assert (equal name (char-name (code-char code))))))

;;; bug 230: CHAR= didn't check types of &REST arguments
(with-test (:name :type-errors)
 (dolist (form '((code-char char-code-limit)
                 (standard-char-p "a")
                 (graphic-char-p "a")
                 (alpha-char-p "a")
                 (upper-case-p "a")
                 (lower-case-p "a")
                 (both-case-p "a")
                 (digit-char-p "a")
                 (alphanumericp "a")
                 (char= #\a "a")
                 (char/= #\a "a")
                 (char< #\a #\b "c")
                 (char-equal #\a #\a "b")
                 (digit-char -1)
                 (digit-char 4 1)
                 (digit-char 4 37)
                 (sb-int:two-arg-char-equal 10 10)))
   (assert-error (apply (car form) (mapcar 'eval (cdr form))) type-error)))

;; All of the inequality predicates when called out-of-line
;; were lazy in their type-checking, and would allow junk
;; if short-circuit evaluation allowed early loop termination.
(with-test (:name :char-inequality-&rest-arguments)
  (dolist (f '(char= char< char<= char> char>=
               char-equal char-lessp char-not-greaterp
               char-greaterp char-not-lessp))
    ;; 1 arg
    (assert-error (funcall f 'feep) type-error)
    ;; 2 arg
    (assert-error (funcall f #\a 'feep) type-error)
    (assert-error (funcall f 'feep #\a) type-error)
    ;; 3 arg
    (assert-error (funcall f #\a #\a 'feep) type-error)
    (assert-error (funcall f #\a #\b 'feep) type-error)
    (assert-error (funcall f #\b #\a 'feep) type-error)
    ;; 4 arg
    (assert-error (funcall f #\a #\a #\a 'feep) type-error)
    (assert-error (funcall f #\a #\a #\a 'feep) type-error)))

(dotimes (i 256)
  (let* ((char (code-char i))
         (graphicp (graphic-char-p char))
         (name (char-name char)))
    (unless graphicp
      (assert name))))

(assert (null (name-char 'foo)))

;;; Between 1.0.4.53 and 1.0.4.69 character untagging was broken on
;;; x86-64 if the result of the VOP was allocated on the stack, failing
;;; an aver in the compiler.
(with-test (:name :character-untagging)
  (compile nil
           '(lambda (c0 c1 c2 c3 c4 c5 c6 c7
                     c8 c9 ca cb cc cd ce cf)
             (declare (type character c0 c1 c2 c3 c4 c5 c6 c7
                       c8 c9 ca cb cc cd ce cf))
             (char< c0 c1 c2 c3 c4 c5 c6 c7
              c8 c9 ca cb cc cd ce cf))))

;;; Characters could be coerced to subtypes of CHARACTER to which they
;;; don't belong. Also, character designators that are not characters
;;; could be coerced to proper subtypes of CHARACTER.
(with-test (:name :bug-841312)
  ;; First let's make sure that the conditions hold that make the test
  ;; valid: #\Nak is a BASE-CHAR, which at the same time ensures that
  ;; STANDARD-CHAR is a proper subtype of BASE-CHAR, and under
  ;; #+SB-UNICODE the character with code 955 exists and is not a
  ;; BASE-CHAR.
  (assert (typep #\Nak 'base-char))
  #+sb-unicode
  (assert (let ((c (code-char 955)))
            (and c (not (typep c 'base-char)))))
  ;; Test the formerly buggy coercions:
  (macrolet ((assert-coerce-type-error (object type)
               `(assert-error (coerce ,object ',type)
                              type-error)))
    (assert-coerce-type-error #\Nak standard-char)
    (assert-coerce-type-error #\a extended-char)
    #+sb-unicode
    (assert-coerce-type-error (code-char 955) base-char)
    (assert-coerce-type-error 'a standard-char)
    (assert-coerce-type-error "a" standard-char))
  ;; The following coercions still need to be possible:
  (macrolet ((assert-coercion (object type)
               `(assert (typep (coerce ,object ',type) ',type))))
    (assert-coercion #\a standard-char)
    (assert-coercion #\Nak base-char)
    #+sb-unicode
    (assert-coercion (code-char 955) character)
    (assert-coercion 'a character)
    (assert-coercion "a" character)))

(with-test (:name :bug-994487)
  (let ((f (compile nil `(lambda (char)
                           (code-char (1+ (char-code char)))))))
    (assert (equal `(function (t) (values (sb-kernel:character-set
                                           ((1 . ,(1- char-code-limit))))
                                          &optional))
                   (sb-impl::%fun-ftype f)))))

(with-test (:name (:case-insensitive-char-comparisons :eacute))
  (assert (char-equal (code-char 201) (code-char 233))))

(with-test (:name (:case-insensitive-char-comparisons :exhaustive))
  (dotimes (i char-code-limit)
    (let* ((char (code-char i))
           (down (char-downcase char))
           (up (char-upcase char)))
      (assert (char-equal char char))
      (when (char/= char down)
        (assert (char-equal char down)))
      (when (char/= char up)
        (assert (char-equal char up))))))

(macrolet ((frob (predicate yes)
             `(with-test (:name (,predicate standard-char))
                (dotimes (i 256)
                  (let ((char (code-char i)))
                    (when (typep char 'standard-char)
                      (if (find char ,yes)
                          (assert (,predicate char))
                          (assert (not (,predicate char))))))))))
  (frob lower-case-p "abcdefghijklmnopqrstuvwxyz")
  (frob upper-case-p "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  (frob digit-char-p "0123456789")
  (frob both-case-p "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
  (frob alphanumericp "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(with-test (:name :name-char-short-string)
  (name-char "")
  (name-char "A"))
