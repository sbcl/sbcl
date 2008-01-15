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

(cl:in-package :cl-user)

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
                (digit-char 4 37)))
  (assert (raises-error? (apply (car form) (mapcar 'eval (cdr form))) type-error)))

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
