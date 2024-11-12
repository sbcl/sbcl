;;;; tests for the code walker

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is derived from software originally released by Xerox
;;;; Corporation. Copyright and release statements follow. Later modifications
;;;; to the software are in the public domain and are provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for more
;;;; information.

;;;; copyright information from original PCL sources:
;;;;
;;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;;; All rights reserved.
;;;;
;;;; Use and copying of this software and preparation of derivative works based
;;;; upon this software are permitted. Any distribution of this software or
;;;; derivative works must comply with all applicable United States export
;;;; control laws.
;;;;
;;;; This software is made available AS IS, and Xerox Corporation makes no
;;;; warranty about the software, its performance or its conformity to any
;;;; specification.

(in-package :sb-walker)

;;;; utilities to support tests

;;; string equality modulo deletion of consecutive whitespace (as a crude way
;;; of washing away irrelevant differences in indentation)
(defun string-modulo-tabspace (s)
  (let ((s (string-trim '(#\Space) (substitute #\Space #\Newline
                                               (substitute #\Space #\Tab s)))))
    (loop (let ((p (search "  " s)))
            (if (not p) (return s))
            ;; Extremely inefficient but simple algorithm.
            (setq s (concatenate 'string (subseq s 0 p) (subseq s (1+ p))))))))

(defun string=-modulo-tabspace (x y)
  (string= (string-modulo-tabspace x)
           (string-modulo-tabspace y)))

;;;; tests based on stuff at the end of the original CMU CL
;;;; pcl/walk.lisp file

(defmacro take-it-out-for-a-test-walk (form)
  `(take-it-out-for-a-test-walk-1 ',form))

(defun take-it-out-for-a-test-walk-1 (form)
  (let ((copy-of-form (copy-tree form))
        (result (walk-form form nil
                  (lambda (x y env)
                    (format t "~&Form: ~S ~3T Context: ~A" x y)
                    (when (symbolp x)
                      (let ((lexical (var-lexical-p x env))
                            (special (var-special-p x env)))
                        (when lexical
                          (format t ";~3T")
                          (format t "lexically bound"))
                        (when special
                          (format t ";~3T")
                          (format t "declared special"))
                        (when (boundp x)
                          (format t ";~3T")
                          (format t "bound: ~S " (eval x)))))
                    x))))
    (cond ((not (equal result copy-of-form))
           (format t "~%Warning: Result not EQUAL to copy of start."))
          ((not (eq result form))
           (format t "~%Warning: Result not EQ to copy of start.")))
    (pprint result)
    nil))

(defmacro foo (&rest ignore)
  (declare (ignore ignore))
  ''global-foo)

(defmacro bar (&rest ignore)
  (declare (ignore ignore))
  ''global-bar)

(test-util:with-test (:name (:walk list))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (list arg1 arg2 arg3)))
           "Form: (LIST ARG1 ARG2 ARG3)   Context: EVAL
Form: ARG1   Context: EVAL
Form: ARG2   Context: EVAL
Form: ARG3   Context: EVAL
\(LIST ARG1 ARG2 ARG3)")))

(test-util:with-test (:name (:walk list cons))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (list (cons 1 2) (list 3 4 5))))
           "Form: (LIST (CONS 1 2) (LIST 3 4 5))   Context: EVAL
Form: (CONS 1 2)   Context: EVAL
Form: 1   Context: EVAL
Form: 2   Context: EVAL
Form: (LIST 3 4 5)   Context: EVAL
Form: 3   Context: EVAL
Form: 4   Context: EVAL
Form: 5   Context: EVAL
\(LIST (CONS 1 2) (LIST 3 4 5))")))

(test-util:with-test (:name (:walk progn 1))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (progn (foo) (bar 1))))
           "Form: (PROGN (FOO) (BAR 1))   Context: EVAL
Form: (FOO)   Context: EVAL
Form: 'GLOBAL-FOO   Context: EVAL
Form: (BAR 1)   Context: EVAL
Form: 'GLOBAL-BAR   Context: EVAL
\(PROGN (FOO) (BAR 1))")))

(test-util:with-test (:name (:walk block))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (block block-name a b c)))
           "Form: (BLOCK BLOCK-NAME A B C)   Context: EVAL
Form: A   Context: EVAL
Form: B   Context: EVAL
Form: C   Context: EVAL
\(BLOCK BLOCK-NAME A B C)")))

(test-util:with-test (:name (:walk block list))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (block block-name (list a) b c)))
           "Form: (BLOCK BLOCK-NAME (LIST A) B C)   Context: EVAL
Form: (LIST A)   Context: EVAL
Form: A   Context: EVAL
Form: B   Context: EVAL
Form: C   Context: EVAL
\(BLOCK BLOCK-NAME (LIST A) B C)")))

(test-util:with-test (:name (:walk catch list))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (catch catch-tag (list a) b c)))
           "Form: (CATCH CATCH-TAG (LIST A) B C)   Context: EVAL
Form: CATCH-TAG   Context: EVAL
Form: (LIST A)   Context: EVAL
Form: A   Context: EVAL
Form: B   Context: EVAL
Form: C   Context: EVAL
\(CATCH CATCH-TAG (LIST A) B C)")))

;;; This is a fairly simple MACROLET case. While walking the body of the
;;; macro, X should be lexically bound. In the body of the MACROLET form
;;; itself, X should not be bound.
(test-util:with-test (:name (:walk macrolet))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk
              (macrolet ((foo (x) (list x) ''inner))
                x
                (foo 1))))
           "Form: (MACROLET ((FOO (X)
                   (LIST X)
                   ''INNER))
        X
        (FOO 1))   Context: EVAL
Form: (LIST X)   Context: EVAL
Form: X   Context: EVAL; lexically bound
Form: ''INNER   Context: EVAL
Form: X   Context: EVAL
Form: (FOO 1)   Context: EVAL
Form: 'INNER   Context: EVAL
\(MACROLET ((FOO (X)
             (LIST X)
             ''INNER))
  X
  (FOO 1))")))

;;; The original PCL documentation for this test said
;;;   A slightly more complex MACROLET case. In the body of the macro
;;;   X should not be lexically bound. In the body of the macrolet
;;;   form itself X should be bound. Note that THIS CASE WILL CAUSE AN
;;;   ERROR when it tries to macroexpand the call to FOO.
;;;
;;; This test is commented out in SBCL because ANSI says, in the
;;; definition of the special operator MACROLET,
;;;    The macro-expansion functions defined by MACROLET are defined
;;;    in the lexical environment in which the MACROLET form appears.
;;;    Declarations and MACROLET and SYMBOL-MACROLET definitions affect
;;;    the local macro definitions in a MACROLET, but the consequences
;;;    are undefined if the local macro definitions reference any
;;;    local variable or function bindings that are visible in that
;;;    lexical environment.
;;; Since the behavior is undefined, anything we do conforms.:-|
;;; This is of course less than ideal; see bug 124.
#+nil
(multiple-value-bind (res cond)
    (ignore-errors
      (take-it-out-for-a-test-walk
       (let ((x 1))
         (macrolet ((foo () (list x) ''inner))
           x
           (foo)))))
  (assert (and (null res) cond)))

(test-util:with-test (:name (:walk flet 1))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk
              (flet ((foo (x) (list x y))
                     (bar (x) (list x y)))
                (foo 1))))
           "Form: (FLET ((FOO (X)
               (LIST X Y))
             (BAR (X)
               (LIST X Y)))
        (FOO 1))   Context: EVAL
Form: (LIST X Y)   Context: EVAL
Form: X   Context: EVAL; lexically bound
Form: Y   Context: EVAL
Form: (LIST X Y)   Context: EVAL
Form: X   Context: EVAL; lexically bound
Form: Y   Context: EVAL
Form: (FOO 1)   Context: EVAL
Form: 1   Context: EVAL
\(FLET ((FOO (X)
         (LIST X Y))
       (BAR (X)
         (LIST X Y)))
  (FOO 1))")))

(test-util:with-test (:name (:walk let flet))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk
              (let ((y 2))
                (flet ((foo (x) (list x y))
                       (bar (x) (list x y)))
                  (foo 1)))))
           "Form: (LET ((Y 2))
        (FLET ((FOO (X)
                 (LIST X Y))
               (BAR (X)
                 (LIST X Y)))
          (FOO 1)))   Context: EVAL
Form: 2   Context: EVAL
Form: (FLET ((FOO (X)
               (LIST X Y))
             (BAR (X)
               (LIST X Y)))
        (FOO 1))   Context: EVAL
Form: (LIST X Y)   Context: EVAL
Form: X   Context: EVAL; lexically bound
Form: Y   Context: EVAL; lexically bound
Form: (LIST X Y)   Context: EVAL
Form: X   Context: EVAL; lexically bound
Form: Y   Context: EVAL; lexically bound
Form: (FOO 1)   Context: EVAL
Form: 1   Context: EVAL
\(LET ((Y 2))
  (FLET ((FOO (X)
           (LIST X Y))
         (BAR (X)
           (LIST X Y)))
    (FOO 1)))")))

(test-util:with-test (:name (:walk labels))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk
              (labels ((foo (x) (bar x))
                       (bar (x) (foo x)))
                (foo 1))))
           "Form: (LABELS ((FOO (X)
                 (BAR X))
               (BAR (X)
                 (FOO X)))
        (FOO 1))   Context: EVAL
Form: (BAR X)   Context: EVAL
Form: X   Context: EVAL; lexically bound
Form: (FOO X)   Context: EVAL
Form: X   Context: EVAL; lexically bound
Form: (FOO 1)   Context: EVAL
Form: 1   Context: EVAL
\(LABELS ((FOO (X)
           (BAR X))
         (BAR (X)
           (FOO X)))
  (FOO 1))")))

(test-util:with-test (:name (:walk flet 2))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk
              (flet ((foo (x) (foo x)))
                (foo 1))))
           "Form: (FLET ((FOO (X)
               (FOO X)))
        (FOO 1))   Context: EVAL
Form: (FOO X)   Context: EVAL
Form: 'GLOBAL-FOO   Context: EVAL
Form: (FOO 1)   Context: EVAL
Form: 1   Context: EVAL
\(FLET ((FOO (X)
         (FOO X)))
  (FOO 1))")))

(test-util:with-test (:name (:walk flet 3))
  (assert (string=-modulo-tabspace
         (with-output-to-string (*standard-output*)
           (take-it-out-for-a-test-walk
            (flet ((foo (x) (foo x)))
              (flet ((bar (x) (foo x)))
                (bar 1)))))
         "Form: (FLET ((FOO (X)
               (FOO X)))
        (FLET ((BAR (X)
                 (FOO X)))
          (BAR 1)))   Context: EVAL
Form: (FOO X)   Context: EVAL
Form: 'GLOBAL-FOO   Context: EVAL
Form: (FLET ((BAR (X)
               (FOO X)))
        (BAR 1))   Context: EVAL
Form: (FOO X)   Context: EVAL
Form: X   Context: EVAL; lexically bound
Form: (BAR 1)   Context: EVAL
Form: 1   Context: EVAL
\(FLET ((FOO (X)
         (FOO X)))
  (FLET ((BAR (X)
           (FOO X)))
    (BAR 1)))")))

(test-util:with-test (:name (:walk progn special))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (prog () (declare (special a b)))))
           "Form: (PROG () (DECLARE (SPECIAL A B)))   Context: EVAL
Form: (BLOCK NIL
        (LET ()
          (DECLARE (SPECIAL A B))
          (TAGBODY)))   Context: EVAL
Form: (LET ()
        (DECLARE (SPECIAL A B))
        (TAGBODY))   Context: EVAL
Form: (TAGBODY)   Context: EVAL
\(PROG () (DECLARE (SPECIAL A B)))")))

(test-util:with-test (:name (:walk let special 1))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (let (a b c)
                                            (declare (special a b))
                                            (foo a) b c)))
           "Form: (LET (A B C)
        (DECLARE (SPECIAL A B))
        (FOO A)
        B
        C)   Context: EVAL
Form: (FOO A)   Context: EVAL
Form: 'GLOBAL-FOO   Context: EVAL
Form: B   Context: EVAL; lexically bound; declared special
Form: C   Context: EVAL; lexically bound
\(LET (A B C)
  (DECLARE (SPECIAL A B))
  (FOO A)
  B
  C)")))

(test-util:with-test (:name (:walk let special 2))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (let (a b c)
                                            (declare (special a) (special b))
                                            (foo a) b c)))
           "Form: (LET (A B C)
        (DECLARE (SPECIAL A) (SPECIAL B))
        (FOO A)
        B
        C)   Context: EVAL
Form: (FOO A)   Context: EVAL
Form: 'GLOBAL-FOO   Context: EVAL
Form: B   Context: EVAL; lexically bound; declared special
Form: C   Context: EVAL; lexically bound
\(LET (A B C)
  (DECLARE (SPECIAL A) (SPECIAL B))
  (FOO A)
  B
  C)")))

(test-util:with-test (:name (:walk let special 3))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (let (a b c)
                                            (declare (special a))
                                            (declare (special b))
                                            (foo a) b c)))
           "Form: (LET (A B C)
        (DECLARE (SPECIAL A))
        (DECLARE (SPECIAL B))
        (FOO A)
        B
        C)   Context: EVAL
Form: (FOO A)   Context: EVAL
Form: 'GLOBAL-FOO   Context: EVAL
Form: B   Context: EVAL; lexically bound; declared special
Form: C   Context: EVAL; lexically bound
\(LET (A B C)
  (DECLARE (SPECIAL A))
  (DECLARE (SPECIAL B))
  (FOO A)
  B
  C)")))

(test-util:with-test (:name (:walk let special 4))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (let (a b c)
                                            (declare (special a))
                                            (declare (special b))
                                            (let ((a 1))
                                              (foo a) b c))))
           "Form: (LET (A B C)
        (DECLARE (SPECIAL A))
        (DECLARE (SPECIAL B))
        (LET ((A 1))
          (FOO A)
          B
          C))   Context: EVAL
Form: (LET ((A 1))
        (FOO A)
        B
        C)   Context: EVAL
Form: 1   Context: EVAL
Form: (FOO A)   Context: EVAL
Form: 'GLOBAL-FOO   Context: EVAL
Form: B   Context: EVAL; lexically bound; declared special
Form: C   Context: EVAL; lexically bound
\(LET (A B C)
  (DECLARE (SPECIAL A))
  (DECLARE (SPECIAL B))
  (LET ((A 1))
    (FOO A)
    B
    C))")))

(test-util:with-test (:name (:walk eval-when 1))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (eval-when ()
                                            a
                                            (foo a))))
           "Form: (EVAL-WHEN NIL A (FOO A))   Context: EVAL
Form: A   Context: EVAL
Form: (FOO A)   Context: EVAL
Form: 'GLOBAL-FOO   Context: EVAL
\(EVAL-WHEN NIL A (FOO A))")))

(test-util:with-test (:name (:walk eval-when 2))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk
              (eval-when (:execute :compile-toplevel :load-toplevel)
                a
                (foo a))))
         "Form: (EVAL-WHEN (:EXECUTE :COMPILE-TOPLEVEL :LOAD-TOPLEVEL) A (FOO A))   Context: EVAL
Form: A   Context: EVAL
Form: (FOO A)   Context: EVAL
Form: 'GLOBAL-FOO   Context: EVAL
\(EVAL-WHEN (:EXECUTE :COMPILE-TOPLEVEL :LOAD-TOPLEVEL) A (FOO A))")))

(test-util:with-test (:name (:walk progn function))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (progn (function foo))))
           "Form: (PROGN #'FOO)   Context: EVAL
Form: #'FOO   Context: EVAL
\(PROGN #'FOO)")))

(test-util:with-test (:name (:walk progn go))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (progn a b (go a))))
         "Form: (PROGN A B (GO A))   Context: EVAL
Form: A   Context: EVAL
Form: B   Context: EVAL
Form: (GO A)   Context: EVAL
\(PROGN A B (GO A))")))

(test-util:with-test (:name (:walk if 1))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (if a b c)))
           "Form: (IF A B C)   Context: EVAL
Form: A   Context: EVAL
Form: B   Context: EVAL
Form: C   Context: EVAL
\(IF A B C)")))

(test-util:with-test (:name (:walk if 2))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (if a b)))
           "Form: (IF A B)   Context: EVAL
Form: A   Context: EVAL
Form: B   Context: EVAL
Form: NIL   Context: EVAL; bound: NIL
\(IF A B)")))

(test-util:with-test (:name (:walk lambda))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk ((lambda (a b) (list a b)) 1 2)))
           "Form: ((LAMBDA (A B) (LIST A B)) 1 2)   Context: EVAL
Form: (LAMBDA (A B) (LIST A B))   Context: EVAL
Form: (LIST A B)   Context: EVAL
Form: A   Context: EVAL; lexically bound
Form: B   Context: EVAL; lexically bound
Form: 1   Context: EVAL
Form: 2   Context: EVAL
\((LAMBDA (A B) (LIST A B)) 1 2)")))

(test-util:with-test (:name (:walk lambda special))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk ((lambda (a b)
                                             (declare (special a))
                                             (list a b))
                                           1 2)))
           "Form: ((LAMBDA (A B) (DECLARE (SPECIAL A)) (LIST A B)) 1 2)   Context: EVAL
Form: (LAMBDA (A B) (DECLARE (SPECIAL A)) (LIST A B))   Context: EVAL
Form: (LIST A B)   Context: EVAL
Form: A   Context: EVAL; lexically bound; declared special
Form: B   Context: EVAL; lexically bound
Form: 1   Context: EVAL
Form: 2   Context: EVAL
\((LAMBDA (A B) (DECLARE (SPECIAL A)) (LIST A B)) 1 2)")))

(test-util:with-test (:name (:walk let list))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (let ((a a) (b a) (c b))
                                            (list a b c))))
           "Form: (LET ((A A) (B A) (C B))
        (LIST A B C))   Context: EVAL
Form: A   Context: EVAL
Form: A   Context: EVAL
Form: B   Context: EVAL
Form: (LIST A B C)   Context: EVAL
Form: A   Context: EVAL; lexically bound
Form: B   Context: EVAL; lexically bound
Form: C   Context: EVAL; lexically bound
\(LET ((A A) (B A) (C B))
  (LIST A B C))")))

(test-util:with-test (:name (:walk let* list))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (let* ((a a) (b a) (c b)) (list a b c))))
           "Form: (LET* ((A A) (B A) (C B))
        (LIST A B C))   Context: EVAL
Form: A   Context: EVAL
Form: A   Context: EVAL; lexically bound
Form: B   Context: EVAL; lexically bound
Form: (LIST A B C)   Context: EVAL
Form: A   Context: EVAL; lexically bound
Form: B   Context: EVAL; lexically bound
Form: C   Context: EVAL; lexically bound
\(LET* ((A A) (B A) (C B))
  (LIST A B C))")))

(test-util:with-test (:name (:walk let special list))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (let ((a a) (b a) (c b))
                                            (declare (special a b))
                                            (list a b c))))
           "Form: (LET ((A A) (B A) (C B))
        (DECLARE (SPECIAL A B))
        (LIST A B C))   Context: EVAL
Form: A   Context: EVAL
Form: A   Context: EVAL
Form: B   Context: EVAL
Form: (LIST A B C)   Context: EVAL
Form: A   Context: EVAL; lexically bound; declared special
Form: B   Context: EVAL; lexically bound; declared special
Form: C   Context: EVAL; lexically bound
\(LET ((A A) (B A) (C B))
  (DECLARE (SPECIAL A B))
  (LIST A B C))")))

;;;; Bug in LET* walking!
(test-util:with-test (:name (:walk let* special list :hairy-specials))
  (assert
   (string=-modulo-tabspace
    (with-output-to-string (*standard-output*)
      (take-it-out-for-a-test-walk (let* ((a a) (b a) (c b))
                                     (declare (special a b))
                                     (list a b c))))
    "Form: (LET* ((A A) (B A) (C B))
             (DECLARE (SPECIAL A B))
             (LIST A B C))   Context: EVAL
     Form: A   Context: EVAL
     Form: A   Context: EVAL; lexically bound; declared special
     Form: B   Context: EVAL; lexically bound; declared special
     Form: (LIST A B C)   Context: EVAL
     Form: A   Context: EVAL; lexically bound; declared special
     Form: B   Context: EVAL; lexically bound; declared special
     Form: C   Context: EVAL; lexically bound
     (LET* ((A A) (B A) (C B))
       (DECLARE (SPECIAL A B))
       (LIST A B C))")))

(test-util:with-test (:name (:walk let special 5))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (let ((a 1) (b 2))
                                            (foo bar)
                                            (let ()
                                              (declare (special a))
                                              (foo a b)))))
           "Form: (LET ((A 1) (B 2))
        (FOO BAR)
        (LET ()
          (DECLARE (SPECIAL A))
          (FOO A B)))   Context: EVAL
Form: 1   Context: EVAL
Form: 2   Context: EVAL
Form: (FOO BAR)   Context: EVAL
Form: 'GLOBAL-FOO   Context: EVAL
Form: (LET ()
        (DECLARE (SPECIAL A))
        (FOO A B))   Context: EVAL
Form: (FOO A B)   Context: EVAL
Form: 'GLOBAL-FOO   Context: EVAL
\(LET ((A 1) (B 2))
  (FOO BAR)
  (LET ()
    (DECLARE (SPECIAL A))
    (FOO A B)))")))

(test-util:with-test (:name (:walk multiple-value-call))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (multiple-value-call #'foo a b c)))
           "Form: (MULTIPLE-VALUE-CALL #'FOO A B C)   Context: EVAL
Form: #'FOO   Context: EVAL
Form: A   Context: EVAL
Form: B   Context: EVAL
Form: C   Context: EVAL
\(MULTIPLE-VALUE-CALL #'FOO A B C)")))

(test-util:with-test (:name (:walk multiple-value-prog1))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (multiple-value-prog1 a b c)))
         "Form: (MULTIPLE-VALUE-PROG1 A B C)   Context: EVAL
Form: A   Context: EVAL
Form: B   Context: EVAL
Form: C   Context: EVAL
\(MULTIPLE-VALUE-PROG1 A B C)")))

(test-util:with-test (:name (:walk progn 2))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (progn a b c)))
         "Form: (PROGN A B C)   Context: EVAL
Form: A   Context: EVAL
Form: B   Context: EVAL
Form: C   Context: EVAL
\(PROGN A B C)")))

(test-util:with-test (:name (:walk progv))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (progv vars vals a b c)))
         "Form: (PROGV VARS VALS A B C)   Context: EVAL
Form: VARS   Context: EVAL
Form: VALS   Context: EVAL
Form: A   Context: EVAL
Form: B   Context: EVAL
Form: C   Context: EVAL
\(PROGV VARS VALS A B C)")))

(test-util:with-test (:name (:walk quote))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (quote a)))
         "Form: 'A   Context: EVAL
'A")))

(test-util:with-test (:name (:walk return-from))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (return-from block-name a b c)))
           "Form: (RETURN-FROM BLOCK-NAME A B C)   Context: EVAL
Form: A   Context: EVAL
Form: B   Context: EVAL
Form: C   Context: EVAL
\(RETURN-FROM BLOCK-NAME A B C)")))


(test-util:with-test (:name (:walk setq 1))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (setq a 1)))
           "Form: (SETQ A 1)   Context: EVAL
Form: A   Context: SET
Form: 1   Context: EVAL
\(SETQ A 1)")))
(makunbound 'a)

(test-util:with-test (:name (:walk setq 2))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (setq a (foo 1) b (bar 2) c 3)))
           "Form: (SETQ A (FOO 1) B (BAR 2) C 3)   Context: EVAL
Form: (SETQ A (FOO 1))   Context: EVAL
Form: A   Context: SET
Form: (FOO 1)   Context: EVAL
Form: 'GLOBAL-FOO   Context: EVAL
Form: (SETQ B (BAR 2))   Context: EVAL
Form: B   Context: SET
Form: (BAR 2)   Context: EVAL
Form: 'GLOBAL-BAR   Context: EVAL
Form: (SETQ C 3)   Context: EVAL
Form: C   Context: SET
Form: 3   Context: EVAL
\(SETQ A (FOO 1) B (BAR 2) C 3)")))
(makunbound 'a)
(makunbound 'b)
(makunbound 'c)

(test-util:with-test (:name (:walk tagbody))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (tagbody a b c (go a))))
           "Form: (TAGBODY A B C (GO A))   Context: EVAL
Form: (GO A)   Context: EVAL
\(TAGBODY A B C (GO A))")))

(test-util:with-test (:name (:walk the))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (the foo (foo-form a b c))))
           "Form: (THE FOO (FOO-FORM A B C))   Context: EVAL
Form: (FOO-FORM A B C)   Context: EVAL
Form: A   Context: EVAL
Form: B   Context: EVAL
Form: C   Context: EVAL
\(THE FOO (FOO-FORM A B C))")))

(test-util:with-test (:name (:walk throw))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (throw tag-form a)))
           "Form: (THROW TAG-FORM A)   Context: EVAL
Form: TAG-FORM   Context: EVAL
Form: A   Context: EVAL
\(THROW TAG-FORM A)")))

(test-util:with-test (:name (:walk unwind-protect))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (unwind-protect (foo a b) d e f)))
           "Form: (UNWIND-PROTECT (FOO A B) D E F)   Context: EVAL
Form: (FOO A B)   Context: EVAL
Form: 'GLOBAL-FOO   Context: EVAL
Form: D   Context: EVAL
Form: E   Context: EVAL
Form: F   Context: EVAL
\(UNWIND-PROTECT (FOO A B) D E F)")))

(defmacro flet-1 (a b)
  (declare (ignore a b))
  ''outer)

(defmacro labels-1 (a b)
  (declare (ignore a b))
  ''outer)

(test-util:with-test (:name (:walk flet defmacro))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk
              (flet ((flet-1 (a b) () (flet-1 a b) (list a b)))
                (flet-1 1 2)
                (foo 1 2))))
           "Form: (FLET ((FLET-1 (A B)
               NIL
               (FLET-1 A B)
               (LIST A B)))
        (FLET-1 1 2)
        (FOO 1 2))   Context: EVAL
Form: NIL   Context: EVAL; bound: NIL
Form: (FLET-1 A B)   Context: EVAL
Form: 'OUTER   Context: EVAL
Form: (LIST A B)   Context: EVAL
Form: A   Context: EVAL; lexically bound
Form: B   Context: EVAL; lexically bound
Form: (FLET-1 1 2)   Context: EVAL
Form: 1   Context: EVAL
Form: 2   Context: EVAL
Form: (FOO 1 2)   Context: EVAL
Form: 'GLOBAL-FOO   Context: EVAL
\(FLET ((FLET-1 (A B)
         NIL
         (FLET-1 A B)
         (LIST A B)))
  (FLET-1 1 2)
  (FOO 1 2))")))

(test-util:with-test (:name (:walk labels defmacro))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk
              (labels ((label-1 (a b) () (label-1 a b)(list a b)))
                (label-1 1 2)
                (foo 1 2))))
           "Form: (LABELS ((LABEL-1 (A B)
                 NIL
                 (LABEL-1 A B)
                 (LIST A B)))
        (LABEL-1 1 2)
        (FOO 1 2))   Context: EVAL
Form: NIL   Context: EVAL; bound: NIL
Form: (LABEL-1 A B)   Context: EVAL
Form: A   Context: EVAL; lexically bound
Form: B   Context: EVAL; lexically bound
Form: (LIST A B)   Context: EVAL
Form: A   Context: EVAL; lexically bound
Form: B   Context: EVAL; lexically bound
Form: (LABEL-1 1 2)   Context: EVAL
Form: 1   Context: EVAL
Form: 2   Context: EVAL
Form: (FOO 1 2)   Context: EVAL
Form: 'GLOBAL-FOO   Context: EVAL
\(LABELS ((LABEL-1 (A B)
           NIL
           (LABEL-1 A B)
           (LIST A B)))
  (LABEL-1 1 2)
  (FOO 1 2))")))

(test-util:with-test (:name (:walk macrolet 1))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (macrolet ((macrolet-1 (a b) (list a b)))
                                            (macrolet-1 a b)
                                            (foo 1 2))))
           "Form: (MACROLET ((MACROLET-1 (A B)
                   (LIST A B)))
        (MACROLET-1 A B)
        (FOO 1 2))   Context: EVAL
Form: (LIST A B)   Context: EVAL
Form: A   Context: EVAL; lexically bound
Form: B   Context: EVAL; lexically bound
Form: (MACROLET-1 A B)   Context: EVAL
Form: (A B)   Context: EVAL
Form: B   Context: EVAL
Form: (FOO 1 2)   Context: EVAL
Form: 'GLOBAL-FOO   Context: EVAL
\(MACROLET ((MACROLET-1 (A B)
             (LIST A B)))
  (MACROLET-1 A B)
  (FOO 1 2))")))

(test-util:with-test (:name (:walk macrolet 2))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (macrolet ((foo (a) `(inner-foo-expanded ,a)))
                                            (foo 1))))
           "Form: (MACROLET ((FOO (A)
                   `(INNER-FOO-EXPANDED ,A)))
        (FOO 1))   Context: EVAL
Form: `(INNER-FOO-EXPANDED ,A)   Context: EVAL
Form: (LIST 'INNER-FOO-EXPANDED A)   Context: EVAL
Form: 'INNER-FOO-EXPANDED   Context: EVAL
Form: A   Context: EVAL; lexically bound
Form: (FOO 1)   Context: EVAL
Form: (INNER-FOO-EXPANDED 1)   Context: EVAL
Form: 1   Context: EVAL
\(MACROLET ((FOO (A)
             `(INNER-FOO-EXPANDED ,A)))
  (FOO 1))")))

(test-util:with-test (:name (:walk macrolet progn 1))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (progn (bar 1)
                                                 (macrolet ((bar (a)
                                                              `(inner-bar-expanded ,a)))
                                                   (bar 2)))))
           "Form: (PROGN
       (BAR 1)
       (MACROLET ((BAR (A)
                    `(INNER-BAR-EXPANDED ,A)))
         (BAR 2)))   Context: EVAL
Form: (BAR 1)   Context: EVAL
Form: 'GLOBAL-BAR   Context: EVAL
Form: (MACROLET ((BAR (A)
                   `(INNER-BAR-EXPANDED ,A)))
        (BAR 2))   Context: EVAL
Form: `(INNER-BAR-EXPANDED ,A)   Context: EVAL
Form: (LIST 'INNER-BAR-EXPANDED A)   Context: EVAL
Form: 'INNER-BAR-EXPANDED   Context: EVAL
Form: A   Context: EVAL; lexically bound
Form: (BAR 2)   Context: EVAL
Form: (INNER-BAR-EXPANDED 2)   Context: EVAL
Form: 2   Context: EVAL
\(PROGN
  (BAR 1)
  (MACROLET ((BAR (A)
               `(INNER-BAR-EXPANDED ,A)))
    (BAR 2)))")))

(test-util:with-test (:name (:walk macrolet progn 2))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (progn (bar 1)
                                                 (macrolet ((bar (s)
                                                              (bar s)
                                                              `(inner-bar-expanded ,s)))
                                                   (bar 2)))))
           "Form: (PROGN
       (BAR 1)
       (MACROLET ((BAR (S)
                    (BAR S)
                    `(INNER-BAR-EXPANDED ,S)))
         (BAR 2)))   Context: EVAL
Form: (BAR 1)   Context: EVAL
Form: 'GLOBAL-BAR   Context: EVAL
Form: (MACROLET ((BAR (S)
                   (BAR S)
                   `(INNER-BAR-EXPANDED ,S)))
        (BAR 2))   Context: EVAL
Form: (BAR S)   Context: EVAL
Form: 'GLOBAL-BAR   Context: EVAL
Form: `(INNER-BAR-EXPANDED ,S)   Context: EVAL
Form: (LIST 'INNER-BAR-EXPANDED S)   Context: EVAL
Form: 'INNER-BAR-EXPANDED   Context: EVAL
Form: S   Context: EVAL; lexically bound
Form: (BAR 2)   Context: EVAL
Form: (INNER-BAR-EXPANDED 2)   Context: EVAL
Form: 2   Context: EVAL
\(PROGN
  (BAR 1)
  (MACROLET ((BAR (S)
               (BAR S)
               `(INNER-BAR-EXPANDED ,S)))
    (BAR 2)))")))

(test-util:with-test (:name (:walk cond))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk (cond (a b)
                                                ((foo bar) a (foo a)))))
           "Form: (COND (A B) ((FOO BAR) A (FOO A)))   Context: EVAL
Form: (IF A B (IF (FOO BAR) (PROGN A (FOO A)) NIL))   Context: EVAL
Form: A   Context: EVAL
Form: B   Context: EVAL
Form: (IF (FOO BAR) (PROGN A (FOO A)) NIL)   Context: EVAL
Form: (FOO BAR)   Context: EVAL
Form: 'GLOBAL-FOO   Context: EVAL
Form: (PROGN A (FOO A))   Context: EVAL
Form: A   Context: EVAL
Form: (FOO A)   Context: EVAL
Form: 'GLOBAL-FOO   Context: EVAL
Form: NIL   Context: EVAL; bound: NIL
\(COND (A B) ((FOO BAR) A (FOO A)))")))

(test-util:with-test (:name (:walk let lambda))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (let ((the-lexical-variables ()))
               (walk-form '(let ((a 1) (b 2))
                            (lambda (x) (list a b x y)))
                          ()
                          (lambda (form context env)
                            (declare (ignore context))
                            (when (and (symbolp form)
                                       (var-lexical-p form env))
                              (push form the-lexical-variables))
                            form))
               (or (and (= (length the-lexical-variables) 3)
                        (member 'a the-lexical-variables)
                        (member 'b the-lexical-variables)
                        (member 'x the-lexical-variables))
                   (error "Walker didn't do lexical variables of a closure properly."))))
           "")))

(test-util:with-test (:name (:walk setq :macro))
  (assert (string=-modulo-tabspace
           (with-output-to-string (*standard-output*)
             (take-it-out-for-a-test-walk
              (macrolet ((x () 'y))
                (setq (x) 3))))
           "Form: (MACROLET ((X ()
                   'Y))
        (SETQ (X) 3))   Context: EVAL
Form: 'Y   Context: EVAL
Form: (SETQ (X) 3)   Context: EVAL
Form: (X)   Context: SET
Form: 3   Context: EVAL
\(MACROLET ((X ()
             'Y))
  (SETQ (X) 3))"
)))

(test-util:with-test (:name (:walk let* special list :hairier-specials))
  (assert
   (string=-modulo-tabspace
    (with-output-to-string (*standard-output*)
      (take-it-out-for-a-test-walk (let* ((a a) (b a) (c b) (b c))
                                     (declare (special a b))
                                     (list a b c))))
    "Form: (LET* ((A A) (B A) (C B) (B C))
        (DECLARE (SPECIAL A B))
        (LIST A B C))   Context: EVAL
Form: A   Context: EVAL
Form: A   Context: EVAL; lexically bound; declared special
Form: B   Context: EVAL; lexically bound
Form: C   Context: EVAL; lexically bound
Form: (LIST A B C)   Context: EVAL
Form: A   Context: EVAL; lexically bound; declared special
Form: B   Context: EVAL; lexically bound; declared special
Form: C   Context: EVAL; lexically bound
\(LET* ((A A) (B A) (C B) (B C))
  (DECLARE (SPECIAL A B))
  (LIST A B C))")))

(test-util:with-test (:name (:walk defclass :type :initform))
  ;; A slot with :TYPE and :INITFORM causes SB-C::WITH-SOURCE-FORM to
  ;; appear in the expansion which didn't have a walker template at
  ;; some point. We just make sure walking the form doesn't signal an
  ;; error.
  (with-output-to-string (*standard-output*)
    (take-it-out-for-a-test-walk
     (defclass foo () ((%bar :type integer :initform 'string))))))

;;;; more tests

;;; Old PCL hung up on this.
(defmethod #:foo ()
  (defun #:bar ()))

;; lp#1912362
(defmethod zook (x) (let ((typep x 'vector)) typep))
(test-util:with-test (:name :let-syntax-error)
  (assertoid:assert-error (zook 1)))

(declaim (inline inlined-fun))

(test-util:with-test (:name :inlined-defun)
  (eval '(defmethod inlined-defun () (defun inlined-fun ()))))

(test-util:with-test (:name :symbol-macrolet-declarations)
  (test-util:checked-compile
   `(lambda ()
      (defmethod ,(gensym) (obj)
        (declare (optimize speed))
        (symbol-macrolet ((x (slot-value obj 'x)))
          (declare (fixnum x))
          (incf x 1))))
   :allow-notes nil))

(defmethod symbol-macrolet-special (s)
  (declare (special s))
  (symbol-macrolet ((s (slot-value x 'x)))
    (let ()
      (declare (special s))
      s)))

(test-util:with-test (:name :symbol-macrolet-declarations)
  (assert (eql (symbol-macrolet-special 10) 10)))
