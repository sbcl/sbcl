;;;; test of the pretty-printer

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

(in-package :cl-user)

;;;; tests for former BUG 99, where pretty-printing was pretty messed
;;;; up, e.g. PPRINT-LOGICAL-BLOCK - because of CHECK-FOR-CIRCULARITY
;;;; - didn't really work:
;;;;   "DESCRIBE interacts poorly with *PRINT-CIRCLE*, e.g. the output from
;;;;    (let ((*print-circle* t)) (describe (make-hash-table)))
;;;;  is weird, [...] #<HASH-TABLE :TEST EQL :COUNT 0 {90BBFC5}> is an . (EQL)
;;;; ..."
;;;; So, this was mainly a pretty printing problem.

;;; Create a circular list.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *circ-list* '(1 1))
  (prog1 nil
    (setf (cdr *circ-list*) *circ-list*)))

;;; I think this test is bogus. PPRINT-LOGICAL-BLOCK needs to print
;;; the #1= and mark *CIRC-LIST* as having been printed for the first
;;; time. After that any attempt to print *CIRC-LIST* must result in
;;; in a #1# being printed. Thus the right output is (for once)
;;; #1=#1#. -- JES, 2005-06-05
#+nil
;;; circular lists are still being printed correctly?
(assert (equal
         (with-output-to-string (*standard-output*)
           (let ((*print-circle* t))
             (pprint-logical-block (*standard-output* *circ-list*)
                                 (format *standard-output* "~S" *circ-list*))))
         "#1=(1 . #1#)"))

;;; test from CLHS
(with-test (:name :pprint-clhs-example)
  (assert (equal
           (with-output-to-string (*standard-output*)
             (let ((a (list 1 2 3)))
               (setf (cdddr a) a)
               (let ((*print-circle* t))
                 (write a :stream *standard-output*))
               :done))
           "#1=(1 2 3 . #1#)")))

(with-test (:name (:pprint :bug-99))
  (assert (equal
           (with-output-to-string (*standard-output*)
             (let* ((*print-circle* t))
               (format *standard-output* "~@<~S ~_is ~S. This was not seen!~:>"
                       'eql 'eql)))
           "EQL is EQL. This was not seen!"))

  (assert (equal
           (with-output-to-string (*standard-output*)
             (let* ((*print-circle* t))
               (format *standard-output*
                       "~@<~S ~_is ~S and ~S. This was not seen!~:>"
                       'eql 'eql 'eql)))
           "EQL is EQL and EQL. This was not seen!")))

;;; the original test for BUG 99 (only interactive), no obvious
;;; way to make an automated test:
;;;  (LET ((*PRINT-CIRCLE* T)) (DESCRIBE (MAKE-HASH-TABLE)))

;;; bug 263: :PREFIX, :PER-LINE-PREFIX and :SUFFIX arguments of
;;; PPRINT-LOGICAL-BLOCK may be complex strings
(with-test (:name :pprint-logical-block-arguments-complex-strings)
  (let ((list '(1 2 3))
        (prefix (make-array 2
                            :element-type 'character
                            :displaced-to ";x"
                            :fill-pointer 1))
        (suffix (make-array 2
                            :element-type 'character
                            :displaced-to ">xy"
                            :displaced-index-offset 1
                            :fill-pointer 1)))
    (assert (equal (with-output-to-string (s)
                     (pprint-logical-block (s list
                                              :per-line-prefix prefix
                                              :suffix suffix)
                       (format s "~{~W~^~:@_~}" list)))
                   (format nil ";1~%~
                              ;2~%~
                              ;3x")))))

;;; bug 141b: not enough care taken to disambiguate ,.FOO and ,@FOO
;;; from , .FOO and , @FOO
(with-test (:name :pprint-backquote-magic)
  (assert (equal
           (with-output-to-string (s)
             (write '`(,  .foo) :stream s :pretty t :readably t))
           "`(, .FOO)"))
  (assert (equal
           (with-output-to-string (s)
             (write '`(,  @foo) :stream s :pretty t :readably t))
           "`(, @FOO)"))
  (assert (equal
           (with-output-to-string (s)
             (write '`(,  ?foo) :stream s :pretty t :readably t))
           "`(,?FOO)")))

;;; bug reported by Paul Dietz on sbcl-devel: unquoted lambda lists
;;; were leaking the SB-IMPL::BACKQ-COMMA implementation.
(with-test (:name :pprint-leaking-backq-comma)
  (assert (equal
           (with-output-to-string (s)
             (write '`(foo ,x) :stream s :pretty t :readably t))
           "`(FOO ,X)"))
  (assert (equal
           (with-output-to-string (s)
             (write '`(foo ,@x) :stream s :pretty t :readably t))
           "`(FOO ,@X)"))
  #+nil                       ; '`(foo ,.x) => '`(foo ,@x) apparently.
  (assert (equal
           (with-output-to-string (s)
             (write '`(foo ,.x) :stream s :pretty t :readably t))
           "`(FOO ,.X)"))
  (assert (equal
           (with-output-to-string (s)
             (write '`(lambda ,x) :stream s :pretty t :readably t))
           "`(LAMBDA ,X)"))
  (assert (equal
           (with-output-to-string (s)
             (write '`(lambda ,@x) :stream s :pretty t :readably t))
           "`(LAMBDA ,@X)"))
  #+nil                                 ; see above
  (assert (equal
           (with-output-to-string (s)
             (write '`(lambda ,.x) :stream s :pretty t :readably t))
           "`(LAMBDA ,.X)"))
  (assert (equal
           (with-output-to-string (s)
             (write '`(lambda (,x)) :stream s :pretty t :readably t))
           "`(LAMBDA (,X))")))

;;; more backquote printing brokenness, fixed quasi-randomly by CSR.
;;; NOTE KLUDGE FIXME: because our backquote optimizes at read-time,
;;; these assertions, like the ones above, are fragile.  Likewise, it
;;; is very possible that at some point READABLY printing backquote
;;; expressions will have to change to printing the low-level conses,
;;; since the magical symbols are accessible though (car '`(,foo)) and
;;; friends.  HATE HATE HATE.  -- CSR, 2004-06-10
(with-test (:name :pprint-more-backquote-brokeness)
  (assert (equal
           (with-output-to-string (s)
             (write '``(foo ,@',@bar) :stream s :pretty t))
           "``(FOO ,@',@BAR)"))
  (assert (equal
           (with-output-to-string (s)
             (write '``(,,foo ,',foo foo) :stream s :pretty t))
           "``(,,FOO ,',FOO FOO)"))
  (assert (equal
           (with-output-to-string (s)
             (write '``(((,,foo) ,',foo) foo) :stream s :pretty t))
           "``(((,,FOO) ,',FOO) FOO)")))

;;; SET-PPRINT-DISPATCH should accept function name arguments, and not
;;; rush to coerce them to functions.
(set-pprint-dispatch '(cons (eql frob)) 'ppd-function-name)
(defun ppd-function-name (s o)
  (print (length o) s))

(with-test (:name (:set-pprint-dispatch :no-function-coerce)))
(let ((s (with-output-to-string (s)
           (pprint '(frob a b) s))))
  (assert (position #\3 s)))

;; Test that circularity detection works with pprint-logical-block
;; (including when called through pprint-dispatch).
(with-test (:name :pprint-circular-detection)
  (let ((*print-pretty* t)
        (*print-circle* t)
        (*print-pprint-dispatch* (copy-pprint-dispatch)))
    (labels ((pprint-a (stream form &rest rest)
               (declare (ignore rest))
               (pprint-logical-block (stream form :prefix "<" :suffix ">")
                 (pprint-exit-if-list-exhausted)
                 (loop
                   (write (pprint-pop) :stream stream)
                   (pprint-exit-if-list-exhausted)
                   (write-char #\space stream)))))
      (set-pprint-dispatch '(cons (eql a)) #'pprint-a)
      (assert (string= "<A 1 2 3>"
                       (with-output-to-string (s)
                         (write '(a 1 2 3) :stream s))))
      (assert (string= "#1=<A 1 #1# #2=#(2) #2#>"
                       (with-output-to-string (s)
                         (write '#2=(a 1 #2# #5=#(2) #5#) :stream s))))
      (assert (string= "#1=(B #2=<A 1 #1# 2 3> #2#)"
                       (with-output-to-string (s)
                         (write '#3=(b #4=(a 1 #3# 2 3) #4#) :stream s)))))))

;; Test that a circular improper list inside a logical block works.
(with-test (:name :pprint-circular-improper-lists-inside-logical-blocks)
  (let ((*print-circle* t)
        (*print-pretty* t))
    (assert (string= "#1=(#2=(#2# . #3=(#1# . #3#)))"
                     (with-output-to-string (s)
                       (write '#1=(#2=(#2# . #3=(#1# . #3#))) :stream s))))))

;;; Printing malformed defpackage forms without errors.
(with-test (:name :pprint-defpackage)
  (let ((*standard-output* (make-broadcast-stream)))
    (pprint '(defpackage :foo nil))
    (pprint '(defpackage :foo 42))))

(with-test (:name :standard-pprint-dispatch-modified)
  (assert
   (eq :error
       (handler-case (with-standard-io-syntax
                       (set-pprint-dispatch 'symbol (constantly nil))
                       :no-error)
         (sb-int:standard-pprint-dispatch-table-modified-error ()
           :error)))))

(with-test (:name :pprint-defmethod-lambda-list-function)
  (flet ((to-string (form)
           (let ((string (with-output-to-string (s) (pprint form s))))
             (assert (eql #\newline (char string 0)))
             (subseq string 1))))
    (assert (equal "(DEFMETHOD FOO ((FUNCTION CONS)) FUNCTION)"
                   (to-string `(defmethod foo ((function cons)) function))))
    (assert (equal "(DEFMETHOD FOO :AFTER (FUNCTION CONS) FUNCTION)"
                   (to-string `(defmethod foo :after (function cons) function))))))

(defclass frob () ())

(defmethod print-object ((obj frob) stream)
  (print-unreadable-object (obj stream :type nil :identity nil)
    (format stream "FRABOTZICATOR")))

;;; SBCL < 1.0.38 printed #<\nFRABOTIZICATOR>
(with-test (:name (:pprint-unreadable-object :no-ugliness-when-type=nil))
  (assert (equal "#<FRABOTZICATOR>"
                 (let ((*print-right-margin* 5)
                       (*print-pretty* t))
                   (format nil "~@<~S~:>" (make-instance 'frob))))))

(with-test (:name :pprint-logical-block-code-deletion-node)
  (handler-case
      (compile nil
               `(lambda (words &key a b c)
                  (pprint-logical-block (nil words :per-line-prefix (or a b c))
                    (pprint-fill *standard-output* (sort (copy-seq words) #'string<) nil))))
    ((or sb-ext:compiler-note warning) (c)
      (error e))))

(with-test (:name :pprint-logical-block-multiple-per-line-prefix-eval)
  (funcall (compile nil
                    `(lambda ()
                       (let ((n 0))
                         (with-output-to-string (s)
                           (pprint-logical-block (s nil :per-line-prefix (if (eql 1 (incf n))
                                                                             "; "
                                                                             (error "oops")))
                             (pprint-newline :mandatory s)
                             (pprint-newline :mandatory s)))
                         n)))))


;;; success
