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

;;; circular lists are still being printed correctly?
(assert (equal
         (with-output-to-string (*standard-output*)
           (let ((*print-circle* t))
             (pprint-logical-block (*standard-output* *circ-list*)
                                 (format *standard-output* "~S" *circ-list*))))
         "#1=(1 . #1#)"))

;;; test from CLHS
(assert (equal
         (with-output-to-string (*standard-output*)
          (let ((a (list 1 2 3)))
            (setf (cdddr a) a)
            (let ((*print-circle* t))
              (write a :stream *standard-output*))
            :done))
         "#1=(1 2 3 . #1#)"))

;;; test case 1 for bug 99
(assert (equal
         (with-output-to-string (*standard-output*)
           (let* ((*print-circle* t))
             (format *standard-output* "~@<~S ~_is ~S. This was not seen!~:>"
                     'eql 'eql)))
         "EQL is EQL. This was not seen!"))

;;; test case 2 for bug 99
(assert (equal
         (with-output-to-string (*standard-output*)
           (let* ((*print-circle* t))
             (format *standard-output*
                     "~@<~S ~_is ~S and ~S. This was not seen!~:>"
                     'eql 'eql 'eql)))
         "EQL is EQL and EQL. This was not seen!"))

;;; the original test for BUG 99 (only interactive), no obvious
;;; way to make an automated test:
;;;  (LET ((*PRINT-CIRCLE* T)) (DESCRIBE (MAKE-HASH-TABLE)))

;;; bug 263: :PREFIX, :PER-LINE-PREFIX and :SUFFIX arguments of
;;; PPRINT-LOGICAL-BLOCK may be complex strings
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
                              ;3x"))))

;;; bug 141b: not enough care taken to disambiguate ,.FOO and ,@FOO
;;; from , .FOO and , @FOO
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
	 "`(,?FOO)"))

;;; bug reported by Paul Dietz on sbcl-devel: unquoted lambda lists
;;; were leaking the SB-IMPL::BACKQ-COMMA implementation.
(assert (equal
	 (with-output-to-string (s)
	   (write '`(foo ,x) :stream s :pretty t :readably t))
	 "`(FOO ,X)"))
(assert (equal
	 (with-output-to-string (s)
	   (write '`(foo ,@x) :stream s :pretty t :readably t))
	 "`(FOO ,@X)"))
#+nil ; '`(foo ,.x) => '`(foo ,@x) apparently. 
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
#+nil ; see above
(assert (equal
	 (with-output-to-string (s)
	   (write '`(lambda ,.x) :stream s :pretty t :readably t))
	 "`(LAMBDA ,.X)"))
(assert (equal
	 (with-output-to-string (s)
	   (write '`(lambda (,x)) :stream s :pretty t :readably t))
	 "`(LAMBDA (,X))"))

;;; success
(quit :unix-status 104)
