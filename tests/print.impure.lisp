;;;; miscellaneous tests of printing stuff

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

(load "assertoid.lisp")
(use-package "ASSERTOID")

;;; We should be able to output X readably (at least when *READ-EVAL*).
(defun assert-readable-output (x)
  (assert (eql x
	       (let ((*read-eval* t))
		 (read-from-string (with-output-to-string (s)
				     (write x :stream s :readably t)))))))

;;; Even when *READ-EVAL* is NIL, we should be able to output some
;;; (not necessarily readable) representation without signalling an
;;; error.
(defun assert-unreadable-output (x)
  (let ((*read-eval* nil))
    (with-output-to-string (s) (write x :stream s :readably nil))))
  
(defun assert-output (x)
  (assert-readable-output x)
  (assert-unreadable-output x))

;;; Nathan Froyd reported that sbcl-0.6.11.34 screwed up output of
;;; floating point infinities.
(dolist (x (list short-float-positive-infinity short-float-negative-infinity
		 single-float-positive-infinity single-float-negative-infinity
		 double-float-positive-infinity double-float-negative-infinity
		 long-float-positive-infinity long-float-negative-infinity))
  (assert-output x))
 
;;; Eric Marsden reported that this would blow up in CMU CL (even
;;; though ANSI says that the mismatch between ~F expected type and
;;; provided string type is supposed to be handled without signalling
;;; an error) and provided a fix which was ported to sbcl-0.6.12.35.
(assert (null (format t "~F" "foo")))

;;; This was a bug in SBCL until 0.6.12.40 (originally reported as a
;;; CMU CL bug by Erik Naggum on comp.lang.lisp).
(loop for base from 2 to 36
      with *print-radix* = t
      do (let ((*print-base* base))
	   (assert (string= "#*101" (format nil "~S" #*101)))))

;;; bug in sbcl-0.7.1.25, reported by DB sbcl-devel 2002-02-25
(assert (string= "0.5" (format nil "~2D" 0.5)))

;;; we want malformed format strings to cause errors rather than have
;;; some DWIM "functionality".
(assert (raises-error? (format nil "~:2T")))

;;; bug reported, with fix, by Robert Strandh, sbcl-devel 2002-03-09,
;;; fixed in sbcl-0.7.1.36:
(assert (string= (format nil "~2,3,8,'0$" 1234567.3d0) "1234567.30"))

;;; checks that other FORMAT-DOLLAR output remains sane after the
;;; 0.7.1.36 change
(assert (string= (format nil "~$" 0) "0.00"))
(assert (string= (format nil "~$" 4) "4.00"))
(assert (string= (format nil "~$" -4.0) "-4.00"))
(assert (string= (format nil "~2,7,11$" -4.0) "-0000004.00"))
(assert (string= (format nil "~2,7,11,' $" 1.1) " 0000001.10"))
(assert (string= (format nil "~1,7,11,' $" 1.1) "  0000001.1"))
(assert (string= (format nil "~1,3,8,' $" 7.3) "   007.3"))
(assert (string= (format nil "~2,3,8,'0$" 7.3) "00007.30"))

;;; Check for symbol lookup in ~/ / directive -- double-colon was
;;; broken in 0.7.1.36 and earlier
(defun print-foo (stream arg colonp atsignp &rest params)
  (declare (ignore colonp atsignp params))
  (format stream "~d" arg))

(assert (string= (format nil "~/print-foo/" 2) "2"))
(assert (string= (format nil "~/cl-user:print-foo/" 2) "2"))
(assert (string= (format nil "~/cl-user::print-foo/" 2) "2"))
(assert (raises-error? (format nil "~/cl-user:::print-foo/" 2)))
(assert (raises-error? (format nil "~/cl-user:a:print-foo/" 2)))
(assert (raises-error? (format nil "~/a:cl-user:print-foo/" 2)))
(assert (raises-error? (format nil "~/cl-user:print-foo:print-foo/" 2)))

;;; better make sure that we get this one right, too
(defun print-foo\:print-foo (stream arg colonp atsignp &rest params)
  (declare (ignore colonp atsignp params))
  (format stream "~d" arg))

(assert (string= (format nil "~/cl-user:print-foo:print-foo/" 2) "2"))
(assert (string= (format nil "~/cl-user::print-foo:print-foo/" 2) "2"))

;;; Check for error detection of illegal directives in a~<..~> justify
;;; block (see ANSI section 22.3.5.2)
(assert (raises-error? (format nil "~<~W~>" 'foo)))
(assert (raises-error? (format nil "~<~<~A~:>~>" '(foo))))
(assert (string= (format nil "~<~<~A~>~>" 'foo) "FOO"))

;;; Check that arrays that we print while *PRINT-READABLY* is true are
;;; in fact generating similar objects.
(assert (equal (array-dimensions
		(read-from-string
		 (with-output-to-string (s)
		   (let ((*print-readably* t))
		     (print (make-array '(1 2 0)) s)))))
	       '(1 2 0)))

(assert (multiple-value-bind (result error)
	    (ignore-errors (read-from-string
			    (with-output-to-string (s)
			      (let ((*print-readably* t))
				(print (make-array '(1 0 1)) s)))))
	  ;; it might not be readably-printable
	  (or (typep error 'print-not-readable)
	      ;; or else it had better have the same dimensions
	      (equal (array-dimensions result) '(1 0 1)))))

;;; before 0.8.0.66 it signalled UNBOUND-VARIABLE
(write #(1 2 3) :pretty nil :readably t)

;;; another UNBOUND-VARIABLE, this time due to a bug in FORMATTER
;;; expanders.
(funcall (formatter "~@<~A~:*~A~:>") nil 3)

;;; the PPC floating point backend was at one point sufficiently
;;; broken that this looped infinitely or caused segmentation
;;; violations through stack corruption.
(print 0.0001)

;;; In sbcl-0.8.7, the ~W format directive interpreter implemented the
;;; sense of the colon and at-sign modifiers exactly backwards.
;;;
;;; (Yes, the test for this *is* substantially hairier than the fix;
;;; wanna make something of it?)
(cl:in-package :cl-user)
(defstruct wexerciser-0-8-7)
(defun wexercise-0-8-7-interpreted (wformat)
  (format t wformat (make-wexerciser-0-8-7)))
(defmacro define-compiled-wexercise-0-8-7 (wexercise wformat)
  `(defun ,wexercise ()
    (declare (optimize (speed 3) (space 1)))
    (format t ,wformat (make-wexerciser-0-8-7))
    (values)))
(define-compiled-wexercise-0-8-7 wexercise-0-8-7-compiled-without-atsign "~W")
(define-compiled-wexercise-0-8-7 wexercise-0-8-7-compiled-with-atsign "~@W")
(defmethod print-object :before ((wexerciser-0-8-7 wexerciser-0-8-7) stream)
  (unless (and *print-level* *print-length*)
    (error "gotcha coming")))
(let ((*print-level* 11)
      (*print-length* 12))
  (wexercise-0-8-7-interpreted "~W")
  (wexercise-0-8-7-compiled-without-atsign))
(remove-method #'print-object
	       (find-method #'print-object
			    '(:before)
			    (mapcar #'find-class '(wexerciser-0-8-7 t))))
(defmethod print-object :before ((wexerciser-0-8-7 wexerciser-0-8-7) stream)
  (when (or *print-level* *print-length*)
    (error "gotcha going")))
(let ((*print-level* 11)
      (*print-length* 12))
  (wexercise-0-8-7-interpreted "~@W")
  (wexercise-0-8-7-compiled-with-atsign))

;;; WRITE-TO-STRING was erroneously DEFKNOWNed as FOLDABLE
;;;
;;; This bug from PFD
(defpackage "SCRATCH-WRITE-TO-STRING" (:use))
(with-standard-io-syntax
  (let* ((*package* (find-package "SCRATCH-WRITE-TO-STRING"))
	 (answer (write-to-string 'scratch-write-to-string::x :readably nil)))
    (assert (string= answer "X"))))
;;; and a couple from Bruno Haible
(defun my-pprint-reverse (out list)
  (write-char #\( out)
  (when (setq list (reverse list))
    (loop
     (write (pop list) :stream out)
     (when (endp list) (return))
     (write-char #\Space out)))
  (write-char #\) out))
(with-standard-io-syntax
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
    (set-pprint-dispatch '(cons (member foo)) 'my-pprint-reverse 0)
    (let ((answer (write-to-string '(foo bar :boo 1) :pretty t :escape t)))
      (assert (string= answer "(1 :BOO BAR FOO)")))))
(defun my-pprint-logical (out list)
  (pprint-logical-block (out list :prefix "(" :suffix ")")
    (when list
      (loop
       (write-char #\? out)
       (write (pprint-pop) :stream out)
       (write-char #\? out)
       (pprint-exit-if-list-exhausted)
       (write-char #\Space out)))))
(with-standard-io-syntax
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
    (set-pprint-dispatch '(cons (member bar)) 'my-pprint-logical 0)
    (let ((answer (write-to-string '(bar foo :boo 1) :pretty t :escape t)))
      (assert (string= answer "(?BAR? ?FOO? ?:BOO? ?1?)")))))

;;; success
(quit :unix-status 104)
