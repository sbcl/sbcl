(in-package :cl-user)

(load "assertoid.lisp")

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
(loop for *print-base* from 2 to 36
      with *print-radix* = t
      do
      (assert (string= "#*101" (format nil "~S" #*101))))

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

;;; success
(quit :unix-status 104)
