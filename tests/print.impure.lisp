(in-package :cl-user)

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

;;; success
(quit :unix-status 104)
