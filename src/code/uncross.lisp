;;;; converting symbols from SB-XC::FOO to COMMON-LISP::FOO when
;;;; cross-compiling (so that we can maintain distinct SB!XC versions
;;;; of fundamental COMMON-LISP things like PROCLAIM and CLASS and
;;;; ARRAY-RANK-LIMIT, so that we don't trash the cross-compilation
;;;; host when defining the cross-compiler, but the distinctions go
;;;; away in the target system)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!INT")

;;;; $Header$

;;; In the target system's compiler, uncrossing is just identity.
#-sb-xc-host
(progn
  #!-sb-fluid (declaim (inline uncross))
  (defun uncross (x) x))
;;; In the cross-compiler, uncrossing is slightly less trivial.

;;; This condition is only a STYLE-WARNING because generally it isn't important
;;; in practice to recurse through anything except CONSes anyway.
#|
#!+sb-show
(define-condition uncross-rcr-failure (style-warning)
  ((form :initarg :form :reader uncross-rcr-failure-form))
  (:report (lambda (c s)
	     (format s
		     "UNCROSS couldn't recurse through ~S~%~
		      (which is OK as long as there are no SB-XC symbols ~
		      down there)"
		     (uncross-rcr-failure-form c)))))
|#

;;; When cross-compiling, EVAL-WHEN :COMPILE-TOPLEVEL code is executed in the
;;; host Common Lisp, not the target. A certain amount of dancing around is
;;; required in order for this to work more or less correctly. (Fortunately,
;;; more or less correctly is good enough -- it only needs to work on the
;;; EVAL-WHEN expressions found in the SBCL sources themselves, and we can
;;; exercise self-control to keep them from including anything which too
;;; strongly resembles a language lawyer's test case.)
;;;
;;; In order to make the dancing happen, we need to make a distinction between
;;; SB!XC and COMMON-LISP when we're executing a form at compile time (i.e.
;;; within EVAL-WHEN :COMPILE-TOPLEVEL) but we need to treat SB!XC as
;;; synonymous with COMMON-LISP otherwise. This can't be done by making SB!XC a
;;; nickname of COMMON-LISP, because the reader processes things before
;;; EVAL-WHEN, so by the time EVAL-WHEN :COMPILE-TOPLEVEL saw a form, the
;;; distinction it needs would be lost. Instead, we read forms preserving this
;;; distinction (treating SB!XC as a separate package), and only when we're
;;; about to process them (for any situation other than
;;; EVAL-WHEN (:COMPILE-TOPLEVEL)) do we call UNCROSS on them to obliterate the
;;; distinction.
#+sb-xc-host
(defun uncross (form)
  (let ((;; KLUDGE: We don't currently try to handle circular program
	 ;; structure, but we do at least detect it and complain about it..
	 inside? (make-hash-table)))
    (labels ((uncross-symbol (symbol)
               (let ((old-symbol-package (symbol-package symbol)))
		 (if (and old-symbol-package
			  (string= (package-name old-symbol-package) "SB-XC"))
		     (values (intern (symbol-name symbol) "COMMON-LISP"))
		     symbol)))
	     (rcr (form)
	       (cond ((symbolp form)
		      (uncross-symbol form))
		     ((or (numberp form)
			  (characterp form)
			  (stringp form))
		      form)
		     (t
		      ;; If we reach here, FORM is something with internal
		      ;; structure which could include symbols in the SB-XC
		      ;; package.
		      (when (gethash form inside?)
			(let ((*print-circle* t))
			  ;; This code could probably be generalized to work on
			  ;; circular structure, but it seems easier just to
			  ;; avoid putting any circular structure into the
			  ;; bootstrap code.
			  (error "circular structure in ~S" form)))
		      (setf (gethash form inside?) t)
		      (unwind-protect
			  (typecase form
			    (cons (rcr-cons form))
			    ;; Note: This function was originally intended to
			    ;; search through structures other than CONS, but
			    ;; it got into trouble with LAYOUT-CLASS and
			    ;; CLASS-LAYOUT circular structure. After some
			    ;; messing around, it turned out that recursing
			    ;; through CONS is all that's needed in practice.)
			    ;; FIXME: This leaves a lot of stale code here
			    ;; (already commented/NILed out) for us to delete.
			    #+nil ; only searching through CONS
			    (simple-vector (rcr-simple-vector form))
			    #+nil ; only searching through CONS
			    (structure!object
			     (rcr-structure!object form))
			    (t
			     ;; KLUDGE: I know that UNCROSS is far from
			     ;; perfect, but it's good enough to cross-compile
			     ;; the current sources, and getting hundreds of
			     ;; warnings about individual cases it can't
			     ;; recurse through, so the warning here has been
			     ;; turned off. Eventually it would be nice either
			     ;; to set up a cleaner way of cross-compiling
			     ;; which didn't have this problem, or to make
			     ;; an industrial-strength version of UNCROSS
			     ;; which didn't fail this way. -- WHN 20000201
			     #+nil (warn 'uncross-rcr-failure :form form)
			     form))
			(remhash form inside?)))))
	     (rcr-cons (form)
	       (declare (type cons form))
	       (let* ((car (car form))
		      (rcr-car (rcr car))
		      (cdr (cdr form))
		      (rcr-cdr (rcr cdr)))
		 (if (and (eq rcr-car car) (eq rcr-cdr cdr))
		   form
		   (cons rcr-car rcr-cdr))))
	     #+nil ; only searching through CONS in this version
	     (rcr-simple-vector (form)
	       (declare (type simple-vector form))
	       (dotimes (i (length form))
		 (let* ((aref (aref form i))
			(rcr-aref (rcr aref)))
		   (unless (eq rcr-aref aref)
		     (return (map 'vector #'rcr form))))
		 form))
	     #+nil ; only searching through CONS in this version
	     (rcr-structure!object (form)
	       (declare (type structure!object form))
	       ;; Note: We skip the zeroth slot because it's used for LAYOUT,
	       ;; which shouldn't require any translation and which is
	       ;; complicated to think about anyway.
	       (do ((i 1 (1+ i)))
		   ((>= i (%instance-length form)) form)
		 (let* ((instance-ref (%instance-ref form i))
			(rcr-instance-ref (rcr instance-ref)))
		   (unless (eq rcr-instance-ref instance-ref)
		     (return (rcr!-structure!object
			      (copy-structure form)))))))
	     #+nil ; only searching through CONS in this version
	     (rcr!-structure!object (form)
	       (declare (type structure!object form))
	       ;; As in RCR-STRUCTURE!OBJECT, we skip the zeroth slot.
	       (do ((i 1 (1+ i)))
		   ((>= i (%instance-length form)))
		 (let* ((instance-ref (%instance-ref form i))
			(rcr-instance-ref (rcr instance-ref)))
		   ;; (By only calling SETF when strictly necessary,
		   ;; we avoid bombing out unnecessarily when the
		   ;; I-th slot happens to be read-only.)
		   (unless (eq rcr-instance-ref instance-ref)
		     (setf (%instance-ref form i)
			   rcr-instance-ref))))))
      (rcr form))))
