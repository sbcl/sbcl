;;;; the INSPECT function

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-INSPECT")

(file-comment
  "$Header$")

;;; The inspector views LISP objects as being composed of parts. A
;;; list, for example, would be divided into its members, and a
;;; instance into its slots. These parts are stored in a list. The
;;; first two elements of this list are for bookkeeping. The first
;;; element is a preamble string that will be displayed before the
;;; object. The second element is a boolean value that indicates
;;; whether a label will be printed in front of a value, or just the
;;; value. Symbols and instances need to display both a slot name and
;;; a value, while lists, vectors, and atoms need only display a
;;; value. If the second member of a parts list is t, then the third
;;; and successive members must be an association list of slot names
;;; and values. When the second slot is nil, the third and successive
;;; slots must be the parts of an object.

;;; *INSPECT-OBJECT-STACK* is an assoc list of objects to their parts.
(defvar *inspect-object-stack* ())

(defparameter *inspect-length* 10)

#-sb-fluid (declaim (inline numbered-parts-p))
(defun numbered-parts-p (parts)
  (second parts))

(defconstant parts-offset 2)

(defun nth-parts (parts n)
  (if (numbered-parts-p parts)
      (cdr (nth (+ n parts-offset) parts))
      (nth (+ n parts-offset) parts)))

(defun inspect (object)
  (unwind-protect
      (input-loop object (describe-parts object) *standard-output*)
    (setf *inspect-object-stack* nil)))

;;; When *ILLEGAL-OBJECT-MARKER* occurs in a parts list, it indicates that that
;;; slot is unbound.
(defvar *illegal-object-marker* (cons nil nil))

(defun input-loop (object parts s)
  (tty-display-object parts s)
  (loop
    (format s "~&> ")
    (force-output)
    (let ((command (read))
	  ;; Use 2 less than length because first 2 elements are bookkeeping.
	  (parts-len-2 (- (length parts) 2)))
      (typecase command
	(integer
	 (cond ((< -1 command parts-len-2)
		(cond ((eq (nth-parts parts command) *illegal-object-marker*)
		       (format s "~%That slot is unbound.~%"))
		      (t
		       (push (cons object parts) *inspect-object-stack*)
		       (setf object (nth-parts parts command))
		       (setf parts (describe-parts object))
		       (tty-display-object parts s))))
	       (t
		(if (= parts-len-2 0)
		    (format s "~%This object contains nothing to inspect.~%~%")
		    (format s "~%Enter a VALID number (~:[0-~D~;0~]).~%~%"
			    (= parts-len-2 1) (1- parts-len-2))))))
	(symbol
	 (case (find-symbol (symbol-name command) *keyword-package*)
	   ((:q :e)
	    (return object))
	   (:u
	    (cond (*inspect-object-stack*
		   (setf object (caar *inspect-object-stack*))
		   (setf parts (cdar *inspect-object-stack*))
		   (pop *inspect-object-stack*)
		   (tty-display-object parts s))
		  (t (format s "~%Bottom of Stack.~%"))))
	   (:r
	    (setf parts (describe-parts object))
	    (tty-display-object parts s))
	   (:d
	    (tty-display-object parts s))
	   ((:h :? :help)
	    (show-help s))
	   (t
	    (do-inspect-eval command s))))
	(t
	 (do-inspect-eval command s))))))

(defun do-inspect-eval (command stream)
  (let ((result-list (restart-case (multiple-value-list (eval command))
		       (nil () :report "Return to the inspector."
			  (format stream "~%returning to the inspector~%")
			  (return-from do-inspect-eval nil)))))
    (setf /// // // / / result-list)
    (setf +++ ++ ++ + + - - command)
    (setf *** ** ** * * (car /))
    (format stream "~&~{~S~%~}" /)))

(defun show-help (s)
  (terpri)
  (write-line "inspector help:" s)
  (write-line "  R           -  recompute current object." s)
  (write-line "  D           -  redisplay current object." s)
  (write-line "  U           -  Move upward through the object stack." s)
  (write-line "  Q, E	     -  Quit inspector." s)
  (write-line "  ?, H, Help  -  Show this help." s))

(defun tty-display-object (parts stream)
  (format stream "~%~A" (car parts))
  (let ((numbered-parts-p (numbered-parts-p parts))
	(parts (cddr parts)))
    (do ((part parts (cdr part))
	 (i 0 (1+ i)))
	((endp part) nil)
      (if numbered-parts-p
	  (format stream "~D. ~A: ~A~%" i (caar part)
		  (if (eq (cdar part) *illegal-object-marker*)
		      "unbound"
		      (cdar part)))
	  (format stream "~D. ~A~%" i (car part))))))

;;;; DESCRIBE-PARTS

(defun describe-parts (object)
  (typecase object
    (symbol (describe-symbol-parts object))
    (instance (describe-instance-parts object :structure))
    (function
     (if (sb-kernel:funcallable-instance-p object)
	 (describe-instance-parts object :funcallable-instance)
	 (describe-function-parts object)))
    (vector (describe-vector-parts object))
    (array (describe-array-parts object))
    (cons (describe-cons-parts object))
    (t (describe-atomic-parts object))))

(defun describe-symbol-parts (object)
  (list (format nil "~S is a symbol.~%" object) t
	(cons "Value" (if (boundp object)
			  (symbol-value object)
			  *illegal-object-marker*))
	(cons "Function" (if (fboundp object)
			     (symbol-function object)
			     *illegal-object-marker*))
	(cons "Plist" (symbol-plist object))
	(cons "Package" (symbol-package object))))

(defun describe-instance-parts (object kind)
  (let ((info (layout-info (sb-kernel:layout-of object)))
	(parts-list ()))
    (push (format nil "~S is a ~(~A~).~%" object kind) parts-list)
    (push t parts-list)
    (when (sb-kernel::defstruct-description-p info)
      (dolist (dd-slot (dd-slots info) (nreverse parts-list))
	(push (cons (dsd-%name dd-slot)
		    (funcall (dsd-accessor dd-slot) object))
	      parts-list)))))

(defun describe-function-parts (object)
  (let* ((type (sb-kernel:get-type object))
	 (object (if (= type sb-vm:closure-header-type)
		     (sb-kernel:%closure-function object)
		     object)))
    (list (format nil "Function ~S.~@[~%Argument List: ~A~]." object
		  (sb-kernel:%function-arglist object)
		  ;; Defined-from stuff used to be here. Someone took
		  ;; it out. FIXME: We should make it easy to get
		  ;; to DESCRIBE from the inspector.
		  )
	  t)))

(defun describe-vector-parts (object)
  (list* (format nil "The object is a ~:[~;displaced ~]vector of length ~D.~%"
		 (and (sb-impl::array-header-p object)
		      (sb-impl::%array-displaced-p object))
		 (length object))
	 nil
	 (coerce object 'list)))

(defun describe-cons-parts (object)
  (list* (format nil "The object is a LIST of length ~D.~%" (length object))
	 nil
	 object))

(defun index-string (index rev-dimensions)
  (if (null rev-dimensions)
      "[]"
      (let ((list nil))
	(dolist (dim rev-dimensions)
	  (multiple-value-bind (q r) (floor index dim)
	    (setq index q)
	    (push r list)))
	(format nil "[~D~{,~D~}]" (car list) (cdr list)))))

(defun describe-array-parts (object)
  (let* ((length (min (array-total-size object) *inspect-length*))
	 (reference-array (make-array length :displaced-to object))
	 (dimensions (array-dimensions object))
	 (parts ()))
    (push (format nil "The object is ~:[a displaced~;an~] array of ~A.~%~
		       Its dimensions are ~S.~%"
		  (array-element-type object)
		  (and (sb-impl::array-header-p object)
		       (sb-impl::%array-displaced-p object))
		  dimensions)
	  parts)
    (push t parts)
    (dotimes (i length (nreverse parts))
      (push (cons (format nil "~A " (index-string i (reverse dimensions)))
		  (aref reference-array i))
	    parts))))

(defun describe-atomic-parts (object)
  (list (format nil "The object is an atom.~%") nil object))
