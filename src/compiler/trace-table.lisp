;;;; trace tables (from codegen.lisp in CMU CL sources)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

(defun trace-table-entry (state)
  (let ((label (gen-label)))
    (emit-label label)
    (push (cons label state) *trace-table-info*))
  (values))

;;; Convert the list of (label . state) entries into an ivector.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant tt-bits-per-state 3)
  (defconstant tt-bytes-per-entry 2)
  (defconstant tt-bits-per-entry (* tt-bytes-per-entry sb!vm:byte-bits))
  (defconstant tt-bits-per-offset (- tt-bits-per-entry tt-bits-per-state))
  (defconstant tt-max-offset (1- (ash 1 tt-bits-per-offset))))
(deftype tt-state ()
  `(unsigned-byte ,tt-bits-per-state))
(deftype tt-entry ()
  `(unsigned-byte ,tt-bits-per-entry))
(deftype tt-offset ()
  `(unsigned-byte ,tt-bits-per-offset))
(declaim (ftype (function (list) (simple-array tt-entry 1)) pack-trace-table))
(defun pack-trace-table (entries)
  (declare (list entries))
  #!-gengc (declare (ignore entries))
  #!+gengc (let ((result (make-array (logandc2 (1+ (length entries)) 1)
				     :element-type 'tt-entry))
		 (index 0)
		 (last-posn 0)
		 (last-state 0))
	     (declare (type index index last-posn)
	     (type tt-state last-state))
	     (flet ((push-entry (offset state)
		      (declare (type tt-offset offset)
			       (type tt-state state))
		      (when (>= index (length result))
			(setf result
			      (replace (make-array
					(truncate (* (length result) 5) 4)
					:element-type
					'tt-entry)
				       result)))
		      (setf (aref result index)
			    (logior (ash offset tt-bits-per-state) state))
		      (incf index)))
	       (dolist (entry entries)
		 (let* ((posn (label-position (car entry)))
			(state (cdr entry)))
		   (declare (type index posn) (type tt-state state))
		   (assert (<= last-posn posn))
		   (do ((offset (- posn last-posn) (- offset tt-max-offset)))
		   ((< offset tt-max-offset)
		    (push-entry offset state))
		   (push-entry tt-max-offset last-state))
		   (setf last-posn posn)
		   (setf last-state state)))
	       (when (oddp index)
		 (push-entry 0 last-state)))
	     (if (eql (length result) index)
	       result
	       (subseq result 0 index)))
  #!-gengc (make-array 0 :element-type 'tt-entry))
