;;;; stubs for the Gray streams implementation for SBCL

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is in the public domain and is provided with absolutely no
;;;; warranty. See the COPYING and CREDITS files for more information.

(in-package "SB!GRAY")

;;; The intent here is that when Gray streams support isn't installed
;;; yet, and someone tries to do a stream operation on something
;;; which isn't an ordinary CL:STREAM, and the code tries to fall
;;; through to the Gray stream operation, we signal a type error,
;;; instead of an undefined function error.
;;;
;;; Real Gray stream functions will overwrite these stubs. FIXME: When
;;; and if Gray stream functions become a stable part of the system,
;;; we should just delete all this.
(defun %gray-stream-stub (oughtta-be-stream &rest rest)
  (declare (ignore rest))
  (error 'simple-type-error
	 :datum oughtta-be-stream
	 :expected-type 'stream
	 :format-control "~@<not a ~S: ~2I~_~S~:>"
	 :format-arguments (list 'stream oughtta-be-stream)))

(dolist (funname
	 '(stream-advance-to-column
	   stream-clear-input stream-clear-output
	   stream-finish-output stream-force-output
	   stream-fresh-line
	   stream-line-column
	   stream-line-length
	   stream-listen stream-peek-char
	   stream-read-byte
	   stream-read-char stream-read-char-no-hang
	   stream-read-line
	   stream-start-line-p
	   stream-terpri
	   stream-unread-char
	   stream-write-byte stream-write-char
	   stream-write-string))
  (setf (fdefinition funname) #'%gray-stream-stub))
