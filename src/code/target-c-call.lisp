;;;; FIXME: This file and host-c-call.lisp are separate from the
;;;; rest of the alien source code for historical reasons: CMU CL
;;;; made a distinction between the stuff in the C-CALL package and
;;;; stuff in the ALIEN package. There's no obvious boundary
;;;; there, though, and SBCL doesn't try to make this distinction,
;;;; so it might make sense to just merge these files in with the
;;;; rest of the SB-ALIEN code.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!ALIEN")

;;;; extra types

(define-alien-type char (integer 8))
(define-alien-type short (integer 16))
(define-alien-type int (integer 32))
(define-alien-type long (integer #.sb!vm::n-machine-word-bits))

(define-alien-type unsigned-char (unsigned 8))
(define-alien-type unsigned-short (unsigned 16))
(define-alien-type unsigned-int (unsigned 32))
(define-alien-type unsigned-long (unsigned #.sb!vm::n-machine-word-bits))

(define-alien-type float single-float)
(define-alien-type double double-float)

(define-alien-type-translator void ()
  (parse-alien-type '(values) (sb!kernel:make-null-lexenv)))

;;; FIXME: %NATURALIZE-C-STRING (and the UTF8 siblings below) would
;;; appear to be vulnerable to the lisp string moving from underneath
;;; them if the world undergoes a GC, possibly triggered by another
;;; thread.  Ugh.
(defun %naturalize-c-string (sap)
  (declare (type system-area-pointer sap))
  (locally
      (declare (optimize (speed 3) (safety 0)))
    (let ((length (loop for offset of-type fixnum upfrom 0
                        until (zerop (sap-ref-8 sap offset))
                        finally (return offset))))
      (let ((result (make-string length :element-type 'base-char)))
	(sb!kernel:copy-from-system-area sap 0
                                         result (* sb!vm:vector-data-offset
                                                   sb!vm:n-word-bits)
                                         (* length sb!vm:n-byte-bits))
	result))))

(defun %naturalize-utf8-string (sap)
  (declare (type system-area-pointer sap))
  (locally
    (declare (optimize (speed 3) (safety 0)))
    (let ((byte-length (do* ((offset 0 (1+ offset))
			     (byte #1=(sap-ref-8 sap offset) #1#))
			    ((zerop byte) offset))))
      (handler-bind ((sb!impl::octet-decoding-error #'sb!impl::use-unicode-replacement-char))
	(sb!impl::utf8->string-sap-ref-8 sap 0 byte-length)))))

(defun %deport-utf8-string (string)
  (declare (type simple-string string))
  (sb!impl::string->utf8 string 0 (length string) 1))
