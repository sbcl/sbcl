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
(define-alien-type long (integer #.sb!vm::n-word-bits))

(define-alien-type unsigned-char (unsigned 8))
(define-alien-type unsigned-short (unsigned 16))
(define-alien-type unsigned-int (unsigned 32))
(define-alien-type unsigned-long (unsigned #.sb!vm::n-word-bits))

(define-alien-type float single-float)
(define-alien-type double double-float)

(define-alien-type-translator void ()
  (parse-alien-type '(values) (sb!kernel:make-null-lexenv)))

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
