;;;; This file contains some extensions to the Alien facility to
;;;; simplify importing C interfaces.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C-CALL")

;;;; extra types

(def-alien-type char (integer 8))
(def-alien-type short (integer 16))
(def-alien-type int (integer 32))
(def-alien-type long (integer #!-alpha 32 #!+alpha 64))

(def-alien-type unsigned-char (unsigned 8))
(def-alien-type unsigned-short (unsigned 16))
(def-alien-type unsigned-int (unsigned 32))
(def-alien-type unsigned-long (unsigned #!-alpha 32 #!+alpha 64))

(def-alien-type float single-float)
(def-alien-type double double-float)

(def-alien-type-translator void ()
  (parse-alien-type '(values) (sb!kernel:make-null-lexenv)))

(defun %naturalize-c-string (sap)
  (declare (type system-area-pointer sap))
  (with-alien ((ptr (* char) sap))
    (locally
     (declare (optimize (speed 3) (safety 0)))
     (let ((length (loop
		     for offset of-type fixnum upfrom 0
		     until (zerop (deref ptr offset))
		     finally (return offset))))
       (let ((result (make-string length)))
	 (sb!kernel:copy-from-system-area (alien-sap ptr) 0
					  result (* sb!vm:vector-data-offset
						    sb!vm:word-bits)
					  (* length sb!vm:byte-bits))
	 result)))))
