;;;; support for System Area Pointers (SAPs) in the target machine

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!SYS")
;;; FIXME: Shouldn't these be IN-PACKAGE SB!KERNEL instead? (They're
;;; not dependent on the OS, only on the CPU architecture.)

(file-comment
  "$Header$")

;;;; primitive SAP operations

(defun sap< (x y)
  #!+sb-doc
  "Return T iff the SAP X points to a smaller address then the SAP Y."
  (declare (type system-area-pointer x y))
  (sap< x y))

(defun sap<= (x y)
  #!+sb-doc
  "Return T iff the SAP X points to a smaller or the same address as
   the SAP Y."
  (declare (type system-area-pointer x y))
  (sap<= x y))

(defun sap= (x y)
  #!+sb-doc
  "Return T iff the SAP X points to the same address as the SAP Y."
  (declare (type system-area-pointer x y))
  (sap= x y))

(defun sap>= (x y)
  #!+sb-doc
  "Return T iff the SAP X points to a larger or the same address as
   the SAP Y."
  (declare (type system-area-pointer x y))
  (sap>= x y))

(defun sap> (x y)
  #!+sb-doc
  "Return T iff the SAP X points to a larger address then the SAP Y."
  (declare (type system-area-pointer x y))
  (sap> x y))

(defun sap+ (sap offset)
  #!+sb-doc
  "Return a new sap OFFSET bytes from SAP."
  (declare (type system-area-pointer sap)
	   (fixnum offset))
  (sap+ sap offset))

(defun sap- (sap1 sap2)
  #!+sb-doc
  "Return the byte offset between SAP1 and SAP2."
  (declare (type system-area-pointer sap1 sap2))
  (sap- sap1 sap2))

(defun sap-int (sap)
  #!+sb-doc
  "Converts a System Area Pointer into an integer."
  (declare (type system-area-pointer sap))
  (sap-int sap))

(defun int-sap (int)
  #!+sb-doc
  "Converts an integer into a System Area Pointer."
  (declare (type sap-int-type int))
  (int-sap int))

(defun sap-ref-8 (sap offset)
  #!+sb-doc
  "Returns the 8-bit byte at OFFSET bytes from SAP."
  (declare (type system-area-pointer sap)
	   (fixnum offset))
  (sap-ref-8 sap offset))

(defun sap-ref-16 (sap offset)
  #!+sb-doc
  "Returns the 16-bit word at OFFSET bytes from SAP."
  (declare (type system-area-pointer sap)
	   (fixnum offset))
  (sap-ref-16 sap offset))

(defun sap-ref-32 (sap offset)
  #!+sb-doc
  "Returns the 32-bit dualword at OFFSET bytes from SAP."
  (declare (type system-area-pointer sap)
	   (fixnum offset))
  (sap-ref-32 sap offset))

#!+alpha
(defun sap-ref-64 (sap offset)
  #!+sb-doc
  "Returns the 64-bit quadword at OFFSET bytes from SAP."
  (declare (type system-area-pointer sap)
	   (fixnum offset))
  (sap-ref-64 sap offset))

(defun sap-ref-sap (sap offset)
  #!+sb-doc
  "Returns the 32-bit system-area-pointer at OFFSET bytes from SAP."
  (declare (type system-area-pointer sap)
	   (fixnum offset))
  (sap-ref-sap sap offset))

(defun sap-ref-single (sap offset)
  #!+sb-doc
  "Returns the 32-bit single-float at OFFSET bytes from SAP."
  (declare (type system-area-pointer sap)
	   (fixnum offset))
  (sap-ref-single sap offset))

(defun sap-ref-double (sap offset)
  #!+sb-doc
  "Returns the 64-bit double-float at OFFSET bytes from SAP."
  (declare (type system-area-pointer sap)
	   (fixnum offset))
  (sap-ref-double sap offset))

#!+(or x86 long-float)
(defun sap-ref-long (sap offset)
  #!+sb-doc
  "Returns the long-float at OFFSET bytes from SAP."
  (declare (type system-area-pointer sap)
	   (fixnum offset))
  (sap-ref-long sap offset))

(defun signed-sap-ref-8 (sap offset)
  #!+sb-doc
  "Returns the signed 8-bit byte at OFFSET bytes from SAP."
  (declare (type system-area-pointer sap)
	   (fixnum offset))
  (signed-sap-ref-8 sap offset))

(defun signed-sap-ref-16 (sap offset)
  #!+sb-doc
  "Returns the signed 16-bit word at OFFSET bytes from SAP."
  (declare (type system-area-pointer sap)
	   (fixnum offset))
  (signed-sap-ref-16 sap offset))

(defun signed-sap-ref-32 (sap offset)
  #!+sb-doc
  "Returns the signed 32-bit dualword at OFFSET bytes from SAP."
  (declare (type system-area-pointer sap)
	   (fixnum offset))
  (signed-sap-ref-32 sap offset))

#!+alpha
(defun signed-sap-ref-64 (sap offset)
  #!+sb-doc
  "Returns the signed 64-bit quadword at OFFSET bytes from SAP."
  (declare (type system-area-pointer sap)
	   (fixnum offset))
  (signed-sap-ref-64 sap offset))

(defun %set-sap-ref-8 (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (fixnum offset)
	   (type (unsigned-byte 8) new-value))
  (setf (sap-ref-8 sap offset) new-value))

(defun %set-sap-ref-16 (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (fixnum offset)
	   (type (unsigned-byte 16) new-value))
  (setf (sap-ref-16 sap offset) new-value))

(defun %set-sap-ref-32 (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (fixnum offset)
	   (type (unsigned-byte 32) new-value))
  (setf (sap-ref-32 sap offset) new-value))

#!+alpha
(defun %set-sap-ref-64 (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (fixnum offset)
	   (type (unsigned-byte 64) new-value))
  (setf (sap-ref-64 sap offset) new-value))

(defun %set-signed-sap-ref-8 (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (fixnum offset)
	   (type (signed-byte 8) new-value))
  (setf (signed-sap-ref-8 sap offset) new-value))

(defun %set-signed-sap-ref-16 (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (fixnum offset)
	   (type (signed-byte 16) new-value))
  (setf (signed-sap-ref-16 sap offset) new-value))

(defun %set-signed-sap-ref-32 (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (fixnum offset)
	   (type (signed-byte 32) new-value))
  (setf (signed-sap-ref-32 sap offset) new-value))

#!+alpha
(defun %set-signed-sap-ref-64 (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (fixnum offset)
	   (type (signed-byte 64) new-value))
  (setf (signed-sap-ref-64 sap offset) new-value))

(defun %set-sap-ref-sap (sap offset new-value)
  (declare (type system-area-pointer sap new-value)
	   (fixnum offset))
  (setf (sap-ref-sap sap offset) new-value))

(defun %set-sap-ref-single (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (fixnum offset)
	   (type single-float new-value))
  (setf (sap-ref-single sap offset) new-value))

(defun %set-sap-ref-double (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (fixnum offset)
	   (type double-float new-value))
  (setf (sap-ref-double sap offset) new-value))

#!+long-float
(defun %set-sap-ref-long (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (fixnum offset)
	   (type long-float new-value))
  (setf (sap-ref-long sap offset) new-value))

;;;; system memory allocation

(sb!alien:def-alien-routine ("os_allocate" allocate-system-memory)
			    system-area-pointer
  (bytes sb!c-call:unsigned-long))

(sb!alien:def-alien-routine ("os_allocate_at" allocate-system-memory-at)
			    system-area-pointer
  (address system-area-pointer)
  (bytes sb!c-call:unsigned-long))

(sb!alien:def-alien-routine ("os_reallocate" reallocate-system-memory)
			    system-area-pointer
  (old system-area-pointer)
  (old-size sb!c-call:unsigned-long)
  (new-size sb!c-call:unsigned-long))

(sb!alien:def-alien-routine ("os_deallocate" deallocate-system-memory)
			    sb!c-call:void
  (addr system-area-pointer)
  (bytes sb!c-call:unsigned-long))
