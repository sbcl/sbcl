;;;; support for System Area Pointers (SAPs) in the target machine

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;; Return T iff the SAP X points to a smaller address then the SAP Y.
(defun sap< (x y)
  (declare (type system-area-pointer x y))
  (sap< x y))

;;; Return T iff the SAP X points to a smaller or the same address as
;;; the SAP Y.
(defun sap<= (x y)
  (declare (type system-area-pointer x y))
  (sap<= x y))

;;; Return T iff the SAP X points to the same address as the SAP Y.
(defun sap= (x y)
  (declare (type system-area-pointer x y))
  (sap= x y))

;;; Return T iff the SAP X points to a larger or the same address as
;;; the SAP Y.
(defun sap>= (x y)
  (declare (type system-area-pointer x y))
  (sap>= x y))

;;; Return T iff the SAP X points to a larger address then the SAP Y.
(defun sap> (x y)
  (declare (type system-area-pointer x y))
  (sap> x y))

;;; Return a new SAP, OFFSET bytes from SAP.
(defun sap+ (sap offset)
  (declare (type system-area-pointer sap)
           (type (signed-byte #.sb!vm:n-word-bits) offset))
  (sap+ sap offset))

;;; Return the byte offset between SAP1 and SAP2.
(defun sap- (sap1 sap2)
  (declare (type system-area-pointer sap1 sap2))
  (sap- sap1 sap2))

;;; Convert SAP into an integer.
(defun sap-int (sap)
  (declare (type system-area-pointer sap))
  (sap-int sap))

;;; Convert an integer into a SAP.
(defun int-sap (int)
  (int-sap int))

;;; Return the 8-bit byte at OFFSET bytes from SAP.
(defun sap-ref-8 (sap offset)
  (declare (type system-area-pointer sap)
           (fixnum offset))
  (sap-ref-8 sap offset))

(defun sap-ref-octets (sap offset count)
  (declare (type system-area-pointer sap)
           (fixnum offset count))
  (let ((buffer (make-array count :element-type '(unsigned-byte 8))))
    (dotimes (i count)
      (setf (aref buffer i) (sap-ref-8 sap (+ offset i))))
    buffer))

;;; Return the 16-bit word at OFFSET bytes from SAP.
(defun sap-ref-16 (sap offset)
  (declare (type system-area-pointer sap)
           (fixnum offset))
  (sap-ref-16 sap offset))

;;; Returns the 32-bit dualword at OFFSET bytes from SAP.
(defun sap-ref-32 (sap offset)
  (declare (type system-area-pointer sap)
           (fixnum offset))
  (sap-ref-32 sap offset))

;;; Return the 64-bit quadword at OFFSET bytes from SAP.
(defun sap-ref-64 (sap offset)
  (declare (type system-area-pointer sap)
           (fixnum offset))
  (sap-ref-64 sap offset))

;;; Return the unsigned word of natural size OFFSET bytes from SAP.
(defun sap-ref-word (sap offset)
  (declare (type system-area-pointer sap)
           (fixnum offset))
  (sap-ref-word sap offset))

;;; Return the 32-bit SAP at OFFSET bytes from SAP.
(defun sap-ref-sap (sap offset)
  (declare (type system-area-pointer sap)
           (fixnum offset))
  (sap-ref-sap sap offset))

;; Return the LISPOBJ at OFFSET bytes from SAP.
(defun sap-ref-lispobj (sap offset)
  (declare (type system-area-pointer sap)
           (fixnum offset))
  (sap-ref-lispobj sap offset))

;;; Return the 32-bit SINGLE-FLOAT at OFFSET bytes from SAP.
(defun sap-ref-single (sap offset)
  (declare (type system-area-pointer sap)
           (fixnum offset))
  (sap-ref-single sap offset))

;;; Return the 64-bit DOUBLE-FLOAT at OFFSET bytes from SAP.
(defun sap-ref-double (sap offset)
  (declare (type system-area-pointer sap)
           (fixnum offset))
  (sap-ref-double sap offset))

;;; Return the LONG-FLOAT at OFFSET bytes from SAP.
#!+(or x86 long-float)
(defun sap-ref-long (sap offset)
  (declare (type system-area-pointer sap)
           (fixnum offset))
  (sap-ref-long sap offset))

;;; Return the signed 8-bit byte at OFFSET bytes from SAP.
(defun signed-sap-ref-8 (sap offset)
  (declare (type system-area-pointer sap)
           (fixnum offset))
  (signed-sap-ref-8 sap offset))

;;; Return the signed 16-bit word at OFFSET bytes from SAP.
(defun signed-sap-ref-16 (sap offset)
  (declare (type system-area-pointer sap)
           (fixnum offset))
  (signed-sap-ref-16 sap offset))

;;; Return the signed 32-bit dualword at OFFSET bytes from SAP.
(defun signed-sap-ref-32 (sap offset)
  (declare (type system-area-pointer sap)
           (fixnum offset))
  (signed-sap-ref-32 sap offset))

;;; Return the signed 64-bit quadword at OFFSET bytes from SAP.
(defun signed-sap-ref-64 (sap offset)
  (declare (type system-area-pointer sap)
           (fixnum offset))
  (signed-sap-ref-64 sap offset))

;;; Return the signed word of natural size OFFSET bytes from SAP.
(defun signed-sap-ref-word (sap offset)
  (declare (type system-area-pointer sap)
           (fixnum offset))
  (signed-sap-ref-word sap offset))

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

(defun %set-sap-ref-64 (sap offset new-value)
  (declare (type system-area-pointer sap)
           (fixnum offset)
           (type (unsigned-byte 64) new-value))
  (setf (sap-ref-64 sap offset) new-value))

(defun %set-sap-ref-word (sap offset new-value)
  (declare (type system-area-pointer sap)
           (fixnum offset)
           (type (unsigned-byte #.sb!vm:n-machine-word-bits) new-value))
  (setf (sap-ref-word sap offset) new-value))

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

(defun %set-signed-sap-ref-64 (sap offset new-value)
  (declare (type system-area-pointer sap)
           (fixnum offset)
           (type (signed-byte 64) new-value))
  (setf (signed-sap-ref-64 sap offset) new-value))

(defun %set-signed-sap-ref-word (sap offset new-value)
  (declare (type system-area-pointer sap)
           (fixnum offset)
           (type (signed-byte #.sb!vm:n-machine-word-bits) new-value))
  (setf (signed-sap-ref-word sap offset) new-value))

(defun %set-sap-ref-sap (sap offset new-value)
  (declare (type system-area-pointer sap new-value)
           (fixnum offset))
  (setf (sap-ref-sap sap offset) new-value))

(defun %set-sap-ref-lispobj (sap offset new-value)
  (declare (type system-area-pointer sap)
           (fixnum offset)
           (t new-value))
  (setf (sap-ref-lispobj sap offset) new-value))

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
