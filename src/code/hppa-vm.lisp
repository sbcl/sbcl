(in-package "SB!VM")

(define-alien-type os-context-t (struct os-context-t-struct))

;;;; MACHINE-TYPE and MACHINE-VERSION

(defun machine-type ()
  "Returns a string describing the type of the local machine."
  "HPPA")

;;; support for CL:MACHINE-VERSION defined OAOO elsewhere
(defun get-machine-version ()
  nil)

;;;; FIXUP-CODE-OBJECT

(defun fixup-code-object (code offset value kind)
  (unless (zerop (rem offset n-word-bytes))
    (error "Unaligned instruction?  offset=#x~X." offset))
  (sb!sys:without-gcing
   (let* ((sap (truly-the system-area-pointer
			  (%primitive sb!kernel::code-instructions code)))
	  (inst (sap-ref-32 sap offset)))
     (setf (sap-ref-32 sap offset)
	   (ecase kind
	     (:load
	      (logior (ash (ldb (byte 11 0) value) 1)
		      (logand inst #xffffc000)))
	     (:load-short
	      (let ((low-bits (ldb (byte 11 0) value)))
		(assert (<= 0 low-bits (1- (ash 1 4))))
		(logior (ash low-bits 17)
			(logand inst #xffe0ffff))))
	     (:hi
	      (logior (ash (ldb (byte 5 13) value) 16)
		      (ash (ldb (byte 2 18) value) 14)
		      (ash (ldb (byte 2 11) value) 12)
		      (ash (ldb (byte 11 20) value) 1)
		      (ldb (byte 1 31) value)
		      (logand inst #xffe00000)))
	     (:branch
	      (let ((bits (ldb (byte 9 2) value)))
		(assert (zerop (ldb (byte 2 0) value)))
		(logior (ash bits 3)
			(logand inst #xffe0e002)))))))))

(define-alien-routine ("os_context_pc_addr" context-pc-addr) (* unsigned-int)
  (context (* os-context-t)))

(defun context-pc (context)
  (declare (type (alien (* os-context-t)) context))
  (int-sap (logandc2 (deref (context-pc-addr context)) 3)))

(define-alien-routine ("os_context_register_addr" context-register-addr)
  (* unsigned-int)
  (context (* os-context-t))
  (index int))

;;; FIXME: Should this and CONTEXT-PC be INLINE to reduce consing?
;;; (Are they used in anything time-critical, or just the debugger?)
(defun context-register (context index)
  (declare (type (alien (* os-context-t)) context))
  (deref (context-register-addr context index)))

(defun %set-context-register (context index new)
(declare (type (alien (* os-context-t)) context))
(setf (deref (context-register-addr context index))
      new))

#!+linux
;;; For now.
(defun context-floating-point-modes (context)
  (warn "stub CONTEXT-FLOATING-POINT-MODES")
  0)

;;;; Internal-error-arguments.

;;; INTERNAL-ERROR-ARGUMENTS -- interface.
;;;
;;; Given the sigcontext, extract the internal error arguments from the
;;; instruction stream.
;;; 
(defun internal-error-args (context)
  (declare (type (alien (* os-context-t)) context))
  (let ((pc (context-pc context)))
    (declare (type system-area-pointer pc))
    (let* ((length (sap-ref-8 pc 4))
	   (vector (make-array length :element-type '(unsigned-byte 8))))
      (declare (type (unsigned-byte 8) length)
	       (type (simple-array (unsigned-byte 8) (*)) vector))
      (copy-from-system-area pc (* n-byte-bits 5)
			     vector (* n-word-bits
				       vector-data-offset)
			     (* length n-byte-bits))
      (let* ((index 0)
	     (error-number (sb!c:read-var-integer vector index)))
	(collect ((sc-offsets))
	 (loop
	  (when (>= index length)
	    (return))
	  (sc-offsets (sb!c:read-var-integer vector index)))
	 (values error-number (sc-offsets)))))))
