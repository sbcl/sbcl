(in-package "SB!VM")

(define-alien-type os-context-t (struct os-context-t-struct))

;;;; MACHINE-TYPE and MACHINE-VERSION

(defun machine-type ()
  "Returns a string describing the type of the local machine."
  "MIPS")

(defun machine-version ()
  "Returns a string describing the version of the local machine."
  #!+little-endian "little-endian"
  #!-little-endian "big-endian")


;;;; FIXUP-CODE-OBJECT

(defun fixup-code-object (code offset value kind)
  (unless (zerop (rem offset n-word-bytes))
    (error "Unaligned instruction?  offset=#x~X." offset))
  (sb!sys:without-gcing
   (let ((sap (truly-the system-area-pointer
			 (%primitive sb!c::code-instructions code))))
     (ecase kind
       (:jump
	(assert (zerop (ash value -28)))
	(setf (ldb (byte 26 0) (sap-ref-32 sap offset))
	      (ash value -2)))
       (:lui
	(setf (sap-ref-16 sap 
			  #!+little-endian offset
			  #!-little-endian (+ offset 2))
	      (+ (ash value -16)
		 (if (logbitp 15 value) 1 0))))
       (:addi
	(setf (sap-ref-16 sap 
			  #!+little-endian offset
			  #!-little-endian (+ offset 2))
	      (ldb (byte 16 0) value)))))))


(define-alien-routine ("os_context_pc_addr" context-pc-addr) (* unsigned-int)
  (context (* os-context-t)))

(defun context-pc (context)
  (declare (type (alien (* os-context-t)) context))
  ;; KLUDGE: this sucks, and furthermore will break on either of (a)
  ;; porting back to IRIX or (b) running on proper 64-bit support.
  ;; Linux on the MIPS defines its registers in the sigcontext as
  ;; 64-bit quantities ("unsigned long long"), presumably to be
  ;; binary-compatible with 64-bit mode.  Since there appears not to
  ;; be ALIEN support for 64-bit return values, we have to do the
  ;; hacky pointer arithmetic thing.  -- CSR, 2002-09-01
  (int-sap (deref (context-pc-addr context) 
		  #!-little-endian 1
		  ;; Untested
		  #!+little-endian 0)))

(define-alien-routine ("os_context_register_addr" context-register-addr)
  (* unsigned-int)
  (context (* os-context-t))
  (index int))

(define-alien-routine ("os_context_bd_cause" context-bd-cause-int)
    (unsigned 32)
  (context (* os-context-t)))

;;; FIXME: Should this and CONTEXT-PC be INLINE to reduce consing?
;;; (Are they used in anything time-critical, or just the debugger?)
(defun context-register (context index)
  (declare (type (alien (* os-context-t)) context))
  (deref (context-register-addr context index) 
	 #!-little-endian 1
	 #!+little-endian 0))

(defun %set-context-register (context index new)
  (declare (type (alien (* os-context-t)) context))
  (setf (deref (context-register-addr context index) 
	       #!-little-endian 1
	       #!+little-endian 0)
	new))

#!+linux
;;; For now.
(defun context-floating-point-modes (context)
  (declare (ignore context))
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
  (/show0 "entering INTERNAL-ERROR-ARGS, CONTEXT=..")
  (/hexstr context)
  (let ((pc (context-pc context))
	(cause (context-bd-cause-int context)))
    (declare (type system-area-pointer pc))
    (/show0 "got PC=..")
    (/hexstr (sap-int pc))
    ;; KLUDGE: This exposure of the branch delay mechanism hurts.
    (when (logbitp 31 cause)
      (setf pc (sap+ pc 4)))
    (when (= (sap-ref-8 pc 4) 255)
      (setf pc (sap+ pc 1)))
    (/show0 "now PC=..")
    (/hexstr (sap-int pc))
    (let* ((length (sap-ref-8 pc 4))
	   (vector (make-array length :element-type '(unsigned-byte 8))))
      (declare (type (unsigned-byte 8) length)
	       (type (simple-array (unsigned-byte 8) (*)) vector))
      (/show0 "LENGTH,VECTOR,ERROR-NUMBER=..")
      (/hexstr length)
      (/hexstr vector)
      (copy-from-system-area pc (* n-byte-bits 5)
			     vector (* n-word-bits
				       vector-data-offset)
			     (* length n-byte-bits))
      (let* ((index 0)
	     (error-number (sb!c:read-var-integer vector index)))
	(/hexstr error-number)
	(collect ((sc-offsets))
	 (loop
	  (/show0 "INDEX=..")
	  (/hexstr index)
	  (when (>= index length)
	    (return))
	  (sc-offsets (sb!c:read-var-integer vector index)))
	 (values error-number (sc-offsets)))))))





