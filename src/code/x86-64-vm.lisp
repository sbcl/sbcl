;;;; X86-64-specific runtime stuff

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; OS-CONTEXT-T

;;; a POSIX signal context, i.e. the type passed as the third 
;;; argument to an SA_SIGACTION-style signal handler
;;;
;;; The real type does have slots, but at Lisp level, we never
;;; access them, or care about the size of the object. Instead, we
;;; always refer to these objects by pointers handed to us by the C
;;; runtime library, and ask the runtime library any time we need
;;; information about the contents of one of these objects. Thus, it
;;; works to represent this as an object with no slots.
;;;
;;; KLUDGE: It would be nice to have a type definition analogous to
;;; C's "struct os_context_t;", for an incompletely specified object
;;; which can only be referred to by reference, but I don't know how
;;; to do that in the FFI, so instead we just this bogus no-slots
;;; representation. -- WHN 20000730
;;;
;;; FIXME: Since SBCL, unlike CMU CL, uses this as an opaque type,
;;; it's no longer architecture-dependent, and probably belongs in
;;; some other package, perhaps SB-KERNEL.
(define-alien-type os-context-t (struct os-context-t-struct))

;;;; MACHINE-TYPE and MACHINE-VERSION

(defun machine-type ()
  #!+sb-doc
  "Return a string describing the type of the local machine."
  "X86-64")

;;; arch-specific support for CL:MACHINE-VERSION, defined OAOO elsewhere
(defun get-machine-version ()
  #!+linux
  (with-open-file (stream "/proc/cpuinfo"
			  ;; Even on Linux it's an option to build
			  ;; kernels without /proc filesystems, so
			  ;; degrade gracefully.
			  :if-does-not-exist nil)
    (loop with line while (setf line (read-line stream nil))
	  ;; The field "model name" exists on kernel 2.4.21-rc6-ac1
	  ;; anyway, with values e.g.
	  ;;   "AMD Athlon(TM) XP 2000+"
	  ;;   "Intel(R) Pentium(R) M processor 1300MHz"
	  ;; which seem comparable to the information in the example
	  ;; in the MACHINE-VERSION page of the ANSI spec.
          when (eql (search "model name" line) 0)
          return (string-trim " " (subseq line (1+ (position #\: line))))))
  #!-linux
  nil)

;;;; :CODE-OBJECT fixups

;;; a counter to measure the storage overhead of these fixups
(defvar *num-fixups* 0)
;;; FIXME: When the system runs, it'd be interesting to see what this is.

(declaim (inline adjust-fixup-array))
(defun adjust-fixup-array (array size)
  (let ((new (make-array size :element-type '(unsigned-byte 64))))
    (replace new array)
    new))

;;; This gets called by LOAD to resolve newly positioned objects
;;; with things (like code instructions) that have to refer to them.
;;;
;;; Add a fixup offset to the vector of fixup offsets for the given
;;; code object.
(defun fixup-code-object (code offset fixup kind)
  (declare (type index offset))
  (flet ((add-fixup (code offset)
	   ;; (We check for and ignore fixups for code objects in the
	   ;; read-only and static spaces. (In the old CMU CL code
	   ;; this check was conditional on *ENABLE-DYNAMIC-SPACE-CODE*,
	   ;; but in SBCL relocatable dynamic space code is always in
	   ;; use, so we always do the check.)
	   (incf *num-fixups*)
	   (let ((fixups (code-header-ref code code-constants-offset)))
	     (cond ((typep fixups '(simple-array (unsigned-byte 64) (*)))
		    (let ((new-fixups
			   (adjust-fixup-array fixups (1+ (length fixups)))))
		      (setf (aref new-fixups (length fixups)) offset)
		      (setf (code-header-ref code code-constants-offset)
			    new-fixups)))
		   (t
		    (unless (or (eq (widetag-of fixups)
				    unbound-marker-widetag)
				(zerop fixups))
		      (format t "** Init. code FU = ~S~%" fixups)) ; FIXME
		    (setf (code-header-ref code code-constants-offset)
			  (make-array
			   1
			   :element-type '(unsigned-byte 64)
			   :initial-element offset)))))))
    (sb!sys:without-gcing
     (let* ((sap (truly-the system-area-pointer
			    (sb!kernel:code-instructions code)))
	    (obj-start-addr (logand (sb!kernel:get-lisp-obj-address code)
				    #xfffffffffffffff8))
	    (code-start-addr (sb!sys:sap-int (sb!kernel:code-instructions
					      code)))
	    (ncode-words (sb!kernel:code-header-ref code 1))
	    (code-end-addr (+ code-start-addr (* ncode-words n-word-bytes))))
       (unless (member kind '(:absolute :absolute64 :relative))
	 (error "Unknown code-object-fixup kind ~S." kind))
       (ecase kind
	 (:absolute64
	  ;; Word at sap + offset contains a value to be replaced by
	  ;; adding that value to fixup.
	  (setf (sap-ref-64 sap offset) (+ fixup (sap-ref-64 sap offset)))
	  ;; Record absolute fixups that point within the code object.
	  (when (> code-end-addr (sap-ref-64 sap offset) obj-start-addr)
	    (add-fixup code offset)))
	 (:absolute
	  ;; Word at sap + offset contains a value to be replaced by
	  ;; adding that value to fixup.
	  (setf (sap-ref-32 sap offset) (+ fixup (sap-ref-32 sap offset)))
	  ;; Record absolute fixups that point within the code object.
	  (when (> code-end-addr (sap-ref-32 sap offset) obj-start-addr)
	    (add-fixup code offset)))
	 (:relative
	  ;; Fixup is the actual address wanted.
	  ;;
	  ;; Record relative fixups that point outside the code
	  ;; object.
	  (when (or (< fixup obj-start-addr) (> fixup code-end-addr))
	    (add-fixup code offset))
	  ;; Replace word with value to add to that loc to get there.
	  (let* ((loc-sap (+ (sap-int sap) offset))
		 (rel-val (- fixup loc-sap (/ n-word-bytes 2))))
	    (declare (type (unsigned-byte 64) loc-sap)
		     (type (signed-byte 32) rel-val))
	    (setf (signed-sap-ref-32 sap offset) rel-val))))))
    nil))

;;; Add a code fixup to a code object generated by GENESIS. The fixup
;;; has already been applied, it's just a matter of placing the fixup
;;; in the code's fixup vector if necessary.
;;;
;;; KLUDGE: I'd like a good explanation of why this has to be done at
;;; load time instead of in GENESIS. It's probably simple, I just haven't
;;; figured it out, or found it written down anywhere. -- WHN 19990908
#!+gencgc
(defun !envector-load-time-code-fixup (code offset fixup kind)
  (flet ((frob (code offset)
	   (let ((fixups (code-header-ref code code-constants-offset)))
	     (cond ((typep fixups '(simple-array (unsigned-byte 64) (*)))
		    (let ((new-fixups
			   (adjust-fixup-array fixups (1+ (length fixups)))))
		      (setf (aref new-fixups (length fixups)) offset)
		      (setf (code-header-ref code code-constants-offset)
			    new-fixups)))
		   (t
		    (unless (or (eq (widetag-of fixups)
				    unbound-marker-widetag)
				(zerop fixups))
		      (sb!impl::!cold-lose "Argh! can't process fixup"))
		    (setf (code-header-ref code code-constants-offset)
			  (make-array
			   1
			   :element-type '(unsigned-byte 64)
			   :initial-element offset)))))))
    (let* ((sap (truly-the system-area-pointer
			   (sb!kernel:code-instructions code)))
	   (obj-start-addr
	    ;; FIXME: looks like (LOGANDC2 foo typebits)
	    (logand (sb!kernel:get-lisp-obj-address code) #xfffffffffffffff8))
	   (code-start-addr (sb!sys:sap-int (sb!kernel:code-instructions
					     code)))
	   (ncode-words (sb!kernel:code-header-ref code 1))
	 (code-end-addr (+ code-start-addr (* ncode-words n-word-bytes))))
      (ecase kind
	(:absolute
	 ;; Record absolute fixups that point within the code object.
	 ;; The fixup data is 32 bits, don't use SAP-REF-64 here.
	 (when (> code-end-addr (sap-ref-32 sap offset) obj-start-addr)
	   (frob code offset)))
	(:relative
	 ;; Record relative fixups that point outside the code object.
	 (when (or (< fixup obj-start-addr) (> fixup code-end-addr))
	   (frob code offset)))))))

;;;; low-level signal context access functions
;;;;
;;;; Note: In CMU CL, similar functions were hardwired to access
;;;; BSD-style sigcontext structures defined as alien objects. Our
;;;; approach is different in two ways:
;;;;   1. We use POSIX SA_SIGACTION-style signals, so our context is
;;;;      whatever the void pointer in the sigaction handler dereferences
;;;;      to, not necessarily a sigcontext.
;;;;   2. We don't try to maintain alien definitions of the context
;;;;      structure at Lisp level, but instead call alien C functions
;;;;      which take care of access for us. (Since the C functions can
;;;;      be defined in terms of system standard header files, they
;;;;      should be easier to maintain; and since Lisp code uses signal
;;;;      contexts only in interactive or exception code (like the debugger
;;;;      and internal error handling) the extra runtime cost should be
;;;;      negligible.

(define-alien-routine ("os_context_pc_addr" context-pc-addr) (* unsigned-long)
  ;; (Note: Just as in CONTEXT-REGISTER-ADDR, we intentionally use an
  ;; 'unsigned *' interpretation for the 32-bit word passed to us by
  ;; the C code, even though the C code may think it's an 'int *'.)
  (context (* os-context-t)))

(defun context-pc (context)
  (declare (type (alien (* os-context-t)) context))
  (let ((addr (context-pc-addr context)))
    (declare (type (alien (* unsigned-long)) addr))
    (int-sap (deref addr))))

(define-alien-routine ("os_context_register_addr" context-register-addr)
  (* unsigned-long)
  ;; (Note the mismatch here between the 'int *' value that the C code
  ;; may think it's giving us and the 'unsigned *' value that we
  ;; receive. It's intentional: the C header files may think of
  ;; register values as signed, but the CMU CL code tends to think of
  ;; register values as unsigned, and might get bewildered if we ask
  ;; it to work with signed values.)
  (context (* os-context-t))
  (index int))

(defun context-register (context index)
  (declare (type (alien (* os-context-t)) context))
  (let ((addr (context-register-addr context index)))
    (declare (type (alien (* unsigned-long)) addr))
    (deref addr)))

(defun %set-context-register (context index new)
  (declare (type (alien (* os-context-t)) context))
  (let ((addr (context-register-addr context index)))
    (declare (type (alien (* unsigned-long)) addr))
    (setf (deref addr) new)))

;;; This is like CONTEXT-REGISTER, but returns the value of a float
;;; register. FORMAT is the type of float to return.
;;;
;;; As of sbcl-0.6.7, there is no working code which calls this code,
;;; so it's stubbed out. Someday, in order to make the debugger work
;;; better, it may be necessary to unstubify it.
(defun context-float-register (context index format)
  (declare (ignore context index))
  (warn "stub CONTEXT-FLOAT-REGISTER")
  (coerce 0.0 format))
(defun %set-context-float-register (context index format new-value)
  (declare (ignore context index))
  (warn "stub %SET-CONTEXT-FLOAT-REGISTER")
  (coerce new-value format))

;;; Given a signal context, return the floating point modes word in
;;; the same format as returned by FLOATING-POINT-MODES.
#!-linux
(defun context-floating-point-modes (context)
  (declare (ignore context)) ; stub!
  (warn "stub CONTEXT-FLOATING-POINT-MODES")
  0)
#!+linux
(define-alien-routine ("os_context_fp_control" context-floating-point-modes)
    (sb!alien:unsigned 32)
  (context (* os-context-t)))

(define-alien-routine
    ("arch_get_fp_modes" floating-point-modes) (sb!alien:unsigned 32))

(define-alien-routine
    ("arch_set_fp_modes" %floating-point-modes-setter) void (fp (sb!alien:unsigned 32)))

(defun (setf floating-point-modes) (val) (%floating-point-modes-setter val))


;;;; INTERNAL-ERROR-ARGS

;;; Given a (POSIX) signal context, extract the internal error
;;; arguments from the instruction stream.
(defun internal-error-args (context)
  (declare (type (alien (* os-context-t)) context))
  (/show0 "entering INTERNAL-ERROR-ARGS, CONTEXT=..")
  (/hexstr context)
  (let ((pc (context-pc context)))
    (declare (type system-area-pointer pc))
    (/show0 "got PC")
    ;; using INT3 the pc is .. INT3 <here> code length bytes...
    (let* ((length (sap-ref-8 pc 1))
	   (vector (make-array length :element-type '(unsigned-byte 8))))
      (declare (type (unsigned-byte 8) length)
	       (type (simple-array (unsigned-byte 8) (*)) vector))
      (/show0 "LENGTH,VECTOR,ERROR-NUMBER=..")
      (/hexstr length)
      (/hexstr vector)
      (copy-ub8-from-system-area pc 2 vector 0 length)
      (let* ((index 0)
	     (error-number (sb!c:read-var-integer vector index)))
	(/hexstr error-number)
	(collect ((sc-offsets))
	  (loop
	   (/show0 "INDEX=..")
	   (/hexstr index)
	   (when (>= index length)
	     (return))
	   (let ((sc-offset (sb!c:read-var-integer vector index)))
	     (/show0 "SC-OFFSET=..")
	     (/hexstr sc-offset)
	     (sc-offsets sc-offset)))
	  (values error-number (sc-offsets)))))))


;;; the current alien stack pointer; saved/restored for non-local exits
(defvar *alien-stack*)
