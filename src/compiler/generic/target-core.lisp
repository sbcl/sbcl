;;;; target-only code that knows how to load compiled code directly
;;;; into core
;;;;
;;;; FIXME: The filename here is confusing because "core" here means
;;;; "main memory", while elsewhere in the system it connotes a
;;;; ".core" file dumping the contents of main memory.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;; Make a function entry, filling in slots from the ENTRY-INFO.
(defun make-function-entry (entry code-obj object)
  (declare (type entry-info entry) (type core-object object))
  (let ((offset (label-position (entry-info-offset entry))))
    (declare (type index offset))
    (unless (zerop (logand offset sb!vm:lowtag-mask))
      (error "Unaligned function object, offset = #X~X." offset))
    (let ((res (%primitive compute-function code-obj offset)))
      (setf (%simple-fun-self res) res)
      (setf (%simple-fun-next res) (%code-entry-points code-obj))
      (setf (%code-entry-points code-obj) res)
      (setf (%simple-fun-name res) (entry-info-name entry))
      (setf (%simple-fun-arglist res) (entry-info-arguments entry))
      (setf (%simple-fun-type res) (entry-info-type entry))

      (note-function entry res object))))

;;; Dump a component to core. We pass in the assembler fixups, code vector
;;; and node info.
(defun make-core-component (component segment length trace-table fixups object)
  (declare (type component component)
	   (type sb!assem:segment segment)
	   (type index length)
	   (list trace-table fixups)
	   (type core-object object))
  (without-gcing
    (let* ((2comp (component-info component))
	   (constants (ir2-component-constants 2comp))
	   (trace-table (pack-trace-table trace-table))
	   (trace-table-len (length trace-table))
	   (trace-table-bits (* trace-table-len tt-bits-per-entry))
	   (total-length (+ length
			    (ceiling trace-table-bits sb!vm:n-byte-bits)))
	   (box-num (- (length constants) sb!vm:code-trace-table-offset-slot))
	   (code-obj
	    ;; FIXME: In CMU CL the X86 behavior here depended on
	    ;; *ENABLE-DYNAMIC-SPACE-CODE*, but in SBCL we always use
	    ;; dynamic space code, so we could make
	    ;; ALLOCATE-DYNAMIC-CODE-OBJECT more parallel with
	    ;; ALLOCATE-CODE-OBJECT and remove this confusing
	    ;; read-macro conditionalization.
	    #!+x86
	    (%primitive allocate-dynamic-code-object box-num total-length)
	    #!-x86
	    (%primitive allocate-code-object box-num total-length))
	   (fill-ptr (code-instructions code-obj)))
      (declare (type index box-num total-length))

      (sb!assem:on-segment-contents-vectorly
       segment
       (lambda (v)
	 (declare (type (simple-array sb!assem:assembly-unit 1) v))
	 (copy-byte-vector-to-system-area v fill-ptr)
	 (setf fill-ptr (sap+ fill-ptr (length v)))))

      (do-core-fixups code-obj fixups)

      (dolist (entry (ir2-component-entries 2comp))
	(make-function-entry entry code-obj object))

      (sb!vm:sanctify-for-execution code-obj)

      (let ((info (debug-info-for-component component)))
	(push info (core-object-debug-info object))
	(setf (%code-debug-info code-obj) info))

      (setf (code-header-ref code-obj sb!vm:code-trace-table-offset-slot)
	    length)
      (copy-to-system-area trace-table
			   (* sb!vm:vector-data-offset sb!vm:n-word-bits)
			   fill-ptr
			   0
			   trace-table-bits)

      (do ((index sb!vm:code-constants-offset (1+ index)))
	  ((>= index (length constants)))
	(let ((const (aref constants index)))
	  (etypecase const
	    (null)
	    (constant
	     (setf (code-header-ref code-obj index)
		   (constant-value const)))
	    (list
	     (ecase (car const)
	       (:entry
		(reference-core-function code-obj index
					 (cdr const) object))
	       (:fdefinition
		(setf (code-header-ref code-obj index)
		      (fdefinition-object (cdr const) t))))))))))
  (values))
