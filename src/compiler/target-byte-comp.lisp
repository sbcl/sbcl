;;;; This file contains the noise to byte-compile stuff. It uses the
;;;; same front end as the real compiler, but generates byte code
;;;; instead of native code.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;; Generate trace file output for the byte compiler back end.
;;;
;;; (Note: As of sbcl-0.6.7, this is target-only code not because it's
;;; logically target-only, but just because it's still implemented in
;;; terms of SAPs.)
(defun describe-byte-component (component xeps segment *standard-output*)
  (format t "~|~%;;;; byte component ~S~2%" (component-name component))
  (format t ";;; functions:~%")
  (dolist (fun (component-lambdas component))
    (when (leaf-name fun)
      (let ((info (leaf-info fun)))
	(when info
	  (format t "~6D: ~S~%"
		  (sb!assem:label-position (byte-lambda-info-label info))
		  (leaf-name fun))))))

  (format t "~%;;;disassembly:~2%")
  (collect ((eps)
	    (chunks))
    (dolist (x xeps)
      (let ((xep (cdr x)))
	(etypecase xep
	  (simple-byte-function
	   (eps (simple-byte-function-entry-point xep)))
	  (hairy-byte-function
	   (dolist (ep (hairy-byte-function-entry-points xep))
	     (eps ep))
	       (when (hairy-byte-function-more-args-entry-point xep)
		 (eps (hairy-byte-function-more-args-entry-point xep)))))))
    ;; In CMU CL, this was
    ;;   (SB!ASSEM:SEGMENT-MAP-OUTPUT
    ;;      SEGMENT
    ;;      #'(LAMBDA (SAP BYTES) (CHUNKS (CONS SAP BYTES))))
    ;; -- WHN 19990811
    (sb!assem:on-segment-contents-vectorly segment
					   (lambda (chunk) (chunks chunk)))
    (flet ((chunk-n-bytes (chunk) (length chunk)))
      (let* ((total-bytes (reduce #'+ (chunks) :key #'chunk-n-bytes))
             ;; FIXME: It's not clear that BUF has to be a SAP instead
             ;; of a nice high-level, safe, friendly vector. Perhaps
             ;; this code could be rewritten to use ordinary indices and
             ;; vectors instead of SAP references to chunks of raw
             ;; system memory? Failing that, the DEALLOCATE-SYSTEM-MEMORY
	     ;; operation below should probably be tied to the
	     ;; allocation here with an UNWIND-PROTECT relationship.
             (buf (allocate-system-memory total-bytes)))
        (let ((offset 0))
          (dolist (chunk (chunks))
            (let ((chunk-n-bits (* (chunk-n-bytes chunk) sb!vm:byte-bits)))
              (declare (type (simple-array (unsigned-byte 8)) chunk))
              (copy-byte-vector-to-system-area chunk buf offset)
              (incf offset chunk-n-bits))))
        (disassem-byte-sap buf
                           total-bytes
                           (map 'vector
                                (lambda (x)
				  (if (constant-p x)
				      (constant-value x)
				      x))
				(byte-component-info-constants
				 (component-info component)))
			   (sort (eps) #'<))
	(terpri)
	(deallocate-system-memory buf total-bytes)
	(values)))))

;;; Given a byte-compiled function, disassemble it to standard output.
(defun disassem-byte-fun (xep)
  (declare (optimize (inhibit-warnings 3)))
  (disassem-byte-component
   (byte-function-component xep)
   (etypecase xep
     (simple-byte-function
      (list (simple-byte-function-entry-point xep)))
     (hairy-byte-function
      (sort (copy-list
	     (if (hairy-byte-function-more-args-entry-point xep)
		 (cons (hairy-byte-function-more-args-entry-point xep)
		       (hairy-byte-function-entry-points xep))
		 (hairy-byte-function-entry-points xep)))
	    #'<)))))

;;; Given a byte-compiled component, disassemble it to standard
;;; output. EPS is a list of the entry points.
(defun disassem-byte-component (component &optional (eps '(0)))
  (let* ((bytes (* (code-header-ref component sb!vm:code-code-size-slot)
		   sb!vm:word-bytes))
	 (num-consts (- (get-header-data component)
			sb!vm:code-constants-offset))
	 (consts (make-array num-consts)))
    (dotimes (i num-consts)
      (setf (aref consts i)
	    (code-header-ref component (+ i sb!vm:code-constants-offset))))
    (without-gcing
      (disassem-byte-sap (code-instructions component) bytes
			 consts eps))
    (values)))

;;; Disassemble byte code from a SAP and constants vector.
(defun disassem-byte-sap (sap bytes constants eps)
  (declare (optimize (inhibit-warnings 3)))
  (let ((index 0))
    (declare (type index index))
    (labels ((newline ()
	       (format t "~&~4D:" index))
	     (next-byte ()
	       (let ((byte (sap-ref-8 sap index)))
		 (format t " ~2,'0X" byte)
		 (incf index)
		 byte))
	     (extract-24-bits ()
	       (logior (ash (next-byte) 16)
		       (ash (next-byte) 8)
		       (next-byte)))
	     (extract-extended-op ()
	       (let ((byte (next-byte)))
		 (if (= byte 255)
		     (extract-24-bits)
		     byte)))
	     (extract-4-bit-op (byte)
	       (let ((4-bits (ldb (byte 4 0) byte)))
		 (if (= 4-bits 15)
		     (extract-extended-op)
		     4-bits)))
	     (extract-3-bit-op (byte)
	       (let ((3-bits (ldb (byte 3 0) byte)))
		 (if (= 3-bits 7)
		     :var
		     3-bits)))
	     (extract-branch-target (byte)
	       (if (logbitp 0 byte)
		   (let ((disp (next-byte)))
		     (if (logbitp 7 disp)
			 (+ index disp -256)
			 (+ index disp)))
		   (extract-24-bits)))
	     (note (string &rest noise)
	       (format t " ~14T~?" string noise))
	     (get-constant (index)
	       (if (< -1 index (length constants))
		   (aref constants index)
		   "<bogus index>")))
      (loop
	(unless (< index bytes)
	  (return))

	(when (eql index (first eps))
	  (newline)
	  (pop eps)
	  (let ((frame-size
		 (let ((byte (next-byte)))
		   (if (< byte 255)
		       (* byte 2)
		       (logior (ash (next-byte) 16)
			       (ash (next-byte) 8)
			       (next-byte))))))
	    (note "entry point, frame-size=~D~%" frame-size)))

	(newline)
	(let ((byte (next-byte)))
	  (macrolet ((dispatch (&rest clauses)
		       `(cond ,@(mapcar (lambda (clause)
					  (destructuring-bind
					      ((mask match) &body body)
					      clause
					    `((= (logand byte ,mask) ,match)
					      ,@body)))
					clauses)
			      (t (error "disassembly failure for bytecode ~X"
					byte)))))
	    (dispatch
	     ((#b11110000 #b00000000)
	      (let ((op (extract-4-bit-op byte)))
		(note "push-local ~D" op)))
	     ((#b11110000 #b00010000)
	      (let ((op (extract-4-bit-op byte)))
		(note "push-arg ~D" op)))
	     ((#b11110000 #b00100000)
	      ;; FIXME: could use WITH-PRINT-RESTRICTIONS here and in
	      ;; next clause (or just in LABELS NOTE) instead of
	      ;; hand-rolling values in each case here
	      (let ((*print-level* 3)
		    (*print-lines* 2))
		(note "push-const ~S" (get-constant (extract-4-bit-op byte)))))
	     ((#b11110000 #b00110000)
	      (let ((op (extract-4-bit-op byte))
		    (*print-level* 3)
		    (*print-lines* 2))
		(note "push-sys-const ~S"
		      (svref *system-constants* op))))
	     ((#b11110000 #b01000000)
	      (let ((op (extract-4-bit-op byte)))
		(note "push-int ~D" op)))
	     ((#b11110000 #b01010000)
	      (let ((op (extract-4-bit-op byte)))
		(note "push-neg-int ~D" (- (1+ op)))))
	     ((#b11110000 #b01100000)
	      (let ((op (extract-4-bit-op byte)))
		(note "pop-local ~D" op)))
	     ((#b11110000 #b01110000)
	      (let ((op (extract-4-bit-op byte)))
		(note "pop-n ~D" op)))
	     ((#b11110000 #b10000000)
	      (let ((op (extract-3-bit-op byte)))
		(note "~:[~;named-~]call, ~D args"
		      (logbitp 3 byte) op)))
	     ((#b11110000 #b10010000)
	      (let ((op (extract-3-bit-op byte)))
		(note "~:[~;named-~]tail-call, ~D args"
		      (logbitp 3 byte) op)))
	     ((#b11110000 #b10100000)
	      (let ((op (extract-3-bit-op byte)))
		(note "~:[~;named-~]multiple-call, ~D args"
		      (logbitp 3 byte) op)))
	     ((#b11111000 #b10110000)
	      ;; local call
	      (let ((op (extract-3-bit-op byte))
		    (target (extract-24-bits)))
		(note "local call ~D, ~D args" target op)))
	     ((#b11111000 #b10111000)
	      ;; local tail-call
	      (let ((op (extract-3-bit-op byte))
		    (target (extract-24-bits)))
		(note "local tail-call ~D, ~D args" target op)))
	     ((#b11111000 #b11000000)
	      ;; local-multiple-call
	      (let ((op (extract-3-bit-op byte))
		    (target (extract-24-bits)))
		(note "local multiple-call ~D, ~D args" target op)))
	     ((#b11111000 #b11001000)
	      ;; return
	      (let ((op (extract-3-bit-op byte)))
		(note "return, ~D vals" op)))
	     ((#b11111110 #b11010000)
	      ;; branch
	      (note "branch ~D" (extract-branch-target byte)))
	     ((#b11111110 #b11010010)
	      ;; if-true
	      (note "if-true ~D" (extract-branch-target byte)))
	     ((#b11111110 #b11010100)
	      ;; if-false
	      (note "if-false ~D" (extract-branch-target byte)))
	     ((#b11111110 #b11010110)
	      ;; if-eq
	      (note "if-eq ~D" (extract-branch-target byte)))
	     ((#b11111000 #b11011000)
	      ;; XOP
	      (let* ((low-3-bits (extract-3-bit-op byte))
		     (xop (nth (if (eq low-3-bits :var) (next-byte) low-3-bits)
			       *xop-names*)))
		(note "xop ~A~@[ ~D~]"
		      xop
		      (case xop
			((catch go unwind-protect)
			 (extract-24-bits))
			((type-check push-n-under)
			 (get-constant (extract-extended-op)))))))

	     ((#b11100000 #b11100000)
	      ;; inline
	      (note "inline ~A"
		    (inline-function-info-function
		     (svref *inline-functions* (ldb (byte 5 0) byte))))))))))))
