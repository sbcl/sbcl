;;;; stack-group and multi-process support for CMU CL x86

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!MP")

(file-comment
  "$Header$")

;;;; Handle the binding stack.

;;; Undo all the bindings in the bind stack, restoring the global
;;; values.
(defun unbind-binding-stack ()
  (declare (optimize (speed 3) (safety 0)))
  (let* ((binding-stack-pointer (sb!kernel:binding-stack-pointer-sap))
	 (binding-stack
	  (sb!sys:int-sap (sb!alien:extern-alien "binding_stack"
						 sb!alien:unsigned)))
	 (size (sb!sys:sap- binding-stack-pointer binding-stack)))
    (declare (type (unsigned-byte 29) size))
    (do ((binding size))
	((zerop binding))
      (declare (type (unsigned-byte 29) binding))
      (decf binding 8)
      (let* ((value
	      (sb!kernel:make-lisp-obj
	       (sb!sys:sap-int (sb!sys:sap-ref-sap binding-stack binding))))
	     (symbol
	      (sb!kernel:make-lisp-obj
	       (sb!sys:sap-int (sb!sys:sap-ref-sap binding-stack
						   (+ binding 4))))))
	(cond ((symbolp symbol)
	       (let ((symbol-value (sb!c::%primitive sb!c:fast-symbol-value
						     symbol)))
		 #+nil
		 (format t "undoing: ~S ~S <-> ~S~%" symbol value symbol-value)
		 (sb!kernel:%set-symbol-value symbol value)
		 (setf (sb!sys:sap-ref-sap binding-stack binding)
		       (sb!sys:int-sap (sb!kernel:get-lisp-obj-address
					symbol-value)))))
	      (t
	       #+nil
	       (format t "ignoring undoing: ~S ~S~%" symbol value)))))))

;;; Re-apply the bindings in a binding stack after an
;;; unbind-binding-stack.
(defun rebind-binding-stack ()
  (declare (optimize (speed 3) (safety 0)))
  (let* ((binding-stack-pointer (sb!kernel:binding-stack-pointer-sap))
	 (binding-stack
	  (sb!sys:int-sap (sb!alien:extern-alien "binding_stack"
						 sb!alien:unsigned)))
	 (size (sb!sys:sap- binding-stack-pointer binding-stack)))
    (declare (type (unsigned-byte 29) size))
    (do ((binding 0 (+ 8 binding)))
	((= binding size))
      (declare (type (unsigned-byte 29) binding))
      (let* ((value
	      (sb!kernel:make-lisp-obj
	       (sb!sys:sap-int (sb!sys:sap-ref-sap binding-stack binding))))
	     (symbol
	      (sb!kernel:make-lisp-obj
	       (sb!sys:sap-int (sb!sys:sap-ref-sap binding-stack
						   (+ binding 4))))))
	(cond ((symbolp symbol)
	       (let ((symbol-value (sb!c::%primitive sb!c:fast-symbol-value
						     symbol)))
		 #+nil
		 (format t "rebinding: ~S ~S <-> ~S~%"
			 symbol value symbol-value)
		 (sb!kernel:%set-symbol-value symbol value)
		 (setf (sb!sys:sap-ref-sap binding-stack binding)
		       (sb!sys:int-sap (sb!kernel:get-lisp-obj-address
					symbol-value)))))
	      (t
	       #+nil
	       (format t "ignoring rebinding: ~S ~S~%" symbol value)))))))

(defun save-binding-stack (binding-save-stack)
  (declare (type (simple-array t (*)) binding-save-stack)
	   (optimize (speed 3) (safety 0)))
  (let* ((binding-stack-pointer (sb!kernel:binding-stack-pointer-sap))
	 (binding-stack
	  (sb!sys:int-sap (sb!alien:extern-alien "binding_stack"
						 sb!alien:unsigned)))
	 (size (sb!sys:sap- binding-stack-pointer binding-stack))
	 (vector-size (truncate size 4)))
    (declare (type (unsigned-byte 29) size))
    ;; Grow binding-save-stack if necessary.
    (when (< (length binding-save-stack) vector-size)
      (setq binding-save-stack
	    (adjust-array binding-save-stack vector-size :element-type t)))
    ;; Save the stack.
    (do ((binding 0 (+ 4 binding))
	 (index 0 (1+ index)))
	((= binding size))
      (declare (type (unsigned-byte 29) binding index))
      (setf (aref binding-save-stack index)
	    (sb!kernel:make-lisp-obj
	     (sb!sys:sap-int (sb!sys:sap-ref-sap binding-stack binding)))))
    (values binding-save-stack vector-size)))

(defun restore-binding-stack (new-binding-stack size)
  (declare (type (simple-array t (*)) new-binding-stack)
	   (type (unsigned-byte 29) size)
	   (optimize (speed 3) (safety 0)))
  (let* ((binding-stack-size (* size 4))
	 (binding-stack (sb!alien:extern-alien "binding_stack"
					       sb!alien:unsigned)))
    (declare (type (unsigned-byte 32) binding-stack-size binding-stack))
    (setf (sb!kernel:binding-stack-pointer-sap)
	  (sb!sys:int-sap (+ binding-stack binding-stack-size)))
    (do ((binding 0 (+ 4 binding))
	 (index 0 (1+ index)))
	((= binding binding-stack-size))
      (declare (type (unsigned-byte 29) binding index))
      (setf (sb!sys:sap-ref-sap (sb!sys:int-sap binding-stack) binding)
	    (sb!sys:int-sap (sb!kernel:get-lisp-obj-address
			     (aref new-binding-stack index))))))
  (values))

;;;; alien stack

;;; The Top of the Alien-stack.
(declaim (type (unsigned-byte 32) *alien-stack-top*))
(defvar *alien-stack-top* 0)

;;; Save the alien-stack.
(defun save-alien-stack (save-stack)
  (declare (type (simple-array (unsigned-byte 32) (*)) save-stack)
	   (optimize (speed 3) (safety 0)))
  (let* ((alien-stack (sb!kernel:get-lisp-obj-address sb!vm::*alien-stack*))
	 (size (- *alien-stack-top* alien-stack))
	 (vector-size (ceiling size 4)))
    (declare (type (unsigned-byte 32) alien-stack)
	     (type (unsigned-byte 29) size))
    #+nil
    (format t "alien-stack ~X; size ~X~%" alien-stack size)
    ;; Grow save-stack if necessary.
    (when (< (length save-stack) vector-size)
      (setq save-stack
	    (adjust-array save-stack vector-size
			  :element-type '(unsigned-byte 32))))
    ;; Save the stack.
    (do ((index 0 (1+ index)))
	((>= index vector-size))
      (declare (type (unsigned-byte 29) index))
      (setf (aref save-stack index)
	    (sb!sys:sap-ref-32 (sb!sys:int-sap *alien-stack-top*)
			       (* 4 (- (1+ index))))))
    (values save-stack vector-size alien-stack)))

(defun restore-alien-stack (save-stack size alien-stack)
  (declare (type (simple-array (unsigned-byte 32) (*)) save-stack)
	   (type (unsigned-byte 29) size)
	   (type (unsigned-byte 32) alien-stack)
	   (optimize (speed 3) (safety 0)))
  (setf sb!vm::*alien-stack* (sb!kernel:make-lisp-obj alien-stack))
  (do ((index 0 (1+ index)))
      ((>= index size))
    (declare (type (unsigned-byte 29) index))
    (setf (sb!sys:sap-ref-32 (sb!sys:int-sap *alien-stack-top*)
			     (* 4 (- (1+ index))))
	  (aref save-stack index)))
  (values))

;;;; interrupt contexts

;;; Save the interrupt contexts.
(defun save-interrupt-contexts (save-vector)
  (declare (type (simple-array (unsigned-byte 32) (*)) save-vector)
	   (optimize (speed 3) (safety 0)))
  (let* ((size sb!impl::*free-interrupt-context-index*))
    (declare (type (unsigned-byte 29) size))
    ;; Grow save-stack if necessary.
    (when (< (length save-vector) size)
      (setq save-vector
	    (adjust-array save-vector size :element-type '(unsigned-byte 32))))
    (sb!alien:with-alien
	((lisp-interrupt-contexts (array sb!alien:unsigned nil) :extern))
      (dotimes (index size)
	(setf (aref save-vector index)
	      (sb!alien:deref lisp-interrupt-contexts index))))
    save-vector))

;;; Restore the interrupt contexts.
(defun restore-interrupt-contexts (save-vector)
  (declare (type (simple-array (unsigned-byte 32) (*)) save-vector)
	   (optimize (speed 3) (safety 0)))
  (let* ((size sb!impl::*free-interrupt-context-index*))
    (declare (type (unsigned-byte 29) size))
    (sb!alien:with-alien
	((lisp-interrupt-contexts (array sb!alien:unsigned nil) :extern))
      (dotimes (index size)
	(setf (sb!alien:deref lisp-interrupt-contexts index)
	      (aref save-vector index)))))
  (values))

;;; The control stacks need special handling on the X86 as they
;;; contain conservative roots. When placed in the *control-stacks*
;;; vector they will be scavenged for conservative roots by the
;;; garbage collector.
(declaim (type (simple-array (or null (simple-array (unsigned-byte 32) (*)))
			     (*)) sb!vm::*control-stacks*))
(defvar sb!vm::*control-stacks*
  (make-array 0 :element-type '(or null (unsigned-byte 32))
	      :initial-element nil))

;;; Stack-group structure.
(defstruct (stack-group
	     (:constructor %make-stack-group)
	     (:print-object
	      (lambda (stack-group stream)
		(declare (type stack-group stack-group)
			 (stream stream))
		(print-unreadable-object (stack-group stream :identity t)
		 (format stream "stack-group ~A, ~A"
			 (stack-group-name stack-group)
			 (stack-group-state stack-group))))))
  ;; Must have a name.
  (name "Anonymous" :type simple-base-string)
  ;; State: :active or :inactive.
  (state :inactive :type (member :active :inactive))
  ;; The control stack; an index into *control-stacks*.
  (control-stack-id nil :type (or sb!kernel:index null))
  ;; Binding stack.
  (binding-stack nil :type (or (simple-array t (*)) null))
  ;; Twice the number of bindings.
  (binding-stack-size 0 :type (unsigned-byte 29))
  ;; Current catch block, on the control stack.
  (current-catch-block 0 :type fixnum)
  ;; Unwind protect block, on the control stack.
  (current-unwind-protect-block 0 :type fixnum)
  ;; Alien stack
  (alien-stack nil :type (or (simple-array (unsigned-byte 32) (*)) null))
  (alien-stack-size 0 :type (unsigned-byte 29))
  (alien-stack-pointer 0 :type (unsigned-byte 32))
  ;; Eval-stack
  (eval-stack nil :type (or (simple-array t (*)) null))
  (eval-stack-top 0 :type fixnum)
  ;; Interrupt contexts
  (interrupt-contexts nil :type (or (simple-array (unsigned-byte 32) (*))
				    null))
  ;; Resumer
  (resumer nil :type (or stack-group null)))

;;; The current stack group.
(declaim (type (or stack-group null) *current-stack-group*))
(defvar *current-stack-group* nil)

(declaim (type (or stack-group null) *initial-stack-group*))
(defvar *initial-stack-group* nil)

;;; Setup the initial stack group.
(defun init-stack-groups ()
  ;; Grab the top of the alien-stack; it's currently stored at the top
  ;; of the control stack.
  (setf *alien-stack-top*
	(sb!sys:sap-ref-32
	 (sb!sys:int-sap (sb!alien:extern-alien "control_stack_end"
						sb!alien:unsigned))
	 -4))
  ;; Initialise the *control-stacks* vector.
  (setq sb!vm::*control-stacks*
	(make-array 10 :element-type '(or null (unsigned-byte 32))
		    :initial-element nil))
  ;; Setup a control-stack for the initial stack-group.
  (setf (aref sb!vm::*control-stacks* 0)
	(make-array 0
		    :element-type '(unsigned-byte 32)
		    :initial-element 0))
  ;; Make and return the initial stack group.
  (setf *current-stack-group*
	(%make-stack-group
	 :name "initial"
	 :state :active
	 :control-stack-id 0
	 :binding-stack #()
	 :alien-stack (make-array 0 :element-type '(unsigned-byte 32))
	 :interrupt-contexts (make-array 0 :element-type '(unsigned-byte 32))
	 :eval-stack #()))
  (setf *initial-stack-group* *current-stack-group*))

;;; Inactivate the stack group, cleaning its slot and freeing the
;;; control stack.
(defun inactivate-stack-group (stack-group)
  (declare (type stack-group stack-group))
  (setf (stack-group-state stack-group) :inactive)
  (let ((cs-id (stack-group-control-stack-id stack-group)))
    (when (and cs-id (aref sb!vm::*control-stacks* cs-id))
      (setf (aref sb!vm::*control-stacks* cs-id) nil)))
  (setf (stack-group-control-stack-id stack-group) nil)
  (setf (stack-group-binding-stack stack-group) nil)
  (setf (stack-group-binding-stack-size stack-group) 0)
  (setf (stack-group-current-catch-block stack-group) 0)
  (setf (stack-group-current-unwind-protect-block stack-group) 0)
  (setf (stack-group-alien-stack stack-group) nil)
  (setf (stack-group-alien-stack-size stack-group) 0)
  (setf (stack-group-alien-stack-pointer stack-group) 0)
  (setf (stack-group-eval-stack stack-group) nil)
  (setf (stack-group-eval-stack-top stack-group) 0)
  (setf (stack-group-resumer stack-group) nil))

;;; Scrub the binding and eval stack of the give stack-group.
(defun scrub-stack-group-stacks (stack-group)
  (declare (type stack-group stack-group)
	   (optimize (speed 3) (safety 0)))
  ;; Binding stack.
  (let ((binding-save-stack (stack-group-binding-stack stack-group)))
    (when binding-save-stack
      (let ((size
	     ;; The stored binding stack for the current stack group
	     ;; can be completely scrubbed.
	     (if (eq stack-group *current-stack-group*)
		 0
		 (stack-group-binding-stack-size stack-group)))
	    (len (length binding-save-stack)))
	;; Scrub the remainder of the binding stack.
	(do ((index size (+ index 1)))
	    ((>= index len))
	  (declare (type (unsigned-byte 29) index))
	  (setf (aref binding-save-stack index) 0)))))
  ;; If this is the current stack group then update the stored
  ;; eval-stack and eval-stack-top before scrubbing.
  (when (eq stack-group *current-stack-group*)
    ;; Updare the stored vector, flushing an old vector if a new one
    ;; has been allocated.
    (setf (stack-group-eval-stack stack-group) sb!impl::*eval-stack*)
    ;; Ensure that the stack-top is valid.
    (setf (stack-group-eval-stack-top stack-group) sb!impl::*eval-stack-top*))
  ;; Scrub the eval stack.
  (let ((eval-stack (stack-group-eval-stack stack-group)))
    (when eval-stack
      (let ((eval-stack-top (stack-group-eval-stack-top stack-group))
	    (len (length eval-stack)))
	(do ((i eval-stack-top (1+ i)))
	    ((= i len))
	  (declare (type sb!kernel:index i))
	  (setf (svref eval-stack i) nil))))))

;;; Generate the initial bindings for a newly created stack-group.
;;; This function may be redefined to return a vector with other bindings
;;; but *interrupts-enabled* and *gc-inhibit* must be the last two.
(defun initial-binding-stack ()
  (vector
   (find-package "COMMON-LISP-USER") '*package*
   ;; Other bindings may be added here.
   nil 'sb!unix::*interrupts-enabled*
   t 'sb!impl::*gc-inhibit*))

;;; Fork a new stack-group from the *current-stack-group*. Execution
;;; continues with the *current-stack-group* returning the new stack
;;; group. Control may be transfer to the child by stack-group-resume
;;; and it executes the initial-function.
(defun make-stack-group (name initial-function &optional
			      (resumer *current-stack-group*)
			      (inherit t))
  (declare (type simple-base-string name)
	   (type function initial-function)
	   (type stack-group resumer))
  (flet ((allocate-control-stack ()
	   (let* (;; Allocate a new control-stack ID.
		  (control-stack-id (position nil sb!vm::*control-stacks*))
		  ;; Find the required stack size.
		  (control-stack-end
		   (sb!alien:extern-alien "control_stack_end"
					  sb!alien:unsigned))
		  (control-stack-pointer (sb!kernel:control-stack-pointer-sap))
		  (control-stack-size
		   (- control-stack-end
		      (sb!sys:sap-int control-stack-pointer)))
		  ;; Saved control stack needs three extra words. The
		  ;; stack pointer will be stored in the first
		  ;; element, and the frame pointer and return address
		  ;; push onto the bottom of the stack.
		  (control-stack
		   (make-array (+ (ceiling control-stack-size 4) 3)
			       :element-type '(unsigned-byte 32)
			       :initial-element 0)))
	     (declare (type (unsigned-byte 29) control-stack-size))
	     (unless control-stack-id
	       ;; Need to extend the *control-stacks* vector.
	       (setq control-stack-id (length sb!vm::*control-stacks*))
	       (setq sb!vm::*control-stacks*
		     (adjust-array sb!vm::*control-stacks*
				   (* 2 (length sb!vm::*control-stacks*))
				   :element-type '(or null (unsigned-byte 32))
				   :initial-element nil)))
	     (setf (aref sb!vm::*control-stacks* control-stack-id)
		   control-stack)
	     (values control-stack control-stack-id)))
	 ;; Allocate a stack group inheriting stacks and bindings from
	 ;; the current stack group.
	 (allocate-child-stack-group (control-stack-id)
	   ;; Save the interrupt-contexts while the size is still
	   ;; bound.
	   (let ((interrupt-contexts
		  (save-interrupt-contexts
		   (make-array 0 :element-type '(unsigned-byte 32)))))
	     ;; Save the binding stack. Note that
	     ;; *interrutps-enabled* could be briefly set during the
	     ;; unbinding and re-binding process so signals are
	     ;; blocked.
	     (let ((old-sigs (sb!unix:unix-sigblock
			      (sb!unix:sigmask :sigint :sigalrm))))
	       (declare (type (unsigned-byte 32) old-sigs))
	       (unbind-binding-stack)
	       (multiple-value-bind (binding-stack binding-stack-size)
		   (save-binding-stack #())
		 (rebind-binding-stack)
		 (sb!unix:unix-sigsetmask old-sigs)
		 ;; Save the Alien stack.
		 (multiple-value-bind
		     (alien-stack alien-stack-size alien-stack-pointer)
		     (save-alien-stack
		      (make-array 0 :element-type '(unsigned-byte 32)))
		   ;; Allocate a stack-group structure.
		   (%make-stack-group
		    :name name
		    :state :active
		    :control-stack-id control-stack-id
		    ;; Save the Eval stack.
		    :eval-stack (copy-seq (the simple-vector
					       sb!kernel:*eval-stack*))
		    :eval-stack-top sb!kernel:*eval-stack-top*
		    ;; Misc stacks.
		    :current-catch-block sb!impl::*current-catch-block*
		    :current-unwind-protect-block
		    sb!impl::*current-unwind-protect-block*
		    ;; Alien stack.
		    :alien-stack alien-stack
		    :alien-stack-size alien-stack-size
		    :alien-stack-pointer alien-stack-pointer
		    ;; Interrupt contexts
		    :interrupt-contexts interrupt-contexts
		    ;; Binding stack.
		    :binding-stack binding-stack
		    :binding-stack-size binding-stack-size
		    ;; Resumer
		    :resumer resumer))))))
	 ;; Allocate a new stack group with fresh stacks and bindings.
	 (allocate-new-stack-group (control-stack-id)
	   (let ((binding-stack (initial-binding-stack)))
	     ;; Allocate a stack-group structure.
	     (%make-stack-group
	      :name name
	      :state :active
	      :control-stack-id control-stack-id
	      ;; Eval stack. Needs at least one element be because
	      ;; push doubles the size when full.
	      :eval-stack (make-array 32)
	      :eval-stack-top 0
	      ;; Misc stacks.
	      :current-catch-block 0
	      :current-unwind-protect-block 0
	      ;; Alien stack.
	      :alien-stack (make-array 0 :element-type '(unsigned-byte 32))
	      :alien-stack-size 0
	      :alien-stack-pointer *alien-stack-top*
	      ;; Interrupt contexts
	      :interrupt-contexts (make-array 0 :element-type
					      '(unsigned-byte 32))
	      ;; Binding stack - some initial bindings.
	      :binding-stack binding-stack
	      :binding-stack-size (length binding-stack)
	      ;; Resumer
	      :resumer resumer))))
    (let ((child-stack-group nil))
      (let ((sb!unix::*interrupts-enabled* nil)
	    (sb!impl::*gc-inhibit* t))
	(multiple-value-bind (control-stack control-stack-id)
	    (allocate-control-stack)
	  (setq child-stack-group
		(if inherit
		    (allocate-child-stack-group control-stack-id)
		    (allocate-new-stack-group control-stack-id)))
	  ;; Fork the control-stack.
	  (if (sb!vm:control-stack-fork control-stack inherit)
	      ;; Current-stack-group returns the child-stack-group.
	      child-stack-group
	      ;; Child starts.
	      (unwind-protect
		   (progn
		     (setq *current-stack-group* child-stack-group)
		     (assert (eq *current-stack-group*
				 (process-stack-group *current-process*)))
		     ;; Enable interrupts and GC.
		     (setf sb!unix::*interrupts-enabled* t)
		     (setf sb!impl::*gc-inhibit* nil)
		     (when sb!unix::*interrupt-pending*
		       (sb!unix::do-pending-interrupt))
		     (when sb!impl::*need-to-collect-garbage*
		       (sb!impl::maybe-gc))
		     (funcall initial-function))
		(let ((resumer (stack-group-resumer child-stack-group)))
		  ;; Disable interrupts and GC.
		  (setf sb!unix::*interrupts-enabled* nil)
		  (setf sb!impl::*gc-inhibit* t)
		  (inactivate-stack-group child-stack-group)
		  ;; Verify the resumer.
		  (unless (and resumer
			       (eq (stack-group-state resumer) :active))
		    (format t "*resuming stack-group ~S instead of ~S~%"
			    *initial-stack-group* resumer)
		    (setq resumer *initial-stack-group*))
		  ;; Restore the resumer state.
		  (setq *current-stack-group* resumer)
		  ;; Eval-stack
		  (setf sb!kernel:*eval-stack*
			(stack-group-eval-stack resumer))
		  (setf sb!kernel:*eval-stack-top*
			(stack-group-eval-stack-top resumer))
		  ;; The binding stack. Note that
		  ;; *interrutps-enabled* could be briefly set during
		  ;; the unbinding and re-binding process so signals
		  ;; are blocked.
		  (let ((old-sigs (sb!unix:unix-sigblock
				   (sb!unix:sigmask :sigint :sigalrm))))
		    (declare (type (unsigned-byte 32) old-sigs))
		    (unbind-binding-stack)
		    (restore-binding-stack
		     (stack-group-binding-stack resumer)
		     (stack-group-binding-stack-size resumer))
		    (rebind-binding-stack)
		    (sb!unix:unix-sigsetmask old-sigs))
		  ;; Misc stacks.
		  (setf sb!impl::*current-catch-block*
			(stack-group-current-catch-block resumer))
		  (setf sb!impl::*current-unwind-protect-block*
			(stack-group-current-unwind-protect-block resumer))
		  ;; The Alien stack
		  (restore-alien-stack
		   (stack-group-alien-stack resumer)
		   (stack-group-alien-stack-size resumer)
		   (stack-group-alien-stack-pointer resumer))
		  ;; Interrupt-contexts.
		  (restore-interrupt-contexts
		   (stack-group-interrupt-contexts resumer))
		  (let ((new-control-stack
			 (aref sb!vm::*control-stacks*
			       (stack-group-control-stack-id resumer))))
		    (declare (type (simple-array (unsigned-byte 32) (*))
				   new-control-stack))
		    (sb!vm:control-stack-return new-control-stack)))))))
      (when (and sb!unix::*interrupts-enabled* sb!unix::*interrupt-pending*)
	(sb!unix::do-pending-interrupt))
      (when (and sb!impl::*need-to-collect-garbage*
		 (not sb!impl::*gc-inhibit*))
	(sb!impl::maybe-gc))
      child-stack-group)))

;;; Transfer control to the given stack-group, resuming its execution,
;;; and saving the *current-stack-group*.
(defun stack-group-resume (new-stack-group)
  (declare (type stack-group new-stack-group)
	   (optimize (speed 3)))
  (assert (and (eq (stack-group-state new-stack-group) :active)
	       (not (eq new-stack-group *current-stack-group*))))
  (assert (eq new-stack-group (process-stack-group *current-process*)))
  (let ((sb!unix::*interrupts-enabled* nil)
	(sb!impl::*gc-inhibit* t))
    (let* (;; Save the current stack-group on its stack.
	   (stack-group *current-stack-group*)
	   ;; Find the required stack size.
	   (control-stack-end
	    (sb!alien:extern-alien "control_stack_end" sb!alien:unsigned))
	   (control-stack-pointer (sb!kernel:control-stack-pointer-sap))
	   (control-stack-size (- control-stack-end
				  (sb!sys:sap-int control-stack-pointer)))
	   ;; Stack-save array needs three extra elements. The stack
	   ;; pointer will be stored in the first, and the frame
	   ;; pointer and return address push onto the bottom of the
	   ;; stack.
	   (save-stack-size (+ (ceiling control-stack-size 4) 3))
	   ;; the save-stack vector
	   (control-stack (aref sb!vm::*control-stacks*
				(stack-group-control-stack-id stack-group))))
      (declare (type (unsigned-byte 29) control-stack-size save-stack-size)
	       (type (simple-array (unsigned-byte 32) (*)) control-stack))
      ;; Increase the save-stack size if necessary.
      (when (> save-stack-size (length control-stack))
	(setf control-stack (adjust-array control-stack save-stack-size
					  :element-type '(unsigned-byte 32)
					  :initial-element 0))
	(setf (aref sb!vm::*control-stacks*
		    (stack-group-control-stack-id stack-group))
	      control-stack))

      ;; eval-stack
      (setf (stack-group-eval-stack stack-group) sb!kernel:*eval-stack*)
      (setf (stack-group-eval-stack-top stack-group)
	    sb!kernel:*eval-stack-top*)
      (setf sb!kernel:*eval-stack* (stack-group-eval-stack new-stack-group))
      (setf sb!kernel:*eval-stack-top*
	    (stack-group-eval-stack-top new-stack-group))

      ;; misc stacks
      (setf (stack-group-current-catch-block stack-group)
	    sb!impl::*current-catch-block*)
      (setf (stack-group-current-unwind-protect-block stack-group)
	    sb!impl::*current-unwind-protect-block*)
      (setf sb!impl::*current-catch-block*
	    (stack-group-current-catch-block new-stack-group))
      (setf sb!impl::*current-unwind-protect-block*
	    (stack-group-current-unwind-protect-block new-stack-group))

      ;; Save the interrupt-contexts.
      (setf (stack-group-interrupt-contexts stack-group)
	    (save-interrupt-contexts
	     (stack-group-interrupt-contexts stack-group)))

      ;; the binding stack. Note that *interrutps-enabled* could be
      ;; briefly set during the unbinding and re-binding process so
      ;; signals are blocked.
      (let ((old-sigs (sb!unix:unix-sigblock (sb!unix:sigmask :sigint
							      :sigalrm))))
	(declare (type (unsigned-byte 32) old-sigs))
	(unbind-binding-stack)
	(multiple-value-bind (stack size)
	    (save-binding-stack (stack-group-binding-stack stack-group))
	  (setf (stack-group-binding-stack stack-group) stack)
	  (setf (stack-group-binding-stack-size stack-group) size))
	(restore-binding-stack (stack-group-binding-stack new-stack-group)
			       (stack-group-binding-stack-size
				new-stack-group))
	(rebind-binding-stack)
	(sb!unix:unix-sigsetmask old-sigs))

      ;; Restore the interrupt-contexts.
      (restore-interrupt-contexts
       (stack-group-interrupt-contexts new-stack-group))

      ;; The Alien stack
      (multiple-value-bind (save-stack size alien-stack)
	  (save-alien-stack (stack-group-alien-stack stack-group))
	(setf (stack-group-alien-stack stack-group) save-stack)
	(setf (stack-group-alien-stack-size stack-group) size)
	(setf (stack-group-alien-stack-pointer stack-group) alien-stack))
      (restore-alien-stack (stack-group-alien-stack new-stack-group)
			   (stack-group-alien-stack-size new-stack-group)
			   (stack-group-alien-stack-pointer new-stack-group))
      (let ((new-control-stack
	     (aref sb!vm::*control-stacks*
		   (stack-group-control-stack-id new-stack-group))))
	(declare (type (simple-array (unsigned-byte 32) (*))
		       new-control-stack))
	(sb!vm:control-stack-resume control-stack new-control-stack))
      ;; Thread returns.
      (setq *current-stack-group* stack-group)))
  (assert (eq *current-stack-group* (process-stack-group *current-process*)))
  (when (and sb!unix::*interrupts-enabled* sb!unix::*interrupt-pending*)
    (sb!unix::do-pending-interrupt))
  (when (and sb!impl::*need-to-collect-garbage*
	     (not sb!impl::*gc-inhibit*))
    (sb!impl::maybe-gc))
  (values))

;;;; DOUBLE-FLOAT timing functions for use by the scheduler

;;; These timer functions use double-floats for accuracy. In most
;;; cases consing is avoided.

#!-sb-fluid (declaim (inline get-real-time))
(defun get-real-time ()
  #!+sb-doc
  "Return the real time in seconds."
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (ignore seconds useconds) (sb!unix:unix-gettimeofday)
    (declare (ignore ignore)
	     (type (unsigned-byte 32) seconds useconds))
    (+ (coerce seconds 'double-float)
       (* (coerce useconds 'double-float) 1d-6))))

#!-sb-fluid (declaim (inline get-run-time))
(defun get-run-time ()
  #!+sb-doc
  "Return the run time in seconds"
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (ignore utime-sec utime-usec stime-sec stime-usec)
      (sb!unix:unix-fast-getrusage sb!unix:rusage_self)
    (declare (ignore ignore)
	     (type (unsigned-byte 31) utime-sec stime-sec)
	     ;; (Classic CMU CL had these (MOD 1000000) instead, but
	     ;; at least in Linux 2.2.12, the type doesn't seem to be
	     ;; documented anywhere and the observed behavior is to
	     ;; sometimes return 1000000 exactly.)
	     (type (integer 0 1000000) utime-usec stime-usec))
    (+ (coerce utime-sec 'double-float) (coerce stime-sec 'double-float)
       (* (+ (coerce utime-usec 'double-float)
	     (coerce stime-usec 'double-float))
	  1d-6))))

;;;; Multi-process support. The interface is based roughly on the
;;;; CLIM-SYS spec. and support needed for cl-http.

(defvar *multi-processing* t)

(defstruct (process
	     (:constructor %make-process)
	     (:predicate processp)
	     (:print-object
	      (lambda (process stream)
		(print-unreadable-object (process stream :identity t :type t)
		 (write-string (process-name process) stream)))))
  (name "Anonymous" :type simple-base-string)
  (state :killed :type (member :killed :active :inactive))
  (%whostate nil :type (or null simple-base-string))
  (initial-function nil :type (or null function))
  (initial-args nil :type list)
  (wait-function nil :type (or null function))
  ;; The real time after which the wait will timeout.
  (wait-timeout nil :type (or null double-float))
  (wait-return-value nil :type t)
  (interrupts '() :type list)
  (stack-group nil :type (or null stack-group))
  ;; The real and run times when the current process was last
  ;; scheduled or yielded.
  (scheduled-real-time (get-real-time) :type double-float)
  (scheduled-run-time (get-run-time) :type double-float)
  ;; Accrued real and run times in seconds.
  (%real-time 0d0 :type double-float)
  (%run-time 0d0 :type double-float))

(defun process-whostate (process)
  #!+sb-doc
  "Return the process state which is either Run, Killed, or a wait reason."
  (cond ((eq (process-state process) :killed)
	 "Killed")
	((process-wait-function process)
	 (or (process-%whostate process) "Run"))
	(t
	 "Run")))

#!-sb-fluid (declaim (inline process-active-p))
(defun process-active-p (process)
  (eq (process-state process) :active))

#!-sb-fluid (declaim (inline process-alive-p))
(defun process-alive-p (process)
  (let ((state (process-state process)))
    (or (eq state :active) (eq state :inactive))))

(declaim (type (or null process) *current-process*))
(defvar *current-process* nil)

#!-sb-fluid (declaim (inline current-process))
(defun current-process ()
  #!+sb-doc
  "Returns the current process."
  *current-process*)

(declaim (list *all-processes*))
(defvar *all-processes* nil
  #!+sb-doc
  "A list of all alive processes.")

#!-sb-fluid (declaim (inline all-processes))
(defun all-processes ()
  #!+sb-doc
  "Return a list of all the live processes."
  *all-processes*)

(declaim (type (or null process) *intial-process*))
(defvar *initial-process* nil)

;;; Disable scheduling while the body is executed. Scheduling is
;;; typically inhibited when process state is being modified.
(defvar *inhibit-scheduling* t)
(defmacro without-scheduling (&body body)
  #!+sb-doc
  "Execute the body the scheduling disabled."
  `(let ((inhibit *inhibit-scheduling*))
    (unwind-protect
	 (progn
	   (setf *inhibit-scheduling* t)
	   ,@body)
      (setf *inhibit-scheduling* inhibit))))

(defmacro atomic-incf (reference &optional (delta 1))
  #!+sb-doc
  "Increments the reference by delta in a single atomic operation"
  `(without-scheduling
    (incf ,reference ,delta)))

(defmacro atomic-decf (reference &optional (delta 1))
  #!+sb-doc
  "Decrements the reference by delta in a single atomic operation"
  `(without-scheduling
    (decf ,reference ,delta)))

(defmacro atomic-push (obj place)
  #!+sb-doc
  "Atomically push object onto place."
  `(without-scheduling
    (push ,obj ,place)))

(defmacro atomic-pop (place)
  #!+sb-doc
  "Atomically pop place."
  `(without-scheduling
    (pop ,place)))

;;; If a process other than the initial process throws to the
;;; %END-OF-THE-WORLD then *QUITTING-LISP* is set to the exit value,
;;; after which further process creation blocks. If the initial
;;; process is running the idle loop then it will perform the exit
;;; when it runs.
(defvar *quitting-lisp* nil)

;;; Update the processes times for the current and new process before
;;; a process switch.
(defun update-process-timers (current-process new-process)
  (declare (type process current-process new-process)
	   (optimize (speed 3) (safety 0)))
  (let ((real-time (get-real-time)))
    (incf (process-%real-time current-process)
	  (- real-time (process-scheduled-real-time current-process)))
    (setf (process-scheduled-real-time current-process) real-time)
    (setf (process-scheduled-real-time new-process) real-time))
  (let ((run-time (get-run-time)))
    (incf (process-%run-time current-process)
	  (- run-time (process-scheduled-run-time current-process)))
    (setf (process-scheduled-run-time current-process) run-time)
    (setf (process-scheduled-run-time new-process) run-time))
  (values))

(defun make-process (function &key (name "Anonymous"))
  #!+sb-doc
  "Make a process which will run function when it starts up. The process
  may be given an optional name which defaults to Anonymous. The new
  process has a fresh set of special bindings, with *PACKAGE* set to be
  the COMMON-LISP-USER package."
  (declare (type (or null function) function))
  (cond (*quitting-lisp*
	 ;; No more processes if about to quit lisp.
	 (process-wait "Quitting Lisp" #'(lambda () nil)))
	((null function)
	 ;; If function is nil then create a dead process; can be
	 ;; restarted with process-preset.
	 (%make-process :initial-function nil :name name :state :killed))
	(t
	 ;; Create a stack-group.
	 (let ((process
		(%make-process
		 :name name
		 :state :active
		 :initial-function function
		 :stack-group
		 (make-stack-group
		  name
		  #'(lambda ()
		      (unwind-protect
			   (catch '%end-of-the-process
			     ;; Catch throws to the %END-OF-THE-WORLD.
			     (setf *quitting-lisp*
				   (catch 'sb!impl::%end-of-the-world
				     (with-simple-restart
					 (destroy "Destroy the process")
				       (setf *inhibit-scheduling* nil)
				       (funcall function))
				     ;; Normal exit.
				     (throw '%end-of-the-process nil))))
			(setf *inhibit-scheduling* t)
			;; About to return to the resumer's
			;; stack-group, which in this case is the
			;; initial process's stack-group.
			(setf (process-state *current-process*) :killed)
			(setf *all-processes*
			      (delete *current-process* *all-processes*))
			(setf (process-%whostate *current-process*) nil)
			(setf (process-wait-function *current-process*) nil)
			(setf (process-wait-timeout *current-process*) nil)
			(setf (process-wait-return-value *current-process*)
			      nil)
			(setf (process-interrupts *current-process*) nil)
			(update-process-timers *current-process*
					       *initial-process*)
			(setf *current-process* *initial-process*)))
		  *initial-stack-group* nil))))
	   (atomic-push process *all-processes*)
	   process))))

(defun process-interrupt (process function)
  #!+sb-doc
  "Interrupt process and cause it to evaluate function."
  ;; Place the interrupt function at the end of process's interrupts
  ;; queue, to be called the next time the process is scheduled.
  (without-scheduling
   (setf (process-interrupts process)
	 (append (list function) (process-interrupts process))))
  (process-yield))

(defun destroy-process (process)
  #!+sb-doc
  "Destroy a process. The process is sent a interrupt which throws to
  the end of the process allowing it to unwind gracefully."
  (declare (type process process))
  (assert (not (eq process *current-process*)))
  (without-scheduling
   (unless (eq (process-state process) :killed)
     ;; Place a throw to end-of-the-world at the start of process's
     ;; interrupts queue, to be called the next time the process is
     ;; scheduled.
     (push #'(lambda ()
	       (throw '%end-of-the-process nil))
	   (process-interrupts process))
     ;; Ensure that the process is active so that it can accept this
     ;; interrupt.
     (setf (process-state process) :active)))
  ;; Should we wait until it's dead?
  (process-yield))

(defun restart-process (process)
  #!+sb-doc
  "Restart process by unwinding it to its initial state and calling its
  initial function."
  (destroy-process process)
  (process-wait "Waiting for process to die"
		#'(lambda ()
		    (eq (process-state process) :killed)))
  ;; No more processes if about to quit lisp.
  (when *quitting-lisp*
    (process-wait "Quitting Lisp" #'(lambda () nil)))
  ;; Create a new stack-group.
  (without-scheduling
   (setf (process-stack-group process)
	 (make-stack-group
	  (process-name process)
	  #'(lambda ()
	      (unwind-protect
		   (catch '%end-of-the-process
		     ;; Catch throws to the %END-OF-THE-WORLD.
		     (setf *quitting-lisp*
			   (catch 'sb!impl::%end-of-the-world
			     (with-simple-restart
				 (destroy "Destroy the process")
			       (setf *inhibit-scheduling* nil)
			       (apply (process-initial-function process)
				      (process-initial-args process)))
			     ;; Normal exit.
			     (throw '%end-of-the-process nil))))
		(setf *inhibit-scheduling* t)
		;; About to return to the resumer's stack-group, which
		;; in this case is the initial process's stack-group.
		(setf (process-state *current-process*) :killed)
		(setf *all-processes*
		      (delete *current-process* *all-processes*))
		(setf (process-%whostate *current-process*) nil)
		(setf (process-wait-function *current-process*) nil)
		(setf (process-wait-timeout *current-process*) nil)
		(setf (process-wait-return-value *current-process*) nil)
		(setf (process-interrupts *current-process*) nil)
		(update-process-timers *current-process* *initial-process*)
		(setf *current-process* *initial-process*)))
	  *initial-stack-group* nil))
   (setf (process-%whostate process) nil)
   (setf (process-wait-function process) nil)
   (setf (process-wait-timeout process) nil)
   (setf (process-wait-return-value process) nil)
   (setf (process-interrupts process) nil)
   (setf (process-state process) :active)
   (push process *all-processes*))
  process)

(defun process-preset (process function &rest args)
  #!+sb-doc
  "Restart process, unwinding it to its initial state and calls
  function with args."
  (setf (process-initial-function process) function)
  (setf (process-initial-args process) args)
  (restart-process process))

(defun disable-process (process)
  #!+sb-doc
  "Disable process from being runnable until enabled."
  (without-scheduling
   (assert (not (eq (process-state process) :killed)))
   (setf (process-state process) :inactive)))

(defun enable-process (process)
  #!+sb-doc
  "Allow process to become runnable again after it has been disabled."
  (without-scheduling
   (assert (not (eq (process-state process) :killed)))
   (setf (process-state process) :active)))

(defun process-wait (whostate predicate)
  #!+sb-doc
  "Causes the process to wait until predicate returns True. Processes
  can only call process-wait when scheduling is enabled, and the predicate
  can not call process-wait. Since the predicate may be evaluated may
  times by the scheduler it should be relative fast native compiled code.
  The single True predicate value is returned."
  (assert (not *inhibit-scheduling*))
  (assert (not (process-wait-function *current-process*)))
  ;; Don't need the disable scheduling here because the scheduler
  ;; doesn't mess with the whostate or timeout until the function is
  ;; setup, unless the process is interrupted in which case the
  ;; scheduler restores the state when execution resumers here.
  (setf (process-%whostate *current-process*) whostate)
  (setf (process-wait-timeout *current-process*) nil)
  (setf (process-wait-function *current-process*) predicate)
  (process-yield)
  (process-wait-return-value *current-process*))

(defun process-wait-with-timeout (whostate timeout predicate)
  (declare (type (or fixnum float) timeout))
  #!+sb-doc
  "Causes the process to wait until predicate returns True, or the
  number of seconds specified by timeout has elapsed. The timeout may
  be a fixnum or a float in seconds. The single True predicate value is
  returned, or NIL if the timeout was reached."
  (assert (not *inhibit-scheduling*))
  (assert (not (process-wait-function *current-process*)))
  ;; Don't need the disable scheduling here because the scheduler
  ;; doesn't mess with the whostate or timeout until the function is
  ;; setup, unless the process is interrupted in which case the
  ;; scheduler restores the state when execution resumers here.
  (setf (process-%whostate *current-process*) whostate)
  (let ((timeout (etypecase timeout
		   (fixnum
		    (coerce timeout 'double-float))
		   (single-float
		    (coerce timeout 'double-float))
		   (double-float
		    (coerce timeout 'double-float)))))
    (declare (double-float timeout))
    (setf (process-wait-timeout *current-process*)
	  (+ timeout (get-real-time)))
    (setf (process-wait-function *current-process*) predicate))
  (process-yield)
  (process-wait-return-value *current-process*))

;;; The remaining processes in the scheduling queue for this cycle,
;;; the remainder of *all-processes*. The *current-process* is the
;;; first element of this list.
(defvar *remaining-processes* nil)

;;; The idle process will only run when there are no other runnable
;;; processes.
(defvar *idle-process* nil)

;;; Decide when to allow the idle process to run.
(defun run-idle-process-p ()
  ;; Check whether there are any other runnable processes.
  (dolist (process *all-processes* t)
    (when (and (not (eq process *idle-process*))
	       (process-active-p process)
	       (not (process-wait-function process)))
      (return nil))))

(defun shutdown-multi-processing ()
  #!+sb-doc
  "Try to gracefully destroy all the processes giving them some
  chance to unwind, before shutting down multi-processing. This is
  currently necessary before a purify and is performed before a save-lisp.
  Multi-processing can be restarted by calling init-multi-processing."
  (assert (eq *current-process* *initial-process*) ()
	  "Only the *initial-process* can shutdown multi-processing")

  (let ((destroyed-processes nil))
    (do ((cnt 0 (1+ cnt)))
	((> cnt 10))
      (declare (type sb!kernel:index cnt))
      (dolist (process *all-processes*)
	(when (and (not (eq process *current-process*))
		   (process-active-p process)
		   (not (member process destroyed-processes)))
	  (destroy-process process)
	  (push process destroyed-processes)))
      (unless (rest *all-processes*)
	(return))
      (format t "destroyed ~D process~:P; remaining ~D~%"
	      (length destroyed-processes) (length *all-processes*))
      (process-yield)))

  (start-sigalrm-yield 0 0)	; Off with the interrupts.
  ;; Reset the multi-processing state.
  (setf *inhibit-scheduling* t)
  (setf *initial-process* nil)
  (setf *idle-process* nil)
  (setf *current-process* nil)
  (setf *all-processes* nil)
  (setf *remaining-processes* nil)
  ;; Clean up the stack groups.
  (setf sb!vm::*control-stacks*
	(make-array 0 :element-type '(or null (unsigned-byte 32))
		    :initial-element nil))
  (setf *current-stack-group* nil)
  (setf *initial-stack-group* nil))

;;; A useful idle process loop, waiting on events using the select
;;; based event server, which is assumed to be setup to call
;;; process-yielding periodically.
(declaim (double-float *idle-loop-timeout*))
(defvar *idle-loop-timeout* 0.1d0)
(defun idle-process-loop ()
  #!+sb-doc
  "An idle loop to be run by the initial process. The select based event
  server is called with a timeout calculated from the minimum of the
  *idle-loop-timeout* and the time to the next process wait timeout.
  To avoid this delay when there are runnable processes the *idle-process*
  should be setup to the *initial-process*. If one of the processes quits
  by throwing to %end-of-the-world then *quitting-lisp* will have been
  set to the exit value which is noted by the idle loop which tries to
  exit gracefully destroying all the processes and giving them a chance
  to unwind."
  (declare (optimize (speed 3)))
  (assert (eq *current-process* *initial-process*) ()
	  "Only the *initial-process* is intended to run the idle loop")
  ;; Ensure the *idle-process* is setup.
  (unless *idle-process*
    (setf *idle-process* *current-process*))
  ;; Adjust the process name.
  (setf (process-name *current-process*) "Idle Loop")
  (do ()
      (*quitting-lisp*)
    ;; Calculate the wait period.
    (let ((real-time (get-real-time))
	  (timeout *idle-loop-timeout*))
      (declare (double-float timeout))
      (dolist (process *all-processes*)
	(when (process-active-p process)
	  (let ((wait-timeout (process-wait-timeout process)))
	    (when wait-timeout
	      (let ((delta (- wait-timeout real-time)))
		(when (< delta timeout)
		  (sb!vm::double-float-reg-bias timeout)
		  (setf timeout delta)))))))
      (when (> timeout 1d-5)
	(sb!sys:serve-all-events timeout))
      (process-yield)))
  (shutdown-multi-processing)
  (throw 'sb!impl::%end-of-the-world *quitting-lisp*))

;;; the scheduler
(defun process-yield ()
  (declare (optimize (speed 3)))
  #!+sb-doc
  "Allow other processes to run."
  (unless *inhibit-scheduling*
    ;; Catch any FP exceptions before entering the scheduler.
    (sb!kernel:float-wait)
    ;; Inhibit recursive entry of the scheduler.
    (setf *inhibit-scheduling* t)
    (assert (eq (first *remaining-processes*) *current-process*))
    (assert (eq *current-stack-group* (process-stack-group *current-process*)))
    (loop
     ;; Rotate the queue.
     (setf *remaining-processes*
	   (or (rest *remaining-processes*) *all-processes*))

     (let ((next (first *remaining-processes*)))
       ;; Shouldn't see any :killed porcesses here.
       (assert (process-alive-p next))

       (cond
	 ;; New process at the head of the queue?
	 ((eq next *current-process*))
	 ;; Ignore inactive processes.
	 ((not (process-active-p next)))
	 ;; If the next process has pending interrupts then return to
	 ;; it to execute these.
	 ((process-interrupts next)
	  (update-process-timers *current-process* next)
	  (setf *current-process* next)
	  (stack-group-resume (process-stack-group next)))
	 (t
	  ;; If not waiting then return.
	  (let ((wait-fn (process-wait-function next)))
	    (cond
	      ((null wait-fn)
	       ;; Skip the idle process if there are other runnable
	       ;; processes.
	       (when (or (not (eq next *idle-process*))
			 (run-idle-process-p))
		 (update-process-timers *current-process* next)
		 (setf *current-process* next)
		 (stack-group-resume (process-stack-group next))))
	      (t
	       ;; Check the wait function in the current context
	       ;; saving a stack-group switch; although
	       ;; *current-process* is setup.
	       (let ((current-process *current-process*))
		 (setf *current-process* next)
		 ;; Predicate true?
		 (let ((wait-return-value (funcall wait-fn)))
		   (cond (wait-return-value
			  ;; Flush the wait.
			  (setf (process-wait-return-value next)
				wait-return-value)
			  (setf (process-wait-timeout next) nil)
			  (setf (process-wait-function next) nil)
			  (setf (process-%whostate next) nil)
			  (update-process-timers current-process next)
			  (stack-group-resume (process-stack-group next)))
			 (t
			  ;; Timeout?
			  (let ((timeout (process-wait-timeout next)))
			    (when (and timeout (> (get-real-time) timeout))
			      ;; Flush the wait.
			      (setf (process-wait-return-value next) nil)
			      (setf (process-wait-timeout next) nil)
			      (setf (process-wait-function next) nil)
			      (setf (process-%whostate next) nil)
			      (update-process-timers current-process next)
			      (stack-group-resume
			       (process-stack-group next)))))))
		 ;; Restore the *current-process*.
		 (setf *current-process* current-process))))))))

     ;; May have just returned, or have cycled the queue.
     (let ((next (first *remaining-processes*)))
       ;; Tolerate :killed processes on the *remaining-processes* list
       ;; saving their deletion from this list when killed; will be
       ;; corrected when it cycles back to *all-processes*.
       (when (and (process-active-p next)
		  ;; Current process at the head of the queue?
		  (eq next *current-process*))
	 ;; Run any pending interrupts.
	 (let ((interrupt (pop (process-interrupts next))))
	   (declare (type (or null function) interrupt))
	   (cond (interrupt
		  ;; Save and reset any wait reasons so that the
		  ;; interrupt can wait. The return-value is also
		  ;; saved and restored in case a process is
		  ;; interrupted before it is read.
		  (let ((wait-function (process-wait-function next))
			(wait-timeout (process-wait-timeout next))
			(whostate (process-%whostate next))
			(wait-return-value (process-wait-return-value next)))
		    (setf (process-wait-function next) nil)
		    (setf (process-wait-timeout next) nil)
		    (setf (process-%whostate next) nil)
		    (setf (process-wait-return-value next) nil)
		    ;; Allow recursive scheduling during the interrupt
		    ;; processing. Only one interrupt is processed on
		    ;; each scheduler queue cycle. The process doesn't
		    ;; return until there are no interrupts.
		    (setf *inhibit-scheduling* nil)
		    (funcall interrupt)
		    (setf *inhibit-scheduling* t)
		    ;; Restore any wait reasons.
		    (setf (process-wait-function next) wait-function)
		    (setf (process-wait-timeout next) wait-timeout)
		    (setf (process-%whostate next) whostate)
		    (setf (process-wait-return-value next) wait-return-value)))
		 (t
		  ;; Check the wait function.
		  (let ((wait-fn (process-wait-function next)))
		    (cond
		      ((null wait-fn)
		       (when (or (not (eq next *idle-process*))
				 (run-idle-process-p))
			 (return)))
		      (t
		       ;; Predicate true?
		       (let ((return-value (funcall wait-fn)))
			 (when return-value
			   ;; Flush the wait.
			   (setf (process-wait-return-value next) return-value)
			   (setf (process-wait-timeout next) nil)
			   (setf (process-wait-function next) nil)
			   (setf (process-%whostate next) nil)
			   (return)))
		       ;; Timeout?
		       (let ((timeout (process-wait-timeout next)))
			 (when (and timeout (> (get-real-time) timeout))
			   ;; Flush the wait.
			   (setf (process-wait-return-value next) nil)
			   (setf (process-wait-timeout next) nil)
			   (setf (process-wait-function next) nil)
			   (setf (process-%whostate next) nil)
			   (return))))))))))))
    (setf *inhibit-scheduling* nil)))

;;; Return the real time in seconds accrued while the process was scheduled.
(defun process-real-time (process)
  #!+sb-doc
  "Return the accrued real time elapsed while the given process was
  scheduled. The returned time is a double-float in seconds."
  (declare (type process process))
  (if (eq process *current-process*)
      (without-scheduling
       (let ((real-time (get-real-time)))
	 (+ (process-%real-time process)
	    (- real-time (process-scheduled-real-time process)))))
      (process-%real-time process)))

;;; The run time in seconds accrued while the process was scheduled.
(defun process-run-time (process)
  #!+sb-doc
  "Return the accrued run time elapsed for the given process. The returned
  time is a double-float in seconds."
  (declare (type process process))
  (if (eq process *current-process*)
      (without-scheduling
       (let ((run-time (get-run-time)))
	 (+ (process-%run-time process)
	    (- run-time (process-scheduled-run-time process)))))
      (process-%run-time process)))

;;; Return the real time in seconds elapsed since the process was last
;;; de-scheduled.
(defun process-idle-time (process)
  #!+sb-doc
  "Return the real time elapsed since the given process was last
  descheduled. The returned time is a double-float in seconds."
  (declare (type process process))
  (if (eq process *current-process*)
      0
      (without-scheduling
       (let ((real-time (get-real-time)))
	 (- real-time (process-scheduled-real-time process))))))

;;; Start a regular interrupt to switch processes. This may not be a
;;; good idea yet as the SBCL code is not too interrupt safe.
(defun start-sigalrm-yield (&optional (sec 0) (usec 500000))
  #!+sb-doc
  "Start a regular SIGALRM interrupt which calls process-yield. An optional
  time in seconds and micro seconds may be provided. Note that SBCL code
  base is not too interrupt safe so this may cause problems."
  (declare (fixnum sec usec))
  ;; Disable the gencgc pointer filter to improve interrupt safety.
  #!+(and gencgc nil)
  (setf (sb!alien:extern-alien "enable_pointer_filter" sb!alien:unsigned) 0)
  (flet ((sigalrm-handler (signal info context)
	   (declare (ignore signal info context))
	   (cond ((<= sb!impl::*free-interrupt-context-index* 1)
		  #+nil (format t ".~%")
		  (process-yield))
		 (t
		  #+nil (format t "-~%")))))
    (sb!sys:enable-interrupt :sigalrm #'sigalrm-handler))
  (sb!unix:unix-setitimer :real sec usec 0 1)
  (values))

;;; Startup multi-processing, initializing the initial process. This
;;; must be called before use of the other multi-process functions.
(defun init-multi-processing ()
  (unless *initial-process*
    (init-stack-groups)
    (setf *initial-process*
	  (%make-process
	   :name "initial"
	   :state :active
	   :stack-group *initial-stack-group*))
    (setf *current-process* *initial-process*)
    (setf *all-processes* (list *initial-process*))
    (setf *remaining-processes* *all-processes*)
    #+nil (start-sigalrm-yield)
    (setf *inhibit-scheduling* nil)))

(pushnew 'init-multi-processing sb!int:*after-save-initializations*)

;;; Scrub the stored stacks of all the processes.
(defun scrub-all-processes-stacks ()
  (sb!sys:without-interrupts
   (dolist (process *all-processes*)
     (let ((stack-group (process-stack-group process)))
       (when stack-group
	 (scrub-stack-group-stacks stack-group))))))
(pushnew 'scrub-all-processes-stacks sb!ext:*before-gc-hooks*)

;;; Wait until FD is usable for DIRECTION.
(defun process-wait-until-fd-usable (fd direction &optional timeout)
  #!+sb-doc
  "Wait until FD is usable for DIRECTION and return True. DIRECTION should be
  either :INPUT or :OUTPUT. TIMEOUT, if supplied, is the number of seconds to
  wait before giving up and returning NIL."
  (declare (type sb!kernel:index fd)
	   (type (or real null) timeout)
	   (optimize (speed 3)))
  (if (or (eq *current-process* *initial-process*)
	  ;; Can't call process-wait if the scheduling is inhibited.
	  *inhibit-scheduling*)
      ;; The initial-process calls the event server to block.
      (sb!sys:wait-until-fd-usable fd direction timeout)
      ;; Other processes use process-wait.
      (flet ((fd-usable-for-input ()
	       (declare (optimize (speed 3) (safety 1)))
	       (not (eql (sb!alien:with-alien ((read-fds
					     (sb!alien:struct sb!unix:fd-set)))
			   (sb!unix:fd-zero read-fds)
			   (sb!unix:fd-set fd read-fds)
			   (sb!unix:unix-fast-select
			    (1+ fd) (sb!alien:addr read-fds) nil nil 0 0))
			 0)))
	     (fd-usable-for-output ()
	       (declare (optimize (speed 3) (safety 1)))
	       (not (eql (sb!alien:with-alien ((write-fds
					     (sb!alien:struct sb!unix:fd-set)))
			   (sb!unix:fd-zero write-fds)
			   (sb!unix:fd-set fd write-fds)
			   (sb!unix:unix-fast-select
			    (1+ fd) nil (sb!alien:addr write-fds) nil 0 0))
			 0))))

	(ecase direction
	  (:input
	   (unless (fd-usable-for-input)
	     ;; Wait until input possible.
	     (sb!sys:with-fd-handler (fd :input
				      #'(lambda (fd)
					  (declare (ignore fd)
						   (optimize (speed 3)
							     (safety 0)))
					  (sb!mp:process-yield)))
	       (if timeout
		   (sb!mp:process-wait-with-timeout "Input Wait"
						    timeout
						    #'fd-usable-for-input)
		   (sb!mp:process-wait "Input Wait" #'fd-usable-for-input)))))
	  (:output
	   (unless (fd-usable-for-output)
	     ;; Wait until output possible.
	     (sb!sys:with-fd-handler (fd :output
				      #'(lambda (fd)
					  (declare (ignore fd)
						   (optimize (speed 3)
							     (safety 0)))
					  (sb!mp:process-yield)))
	       (if timeout
		   (sb!mp:process-wait-with-timeout "Output Wait"
						    timeout
						    #'fd-usable-for-output)
		   (sb!mp:process-wait "Output Wait"
				       #'fd-usable-for-output)))))))))

;;; Redefine the sleep function to call process-wait-with-timeout,
;;; rather than blocking.
(defun sleep (n)
  #!+sb-doc
  "This function causes execution to be suspended for N seconds. N may
  be any non-negative, non-complex number."
  (when (or (not (realp n))
	    (minusp n))
    (error "Invalid argument to SLEEP: ~S.~%~
	    Must be a non-negative, non-complex number."
	   n))
  (cond ((or (eq *current-process* *initial-process*)
	     ;; Can't call process-wait if the scheduling is inhibited.
	     *inhibit-scheduling*)
	 ;; The initial-process may block.
	 (multiple-value-bind (sec usec)
	     (if (integerp n)
		 (values n 0)
		 (multiple-value-bind (sec frac) (truncate n)
		   (values sec (truncate frac 1e-6))))
	   (sb!unix:unix-select 0 0 0 0 sec usec))
	 nil)
	(t
	 (process-wait-with-timeout "Sleep" n (constantly nil)))))

(defun show-processes (&optional verbose)
  #!+sb-doc
  "Show the all the processes, their whostate, and state. If the optional
  verbose argument is true then the run, real, and idle times are also
  shown."
  (fresh-line)
  (dolist (process *all-processes*)
    (when (eq process *current-process*)
      (format t "* "))
    (format t "~S ~S ~A~%" process (process-whostate process)
	    (process-state process))
    (when verbose
      (format t "~4TRun time: ~,3f; Real time: ~,3f; Idle time: ~,3f~%"
	      (process-run-time process)
	      (process-real-time process)
	      (process-idle-time process)))))

(defun top-level ()
  #!+sb-doc
  "Top-level READ-EVAL-PRINT loop for processes."
  (let ((* nil) (** nil) (*** nil)
	(- nil) (+ nil) (++ nil) (+++ nil)
	(/// nil) (// nil) (/ nil)
	(magic-eof-cookie (cons :eof nil)))
    (loop
      (with-simple-restart (abort "Return to Top-Level.")
	(catch 'sb!impl::top-level-catcher
	  (sb!unix:unix-sigsetmask 0)
	  (let ((sb!impl::*in-top-level-catcher* t))
	    (loop
	      (sb!sys:scrub-control-stack)
	      (fresh-line)
	      (princ (if (functionp sb!int:*prompt*)
			 (funcall sb!int:*prompt*)
			 sb!int:*prompt*))
	      (force-output)
	      (let ((form (read *standard-input* nil magic-eof-cookie)))
		(cond ((not (eq form magic-eof-cookie))
		       (let ((results
			      (multiple-value-list
				  (sb!int:interactive-eval form))))
			 (dolist (result results)
			   (fresh-line)
			   (prin1 result))))
		      (t
		       (throw '%end-of-the-process nil)))))))))))

(defun startup-idle-and-top-level-loops ()
  #!+sb-doc
  "Enter the idle loop, starting a new process to run the top level loop.
  The awaking of sleeping processes is timed better with the idle loop process
  running, and starting a new process for the top level loop supports a
  simultaneous interactive session. Such an initialization will likely be the
  default when there is better MP debug support etc."
  (assert (eq *current-process* *initial-process*) ()
	  "Only the *initial-process* is intended to run the idle loop")
  (init-multi-processing)	; Initialise in case MP had been shutdown.
  ;; Start a new Top Level loop.
  (make-process #'top-level :name "top level loop")
  ;; Enter the idle loop.
  (idle-process-loop))

;;;; simple locking

(defstruct (lock (:constructor make-lock (&optional name)))
  (name nil :type (or null simple-base-string))
  (process nil :type (or null process)))
(def!method print-object ((lock lock) stream)
  (print-unreadable-object (lock stream :identity t)
    (write-string "Lock" stream)
    (let ((name (lock-name lock)))
      (when name
	(format stream " ~A" name)))
    (let ((process (lock-process lock)))
      (cond (process
	     (format stream ", held by ~S" process))
	    (t
	     (write-string ", free" stream))))))

;;; Wait for the lock to be free and acquire it for the *current-process*.
(defun lock-wait (lock whostate)
  (declare (type lock lock))
  (process-wait whostate
		#'(lambda ()
		    (declare (optimize (speed 3)))
		    #!-mp-i486
		    (unless (lock-process lock)
		      (setf (lock-process lock) *current-process*))
		    #!+mp-i486
		    (null (sb!kernel:%instance-set-conditional
			   lock 2 nil *current-process*)))))

;;; Wait with a timeout for the lock to be free and acquire it for the
;;; *current-process*.
(defun lock-wait-with-timeout (lock whostate timeout)
  (declare (type lock lock))
  (process-wait-with-timeout
   whostate timeout
   #'(lambda ()
       (declare (optimize (speed 3)))
       #!-mp-i486
       (unless (lock-process lock)
	 (setf (lock-process lock) *current-process*))
       #!+mp-i486
       (null (sb!kernel:%instance-set-conditional
	      lock 2 nil *current-process*)))))

;;; Atomically seize a lock if it's free.
#!-mp-i486
(defun seize-lock (lock)
  (declare (type lock lock)
	   (optimize (speed 3)))
  (sb!sys:without-interrupts
   (unless (lock-process lock)
     (setf (lock-process lock) *current-process*))))

(defmacro with-lock-held ((lock &optional (whostate "Lock Wait") &key timeout)
			  &body body)

  #!+sb-doc
  "Execute the body with the lock held. If the lock is held by another
  process then the current process waits until the lock is released or a
  optional timeout is reached - recursive locks are allowed. The
  optional wait timeout is a time in seconds acceptable to
  process-wait-with-timeout. The results of the body are return upon
  success and NIL is return if the timeout is reached."
  (let ((have-lock (gensym)))
    `(let ((,have-lock (eq (lock-process ,lock) *current-process*)))
      (unwind-protect
	   ,(if timeout
		`(when (cond (,have-lock)
			     #!+mp-i486 ((null (sb!kernel:%instance-set-conditional
					    ,lock 2 nil *current-process*)))
			     #!-mp-i486 ((seize-lock ,lock))
			     ((null ,timeout)
			      (lock-wait ,lock ,whostate))
			     ((lock-wait-with-timeout
			       ,lock ,whostate ,timeout)))
		  ,@body)
		`(progn
		  (unless (or ,have-lock
			      #!+mp-i486 (null (sb!kernel:%instance-set-conditional
					    ,lock 2 nil *current-process*))
			      #!-mp-i486 (seize-lock ,lock))
		    (lock-wait ,lock ,whostate))
		  ,@body))
	(unless ,have-lock
	  #!+mp-i486 (sb!kernel:%instance-set-conditional
		  ,lock 2 *current-process* nil)
	  #!-mp-i486 (when (eq (lock-process ,lock) *current-process*)
		   (setf (lock-process ,lock) nil)))))))
