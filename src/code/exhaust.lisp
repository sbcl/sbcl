;;;; detecting and handling exhaustion of fundamental system resources
;;;; (stack or heap)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;; a soft limit on stack overflow; the boundary beyond which the
;;; control stack will be considered to've overflowed
;;;
;;; When stack overflow is detected, this soft limit is to be bound to
;;; a new value closer to the hard limit (allowing some more space for
;;; error handling) around the call to ERROR.
;;;
;;; FIXME: Maybe (probably?) this should be in SB!VM. And maybe the
;;; size of the buffer zone should be set in src/compiler/cpu/parms.lisp
;;; instead of constantly 1Mb for all CPU architectures?
(defvar *stack-exhaustion-sap*
  ;; (initialized in cold init)
  )
(defun !exhaust-cold-init ()
  (let (;; initial difference between soft limit and hard limit
	(initial-slack (expt 2 20)))
    (setf *stack-exhaustion-sap*
	  (int-sap #!+stack-grows-downward (+ sb!vm:control-stack-start
					      initial-slack)
		   #!+stack-grows-upward (- sb!vm:control-stack-end
					    initial-slack)))))
  
;;; FIXME: Even though this is only called when (> SAFETY (MAX SPEED SPACE))
;;; it's still annoyingly wasteful for it to be a full function call.
;;; It should probably be a VOP calling an assembly routine or something
;;; like that.
(defun %detect-stack-exhaustion ()
  (when (#!+stack-grows-upward sap>=
	 #!+stack-grows-downward sap<=
	 (current-sp)
	 *stack-exhaustion-sap*)
    (let ((*stack-exhaustion-sap* (revised-stack-exhaustion-sap)))
      (warn "~@<ordinary control stack soft limit temporarily displaced to ~
             allow possible interactive debugging~@:>")
      (error "The system control stack was exhausted."))))

;;; Return a revised value for the *STACK-EXHAUSTION-SAP* soft limit,
;;; allocating half the remaining space up to the hard limit in order
;;; to allow interactive debugging to be used around the point of a
;;; stack overflow failure without immediately failing again from the
;;; (continuing) stack overflow.
(defun revised-stack-exhaustion-sap ()
  (let* ((old-slack
	  #!+stack-grows-upward (- sb!vm:control-stack-end
				   (sap-int *stack-exhaustion-sap*))
	  #!+stack-grows-downward (- (sap-int *stack-exhaustion-sap*)
				     sb!vm:control-stack-start))
	 (new-slack (ash old-slack -1)))
    (int-sap
     #!+stack-grows-upward (- sb!vm:control-stack-end new-slack)
     #!+stack-grows-downward (+ sb!vm:control-stack-start new-slack))))
