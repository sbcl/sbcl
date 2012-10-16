;;;; the VM definition of function call for the ARM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; Interfaces to IR2 conversion:

;;; Return a wired TN describing the N'th full call argument passing
;;; location.
(defun standard-arg-location (n)
  (declare (type unsigned-byte n))
  (if (< n register-arg-count)
      (make-wired-tn *backend-t-primitive-type* register-arg-scn
                     (elt *register-arg-offsets* n))
      (make-wired-tn *backend-t-primitive-type* control-stack-arg-scn n)))


;;; Make a passing location TN for a local call return PC.  If
;;; standard is true, then use the standard (full call) location,
;;; otherwise use any legal location.  Even in the non-standard case,
;;; this may be restricted by a desire to use a subroutine call
;;; instruction.
(defun make-return-pc-passing-location (standard)
  (if standard
      (make-wired-tn *backend-t-primitive-type* register-arg-scn lra-offset)
      (make-restricted-tn *backend-t-primitive-type* register-arg-scn)))

;;; This is similar to MAKE-RETURN-PC-PASSING-LOCATION, but makes a
;;; location to pass OLD-FP in. This is (obviously) wired in the
;;; standard convention, but is totally unrestricted in non-standard
;;; conventions, since we can always fetch it off of the stack using
;;; the arg pointer.
(defun make-old-fp-passing-location (standard)
  (if standard
      (make-wired-tn *fixnum-primitive-type* immediate-arg-scn ocfp-offset)
      (make-normal-tn *fixnum-primitive-type*)))

;;; Make the TNs used to hold OLD-FP and RETURN-PC within the current
;;; function. We treat these specially so that the debugger can find
;;; them at a known location.
(defun make-old-fp-save-location (env)
  (specify-save-tn
   (physenv-debug-live-tn (make-normal-tn *fixnum-primitive-type*) env)
   (make-wired-tn *fixnum-primitive-type*
                  control-stack-arg-scn
                  ocfp-save-offset)))
(defun make-return-pc-save-location (env)
  (specify-save-tn
   (physenv-debug-live-tn (make-normal-tn *backend-t-primitive-type*) env)
   (make-wired-tn *backend-t-primitive-type*
                  control-stack-arg-scn
                  lra-save-offset)))

;;; Make a TN for the standard argument count passing location.  We
;;; only need to make the standard location, since a count is never
;;; passed when we are using non-standard conventions.
(defun make-arg-count-location ()
  (make-wired-tn *fixnum-primitive-type* immediate-arg-scn nargs-offset))


;;; Make a TN to hold the number-stack frame pointer.  This is
;;; allocated once per component, and is component-live.
(defun make-nfp-tn ()
  (component-live-tn
   (make-wired-tn *fixnum-primitive-type* immediate-arg-scn nfp-offset)))

(defun make-stack-pointer-tn ()
  (make-normal-tn *fixnum-primitive-type*))

(defun make-number-stack-pointer-tn ()
  (make-normal-tn *fixnum-primitive-type*))

;;; Return a list of TNs that can be used to represent an unknown-values
;;; continuation within a function.
(defun make-unknown-values-locations ()
  (list (make-stack-pointer-tn)
        (make-normal-tn *fixnum-primitive-type*)))

;;; This function is called by the ENTRY-ANALYZE phase, allowing
;;; VM-dependent initialization of the IR2-COMPONENT structure.  We push
;;; placeholder entries in the Constants to leave room for additional
;;; noise in the code object header.
(defun select-component-format (component)
  (declare (type component component))
  (dotimes (i code-constants-offset)
    (vector-push-extend nil
                        (ir2-component-constants (component-info component))))
  (values))

;;;; Frame hackery:

;;; Used for setting up the Old-FP in local call.
(define-vop (current-fp)
  (:results (val :scs (any-reg)))
  (:generator 1
    (move val fp-tn)))

(define-vop (xep-allocate-frame)
  (:info start-lab copy-more-arg-follows)
  (:ignore copy-more-arg-follows)
  (:vop-var vop)
  (:temporary (:scs (any-reg)) temp)
  (:generator 1
    ;; Make sure the function is aligned, and drop a label pointing to this
    ;; function header.
    (emit-alignment n-lowtag-bits)
    (trace-table-entry trace-table-fun-prologue)
    (emit-label start-lab)
    ;; Allocate function header.
    (inst simple-fun-header-word)
    (dotimes (i (1- simple-fun-code-offset))
      (inst word 0))
    ;; Calculate the address of the code component.  This is an
    ;; exercise in excess cleverness.  First, we calculate (from our
    ;; program counter only) the address of START-LAB plus
    ;; OTHER-POINTER-LOWTAG.  The extra two words are to compensate
    ;; for the offset applied by ARM CPUs when reading the program
    ;; counter.
    (inst sub lip-tn pc-tn (- (* (+ 2 simple-fun-code-offset)
                                 n-word-bytes)
                              other-pointer-lowtag))
    ;; Next, we read the function header.
    (loadw temp lip-tn 0 other-pointer-lowtag)
    ;; And finally we use the header value (a count in words), plus
    ;; the fact that the top two bits of SIMPLE-FUN-HEADER-LOWTAG are
    ;; clear (the value is #x2A) to compute the boxed address of the
    ;; code component.
    (inst sub code-tn lip-tn (lsr temp (- 8 word-shift)))
    ;; Build our stack frames.
    (inst add sp-tn fp-tn
          (* n-word-bytes (sb-allocated-size 'control-stack)))
    (let ((nfp-tn (current-nfp-tn vop)))
      (when nfp-tn
        (error "Don't know how to allocate number stack space")))
    (trace-table-entry trace-table-normal)))


;;; This hook in the codegen pass lets us insert code before fall-thru entry
;;; points, local-call entry points, and tail-call entry points.  The default
;;; does nothing.
(defun emit-block-header (start-label trampoline-label fall-thru-p alignp)
  (declare (ignore fall-thru-p alignp))
  (when trampoline-label
    (emit-label trampoline-label))
  (emit-label start-label))


;;;; XEP hackery:

;;; We don't need to do anything special for regular functions.
;;;
(define-vop (setup-environment)
  (:info label)
  (:ignore label)
  (:generator 0
    ;; Don't bother doing anything.
    ))

;;;; Unknown values return:

;;; Return a single value using the unknown-values convention.
(define-vop (return-single)
  (:args (old-fp :scs (any-reg) :to :eval)
         (return-pc :scs (descriptor-reg))
         (value))
  (:ignore value)
  (:vop-var vop)
  (:generator 6
    (trace-table-entry trace-table-fun-epilogue)
    ;; Clear the number stack.
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (error "Don't know how to clear number stack space in RETURN-SINGLE")))
    ;; Clear the control stack, and restore the frame pointer.
    (move sp-tn fp-tn)
    (move fp-tn old-fp)
    ;; Indicate a single-valued return by clearing all of the status
    ;; flags.
    (inst msr (cpsr :f) 0)
    ;; Out of here.
    #+(or) ;; Doesn't work, can't have a negative immediate value.
    (inst add pc-tn return-pc (- 4 other-pointer-lowtag))
    (inst sub pc-tn return-pc (- other-pointer-lowtag 4))
    (trace-table-entry trace-table-normal)))
