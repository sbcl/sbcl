;;;; function call for the x86 VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(defun make-return-pc-passing-location (standard)
  (if standard
      (make-wired-tn *backend-t-primitive-type* descriptor-reg-sc-number lra-offset)
      (make-restricted-tn *backend-t-primitive-type* descriptor-reg-sc-number)))

(defun make-old-fp-passing-location ()
  (make-wired-tn *fixnum-primitive-type* immediate-arg-scn ocfp-offset))

(defconstant old-fp-passing-offset
  (make-sc+offset descriptor-reg-sc-number ocfp-offset))

(defun make-old-fp-save-location (env)
  (specify-save-tn
   (physenv-debug-live-tn (make-normal-tn *fixnum-primitive-type*) env)
   (make-wired-tn *fixnum-primitive-type* control-stack-arg-scn ocfp-save-offset)))
(defun make-return-pc-save-location (env)
  (specify-save-tn
   (physenv-debug-live-tn (make-normal-tn *backend-t-primitive-type*) env)
   (make-wired-tn *backend-t-primitive-type* control-stack-arg-scn lra-save-offset)))
(defun make-arg-count-location ()
  (make-wired-tn *fixnum-primitive-type* immediate-arg-scn nargs-offset))

(define-vop (current-fp)
  (:results (val :scs (any-reg)))
  (:generator 1))

(define-vop (ancestor-frame-ref)
  (:args (frame-pointer :scs (descriptor-reg)) (variable-home-tn))
  (:results (value :scs (descriptor-reg any-reg)))
  (:generator 4))

(define-vop (ancestor-frame-set)
  (:args (frame-pointer :scs (descriptor-reg))
         (value :scs (descriptor-reg any-reg)))
  (:results (variable-home-tn))
  (:generator 4))

(define-vop (xep-allocate-frame)
  (:info start-lab)
  (:generator 1
    (emit-label start-lab)))

(define-vop (xep-setup-sp)
  (:generator 1))

(define-vop (allocate-frame)
  (:results (res :scs (any-reg)) (nfp :scs (any-reg)))
  (:info callee)
  (:generator 2))

(define-vop (allocate-full-call-frame)
  (:info nargs)
  (:results (res :scs (any-reg)))
  (:generator 2))

(define-vop (unknown-values-receiver)
  (:results (start :scs (any-reg)) (count :scs (any-reg))))

(defun emit-block-header (start-label trampoline-label fall-thru-p align-p)
  (declare (ignore fall-thru-p alignp))
  (when trampoline-label
    (emit-label trampoline-label))
  (emit-label start-label))

(define-vop (call-local)
  (:args (fp) (nfp) (args :more t))
  (:results (values :more t))
  (:info arg-locs callee target nvals)
  (:generator 5))

(define-vop (multiple-call-local unknown-values-receiver)
  (:args (fp) (nfp) (args :more t))
  (:info save callee target)
  (:generator 20))

(define-vop (known-call-local)
  (:args (fp) (nfp) (args :more t))
  (:results (res :more t))
  (:info save callee target)
  (:generator 5))

(define-vop (known-return)
  (:args (old-fp) (return-pc) (values :more t))
  (:info val-locs)
  (:ignore val-locs vals)
  (:generator 6))

(defmacro define-full-call (name named return variable)
  (aver (not (and variable (eq return :tail))))
  `(define-vop (,name ,@(when (eql return :unknown) '(unknown-values-receiver)))
     (:args
      ,@(unless (eq return :tail) '((new-fp :scs (any-reg))))
      ,@(case named
          ((nil) '((arg-fun)))
          (:direct)
          (t '((name))))
      ,@(when (eq return :tail)
          '((ocfp) (return-pc)))
      ,@(unless variable '((args :more t :scs (descriptor-reg)))))
     ,@(when (eq return :fixed) '((:results (values :more t))))
     (:info
      ,@(unless (or variable (eq return :tail)) '(arg-locs))
      ,@(unless variable '(nargs))
      ,@(when (eq named :direct) '(fun))
      ,@(when (eq return :fixed) '(nvals))
      step-instrumenting)
     (:generator ,(+ (if named 5 0)
                     (if variable 19 1)
                     (if (eq return :tail) 0 10)
                     15
                     (if (eq return :unknown) 25 0)))))

(define-full-call call nil :fixed nil)
(define-full-call call-named t :fixed nil)
(define-full-call static-call-named :direct :fixed nil)
(define-full-call multiple-call nil :unknown nil)
(define-full-call multiple-call-named t :unknown nil)
(define-full-call static-multiple-call-named :direct :unknown nil)
(define-full-call tail-call nil :tail nil)
(define-full-call tail-call-named t :tail nil)
(define-full-call static-tail-call-named :direct :tail nil)
(define-full-call call-variable nil :fixed t)
(define-full-call multiple-call-variable nil :unknown t)

(define-vop (tail-call-variable)
  (:args
   (args-arg :scs (any-reg))
   (function-arg :scs (descriptor-reg))
   (old-fp-arg :scs (any-reg))
   (lra-arg :scs (descriptor-reg)))
  (:generator 75))

(define-vop (return-single)
  (:args (old-fp) (return-pc) (value))
  (:ignore value)
  (:generator 6))

(define-vop (return)
  (:args (old-fp) (return-pc) (values :more t))
  (:ignore values)
  (:info nvals)
  (:generator 6))

(define-vop (return-multiple)
  (:args (old-fp) (return-pc) (vals :scs (any-reg)) (nvals :scs (any-reg)))
  (:generator 13))

(define-vop (setup-closure-environment)
  (:results (closure :scs (descriptor-reg)))
  (:info label)
  (:ignore label)
  (:generator 6))

(define-vop (copy-more-arg)
  (:info fixed)
  (:generator 20))

(define-full-reffer more-arg * 0 0 (descriptor-reg any-reg) * %more-arg)

(define-vop (verify-arg-count)
  (:args (nargs :scs (any-reg)))
  (:info min max)
  (:arg-types positive-fixnum (:constant t) (:constant t))
  (:generator 3))

(define-vop (step-instrument-before-vop)
  (:generator 3))
