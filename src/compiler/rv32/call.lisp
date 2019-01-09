;;;; function call for the RV32 VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")


(defconstant arg-count-sc (make-sc+offset immediate-arg-scn nargs-offset))
(defconstant closure-sc (make-sc+offset descriptor-reg-sc-number lexenv-offset))

;;; Make a passing location TN for a local call return PC.
(defun make-return-pc-passing-location (standard)
  (declare (ignore standard))
  (make-wired-tn *backend-t-primitive-type* descriptor-reg-sc-number lra-offset))

;;; This is similar to MAKE-RETURN-PC-PASSING-LOCATION, but makes a
;;; location to pass OLD-FP in. This is (obviously) wired in the
;;; standard convention, but is totally unrestricted in non-standard
;;; conventions, since we can always fetch it off of the stack using
;;; the arg pointer.
(defun make-old-fp-passing-location ()
  (make-wired-tn *fixnum-primitive-type* immediate-arg-scn ocfp-offset))

(defconstant old-fp-passing-offset
  (make-sc+offset descriptor-reg-sc-number ocfp-offset))

;;; Make the TNs used to hold OLD-FP and RETURN-PC within the current
;;; function. We treat these specially so that the debugger can find
;;; them at a known location.
(defun make-old-fp-save-location (env)
  (specify-save-tn
   (physenv-debug-live-tn (make-normal-tn *fixnum-primitive-type*) env)
   (make-wired-tn *fixnum-primitive-type* control-stack-arg-scn ocfp-save-offset)))

(defun make-return-pc-save-location (env)
  (specify-save-tn
   (physenv-debug-live-tn (make-normal-tn *backend-t-primitive-type*) env)
   (make-wired-tn *backend-t-primitive-type* control-stack-arg-scn lra-save-offset)))

;;; Make a TN for the standard argument count passing location.  We
;;; only need to make the standard location, since a count is never
;;; passed when we are using non-standard conventions.
(defun make-arg-count-location ()
  (make-wired-tn *fixnum-primitive-type* immediate-arg-scn nargs-offset))


;;;; Frame hackery:

;;; Return the number of bytes needed for the current non-descriptor
;;; stack frame.
(defun bytes-needed-for-non-descriptor-stack-frame ()
  (* (logandc2 (1+ (sb-allocated-size 'non-descriptor-stack)) 1)
     n-word-bytes))

;;; This is used for setting up the Old-FP in local call.
(define-vop (current-fp)
  (:results (val :scs (any-reg)))
  (:generator 1
    (move val cfp-tn)))

;;; Used for computing the caller's NFP for use in known-values return.  Only
;;; works assuming there is no variable size stuff on the nstack.
(define-vop (compute-old-nfp)
  (:results (val :scs (any-reg)))
  (:vop-var vop)
  (:generator 1
    (let ((nfp (current-nfp-tn vop)))
      (when nfp
        (inst addi val nfp (bytes-needed-for-non-descriptor-stack-frame))))))

;;; Accessing a slot from an earlier stack frame is definite hackery.
(define-vop (ancestor-frame-ref)
  (:args (frame-pointer :scs (descriptor-reg))
         (variable-home-tn :load-if nil))
  (:results (value :scs (descriptor-reg any-reg)))
  (:policy :fast-safe)
  (:generator 4
    (aver (sc-is variable-home-tn control-stack))
    (loadw value frame-pointer (tn-offset variable-home-tn))))

(define-vop (ancestor-frame-set)
  (:args (frame-pointer :scs (descriptor-reg))
         (value :scs (descriptor-reg any-reg)))
  (:results (variable-home-tn :load-if nil))
  (:generator 4
    (aver (sc-is variable-home-tn control-stack))
    (storew value frame-pointer (tn-offset variable-home-tn))))

(define-vop (xep-allocate-frame)
  (:info start-lab)
  (:temporary (:scs (non-descriptor-reg) :offset nl3-offset) temp)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 1
    ;; Make sure the function is aligned, and drop a label pointing to this
    ;; function header.
    (emit-alignment n-lowtag-bits)
    (emit-label start-lab)
    ;; Allocate function header.
    (inst simple-fun-header-word)
    (inst .skip (* (1- simple-fun-code-offset) n-word-bytes))
    (inst compute-code code-tn lip start-lab temp)))

(define-vop (xep-setup-sp)
  (:vop-var vop)
  (:generator 1
    (inst addi csp-tn cfp-tn (* n-word-bytes (sb-allocated-size 'control-stack)))
    (let ((nfp (current-nfp-tn vop)))
      (when nfp
        (inst addi nsp-tn nsp-tn (- (bytes-needed-for-non-descriptor-stack-frame)))
        (move nfp nsp-tn)))))

(define-vop (allocate-frame)
  (:results (res :scs (any-reg))
            (nfp :scs (any-reg)))
  (:info callee)
  (:generator 2
    (move res csp-tn)
    (inst addi csp-tn csp-tn (* n-word-bytes (sb-allocated-size 'control-stack)))
    (when (ir2-physenv-number-stack-p callee)
      (inst addi nsp-tn nsp-tn (- (bytes-needed-for-non-descriptor-stack-frame)))
      (move nfp nsp-tn))))

;;; Allocate a partial frame for passing stack arguments in a full
;;; call.  NARGS is the number of arguments passed.  If no stack
;;; arguments are passed, then we don't have to do anything.
(define-vop (allocate-full-call-frame)
  (:info nargs)
  (:results (res :scs (any-reg)))
  (:generator 2
    (when (> nargs register-arg-count)
      (move res csp-tn)
      (inst addi csp-tn csp-tn (* nargs n-word-bytes)))))

;;; Emit code needed at the return-point from an unknown-values call
;;; for a fixed number of values.  VALUES is the head of the TN-REF
;;; list for the locations that the values are to be received into.
;;; NVALS is the number of values that are to be received (should
;;; equal the length of Values).
;;;
;;; MOVE-TEMP is a DESCRIPTOR-REG TN used as a temporary.
;;;
;;; This code exploits the fact that in the unknown-values convention,
;;; a single-value return is indicated with 0 in NL0 register and a
;;; multiple-value return is indicated with 1.
;;;
;;; If 0 or 1 values are expected, then we just emit an instruction to
;;; reset the SP (which will only be executed when other than 1 value
;;; is returned.)
;;;
;;; In the general case, we have to do three things:
;;;  -- Default unsupplied register values.  This need only be done when a
;;;     single value is returned, since register values are defaulted by the
;;;     callee in the non-single case.
;;;  -- Default unsupplied stack values.  This needs to be done whenever there
;;;     are stack values.
;;;  -- Reset SP.  This must be done whenever other than 1 value is returned,
;;;     regardless of the number of values desired.
#+nil
(defun default-unknown-values (vop values nvals move-temp temp lip lra-label)
  (declare (type (or tn-ref null) values)
           (type unsigned-byte nvals) (type tn move-temp temp))
  (let ((expecting-values-on-stack (> nvals register-arg-count))
        (values-on-stack temp))
    (note-this-location vop (if (<= nvals 1)
                                :single-value-return
                                :unknown-return))
    (inst compute-code code-tn lip lra-label temp)
    ;; Pick off the single-value case first.
    (sb-assem:without-scheduling ()

      ;; Default register values for single-value return case.
      ;; The callee returns with condition bits CLEAR in the
      ;; single-value case.
      (when values
        (do ((i 1 (1+ i))
             (val (tn-ref-across values) (tn-ref-across val)))
            ((= i (min nvals register-arg-count)))
          (inst mov :ne (tn-ref-tn val) null-tn)))

      ;; If we're not expecting values on the stack, all that
      ;; remains is to clear the stack frame (for the multiple-
      ;; value return case).
      (unless expecting-values-on-stack
        (store-csp ocfp-tn :eq))

      ;; If we ARE expecting values on the stack, we need to
      ;; either move them to their result location or to set their
      ;; result location to the default.
      (when expecting-values-on-stack

        ;; For the single-value return case, fake up NARGS and
        ;; OCFP so that we don't screw ourselves with the
        ;; defaulting and stack clearing logic.
        (load-csp ocfp-tn :ne)
        (inst mov :ne nargs-tn n-word-bytes)

        ;; Compute the number of stack values (may be negative if
        ;; not all of the register values are populated).
        (inst sub values-on-stack nargs-tn (fixnumize register-arg-count))

        ;; For each expected stack value...
        (do ((i register-arg-count (1+ i))
             (val (do ((i 0 (1+ i))
                       (val values (tn-ref-across val)))
                      ((= i register-arg-count) val))
                  (tn-ref-across val)))
            ((null val))

          ;; ... Load it if there is a stack value available, or
          ;; default it if there isn't.
          (inst subs values-on-stack values-on-stack 4)
          (loadw move-temp ocfp-tn i 0 :ge)
          (store-stack-tn (tn-ref-tn val) move-temp :ge)
          (store-stack-tn (tn-ref-tn val) null-tn :lt))

        ;; Deallocate the callee stack frame.
        (store-csp ocfp-tn))))
  (values))

;;; VOP that can be inherited by unknown values receivers.  The main
;;; thing this handles is allocation of the result temporaries.
(define-vop (unknown-values-receiver)
  (:results (start :scs (any-reg))
            (count :scs (any-reg))))

;;; This hook in the codegen pass lets us insert code before fall-thru
;;; entry points, local-call entry points, and tail-call entry points.
;;; The default does nothing.
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
  (:args (old-fp :scs (any-reg))
         (return-pc :scs (descriptor-reg))
         (values :more t))
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

;;; Defined separately, since needs special code that BLT's the
;;; arguments down.
(define-vop (tail-call-variable)
  (:args
   (args-arg :scs (any-reg) :target args)
   (function-arg :scs (descriptor-reg) :target lexenv)
   (old-fp-arg :scs (any-reg) :target old-fp)
   (lra-arg :scs (descriptor-reg) :target lra))
  (:temporary (:sc any-reg :offset nl0-offset :from (:argument 0)) args)
  (:temporary (:sc any-reg :offset lexenv-offset :from (:argument 1)) lexenv)
  (:temporary (:sc any-reg :offset ocfp-offset :from (:argument 2)) old-fp)
  (:temporary (:sc any-reg :offset lra-offset :from (:argument 3)) lra)
  (:vop-var vop)
  (:generator 75
    ;; Move these into the passing locations if they are not already there.
    (move args args-arg)
    (move lexenv function-arg)
    (move old-fp old-fp-arg)
    (move lra lra-arg)
    ;; Clear the number stack if anything is there.
    (clear-number-stack vop)
    ;; And jump to the assembly routine.
    (invoke-asm-routine 'tail-call-variable t)))


;;;; Unknown values return:

(defun clear-number-stack (vop)
  (let ((cur-nfp (current-nfp-tn vop)))
    (when cur-nfp
      (inst addi nsp-tn cur-nfp (bytes-needed-for-non-descriptor-stack-frame)))))

;;; Return a single value using the unknown-values convention.
(define-vop (return-single)
  (:args (old-fp :scs (any-reg) :to :eval)
         (return-pc :scs (descriptor-reg))
         (value))
  (:temporary (:scs (non-descriptor-reg) :offset nl0-offset) mv-p)
  (:ignore value)
  (:vop-var vop)
  (:generator 6
    ;; Clear the number stack.
    (clear-number-stack vop)
    ;; Clear the control stack, and restore the frame pointer.
    (move csp-tn cfp-tn)
    (move cfp-tn old-fp)
    ;; Out of here.
    (lisp-return return-pc mv-p :single-value)))

;;; Do unknown-values return of a fixed number of values.  The Values
;;; are required to be set up in the standard passing locations.
;;; Nvals is the number of values returned.

;;; If returning other than one value, then load the number of values
;;; returned, NIL out unsupplied values registers, restore FP and
;;; return at Return-PC with 1 in NL0.  When there
;;; are stack values, we must initialize the argument pointer to point
;;; to the beginning of the values block (which is the beginning of
;;; the current frame.)
(macrolet ((frob ()
             (let ((a (loop repeat register-arg-count
                            collect (gensym))))
               `(define-vop (return)
                  (:args (old-fp :scs (any-reg))
                         (return-pc :scs (descriptor-reg))
                         (values :more t))
                  (:ignore values)
                  ,@(loop for an-offset in *register-arg-offsets*
                          for an in a
                          collect `(:temporary (:sc descriptor-reg
                                                :offset ,an-offset
                                                :from :eval)
                                               ,an))
                  (:temporary (:sc any-reg :offset nargs-offset) nargs)
                  (:temporary (:sc any-reg :offset ocfp-offset) val-ptr)
                  (:temporary (:sc any-reg :offset nl0-offset) mv-p)
                  (:info nvals)
                  (:vop-var vop)
                  (:generator 6
                    (when (= nvals 1)
                      ;; This is handled in RETURN-SINGLE.
                      (error "nvalues is 1"))
                    ;; Clear the number stack.
                    (clear-number-stack vop)
                    ;; Establish the values pointer and values count.
                    (move val-ptr cfp-tn)
                    (inst li nargs (fixnumize nvals))
                    ;; restore the frame pointer and clear as much of
                    ;; the control stack as possible.
                    (move cfp-tn old-fp)
                    (inst addi csp-tn val-ptr (* nvals n-word-bytes))
                    ;; pre-default any argument register that need it.
                    (when (< nvals register-arg-count)
                      (dolist (reg (subseq (list ,@a) nvals))
                        (move reg null-tn)))
                    ;; And away we go.
                    (lisp-return return-pc mv-p :multiple-values))))))
  (frob))

;;; Do unknown-values return of an arbitrary number of values (passed
;;; on the stack.)  We check for the common case of a single return
;;; value, and do that inline using the normal single value return
;;; convention.  Otherwise, we branch off to code that calls an
;;; assembly-routine.
(define-vop (return-multiple)
  (:args (old-fp-arg :scs (any-reg) :target old-fp)
         (lra-arg :scs (descriptor-reg) :target lra)
         (vals-arg :scs (any-reg) :target vals)
         (nvals-arg :scs (any-reg) :target nvals))
  (:temporary (:sc any-reg :offset nl1-offset :from (:argument 0)) old-fp)
  (:temporary (:sc descriptor-reg :offset lra-offset :from (:argument 1)) lra)
  (:temporary (:sc any-reg :offset nl0-offset :from (:argument 2)) vals)
  (:temporary (:sc any-reg :offset nargs-offset :from (:argument 3)) nvals)
  (:temporary (:scs (descriptor-reg) :offset a0-offset) a0)
  (:vop-var vop)
  (:generator 13
    ;; Clear the number stack.
    (clear-number-stack vop)
    ;; Check for the single case.
    (inst li a0 (fixnumize 1))
    (inst bne nvals-arg a0 NOT-SINGLE)
    ;; Return with one value.
    (loadw a0 vals-arg)

    ;;Return with one value.
    (move csp-tn cfp-tn)
    (move cfp-tn old-fp)
    (lisp-return lra-arg vals :single-value)

    NOT-SINGLE
    (move old-fp old-fp-arg)
    (move lra lra-arg)
    (move vals vals-arg)
    (move nvals nvals-arg)

    (invoke-asm-routine 'return-multiple t)))


;;;; XEP hackery:

;;; Get the lexical environment from its passing location.
(define-vop (setup-closure-environment)
  (:temporary (:sc descriptor-reg :offset lexenv-offset :target closure
               :to (:result 0))
              lexenv)
  (:results (closure :scs (descriptor-reg)))
  (:info label)
  (:ignore label)
  (:generator 6
    ;; Get result.
    (move closure lexenv)))

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
