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
#!+(or) ;; We don't pass OLD-FP, we generate it in the save-location
        ;; directly.
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
    (inst compute-code code-tn lip-tn start-lab temp)
    ;; Build our stack frames.
    (inst add sp-tn fp-tn
          (* n-word-bytes (sb-allocated-size 'control-stack)))
    (let ((nfp-tn (current-nfp-tn vop)))
      (when nfp-tn
        (error "Don't know how to allocate number stack space")))
    (trace-table-entry trace-table-normal)))

;;; Allocate a partial frame for passing stack arguments in a full call.  Nargs
;;; is the number of arguments passed.  If no stack arguments are passed, then
;;; we don't have to do anything.
(define-vop (allocate-full-call-frame)
  (:info nargs)
  (:results (res :scs (any-reg)))
  (:generator 2
    ;; Unlike most other backends, we store the "OCFP" at frame
    ;; allocation time rather than at function-entry time, largely due
    ;; to a lack of usable registers.
    (move res sp-tn)
    (inst add sp-tn sp-tn (* (min 1 nargs) n-word-bytes))
    (storew fp-tn res ocfp-save-offset)))


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

;;;; Full call:
;;;
;;; There is something of a cross-product effect with full calls.
;;; Different versions are used depending on whether we know the
;;; number of arguments or the name of the called function, and
;;; whether we want fixed values, unknown values, or a tail call.
;;;
;;; In full call, the arguments are passed creating a partial frame on
;;; the stack top and storing stack arguments into that frame.  On
;;; entry to the callee, this partial frame is pointed to by FP.  If
;;; there are no stack arguments, we don't bother allocating a partial
;;; frame, and instead set FP to SP just before the call.

;;; This macro helps in the definition of full call VOPs by avoiding code
;;; replication in defining the cross-product VOPs.
;;;
;;; Name is the name of the VOP to define.
;;;
;;; Named is true if the first argument is a symbol whose global function
;;; definition is to be called.
;;;
;;; Return is either :Fixed, :Unknown or :Tail:
;;; -- If :Fixed, then the call is for a fixed number of values, returned in
;;;    the standard passing locations (passed as result operands).
;;; -- If :Unknown, then the result values are pushed on the stack, and the
;;;    result values are specified by the Start and Count as in the
;;;    unknown-values continuation representation.
;;; -- If :Tail, then do a tail-recursive call.  No values are returned.
;;;    The Old-Fp and Return-PC are passed as the second and third arguments.
;;;
;;; In non-tail calls, the pointer to the stack arguments is passed as the last
;;; fixed argument.  If Variable is false, then the passing locations are
;;; passed as a more arg.  Variable is true if there are a variable number of
;;; arguments passed on the stack.  Variable cannot be specified with :Tail
;;; return.  TR variable argument call is implemented separately.
;;;
;;; In tail call with fixed arguments, the passing locations are passed as a
;;; more arg, but there is no new-FP, since the arguments have been set up in
;;; the current frame.
(defmacro define-full-call (name named return variable)
  (aver (not (and variable (eq return :tail))))
  `(define-vop (,name
                ,@(when (eq return :unknown)
                    '(unknown-values-receiver)))
     (:args
      ,@(unless (eq return :tail)
          '((new-fp :scs (any-reg) :to :eval)))

      ,(if named
           '(name :target name-pass)
           '(arg-fun :target lexenv))

      ,@(when (eq return :tail)
          '((return-pc :target return-pc-pass)))

      ,@(unless variable '((args :more t :scs (descriptor-reg)))))

     ,@(when (eq return :fixed)
         '((:results (values :more t))))

     (:save-p ,(if (eq return :tail) :compute-only t))

     ,@(unless (or (eq return :tail) variable)
         '((:move-args :full-call)))

     (:vop-var vop)
     (:info ,@(unless (or variable (eq return :tail)) '(arg-locs))
            ,@(unless variable '(nargs))
            ,@(when (eq return :fixed) '(nvals))
            step-instrumenting)

     (:ignore
      ,@(unless (or variable (eq return :tail)) '(arg-locs))
      ,@(unless variable '(args)))

     (:temporary (:sc descriptor-reg
                  :offset lra-offset
                  :from (:argument 1)
                  :to :eval)
                 return-pc-pass)

     (:temporary (:sc descriptor-reg :offset lexenv-offset
                      :from (:argument ,(if (eq return :tail) 0 1))
                      :to :eval)
                 ,(if named 'name-pass 'lexenv))

     (:temporary (:scs (descriptor-reg) :from (:argument 0) :to :eval)
                 function)
     (:temporary (:sc any-reg :offset nargs-offset :to :eval)
                 nargs-pass)

     ,@(when variable
         (mapcar #'(lambda (name offset)
                     `(:temporary (:sc descriptor-reg
                                   :offset ,offset
                                   :to :eval)
                         ,name))
                 *register-arg-names* *register-arg-offsets*))
     ,@(when (eq return :fixed)
         '((:temporary (:scs (descriptor-reg) :from :eval) move-temp)))

     ,@(unless (eq return :tail)
         '((:temporary (:scs (non-descriptor-reg)) temp)
           (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)))

     (:generator ,(+ (if named 5 0)
                     (if variable 19 1)
                     (if (eq return :tail) 0 10)
                     15
                     (if (eq return :unknown) 25 0))
       (trace-table-entry trace-table-call-site)
       (let* ((cur-nfp (current-nfp-tn vop))
              ,@(unless (eq return :tail)
                  '((lra-label (gen-label))))
              (step-done-label (gen-label))
              (filler
               (remove nil
                       (list :load-nargs
                             ,@(if (eq return :tail)
                                   '((unless (location= return-pc
                                                        return-pc-pass)
                                       :load-return-pc)
                                     (when cur-nfp
                                       :frob-nfp))
                                   '(:comp-lra
                                     (when cur-nfp
                                       :frob-nfp)
                                     :load-fp))))))
         (flet ((do-next-filler ()
                  (let* ((next (pop filler))
                         (what (if (consp next) (car next) next)))
                    (ecase what
                      (:load-nargs
                       ,@(if variable
                             `((inst sub nargs-pass sp-tn new-fp)
                               ,@(let ((index -1))
                                   (mapcar #'(lambda (name)
                                               `(loadw ,name new-fp
                                                       ,(incf index)))
                                           *register-arg-names*)))
                             '((inst mov nargs-pass (fixnumize nargs)))))
                      ,@(if (eq return :tail)
                            '((:load-return-pc
                               (sc-case return-pc
                                 (descriptor-reg
                                  (inst mov return-pc-pass return-pc))
                                 (control-stack
                                  (loadw return-pc-pass fp-tn
                                         (tn-offset return-pc)))))
                              (:frob-nfp
                               (inst add nsp-tn cur-nfp
                                     (- (bytes-needed-for-non-descriptor-stack-frame)
                                        number-stack-displacement))))
                            `((:comp-lra
                               (inst compute-lra return-pc-pass lra-label))
                              (:frob-nfp
                               (store-stack-tn nfp-save cur-nfp))
                              (:load-fp
                               (move fp-tn new-fp))))
                      ((nil)))))
                (insert-step-instrumenting (callable-tn)
                  ;; Conditionally insert a conditional trap:
                  (when step-instrumenting
                    ;; Get the symbol-value of SB!IMPL::*STEPPING*
                    (load-symbol-value temp sb!impl::*stepping*)
                    (error "Don't know how to STEP-INSTRUMENT a CALL"))))


           ,@(if named
                 `((sc-case name
                     (descriptor-reg (move name-pass name))
                     (control-stack
                      (loadw name-pass fp-tn (tn-offset name))
                      (do-next-filler))
                     (constant
                      (loadw name-pass code-tn (tn-offset name)
                             other-pointer-lowtag)
                      (do-next-filler)))
                   (insert-step-instrumenting name-pass)
                   (loadw function name-pass fdefn-raw-addr-slot
                          other-pointer-lowtag)
                   (do-next-filler))
                 `((sc-case arg-fun
                     (descriptor-reg (move lexenv arg-fun))
                     (control-stack
                      (loadw lexenv fp-tn (tn-offset arg-fun))
                      (do-next-filler))
                     (constant
                      (loadw lexenv code-tn (tn-offset arg-fun)
                             other-pointer-lowtag)
                      (do-next-filler)))
                   (loadw function lexenv closure-fun-slot
                          fun-pointer-lowtag)
                   (do-next-filler)
                   (insert-step-instrumenting function)))
           (loop
             (if filler
                 (do-next-filler)
                 (return)))

           (note-this-location vop :call-site)
           (lisp-jump function))

         ,@(ecase return
             (:fixed
              '((emit-return-pc lra-label)
                (default-unknown-values vop values nvals move-temp
                                        temp lra-label)
                (when cur-nfp
                  (load-stack-tn cur-nfp nfp-save))))
             (:unknown
              '((emit-return-pc lra-label)
                (note-this-location vop :unknown-return)
                (receive-unknown-values values-start nvals start count
                                        lra-label temp)
                (when cur-nfp
                  (load-stack-tn cur-nfp nfp-save))))
             (:tail)))
       (trace-table-entry trace-table-normal))))


(define-full-call call nil :fixed nil)
(define-full-call call-named t :fixed nil)

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
    (lisp-return return-pc)
    (trace-table-entry trace-table-normal)))
