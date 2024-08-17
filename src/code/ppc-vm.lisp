;;; This file contains the PPC specific runtime stuff.
;;;
(in-package "SB-VM")

(defun machine-type ()
  "Returns a string describing the type of the local machine."
  #-64-bit "PowerPC"
  #+64-bit "PowerPC64")

(defun return-machine-address (scp)
  (sap-int (context-lr scp)))



;;;; "Sigcontext" access functions, cut & pasted from x86-vm.lisp then
;;;; hacked for types.

(define-alien-routine ("os_context_lr_addr" context-lr-addr) (* unsigned-long)
  (context (* os-context-t)))

(defun context-lr (context)
  (declare (type (alien (* os-context-t)) context))
  (int-sap (deref (context-lr-addr context))))
;;; This is like CONTEXT-REGISTER, but returns the value of a float
;;; register. FORMAT is the type of float to return.

;;; FIXME: Whether COERCE actually knows how to make a float out of a
;;; long is another question. This stuff still needs testing.
#+nil
(define-alien-routine ("os_context_fpregister_addr" context-float-register-addr)
  (* long)
  (context (* os-context-t))
  (index int))
(defun context-float-register (context index format)
  (declare (type (alien (* os-context-t)) context))
  (error "context-float-register not working yet? ~S" (list context index format))
  #+nil
  (coerce (deref (context-float-register-addr context index)) format))
(defun %set-context-float-register (context index format new)
  (declare (type (alien (* os-context-t)) context))
  (error "%set-context-float-register not working yet? ~S" (list context index format new))
  #+nil
  (setf (deref (context-float-register-addr context index))
        (coerce new format)))

;;; Given a signal context, return the floating point modes word in
;;; the same format as returned by FLOATING-POINT-MODES.
;;;
;;; FIXME: surely this must be accessible under some other operating systems?
#+linux
(define-alien-routine ("os_context_fp_control" context-floating-point-modes)
    (unsigned 32)
  (context (* os-context-t)))


;;;; INTERNAL-ERROR-ARGS.

;;; GIVEN a (POSIX) signal context, extract the internal error
;;; arguments from the instruction stream.  This is e.g.

;;; INTERNAL-ERROR-ARGS -- interface.
;;;
;;; Given the sigcontext, extract the internal error arguments from the
;;; instruction stream.
;;;
(defun internal-error-args (context)
  (declare (type (alien (* os-context-t)) context))
  (let* ((pc (context-pc context))
         (bad-inst (sap-ref-32 pc 0))
         (op (ldb (byte 16 16) bad-inst))
         (regnum (ldb (byte 5 0) op)))
    (declare (type system-area-pointer pc))
    (cond ((= op #+64-bit
                 (logior (ash 2 10) (ash 1 5) null-offset) ;; TDI LGT,$NULL
                 #-64-bit
                 (logior (ash 3 10) (ash 6 5))) ;; twllei r0
           (let ((trap-number (ldb (byte 8 0) bad-inst)))
             (sb-kernel::decode-internal-error-args (sap+ pc 4) trap-number)))
          ((and (= (ldb (byte 6 10) op) 3) ;; twi
                (or (= regnum #.(sc+offset-offset arg-count-sc))
                    (= (ldb (byte 5 5) op) 24))) ;; :ne
           ;; Type errors are encoded as
           ;; twi 0 value-register error-code
           ;; twi :ne temp-register x
           (let ((prev (sap-ref-32 (int-sap (- (sap-int pc) 4)) 0)))
             (if (and (= (ldb (byte 5 5) op) 24) ;; is the condition :ne?
                      (= (ldb (byte 6 26) prev) 3) ;; is it twi?
                      (= (ldb (byte 5 21) prev) 0)) ;; is it non-trapping?
                 (values (ldb (byte 16 0) prev)
                         (list (make-sc+offset any-reg-sc-number
                                               (ldb (byte 5 16) prev))))
                 ;; arg-count errors are encoded as
                 ;; twi {:ne :llt :lgt} nargs arg-count
                 (values #.(error-number-or-lose 'invalid-arg-count-error)
                         '(#.arg-count-sc)))))
          (t
           (values #.(error-number-or-lose 'unknown-error) nil)))))

;;; To support linkage-space as efficiently as on x86-64, these
;;; things have to happen:
;;; * funcallable-instances must become directly callable objects
;;; * simple-fun-self, closure-fun, fin-fun must be raw addresses
;;; * LRAs should be removed
;;; For now, I'm using a hand-assembled simple-fun as simplifying wrapper
;;; since executable funinstances are not supported.
#+64-bit
(defun ensure-simplistic (function name)
  (when (simple-fun-p function)
    (return-from ensure-simplistic function))
  (binding*
      ((nraw (* 8 n-word-bytes))
       (code (sb-c:allocate-code-object nil 4 nraw))
       ;; FIXME: why does an undef FUNCTION come in as either/or? Can I settle on just one?
       (undef (or (eql function 0) (null function)))
       ;; It's nice if these blobs of code are all the same size but the funinstance case
       ;; doesn't fit entirely within NRAW bytes, so it calls an ASM routine.
       ((insts helper)
        (cond (undef
               (values
                #(#xE95FFFD8     ; LD $FDEFN,-40($LIP)     ; [debug-info] = the function name
                  #x3BF20000     ; ADDI $LIP,$NULL,x       ; UNDEFINED-TRAMP
                  #x7FE903A6     ; MTCTR $LIP
                  #x4E800420     ; BCTR
                  0 0)
                'undefined-tramp))
              ((funcallable-instance-p function)
               (values
                #(#xEABFFFD8     ; LD $LEXENV,-40($LIP)    ; [debug-info] = the funinstance
                  #x3BF20000     ; ADDI $LIP,$NULL,x       ; FUNCALLABLE-INSTANCE-SHIM
                  #x7FE903A6     ; MTCTR $LIP
                  #x4E800420     ; BCTR
                  0 0)
                'funinstance-shim))
              (t ; closure
               #(#xEABFFFD8      ; LD $LEXENV,-40($LIP)    ; [debug-info] = the closure
                 #x38000002      ; ADDI $ZERO,$ZERO,2
                 #x7E75002A      ; LDX $CODE,$LEXENV,$ZERO ; get closure's simple-fun
                 #x3BF3000A      ; ADDI $LIP,$CODE,10      ; simple-fun entrypoint
                 #x7FE903A6      ; MTCTR $LIP
                 #x4E800420))))) ; BCTR
    (with-pinned-objects (code)
      (let ((self (sap+ (code-instructions code) 16)))
        (setf (sap-ref-word self (ash -2 word-shift)) 1 ; jump table word count
              (sap-ref-word self (ash -1 word-shift)) 0 ; unused
              (sap-ref-word self 0) (logior #x600 simple-fun-widetag)
              (sap-ref-sap self 8) (sap+ self fun-pointer-lowtag))
        (let ((start (sap+ self 16)))
          (dotimes (i (length insts))
            (setf (sap-ref-32 start (ash i 2)) (aref insts i)))
          (when helper
            (let ((imm (- (sb-fasl:get-asm-routine helper) nil-value)))
              (setf (sap-ref-32 start 4) (logior (sap-ref-32 start 4) imm)))))
        ;; Store trailing data
        (let ((end (sap+ self (- nraw 16)))) ; undo 16 added above
          (setf (sap-ref-32 end -8) #x10 ; code-instructions to fun-base offset
                (sap-ref-16 end -4) (ash 1 5) ; simple-fun count
                (sap-ref-16 end -2) 8)))) ; trailer len in bytes
    (code-header-set code code-debug-info-slot (if undef name function))
    (%code-entry-point code 0)))

(defun stepper-fun (closure) (ensure-simplistic closure nil))
