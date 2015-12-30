;;; the instruction set definition for the Alpha

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!ALPHA-ASM")

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Imports from this package into SB-VM
  (import '(reg-tn-encoding) 'sb!vm)
  ;; Imports from SB-VM into this package
  (import '(sb!vm::zero sb!vm::fp-single-zero sb!vm::fp-double-zero
            sb!vm::registers sb!vm::float-registers
            sb!vm::zero-tn sb!vm::fp-single-zero-tn sb!vm::fp-double-zero-tn
            sb!vm::zero-offset sb!vm::null-offset sb!vm::code-offset)))

(!begin-instruction-definitions)

(setf *disassem-inst-alignment-bytes* 4)


;;;; utility functions

(defun reg-tn-encoding (tn)
  (declare (type tn tn)
           (values (unsigned-byte 5)))
  (sc-case tn
    (zero zero-offset)
    (null null-offset)
    (t
     (aver (eq (sb-name (sc-sb (tn-sc tn))) 'registers))
     (tn-offset tn))))

(defun fp-reg-tn-encoding (tn)
  (declare (type tn tn))
  (sc-case tn
    (fp-single-zero (tn-offset fp-single-zero-tn))
    (fp-double-zero (tn-offset fp-double-zero-tn))
    (t
     (unless (eq (sb-name (sc-sb (tn-sc tn))) 'float-registers)
       (error "~S isn't a floating-point register." tn))
     (tn-offset tn))))

;;;; initial disassembler setup

(defvar *disassem-use-lisp-reg-names* t)

(defparameter reg-symbols
  (map 'vector
       (lambda (name)
         (cond ((null name) nil)
               (t (make-symbol (concatenate 'string "$" name)))))
       sb!vm::*register-names*))

(define-arg-type reg
  :printer (lambda (value stream dstate)
             (declare (stream stream) (fixnum value))
             (let ((regname (aref reg-symbols value)))
               (princ regname stream)
               (maybe-note-associated-storage-ref
                value
                'registers
                regname
                dstate))))

(define-arg-type memory-address-annotation
  :printer (lambda (value stream dstate)
             (declare (ignore stream))
             (destructuring-bind (reg offset) value
               (cond
                 ((= reg code-offset)
                  (note-code-constant offset dstate))
                 ((= reg null-offset)
                  (maybe-note-nil-indexed-object offset dstate))))))

(defparameter float-reg-symbols
  #.(coerce
     (loop for n from 0 to 31 collect (make-symbol (format nil "~D" n)))
     'vector))

(define-arg-type fp-reg
  :printer (lambda (value stream dstate)
             (declare (stream stream) (fixnum value))
             (let ((regname (aref float-reg-symbols value)))
               (princ regname stream)
               (maybe-note-associated-storage-ref
                value
                'float-registers
                regname
                dstate))))

(define-arg-type relative-label
  :sign-extend t
  :use-label (lambda (value dstate)
               (declare (type (signed-byte 21) value)
                        (type disassem-state dstate))
               (+ 4 (ash value 2) (dstate-cur-addr dstate))))

;; We use CALL-PAL BUGCHK as part of our trap logic.  It is invariably
;; followed by a trap-code word, which we pick out with the
;; semi-traditional prefilter approach.
(define-arg-type bugchk-trap-code
    :prefilter (lambda (value dstate)
                 (declare (ignore value))
                 (read-suffix 32 dstate)))

;;;; DEFINE-INSTRUCTION-FORMATs for the disassembler

(define-instruction-format (memory 32
                            :default-printer '(:name :tab ra "," disp "(" rb ")"
                                               memory-address-annotation))
  (op   :field (byte 6 26))
  (ra   :field (byte 5 21) :type 'reg)
  (rb   :field (byte 5 16) :type 'reg)
  (disp :field (byte 16 0) :sign-extend t)
  (memory-address-annotation :fields (list (byte 5 16) (byte 16 0))
                             :type 'memory-address-annotation))

(define-instruction-format (jump 32
                            :default-printer '(:name :tab ra ",(" rb ")," hint))
  (op    :field (byte 6 26))
  (ra    :field (byte 5 21) :type 'reg)
  (rb    :field (byte 5 16) :type 'reg)
  (subop :field (byte 2 14))
  (hint  :field (byte 14 0)))

(define-instruction-format (branch 32
                            :default-printer '(:name :tab ra "," disp))
  (op   :field (byte 6 26))
  (ra   :field (byte 5 21) :type 'reg)
  (disp :field (byte 21 0) :type 'relative-label))

(define-instruction-format (reg-operate 32
                            :default-printer '(:name :tab ra "," rb "," rc))
  (op  :field (byte 6 26))
  (ra  :field (byte 5 21) :type 'reg)
  (rb  :field (byte 5 16) :type 'reg)
  (sbz :field (byte 3 13))
  (f   :field (byte 1 12) :value 0)
  (fn  :field (byte 7 5))
  (rc  :field (byte 5 0) :type 'reg))

(define-instruction-format (lit-operate 32
                            :default-printer '(:name :tab ra "," lit "," rc))
  (op  :field (byte 6 26))
  (ra  :field (byte 5 21) :type 'reg)
  (lit :field (byte 8 13))
  (f   :field (byte 1 12) :value 1)
  (fn  :field (byte 7 5))
  (rc  :field (byte 5 0) :type 'reg))

(define-instruction-format (fp-operate 32
                            :default-printer '(:name :tab fa "," fb "," fc))
  (op :field (byte 6 26))
  (fa :field (byte 5 21) :type 'fp-reg)
  (fb :field (byte 5 16) :type 'fp-reg)
  (fn :field (byte 11 5))
  (fc :field (byte 5 0) :type 'fp-reg))

(define-instruction-format (call-pal 32
                            :default-printer '('call_pal :tab 'pal_ :name))
  (op      :field (byte 6 26) :value 0)
  (palcode :field (byte 26 0)))

(define-instruction-format (bugchk 32
                            :default-printer '('call_pal :tab 'pal_bugchk "," code))
  (op      :field (byte 6 26) :value 0)
  (palcode :field (byte 26 0) :value #x81)
  (code :type 'bugchk-trap-code :reader bugchk-trap-code))

;;;; emitters

(define-bitfield-emitter emit-word 16
  (byte 16 0))

(define-bitfield-emitter emit-lword 32
  (byte 32 0))

(define-bitfield-emitter emit-qword 64
  (byte 64 0))

(define-bitfield-emitter emit-memory 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 16 0))

(define-bitfield-emitter emit-branch 32
  (byte 6 26) (byte 5 21) (byte 21 0))

(define-bitfield-emitter emit-reg-operate 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 3 13) (byte 1 12) (byte 7 5)
  (byte 5 0))

(define-bitfield-emitter emit-lit-operate 32
  (byte 6 26) (byte 5 21) (byte 8 13) (byte 1 12) (byte 7 5) (byte 5 0))

(define-bitfield-emitter emit-fp-operate 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 11 5) (byte 5 0))

(define-bitfield-emitter emit-pal 32
  (byte 6 26) (byte 26 0))

;;;; macros for instructions

(macrolet ((define-memory (name op &optional fixup float)
             `(define-instruction ,name (segment ra disp rb ,@(if fixup
                                                                  '(&optional type)))
                (:declare (type tn ra rb)
                          ,@(if fixup    ; ### unsigned-byte 16 bad idea?
                                '((type (or (unsigned-byte 16) (signed-byte 16) fixup)
                                        disp))
                              '((type (or (unsigned-byte 16) (signed-byte 16)) disp))))
                (:printer memory ((op ,op))
                          ,@(when fixup
                              ;; Don't try to parse a constant
                              ;; reference if we're doing LDA or LDAH
                              ;; against $CODE.
                              '('(:name :tab ra "," disp "(" rb ")"))))
                (:emitter
                 ,@(when fixup
                     `((when (fixup-p disp)
                         (note-fixup segment (or type ,fixup) disp)
                         (setf disp 0))))
                 (emit-memory segment ,op ,@(if float
                                                '((fp-reg-tn-encoding ra))
                                              '((reg-tn-encoding ra)))
                              (reg-tn-encoding rb)
                              disp)))))
  (define-memory lda   #x08 :lda)
  (define-memory ldah  #x09 :ldah)
  (define-memory ldbu  #x0a)            ; BWX extension
  (define-memory ldwu  #x0c)            ; BWX extension
  (define-memory ldl   #x28)
  (define-memory ldq   #x29)
  (define-memory ldl_l #x2a)
  (define-memory ldq_q #x2b)
  (define-memory ldq_u #x0b)
  (define-memory stw   #x0d)            ; BWX extension
  (define-memory stb   #x0e)            ; BWX extension
  (define-memory stl   #x2c)
  (define-memory stq   #x2d)
  (define-memory stl_c #x2e)
  (define-memory stq_c #x2f)
  (define-memory stq_u #x0f)
  (define-memory ldf   #x20 nil t)
  (define-memory ldg   #x21 nil t)
  (define-memory lds   #x22 nil t)
  (define-memory ldt   #x23 nil t)
  (define-memory stf   #x24 nil t)
  (define-memory stg   #x25 nil t)
  (define-memory sts   #x26 nil t)
  (define-memory stt   #x27 nil t))

(macrolet ((define-jump (name subop)
             `(define-instruction ,name (segment ra rb &optional (hint 0))
                (:declare (type tn ra rb)
                          (type (or (unsigned-byte 14) fixup) hint))
                (:printer jump ((op #x1a) (subop ,subop)))
                (:emitter
                 (when (fixup-p hint)
                   (note-fixup segment :jmp-hint hint)
                   (setf hint 0))
                 (emit-memory segment #x1a (reg-tn-encoding ra) (reg-tn-encoding rb)
                              (logior (ash ,subop 14) hint))))))
  (define-jump jmp 0)
  (define-jump jsr 1)
  (define-jump ret 2)
  (define-jump jsr-coroutine 3))


(macrolet ((define-branch (name op &optional (float nil))
             `(define-instruction ,name (segment ra target)
                (:declare (type tn ra)
                          (type label target))
                (:printer branch ((op ,op)
                                  ,@(when float
                                      '((ra nil :type 'fp-reg)))))
                (:emitter
                 (emit-back-patch segment 4
                                  (lambda (segment posn)
                                    (emit-branch segment ,op
                                                 ,@(if float
                                                       '((fp-reg-tn-encoding ra))
                                                       '((reg-tn-encoding ra)))
                                                 (ash (- (label-position target)
                                                         (+ posn 4))
                                                      -2))))))))
  (define-branch br   #x30)
  (define-branch bsr  #x34)
  (define-branch blbc #x38)
  (define-branch blbs #x3c)
  (define-branch fbeq #x31 t)
  (define-branch fbne #x35 t)
  (define-branch beq  #x39)
  (define-branch bne  #x3d)
  (define-branch fblt #x32 t)
  (define-branch fbge #x36 t)
  (define-branch blt  #x3a)
  (define-branch bge  #x3e)
  (define-branch fble #x33 t)
  (define-branch fbgt #x37 t)
  (define-branch ble  #x3b)
  (define-branch bgt  #x3f))

(macrolet ((define-operate (name op fn)
             `(define-instruction ,name (segment ra rb rc)
                (:declare (type tn ra rc)
                          (type (or tn (unsigned-byte 8)) rb))
                (:printer reg-operate ((op ,op) (fn ,fn)))
                (:printer lit-operate ((op ,op) (fn ,fn)))
                ,@(when (and (= op #x11) (= fn #x20))
                    `((:printer reg-operate ((op ,op) (fn ,fn) (ra 31))
                                '('move :tab rb "," rc))
                      (:printer reg-operate ((op ,op) (fn ,fn) (ra 31) (rb 31) (rc 31))
                                '('nop))))
                (:emitter
                 (etypecase rb
                   (tn
                    (emit-reg-operate segment ,op (reg-tn-encoding ra)
                                      (reg-tn-encoding rb) 0 0 ,fn (reg-tn-encoding rc)))
                   (number
                    (emit-lit-operate segment ,op (reg-tn-encoding ra) rb 1 ,fn
                                      (reg-tn-encoding rc))))))))
  (define-operate addl   #x10 #x00)
  (define-operate addl/v #x10 #x40)
  (define-operate addq   #x10 #x20)
  (define-operate addq/v #x10 #x60)
  (define-operate cmpule #x10 #x3d)
  (define-operate cmpbge #x10 #x0f)
  (define-operate subl   #x10 #x09)
  (define-operate subl/v #x10 #x49)
  (define-operate subq   #x10 #x29)
  (define-operate subq/v #x10 #x69)
  (define-operate cmpeq  #x10 #x2d)
  (define-operate cmplt  #x10 #x4d)
  (define-operate cmple  #x10 #x6d)
  (define-operate cmpult #x10 #x1d)
  (define-operate s4addl #x10 #x02)
  (define-operate s4addq #x10 #x22)
  (define-operate s4subl #x10 #x0b)
  (define-operate s4subq #x10 #x2b)
  (define-operate s8addl #x10 #x12)
  (define-operate s8addq #x10 #x32)
  (define-operate s8subl #x10 #x1b)
  (define-operate s8subq #x10 #x3b)

  (define-operate and     #x11 #x00)
  (define-operate bic     #x11 #x08)
  (define-operate cmoveq  #x11 #x24)
  (define-operate cmovne  #x11 #x26)
  (define-operate cmovlbs #x11 #x14)
  (define-operate bis     #x11 #x20)
  (define-operate ornot   #x11 #x28)
  (define-operate cmovlt  #x11 #x44)
  (define-operate cmovge  #x11 #x46)
  (define-operate cmovlbc #x11 #x16)
  (define-operate xor     #x11 #x40)
  (define-operate eqv     #x11 #x48)
  (define-operate cmovle  #x11 #x64)
  (define-operate cmovgt  #x11 #x66)

  (define-operate sll    #x12 #x39)
  (define-operate extbl  #x12 #x06)
  (define-operate extwl  #x12 #x16)
  (define-operate extll  #x12 #x26)
  (define-operate extql  #x12 #x36)
  (define-operate extwh  #x12 #x5a)
  (define-operate extlh  #x12 #x6a)
  (define-operate extqh  #x12 #x7a)
  (define-operate sra    #x12 #x3c)
  (define-operate insbl  #x12 #x0b)
  (define-operate inswl  #x12 #x1b)
  (define-operate insll  #x12 #x2b)
  (define-operate insql  #x12 #x3b)
  (define-operate inswh  #x12 #x57)
  (define-operate inslh  #x12 #x67)
  (define-operate insqh  #x12 #x77)
  (define-operate srl    #x12 #x34)
  (define-operate mskbl  #x12 #x02)
  (define-operate mskwl  #x12 #x12)
  (define-operate mskll  #x12 #x22)
  (define-operate mskql  #x12 #x32)
  (define-operate mskwh  #x12 #x52)
  (define-operate msklh  #x12 #x62)
  (define-operate mskqh  #x12 #x72)
  (define-operate zap    #x12 #x30)
  (define-operate zapnot #x12 #x31)

  (define-operate mull   #x13 #x00)
  (define-operate mulq/v #x13 #x60)
  (define-operate mull/v #x13 #x40)
  (define-operate umulh  #x13 #x30)
  (define-operate mulq   #x13 #x20)

  (define-operate ctpop  #x1c #x30)     ; CIX extension
  (define-operate ctlz   #x1c #x32)     ; CIX extension
  (define-operate cttz   #x1c #x33))    ; CIX extension


(macrolet ((define-fp-operate (name op fn &optional (args 3))
             `(define-instruction ,name (segment ,@(when (= args 3) '(fa)) fb fc)
                (:declare (type tn ,@(when (= args 3) '(fa)) fb fc))
                (:printer fp-operate ((op ,op) (fn ,fn) ,@(when (= args 2) '((fa 31))))
                          ,@(when (= args 2)
                              '('(:name :tab fb "," fc))))
                ,@(when (and (= op #x17) (= fn #x20))
                    `((:printer fp-operate ((op ,op) (fn ,fn) (fa 31))
                                '('fabs :tab fb "," fc))))
                (:emitter
                 (emit-fp-operate segment ,op ,@(if (= args 3)
                                                    '((fp-reg-tn-encoding fa))
                                                  '(31))
                                  (fp-reg-tn-encoding fb) ,fn (fp-reg-tn-encoding fc))))))
  (define-fp-operate cpys     #x17 #x020)
  (define-fp-operate mf_fpcr  #x17 #x025)
  (define-fp-operate cpysn    #x17 #x021)
  (define-fp-operate mt_fpcr  #x17 #x024)
  (define-fp-operate cpyse    #x17 #x022)
  (define-fp-operate cvtql/sv #x17 #x530 2)
  (define-fp-operate cvtlq    #x17 #x010 2)
  (define-fp-operate cvtql    #x17 #x030 2)
  (define-fp-operate cvtql/v  #x17 #x130 2)
  (define-fp-operate fcmoveq  #x17 #x02a)
  (define-fp-operate fcmovne  #x17 #x02b)
  (define-fp-operate fcmovlt  #x17 #x02c)
  (define-fp-operate fcmovge  #x17 #x02d)
  (define-fp-operate fcmovle  #x17 #x02e)
  (define-fp-operate fcmovgt  #x17 #x02f)

  (define-fp-operate cvtqs #x16 #x0bc 2)
  (define-fp-operate cvtqt #x16 #x0be 2)
  (define-fp-operate cvtts #x16 #x0ac 2)
  (define-fp-operate cvttq #x16 #x0af 2)
  (define-fp-operate cvttq/c #x16 #x02f 2)
  (define-fp-operate cmpteq #x16 #x5a5)
  (define-fp-operate cmptlt #x16 #x5a6)
  (define-fp-operate cmptle #x16 #x5a7)
  (define-fp-operate cmptun #x16 #x5a4)
  (define-fp-operate adds #x16 #x080)
  (define-fp-operate addt #x16 #x0a0)
  (define-fp-operate divs #x16 #x083)
  (define-fp-operate divt #x16 #x0a3)
  (define-fp-operate muls #x16 #x082)
  (define-fp-operate mult #x16 #x0a2)
  (define-fp-operate subs #x16 #x081)
  (define-fp-operate subt #x16 #x0a1)

;;; IEEE support
  (def!constant +su+   #x500)           ; software, underflow enabled
  (def!constant +sui+  #x700)           ; software, inexact & underflow enabled
  (def!constant +sv+   #x500)           ; software, interger overflow enabled
  (def!constant +svi+  #x700)
  (def!constant +rnd+  #x0c0)           ; dynamic rounding mode
  (def!constant +sud+  #x5c0)
  (def!constant +svid+ #x7c0)
  (def!constant +suid+ #x7c0)

  (define-fp-operate cvtqs_su #x16 (logior +su+ #x0bc) 2)
  (define-fp-operate cvtqs_sui #x16 (logior +sui+ #x0bc) 2)
  (define-fp-operate cvtqt_su #x16 (logior +su+ #x0be) 2)
  (define-fp-operate cvtqt_sui #x16 (logior +sui+ #x0be) 2)
  (define-fp-operate cvtts_su #x16 (logior +su+ #x0ac) 2)

  (define-fp-operate cvttq_sv #x16 (logior +su+ #x0af) 2)
  (define-fp-operate cvttq/c_sv #x16 (logior +su+ #x02f) 2)

  (define-fp-operate adds_su #x16 (logior +su+ #x080))
  (define-fp-operate addt_su #x16 (logior +su+ #x0a0))
  (define-fp-operate divs_su #x16 (logior +su+ #x083))
  (define-fp-operate divt_su #x16 (logior +su+ #x0a3))
  (define-fp-operate muls_su #x16 (logior +su+ #x082))
  (define-fp-operate mult_su #x16 (logior +su+ #x0a2))
  (define-fp-operate subs_su #x16 (logior +su+ #x081))
  (define-fp-operate subt_su #x16 (logior +su+ #x0a1)))

(define-instruction  excb (segment)
  (:emitter (emit-lword segment #x63ff0400)))

(define-instruction trapb (segment)
  (:emitter (emit-lword segment #x63ff0000)))

(define-instruction imb (segment)
  (:emitter (emit-lword segment #x00000086)))

(defun snarf-error-junk (sap offset &optional length-only)
  (let* ((length (sap-ref-8 sap offset))
         (vector (make-array length :element-type '(unsigned-byte 8))))
    (declare (type system-area-pointer sap)
             (type (unsigned-byte 8) length)
             (type (simple-array (unsigned-byte 8) (*)) vector))
    (cond (length-only
           (values 0 (1+ length) nil nil))
          (t
           (copy-ub8-from-system-area sap (1+ offset) vector 0 length)
           (collect ((sc-offsets)
                     (lengths))
             (lengths 1)                ; the length byte
             (let* ((index 0)
                    (error-number (read-var-integer vector index)))
               (lengths index)
               (loop
                 (when (>= index length)
                   (return))
                 (let ((old-index index))
                   (sc-offsets (read-var-integer vector index))
                   (lengths (- index old-index))))
               (values error-number
                       (1+ length)
                       (sc-offsets)
                       (lengths))))))))

(defun bugchk-trap-control (chunk inst stream dstate)
  (declare (ignore inst))
  (flet ((nt (x) (if stream (note x dstate))))
    (case (bugchk-trap-code chunk dstate)
      (#.halt-trap
       (nt "Halt trap"))
      (#.pending-interrupt-trap
       (nt "Pending interrupt trap"))
      (#.error-trap
       (nt "Error trap")
       (handle-break-args #'snarf-error-junk stream dstate))
      (#.cerror-trap
       (nt "Cerror trap")
       (handle-break-args #'snarf-error-junk stream dstate))
      (#.breakpoint-trap
       (nt "Breakpoint trap"))
      (#.fun-end-breakpoint-trap
       (nt "Function end breakpoint trap"))
      (#.single-step-breakpoint-trap
       (nt "Single step breakpoint trap"))
      (#.single-step-around-trap
       (nt "Single step around trap"))
      (#.single-step-before-trap
       (nt "Single step before trap")))))

(define-instruction gentrap (segment code)
  (:printer bugchk () :default
            :control #'bugchk-trap-control)
  (:emitter
   (emit-lword segment #x000081)        ;actually bugchk
   (emit-lword segment code)))

(define-instruction-macro move (src dst)
  `(inst bis zero-tn ,src ,dst))

(define-instruction-macro not (src dst)
  `(inst ornot zero-tn ,src ,dst))

(define-instruction-macro fmove (src dst)
  `(inst cpys ,src ,src ,dst))

(define-instruction-macro fabs (src dst)
  `(inst cpys fp-single-zero-tn ,src ,dst))

(define-instruction-macro fneg (src dst)
  `(inst cpysn ,src ,src ,dst))

(define-instruction-macro nop ()
  `(inst bis zero-tn zero-tn zero-tn))

(defun %li (value reg)
  (etypecase value
    ((signed-byte 16)
     (inst lda reg value zero-tn))
    ((signed-byte 32)
     (flet ((se (x n)
              (let ((x (logand x (lognot (ash -1 n)))))
                (if (logbitp (1- n) x)
                    (logior (ash -1 (1- n)) x)
                    x))))
       (let* ((value (se value 32))
              (low (ldb (byte 16 0) value))
              (tmp1 (- value (se low 16)))
              (high (ldb (byte 16 16) tmp1))
              (tmp2 (- tmp1 (se (ash high 16) 32)))
              (extra 0))
         (unless (= tmp2 0)
           (setf extra #x4000)
           (setf tmp1 (- tmp1 #x40000000))
           (setf high (ldb (byte 16 16) tmp1)))
         (inst lda reg low zero-tn)
         (unless (= extra 0)
           (inst ldah reg extra reg))
         (unless (= high 0)
           (inst ldah reg high reg)))))
    ((or (unsigned-byte 32) (signed-byte 64) (unsigned-byte 64))
     ;; Since it took NJF and CSR a good deal of puzzling to work out
     ;; (a) what a previous version of this was doing and (b) why it
     ;; was wrong:
     ;;
     ;; write VALUE = a_63 * 2^63 + a_48-62 * 2^48
     ;;               + a_47 * 2^47 + a_32-46 * 2^32
     ;;               + a_31 * 2^31 + a_16-30 * 2^16
     ;;               + a_15 * 2^15 + a_0-14
     ;;
     ;; then, because of the wonders of sign-extension and
     ;; twos-complement arithmetic modulo 2^64, if a_15 is set, LDA
     ;; (which sign-extends its argument) will add
     ;;
     ;;    (a_15 * 2^15 + a_0-14 - 65536).
     ;;
     ;; So we need to add that 65536 back on, which is what this
     ;; LOGBITP business is doing.  The same applies for bits 31 and
     ;; 47 (bit 63 is taken care of by the fact that all of this
     ;; arithmetic is mod 2^64 anyway), but we have to be careful that
     ;; we consider the altered value, not the original value.
     ;;
     ;; I think, anyway.  -- CSR, 2003-09-26
     (let* ((value1 (if (logbitp 15 value) (+ value (ash 1 16)) value))
            (value2 (if (logbitp 31 value1) (+ value1 (ash 1 32)) value1))
            (value3 (if (logbitp 47 value2) (+ value2 (ash 1 48)) value2)))
       (inst lda reg (ldb (byte 16 32) value2) zero-tn)
       ;; FIXME: Don't yet understand these conditionals.  If I'm
       ;; right, surely we can just consider the zeroness of the
       ;; particular bitfield, not the zeroness of the whole thing?
       ;; -- CSR, 2003-09-26
       (unless (= value3 0)
         (inst ldah reg (ldb (byte 16 48) value3) reg))
       (unless (and (= value2 0) (= value3 0))
         (inst sll reg 32 reg))
       (unless (= value 0)
         (inst lda reg (ldb (byte 16 0) value) reg))
       (unless (= value1 0)
         (inst ldah reg (ldb (byte 16 16) value1) reg))))
    (fixup
     (inst lda reg value zero-tn :bits-47-32)
     (inst ldah reg value reg :bits-63-48)
     (inst sll reg 32 reg)
     (inst lda reg value reg)
     (inst ldah reg value reg))))

(define-instruction-macro li (value reg)
  `(%li ,value ,reg))


;;;;

(define-instruction lword (segment lword)
  (:declare (type (or (unsigned-byte 32) (signed-byte 32)) lword))
  (:cost 0)
  (:emitter
   (emit-lword segment lword)))

(define-instruction short (segment word)
  (:declare (type (or (unsigned-byte 16) (signed-byte 16)) word))
  (:cost 0)
  (:emitter
   (emit-word segment word)))

(define-instruction byte (segment byte)
  (:declare (type (or (unsigned-byte 8) (signed-byte 8)) byte))
  (:cost 0)
  (:emitter
   (emit-byte segment byte)))

(defun emit-header-data (segment type)
  (emit-back-patch
   segment 4
   (lambda (segment posn)
     (emit-lword segment
                 (logior type
                         (ash (+ posn (component-header-length))
                              (- n-widetag-bits word-shift)))))))

(define-instruction simple-fun-header-word (segment)
  (:cost 0)
  (:emitter
   (emit-header-data segment simple-fun-header-widetag)))

(define-instruction lra-header-word (segment)
  (:cost 0)
  (:emitter
   (emit-header-data segment return-pc-header-widetag)))

(defun emit-compute-inst (segment vop dst src label temp calc)
  (declare (ignore temp))
  (emit-chooser
   ;; We emit either 12 or 4 bytes, so we maintain 8 byte alignments.
   segment 12 3
   (lambda (segment posn delta-if-after)
     (let ((delta (funcall calc label posn delta-if-after)))
       (when (<= (- (ash 1 15)) delta (1- (ash 1 15)))
         (emit-back-patch segment 4
                          (lambda (segment posn)
                            (assemble (segment vop)
                                      (inst lda dst
                                            (funcall calc label posn 0)
                                            src))))
         t)))
   (lambda (segment posn)
     (assemble (segment vop)
               (flet ((se (x n)
                          (let ((x (logand x (lognot (ash -1 n)))))
                            (if (logbitp (1- n) x)
                                (logior (ash -1 (1- n)) x)
                                x))))
                 (let* ((value (se (funcall calc label posn 0) 32))
                        (low (ldb (byte 16 0) value))
                        (tmp1 (- value (se low 16)))
                        (high (ldb (byte 16 16) tmp1))
                        (tmp2 (- tmp1 (se (ash high 16) 32)))
                        (extra 0))
                   (unless (= tmp2 0)
                     (setf extra #x4000)
                     (setf tmp1 (- tmp1 #x40000000))
                     (setf high (ldb (byte 16 16) tmp1)))
                   (inst lda dst low src)
                   (inst ldah dst extra dst)
                   (inst ldah dst high dst)))))))

;; code = lip - header - label-offset + other-pointer-tag
(define-instruction compute-code-from-lip (segment dst src label temp)
  (:declare (type tn dst src temp) (type label label))
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop dst src label temp
                      (lambda (label posn delta-if-after)
                        (- other-pointer-lowtag
                           (label-position label posn delta-if-after)
                           (component-header-length))))))

;; code = lra - other-pointer-tag - header - label-offset + other-pointer-tag
;;      = lra - (header + label-offset)
(define-instruction compute-code-from-lra (segment dst src label temp)
  (:declare (type tn dst src temp) (type label label))
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop dst src label temp
                      (lambda (label posn delta-if-after)
                        (- (+ (label-position label posn delta-if-after)
                              (component-header-length)))))))

;; lra = code + other-pointer-tag + header + label-offset - other-pointer-tag
;;     = code + header + label-offset
(define-instruction compute-lra-from-code (segment dst src label temp)
  (:declare (type tn dst src temp) (type label label))
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop dst src label temp
                      (lambda (label posn delta-if-after)
                        (+ (label-position label posn delta-if-after)
                           (component-header-length))))))
