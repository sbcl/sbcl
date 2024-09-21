;;;; miscellaneous VM definition noise for the ARM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(defconstant-eqx +fixup-kinds+ #(:absolute :cond-branch :uncond-branch :layout-id :ubfm-imms
                                 :pc-relative :pc-relative-ldr-str :ldr-str :move-wide)
  #'equalp)


;;;; register specs

(defvar *register-names* (make-array 32 :initial-element nil))

(macrolet ((defreg (name offset)
             (let ((offset-sym (symbolicate name "-OFFSET")))
               `(progn
                  (defconstant ,offset-sym ,offset)
                  (setf (svref *register-names* ,offset-sym) ,(symbol-name name)))))

           (defregset (name &rest regs)
             `(defglobal ,name
                  (list ,@(mapcar #'(lambda (name)
                                      (symbolicate name "-OFFSET")) regs)))))

  (defreg nl0 0)
  (defreg nl1 1)
  (defreg nl2 2)
  (defreg nl3 3)
  (defreg nl4 4)
  (defreg nl5 5)
  (defreg nl6 6)
  (defreg nl7 7)
  (defreg nl8 8)
  (defreg tmp 9)

  (defreg r0 10)
  (defreg r1 11)
  (defreg r2 12)
  (defreg r3 13)
  (defreg r4 14)
  (defreg r5 15)
  (defreg r6 16)
  (defreg r7 17)
  (defreg #-darwin r8 #+darwin reserved 18)
  (defreg r9 19)

  (defreg r10 20)
  (defreg #+sb-thread thread #-sb-thread r11 21)

  (defreg lexenv 22)

  (defreg nargs 23)
  (defreg nfp 24)
  (defreg ocfp 25)
  (defreg cfp 26)
  (defreg csp 27)
  (defreg cardtable 28) ; preserved across C calls
  (defreg null 29)
  (defreg lr 30)
  (defreg nsp 31)
  (defreg zr 31)

  (defregset system-regs
      null cfp nsp lr)

  (defregset descriptor-regs
      r0 r1 r2 r3 r4 r5 r6 r7 #-darwin r8 r9 r10 #-sb-thread r11 lexenv)

  ;; nl9 can't be selected by PACK as it is a freely usable temp reg
  (defregset non-descriptor-regs
      nl0 nl1 nl2 nl3 nl4 nl5 nl6 nl7 nl8 nargs nfp ocfp lr)

  (defregset boxed-regs
      r0 r1 r2 r3 r4 r5 r6
      r7 #-darwin r8 r9 r10 #-sb-thread r11 lexenv)

  ;; registers used to pass arguments
  ;;
  ;; the number of arguments/return values passed in registers
  (defconstant register-arg-count 4)
  ;; names and offsets for registers used to pass arguments
  (defregset *register-arg-offsets*  r0 r1 r2 r3)
  (defparameter *register-arg-names* '(r0 r1 r2 r3))
  (defregset *descriptor-args* r0 r1 r2 r3 r4 r5 r6 r7 #-darwin r8 r9 r10)
  (defregset *non-descriptor-args* nl0 nl1 nl2 nl3 nl4 nl5 nl6 nl7 nl8)
  (defglobal *float-regs* (loop for i below 32 collect i)))


;;;; SB and SC definition:

(!define-storage-bases
(define-storage-base registers :finite :size 32)
(define-storage-base control-stack :unbounded :size 2 :size-increment 1)
(define-storage-base non-descriptor-stack :unbounded :size 0)
(define-storage-base constant :non-packed)
(define-storage-base immediate-constant :non-packed)
(define-storage-base float-registers :finite :size 32)
)

(!define-storage-classes
  ;; Non-immediate contstants in the constant pool
  (constant constant)

  ;; Anything else that can be an immediate.
  (immediate immediate-constant)

  ;; **** The stacks.

  ;; The control stack.  (Scanned by GC)
  (control-stack control-stack)

  ;; We put ANY-REG and DESCRIPTOR-REG early so that their SC-NUMBER
  ;; is small and therefore the error trap information is smaller.
  ;; Moving them up here from their previous place down below saves
  ;; ~250K in core file size.  --njf, 2006-01-27

  ;; Immediate descriptor objects.  Don't have to be seen by GC, but nothing
  ;; bad will happen if they are.  (fixnums, characters, header values, etc).
  (any-reg
   registers
   :locations #.(append non-descriptor-regs descriptor-regs)
   :constant-scs (immediate)
   :save-p t
   :alternate-scs (control-stack))

  ;; Pointer descriptor objects.  Must be seen by GC.
  (descriptor-reg registers
                  :locations #.descriptor-regs
                  :constant-scs (constant immediate)
                  :save-p t
                  :alternate-scs (control-stack))

  (32-bit-reg registers
              :locations #.(loop for i below 32 collect i))

  ;; The non-descriptor stacks.
  (signed-stack non-descriptor-stack)    ; (signed-byte 64)
  (unsigned-stack non-descriptor-stack)  ; (unsigned-byte 64)
  (character-stack non-descriptor-stack) ; non-descriptor characters.
  (sap-stack non-descriptor-stack)       ; System area pointers.
  (single-stack non-descriptor-stack)    ; single-floats
  (double-stack non-descriptor-stack) ; double floats.
  (complex-single-stack non-descriptor-stack)
  (complex-double-stack non-descriptor-stack :element-size 2 :alignment 2)

  ;; **** Things that can go in the integer registers.

  ;; Non-Descriptor characters
  (character-reg registers
                 :locations #.non-descriptor-regs
                 :constant-scs (immediate)
                 :save-p t
                 :alternate-scs (character-stack))

  ;; Non-Descriptor SAP's (arbitrary pointers into address space)
  (sap-reg registers
           :locations #.non-descriptor-regs
           :constant-scs (immediate)
           :save-p t
           :alternate-scs (sap-stack))

  ;; Non-Descriptor (signed or unsigned) numbers.
  (signed-reg registers
              :locations #.non-descriptor-regs
              :constant-scs (immediate)
              :save-p t
              :alternate-scs (signed-stack))
  (unsigned-reg registers
                :locations #.non-descriptor-regs
                :constant-scs (immediate)
                :save-p t
                :alternate-scs (unsigned-stack))

  ;; Random objects that must not be seen by GC.  Used only as temporaries.
  (non-descriptor-reg registers
                      :locations #.non-descriptor-regs)

  ;; **** Things that can go in the floating point registers.

  (single-immediate immediate-constant)
  (double-immediate immediate-constant)

  (single-reg float-registers
              :locations #.(loop for i below 32 collect i)
              :constant-scs (single-immediate)
              :save-p t
              :alternate-scs (single-stack))
  (double-reg float-registers
              :locations #.(loop for i below 32 collect i)
              :constant-scs (double-immediate)
              :save-p t
              :alternate-scs (double-stack))

  (complex-single-reg float-registers
                      :locations #.(loop for i below 32 collect i)
                      :constant-scs ()
                      :save-p t
                      :alternate-scs (complex-single-stack))

  (complex-double-reg float-registers
                      :locations #.(loop for i below 32 collect i)
                      :constant-scs ()
                      :save-p t
                      :alternate-scs (complex-double-stack))

  (catch-block control-stack :element-size catch-block-size)
  (unwind-block control-stack :element-size unwind-block-size)
  (zero immediate-constant))

;;;; Make some random tns for important registers.

(macrolet ((defregtn (name sc)
               (let ((offset-sym (symbolicate name "-OFFSET"))
                     (tn-sym (symbolicate name "-TN")))
                 `(defglobal ,tn-sym
                   (make-random-tn :kind :normal
                    :sc (sc-or-lose ',sc)
                    :offset ,offset-sym)))))

  (defregtn null descriptor-reg)
  (defregtn lexenv descriptor-reg)
  (defregtn tmp any-reg)
  (defregtn cardtable any-reg)

  (defregtn nargs any-reg)
  (defregtn ocfp any-reg)
  (defregtn nsp any-reg)
  (defregtn zr any-reg)
  (defregtn cfp any-reg)
  (defregtn csp any-reg)
  (defregtn lr any-reg)
  #+sb-thread
  (defregtn thread any-reg))

;;; If VALUE can be represented as an immediate constant, then return the
;;; appropriate SC number, otherwise return NIL.
(defun immediate-constant-sc (value)
  (typecase value
    (null
     (values descriptor-reg-sc-number null-offset))
    ((or (integer #.most-negative-fixnum #.most-positive-fixnum)
         character)
     immediate-sc-number)
    (symbol
     (if (static-symbol-p value)
         immediate-sc-number
         nil))
    (double-float
     double-immediate-sc-number)
    (single-float
     single-immediate-sc-number)
    (structure-object
     (when (eq value sb-lockless:+tail+)
       immediate-sc-number))))

(defun boxed-immediate-sc-p (sc)
  (eql sc immediate-sc-number))

;;;; function call parameters

;;; the SC numbers for register and stack arguments/return values
(defconstant immediate-arg-scn any-reg-sc-number)
(defconstant control-stack-arg-scn control-stack-sc-number)

;;; offsets of special stack frame locations
(defconstant ocfp-save-offset 0)
(defconstant lra-save-offset 1)
(defconstant nfp-save-offset 2)

;;; This is used by the debugger.
;;; < nyef> Ah, right. So, SINGLE-VALUE-RETURN-BYTE-OFFSET doesn't apply to x86oids or ARM.
(defconstant single-value-return-byte-offset 0)


;;; A list of TN's describing the register arguments.
;;;
(defparameter *register-arg-tns*
  (mapcar #'(lambda (n)
              (make-random-tn :kind :normal
                              :sc (sc-or-lose 'descriptor-reg)
                              :offset n))
          *register-arg-offsets*))

;;; This function is called by debug output routines that want a pretty name
;;; for a TN's location.  It returns a thing that can be printed with PRINC.
(defun location-print-name (tn)
  (declare (type tn tn))
  (let ((sb (sb-name (sc-sb (tn-sc tn))))
        (offset (tn-offset tn)))
    (ecase sb
      (registers (format nil "~:[~;W~]~A"
                         (sc-is tn 32-bit-reg)
                         (svref *register-names* offset)))
      (control-stack (format nil "CS~D" offset))
      (non-descriptor-stack (format nil "NS~D" offset))
      (constant (format nil "Const~D" offset))
      (immediate-constant "Immed")
      (float-registers
       (format nil "~A~D"
               (sc-case tn
                 (single-reg "S")
                 ((double-reg complex-single-reg) "D")
                 (complex-double-reg "Q"))
               offset)))))

(defun primitive-type-indirect-cell-type (ptype)
  (declare (ignore ptype))
  nil)

(defun 32-bit-reg (tn)
  (make-random-tn :kind :normal
                  :sc (sc-or-lose '32-bit-reg)
                  :offset (tn-offset tn)))

;;; null-tn will be used for setting it, just check the lowtag
#+sb-thread
(defconstant pseudo-atomic-flag
    (ash list-pointer-lowtag #+little-endian 0 #+big-endian 32))
#+sb-thread
(defconstant pseudo-atomic-interrupted-flag
    (ash list-pointer-lowtag #+little-endian 32 #+big-endian 0))

#+sb-xc-host
(setq *backend-cross-foldable-predicates*
      '(char-immediate-p
        abs-add-sub-immediate-p
        fixnum-abs-add-sub-immediate-p
        sb-arm64-asm::add-sub-immediate-p
        sb-arm64-asm::fixnum-add-sub-immediate-p
        sb-arm64-asm::encode-logical-immediate
        sb-arm64-asm::fixnum-encode-logical-immediate
        fixnum-encode-logical-immediate-ignore-tag
        bic-encode-immediate
        bic-fixnum-encode-immediate
        logical-immediate-or-word-mask
        sb-arm64-asm::ldr-str-offset-encodable
        power-of-two-p))
