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

(defconstant-eqx +fixup-kinds+ #(:absolute :layout-id) #'equalp)

;;;; register specs

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *register-names* (make-array 16 :initial-element nil)))

(macrolet ((defreg (name offset)
             (let ((offset-sym (symbolicate name "-OFFSET")))
               `(eval-when (:compile-toplevel :load-toplevel :execute)
                  (defconstant ,offset-sym ,offset)
                  (setf (svref *register-names* ,offset-sym) ,(symbol-name name))))))

  (defreg r0 0)
  (defreg r1 1)
  (defreg r2 2)
  (defreg lexenv 3)
  (defreg nl2 4)
  (defreg code 5)
  (defreg nl3 6)
  (defreg ocfp 7)
  (defreg r8 8)
  (defreg nfp 9)
  (defreg null 10)
  (defreg cfp 11)
  (defreg nargs 12)
  (defreg nsp 13)
  (defreg lr 14)
  (defreg pc 15) ;; Yes, the program counter.

  (defregset system-regs
      null cfp nsp lr pc code)

  (defregset descriptor-regs
      r0 r1 r2 lexenv r8)

  (defregset non-descriptor-regs
      ocfp nfp nargs nl2 nl3)

  (defregset boxed-regs
      r0 r1 r2 lexenv r8 code)

  ;; registers used to pass arguments
  ;;
  ;; the number of arguments/return values passed in registers
  (defconstant register-arg-count 3)
  ;; names and offsets for registers used to pass arguments
  (defregset *register-arg-offsets*  r0 r1 r2)
  (defconstant-eqx register-arg-names '(r0 r1 r2) #'equal))


;;;; SB and SC definition:

(!define-storage-bases
(define-storage-base registers :finite :size 16)
(define-storage-base control-stack :unbounded :size 2 :size-increment 1)
(define-storage-base non-descriptor-stack :unbounded :size 0)
(define-storage-base constant :non-packed)
(define-storage-base immediate-constant :non-packed)
#+arm-vfp
(define-storage-base float-registers :finite :size 32)
;; NOTE: If you fix the following, please to so with its own feature
;; conditional, and also adjust the definitions of the
;; {,COMPLEX-}{SINGLE,DOUBLE}-REG SCs below.
#-arm-vfp
(error "Don't know how many float registers for non-VFP systems")
)

(!define-storage-classes

    ;; Non-immediate contstants in the constant pool
    (constant constant)

    ;; NULL is in a register.
    (null immediate-constant)

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
                  :constant-scs (constant null immediate)
                  :save-p t
                  :alternate-scs (control-stack))

  ;; The non-descriptor stacks.
  (signed-stack non-descriptor-stack)    ; (signed-byte 32)
  (unsigned-stack non-descriptor-stack)  ; (unsigned-byte 32)
  (character-stack non-descriptor-stack) ; non-descriptor characters.
  (sap-stack non-descriptor-stack)       ; System area pointers.
  (single-stack non-descriptor-stack)    ; single-floats
  (double-stack non-descriptor-stack
                :element-size 2 :alignment 2) ; double floats.
  (complex-single-stack non-descriptor-stack :element-size 2)
  (complex-double-stack non-descriptor-stack :element-size 4 :alignment 2)

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

  ;; Pointers to the interior of objects.  Used only as a temporary.
  (interior-reg registers
                :locations (#.lr-offset))

  ;; **** Things that can go in the floating point registers.

  ;; Non-Descriptor single-floats.
  (single-reg float-registers
              :locations #.(loop for i below 32 collect i)
              :constant-scs ()
              :save-p t
              :alternate-scs (single-stack))

  ;; Non-Descriptor double-floats.
  (double-reg float-registers
              :locations #.(loop for i below 32 by 2 collect i)
              :element-size 2
              :constant-scs ()
              :save-p t
              :alternate-scs (double-stack))

  (complex-single-reg float-registers
                      :locations #.(loop for i from 0 below 32 by 2 collect i)
                      :element-size 2
                      :constant-scs ()
                      :save-p t
                      :alternate-scs (complex-single-stack))

  (complex-double-reg float-registers
                      :locations #.(loop for i from 0 below 32 by 4 collect i)
                      :element-size 4
                      :constant-scs ()
                      :save-p t
                      :alternate-scs (complex-double-stack))

  (catch-block control-stack :element-size catch-block-size)
  (unwind-block control-stack :element-size unwind-block-size))

;;;; Make some random tns for important registers.

(macrolet ((defregtn (name sc)
               (let ((offset-sym (symbolicate name "-OFFSET"))
                     (tn-sym (symbolicate name "-TN")))
                 `(defparameter ,tn-sym
                   (make-random-tn (sc-or-lose ',sc) ,offset-sym)))))

  (defregtn null descriptor-reg)
  (defregtn code descriptor-reg)

  (defregtn nargs any-reg)
  (defregtn ocfp any-reg)
  (defregtn nsp any-reg)
  (defregtn cfp any-reg)
  (defregtn lr interior-reg)
  (defregtn pc any-reg))

;;; If VALUE can be represented as an immediate constant, then return the
;;; appropriate SC number, otherwise return NIL.
(defun immediate-constant-sc (value)
  (typecase value
    (null
     null-sc-number)
    ((or (integer #.most-negative-fixnum #.most-positive-fixnum)
         character)
     immediate-sc-number)
    (symbol
     (if (static-symbol-p value)
         immediate-sc-number
         nil))
    (structure-object
     (when (eq value sb-lockless:+tail+)
       immediate-sc-number))))

(defun boxed-immediate-sc-p (sc)
  (or (eql sc null-sc-number)
      (eql sc immediate-sc-number)))

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
(define-load-time-global *register-arg-tns*
  (mapcar #'(lambda (n)
              (make-random-tn (sc-or-lose 'descriptor-reg) n))
          *register-arg-offsets*))

;;; This function is called by debug output routines that want a pretty name
;;; for a TN's location.  It returns a thing that can be printed with PRINC.
(defun location-print-name (tn)
  (declare (type tn tn))
  (let ((sb (sb-name (sc-sb (tn-sc tn))))
        (offset (tn-offset tn)))
    (ecase sb
      (registers (or (svref *register-names* offset)
                     (format nil "R~D" offset)))
      (control-stack (format nil "CS~D" offset))
      (non-descriptor-stack (format nil "NS~D" offset))
      (constant (format nil "Const~D" offset))
      (immediate-constant "Immed")
      (float-registers (format nil "F~D" offset)))))

(defun primitive-type-indirect-cell-type (ptype)
  (declare (ignore ptype))
  nil)
