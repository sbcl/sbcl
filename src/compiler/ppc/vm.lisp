;;;; miscellaneous VM definition noise for the PPC

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(defconstant-eqx +fixup-kinds+ #(:absolute :layout-id :b :ba :ha :l) #'equalp)

;;; NUMBER-STACK-DISPLACEMENT
;;;
;;; The number of bytes reserved above the number stack pointer.  These
;;; slots are required by architecture, mostly (?) to make C backtrace
;;; work. This must be a power of 2 - see BYTES-REQUIRED-FOR-NUMBER-STACK.
;;;
(defconstant number-stack-displacement
  (* #-darwin 2
     #+darwin 8
     n-word-bytes))

;;;; Define the registers

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *register-names* (make-array 32 :initial-element nil)))

(macrolet ((defreg (name offset)
               (let ((offset-sym (symbolicate name "-OFFSET")))
                 `(eval-when (:compile-toplevel :load-toplevel :execute)
                   (defconstant ,offset-sym ,offset)
                   (setf (svref *register-names* ,offset-sym) ,(symbol-name name))))))

  (defreg zero 0)
  (defreg nsp 1)
  (defreg rtoc 2)                         ; May be "NULL" someday.
  (defreg nl0 3)
  (defreg nl1 4)
  (defreg nl2 5)
  (defreg nl3 6)
  (defreg nl4 7)
  (defreg nl5 8)
  (defreg nl6 9)
  (defreg fdefn 10)
  (defreg nargs 11)
  ;; FIXME: some kind of comment here would be nice.
  ;;
  ;; FIXME II: this also reveals the need to autogenerate lispregs.h
  #+darwin  (defreg cfunc 12)
  #-darwin  (defreg nfp 12)
  #+darwin  (defreg nfp 13)
  #-darwin  (defreg cfunc 13)
  (defreg bsp 14)
  (defreg cfp 15)
  (defreg csp 16)
  (defreg thread 17)
  (defreg null 18)
  (defreg code 19)
  (defreg cname 20)
  (defreg lexenv 21)
  (defreg ocfp 22)
  (defreg lra 23)
  (defreg a0 24)
  (defreg a1 25)
  (defreg a2 26)
  (defreg a3 27)
  (defreg l0 28)
  (defreg l1 29)
  (defreg l2 30)
  (defreg lip 31)

  (defregset non-descriptor-regs
      nl0 nl1 nl2 nl3 nl4 nl5 nl6 cfunc nargs nfp)

  (defregset descriptor-regs
      fdefn a0 a1 a2 a3  ocfp lra cname lexenv l0 l1 l2)

  (defregset boxed-regs
      fdefn code cname lexenv ocfp lra
      a0 a1 a2 a3
      l0 l1 l2)


 (defregset *register-arg-offsets*  a0 a1 a2 a3)
 (defconstant-eqx register-arg-names '(a0 a1 a2 a3) #'equal))



;;;; SB and SC definition:

(!define-storage-bases
(define-storage-base registers :finite :size 32)
(define-storage-base float-registers :finite :size 32)
(define-storage-base control-stack :unbounded :size 8)
(define-storage-base non-descriptor-stack :unbounded :size 0)
(define-storage-base constant :non-packed)
(define-storage-base immediate-constant :non-packed)
)

(!define-storage-classes

  ;; Non-immediate contstants in the constant pool
  (constant constant)

  ;; ZERO and NULL are in registers.
  (zero immediate-constant)
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
   :constant-scs (zero immediate)
   :save-p t
   :alternate-scs (control-stack))

  ;; Pointer descriptor objects.  Must be seen by GC.
  (descriptor-reg registers
   :locations #.descriptor-regs
   :constant-scs (constant null immediate)
   :save-p t
   :alternate-scs (control-stack))

  ;; The non-descriptor stacks.
  (signed-stack non-descriptor-stack) ; (signed-byte 32)
  (unsigned-stack non-descriptor-stack) ; (unsigned-byte 32)
  (character-stack non-descriptor-stack) ; non-descriptor characters.
  (sap-stack non-descriptor-stack) ; System area pointers.
  (single-stack non-descriptor-stack) ; single-floats
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
   :constant-scs (zero immediate)
   :save-p t
   :alternate-scs (signed-stack))
  (unsigned-reg registers
   :locations #.non-descriptor-regs
   :constant-scs (zero immediate)
   :save-p t
   :alternate-scs (unsigned-stack))

  ;; Random objects that must not be seen by GC.  Used only as temporaries.
  (non-descriptor-reg registers
   :locations #.non-descriptor-regs)

  ;; Pointers to the interior of objects.  Used only as a temporary.
  (interior-reg registers
   :locations (#.lip-offset))


  ;; **** Things that can go in the floating point registers.

  ;; Non-Descriptor single-floats.
  (single-reg float-registers
   :locations #.(loop for i from 0 to 31 collect i)
   ;; ### Note: We really should have every location listed, but then we
   ;; would have to make load-tns work with element-sizes other than 1.
   :constant-scs ()
   :save-p t
   :alternate-scs (single-stack))

  ;; Non-Descriptor double-floats.
  (double-reg float-registers
   :locations #.(loop for i from 0 to 31 collect i)
   ;; ### Note: load-tns don't work with an element-size other than 1.
   ;; :element-size 2 :alignment 2
   :constant-scs ()
   :save-p t
   :alternate-scs (double-stack))

  (complex-single-reg float-registers
   :locations #.(loop for i from 0 to 30 by 2 collect i)
   :element-size 2
   :constant-scs ()
   :save-p t
   :alternate-scs (complex-single-stack))

  (complex-double-reg float-registers
   :locations #.(loop for i from 0 to 30 by 2 collect i)
   :element-size 2
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

  (defregtn zero any-reg)
  (defregtn lip interior-reg)
  (defregtn null descriptor-reg)
  (defregtn code descriptor-reg)
  (defregtn lra descriptor-reg)
  (defregtn lexenv descriptor-reg)

  (defregtn nargs any-reg)
  (defregtn bsp any-reg)
  (defregtn csp any-reg)
  (defregtn cfp any-reg)
  (defregtn ocfp any-reg)
  (defregtn nsp any-reg))

;;; If VALUE can be represented as an immediate constant, then return the
;;; appropriate SC number, otherwise return NIL.
(defun immediate-constant-sc (value)
  (typecase value
    ((integer 0 0)
     zero-sc-number)
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
  (or (eql sc zero-sc-number)
      (eql sc null-sc-number)
      (eql sc immediate-sc-number)))

;;;; function call parameters

;;; the SC numbers for register and stack arguments/return values
(defconstant immediate-arg-scn any-reg-sc-number)
(defconstant control-stack-arg-scn control-stack-sc-number)

(eval-when (:compile-toplevel :load-toplevel :execute)

;;; offsets of special stack frame locations
(defconstant ocfp-save-offset 0)
(defconstant lra-save-offset 1)
(defconstant nfp-save-offset 2)

;;; the number of arguments/return values passed in registers
(defconstant register-arg-count 4)

;;; names to use for the argument registers


) ; EVAL-WHEN


;;; A list of TN's describing the register arguments.
;;;
(define-load-time-global *register-arg-tns*
  (mapcar #'(lambda (n)
              (make-random-tn (sc-or-lose 'descriptor-reg) n))
          *register-arg-offsets*))

(defparameter thread-base-tn
  (make-random-tn (sc-or-lose 'unsigned-reg) thread-offset))

(export 'single-value-return-byte-offset)

;;; This is used by the debugger.
(defconstant single-value-return-byte-offset 8)

;;; This function is called by debug output routines that want a pretty name
;;; for a TN's location.  It returns a thing that can be printed with PRINC.
(defun location-print-name (tn)
  (declare (type tn tn))
  (let ((sb (sb-name (sc-sb (tn-sc tn))))
        (offset (tn-offset tn)))
    (ecase sb
      (registers (or (svref *register-names* offset)
                     (format nil "R~D" offset)))
      (float-registers (format nil "F~D" offset))
      (control-stack (format nil "CS~D" offset))
      (non-descriptor-stack (format nil "NS~D" offset))
      (constant (format nil "Const~D" offset))
      (immediate-constant "Immed"))))

(defun primitive-type-indirect-cell-type (ptype)
  (declare (ignore ptype))
  nil)
