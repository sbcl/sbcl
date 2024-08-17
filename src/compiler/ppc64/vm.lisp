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

(defconstant-eqx +fixup-kinds+ #(:absolute :layout-id :b :ba :ha :l :rldic-m :addis+ld)
  #'equalp)

;;; NUMBER-STACK-DISPLACEMENT
;;;
;;; The number of bytes reserved above the number stack pointer.  These
;;; slots are required by architecture, mostly (?) to make C backtrace
;;; work. This must be a power of 2 - see BYTES-NEEDED-FOR-NON-DESCRIPTOR-STACK-FRAME.
;;;
(defconstant number-stack-displacement (* 4 n-word-bytes))

;;;; Define the registers

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *register-names* (make-array 32 :initial-element nil)))

(macrolet ((defreg (name offset)
               (let ((offset-sym (symbolicate name "-OFFSET")))
                 `(eval-when (:compile-toplevel :load-toplevel :execute)
                   (defconstant ,offset-sym ,offset)
                   (setf (svref *register-names* ,offset-sym) ,(symbol-name name)))))

           (defregset (name &rest regs)
               `(eval-when (:compile-toplevel :load-toplevel :execute)
                 (defparameter ,name
                   (list ,@(mapcar #'(lambda (name)
                                       (symbolicate name "-OFFSET")) regs))))))

  (defreg zero 0)
  (defreg nsp 1)
  (defreg rtoc 2)
  (defreg nl0 3)
  (defreg nl1 4)
  (defreg nl2 5)
  (defreg nl3 6)
  (defreg nl4 7)
  (defreg nl5 8)
  (defreg nl6 9)
  (defreg fdefn 10)
  (defreg nargs 11)
  (defreg cfunc 12)
  (defreg r13 13) ; reserved
  (defreg bsp 14)
  (defreg cfp 15)
  (defreg csp 16)
  (defreg gc-card-table 17)
  (defreg null 18)
  ;; Use of a tagged pointer in reg_CODE on PPC64 is expensive, adding an extra
  ;; instruction to each load of a boxed constant. We should allow this register
  ;; to contain an untagged pointer, and allow "fixnums" (ambiguous pointers)
  ;; to pin code just as is done on the x86 backends.  And if we implicitly pin
  ;; executing code, then we can get rid all LRA-related noise as well.
  (defreg code 19)
  (defreg nfp 20)
  (defreg lexenv 21)
  (defreg ocfp 22)
  (defreg lra 23)
  (defreg a0 24)
  (defreg a1 25)
  (defreg a2 26)
  (defreg a3 27)
  (defreg l0 28)
  (defreg l1 29)
  (defreg thread 30)
  (defreg lip 31)

  (defregset non-descriptor-regs
      nl0 nl1 nl2 nl3 nl4 nl5 nl6 cfunc nargs nfp)

  (defregset descriptor-regs
      fdefn a0 a1 a2 a3  ocfp lra lexenv l0 l1)

  (defregset boxed-regs
      fdefn code lexenv ocfp lra
      a0 a1 a2 a3
      l0 l1 thread)


 (defregset *register-arg-offsets*  a0 a1 a2 a3)
 (defparameter register-arg-names '(a0 a1 a2 a3)))



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
  (signed-stack non-descriptor-stack) ; (signed-byte 32)
  (unsigned-stack non-descriptor-stack) ; (unsigned-byte 32)
  (character-stack non-descriptor-stack) ; non-descriptor characters.
  (sap-stack non-descriptor-stack) ; System area pointers.
  (single-stack non-descriptor-stack) ; single-floats
  (double-stack non-descriptor-stack) ; double floats.
  (complex-single-stack non-descriptor-stack :element-size 2 :alignment 2)
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

  ;; Pointers to the interior of objects.  Used only as a temporary.
  (interior-reg registers
   :locations (#.lip-offset))


  ;; **** Things that can go in the floating point registers.

  ;; Non-Descriptor single-floats.
  (single-reg float-registers
   :locations #.(loop for i from 0 to 31 collect i)
   :constant-scs ()
   :save-p t
   :alternate-scs (single-stack))

  ;; Non-Descriptor double-floats.
  (double-reg float-registers
   :locations #.(loop for i from 0 to 31 collect i)
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

(defparameter thread-base-tn
  (make-random-tn :kind :normal :sc (sc-or-lose 'unsigned-reg)
                  :offset thread-offset))
(defparameter card-table-base-tn
  (make-random-tn :kind :normal :sc (sc-or-lose 'unsigned-reg)
                  :offset gc-card-table-offset))

(macrolet ((defregtn (name sc)
               (let ((offset-sym (symbolicate name "-OFFSET"))
                     (tn-sym (symbolicate name "-TN")))
                 `(defparameter ,tn-sym
                   (make-random-tn :kind :normal
                    :sc (sc-or-lose ',sc)
                    :offset ,offset-sym)))))

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
(defparameter *register-arg-tns*
  (mapcar #'(lambda (n)
              (make-random-tn :kind :normal
                              :sc (sc-or-lose 'descriptor-reg)
                              :offset n))
          *register-arg-offsets*))

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

;;; We need a permanently reserved register to hold the offset in a load
;;; or store instruction of 8 bytes.
;;; Those instructions require the displacement be a multiple of 4 bytes
;;; which precludes subtracting a lowtag in the same instruction.
;;; See ld instruction for reference.
;;; See also the comments above DEFINE-INDEXER for some thoughts on how to
;;; regain the ability to subtract lowtags "for free".
(defglobal temp-reg-tn (make-random-tn :kind :normal
                                       :sc (sc-or-lose 'unsigned-reg)
                                       :offset zero-offset))
