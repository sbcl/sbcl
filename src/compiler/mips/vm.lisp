;;;; miscellaneous VM definition noise for MIPS

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")


;;;; Registers

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *register-names* (make-array 32 :initial-element nil)))

(macrolet ((defreg (name offset)
	       (let ((offset-sym (symbolicate name "-OFFSET")))
		 `(eval-when (:compile-toplevel :load-toplevel :execute)
		   (def!constant ,offset-sym ,offset)
		   (setf (svref *register-names* ,offset-sym) ,(symbol-name name)))))

	   (defregset (name &rest regs)
	       `(eval-when (:compile-toplevel :load-toplevel :execute)
		 (defparameter ,name
		   (list ,@(mapcar #'(lambda (name) (symbolicate name "-OFFSET")) regs))))))
  (defreg zero 0)
  (defreg nl3 1)
  (defreg cfunc 2)
  (defreg nl4 3)
  (defreg nl0 4) ; First C argument reg.
  (defreg nl1 5)
  (defreg nl2 6)
  (defreg nargs 7)
  (defreg a0 8)
  (defreg a1 9)
  (defreg a2 10)
  (defreg a3 11)
  (defreg a4 12)
  (defreg a5 13)
  (defreg fdefn 14)
  (defreg lexenv 15)
  ;; First saved reg
  (defreg nfp 16)
  (defreg ocfp 17)
  (defreg lra 18)
  (defreg l0 19)
  (defreg null 20)
  (defreg bsp 21)
  (defreg cfp 22)
  (defreg csp 23)
  (defreg l1 24)
  (defreg alloc 25)
  (defreg nsp 29)
  (defreg code 30)
  (defreg lip 31)

  (defregset non-descriptor-regs
      nl0 nl1 nl2 nl3 nl4 cfunc nargs)

  (defregset descriptor-regs
      a0 a1 a2 a3 a4 a5 fdefn lexenv nfp ocfp lra l0 l1)

  (defregset *register-arg-offsets*
      a0 a1 a2 a3 a4 a5)

  (defregset reserve-descriptor-regs
      fdefn lexenv)

  (defregset reserve-non-descriptor-regs
      nl4 cfunc))


;;;; SB and SC definition:

(define-storage-base registers :finite :size 32)
(define-storage-base float-registers :finite :size 32)
(define-storage-base control-stack :unbounded :size 8)
(define-storage-base non-descriptor-stack :unbounded :size 0)
(define-storage-base constant :non-packed)
(define-storage-base immediate-constant :non-packed)

;;;
;;; Handy macro so we don't have to keep changing all the numbers whenever
;;; we insert a new storage class.
;;; 
(defmacro !define-storage-classes (&rest classes)
  (do ((forms (list 'progn)
	      (let* ((class (car classes))
		     (sc-name (car class))
		     (constant-name (intern (concatenate 'simple-string
							 (string sc-name)
							 "-SC-NUMBER"))))
		(list* `(define-storage-class ,sc-name ,index
			  ,@(cdr class))
		       `(defconstant ,constant-name ,index)
		       `(export ',constant-name)
		       forms)))
       (index 0 (1+ index))
       (classes classes (cdr classes)))
      ((null classes)
       (nreverse forms))))

(def!constant kludge-nondeterministic-catch-block-size 7)

(!define-storage-classes

  ;; Non-immediate constants in the constant pool
  (constant constant)

  ;; Immediate constant.
  (null immediate-constant)
  (zero immediate-constant)
  (immediate immediate-constant)

  ;; **** The stacks.

  ;; The control stack.  (Scanned by GC)
  (control-stack control-stack)

  ;; The non-descriptor stacks.
  (signed-stack non-descriptor-stack) ; (signed-byte 32)
  (unsigned-stack non-descriptor-stack) ; (unsigned-byte 32)
  (character-stack non-descriptor-stack) ; non-descriptor characters.
  (sap-stack non-descriptor-stack) ; System area pointers.
  (single-stack non-descriptor-stack) ; single-floats
  (double-stack non-descriptor-stack :element-size 2) ; double floats.
  ;; complex-single-floats
  (complex-single-stack non-descriptor-stack :element-size 2)
  ;; complex-double-floats.
  (complex-double-stack non-descriptor-stack :element-size 4 :alignment 2)


  ;; **** Things that can go in the integer registers.

  ;; Immediate descriptor objects.  Don't have to be seen by GC, but nothing
  ;; bad will happen if they are.  (fixnums, characters, header values, etc).
  (any-reg
   registers
   :locations #.(append non-descriptor-regs descriptor-regs)
   :reserve-locations #.(append reserve-non-descriptor-regs
				reserve-descriptor-regs)
   :constant-scs (constant zero immediate)
   :save-p t
   :alternate-scs (control-stack))

  ;; Pointer descriptor objects.  Must be seen by GC.
  (descriptor-reg registers
   :locations #.descriptor-regs
   :reserve-locations #.reserve-descriptor-regs
   :constant-scs (constant null immediate)
   :save-p t
   :alternate-scs (control-stack))

  ;; Non-Descriptor characters
  (character-reg registers
   :locations #.non-descriptor-regs
   :reserve-locations #.reserve-non-descriptor-regs
   :constant-scs (immediate)
   :save-p t
   :alternate-scs (character-stack))

  ;; Non-Descriptor SAP's (arbitrary pointers into address space)
  (sap-reg registers
   :locations #.non-descriptor-regs
   :reserve-locations #.reserve-non-descriptor-regs
   :constant-scs (immediate)
   :save-p t
   :alternate-scs (sap-stack))

  ;; Non-Descriptor (signed or unsigned) numbers.
  (signed-reg registers
   :locations #.non-descriptor-regs
   :reserve-locations #.reserve-non-descriptor-regs
   :constant-scs (zero immediate)
   :save-p t
   :alternate-scs (signed-stack))
  (unsigned-reg registers
   :locations #.non-descriptor-regs
   :reserve-locations #.reserve-non-descriptor-regs
   :constant-scs (zero immediate)
   :save-p t
   :alternate-scs (unsigned-stack))

  ;; Random objects that must not be seen by GC.  Used only as temporaries.
  (non-descriptor-reg registers
   :locations #.non-descriptor-regs)

  ;; Pointers to the interior of objects.  Used only as an temporary.
  (interior-reg registers
   :locations (#.lip-offset))


  ;; **** Things that can go in the floating point registers.

  ;; Non-Descriptor single-floats.
  (single-reg float-registers
   :locations (0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30)
   :reserve-locations (26 28 30)
   :constant-scs ()
   :save-p t
   :alternate-scs (single-stack))

  ;; Non-Descriptor double-floats.
  (double-reg float-registers
   :locations (0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30)
   :reserve-locations (26 28 30)
   ;; Note: we don't bother with the element size, 'cause nothing can be
   ;; allocated in the odd fp regs anyway.
   :constant-scs ()
   :save-p t
   :alternate-scs (double-stack))

  (complex-single-reg float-registers
   :locations (0 4 8 12 16 20 24 28)
   :element-size 4
   :reserve-locations (24 28)
   :constant-scs ()
   :save-p t
   :alternate-scs (complex-single-stack))

  (complex-double-reg float-registers
   :locations (0 4 8 12 16 20 24 28)
   :element-size 4
   :reserve-locations (24 28)
   :constant-scs ()
   :save-p t
   :alternate-scs (complex-double-stack))

  ;; A catch or unwind block.
  (catch-block control-stack :element-size kludge-nondeterministic-catch-block-size)

  ;; floating point numbers temporarily stuck in integer registers for c-call
  (single-int-carg-reg registers
                  :locations (4 5 6 7)
                  :alternate-scs ()
                  :constant-scs ())
  (double-int-carg-reg registers
                  :locations (4 6)
                  :constant-scs ()
                  :alternate-scs ()
                  :alignment 2          ;is this needed?
                  :element-size 2))




;;;; Random TNs for interesting registers

(macrolet ((defregtn (name sc)
	       (let ((offset-sym (symbolicate name "-OFFSET"))
		     (tn-sym (symbolicate name "-TN")))
		 `(defparameter ,tn-sym
		   (make-random-tn :kind :normal
		    :sc (sc-or-lose ',sc)
		    :offset ,offset-sym)))))
  (defregtn zero any-reg)
  (defregtn lip interior-reg)
  (defregtn code descriptor-reg)
  (defregtn alloc any-reg)
  (defregtn null descriptor-reg)

  (defregtn nargs any-reg)
  (defregtn fdefn descriptor-reg)
  (defregtn lexenv descriptor-reg)

  (defregtn bsp any-reg)
  (defregtn csp any-reg)
  (defregtn cfp any-reg)
  (defregtn ocfp any-reg)
  (defregtn nsp any-reg)
  (defregtn nfp any-reg))

;;; If VALUE can be represented as an immediate constant, then return the
;;; appropriate SC number, otherwise return NIL.
(!def-vm-support-routine immediate-constant-sc (value)
  (typecase value
    ((integer 0 0)
     (sc-number-or-lose 'zero))
    (null
     (sc-number-or-lose 'null))
    (symbol
     (if (static-symbol-p value)
	 (sc-number-or-lose 'immediate)
	 nil))
    ((signed-byte 30)
     (sc-number-or-lose 'immediate))
    (system-area-pointer
     (sc-number-or-lose 'immediate))
    (character
     (sc-number-or-lose 'immediate))))


;;;; Function Call Parameters

;;; The SC numbers for register and stack arguments/return values.
;;;
(defconstant register-arg-scn (meta-sc-number-or-lose 'descriptor-reg))
(defconstant immediate-arg-scn (meta-sc-number-or-lose 'any-reg))
(defconstant control-stack-arg-scn (meta-sc-number-or-lose 'control-stack))

(eval-when (:compile-toplevel :load-toplevel :execute)

;;; Offsets of special stack frame locations
(defconstant ocfp-save-offset 0)
(defconstant lra-save-offset 1)
(defconstant nfp-save-offset 2)

;;; The number of arguments/return values passed in registers.
;;;
(defconstant register-arg-count 6)

;;; The offsets within the register-arg SC that we pass values in, first
;;; value first.
;;;

;;; Names to use for the argument registers.
;;; 
(defconstant-eqx register-arg-names '(a0 a1 a2 a3 a4 a5) #'equal)

) ; EVAL-WHEN


;;; A list of TN's describing the register arguments.
;;;
(defparameter register-arg-tns
  (mapcar #'(lambda (n)
	      (make-random-tn :kind :normal
			      :sc (sc-or-lose 'descriptor-reg)
			      :offset n))
	  *register-arg-offsets*))

;;; This is used by the debugger.
(defconstant single-value-return-byte-offset 8)

;;; This function is called by debug output routines that want a pretty name
;;; for a TN's location.  It returns a thing that can be printed with PRINC.
(!def-vm-support-routine location-print-name (tn)
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

(defun extern-alien-name (name)
  (declare (type simple-base-string name))
  name)
