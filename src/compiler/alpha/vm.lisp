;;;; miscellaneous VM definition noise for the x86

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")



;;;; Define the registers

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *register-names* (make-array 32 :initial-element nil)))

(macrolet ((defreg (name offset)
             (let ((offset-sym (symbolicate name "-OFFSET")))
               `(eval-when (:compile-toplevel :load-toplevel :execute)
                  (defconstant ,offset-sym ,offset)
                  (setf (svref *register-names* ,offset-sym) ,(symbol-name name)))))
           (defregset (name &rest regs)
             `(eval-when  (:compile-toplevel :load-toplevel :execute)
                (defparameter ,name
                  (list ,@(mapcar #'(lambda (name)
                                      (symbolicate name "-OFFSET"))
                                  regs))))))
  ;; c.f. src/runtime/alpha-lispregs.h
  
  ;; Ra
  (defreg lip 0)
  ;; Caller saved 0-7
  (defreg a0 1)
  (defreg a1 2)
  (defreg a2 3)
  (defreg a3 4)
  (defreg a4 5)
  (defreg a5 6)
  (defreg l0 7)
  (defreg nargs 8)
  ;; Callee saved 0-6
  (defreg csp 9)
  (defreg cfp 10)
  (defreg ocfp 11)
  (defreg bsp 12)
  (defreg lexenv 13)
  (defreg code 14)
  (defreg null 15)
  ;; Arg 0-5
  (defreg nl0 16)
  (defreg nl1 17)
  (defreg nl2 18)
  (defreg nl3 19)
  (defreg nl4 20)
  (defreg nl5 21)
  ;; Caller saved 8-11
  (defreg alloc 22)
  (defreg fdefn 23)
  (defreg cfunc 24)
  (defreg nfp 25)
  ;; Ra
  (defreg lra 26)
  ;; Caller saved 12
  (defreg l1 27)
  ;; Assembler temp (at)
  (defreg l2 28)
  ;; Global pointer (gp)
  (defreg gp 29)
  ;; Stack pointer
  (defreg nsp 30)
  ;; Wired zero
  (defreg zero 31)
  
  (defregset non-descriptor-regs
    nl0 nl1 nl2 nl3 nl4 nl5 nfp cfunc)
  
  (defregset descriptor-regs
    fdefn lexenv nargs ocfp lra a0 a1 a2 a3 a4 a5 l0 l1 l2)
  
  (defregset *register-arg-offsets*
    a0 a1 a2 a3 a4 a5)
  (defparameter register-arg-names '(a0 a1 a2 a3 a4 a5)))

(define-storage-base registers :finite :size 32)
(define-storage-base float-registers :finite :size 64)
(define-storage-base control-stack :unbounded :size 8)
(define-storage-base non-descriptor-stack :unbounded :size 0)
(define-storage-base constant :non-packed)
(define-storage-base immediate-constant :non-packed)

;;;
;;; Handy macro so we don't have to keep changing all the numbers whenever
;;; we insert a new storage class.
;;; FIXME: This macro is not needed in the runtime target.

(defmacro define-storage-classes (&rest classes)
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

;;; see comment in ../x86/vm.lisp.  The value of 7 was taken from
;;; vm:catch-block-size in a cmucl that I happened to have around
;;; and seems to be working so far    -dan
(defconstant sb!vm::kludge-nondeterministic-catch-block-size 7)


(define-storage-classes

  ;; Non-immediate contstants in the constant pool
  (constant constant)

  ;; ZERO and NULL are in registers.
  (zero immediate-constant)
  (null immediate-constant)
  (fp-single-zero immediate-constant)
  (fp-double-zero immediate-constant)

  ;; Anything else that can be an immediate.
  (immediate immediate-constant)


  ;; **** The stacks.

  ;; The control stack.  (Scanned by GC)
  (control-stack control-stack)

  ;; The non-descriptor stacks.
  (signed-stack non-descriptor-stack
		:element-size 2 :alignment 2) ; (signed-byte 64)
  (unsigned-stack non-descriptor-stack
		  :element-size 2 :alignment 2) ; (unsigned-byte 64)
  (base-char-stack non-descriptor-stack) ; non-descriptor characters.
  (sap-stack non-descriptor-stack
	     :element-size 2 :alignment 2) ; System area pointers.
  (single-stack non-descriptor-stack) ; single-floats
  (double-stack non-descriptor-stack
		:element-size 2 :alignment 2) ; double floats.
  (complex-single-stack non-descriptor-stack :element-size 2)
  (complex-double-stack non-descriptor-stack :element-size 4 :alignment 2)


  ;; **** Things that can go in the integer registers.

  ;; Immediate descriptor objects.  Don't have to be seen by GC, but nothing
  ;; bad will happen if they are.  (fixnums, characters, header values, etc).
  (any-reg
   registers
   :locations #.(append non-descriptor-regs descriptor-regs)
;   :locations #.non-descriptor-regs
   :constant-scs (zero immediate)
   :save-p t
   :alternate-scs (control-stack))

  ;; Pointer descriptor objects.  Must be seen by GC.
  (descriptor-reg registers
                  :locations #.descriptor-regs
                  :constant-scs (constant null immediate)
                  :save-p t
                  :alternate-scs (control-stack))

  ;; Non-Descriptor characters
  (base-char-reg registers
                 :locations #.non-descriptor-regs
   :constant-scs (immediate)
   :save-p t
   :alternate-scs (base-char-stack))

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

  ;; Pointers to the interior of objects.  Used only as an temporary.
  (interior-reg registers
   :locations (#.lip-offset))


  ;; **** Things that can go in the floating point registers.

  ;; Non-Descriptor single-floats.
  (single-reg float-registers
   :locations #.(loop for i from 4 to 30 collect i)
   :constant-scs (fp-single-zero)
   :save-p t
   :alternate-scs (single-stack))

  ;; Non-Descriptor double-floats.
  (double-reg float-registers
   :locations #.(loop for i from 4 to 30 collect i)
   :constant-scs (fp-double-zero)
   :save-p t
   :alternate-scs (double-stack))

  (complex-single-reg float-registers
   :locations #.(loop for i from 4 to 28 by 2 collect i)
   :element-size 2
   :constant-scs ()
   :save-p t
   :alternate-scs (complex-single-stack))

  (complex-double-reg float-registers
   :locations #.(loop for i from 4 to 28 by 2 collect i)
   :element-size 2
   :constant-scs ()
   :save-p t
   :alternate-scs (complex-double-stack))

  ;; A catch or unwind block.
  (catch-block control-stack
               :element-size sb!vm::kludge-nondeterministic-catch-block-size))


;;;; Make some random tns for important registers.

(macrolet ((defregtn (name sc)
             (let ((offset-sym (symbolicate name "-OFFSET"))
                   (tn-sym (symbolicate name "-TN")))
               `(defparameter ,tn-sym
                  (make-random-tn :kind :normal
		       :sc (sc-or-lose ',sc)
		       :offset ,offset-sym)))))

  ;; These, we access by foo-TN only

  (defregtn zero any-reg)
  (defregtn null descriptor-reg)
  (defregtn code descriptor-reg)
  (defregtn alloc any-reg)
  (defregtn bsp any-reg)
  (defregtn csp any-reg)
  (defregtn cfp any-reg)
  (defregtn nsp any-reg)

  ;; These alias regular locations, so we have to make sure we don't bypass
  ;; the register allocator when using them.
  (defregtn nargs any-reg)
  (defregtn ocfp any-reg)
  (defregtn lip interior-reg))

;; And some floating point values.
(defparameter fp-single-zero-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'single-reg)
		  :offset 31))
(defparameter fp-double-zero-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'double-reg)
		  :offset 31))


;;; Immediate-Constant-SC  --  Interface
;;;
;;; If value can be represented as an immediate constant, then return the
;;; appropriate SC number, otherwise return NIL.
;;;
(!def-vm-support-routine immediate-constant-sc (value)
  (typecase value
    ((integer 0 0)
     (sc-number-or-lose 'zero))
    (null
     (sc-number-or-lose 'null ))
    ((or fixnum system-area-pointer character)
     (sc-number-or-lose 'immediate ))
    (symbol
     (if (static-symbol-p value)
	 (sc-number-or-lose 'immediate )
	 nil))
    (single-float
     (if (eql value 0f0)
	 (sc-number-or-lose 'fp-single-zero )
	 nil))
    (double-float
     (if (eql value 0d0)
	 (sc-number-or-lose 'fp-double-zero )
	 nil))))

;;;; Function Call Parameters

;;; The SC numbers for register and stack arguments/return values.
;;;
(defconstant register-arg-scn (meta-sc-number-or-lose 'descriptor-reg))
(defconstant immediate-arg-scn (meta-sc-number-or-lose 'any-reg))
(defconstant control-stack-arg-scn (meta-sc-number-or-lose 'control-stack))

(eval-when  (:compile-toplevel :load-toplevel :execute)

;;; Offsets of special stack frame locations
(defconstant ocfp-save-offset 0)
(defconstant lra-save-offset 1)
(defconstant nfp-save-offset 2)

;;; The number of arguments/return values passed in registers.
;;;
(defconstant register-arg-count 6)

;;; Names to use for the argument registers.
;;; 


); Eval-When (Compile Load Eval)


;;; A list of TN's describing the register arguments.
;;;
(defparameter *register-arg-tns*
  (mapcar #'(lambda (n)
	      (make-random-tn :kind :normal
			      :sc (sc-or-lose 'descriptor-reg)
			      :offset n))
	  *register-arg-offsets*))

;;; SINGLE-VALUE-RETURN-BYTE-OFFSET
;;;
;;; This is used by the debugger.
;;;
(export 'single-value-return-byte-offset)
(defconstant single-value-return-byte-offset 4)


;;; LOCATION-PRINT-NAME  --  Interface
;;;
;;;    This function is called by debug output routines that want a pretty name
;;; for a TN's location.  It returns a thing that can be printed with PRINC.
;;;
(!def-vm-support-routine location-print-name (tn)
;  (declare (type tn tn))
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
