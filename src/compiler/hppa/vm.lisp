;;;; miscellaneous VM definition noise for HPPA

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

;;; FIXME: These want to turn into macrolets.
(macrolet ((defreg (name offset)
               (let ((offset-sym (symbolicate name "-OFFSET")))
                 `(eval-when (:compile-toplevel :load-toplevel :execute)
                   (def!constant ,offset-sym ,offset)
                   (setf (svref *register-names* ,offset-sym) ,(symbol-name name)))))
           (defregset (name &rest regs)
               `(eval-when (:compile-toplevel :load-toplevel :execute)
                 (defparameter ,name
                   (list ,@(mapcar #'(lambda (name) (symbolicate name "-OFFSET")) regs))))))

  ;; Wired-zero
  (defreg zero 0)
  ;; This gets trashed by the C call convention.
  (defreg nfp 1)
  (defreg cfunc 2)
  ;; These are the callee saves, so these registers are stay live over
  ;; call-out.
  (defreg csp 3)
  (defreg cfp 4)
  (defreg bsp 5)
  (defreg null 6)
  (defreg alloc 7)
  (defreg code 8)
  (defreg fdefn 9)
  (defreg lexenv 10)
  (defreg nargs 11)
  (defreg ocfp 12)
  (defreg lra 13)
  (defreg a0 14)
  (defreg a1 15)
  (defreg a2 16)
  (defreg a3 17)
  (defreg a4 18)
  ;; This is where the caller-saves registers start, but we don't
  ;; really care because we need to clear the above after call-out to
  ;; make sure no pointers into oldspace are kept around.
  (defreg a5 19)
  (defreg l0 20)
  (defreg l1 21)
  (defreg l2 22)
  ;; These are the 4 C argument registers.
  (defreg nl3 23)
  (defreg nl2 24)
  (defreg nl1 25)
  (defreg nl0 26)
  ;; The global Data Pointer.  We just leave it alone, because we
  ;; don't need it.
  (defreg dp 27)
  ;; These two are use for C return values.
  (defreg nl4 28)
  (defreg nl5 29)
  (defreg nsp 30)
  (defreg lip 31)

  (defregset non-descriptor-regs
      nl0 nl1 nl2 nl3 nl4 nl5 nfp cfunc)

  (defregset descriptor-regs
      fdefn lexenv nargs ocfp lra a0 a1 a2 a3 a4 a5 l0 l1 l2)

  (defregset *register-arg-offsets*
      a0 a1 a2 a3 a4 a5))


(define-storage-base registers :finite :size 32)
(define-storage-base float-registers :finite :size 64)
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

  ;; Pointers to the interior of objects.  Used only as an temporary.
  (interior-reg registers
   :locations (#.lip-offset))


  ;; **** Things that can go in the floating point registers.

  ;; Non-Descriptor single-floats.
  (single-reg float-registers
   :locations #.(loop for i from 4 to 31 collect i)
   :constant-scs (fp-single-zero)
   :save-p t
   :alternate-scs (single-stack))

  ;; Non-Descriptor double-floats.
  (double-reg float-registers
   :locations #.(loop for i from 4 to 31 collect i)
   :constant-scs (fp-double-zero)
   :save-p t
   :alternate-scs (double-stack))

  (complex-single-reg float-registers
   :locations #.(loop for i from 4 to 30 by 2 collect i)
   :element-size 2
   :constant-scs ()
   :save-p t
   :alternate-scs (complex-single-stack))

  (complex-double-reg float-registers
   :locations #.(loop for i from 4 to 30 by 2 collect i)
   :element-size 2
   :constant-scs ()
   :save-p t
   :alternate-scs (complex-double-stack))

  ;; A catch or unwind block.
  (catch-block control-stack :element-size kludge-nondeterministic-catch-block-size))


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
                  :offset 0))
(defparameter fp-double-zero-tn
  (make-random-tn :kind :normal
                  :sc (sc-or-lose 'double-reg)
                  :offset 0))


;;; If VALUE can be represented as an immediate constant, then return
;;; the appropriate SC number, otherwise return NIL.
(!def-vm-support-routine immediate-constant-sc (value)
  (typecase value
    ((integer 0 0)
     (sc-number-or-lose 'zero))
    (null
     (sc-number-or-lose 'null))
    ((or (integer #.sb!xc:most-negative-fixnum #.sb!xc:most-positive-fixnum)
         system-area-pointer character)
     (sc-number-or-lose 'immediate))
    (symbol
     (if (static-symbol-p value)
         (sc-number-or-lose 'immediate)
         nil))
    (single-float
     (if (zerop value)
         (sc-number-or-lose 'fp-single-zero)
         nil))
    (double-float
     (if (zerop value)
         (sc-number-or-lose 'fp-double-zero)
         nil))))


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
(defconstant single-value-return-byte-offset 4)

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

(!def-vm-support-routine combination-implementation-style (node)
  (declare (type sb!c::combination node) (ignore node))
  (values :default nil))
