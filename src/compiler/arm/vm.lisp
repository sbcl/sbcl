;;;; miscellaneous VM definition noise for the ARM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")


;;;; register specs

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *register-names* (make-array 16 :initial-element nil)))

(macrolet ((defreg (name offset)
             (let ((offset-sym (symbolicate name "-OFFSET")))
               `(eval-when (:compile-toplevel :load-toplevel :execute)
                  (def!constant ,offset-sym ,offset)
                  (setf (svref *register-names* ,offset-sym) ,(symbol-name name)))))

           (defregset (name &rest regs)
             `(eval-when (:compile-toplevel :load-toplevel :execute)
                (defparameter ,name
                  (list ,@(mapcar #'(lambda (name)
                                      (symbolicate name "-OFFSET")) regs))))))

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

  ;; registers used to pass arguments
  ;;
  ;; the number of arguments/return values passed in registers
  (def!constant register-arg-count 3)
  ;; names and offsets for registers used to pass arguments
  (defregset *register-arg-offsets*  r0 r1 r2)
  (defparameter *register-arg-names* '(r0 r1 r2)))


;;;; SB and SC definition:

(define-storage-base registers :finite :size 16)
(define-storage-base control-stack :unbounded :size 2 :size-increment 1)
(define-storage-base non-descriptor-stack :unbounded :size 0)
(define-storage-base constant :non-packed)
(define-storage-base immediate-constant :non-packed)
#!+arm-vfp
(define-storage-base float-registers :finite :size 32)
;; NOTE: If you fix the following, please to so with its own feature
;; conditional, and also adjust the definitions of the
;; {,COMPLEX-}{SINGLE,DOUBLE}-REG SCs below.
#!-arm-vfp
(error "Don't know how many float registers for non-VFP systems")

;;;
;;; Handy macro so we don't have to keep changing all the numbers whenever
;;; we insert a new storage class.
;;;
(defmacro define-storage-classes (&rest classes)
  (do ((forms (list 'progn)
              (let* ((class (car classes))
                     (sc-name (car class))
                     (constant-name (intern (concatenate 'simple-string
                                                         (string sc-name)
                                                         "-SC-NUMBER"))))
                (list* `(define-storage-class ,sc-name ,index
                          ,@(cdr class))
                       `(def!constant ,constant-name ,index)
                       forms)))
       (index 0 (1+ index))
       (classes classes (cdr classes)))
      ((null classes)
       (nreverse forms))))

(define-storage-classes

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

  ;; A catch or unwind block.
  (catch-block control-stack :element-size catch-block-size))

;;;; Make some random tns for important registers.

(macrolet ((defregtn (name sc)
               (let ((offset-sym (symbolicate name "-OFFSET"))
                     (tn-sym (symbolicate name "-TN")))
                 `(defparameter ,tn-sym
                   (make-random-tn :kind :normal
                    :sc (sc-or-lose ',sc)
                    :offset ,offset-sym)))))

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
     (sc-number-or-lose 'null))
    ((or (integer #.sb!xc:most-negative-fixnum #.sb!xc:most-positive-fixnum)
         character)
     (sc-number-or-lose 'immediate))
    (symbol
     (if (static-symbol-p value)
         (sc-number-or-lose 'immediate)
         nil))))

(defun boxed-immediate-sc-p (sc)
  (or (eql sc (sc-number-or-lose 'null))
      (eql sc (sc-number-or-lose 'immediate))))

;;;; function call parameters

;;; the SC numbers for register and stack arguments/return values
(def!constant immediate-arg-scn (sc-number-or-lose 'any-reg))
(def!constant control-stack-arg-scn (sc-number-or-lose 'control-stack))

;;; offsets of special stack frame locations
(def!constant ocfp-save-offset 0)
(def!constant lra-save-offset 1)
(def!constant nfp-save-offset 2)

;;; This is used by the debugger.
;;; < nyef> Ah, right. So, SINGLE-VALUE-RETURN-BYTE-OFFSET doesn't apply to x86oids or ARM.
(def!constant single-value-return-byte-offset 0)


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
      (registers (or (svref *register-names* offset)
                     (format nil "R~D" offset)))
      (control-stack (format nil "CS~D" offset))
      (non-descriptor-stack (format nil "NS~D" offset))
      (constant (format nil "Const~D" offset))
      (immediate-constant "Immed")
      (float-registers (format nil "F~D" offset)))))

(defun combination-implementation-style (node)
  (flet ((valid-funtype (args result)
           (sb!c::valid-fun-use node
                                (sb!c::specifier-type
                                 `(function ,args ,result)))))
    (case (sb!c::combination-fun-source-name node)
      (logtest
       (cond
         ((valid-funtype '(fixnum fixnum) '*)
          (values :maybe nil))
         ((valid-funtype '((signed-byte 32) (signed-byte 32)) '*)
          (values :maybe nil))
         ((valid-funtype '((unsigned-byte 32) (unsigned-byte 32)) '*)
          (values :maybe nil))
         (t (values :default nil))))
      (logbitp
       (cond
         ((or (valid-funtype '((constant-arg (integer 0 29)) fixnum) '*)
              (valid-funtype '((constant-arg (integer 0 31)) (signed-byte 32)) '*)
              (valid-funtype '((constant-arg (integer 0 31)) (unsigned-byte 32)) '*))
          (values :transform '(lambda (index integer)
                               (%logbitp integer index))))
         (t (values :default nil))))
      (t (values :default nil)))))

(defun primitive-type-indirect-cell-type (ptype)
  (declare (ignore ptype))
  nil)
