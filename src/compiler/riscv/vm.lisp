;;;; miscellaneous VM definition noise for the RISC-V

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *register-names* (make-array 32 :initial-element nil)))

(macrolet ((defreg (name offset)
             (let ((offset-sym (symbolicate name "-OFFSET")))
               `(eval-when (:compile-toplevel :load-toplevel :execute)
                  (defconstant ,offset-sym ,offset)
                  (setf (svref *register-names* ,offset-sym) ,(symbol-name name)))))
           (defregset (name &rest regs)
             (flet ((offset-namify (n) (symbolicate n "-OFFSET")))
               `(eval-when (:compile-toplevel :load-toplevel :execute)
                  (defparameter ,name
                    (list ,@(mapcar #'offset-namify regs))))))
           (define-argument-register-set (&rest args)
             `(progn
                (defregset *register-arg-offsets* ,@args)
                (defconstant register-arg-count ,(length args)))))
  (defreg zero 0)
  (defreg lr 1)
  (defreg nsp 2)
  (defreg global 3)
  (defreg thread 4)
  (defreg lra 5) ; alternate link register
  (defreg cfp 6)
  (defreg ocfp 7)
  (defreg nfp 8)
  (defreg csp 9)

  (defreg a0 10)
  (defreg nl0 11)
  (defreg a1 12)
  (defreg nl1 13)
  (defreg a2 14)
  (defreg nl2 15)
  (defreg a3 16)
  (defreg nl3 17)
  (defreg l0 18)
  (defreg nl4 19)
  (defreg l1 20)
  (defreg nl5 21)
  (defreg l2 22)
  (defreg nl6 23)
  (defreg l3 24)
  (defreg nl7 25)

  (defreg cfunc 26)
  (defreg lexenv 27)
  (defreg null 28)
  (defreg code 29)
  (defreg lip 30)
  (defreg nargs 31)

  (defregset non-descriptor-regs nl0 nl1 nl2 nl3 nl4 nl5 nl6 nl7 nargs nfp cfunc)
  (defregset descriptor-regs a0 a1 a2 a3 l0 l1 l2 l3 ocfp lra lexenv)
  (defregset boxed-regs a0 a1 a2 a3 l0 l1 l2 l3 ocfp lra lexenv code)

  (define-argument-register-set a0 a1 a2 a3))

(!define-storage-bases
 (define-storage-base registers :finite :size 32)
 (define-storage-base control-stack :unbounded :size 8)
 (define-storage-base non-descriptor-stack :unbounded :size 0)

 (define-storage-base float-registers :finite :size 32)

 (define-storage-base constant :non-packed)
 (define-storage-base immediate-constant :non-packed)
 )

(!define-storage-classes
 (constant constant)
 (immediate immediate-constant)

 (control-stack control-stack)
 (any-reg registers
          :locations #.(append non-descriptor-regs descriptor-regs)
          :alternate-scs (control-stack)
          :constant-scs (immediate constant)
          :save-p t)

 ;; Pointer descriptor objects.  Must be seen by GC.
 (descriptor-reg registers
                 :locations #.descriptor-regs
                 :alternate-scs (control-stack)
                 :constant-scs (immediate constant)
                 :save-p t)

 ;; Random objects that must not be seen by GC.  Used only as temporaries.
 (non-descriptor-reg registers :locations #.non-descriptor-regs)

 ;; Pointers to the interior of objects.  Used only as a temporary.
 (interior-reg registers :locations (#.lip-offset))

 (character-stack non-descriptor-stack)

 ;; Non-Descriptor characters
 (character-reg registers
                :locations #.non-descriptor-regs
                :alternate-scs (character-stack)
                :constant-scs (immediate)
                :save-p t)

 (sap-stack non-descriptor-stack)
 (sap-reg registers
          :locations #.non-descriptor-regs
          :constant-scs (immediate)
          :alternate-scs (sap-stack)
          :save-p t)
 (signed-stack non-descriptor-stack)
 (signed-reg registers
             :locations #.non-descriptor-regs
             :alternate-scs (signed-stack)
             :constant-scs (immediate)
             :save-p t)
 (unsigned-stack non-descriptor-stack)
 (unsigned-reg registers
               :locations #.non-descriptor-regs
               :alternate-scs (unsigned-stack)
               :constant-scs (immediate)
               :save-p t)

 ;; Non-descriptor floating point.
 (single-stack non-descriptor-stack)
 (single-reg float-registers
             :locations #.(loop for i below 32 collect i)
             :alternate-scs (single-stack)
             :save-p t)
 (double-stack non-descriptor-stack :element-size (/ 64 n-word-bits))
 (double-reg float-registers
             :locations #.(loop for i below 32 collect i)
             :alternate-scs (double-stack)
             :save-p t)

 (complex-single-stack non-descriptor-stack :element-size (/ (* 2 32) n-word-bits))
 (complex-single-reg float-registers
                     :locations #.(loop for i below 32 collect i)
                     :element-size (/ (* 2 32) n-word-bits)
                     :alternate-scs (complex-single-stack)
                     :save-p t)
 (complex-double-stack non-descriptor-stack :element-size (/ (* 2 64) n-word-bits))
 (complex-double-reg float-registers
                     :locations #.(loop for i below 32 by 2 collect i)
                     :element-size (/ (* 2 64) n-word-bits)
                     :save-p t
                     :alternate-scs (complex-double-stack))

 (catch-block control-stack :element-size catch-block-size)
 (unwind-block control-stack :element-size unwind-block-size)
 )

;;;; Random TNs for interesting registers

(macrolet ((defregtn (name sc)
               (let ((offset-sym (symbolicate name "-OFFSET"))
                     (tn-sym (symbolicate name "-TN")))
                 `(defglobal ,tn-sym
                   (make-random-tn :kind :normal
                    :sc (sc-or-lose ',sc)
                    :offset ,offset-sym)))))
  (defregtn zero any-reg)
  (defregtn nargs any-reg)

  (defregtn nfp any-reg)
  (defregtn ocfp any-reg)

  (defregtn null descriptor-reg)
  (defregtn lexenv descriptor-reg)

  (defregtn cfp any-reg)
  (defregtn csp any-reg)
  (defregtn nsp any-reg)

  (defregtn code descriptor-reg)
  (defregtn lr interior-reg)
  (defregtn lip interior-reg))

;;; If VALUE can be represented as an immediate constant, then return the
;;; appropriate SC number, otherwise return NIL.
(defun immediate-constant-sc (value)
  (typecase value
    (null
     (values descriptor-reg-sc-number null-offset))
    ((or (integer #.sb-xc:most-negative-fixnum #.sb-xc:most-positive-fixnum)
         character)
     immediate-sc-number)
    (symbol
     (if (static-symbol-p value)
         immediate-sc-number
         nil))))

(defun boxed-immediate-sc-p (sc)
  (eql sc immediate-sc-number))

;;;; Function Call Parameters

;;; The SC numbers for register and stack arguments/return values.
;;;
(defconstant immediate-arg-scn any-reg-sc-number)
(defconstant control-stack-arg-scn control-stack-sc-number)

;;; Offsets of special stack frame locations
(defconstant ocfp-save-offset 0)
(defconstant lra-save-offset 1)
(defconstant nfp-save-offset 2)

(defparameter *register-arg-tns*
  (let ((drsc (sc-or-lose 'descriptor-reg)))
    (flet ((make (n) (make-random-tn :kind :normal :sc drsc :offset n)))
      (mapcar #'make *register-arg-offsets*))))

;;; This is used by the debugger.  Our calling convention for
;;; unknown-values-return does not involve manipulating return
;;; addresses.
(defconstant single-value-return-byte-offset 0)

(defun location-print-name (tn)
  (declare (type tn tn))
  (let ((sb (sb-name (sc-sb (tn-sc tn))))
        (offset (tn-offset tn)))
    (ecase sb
      (registers (or (svref *register-names* offset)
                     (format nil "x~D" offset)))
      (control-stack (format nil "CS~D" offset))
      (float-registers (format nil "f~D" offset))
      (non-descriptor-stack (format nil "NS~D" offset))
      (constant (format nil "Const~D" offset))
      (immediate-constant "Immed"))))

(defun combination-implementation-style (node)
  (declare (type sb-c::combination node) (ignore node))
  (values :default nil))

(defun primitive-type-indirect-cell-type (ptype)
  (declare (ignore ptype))
  nil)
