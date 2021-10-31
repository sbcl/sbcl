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

(defconstant-eqx +fixup-kinds+ #(:absolute :i-type :s-type :u-type :u+i-type) #'equalp)

(defun u-and-i-inst-immediate (value)
  (let ((hi (ash (+ value (expt 2 11)) -12)))
    (values hi (- value (ash hi 12)))))

(def!type short-immediate () `(signed-byte 12))
(def!type short-immediate-fixnum () `(signed-byte ,(- 12 n-fixnum-tag-bits)))

(deftype u+i-immediate ()
  #-64-bit `(or (signed-byte 32) (unsigned-byte 32))
  #+64-bit `(or (integer #x-80000800 #x7ffff7ff)
                (integer ,(+ (ash 1 64) #x-80000800)
                         ,(1- (ash 1 64)))))

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
                     ; ABI register mnemonic
  (defreg zero 0)    ; zero
  (defreg lip 1)     ; ra
  (defreg nsp 2)     ; sp
  (defreg global 3)  ; gp
  (defreg tp 4)      ; tp
  (defreg lra 5)     ; t0, alternate link register
  (defreg cfp 6)     ; t1
  (defreg ocfp 7)    ; t2
  (defreg nfp 8)     ; s0, callee-saved
  (defreg csp 9)     ; s1  "

  (defreg a0 10)     ; a0, arg
  (defreg nl0 11)    ; a1
  (defreg a1 12)     ; a2
  (defreg nl1 13)    ; a3
  (defreg a2 14)     ; a4
  (defreg nl2 15)    ; a5
  (defreg a3 16)     ; a6
  (defreg nl3 17)    ; a7
  (defreg a4 18)     ; s2, callee-saved
  (defreg nl4 19)    ; s3
  (defreg a5 20)     ; s4
  (defreg nl5 21)    ; s5
  (defreg l0 22)     ; s6
  (defreg nl6 23)    ; s7
  (defreg l1 24)     ; s8
  (defreg nl7 25)    ; s9
  (defreg #-sb-thread l2 #+sb-thread thread 26) ; s10

  (defreg cfunc 27)  ; s11
  (defreg lexenv 28) ; t3
  (defreg null 29)   ; t4
  (defreg code 30)   ; t5
  (defreg nargs 31)  ; t6

  (defregset non-descriptor-regs nl0 nl1 nl2 nl3 nl4 nl5 nl6 nl7 nargs nfp cfunc)
  (defregset descriptor-regs a0 a1 a2 a3 a4 a5 l0 l1 #-sb-thread l2 ocfp lra lexenv)
  (defregset reserve-descriptor-regs lexenv)
  (defregset reserve-non-descriptor-regs cfunc)
  (defregset boxed-regs a0 a1 a2 a3 a4 a5 l0 l1
    #-sb-thread l2 #+sb-thread thread
    ocfp lra lexenv code)

  (define-argument-register-set a0 a1 a2 a3 a4 a5))

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
          :reserve-locations #.(append reserve-non-descriptor-regs
                                       reserve-descriptor-regs)
          :alternate-scs (control-stack)
          :constant-scs (immediate constant)
          :save-p t)

 ;; Pointer descriptor objects.  Must be seen by GC.
 (descriptor-reg registers
                 :locations #.descriptor-regs
                 :reserve-locations #.reserve-descriptor-regs
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
                :reserve-locations #.reserve-non-descriptor-regs
                :alternate-scs (character-stack)
                :constant-scs (immediate)
                :save-p t)

 (sap-stack non-descriptor-stack)
 (sap-reg registers
          :locations #.non-descriptor-regs
          :reserve-locations #.reserve-non-descriptor-regs
          :constant-scs (immediate)
          :alternate-scs (sap-stack)
          :save-p t)
 (signed-stack non-descriptor-stack)
 (signed-reg registers
             :locations #.non-descriptor-regs
             :reserve-locations #.reserve-non-descriptor-regs
             :alternate-scs (signed-stack)
             :constant-scs (immediate)
             :save-p t)
 (unsigned-stack non-descriptor-stack)
 (unsigned-reg registers
               :locations #.non-descriptor-regs
               :reserve-locations #.reserve-non-descriptor-regs
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
                     :locations #.(loop for i below 32 by (/ (* 2 32) n-word-bits)
                                        collect i)
                     :element-size (/ (* 2 32) n-word-bits)
                     :alternate-scs (complex-single-stack)
                     :save-p t)
 (complex-double-stack non-descriptor-stack :element-size (/ (* 2 64) n-word-bits))
 (complex-double-reg float-registers
                     :locations #.(loop for i below 32 by (/ (* 2 64) n-word-bits)
                                        collect i)
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
  (defregtn lip interior-reg))

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

#+sb-thread
(defparameter thread-base-tn
  (make-random-tn :kind :normal :sc (sc-or-lose 'unsigned-reg)
                  :offset thread-offset))

;;; This is used by the debugger.  Our calling convention for
;;; unknown-values-return does not involve manipulating return
;;; addresses.
(defconstant single-value-return-byte-offset 0)

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

(defun combination-implementation-style (node)
  (declare (type sb-c::combination node) (ignore node))
  (values :default nil))

(defun primitive-type-indirect-cell-type (ptype)
  (declare (ignore ptype))
  nil)

#+sb-thread
(progn
  (defconstant pseudo-atomic-flag (ash list-pointer-lowtag 0))
  (defconstant pseudo-atomic-interrupted-flag (ash list-pointer-lowtag 16)))
