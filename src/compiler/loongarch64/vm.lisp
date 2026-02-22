;;;; miscellaneous VM definition noise for the LoongArch

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(defconstant-eqx +fixup-kinds+
  #(:absolute :i-type :s-type :u-type :u+i-type)
  #'equalp)

(defun u-and-i-inst-immediate (value)
  (let ((hi (ash (+ value (expt 2 11)) -12)))
    (values hi (- value (ash hi 12)))))

(def!type short-immediate () `(signed-byte 12))
(def!type short-immediate-fixnum ()
  `(signed-byte ,(- 12 n-fixnum-tag-bits)))

(deftype u+i-immediate ()
  `(or (integer #x-80000800 #x7ffff7ff)
              (integer ,(+ (ash 1 64) #x-80000800)
                       ,(1- (ash 1 64)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *register-names* (make-array 32 :initial-element nil)))

(macrolet ((defreg (name offset)
             (let ((offset-sym (symbolicate name "-OFFSET")))
               `(eval-when (:compile-toplevel :load-toplevel :execute)
                  (defconstant ,offset-sym ,offset)
                  (setf (svref *register-names* ,offset-sym) ,(symbol-name name)))))
           (define-argument-register-set (&rest args)
             `(progn
                (defregset *register-arg-offsets* ,@args)
                (defconstant register-arg-count ,(length args)))))
  (defreg zero 0)     ; $zero
  (defreg ra 1)       ; $ra
  (defreg tp 2)       ; $tp
  (defreg nsp 3)      ; $sp

  (defreg a0 4)       ; $a0
  (defreg a1 5)       ; $a1

  (defreg nl0 6)      ; $a2
  (defreg nl1 7)      ; $a3
  (defreg a2 8)       ; $a4
  (defreg nl2 9)      ; $a5
  (defreg a3 10)      ; $a6
  (defreg nl3 11)     ; $a7
  ;; temporary
  (defreg lip 12)     ; $t0
  (defreg cfp 13)     ; $t1
  (defreg ocfp 14)    ; $t2
  (defreg lexenv 15)  ; $t3
  (defreg null 16)    ; $t4
  (defreg code 17)    ; $t5
  (defreg nargs 18)   ; $t6
  (defreg t7 19)      ; $t7
  (defreg t8 20)      ; $t8
  (defreg rsv 21)
  ;;  Frame pointer
  (defreg nfp 22)     ; $fp
  ;;  Static register
  (defreg csp 23)     ; $s0
  (defreg a4 24)      ; $s1
  (defreg nl4 25)     ; $s2
  (defreg a5 26)      ; $s3
  (defreg nl5 27)     ; $s4
  (defreg l0 28)      ; $s5
  (defreg l1 29)      ; $s6

  (defreg #-sb-thread l2 #+sb-thread thread 30) ; $s7
  (defreg cfunc 31)   ; $s8

  (defregset non-descriptor-regs ra nl0 nl1 nl2 nl3 nl4 nl5 nargs nfp cfunc)
  (defregset descriptor-regs a0 a1 a2 a3 a4 l0 l1 #-sb-thread l2 lexenv ocfp)
  (defregset reserve-descriptor-regs lexenv)
  (defregset reserve-non-descriptor-regs cfunc)
  (defregset boxed-regs a0 a1 a2 a3 a4 a5 l0 l1 #-sb-thread l2 ocfp lexenv code)

  (define-argument-register-set a0 a1 a2 a3 a4 a5))

(!define-storage-bases
 (define-storage-base registers :finite :size 32)
 (define-storage-base control-stack :unbounded :size 0)
 (define-storage-base non-descriptor-stack :unbounded :size 0)
 (define-storage-base float-registers :finite :size 32)
 (define-storage-base constant :non-packed)
 (define-storage-base immediate-constant :non-packed))

(!define-storage-classes
 (constant constant)

 ;; Immediate constant.
 (zero immediate-constant)
 (immediate immediate-constant)

 (control-stack control-stack)
 (any-reg registers
          :locations #.(append non-descriptor-regs descriptor-regs)
          :reserve-locations #.(append reserve-non-descriptor-regs
                                       reserve-descriptor-regs)
          :alternate-scs (control-stack)
          :constant-scs (immediate zero constant)
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
             :constant-scs (zero immediate)
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
 (unwind-block control-stack :element-size unwind-block-size))

;;;; Random TNs for interesting registers
(macrolet ((defregtn (name sc)
  (let ((offset-sym (symbolicate name "-OFFSET"))
       (tn-sym (symbolicate name "-TN")))
    `(defglobal ,tn-sym
       (make-random-tn (sc-or-lose ',sc) ,offset-sym)))))

(defregtn zero any-reg)
(defregtn lip interior-reg)
(defregtn code descriptor-reg)
(defregtn null descriptor-reg)
(defregtn t7 any-reg)
(defregtn t8 any-reg)
(defregtn cfunc any-reg)

(defregtn nargs any-reg)
(defregtn lexenv descriptor-reg)

(defregtn csp any-reg)
(defregtn cfp any-reg)
(defregtn nsp any-reg)
(defregtn ocfp any-reg)
(defregtn nfp any-reg)

(defregtn ra any-reg))

(defun immediate-constant-sc (value)
  (typecase value
    ((integer 0 0)
     zero-sc-number)
    (null
     (values descriptor-reg-sc-number null-offset))
    (symbol
     (if (static-symbol-p value)
         immediate-sc-number
         nil))
    ((integer #.most-negative-fixnum #.most-positive-fixnum)
     immediate-sc-number)
    (character
     immediate-sc-number)
    (structure-object
     (when (eq value sb-lockless:+tail+)
       immediate-sc-number))))

(defun boxed-immediate-sc-p (sc)
  (or (eql sc zero-sc-number)
      (eql sc immediate-sc-number)))

;;;; Function Call Parameters
;;; The SC numbers for register and stack arguments/return values.
(defconstant immediate-arg-scn any-reg-sc-number)
(defconstant control-stack-arg-scn control-stack-sc-number)

(defconstant ocfp-save-offset 0)
(defconstant ra-save-offset 1)
(defconstant nfp-save-offset 2)

(define-load-time-global *register-arg-tns*
  (let ((drsc (sc-or-lose 'descriptor-reg)))
    (flet ((make (n) (make-random-tn drsc n)))
      (mapcar #'make *register-arg-offsets*))))

#+sb-thread
(defparameter thread-base-tn
  (make-random-tn (sc-or-lose 'unsigned-reg) thread-offset))

(defconstant single-value-return-byte-offset 0)

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

#+sb-thread
(progn
  (defconstant pseudo-atomic-flag (ash list-pointer-lowtag 0))
  (defconstant pseudo-atomic-interrupted-flag (ash list-pointer-lowtag 16)))
