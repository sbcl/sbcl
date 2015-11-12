;;;; VOPs and other machine-specific support routines for call-out to C

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

(defun my-make-wired-tn (prim-type-name sc-name offset)
  (make-wired-tn (primitive-type-or-lose prim-type-name )
                 (sc-number-or-lose sc-name )
                 offset))

(defstruct arg-state
  (stack-frame-size 0))

(define-alien-type-method (integer :arg-tn) (type state)
  (let ((stack-frame-size (arg-state-stack-frame-size state)))
    (setf (arg-state-stack-frame-size state) (1+ stack-frame-size))
    (multiple-value-bind
        (ptype reg-sc stack-sc)
        (if (alien-integer-type-signed type)
            (values 'signed-byte-64 'signed-reg 'signed-stack)
            (values 'unsigned-byte-64 'unsigned-reg 'unsigned-stack))
      (if (< stack-frame-size 4)
          (my-make-wired-tn ptype reg-sc (+ stack-frame-size nl0-offset))
          (my-make-wired-tn ptype stack-sc (* 2 (- stack-frame-size 4)))))))

(define-alien-type-method (system-area-pointer :arg-tn) (type state)
  (declare (ignore type))
  (let ((stack-frame-size (arg-state-stack-frame-size state)))
    (setf (arg-state-stack-frame-size state) (1+ stack-frame-size))
    (if (< stack-frame-size 4)
        (my-make-wired-tn 'system-area-pointer
                          'sap-reg
                          (+ stack-frame-size nl0-offset))
        (my-make-wired-tn 'system-area-pointer
                          'sap-stack
                          (* 2 (- stack-frame-size 4))))))

(define-alien-type-method (double-float :arg-tn) (type state)
  (declare (ignore type))
  (let ((stack-frame-size (arg-state-stack-frame-size state)))
    (setf (arg-state-stack-frame-size state) (1+ stack-frame-size))
    (if (< stack-frame-size 6)
        (my-make-wired-tn 'double-float
                          'double-reg
                          (+ stack-frame-size nl0-offset))
        (my-make-wired-tn 'double-float
                          'double-stack
                          (* 2 (- stack-frame-size 4))))))

(define-alien-type-method (single-float :arg-tn) (type state)
  (declare (ignore type))
  (let ((stack-frame-size (arg-state-stack-frame-size state)))
    (setf (arg-state-stack-frame-size state) (1+ stack-frame-size))
    (if (< stack-frame-size 6)
        (my-make-wired-tn 'single-float
                          'single-reg
                          (+ stack-frame-size nl0-offset))
        (my-make-wired-tn 'single-float
                          'single-stack
                          (* 2 (- stack-frame-size 4))))))

(define-alien-type-method (integer :result-tn) (type state)
  (declare (ignore state))
  (multiple-value-bind
      (ptype reg-sc)
      (if (alien-integer-type-signed type)
          (values 'signed-byte-64 'signed-reg)
          (values 'unsigned-byte-64 'unsigned-reg))
    (my-make-wired-tn ptype reg-sc lip-offset)))

(define-alien-type-method (integer :naturalize-gen) (type alien)
  (if (<= (alien-type-bits type) 32)
      (if (alien-integer-type-signed type)
          `(sign-extend ,alien ,(alien-type-bits type))
          `(logand ,alien ,(1- (ash 1 (alien-type-bits type)))))
      alien))

(define-alien-type-method (system-area-pointer :result-tn) (type state)
  (declare (ignore type state))
  (my-make-wired-tn 'system-area-pointer 'sap-reg lip-offset))

(define-alien-type-method (double-float :result-tn) (type state)
  (declare (ignore type state))
  (my-make-wired-tn 'double-float 'double-reg lip-offset))

(define-alien-type-method (single-float :result-tn) (type state)
  (declare (ignore type state))
  (my-make-wired-tn 'single-float 'single-reg lip-offset))

(define-alien-type-method (values :result-tn) (type state)
  (let ((values (alien-values-type-values type)))
    (when (cdr values)
      (error "Too many result values from c-call."))
    (when values
      (invoke-alien-type-method :result-tn (car values) state))))

(defun make-call-out-tns (type)
  (let ((arg-state (make-arg-state)))
    (collect ((arg-tns))
      (dolist (arg-type (alien-fun-type-arg-types type))
        (arg-tns (invoke-alien-type-method :arg-tn arg-type arg-state)))
      (values (my-make-wired-tn 'positive-fixnum 'any-reg nsp-offset)
              (* (max (- (logandc2 (1+ (arg-state-stack-frame-size arg-state)) 1) 4) 2)
                 n-word-bytes
                 #.(floor n-machine-word-bits n-word-bits))
              (arg-tns)
              (invoke-alien-type-method :result-tn
                                        (alien-fun-type-result-type type)
                                        nil)))))

(defknown sign-extend ((signed-byte 64) t) fixnum
    (foldable flushable movable))

(define-vop (sign-extend)
  (:translate sign-extend)
  (:policy :fast-safe)
  (:args (val :scs (signed-reg) :target res))
  (:arg-types signed-num (:constant fixnum))
  (:info size)
  (:results (res :scs (signed-reg)))
  (:result-types fixnum)
  (:generator 1
   (ecase size
     (8
      ;;(inst sextb val res) ;; Under what circumstances can we use this?
      (inst sll val 56 res)
      (inst sra res 56 res))
     (16
      ;;(inst sextw val res) ;; Under what circumstances can we use this?
      (inst sll val 48 res)
      (inst sra res 48 res))
     (32
      (inst sll val 32 res)
      (inst sra res 32 res)))))

#-sb-xc-host
(defun sign-extend (x size)
  (declare (type (signed-byte 64) x))
  (ecase size
    (8 (sign-extend x size))
    (16 (sign-extend x size))
    (32 (sign-extend x size))))

#+sb-xc-host
(defun sign-extend (x size)
  (if (logbitp (1- size) x)
      (dpb x (byte size 0) -1)
      x))

(define-vop (foreign-symbol-sap)
  (:translate foreign-symbol-sap)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-string))
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (inst li (make-fixup foreign-symbol :foreign) res)))

(define-vop (call-out)
  (:args (function :scs (sap-reg) :target cfunc)
         (args :more t))
  (:results (results :more t))
  (:ignore args results)
  (:save-p t)
  (:temporary (:sc any-reg :offset cfunc-offset
                   :from (:argument 0) :to (:result 0)) cfunc)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:vop-var vop)
  (:generator 0
    (let ((cur-nfp (sb!c::current-nfp-tn vop)))
      (when cur-nfp
        (store-stack-tn nfp-save cur-nfp))
      (move function cfunc)
      (inst li (make-fixup "call_into_c" :foreign) temp)
      (inst jsr lip-tn temp (make-fixup "call_into_c" :foreign))
      (when cur-nfp
        (maybe-load-stack-nfp-tn cur-nfp nfp-save temp)))))

(define-vop (alloc-number-stack-space)
  (:info amount)
  (:results (result :scs (sap-reg any-reg)))
  (:result-types system-area-pointer)
  (:temporary (:scs (unsigned-reg) :to (:result 0)) temp)
  (:generator 0
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 7) 7)))
        (cond ((< delta (ash 1 15))
               (inst lda nsp-tn (- delta) nsp-tn))
              (t
               (inst li delta temp)
               (inst subq nsp-tn temp nsp-tn)))))
    (move nsp-tn result)))

(define-vop (dealloc-number-stack-space)
  (:info amount)
  (:policy :fast-safe)
  (:temporary (:scs (unsigned-reg) :to (:result 0)) temp)
  (:generator 0
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 7) 7)))
        (cond ((< delta (ash 1 15))
               (inst lda nsp-tn delta nsp-tn))
              (t
               (inst li delta temp)
               (inst addq nsp-tn temp nsp-tn)))))))
