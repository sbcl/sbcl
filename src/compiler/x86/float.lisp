;;;; floating point support for the x86

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(macrolet ((ea-for-xf-desc (tn slot)
             `(object-slot-ea ,tn ,slot other-pointer-lowtag)))
  (defun ea-for-sf-desc (tn)
    (ea-for-xf-desc tn single-float-value-slot))
  (defun ea-for-df-desc (tn)
    (ea-for-xf-desc tn double-float-value-slot))
  #+long-float
  (defun ea-for-lf-desc (tn)
    (ea-for-xf-desc tn long-float-value-slot))
  ;; complex floats
  (defun ea-for-csf-real-desc (tn)
    (ea-for-xf-desc tn complex-single-float-real-slot))
  (defun ea-for-csf-imag-desc (tn)
    (ea-for-xf-desc tn complex-single-float-imag-slot))
  (defun ea-for-cdf-real-desc (tn)
    (ea-for-xf-desc tn complex-double-float-real-slot))
  (defun ea-for-cdf-imag-desc (tn)
    (ea-for-xf-desc tn complex-double-float-imag-slot))
  #+long-float
  (defun ea-for-clf-real-desc (tn)
    (ea-for-xf-desc tn complex-long-float-real-slot))
  #+long-float
  (defun ea-for-clf-imag-desc (tn)
    (ea-for-xf-desc tn complex-long-float-imag-slot)))

(macrolet ((ea-for-xf-stack (tn kind)
             `(make-ea
               :dword :base ebp-tn
               :disp (frame-byte-offset
                      (+ (tn-offset ,tn)
                       (ecase ,kind (:single 0) (:double 1) (:long 2)))))))
  (defun ea-for-sf-stack (tn)
    (ea-for-xf-stack tn :single))
  (defun ea-for-df-stack (tn)
    (ea-for-xf-stack tn :double))
  #+long-float
  (defun ea-for-lf-stack (tn)
    (ea-for-xf-stack tn :long)))

;;; Telling the FPU to wait is required in order to make signals occur
;;; at the expected place, but naturally slows things down.
;;;
;;; NODE is the node whose compilation policy controls the decision
;;; whether to just blast through carelessly or carefully emit wait
;;; instructions and whatnot.
;;;
;;; NOTE-NEXT-INSTRUCTION, if supplied, is to be passed to
;;; #'NOTE-NEXT-INSTRUCTION.
;;;
;;; Until 2004-03-15, the implementation of this was buggy; it
;;; unconditionally emitted the WAIT instruction.  It turns out that
;;; this is the right thing to do anyway; omitting them can lead to
;;; system corruption on conforming code.  -- CSR
(defun maybe-fp-wait (node &optional note-next-instruction)
  (declare (ignore node))
  #+nil
  (when (policy node (or (= debug 3) (> safety speed))))
  (when note-next-instruction
    (note-next-instruction note-next-instruction :internal-error))
  (inst wait))

;;; complex float stack EAs
(macrolet ((ea-for-cxf-stack (tn kind slot &optional base)
             `(make-ea
               :dword :base ,base
               :disp (frame-byte-offset
                      (+ (tn-offset ,tn)
                       -1
                       (* (ecase ,kind
                            (:single 1)
                            (:double 2)
                            (:long 3))
                          (ecase ,slot (:real 1) (:imag 2))))))))
  (defun ea-for-csf-real-stack (tn &optional (base ebp-tn))
    (ea-for-cxf-stack tn :single :real base))
  (defun ea-for-csf-imag-stack (tn &optional (base ebp-tn))
    (ea-for-cxf-stack tn :single :imag base))
  (defun ea-for-cdf-real-stack (tn &optional (base ebp-tn))
    (ea-for-cxf-stack tn :double :real base))
  (defun ea-for-cdf-imag-stack (tn &optional (base ebp-tn))
    (ea-for-cxf-stack tn :double :imag base))
  #+long-float
  (defun ea-for-clf-real-stack (tn &optional (base ebp-tn))
    (ea-for-cxf-stack tn :long :real base))
  #+long-float
  (defun ea-for-clf-imag-stack (tn &optional (base ebp-tn))
    (ea-for-cxf-stack tn :long :imag base)))

;;; Abstract out the copying of a FP register to the FP stack top, and
;;; provide two alternatives for its implementation. Note: it's not
;;; necessary to distinguish between a single or double register move
;;; here.
;;;
;;; Using a Pop then load.
(defun copy-fp-reg-to-fr0 (reg)
  (aver (not (zerop (tn-offset reg))))
  (inst fstp fr0-tn)
  (inst fld (make-random-tn (sc-or-lose 'double-reg) (1- (tn-offset reg)))))
;;; Using Fxch then Fst to restore the original reg contents.
#+nil
(defun copy-fp-reg-to-fr0 (reg)
  (aver (not (zerop (tn-offset reg))))
  (inst fxch reg)
  (inst fst  reg))

;;; The x86 can't store a long-float to memory without popping the
;;; stack and marking a register as empty, so it is necessary to
;;; restore the register from memory.
#+long-float
(defun store-long-float (ea)
   (inst fstpl ea)
   (inst fldl ea))

;;;; move functions

;;; X is source, Y is destination.
(define-move-fun (load-single 2) (vop x y)
  ((single-stack) (single-reg))
  (with-empty-tn@fp-top(y)
     (inst fld (ea-for-sf-stack x))))

(define-move-fun (store-single 2) (vop x y)
  ((single-reg) (single-stack))
  (cond ((zerop (tn-offset x))
         (inst fst (ea-for-sf-stack y)))
        (t
         (inst fxch x)
         (inst fst (ea-for-sf-stack y))
         ;; This may not be necessary as ST0 is likely invalid now.
         (inst fxch x))))

(define-move-fun (load-double 2) (vop x y)
  ((double-stack) (double-reg))
  (with-empty-tn@fp-top(y)
     (inst fldd (ea-for-df-stack x))))

(define-move-fun (store-double 2) (vop x y)
  ((double-reg) (double-stack))
  (cond ((zerop (tn-offset x))
         (inst fstd (ea-for-df-stack y)))
        (t
         (inst fxch x)
         (inst fstd (ea-for-df-stack y))
         ;; This may not be necessary as ST0 is likely invalid now.
         (inst fxch x))))

#+long-float
(define-move-fun (load-long 2) (vop x y)
  ((long-stack) (long-reg))
  (with-empty-tn@fp-top(y)
     (inst fldl (ea-for-lf-stack x))))

#+long-float
(define-move-fun (store-long 2) (vop x y)
  ((long-reg) (long-stack))
  (cond ((zerop (tn-offset x))
         (store-long-float (ea-for-lf-stack y)))
        (t
         (inst fxch x)
         (store-long-float (ea-for-lf-stack y))
         ;; This may not be necessary as ST0 is likely invalid now.
         (inst fxch x))))

;;; The i387 has instructions to load some useful constants. This
;;; doesn't save much time but might cut down on memory access and
;;; reduce the size of the constant vector (CV). Intel claims they are
;;; stored in a more precise form on chip. Anyhow, might as well use
;;; the feature. It can be turned off by hacking the
;;; "immediate-constant-sc" in vm.lisp.
(define-move-fun (load-fp-constant 2) (vop x y)
  ((fp-constant) (single-reg double-reg #+long-float long-reg))
  (let ((value (tn-value x)))
    (with-empty-tn@fp-top(y)
      (cond ((zerop value)
             (inst fldz))
            ((sb-xc:= value 1l0)
             (inst fld1))
            #+long-float
            ((= value pi)
             (inst fldpi))
            #+long-float
            ((= value (log 10l0 2l0))
             (inst fldl2t))
            #+long-float
            ((= value (log 2.718281828459045235360287471352662L0 2l0))
             (inst fldl2e))
            #+long-float
            ((= value (log 2l0 10l0))
             (inst fldlg2))
            #+long-float
            ((= value (log 2l0 2.718281828459045235360287471352662L0))
             (inst fldln2))
            (t (warn "ignoring bogus i387 constant ~A" value))))))

(define-move-fun (load-fp-immediate 2) (vop x y)
  ((fp-single-immediate) (single-reg)
   (fp-double-immediate) (double-reg))
  (let ((value (register-inline-constant (tn-value x))))
    (with-empty-tn@fp-top(y)
      (sc-case y
        (single-reg
         (inst fld value))
        (double-reg
         (inst fldd value))))))

;;;; complex float move functions

(defun complex-single-reg-real-tn (x)
  (make-random-tn (sc-or-lose 'single-reg) (tn-offset x)))
(defun complex-single-reg-imag-tn (x)
  (make-random-tn (sc-or-lose 'single-reg) (1+ (tn-offset x))))

(defun complex-double-reg-real-tn (x)
  (make-random-tn (sc-or-lose 'double-reg) (tn-offset x)))
(defun complex-double-reg-imag-tn (x)
  (make-random-tn (sc-or-lose 'double-reg) (1+ (tn-offset x))))

#+long-float
(defun complex-long-reg-real-tn (x)
  (make-random-tn (sc-or-lose 'long-reg) (tn-offset x)))
#+long-float
(defun complex-long-reg-imag-tn (x)
  (make-random-tn (sc-or-lose 'long-reg) (1+ (tn-offset x))))

;;; X is source, Y is destination.
(define-move-fun (load-complex-single 2) (vop x y)
  ((complex-single-stack) (complex-single-reg))
  (let ((real-tn (complex-single-reg-real-tn y)))
    (with-empty-tn@fp-top (real-tn)
      (inst fld (ea-for-csf-real-stack x))))
  (let ((imag-tn (complex-single-reg-imag-tn y)))
    (with-empty-tn@fp-top (imag-tn)
      (inst fld (ea-for-csf-imag-stack x)))))

(define-move-fun (store-complex-single 2) (vop x y)
  ((complex-single-reg) (complex-single-stack))
  (let ((real-tn (complex-single-reg-real-tn x)))
    (cond ((zerop (tn-offset real-tn))
           (inst fst (ea-for-csf-real-stack y)))
          (t
           (inst fxch real-tn)
           (inst fst (ea-for-csf-real-stack y))
           (inst fxch real-tn))))
  (let ((imag-tn (complex-single-reg-imag-tn x)))
    (inst fxch imag-tn)
    (inst fst (ea-for-csf-imag-stack y))
    (inst fxch imag-tn)))

(define-move-fun (load-complex-double 2) (vop x y)
  ((complex-double-stack) (complex-double-reg))
  (let ((real-tn (complex-double-reg-real-tn y)))
    (with-empty-tn@fp-top(real-tn)
      (inst fldd (ea-for-cdf-real-stack x))))
  (let ((imag-tn (complex-double-reg-imag-tn y)))
    (with-empty-tn@fp-top(imag-tn)
      (inst fldd (ea-for-cdf-imag-stack x)))))

(define-move-fun (store-complex-double 2) (vop x y)
  ((complex-double-reg) (complex-double-stack))
  (let ((real-tn (complex-double-reg-real-tn x)))
    (cond ((zerop (tn-offset real-tn))
           (inst fstd (ea-for-cdf-real-stack y)))
          (t
           (inst fxch real-tn)
           (inst fstd (ea-for-cdf-real-stack y))
           (inst fxch real-tn))))
  (let ((imag-tn (complex-double-reg-imag-tn x)))
    (inst fxch imag-tn)
    (inst fstd (ea-for-cdf-imag-stack y))
    (inst fxch imag-tn)))

#+long-float
(define-move-fun (load-complex-long 2) (vop x y)
  ((complex-long-stack) (complex-long-reg))
  (let ((real-tn (complex-long-reg-real-tn y)))
    (with-empty-tn@fp-top(real-tn)
      (inst fldl (ea-for-clf-real-stack x))))
  (let ((imag-tn (complex-long-reg-imag-tn y)))
    (with-empty-tn@fp-top(imag-tn)
      (inst fldl (ea-for-clf-imag-stack x)))))

#+long-float
(define-move-fun (store-complex-long 2) (vop x y)
  ((complex-long-reg) (complex-long-stack))
  (let ((real-tn (complex-long-reg-real-tn x)))
    (cond ((zerop (tn-offset real-tn))
           (store-long-float (ea-for-clf-real-stack y)))
          (t
           (inst fxch real-tn)
           (store-long-float (ea-for-clf-real-stack y))
           (inst fxch real-tn))))
  (let ((imag-tn (complex-long-reg-imag-tn x)))
    (inst fxch imag-tn)
    (store-long-float (ea-for-clf-imag-stack y))
    (inst fxch imag-tn)))


;;;; move VOPs

;;; float register to register moves
(define-vop (float-move)
  (:args (x))
  (:results (y))
  (:note "float move")
  (:generator 0
     (unless (location= x y)
        (cond ((zerop (tn-offset y))
               (copy-fp-reg-to-fr0 x))
              ((zerop (tn-offset x))
               (inst fstd y))
              (t
               (inst fxch x)
               (inst fstd y)
               (inst fxch x))))))

(define-vop (single-move float-move)
  (:args (x :scs (single-reg) :target y :load-if (not (location= x y))))
  (:results (y :scs (single-reg) :load-if (not (location= x y)))))
(define-move-vop single-move :move (single-reg) (single-reg))

(define-vop (double-move float-move)
  (:args (x :scs (double-reg) :target y :load-if (not (location= x y))))
  (:results (y :scs (double-reg) :load-if (not (location= x y)))))
(define-move-vop double-move :move (double-reg) (double-reg))

#+long-float
(define-vop (long-move float-move)
  (:args (x :scs (long-reg) :target y :load-if (not (location= x y))))
  (:results (y :scs (long-reg) :load-if (not (location= x y)))))
#+long-float
(define-move-vop long-move :move (long-reg) (long-reg))

;;; complex float register to register moves
(define-vop (complex-float-move)
  (:args (x :target y :load-if (not (location= x y))))
  (:results (y :load-if (not (location= x y))))
  (:note "complex float move")
  (:generator 0
     (unless (location= x y)
       ;; Note the complex-float-regs are aligned to every second
       ;; float register so there is not need to worry about overlap.
       (let ((x-real (complex-double-reg-real-tn x))
             (y-real (complex-double-reg-real-tn y)))
         (cond ((zerop (tn-offset y-real))
                (copy-fp-reg-to-fr0 x-real))
               ((zerop (tn-offset x-real))
                (inst fstd y-real))
               (t
                (inst fxch x-real)
                (inst fstd y-real)
                (inst fxch x-real))))
       (let ((x-imag (complex-double-reg-imag-tn x))
             (y-imag (complex-double-reg-imag-tn y)))
         (inst fxch x-imag)
         (inst fstd y-imag)
         (inst fxch x-imag)))))

(define-vop (complex-single-move complex-float-move)
  (:args (x :scs (complex-single-reg) :target y
            :load-if (not (location= x y))))
  (:results (y :scs (complex-single-reg) :load-if (not (location= x y)))))
(define-move-vop complex-single-move :move
  (complex-single-reg) (complex-single-reg))

(define-vop (complex-double-move complex-float-move)
  (:args (x :scs (complex-double-reg)
            :target y :load-if (not (location= x y))))
  (:results (y :scs (complex-double-reg) :load-if (not (location= x y)))))
(define-move-vop complex-double-move :move
  (complex-double-reg) (complex-double-reg))

#+long-float
(define-vop (complex-long-move complex-float-move)
  (:args (x :scs (complex-long-reg)
            :target y :load-if (not (location= x y))))
  (:results (y :scs (complex-long-reg) :load-if (not (location= x y)))))
#+long-float
(define-move-vop complex-long-move :move
  (complex-long-reg) (complex-long-reg))

;;; Move from float to a descriptor reg. allocating a new float
;;; object in the process.
(define-vop (move-from-single)
  (:args (x :scs (single-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:node-var node)
  (:note "float to pointer coercion")
  (:generator 13
     (alloc-other y single-float-widetag single-float-size node)
     (with-tn@fp-top(x)
       (inst fst (ea-for-sf-desc y)))))
(define-move-vop move-from-single :move
  (single-reg) (descriptor-reg))

(define-vop (move-from-double)
  (:args (x :scs (double-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:node-var node)
  (:note "float to pointer coercion")
  (:generator 13
     (alloc-other y double-float-widetag double-float-size node)
     (with-tn@fp-top(x)
       (inst fstd (ea-for-df-desc y)))))
(define-move-vop move-from-double :move
  (double-reg) (descriptor-reg))

#+long-float
(define-vop (move-from-long)
  (:args (x :scs (long-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:node-var node)
  (:note "float to pointer coercion")
  (:generator 13
     (alloc-other y long-float-widetag long-float-size node)
     (with-tn@fp-top(x)
       (store-long-float (ea-for-lf-desc y)))))
#+long-float
(define-move-vop move-from-long :move
  (long-reg) (descriptor-reg))

(define-vop (move-from-fp-constant)
  (:args (x :scs (fp-constant)))
  (:results (y :scs (descriptor-reg)))
  (:generator 2
     (ecase (sb-c::constant-value (sb-c::tn-leaf x))
       (0f0 (load-symbol-value y *fp-constant-0f0*))
       (1f0 (load-symbol-value y *fp-constant-1f0*))
       (0d0 (load-symbol-value y *fp-constant-0d0*))
       (1d0 (load-symbol-value y *fp-constant-1d0*))
       #+long-float
       (0l0 (load-symbol-value y *fp-constant-0l0*))
       #+long-float
       (1l0 (load-symbol-value y *fp-constant-1l0*))
       #+long-float
       (#.pi (load-symbol-value y *fp-constant-pi*))
       #+long-float
       (#.(log 10l0 2l0) (load-symbol-value y *fp-constant-l2t*))
       #+long-float
       (#.(log 2.718281828459045235360287471352662L0 2l0)
          (load-symbol-value y *fp-constant-l2e*))
       #+long-float
       (#.(log 2l0 10l0) (load-symbol-value y *fp-constant-lg2*))
       #+long-float
       (#.(log 2l0 2.718281828459045235360287471352662L0)
          (load-symbol-value y *fp-constant-ln2*)))))
(define-move-vop move-from-fp-constant :move
  (fp-constant) (descriptor-reg))

;;; Move from a descriptor to a float register.
(define-vop (move-to-single)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (single-reg)))
  (:note "pointer to float coercion")
  (:generator 2
     (with-empty-tn@fp-top(y)
       (inst fld (ea-for-sf-desc x)))))
(define-move-vop move-to-single :move (descriptor-reg) (single-reg))

(define-vop (move-to-double)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (double-reg)))
  (:note "pointer to float coercion")
  (:generator 2
     (with-empty-tn@fp-top(y)
       (inst fldd (ea-for-df-desc x)))))
(define-move-vop move-to-double :move (descriptor-reg) (double-reg))

#+long-float
(define-vop (move-to-long)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (long-reg)))
  (:note "pointer to float coercion")
  (:generator 2
     (with-empty-tn@fp-top(y)
       (inst fldl (ea-for-lf-desc x)))))
#+long-float
(define-move-vop move-to-long :move (descriptor-reg) (long-reg))

;;; Move from complex float to a descriptor reg. allocating a new
;;; complex float object in the process.
(define-vop (move-from-complex-single)
  (:args (x :scs (complex-single-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:node-var node)
  (:note "complex float to pointer coercion")
  (:generator 13
    (alloc-other y complex-single-float-widetag complex-single-float-size node)
    (let ((real-tn (complex-single-reg-real-tn x)))
      (with-tn@fp-top(real-tn)
        (inst fst (ea-for-csf-real-desc y))))
    (let ((imag-tn (complex-single-reg-imag-tn x)))
      (with-tn@fp-top(imag-tn)
        (inst fst (ea-for-csf-imag-desc y))))))
(define-move-vop move-from-complex-single :move
  (complex-single-reg) (descriptor-reg))

(define-vop (move-from-complex-double)
  (:args (x :scs (complex-double-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:node-var node)
  (:note "complex float to pointer coercion")
  (:generator 13
     (alloc-other y complex-double-float-widetag complex-double-float-size node)
     (let ((real-tn (complex-double-reg-real-tn x)))
       (with-tn@fp-top(real-tn)
         (inst fstd (ea-for-cdf-real-desc y))))
     (let ((imag-tn (complex-double-reg-imag-tn x)))
       (with-tn@fp-top(imag-tn)
         (inst fstd (ea-for-cdf-imag-desc y))))))
(define-move-vop move-from-complex-double :move
  (complex-double-reg) (descriptor-reg))

#+long-float
(define-vop (move-from-complex-long)
  (:args (x :scs (complex-long-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:node-var node)
  (:note "complex float to pointer coercion")
  (:generator 13
     (alloc-other y complex-long-float-widetag complex-long-float-size node)
     (let ((real-tn (complex-long-reg-real-tn x)))
       (with-tn@fp-top(real-tn)
         (store-long-float (ea-for-clf-real-desc y))))
     (let ((imag-tn (complex-long-reg-imag-tn x)))
       (with-tn@fp-top(imag-tn)
         (store-long-float (ea-for-clf-imag-desc y))))))
#+long-float
(define-move-vop move-from-complex-long :move
  (complex-long-reg) (descriptor-reg))

;;; Move from a descriptor to a complex float register.
(macrolet ((frob (name sc format)
             `(progn
                (define-vop (,name)
                  (:args (x :scs (descriptor-reg)))
                  (:results (y :scs (,sc)))
                  (:note "pointer to complex float coercion")
                  (:generator 2
                    (let ((real-tn (complex-double-reg-real-tn y)))
                      (with-empty-tn@fp-top(real-tn)
                        ,@(ecase format
                           (:single '((inst fld (ea-for-csf-real-desc x))))
                           (:double '((inst fldd (ea-for-cdf-real-desc x))))
                           #+long-float
                           (:long '((inst fldl (ea-for-clf-real-desc x)))))))
                    (let ((imag-tn (complex-double-reg-imag-tn y)))
                      (with-empty-tn@fp-top(imag-tn)
                        ,@(ecase format
                           (:single '((inst fld (ea-for-csf-imag-desc x))))
                           (:double '((inst fldd (ea-for-cdf-imag-desc x))))
                           #+long-float
                           (:long '((inst fldl (ea-for-clf-imag-desc x)))))))))
                (define-move-vop ,name :move (descriptor-reg) (,sc)))))
          (frob move-to-complex-single complex-single-reg :single)
          (frob move-to-complex-double complex-double-reg :double)
          #+long-float
          (frob move-to-complex-double complex-long-reg :long))

;;;; the move argument vops
;;;;
;;;; Note these are also used to stuff fp numbers onto the c-call
;;;; stack so the order is different than the lisp-stack.

;;; the general MOVE-ARG VOP
(macrolet ((frob (name sc stack-sc format)
             `(progn
                (define-vop (,name)
                  (:args (x :scs (,sc) :target y)
                         (fp :scs (any-reg)
                             :load-if (not (sc-is y ,sc))))
                  (:results (y))
                  (:note "float argument move")
                  (:generator ,(case format (:single 2) (:double 3) (:long 4))
                    (sc-case y
                      (,sc
                       (unless (location= x y)
                          (cond ((zerop (tn-offset y))
                                 (copy-fp-reg-to-fr0 x))
                                ((zerop (tn-offset x))
                                 (inst fstd y))
                                (t
                                 (inst fxch x)
                                 (inst fstd y)
                                 (inst fxch x)))))
                      (,stack-sc
                       (if (= (tn-offset fp) esp-offset)
                           ;; C-call
                           (let* ((offset (tn-byte-offset y))
                                  (ea (make-ea :dword :base fp :disp offset)))
                             (with-tn@fp-top(x)
                                ,@(ecase format
                                         (:single '((inst fst ea)))
                                         (:double '((inst fstd ea)))
                                         #+long-float
                                         (:long '((store-long-float ea))))))
                           ;; Lisp stack
                           (let ((ea (make-ea
                                      :dword :base fp
                                      :disp (frame-byte-offset
                                             (+ (tn-offset y)
                                                ,(case format
                                                       (:single 0)
                                                       (:double 1)
                                                       (:long 2)))))))
                             (with-tn@fp-top(x)
                               ,@(ecase format
                                    (:single '((inst fst  ea)))
                                    (:double '((inst fstd ea)))
                                    #+long-float
                                    (:long '((store-long-float ea)))))))))))
                (define-move-vop ,name :move-arg
                  (,sc descriptor-reg) (,sc)))))
  (frob move-single-float-arg single-reg single-stack :single)
  (frob move-double-float-arg double-reg double-stack :double)
  #+long-float
  (frob move-long-float-arg long-reg long-stack :long))

;;;; complex float MOVE-ARG VOP
(macrolet ((frob (name sc stack-sc format)
             `(progn
                (define-vop (,name)
                  (:args (x :scs (,sc) :target y)
                         (fp :scs (any-reg)
                             :load-if (not (sc-is y ,sc))))
                  (:results (y))
                  (:note "complex float argument move")
                  (:generator ,(ecase format (:single 2) (:double 3) (:long 4))
                    (sc-case y
                      (,sc
                       (unless (location= x y)
                         (let ((x-real (complex-double-reg-real-tn x))
                               (y-real (complex-double-reg-real-tn y)))
                           (cond ((zerop (tn-offset y-real))
                                  (copy-fp-reg-to-fr0 x-real))
                                 ((zerop (tn-offset x-real))
                                  (inst fstd y-real))
                                 (t
                                  (inst fxch x-real)
                                  (inst fstd y-real)
                                  (inst fxch x-real))))
                         (let ((x-imag (complex-double-reg-imag-tn x))
                               (y-imag (complex-double-reg-imag-tn y)))
                           (inst fxch x-imag)
                           (inst fstd y-imag)
                           (inst fxch x-imag))))
                      (,stack-sc
                       (let ((real-tn (complex-double-reg-real-tn x)))
                         (cond ((zerop (tn-offset real-tn))
                                ,@(ecase format
                                    (:single
                                     '((inst fst
                                        (ea-for-csf-real-stack y fp))))
                                    (:double
                                     '((inst fstd
                                        (ea-for-cdf-real-stack y fp))))
                                    #+long-float
                                    (:long
                                     '((store-long-float
                                        (ea-for-clf-real-stack y fp))))))
                               (t
                                (inst fxch real-tn)
                                ,@(ecase format
                                    (:single
                                     '((inst fst
                                        (ea-for-csf-real-stack y fp))))
                                    (:double
                                     '((inst fstd
                                        (ea-for-cdf-real-stack y fp))))
                                    #+long-float
                                    (:long
                                     '((store-long-float
                                        (ea-for-clf-real-stack y fp)))))
                                (inst fxch real-tn))))
                       (let ((imag-tn (complex-double-reg-imag-tn x)))
                         (inst fxch imag-tn)
                         ,@(ecase format
                             (:single
                              '((inst fst (ea-for-csf-imag-stack y fp))))
                             (:double
                              '((inst fstd (ea-for-cdf-imag-stack y fp))))
                             #+long-float
                             (:long
                              '((store-long-float
                                 (ea-for-clf-imag-stack y fp)))))
                         (inst fxch imag-tn))))))
                (define-move-vop ,name :move-arg
                  (,sc descriptor-reg) (,sc)))))
  (frob move-complex-single-float-arg
        complex-single-reg complex-single-stack :single)
  (frob move-complex-double-float-arg
        complex-double-reg complex-double-stack :double)
  #+long-float
  (frob move-complex-long-float-arg
        complex-long-reg complex-long-stack :long))

(define-move-vop move-arg :move-arg
  (single-reg double-reg #+long-float long-reg
   complex-single-reg complex-double-reg #+long-float complex-long-reg)
  (descriptor-reg))


;;;; arithmetic VOPs

;;; dtc: the floating point arithmetic vops
;;;
;;; Note: Although these can accept x and y on the stack or pointed to
;;; from a descriptor register, they will work with register loading
;;; without these. Same deal with the result - it need only be a
;;; register. When load-tns are needed they will probably be in ST0
;;; and the code below should be able to correctly handle all cases.
;;;
;;; However it seems to produce better code if all arg. and result
;;; options are used; on the P86 there is no extra cost in using a
;;; memory operand to the FP instructions - not so on the PPro.
;;;
;;; It may also be useful to handle constant args?
;;;
;;; 22-Jul-97: descriptor args lose in some simple cases when
;;; a function result computed in a loop. Then Python insists
;;; on consing the intermediate values! For example
;;;
;;; (defun test(a n)
;;;   (declare (type (simple-array double-float (*)) a)
;;;        (fixnum n))
;;;   (let ((sum 0d0))
;;;     (declare (type double-float sum))
;;;   (dotimes (i n)
;;;     (incf sum (* (aref a i)(aref a i))))
;;;     sum))
;;;
;;; So, disabling descriptor args until this can be fixed elsewhere.
(macrolet
    ((frob (op fop-sti fopr-sti
               fop fopr sname scost
               fopd foprd dname dcost
               lname lcost)
       #-long-float (declare (ignore lcost lname))
       `(progn
         (define-vop (,sname)
           (:translate ,op)
           (:args (x :scs (single-reg single-stack #+nil descriptor-reg)
                     :to :eval)
                  (y :scs (single-reg single-stack #+nil descriptor-reg)
                     :to :eval))
           (:temporary (:sc single-reg :offset fr0-offset
                            :from :eval :to :result) fr0)
           (:results (r :scs (single-reg single-stack)))
           (:arg-types single-float single-float)
           (:result-types single-float)
           (:policy :fast-safe)
           (:note "inline float arithmetic")
           (:vop-var vop)
           (:save-p :compute-only)
           (:node-var node)
           (:generator ,scost
             ;; Handle a few special cases
             (cond
              ;; x, y, and r are the same register.
              ((and (sc-is x single-reg) (location= x r) (location= y r))
               (cond ((zerop (tn-offset r))
                      (inst ,fop fr0))
                     (t
                      (inst fxch r)
                      (inst ,fop fr0)
                      ;; XX the source register will not be valid.
                      (note-next-instruction vop :internal-error)
                      (inst fxch r))))

              ;; x and r are the same register.
              ((and (sc-is x single-reg) (location= x r))
               (cond ((zerop (tn-offset r))
                      (sc-case y
                         (single-reg
                          ;; ST(0) = ST(0) op ST(y)
                          (inst ,fop y))
                         (single-stack
                          ;; ST(0) = ST(0) op Mem
                          (inst ,fop (ea-for-sf-stack y)))
                         (descriptor-reg
                          (inst ,fop (ea-for-sf-desc y)))))
                     (t
                      ;; y to ST0
                      (sc-case y
                         (single-reg
                          (unless (zerop (tn-offset y))
                                  (copy-fp-reg-to-fr0 y)))
                         ((single-stack descriptor-reg)
                          (inst fstp fr0)
                          (if (sc-is y single-stack)
                              (inst fld (ea-for-sf-stack y))
                            (inst fld (ea-for-sf-desc y)))))
                      ;; ST(i) = ST(i) op ST0
                      (inst ,fop-sti r)))
               (maybe-fp-wait node vop))
              ;; y and r are the same register.
              ((and (sc-is y single-reg) (location= y r))
               (cond ((zerop (tn-offset r))
                      (sc-case x
                         (single-reg
                          ;; ST(0) = ST(x) op ST(0)
                          (inst ,fopr x))
                         (single-stack
                          ;; ST(0) = Mem op ST(0)
                          (inst ,fopr (ea-for-sf-stack x)))
                         (descriptor-reg
                          (inst ,fopr (ea-for-sf-desc x)))))
                     (t
                      ;; x to ST0
                      (sc-case x
                        (single-reg
                         (unless (zerop (tn-offset x))
                                 (copy-fp-reg-to-fr0 x)))
                        ((single-stack descriptor-reg)
                         (inst fstp fr0)
                         (if (sc-is x single-stack)
                             (inst fld (ea-for-sf-stack x))
                           (inst fld (ea-for-sf-desc x)))))
                      ;; ST(i) = ST(0) op ST(i)
                      (inst ,fopr-sti r)))
               (maybe-fp-wait node vop))
              ;; the default case
              (t
               ;; Get the result to ST0.

               ;; Special handling is needed if x or y are in ST0, and
               ;; simpler code is generated.
               (cond
                ;; x is in ST0
                ((and (sc-is x single-reg) (zerop (tn-offset x)))
                 ;; ST0 = ST0 op y
                 (sc-case y
                   (single-reg
                    (inst ,fop y))
                   (single-stack
                    (inst ,fop (ea-for-sf-stack y)))
                   (descriptor-reg
                    (inst ,fop (ea-for-sf-desc y)))))
                ;; y is in ST0
                ((and (sc-is y single-reg) (zerop (tn-offset y)))
                 ;; ST0 = x op ST0
                 (sc-case x
                   (single-reg
                    (inst ,fopr x))
                   (single-stack
                    (inst ,fopr (ea-for-sf-stack x)))
                   (descriptor-reg
                    (inst ,fopr (ea-for-sf-desc x)))))
                (t
                 ;; x to ST0
                 (sc-case x
                   (single-reg
                    (copy-fp-reg-to-fr0 x))
                   (single-stack
                    (inst fstp fr0)
                    (inst fld (ea-for-sf-stack x)))
                   (descriptor-reg
                    (inst fstp fr0)
                    (inst fld (ea-for-sf-desc x))))
                 ;; ST0 = ST0 op y
                 (sc-case y
                   (single-reg
                    (inst ,fop y))
                   (single-stack
                    (inst ,fop (ea-for-sf-stack y)))
                   (descriptor-reg
                    (inst ,fop (ea-for-sf-desc y))))))

               (note-next-instruction vop :internal-error)

               ;; Finally save the result.
               (sc-case r
                 (single-reg
                  (cond ((zerop (tn-offset r))
                         (maybe-fp-wait node))
                        (t
                         (inst fst r))))
                 (single-stack
                  (inst fst (ea-for-sf-stack r))))))))

         (define-vop (,dname)
           (:translate ,op)
           (:args (x :scs (double-reg double-stack #+nil descriptor-reg)
                     :to :eval)
                  (y :scs (double-reg double-stack #+nil descriptor-reg)
                     :to :eval))
           (:temporary (:sc double-reg :offset fr0-offset
                            :from :eval :to :result) fr0)
           (:results (r :scs (double-reg double-stack)))
           (:arg-types double-float double-float)
           (:result-types double-float)
           (:policy :fast-safe)
           (:note "inline float arithmetic")
           (:vop-var vop)
           (:save-p :compute-only)
           (:node-var node)
           (:generator ,dcost
             ;; Handle a few special cases.
             (cond
              ;; x, y, and r are the same register.
              ((and (sc-is x double-reg) (location= x r) (location= y r))
               (cond ((zerop (tn-offset r))
                      (inst ,fop fr0))
                     (t
                      (inst fxch x)
                      (inst ,fopd fr0)
                      ;; XX the source register will not be valid.
                      (note-next-instruction vop :internal-error)
                      (inst fxch r))))

              ;; x and r are the same register.
              ((and (sc-is x double-reg) (location= x r))
               (cond ((zerop (tn-offset r))
                      (sc-case y
                         (double-reg
                          ;; ST(0) = ST(0) op ST(y)
                          (inst ,fopd y))
                         (double-stack
                          ;; ST(0) = ST(0) op Mem
                          (inst ,fopd (ea-for-df-stack y)))
                         (descriptor-reg
                          (inst ,fopd (ea-for-df-desc y)))))
                     (t
                      ;; y to ST0
                      (sc-case y
                         (double-reg
                          (unless (zerop (tn-offset y))
                                  (copy-fp-reg-to-fr0 y)))
                         ((double-stack descriptor-reg)
                          (inst fstp fr0)
                          (if (sc-is y double-stack)
                              (inst fldd (ea-for-df-stack y))
                            (inst fldd (ea-for-df-desc y)))))
                      ;; ST(i) = ST(i) op ST0
                      (inst ,fop-sti r)))
               (maybe-fp-wait node vop))
              ;; y and r are the same register.
              ((and (sc-is y double-reg) (location= y r))
               (cond ((zerop (tn-offset r))
                      (sc-case x
                         (double-reg
                          ;; ST(0) = ST(x) op ST(0)
                          (inst ,foprd x))
                         (double-stack
                          ;; ST(0) = Mem op ST(0)
                          (inst ,foprd (ea-for-df-stack x)))
                         (descriptor-reg
                          (inst ,foprd (ea-for-df-desc x)))))
                     (t
                      ;; x to ST0
                      (sc-case x
                         (double-reg
                          (unless (zerop (tn-offset x))
                                  (copy-fp-reg-to-fr0 x)))
                         ((double-stack descriptor-reg)
                          (inst fstp fr0)
                          (if (sc-is x double-stack)
                              (inst fldd (ea-for-df-stack x))
                            (inst fldd (ea-for-df-desc x)))))
                      ;; ST(i) = ST(0) op ST(i)
                      (inst ,fopr-sti r)))
               (maybe-fp-wait node vop))
              ;; the default case
              (t
               ;; Get the result to ST0.

               ;; Special handling is needed if x or y are in ST0, and
               ;; simpler code is generated.
               (cond
                ;; x is in ST0
                ((and (sc-is x double-reg) (zerop (tn-offset x)))
                 ;; ST0 = ST0 op y
                 (sc-case y
                   (double-reg
                    (inst ,fopd y))
                   (double-stack
                    (inst ,fopd (ea-for-df-stack y)))
                   (descriptor-reg
                    (inst ,fopd (ea-for-df-desc y)))))
                ;; y is in ST0
                ((and (sc-is y double-reg) (zerop (tn-offset y)))
                 ;; ST0 = x op ST0
                 (sc-case x
                   (double-reg
                    (inst ,foprd x))
                   (double-stack
                    (inst ,foprd (ea-for-df-stack x)))
                   (descriptor-reg
                    (inst ,foprd (ea-for-df-desc x)))))
                (t
                 ;; x to ST0
                 (sc-case x
                   (double-reg
                    (copy-fp-reg-to-fr0 x))
                   (double-stack
                    (inst fstp fr0)
                    (inst fldd (ea-for-df-stack x)))
                   (descriptor-reg
                    (inst fstp fr0)
                    (inst fldd (ea-for-df-desc x))))
                 ;; ST0 = ST0 op y
                 (sc-case y
                   (double-reg
                    (inst ,fopd y))
                   (double-stack
                    (inst ,fopd (ea-for-df-stack y)))
                   (descriptor-reg
                    (inst ,fopd (ea-for-df-desc y))))))

               (note-next-instruction vop :internal-error)

               ;; Finally save the result.
               (sc-case r
                 (double-reg
                  (cond ((zerop (tn-offset r))
                         (maybe-fp-wait node))
                        (t
                         (inst fst r))))
                 (double-stack
                  (inst fstd (ea-for-df-stack r))))))))

         #+long-float
         (define-vop (,lname)
           (:translate ,op)
           (:args (x :scs (long-reg) :to :eval)
                  (y :scs (long-reg) :to :eval))
           (:temporary (:sc long-reg :offset fr0-offset
                            :from :eval :to :result) fr0)
           (:results (r :scs (long-reg)))
           (:arg-types long-float long-float)
           (:result-types long-float)
           (:policy :fast-safe)
           (:note "inline float arithmetic")
           (:vop-var vop)
           (:save-p :compute-only)
           (:node-var node)
           (:generator ,lcost
             ;; Handle a few special cases.
             (cond
              ;; x, y, and r are the same register.
              ((and (location= x r) (location= y r))
               (cond ((zerop (tn-offset r))
                      (inst ,fop fr0))
                     (t
                      (inst fxch x)
                      (inst ,fopd fr0)
                      ;; XX the source register will not be valid.
                      (note-next-instruction vop :internal-error)
                      (inst fxch r))))

              ;; x and r are the same register.
              ((location= x r)
               (cond ((zerop (tn-offset r))
                      ;; ST(0) = ST(0) op ST(y)
                      (inst ,fopd y))
                     (t
                      ;; y to ST0
                      (unless (zerop (tn-offset y))
                        (copy-fp-reg-to-fr0 y))
                      ;; ST(i) = ST(i) op ST0
                      (inst ,fop-sti r)))
               (maybe-fp-wait node vop))
              ;; y and r are the same register.
              ((location= y r)
               (cond ((zerop (tn-offset r))
                      ;; ST(0) = ST(x) op ST(0)
                      (inst ,foprd x))
                     (t
                      ;; x to ST0
                      (unless (zerop (tn-offset x))
                        (copy-fp-reg-to-fr0 x))
                      ;; ST(i) = ST(0) op ST(i)
                      (inst ,fopr-sti r)))
               (maybe-fp-wait node vop))
              ;; the default case
              (t
               ;; Get the result to ST0.

               ;; Special handling is needed if x or y are in ST0, and
               ;; simpler code is generated.
               (cond
                ;; x is in ST0.
                ((zerop (tn-offset x))
                 ;; ST0 = ST0 op y
                 (inst ,fopd y))
                ;; y is in ST0
                ((zerop (tn-offset y))
                 ;; ST0 = x op ST0
                 (inst ,foprd x))
                (t
                 ;; x to ST0
                 (copy-fp-reg-to-fr0 x)
                 ;; ST0 = ST0 op y
                 (inst ,fopd y)))

               (note-next-instruction vop :internal-error)

               ;; Finally save the result.
               (cond ((zerop (tn-offset r))
                      (maybe-fp-wait node))
                     (t
                      (inst fst r))))))))))

    (frob + fadd-sti fadd-sti
          fadd fadd +/single-float 2
          faddd faddd +/double-float 2
          +/long-float 2)
    (frob - fsub-sti fsubr-sti
          fsub fsubr -/single-float 2
          fsubd fsubrd -/double-float 2
          -/long-float 2)
    (frob * fmul-sti fmul-sti
          fmul fmul */single-float 3
          fmuld fmuld */double-float 3
          */long-float 3)
    (frob / fdiv-sti fdivr-sti
          fdiv fdivr //single-float 12
          fdivd fdivrd //double-float 12
          //long-float 12))

(macrolet ((frob (name inst translate sc type)
             `(define-vop (,name)
               (:args (x :scs (,sc) :target fr0))
               (:results (y :scs (,sc)))
               (:translate ,translate)
               (:policy :fast-safe)
               (:arg-types ,type)
               (:result-types ,type)
               (:temporary (:sc double-reg :offset fr0-offset
                                :from :argument :to :result) fr0)
               (:ignore fr0)
               (:note "inline float arithmetic")
               (:vop-var vop)
               (:save-p :compute-only)
               (:generator 1
                (note-this-location vop :internal-error)
                (unless (zerop (tn-offset x))
                  (inst fxch x)         ; x to top of stack
                  (unless (location= x y)
                    (inst fst x)))      ; Maybe save it.
                (inst ,inst)            ; Clobber st0.
                (unless (zerop (tn-offset y))
                  (inst fst y))))))

  (frob abs/single-float fabs abs single-reg single-float)
  (frob abs/double-float fabs abs double-reg double-float)
  #+long-float
  (frob abs/long-float fabs abs long-reg long-float)
  (frob %negate/single-float fchs %negate single-reg single-float)
  (frob %negate/double-float fchs %negate double-reg double-float)
  #+long-float
  (frob %negate/long-float fchs %negate long-reg long-float))

;;;; comparison

(define-vop (=/float)
  (:args (x) (y))
  (:temporary (:sc word-reg :offset eax-offset :from :eval) temp)
  (:conditional :e)
  (:policy :fast-safe)
  (:vop-var vop)
  (:save-p :compute-only)
  (:note "inline float comparison")
  (:ignore temp)
  (:generator 3
     (note-this-location vop :internal-error)
     (cond
      ;; x is in ST0; y is in any reg.
      ((zerop (tn-offset x))
       (inst fucom y))
      ;; y is in ST0; x is in another reg.
      ((zerop (tn-offset y))
       (inst fucom x))
      ;; x and y are the same register, not ST0
      ((location= x y)
       (inst fxch x)
       (inst fucom fr0-tn)
       (inst fxch x))
      ;; x and y are different registers, neither ST0.
      (t
       (inst fxch x)
       (inst fucom y)
       (inst fxch x)))
     (inst fnstsw)                      ; status word to ax
     (inst and ah-tn #x45)              ; C3 C2 C0
     (inst cmp ah-tn #x40)))

(define-vop (=/single-float =/float)
  (:translate =)
  (:args (x :scs (single-reg))
         (y :scs (single-reg)))
  (:arg-types single-float single-float))

(define-vop (=/double-float =/float)
  (:translate =)
  (:args (x :scs (double-reg))
         (y :scs (double-reg)))
  (:arg-types double-float double-float))

#+long-float
(define-vop (=/long-float =/float)
  (:translate =)
  (:args (x :scs (long-reg))
         (y :scs (long-reg)))
  (:arg-types long-float long-float))

(define-vop (<single-float)
  (:translate <)
  (:args (x :scs (single-reg single-stack descriptor-reg))
         (y :scs (single-reg single-stack descriptor-reg)))
  (:arg-types single-float single-float)
  (:temporary (:sc single-reg :offset fr0-offset :from :eval) fr0)
  (:temporary (:sc word-reg :offset eax-offset :from :eval) temp)
  (:conditional :e)
  (:policy :fast-safe)
  (:note "inline float comparison")
  (:ignore temp)
  (:generator 3
    ;; Handle a few special cases.
    (cond
     ;; y is ST0.
     ((and (sc-is y single-reg) (zerop (tn-offset y)))
      (sc-case x
        (single-reg
         (inst fcom x))
        ((single-stack descriptor-reg)
         (if (sc-is x single-stack)
             (inst fcom (ea-for-sf-stack x))
           (inst fcom (ea-for-sf-desc x)))))
      (inst fnstsw)                     ; status word to ax
      (inst and ah-tn #x45))

     ;; general case when y is not in ST0
     (t
      ;; x to ST0
      (sc-case x
         (single-reg
          (unless (zerop (tn-offset x))
                  (copy-fp-reg-to-fr0 x)))
         ((single-stack descriptor-reg)
          (inst fstp fr0)
          (if (sc-is x single-stack)
              (inst fld (ea-for-sf-stack x))
            (inst fld (ea-for-sf-desc x)))))
      (sc-case y
        (single-reg
         (inst fcom y))
        ((single-stack descriptor-reg)
         (if (sc-is y single-stack)
             (inst fcom (ea-for-sf-stack y))
           (inst fcom (ea-for-sf-desc y)))))
      (inst fnstsw)                     ; status word to ax
      (inst and ah-tn #x45)             ; C3 C2 C0
      (inst cmp ah-tn #x01)))))

(define-vop (<double-float)
  (:translate <)
  (:args (x :scs (double-reg double-stack descriptor-reg))
         (y :scs (double-reg double-stack descriptor-reg)))
  (:arg-types double-float double-float)
  (:temporary (:sc double-reg :offset fr0-offset :from :eval) fr0)
  (:temporary (:sc word-reg :offset eax-offset :from :eval) temp)
  (:conditional :e)
  (:policy :fast-safe)
  (:note "inline float comparison")
  (:ignore temp)
  (:generator 3
    ;; Handle a few special cases
    (cond
     ;; y is ST0.
     ((and (sc-is y double-reg) (zerop (tn-offset y)))
      (sc-case x
        (double-reg
         (inst fcomd x))
        ((double-stack descriptor-reg)
         (if (sc-is x double-stack)
             (inst fcomd (ea-for-df-stack x))
           (inst fcomd (ea-for-df-desc x)))))
      (inst fnstsw)                     ; status word to ax
      (inst and ah-tn #x45))

     ;; General case when y is not in ST0.
     (t
      ;; x to ST0
      (sc-case x
         (double-reg
          (unless (zerop (tn-offset x))
                  (copy-fp-reg-to-fr0 x)))
         ((double-stack descriptor-reg)
          (inst fstp fr0)
          (if (sc-is x double-stack)
              (inst fldd (ea-for-df-stack x))
            (inst fldd (ea-for-df-desc x)))))
      (sc-case y
        (double-reg
         (inst fcomd y))
        ((double-stack descriptor-reg)
         (if (sc-is y double-stack)
             (inst fcomd (ea-for-df-stack y))
           (inst fcomd (ea-for-df-desc y)))))
      (inst fnstsw)                     ; status word to ax
      (inst and ah-tn #x45)             ; C3 C2 C0
      (inst cmp ah-tn #x01)))))

#+long-float
(define-vop (<long-float)
  (:translate <)
  (:args (x :scs (long-reg))
         (y :scs (long-reg)))
  (:arg-types long-float long-float)
  (:temporary (:sc word-reg :offset eax-offset :from :eval) temp)
  (:conditional :e)
  (:policy :fast-safe)
  (:note "inline float comparison")
  (:ignore temp)
  (:generator 3
    (cond
      ;; x is in ST0; y is in any reg.
      ((zerop (tn-offset x))
       (inst fcomd y)
       (inst fnstsw)                    ; status word to ax
       (inst and ah-tn #x45)            ; C3 C2 C0
       (inst cmp ah-tn #x01))
      ;; y is in ST0; x is in another reg.
      ((zerop (tn-offset y))
       (inst fcomd x)
       (inst fnstsw)                    ; status word to ax
       (inst and ah-tn #x45))
      ;; x and y are the same register, not ST0
      ;; x and y are different registers, neither ST0.
      (t
       (inst fxch y)
       (inst fcomd x)
       (inst fxch y)
       (inst fnstsw)                    ; status word to ax
       (inst and ah-tn #x45)))))        ; C3 C2 C0


(define-vop (>single-float)
  (:translate >)
  (:args (x :scs (single-reg single-stack descriptor-reg))
         (y :scs (single-reg single-stack descriptor-reg)))
  (:arg-types single-float single-float)
  (:temporary (:sc single-reg :offset fr0-offset :from :eval) fr0)
  (:temporary (:sc word-reg :offset eax-offset :from :eval) temp)
  (:conditional :e)
  (:policy :fast-safe)
  (:note "inline float comparison")
  (:ignore temp)
  (:generator 3
    ;; Handle a few special cases.
    (cond
     ;; y is ST0.
     ((and (sc-is y single-reg) (zerop (tn-offset y)))
      (sc-case x
        (single-reg
         (inst fcom x))
        ((single-stack descriptor-reg)
         (if (sc-is x single-stack)
             (inst fcom (ea-for-sf-stack x))
           (inst fcom (ea-for-sf-desc x)))))
      (inst fnstsw)                     ; status word to ax
      (inst and ah-tn #x45)
      (inst cmp ah-tn #x01))

     ;; general case when y is not in ST0
     (t
      ;; x to ST0
      (sc-case x
         (single-reg
          (unless (zerop (tn-offset x))
                  (copy-fp-reg-to-fr0 x)))
         ((single-stack descriptor-reg)
          (inst fstp fr0)
          (if (sc-is x single-stack)
              (inst fld (ea-for-sf-stack x))
            (inst fld (ea-for-sf-desc x)))))
      (sc-case y
        (single-reg
         (inst fcom y))
        ((single-stack descriptor-reg)
         (if (sc-is y single-stack)
             (inst fcom (ea-for-sf-stack y))
           (inst fcom (ea-for-sf-desc y)))))
      (inst fnstsw)                     ; status word to ax
      (inst and ah-tn #x45)))))

(define-vop (>double-float)
  (:translate >)
  (:args (x :scs (double-reg double-stack descriptor-reg))
         (y :scs (double-reg double-stack descriptor-reg)))
  (:arg-types double-float double-float)
  (:temporary (:sc double-reg :offset fr0-offset :from :eval) fr0)
  (:temporary (:sc word-reg :offset eax-offset :from :eval) temp)
  (:conditional :e)
  (:policy :fast-safe)
  (:note "inline float comparison")
  (:ignore temp)
  (:generator 3
    ;; Handle a few special cases.
    (cond
     ;; y is ST0.
     ((and (sc-is y double-reg) (zerop (tn-offset y)))
      (sc-case x
        (double-reg
         (inst fcomd x))
        ((double-stack descriptor-reg)
         (if (sc-is x double-stack)
             (inst fcomd (ea-for-df-stack x))
           (inst fcomd (ea-for-df-desc x)))))
      (inst fnstsw)                     ; status word to ax
      (inst and ah-tn #x45)
      (inst cmp ah-tn #x01))

     ;; general case when y is not in ST0
     (t
      ;; x to ST0
      (sc-case x
         (double-reg
          (unless (zerop (tn-offset x))
                  (copy-fp-reg-to-fr0 x)))
         ((double-stack descriptor-reg)
          (inst fstp fr0)
          (if (sc-is x double-stack)
              (inst fldd (ea-for-df-stack x))
            (inst fldd (ea-for-df-desc x)))))
      (sc-case y
        (double-reg
         (inst fcomd y))
        ((double-stack descriptor-reg)
         (if (sc-is y double-stack)
             (inst fcomd (ea-for-df-stack y))
           (inst fcomd (ea-for-df-desc y)))))
      (inst fnstsw)                     ; status word to ax
      (inst and ah-tn #x45)))))

#+long-float
(define-vop (>long-float)
  (:translate >)
  (:args (x :scs (long-reg))
         (y :scs (long-reg)))
  (:arg-types long-float long-float)
  (:temporary (:sc word-reg :offset eax-offset :from :eval) temp)
  (:conditional :e)
  (:policy :fast-safe)
  (:note "inline float comparison")
  (:ignore temp)
  (:generator 3
    (cond
      ;; y is in ST0; x is in any reg.
      ((zerop (tn-offset y))
       (inst fcomd x)
       (inst fnstsw)                    ; status word to ax
       (inst and ah-tn #x45)
       (inst cmp ah-tn #x01))
      ;; x is in ST0; y is in another reg.
      ((zerop (tn-offset x))
       (inst fcomd y)
       (inst fnstsw)                    ; status word to ax
       (inst and ah-tn #x45))
      ;; y and x are the same register, not ST0
      ;; y and x are different registers, neither ST0.
      (t
       (inst fxch x)
       (inst fcomd y)
       (inst fxch x)
       (inst fnstsw)                    ; status word to ax
       (inst and ah-tn #x45)))))

;;; Comparisons with 0 can use the FTST instruction.

(define-vop (float-test)
  (:args (x))
  (:temporary (:sc word-reg :offset eax-offset :from :eval) temp)
  (:conditional :e)
  (:info y)
  (:variant-vars code)
  (:policy :fast-safe)
  (:vop-var vop)
  (:save-p :compute-only)
  (:note "inline float comparison")
  (:ignore temp y)
  (:generator 2
     (note-this-location vop :internal-error)
     (cond
      ;; x is in ST0
      ((zerop (tn-offset x))
       (inst ftst))
      ;; x not ST0
      (t
       (inst fxch x)
       (inst ftst)
       (inst fxch x)))
     (inst fnstsw)                      ; status word to ax
     (inst and ah-tn #x45)              ; C3 C2 C0
     (unless (zerop code)
        (inst cmp ah-tn code))))

(define-vop (=0/single-float float-test)
  (:translate =)
  (:args (x :scs (single-reg)))
  (:arg-types single-float (:constant (single-float 0f0 0f0)))
  (:variant #x40))
(define-vop (=0/double-float float-test)
  (:translate =)
  (:args (x :scs (double-reg)))
  (:arg-types double-float (:constant (double-float 0d0 0d0)))
  (:variant #x40))
#+long-float
(define-vop (=0/long-float float-test)
  (:translate =)
  (:args (x :scs (long-reg)))
  (:arg-types long-float (:constant (long-float 0l0 0l0)))
  (:variant #x40))

(define-vop (<0/single-float float-test)
  (:translate <)
  (:args (x :scs (single-reg)))
  (:arg-types single-float (:constant (single-float 0f0 0f0)))
  (:variant #x01))
(define-vop (<0/double-float float-test)
  (:translate <)
  (:args (x :scs (double-reg)))
  (:arg-types double-float (:constant (double-float 0d0 0d0)))
  (:variant #x01))
#+long-float
(define-vop (<0/long-float float-test)
  (:translate <)
  (:args (x :scs (long-reg)))
  (:arg-types long-float (:constant (long-float 0l0 0l0)))
  (:variant #x01))

(define-vop (>0/single-float float-test)
  (:translate >)
  (:args (x :scs (single-reg)))
  (:arg-types single-float (:constant (single-float 0f0 0f0)))
  (:variant #x00))
(define-vop (>0/double-float float-test)
  (:translate >)
  (:args (x :scs (double-reg)))
  (:arg-types double-float (:constant (double-float 0d0 0d0)))
  (:variant #x00))
#+long-float
(define-vop (>0/long-float float-test)
  (:translate >)
  (:args (x :scs (long-reg)))
  (:arg-types long-float (:constant (long-float 0l0 0l0)))
  (:variant #x00))

#+long-float
(deftransform eql ((x y) (long-float long-float))
  `(and (= (long-float-low-bits x) (long-float-low-bits y))
        (= (long-float-high-bits x) (long-float-high-bits y))
        (= (long-float-exp-bits x) (long-float-exp-bits y))))

;;;; conversion

(macrolet ((frob (name translate to-sc to-type)
             `(define-vop (,name)
                (:args (x :scs (signed-stack signed-reg) :target temp))
                (:temporary (:sc signed-stack) temp)
                (:results (y :scs (,to-sc)))
                (:arg-types signed-num)
                (:result-types ,to-type)
                (:policy :fast-safe)
                (:note "inline float coercion")
                (:translate ,translate)
                (:vop-var vop)
                (:save-p :compute-only)
                (:generator 5
                  (sc-case x
                    (signed-reg
                     (inst mov temp x)
                     (with-empty-tn@fp-top(y)
                       (note-this-location vop :internal-error)
                       (inst fild temp)))
                    (signed-stack
                     (with-empty-tn@fp-top(y)
                       (note-this-location vop :internal-error)
                       (inst fild x))))))))
  (frob %single-float/signed %single-float single-reg single-float)
  (frob %double-float/signed %double-float double-reg double-float)
  #+long-float
  (frob %long-float/signed %long-float long-reg long-float))

(macrolet ((frob (name translate to-sc to-type)
             `(define-vop (,name)
                (:args (x :scs (unsigned-reg)))
                (:results (y :scs (,to-sc)))
                (:arg-types unsigned-num)
                (:result-types ,to-type)
                (:policy :fast-safe)
                (:note "inline float coercion")
                (:translate ,translate)
                (:vop-var vop)
                (:save-p :compute-only)
                (:generator 6
                 (inst push 0)
                 (inst push x)
                 (with-empty-tn@fp-top(y)
                   (note-this-location vop :internal-error)
                   (inst fildl (make-ea :dword :base esp-tn)))
                 (inst add esp-tn 8)))))
  (frob %single-float/unsigned %single-float single-reg single-float)
  (frob %double-float/unsigned %double-float double-reg double-float)
  #+long-float
  (frob %long-float/unsigned %long-float long-reg long-float))

(macrolet ((frob (name translate from-sc from-type to-sc to-type
                  &optional to-stack-sc store-inst load-inst)
             `(define-vop (,name)
               (:args (x :scs (,from-sc) :target y))
                ,@(and to-stack-sc
                       `((:temporary (:sc ,to-stack-sc) temp)))
               (:results (y :scs (,to-sc)))
               (:arg-types ,from-type)
               (:result-types ,to-type)
               (:policy :fast-safe)
               (:note "inline float coercion")
               (:translate ,translate)
               (:vop-var vop)
               (:save-p :compute-only)
               (:generator 2
                 (note-this-location vop :internal-error)
                ,(if to-stack-sc
                     `(progn
                        (with-tn@fp-top (x)
                          (inst ,store-inst temp))
                        (with-empty-tn@fp-top (y)
                          (inst ,load-inst temp)))
                     `(unless (location= x y)
                        (cond
                          ((zerop (tn-offset x))
                           ;; x is in ST0, y is in another reg. not ST0
                           (inst fst  y))
                          ((zerop (tn-offset y))
                           ;; y is in ST0, x is in another reg. not ST0
                           (copy-fp-reg-to-fr0 x))
                          (t
                           ;; Neither x or y are in ST0, and they are not in
                           ;; the same reg.
                           (inst fxch x)
                           (inst fst  y)
                           (inst fxch x)))))))))

  (frob %single-float/double-float %single-float double-reg double-float
        single-reg single-float
        single-stack fst fld)
  #+long-float
  (frob %single-float/long-float %single-float long-reg
        long-float single-reg single-float
        single-stack fst fld)
  (frob %double-float/single-float %double-float single-reg single-float
        double-reg double-float)
  #+long-float
  (frob %double-float/long-float %double-float long-reg long-float
        double-reg double-float
        double-stack fstd fldd)
  #+long-float
  (frob %long-float/single-float %long-float single-reg single-float
        long-reg long-float)
  #+long-float
  (frob %long-float/double-float %long-float double-reg double-float
        long-reg long-float))

(macrolet ((frob (trans from-sc from-type round-p)
             `(define-vop (,(symbolicate trans "/" from-type))
               (:args (x :scs (,from-sc)))
               (:temporary (:sc signed-stack) stack-temp)
               ,@(unless round-p
                       '((:temporary (:sc unsigned-stack) scw)
                         (:temporary (:sc any-reg) rcw)))
               (:results (y :scs (signed-reg)))
               (:arg-types ,from-type)
               (:result-types signed-num)
               (:translate ,trans)
               (:policy :fast-safe)
               (:note "inline float truncate")
               (:vop-var vop)
               (:save-p :compute-only)
               (:generator 5
                ,@(unless round-p
                   '((note-this-location vop :internal-error)
                     ;; Catch any pending FPE exceptions.
                     (inst wait)))
                (,@(if round-p '(progn) '(pseudo-atomic ()))
                 ;; Normal mode (for now) is "round to best".
                 (with-tn@fp-top (x)
                   ,@(unless round-p
                     '((inst fnstcw scw) ; save current control word
                       (move rcw scw)   ; into 16-bit register
                       (inst or rcw (ash #b11 10)) ; CHOP
                       (move stack-temp rcw)
                       (inst fldcw stack-temp)))
                   (sc-case y
                     (signed-stack
                      (inst fist y))
                     (signed-reg
                      (inst fist stack-temp)
                      (inst mov y stack-temp)))
                   ,@(unless round-p
                      '((inst fldcw scw)))))))))
  (frob %unary-truncate/single-float single-reg single-float nil)
  (frob %unary-truncate/double-float double-reg double-float nil)
  #+long-float
  (frob %unary-truncate/long-float long-reg long-float nil)
  (frob %unary-round single-reg single-float t)
  (frob %unary-round double-reg double-float t)
  #+long-float
  (frob %unary-round long-reg long-float t))

(macrolet ((frob (trans from-sc from-type round-p)
             `(define-vop (,(symbolicate trans "/" from-type "=>UNSIGNED"))
               (:args (x :scs (,from-sc) :target fr0))
               (:temporary (:sc double-reg :offset fr0-offset
                            :from :argument :to :result) fr0)
               ,@(unless round-p
                  '((:temporary (:sc unsigned-stack) stack-temp)
                    (:temporary (:sc unsigned-stack) scw)
                    (:temporary (:sc any-reg) rcw)))
               (:results (y :scs (unsigned-reg)))
               (:arg-types ,from-type)
               (:result-types unsigned-num)
               (:translate ,trans)
               (:policy :fast-safe)
               (:note "inline float truncate")
               (:vop-var vop)
               (:save-p :compute-only)
               (:generator 5
                ,@(unless round-p
                   '((note-this-location vop :internal-error)
                     ;; Catch any pending FPE exceptions.
                     (inst wait)))
                ;; Normal mode (for now) is "round to best".
                (unless (zerop (tn-offset x))
                  (copy-fp-reg-to-fr0 x))
                ,@(unless round-p
                   '((inst fnstcw scw)  ; save current control word
                     (move rcw scw)     ; into 16-bit register
                     (inst or rcw (ash #b11 10)) ; CHOP
                     (move stack-temp rcw)
                     (inst fldcw stack-temp)))
                (inst sub esp-tn 8)
                (inst fistpl (make-ea :dword :base esp-tn))
                (inst pop y)
                (inst fld fr0) ; copy fr0 to at least restore stack.
                (inst add esp-tn 4)
                ,@(unless round-p
                   '((inst fldcw scw)))))))
  (frob %unary-truncate/single-float single-reg single-float nil)
  (frob %unary-truncate/double-float double-reg double-float nil)
  #+long-float
  (frob %unary-truncate/long-float long-reg long-float nil)
  (frob %unary-round single-reg single-float t)
  (frob %unary-round double-reg double-float t)
  #+long-float
  (frob %unary-round long-reg long-float t))

(define-vop (make-single-float)
  (:args (bits :scs (signed-reg) :target res
               :load-if (not (or (and (sc-is bits signed-stack)
                                      (sc-is res single-reg))
                                 (and (sc-is bits signed-stack)
                                      (sc-is res single-stack)
                                      (location= bits res))))))
  (:results (res :scs (single-reg single-stack)))
  (:temporary (:sc signed-stack) stack-temp)
  (:arg-types signed-num)
  (:result-types single-float)
  (:translate make-single-float)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 4
    (sc-case res
       (single-stack
        (sc-case bits
          (signed-reg
           (inst mov res bits))
          (signed-stack
           (aver (location= bits res)))))
       (single-reg
        (sc-case bits
          (signed-reg
           ;; source must be in memory
           (inst mov stack-temp bits)
           (with-empty-tn@fp-top(res)
              (inst fld stack-temp)))
          (signed-stack
           (with-empty-tn@fp-top(res)
              (inst fld bits))))))))

(define-vop (make-double-float)
  (:args (hi-bits :scs (signed-reg))
         (lo-bits :scs (unsigned-reg)))
  (:results (res :scs (double-reg)))
  (:temporary (:sc double-stack) temp)
  (:arg-types signed-num unsigned-num)
  (:result-types double-float)
  (:translate make-double-float)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (let ((offset (tn-offset temp)))
      (storew hi-bits ebp-tn (frame-word-offset offset))
      (storew lo-bits ebp-tn (frame-word-offset (1+ offset)))
      (with-empty-tn@fp-top(res)
        (inst fldd (make-ea :dword :base ebp-tn
                            :disp (frame-byte-offset (1+ offset))))))))

#+long-float
(define-vop (make-long-float)
  (:args (exp-bits :scs (signed-reg))
         (hi-bits :scs (unsigned-reg))
         (lo-bits :scs (unsigned-reg)))
  (:results (res :scs (long-reg)))
  (:temporary (:sc long-stack) temp)
  (:arg-types signed-num unsigned-num unsigned-num)
  (:result-types long-float)
  (:translate make-long-float)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 3
    (let ((offset (tn-offset temp)))
      (storew exp-bits ebp-tn (frame-word-offset offset))
      (storew hi-bits ebp-tn (frame-word-offset (1+ offset)))
      (storew lo-bits ebp-tn (frame-word-offset (+ offset 2)))
      (with-empty-tn@fp-top(res)
        (inst fldl (make-ea :dword :base ebp-tn
                            :disp (frame-byte-offset (+ offset 2))))))))

(define-vop (single-float-bits)
  (:args (float :scs (single-reg descriptor-reg)
                :load-if (not (sc-is float single-stack))))
  (:results (bits :scs (signed-reg)))
  (:temporary (:sc signed-stack :from :argument :to :result) stack-temp)
  (:arg-types single-float)
  (:result-types signed-num)
  (:translate single-float-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 4
    (sc-case bits
      (signed-reg
       (sc-case float
         (single-reg
          (with-tn@fp-top(float)
            (inst fst stack-temp)
            (inst mov bits stack-temp)))
         (single-stack
          (inst mov bits float))
         (descriptor-reg
          (loadw
           bits float single-float-value-slot
           other-pointer-lowtag))))
      (signed-stack
       (sc-case float
         (single-reg
          (with-tn@fp-top(float)
            (inst fst bits))))))))

(define-vop (double-float-high-bits)
  (:args (float :scs (double-reg descriptor-reg)
                :load-if (not (sc-is float double-stack))))
  (:results (hi-bits :scs (signed-reg)))
  (:temporary (:sc double-stack) temp)
  (:arg-types double-float)
  (:result-types signed-num)
  (:translate double-float-high-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
     (sc-case float
       (double-reg
        (with-tn@fp-top(float)
          (let ((where (make-ea :dword :base ebp-tn
                                :disp (frame-byte-offset (1+ (tn-offset temp))))))
            (inst fstd where)))
        (loadw hi-bits ebp-tn (frame-word-offset (tn-offset temp))))
       (double-stack
        (loadw hi-bits ebp-tn (frame-word-offset (tn-offset float))))
       (descriptor-reg
        (loadw hi-bits float (1+ double-float-value-slot)
               other-pointer-lowtag)))))

(define-vop (double-float-low-bits)
  (:args (float :scs (double-reg descriptor-reg)
                :load-if (not (sc-is float double-stack))))
  (:results (lo-bits :scs (unsigned-reg)))
  (:temporary (:sc double-stack) temp)
  (:arg-types double-float)
  (:result-types unsigned-num)
  (:translate double-float-low-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
     (sc-case float
       (double-reg
        (with-tn@fp-top(float)
          (let ((where (make-ea :dword :base ebp-tn
                                :disp (frame-byte-offset (1+ (tn-offset temp))))))
            (inst fstd where)))
        (loadw lo-bits ebp-tn (frame-word-offset (1+ (tn-offset temp)))))
       (double-stack
        (loadw lo-bits ebp-tn (frame-word-offset (1+ (tn-offset float)))))
       (descriptor-reg
        (loadw lo-bits float double-float-value-slot
               other-pointer-lowtag)))))

#+long-float
(define-vop (long-float-exp-bits)
  (:args (float :scs (long-reg descriptor-reg)
                :load-if (not (sc-is float long-stack))))
  (:results (exp-bits :scs (signed-reg)))
  (:temporary (:sc long-stack) temp)
  (:arg-types long-float)
  (:result-types signed-num)
  (:translate long-float-exp-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
     (sc-case float
       (long-reg
        (with-tn@fp-top(float)
          (let ((where (make-ea :dword :base ebp-tn
                                :disp (frame-byte-offset (+ 2 (tn-offset temp))))))
            (store-long-float where)))
        (inst movsx exp-bits
              (make-ea :word :base ebp-tn
                       :disp (frame-byte-offset (tn-offset temp)))))
       (long-stack
        (inst movsx exp-bits
              (make-ea :word :base ebp-tn
                       :disp (frame-byte-offset (tn-offset temp)))))
       (descriptor-reg
        (inst movsx exp-bits
              (object-slot-ea float (+ 2 long-float-value-slot)
                                       other-pointer-lowtag :word))))))

#+long-float
(define-vop (long-float-high-bits)
  (:args (float :scs (long-reg descriptor-reg)
                :load-if (not (sc-is float long-stack))))
  (:results (hi-bits :scs (unsigned-reg)))
  (:temporary (:sc long-stack) temp)
  (:arg-types long-float)
  (:result-types unsigned-num)
  (:translate long-float-high-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
     (sc-case float
       (long-reg
        (with-tn@fp-top(float)
          (let ((where (make-ea :dword :base ebp-tn
                                :disp (frame-byte-offset (+ 2 (tn-offset temp))))))
            (store-long-float where)))
        (loadw hi-bits ebp-tn (frame-word-offset (1+ (tn-offset temp)))))
       (long-stack
        (loadw hi-bits ebp-tn (frame-word-offset (1+ (tn-offset temp)))))
       (descriptor-reg
        (loadw hi-bits float (1+ long-float-value-slot)
               other-pointer-lowtag)))))

#+long-float
(define-vop (long-float-low-bits)
  (:args (float :scs (long-reg descriptor-reg)
                :load-if (not (sc-is float long-stack))))
  (:results (lo-bits :scs (unsigned-reg)))
  (:temporary (:sc long-stack) temp)
  (:arg-types long-float)
  (:result-types unsigned-num)
  (:translate long-float-low-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
     (sc-case float
       (long-reg
        (with-tn@fp-top(float)
          (let ((where (make-ea :dword :base ebp-tn
                                :disp (frame-byte-offset (+ 2 (tn-offset temp))))))
            (store-long-float where)))
        (loadw lo-bits ebp-tn (frame-word-offset (+ (tn-offset temp) 2))))
       (long-stack
        (loadw lo-bits ebp-tn (frame-word-offset (+ (tn-offset float) 2))))
       (descriptor-reg
        (loadw lo-bits float long-float-value-slot
               other-pointer-lowtag)))))

;;;; float mode hackery

(sb-xc:deftype float-modes () '(unsigned-byte 32)) ; really only 16
(defknown floating-point-modes () float-modes (flushable))
(defknown ((setf floating-point-modes)) (float-modes)
  float-modes)

(defconstant npx-env-size (* 7 n-word-bytes))
(defconstant npx-cw-offset 0)
(defconstant npx-sw-offset 4)

(define-vop (floating-point-modes)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate floating-point-modes)
  (:policy :fast-safe)
  (:temporary (:sc unsigned-reg :offset eax-offset :target res
                   :to :result) eax)
  (:generator 8
   (inst sub esp-tn npx-env-size)       ; Make space on stack.
   (inst wait)                          ; Catch any pending FPE exceptions
   (inst fstenv (make-ea :dword :base esp-tn)) ; masks all exceptions
   (inst fldenv (make-ea :dword :base esp-tn)) ; Restore previous state.
   ;; Move current status to high word.
   (inst mov eax (make-ea :dword :base esp-tn :disp (- npx-sw-offset 2)))
   ;; Move exception mask to low word.
   (inst mov ax-tn (make-ea :word :base esp-tn :disp npx-cw-offset))
   (inst add esp-tn npx-env-size)       ; Pop stack.
   (inst xor eax #x3f)            ; Flip exception mask to trap enable bits.
   (move res eax)))

(define-vop (set-floating-point-modes)
  (:args (new :scs (unsigned-reg) :to :result :target res))
  (:results (res :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:result-types unsigned-num)
  (:translate (setf floating-point-modes))
  (:policy :fast-safe)
  (:temporary (:sc unsigned-reg :offset eax-offset
                   :from :eval :to :result) eax)
  (:generator 3
   (inst sub esp-tn npx-env-size)       ; Make space on stack.
   (inst wait)                          ; Catch any pending FPE exceptions.
   (inst fstenv (make-ea :dword :base esp-tn))
   (inst mov eax new)
   (inst xor eax #x3f)            ; Turn trap enable bits into exception mask.
   (inst mov (make-ea :word :base esp-tn :disp npx-cw-offset) ax-tn)
   (inst shr eax 16)                    ; position status word
   (inst mov (make-ea :word :base esp-tn :disp npx-sw-offset) ax-tn)
   (inst fldenv (make-ea :dword :base esp-tn))
   (inst add esp-tn npx-env-size)       ; Pop stack.
   (move res new)))

#-long-float
(progn

;;; Let's use some of the 80387 special functions.
;;;
;;; These defs will not take effect unless code/irrat.lisp is modified
;;; to remove the inlined alien routine def.

(macrolet ((frob (func trans op)
             `(define-vop (,func)
               (:args (x :scs (double-reg) :target fr0))
               (:temporary (:sc double-reg :offset fr0-offset
                                :from :argument :to :result) fr0)
               (:ignore fr0)
               (:results (y :scs (double-reg)))
               (:arg-types double-float)
               (:result-types double-float)
               (:translate ,trans)
               (:policy :fast-safe)
               (:note "inline NPX function")
               (:vop-var vop)
               (:save-p :compute-only)
               (:node-var node)
               (:generator 5
                (note-this-location vop :internal-error)
                (unless (zerop (tn-offset x))
                  (inst fxch x)         ; x to top of stack
                  (unless (location= x y)
                    (inst fst x)))      ; maybe save it
                (inst ,op)              ; clobber st0
                (cond ((zerop (tn-offset y))
                       (maybe-fp-wait node))
                      (t
                       (inst fst y)))))))

  ;; Quick versions of fsin and fcos that require the argument to be
  ;; within range 2^63.
  (frob fsin-quick %sin-quick fsin)
  (frob fcos-quick %cos-quick fcos)
  (frob fsqrt %sqrt fsqrt))

;;; Quick version of ftan that requires the argument to be within
;;; range 2^63.
(define-vop (ftan-quick)
  (:translate %tan-quick)
  (:args (x :scs (double-reg) :target fr0))
  (:temporary (:sc double-reg :offset fr0-offset
                   :from :argument :to :result) fr0)
  (:temporary (:sc double-reg :offset fr1-offset
                   :from :argument :to :result) fr1)
  (:results (y :scs (double-reg)))
  (:arg-types double-float)
  (:result-types double-float)
  (:policy :fast-safe)
  (:note "inline tan function")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 5
    (note-this-location vop :internal-error)
    (case (tn-offset x)
       (0
        (inst fstp fr1))
       (1
        (inst fstp fr0))
       (t
        (inst fstp fr0)
        (inst fstp fr0)
        (inst fldd (make-random-tn (sc-or-lose 'double-reg) (- (tn-offset x) 2)))))
    (inst fptan)
    ;; Result is in fr1
    (case (tn-offset y)
       (0
        (inst fxch fr1))
       (1)
       (t
        (inst fxch fr1)
        (inst fstd y)))))

;;; KLUDGE: these versions of fsin, fcos, and ftan simply load a 0.0
;;; result if the argument is out of range 2^63 and would thus be
;;; hopelessly inaccurate.
(macrolet ((frob (func trans op)
             `(define-vop (,func)
                (:translate ,trans)
                (:args (x :scs (double-reg) :target fr0))
                (:temporary (:sc double-reg :offset fr0-offset
                                 :from :argument :to :result) fr0)
                ;; FIXME: make that an arbitrary location and
                ;; FXCH only when range reduction needed
                (:temporary (:sc double-reg :offset fr1-offset
                                 :from :argument :to :result) fr1)
                (:temporary (:sc unsigned-reg :offset eax-offset
                             :from :argument :to :result) eax)
                (:results (y :scs (double-reg)))
                (:arg-types double-float)
                (:result-types double-float)
                (:policy :fast-safe)
                (:note "inline sin/cos function")
                (:vop-var vop)
                (:save-p :compute-only)
                (:ignore eax)
                (:generator 5
                  (let ((DONE (gen-label))
                        (REDUCE (gen-label))
                        (REDUCE-LOOP (gen-label)))
                    (note-this-location vop :internal-error)
                    (unless (zerop (tn-offset x))
                      (inst fxch x)          ; x to top of stack
                      (unless (location= x y)
                        (inst fst x))) ; maybe save it
                    (inst ,op)
                    (inst fnstsw)                  ; status word to ax
                    (inst and ah-tn #x04)          ; C2
                    (inst jmp :nz REDUCE)
                    (emit-label DONE)
                    (unless (zerop (tn-offset y))
                      (inst fstd y))
                    (assemble (:elsewhere)
                      (emit-label REDUCE)
                      ;; Else x was out of range so reduce it; ST0 is unchanged.
                      (with-empty-tn@fp-top (fr1)
                        (inst fldpi)
                        (inst fadd fr0))
                      (emit-label REDUCE-LOOP)
                      (inst fprem1)
                      (inst fnstsw)
                      (inst and ah-tn #x04)
                      (inst jmp :nz REDUCE-LOOP)
                      (inst ,op)
                      (inst jmp DONE)))))))
          (frob fsin  %sin fsin)
          (frob fcos  %cos fcos))

(define-vop (ftan)
  (:translate %tan)
  (:args (x :scs (double-reg) :target fr0))
  (:temporary (:sc double-reg :offset fr0-offset
                   :from :argument :to :result) fr0)
  (:temporary (:sc double-reg :offset fr1-offset
                   :from :argument :to :result) fr1)
  (:temporary (:sc unsigned-reg :offset eax-offset
                   :from :argument :to :result) eax)
  (:results (y :scs (double-reg)))
  (:arg-types double-float)
  (:result-types double-float)
  (:ignore eax)
  (:policy :fast-safe)
  (:note "inline tan function")
  (:vop-var vop)
  (:save-p :compute-only)
  (:ignore eax)
  (:generator 5
    (note-this-location vop :internal-error)
    (case (tn-offset x)
       (0
        (inst fstp fr1))
       (1
        (inst fstp fr0))
       (t
        (inst fstp fr0)
        (inst fstp fr0)
        (inst fldd (make-random-tn (sc-or-lose 'double-reg) (- (tn-offset x) 2)))))
    (inst fptan)
    (let ((REDUCE (gen-label))
          (REDUCE-LOOP (gen-label)))
      (inst fnstsw)                        ; status word to ax
      (inst and ah-tn #x04)                ; C2
      (inst jmp :nz REDUCE)
      (assemble (:elsewhere)
        (emit-label REDUCE)
        ;; Else x was out of range so reduce it; ST0 is unchanged.
        (with-empty-tn@fp-top (fr1)
          (inst fldpi)
          (inst fadd fr0))
        (emit-label REDUCE-LOOP)
        (inst fprem1)
        (inst fnstsw)
        (inst and ah-tn #x04)
        (inst jmp :nz REDUCE-LOOP)
        (inst fptan)
        (inst jmp DONE)))
    DONE
    ;; Result is in fr1
    (case (tn-offset y)
       (0
        (inst fxch fr1))
       (1)
       (t
        (inst fxch fr1)
        (inst fstd y)))))

;;; %exp that handles the following special cases: exp(+Inf) is +Inf;
;;; exp(-Inf) is 0; exp(NaN) is NaN.
(define-vop (fexp)
  (:translate %exp)
  (:args (x :scs (double-reg) :target fr0))
  (:temporary (:sc word-reg :offset eax-offset :from :eval :to :result) temp)
  (:temporary (:sc double-reg :offset fr0-offset
                   :from :argument :to :result) fr0)
  (:temporary (:sc double-reg :offset fr1-offset
                   :from :argument :to :result) fr1)
  (:temporary (:sc double-reg :offset fr2-offset
                   :from :argument :to :result) fr2)
  (:results (y :scs (double-reg)))
  (:arg-types double-float)
  (:result-types double-float)
  (:policy :fast-safe)
  (:note "inline exp function")
  (:vop-var vop)
  (:save-p :compute-only)
  (:ignore temp)
  (:generator 5
     (note-this-location vop :internal-error)
     (unless (zerop (tn-offset x))
       (inst fxch x)            ; x to top of stack
       (unless (location= x y)
         (inst fst x))) ; maybe save it
     ;; Check for Inf or NaN
     (inst fxam)
     (inst fnstsw)
     (inst sahf)
     (inst jmp :nc NOINFNAN)        ; Neither Inf or NaN.
     (inst jmp :np NOINFNAN)        ; NaN gives NaN? Continue.
     (inst and ah-tn #x02)            ; Test sign of Inf.
     (inst jmp :z DONE)          ; +Inf gives +Inf.
     (inst fstp fr0)                ; -Inf gives 0
     (inst fldz)
     (inst jmp DONE)
     NOINFNAN
     (inst fstp fr1)
     (inst fldl2e)
     (inst fmul fr1)
     ;; Now fr0=x log2(e)
     (inst fst fr1)
     (inst frndint)
     (inst fst fr2)
     (inst fsubp-sti fr1)
     (inst f2xm1)
     (inst fld1)
     (inst faddp-sti fr1)
     (inst fscale)
     (inst fld fr0)
     DONE
     (unless (zerop (tn-offset y))
             (inst fstd y))))

;;; Expm1 = exp(x) - 1.
;;; Handles the following special cases:
;;;   expm1(+Inf) is +Inf; expm1(-Inf) is -1.0; expm1(NaN) is NaN.
(define-vop (fexpm1)
  (:translate %expm1)
  (:args (x :scs (double-reg) :target fr0))
  (:temporary (:sc word-reg :offset eax-offset :from :eval :to :result) temp)
  (:temporary (:sc double-reg :offset fr0-offset
                   :from :argument :to :result) fr0)
  (:temporary (:sc double-reg :offset fr1-offset
                   :from :argument :to :result) fr1)
  (:temporary (:sc double-reg :offset fr2-offset
                   :from :argument :to :result) fr2)
  (:results (y :scs (double-reg)))
  (:arg-types double-float)
  (:result-types double-float)
  (:policy :fast-safe)
  (:note "inline expm1 function")
  (:vop-var vop)
  (:save-p :compute-only)
  (:ignore temp)
  (:generator 5
     (note-this-location vop :internal-error)
     (unless (zerop (tn-offset x))
       (inst fxch x)            ; x to top of stack
       (unless (location= x y)
         (inst fst x))) ; maybe save it
     ;; Check for Inf or NaN
     (inst fxam)
     (inst fnstsw)
     (inst sahf)
     (inst jmp :nc NOINFNAN)        ; Neither Inf or NaN.
     (inst jmp :np NOINFNAN)        ; NaN gives NaN? Continue.
     (inst and ah-tn #x02)            ; Test sign of Inf.
     (inst jmp :z DONE)          ; +Inf gives +Inf.
     (inst fstp fr0)                ; -Inf gives -1.0
     (inst fld1)
     (inst fchs)
     (inst jmp DONE)
     NOINFNAN
     ;; Free two stack slots leaving the argument on top.
     (inst fstp fr2)
     (inst fstp fr0)
     (inst fldl2e)
     (inst fmul fr1)    ; Now fr0 = x log2(e)
     (inst fst fr1)
     (inst frndint)
     (inst fsub-sti fr1)
     (inst fxch fr1)
     (inst f2xm1)
     (inst fscale)
     (inst fxch fr1)
     (inst fld1)
     (inst fscale)
     (inst fstp fr1)
     (inst fld1)
     (inst fsub fr1)
     (inst fsubr fr2)
     DONE
     (unless (zerop (tn-offset y))
       (inst fstd y))))

(define-vop (flog)
  (:translate %log)
  (:args (x :scs (double-reg double-stack descriptor-reg) :target fr0))
  (:temporary (:sc double-reg :offset fr0-offset
                   :from :argument :to :result) fr0)
  (:temporary (:sc double-reg :offset fr1-offset
                   :from :argument :to :result) fr1)
  (:results (y :scs (double-reg)))
  (:arg-types double-float)
  (:result-types double-float)
  (:policy :fast-safe)
  (:note "inline log function")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 5
     (note-this-location vop :internal-error)
     (sc-case x
        (double-reg
         (case (tn-offset x)
            (0
             ;; x is in fr0
             (inst fstp fr1)
             (inst fldln2)
             (inst fxch fr1))
            (1
             ;; x is in fr1
             (inst fstp fr0)
             (inst fldln2)
             (inst fxch fr1))
            (t
             ;; x is in a FP reg, not fr0 or fr1
             (inst fstp fr0)
             (inst fstp fr0)
             (inst fldln2)
             (inst fldd (make-random-tn (sc-or-lose 'double-reg) (1- (tn-offset x))))))
         (inst fyl2x))
        ((double-stack descriptor-reg)
         (inst fstp fr0)
         (inst fstp fr0)
         (inst fldln2)
         (if (sc-is x double-stack)
             (inst fldd (ea-for-df-stack x))
             (inst fldd (ea-for-df-desc x)))
         (inst fyl2x)))
     (inst fld fr0)
     (case (tn-offset y)
       ((0 1))
       (t (inst fstd y)))))

(define-vop (flog10)
  (:translate %log10)
  (:args (x :scs (double-reg double-stack descriptor-reg) :target fr0))
  (:temporary (:sc double-reg :offset fr0-offset
                   :from :argument :to :result) fr0)
  (:temporary (:sc double-reg :offset fr1-offset
                   :from :argument :to :result) fr1)
  (:results (y :scs (double-reg)))
  (:arg-types double-float)
  (:result-types double-float)
  (:policy :fast-safe)
  (:note "inline log10 function")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 5
     (note-this-location vop :internal-error)
     (sc-case x
        (double-reg
         (case (tn-offset x)
            (0
             ;; x is in fr0
             (inst fstp fr1)
             (inst fldlg2)
             (inst fxch fr1))
            (1
             ;; x is in fr1
             (inst fstp fr0)
             (inst fldlg2)
             (inst fxch fr1))
            (t
             ;; x is in a FP reg, not fr0 or fr1
             (inst fstp fr0)
             (inst fstp fr0)
             (inst fldlg2)
             (inst fldd (make-random-tn (sc-or-lose 'double-reg) (1- (tn-offset x))))))
         (inst fyl2x))
        ((double-stack descriptor-reg)
         (inst fstp fr0)
         (inst fstp fr0)
         (inst fldlg2)
         (if (sc-is x double-stack)
             (inst fldd (ea-for-df-stack x))
             (inst fldd (ea-for-df-desc x)))
         (inst fyl2x)))
     (inst fld fr0)
     (case (tn-offset y)
       ((0 1))
       (t (inst fstd y)))))

(define-vop (flog2)
  (:translate %log2)
  (:args (x :scs (double-reg double-stack descriptor-reg) :target fr0))
  (:temporary (:sc double-reg :offset fr0-offset
                   :from :argument :to :result) fr0)
  (:temporary (:sc double-reg :offset fr1-offset
                   :from :argument :to :result) fr1)
  (:results (y :scs (double-reg)))
  (:arg-types double-float)
  (:result-types double-float)
  (:policy :fast-safe)
  (:note "inline log2 function")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 5
     (note-this-location vop :internal-error)
     (sc-case x
        (double-reg
         (case (tn-offset x)
            (0
             ;; x is in fr0
             (inst fstp fr1)
             (inst fld1)
             (inst fxch fr1))
            (1
             ;; x is in fr1
             (inst fstp fr0)
             (inst fld1)
             (inst fxch fr1))
            (t
             ;; x is in a FP reg, not fr0 or fr1
             (inst fstp fr0)
             (inst fstp fr0)
             (inst fld1)
             (inst fldd (make-random-tn (sc-or-lose 'double-reg) (1- (tn-offset x))))))
         (inst fyl2x))
        ((double-stack descriptor-reg)
         (inst fstp fr0)
         (inst fstp fr0)
         (inst fld1)
         (if (sc-is x double-stack)
             (inst fldd (ea-for-df-stack x))
             (inst fldd (ea-for-df-desc x)))
         (inst fyl2x)))
     (inst fld fr0)
     (case (tn-offset y)
       ((0 1))
       (t (inst fstd y)))))

(define-vop (fpow)
  (:translate %pow)
  (:args (x :scs (double-reg double-stack descriptor-reg) :target fr0)
         (y :scs (double-reg double-stack descriptor-reg) :target fr1))
  (:temporary (:sc double-reg :offset fr0-offset
                   :from (:argument 0) :to :result) fr0)
  (:temporary (:sc double-reg :offset fr1-offset
                   :from (:argument 1) :to :result) fr1)
  (:temporary (:sc double-reg :offset fr2-offset
                   :from :load :to :result) fr2)
  (:results (r :scs (double-reg)))
  (:arg-types double-float double-float)
  (:result-types double-float)
  (:policy :fast-safe)
  (:note "inline pow function")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 5
     (note-this-location vop :internal-error)
     ;; Setup x in fr0 and y in fr1
     (cond
      ;; x in fr0; y in fr1
      ((and (sc-is x double-reg) (zerop (tn-offset x))
            (sc-is y double-reg) (= 1 (tn-offset y))))
      ;; y in fr1; x not in fr0
      ((and (sc-is y double-reg) (= 1 (tn-offset y)))
       ;; Load x to fr0
       (sc-case x
          (double-reg
           (copy-fp-reg-to-fr0 x))
          (double-stack
           (inst fstp fr0)
           (inst fldd (ea-for-df-stack x)))
          (descriptor-reg
           (inst fstp fr0)
           (inst fldd (ea-for-df-desc x)))))
      ;; x in fr0; y not in fr1
      ((and (sc-is x double-reg) (zerop (tn-offset x)))
       (inst fxch fr1)
       ;; Now load y to fr0
       (sc-case y
          (double-reg
           (copy-fp-reg-to-fr0 y))
          (double-stack
           (inst fstp fr0)
           (inst fldd (ea-for-df-stack y)))
          (descriptor-reg
           (inst fstp fr0)
           (inst fldd (ea-for-df-desc y))))
       (inst fxch fr1))
      ;; x in fr1; y not in fr1
      ((and (sc-is x double-reg) (= 1 (tn-offset x)))
       ;; Load y to fr0
       (sc-case y
          (double-reg
           (copy-fp-reg-to-fr0 y))
          (double-stack
           (inst fstp fr0)
           (inst fldd (ea-for-df-stack y)))
          (descriptor-reg
           (inst fstp fr0)
           (inst fldd (ea-for-df-desc y))))
       (inst fxch fr1))
      ;; y in fr0;
      ((and (sc-is y double-reg) (zerop (tn-offset y)))
       (inst fxch fr1)
       ;; Now load x to fr0
       (sc-case x
          (double-reg
           (copy-fp-reg-to-fr0 x))
          (double-stack
           (inst fstp fr0)
           (inst fldd (ea-for-df-stack x)))
          (descriptor-reg
           (inst fstp fr0)
           (inst fldd (ea-for-df-desc x)))))
      ;; Neither x or y are in either fr0 or fr1
      (t
       ;; Load y then x
       (inst fstp fr0)
       (inst fstp fr0)
       (sc-case y
          (double-reg
           (inst fldd (make-random-tn (sc-or-lose 'double-reg) (- (tn-offset y) 2))))
          (double-stack
           (inst fldd (ea-for-df-stack y)))
          (descriptor-reg
           (inst fldd (ea-for-df-desc y))))
       ;; Load x to fr0
       (sc-case x
          (double-reg
           (inst fldd (make-random-tn (sc-or-lose 'double-reg) (1- (tn-offset x)))))
          (double-stack
           (inst fldd (ea-for-df-stack x)))
          (descriptor-reg
           (inst fldd (ea-for-df-desc x))))))

     ;; Now have x at fr0; and y at fr1
     (inst fyl2x)
     ;; Now fr0=y log2(x)
     (inst fld fr0)
     (inst frndint)
     (inst fst fr2)
     (inst fsubp-sti fr1)
     (inst f2xm1)
     (inst fld1)
     (inst faddp-sti fr1)
     (inst fscale)
     (inst fld fr0)
     (case (tn-offset r)
       ((0 1))
       (t (inst fstd r)))))

(define-vop (fscalen)
  (:translate %scalbn)
  (:args (x :scs (double-reg double-stack descriptor-reg)
            :to (:argument 2) :target fr0)
         (y :scs (signed-stack signed-reg) :target temp))
  (:temporary (:sc double-reg :offset fr0-offset
               :from (:argument 2) :to :result) fr0)
  (:temporary (:sc double-reg :offset fr1-offset :from :eval :to :result) fr1)
  (:temporary (:sc signed-stack :from (:argument 1) :to :result) temp)
  (:results (r :scs (double-reg)))
  (:arg-types double-float signed-num)
  (:result-types double-float)
  (:policy :fast-safe)
  (:note "inline scalbn function")
  (:generator 5
     ;; Setup x in fr0 and y in fr1
     (sc-case x
       (double-reg
        (case (tn-offset x)
          (0
           (inst fstp fr1)
           (sc-case y
             (signed-reg
              (inst mov temp y)
              (inst fild temp))
             (signed-stack
              (inst fild y)))
           (inst fxch fr1))
          (1
           (inst fstp fr0)
           (sc-case y
             (signed-reg
              (inst mov temp y)
              (inst fild temp))
             (signed-stack
              (inst fild y)))
           (inst fxch fr1))
          (t
           (inst fstp fr0)
           (inst fstp fr0)
           (sc-case y
             (signed-reg
              (inst mov temp y)
              (inst fild temp))
             (signed-stack
              (inst fild y)))
           (inst fld (make-random-tn (sc-or-lose 'double-reg) (1- (tn-offset x)))))))
       ((double-stack descriptor-reg)
        (inst fstp fr0)
        (inst fstp fr0)
        (sc-case y
          (signed-reg
           (inst mov temp y)
           (inst fild temp))
          (signed-stack
           (inst fild y)))
        (if (sc-is x double-stack)
            (inst fldd (ea-for-df-stack x))
            (inst fldd (ea-for-df-desc x)))))
     (inst fscale)
     (unless (zerop (tn-offset r))
       (inst fstd r))))

(define-vop (fscale)
  (:translate %scalb)
  (:args (x :scs (double-reg double-stack descriptor-reg) :target fr0)
         (y :scs (double-reg double-stack descriptor-reg) :target fr1))
  (:temporary (:sc double-reg :offset fr0-offset
                   :from (:argument 0) :to :result) fr0)
  (:temporary (:sc double-reg :offset fr1-offset
                   :from (:argument 1) :to :result) fr1)
  (:results (r :scs (double-reg)))
  (:arg-types double-float double-float)
  (:result-types double-float)
  (:policy :fast-safe)
  (:note "inline scalb function")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 5
     (note-this-location vop :internal-error)
     ;; Setup x in fr0 and y in fr1
     (cond
      ;; x in fr0; y in fr1
      ((and (sc-is x double-reg) (zerop (tn-offset x))
            (sc-is y double-reg) (= 1 (tn-offset y))))
      ;; y in fr1; x not in fr0
      ((and (sc-is y double-reg) (= 1 (tn-offset y)))
       ;; Load x to fr0
       (sc-case x
          (double-reg
           (copy-fp-reg-to-fr0 x))
          (double-stack
           (inst fstp fr0)
           (inst fldd (ea-for-df-stack x)))
          (descriptor-reg
           (inst fstp fr0)
           (inst fldd (ea-for-df-desc x)))))
      ;; x in fr0; y not in fr1
      ((and (sc-is x double-reg) (zerop (tn-offset x)))
       (inst fxch fr1)
       ;; Now load y to fr0
       (sc-case y
          (double-reg
           (copy-fp-reg-to-fr0 y))
          (double-stack
           (inst fstp fr0)
           (inst fldd (ea-for-df-stack y)))
          (descriptor-reg
           (inst fstp fr0)
           (inst fldd (ea-for-df-desc y))))
       (inst fxch fr1))
      ;; x in fr1; y not in fr1
      ((and (sc-is x double-reg) (= 1 (tn-offset x)))
       ;; Load y to fr0
       (sc-case y
          (double-reg
           (copy-fp-reg-to-fr0 y))
          (double-stack
           (inst fstp fr0)
           (inst fldd (ea-for-df-stack y)))
          (descriptor-reg
           (inst fstp fr0)
           (inst fldd (ea-for-df-desc y))))
       (inst fxch fr1))
      ;; y in fr0;
      ((and (sc-is y double-reg) (zerop (tn-offset y)))
       (inst fxch fr1)
       ;; Now load x to fr0
       (sc-case x
          (double-reg
           (copy-fp-reg-to-fr0 x))
          (double-stack
           (inst fstp fr0)
           (inst fldd (ea-for-df-stack x)))
          (descriptor-reg
           (inst fstp fr0)
           (inst fldd (ea-for-df-desc x)))))
      ;; Neither x or y are in either fr0 or fr1
      (t
       ;; Load y then x
       (inst fstp fr0)
       (inst fstp fr0)
       (sc-case y
          (double-reg
           (inst fldd (make-random-tn (sc-or-lose 'double-reg) (- (tn-offset y) 2))))
          (double-stack
           (inst fldd (ea-for-df-stack y)))
          (descriptor-reg
           (inst fldd (ea-for-df-desc y))))
       ;; Load x to fr0
       (sc-case x
          (double-reg
           (inst fldd (make-random-tn (sc-or-lose 'double-reg) (1- (tn-offset x)))))
          (double-stack
           (inst fldd (ea-for-df-stack x)))
          (descriptor-reg
           (inst fldd (ea-for-df-desc x))))))

     ;; Now have x at fr0; and y at fr1
     (inst fscale)
     (unless (zerop (tn-offset r))
             (inst fstd r))))

(define-vop (flog1p)
  (:translate %log1p)
  (:args (x :scs (double-reg) :to :result))
  (:temporary (:sc double-reg :offset fr0-offset
                   :from :argument :to :result) fr0)
  (:temporary (:sc double-reg :offset fr1-offset
                   :from :argument :to :result) fr1)
  (:temporary (:sc word-reg :offset eax-offset :from :eval) temp)
  (:results (y :scs (double-reg)))
  (:arg-types double-float)
  (:result-types double-float)
  (:policy :fast-safe)
  (:note "inline log1p function")
  (:ignore temp)
  (:generator 5
     ;; x is in a FP reg, not fr0, fr1.
     (inst fstp fr0)
     (inst fstp fr0)
     (inst fldd (make-random-tn (sc-or-lose 'double-reg) (- (tn-offset x) 2)))
     ;; Check the range
     (inst push #x3e947ae1)     ; Constant 0.29
     (inst fabs)
     (inst fld (make-ea :dword :base esp-tn))
     (inst fcompp)
     (inst add esp-tn 4)
     (inst fnstsw)                      ; status word to ax
     (inst and ah-tn #x45)
     (inst jmp :z WITHIN-RANGE)
     ;; Out of range for fyl2xp1.
     (inst fld1)
     (inst faddd (make-random-tn (sc-or-lose 'double-reg) (- (tn-offset x) 1)))
     (inst fldln2)
     (inst fxch fr1)
     (inst fyl2x)
     (inst jmp DONE)

     WITHIN-RANGE
     (inst fldln2)
     (inst fldd (make-random-tn (sc-or-lose 'double-reg) (- (tn-offset x) 1)))
     (inst fyl2xp1)
     DONE
     (inst fld fr0)
     (case (tn-offset y)
       ((0 1))
       (t (inst fstd y)))))

;;; The Pentium has a less restricted implementation of the fyl2xp1
;;; instruction and a range check can be avoided.
(define-vop (flog1p-pentium)
  (:translate %log1p)
  (:args (x :scs (double-reg double-stack descriptor-reg) :target fr0))
  (:temporary (:sc double-reg :offset fr0-offset
                   :from :argument :to :result) fr0)
  (:temporary (:sc double-reg :offset fr1-offset
                   :from :argument :to :result) fr1)
  (:results (y :scs (double-reg)))
  (:arg-types double-float)
  (:result-types double-float)
  (:policy :fast-safe)
  (:guard (member :pentium-style-fyl2xp1 *backend-subfeatures*))
  (:note "inline log1p with limited x range function")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 4
     (note-this-location vop :internal-error)
     (sc-case x
        (double-reg
         (case (tn-offset x)
            (0
             ;; x is in fr0
             (inst fstp fr1)
             (inst fldln2)
             (inst fxch fr1))
            (1
             ;; x is in fr1
             (inst fstp fr0)
             (inst fldln2)
             (inst fxch fr1))
            (t
             ;; x is in a FP reg, not fr0 or fr1
             (inst fstp fr0)
             (inst fstp fr0)
             (inst fldln2)
             (inst fldd (make-random-tn (sc-or-lose 'double-reg) (1- (tn-offset x)))))))
        ((double-stack descriptor-reg)
         (inst fstp fr0)
         (inst fstp fr0)
         (inst fldln2)
         (if (sc-is x double-stack)
             (inst fldd (ea-for-df-stack x))
           (inst fldd (ea-for-df-desc x)))))
     (inst fyl2xp1)
     (inst fld fr0)
     (case (tn-offset y)
       ((0 1))
       (t (inst fstd y)))))

(define-vop (flogb)
  (:translate %logb)
  (:args (x :scs (double-reg double-stack descriptor-reg) :target fr0))
  (:temporary (:sc double-reg :offset fr0-offset
                   :from :argument :to :result) fr0)
  (:temporary (:sc double-reg :offset fr1-offset
                   :from :argument :to :result) fr1)
  (:results (y :scs (double-reg)))
  (:arg-types double-float)
  (:result-types double-float)
  (:policy :fast-safe)
  (:note "inline logb function")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 5
     (note-this-location vop :internal-error)
     (sc-case x
        (double-reg
         (case (tn-offset x)
            (0
             ;; x is in fr0
             (inst fstp fr1))
            (1
             ;; x is in fr1
             (inst fstp fr0))
            (t
             ;; x is in a FP reg, not fr0 or fr1
             (inst fstp fr0)
             (inst fstp fr0)
             (inst fldd (make-random-tn (sc-or-lose 'double-reg) (- (tn-offset x) 2))))))
        ((double-stack descriptor-reg)
         (inst fstp fr0)
         (inst fstp fr0)
         (if (sc-is x double-stack)
             (inst fldd (ea-for-df-stack x))
           (inst fldd (ea-for-df-desc x)))))
     (inst fxtract)
     (case (tn-offset y)
       (0
        (inst fxch fr1))
       (1)
       (t (inst fxch fr1)
          (inst fstd y)))))

(define-vop (fatan)
  (:translate %atan)
  (:args (x :scs (double-reg double-stack descriptor-reg) :target fr0))
  (:temporary (:sc double-reg :offset fr0-offset
                   :from (:argument 0) :to :result) fr0)
  (:temporary (:sc double-reg :offset fr1-offset
                   :from (:argument 0) :to :result) fr1)
  (:results (r :scs (double-reg)))
  (:arg-types double-float)
  (:result-types double-float)
  (:policy :fast-safe)
  (:note "inline atan function")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 5
     (note-this-location vop :internal-error)
     ;; Setup x in fr1 and 1.0 in fr0
     (cond
      ;; x in fr0
      ((and (sc-is x double-reg) (zerop (tn-offset x)))
       (inst fstp fr1))
      ;; x in fr1
      ((and (sc-is x double-reg) (= 1 (tn-offset x)))
       (inst fstp fr0))
      ;; x not in fr0 or fr1
      (t
       ;; Load x then 1.0
       (inst fstp fr0)
       (inst fstp fr0)
       (sc-case x
          (double-reg
           (inst fldd (make-random-tn (sc-or-lose 'double-reg) (- (tn-offset x) 2))))
          (double-stack
           (inst fldd (ea-for-df-stack x)))
          (descriptor-reg
           (inst fldd (ea-for-df-desc x))))))
     (inst fld1)
     ;; Now have x at fr1; and 1.0 at fr0
     (inst fpatan)
     (inst fld fr0)
     (case (tn-offset r)
       ((0 1))
       (t (inst fstd r)))))

(define-vop (fatan2)
  (:translate %atan2)
  (:args (x :scs (double-reg double-stack descriptor-reg) :target fr1)
         (y :scs (double-reg double-stack descriptor-reg) :target fr0))
  (:temporary (:sc double-reg :offset fr0-offset
                   :from (:argument 1) :to :result) fr0)
  (:temporary (:sc double-reg :offset fr1-offset
                   :from (:argument 0) :to :result) fr1)
  (:results (r :scs (double-reg)))
  (:arg-types double-float double-float)
  (:result-types double-float)
  (:policy :fast-safe)
  (:note "inline atan2 function")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 5
     (note-this-location vop :internal-error)
     ;; Setup x in fr1 and y in fr0
     (cond
      ;; y in fr0; x in fr1
      ((and (sc-is y double-reg) (zerop (tn-offset y))
            (sc-is x double-reg) (= 1 (tn-offset x))))
      ;; x in fr1; y not in fr0
      ((and (sc-is x double-reg) (= 1 (tn-offset x)))
       ;; Load y to fr0
       (sc-case y
          (double-reg
           (copy-fp-reg-to-fr0 y))
          (double-stack
           (inst fstp fr0)
           (inst fldd (ea-for-df-stack y)))
          (descriptor-reg
           (inst fstp fr0)
           (inst fldd (ea-for-df-desc y)))))
      ((and (sc-is x double-reg) (zerop (tn-offset x))
            (sc-is y double-reg) (zerop (tn-offset x)))
       ;; copy x to fr1
       (inst fst fr1))
      ;; y in fr0; x not in fr1
      ((and (sc-is y double-reg) (zerop (tn-offset y)))
       (inst fxch fr1)
       ;; Now load x to fr0
       (sc-case x
          (double-reg
           (copy-fp-reg-to-fr0 x))
          (double-stack
           (inst fstp fr0)
           (inst fldd (ea-for-df-stack x)))
          (descriptor-reg
           (inst fstp fr0)
           (inst fldd (ea-for-df-desc x))))
       (inst fxch fr1))
      ;; y in fr1; x not in fr1
      ((and (sc-is y double-reg) (= 1 (tn-offset y)))
       ;; Load x to fr0
       (sc-case x
          (double-reg
           (copy-fp-reg-to-fr0 x))
          (double-stack
           (inst fstp fr0)
           (inst fldd (ea-for-df-stack x)))
          (descriptor-reg
           (inst fstp fr0)
           (inst fldd (ea-for-df-desc x))))
       (inst fxch fr1))
      ;; x in fr0;
      ((and (sc-is x double-reg) (zerop (tn-offset x)))
       (inst fxch fr1)
       ;; Now load y to fr0
       (sc-case y
          (double-reg
           (copy-fp-reg-to-fr0 y))
          (double-stack
           (inst fstp fr0)
           (inst fldd (ea-for-df-stack y)))
          (descriptor-reg
           (inst fstp fr0)
           (inst fldd (ea-for-df-desc y)))))
      ;; Neither y or x are in either fr0 or fr1
      (t
       ;; Load x then y
       (inst fstp fr0)
       (inst fstp fr0)
       (sc-case x
          (double-reg
           (inst fldd (make-random-tn (sc-or-lose 'double-reg) (- (tn-offset x) 2))))
          (double-stack
           (inst fldd (ea-for-df-stack x)))
          (descriptor-reg
           (inst fldd (ea-for-df-desc x))))
       ;; Load y to fr0
       (sc-case y
          (double-reg
           (inst fldd (make-random-tn (sc-or-lose 'double-reg) (1- (tn-offset y)))))
          (double-stack
           (inst fldd (ea-for-df-stack y)))
          (descriptor-reg
           (inst fldd (ea-for-df-desc y))))))

     ;; Now have y at fr0; and x at fr1
     (inst fpatan)
     (inst fld fr0)
     (case (tn-offset r)
       ((0 1))
       (t (inst fstd r)))))
) ; PROGN #-LONG-FLOAT

#+long-float
(progn

;;; Lets use some of the 80387 special functions.
;;;
;;; These defs will not take effect unless code/irrat.lisp is modified
;;; to remove the inlined alien routine def.

(macrolet ((frob (func trans op)
             `(define-vop (,func)
               (:args (x :scs (long-reg) :target fr0))
               (:temporary (:sc long-reg :offset fr0-offset
                                :from :argument :to :result) fr0)
               (:ignore fr0)
               (:results (y :scs (long-reg)))
               (:arg-types long-float)
               (:result-types long-float)
               (:translate ,trans)
               (:policy :fast-safe)
               (:note "inline NPX function")
               (:vop-var vop)
               (:save-p :compute-only)
               (:node-var node)
               (:generator 5
                (note-this-location vop :internal-error)
                (unless (zerop (tn-offset x))
                  (inst fxch x)         ; x to top of stack
                  (unless (location= x y)
                    (inst fst x)))      ; maybe save it
                (inst ,op)              ; clobber st0
                (cond ((zerop (tn-offset y))
                       (maybe-fp-wait node))
                      (t
                       (inst fst y)))))))

  ;; Quick versions of FSIN and FCOS that require the argument to be
  ;; within range 2^63.
  (frob fsin-quick %sin-quick fsin)
  (frob fcos-quick %cos-quick fcos)
  (frob fsqrt %sqrt fsqrt))

;;; Quick version of ftan that requires the argument to be within
;;; range 2^63.
(define-vop (ftan-quick)
  (:translate %tan-quick)
  (:args (x :scs (long-reg) :target fr0))
  (:temporary (:sc long-reg :offset fr0-offset
                   :from :argument :to :result) fr0)
  (:temporary (:sc long-reg :offset fr1-offset
                   :from :argument :to :result) fr1)
  (:results (y :scs (long-reg)))
  (:arg-types long-float)
  (:result-types long-float)
  (:policy :fast-safe)
  (:note "inline tan function")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 5
    (note-this-location vop :internal-error)
    (case (tn-offset x)
       (0
        (inst fstp fr1))
       (1
        (inst fstp fr0))
       (t
        (inst fstp fr0)
        (inst fstp fr0)
        (inst fldd (make-random-tn (sc-or-lose 'double-reg) (- (tn-offset x) 2)))))
    (inst fptan)
    ;; Result is in fr1
    (case (tn-offset y)
       (0
        (inst fxch fr1))
       (1)
       (t
        (inst fxch fr1)
        (inst fstd y)))))

;;; These versions of fsin, fcos, and ftan simply load a 0.0 result if
;;; the argument is out of range 2^63 and would thus be hopelessly
;;; inaccurate.
(macrolet ((frob (func trans op)
             `(define-vop (,func)
                (:translate ,trans)
                (:args (x :scs (long-reg) :target fr0))
                (:temporary (:sc long-reg :offset fr0-offset
                                 :from :argument :to :result) fr0)
                (:temporary (:sc unsigned-reg :offset eax-offset
                             :from :argument :to :result) eax)
                (:results (y :scs (long-reg)))
                (:arg-types long-float)
                (:result-types long-float)
                (:policy :fast-safe)
                (:note "inline sin/cos function")
                (:vop-var vop)
                (:save-p :compute-only)
                (:ignore eax)
                (:generator 5
                  (note-this-location vop :internal-error)
                  (unless (zerop (tn-offset x))
                          (inst fxch x)          ; x to top of stack
                          (unless (location= x y)
                                  (inst fst x))) ; maybe save it
                  (inst ,op)
                  (inst fnstsw)                  ; status word to ax
                  (inst and ah-tn #x04)          ; C2
                  (inst jmp :z DONE)
                  ;; Else x was out of range so reduce it; ST0 is unchanged.
                  (inst fstp fr0)               ; Load 0.0
                  (inst fldz)
                  DONE
                  (unless (zerop (tn-offset y))
                          (inst fstd y))))))
          (frob fsin  %sin fsin)
          (frob fcos  %cos fcos))

(define-vop (ftan)
  (:translate %tan)
  (:args (x :scs (long-reg) :target fr0))
  (:temporary (:sc long-reg :offset fr0-offset
                   :from :argument :to :result) fr0)
  (:temporary (:sc long-reg :offset fr1-offset
                   :from :argument :to :result) fr1)
  (:temporary (:sc unsigned-reg :offset eax-offset
                   :from :argument :to :result) eax)
  (:results (y :scs (long-reg)))
  (:arg-types long-float)
  (:result-types long-float)
  (:ignore eax)
  (:policy :fast-safe)
  (:note "inline tan function")
  (:vop-var vop)
  (:save-p :compute-only)
  (:ignore eax)
  (:generator 5
    (note-this-location vop :internal-error)
    (case (tn-offset x)
       (0
        (inst fstp fr1))
       (1
        (inst fstp fr0))
       (t
        (inst fstp fr0)
        (inst fstp fr0)
        (inst fldd (make-random-tn (sc-or-lose 'double-reg) (- (tn-offset x) 2)))))
    (inst fptan)
    (inst fnstsw)                        ; status word to ax
    (inst and ah-tn #x04)                ; C2
    (inst jmp :z DONE)
    ;; Else x was out of range so reduce it; ST0 is unchanged.
    (inst fldz)                  ; Load 0.0
    (inst fxch fr1)
    DONE
    ;; Result is in fr1
    (case (tn-offset y)
       (0
        (inst fxch fr1))
       (1)
       (t
        (inst fxch fr1)
        (inst fstd y)))))

;;; Modified exp that handles the following special cases:
;;; exp(+Inf) is +Inf; exp(-Inf) is 0; exp(NaN) is NaN.
(define-vop (fexp)
  (:translate %exp)
  (:args (x :scs (long-reg) :target fr0))
  (:temporary (:sc word-reg :offset eax-offset :from :eval :to :result) temp)
  (:temporary (:sc long-reg :offset fr0-offset
                   :from :argument :to :result) fr0)
  (:temporary (:sc long-reg :offset fr1-offset
                   :from :argument :to :result) fr1)
  (:temporary (:sc long-reg :offset fr2-offset
                   :from :argument :to :result) fr2)
  (:results (y :scs (long-reg)))
  (:arg-types long-float)
  (:result-types long-float)
  (:policy :fast-safe)
  (:note "inline exp function")
  (:vop-var vop)
  (:save-p :compute-only)
  (:ignore temp)
  (:generator 5
     (note-this-location vop :internal-error)
     (unless (zerop (tn-offset x))
             (inst fxch x)              ; x to top of stack
             (unless (location= x y)
                     (inst fst x)))     ; maybe save it
     ;; Check for Inf or NaN
     (inst fxam)
     (inst fnstsw)
     (inst sahf)
     (inst jmp :nc NOINFNAN)        ; Neither Inf or NaN.
     (inst jmp :np NOINFNAN)        ; NaN gives NaN? Continue.
     (inst and ah-tn #x02)            ; Test sign of Inf.
     (inst jmp :z DONE)          ; +Inf gives +Inf.
     (inst fstp fr0)                ; -Inf gives 0
     (inst fldz)
     (inst jmp DONE)
     NOINFNAN
     (inst fstp fr1)
     (inst fldl2e)
     (inst fmul fr1)
     ;; Now fr0=x log2(e)
     (inst fst fr1)
     (inst frndint)
     (inst fst fr2)
     (inst fsubp-sti fr1)
     (inst f2xm1)
     (inst fld1)
     (inst faddp-sti fr1)
     (inst fscale)
     (inst fld fr0)
     DONE
     (unless (zerop (tn-offset y))
             (inst fstd y))))

;;; Expm1 = exp(x) - 1.
;;; Handles the following special cases:
;;;   expm1(+Inf) is +Inf; expm1(-Inf) is -1.0; expm1(NaN) is NaN.
(define-vop (fexpm1)
  (:translate %expm1)
  (:args (x :scs (long-reg) :target fr0))
  (:temporary (:sc word-reg :offset eax-offset :from :eval :to :result) temp)
  (:temporary (:sc long-reg :offset fr0-offset
                   :from :argument :to :result) fr0)
  (:temporary (:sc long-reg :offset fr1-offset
                   :from :argument :to :result) fr1)
  (:temporary (:sc long-reg :offset fr2-offset
                   :from :argument :to :result) fr2)
  (:results (y :scs (long-reg)))
  (:arg-types long-float)
  (:result-types long-float)
  (:policy :fast-safe)
  (:note "inline expm1 function")
  (:vop-var vop)
  (:save-p :compute-only)
  (:ignore temp)
  (:generator 5
     (note-this-location vop :internal-error)
     (unless (zerop (tn-offset x))
       (inst fxch x)            ; x to top of stack
       (unless (location= x y)
         (inst fst x))) ; maybe save it
     ;; Check for Inf or NaN
     (inst fxam)
     (inst fnstsw)
     (inst sahf)
     (inst jmp :nc NOINFNAN)        ; Neither Inf or NaN.
     (inst jmp :np NOINFNAN)        ; NaN gives NaN? Continue.
     (inst and ah-tn #x02)            ; Test sign of Inf.
     (inst jmp :z DONE)          ; +Inf gives +Inf.
     (inst fstp fr0)                ; -Inf gives -1.0
     (inst fld1)
     (inst fchs)
     (inst jmp DONE)
     NOINFNAN
     ;; Free two stack slots leaving the argument on top.
     (inst fstp fr2)
     (inst fstp fr0)
     (inst fldl2e)
     (inst fmul fr1)    ; Now fr0 = x log2(e)
     (inst fst fr1)
     (inst frndint)
     (inst fsub-sti fr1)
     (inst fxch fr1)
     (inst f2xm1)
     (inst fscale)
     (inst fxch fr1)
     (inst fld1)
     (inst fscale)
     (inst fstp fr1)
     (inst fld1)
     (inst fsub fr1)
     (inst fsubr fr2)
     DONE
     (unless (zerop (tn-offset y))
       (inst fstd y))))

(define-vop (flog)
  (:translate %log)
  (:args (x :scs (long-reg long-stack descriptor-reg) :target fr0))
  (:temporary (:sc long-reg :offset fr0-offset
                   :from :argument :to :result) fr0)
  (:temporary (:sc long-reg :offset fr1-offset
                   :from :argument :to :result) fr1)
  (:results (y :scs (long-reg)))
  (:arg-types long-float)
  (:result-types long-float)
  (:policy :fast-safe)
  (:note "inline log function")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 5
     (note-this-location vop :internal-error)
     (sc-case x
        (long-reg
         (case (tn-offset x)
            (0
             ;; x is in fr0
             (inst fstp fr1)
             (inst fldln2)
             (inst fxch fr1))
            (1
             ;; x is in fr1
             (inst fstp fr0)
             (inst fldln2)
             (inst fxch fr1))
            (t
             ;; x is in a FP reg, not fr0 or fr1
             (inst fstp fr0)
             (inst fstp fr0)
             (inst fldln2)
             (inst fldd (make-random-tn (sc-or-lose 'double-reg) (1- (tn-offset x))))))
         (inst fyl2x))
        ((long-stack descriptor-reg)
         (inst fstp fr0)
         (inst fstp fr0)
         (inst fldln2)
         (if (sc-is x long-stack)
             (inst fldl (ea-for-lf-stack x))
             (inst fldl (ea-for-lf-desc x)))
         (inst fyl2x)))
     (inst fld fr0)
     (case (tn-offset y)
       ((0 1))
       (t (inst fstd y)))))

(define-vop (flog10)
  (:translate %log10)
  (:args (x :scs (long-reg long-stack descriptor-reg) :target fr0))
  (:temporary (:sc long-reg :offset fr0-offset
                   :from :argument :to :result) fr0)
  (:temporary (:sc long-reg :offset fr1-offset
                   :from :argument :to :result) fr1)
  (:results (y :scs (long-reg)))
  (:arg-types long-float)
  (:result-types long-float)
  (:policy :fast-safe)
  (:note "inline log10 function")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 5
     (note-this-location vop :internal-error)
     (sc-case x
        (long-reg
         (case (tn-offset x)
            (0
             ;; x is in fr0
             (inst fstp fr1)
             (inst fldlg2)
             (inst fxch fr1))
            (1
             ;; x is in fr1
             (inst fstp fr0)
             (inst fldlg2)
             (inst fxch fr1))
            (t
             ;; x is in a FP reg, not fr0 or fr1
             (inst fstp fr0)
             (inst fstp fr0)
             (inst fldlg2)
             (inst fldd (make-random-tn (sc-or-lose 'double-reg) (1- (tn-offset x))))))
         (inst fyl2x))
        ((long-stack descriptor-reg)
         (inst fstp fr0)
         (inst fstp fr0)
         (inst fldlg2)
         (if (sc-is x long-stack)
             (inst fldl (ea-for-lf-stack x))
             (inst fldl (ea-for-lf-desc x)))
         (inst fyl2x)))
     (inst fld fr0)
     (case (tn-offset y)
       ((0 1))
       (t (inst fstd y)))))

(define-vop (fpow)
  (:translate %pow)
  (:args (x :scs (long-reg long-stack descriptor-reg) :target fr0)
         (y :scs (long-reg long-stack descriptor-reg) :target fr1))
  (:temporary (:sc long-reg :offset fr0-offset
                   :from (:argument 0) :to :result) fr0)
  (:temporary (:sc long-reg :offset fr1-offset
                   :from (:argument 1) :to :result) fr1)
  (:temporary (:sc long-reg :offset fr2-offset
                   :from :load :to :result) fr2)
  (:results (r :scs (long-reg)))
  (:arg-types long-float long-float)
  (:result-types long-float)
  (:policy :fast-safe)
  (:note "inline pow function")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 5
     (note-this-location vop :internal-error)
     ;; Setup x in fr0 and y in fr1
     (cond
      ;; x in fr0; y in fr1
      ((and (sc-is x long-reg) (zerop (tn-offset x))
            (sc-is y long-reg) (= 1 (tn-offset y))))
      ;; y in fr1; x not in fr0
      ((and (sc-is y long-reg) (= 1 (tn-offset y)))
       ;; Load x to fr0
       (sc-case x
          (long-reg
           (copy-fp-reg-to-fr0 x))
          (long-stack
           (inst fstp fr0)
           (inst fldl (ea-for-lf-stack x)))
          (descriptor-reg
           (inst fstp fr0)
           (inst fldl (ea-for-lf-desc x)))))
      ;; x in fr0; y not in fr1
      ((and (sc-is x long-reg) (zerop (tn-offset x)))
       (inst fxch fr1)
       ;; Now load y to fr0
       (sc-case y
          (long-reg
           (copy-fp-reg-to-fr0 y))
          (long-stack
           (inst fstp fr0)
           (inst fldl (ea-for-lf-stack y)))
          (descriptor-reg
           (inst fstp fr0)
           (inst fldl (ea-for-lf-desc y))))
       (inst fxch fr1))
      ;; x in fr1; y not in fr1
      ((and (sc-is x long-reg) (= 1 (tn-offset x)))
       ;; Load y to fr0
       (sc-case y
          (long-reg
           (copy-fp-reg-to-fr0 y))
          (long-stack
           (inst fstp fr0)
           (inst fldl (ea-for-lf-stack y)))
          (descriptor-reg
           (inst fstp fr0)
           (inst fldl (ea-for-lf-desc y))))
       (inst fxch fr1))
      ;; y in fr0;
      ((and (sc-is y long-reg) (zerop (tn-offset y)))
       (inst fxch fr1)
       ;; Now load x to fr0
       (sc-case x
          (long-reg
           (copy-fp-reg-to-fr0 x))
          (long-stack
           (inst fstp fr0)
           (inst fldl (ea-for-lf-stack x)))
          (descriptor-reg
           (inst fstp fr0)
           (inst fldl (ea-for-lf-desc x)))))
      ;; Neither x or y are in either fr0 or fr1
      (t
       ;; Load y then x
       (inst fstp fr0)
       (inst fstp fr0)
       (sc-case y
          (long-reg
           (inst fldd (make-random-tn (sc-or-lose 'double-reg) (- (tn-offset y) 2))))
          (long-stack
           (inst fldl (ea-for-lf-stack y)))
          (descriptor-reg
           (inst fldl (ea-for-lf-desc y))))
       ;; Load x to fr0
       (sc-case x
          (long-reg
           (inst fldd (make-random-tn (sc-or-lose 'double-reg) (1- (tn-offset x)))))
          (long-stack
           (inst fldl (ea-for-lf-stack x)))
          (descriptor-reg
           (inst fldl (ea-for-lf-desc x))))))

     ;; Now have x at fr0; and y at fr1
     (inst fyl2x)
     ;; Now fr0=y log2(x)
     (inst fld fr0)
     (inst frndint)
     (inst fst fr2)
     (inst fsubp-sti fr1)
     (inst f2xm1)
     (inst fld1)
     (inst faddp-sti fr1)
     (inst fscale)
     (inst fld fr0)
     (case (tn-offset r)
       ((0 1))
       (t (inst fstd r)))))

(define-vop (fscalen)
  (:translate %scalbn)
  (:args (x :scs (long-reg long-stack descriptor-reg) :target fr0)
         (y :scs (signed-stack signed-reg) :target temp))
  (:temporary (:sc long-reg :offset fr0-offset
                   :from (:argument 0) :to :result) fr0)
  (:temporary (:sc long-reg :offset fr1-offset :from :eval :to :result) fr1)
  (:temporary (:sc signed-stack :from (:argument 1) :to :result) temp)
  (:results (r :scs (long-reg)))
  (:arg-types long-float signed-num)
  (:result-types long-float)
  (:policy :fast-safe)
  (:note "inline scalbn function")
  (:generator 5
     ;; Setup x in fr0 and y in fr1
     (sc-case x
       (long-reg
        (case (tn-offset x)
          (0
           (inst fstp fr1)
           (sc-case y
             (signed-reg
              (inst mov temp y)
              (inst fild temp))
             (signed-stack
              (inst fild y)))
           (inst fxch fr1))
          (1
           (inst fstp fr0)
           (sc-case y
             (signed-reg
              (inst mov temp y)
              (inst fild temp))
             (signed-stack
              (inst fild y)))
           (inst fxch fr1))
          (t
           (inst fstp fr0)
           (inst fstp fr0)
           (sc-case y
             (signed-reg
              (inst mov temp y)
              (inst fild temp))
             (signed-stack
              (inst fild y)))
           (inst fld (make-random-tn (sc-or-lose 'double-reg) (1- (tn-offset x)))))))
       ((long-stack descriptor-reg)
        (inst fstp fr0)
        (inst fstp fr0)
        (sc-case y
          (signed-reg
           (inst mov temp y)
           (inst fild temp))
          (signed-stack
           (inst fild y)))
        (if (sc-is x long-stack)
            (inst fldl (ea-for-lf-stack x))
            (inst fldl (ea-for-lf-desc x)))))
     (inst fscale)
     (unless (zerop (tn-offset r))
       (inst fstd r))))

(define-vop (fscale)
  (:translate %scalb)
  (:args (x :scs (long-reg long-stack descriptor-reg) :target fr0)
         (y :scs (long-reg long-stack descriptor-reg) :target fr1))
  (:temporary (:sc long-reg :offset fr0-offset
                   :from (:argument 0) :to :result) fr0)
  (:temporary (:sc long-reg :offset fr1-offset
                   :from (:argument 1) :to :result) fr1)
  (:results (r :scs (long-reg)))
  (:arg-types long-float long-float)
  (:result-types long-float)
  (:policy :fast-safe)
  (:note "inline scalb function")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 5
     (note-this-location vop :internal-error)
     ;; Setup x in fr0 and y in fr1
     (cond
      ;; x in fr0; y in fr1
      ((and (sc-is x long-reg) (zerop (tn-offset x))
            (sc-is y long-reg) (= 1 (tn-offset y))))
      ;; y in fr1; x not in fr0
      ((and (sc-is y long-reg) (= 1 (tn-offset y)))
       ;; Load x to fr0
       (sc-case x
          (long-reg
           (copy-fp-reg-to-fr0 x))
          (long-stack
           (inst fstp fr0)
           (inst fldl (ea-for-lf-stack x)))
          (descriptor-reg
           (inst fstp fr0)
           (inst fldl (ea-for-lf-desc x)))))
      ;; x in fr0; y not in fr1
      ((and (sc-is x long-reg) (zerop (tn-offset x)))
       (inst fxch fr1)
       ;; Now load y to fr0
       (sc-case y
          (long-reg
           (copy-fp-reg-to-fr0 y))
          (long-stack
           (inst fstp fr0)
           (inst fldl (ea-for-lf-stack y)))
          (descriptor-reg
           (inst fstp fr0)
           (inst fldl (ea-for-lf-desc y))))
       (inst fxch fr1))
      ;; x in fr1; y not in fr1
      ((and (sc-is x long-reg) (= 1 (tn-offset x)))
       ;; Load y to fr0
       (sc-case y
          (long-reg
           (copy-fp-reg-to-fr0 y))
          (long-stack
           (inst fstp fr0)
           (inst fldl (ea-for-lf-stack y)))
          (descriptor-reg
           (inst fstp fr0)
           (inst fldl (ea-for-lf-desc y))))
       (inst fxch fr1))
      ;; y in fr0;
      ((and (sc-is y long-reg) (zerop (tn-offset y)))
       (inst fxch fr1)
       ;; Now load x to fr0
       (sc-case x
          (long-reg
           (copy-fp-reg-to-fr0 x))
          (long-stack
           (inst fstp fr0)
           (inst fldl (ea-for-lf-stack x)))
          (descriptor-reg
           (inst fstp fr0)
           (inst fldl (ea-for-lf-desc x)))))
      ;; Neither x or y are in either fr0 or fr1
      (t
       ;; Load y then x
       (inst fstp fr0)
       (inst fstp fr0)
       (sc-case y
          (long-reg
           (inst fldd (make-random-tn (sc-or-lose 'double-reg) (- (tn-offset y) 2))))
          (long-stack
           (inst fldl (ea-for-lf-stack y)))
          (descriptor-reg
           (inst fldl (ea-for-lf-desc y))))
       ;; Load x to fr0
       (sc-case x
          (long-reg
           (inst fldd (make-random-tn (sc-or-lose 'double-reg) (1- (tn-offset x)))))
          (long-stack
           (inst fldl (ea-for-lf-stack x)))
          (descriptor-reg
           (inst fldl (ea-for-lf-desc x))))))

     ;; Now have x at fr0; and y at fr1
     (inst fscale)
     (unless (zerop (tn-offset r))
             (inst fstd r))))

(define-vop (flog1p)
  (:translate %log1p)
  (:args (x :scs (long-reg) :to :result))
  (:temporary (:sc long-reg :offset fr0-offset
                   :from :argument :to :result) fr0)
  (:temporary (:sc long-reg :offset fr1-offset
                   :from :argument :to :result) fr1)
  (:temporary (:sc word-reg :offset eax-offset :from :eval) temp)
  (:results (y :scs (long-reg)))
  (:arg-types long-float)
  (:result-types long-float)
  (:policy :fast-safe)
  ;; FIXME 1: This appears to be the second DEFINE-VOP of FLOG1P.
  ;;   Perhaps this is OK because of the #+LONG-FLOAT wrapped around
  ;;   an enormous PROGN above. Still, it would be probably be good to
  ;;   add some code to warn about redefining VOPs.
  (:note "inline log1p function")
  (:ignore temp)
  (:generator 5
     ;; x is in a FP reg, not fr0, fr1.
     (inst fstp fr0)
     (inst fstp fr0)
     (inst fldd (make-random-tn (sc-or-lose 'double-reg) (- (tn-offset x) 2)))
     ;; Check the range
     (inst push #x3e947ae1)     ; Constant 0.29
     (inst fabs)
     (inst fld (make-ea :dword :base esp-tn))
     (inst fcompp)
     (inst add esp-tn 4)
     (inst fnstsw)                      ; status word to ax
     (inst and ah-tn #x45)
     (inst jmp :z WITHIN-RANGE)
     ;; Out of range for fyl2xp1.
     (inst fld1)
     (inst faddd (make-random-tn (sc-or-lose 'double-reg) (- (tn-offset x) 1)))
     (inst fldln2)
     (inst fxch fr1)
     (inst fyl2x)
     (inst jmp DONE)

     WITHIN-RANGE
     (inst fldln2)
     (inst fldd (make-random-tn (sc-or-lose 'double-reg) (- (tn-offset x) 1)))
     (inst fyl2xp1)
     DONE
     (inst fld fr0)
     (case (tn-offset y)
       ((0 1))
       (t (inst fstd y)))))

;;; The Pentium has a less restricted implementation of the fyl2xp1
;;; instruction and a range check can be avoided.
(define-vop (flog1p-pentium)
  (:translate %log1p)
  (:args (x :scs (long-reg long-stack descriptor-reg) :target fr0))
  (:temporary (:sc long-reg :offset fr0-offset
                   :from :argument :to :result) fr0)
  (:temporary (:sc long-reg :offset fr1-offset
                   :from :argument :to :result) fr1)
  (:results (y :scs (long-reg)))
  (:arg-types long-float)
  (:result-types long-float)
  (:policy :fast-safe)
  (:guard (member :pentium-style-fyl2xp1 *backend-subfeatures*))
  (:note "inline log1p function")
  (:generator 5
     (sc-case x
        (long-reg
         (case (tn-offset x)
            (0
             ;; x is in fr0
             (inst fstp fr1)
             (inst fldln2)
             (inst fxch fr1))
            (1
             ;; x is in fr1
             (inst fstp fr0)
             (inst fldln2)
             (inst fxch fr1))
            (t
             ;; x is in a FP reg, not fr0 or fr1
             (inst fstp fr0)
             (inst fstp fr0)
             (inst fldln2)
             (inst fldd (make-random-tn (sc-or-lose 'double-reg) (1- (tn-offset x)))))))
        ((long-stack descriptor-reg)
         (inst fstp fr0)
         (inst fstp fr0)
         (inst fldln2)
         (if (sc-is x long-stack)
             (inst fldl (ea-for-lf-stack x))
           (inst fldl (ea-for-lf-desc x)))))
     (inst fyl2xp1)
     (inst fld fr0)
     (case (tn-offset y)
       ((0 1))
       (t (inst fstd y)))))

(define-vop (flogb)
  (:translate %logb)
  (:args (x :scs (long-reg long-stack descriptor-reg) :target fr0))
  (:temporary (:sc long-reg :offset fr0-offset
                   :from :argument :to :result) fr0)
  (:temporary (:sc long-reg :offset fr1-offset
                   :from :argument :to :result) fr1)
  (:results (y :scs (long-reg)))
  (:arg-types long-float)
  (:result-types long-float)
  (:policy :fast-safe)
  (:note "inline logb function")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 5
     (note-this-location vop :internal-error)
     (sc-case x
        (long-reg
         (case (tn-offset x)
            (0
             ;; x is in fr0
             (inst fstp fr1))
            (1
             ;; x is in fr1
             (inst fstp fr0))
            (t
             ;; x is in a FP reg, not fr0 or fr1
             (inst fstp fr0)
             (inst fstp fr0)
             (inst fldd (make-random-tn (sc-or-lose 'double-reg) (- (tn-offset x) 2))))))
        ((long-stack descriptor-reg)
         (inst fstp fr0)
         (inst fstp fr0)
         (if (sc-is x long-stack)
             (inst fldl (ea-for-lf-stack x))
           (inst fldl (ea-for-lf-desc x)))))
     (inst fxtract)
     (case (tn-offset y)
       (0
        (inst fxch fr1))
       (1)
       (t (inst fxch fr1)
          (inst fstd y)))))

(define-vop (fatan)
  (:translate %atan)
  (:args (x :scs (long-reg long-stack descriptor-reg) :target fr0))
  (:temporary (:sc long-reg :offset fr0-offset
                   :from (:argument 0) :to :result) fr0)
  (:temporary (:sc long-reg :offset fr1-offset
                   :from (:argument 0) :to :result) fr1)
  (:results (r :scs (long-reg)))
  (:arg-types long-float)
  (:result-types long-float)
  (:policy :fast-safe)
  (:note "inline atan function")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 5
     (note-this-location vop :internal-error)
     ;; Setup x in fr1 and 1.0 in fr0
     (cond
      ;; x in fr0
      ((and (sc-is x long-reg) (zerop (tn-offset x)))
       (inst fstp fr1))
      ;; x in fr1
      ((and (sc-is x long-reg) (= 1 (tn-offset x)))
       (inst fstp fr0))
      ;; x not in fr0 or fr1
      (t
       ;; Load x then 1.0
       (inst fstp fr0)
       (inst fstp fr0)
       (sc-case x
          (long-reg
           (inst fldd (make-random-tn (sc-or-lose 'double-reg) (- (tn-offset x) 2))))
          (long-stack
           (inst fldl (ea-for-lf-stack x)))
          (descriptor-reg
           (inst fldl (ea-for-lf-desc x))))))
     (inst fld1)
     ;; Now have x at fr1; and 1.0 at fr0
     (inst fpatan)
     (inst fld fr0)
     (case (tn-offset r)
       ((0 1))
       (t (inst fstd r)))))

(define-vop (fatan2)
  (:translate %atan2)
  (:args (x :scs (long-reg long-stack descriptor-reg) :target fr1)
         (y :scs (long-reg long-stack descriptor-reg) :target fr0))
  (:temporary (:sc long-reg :offset fr0-offset
                   :from (:argument 1) :to :result) fr0)
  (:temporary (:sc long-reg :offset fr1-offset
                   :from (:argument 0) :to :result) fr1)
  (:results (r :scs (long-reg)))
  (:arg-types long-float long-float)
  (:result-types long-float)
  (:policy :fast-safe)
  (:note "inline atan2 function")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 5
     (note-this-location vop :internal-error)
     ;; Setup x in fr1 and y in fr0
     (cond
      ;; y in fr0; x in fr1
      ((and (sc-is y long-reg) (zerop (tn-offset y))
            (sc-is x long-reg) (= 1 (tn-offset x))))
      ;; x in fr1; y not in fr0
      ((and (sc-is x long-reg) (= 1 (tn-offset x)))
       ;; Load y to fr0
       (sc-case y
          (long-reg
           (copy-fp-reg-to-fr0 y))
          (long-stack
           (inst fstp fr0)
           (inst fldl (ea-for-lf-stack y)))
          (descriptor-reg
           (inst fstp fr0)
           (inst fldl (ea-for-lf-desc y)))))
      ;; y in fr0; x not in fr1
      ((and (sc-is y long-reg) (zerop (tn-offset y)))
       (inst fxch fr1)
       ;; Now load x to fr0
       (sc-case x
          (long-reg
           (copy-fp-reg-to-fr0 x))
          (long-stack
           (inst fstp fr0)
           (inst fldl (ea-for-lf-stack x)))
          (descriptor-reg
           (inst fstp fr0)
           (inst fldl (ea-for-lf-desc x))))
       (inst fxch fr1))
      ;; y in fr1; x not in fr1
      ((and (sc-is y long-reg) (= 1 (tn-offset y)))
       ;; Load x to fr0
       (sc-case x
          (long-reg
           (copy-fp-reg-to-fr0 x))
          (long-stack
           (inst fstp fr0)
           (inst fldl (ea-for-lf-stack x)))
          (descriptor-reg
           (inst fstp fr0)
           (inst fldl (ea-for-lf-desc x))))
       (inst fxch fr1))
      ;; x in fr0;
      ((and (sc-is x long-reg) (zerop (tn-offset x)))
       (inst fxch fr1)
       ;; Now load y to fr0
       (sc-case y
          (long-reg
           (copy-fp-reg-to-fr0 y))
          (long-stack
           (inst fstp fr0)
           (inst fldl (ea-for-lf-stack y)))
          (descriptor-reg
           (inst fstp fr0)
           (inst fldl (ea-for-lf-desc y)))))
      ;; Neither y or x are in either fr0 or fr1
      (t
       ;; Load x then y
       (inst fstp fr0)
       (inst fstp fr0)
       (sc-case x
          (long-reg
           (inst fldd (make-random-tn (sc-or-lose 'double-reg) (- (tn-offset x) 2))))
          (long-stack
           (inst fldl (ea-for-lf-stack x)))
          (descriptor-reg
           (inst fldl (ea-for-lf-desc x))))
       ;; Load y to fr0
       (sc-case y
          (long-reg
           (inst fldd (make-random-tn (sc-or-lose 'double-reg) (1- (tn-offset y)))))
          (long-stack
           (inst fldl (ea-for-lf-stack y)))
          (descriptor-reg
           (inst fldl (ea-for-lf-desc y))))))

     ;; Now have y at fr0; and x at fr1
     (inst fpatan)
     (inst fld fr0)
     (case (tn-offset r)
       ((0 1))
       (t (inst fstd r)))))

) ; PROGN #+LONG-FLOAT

;;;; complex float VOPs

(define-vop (make-complex-single-float)
  (:translate complex)
  (:args (real :scs (single-reg) :to :result :target r
               :load-if (not (location= real r)))
         (imag :scs (single-reg) :to :save))
  (:arg-types single-float single-float)
  (:results (r :scs (complex-single-reg) :from (:argument 0)
               :load-if (not (sc-is r complex-single-stack))))
  (:result-types complex-single-float)
  (:note "inline complex single-float creation")
  (:policy :fast-safe)
  (:generator 5
    (sc-case r
      (complex-single-reg
       (let ((r-real (complex-double-reg-real-tn r)))
         (unless (location= real r-real)
           (cond ((zerop (tn-offset r-real))
                  (copy-fp-reg-to-fr0 real))
                 ((zerop (tn-offset real))
                  (inst fstd r-real))
                 (t
                  (inst fxch real)
                  (inst fstd r-real)
                  (inst fxch real)))))
       (let ((r-imag (complex-double-reg-imag-tn r)))
         (unless (location= imag r-imag)
           (cond ((zerop (tn-offset imag))
                  (inst fstd r-imag))
                 (t
                  (inst fxch imag)
                  (inst fstd r-imag)
                  (inst fxch imag))))))
      (complex-single-stack
       (unless (location= real r)
         (cond ((zerop (tn-offset real))
                (inst fst (ea-for-csf-real-stack r)))
               (t
                (inst fxch real)
                (inst fst (ea-for-csf-real-stack r))
                (inst fxch real))))
       (inst fxch imag)
       (inst fst (ea-for-csf-imag-stack r))
       (inst fxch imag)))))

(define-vop (make-complex-double-float)
  (:translate complex)
  (:args (real :scs (double-reg) :target r
               :load-if (not (location= real r)))
         (imag :scs (double-reg) :to :save))
  (:arg-types double-float double-float)
  (:results (r :scs (complex-double-reg) :from (:argument 0)
               :load-if (not (sc-is r complex-double-stack))))
  (:result-types complex-double-float)
  (:note "inline complex double-float creation")
  (:policy :fast-safe)
  (:generator 5
    (sc-case r
      (complex-double-reg
       (let ((r-real (complex-double-reg-real-tn r)))
         (unless (location= real r-real)
           (cond ((zerop (tn-offset r-real))
                  (copy-fp-reg-to-fr0 real))
                 ((zerop (tn-offset real))
                  (inst fstd r-real))
                 (t
                  (inst fxch real)
                  (inst fstd r-real)
                  (inst fxch real)))))
       (let ((r-imag (complex-double-reg-imag-tn r)))
         (unless (location= imag r-imag)
           (cond ((zerop (tn-offset imag))
                  (inst fstd r-imag))
                 (t
                  (inst fxch imag)
                  (inst fstd r-imag)
                  (inst fxch imag))))))
      (complex-double-stack
       (unless (location= real r)
         (cond ((zerop (tn-offset real))
                (inst fstd (ea-for-cdf-real-stack r)))
               (t
                (inst fxch real)
                (inst fstd (ea-for-cdf-real-stack r))
                (inst fxch real))))
       (inst fxch imag)
       (inst fstd (ea-for-cdf-imag-stack r))
       (inst fxch imag)))))

#+long-float
(define-vop (make-complex-long-float)
  (:translate complex)
  (:args (real :scs (long-reg) :target r
               :load-if (not (location= real r)))
         (imag :scs (long-reg) :to :save))
  (:arg-types long-float long-float)
  (:results (r :scs (complex-long-reg) :from (:argument 0)
               :load-if (not (sc-is r complex-long-stack))))
  (:result-types complex-long-float)
  (:note "inline complex long-float creation")
  (:policy :fast-safe)
  (:generator 5
    (sc-case r
      (complex-long-reg
       (let ((r-real (complex-double-reg-real-tn r)))
         (unless (location= real r-real)
           (cond ((zerop (tn-offset r-real))
                  (copy-fp-reg-to-fr0 real))
                 ((zerop (tn-offset real))
                  (inst fstd r-real))
                 (t
                  (inst fxch real)
                  (inst fstd r-real)
                  (inst fxch real)))))
       (let ((r-imag (complex-double-reg-imag-tn r)))
         (unless (location= imag r-imag)
           (cond ((zerop (tn-offset imag))
                  (inst fstd r-imag))
                 (t
                  (inst fxch imag)
                  (inst fstd r-imag)
                  (inst fxch imag))))))
      (complex-long-stack
       (unless (location= real r)
         (cond ((zerop (tn-offset real))
                (store-long-float (ea-for-clf-real-stack r)))
               (t
                (inst fxch real)
                (store-long-float (ea-for-clf-real-stack r))
                (inst fxch real))))
       (inst fxch imag)
       (store-long-float (ea-for-clf-imag-stack r))
       (inst fxch imag)))))


(define-vop (complex-float-value)
  (:args (x :target r))
  (:results (r))
  (:variant-vars offset)
  (:policy :fast-safe)
  (:generator 3
    (cond ((sc-is x complex-single-reg complex-double-reg
                  #+long-float complex-long-reg)
           (let ((value-tn
                  (make-random-tn (sc-or-lose 'double-reg) (+ offset (tn-offset x)))))
             (unless (location= value-tn r)
               (cond ((zerop (tn-offset r))
                      (copy-fp-reg-to-fr0 value-tn))
                     ((zerop (tn-offset value-tn))
                      (inst fstd r))
                     (t
                      (inst fxch value-tn)
                      (inst fstd r)
                      (inst fxch value-tn))))))
          ((sc-is r single-reg)
           (let ((ea (sc-case x
                       (complex-single-stack
                        (ecase offset
                          (0 (ea-for-csf-real-stack x))
                          (1 (ea-for-csf-imag-stack x))))
                       (descriptor-reg
                        (ecase offset
                          (0 (ea-for-csf-real-desc x))
                          (1 (ea-for-csf-imag-desc x)))))))
             (with-empty-tn@fp-top(r)
               (inst fld ea))))
          ((sc-is r double-reg)
           (let ((ea (sc-case x
                       (complex-double-stack
                        (ecase offset
                          (0 (ea-for-cdf-real-stack x))
                          (1 (ea-for-cdf-imag-stack x))))
                       (descriptor-reg
                        (ecase offset
                          (0 (ea-for-cdf-real-desc x))
                          (1 (ea-for-cdf-imag-desc x)))))))
             (with-empty-tn@fp-top(r)
               (inst fldd ea))))
          #+long-float
          ((sc-is r long-reg)
           (let ((ea (sc-case x
                       (complex-long-stack
                        (ecase offset
                          (0 (ea-for-clf-real-stack x))
                          (1 (ea-for-clf-imag-stack x))))
                       (descriptor-reg
                        (ecase offset
                          (0 (ea-for-clf-real-desc x))
                          (1 (ea-for-clf-imag-desc x)))))))
             (with-empty-tn@fp-top(r)
               (inst fldl ea))))
          (t (error "COMPLEX-FLOAT-VALUE VOP failure")))))

(define-vop (realpart/complex-single-float complex-float-value)
  (:translate realpart)
  (:args (x :scs (complex-single-reg complex-single-stack descriptor-reg)
            :target r))
  (:arg-types complex-single-float)
  (:results (r :scs (single-reg)))
  (:result-types single-float)
  (:note "complex float realpart")
  (:variant 0))

(define-vop (realpart/complex-double-float complex-float-value)
  (:translate realpart)
  (:args (x :scs (complex-double-reg complex-double-stack descriptor-reg)
            :target r))
  (:arg-types complex-double-float)
  (:results (r :scs (double-reg)))
  (:result-types double-float)
  (:note "complex float realpart")
  (:variant 0))

#+long-float
(define-vop (realpart/complex-long-float complex-float-value)
  (:translate realpart)
  (:args (x :scs (complex-long-reg complex-long-stack descriptor-reg)
            :target r))
  (:arg-types complex-long-float)
  (:results (r :scs (long-reg)))
  (:result-types long-float)
  (:note "complex float realpart")
  (:variant 0))

(define-vop (imagpart/complex-single-float complex-float-value)
  (:translate imagpart)
  (:args (x :scs (complex-single-reg complex-single-stack descriptor-reg)
            :target r))
  (:arg-types complex-single-float)
  (:results (r :scs (single-reg)))
  (:result-types single-float)
  (:note "complex float imagpart")
  (:variant 1))

(define-vop (imagpart/complex-double-float complex-float-value)
  (:translate imagpart)
  (:args (x :scs (complex-double-reg complex-double-stack descriptor-reg)
            :target r))
  (:arg-types complex-double-float)
  (:results (r :scs (double-reg)))
  (:result-types double-float)
  (:note "complex float imagpart")
  (:variant 1))

#+long-float
(define-vop (imagpart/complex-long-float complex-float-value)
  (:translate imagpart)
  (:args (x :scs (complex-long-reg complex-long-stack descriptor-reg)
            :target r))
  (:arg-types complex-long-float)
  (:results (r :scs (long-reg)))
  (:result-types long-float)
  (:note "complex float imagpart")
  (:variant 1))

;;; hack dummy VOPs to bias the representation selection of their
;;; arguments towards a FP register, which can help avoid consing at
;;; inappropriate locations
(defknown double-float-reg-bias (double-float) (values))
(define-vop (double-float-reg-bias)
  (:translate double-float-reg-bias)
  (:args (x :scs (double-reg double-stack) :load-if nil))
  (:arg-types double-float)
  (:policy :fast-safe)
  (:note "inline dummy FP register bias")
  (:ignore x)
  (:generator 0))
(defknown single-float-reg-bias (single-float) (values))
(define-vop (single-float-reg-bias)
  (:translate single-float-reg-bias)
  (:args (x :scs (single-reg single-stack) :load-if nil))
  (:arg-types single-float)
  (:policy :fast-safe)
  (:note "inline dummy FP register bias")
  (:ignore x)
  (:generator 0))
