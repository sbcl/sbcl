;;;; floating point support for the Sparc

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;;; float move functions

(define-move-fun (load-single 1) (vop x y)
  ((single-stack) (single-reg))
  (inst ldf y (current-nfp-tn vop) (tn-byte-offset x)))

(define-move-fun (store-single 1) (vop x y)
  ((single-reg) (single-stack))
  (inst stf x (current-nfp-tn vop) (tn-byte-offset y)))


(define-move-fun (load-double 2) (vop x y)
  ((double-stack) (double-reg))
  (let ((nfp (current-nfp-tn vop))
        (offset (tn-byte-offset x)))
    (inst lddf y nfp offset)))

(define-move-fun (store-double 2) (vop x y)
  ((double-reg) (double-stack))
  (let ((nfp (current-nfp-tn vop))
        (offset (tn-byte-offset y)))
    (inst stdf x nfp offset)))

;;; The offset may be an integer or a TN in which case it will be
;;; temporarily modified but is restored if restore-offset is true.
(defun load-long-reg (reg base offset &optional (restore-offset t))
  (cond
    ((member :sparc-v9 *backend-subfeatures*)
     (inst ldqf reg base offset))
    (t
     (let ((reg0 (make-random-tn :kind :normal
                                 :sc (sc-or-lose 'double-reg)
                                 :offset (tn-offset reg)))
           (reg2 (make-random-tn :kind :normal
                                 :sc (sc-or-lose 'double-reg)
                                 :offset (+ 2 (tn-offset reg)))))
       (cond ((integerp offset)
              (inst lddf reg0 base offset)
              (inst lddf reg2 base (+ offset (* 2 n-word-bytes))))
             (t
              (inst lddf reg0 base offset)
              (inst add offset (* 2 n-word-bytes))
              (inst lddf reg2 base offset)
              (when restore-offset
                (inst sub offset (* 2 n-word-bytes)))))))))

#+long-float
(define-move-fun (load-long 2) (vop x y)
  ((long-stack) (long-reg))
  (let ((nfp (current-nfp-tn vop))
        (offset (tn-byte-offset x)))
    (load-long-reg y nfp offset)))

;;; The offset may be an integer or a TN in which case it will be
;;; temporarily modified but is restored if restore-offset is true.
(defun store-long-reg (reg base offset &optional (restore-offset t))
  (cond
    ((member :sparc-v9 *backend-subfeatures*)
     (inst stqf reg base offset))
    (t
     (let ((reg0 (make-random-tn :kind :normal
                                 :sc (sc-or-lose 'double-reg)
                                 :offset (tn-offset reg)))
           (reg2 (make-random-tn :kind :normal
                                 :sc (sc-or-lose 'double-reg)
                                 :offset (+ 2 (tn-offset reg)))))
       (cond ((integerp offset)
              (inst stdf reg0 base offset)
              (inst stdf reg2 base (+ offset (* 2 n-word-bytes))))
             (t
              (inst stdf reg0 base offset)
              (inst add offset (* 2 n-word-bytes))
              (inst stdf reg2 base offset)
              (when restore-offset
                (inst sub offset (* 2 n-word-bytes)))))))))

#+long-float
(define-move-fun (store-long 2) (vop x y)
  ((long-reg) (long-stack))
  (let ((nfp (current-nfp-tn vop))
        (offset (tn-byte-offset y)))
    (store-long-reg x nfp offset)))


;;;; Move VOPs:

;;; Exploit the V9 double-float move instruction. This is conditional
;;; on the :sparc-v9 feature.
(defun move-double-reg (dst src)
  (cond
    ((member :sparc-v9 *backend-subfeatures*)
     (inst fmovd dst src))
    (t
     (dotimes (i 2)
       (let ((dst (make-random-tn :kind :normal
                                  :sc (sc-or-lose 'single-reg)
                                  :offset (+ i (tn-offset dst))))
             (src (make-random-tn :kind :normal
                                  :sc (sc-or-lose 'single-reg)
                                  :offset (+ i (tn-offset src)))))
         (inst fmovs dst src))))))

;;; Exploit the V9 long-float move instruction. This is conditional
;;; on the :sparc-v9 feature.
(defun move-long-reg (dst src)
  (cond
    ((member :sparc-v9 *backend-subfeatures*)
     (inst fmovq dst src))
    (t
     (dotimes (i 4)
       (let ((dst (make-random-tn :kind :normal
                                  :sc (sc-or-lose 'single-reg)
                                  :offset (+ i (tn-offset dst))))
             (src (make-random-tn :kind :normal
                                  :sc (sc-or-lose 'single-reg)
                                  :offset (+ i (tn-offset src)))))
         (inst fmovs dst src))))))

(macrolet ((frob (vop sc format)
             `(progn
                (define-vop (,vop)
                  (:args (x :scs (,sc)
                            :target y
                            :load-if (not (location= x y))))
                  (:results (y :scs (,sc)
                               :load-if (not (location= x y))))
                  (:note "float move")
                  (:generator 0
                    (unless (location= y x)
                      ,@(ecase format
                          (:single `((inst fmovs y x)))
                          (:double `((move-double-reg y x)))
                          (:long `((move-long-reg y x)))))))
                (define-move-vop ,vop :move (,sc) (,sc)))))
  (frob single-move single-reg :single)
  (frob double-move double-reg :double)
  #+long-float
  (frob long-move long-reg :long))


(define-vop (move-from-float)
  (:args (x :to :save))
  (:results (y))
  (:note "float to pointer coercion")
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:variant-vars format size type data)
  (:generator 13
    (with-fixed-allocation (y ndescr type size)
      (ecase format
        (:single
         (inst stf x y (- (* data n-word-bytes) other-pointer-lowtag)))
        (:double
         (inst stdf x y (- (* data n-word-bytes) other-pointer-lowtag)))
        (:long
         (store-long-reg x y (- (* data n-word-bytes)
                                other-pointer-lowtag)))))))

(macrolet ((frob (name sc &rest args)
             `(progn
                (define-vop (,name move-from-float)
                  (:args (x :scs (,sc) :to :save))
                  (:results (y :scs (descriptor-reg)))
                  (:variant ,@args))
                (define-move-vop ,name :move (,sc) (descriptor-reg)))))
  (frob move-from-single single-reg :single
    single-float-size single-float-widetag single-float-value-slot)
  (frob move-from-double double-reg :double
    double-float-size double-float-widetag double-float-value-slot)
  #+long-float
  (frob move-from-long long-reg :long
     long-float-size long-float-widetag long-float-value-slot))

(macrolet ((frob (name sc format value)
             `(progn
                (define-vop (,name)
                  (:args (x :scs (descriptor-reg)))
                  (:results (y :scs (,sc)))
                  (:note "pointer to float coercion")
                  (:generator 2
                    (inst ,(ecase format
                             (:single 'ldf)
                             (:double 'lddf))
                          y x
                          (- (* ,value n-word-bytes) other-pointer-lowtag))))
                (define-move-vop ,name :move (descriptor-reg) (,sc)))))
  (frob move-to-single single-reg :single single-float-value-slot)
  (frob move-to-double double-reg :double double-float-value-slot))

#+long-float
(define-vop (move-to-long)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (long-reg)))
  (:note "pointer to float coercion")
  (:generator 2
    (load-long-reg y x (- (* long-float-value-slot n-word-bytes)
                          other-pointer-lowtag))))
#+long-float
(define-move-vop move-to-long :move (descriptor-reg) (long-reg))

(macrolet ((frob (name sc stack-sc format)
             `(progn
                (define-vop (,name)
                  (:args (x :scs (,sc) :target y)
                         (nfp :scs (any-reg)
                              :load-if (not (sc-is y ,sc))))
                  (:results (y))
                  (:note "float argument move")
                  (:generator ,(ecase format (:single 1) (:double 2))
                    (sc-case y
                      (,sc
                       (unless (location= x y)
                         ,@(ecase format
                             (:single '((inst fmovs y x)))
                             (:double '((move-double-reg y x))))))
                      (,stack-sc
                       (let ((offset (tn-byte-offset y)))
                         (inst ,(ecase format
                                  (:single 'stf)
                                  (:double 'stdf))
                               x nfp offset))))))
                (define-move-vop ,name :move-arg
                  (,sc descriptor-reg) (,sc)))))
  (frob move-single-float-arg single-reg single-stack :single)
  (frob move-double-float-arg double-reg double-stack :double))

#+long-float
(define-vop (move-long-float-arg)
  (:args (x :scs (long-reg) :target y)
         (nfp :scs (any-reg) :load-if (not (sc-is y long-reg))))
  (:results (y))
  (:note "float argument move")
  (:generator 3
    (sc-case y
      (long-reg
       (unless (location= x y)
         (move-long-reg y x)))
      (long-stack
       (let ((offset (tn-byte-offset y)))
         (store-long-reg x nfp offset))))))
;;;
#+long-float
(define-move-vop move-long-float-arg :move-arg
  (long-reg descriptor-reg) (long-reg))


;;;; Complex float move functions

(defun complex-single-reg-real-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'single-reg)
                  :offset (tn-offset x)))
(defun complex-single-reg-imag-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'single-reg)
                  :offset (1+ (tn-offset x))))

(defun complex-double-reg-real-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg)
                  :offset (tn-offset x)))
(defun complex-double-reg-imag-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg)
                  :offset (+ (tn-offset x) 2)))

#+long-float
(defun complex-long-reg-real-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'long-reg)
                  :offset (tn-offset x)))
#+long-float
(defun complex-long-reg-imag-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'long-reg)
                  :offset (+ (tn-offset x) 4)))


(define-move-fun (load-complex-single 2) (vop x y)
  ((complex-single-stack) (complex-single-reg))
  (let ((nfp (current-nfp-tn vop))
        (offset (tn-byte-offset x)))
    (let ((real-tn (complex-single-reg-real-tn y)))
      (inst ldf real-tn nfp offset))
    (let ((imag-tn (complex-single-reg-imag-tn y)))
      (inst ldf imag-tn nfp (+ offset n-word-bytes)))))

(define-move-fun (store-complex-single 2) (vop x y)
  ((complex-single-reg) (complex-single-stack))
  (let ((nfp (current-nfp-tn vop))
        (offset (tn-byte-offset y)))
    (let ((real-tn (complex-single-reg-real-tn x)))
      (inst stf real-tn nfp offset))
    (let ((imag-tn (complex-single-reg-imag-tn x)))
      (inst stf imag-tn nfp (+ offset n-word-bytes)))))


(define-move-fun (load-complex-double 4) (vop x y)
  ((complex-double-stack) (complex-double-reg))
  (let ((nfp (current-nfp-tn vop))
        (offset (tn-byte-offset x)))
    (let ((real-tn (complex-double-reg-real-tn y)))
      (inst lddf real-tn nfp offset))
    (let ((imag-tn (complex-double-reg-imag-tn y)))
      (inst lddf imag-tn nfp (+ offset (* 2 n-word-bytes))))))

(define-move-fun (store-complex-double 4) (vop x y)
  ((complex-double-reg) (complex-double-stack))
  (let ((nfp (current-nfp-tn vop))
        (offset (tn-byte-offset y)))
    (let ((real-tn (complex-double-reg-real-tn x)))
      (inst stdf real-tn nfp offset))
    (let ((imag-tn (complex-double-reg-imag-tn x)))
      (inst stdf imag-tn nfp (+ offset (* 2 n-word-bytes))))))


#+long-float
(define-move-fun (load-complex-long 5) (vop x y)
  ((complex-long-stack) (complex-long-reg))
  (let ((nfp (current-nfp-tn vop))
        (offset (tn-byte-offset x)))
    (let ((real-tn (complex-long-reg-real-tn y)))
      (load-long-reg real-tn nfp offset))
    (let ((imag-tn (complex-long-reg-imag-tn y)))
      (load-long-reg imag-tn nfp (+ offset (* 4 n-word-bytes))))))

#+long-float
(define-move-fun (store-complex-long 5) (vop x y)
  ((complex-long-reg) (complex-long-stack))
  (let ((nfp (current-nfp-tn vop))
        (offset (tn-byte-offset y)))
    (let ((real-tn (complex-long-reg-real-tn x)))
      (store-long-reg real-tn nfp offset))
    (let ((imag-tn (complex-long-reg-imag-tn x)))
      (store-long-reg imag-tn nfp (+ offset (* 4 n-word-bytes))))))

;;;
;;; Complex float register to register moves.
;;;
(define-vop (complex-single-move)
  (:args (x :scs (complex-single-reg) :target y
            :load-if (not (location= x y))))
  (:results (y :scs (complex-single-reg) :load-if (not (location= x y))))
  (:note "complex single float move")
  (:generator 0
     (unless (location= x y)
       ;; Note the complex-float-regs are aligned to every second
       ;; float register so there is not need to worry about overlap.
       (let ((x-real (complex-single-reg-real-tn x))
             (y-real (complex-single-reg-real-tn y)))
         (inst fmovs y-real x-real))
       (let ((x-imag (complex-single-reg-imag-tn x))
             (y-imag (complex-single-reg-imag-tn y)))
         (inst fmovs y-imag x-imag)))))
;;;
(define-move-vop complex-single-move :move
  (complex-single-reg) (complex-single-reg))

(define-vop (complex-double-move)
  (:args (x :scs (complex-double-reg)
            :target y :load-if (not (location= x y))))
  (:results (y :scs (complex-double-reg) :load-if (not (location= x y))))
  (:note "complex double float move")
  (:generator 0
     (unless (location= x y)
       ;; Note the complex-float-regs are aligned to every second
       ;; float register so there is not need to worry about overlap.
       (let ((x-real (complex-double-reg-real-tn x))
             (y-real (complex-double-reg-real-tn y)))
         (move-double-reg y-real x-real))
       (let ((x-imag (complex-double-reg-imag-tn x))
             (y-imag (complex-double-reg-imag-tn y)))
         (move-double-reg y-imag x-imag)))))
;;;
(define-move-vop complex-double-move :move
  (complex-double-reg) (complex-double-reg))

#+long-float
(define-vop (complex-long-move)
  (:args (x :scs (complex-long-reg)
            :target y :load-if (not (location= x y))))
  (:results (y :scs (complex-long-reg) :load-if (not (location= x y))))
  (:note "complex long float move")
  (:generator 0
     (unless (location= x y)
       ;; Note the complex-float-regs are aligned to every second
       ;; float register so there is not need to worry about overlap.
       (let ((x-real (complex-long-reg-real-tn x))
             (y-real (complex-long-reg-real-tn y)))
         (move-long-reg y-real x-real))
       (let ((x-imag (complex-long-reg-imag-tn x))
             (y-imag (complex-long-reg-imag-tn y)))
         (move-long-reg y-imag x-imag)))))
;;;
#+long-float
(define-move-vop complex-long-move :move
  (complex-long-reg) (complex-long-reg))

;;;
;;; Move from a complex float to a descriptor register allocating a
;;; new complex float object in the process.
;;;
(define-vop (move-from-complex-single)
  (:args (x :scs (complex-single-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:note "complex single float to pointer coercion")
  (:generator 13
     (with-fixed-allocation (y ndescr complex-single-float-widetag
                               complex-single-float-size)
       (let ((real-tn (complex-single-reg-real-tn x)))
         (inst stf real-tn y (- (* complex-single-float-real-slot
                                   n-word-bytes)
                                other-pointer-lowtag)))
       (let ((imag-tn (complex-single-reg-imag-tn x)))
         (inst stf imag-tn y (- (* complex-single-float-imag-slot
                                   n-word-bytes)
                                other-pointer-lowtag))))))
;;;
(define-move-vop move-from-complex-single :move
  (complex-single-reg) (descriptor-reg))

(define-vop (move-from-complex-double)
  (:args (x :scs (complex-double-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:note "complex double float to pointer coercion")
  (:generator 13
     (with-fixed-allocation (y ndescr complex-double-float-widetag
                               complex-double-float-size)
       (let ((real-tn (complex-double-reg-real-tn x)))
         (inst stdf real-tn y (- (* complex-double-float-real-slot
                                    n-word-bytes)
                                 other-pointer-lowtag)))
       (let ((imag-tn (complex-double-reg-imag-tn x)))
         (inst stdf imag-tn y (- (* complex-double-float-imag-slot
                                    n-word-bytes)
                                 other-pointer-lowtag))))))
;;;
(define-move-vop move-from-complex-double :move
  (complex-double-reg) (descriptor-reg))

#+long-float
(define-vop (move-from-complex-long)
  (:args (x :scs (complex-long-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:note "complex long float to pointer coercion")
  (:generator 13
     (with-fixed-allocation (y ndescr complex-long-float-widetag
                               complex-long-float-size)
       (let ((real-tn (complex-long-reg-real-tn x)))
         (store-long-reg real-tn y (- (* complex-long-float-real-slot
                                         n-word-bytes)
                                      other-pointer-lowtag)))
       (let ((imag-tn (complex-long-reg-imag-tn x)))
         (store-long-reg imag-tn y (- (* complex-long-float-imag-slot
                                         n-word-bytes)
                                      other-pointer-lowtag))))))
;;;
#+long-float
(define-move-vop move-from-complex-long :move
  (complex-long-reg) (descriptor-reg))

;;;
;;; Move from a descriptor to a complex float register
;;;
(define-vop (move-to-complex-single)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (complex-single-reg)))
  (:note "pointer to complex float coercion")
  (:generator 2
    (let ((real-tn (complex-single-reg-real-tn y)))
      (inst ldf real-tn x (- (* complex-single-float-real-slot n-word-bytes)
                             other-pointer-lowtag)))
    (let ((imag-tn (complex-single-reg-imag-tn y)))
      (inst ldf imag-tn x (- (* complex-single-float-imag-slot n-word-bytes)
                             other-pointer-lowtag)))))
(define-move-vop move-to-complex-single :move
  (descriptor-reg) (complex-single-reg))

(define-vop (move-to-complex-double)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (complex-double-reg)))
  (:note "pointer to complex float coercion")
  (:generator 2
    (let ((real-tn (complex-double-reg-real-tn y)))
      (inst lddf real-tn x (- (* complex-double-float-real-slot n-word-bytes)
                              other-pointer-lowtag)))
    (let ((imag-tn (complex-double-reg-imag-tn y)))
      (inst lddf imag-tn x (- (* complex-double-float-imag-slot n-word-bytes)
                              other-pointer-lowtag)))))
(define-move-vop move-to-complex-double :move
  (descriptor-reg) (complex-double-reg))

#+long-float
(define-vop (move-to-complex-long)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (complex-long-reg)))
  (:note "pointer to complex float coercion")
  (:generator 2
    (let ((real-tn (complex-long-reg-real-tn y)))
      (load-long-reg real-tn x (- (* complex-long-float-real-slot n-word-bytes)
                                  other-pointer-lowtag)))
    (let ((imag-tn (complex-long-reg-imag-tn y)))
      (load-long-reg imag-tn x (- (* complex-long-float-imag-slot n-word-bytes)
                                  other-pointer-lowtag)))))
#+long-float
(define-move-vop move-to-complex-long :move
  (descriptor-reg) (complex-long-reg))

;;;
;;; Complex float move-arg vop
;;;
(define-vop (move-complex-single-float-arg)
  (:args (x :scs (complex-single-reg) :target y)
         (nfp :scs (any-reg) :load-if (not (sc-is y complex-single-reg))))
  (:results (y))
  (:note "complex single-float argument move")
  (:generator 1
    (sc-case y
      (complex-single-reg
       (unless (location= x y)
         (let ((x-real (complex-single-reg-real-tn x))
               (y-real (complex-single-reg-real-tn y)))
           (inst fmovs y-real x-real))
         (let ((x-imag (complex-single-reg-imag-tn x))
               (y-imag (complex-single-reg-imag-tn y)))
           (inst fmovs y-imag x-imag))))
      (complex-single-stack
       (let ((offset (tn-byte-offset y)))
         (let ((real-tn (complex-single-reg-real-tn x)))
           (inst stf real-tn nfp offset))
         (let ((imag-tn (complex-single-reg-imag-tn x)))
           (inst stf imag-tn nfp (+ offset n-word-bytes))))))))
(define-move-vop move-complex-single-float-arg :move-arg
  (complex-single-reg descriptor-reg) (complex-single-reg))

(define-vop (move-complex-double-float-arg)
  (:args (x :scs (complex-double-reg) :target y)
         (nfp :scs (any-reg) :load-if (not (sc-is y complex-double-reg))))
  (:results (y))
  (:note "complex double-float argument move")
  (:generator 2
    (sc-case y
      (complex-double-reg
       (unless (location= x y)
         (let ((x-real (complex-double-reg-real-tn x))
               (y-real (complex-double-reg-real-tn y)))
           (move-double-reg y-real x-real))
         (let ((x-imag (complex-double-reg-imag-tn x))
               (y-imag (complex-double-reg-imag-tn y)))
           (move-double-reg y-imag x-imag))))
      (complex-double-stack
       (let ((offset (tn-byte-offset y)))
         (let ((real-tn (complex-double-reg-real-tn x)))
           (inst stdf real-tn nfp offset))
         (let ((imag-tn (complex-double-reg-imag-tn x)))
           (inst stdf imag-tn nfp (+ offset (* 2 n-word-bytes)))))))))
(define-move-vop move-complex-double-float-arg :move-arg
  (complex-double-reg descriptor-reg) (complex-double-reg))

#+long-float
(define-vop (move-complex-long-float-arg)
  (:args (x :scs (complex-long-reg) :target y)
         (nfp :scs (any-reg) :load-if (not (sc-is y complex-long-reg))))
  (:results (y))
  (:note "complex long-float argument move")
  (:generator 2
    (sc-case y
      (complex-long-reg
       (unless (location= x y)
         (let ((x-real (complex-long-reg-real-tn x))
               (y-real (complex-long-reg-real-tn y)))
           (move-long-reg y-real x-real))
         (let ((x-imag (complex-long-reg-imag-tn x))
               (y-imag (complex-long-reg-imag-tn y)))
           (move-long-reg y-imag x-imag))))
      (complex-long-stack
       (let ((offset (tn-byte-offset y)))
         (let ((real-tn (complex-long-reg-real-tn x)))
           (store-long-reg real-tn nfp offset))
         (let ((imag-tn (complex-long-reg-imag-tn x)))
           (store-long-reg imag-tn nfp (+ offset (* 4 n-word-bytes)))))))))
#+long-float
(define-move-vop move-complex-long-float-arg :move-arg
  (complex-long-reg descriptor-reg) (complex-long-reg))


(define-move-vop move-arg :move-arg
  (single-reg double-reg #+long-float long-reg
   complex-single-reg complex-double-reg #+long-float complex-long-reg)
  (descriptor-reg))


;;;; Arithmetic VOPs:

(define-vop (float-op)
  (:args (x) (y))
  (:results (r))
  (:policy :fast-safe)
  (:note "inline float arithmetic")
  (:vop-var vop)
  (:save-p :compute-only))

(macrolet ((frob (name sc ptype)
             `(define-vop (,name float-op)
                (:args (x :scs (,sc))
                       (y :scs (,sc)))
                (:results (r :scs (,sc)))
                (:arg-types ,ptype ,ptype)
                (:result-types ,ptype))))
  (frob single-float-op single-reg single-float)
  (frob double-float-op double-reg double-float)
  #+long-float
  (frob long-float-op long-reg long-float))

(macrolet ((frob (op sinst sname scost dinst dname dcost)
             `(progn
                (define-vop (,sname single-float-op)
                  (:translate ,op)
                  (:generator ,scost
                    (inst ,sinst r x y)))
                (define-vop (,dname double-float-op)
                  (:translate ,op)
                  (:generator ,dcost
                    (inst ,dinst r x y))))))
  (frob + fadds +/single-float 2 faddd +/double-float 2)
  (frob - fsubs -/single-float 2 fsubd -/double-float 2)
  (frob * fmuls */single-float 4 fmuld */double-float 5)
  (frob / fdivs //single-float 12 fdivd //double-float 19))

#+long-float
(macrolet ((frob (op linst lname lcost)
             `(define-vop (,lname long-float-op)
                  (:translate ,op)
                  (:generator ,lcost
                    (inst ,linst r x y)))))
  (frob + faddq +/long-float 2)
  (frob - fsubq -/long-float 2)
  (frob * fmulq */long-float 6)
  (frob / fdivq //long-float 20))


(macrolet ((frob (name inst translate sc type)
             `(define-vop (,name)
                (:args (x :scs (,sc)))
                (:results (y :scs (,sc)))
                (:translate ,translate)
                (:policy :fast-safe)
                (:arg-types ,type)
                (:result-types ,type)
                (:note "inline float arithmetic")
                (:vop-var vop)
                (:save-p :compute-only)
                (:generator 1
                  (note-this-location vop :internal-error)
                  (inst ,inst y x)))))
  (frob abs/single-float fabss abs single-reg single-float)
  (frob %negate/single-float fnegs %negate single-reg single-float))

(defun negate-double-reg (dst src)
  (cond
    ((member :sparc-v9 *backend-subfeatures*)
     (inst fnegd dst src))
    (t
     ;; Negate the MS part of the numbers, then copy over the rest
     ;; of the bits.
     (inst fnegs dst src)
     (let ((dst-odd (make-random-tn :kind :normal
                                    :sc (sc-or-lose 'single-reg)
                                    :offset (+ 1 (tn-offset dst))))
           (src-odd (make-random-tn :kind :normal
                                    :sc (sc-or-lose 'single-reg)
                                    :offset (+ 1 (tn-offset src)))))
       (inst fmovs dst-odd src-odd)))))

(defun abs-double-reg (dst src)
  (cond
    ((member :sparc-v9 *backend-subfeatures*)
     (inst fabsd dst src))
    (t
     ;; Abs the MS part of the numbers, then copy over the rest
     ;; of the bits.
     (inst fabss dst src)
     (let ((dst-2 (make-random-tn :kind :normal
                                  :sc (sc-or-lose 'single-reg)
                                  :offset (+ 1 (tn-offset dst))))
           (src-2 (make-random-tn :kind :normal
                                  :sc (sc-or-lose 'single-reg)
                                  :offset (+ 1 (tn-offset src)))))
       (inst fmovs dst-2 src-2)))))

(define-vop (abs/double-float)
  (:args (x :scs (double-reg)))
  (:results (y :scs (double-reg)))
  (:translate abs)
  (:policy :fast-safe)
  (:arg-types double-float)
  (:result-types double-float)
  (:note "inline float arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 1
    (note-this-location vop :internal-error)
    (abs-double-reg y x)))

(define-vop (%negate/double-float)
  (:args (x :scs (double-reg)))
  (:results (y :scs (double-reg)))
  (:translate %negate)
  (:policy :fast-safe)
  (:arg-types double-float)
  (:result-types double-float)
  (:note "inline float arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 1
    (note-this-location vop :internal-error)
    (negate-double-reg y x)))

#+long-float
(define-vop (abs/long-float)
  (:args (x :scs (long-reg)))
  (:results (y :scs (long-reg)))
  (:translate abs)
  (:policy :fast-safe)
  (:arg-types long-float)
  (:result-types long-float)
  (:note "inline float arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 1
    (note-this-location vop :internal-error)
    (cond
      ((member :sparc-v9 *backend-subfeatures*)
       (inst fabsq y x))
      (t
       (inst fabss y x)
       (dotimes (i 3)
         (let ((y-odd (make-random-tn
                       :kind :normal
                       :sc (sc-or-lose 'single-reg)
                       :offset (+ i 1 (tn-offset y))))
               (x-odd (make-random-tn
                       :kind :normal
                       :sc (sc-or-lose 'single-reg)
                       :offset (+ i 1 (tn-offset x)))))
           (inst fmovs y-odd x-odd)))))))

#+long-float
(define-vop (%negate/long-float)
  (:args (x :scs (long-reg)))
  (:results (y :scs (long-reg)))
  (:translate %negate)
  (:policy :fast-safe)
  (:arg-types long-float)
  (:result-types long-float)
  (:note "inline float arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 1
    (note-this-location vop :internal-error)
    (cond
      ((member :sparc-v9 *backend-subfeatures*)
       (inst fnegq y x))
      (t
       (inst fnegs y x)
       (dotimes (i 3)
         (let ((y-odd (make-random-tn
                       :kind :normal
                       :sc (sc-or-lose 'single-reg)
                       :offset (+ i 1 (tn-offset y))))
               (x-odd (make-random-tn
                       :kind :normal
                       :sc (sc-or-lose 'single-reg)
                       :offset (+ i 1 (tn-offset x)))))
           (inst fmovs y-odd x-odd)))))))


;;;; Comparison:

(define-vop (float-compare)
  (:args (x) (y))
  (:conditional)
  (:info target not-p)
  (:variant-vars format yep nope)
  (:policy :fast-safe)
  (:note "inline float comparison")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 3
    (note-this-location vop :internal-error)
    (ecase format
      (:single (inst fcmps x y))
      (:double (inst fcmpd x y))
      (:long (inst fcmpq x y)))
    ;; The SPARC V9 doesn't need an instruction between a
    ;; floating-point compare and a floating-point branch.
    (unless (member :sparc-v9 *backend-subfeatures*)
      (inst nop))
    (inst fb (if not-p nope yep) target)
    (inst nop)))

(macrolet ((frob (name sc ptype)
             `(define-vop (,name float-compare)
                (:args (x :scs (,sc))
                       (y :scs (,sc)))
                (:arg-types ,ptype ,ptype))))
  (frob single-float-compare single-reg single-float)
  (frob double-float-compare double-reg double-float)
  #+long-float
  (frob long-float-compare long-reg long-float))

(macrolet ((frob (translate yep nope sname dname #+long-float lname)
             `(progn
                (define-vop (,sname single-float-compare)
                  (:translate ,translate)
                  (:variant :single ,yep ,nope))
                (define-vop (,dname double-float-compare)
                  (:translate ,translate)
                  (:variant :double ,yep ,nope))
                #+long-float
                (define-vop (,lname long-float-compare)
                  (:translate ,translate)
                  (:variant :long ,yep ,nope)))))
  (frob < :l :ge </single-float </double-float #+long-float </long-float)
  (frob > :g :le >/single-float >/double-float #+long-float >/long-float)
  (frob = :eq :ne =/single-float =/double-float #+long-float =/long-float))

#+long-float
(deftransform eql ((x y) (long-float long-float))
  '(and (= (long-float-low-bits x) (long-float-low-bits y))
        (= (long-float-mid-bits x) (long-float-mid-bits y))
        (= (long-float-high-bits x) (long-float-high-bits y))
        (= (long-float-exp-bits x) (long-float-exp-bits y))))


;;;; Conversion:

(macrolet ((frob (name translate inst to-sc to-type)
             `(define-vop (,name)
                (:args (x :scs (signed-reg) :target stack-temp
                          :load-if (not (sc-is x signed-stack))))
                (:temporary (:scs (single-stack) :from :argument) stack-temp)
                (:temporary (:scs (single-reg) :to :result :target y) temp)
                (:results (y :scs (,to-sc)))
                (:arg-types signed-num)
                (:result-types ,to-type)
                (:policy :fast-safe)
                (:note "inline float coercion")
                (:translate ,translate)
                (:vop-var vop)
                (:save-p :compute-only)
                (:generator 5
                  (let ((stack-tn
                         (sc-case x
                           (signed-reg
                            (inst st x
                                  (current-nfp-tn vop)
                                  (tn-byte-offset stack-temp))
                            stack-temp)
                           (signed-stack
                            x))))
                    (inst ldf temp
                          (current-nfp-tn vop)
                          (tn-byte-offset stack-tn))
                    (note-this-location vop :internal-error)
                    (inst ,inst y temp))))))
  (frob %single-float/signed %single-float fitos single-reg single-float)
  (frob %double-float/signed %double-float fitod double-reg double-float)
  #+long-float
  (frob %long-float/signed %long-float fitoq long-reg long-float))

(macrolet ((frob (name translate inst from-sc from-type to-sc to-type)
             `(define-vop (,name)
                (:args (x :scs (,from-sc)))
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
                  (inst ,inst y x)))))
  (frob %single-float/double-float %single-float fdtos
    double-reg double-float single-reg single-float)
  #+long-float
  (frob %single-float/long-float %single-float fqtos
    long-reg long-float single-reg single-float)
  (frob %double-float/single-float %double-float fstod
    single-reg single-float double-reg double-float)
  #+long-float
  (frob %double-float/long-float %double-float fqtod
    long-reg long-float double-reg double-float)
  #+long-float
  (frob %long-float/single-float %long-float fstoq
    single-reg single-float long-reg long-float)
  #+long-float
  (frob %long-float/double-float %long-float fdtoq
    double-reg double-float long-reg long-float))

(macrolet ((frob (trans from-sc from-type inst)
             `(define-vop (,(symbolicate trans "/" from-type))
                (:args (x :scs (,from-sc) :target temp))
                (:temporary (:from (:argument 0) :sc single-reg) temp)
                (:temporary (:scs (signed-stack)) stack-temp)
                (:results (y :scs (signed-reg)
                             :load-if (not (sc-is y signed-stack))))
                (:arg-types ,from-type)
                (:result-types signed-num)
                (:translate ,trans)
                (:policy :fast-safe)
                (:note "inline float truncate")
                (:vop-var vop)
                (:save-p :compute-only)
                (:generator 5
                  (note-this-location vop :internal-error)
                  (inst ,inst temp x)
                  (sc-case y
                    (signed-stack
                     (inst stf temp (current-nfp-tn vop)
                           (tn-byte-offset y)))
                    (signed-reg
                     (inst stf temp (current-nfp-tn vop)
                           (tn-byte-offset stack-temp))
                     (inst ld y (current-nfp-tn vop)
                           (tn-byte-offset stack-temp))))))))
  (frob %unary-truncate/single-float single-reg single-float fstoi)
  (frob %unary-truncate/double-float double-reg double-float fdtoi)
  #+long-float
  (frob %unary-truncate/long-float long-reg long-float fqtoi)
  ;; KLUDGE -- these two forms were protected by #-sun4.
  ;; (frob %unary-round single-reg single-float fstoir)
  ;; (frob %unary-round double-reg double-float fdtoir)
)

(deftransform %unary-round ((x) (float) (signed-byte 32))
  '(let* ((trunc (truly-the (signed-byte 32) (%unary-truncate x)))
          (extra (- x trunc))
          (absx (abs extra))
          (one-half (float 1/2 x)))
     (if (if (oddp trunc)
             (>= absx one-half)
             (> absx one-half))
         (truly-the (signed-byte 32) (%unary-truncate (+ x extra)))
         trunc)))

(define-vop (make-single-float)
  (:args (bits :scs (signed-reg) :target res
               :load-if (not (sc-is bits signed-stack))))
  (:results (res :scs (single-reg)
                 :load-if (not (sc-is res single-stack))))
  (:temporary (:scs (signed-reg) :from (:argument 0) :to (:result 0)) temp)
  (:temporary (:scs (signed-stack)) stack-temp)
  (:arg-types signed-num)
  (:result-types single-float)
  (:translate make-single-float)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 4
    (sc-case bits
      (signed-reg
       (sc-case res
         (single-reg
          (inst st bits (current-nfp-tn vop)
                (tn-byte-offset stack-temp))
          (inst ldf res (current-nfp-tn vop)
                (tn-byte-offset stack-temp)))
         (single-stack
          (inst st bits (current-nfp-tn vop)
                (tn-byte-offset res)))))
      (signed-stack
       (sc-case res
         (single-reg
          (inst ldf res (current-nfp-tn vop)
                (tn-byte-offset bits)))
         (single-stack
          (unless (location= bits res)
            (inst ld temp (current-nfp-tn vop)
                  (tn-byte-offset bits))
            (inst st temp (current-nfp-tn vop)
                  (tn-byte-offset res)))))))))

(define-vop (make-double-float)
  (:args (hi-bits :scs (signed-reg))
         (lo-bits :scs (unsigned-reg)))
  (:results (res :scs (double-reg)
                 :load-if (not (sc-is res double-stack))))
  (:temporary (:scs (double-stack)) temp)
  (:arg-types signed-num unsigned-num)
  (:result-types double-float)
  (:translate make-double-float)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (let ((stack-tn (sc-case res
                      (double-stack res)
                      (double-reg temp))))
      (inst st hi-bits (current-nfp-tn vop)
            (tn-byte-offset stack-tn))
      (inst st lo-bits (current-nfp-tn vop)
            (* (1+ (tn-offset stack-tn)) n-word-bytes)))
    (when (sc-is res double-reg)
      (inst lddf res (current-nfp-tn vop)
            (tn-byte-offset temp)))))

#+long-float
(define-vop (make-long-float)
    (:args (hi-bits :scs (signed-reg))
           (lo1-bits :scs (unsigned-reg))
           (lo2-bits :scs (unsigned-reg))
           (lo3-bits :scs (unsigned-reg)))
  (:results (res :scs (long-reg)
                 :load-if (not (sc-is res long-stack))))
  (:temporary (:scs (long-stack)) temp)
  (:arg-types signed-num unsigned-num unsigned-num unsigned-num)
  (:result-types long-float)
  (:translate make-long-float)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (let ((stack-tn (sc-case res
                      (long-stack res)
                      (long-reg temp))))
      (inst st hi-bits (current-nfp-tn vop)
            (tn-byte-offset stack-tn))
      (inst st lo1-bits (current-nfp-tn vop)
            (* (1+ (tn-offset stack-tn)) n-word-bytes))
      (inst st lo2-bits (current-nfp-tn vop)
            (* (+ 2 (tn-offset stack-tn)) n-word-bytes))
      (inst st lo3-bits (current-nfp-tn vop)
            (* (+ 3 (tn-offset stack-tn)) n-word-bytes)))
    (when (sc-is res long-reg)
      (load-long-reg res (current-nfp-tn vop)
                     (tn-byte-offset temp)))))

(define-vop (single-float-bits)
  (:args (float :scs (single-reg descriptor-reg)
                :load-if (not (sc-is float single-stack))))
  (:results (bits :scs (signed-reg)
                  :load-if (or (sc-is float descriptor-reg single-stack)
                               (not (sc-is bits signed-stack)))))
  (:temporary (:scs (signed-stack)) stack-temp)
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
          (inst stf float (current-nfp-tn vop)
                (tn-byte-offset stack-temp))
          (inst ld bits (current-nfp-tn vop)
                (tn-byte-offset stack-temp)))
         (single-stack
          (inst ld bits (current-nfp-tn vop)
                (tn-byte-offset float)))
         (descriptor-reg
          (loadw bits float single-float-value-slot
                 other-pointer-lowtag))))
      (signed-stack
       (sc-case float
         (single-reg
          (inst stf float (current-nfp-tn vop)
                (tn-byte-offset bits))))))))

(define-vop (double-float-high-bits)
  (:args (float :scs (double-reg descriptor-reg)
                :load-if (not (sc-is float double-stack))))
  (:results (hi-bits :scs (signed-reg)))
  (:temporary (:scs (double-stack)) stack-temp)
  (:arg-types double-float)
  (:result-types signed-num)
  (:translate double-float-high-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case float
      (double-reg
       (inst stdf float (current-nfp-tn vop)
             (tn-byte-offset stack-temp))
       (inst ld hi-bits (current-nfp-tn vop)
             (tn-byte-offset stack-temp)))
      (double-stack
       (inst ld hi-bits (current-nfp-tn vop)
             (tn-byte-offset float)))
      (descriptor-reg
       (loadw hi-bits float double-float-value-slot
              other-pointer-lowtag)))))

(define-vop (double-float-low-bits)
  (:args (float :scs (double-reg descriptor-reg)
                :load-if (not (sc-is float double-stack))))
  (:results (lo-bits :scs (unsigned-reg)))
  (:temporary (:scs (double-stack)) stack-temp)
  (:arg-types double-float)
  (:result-types unsigned-num)
  (:translate double-float-low-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case float
      (double-reg
       (inst stdf float (current-nfp-tn vop)
             (tn-byte-offset stack-temp))
       (inst ld lo-bits (current-nfp-tn vop)
             (* (1+ (tn-offset stack-temp)) n-word-bytes)))
      (double-stack
       (inst ld lo-bits (current-nfp-tn vop)
             (* (1+ (tn-offset float)) n-word-bytes)))
      (descriptor-reg
       (loadw lo-bits float (1+ double-float-value-slot)
              other-pointer-lowtag)))))

#+long-float
(define-vop (long-float-exp-bits)
  (:args (float :scs (long-reg descriptor-reg)
                :load-if (not (sc-is float long-stack))))
  (:results (exp-bits :scs (signed-reg)))
  (:temporary (:scs (double-stack)) stack-temp)
  (:arg-types long-float)
  (:result-types signed-num)
  (:translate long-float-exp-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case float
      (long-reg
       (let ((float (make-random-tn :kind :normal
                                    :sc (sc-or-lose 'double-reg)
                                    :offset (tn-offset float))))
         (inst stdf float (current-nfp-tn vop)
               (tn-byte-offset stack-temp)))
       (inst ld exp-bits (current-nfp-tn vop)
             (tn-byte-offset stack-temp)))
      (long-stack
       (inst ld exp-bits (current-nfp-tn vop)
             (tn-byte-offset float)))
      (descriptor-reg
       (loadw exp-bits float long-float-value-slot
              other-pointer-lowtag)))))

#+long-float
(define-vop (long-float-high-bits)
  (:args (float :scs (long-reg descriptor-reg)
                :load-if (not (sc-is float long-stack))))
  (:results (high-bits :scs (unsigned-reg)))
  (:temporary (:scs (double-stack)) stack-temp)
  (:arg-types long-float)
  (:result-types unsigned-num)
  (:translate long-float-high-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case float
      (long-reg
       (let ((float (make-random-tn :kind :normal
                                    :sc (sc-or-lose 'double-reg)
                                    :offset (tn-offset float))))
         (inst stdf float (current-nfp-tn vop)
               (tn-byte-offset stack-temp)))
       (inst ld high-bits (current-nfp-tn vop)
             (* (1+ (tn-offset stack-temp)) n-word-bytes)))
      (long-stack
       (inst ld high-bits (current-nfp-tn vop)
             (* (1+ (tn-offset float)) n-word-bytes)))
      (descriptor-reg
       (loadw high-bits float (1+ long-float-value-slot)
              other-pointer-lowtag)))))

#+long-float
(define-vop (long-float-mid-bits)
  (:args (float :scs (long-reg descriptor-reg)
                :load-if (not (sc-is float long-stack))))
  (:results (mid-bits :scs (unsigned-reg)))
  (:temporary (:scs (double-stack)) stack-temp)
  (:arg-types long-float)
  (:result-types unsigned-num)
  (:translate long-float-mid-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case float
      (long-reg
       (let ((float (make-random-tn :kind :normal
                                    :sc (sc-or-lose 'double-reg)
                                    :offset (+ 2 (tn-offset float)))))
         (inst stdf float (current-nfp-tn vop)
               (tn-byte-offset stack-temp)))
       (inst ld mid-bits (current-nfp-tn vop)
             (tn-byte-offset stack-temp)))
      (long-stack
       (inst ld mid-bits (current-nfp-tn vop)
             (* (+ 2 (tn-offset float)) n-word-bytes)))
      (descriptor-reg
       (loadw mid-bits float (+ 2 long-float-value-slot)
              other-pointer-lowtag)))))

#+long-float
(define-vop (long-float-low-bits)
  (:args (float :scs (long-reg descriptor-reg)
                :load-if (not (sc-is float long-stack))))
  (:results (lo-bits :scs (unsigned-reg)))
  (:temporary (:scs (double-stack)) stack-temp)
  (:arg-types long-float)
  (:result-types unsigned-num)
  (:translate long-float-low-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case float
      (long-reg
       (let ((float (make-random-tn :kind :normal
                                    :sc (sc-or-lose 'double-reg)
                                    :offset (+ 2 (tn-offset float)))))
         (inst stdf float (current-nfp-tn vop)
               (tn-byte-offset stack-temp)))
       (inst ld lo-bits (current-nfp-tn vop)
             (* (1+ (tn-offset stack-temp)) n-word-bytes)))
      (long-stack
       (inst ld lo-bits (current-nfp-tn vop)
             (* (+ 3 (tn-offset float)) n-word-bytes)))
      (descriptor-reg
       (loadw lo-bits float (+ 3 long-float-value-slot)
              other-pointer-lowtag)))))


;;;; Float mode hackery:

(sb-xc:deftype float-modes () '(unsigned-byte 32))
(defknown floating-point-modes () float-modes (flushable))
(defknown ((setf floating-point-modes)) (float-modes)
  float-modes)

(define-vop (floating-point-modes)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate floating-point-modes)
  (:policy :fast-safe)
  (:vop-var vop)
  (:temporary (:sc unsigned-stack) temp)
  (:generator 3
    (let ((nfp (current-nfp-tn vop)))
      (inst stfsr nfp (* n-word-bytes (tn-offset temp)))
      (loadw res nfp (tn-offset temp))
      (inst nop))))

#+nil
(define-vop (floating-point-modes)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate floating-point-modes)
  (:policy :fast-safe)
  (:vop-var vop)
  (:temporary (:sc double-stack) temp)
  (:generator 3
    (let* ((nfp (current-nfp-tn vop))
           (offset (* 4 (tn-offset temp))))
      (inst stxfsr nfp offset)
      ;; The desired FP mode data is in the least significant 32
      ;; bits, which is stored at the next higher word in memory.
      (loadw res nfp (+ offset 4))
      ;; Is this nop needed? -- rtoy
      (inst nop))))

(define-vop (set-floating-point-modes)
  (:args (new :scs (unsigned-reg) :target res))
  (:results (res :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:result-types unsigned-num)
  (:translate (setf floating-point-modes))
  (:policy :fast-safe)
  (:temporary (:sc unsigned-stack) temp)
  (:vop-var vop)
  (:generator 3
    (let ((nfp (current-nfp-tn vop)))
      (storew new nfp (tn-offset temp))
      (inst ldfsr nfp (* n-word-bytes (tn-offset temp)))
      (move res new))))

#+nil
(define-vop (set-floating-point-modes)
  (:args (new :scs (unsigned-reg) :target res))
  (:results (res :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:result-types unsigned-num)
  (:translate (setf floating-point-modes))
  (:policy :fast-safe)
  (:temporary (:sc double-stack) temp)
  (:temporary (:sc unsigned-reg) my-fsr)
  (:vop-var vop)
  (:generator 3
    (let ((nfp (current-nfp-tn vop))
          (offset (* n-word-bytes (tn-offset temp))))
      (pseudo-atomic ()
        ;; Get the current FSR, so we can get the new %fcc's
        (inst stxfsr nfp offset)
        (inst ldx my-fsr nfp offset)
        ;; Carefully merge in the new mode bits with the rest of the
        ;; FSR.  This is only needed if we care about preserving the
        ;; high 32 bits of the FSR, which contain the additional
        ;; %fcc's on the sparc V9.  If not, we don't need this, but we
        ;; do need to make sure that the unused bits are written as
        ;; zeroes, according the V9 architecture manual.
        (inst sra new 0)
        (inst srlx my-fsr 32)
        (inst sllx my-fsr 32)
        (inst or my-fsr new)
        ;; Save it back and load it into the fsr register
        (inst stx my-fsr nfp offset)
        (inst ldxfsr nfp offset)
        (move res new)))))

#+nil
(define-vop (set-floating-point-modes)
  (:args (new :scs (unsigned-reg) :target res))
  (:results (res :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:result-types unsigned-num)
  (:translate (setf floating-point-modes))
  (:policy :fast-safe)
  (:temporary (:sc double-stack) temp)
  (:temporary (:sc unsigned-reg) my-fsr)
  (:vop-var vop)
  (:generator 3
    (let ((nfp (current-nfp-tn vop))
          (offset (* n-word-bytes (tn-offset temp))))
      (inst stx new nfp offset)
      (inst ldxfsr nfp offset)
      (move res new))))


;;;; Special functions.

#-long-float
(define-vop (fsqrt)
  (:args (x :scs (double-reg)))
  (:results (y :scs (double-reg)))
  (:translate %sqrt)
  (:policy :fast-safe)
  (:guard (or (member :sparc-v7 *backend-subfeatures*)
              (member :sparc-v8 *backend-subfeatures*)
              (member :sparc-v9 *backend-subfeatures*)))
  (:arg-types double-float)
  (:result-types double-float)
  (:note "inline float arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 1
    (note-this-location vop :internal-error)
    (inst fsqrtd y x)))

#+long-float
(define-vop (fsqrt-long)
  (:args (x :scs (long-reg)))
  (:results (y :scs (long-reg)))
  (:translate %sqrt)
  (:policy :fast-safe)
  (:arg-types long-float)
  (:result-types long-float)
  (:note "inline float arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 1
    (note-this-location vop :internal-error)
    (inst fsqrtq y x)))


;;;; Complex float VOPs

(define-vop (make-complex-single-float)
  (:translate complex)
  (:args (real :scs (single-reg) :target r
               :load-if (not (location= real r)))
         (imag :scs (single-reg) :to :save))
  (:arg-types single-float single-float)
  (:results (r :scs (complex-single-reg) :from (:argument 0)
               :load-if (not (sc-is r complex-single-stack))))
  (:result-types complex-single-float)
  (:note "inline complex single-float creation")
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case r
      (complex-single-reg
       (let ((r-real (complex-single-reg-real-tn r)))
         (unless (location= real r-real)
           (inst fmovs r-real real)))
       (let ((r-imag (complex-single-reg-imag-tn r)))
         (unless (location= imag r-imag)
           (inst fmovs r-imag imag))))
      (complex-single-stack
       (let ((nfp (current-nfp-tn vop))
             (offset (tn-byte-offset r)))
         (unless (location= real r)
           (inst stf real nfp offset))
         (inst stf imag nfp (+ offset n-word-bytes)))))))

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
  (:vop-var vop)
  (:generator 5
    (sc-case r
      (complex-double-reg
       (let ((r-real (complex-double-reg-real-tn r)))
         (unless (location= real r-real)
           (move-double-reg r-real real)))
       (let ((r-imag (complex-double-reg-imag-tn r)))
         (unless (location= imag r-imag)
           (move-double-reg r-imag imag))))
      (complex-double-stack
       (let ((nfp (current-nfp-tn vop))
             (offset (tn-byte-offset r)))
         (unless (location= real r)
           (inst stdf real nfp offset))
         (inst stdf imag nfp (+ offset (* 2 n-word-bytes))))))))

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
  (:vop-var vop)
  (:generator 5
    (sc-case r
      (complex-long-reg
       (let ((r-real (complex-long-reg-real-tn r)))
         (unless (location= real r-real)
           (move-long-reg r-real real)))
       (let ((r-imag (complex-long-reg-imag-tn r)))
         (unless (location= imag r-imag)
           (move-long-reg r-imag imag))))
      (complex-long-stack
       (let ((nfp (current-nfp-tn vop))
             (offset (tn-byte-offset r)))
         (unless (location= real r)
           (store-long-reg real nfp offset))
         (store-long-reg imag nfp (+ offset (* 4 n-word-bytes))))))))

(define-vop (complex-single-float-value)
  (:args (x :scs (complex-single-reg) :target r
            :load-if (not (sc-is x complex-single-stack))))
  (:arg-types complex-single-float)
  (:results (r :scs (single-reg)))
  (:result-types single-float)
  (:variant-vars slot)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 3
    (sc-case x
      (complex-single-reg
       (let ((value-tn (ecase slot
                         (:real (complex-single-reg-real-tn x))
                         (:imag (complex-single-reg-imag-tn x)))))
         (unless (location= value-tn r)
           (inst fmovs r value-tn))))
      (complex-single-stack
       (inst ldf r (current-nfp-tn vop) (* (+ (ecase slot (:real 0) (:imag 1))
                                              (tn-offset x))
                                           n-word-bytes))))))

(define-vop (realpart/complex-single-float complex-single-float-value)
  (:translate realpart)
  (:note "complex single float realpart")
  (:variant :real))

(define-vop (imagpart/complex-single-float complex-single-float-value)
  (:translate imagpart)
  (:note "complex single float imagpart")
  (:variant :imag))

(define-vop (complex-double-float-value)
  (:args (x :scs (complex-double-reg) :target r
            :load-if (not (sc-is x complex-double-stack))))
  (:arg-types complex-double-float)
  (:results (r :scs (double-reg)))
  (:result-types double-float)
  (:variant-vars slot)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 3
    (sc-case x
      (complex-double-reg
       (let ((value-tn (ecase slot
                         (:real (complex-double-reg-real-tn x))
                         (:imag (complex-double-reg-imag-tn x)))))
         (unless (location= value-tn r)
           (move-double-reg r value-tn))))
      (complex-double-stack
       (inst lddf r (current-nfp-tn vop) (* (+ (ecase slot (:real 0) (:imag 2))
                                               (tn-offset x))
                                            n-word-bytes))))))

(define-vop (realpart/complex-double-float complex-double-float-value)
  (:translate realpart)
  (:note "complex double float realpart")
  (:variant :real))

(define-vop (imagpart/complex-double-float complex-double-float-value)
  (:translate imagpart)
  (:note "complex double float imagpart")
  (:variant :imag))

#+long-float
(define-vop (complex-long-float-value)
  (:args (x :scs (complex-long-reg) :target r
            :load-if (not (sc-is x complex-long-stack))))
  (:arg-types complex-long-float)
  (:results (r :scs (long-reg)))
  (:result-types long-float)
  (:variant-vars slot)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 4
    (sc-case x
      (complex-long-reg
       (let ((value-tn (ecase slot
                         (:real (complex-long-reg-real-tn x))
                         (:imag (complex-long-reg-imag-tn x)))))
         (unless (location= value-tn r)
           (move-long-reg r value-tn))))
      (complex-long-stack
       (load-long-reg r (current-nfp-tn vop)
                      (* (+ (ecase slot (:real 0) (:imag 4)) (tn-offset x))
                         n-word-bytes))))))

#+long-float
(define-vop (realpart/complex-long-float complex-long-float-value)
  (:translate realpart)
  (:note "complex long float realpart")
  (:variant :real))

#+long-float
(define-vop (imagpart/complex-long-float complex-long-float-value)
  (:translate imagpart)
  (:note "complex long float imagpart")
  (:variant :imag))



;;;; Complex float arithmetic

#+complex-fp-vops
(progn

;; Negate a complex
(macrolet
    ((frob (float-type fneg cost)
       (let* ((vop-name (symbolicate "%NEGATE/COMPLEX-" float-type))
              (c-type (symbolicate "COMPLEX-" float-type "-FLOAT"))
              (complex-reg (symbolicate "COMPLEX-" float-type "-REG"))
              (real-tn (symbolicate "COMPLEX-" float-type "-REG-REAL-TN"))
              (imag-tn (symbolicate "COMPLEX-" float-type "-REG-IMAG-TN")))
         `(define-vop (,vop-name)
            (:args (x :scs (,complex-reg)))
            (:arg-types ,c-type)
            (:results (r :scs (,complex-reg)))
            (:result-types ,c-type)
            (:policy :fast-safe)
            (:note "inline complex float arithmetic")
            (:translate %negate)
            (:generator ,cost
              (let ((xr (,real-tn x))
                    (xi (,imag-tn x))
                    (rr (,real-tn r))
                    (ri (,imag-tn r)))
                (,@fneg rr xr)
                (,@fneg ri xi)))))))
  (frob single (inst fnegs) 4)
  (frob double (negate-double-reg) 4))

;; Add and subtract for two complex arguments
(macrolet
    ((frob (op inst float-type cost)
       (let* ((vop-name (symbolicate (symbol-name op) "/COMPLEX-" float-type "-FLOAT"))
              (c-type (symbolicate "COMPLEX-" float-type "-FLOAT"))
              (complex-reg (symbolicate "COMPLEX-" float-type "-REG"))
              (real-part (symbolicate "COMPLEX-" float-type "-REG-REAL-TN"))
              (imag-part (symbolicate "COMPLEX-" float-type "-REG-IMAG-TN")))
         `(define-vop (,vop-name)
           (:args (x :scs (,complex-reg)) (y :scs (,complex-reg)))
           (:results (r :scs (,complex-reg)))
           (:arg-types ,c-type ,c-type)
           (:result-types ,c-type)
           (:policy :fast-safe)
           (:note "inline complex float arithmetic")
           (:translate ,op)
           (:generator ,cost
            (let ((xr (,real-part x))
                  (xi (,imag-part x))
                  (yr (,real-part y))
                  (yi (,imag-part y))
                  (rr (,real-part r))
                  (ri (,imag-part r)))
              (inst ,inst rr xr yr)
              (inst ,inst ri xi yi)))))))
  (frob + fadds single 4)
  (frob + faddd double 4)
  (frob - fsubs single 4)
  (frob - fsubd double 4))

;; Add and subtract a complex and a float

(macrolet
    ((frob (size op fop fmov cost)
       (let ((vop-name (symbolicate "COMPLEX-" size "-FLOAT-"
                                    op
                                    "-" size "-FLOAT"))
             (complex-reg (symbolicate "COMPLEX-" size "-REG"))
             (real-reg (symbolicate size "-REG"))
             (c-type (symbolicate "COMPLEX-" size "-FLOAT"))
             (r-type (symbolicate size "-FLOAT"))
             (real-part (symbolicate "COMPLEX-" size "-REG-REAL-TN"))
             (imag-part (symbolicate "COMPLEX-" size "-REG-IMAG-TN")))
         `(define-vop (,vop-name)
              (:args (x :scs (,complex-reg))
                     (y :scs (,real-reg)))
            (:results (r :scs (,complex-reg)))
            (:arg-types ,c-type ,r-type)
            (:result-types ,c-type)
            (:policy :fast-safe)
            (:note "inline complex float/float arithmetic")
            (:translate ,op)
            (:generator ,cost
              (let ((xr (,real-part x))
                    (xi (,imag-part x))
                    (rr (,real-part r))
                    (ri (,imag-part r)))
                (inst ,fop rr xr y)
                (unless (location= ri xi)
                  (,@fmov ri xi))))))))

  (frob single + fadds (inst fmovs) 2)
  (frob single - fsubs (inst fmovs) 2)
  (frob double + faddd (move-double-reg) 4)
  (frob double - fsubd (move-double-reg) 4))

;; Add a float and a complex
(macrolet
    ((frob (size fop fmov cost)
       (let ((vop-name
              (symbolicate size "-FLOAT-+-COMPLEX-" size "-FLOAT"))
             (complex-reg (symbolicate "COMPLEX-" size "-REG"))
             (real-reg (symbolicate size "-REG"))
             (c-type (symbolicate "COMPLEX-" size "-FLOAT"))
             (r-type (symbolicate size "-FLOAT"))
             (real-part (symbolicate "COMPLEX-" size "-REG-REAL-TN"))
             (imag-part (symbolicate "COMPLEX-" size "-REG-IMAG-TN")))
         `(define-vop (,vop-name)
              (:args (y :scs (,real-reg))
                     (x :scs (,complex-reg)))
            (:results (r :scs (,complex-reg)))
            (:arg-types ,r-type ,c-type)
            (:result-types ,c-type)
            (:policy :fast-safe)
            (:note "inline complex float/float arithmetic")
            (:translate +)
            (:generator ,cost
              (let ((xr (,real-part x))
                    (xi (,imag-part x))
                    (rr (,real-part r))
                    (ri (,imag-part r)))
                (inst ,fop rr xr y)
                (unless (location= ri xi)
                  (,@fmov ri xi))))))))
  (frob single fadds (inst fmovs) 1)
  (frob double faddd (move-double-reg) 2))

;; Subtract a complex from a float

(macrolet
    ((frob (size fop fneg cost)
       (let ((vop-name (symbolicate size "-FLOAT---COMPLEX-" size "-FLOAT"))
             (complex-reg (symbolicate "COMPLEX-" size "-REG"))
             (real-reg (symbolicate size "-REG"))
             (c-type (symbolicate "COMPLEX-" size "-FLOAT"))
             (r-type (symbolicate size "-FLOAT"))
             (real-part (symbolicate "COMPLEX-" size "-REG-REAL-TN"))
             (imag-part (symbolicate "COMPLEX-" size "-REG-IMAG-TN")))
         `(define-vop (single-float---complex-single-float)
              (:args (x :scs (,real-reg)) (y :scs (,complex-reg)))
            (:results (r :scs (,complex-reg)))
            (:arg-types ,r-type ,c-type)
            (:result-types ,c-type)
            (:policy :fast-safe)
            (:note "inline complex float/float arithmetic")
            (:translate -)
            (:generator ,cost
               (let ((yr (,real-part y))
                     (yi (,imag-part y))
                     (rr (,real-part r))
                     (ri (,imag-part r)))
                 (inst ,fop rr x yr)
                 (,@fneg ri yi))))
       ))

  (frob single fsubs (inst fnegs) 2)
  (frob double fsubd (negate-double-reg) 2)))

;; Multiply two complex numbers

#+nil
(macrolet
    ((frob (size fmul fadd fsub cost)
       (let ((vop-name (symbolicate "*/COMPLEX-" size "-FLOAT"))
             (complex-reg (symbolicate "COMPLEX-" size "-REG"))
             (real-reg (symbolicate size "-REG"))
             (c-type (symbolicate "COMPLEX-" size "-FLOAT"))
             (real-part (symbolicate "COMPLEX-" size "-REG-REAL-TN"))
             (imag-part (symbolicate "COMPLEX-" size "-REG-IMAG-TN")))
         `(define-vop (,vop-name)
            (:args (x :scs (,complex-reg))
                   (y :scs (,complex-reg)))
            (:results (r :scs (,complex-reg)))
            (:arg-types ,c-type ,c-type)
            (:result-types ,c-type)
            (:policy :fast-safe)
            (:note "inline complex float multiplication")
            (:translate *)
            (:temporary (:scs (,real-reg)) prod-1 prod-2 prod-3 prod-4)
            (:generator ,cost
              (let ((xr (,real-part x))
                    (xi (,imag-part x))
                    (yr (,real-part y))
                    (yi (,imag-part y))
                    (rr (,real-part r))
                    (ri (,imag-part r)))
                ;; All of the temps are needed in case the result TN happens to
                ;; be the same as one of the arg TN's
                (inst ,fmul prod-1 xr yr)
                (inst ,fmul prod-2 xi yi)
                (inst ,fmul prod-3 xr yi)
                (inst ,fmul prod-4 xi yr)
                (inst ,fsub rr prod-1 prod-2)
                (inst ,fadd ri prod-3 prod-4)))))))

  (frob single fmuls fadds fsubs 6)
  (frob double fmuld faddd fsubd 6))

(macrolet
    ((frob (size fmul fadd fsub cost)
       (let ((vop-name (symbolicate "*/COMPLEX-" size "-FLOAT"))
             (complex-reg (symbolicate "COMPLEX-" size "-REG"))
             (real-reg (symbolicate size "-REG"))
             (c-type (symbolicate "COMPLEX-" size "-FLOAT"))
             (real-part (symbolicate "COMPLEX-" size "-REG-REAL-TN"))
             (imag-part (symbolicate "COMPLEX-" size "-REG-IMAG-TN")))
         `(define-vop (,vop-name)
            (:args (x :scs (,complex-reg))
                   (y :scs (,complex-reg)))
            (:results (r :scs (,complex-reg)))
            (:arg-types ,c-type ,c-type)
            (:result-types ,c-type)
            (:policy :fast-safe)
            (:note "inline complex float multiplication")
            (:translate *)
            (:temporary (:scs (,real-reg)) p1 p2)
            (:generator ,cost
              (let ((xr (,real-part x))
                    (xi (,imag-part x))
                    (yr (,real-part y))
                    (yi (,imag-part y))
                    (rr (,real-part r))
                    (ri (,imag-part r)))
                (cond ((location= r x)
                       (inst ,fmul p1 xr yr)
                       (inst ,fmul p2 xr yi)
                       (inst ,fmul rr xi yi)
                       (inst ,fsub rr p1 xr)
                       (inst ,fmul p1 xi yr)
                       (inst ,fadd ri p2 p1))
                      ((location= r y)
                       (inst ,fmul p1 yr xr)
                       (inst ,fmul p2 yr xi)
                       (inst ,fmul rr yi xi)
                       (inst ,fsub rr p1 rr)
                       (inst ,fmul p1 yi xr)
                       (inst ,fadd ri p2 p1))
                      (t
                       (inst ,fmul rr yr xr)
                       (inst ,fmul ri xi yi)
                       (inst ,fsub rr rr ri)
                       (inst ,fmul p1 xr yi)
                       (inst ,fmul ri xi yr)
                       (inst ,fadd ri ri p1)))))))))

  (frob single fmuls fadds fsubs 6)
  (frob double fmuld faddd fsubd 6))

;; Multiply a complex by a float.  The case of float * complex is
;; handled by a deftransform to convert it to the complex*float case.
(macrolet
    ((frob (float-type fmul mov cost)
       (let* ((vop-name (symbolicate "COMPLEX-"
                                     float-type
                                     "-FLOAT-*-"
                                     float-type
                                     "-FLOAT"))
              (vop-name-r (symbolicate float-type
                                       "-FLOAT-*-COMPLEX-"
                                       float-type
                                       "-FLOAT"))
              (complex-sc-type (symbolicate "COMPLEX-" float-type "-REG"))
              (real-sc-type (symbolicate float-type "-REG"))
              (c-type (symbolicate "COMPLEX-" float-type "-FLOAT"))
              (r-type (symbolicate float-type "-FLOAT"))
              (real-part (symbolicate "COMPLEX-" float-type "-REG-REAL-TN"))
              (imag-part (symbolicate "COMPLEX-" float-type "-REG-IMAG-TN")))
         `(progn
           ;; Complex * float
           (define-vop (,vop-name)
             (:args (x :scs (,complex-sc-type))
                    (y :scs (,real-sc-type)))
             (:results (r :scs (,complex-sc-type)))
             (:arg-types ,c-type ,r-type)
             (:result-types ,c-type)
             (:policy :fast-safe)
             (:note "inline complex float arithmetic")
             (:translate *)
             (:temporary (:scs (,real-sc-type)) temp)
             (:generator ,cost
              (let ((xr (,real-part x))
                    (xi (,imag-part x))
                    (rr (,real-part r))
                    (ri (,imag-part r)))
                (cond ((location= y rr)
                       (inst ,fmul temp xr y) ; xr * y
                       (inst ,fmul ri xi y) ; xi * yi
                       (,@mov rr temp))
                      (t
                       (inst ,fmul rr xr y)
                       (inst ,fmul ri xi y))))))
           ;; Float * complex
           (define-vop (,vop-name-r)
             (:args (y :scs (,real-sc-type))
                    (x :scs (,complex-sc-type)))
             (:results (r :scs (,complex-sc-type)))
             (:arg-types ,r-type ,c-type)
             (:result-types ,c-type)
             (:policy :fast-safe)
             (:note "inline complex float arithmetic")
             (:translate *)
             (:temporary (:scs (,real-sc-type)) temp)
             (:generator ,cost
              (let ((xr (,real-part x))
                    (xi (,imag-part x))
                    (rr (,real-part r))
                    (ri (,imag-part r)))
                (cond ((location= y rr)
                       (inst ,fmul temp xr y) ; xr * y
                       (inst ,fmul ri xi y) ; xi * yi
                       (,@mov rr temp))
                      (t
                       (inst ,fmul rr xr y)
                       (inst ,fmul ri xi y))))))))))
  (frob single fmuls (inst fmovs) 4)
  (frob double fmuld (move-double-reg) 4))


;; Divide a complex by a complex

;; Here's how we do a complex division
;;
;; Compute (xr + i*xi)/(yr + i*yi)
;;
;; Assume |yi| < |yr|.  Then
;;
;; (xr + i*xi)      (xr + i*xi)
;; ----------- = -----------------
;; (yr + i*yi)   yr*(1 + i*(yi/yr))
;;
;;               (xr + i*xi)*(1 - i*(yi/yr))
;;             = ---------------------------
;;                   yr*(1 + (yi/yr)^2)
;;
;;               (xr + (yi/yr)*xi) + i*(xi - (yi/yr)*xr)
;;             = --------------------------------------
;;                        yr + (yi/yr)*yi
;;
;;
;; We do the similar thing when |yi| > |yr|.  The result is
;;
;;
;; (xr + i*xi)      (xr + i*xi)
;; ----------- = -----------------
;; (yr + i*yi)   yi*((yr/yi) + i)
;;
;;               (xr + i*xi)*((yr/yi) - i)
;;             = -------------------------
;;                  yi*((yr/yi)^2 + 1)
;;
;;               (xr*(yr/yi) + xi) + i*(xi*(yr/yi) - xr)
;;             = ---------------------------------------
;;                       yi + (yr/yi)*yr
;;

#+nil
(macrolet
    ((frob (float-type fcmp fadd fsub fmul fdiv fabs fmov cost)
       (let ((vop-name (symbolicate "//COMPLEX-" float-type "-FLOAT"))
             (complex-reg (symbolicate "COMPLEX-" float-type "-REG"))
             (real-reg (symbolicate float-type "-REG"))
             (c-type (symbolicate "COMPLEX-" float-type "-FLOAT"))
             (real-part (symbolicate "COMPLEX-" float-type "-REG-REAL-TN"))
             (imag-part (symbolicate "COMPLEX-" float-type "-REG-IMAG-TN")))
         `(define-vop (,vop-name)
            (:args (x :scs (,complex-reg))
                   (y :scs (,complex-reg)))
            (:results (r :scs (,complex-reg)))
            (:arg-types ,c-type ,c-type)
            (:result-types ,c-type)
            (:policy :fast-safe)
            (:note "inline complex float division")
            (:translate /)
            (:temporary (:sc ,real-reg) ratio)
            (:temporary (:sc ,real-reg) den)
            (:temporary (:sc ,real-reg) temp-r)
            (:temporary (:sc ,real-reg) temp-i)
            (:generator ,cost
              (let ((xr (,real-part x))
                    (xi (,imag-part x))
                    (yr (,real-part y))
                    (yi (,imag-part y))
                    (rr (,real-part r))
                    (ri (,imag-part r))
                    (bigger (gen-label))
                    (done (gen-label)))
                (,@fabs ratio yr)
                (,@fabs den yi)
                (inst ,fcmp ratio den)
                (unless (member :sparc-v9 *backend-subfeatures*)
                  (inst nop))
                (inst fb :ge bigger)
                (inst nop)
                ;; The case of |yi| <= |yr|
                (inst ,fdiv ratio yi yr) ; ratio = yi/yr
                (inst ,fmul den ratio yi)
                (inst ,fadd den den yr) ; den = yr + (yi/yr)*yi

                (inst ,fmul temp-r ratio xi)
                (inst ,fadd temp-r temp-r xr) ; temp-r = xr + (yi/yr)*xi
                (inst ,fdiv temp-r temp-r den)

                (inst ,fmul temp-i ratio xr)
                (inst ,fsub temp-i xi temp-i) ; temp-i = xi - (yi/yr)*xr
                (inst b done)
                (inst ,fdiv temp-i temp-i den)

                (emit-label bigger)
                ;; The case of |yi| > |yr|
                (inst ,fdiv ratio yr yi) ; ratio = yr/yi
                (inst ,fmul den ratio yr)
                (inst ,fadd den den yi) ; den = yi + (yr/yi)*yr

                (inst ,fmul temp-r ratio xr)
                (inst ,fadd temp-r temp-r xi) ; temp-r = xi + xr*(yr/yi)
                (inst ,fdiv temp-r temp-r den)

                (inst ,fmul temp-i ratio xi)
                (inst ,fsub temp-i temp-i xr) ; temp-i = xi*(yr/yi) - xr
                (inst ,fdiv temp-i temp-i den)

                (emit-label done)
                (unless (location= temp-r rr)
                  (,@fmov rr temp-r))
                (unless (location= temp-i ri)
                  (,@fmov ri temp-i))
                ))))))

  (frob single fcmps fadds fsubs fmuls fdivs (inst fabss) (inst fmovs) 15)
  (frob double fcmpd faddd fsubd fmuld fdivd (abs-double-reg) (move-double-reg) 15))

(macrolet
    ((frob (float-type fcmp fadd fsub fmul fdiv fabs cost)
       (let ((vop-name (symbolicate "//COMPLEX-" float-type "-FLOAT"))
             (complex-reg (symbolicate "COMPLEX-" float-type "-REG"))
             (real-reg (symbolicate float-type "-REG"))
             (c-type (symbolicate "COMPLEX-" float-type "-FLOAT"))
             (real-part (symbolicate "COMPLEX-" float-type "-REG-REAL-TN"))
             (imag-part (symbolicate "COMPLEX-" float-type "-REG-IMAG-TN")))
         `(define-vop (,vop-name)
            (:args (x :scs (,complex-reg))
                   (y :scs (,complex-reg)))
            (:results (r :scs (,complex-reg)))
            (:arg-types ,c-type ,c-type)
            (:result-types ,c-type)
            (:policy :fast-safe)
            (:note "inline complex float division")
            (:translate /)
            (:temporary (:sc ,real-reg) ratio)
            (:temporary (:sc ,real-reg) den)
            (:temporary (:sc ,real-reg) temp-r)
            (:temporary (:sc ,real-reg) temp-i)
            (:generator ,cost
              (let ((xr (,real-part x))
                    (xi (,imag-part x))
                    (yr (,real-part y))
                    (yi (,imag-part y))
                    (rr (,real-part r))
                    (ri (,imag-part r))
                    (bigger (gen-label))
                    (done (gen-label)))
                (,@fabs ratio yr)
                (,@fabs den yi)
                (inst ,fcmp ratio den)
                (unless (member :sparc-v9 *backend-subfeatures*)
                  (inst nop))
                (inst fb :ge bigger)
                (inst nop)
                ;; The case of |yi| <= |yr|
                (inst ,fdiv ratio yi yr) ; ratio = yi/yr
                (inst ,fmul den ratio yi)
                (inst ,fmul temp-r ratio xi)
                (inst ,fmul temp-i ratio xr)

                (inst ,fadd den den yr) ; den = yr + (yi/yr)*yi
                (inst ,fadd temp-r temp-r xr) ; temp-r = xr + (yi/yr)*xi
                (inst b done)
                (inst ,fsub temp-i xi temp-i) ; temp-i = xi - (yi/yr)*xr


                (emit-label bigger)
                ;; The case of |yi| > |yr|
                (inst ,fdiv ratio yr yi) ; ratio = yr/yi
                (inst ,fmul den ratio yr)
                (inst ,fmul temp-r ratio xr)
                (inst ,fmul temp-i ratio xi)

                (inst ,fadd den den yi) ; den = yi + (yr/yi)*yr
                (inst ,fadd temp-r temp-r xi) ; temp-r = xi + xr*(yr/yi)

                (inst ,fsub temp-i temp-i xr) ; temp-i = xi*(yr/yi) - xr

                (emit-label done)

                (inst ,fdiv rr temp-r den)
                (inst ,fdiv ri temp-i den)
                ))))))

  (frob single fcmps fadds fsubs fmuls fdivs (inst fabss) 15)
  (frob double fcmpd faddd fsubd fmuld fdivd (abs-double-reg) 15))


;; Divide a complex by a real
(macrolet
    ((frob (float-type fdiv cost)
       (let* ((vop-name (symbolicate "COMPLEX-" float-type "-FLOAT-/-" float-type "-FLOAT"))
              (complex-sc-type (symbolicate "COMPLEX-" float-type "-REG"))
              (real-sc-type (symbolicate float-type "-REG"))
              (c-type (symbolicate "COMPLEX-" float-type "-FLOAT"))
              (r-type (symbolicate float-type "-FLOAT"))
              (real-part (symbolicate "COMPLEX-" float-type "-REG-REAL-TN"))
              (imag-part (symbolicate "COMPLEX-" float-type "-REG-IMAG-TN")))
         `(define-vop (,vop-name)
           (:args (x :scs (,complex-sc-type)) (y :scs (,real-sc-type)))
           (:results (r :scs (,complex-sc-type)))
           (:arg-types ,c-type ,r-type)
           (:result-types ,c-type)
           (:policy :fast-safe)
           (:note "inline complex float arithmetic")
           (:translate /)
           (:generator ,cost
            (let ((xr (,real-part x))
                  (xi (,imag-part x))
                  (rr (,real-part r))
                  (ri (,imag-part r)))
              (inst ,fdiv rr xr y)      ; xr * y
              (inst ,fdiv ri xi y)      ; xi * yi
              ))))))
  (frob single fdivs 2)
  (frob double fdivd 2))

;; Divide a real by a complex

(macrolet
    ((frob (float-type fcmp fadd fmul fdiv fneg fabs cost)
       (let ((vop-name (symbolicate float-type "-FLOAT-/-COMPLEX-" float-type "-FLOAT"))
             (complex-reg (symbolicate "COMPLEX-" float-type "-REG"))
             (real-reg (symbolicate float-type "-REG"))
             (r-type (symbolicate float-type "-FLOAT"))
             (c-type (symbolicate "COMPLEX-" float-type "-FLOAT"))
             (real-tn (symbolicate "COMPLEX-" float-type "-REG-REAL-TN"))
             (imag-tn (symbolicate "COMPLEX-" float-type "-REG-IMAG-TN")))
         `(define-vop (,vop-name)
            (:args (x :scs (,real-reg))
                   (y :scs (,complex-reg)))
            (:results (r :scs (,complex-reg)))
            (:arg-types ,r-type ,c-type)
            (:result-types ,c-type)
            (:policy :fast-safe)
            (:note "inline complex float division")
            (:translate /)
            (:temporary (:sc ,real-reg) ratio)
            (:temporary (:sc ,real-reg) den)
            (:temporary (:sc ,real-reg) temp)
            (:generator ,cost
              (let ((yr (,real-tn y))
                    (yi (,imag-tn y))
                    (rr (,real-tn r))
                    (ri (,imag-tn r))
                    (bigger (gen-label))
                    (done (gen-label)))
                (,@fabs ratio yr)
                (,@fabs den yi)
                (inst ,fcmp ratio den)
                (unless (member :sparc-v9 *backend-subfeatures*)
                  (inst nop))
                (inst fb :ge bigger)
                (inst nop)
                ;; The case of |yi| <= |yr|
                (inst ,fdiv ratio yi yr) ; ratio = yi/yr
                (inst ,fmul den ratio yi)
                (inst ,fadd den den yr) ; den = yr + (yi/yr)*yi

                (inst ,fmul temp ratio x) ; temp = (yi/yr)*x
                (inst ,fdiv rr x den)   ; rr = x/den
                (inst b done)
                (inst ,fdiv temp temp den) ; temp = (yi/yr)*x/den

                (emit-label bigger)
                ;; The case of |yi| > |yr|
                (inst ,fdiv ratio yr yi) ; ratio = yr/yi
                (inst ,fmul den ratio yr)
                (inst ,fadd den den yi) ; den = yi + (yr/yi)*yr

                (inst ,fmul temp ratio x) ; temp = (yr/yi)*x
                (inst ,fdiv rr temp den) ; rr = (yr/yi)*x/den
                (inst ,fdiv temp x den) ; temp = x/den
                (emit-label done)

                (,@fneg ri temp)))))))

  (frob single fcmps fadds fmuls fdivs (inst fnegs) (inst fabss) 10)
  (frob double fcmpd faddd fmuld fdivd (negate-double-reg) (abs-double-reg) 10))

;; Conjugate of a complex number

(macrolet
    ((frob (float-type fneg fmov cost)
       (let ((vop-name (symbolicate "CONJUGATE/COMPLEX-" float-type "-FLOAT"))
             (complex-reg (symbolicate "COMPLEX-" float-type "-REG"))
             (c-type (symbolicate "COMPLEX-" float-type "-FLOAT"))
             (real-part (symbolicate "COMPLEX-" float-type "-REG-REAL-TN"))
             (imag-part (symbolicate "COMPLEX-" float-type "-REG-IMAG-TN")))
         `(define-vop (,vop-name)
            (:args (x :scs (,complex-reg)))
            (:results (r :scs (,complex-reg)))
            (:arg-types ,c-type)
            (:result-types ,c-type)
            (:policy :fast-safe)
            (:note "inline complex conjugate")
            (:translate conjugate)
            (:generator ,cost
              (let ((xr (,real-part x))
                    (xi (,imag-part x))
                    (rr (,real-part r))
                    (ri (,imag-part r)))
                (,@fneg ri xi)
                (unless (location= rr xr)
                  (,@fmov rr xr))))))))

  (frob single (inst fnegs) (inst fmovs) 4)
  (frob double (negate-double-reg) (move-double-reg) 4))

;; Compare a float with a complex or a complex with a float
#+nil
(macrolet
    ((frob (name name-r f-type c-type)
       `(progn
         (defknown ,name (,f-type ,c-type) t)
         (defknown ,name-r (,c-type ,f-type) t)
         (defun ,name (x y)
           (declare (type ,f-type x)
                    (type ,c-type y))
           (,name x y))
         (defun ,name-r (x y)
           (declare (type ,c-type x)
                    (type ,f-type y))
           (,name-r x y))
         )))
  (frob %compare-complex-single-single %compare-single-complex-single
        single-float (complex single-float))
  (frob %compare-complex-double-double %compare-double-complex-double
        double-float (complex double-float)))

#+nil
(macrolet
    ((frob (trans-1 trans-2 float-type fcmp fsub)
       (let ((vop-name
              (symbolicate "COMPLEX-" float-type "-FLOAT-"
                           float-type "-FLOAT-COMPARE"))
             (vop-name-r
              (symbolicate float-type "-FLOAT-COMPLEX-"
                           float-type "-FLOAT-COMPARE"))
             (complex-reg (symbolicate "COMPLEX-" float-type "-REG"))
             (real-reg (symbolicate float-type "-REG"))
             (c-type (symbolicate "COMPLEX-" float-type "-FLOAT"))
             (r-type (symbolicate float-type "-FLOAT"))
             (real-part (symbolicate "COMPLEX-" float-type "-REG-REAL-TN"))
             (imag-part (symbolicate "COMPLEX-" float-type "-REG-IMAG-TN")))
         `(progn
            ;; (= float complex)
            (define-vop (,vop-name)
              (:args (x :scs (,real-reg))
                     (y :scs (,complex-reg)))
              (:arg-types ,r-type ,c-type)
              (:translate ,trans-1)
              (:conditional)
              (:info target not-p)
              (:policy :fast-safe)
              (:note "inline complex float/float comparison")
              (:vop-var vop)
              (:save-p :compute-only)
              (:temporary (:sc ,real-reg) fp-zero)
              (:guard #-:sparc-v9 nil #+:sparc-v9 t)
              (:generator 6
               (note-this-location vop :internal-error)
               (let ((yr (,real-part y))
                     (yi (,imag-part y)))
                 ;; Set fp-zero to zero
                 (inst ,fsub fp-zero fp-zero fp-zero)
                 (inst ,fcmp x yr)
                 (inst nop)
                 (inst fb (if not-p :ne :eq) target #+sparc-v9 :fcc0 #+sparc-v9 :pn)
                 (inst ,fcmp yi fp-zero)
                 (inst nop)
                 (inst fb (if not-p :ne :eq) target #+sparc-v9 :fcc0 #+sparc-v9 :pn)
                 (inst nop))))
            ;; (= complex float)
            (define-vop (,vop-name-r)
              (:args (y :scs (,complex-reg))
                     (x :scs (,real-reg)))
              (:arg-types ,c-type ,r-type)
              (:translate ,trans-2)
              (:conditional)
              (:info target not-p)
              (:policy :fast-safe)
              (:note "inline complex float/float comparison")
              (:vop-var vop)
              (:save-p :compute-only)
              (:temporary (:sc ,real-reg) fp-zero)
              (:guard #-:sparc-v9 t #+:sparc-v9 nil)
              (:generator 6
               (note-this-location vop :internal-error)
               (let ((yr (,real-part y))
                     (yi (,imag-part y)))
                 ;; Set fp-zero to zero
                 (inst ,fsub fp-zero fp-zero fp-zero)
                 (inst ,fcmp x yr)
                 (inst nop)
                 (inst fb (if not-p :ne :eq) target #+sparc-v9 :fcc0 #+sparc-v9 :pn)
                 (inst ,fcmp yi fp-zero)
                 (inst nop)
                 (inst fb (if not-p :ne :eq) target #+sparc-v9 :fcc0 #+sparc-v9 :pn)
                 (inst nop))))))))
  (frob %compare-complex-single-single %compare-single-complex-single
        single fcmps fsubs)
  (frob %compare-complex-double-double %compare-double-complex-double
        double fcmpd fsubd))

;; Compare two complex numbers for equality
(macrolet
    ((frob (float-type fcmp)
       (let ((vop-name
              (symbolicate "COMPLEX-" float-type "-FLOAT-COMPARE"))
             (complex-reg (symbolicate "COMPLEX-" float-type "-REG"))
             (c-type (symbolicate "COMPLEX-" float-type "-FLOAT"))
             (real-part (symbolicate "COMPLEX-" float-type "-REG-REAL-TN"))
             (imag-part (symbolicate "COMPLEX-" float-type "-REG-IMAG-TN")))
         `(define-vop (,vop-name)
            (:args (x :scs (,complex-reg))
                   (y :scs (,complex-reg)))
            (:arg-types ,c-type ,c-type)
            (:translate =)
            (:conditional)
            (:info target not-p)
            (:policy :fast-safe)
            (:note "inline complex float comparison")
            (:vop-var vop)
            (:save-p :compute-only)
            (:generator 6
              (note-this-location vop :internal-error)
              (let ((xr (,real-part x))
                    (xi (,imag-part x))
                    (yr (,real-part y))
                    (yi (,imag-part y)))
                (inst ,fcmp xr yr)
                (inst nop)
                (inst fb (if not-p :ne :eq) target #+sparc-v9 :fcc0 #+sparc-v9 :pn)
                (inst ,fcmp xi yi)
                (inst nop)
                (inst fb (if not-p :ne :eq) target #+sparc-v9 :fcc0 #+sparc-v9 :pn)
                (inst nop)))))))
  (frob single fcmps)
  (frob double fcmpd))

;; Compare a complex with a complex, for V9
(macrolet
    ((frob (float-type fcmp)
       (let ((vop-name
              (symbolicate "V9-COMPLEX-" float-type "-FLOAT-COMPARE"))
             (complex-reg (symbolicate "COMPLEX-" float-type "-REG"))
             (c-type (symbolicate "COMPLEX-" float-type "-FLOAT"))
             (real-part (symbolicate "COMPLEX-" float-type "-REG-REAL-TN"))
             (imag-part (symbolicate "COMPLEX-" float-type "-REG-IMAG-TN")))
         `(define-vop (,vop-name)
            (:args (x :scs (,complex-reg))
                   (y :scs (,complex-reg)))
            (:arg-types ,c-type ,c-type)
            (:translate =)
            (:conditional)
            (:info target not-p)
            (:policy :fast-safe)
            (:note "inline complex float comparison")
            (:vop-var vop)
            (:save-p :compute-only)
            (:temporary (:sc descriptor-reg) true)
            (:guard (member :sparc-v9 *backend-subfeatures*))
            (:generator 5
              (note-this-location vop :internal-error)
              (let ((xr (,real-part x))
                    (xi (,imag-part x))
                    (yr (,real-part y))
                    (yi (,imag-part y)))
                ;; Assume comparison is true
                (load-symbol true t)
                (inst ,fcmp xr yr)
                (inst cmove (if not-p :eq :ne) true null-tn :fcc0)
                (inst ,fcmp xi yi)
                (inst cmove (if not-p :eq :ne) true null-tn :fcc0)
                (inst cmp true null-tn)
                (inst b (if not-p :eq :ne) target :pt)
                (inst nop)))))))
  (frob single fcmps)
  (frob double fcmpd))

) ; end progn complex-fp-vops


;;; XXX FIXME:
;;;
;;; The stuff below looks good, but we already have transforms for max
;;; and min. How should we arrange that?
#+nil
(progn

;; Vops to take advantage of the conditional move instruction
;; available on the Sparc V9

(defknown (%%max %%min) ((or (unsigned-byte #.n-word-bits)
                             (signed-byte #.n-word-bits)
                             single-float double-float)
                         (or (unsigned-byte #.n-word-bits)
                             (signed-byte #.n-word-bits)
                             single-float double-float))
  (or (unsigned-byte #.n-word-bits)
      (signed-byte #.n-word-bits)
      single-float double-float)
  (movable foldable flushable))

;; We need these definitions for byte-compiled code
;;
;; Well, we (SBCL) probably don't, having deleted the byte
;; compiler. Let's see what happens if we comment out these
;; definitions:
#+nil
(defun %%min (x y)
  (declare (type (or (unsigned-byte 32) (signed-byte 32)
                     single-float double-float) x y))
  (if (<= x y)
      x y))

#+nil
(defun %%max (x y)
  (declare (type (or (unsigned-byte 32) (signed-byte 32)
                     single-float double-float) x y))
  (if (>= x y)
      x y))
#+nil
(macrolet
    ((frob (name sc-type type compare cmov cost cc max min note)
       (let ((vop-name (symbolicate name "-" type "=>" type))
             (trans-name (symbolicate "%%" name)))
         `(define-vop (,vop-name)
            (:args (x :scs (,sc-type))
                   (y :scs (,sc-type)))
            (:results (r :scs (,sc-type)))
            (:arg-types ,type ,type)
            (:result-types ,type)
            (:policy :fast-safe)
            (:note ,note)
            (:translate ,trans-name)
            (:guard (member :sparc-v9 *backend-subfeatures*))
            (:generator ,cost
              (inst ,compare x y)
              (cond ((location= r x)
                     ;; If x < y, need to move y to r, otherwise r already has
                     ;; the max.
                     (inst ,cmov ,min r y ,cc))
                    ((location= r y)
                     ;; If x > y, need to move x to r, otherwise r already has
                     ;; the max.
                     (inst ,cmov ,max r x ,cc))
                    (t
                     ;; It doesn't matter what R is, just copy the min to R.
                     (inst ,cmov ,max r x ,cc)
                     (inst ,cmov ,min r y ,cc))))))))
  (frob max single-reg single-float fcmps cfmovs 3
        :fcc0 :ge :l "inline float max")
  (frob max double-reg double-float fcmpd cfmovd 3
        :fcc0 :ge :l "inline float max")
  (frob min single-reg single-float fcmps cfmovs 3
        :fcc0 :l :ge "inline float min")
  (frob min double-reg double-float fcmpd cfmovd 3
        :fcc0 :l :ge "inline float min")
  ;; Strictly speaking these aren't float ops, but it's convenient to
  ;; do them here.
  ;;
  ;; The cost is here is the worst case number of instructions.  For
  ;; 32-bit integer operands, we add 2 more to account for the
  ;; untagging of fixnums, if necessary.
  (frob max signed-reg signed-num cmp cmove 5
        :icc :ge :lt "inline (signed-byte 32) max")
  (frob max unsigned-reg unsigned-num cmp cmove 5
        :icc :ge :lt "inline (unsigned-byte 32) max")
  ;; For fixnums, make the cost lower so we don't have to untag the
  ;; numbers.
  (frob max any-reg tagged-num cmp cmove 3
        :icc :ge :lt "inline fixnum max")
  (frob min signed-reg signed-num cmp cmove 5
        :icc :lt :ge "inline (signed-byte 32) min")
  (frob min unsigned-reg unsigned-num cmp cmove 5
        :icc :lt :ge "inline (unsigned-byte 32) min")
  ;; For fixnums, make the cost lower so we don't have to untag the
  ;; numbers.
  (frob min any-reg tagged-num cmp cmove 3
        :icc :lt :ge "inline fixnum min"))

#+nil
(define-vop (max-boxed-double-float=>boxed-double-float)
  (:args (x :scs (descriptor-reg))
         (y :scs (descriptor-reg)))
  (:results (r :scs (descriptor-reg)))
  (:arg-types double-float double-float)
  (:result-types double-float)
  (:policy :fast-safe)
  (:note "inline float max/min")
  (:translate %max-double-float)
  (:temporary (:scs (double-reg)) xval)
  (:temporary (:scs (double-reg)) yval)
  (:guard #+:sparc-v9 t #-:sparc-v9 nil)
  (:vop-var vop)
  (:generator 3
    (let ((offset (- (* double-float-value-slot n-word-bytes)
                     other-pointer-lowtag)))
      (inst lddf xval x offset)
      (inst lddf yval y offset)
      (inst fcmpd xval yval)
      (cond ((location= r x)
             ;; If x < y, need to move y to r, otherwise r already has
             ;; the max.
             (inst cmove :l r y :fcc0))
            ((location= r y)
             ;; If x > y, need to move x to r, otherwise r already has
             ;; the max.
             (inst cmove :ge r x :fcc0))
            (t
             ;; It doesn't matter what R is, just copy the min to R.
             (inst cmove :ge r x :fcc0)
             (inst cmove :l r y :fcc0))))))

) ; PROGN

#+nil
(in-package "SB-C")
;;; FIXME
#+nil
(progn
;;; The sparc-v9 architecture has conditional move instructions that
;;; can be used.  This should be faster than using the obvious if
;;; expression since we don't have to do branches.

(define-source-transform min (&rest args)
  (if (member :sparc-v9 *backend-subfeatures*)
      (case (length args)
        ((0 2) (values nil t))
        (1 `(values ,(first args)))
        (t (sb-c::associate-arguments 'min (first args) (rest args))))
      (values nil t)))

(define-source-transform max (&rest args)
  (if (member :sparc-v9 *backend-subfeatures*)
      (case (length args)
        ((0 2) (values nil t))
        (1 `(values ,(first args)))
        (t (sb-c::associate-arguments 'max (first args) (rest args))))
      (values nil t)))

;; Derive the types of max and min
(defoptimizer (max derive-type) ((x y))
  (multiple-value-bind (definitely-< definitely->=)
      (ir1-transform-<-helper x y)
    (cond (definitely-<
              (lvar-type y))
          (definitely->=
              (lvar-type x))
          (t
           (make-canonical-union-type (list (lvar-type x)
                                            (lvar-type y)))))))

(defoptimizer (min derive-type) ((x y))
  (multiple-value-bind (definitely-> definitely-<=)
      (ir1-transform-<-helper y x)
    (cond (definitely-<=
              (lvar-type x))
          (definitely->
              (lvar-type y))
          (t
           (make-canonical-union-type (list (lvar-type x)
                                            (lvar-type y)))))))

(deftransform max ((x y) (number number) *)
  (let ((x-type (lvar-type x))
        (y-type (lvar-type y))
        (signed (specifier-type `(signed-byte ,n-word-bits)))
        (unsigned (specifier-type `(unsigned-byte ,n-word-bits)))
        (d-float (specifier-type 'double-float))
        (s-float (specifier-type 'single-float)))
    ;; Use %%max if both args are good types of the same type.  As a
    ;; last resort, use the obvious comparison to select the desired
    ;; element.
    (cond ((and (csubtypep x-type signed)
                (csubtypep y-type signed))
           `(%%max x y))
          ((and (csubtypep x-type unsigned)
                (csubtypep y-type unsigned))
           `(%%max x y))
          ((and (csubtypep x-type d-float)
                (csubtypep y-type d-float))
           `(%%max x y))
          ((and (csubtypep x-type s-float)
                (csubtypep y-type s-float))
           `(%%max x y))
          (t
           (let ((arg1 (gensym))
                 (arg2 (gensym)))
             `(let ((,arg1 x)
                    (,arg2 y))
               (if (>= ,arg1 ,arg2)
                   ,arg1 ,arg2)))))))

(deftransform min ((x y) (real real) *)
  (let ((x-type (lvar-type x))
        (y-type (lvar-type y))
        (signed (specifier-type `(signed-byte ,n-word-bits)))
        (unsigned (specifier-type `(unsigned-byte ,n-word-bits)))
        (d-float (specifier-type 'double-float))
        (s-float (specifier-type 'single-float)))
    (cond ((and (csubtypep x-type signed)
                (csubtypep y-type signed))
           `(%%min x y))
          ((and (csubtypep x-type unsigned)
                (csubtypep y-type unsigned))
           `(%%min x y))
          ((and (csubtypep x-type d-float)
                (csubtypep y-type d-float))
           `(%%min x y))
          ((and (csubtypep x-type s-float)
                (csubtypep y-type s-float))
           `(%%min x y))
          (t
           (let ((arg1 (gensym))
                 (arg2 (gensym)))
             `(let ((,arg1 x)
                    (,arg2 y))
                (if (<= ,arg1 ,arg2)
                    ,arg1 ,arg2)))))))

) ; PROGN

