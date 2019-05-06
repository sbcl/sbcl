;;;; the MIPS VM definition of floating point operations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")


;;;; Move functions:
(define-move-fun (load-single 1) (vop x y)
  ((single-stack) (single-reg))
  (inst lwc1 y (current-nfp-tn vop) (tn-byte-offset x))
  (inst nop))

(define-move-fun (store-single 1) (vop x y)
  ((single-reg) (single-stack))
  (inst swc1 x (current-nfp-tn vop) (tn-byte-offset y)))

(defun ld-double (r base offset)
  (ecase *backend-byte-order*
    (:big-endian
     (inst lwc1 r base (+ offset n-word-bytes))
     (inst lwc1-odd r base offset))
    (:little-endian
     (inst lwc1 r base offset)
     (inst lwc1-odd r base (+ offset n-word-bytes)))))

(define-move-fun (load-double 2) (vop x y)
  ((double-stack) (double-reg))
  (let ((nfp (current-nfp-tn vop))
        (offset (tn-byte-offset x)))
    (ld-double y nfp offset))
  (inst nop))

(defun str-double (x base offset)
  (ecase *backend-byte-order*
    (:big-endian
     (inst swc1 x base (+ offset n-word-bytes))
     (inst swc1-odd x base offset))
    (:little-endian
     (inst swc1 x base offset)
     (inst swc1-odd x base (+ offset n-word-bytes)))))

(define-move-fun (store-double 2) (vop x y)
  ((double-reg) (double-stack))
  (let ((nfp (current-nfp-tn vop))
        (offset (tn-byte-offset y)))
    (str-double x nfp offset)))

;;;; Move VOPs:
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
                      (inst fmove ,format y x))))
                (define-move-vop ,vop :move (,sc) (,sc)))))
  (frob single-move single-reg :single)
  (frob double-move double-reg :double))

(define-vop (move-from-float)
  (:args (x :to :save))
  (:results (y))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:variant-vars double-p size type data)
  (:note "float to pointer coercion")
  (:generator 13
    (with-fixed-allocation (y pa-flag ndescr type size nil)
      (if double-p
          (str-double x y (- (* data n-word-bytes) other-pointer-lowtag))
          (inst swc1 x y (- (* data n-word-bytes) other-pointer-lowtag))))))

(macrolet ((frob (name sc &rest args)
             `(progn
                (define-vop (,name move-from-float)
                  (:args (x :scs (,sc) :to :save))
                  (:results (y :scs (descriptor-reg)))
                  (:variant ,@args))
                (define-move-vop ,name :move (,sc) (descriptor-reg)))))
  (frob move-from-single single-reg
    nil single-float-size single-float-widetag single-float-value-slot)
  (frob move-from-double double-reg
    t double-float-size double-float-widetag double-float-value-slot))

(macrolet ((frob (name sc double-p value)
             `(progn
                (define-vop (,name)
                  (:args (x :scs (descriptor-reg)))
                  (:results (y :scs (,sc)))
                  (:note "pointer to float coercion")
                  (:generator 2
                    ,@(ecase *backend-byte-order*
                        (:big-endian
                         (cond
                          (double-p
                           `((inst lwc1 y x (- (* (1+ ,value) n-word-bytes)
                                               other-pointer-lowtag))
                             (inst lwc1-odd y x (- (* ,value n-word-bytes)
                                                   other-pointer-lowtag))))
                          (t
                           `((inst lwc1 y x (- (* ,value n-word-bytes)
                                               other-pointer-lowtag))))))
                        (:little-endian
                         `((inst lwc1 y x (- (* ,value n-word-bytes)
                                             other-pointer-lowtag))
                           ,@(when double-p
                               `((inst lwc1-odd y x
                                       (- (* (1+ ,value) n-word-bytes)
                                          other-pointer-lowtag)))))))
                    (inst nop)))
                (define-move-vop ,name :move (descriptor-reg) (,sc)))))
  (frob move-to-single single-reg nil single-float-value-slot)
  (frob move-to-double double-reg t double-float-value-slot))

(macrolet ((frob (name sc stack-sc format double-p)
             `(progn
                (define-vop (,name)
                  (:args (x :scs (,sc) :target y)
                         (nfp :scs (any-reg)
                              :load-if (not (sc-is y ,sc))))
                  (:results (y))
                  (:note "float argument move")
                  (:generator ,(if double-p 2 1)
                    (sc-case y
                      (,sc
                       (unless (location= x y)
                         (inst fmove ,format y x)))
                      (,stack-sc
                       (let ((offset (tn-byte-offset y)))
                         ,@(ecase *backend-byte-order*
                             (:big-endian
                              (cond
                               (double-p
                                '((inst swc1 x nfp (+ offset n-word-bytes))
                                  (inst swc1-odd x nfp offset)))
                               (t
                                '((inst swc1 x nfp offset)))))
                             (:little-endian
                              `((inst swc1 x nfp offset)
                                ,@(when double-p
                                    '((inst swc1-odd x nfp
                                            (+ offset n-word-bytes))))))))))))
                (define-move-vop ,name :move-arg
                  (,sc descriptor-reg) (,sc)))))
  (frob move-single-float-arg single-reg single-stack :single nil)
  (frob move-double-float-arg double-reg double-stack :double t))

;;;; Complex float move functions

(defun complex-single-reg-real-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'single-reg)
                  :offset (tn-offset x)))
(defun complex-single-reg-imag-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'single-reg)
                  :offset (+ (tn-offset x) 2)))

(defun complex-double-reg-real-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg)
                  :offset (tn-offset x)))
(defun complex-double-reg-imag-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg)
                  :offset (+ (tn-offset x) 2)))

(define-move-fun (load-complex-single 2) (vop x y)
  ((complex-single-stack) (complex-single-reg))
  (let ((nfp (current-nfp-tn vop))
        (offset (tn-byte-offset x)))
    (let ((real-tn (complex-single-reg-real-tn y)))
      (inst lwc1 real-tn nfp offset))
    (let ((imag-tn (complex-single-reg-imag-tn y)))
      (inst lwc1 imag-tn nfp (+ offset n-word-bytes))))
  (inst nop))

(define-move-fun (store-complex-single 2) (vop x y)
  ((complex-single-reg) (complex-single-stack))
  (let ((nfp (current-nfp-tn vop))
        (offset (tn-byte-offset y)))
    (let ((real-tn (complex-single-reg-real-tn x)))
      (inst swc1 real-tn nfp offset))
    (let ((imag-tn (complex-single-reg-imag-tn x)))
      (inst swc1 imag-tn nfp (+ offset n-word-bytes)))))

(define-move-fun (load-complex-double 4) (vop x y)
  ((complex-double-stack) (complex-double-reg))
  (let ((nfp (current-nfp-tn vop))
        (offset (tn-byte-offset x)))
    (let ((real-tn (complex-double-reg-real-tn y)))
      (ld-double real-tn nfp offset))
    (let ((imag-tn (complex-double-reg-imag-tn y)))
      (ld-double imag-tn nfp (+ offset (* 2 n-word-bytes))))
    (inst nop)))

(define-move-fun (store-complex-double 4) (vop x y)
  ((complex-double-reg) (complex-double-stack))
  (let ((nfp (current-nfp-tn vop))
        (offset (tn-byte-offset y)))
    (let ((real-tn (complex-double-reg-real-tn x)))
      (str-double real-tn nfp offset))
    (let ((imag-tn (complex-double-reg-imag-tn x)))
      (str-double imag-tn nfp (+ offset (* 2 n-word-bytes))))))

;;; Complex float register to register moves.
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
         (inst fmove :single y-real x-real))
       (let ((x-imag (complex-single-reg-imag-tn x))
             (y-imag (complex-single-reg-imag-tn y)))
         (inst fmove :single y-imag x-imag)))))
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
         (inst fmove :double y-real x-real))
       (let ((x-imag (complex-double-reg-imag-tn x))
             (y-imag (complex-double-reg-imag-tn y)))
         (inst fmove :double y-imag x-imag)))))
(define-move-vop complex-double-move :move
  (complex-double-reg) (complex-double-reg))

;;; Move from a complex float to a descriptor register allocating a
;;; new complex float object in the process.
(define-vop (move-from-complex-single)
  (:args (x :scs (complex-single-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:note "complex single float to pointer coercion")
  (:generator 13
    (with-fixed-allocation (y pa-flag ndescr complex-single-float-widetag
                              complex-single-float-size nil)
      (let ((real-tn (complex-single-reg-real-tn x)))
        (inst swc1 real-tn y (- (* complex-single-float-real-slot
                                   n-word-bytes)
                                other-pointer-lowtag)))
      (let ((imag-tn (complex-single-reg-imag-tn x)))
        (inst swc1 imag-tn y (- (* complex-single-float-imag-slot
                                   n-word-bytes)
                                other-pointer-lowtag))))))
(define-move-vop move-from-complex-single :move
  (complex-single-reg) (descriptor-reg))

(define-vop (move-from-complex-double)
  (:args (x :scs (complex-double-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:note "complex double float to pointer coercion")
  (:generator 13
    (with-fixed-allocation (y pa-flag ndescr complex-double-float-widetag
                              complex-double-float-size nil)
      (let ((real-tn (complex-double-reg-real-tn x)))
        (str-double real-tn y (- (* complex-double-float-real-slot
                                    n-word-bytes)
                                 other-pointer-lowtag)))
      (let ((imag-tn (complex-double-reg-imag-tn x)))
        (str-double imag-tn y (- (* complex-double-float-imag-slot
                                    n-word-bytes)
                                 other-pointer-lowtag))))))
(define-move-vop move-from-complex-double :move
  (complex-double-reg) (descriptor-reg))

;;; Move from a descriptor to a complex float register
(define-vop (move-to-complex-single)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (complex-single-reg)))
  (:note "pointer to complex float coercion")
  (:generator 2
    (let ((real-tn (complex-single-reg-real-tn y)))
      (inst lwc1 real-tn x (- (* complex-single-float-real-slot n-word-bytes)
                              other-pointer-lowtag)))
    (let ((imag-tn (complex-single-reg-imag-tn y)))
      (inst lwc1 imag-tn x (- (* complex-single-float-imag-slot n-word-bytes)
                              other-pointer-lowtag)))
    (inst nop)))
(define-move-vop move-to-complex-single :move
  (descriptor-reg) (complex-single-reg))

(define-vop (move-to-complex-double)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (complex-double-reg)))
  (:note "pointer to complex float coercion")
  (:generator 2
    (let ((real-tn (complex-double-reg-real-tn y)))
      (ld-double real-tn x (- (* complex-double-float-real-slot n-word-bytes)
                              other-pointer-lowtag)))
    (let ((imag-tn (complex-double-reg-imag-tn y)))
      (ld-double imag-tn x (- (* complex-double-float-imag-slot n-word-bytes)
                              other-pointer-lowtag)))
    (inst nop)))
(define-move-vop move-to-complex-double :move
  (descriptor-reg) (complex-double-reg))

;;; complex float MOVE-ARG VOP
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
           (inst fmove :single y-real x-real))
         (let ((x-imag (complex-single-reg-imag-tn x))
               (y-imag (complex-single-reg-imag-tn y)))
           (inst fmove :single y-imag x-imag))))
      (complex-single-stack
       (let ((offset (tn-byte-offset y)))
         (let ((real-tn (complex-single-reg-real-tn x)))
           (inst swc1 real-tn nfp offset))
         (let ((imag-tn (complex-single-reg-imag-tn x)))
           (inst swc1 imag-tn nfp (+ offset n-word-bytes))))))))
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
           (inst fmove :double y-real x-real))
         (let ((x-imag (complex-double-reg-imag-tn x))
               (y-imag (complex-double-reg-imag-tn y)))
           (inst fmove :double y-imag x-imag))))
      (complex-double-stack
       (let ((offset (tn-byte-offset y)))
         (let ((real-tn (complex-double-reg-real-tn x)))
           (str-double real-tn nfp offset))
         (let ((imag-tn (complex-double-reg-imag-tn x)))
           (str-double imag-tn nfp (+ offset (* 2 n-word-bytes)))))))))
(define-move-vop move-complex-double-float-arg :move-arg
  (complex-double-reg descriptor-reg) (complex-double-reg))

(define-move-vop move-arg :move-arg
  (single-reg double-reg complex-single-reg complex-double-reg)
  (descriptor-reg))


;;;; stuff for c-call float-in-int-register arguments
(define-vop (move-to-single-int-reg)
  (:args (x :scs (single-reg descriptor-reg)))
  (:results (y :scs (single-int-carg-reg) :load-if nil))
  (:note "pointer to float-in-int coercion")
  (:generator 1
    (sc-case x
      (single-reg
       (inst mfc1 y x))
      (descriptor-reg
       (inst lw y x (- (* single-float-value-slot n-word-bytes)
                       other-pointer-lowtag))))
    (inst nop)))                        ;nop needed here?
(define-move-vop move-to-single-int-reg
    :move (single-reg descriptor-reg) (single-int-carg-reg))

(define-vop (move-single-int-reg)
  (:args (x :target y :scs (single-int-carg-reg) :load-if nil)
         (fp :scs (any-reg) :load-if (not (sc-is y single-int-carg-reg))))
  (:results (y :scs (single-int-carg-reg) :load-if nil))
  (:ignore fp)
  (:generator 1
    (unless (location= x y)
      (error "Huh? why did it do that?"))))
(define-move-vop move-single-int-reg :move-arg
  (single-int-carg-reg) (single-int-carg-reg))

(define-vop (move-to-double-int-reg)
  (:args (x :scs (double-reg descriptor-reg)))
  (:results (y :scs (double-int-carg-reg) :load-if nil))
  (:note "pointer to float-in-int coercion")
  (:generator 2
    (sc-case x
      (double-reg
       (ecase *backend-byte-order*
         (:big-endian
          (inst mfc1-odd2 y x)
          (inst mfc1-odd y x))
         (:little-endian
          (inst mfc1 y x)
          (inst mfc1-odd3 y x))))
      (descriptor-reg
       (inst lw y x (- (* double-float-value-slot n-word-bytes)
                       other-pointer-lowtag))
       (inst lw-odd y x (- (* (1+ double-float-value-slot) n-word-bytes)
                           other-pointer-lowtag))))
    (inst nop)))                        ;nop needed here?
(define-move-vop move-to-double-int-reg
    :move (double-reg descriptor-reg) (double-int-carg-reg))

(define-vop (move-double-int-reg)
  (:args (x :target y :scs (double-int-carg-reg) :load-if nil)
         (fp :scs (any-reg) :load-if (not (sc-is y double-int-carg-reg))))
  (:results (y :scs (double-int-carg-reg) :load-if nil))
  (:ignore fp)
  (:generator 2
    (unless (location= x y)
      (error "Huh? why did it do that?"))))
(define-move-vop move-double-int-reg :move-arg
  (double-int-carg-reg) (double-int-carg-reg))


;;;; Arithmetic VOPs:

(define-vop (float-op)
  (:args (x) (y))
  (:results (r))
  (:variant-vars format operation)
  (:policy :fast-safe)
  (:note "inline float arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 0
    (note-this-location vop :internal-error)
    (inst float-op operation format r x y)))

(macrolet ((frob (name sc ptype)
             `(define-vop (,name float-op)
                (:args (x :scs (,sc))
                       (y :scs (,sc)))
                (:results (r :scs (,sc)))
                (:arg-types ,ptype ,ptype)
                (:result-types ,ptype))))
  (frob single-float-op single-reg single-float)
  (frob double-float-op double-reg double-float))

(macrolet ((frob (op sname scost dname dcost)
             `(progn
                (define-vop (,sname single-float-op)
                  (:translate ,op)
                  (:variant :single ',op)
                  (:variant-cost ,scost))
                (define-vop (,dname double-float-op)
                  (:translate ,op)
                  (:variant :double ',op)
                  (:variant-cost ,dcost)))))
  (frob + +/single-float 2 +/double-float 2)
  (frob - -/single-float 2 -/double-float 2)
  (frob * */single-float 4 */double-float 5)
  (frob / //single-float 12 //double-float 19))

(macrolet ((frob (name inst translate format sc type)
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
                  (inst ,inst ,format y x)))))
  (frob abs/single-float fabs abs :single single-reg single-float)
  (frob abs/double-float fabs abs :double double-reg double-float)
  (frob %negate/single-float fneg %negate :single single-reg single-float)
  (frob %negate/double-float fneg %negate :double double-reg double-float))


;;;; Comparison:

(define-vop (float-compare)
  (:args (x) (y))
  (:conditional)
  (:info target not-p)
  (:variant-vars format operation complement)
  (:policy :fast-safe)
  (:note "inline float comparison")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 3
    (note-this-location vop :internal-error)
    (inst fcmp operation format x y)
    (inst nop)
    (if (if complement (not not-p) not-p)
        (inst bc1f target)
        (inst bc1t target))
    (inst nop)))

(macrolet ((frob (name sc ptype)
             `(define-vop (,name float-compare)
                (:args (x :scs (,sc))
                       (y :scs (,sc)))
                (:arg-types ,ptype ,ptype))))
  (frob single-float-compare single-reg single-float)
  (frob double-float-compare double-reg double-float))

(macrolet ((frob (translate op complement sname dname)
             `(progn
                (define-vop (,sname single-float-compare)
                  (:translate ,translate)
                  (:variant :single ,op ,complement))
                (define-vop (,dname double-float-compare)
                  (:translate ,translate)
                  (:variant :double ,op ,complement)))))
  (frob < :lt nil </single-float </double-float)
  (frob > :ngt t >/single-float >/double-float)
  (frob = :seq nil =/single-float =/double-float))


;;;; Conversion:

(macrolet ((frob (name translate
                       from-sc from-type from-format
                       to-sc to-type to-format)
             (let ((word-p (eq from-format :word)))
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
                  (:generator ,(if word-p 3 2)
                    ,@(if word-p
                          `((inst mtc1 y x)
                            (inst nop)
                            (note-this-location vop :internal-error)
                            (inst fcvt ,to-format :word y y))
                          `((note-this-location vop :internal-error)
                            (inst fcvt ,to-format ,from-format y x))))))))
  (frob %single-float/signed %single-float
    signed-reg signed-num :word
    single-reg single-float :single)
  (frob %double-float/signed %double-float
    signed-reg signed-num :word
    double-reg double-float :double)
  (frob %single-float/double-float %single-float
    double-reg double-float :double
    single-reg single-float :single)
  (frob %double-float/single-float %double-float
    single-reg single-float :single
    double-reg double-float :double))


(macrolet ((frob (name from-sc from-type from-format)
             `(define-vop (,name)
                (:args (x :scs (,from-sc)))
                (:results (y :scs (signed-reg)))
                (:temporary (:from (:argument 0) :sc ,from-sc) temp)
                (:arg-types ,from-type)
                (:result-types signed-num)
                (:translate %unary-round)
                (:policy :fast-safe)
                (:note "inline float round")
                (:vop-var vop)
                (:save-p :compute-only)
                (:generator 3
                  (note-this-location vop :internal-error)
                  (inst fcvt :word ,from-format temp x)
                  (inst mfc1 y temp)
                  (inst nop)))))
  (frob %unary-round/single-float single-reg single-float :single)
  (frob %unary-round/double-float double-reg double-float :double))


;;; These VOPs have to uninterruptibly frob the rounding mode in order to get
;;; the desired round-to-zero behavior.
;;;
(macrolet ((frob (name from-sc from-type from-format)
             `(define-vop (,name)
                (:args (x :scs (,from-sc)))
                (:results (y :scs (signed-reg)))
                (:temporary (:from (:argument 0) :sc ,from-sc) temp)
                (:temporary (:sc non-descriptor-reg) status-save new-status)
                (:temporary (:sc non-descriptor-reg :offset nl4-offset)
                            pa-flag)
                (:arg-types ,from-type)
                (:result-types signed-num)
                (:translate ,name)
                (:policy :fast-safe)
                (:note "inline float truncate")
                (:vop-var vop)
                (:save-p :compute-only)
                (:generator 16
                  (pseudo-atomic (pa-flag)
                    (inst cfc1 status-save 31)
                    (inst li new-status (lognot 3))
                    (inst and new-status status-save)
                    (inst or new-status float-round-to-zero)
                    (inst ctc1 new-status 31)

                    ;; These instructions seem to be necessary to ensure that
                    ;; the new modes affect the fcvt instruction.
                    (inst nop)
                    (inst cfc1 new-status 31)

                    (note-this-location vop :internal-error)
                    (inst fcvt :word ,from-format temp x)
                    (inst mfc1 y temp)
                    (inst nop)
                    (inst ctc1 status-save 31))))))
  (frob %unary-truncate/single-float single-reg single-float :single)
  (frob %unary-truncate/double-float double-reg double-float :double))


(define-vop (make-single-float)
  (:args (bits :scs (signed-reg)))
  (:results (res :scs (single-reg)))
  (:arg-types signed-num)
  (:result-types single-float)
  (:translate make-single-float)
  (:policy :fast-safe)
  (:generator 2
    (inst mtc1 res bits)
    (inst nop)))

(define-vop (make-double-float)
  (:args (hi-bits :scs (signed-reg))
         (lo-bits :scs (unsigned-reg)))
  (:results (res :scs (double-reg)))
  (:arg-types signed-num unsigned-num)
  (:result-types double-float)
  (:translate make-double-float)
  (:policy :fast-safe)
  (:generator 2
    (inst mtc1 res lo-bits)
    (inst mtc1-odd res hi-bits)
    (inst nop)))

(define-vop (single-float-bits)
  (:args (float :scs (single-reg)))
  (:results (bits :scs (signed-reg)))
  (:arg-types single-float)
  (:result-types signed-num)
  (:translate single-float-bits)
  (:policy :fast-safe)
  (:generator 2
    (inst mfc1 bits float)
    (inst nop)))

(define-vop (double-float-high-bits)
  (:args (float :scs (double-reg)))
  (:results (hi-bits :scs (signed-reg)))
  (:arg-types double-float)
  (:result-types signed-num)
  (:translate double-float-high-bits)
  (:policy :fast-safe)
  (:generator 2
    (inst mfc1-odd hi-bits float)
    (inst nop)))

(define-vop (double-float-low-bits)
  (:args (float :scs (double-reg)))
  (:results (lo-bits :scs (unsigned-reg)))
  (:arg-types double-float)
  (:result-types unsigned-num)
  (:translate double-float-low-bits)
  (:policy :fast-safe)
  (:generator 2
    (inst mfc1 lo-bits float)
    (inst nop)))


;;;; Complex float VOPs

(define-vop (make-complex-single-float)
  (:translate complex)
  (:args (real :scs (single-reg) :target r)
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
           (inst fmove :single r-real real)))
       (let ((r-imag (complex-single-reg-imag-tn r)))
         (unless (location= imag r-imag)
           (inst fmove :single r-imag imag))))
      (complex-single-stack
       (let ((nfp (current-nfp-tn vop))
             (offset (tn-byte-offset r)))
         (inst swc1 real nfp offset)
         (inst swc1 imag nfp (+ offset n-word-bytes)))))))

(define-vop (make-complex-double-float)
  (:translate complex)
  (:args (real :scs (double-reg) :target r)
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
           (inst fmove :double r-real real)))
       (let ((r-imag (complex-double-reg-imag-tn r)))
         (unless (location= imag r-imag)
           (inst fmove :double r-imag imag))))
      (complex-double-stack
       (let ((nfp (current-nfp-tn vop))
             (offset (tn-byte-offset r)))
         (str-double real nfp offset)
         (str-double imag nfp (+ offset (* 2 n-word-bytes))))))))


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
           (inst fmove :single r value-tn))))
      (complex-single-stack
       (inst lwc1 r (current-nfp-tn vop) (* (+ (ecase slot (:real 0) (:imag 1))
                                               (tn-offset x))
                                            n-word-bytes))
       (inst nop)))))

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
           (inst fmove :double r value-tn))))
      (complex-double-stack
       (ld-double r (current-nfp-tn vop) (* (+ (ecase slot (:real 0) (:imag 2))
                                               (tn-offset x))
                                            n-word-bytes))
       (inst nop)))))

(define-vop (realpart/complex-double-float complex-double-float-value)
  (:translate realpart)
  (:note "complex double float realpart")
  (:variant :real))

(define-vop (imagpart/complex-double-float complex-double-float-value)
  (:translate imagpart)
  (:note "complex double float imagpart")
  (:variant :imag))
