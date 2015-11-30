;;;; the HPPA VM definition of floating point operations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; Move functions.
(define-move-fun (load-fp-zero 1) (vop x y)
  ((fp-single-zero) (single-reg)
   (fp-double-zero) (double-reg))
  (inst funop :copy x y))

(defun ld-float (offset base r)
  (cond ((< offset (ash 1 4))
         (inst flds offset base r))
        ((and (< offset (ash 1 13))
              (> offset 0))
         (inst ldo offset zero-tn lip-tn)
         (inst fldx lip-tn base r))
        (t
          (error "ld-float: bad offset: ~s~%" offset))))

(define-move-fun (load-float 1) (vop x y)
  ((single-stack) (single-reg)
   (double-stack) (double-reg))
  (let ((offset (* (tn-offset x) n-word-bytes)))
    (ld-float offset (current-nfp-tn vop) y)))

(defun str-float (x offset base)
  (cond ((< offset (ash 1 4))
         ;(note-next-instruction vop :internal-error)
         (inst fsts x offset base))
        ((and (< offset (ash 1 13))
              (> offset 0))
         ;; FIXME-lav, ok with GC to use lip-tn for arbitrary offsets ?
         (inst ldo offset zero-tn lip-tn)
         ;(note-next-instruction vop :internal-error)
         (inst fstx x lip-tn base))
        (t
          (error "str-float: bad offset: ~s~%" offset))))

(define-move-fun (store-float 1) (vop x y)
  ((single-reg) (single-stack)
   (double-reg) (double-stack))
  (let ((offset (* (tn-offset y) n-word-bytes)))
    (str-float x offset (current-nfp-tn vop))))

;;;; Move VOPs
(define-vop (move-float)
  (:args (x :scs (single-reg double-reg)
            :target y
            :load-if (not (location= x y))))
  (:results (y :scs (single-reg double-reg)
               :load-if (not (location= x y))))
  (:note "float move")
  (:generator 0
    (unless (location= y x)
      (inst funop :copy x y))))
(define-move-vop move-float :move (single-reg) (single-reg))
(define-move-vop move-float :move (double-reg) (double-reg))

(define-vop (move-from-float)
  (:args (x :to :save))
  (:results (y :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:variant-vars size type data)
  (:note "float to pointer coercion")
  (:generator 13
    (with-fixed-allocation (y nil ndescr type size nil)
      nil)
    (inst fsts x (- (* data n-word-bytes) other-pointer-lowtag) y)))

(macrolet ((frob (name sc &rest args)
             `(progn
                (define-vop (,name move-from-float)
                  (:args (x :scs (,sc) :to :save))
                  (:variant ,@args))
                (define-move-vop ,name :move (,sc) (descriptor-reg)))))
  (frob move-from-single single-reg
    single-float-size single-float-widetag single-float-value-slot)
  (frob move-from-double double-reg
    double-float-size double-float-widetag double-float-value-slot))

(define-vop (move-to-float)
  (:args (x :scs (descriptor-reg)))
  (:results (y))
  (:variant-vars offset)
  (:note "pointer to float coercion")
  (:generator 2
    (inst flds (- (* offset n-word-bytes) other-pointer-lowtag) x y)))

(macrolet ((frob (name sc offset)
             `(progn
                (define-vop (,name move-to-float)
                  (:results (y :scs (,sc)))
                  (:variant ,offset))
                (define-move-vop ,name :move (descriptor-reg) (,sc)))))
  (frob move-to-single single-reg single-float-value-slot)
  (frob move-to-double double-reg double-float-value-slot))

(define-vop (move-float-arg)
  (:args (x :scs (single-reg double-reg) :target y)
         (nfp :scs (any-reg)
              :load-if (not (sc-is y single-reg double-reg))))
  (:results (y))
  (:note "float argument move")
  (:generator 1
    (sc-case y
      ((single-reg double-reg)
       (unless (location= x y)
         (inst funop :copy x y)))
      ((single-stack double-stack)
       (let ((offset (* (tn-offset y) n-word-bytes)))
         (str-float x offset nfp))))))
(define-move-vop move-float-arg :move-arg
  (single-reg descriptor-reg) (single-reg))
(define-move-vop move-float-arg :move-arg
  (double-reg descriptor-reg) (double-reg))

;;;; Complex float move functions
(defun complex-single-reg-real-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'single-reg)
                  :offset (tn-offset x)))
(defun complex-single-reg-imag-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'single-reg)
                  :offset (1+ (tn-offset x))))

(defun complex-double-reg-real-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'complex-double-reg)
                  :offset (tn-offset x)))
(defun complex-double-reg-imag-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'complex-double-reg)
                  :offset (1+ (tn-offset x))))

(macrolet
  ((def-move-fun (dir type size from to)
     `(define-move-fun (,(symbolicate dir "-" type) ,size) (vop x y)
        ((,(symbolicate type "-" from)) (,(symbolicate type "-" to)))
        (let ((nfp (current-nfp-tn vop))
              (offset (* (tn-offset ,(if (eq dir 'load) 'x 'y)) n-word-bytes)))
          ,@(if (eq dir 'load)
              `((let ((real-tn (,(symbolicate type "-REG-REAL-TN") y)))
                  (ld-float offset nfp real-tn))
                (let ((imag-tn (,(symbolicate type "-REG-IMAG-TN") y)))
                  (ld-float (+ offset (* ,(/ size 2) n-word-bytes)) nfp imag-tn)))
              `((let ((real-tn (,(symbolicate type "-REG-REAL-TN") x)))
                  (str-float real-tn offset nfp))
                (let ((imag-tn (,(symbolicate type "-REG-IMAG-TN") x)))
                  (str-float imag-tn
                             (+ offset (* ,(/ size 2) n-word-bytes))
                             nfp))))))))
  (def-move-fun load  complex-single 2 stack reg)
  (def-move-fun store complex-single 2 reg stack)
  (def-move-fun load  complex-double 4 stack reg)
  (def-move-fun store complex-double 4 reg stack))

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
         (inst funop :copy x-real y-real))
       (let ((x-imag (complex-single-reg-imag-tn x))
             (y-imag (complex-single-reg-imag-tn y)))
         (inst funop :copy x-imag y-imag)))))
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
         (inst funop :copy x-real y-real))
       (let ((x-imag (complex-double-reg-imag-tn x))
             (y-imag (complex-double-reg-imag-tn y)))
         (inst funop :copy x-imag y-imag)))))
(define-move-vop complex-double-move :move
  (complex-double-reg) (complex-double-reg))

;;; Move from a complex float to a descriptor register allocating a
;;; new complex float object in the process.
(define-vop (move-from-complex-single)
  (:args (x :scs (complex-single-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:note "complex single float to pointer coercion")
  (:generator 13
     (with-fixed-allocation (y nil ndescr complex-single-float-widetag
                               complex-single-float-size nil)
       nil)
     (let ((real-tn (complex-single-reg-real-tn x)))
       (inst fsts real-tn (- (* complex-single-float-real-slot n-word-bytes)
                             other-pointer-lowtag) y))
     (let ((imag-tn (complex-single-reg-imag-tn x)))
       (inst fsts imag-tn (- (* complex-single-float-imag-slot n-word-bytes)
                             other-pointer-lowtag) y))))
(define-move-vop move-from-complex-single :move
  (complex-single-reg) (descriptor-reg))

(define-vop (move-from-complex-double)
  (:args (x :scs (complex-double-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:note "complex double float to pointer coercion")
  (:generator 13
     (with-fixed-allocation (y nil ndescr complex-double-float-widetag
                               complex-double-float-size nil)
       nil)
     (let ((real-tn (complex-double-reg-real-tn x)))
       (inst fsts real-tn (- (* complex-double-float-real-slot n-word-bytes)
                             other-pointer-lowtag) y))
     (let ((imag-tn (complex-double-reg-imag-tn x)))
       (inst fsts imag-tn (- (* complex-double-float-imag-slot n-word-bytes)
                             other-pointer-lowtag) y))))
(define-move-vop move-from-complex-double :move
  (complex-double-reg) (descriptor-reg))

;;; Move from a descriptor to a complex float register
(define-vop (move-to-complex-single)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (complex-single-reg)))
  (:note "pointer to complex float coercion")
  (:generator 2
    (let ((real-tn (complex-single-reg-real-tn y)))
      (inst flds (- (* complex-single-float-real-slot n-word-bytes)
                    other-pointer-lowtag)
                 x real-tn))
    (let ((imag-tn (complex-single-reg-imag-tn y)))
      (inst flds (- (* complex-single-float-imag-slot n-word-bytes)
                    other-pointer-lowtag)
                 x imag-tn))))
(define-move-vop move-to-complex-single :move
  (descriptor-reg) (complex-single-reg))

(define-vop (move-to-complex-double)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (complex-double-reg)))
  (:note "pointer to complex float coercion")
  (:generator 2
    (let ((real-tn (complex-double-reg-real-tn y)))
      (inst flds (- (* complex-double-float-real-slot n-word-bytes)
                    other-pointer-lowtag)
            x real-tn))
    (let ((imag-tn (complex-double-reg-imag-tn y)))
      (inst flds (- (* complex-double-float-imag-slot n-word-bytes)
                    other-pointer-lowtag)
            x imag-tn))))
(define-move-vop move-to-complex-double :move
  (descriptor-reg) (complex-double-reg))

;;; Complex float move-arg vop
(define-vop (move-complex-single-float-arg)
  (:args (x :scs (complex-single-reg) :target y)
         (nfp :scs (any-reg) :load-if (not (sc-is y complex-single-reg))))
  (:results (y))
  (:note "float argument move")
  (:generator 1
    (sc-case y
      (complex-single-reg
       (unless (location= x y)
         (let ((x-real (complex-single-reg-real-tn x))
               (y-real (complex-single-reg-real-tn y)))
           (inst funop :copy x-real y-real))
         (let ((x-imag (complex-single-reg-imag-tn x))
               (y-imag (complex-single-reg-imag-tn y)))
           (inst funop :copy x-imag y-imag))))
      (complex-single-stack
       (let ((offset (* (tn-offset y) n-word-bytes)))
         (let ((real-tn (complex-single-reg-real-tn x)))
           (str-float real-tn offset nfp))
         (let ((imag-tn (complex-single-reg-imag-tn x)))
           (str-float imag-tn (+ offset n-word-bytes) nfp)))))))
(define-move-vop move-complex-single-float-arg :move-arg
  (complex-single-reg descriptor-reg) (complex-single-reg))

(define-vop (move-complex-double-float-arg)
  (:args (x :scs (complex-double-reg) :target y)
         (nfp :scs (any-reg) :load-if (not (sc-is y complex-double-reg))))
  (:results (y))
  (:note "float argument move")
  (:generator 1
    (sc-case y
      (complex-double-reg
       (unless (location= x y)
         (let ((x-real (complex-double-reg-real-tn x))
               (y-real (complex-double-reg-real-tn y)))
           (inst funop :copy x-real y-real))
         (let ((x-imag (complex-double-reg-imag-tn x))
               (y-imag (complex-double-reg-imag-tn y)))
           (inst funop :copy x-imag y-imag))))
      (complex-double-stack
       (let ((offset (* (tn-offset y) n-word-bytes)))
         (let ((real-tn (complex-double-reg-real-tn x)))
           (str-float real-tn offset nfp))
         (let ((imag-tn (complex-double-reg-imag-tn x)))
           (str-float imag-tn (+ offset (* 2 n-word-bytes)) nfp)))))))
(define-move-vop move-complex-double-float-arg :move-arg
  (complex-double-reg descriptor-reg) (complex-double-reg))

(define-move-vop move-arg :move-arg
  (single-reg double-reg complex-single-reg complex-double-reg)
  (descriptor-reg))

;;;; stuff for c-call float-in-int-register arguments
(define-vop (move-to-single-int-reg)
  (:note "pointer to float-in-int coercion")
  (:args (x :scs (single-reg descriptor-reg)))
  (:results (y :scs (single-int-carg-reg) :load-if nil))
  (:generator 1
    (sc-case x
      (single-reg
        (inst funop :copy x y))
      (descriptor-reg
        (inst ldw (- (* single-float-value-slot n-word-bytes)
                     other-pointer-lowtag) x y)))))
(define-move-vop move-to-single-int-reg
  :move (single-reg descriptor-reg) (single-int-carg-reg))

(define-vop (move-single-int-reg)
  (:args (x :target y :scs (single-int-carg-reg) :load-if nil)
         (fp :scs (any-reg) :load-if (not (sc-is y single-int-carg-reg))))
  (:results (y :scs (single-int-carg-reg) :load-if nil))
  (:generator 1
    (unless (location= x y)
      (error "Huh? why did it do that?"))))
(define-move-vop move-single-int-reg :move-arg
  (single-int-carg-reg) (single-int-carg-reg))

; move contents of float register x to register y
(define-vop (move-to-double-int-reg)
  (:note "pointer to float-in-int coercion")
  (:args (x :scs (double-reg descriptor-reg)))
  (:results (y :scs (double-int-carg-reg) :load-if nil))
  (:temporary (:scs (signed-stack) :to (:result 0)) temp)
  (:temporary (:scs (signed-reg) :to (:result 0) :target y) old1)
  (:temporary (:scs (signed-reg) :to (:result 0) :target y) old2)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 2
    (sc-case x
      (double-reg
        (let* ((nfp (current-nfp-tn vop))
               (stack-tn (sc-case y
                           (double-stack y)
                           (double-int-carg-reg temp)))
               (offset (* (tn-offset stack-tn) n-word-bytes)))
          ;; save 8 bytes of stack to two register,
          ;; write down float in stack and load it back
          ;; into result register. Notice the result hack,
          ;; we are writing to one extra register.
          ;; Double float argument convention uses two registers,
          ;; but we only know about one (thanks to c-call).
          (inst ldw offset nfp old1)
          (inst ldw (+ offset n-word-bytes) nfp old2)
          (str-float x offset nfp) ; writes 8 bytes
          (inst ldw offset nfp y)
          (inst ldw (+ offset n-word-bytes) nfp
                (make-wired-tn (primitive-type-or-lose 'unsigned-byte-32)
                               (sc-number-or-lose 'unsigned-reg)
                               (+ 1 (tn-offset y))))
          (inst stw old1 offset nfp)
          (inst stw old2 (+ offset n-word-bytes) nfp)))
      (descriptor-reg
        (inst ldw (- (* double-float-value-slot n-word-bytes)
                     other-pointer-lowtag) x y)
        (inst ldw (- (* (1+ double-float-value-slot) n-word-bytes)
                     other-pointer-lowtag) x
                  (make-wired-tn (primitive-type-or-lose 'unsigned-byte-32)
                                 (sc-number-or-lose 'unsigned-reg)
                                 (+ 1 (tn-offset y))))))))
(define-move-vop move-to-double-int-reg
  :move (double-reg descriptor-reg) (double-int-carg-reg))

(define-vop (move-double-int-reg)
  (:args (x :target y :scs (double-int-carg-reg) :load-if nil)
         (fp :scs (any-reg) :load-if (not (sc-is y double-int-carg-reg))))
  (:results (y :scs (double-int-carg-reg) :load-if nil))
  (:generator 2
    (unless (location= x y)
      (error "Huh? why did it do that?"))))
(define-move-vop move-double-int-reg :move-arg
  (double-int-carg-reg) (double-int-carg-reg))

;;;; Arithmetic VOPs.

(define-vop (float-op)
  (:args (x) (y))
  (:results (r))
  (:variant-vars operation)
  (:policy :fast-safe)
  (:note "inline float arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 0
    (note-this-location vop :internal-error)
    (inst fbinop operation x y r)))

(macrolet ((frob (name sc zero-sc ptype)
             `(define-vop (,name float-op)
                (:args (x :scs (,sc ,zero-sc))
                       (y :scs (,sc ,zero-sc)))
                (:results (r :scs (,sc)))
                (:arg-types ,ptype ,ptype)
                (:result-types ,ptype))))
  (frob single-float-op single-reg fp-single-zero single-float)
  (frob double-float-op double-reg fp-double-zero double-float))

(macrolet ((frob (translate op sname scost dname dcost)
             `(progn
                (define-vop (,sname single-float-op)
                  (:translate ,translate)
                  (:variant ,op)
                  (:variant-cost ,scost))
                (define-vop (,dname double-float-op)
                  (:translate ,translate)
                  (:variant ,op)
                  (:variant-cost ,dcost)))))
  (frob + :add +/single-float 2 +/double-float 2)
  (frob - :sub -/single-float 2 -/double-float 2)
  (frob * :mpy */single-float 4 */double-float 5)
  (frob / :div //single-float 12 //double-float 19))

(macrolet ((frob (name translate sc type inst)
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
                  ,inst))))
  (frob abs/single-float abs single-reg single-float
    (inst funop :abs x y))
  (frob abs/double-float abs double-reg double-float
    (inst funop :abs x y)))

(macrolet ((frob (name translate sc type zero-tn)
             `(define-vop (,name)
                (:args (x :scs (,sc)))
                (:results (y :scs (,sc)))
                (:temporary (:scs (,sc)) float-temp)
                (:temporary (:scs (signed-reg)) reg-temp)
                (:temporary (:scs (signed-stack)) stack-temp)
                (:translate ,translate)
                (:policy :fast-safe)
                (:arg-types ,type)
                (:result-types ,type)
                (:note "inline float arithmetic")
                (:vop-var vop)
                (:save-p :compute-only)
                (:generator 1
                  (note-this-location vop :internal-error)
                  ;; KLUDGE: Subtracting the input from zero fails to
                  ;; produce negative zero from positive zero.
                  ;; Multiplying by -1 causes overflow conditions on
                  ;; some inputs.  The FNEG instruction is available
                  ;; in PA-RISC 2.0 only, and we're supposed to be
                  ;; PA-RISC 1.1 compatible.  To do the negation as an
                  ;; integer operation requires writing out the value
                  ;; (or its high bits) to memory, reading them up
                  ;; into a non-descriptor-reg, flipping the sign bit
                  ;; (most likely requiring another unsigned-reg to
                  ;; hold a constant to XOR with), then getting the
                  ;; result back to the FPU via memory again.  So
                  ;; instead we test for zeroness explicitly and
                  ;; decide which of the two FPU-based strategies to
                  ;; use.  I feel unclean for having implemented this,
                  ;; but it seems to be the least dreadful option.
                  ;; Help?  -- AB, 2015-11-26
                  (inst fcmp #b00111 x ,zero-tn)
                  (inst ftest)
                  (inst b SUBTRACT-FROM-ZERO :nullify t)

                  MULTIPLY-BY-NEGATIVE-ONE
                  (let ((nfp (current-nfp-tn vop))
                        (short-float-temp (make-random-tn :kind :normal
                                                          :sc (sc-or-lose 'single-reg)
                                                          :offset (tn-offset reg-temp))))
                    (inst li -1 reg-temp)
                    (storew reg-temp nfp (tn-offset stack-temp))
                    (ld-float (* (tn-offset stack-temp) n-word-bytes) nfp short-float-temp)
                    (inst fcnvxf short-float-temp float-temp)
                    (inst fbinop :mpy x float-temp y))
                  (inst b DONE :nullify t)

                  SUBTRACT-FROM-ZERO
                  (inst fbinop :sub ,zero-tn x y)

                  DONE))))
  (frob %negate/single-float %negate single-reg single-float fp-single-zero-tn)
  (frob %negate/double-float %negate double-reg double-float fp-double-zero-tn))

;;;; Comparison:

(define-vop (float-compare)
  (:args (x) (y))
  (:conditional)
  (:info target not-p)
  (:variant-vars condition complement)
  (:policy :fast-safe)
  (:note "inline float comparison")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 3
    (note-this-location vop :internal-error)
    ;; This is the condition to nullify the branch, so it is inverted.
    (inst fcmp (if not-p condition complement) x y)
    (inst ftest)
    (inst b target :nullify t)))

(macrolet ((frob (name sc zero-sc ptype)
             `(define-vop (,name float-compare)
                (:args (x :scs (,sc ,zero-sc))
                       (y :scs (,sc ,zero-sc)))
                (:arg-types ,ptype ,ptype))))
  (frob single-float-compare single-reg fp-single-zero single-float)
  (frob double-float-compare double-reg fp-double-zero double-float))

(macrolet ((frob (translate condition complement sname dname)
             `(progn
                (define-vop (,sname single-float-compare)
                  (:translate ,translate)
                  (:variant ,condition ,complement))
                (define-vop (,dname double-float-compare)
                  (:translate ,translate)
                  (:variant ,condition ,complement)))))
  ;; FIXME-lav: let 'inst cmp' translate keywords into raw binary instead of giving it here
  (frob < #b01001 #b10101 </single-float </double-float)
  (frob > #b10001 #b01101 >/single-float >/double-float)
  (frob = #b00101 #b11001 eql/single-float eql/double-float))


;;;; Conversion:

(macrolet ((frob (name translate from-sc from-type to-sc to-type)
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
                  (inst fcnvff x y)))))
  (frob %single-float/double-float %single-float
    double-reg double-float
    single-reg single-float)
  (frob %double-float/single-float %double-float
    single-reg single-float
    double-reg double-float))

; convert register-integer to registersingle/double by
; putting it on single-float-stack and then float-loading it into
; an float register, and finally convert the float-register and
; storing the result into y
(macrolet ((frob (name translate to-sc to-type)
             `(define-vop (,name)
                (:args (x :scs (signed-reg)
                          :load-if (not (sc-is x signed-stack))
                          :target stack-temp))
                (:arg-types signed-num)
                (:results (y :scs (,to-sc)))
                (:result-types ,to-type)
                (:policy :fast-safe)
                (:note "inline float coercion")
                (:translate ,translate)
                (:vop-var vop)
                (:save-p :compute-only)
                (:temporary (:scs (signed-stack) :from (:argument 0))
                            stack-temp)
                (:temporary (:scs (single-reg) :to (:result 0) :target y)
                            fp-temp)
                (:temporary (:scs (any-reg) :from (:argument 0)
                                  :to (:result 0)) index)
                (:generator 5
                  (let* ((nfp (current-nfp-tn vop))
                         (stack-tn
                          (sc-case x
                            (signed-stack
                             x)
                            (signed-reg
                             (storew x nfp (tn-offset stack-temp))
                             stack-temp)))
                         (offset (* (tn-offset stack-tn) n-word-bytes)))
                    (cond ((< offset (ash 1 4))
                           (inst flds offset nfp fp-temp))
                          ((and (< offset (ash 1 13))
                                (> offset 0))
                           (inst ldo offset zero-tn index)
                           (inst fldx index nfp fp-temp))
                          (t
                           (error "in vop ~s offset ~s is out-of-range" ',name offset)))
                    (note-this-location vop :internal-error)
                    (inst fcnvxf fp-temp y))))))
  (frob %single-float/signed %single-float
    single-reg single-float)
  (frob %double-float/signed %double-float
    double-reg double-float))

(macrolet ((frob (trans from-sc from-type inst note)
             `(define-vop (,(symbolicate trans "/" from-type))
                (:args (x :scs (,from-sc)
                          :target fp-temp))
                (:results (y :scs (signed-reg)
                             :load-if (not (sc-is y signed-stack))))
                (:arg-types ,from-type)
                (:result-types signed-num)
                (:translate ,trans)
                (:policy :fast-safe)
                (:note ,note)
                (:vop-var vop)
                (:save-p :compute-only)
                (:temporary (:scs (single-reg) :from (:argument 0)) fp-temp)
                (:temporary (:scs (signed-stack) :to (:result 0) :target y)
                            stack-temp)
                (:temporary (:scs (any-reg) :from (:argument 0)
                                  :to (:result 0)) index)
                (:generator 3
                  (let* ((nfp (current-nfp-tn vop))
                         (stack-tn
                          (sc-case y
                            (signed-stack y)
                            (signed-reg stack-temp)))
                         (offset (* (tn-offset stack-tn) n-word-bytes)))
                    (inst ,inst x fp-temp)
                    (cond ((< offset (ash 1 4))
                           (note-next-instruction vop :internal-error)
                           (inst fsts fp-temp offset nfp))
                          ((and (< offset (ash 1 13))
                                (> offset 0))
                           (inst ldo offset zero-tn index)
                           (note-next-instruction vop :internal-error)
                           (inst fstx fp-temp index nfp))
                          (t
                           (error "unary error, ldo offset too high")))
                    (unless (eq y stack-tn)
                      (loadw y nfp (tn-offset stack-tn))))))))
  (frob %unary-round single-reg single-float fcnvfx "inline float round")
  (frob %unary-round double-reg double-float fcnvfx "inline float round")
  (frob %unary-truncate/single-float single-reg single-float fcnvfxt
    "inline float truncate")
  (frob %unary-truncate/double-float double-reg double-float fcnvfxt
    "inline float truncate"))

(define-vop (make-single-float)
  (:args (bits :scs (signed-reg)
               :load-if (or (not (sc-is bits signed-stack))
                            (sc-is res single-stack))
               :target res))
  (:results (res :scs (single-reg)
                 :load-if (not (sc-is bits single-stack))))
  (:arg-types signed-num)
  (:result-types single-float)
  (:translate make-single-float)
  (:policy :fast-safe)
  (:vop-var vop)
  (:temporary (:scs (single-stack) :from (:argument 0) :to (:result 0)) temp)
  (:temporary (:scs (any-reg) :from (:argument 0) :to (:result 0)) index)
  (:generator 2
    (let ((nfp (current-nfp-tn vop)))
      (sc-case bits
        (signed-reg
         (sc-case res
           (single-reg
            (let ((offset (* (tn-offset temp) n-word-bytes)))
              (inst stw bits offset nfp)
              (cond ((< offset (ash 1 4))
                     (inst flds offset nfp res))
                    ((and (< offset (ash 1 13))
                          (> offset 0))
                     (inst ldo offset zero-tn index)
                     (inst fldx index nfp res))
                    (t
                     (error "make-single-float error, ldo offset too large")))))
           (single-stack
            (inst stw bits (* (tn-offset res) n-word-bytes) nfp))))
        (signed-stack
         (sc-case res
           (single-reg
            (let ((offset (* (tn-offset bits) n-word-bytes)))
              (cond ((< offset (ash 1 4))
                     (inst flds offset nfp res))
                    ((and (< offset (ash 1 13))
                          (> offset 0))
                     (inst ldo offset zero-tn index)
                     (inst fldx index nfp res))
                    (t
                     (error "make-single-float error, ldo offset too large")))))))))))

(define-vop (make-double-float)
  (:args (hi-bits :scs (signed-reg))
         (lo-bits :scs (unsigned-reg)))
  (:results (res :scs (double-reg)
                 :load-if (not (sc-is res double-stack))))
  (:arg-types signed-num unsigned-num)
  (:result-types double-float)
  (:translate make-double-float)
  (:policy :fast-safe)
  (:temporary (:scs (double-stack) :to (:result 0)) temp)
  (:temporary (:scs (any-reg) :from (:argument 0) :to (:result 0)) index)
  (:vop-var vop)
  (:generator 2
    (let* ((nfp (current-nfp-tn vop))
           (stack-tn (sc-case res
                       (double-stack res)
                       (double-reg temp)))
           (offset (* (tn-offset stack-tn) n-word-bytes)))
      (inst stw hi-bits offset nfp)
      (inst stw lo-bits (+ offset n-word-bytes) nfp)
      (cond ((eq stack-tn res))
            ((< offset (ash 1 4))
             (inst flds offset nfp res))
            ((and (< offset (ash 1 13))
                  (> offset 0))
             (inst ldo offset zero-tn index)
             (inst fldx index nfp res))
            (t
             (error "make-single-float error, ldo offset too large"))))))

(macrolet
  ((float-bits (name reg rreg stack rstack atype anum side offset)
   `(define-vop (,name)
    (:args (float :scs (,reg)
                  :load-if (not (sc-is float ,stack))))
    (:results (bits :scs (,rreg)
                    :load-if (or (not (sc-is bits ,rstack))
                                 (sc-is float ,stack))))
    (:arg-types ,atype)
    (:result-types ,anum)
    (:translate ,name)
    (:policy :fast-safe)
    (:vop-var vop)
    (:temporary (:scs (signed-stack) :from (:argument 0) :to (:result 0)) temp)
    (:temporary (:scs (any-reg) :from (:argument 0) :to (:result 0)) index)
    (:generator 2
      (let ((nfp (current-nfp-tn vop)))
        (sc-case float
          (,reg
           (sc-case bits
             (,rreg
              (let ((offset (* (tn-offset temp) n-word-bytes)))
                (cond ((< offset (ash 1 4))
                       ,@(if side
                           `((inst fsts float offset nfp :side ,side))
                           `((inst fsts float offset nfp))))
                      ((and (< offset (ash 1 13))
                            (> offset 0))
                       (inst ldo offset zero-tn index)
                       ,@(if side
                         `((inst fstx float index nfp :side ,side))
                         `((inst fstx float index nfp))))
                      (t
                       (error ,(format nil "~s,~s: inst-LDO offset too large"
                                       name rreg))))
                (inst ldw offset nfp bits)))
             (,rstack
              (let ((offset (* (tn-offset bits) n-word-bytes)))
                (cond ((< offset (ash 1 4))
                       ,@(if side
                         `((inst fsts float offset nfp :side ,side))
                         `((inst fsts float offset nfp))))
                      ((and (< offset (ash 1 13))
                            (> offset 0))
                       (inst ldo offset zero-tn index)
                       ,@(if side
                           `((inst fstx float index nfp :side ,side))
                           `((inst fstx float index nfp))))
                      (t
                       (error ,(format nil "~s,~s: inst-LDO offset too large"
                                       name rstack))))))))
          (,stack
           (sc-case bits
             (,rreg
              (inst ldw (* (+ (tn-offset float) ,offset) n-word-bytes)
                    nfp bits))))))))))
  (float-bits single-float-bits single-reg signed-reg single-stack
              signed-stack single-float signed-num nil 0)
  (float-bits double-float-high-bits double-reg signed-reg
              double-stack signed-stack double-float signed-num 0 0)
  (float-bits double-float-low-bits double-reg unsigned-reg
              double-stack unsigned-stack double-float unsigned-num 1 1))

;;;; Float mode hackery:

(sb!xc:deftype float-modes () '(unsigned-byte 32))
(defknown floating-point-modes () float-modes (flushable))
(defknown ((setf floating-point-modes)) (float-modes)
            float-modes)

(define-vop (floating-point-modes)
            (:results (res :scs (unsigned-reg)))
            (:result-types unsigned-num)
            (:translate floating-point-modes)
            (:policy :fast-safe)
            (:temporary (:scs (double-stack)) temp)
            (:temporary (:scs (any-reg) :to (:result 0)) index)
            (:vop-var vop)
  (:generator 3
              (let* ((nfp (current-nfp-tn vop))
                     (stack-tn (sc-case res
                                        (unsigned-stack res)
                                        (unsigned-reg temp)))
                     (offset (* (tn-offset stack-tn) n-word-bytes)))
                (cond ((< offset (ash 1 4))
                       (inst fsts fp-double-zero-tn offset nfp))
                      ((and (< offset (ash 1 13))
                            (> offset 0))
                       (inst ldo offset zero-tn index)
                       (inst fstx fp-double-zero-tn index nfp))
                      (t
                       (error "floating-point-modes error, ldo offset too large")))
                (ecase *backend-byte-order*
                  (:big-endian
                   (inst ldw offset nfp res))
                  (:little-endian
                   (inst ldw (+ offset 4) nfp res))))))

(define-vop (set-floating-point-modes)
            (:args (new :scs (unsigned-reg) :target res))
            (:results (res :scs (unsigned-reg)))
            (:arg-types unsigned-num)
            (:result-types unsigned-num)
            (:translate (setf floating-point-modes))
            (:policy :fast-safe)
            (:temporary (:scs (double-stack)) stack-tn)
            (:temporary (:scs (any-reg)) index)
            (:vop-var vop)
  (:generator 3
              (let* ((nfp (current-nfp-tn vop))
                     (offset (* (tn-offset stack-tn) n-word-bytes)))
                (ecase *backend-byte-order*
                  (:big-endian
                   (inst stw new offset nfp)
                   (inst stw zero-tn (+ offset 4) nfp))
                  (:little-endian
                   (inst stw zero-tn offset nfp)
                   (inst stw new (+ offset 4) nfp)))
                (cond ((< offset (ash 1 4))
                       (inst flds offset nfp fp-double-zero-tn))
                      ((and (< offset (ash 1 13))
                            (> offset 0))
                       (inst ldo offset zero-tn index)
                       (inst fldx index nfp fp-double-zero-tn))
                      (t
                       (error "set-floating-point-modes error, ldo offset too large")))
                (move new res))))

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
           (inst funop :copy real r-real)))
       (let ((r-imag (complex-single-reg-imag-tn r)))
         (unless (location= imag r-imag)
           (inst funop :copy imag r-imag))))
      (complex-single-stack
       (let ((nfp (current-nfp-tn vop))
             (offset (* (tn-offset r) n-word-bytes)))
         (str-float real offset nfp)
         (str-float imag (+ offset n-word-bytes) nfp))))))

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
           (inst funop :copy real r-real)))
       (let ((r-imag (complex-double-reg-imag-tn r)))
         (unless (location= imag r-imag)
           (inst funop :copy imag r-imag))))
      (complex-double-stack
       (let ((nfp (current-nfp-tn vop))
             (offset (* (tn-offset r) n-word-bytes)))
         (str-float real offset nfp)
         (str-float imag (+ offset (* 2 n-word-bytes)) nfp))))))

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
           (inst funop :copy value-tn r))))
      (complex-single-stack
       (ld-float (* (+ (ecase slot (:real 0) (:imag 1)) (tn-offset x))
                    n-word-bytes)
                 (current-nfp-tn vop) r)))))

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
           (inst funop :copy value-tn r))))
      (complex-double-stack
       (ld-float (* (+ (ecase slot (:real 0) (:imag 2)) (tn-offset x))
                    n-word-bytes)
                 (current-nfp-tn vop) r)))))

(define-vop (realpart/complex-double-float complex-double-float-value)
  (:translate realpart)
  (:note "complex double float realpart")
  (:variant :real))

(define-vop (imagpart/complex-double-float complex-double-float-value)
  (:translate imagpart)
  (:note "complex double float imagpart")
  (:variant :imag))
