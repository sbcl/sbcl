;;;; floating point support for x86-64

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
             `(ea (- (* ,slot n-word-bytes) other-pointer-lowtag) ,tn)))
  (defun ea-for-df-desc (tn)
    (ea-for-xf-desc tn double-float-value-slot))
  ;; complex floats
  (defun ea-for-csf-data-desc (tn)
    (ea-for-xf-desc tn complex-single-float-data-slot))
  (defun ea-for-csf-real-desc (tn)
    (ea-for-xf-desc tn complex-single-float-data-slot))
  (defun ea-for-csf-imag-desc (tn)
    (ea-for-xf-desc tn (+ complex-single-float-data-slot 1/2)))

  (defun ea-for-cdf-data-desc (tn)
    (ea-for-xf-desc tn complex-double-float-real-slot))
  (defun ea-for-cdf-real-desc (tn)
    (ea-for-xf-desc tn complex-double-float-real-slot))
  (defun ea-for-cdf-imag-desc (tn)
    (ea-for-xf-desc tn complex-double-float-imag-slot)))

(macrolet ((ea-for-xf-stack (tn kind)
             (declare (ignore kind))
             `(ea (frame-byte-offset (tn-offset ,tn)) rbp-tn)))
  (defun ea-for-sf-stack (tn)
    (ea-for-xf-stack tn :single))
  (defun ea-for-df-stack (tn)
    (ea-for-xf-stack tn :double)))

;;; complex float stack EAs
(macrolet ((ea-for-cxf-stack (tn kind slot &optional base)
             `(ea (frame-byte-offset
                      (+ (tn-offset ,tn)
                       (cond ((= (tn-offset ,base) rsp-offset)
                              sp->fp-offset)
                             (t 0))
                       (ecase ,kind
                         (:single
                            (ecase ,slot
                              (:real 0)
                              (:imag -1/2)))
                         (:double
                            (ecase ,slot
                              (:real 1)
                              (:imag 0))))))
                  ,base)))
  (defun ea-for-csf-data-stack (tn &optional (base rbp-tn))
    (ea-for-cxf-stack tn :single :real base))
  (defun ea-for-csf-real-stack (tn &optional (base rbp-tn))
    (ea-for-cxf-stack tn :single :real base))
  (defun ea-for-csf-imag-stack (tn &optional (base rbp-tn))
    (ea-for-cxf-stack tn :single :imag base))

  (defun ea-for-cdf-data-stack (tn &optional (base rbp-tn))
    (ea-for-cxf-stack tn :double :real base))
  (defun ea-for-cdf-real-stack (tn &optional (base rbp-tn))
    (ea-for-cxf-stack tn :double :real base))
  (defun ea-for-cdf-imag-stack (tn &optional (base rbp-tn))
    (ea-for-cxf-stack tn :double :imag base)))

;;;; move functions

;;; X is source, Y is destination.

(define-move-fun (load-fp-zero 1) (vop x y)
  ((fp-single-zero) (single-reg)
   (fp-double-zero) (double-reg)
   (fp-complex-single-zero) (complex-single-reg)
   (fp-complex-double-zero) (complex-double-reg))
  (identity x)
  (sc-case y
    ((single-reg complex-single-reg) (inst xorps y y))
    ((double-reg complex-double-reg) (inst xorpd y y))))

(define-move-fun (load-fp-immediate 1) (vop x y)
  ((fp-single-immediate) (single-reg)
   (fp-double-immediate) (double-reg)
   (fp-complex-single-immediate) (complex-single-reg)
   (fp-complex-double-immediate) (complex-double-reg))
  (let ((x (register-inline-constant (tn-value x))))
    (sc-case y
      (single-reg (inst movss y x))
      (double-reg (inst movsd y x))
      (complex-single-reg (inst movq y x))
      (complex-double-reg (inst movapd y x)))))

(define-move-fun (load-single 2) (vop x y)
  ((single-stack) (single-reg))
  (inst movss y (ea-for-sf-stack x)))

(define-move-fun (store-single 2) (vop x y)
  ((single-reg) (single-stack))
  (inst movss (ea-for-sf-stack y) x))

(define-move-fun (load-double 2) (vop x y)
  ((double-stack) (double-reg))
  (inst movsd y (ea-for-df-stack x)))

(define-move-fun (store-double 2) (vop x y)
  ((double-reg) (double-stack))
  (inst movsd  (ea-for-df-stack y) x))


;;;; complex float move functions

;;; X is source, Y is destination.
(define-move-fun (load-complex-single 2) (vop x y)
  ((complex-single-stack) (complex-single-reg))
  (inst movq y (ea-for-csf-data-stack x)))

(define-move-fun (store-complex-single 2) (vop x y)
  ((complex-single-reg) (complex-single-stack))
  (inst movq (ea-for-csf-data-stack y) x))

(define-move-fun (load-complex-double 2) (vop x y)
  ((complex-double-stack) (complex-double-reg))
  (inst movupd y (ea-for-cdf-data-stack x)))

(define-move-fun (store-complex-double 2) (vop x y)
  ((complex-double-reg) (complex-double-stack))
  (inst movupd (ea-for-cdf-data-stack y) x))

;;;; move VOPs

;;; float register to register moves
(macrolet ((frob (vop sc)
             `(progn
                (define-vop (,vop)
                  (:args (x :scs (,sc)
                            :target y
                            :load-if (not (location= x y))))
                  (:results (y :scs (,sc)
                               :load-if (not (location= x y))))
                  (:note "float move")
                  (:generator 0
                    (move y x)))
                (define-move-vop ,vop :move (,sc) (,sc)))))
  (frob single-move single-reg)
  (frob double-move double-reg)
  (frob complex-single-move complex-single-reg)
  (frob complex-double-move complex-double-reg))


;;; Move from float to a descriptor reg. allocating a new float
;;; object in the process.
(define-vop (move-from-single)
  (:args (x :scs (single-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:note "float to pointer coercion")
  (:generator 4
    (inst movd y x)
    (inst shl y 32)
    (inst or :byte y single-float-widetag)))

(define-move-vop move-from-single :move
  (single-reg) (descriptor-reg))

(define-allocator (move-from-double)
  (:args (x :scs (double-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:note "float to pointer coercion")
  (:generator 13
     (alloc-other double-float-widetag double-float-size y)
     (inst movsd (ea-for-df-desc y) x)))
(define-move-vop move-from-double :move
  (double-reg) (descriptor-reg))

;;; Move from a descriptor to a float register.
(define-vop (move-to-single-reg)
   (:args (x :scs (descriptor-reg)
             :load-if (not (sc-is x control-stack))))
   (:results (y :scs (single-reg)))
   (:note "descriptor to float coercion")
   (:generator 2
     (sc-case x
       (descriptor-reg
        ;; After MOVQ, vector element 0 holds a single-float whose bits are
        ;; SINGLE-FLOAT-WIDETAG. That's fine, it's an ordinary denormal float.
        (inst movq y x)
        ;; Move bits [63:32] into [31:0] and move bits [127:96]
        ;; into the other 3 vector elements so that [63:32] is zeroed.
        (inst shufps y y #4r3331))
       (control-stack
        ;; Directly load high 4 bytes of descriptor from its stack address
        (inst movss y (ea (+ (frame-byte-offset (tn-offset x)) 4) rbp-tn))))))
(define-move-vop move-to-single-reg :move (descriptor-reg) (single-reg))

;;; Move from a descriptor to a float stack.
(define-vop (move-to-single-stack)
  (:args (x :scs (descriptor-reg) :target tmp))
  (:temporary (:sc unsigned-reg :from :argument :to :result) tmp)
  (:results (y :scs (single-stack)))
  (:note "pointer to float coercion")
  (:generator 2
    (move tmp x)
    (inst shr tmp 32)
    (inst mov :dword (ea (frame-byte-offset (tn-offset y)) rbp-tn) tmp)))
(define-move-vop move-to-single-stack :move (descriptor-reg) (single-stack))

(define-vop (move-to-double)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (double-reg)))
  (:note "pointer to float coercion")
  (:generator 2
    (inst movsd y (ea-for-df-desc x))))
(define-move-vop move-to-double :move (descriptor-reg) (double-reg))


;;; Move from complex float to a descriptor reg. allocating a new
;;; complex float object in the process.
(define-allocator (move-from-complex-single)
  (:args (x :scs (complex-single-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:note "complex float to pointer coercion")
  (:generator 13
     (alloc-other complex-single-float-widetag complex-single-float-size y)
     (inst movlps (ea-for-csf-data-desc y) x)))
(define-move-vop move-from-complex-single :move
  (complex-single-reg) (descriptor-reg))

(define-allocator (move-from-complex-double)
  (:args (x :scs (complex-double-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:note "complex float to pointer coercion")
  (:generator 13
     (alloc-other complex-double-float-widetag complex-double-float-size y)
     (inst movapd (ea-for-cdf-data-desc y) x)))
(define-move-vop move-from-complex-double :move
  (complex-double-reg) (descriptor-reg))

;;; Move from a descriptor to a complex float register.
(macrolet ((frob (name sc format)
             `(progn
                (define-vop (,name)
                  (:args (x :scs (descriptor-reg)))
                  (:results (y :scs (,sc)))
                  (:note "pointer to complex float coercion")
                  (:generator 2
                    ,(ecase format
                      (:single
                       ;; Use an integer move since there's no better choice.
                       ;; - movaps moves 128 bits of data, which is wrong.
                       ;; - movsd moves 64 bits as one double-float
                       ;; - movlps moves 64 bits, but doesn't zero the upper bits
                         '(inst movq y (ea-for-csf-data-desc x)))
                      (:double
                         '(inst movapd y (ea-for-cdf-data-desc x))))))
                (define-move-vop ,name :move (descriptor-reg) (,sc)))))
  (frob move-to-complex-single complex-single-reg :single)
  (frob move-to-complex-double complex-double-reg :double))

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
                  (:generator ,(case format (:single 2) (:double 3) )
                    (sc-case y
                      (,sc
                       (move y x))
                      (,stack-sc
                       (if (= (tn-offset fp) rsp-offset)
                           (let* ((offset (tn-byte-offset y))
                                  (ea (ea offset fp)))
                             ,@(ecase format
                                      (:single '((inst movss ea x)))
                                      (:double '((inst movsd ea x)))))
                           (let ((ea (ea (frame-byte-offset (tn-offset y)) fp)))
                             ,@(ecase format
                                 (:single '((inst movss ea x)))
                                 (:double '((inst movsd ea x))))))))))
                (define-move-vop ,name :move-arg
                  (,sc descriptor-reg) (,sc)))))
  (frob move-single-float-arg single-reg single-stack :single)
  (frob move-double-float-arg double-reg double-stack :double))

;;;; complex float MOVE-ARG VOP
(macrolet ((frob (name sc stack-sc format)
             `(progn
                (define-vop (,name)
                  (:args (x :scs (,sc) :target y)
                         (fp :scs (any-reg)
                             :load-if (not (sc-is y ,sc))))
                  (:results (y))
                  (:note "complex float argument move")
                  (:generator ,(ecase format (:single 2) (:double 3))
                    (sc-case y
                      (,sc
                       (move y x))
                      (,stack-sc
                       ,(ecase format
                          (:single
                             '(inst movq (ea-for-csf-data-stack y fp) x))
                          (:double
                             '(inst movupd (ea-for-cdf-data-stack y fp) x)))))))
                (define-move-vop ,name :move-arg
                  (,sc descriptor-reg) (,sc)))))
  (frob move-complex-single-float-arg
        complex-single-reg complex-single-stack :single)
  (frob move-complex-double-float-arg
        complex-double-reg complex-double-stack :double))

(define-move-vop move-arg :move-arg
  (single-reg double-reg
   complex-single-reg complex-double-reg)
  (descriptor-reg))


;;;; arithmetic VOPs

(define-vop (float-op)
  (:args (x) (y))
  (:results (r))
  (:policy :fast-safe)
  (:note "inline float arithmetic")
  (:vop-var vop)
  (:save-p :compute-only))

(macrolet ((frob (name comm-name sc constant-sc ptype)
             `(progn
                (define-vop (,name float-op)
                  (:args (x :scs (,sc ,constant-sc)
                            :target r
                            :load-if (not (sc-is x ,constant-sc)))
                         (y :scs (,sc ,constant-sc)
                            :load-if (not (sc-is y ,constant-sc))))
                  (:results (r :scs (,sc)))
                  (:arg-types ,ptype ,ptype)
                  (:result-types ,ptype))
                (define-vop (,comm-name float-op)
                  (:args (x :scs (,sc ,constant-sc)
                            :target r
                            :load-if (not (sc-is x ,constant-sc)))
                         (y :scs (,sc ,constant-sc)
                            :target r
                            :load-if (not (sc-is y ,constant-sc))))
                  (:results (r :scs (,sc)))
                  (:arg-types ,ptype ,ptype)
                  (:result-types ,ptype)))))
  (frob single-float-op single-float-comm-op
        single-reg fp-single-immediate single-float)
  (frob double-float-op double-float-comm-op
        double-reg fp-double-immediate double-float)
  (frob complex-single-float-op complex-single-float-comm-op
        complex-single-reg fp-complex-single-immediate
        complex-single-float)
  (frob complex-double-float-op complex-double-float-comm-op
        complex-double-reg fp-complex-double-immediate
        complex-double-float))

(defun note-float-location (op vop &rest args)
  (let ((*location-context*
          (list* op
                 (loop for arg in args
                       collect
                       (cond ((or (symbolp arg)
                                  (floatp arg)
                                  (complexp arg)) arg)
                             ((eq (tn-kind arg) :constant)
                              (tn-value arg))
                             (t
                              (make-sc+offset (sc-number (tn-sc arg))
                                              (or (tn-offset arg) 0))))))))
    (note-this-location vop :internal-error)))

(macrolet ((generate (op opinst commutative constant-sc load-inst)
             `(flet ((get-constant (tn &optional maybe-aligned)
                       (declare (ignorable maybe-aligned))
                       (let ((value (tn-value tn)))
                         ,(if (eq constant-sc 'fp-complex-single-immediate)
                              `(if maybe-aligned
                                   (register-inline-constant
                                    :aligned value)
                                   (register-inline-constant value))
                              `(register-inline-constant value))))
                     (note-location (x y)
                       (note-float-location ',op vop x y)))
                (declare (ignorable #'get-constant))
                (cond
                  ((location= x r)
                   (note-location x y)
                   (when (sc-is y ,constant-sc)
                     (setf y (get-constant y t)))
                   (inst ,opinst x y))
                  ((and ,commutative (location= y r))
                   (note-location y x)
                   (when (sc-is x ,constant-sc)
                     (setf x (get-constant x t)))
                   (inst ,opinst y x))
                  ((not (location= r y))
                   (if (sc-is x ,constant-sc)
                       (inst ,load-inst r (get-constant x))
                       (move r x))
                   (note-location r y)
                   (when (sc-is y ,constant-sc)
                     (setf y (get-constant y t)))
                   (inst ,opinst r y))
                  (t
                   (if (sc-is x ,constant-sc)
                       (inst ,load-inst tmp (get-constant x))
                       (move tmp x))
                   (note-location tmp y)
                   (inst ,opinst tmp y)
                   (move r tmp)))))
           (frob (op sinst sname scost dinst dname dcost commutative
                     &optional csinst csname cscost cdinst cdname cdcost)
             `(progn
                (define-vop (,sname ,(if commutative
                                         'single-float-comm-op
                                         'single-float-op))
                  (:translate ,op)
                  (:temporary (:sc single-reg) tmp)
                  (:vop-var vop)
                  (:generator ,scost
                    (generate ,op ,sinst ,commutative fp-single-immediate movss)))
                (define-vop (,dname ,(if commutative
                                         'double-float-comm-op
                                         'double-float-op))
                  (:translate ,op)
                  (:temporary (:sc double-reg) tmp)
                  (:vop-var vop)
                  (:generator ,dcost
                    (generate ,op ,dinst ,commutative fp-double-immediate movsd)))
                ,(when csinst
                   `(define-vop (,csname
                                 ,(if commutative
                                      'complex-single-float-comm-op
                                      'complex-single-float-op))
                      (:translate ,op)
                      (:temporary (:sc complex-single-reg) tmp)
                      (:vop-var vop)
                      (:generator ,cscost
                        (generate ,op ,csinst ,commutative
                                  fp-complex-single-immediate movq))))
                ,(when cdinst
                   `(define-vop (,cdname
                                 ,(if commutative
                                      'complex-double-float-comm-op
                                      'complex-double-float-op))
                      (:translate ,op)
                      (:temporary (:sc complex-double-reg) tmp)
                      (:vop-var vop)
                      (:generator ,cdcost
                        (generate ,op ,cdinst ,commutative
                                  fp-complex-double-immediate movapd)))))))
  (frob + addss +/single-float 2 addsd +/double-float 2 t
        addps +/complex-single-float 3 addpd +/complex-double-float 3)
  (frob - subss -/single-float 2 subsd -/double-float 2 nil
        subps -/complex-single-float 3 subpd -/complex-double-float 3)
  (frob * mulss */single-float 4 mulsd */double-float 5 t)
  (frob / divss //single-float 12 divsd //double-float 19 nil))

(macrolet ((frob (op cost commutativep
                     duplicate-inst op-inst real-move-inst complex-move-inst
                     real-sc real-constant-sc real-type
                     complex-sc complex-constant-sc complex-type
                     real-complex-name complex-real-name)
             (cond ((not duplicate-inst) ; simple case
                    `(flet ((load-into (r x)
                              (sc-case x
                                (,real-constant-sc
                                 (inst ,real-move-inst r
                                       (register-inline-constant (tn-value x))))
                                (,complex-constant-sc
                                 (inst ,complex-move-inst r
                                       (register-inline-constant (tn-value x))))
                                (t (move r x)))))
                       ,(when real-complex-name
                          `(define-vop (,real-complex-name float-op)
                             (:translate ,op)
                             (:args (x :scs (,real-sc ,real-constant-sc)
                                       :target r
                                       :load-if (not (sc-is x ,real-constant-sc)))
                                    (y :scs (,complex-sc ,complex-constant-sc)
                                       ,@(when commutativep '(:target r))
                                       :load-if (not (sc-is y ,complex-constant-sc))))
                             (:arg-types ,real-type ,complex-type)
                             (:results (r :scs (,complex-sc)
                                          ,@(unless commutativep '(:from (:argument 0)))))
                             (:result-types ,complex-type)
                             (:vop-var vop)
                             (:generator ,cost
                               ,(when commutativep
                                  `(when (location= y r)
                                     (rotatef x y)))
                               (load-into r x)
                               (note-float-location ',op vop r y)
                               (when (sc-is y ,real-constant-sc ,complex-constant-sc)
                                 (setf y (register-inline-constant
                                          :aligned (tn-value y))))
                               (inst ,op-inst r y))))

                       ,(when complex-real-name
                          `(define-vop (,complex-real-name float-op)
                             (:translate ,op)
                             (:args (x :scs (,complex-sc ,complex-constant-sc)
                                       :target r
                                       :load-if (not (sc-is x ,complex-constant-sc)))
                                    (y :scs (,real-sc ,real-constant-sc)
                                       ,@(when commutativep '(:target r))
                                       :load-if (not (sc-is y ,real-constant-sc))))
                             (:arg-types ,complex-type ,real-type)
                             (:results (r :scs (,complex-sc)
                                          ,@(unless commutativep '(:from (:argument 0)))))
                             (:result-types ,complex-type)
                             (:vop-var vop)
                             (:generator ,cost
                               ,(when commutativep
                                  `(when (location= y r)
                                     (rotatef x y)))
                               (load-into r x)
                               (note-float-location ',op vop r y)
                               (when (sc-is y ,real-constant-sc ,complex-constant-sc)
                                 (setf y (register-inline-constant
                                          :aligned (tn-value y))))
                               (inst ,op-inst r y))))))
                   (commutativep ; must duplicate, but commutative
                    `(progn
                       ,(when real-complex-name
                          `(define-vop (,real-complex-name float-op)
                             (:translate ,op)
                             (:args (x :scs (,real-sc ,real-constant-sc)
                                       :target dup
                                       :load-if (not (sc-is x ,real-constant-sc)))
                                    (y :scs (,complex-sc ,complex-constant-sc)
                                       :target r
                                       :to  :result
                                       :load-if (not (sc-is y ,complex-constant-sc))))
                             (:arg-types ,real-type ,complex-type)
                             (:temporary (:sc ,complex-sc :target r
                                          :from (:argument 0)
                                          :to   :result)
                                         dup)
                             (:results (r :scs (,complex-sc)))
                             (:result-types ,complex-type)
                             (:vop-var vop)
                             (:generator ,cost
                               (let (first-value
                                     (second-value r))
                                 (if (sc-is x ,real-constant-sc)
                                     (inst ,complex-move-inst dup
                                           (register-inline-constant
                                            (complex (setf first-value (tn-value x)) (tn-value x))))
                                     (let ((real x))
                                       (setf first-value x)
                                       ,duplicate-inst))
                                 ;; safe: dup /= y
                                 (when (location= dup r)
                                   (rotatef dup y)
                                   (setf second-value dup))
                                 (if (sc-is y ,complex-constant-sc)
                                     (inst ,complex-move-inst r
                                           (register-inline-constant (tn-value y)))
                                     (move r y))
                                 (note-float-location ',op vop first-value second-value)
                                 (when (sc-is dup ,complex-constant-sc)
                                   (setf dup (register-inline-constant
                                              :aligned (tn-value dup))))
                                 (inst ,op-inst r dup)))))

                       ,(when complex-real-name
                          `(define-vop (,complex-real-name float-op)
                             (:translate ,op)
                             (:args (x :scs (,complex-sc ,complex-constant-sc)
                                       :target r
                                       :to  :result
                                       :load-if (not (sc-is x ,complex-constant-sc)))
                                    (y :scs (,real-sc ,real-constant-sc)
                                       :target dup
                                       :load-if (not (sc-is y ,real-constant-sc))))
                             (:arg-types ,complex-type ,real-type)
                             (:temporary (:sc ,complex-sc :target r
                                          :from (:argument 1)
                                          :to :result)
                                         dup)
                             (:results (r :scs (,complex-sc)))
                             (:result-types ,complex-type)
                             (:vop-var vop)
                             (:generator ,cost
                               (let ((first-value r)
                                     second-value)
                                 (if (sc-is y ,real-constant-sc)
                                     (inst ,complex-move-inst dup
                                           (register-inline-constant
                                            (complex (setf second-value (tn-value y))
                                                     (tn-value y))))
                                     (let ((real y))
                                       (setf second-value y)
                                       ,duplicate-inst))
                                 (when (location= dup r)
                                   (rotatef x dup)
                                   (setf first-value dup))
                                 (if (sc-is x ,complex-constant-sc)
                                     (inst ,complex-move-inst r
                                           (register-inline-constant (tn-value x)))
                                     (move r x))
                                 (note-float-location ',op vop first-value second-value)
                                 (when (sc-is dup ,complex-constant-sc)
                                   (setf dup (register-inline-constant
                                              :aligned (tn-value dup))))
                                 (inst ,op-inst r dup)))))))
                   (t ; duplicate, not commutative
                    `(progn
                       ,(when real-complex-name
                          `(define-vop (,real-complex-name float-op)
                             (:translate ,op)
                             (:args (x :scs (,real-sc ,real-constant-sc)
                                       :target r
                                       :load-if (not (sc-is x ,real-constant-sc)))
                                    (y :scs (,complex-sc ,complex-constant-sc)
                                       :to :result
                                       :load-if (not (sc-is y ,complex-constant-sc))))
                             (:arg-types ,real-type ,complex-type)
                             (:results (r :scs (,complex-sc) :from (:argument 0)))
                             (:result-types ,complex-type)
                             (:vop-var vop)
                             (:generator ,cost
                               (if (sc-is x ,real-constant-sc)
                                   (inst ,complex-move-inst dup
                                         (register-inline-constant
                                          (complex (tn-value x) (tn-value x))))
                                   (let ((real x)
                                         (dup  r))
                                     ,duplicate-inst))
                               (note-float-location ',op vop r y)
                               (when (sc-is y ,complex-constant-sc)
                                 (setf y (register-inline-constant
                                          :aligned (tn-value y))))
                               (inst ,op-inst r y))))

                       ,(when complex-real-name
                          `(define-vop (,complex-real-name float-op)
                             (:translate ,op)
                             (:args (x :scs (,complex-sc)
                                       :target r
                                       :to :eval)
                                    (y :scs (,real-sc ,real-constant-sc)
                                       :target dup
                                       :load-if (not (sc-is y ,complex-constant-sc))))
                             (:arg-types ,complex-type ,real-type)
                             (:temporary (:sc ,complex-sc :from (:argument 1))
                                         dup)
                             (:results (r :scs (,complex-sc) :from :eval))
                             (:result-types ,complex-type)
                             (:vop-var vop)
                             (:generator ,cost
                               (let (second-value)
                                 (if (sc-is y ,real-constant-sc)
                                     (setf dup (register-inline-constant
                                                :aligned (complex (setf second-value (tn-value y))
                                                                  (tn-value y))))
                                     (let ((real y))
                                       (setf second-value y)
                                       ,duplicate-inst))
                                 (move r x)
                                 (note-float-location ',op vop r second-value)
                                 (inst ,op-inst r dup)))))))))
           (def-real-complex-op (op commutativep duplicatep
                                    single-inst single-real-complex-name single-complex-real-name single-cost
                                    double-inst double-real-complex-name double-complex-real-name double-cost)
               `(progn
                  (frob ,op ,single-cost ,commutativep
                        ,(and duplicatep
                              `(progn
                                 (move dup real)
                                 (inst unpcklps dup dup)))
                        ,single-inst movss movq
                        single-reg fp-single-immediate single-float
                        complex-single-reg fp-complex-single-immediate complex-single-float
                        ,single-real-complex-name ,single-complex-real-name)
                  (frob ,op ,double-cost ,commutativep
                        ,(and duplicatep
                              `(progn
                                 (move dup real)
                                 (inst unpcklpd dup dup)))
                        ,double-inst movsd movapd
                        double-reg fp-double-immediate double-float
                        complex-double-reg fp-complex-double-immediate complex-double-float
                        ,double-real-complex-name ,double-complex-real-name))))
  (def-real-complex-op + t nil
    addps +/real-complex-single-float +/complex-real-single-float 3
    addpd +/real-complex-double-float +/complex-real-double-float 4)
  (def-real-complex-op - nil nil
    subps -/real-complex-single-float -/complex-real-single-float 3
    subpd -/real-complex-double-float -/complex-real-double-float 4)
  (def-real-complex-op * t t
    mulps */real-complex-single-float */complex-real-single-float 4
    mulpd */real-complex-double-float */complex-real-double-float 5)
  (def-real-complex-op / nil t
    nil nil nil nil
    divpd nil //complex-real-double-float 19))

(define-vop (//complex-real-single-float float-op)
  (:translate /)
  (:args (x :scs (complex-single-reg fp-complex-single-immediate fp-complex-single-zero)
            :to (:result 0)
            :target r
            :load-if (not (sc-is x fp-complex-single-immediate fp-complex-single-zero)))
         (y :scs (single-reg fp-single-immediate fp-single-zero)
            :target dup
            :load-if (not (sc-is y fp-single-immediate fp-single-zero))))
  (:arg-types complex-single-float single-float)
  (:temporary (:sc complex-single-reg :from (:argument 1)) dup)
  (:results (r :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:vop-var vop)
  (:generator 12
    (let ((second-value dup)
          (first-value r))
      (flet ((duplicate (x)
               (let ((word (ldb (byte 64 0)
                                (logior (ash (single-float-bits (imagpart x)) 32)
                                        (ldb (byte 32 0)
                                             (single-float-bits (realpart x)))))))
                 (register-inline-constant :oword (logior (ash word 64) word)))))
        (sc-case y
          (fp-single-immediate
           (setf dup (duplicate (complex (setf second-value (tn-value y))
                                         (tn-value y)))))
          (fp-single-zero
           (inst xorps dup dup))
          (t (move dup y)
             (setf second-value y)
             (inst shufps dup dup #b00000000)))
        (sc-case x
          (fp-complex-single-immediate
           (inst movaps r (duplicate (setf first-value (tn-value x)))))
          (fp-complex-single-zero
           (inst xorps r r))
          (t
           (move r x)
           (setf first-value x)
           (inst unpcklpd r r)))
        (note-float-location '/ vop first-value second-value)
        (inst divps r dup)
        (inst movq r r)))))

;; Complex multiplication
;; r := rx * ry - ix * iy
;; i := rx * iy + ix * ry
;;
;; Transpose for SIMDness
;;  rx*ry    rx*iy
;; -ix*iy   +ix*ry
;;
;;  [rx rx] * [ry iy]
;;+ [ix ix] * [-iy ry]
;;       [r i]

(macrolet ((define-complex-* (name cost type sc tmp-p &body body)
               `(define-vop (,name float-op)
                  (:translate *)
                  (:args (x :scs (,sc) :target r)
                         (y :scs (,sc) :target copy-y))
                  (:arg-types ,type ,type)
                  (:temporary (:sc ,sc) imag)
                  (:temporary (:sc ,sc :from :eval) copy-y)
                  ,@(when tmp-p
                      `((:temporary (:sc ,sc) xmm)))
                  (:results (r :scs (,sc) :from :eval))
                  (:result-types ,type)
                  (:vop-var vop)
                  (:generator ,cost
                    (when (or (location= x copy-y)
                              (location= y r))
                      (rotatef x y))
                    ,@body))))
  (define-complex-* */complex-single-float 20
    complex-single-float complex-single-reg t
    (inst xorps xmm xmm)
    (move r x)
    (move copy-y y)  ; y == r only if y == x == r
    (setf y copy-y)

    (inst unpcklps r r)
    (move imag r)
    (inst unpckhpd imag xmm)
    (inst unpcklpd r    xmm)

    (note-float-location '* vop r y)
    (inst mulps r y)

    (inst shufps y y #b11110001)
    (inst xorps y (register-inline-constant :oword (ash 1 31)))

    (inst mulps imag y)
    (inst addps r imag))
  (define-complex-* */complex-double-float 25
    complex-double-float complex-double-reg nil
    (move imag x)
    (move r x)
    (move copy-y y)
    (setf y copy-y)
    (inst unpcklpd r r)
    (inst unpckhpd imag imag)

    (note-float-location '* vop r y)
    (inst mulpd r y)

    (inst shufpd y y #b01)
    (inst xorpd y (register-inline-constant :oword (ash 1 63)))
    (inst mulpd imag y)
    (inst addpd r imag)))

(define-vop (fsqrt)
  (:args (x :scs (double-reg)))
  (:results (y :scs (double-reg)))
  (:translate %sqrt)
  (:policy :fast-safe)
  (:arg-types double-float)
  (:result-types double-float)
  (:note "inline float arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 1
     (unless (location= x y)
       (inst xorpd y y))
     (note-float-location 'sqrt vop x)
     (inst sqrtsd y x)))

(macrolet ((frob ((name translate sc type) &body body)
             `(define-vop (,name)
                  (:args (x :scs (,sc) :target y))
                (:results (y :scs (,sc)))
                (:translate ,translate)
                (:policy :fast-safe)
                (:arg-types ,type)
                (:result-types ,type)
                (:note "inline float arithmetic")
                (:vop-var vop)
                (:save-p :compute-only)
                (:generator 1
                  (move y x)
                  (note-float-location ',translate vop y)
                  ,@body))))
  (frob (%negate/double-float %negate double-reg double-float)
        (inst xorpd y (register-inline-constant :oword (ash 1 63))))
  (frob (%negate/complex-double-float %negate complex-double-reg complex-double-float)
        (inst xorpd y (register-inline-constant
                       :oword (logior (ash 1 127) (ash 1 63)))))
  (frob (conjugate/complex-double-float conjugate complex-double-reg complex-double-float)
        (inst xorpd y (register-inline-constant :oword (ash 1 127))))
  (frob (%negate/single-float %negate single-reg single-float)
        (inst xorps y (register-inline-constant :oword (ash 1 31))))
  (frob (%negate/complex-single-float %negate complex-single-reg complex-single-float)
        (inst xorps y (register-inline-constant
                       :oword (logior (ash 1 31) (ash 1 63)))))
  (frob (conjugate/complex-single-float conjugate complex-single-reg complex-single-float)
        (inst xorpd y (register-inline-constant :oword (ash 1 63))))
  (frob (abs/double-float abs double-reg double-float)
        (inst andpd y (register-inline-constant :oword (ldb (byte 63 0) -1))))
  (frob (abs/single-float abs single-reg single-float)
        (inst andps y (register-inline-constant :oword (ldb (byte 31 0) -1)))))


;;;; comparison

(define-vop (float-compare)
  (:policy :fast-safe)
  (:vop-var vop)
  (:save-p :compute-only)
  (:note "inline float comparison"))

;;; EQL
(macrolet ((define-float-eql (name cost sc constant-sc type)
               `(define-vop (,name float-compare)
                  (:translate eql)
                  (:args (x :scs (,sc)
                            :target mask)
                         (y :scs (,sc ,constant-sc)
                            :target mask))
                  (:arg-types ,type ,type)
                  (:temporary (:sc ,sc :from :eval) mask)
                  (:temporary (:sc unsigned-reg) bits)
                  (:conditional :e)
                  (:generator ,cost
                    (when (location= y mask)
                      (rotatef x y))
                    (move mask x)
                    (when (sc-is y ,constant-sc)
                      (setf y (register-inline-constant :aligned (tn-value y))))
                    (inst pcmpeqd mask y)
                    (inst movmskps bits mask)
                    (inst cmp :byte bits #b1111)))))
  (define-float-eql eql/single-float 4
    single-reg fp-single-immediate single-float)
  (define-float-eql eql/double-float 4
    double-reg fp-double-immediate double-float)
  (define-float-eql eql/complex-single-float 5
    complex-single-reg fp-complex-single-immediate complex-single-float)
  (define-float-eql eql/complex-double-float 5
    complex-double-reg fp-complex-double-immediate complex-double-float))

(define-vop (generic-eq/single-float/c float-compare)
  (:translate eq)
  (:args (x :scs (any-reg descriptor-reg)))
  (:info y)
  (:arg-types * (:constant single-float))
  (:conditional :e)
  (:generator 3
    (inst cmp x (constantize (dpb (single-float-bits y) (byte 32 32)
                                  single-float-widetag)))))

;;; comiss and comisd can cope with one or other arg in memory: we
;;; could (should, indeed) extend these to cope with descriptor args
;;; and stack args

(define-vop (single-float-compare float-compare)
  (:args (x :scs (single-reg))
         (y :scs (single-reg single-stack fp-single-immediate)
            :load-if (not (sc-is y single-stack fp-single-immediate))))
  (:arg-types single-float single-float))
(define-vop (double-float-compare float-compare)
  (:args (x :scs (double-reg))
         (y :scs (double-reg double-stack descriptor-reg fp-double-immediate)
            :load-if (not (sc-is y double-stack descriptor-reg fp-double-immediate))))
  (:arg-types double-float double-float))

(define-vop (=/single-float single-float-compare)
  (:translate =)
  (:args (x :scs (single-reg single-stack fp-single-immediate)
            :target xmm
            :load-if (not (sc-is x single-stack fp-single-immediate)))
         (y :scs (single-reg single-stack fp-single-immediate)
            :target xmm
            :load-if (not (sc-is y single-stack fp-single-immediate))))
  (:temporary (:sc single-reg :from :eval) xmm)
  (:conditional not :p :ne)
  (:vop-var vop)
  (:generator 3
    (when (or (location= y xmm)
              (and (not (xmm-tn-p x)) (xmm-tn-p y)))
      (rotatef x y))
    (sc-case x
      (single-reg (setf xmm x))
      (single-stack (inst movss xmm (ea-for-sf-stack x)))
      (fp-single-immediate
       (inst movss xmm (register-inline-constant (tn-value x)))))
    (note-float-location '= vop xmm y)
    (sc-case y
      (single-stack
       (setf y (ea-for-sf-stack y)))
      (fp-single-immediate
       (setf y (register-inline-constant (tn-value y))))
      (t))
    (inst comiss xmm y)
    ;; if PF&CF, there was a NaN involved => not equal
    ;; otherwise, ZF => equal
    ))

(define-vop (=/double-float double-float-compare)
  (:translate =)
  (:args (x :scs (double-reg double-stack fp-double-immediate descriptor-reg)
            :target xmm
            :load-if (not (sc-is x double-stack fp-double-immediate descriptor-reg)))
         (y :scs (double-reg double-stack fp-double-immediate descriptor-reg)
            :target xmm
            :load-if (not (sc-is y double-stack fp-double-immediate descriptor-reg))))
  (:temporary (:sc double-reg :from :eval) xmm)
  (:conditional not :p :ne)
  (:vop-var vop)
  (:generator 3
    (when (or (location= y xmm)
              (and (not (xmm-tn-p x)) (xmm-tn-p y)))
      (rotatef x y))
    (sc-case x
      (double-reg
       (setf xmm x))
      (double-stack
       (inst movsd xmm (ea-for-df-stack x)))
      (fp-double-immediate
       (inst movsd xmm (register-inline-constant (tn-value x))))
      (descriptor-reg
       (inst movsd xmm (ea-for-df-desc x))))
    (note-float-location '= vop xmm y)
    (sc-case y
      (double-stack
       (setf y (ea-for-df-stack y)))
      (fp-double-immediate
       (setf y (register-inline-constant (tn-value y))))
      (descriptor-reg
       (setf y (ea-for-df-desc y)))
      (t))
    (inst comisd xmm y)))

(macrolet ((define-complex-float-= (complex-complex-name complex-real-name real-complex-name
                                    real-sc real-constant-sc real-type
                                    complex-sc complex-constant-sc complex-type
                                    real-move-inst complex-move-inst
                                    cmp-inst mask-inst mask)
               `(progn
                  (define-vop (,complex-complex-name float-compare)
                    (:translate =)
                    (:args (x :scs (,complex-sc ,complex-constant-sc)
                              :target cmp
                              :load-if (not (sc-is x ,complex-constant-sc)))
                           (y :scs (,complex-sc ,complex-constant-sc)
                              :target cmp
                              :load-if (not (sc-is y ,complex-constant-sc))))
                    (:arg-types ,complex-type ,complex-type)
                    (:temporary (:sc ,complex-sc :from :eval) cmp)
                    (:temporary (:sc unsigned-reg) bits)
                    (:info)
                    (:conditional :e)
                    (:generator 3
                      (when (location= y cmp)
                        (rotatef x y))
                      (sc-case x
                        (,real-constant-sc
                         (inst ,real-move-inst cmp (register-inline-constant
                                                    (tn-value x))))
                        (,complex-constant-sc
                         (inst ,complex-move-inst cmp (register-inline-constant
                                                       (tn-value x))))
                        (t
                         (move cmp x)))
                      (note-float-location '= vop cmp y)
                      (when (sc-is y ,real-constant-sc ,complex-constant-sc)
                        (setf y (register-inline-constant :aligned (tn-value y))))
                      (inst ,cmp-inst :eq cmp y)
                      (inst ,mask-inst bits cmp)
                      (inst cmp :byte bits ,mask)))
                  (define-vop (,complex-real-name ,complex-complex-name)
                    (:args (x :scs (,complex-sc ,complex-constant-sc)
                              :target cmp
                              :load-if (not (sc-is x ,complex-constant-sc)))
                           (y :scs (,real-sc ,real-constant-sc)
                              :target cmp
                              :load-if (not (sc-is y ,real-constant-sc))))
                    (:arg-types ,complex-type ,real-type))
                  (define-vop (,real-complex-name ,complex-complex-name)
                    (:args (x :scs (,real-sc ,real-constant-sc)
                              :target cmp
                              :load-if (not (sc-is x ,real-constant-sc)))
                           (y :scs (,complex-sc ,complex-constant-sc)
                              :target cmp
                              :load-if (not (sc-is y ,complex-constant-sc))))
                    (:arg-types ,real-type ,complex-type)))))
  (define-complex-float-= =/complex-single-float =/complex-real-single-float =/real-complex-single-float
    single-reg fp-single-immediate single-float
    complex-single-reg fp-complex-single-immediate complex-single-float
    movss movq cmpps movmskps #b1111)
  (define-complex-float-= =/complex-double-float =/complex-real-double-float =/real-complex-double-float
    double-reg fp-double-immediate double-float
    complex-double-reg fp-complex-double-immediate complex-double-float
    movsd movapd cmppd movmskpd #b11))

(macrolet ((define (op single-name double-name flags &optional flip)
             `(progn
                (define-vop (,double-name double-float-compare)
                  (:translate ,op)
                  (:info)
                  (:vop-var vop)
                  (:conditional ,@flags)
                  (:generator 3
                    (note-float-location ',op vop x y)
                    (sc-case y
                      (double-stack
                       (setf y (ea-for-df-stack y)))
                      (descriptor-reg
                       (setf y (ea-for-df-desc y)))
                      (fp-double-immediate
                       (setf y (register-inline-constant (tn-value y))))
                      ,(if flip
                           `(t
                             (change-vop-flags vop '(,flip))
                             (rotatef x y))
                           `(t)))
                    (inst comisd x y)))
                (define-vop (,single-name single-float-compare)
                  (:translate ,op)
                  (:info)
                  (:conditional ,@flags)
                  (:generator 3
                    (note-float-location ',op vop x y)
                    (sc-case y
                      (single-stack
                       (setf y (ea-for-sf-stack y)))
                      (fp-single-immediate
                       (setf y (register-inline-constant (tn-value y))))
                      ,(if flip
                           `(t
                             (change-vop-flags vop '(,flip))
                             (rotatef x y))
                           `(t)))

                    (inst comiss x y))))))
  ;;   UNORDERED:    ZF,PF,CF <- 111;
  ;;   GREATER_THAN: ZF,PF,CF <- 000;
  ;;   LESS_THAN:    ZF,PF,CF <- 001;
  ;;   EQUAL:        ZF,PF,CF <- 100;
  ;;   Using the flags that get a negated meaning by the unordered bits
  ;;   allows not checking for :P specifically.
  (define < <single-float <double-float (not :p :nc) :a)
  (define > >single-float >double-float (:a))
  (define <= <=single-float <=double-float (not :p :a) :nb)
  (define >= >=single-float >=double-float (:nb)))


;;;; conversion

(macrolet ((frob (name translate inst to-sc to-type)
             `(define-vop (,name)
                (:args (x :scs (signed-stack signed-reg)))
                (:results (y :scs (,to-sc)))
                (:arg-types signed-num)
                (:result-types ,to-type)
                (:policy :fast-safe)
                (:note "inline float coercion")
                (:translate ,translate)
                (:vop-var vop)
                (:save-p :compute-only)
                (:generator 5
                  (sc-case y
                    (single-reg (inst xorps y y))
                    (double-reg (inst xorpd y y)))
                  (note-float-location 'coerce vop x ',to-type)
                  (inst ,inst y x)))))
  (frob %single-float/signed %single-float cvtsi2ss single-reg single-float)
  (frob %double-float/signed %double-float cvtsi2sd double-reg double-float))

(macrolet ((frob (name translate inst from-scs from-type ea-func to-sc to-type)
             `(define-vop (,name)
               (:args (x :scs ,from-scs :target y))
               (:results (y :scs (,to-sc)))
               (:arg-types ,from-type)
               (:result-types ,to-type)
               (:policy :fast-safe)
               (:note "inline float coercion")
               (:translate ,translate)
               (:vop-var vop)
               (:save-p :compute-only)
               (:generator 2
                (unless (location= x y)
                  (sc-case y
                    (single-reg (inst xorps y y))
                    (double-reg (inst xorpd y y))))
                (note-float-location 'coerce vop x ',to-type)
                (inst ,inst y (sc-case x
                                (,(first from-scs) x)
                                (,(second from-scs) (,ea-func x))))
                ,(when (and (eq from-type 'double-float) ; if the input is wider
                            (eq to-type 'single-float))  ; than the output, clear
                   `(when (location= x y)                ; noise in the high part
                      (inst shufps y y #4r3330)))))))
  (frob %single-float/double-float %single-float cvtsd2ss
        (double-reg double-stack) double-float ea-for-df-stack
        single-reg single-float)

  (frob %double-float/single-float %double-float cvtss2sd
        (single-reg single-stack) single-float ea-for-sf-stack
        double-reg double-float))

(macrolet ((frob (trans op inst from-scs from-type ea-func)
             `(define-vop (,(symbolicate trans "/" from-type))
               (:args (x :scs ,from-scs))
               (:results (y :scs (signed-reg)))
               (:arg-types ,from-type)
               (:result-types signed-num)
               (:translate ,trans)
               (:policy :fast-safe)
               (:note "inline float truncate")
               (:vop-var vop)
               (:save-p :compute-only)
               (:generator 5
                 (note-float-location ',op vop x)
                 (inst ,inst y (sc-case x
                                 (,(first from-scs) x)
                                 (,(second from-scs) (,ea-func x))))))))
  (frob %unary-truncate/single-float truncate cvttss2si
        (single-reg single-stack) single-float ea-for-sf-stack)
  (frob %unary-truncate/double-float truncate cvttsd2si
        (double-reg double-stack) double-float ea-for-df-stack)

  (frob %unary-round round cvtss2si
        (single-reg single-stack) single-float ea-for-sf-stack)
  (frob %unary-round round cvtsd2si
        (double-reg double-stack) double-float ea-for-df-stack))

(define-vop (make-single-float)
  (:args (bits :scs (signed-reg) :target res
               :load-if (not (or (and (sc-is bits signed-stack)
                                      (sc-is res single-reg))
                                 (and (sc-is bits signed-stack)
                                      (sc-is res single-stack)
                                      (location= bits res))))))
  (:results (res :scs (single-reg single-stack)))
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
           (inst movd res bits))
          (signed-stack
           (inst movss res
                 (ea (frame-byte-offset (tn-offset bits)) rbp-tn))))))))

(define-vop (make-double-float)
  (:args (hi-bits :scs (signed-reg))
         (lo-bits :scs (unsigned-reg)))
  (:results (res :scs (double-reg)))
  (:temporary (:sc unsigned-reg) temp)
  (:arg-types signed-num unsigned-num)
  (:result-types double-float)
  (:translate make-double-float)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 4
    (move temp hi-bits)
    (inst shl temp 32)
    (inst or temp lo-bits)
    (inst movq res temp)))
(define-vop (make-double-float/sse4) ; 2 instructions and no temp
  (:args (hi-bits :scs (signed-reg))
         (lo-bits :scs (unsigned-reg)))
  (:results (res :scs (double-reg)))
  (:arg-types signed-num unsigned-num)
  (:result-types double-float)
  (:translate make-double-float)
  (:policy :fast-safe)
  (:vop-var vop)
  (:guard (member :sse4 *backend-subfeatures*))
  (:generator 2
    (inst movd res lo-bits)
    (inst pinsrd res hi-bits 1)))

(define-vop (%make-double-float)
  (:args (bits :scs (signed-reg)))
  (:results (res :scs (double-reg)))
  (:arg-types signed-num)
  (:result-types double-float)
  (:translate %make-double-float)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 4
    (inst movq res bits)))

(define-vop (single-float-bits)
  (:args (float :scs (single-reg descriptor-reg)
                :load-if (not (sc-is float single-stack))))
  (:results (bits :scs (signed-reg)))
  (:arg-types single-float)
  (:result-types signed-num)
  (:translate single-float-bits)
  (:policy :fast-safe)
  (:generator 4
     (sc-case float
       (single-reg
        (inst movd bits float)
        (inst movsx '(:dword :qword) bits bits))
       (single-stack ; c.f. ea-for-sf-stack
        (inst movsx '(:dword :qword)
              bits (ea (frame-byte-offset (tn-offset float)) rbp-tn)))
       (descriptor-reg
        (move bits float)
        (inst sar bits 32)))))

(define-vop (single-float-sign)
  (:translate single-float-sign)
  (:args (float :scs (descriptor-reg)))
  (:arg-types single-float)
  (:results (res :scs (descriptor-reg)))
  (:policy :fast-safe)
  (:generator 3
    (move res float)
    ;; preserve only the sign bit and widetag
    (inst and res (constantize (ash #x80000000 32)))
    ;; set the bits of corresponding to 1.0f0
    (inst or  res (constantize (logior (ash #x3F800000 32)
                                       single-float-widetag)))))
;;; Return Y with its sign changed to that of X.
;;; The arguments match FLOAT-SIGN which is the opposite of C's copysign().
(define-vop (single-float-copysign)
  (:translate single-float-copysign)
  (:args (x :scs (descriptor-reg))
         (y :scs (descriptor-reg)))
  (:arg-types single-float single-float)
  (:results (res :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg :from (:argument 0)) temp)
  (:policy :fast-safe)
  (:generator 3
    (move temp x)
    (move res y)
    (inst shl res 1)   ; discard result's sign bit
    (inst shl temp 1)  ; copy X's sign bit into the carry flag
    (inst rcr res 1))) ; rotate carry into the result

(define-vop (double-float-bits)
  (:args (float :scs (double-reg descriptor-reg)
                :load-if (not (sc-is float double-stack))))
  (:results (bits :scs (signed-reg)))
  (:arg-types double-float)
  (:result-types signed-num)
  (:translate double-float-bits)
  (:policy :fast-safe)
  (:generator 5
     (sc-case float
       (double-reg
        (inst movq bits float))
       (double-stack
        (inst mov bits (ea (frame-byte-offset (tn-offset float)) rbp-tn)))
       (descriptor-reg
        (loadw bits float double-float-value-slot other-pointer-lowtag)))))

(define-vop (double-float-high-bits)
  (:args (float :scs (double-reg descriptor-reg)
                :load-if (not (sc-is float double-stack))))
  (:results (hi-bits :scs (signed-reg)))
  (:arg-types double-float)
  (:result-types signed-num)
  (:translate double-float-high-bits)
  (:policy :fast-safe)
  (:generator 5
     (sc-case float
       (double-reg
        (inst movq hi-bits float)
        (inst sar hi-bits 32))
       (double-stack
        (inst movsx '(:dword :qword) hi-bits
              (ea (+ 4 (frame-byte-offset (tn-offset float))) rbp-tn)))
       (descriptor-reg
        (inst movsx '(:dword :qword) hi-bits
              (ea (+ 4 (ash double-float-value-slot word-shift)
                       (- other-pointer-lowtag))
                  float))))))

(define-vop (double-float-low-bits)
  (:args (float :scs (double-reg descriptor-reg)
                :load-if (not (sc-is float double-stack))))
  (:results (lo-bits :scs (unsigned-reg)))
  (:arg-types double-float)
  (:result-types unsigned-num)
  (:translate double-float-low-bits)
  (:policy :fast-safe)
  (:generator 5
     (sc-case float
        (double-reg
         ;; It might be more technically correct to use MOVQ followed by zeroing
         ;; the high 32 bits of the GPR with "MOV :dword lo-bits lo-bits",
         ;; but we know that MOVD moves exactly the right bits.
         (inst movd lo-bits float))
        (double-stack
         (inst mov :dword lo-bits
               (ea (frame-byte-offset (tn-offset float)) rbp-tn)))
        (descriptor-reg
         (inst mov :dword lo-bits
               (object-slot-ea float double-float-value-slot
                                        other-pointer-lowtag))))))


;;;; complex float VOPs

(define-vop (make-complex-single-float)
  (:translate complex)
  (:args (real :scs (single-reg fp-single-zero)
               :target r
               :load-if (not (sc-is real fp-single-zero)))
         (imag :scs (single-reg fp-single-zero)
               :load-if (not (sc-is imag fp-single-zero))))
  (:arg-types single-float single-float)
  (:results (r :scs (complex-single-reg) :from (:argument 0)))
  (:result-types complex-single-float)
  (:note "inline complex single-float creation")
  (:policy :fast-safe)
  (:generator 5
    (cond ((sc-is real fp-single-zero)
           (inst xorps r r)
           (unless (sc-is imag fp-single-zero)
             (inst unpcklps r imag)))
          ((location= real imag)
           (move r real)
           (inst unpcklps r r))
          (t
           (move r real)
           (unless (sc-is imag fp-single-zero)
             (inst unpcklps r imag))))))

(define-vop (make-complex-double-float)
  (:translate complex)
  (:args (real :scs (double-reg fp-double-zero)
               :target r
               :load-if (not (sc-is real fp-double-zero)))
         (imag :scs (double-reg fp-double-zero)
               :load-if (not (sc-is imag fp-double-zero))))
  (:arg-types double-float double-float)
  (:results (r :scs (complex-double-reg) :from (:argument 0)))
  (:result-types complex-double-float)
  (:note "inline complex double-float creation")
  (:policy :fast-safe)
  (:generator 5
    (cond ((sc-is real fp-double-zero)
           (inst xorpd r r)
           (unless (sc-is imag fp-double-zero)
             (inst unpcklpd r imag)))
          ((location= real imag)
           (move r real)
           (inst unpcklpd r r))
          (t
           (move r real)
           (unless (sc-is imag fp-double-zero)
             (inst unpcklpd r imag))))))

(define-vop (complex-float-value)
  (:args (x :target r))
  (:temporary (:sc complex-double-reg) zero)
  (:results (r))
  (:variant-vars offset)
  (:policy :fast-safe)
  (:generator 3
    (cond ((sc-is x complex-double-reg)
           (move r x)
           (inst xorpd zero zero)
           (ecase offset
             (0 (inst unpcklpd r zero))
             (1 (inst unpckhpd r zero))))
          ((sc-is x complex-single-reg)
           (move r x)
           (ecase offset
             (0 (inst shufps r r #b11111100))
             (1 (inst shufps r r #b11111101))))
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
             (inst movss r ea)))
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
             (inst movsd r ea)))
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

(defknown swap-complex ((complex float)) (complex float)
    (foldable flushable movable always-translatable))
(defoptimizer (swap-complex derive-type) ((x))
  (sb-c::lvar-type x))
(defun swap-complex (x)
  (complex (imagpart x) (realpart x)))
(define-vop (swap-complex-single-float)
  (:translate swap-complex)
  (:policy :fast-safe)
  (:args (x :scs (complex-single-reg) :target r))
  (:arg-types complex-single-float)
  (:results (r :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 2
     (move r x)
     (inst shufps r r #b11110001)))
(define-vop (swap-complex-double-float)
  (:translate swap-complex)
  (:policy :fast-safe)
  (:args (x :scs (complex-double-reg) :target r))
  (:arg-types complex-double-float)
  (:results (r :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 2
     (move r x)
     (inst shufpd r r #b01)))

#+round-float
(progn
  (define-vop ()
    (:translate round-double)
    (:policy :fast-safe)
    (:args (x :scs (double-reg) :target r))
    (:arg-types double-float (:constant symbol))
    (:info mode)
    (:results (r :scs (double-reg)))
    (:result-types double-float)
    (:generator 2
      (unless (location= r x)
        (inst xorpd r r))
      (inst roundsd r x
            (logior #b1000
                    (ecase mode
                      (:round 0)
                      (:floor 1)
                      (:ceiling 2)
                      (:truncate 3))))))

 (define-vop ()
   (:translate round-single)
   (:policy :fast-safe)
   (:args (x :scs (single-reg) :target r))
   (:arg-types single-float (:constant symbol))
   (:info mode)
   (:results (r :scs (single-reg)))
   (:result-types single-float)
   (:generator 2
     (unless (location= r x)
       (inst xorps r r))
     (inst roundss r x
           (logior #b1000
                   (ecase mode
                     (:round 0)
                     (:floor 1)
                     (:ceiling 2)
                     (:truncate 3)))))))
