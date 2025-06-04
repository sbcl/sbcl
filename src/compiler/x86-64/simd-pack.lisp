;;;; SSE intrinsics support for x86-64

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(defun ea-for-sse-stack (tn &optional (base rbp-tn))
  (ea (frame-byte-offset (1+ (tn-offset tn))) base))

(defun float-sse-p (tn)
  (sc-is tn single-sse-reg single-sse-stack single-sse-immediate
            double-sse-reg double-sse-stack double-sse-immediate))
(defun int-sse-p (tn)
  (sc-is tn int-sse-reg int-sse-stack int-sse-immediate))

#+sb-xc-host
(progn ; the host compiler will complain about absence of these
  (defun %simd-pack-low (x) (error "Called %SIMD-PACK-LOW ~S" x))
  (defun %simd-pack-high (x) (error "Called %SIMD-PACK-HIGH ~S" x)))

(define-move-fun (load-int-sse-immediate 1) (vop x y)
  ((int-sse-immediate) (int-sse-reg))
  (let* ((x  (tn-value x))
         (lo (%simd-pack-low x))
         (hi (%simd-pack-high x)))
    (cond ((= lo hi 0)
           (inst pxor y y))
          ((= lo hi (ldb (byte 64 0) -1))
           ;; don't think this is recognized as dependency breaking...
           (inst pcmpeqd y y))
          (t
           (inst movdqa y (register-inline-constant x))))))

(define-move-fun (load-float-sse-immediate 1) (vop x y)
  ((single-sse-immediate double-sse-immediate)
   (single-sse-reg double-sse-reg))
  (let* ((x  (tn-value x))
         (lo (%simd-pack-low x))
         (hi (%simd-pack-high x)))
    (cond ((= lo hi 0)
           (inst xorps y y))
          ((= lo hi (ldb (byte 64 0) -1))
           (inst pcmpeqd y y))
          (t
           (inst movaps y (register-inline-constant x))))))

(define-move-fun (load-int-sse 2) (vop x y)
  ((int-sse-stack) (int-sse-reg))
  (inst movdqu y (ea-for-sse-stack x)))

(define-move-fun (load-float-sse 2) (vop x y)
  ((single-sse-stack double-sse-stack) (single-sse-reg double-sse-reg))
  (inst movups y (ea-for-sse-stack x)))

(define-move-fun (store-int-sse 2) (vop x y)
  ((int-sse-reg) (int-sse-stack))
  (inst movdqu (ea-for-sse-stack y) x))

(define-move-fun (store-float-sse 2) (vop x y)
  ((double-sse-reg single-sse-reg) (double-sse-stack single-sse-stack))
  (inst movups (ea-for-sse-stack y) x))

(define-vop (sse-move)
  (:args (x :scs (single-sse-reg double-sse-reg int-sse-reg)
            :target y
            :load-if (not (location= x y))))
  (:results (y :scs (single-sse-reg double-sse-reg int-sse-reg)
               :load-if (not (location= x y))))
  (:note "SSE move")
  (:generator 0
     (move y x)))
(define-move-vop sse-move :move
  (int-sse-reg single-sse-reg double-sse-reg)
  (int-sse-reg single-sse-reg double-sse-reg))

(macrolet ((define-move-from-sse (type tag &rest scs)
             (let ((name (symbolicate "MOVE-FROM-SSE/" type)))
               `(progn
                  (define-allocator (,name)
                    (:args (x :scs ,scs))
                    (:results (y :scs (descriptor-reg)))
                    (:arg-types ,type)
                    (:note "AVX2 to pointer coercion")
                    (:generator 13
                      (alloc-other simd-pack-widetag simd-pack-size y)
                      (storew (fixnumize ,tag)
                              y simd-pack-tag-slot other-pointer-lowtag)
                      (let ((ea (object-slot-ea y simd-pack-lo-value-slot other-pointer-lowtag)))
                        (if (float-sse-p x)
                            (inst movaps ea x)
                            (inst movdqa ea x)))))
                  (define-move-vop ,name :move
                    ,scs (descriptor-reg))))))
  ;; see +simd-pack-element-types+
  (define-move-from-sse simd-pack-single 0 single-sse-reg)
  (define-move-from-sse simd-pack-double 1 double-sse-reg)
  (define-move-from-sse simd-pack-ub8 2 int-sse-reg)
  (define-move-from-sse simd-pack-ub16 3 int-sse-reg)
  (define-move-from-sse simd-pack-ub32 4 int-sse-reg)
  (define-move-from-sse simd-pack-ub64 5 int-sse-reg)
  (define-move-from-sse simd-pack-sb8 6 int-sse-reg)
  (define-move-from-sse simd-pack-sb16 7 int-sse-reg)
  (define-move-from-sse simd-pack-sb32 8 int-sse-reg)
  (define-move-from-sse simd-pack-sb64 9 int-sse-reg))

(define-vop (move-to-sse)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (int-sse-reg double-sse-reg single-sse-reg)))
  (:note "pointer to SSE coercion")
  (:generator 2
    (let ((ea (object-slot-ea x simd-pack-lo-value-slot other-pointer-lowtag)))
      (if (float-sse-p y)
          (inst movaps y ea)
          (inst movdqa y ea)))))
(define-move-vop move-to-sse :move
  (descriptor-reg)
  (int-sse-reg double-sse-reg single-sse-reg))

(define-vop (move-sse-arg)
  (:args (x :scs (int-sse-reg double-sse-reg single-sse-reg) :target y)
         (fp :scs (any-reg)
             :load-if (not (sc-is y int-sse-reg double-sse-reg single-sse-reg))))
  (:results (y))
  (:note "SSE argument move")
  (:generator 4
     (sc-case y
       ((int-sse-reg double-sse-reg single-sse-reg)
        (unless (location= x y)
          (if (or (float-sse-p x)
                  (float-sse-p y))
              (inst movaps y x)
              (inst movdqa y x))))
       ((int-sse-stack double-sse-stack single-sse-stack)
        (if (float-sse-p x)
            (inst movups (ea-for-sse-stack y fp) x)
            (inst movdqu (ea-for-sse-stack y fp) x))))))
(define-move-vop move-sse-arg :move-arg
  (int-sse-reg double-sse-reg single-sse-reg descriptor-reg)
  (int-sse-reg double-sse-reg single-sse-reg))

(define-move-vop move-arg :move-arg
  (int-sse-reg double-sse-reg single-sse-reg)
  (descriptor-reg))


(define-vop (%simd-pack-low)
  (:translate %simd-pack-low)
  (:args (x :scs (int-sse-reg double-sse-reg single-sse-reg)))
  (:arg-types simd-pack)
  (:results (dst :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 3
    (inst movq dst x)))

(define-vop (%simd-pack-high)
  (:translate %simd-pack-high)
  (:args (x :scs (int-sse-reg double-sse-reg single-sse-reg)
            :target tmp))
  (:arg-types simd-pack)
  (:temporary (:sc sse-reg :from (:argument 0)) tmp)
  (:results (dst :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 3
    (move tmp x)
    (inst psrldq tmp 8)
    (inst movq dst tmp)))
(define-vop (%simd-pack-high/sse4) ; 1 instruction and no temp
  (:translate %simd-pack-high)
  (:args (x :scs (int-sse-reg double-sse-reg single-sse-reg)))
  (:arg-types simd-pack)
  (:results (dst :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:guard (member :sse4 *backend-subfeatures*))
  (:generator 1
    (inst pextrq dst x 1)))

(define-allocator (%make-simd-pack)
  (:translate %make-simd-pack)
  (:policy :fast-safe)
  (:args (tag :scs (any-reg))
         (lo :scs (unsigned-reg))
         (hi :scs (unsigned-reg)))
  (:arg-types tagged-num unsigned-num unsigned-num)
  (:results (dst :scs (descriptor-reg) :from :load))
  (:result-types t)
  (:generator 13
    (alloc-other simd-pack-widetag simd-pack-size dst)
      ;; see +simd-pack-element-types+
    (storew tag dst simd-pack-tag-slot other-pointer-lowtag)
    (storew lo dst simd-pack-lo-value-slot other-pointer-lowtag)
    (storew hi dst simd-pack-hi-value-slot other-pointer-lowtag)))

(define-vop (%make-simd-pack-ub64)
  (:translate %make-simd-pack-ub64)
  (:policy :fast-safe)
  (:args (lo :scs (unsigned-reg))
         (hi :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:temporary (:sc int-sse-reg) tmp)
  (:results (dst :scs (int-sse-reg)))
  (:result-types simd-pack-ub64)
  (:generator 5
    (inst movq dst lo)
    (inst movq tmp hi)
    (inst punpcklqdq dst tmp)))

(defmacro simd-pack-dispatch (pack &body body)
  (check-type pack symbol)
  `(let ((,pack ,pack))
     (etypecase ,pack
       ,@(map 'list (lambda (eltype)
                   `((simd-pack ,eltype) ,@body))
          +simd-pack-element-types+))))

#-sb-xc-host
(macrolet ((unpack-unsigned (pack bits)
             `(simd-pack-dispatch ,pack
                (let ((lo (%simd-pack-low ,pack))
                      (hi (%simd-pack-high ,pack)))
                  (values
                   ,@(loop for pos by bits below 64 collect
                           `(unpack-unsigned-1 ,bits ,pos lo))
                   ,@(loop for pos by bits below 64 collect
                           `(unpack-unsigned-1 ,bits ,pos hi))))))
           (unpack-unsigned-1 (bits position ub64)
             `(ldb (byte ,bits ,position) ,ub64)))
  (declaim (inline %simd-pack-ub8s))
  (defun %simd-pack-ub8s (pack)
    (declare (type simd-pack pack))
    (unpack-unsigned pack 8))

  (declaim (inline %simd-pack-ub16s))
  (defun %simd-pack-ub16s (pack)
    (declare (type simd-pack pack))
    (unpack-unsigned pack 16))

  (declaim (inline %simd-pack-ub32s))
  (defun %simd-pack-ub32s (pack)
    (declare (type simd-pack pack))
    (unpack-unsigned pack 32))

  (declaim (inline %simd-pack-ub64s))
  (defun %simd-pack-ub64s (pack)
    (declare (type simd-pack pack))
    (unpack-unsigned pack 64)))

#-sb-xc-host
(macrolet ((unpack-signed (pack bits)
             `(simd-pack-dispatch ,pack
                (let ((lo (%simd-pack-low ,pack))
                      (hi (%simd-pack-high ,pack)))
                  (values
                   ,@(loop for pos by bits below 64 collect
                           `(unpack-signed-1 ,bits ,pos lo))
                   ,@(loop for pos by bits below 64 collect
                           `(unpack-signed-1 ,bits ,pos hi))))))
           (unpack-signed-1 (bits position ub64)
             `(- (mod (+ (ldb (byte ,bits ,position) ,ub64)
                         ,(expt 2 (1- bits)))
                      ,(expt 2 bits))
                 ,(expt 2 (1- bits)))))
  (declaim (inline %simd-pack-sb8s))
  (defun %simd-pack-sb8s (pack)
    (declare (type simd-pack pack))
    (unpack-signed pack 8))

  (declaim (inline %simd-pack-sb16s))
  (defun %simd-pack-sb16s (pack)
    (declare (type simd-pack pack))
    (unpack-signed pack 16))

  (declaim (inline %simd-pack-sb32s))
  (defun %simd-pack-sb32s (pack)
    (declare (type simd-pack pack))
    (unpack-signed pack 32))

  (declaim (inline %simd-pack-sb64s))
  (defun %simd-pack-sb64s (pack)
    (declare (type simd-pack pack))
    (unpack-signed pack 64)))

#-sb-xc-host
(progn
  (declaim (inline %make-simd-pack-ub32))
  (defun %make-simd-pack-ub32 (w x y z)
    (declare (type (unsigned-byte 32) w x y z))
    (%make-simd-pack
     #.(position '(unsigned-byte 32) +simd-pack-element-types+ :test #'equal)
     (logior w (ash x 32))
     (logior y (ash z 32)))))

(define-vop (%make-simd-pack-double)
  (:translate %make-simd-pack-double)
  (:policy :fast-safe)
  (:args (lo :scs (double-reg) :target dst)
         (hi :scs (double-reg) :target tmp))
  (:arg-types double-float double-float)
  (:temporary (:sc double-sse-reg :from (:argument 1)) tmp)
  (:results (dst :scs (double-sse-reg) :from (:argument 0)))
  (:result-types simd-pack-double)
  (:generator 5
    (move dst lo)
    (move tmp hi)
    (inst unpcklpd dst tmp)))

(define-vop (%make-simd-pack-single)
  (:translate %make-simd-pack-single)
  (:policy :fast-safe)
  (:args (x :scs (single-reg) :target dst)
         (y :scs (single-reg) :target tmp)
         (z :scs (single-reg))
         (w :scs (single-reg)))
  (:arg-types single-float single-float single-float single-float)
  (:temporary (:sc single-sse-reg :from (:argument 1)) tmp)
  (:results (dst :scs (single-sse-reg) :from (:argument 0)))
  (:result-types simd-pack-single)
  (:generator 5
    (move dst x)
    (inst unpcklps dst z)
    (move tmp y)
    (inst unpcklps tmp w)
    (inst unpcklps dst tmp)))

(defknown %simd-pack-single-item
  (simd-pack (integer 0 3)) single-float (flushable))

(define-vop (%simd-pack-single-item)
  (:args (x :scs (int-sse-reg double-sse-reg single-sse-reg)
            :target tmp))
  (:translate %simd-pack-single-item)
  (:arg-types simd-pack (:constant t))
  (:info index)
  (:results (dst :scs (single-reg)))
  (:result-types single-float)
  (:temporary (:sc single-sse-reg :from (:argument 0)) tmp)
  (:policy :fast-safe)
  (:generator 3
              (cond ((and (zerop index)
                          (not (location= x dst)))
                     (inst xorps dst dst)
                     (inst movss dst x))
                    (t
                     (move tmp x)
                     (when (plusp index)
                       (inst psrldq tmp (* 4 index)))
                     (inst xorps dst dst)
                     (inst movss dst tmp)))))

#-sb-xc-host
(progn
(declaim (inline %simd-pack-singles))
(defun %simd-pack-singles (pack)
  (declare (type simd-pack pack))
  (simd-pack-dispatch pack
    (values (%simd-pack-single-item pack 0)
            (%simd-pack-single-item pack 1)
            (%simd-pack-single-item pack 2)
            (%simd-pack-single-item pack 3)))))

(defknown %simd-pack-double-item
  (simd-pack (integer 0 1)) double-float (flushable))

(define-vop (%simd-pack-double-item)
  (:translate %simd-pack-double-item)
  (:args (x :scs (int-sse-reg double-sse-reg single-sse-reg)
            :target tmp))
  (:info index)
  (:arg-types simd-pack (:constant t))
  (:results (dst :scs (double-reg)))
  (:result-types double-float)
  (:temporary (:sc double-sse-reg :from (:argument 0)) tmp)
  (:policy :fast-safe)
  (:generator 3
    (cond ((and (zerop index)
                (not (location= x dst)))
           (inst xorpd dst dst)
           (inst movsd dst x))
          (t
           (move tmp x)
           (when (plusp index)
             (inst psrldq tmp (* 8 index)))
           (inst xorpd dst dst)
           (inst movsd dst tmp)))))

#-sb-xc-host
(progn
(declaim (inline %simd-pack-doubles))
(defun %simd-pack-doubles (pack)
  (declare (type simd-pack pack))
  (simd-pack-dispatch pack
    (values (%simd-pack-double-item pack 0)
            (%simd-pack-double-item pack 1)))))
