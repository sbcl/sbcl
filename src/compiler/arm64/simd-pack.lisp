;;;; NEON intrinsics support for arm64

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

#+sb-xc-host
(progn ; the host compiler will complain about absence of these
  (defun %simd-pack-low (x) (error "Called %SIMD-PACK-LOW ~S" x))
  (defun %simd-pack-high (x) (error "Called %SIMD-PACK-HIGH ~S" x)))

(defun emit-movi-vector-imm (dst value)
  (aver (typep value '(unsigned-byte 64)))
  ;; We've got a few options here.
  ;; movi can produce 1, 2, or 4 byte constants with one of the values
  ;; filled in with an imm8. The other option is an 8 byte constant
  ;; where each byte is populated from a single bit in the source imm8.
  (let ((dwordp (= (ldb (byte 32 0) value) (ldb (byte 32 32) value)))
        (wordp (= (ldb (byte 16 0) value) (ldb (byte 16 16) value)
                  (ldb (byte 16 32) value) (ldb (byte 16 48) value)))
        (bytep (= (ldb (byte 8 0) value) (ldb (byte 8 8) value)
                  (ldb (byte 8 16) value) (ldb (byte 8 24) value)
                  (ldb (byte 8 32) value) (ldb (byte 8 40) value)
                  (ldb (byte 8 48) value) (ldb (byte 8 56) value))))
    (cond
      ((and dwordp
            (or (zerop (dpb 0 (byte 8 0) (ldb (byte 32 0) value)))
                (zerop (dpb 0 (byte 8 8) (ldb (byte 32 0) value)))
                (zerop (dpb 0 (byte 8 16) (ldb (byte 32 0) value)))
                (zerop (dpb 0 (byte 8 24) (ldb (byte 32 0) value)))))
       (inst movi dst value :4s))
      ((and wordp
            (or (zerop (dpb 0 (byte 8 0) (ldb (byte 16 0) value)))
                (zerop (dpb 0 (byte 8 8) (ldb (byte 16 0) value)))))
       (inst movi dst value :8h))
      (bytep
       (inst movi dst value :16b))
      ((and (member (ldb (byte 8 0) value) '(0 #xFF))
            (member (ldb (byte 8 8) value) '(0 #xFF))
            (member (ldb (byte 8 16) value) '(0 #xFF))
            (member (ldb (byte 8 24) value) '(0 #xFF))
            (member (ldb (byte 8 32) value) '(0 #xFF))
            (member (ldb (byte 8 40) value) '(0 #xFF))
            (member (ldb (byte 8 48) value) '(0 #xFF))
            (member (ldb (byte 8 56) value) '(0 #xFF)))
       (inst movi value :2d))
      ;; Can't do movi, try via the scalar regs.
      (wordp
       (inst movz tmp-tn (ldb (byte 16 0) value))
       (inst dup dst tmp-tn :8h))
      ((and dwordp
            (zerop (ldb (byte 16 16) value)))
       (inst movz tmp-tn (ldb (byte 16 0) value))
       (inst dup dst tmp-tn :4s))
      ((and dwordp
            (encode-logical-immediate (ldb (byte 32 0) value)))
       (inst orr tmp-tn zr-tn (ldb (byte 32 0) value))
       (inst dup dst tmp-tn :4s))
      (t nil))))

(define-move-fun (load-neon-immediate 1) (vop x y)
  ((fp-immediate)
   (int-neon-reg single-neon-reg double-neon-reg))
  (let* ((x  (tn-value x))
         (lo (%simd-pack-low x))
         (hi (%simd-pack-high x)))
    (or (and (= lo hi) (emit-movi-vector-imm y lo))
        (load-inline-constant y x))))

(define-move-fun (load-int-neon 2) (vop x y)
  ((int-neon-stack) (int-neon-reg))
  (loadw y (current-nfp-tn vop) (tn-offset x)))

(define-move-fun (load-float-neon 2) (vop x y)
  ((single-neon-stack double-neon-stack) (single-neon-reg double-neon-reg))
  (loadw y (current-nfp-tn vop) (tn-offset x)))

(define-move-fun (store-int-neon 2) (vop x y)
  ((int-neon-reg) (int-neon-stack))
  (storew x (current-nfp-tn vop) (tn-offset y)))

(define-move-fun (store-float-neon 2) (vop x y)
  ((double-neon-reg single-neon-reg) (double-neon-stack single-neon-stack))
  (storew x (current-nfp-tn vop) (tn-offset y)))

(define-vop (neon-move)
  (:args (x :scs (single-neon-reg double-neon-reg int-neon-reg)
            :target y
            :load-if (not (location= x y))))
  (:results (y :scs (single-neon-reg double-neon-reg int-neon-reg)
               :load-if (not (location= x y))))
  (:note "NEON move")
  (:generator 0
    (unless (location= y x)
      (inst mov y x :16b))))
(define-move-vop neon-move :move
  (int-neon-reg single-neon-reg double-neon-reg)
  (int-neon-reg single-neon-reg double-neon-reg))


(macrolet ((define-move-from-neon (type tag &rest scs)
             (let ((name (symbolicate "MOVE-FROM-NEON/" type)))
               `(progn
                  (define-vop (,name)
                    (:args (x :scs ,scs))
                    (:results (y :scs (descriptor-reg)))
                    (:arg-types ,type)
                    (:temporary (:scs (non-descriptor-reg) :offset lr-offset) lr)
                    (:temporary (:sc unsigned-reg) header)
                    (:note "NEON to pointer coercion")
                    (:generator 13
                      (with-fixed-allocation (y lr
                                                simd-pack-widetag
                                                simd-pack-size)
                        (inst mov header (fixnumize ,tag))
                        (storew header
                                y simd-pack-tag-slot other-pointer-lowtag)
                        (storew x y simd-pack-lo-value-slot other-pointer-lowtag))))
                  (define-move-vop ,name :move
                    ,scs (descriptor-reg))))))
  ;; see +simd-pack-element-types+
  (define-move-from-neon simd-pack-single 0 single-neon-reg)
  (define-move-from-neon simd-pack-double 1 double-neon-reg)
  (define-move-from-neon simd-pack-ub8 2 int-neon-reg)
  (define-move-from-neon simd-pack-ub16 3 int-neon-reg)
  (define-move-from-neon simd-pack-ub32 4 int-neon-reg)
  (define-move-from-neon simd-pack-ub64 5 int-neon-reg)
  (define-move-from-neon simd-pack-sb8 6 int-neon-reg)
  (define-move-from-neon simd-pack-sb16 7 int-neon-reg)
  (define-move-from-neon simd-pack-sb32 8 int-neon-reg)
  (define-move-from-neon simd-pack-sb64 9 int-neon-reg))

(define-vop (move-to-neon)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (int-neon-reg double-neon-reg single-neon-reg)))
  (:note "pointer to NEON coercion")
  (:generator 2
    (loadw y x simd-pack-lo-value-slot other-pointer-lowtag)))
(define-move-vop move-to-neon :move
  (descriptor-reg)
  (int-neon-reg double-neon-reg single-neon-reg))

(define-vop (move-neon-arg)
  (:args (x :scs (int-neon-reg double-neon-reg single-neon-reg) :target y)
         (fp :scs (any-reg)
             :load-if (not (sc-is y int-neon-reg double-neon-reg single-neon-reg))))
  (:results (y))
  (:note "NEON argument move")
  (:generator 4
     (sc-case y
       ((int-neon-reg double-neon-reg single-neon-reg)
        (unless (location= y x)
          (inst mov y x :16b)))
       ((int-neon-stack double-neon-stack single-neon-stack)
        (storew x fp (tn-offset y))))))
(define-move-vop move-neon-arg :move-arg
  (int-neon-reg double-neon-reg single-neon-reg descriptor-reg)
  (int-neon-reg double-neon-reg single-neon-reg))

(define-move-vop move-arg :move-arg
  (int-neon-reg double-neon-reg single-neon-reg)
  (descriptor-reg))


(define-vop (%simd-pack-low)
  (:translate %simd-pack-low)
  (:args (x :scs (int-neon-reg double-neon-reg single-neon-reg)))
  (:arg-types simd-pack)
  (:results (dst :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 3
    (inst umov dst x 0 :d)))

(define-vop (%simd-pack-high)
  (:translate %simd-pack-high)
  (:args (x :scs (int-neon-reg double-neon-reg single-neon-reg)))
  (:arg-types simd-pack)
  (:results (dst :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 3
    (inst umov dst x 1 :d)))

(define-vop (%make-simd-pack)
  (:translate %make-simd-pack)
  (:policy :fast-safe)
  (:args (tag :scs (any-reg))
         (lo :scs (unsigned-reg))
         (hi :scs (unsigned-reg)))
  (:arg-types tagged-num unsigned-num unsigned-num)
  (:temporary (:scs (non-descriptor-reg) :offset lr-offset) lr)
  (:results (dst :scs (descriptor-reg) :from :load))
  (:result-types t)
  (:generator 13
    (with-fixed-allocation (dst lr
                                simd-pack-widetag
                                simd-pack-size)
      ;; see +simd-pack-element-types+
      (storew tag dst simd-pack-tag-slot other-pointer-lowtag)
      (storew lo dst simd-pack-lo-value-slot other-pointer-lowtag)
      (storew hi dst simd-pack-hi-value-slot other-pointer-lowtag))))

(define-vop (%make-simd-pack-ub64)
  (:translate %make-simd-pack-ub64)
  (:policy :fast-safe)
  (:args (lo :scs (unsigned-reg))
         (hi :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (dst :scs (int-neon-reg)))
  (:result-types simd-pack-ub64)
  (:generator 5
    (inst ins dst 0 lo nil :d)
    (inst ins dst 1 hi nil :d)))

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
  (:args (lo :scs (double-reg))
         (hi :scs (double-reg)))
  (:arg-types double-float double-float)
  (:results (dst :scs (double-neon-reg)))
  (:result-types simd-pack-double)
  (:generator 5
    (inst ins dst 0 lo 0 :d)
    (inst ins dst 1 hi 0 :d)))

(define-vop (%make-simd-pack-single)
  (:translate %make-simd-pack-single)
  (:policy :fast-safe)
  (:args (x :scs (single-reg))
         (y :scs (single-reg))
         (z :scs (single-reg))
         (w :scs (single-reg)))
  (:arg-types single-float single-float single-float single-float)
  (:results (dst :scs (single-neon-reg)))
  (:result-types simd-pack-single)
  (:generator 5
    (inst ins dst 0 x 0 :s)
    (inst ins dst 1 y 0 :s)
    (inst ins dst 2 z 0 :s)
    (inst ins dst 3 w 0 :s)))

(defknown %simd-pack-single-item
  (simd-pack (integer 0 3)) single-float (flushable))

(define-vop (%simd-pack-single-item)
  (:args (x :scs (int-neon-reg double-neon-reg single-neon-reg)))
  (:translate %simd-pack-single-item)
  (:arg-types simd-pack (:constant t))
  (:info index)
  (:results (dst :scs (single-reg)))
  (:result-types single-float)
  (:temporary (:sc double-neon-reg :from (:argument 0)) tmp)
  (:policy :fast-safe)
  (:generator 3
    (unless (location= x tmp)
      (inst mov tmp x :16b))
    (inst movi dst 0 :4s)
    (inst ins dst 0 tmp index :s)))

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
  (:args (x :scs (int-neon-reg double-neon-reg single-neon-reg)
            :target tmp))
  (:info index)
  (:arg-types simd-pack (:constant t))
  (:results (dst :scs (double-reg)))
  (:result-types double-float)
  (:temporary (:sc double-neon-reg :from (:argument 0)) tmp)
  (:policy :fast-safe)
  (:generator 3
    (unless (location= x tmp)
      (inst mov tmp x :16b))
    (inst movi dst 0 :2d)
    (inst ins dst 0 tmp index :d)))

#-sb-xc-host
(progn
(declaim (inline %simd-pack-doubles))
(defun %simd-pack-doubles (pack)
  (declare (type simd-pack pack))
  (simd-pack-dispatch pack
    (values (%simd-pack-double-item pack 0)
            (%simd-pack-double-item pack 1)))))
