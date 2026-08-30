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
  (sc-is tn single-sse-reg single-sse-stack double-sse-reg double-sse-stack))

#+sb-xc-host
(progn ; the host compiler will complain about absence of these
  (defun %simd-pack-low (x) (error "Called %SIMD-PACK-LOW ~S" x))
  (defun %simd-pack-high (x) (error "Called %SIMD-PACK-HIGH ~S" x)))

(define-move-fun (load-sse-immediate 1) (vop x y)
  ((fp-immediate) (int-sse-reg))
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
  ((fp-immediate)
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
(macrolet ((def ()
             `(progn
                ,@(loop for width in '(8 16 32 64 double single)
                        for step = (case width
                                     (double 8)
                                     (single 4)
                                     (t (/ width 8)))
                        append (loop for signed in (if (numberp width)
                                                       '(t nil)
                                                       '(nil))
                                     for name = (symbolicate '%simd-pack- (if signed 'signed- "")
                                                             'ref- width)
                                     for ref = (symbolicate (if signed 'signed- "") 'sap-ref- width)
                                     collect
                                     `(defun ,name (pack n)
                                        (declare (fixnum n))
                                        (with-pinned-objects (pack)
                                          (let ((sap (truly-the word (+ (- (get-lisp-obj-address pack) other-pointer-lowtag)
                                                                        (* simd-pack-lo-value-slot n-word-bytes)))))
                                            (,ref (int-sap sap) (truly-the fixnum (* n ,step)))))))))))
  (def))

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
(defun %simd-pack-doubles (pack)
  (declare (type simd-pack pack))
  (values (%simd-pack-ref-double pack 0)
          (%simd-pack-ref-double pack 1)))

(define-vop ()
  (:translate sap-ref-128)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg))
         (offset :scs (signed-reg immediate)))
  (:arg-types system-area-pointer signed-num)
  (:results (result :scs (int-sse-reg)))
  (:result-types simd-pack-ub64)
  (:temporary
   (:sc unsigned-reg :unused-if (not (offset-needs-temp offset)))
   temp)
  (:generator 3
    (inst movdqu result (sap+offset-to-ea sap offset temp))))

(define-vop (set-sap-ref-128)
  (:translate (setf sap-ref-128))
  (:policy :fast-safe)
  (:args (value :scs (int-sse-reg))
         (sap :scs (sap-reg))
         (offset :scs (signed-reg immediate)))
  (:arg-types simd-pack-ub64 system-area-pointer signed-num)
  (:temporary (:sc unsigned-reg) temp)
  (:generator 3
    (inst movdqu (sap+offset-to-ea sap offset temp) value)))

(defknown %simd-pack-int-to-double
    ((simd-pack (unsigned-byte 64))) (simd-pack double-float) (flushable))
(defknown %simd-pack-int-to-single
    ((simd-pack (unsigned-byte 64))) (simd-pack single-float) (flushable))

(define-vop ()
  (:translate %simd-pack-int-to-double)
  (:args (x :scs (int-sse-reg)))
  (:arg-types simd-pack-ub64)
  (:results (y :scs (double-sse-reg)))
  (:result-types simd-pack-double)
  (:policy :fast-safe)
  (:generator 2
    (move x y)))

(define-vop ()
  (:translate %simd-pack-int-to-single)
  (:args (x :scs (int-sse-reg)))
  (:arg-types simd-pack-ub64)
  (:results (y :scs (single-sse-reg)))
  (:result-types simd-pack-single)
  (:policy :fast-safe)
  (:generator 2
    (move x y)))
