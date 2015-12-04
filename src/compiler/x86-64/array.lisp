;;;; array operations for the x86 VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")


;; For use in constant indexing; we can't use INDEX since the displacement
;; field of an EA can't contain 64 bit values.
(def!type low-index () '(signed-byte 29))

;;;; allocator for the array header

(define-vop (make-array-header)
  (:translate make-array-header)
  (:policy :fast-safe)
  (:args (type :scs (any-reg))
         (rank :scs (any-reg)))
  (:arg-types positive-fixnum positive-fixnum)
  (:temporary (:sc any-reg :to :eval) bytes)
  (:temporary (:sc any-reg :to :result) header)
  (:results (result :scs (descriptor-reg) :from :eval))
  (:node-var node)
  (:generator 13
    (inst lea bytes
          (make-ea :qword
                   :index rank :scale (ash 1 (- word-shift n-fixnum-tag-bits))
                   :disp (+ (* array-dimensions-offset n-word-bytes)
                            lowtag-mask)))
    (inst and bytes (lognot lowtag-mask))
    (inst lea header (make-ea :qword :base rank
                              :disp (fixnumize (1- array-dimensions-offset))))
    (inst shl header n-widetag-bits)
    (inst or  header type)
    (inst shr header n-fixnum-tag-bits)
    (pseudo-atomic
     (allocation result bytes node nil other-pointer-lowtag)
     (storew header result 0 other-pointer-lowtag))))

(define-vop (make-array-header/c)
  (:translate make-array-header)
  (:policy :fast-safe)
  (:arg-types (:constant t) (:constant t))
  (:info type rank)
  (:results (result :scs (descriptor-reg) :from :eval))
  (:node-var node)
  (:generator 12
    (let* ((header-size (+ rank
                           (1- array-dimensions-offset)))
           (bytes (logandc2 (+ (* (1+ header-size) n-word-bytes)
                               lowtag-mask)
                            lowtag-mask))
           (header (logior (ash header-size
                                n-widetag-bits)
                           type)))
     (pseudo-atomic
      (allocation result bytes node nil other-pointer-lowtag)
      (storew header result 0 other-pointer-lowtag)))))


;;;; additional accessors and setters for the array header
(define-full-reffer %array-dimension *
  array-dimensions-offset other-pointer-lowtag
  (any-reg) positive-fixnum %array-dimension)

(define-full-setter %set-array-dimension *
  array-dimensions-offset other-pointer-lowtag
  (any-reg) positive-fixnum %set-array-dimension)

(define-vop (array-rank-vop)
  (:translate %array-rank)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 3
    ;; An unaligned dword read not spanning a 16-byte boundary is as fast as
    ;; and shorter by 5 bytes than a qword read and right-shift by 8.
    (inst mov (reg-in-size res :dword)
          (make-ea :dword :base x :disp (1+ (- other-pointer-lowtag))))
    (inst sub (reg-in-size res :dword) (1- array-dimensions-offset))))

(define-vop (array-rank-vop=>fixnum)
  (:translate %array-rank)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 2
    (inst mov (reg-in-size res :dword)
          (make-ea :dword :base x :disp (1+ (- other-pointer-lowtag))))
    (inst lea (reg-in-size res :dword)
          (let ((scale (ash 1 n-fixnum-tag-bits)))
            ;; Compute [res*N-disp]. for N=2 use [res+res-disp]
            (make-ea :dword
                     :scale (if (= scale 2) 1 scale)
                     :index res
                     :base (if (= scale 2) res nil)
                     :disp (- (* scale (1- array-dimensions-offset))))))))

;;;; bounds checking routine

;;; Note that the immediate SC for the index argument is disabled
;;; because it is not possible to generate a valid error code SC for
;;; an immediate value.
;;;
;;; FIXME: As per the KLUDGE note explaining the :IGNORE-FAILURE-P
;;; flag in build-order.lisp-expr, compiling this file causes warnings
;;;    Argument FOO to VOP CHECK-BOUND has SC restriction
;;;    DESCRIPTOR-REG which is not allowed by the operand type:
;;;      (:OR POSITIVE-FIXNUM)
;;; CSR's message "format ~/ /" on sbcl-devel 2002-03-12 contained
;;; a possible patch, described as
;;;   Another patch is included more for information than anything --
;;;   removing the descriptor-reg SCs from the CHECK-BOUND vop in
;;;   x86/array.lisp seems to allow that file to compile without error[*],
;;;   and build; I haven't tested rebuilding capability, but I'd be
;;;   surprised if there were a problem.  I'm not certain that this is the
;;;   correct fix, though, as the restrictions on the arguments to the VOP
;;;   aren't the same as in the sparc and alpha ports, where, incidentally,
;;;   the corresponding file builds without error currently.
;;; Since neither of us (CSR or WHN) was quite sure that this is the
;;; right thing, I've just recorded the patch here in hopes it might
;;; help when someone attacks this problem again:
;;;   diff -u -r1.7 array.lisp
;;;   --- src/compiler/x86/array.lisp 11 Oct 2001 14:05:26 -0000      1.7
;;;   +++ src/compiler/x86/array.lisp 12 Mar 2002 12:23:37 -0000
;;;   @@ -76,10 +76,10 @@
;;;      (:translate %check-bound)
;;;      (:policy :fast-safe)
;;;      (:args (array :scs (descriptor-reg))
;;;   -        (bound :scs (any-reg descriptor-reg))
;;;   -        (index :scs (any-reg descriptor-reg #+nil immediate) :target result))
;;;   +        (bound :scs (any-reg))
;;;   +        (index :scs (any-reg #+nil immediate) :target result))
;;;      (:arg-types * positive-fixnum tagged-num)
;;;   -  (:results (result :scs (any-reg descriptor-reg)))
;;;   +  (:results (result :scs (any-reg)))
;;;      (:result-types positive-fixnum)
;;;      (:vop-var vop)
;;;      (:save-p :compute-only)
(define-vop (check-bound)
  (:translate %check-bound)
  (:policy :fast-safe)
  (:args (array :scs (descriptor-reg))
         (bound :scs (any-reg descriptor-reg))
         (index :scs (any-reg descriptor-reg)))
  ;(:arg-types * positive-fixnum *)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 5
    (let ((error (generate-error-code vop 'invalid-array-index-error
                                      array bound index))
          (index (if (sc-is index immediate)
                     (fixnumize (tn-value index))
                     index)))
      (unless (integerp index)
        (%test-fixnum index error t))
      (inst cmp bound index)
      ;; We use below-or-equal even though it's an unsigned test,
      ;; because negative indexes appear as large unsigned numbers.
      ;; Therefore, we get the <0 and >=bound test all rolled into one.
      (inst jmp :be error))))

;;;; accessors/setters

;;; variants built on top of WORD-INDEX-REF, etc. I.e., those vectors
;;; whose elements are represented in integer registers and are built
;;; out of 8, 16, or 32 bit elements.
(macrolet ((def-full-data-vector-frobs (type element-type &rest scs)
             `(progn
                (define-full-reffer+offset
                  ,(symbolicate "DATA-VECTOR-REF-WITH-OFFSET/" type)
                  ,type vector-data-offset other-pointer-lowtag ,scs
                  ,element-type data-vector-ref-with-offset)
                (define-full-setter+offset
                  ,(symbolicate "DATA-VECTOR-SET-WITH-OFFSET/" type)
                  ,type vector-data-offset other-pointer-lowtag ,scs
                  ,element-type data-vector-set-with-offset)))
           )
  (def-full-data-vector-frobs simple-vector * descriptor-reg any-reg)
  (def-full-data-vector-frobs simple-array-unsigned-byte-64 unsigned-num
    unsigned-reg)
  (def-full-data-vector-frobs simple-array-fixnum tagged-num any-reg)
  (def-full-data-vector-frobs simple-array-unsigned-fixnum
      positive-fixnum any-reg)
  (def-full-data-vector-frobs simple-array-signed-byte-64
      signed-num signed-reg)
  (def-full-data-vector-frobs simple-array-unsigned-byte-63 unsigned-num
    unsigned-reg))

(define-full-compare-and-swap %compare-and-swap-svref simple-vector
  vector-data-offset other-pointer-lowtag
  (descriptor-reg any-reg) *
  %compare-and-swap-svref)

;;;; integer vectors whose elements are smaller than a byte, i.e.,
;;;; bit, 2-bit, and 4-bit vectors

(define-vop (data-vector-ref-with-offset/simple-bit-vector-c)
  (:translate data-vector-ref-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:arg-types simple-bit-vector
              ;; this constant is possibly off by something
              ;; but (sbit n <huge-constant>) is unlikely to appear in code
              (:constant (integer 0 #x3ffffffff)) (:constant (integer 0 0)))
  (:info index offset)
  (:ignore offset)
  (:results (result :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 3
    ;; using 32-bit operand size might elide the REX prefix on mov + shift
    (multiple-value-bind (dword-index bit) (floor index 32)
      (inst mov (reg-in-size result :dword)
                (make-ea :dword :base object
                         :disp (+ (* dword-index 4)
                                  (- (* vector-data-offset n-word-bytes)
                                     other-pointer-lowtag))))
      (let ((right-shift (- bit n-fixnum-tag-bits)))
        (cond ((plusp right-shift)
               (inst shr (reg-in-size result :dword) right-shift))
              ((minusp right-shift) ; = left shift
               (inst shl (reg-in-size result :dword) (- right-shift))))))
    (inst and (reg-in-size result :dword) (fixnumize 1))))

(define-vop (data-vector-ref-with-offset/simple-bit-vector)
  (:translate data-vector-ref-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (unsigned-reg)))
  (:info offset)
  (:ignore offset)
  (:arg-types simple-bit-vector positive-fixnum (:constant (integer 0 0)))
  (:results (result :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 4
    (inst bt (make-ea :qword :base object
                      :disp (- (* vector-data-offset n-word-bytes)
                               other-pointer-lowtag))
             index)
    (inst sbb (reg-in-size result :dword) (reg-in-size result :dword))
    (inst and (reg-in-size result :dword) (fixnumize 1))))

(macrolet ((def-small-data-vector-frobs (type bits)
             (let* ((elements-per-word (floor n-word-bits bits))
                    (bit-shift (1- (integer-length elements-per-word))))
    `(progn
      ,@(unless (= bits 1)
       `((define-vop (,(symbolicate 'data-vector-ref-with-offset/ type))
         (:note "inline array access")
         (:translate data-vector-ref-with-offset)
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg))
                (index :scs (unsigned-reg)))
         (:info offset)
         (:arg-types ,type positive-fixnum (:constant (integer 0 0)))
         (:results (result :scs (unsigned-reg) :from (:argument 0)))
         (:result-types positive-fixnum)
         (:temporary (:sc unsigned-reg :offset ecx-offset) ecx)
         (:generator 20
           (aver (zerop offset))
           (move ecx index)
           (inst shr ecx ,bit-shift)
           (inst mov result
                 (make-ea :qword :base object :index ecx :scale n-word-bytes
                          :disp (- (* vector-data-offset n-word-bytes)
                                   other-pointer-lowtag)))
           (move ecx index)
           ;; We used to mask ECX for all values of BITS, but since
           ;; Intel's documentation says that the chip will mask shift
           ;; and rotate counts by 63 automatically, we can safely move
           ;; the masking operation under the protection of this UNLESS
           ;; in the bit-vector case.  --njf, 2006-07-14
           ,@(unless (= bits 1)
               `((inst and ecx ,(1- elements-per-word))
                 (inst shl ecx ,(1- (integer-length bits)))))
           (inst shr result :cl)
           (inst and result ,(1- (ash 1 bits)))))
       (define-vop (,(symbolicate 'data-vector-ref-with-offset/ type "-C"))
         (:translate data-vector-ref-with-offset)
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg)))
         (:arg-types ,type (:constant low-index) (:constant (integer 0 0)))
         (:info index offset)
         (:results (result :scs (unsigned-reg)))
         (:result-types positive-fixnum)
         (:generator 15
           (aver (zerop offset))
           (multiple-value-bind (word extra) (floor index ,elements-per-word)
             (loadw result object (+ word vector-data-offset)
                    other-pointer-lowtag)
             (unless (zerop extra)
               (inst shr result (* extra ,bits)))
             (unless (= extra ,(1- elements-per-word))
               (inst and result ,(1- (ash 1 bits)))))))))
       (define-vop (,(symbolicate 'data-vector-set-with-offset/ type))
         (:note "inline array store")
         (:translate data-vector-set-with-offset)
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg))
                (index :scs (unsigned-reg) :target ecx)
                (value :scs (unsigned-reg immediate) :target result))
         (:info offset)
         (:arg-types ,type positive-fixnum (:constant (integer 0 0))
                     positive-fixnum)
         (:results (result :scs (unsigned-reg)))
         (:result-types positive-fixnum)
         (:temporary (:sc unsigned-reg) word-index)
         (:temporary (:sc unsigned-reg) old)
         (:temporary (:sc unsigned-reg :offset ecx-offset) ecx)
         (:generator 25
           (aver (zerop offset))
           (move word-index index)
           (inst shr word-index ,bit-shift)
           (inst mov old
                 (make-ea :qword :base object :index word-index
                          :scale n-word-bytes
                          :disp (- (* vector-data-offset n-word-bytes)
                                   other-pointer-lowtag)))
           (move ecx index)
           ;; We used to mask ECX for all values of BITS, but since
           ;; Intel's documentation says that the chip will mask shift
           ;; and rotate counts by 63 automatically, we can safely move
           ;; the masking operation under the protection of this UNLESS
           ;; in the bit-vector case.  --njf, 2006-07-14
           ,@(unless (= bits 1)
               `((inst and ecx ,(1- elements-per-word))
                 (inst shl ecx ,(1- (integer-length bits)))))
           (inst ror old :cl)
           (unless (and (sc-is value immediate)
                        (= (tn-value value) ,(1- (ash 1 bits))))
             (inst and old ,(lognot (1- (ash 1 bits)))))
           (sc-case value
             (immediate
              (unless (zerop (tn-value value))
                (inst or old (logand (tn-value value) ,(1- (ash 1 bits))))))
             (unsigned-reg
              (inst or old value)))
           (inst rol old :cl)
           (inst mov (make-ea :qword :base object :index word-index
                              :scale n-word-bytes
                              :disp (- (* vector-data-offset n-word-bytes)
                                       other-pointer-lowtag))
                 old)
           (sc-case value
             (immediate
              (inst mov result (tn-value value)))
             (unsigned-reg
              (move result value)))))
       (define-vop (,(symbolicate 'data-vector-set-with-offset/ type "-C"))
         (:translate data-vector-set-with-offset)
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg))
                (value :scs (unsigned-reg immediate) :target result))
         (:arg-types ,type (:constant low-index)
                     (:constant (integer 0 0)) positive-fixnum)
         (:temporary (:sc unsigned-reg) mask-tn)
         (:info index offset)
         (:results (result :scs (unsigned-reg)))
         (:result-types positive-fixnum)
         (:temporary (:sc unsigned-reg :to (:result 0)) old)
         (:generator 20
           (aver (zerop offset))
           (multiple-value-bind (word extra) (floor index ,elements-per-word)
             (inst mov old
                   (make-ea :qword :base object
                            :disp (- (* (+ word vector-data-offset)
                                        n-word-bytes)
                                     other-pointer-lowtag)))
             (sc-case value
               (immediate
                (let* ((value (tn-value value))
                       (mask ,(1- (ash 1 bits)))
                       (shift (* extra ,bits)))
                  (unless (= value mask)
                    (inst mov mask-tn (ldb (byte 64 0)
                                           (lognot (ash mask shift))))
                    (inst and old mask-tn))
                  (unless (zerop value)
                    (inst mov mask-tn (ash value shift))
                    (inst or old mask-tn))))
               (unsigned-reg
                (let ((shift (* extra ,bits)))
                  (unless (zerop shift)
                    (inst ror old shift))
                  (inst mov mask-tn (lognot ,(1- (ash 1 bits))))
                  (inst and old mask-tn)
                  (inst or old value)
                  (unless (zerop shift)
                    (inst rol old shift)))))
             (inst mov (make-ea :qword :base object
                                :disp (- (* (+ word vector-data-offset)
                                            n-word-bytes)
                                         other-pointer-lowtag))
                   old)
             (sc-case value
               (immediate
                (inst mov result (tn-value value)))
               (unsigned-reg
                (move result value))))))))))
  (def-small-data-vector-frobs simple-bit-vector 1)
  (def-small-data-vector-frobs simple-array-unsigned-byte-2 2)
  (def-small-data-vector-frobs simple-array-unsigned-byte-4 4))
;;; And the float variants.

(defun make-ea-for-float-ref (object index offset element-size
                              &key (scale 1) (complex-offset 0))
  (let ((ea-size (if (= element-size 4) :dword :qword)))
    (etypecase index
      (integer
       (make-ea ea-size :base object
                :disp (- (+ (* vector-data-offset n-word-bytes)
                            (* (+ index offset) element-size)
                            complex-offset)
                         other-pointer-lowtag)))
      (tn
       (make-ea ea-size :base object :index index :scale scale
                :disp (- (+ (* vector-data-offset n-word-bytes)
                            (* offset element-size)
                            complex-offset)
                         other-pointer-lowtag))))))

#.
(let ((use-temp (<= word-shift n-fixnum-tag-bits)))
  `(define-vop (data-vector-ref-with-offset/simple-array-single-float)
     (:note "inline array access")
     (:translate data-vector-ref-with-offset)
     (:policy :fast-safe)
     (:args (object :scs (descriptor-reg))
            (index :scs (any-reg)))
     (:info offset)
     (:arg-types simple-array-single-float tagged-num
                 (:constant (constant-displacement other-pointer-lowtag
                                                   4 vector-data-offset)))
     ,@(when use-temp '((:temporary (:sc unsigned-reg) dword-index)))
     (:results (value :scs (single-reg)))
     (:result-types single-float)
     (:generator 5
      ,@(if use-temp
            '((move dword-index index)
              (inst shr dword-index (1+ (- n-fixnum-tag-bits word-shift)))
              (inst movss value (make-ea-for-float-ref object dword-index offset 4)))
            '((inst movss value (make-ea-for-float-ref object index offset 4
                                 :scale (ash 4 (- n-fixnum-tag-bits)))))))))

(define-vop (data-vector-ref-with-offset/simple-array-single-float-c)
  (:note "inline array access")
  (:translate data-vector-ref-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index offset)
  (:arg-types simple-array-single-float (:constant low-index)
              (:constant (constant-displacement other-pointer-lowtag
                                                4 vector-data-offset)))
  (:results (value :scs (single-reg)))
  (:result-types single-float)
  (:generator 4
   (inst movss value (make-ea-for-float-ref object index offset 4))))

#.
(let ((use-temp (<= word-shift n-fixnum-tag-bits)))
  `(define-vop (data-vector-set-with-offset/simple-array-single-float)
     (:note "inline array store")
     (:translate data-vector-set-with-offset)
     (:policy :fast-safe)
     (:args (object :scs (descriptor-reg))
            (index :scs (any-reg))
            (value :scs (single-reg) :target result))
     (:info offset)
     (:arg-types simple-array-single-float tagged-num
                 (:constant (constant-displacement other-pointer-lowtag
                                                   4 vector-data-offset))
                  single-float)
     ,@(when use-temp '((:temporary (:sc unsigned-reg) dword-index)))
     (:results (result :scs (single-reg)))
     (:result-types single-float)
     (:generator 5
      ,@(if use-temp
            '((move dword-index index)
              (inst shr dword-index (1+ (- n-fixnum-tag-bits word-shift)))
              (inst movss (make-ea-for-float-ref object dword-index offset 4) value))
            '((inst movss (make-ea-for-float-ref object index offset 4
                           :scale (ash 4 (- n-fixnum-tag-bits))) value)))
      (move result value))))

(define-vop (data-vector-set-with-offset/simple-array-single-float-c)
  (:note "inline array store")
  (:translate data-vector-set-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (value :scs (single-reg) :target result))
  (:info index offset)
  (:arg-types simple-array-single-float (:constant low-index)
              (:constant (constant-displacement other-pointer-lowtag
                                                4 vector-data-offset))
              single-float)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:generator 4
   (inst movss (make-ea-for-float-ref object index offset 4) value)
   (move result value)))

(define-vop (data-vector-ref-with-offset/simple-array-double-float)
  (:note "inline array access")
  (:translate data-vector-ref-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:info offset)
  (:arg-types simple-array-double-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag
                                                8 vector-data-offset)))
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:generator 7
   (inst movsd value (make-ea-for-float-ref object index offset 8
                                            :scale (ash 1 (- word-shift n-fixnum-tag-bits))))))

(define-vop (data-vector-ref-c/simple-array-double-float)
  (:note "inline array access")
  (:translate data-vector-ref-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index offset)
  (:arg-types simple-array-double-float (:constant low-index)
              (:constant (constant-displacement other-pointer-lowtag
                                                8 vector-data-offset)))
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:generator 6
   (inst movsd value (make-ea-for-float-ref object index offset 8))))

(define-vop (data-vector-set-with-offset/simple-array-double-float)
  (:note "inline array store")
  (:translate data-vector-set-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (double-reg) :target result))
  (:info offset)
  (:arg-types simple-array-double-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag
                                                8 vector-data-offset))
              double-float)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:generator 20
   (inst movsd (make-ea-for-float-ref object index offset 8
                                      :scale (ash 1 (- word-shift n-fixnum-tag-bits)))
         value)
   (move result value)))

(define-vop (data-vector-set-with-offset/simple-array-double-float-c)
  (:note "inline array store")
  (:translate data-vector-set-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (value :scs (double-reg) :target result))
  (:info index offset)
  (:arg-types simple-array-double-float (:constant low-index)
              (:constant (constant-displacement other-pointer-lowtag
                                                8 vector-data-offset))
              double-float)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:generator 19
   (inst movsd (make-ea-for-float-ref object index offset 8) value)
   (move result value)))


;;; complex float variants

(define-vop (data-vector-ref-with-offset/simple-array-complex-single-float)
  (:note "inline array access")
  (:translate data-vector-ref-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:info offset)
  (:arg-types simple-array-complex-single-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag
                                                8 vector-data-offset)))
  (:results (value :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 5
    (inst movq value (make-ea-for-float-ref object index offset 8
                                            :scale (ash 1 (- word-shift n-fixnum-tag-bits))))))

(define-vop (data-vector-ref-with-offset/simple-array-complex-single-float-c)
  (:note "inline array access")
  (:translate data-vector-ref-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index offset)
  (:arg-types simple-array-complex-single-float (:constant low-index)
              (:constant (constant-displacement other-pointer-lowtag
                                                8 vector-data-offset)))
  (:results (value :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 4
    (inst movq value (make-ea-for-float-ref object index offset 8))))

(define-vop (data-vector-set-with-offset/simple-array-complex-single-float)
  (:note "inline array store")
  (:translate data-vector-set-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (complex-single-reg) :target result))
  (:info offset)
  (:arg-types simple-array-complex-single-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag
                                                8 vector-data-offset))
              complex-single-float)
  (:results (result :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 5
    (move result value)
    (inst movq (make-ea-for-float-ref object index offset 8
                                      :scale (ash 1 (- word-shift n-fixnum-tag-bits)))
          value)))

(define-vop (data-vector-set-with-offset/simple-array-complex-single-float-c)
  (:note "inline array store")
  (:translate data-vector-set-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (value :scs (complex-single-reg) :target result))
  (:info index offset)
  (:arg-types simple-array-complex-single-float (:constant low-index)
              (:constant (constant-displacement other-pointer-lowtag
                                                8 vector-data-offset))
              complex-single-float)
  (:results (result :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 4
    (move result value)
    (inst movq (make-ea-for-float-ref object index offset 8) value)))

(define-vop (data-vector-ref-with-offset/simple-array-complex-double-float)
  (:note "inline array access")
  (:translate data-vector-ref-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:info offset)
  (:arg-types simple-array-complex-double-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag
                                                16 vector-data-offset)))
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 7
    (inst movapd value (make-ea-for-float-ref object index offset 16
                                              :scale (ash 2 (- word-shift n-fixnum-tag-bits))))))

(define-vop (data-vector-ref-with-offset/simple-array-complex-double-float-c)
  (:note "inline array access")
  (:translate data-vector-ref-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index offset)
  (:arg-types simple-array-complex-double-float (:constant low-index)
              (:constant (constant-displacement other-pointer-lowtag
                                                16 vector-data-offset)))
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 6
    (inst movapd value (make-ea-for-float-ref object index offset 16))))

(define-vop (data-vector-set-with-offset/simple-array-complex-double-float)
  (:note "inline array store")
  (:translate data-vector-set-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (complex-double-reg) :target result))
  (:info offset)
  (:arg-types simple-array-complex-double-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag
                                                16 vector-data-offset))
              complex-double-float)
  (:results (result :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 20
    (inst movapd (make-ea-for-float-ref object index offset 16
                                        :scale (ash 2 (- word-shift n-fixnum-tag-bits)))
          value)
    (move result value)))

(define-vop (data-vector-set-with-offset/simple-array-complex-double-float-c)
  (:note "inline array store")
  (:translate data-vector-set-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (value :scs (complex-double-reg) :target result))
  (:info index offset)
  (:arg-types simple-array-complex-double-float (:constant low-index)
              (:constant (constant-displacement other-pointer-lowtag
                                                16 vector-data-offset))
              complex-double-float)
  (:results (result :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 19
    (inst movapd (make-ea-for-float-ref object index offset 16) value)
    (move result value)))



;;; {un,}signed-byte-{8,16,32} and characters
(macrolet ((define-data-vector-frobs (ptype mov-inst operand-size
                                            type &rest scs)
  (let ((n-bytes (ecase operand-size
                   (:byte 1)
                   (:word 2)
                   (:dword 4))))
    (multiple-value-bind (index-sc scale)
        (if (>= n-bytes (ash 1 n-fixnum-tag-bits))
            (values 'any-reg (ash n-bytes (- n-fixnum-tag-bits)))
            (values 'signed-reg n-bytes))
      `(progn
         (define-vop (,(symbolicate "DATA-VECTOR-REF-WITH-OFFSET/" ptype))
           (:translate data-vector-ref-with-offset)
           (:policy :fast-safe)
           (:args (object :scs (descriptor-reg))
                  (index :scs (,index-sc)))
           (:info offset)
           (:arg-types ,ptype tagged-num
                       (:constant (constant-displacement other-pointer-lowtag
                                                         ,n-bytes vector-data-offset)))
           (:results (value :scs ,scs))
           (:result-types ,type)
           (:generator 5
                       (inst ,mov-inst value
                             (make-ea ,operand-size :base object :index index :scale ,scale
                                      :disp (- (+ (* vector-data-offset n-word-bytes)
                                                  (* offset ,n-bytes))
                                               other-pointer-lowtag)))))
         (define-vop (,(symbolicate "DATA-VECTOR-REF-WITH-OFFSET/" ptype "-C"))
           (:translate data-vector-ref-with-offset)
           (:policy :fast-safe)
           (:args (object :scs (descriptor-reg)))
           (:info index offset)
           (:arg-types ,ptype (:constant low-index)
                       (:constant (constant-displacement other-pointer-lowtag
                                                         ,n-bytes vector-data-offset)))
           (:results (value :scs ,scs))
           (:result-types ,type)
           (:generator 4
                       (inst ,mov-inst value
                             (make-ea ,operand-size :base object
                                      :disp (- (+ (* vector-data-offset n-word-bytes)
                                                  (* ,n-bytes index)
                                                  (* ,n-bytes offset))
                                               other-pointer-lowtag)))))
         (define-vop (,(symbolicate "DATA-VECTOR-SET-WITH-OFFSET/" ptype))
           (:translate data-vector-set-with-offset)
           (:policy :fast-safe)
           (:args (object :scs (descriptor-reg) :to (:eval 0))
                  (index :scs (,index-sc) :to (:eval 0))
                  (value :scs ,scs :target result))
           (:info offset)
           (:arg-types ,ptype tagged-num
                       (:constant (constant-displacement other-pointer-lowtag
                                                         ,n-bytes vector-data-offset))
                       ,type)
           (:results (result :scs ,scs))
           (:result-types ,type)
           (:generator 5
                       (inst mov (make-ea ,operand-size :base object :index index :scale ,scale
                                          :disp (- (+ (* vector-data-offset n-word-bytes)
                                                      (* offset ,n-bytes))
                                                   other-pointer-lowtag))
                             (reg-in-size value ,operand-size))
                       (move result value)))

         (define-vop (,(symbolicate "DATA-VECTOR-SET-WITH-OFFSET/" ptype "-C"))
           (:translate data-vector-set-with-offset)
           (:policy :fast-safe)
           (:args (object :scs (descriptor-reg) :to (:eval 0))
                  (value :scs ,scs :target result))
           (:info index offset)
           (:arg-types ,ptype (:constant low-index)
                       (:constant (constant-displacement other-pointer-lowtag
                                                         ,n-bytes vector-data-offset))
                       ,type)
           (:results (result :scs ,scs))
           (:result-types ,type)
           (:generator 4
                       (inst mov (make-ea ,operand-size :base object
                                          :disp (- (+ (* vector-data-offset n-word-bytes)
                                                      (* ,n-bytes index)
                                                      (* ,n-bytes offset))
                                                   other-pointer-lowtag))
                             (reg-in-size value ,operand-size))
                       (move result value))))))))
  (define-data-vector-frobs simple-array-unsigned-byte-7 movzx :byte
    positive-fixnum unsigned-reg signed-reg)
  (define-data-vector-frobs simple-array-unsigned-byte-8 movzx :byte
    positive-fixnum unsigned-reg signed-reg)
  (define-data-vector-frobs simple-array-signed-byte-8 movsx :byte
    tagged-num signed-reg)
  (define-data-vector-frobs simple-base-string
     #!+sb-unicode movzx #!-sb-unicode mov :byte
     character character-reg)
  (define-data-vector-frobs simple-array-unsigned-byte-15 movzx :word
    positive-fixnum unsigned-reg signed-reg)
  (define-data-vector-frobs simple-array-unsigned-byte-16 movzx :word
    positive-fixnum unsigned-reg signed-reg)
  (define-data-vector-frobs simple-array-signed-byte-16 movsx :word
    tagged-num signed-reg)
  (define-data-vector-frobs simple-array-unsigned-byte-32 movzxd :dword
    positive-fixnum unsigned-reg signed-reg)
  (define-data-vector-frobs simple-array-unsigned-byte-31 movzxd :dword
    positive-fixnum unsigned-reg signed-reg)
  (define-data-vector-frobs simple-array-signed-byte-32 movsxd :dword
    tagged-num signed-reg)
  #!+sb-unicode
  (define-data-vector-frobs simple-character-string movzxd :dword
    character character-reg))


;;; These vops are useful for accessing the bits of a vector
;;; irrespective of what type of vector it is.
(define-full-reffer vector-raw-bits * vector-data-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num %vector-raw-bits)
(define-full-setter set-vector-raw-bits * vector-data-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num %set-vector-raw-bits)

;;;; ATOMIC-INCF for arrays

(define-vop (array-atomic-incf/word)
  (:translate %array-atomic-incf/word)
  (:policy :fast-safe)
  (:args (array :scs (descriptor-reg))
         (index :scs (any-reg))
         (diff :scs (unsigned-reg) :target result))
  (:arg-types * positive-fixnum unsigned-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 4
    (inst xadd (make-ea :qword :base array
                        :scale (ash 1 (- word-shift n-fixnum-tag-bits))
                        :index index
                        :disp (- (* vector-data-offset n-word-bytes)
                                 other-pointer-lowtag))
          diff :lock)
    (move result diff)))
