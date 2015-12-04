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
          (make-ea :dword :base rank
                   :disp (+ (* array-dimensions-offset n-word-bytes)
                            lowtag-mask)))
    (inst and bytes (lognot lowtag-mask))
    (inst lea header (make-ea :dword :base rank
                              :disp (fixnumize (1- array-dimensions-offset))))
    (inst shl header n-widetag-bits)
    (inst or  header type)
    (inst shr header 2)
    (pseudo-atomic
     (allocation result bytes node nil other-pointer-lowtag)
     (storew header result 0 other-pointer-lowtag))))

(define-vop (make-array-header/c)
  (:translate make-array-header)
  (:policy :fast-safe)
  (:arg-types (:constant t) (:constant t))
  (:info type rank)
   (:results (result :scs (descriptor-reg)))
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
  (:generator 6
    (loadw res x 0 other-pointer-lowtag)
    (inst shr res n-widetag-bits)
    (inst sub res (1- array-dimensions-offset))))

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
         (bound :scs (any-reg))
         (index :scs (any-reg descriptor-reg #+nil immediate)))
  (:arg-types * positive-fixnum *)
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
                (define-full-reffer+offset ,(symbolicate "DATA-VECTOR-REF-WITH-OFFSET/" type)
                  ,type vector-data-offset other-pointer-lowtag ,scs
                  ,element-type data-vector-ref-with-offset)
                (define-full-setter+offset ,(symbolicate "DATA-VECTOR-SET-WITH-OFFSET/" type)
                  ,type vector-data-offset other-pointer-lowtag ,scs
                  ,element-type data-vector-set-with-offset))))
  (def-full-data-vector-frobs simple-vector * descriptor-reg any-reg)
  (def-full-data-vector-frobs simple-array-unsigned-byte-32 unsigned-num
    unsigned-reg)
  (def-full-data-vector-frobs simple-array-fixnum tagged-num any-reg)
  (def-full-data-vector-frobs simple-array-unsigned-fixnum positive-fixnum any-reg)
  (def-full-data-vector-frobs simple-array-signed-byte-32 signed-num
    signed-reg)
  (def-full-data-vector-frobs simple-array-unsigned-byte-31 unsigned-num
    unsigned-reg)
  #!+sb-unicode
  (def-full-data-vector-frobs simple-character-string character character-reg))

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
              (:constant (integer 0 #x7fffffff)) (:constant (integer 0 0)))
  (:info index offset)
  (:ignore offset)
  (:results (result :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 3
    (multiple-value-bind (dword-index bit) (floor index 32)
      (inst mov result
                (make-ea :dword :base object
                         :disp (+ (* dword-index 4)
                                  (- (* vector-data-offset n-word-bytes)
                                     other-pointer-lowtag))))
      (let ((right-shift (- bit n-fixnum-tag-bits)))
        (cond ((plusp right-shift)
               (inst shr result right-shift))
              ((minusp right-shift) ; = left shift
               (inst shl result (- right-shift))))))
    (inst and result (fixnumize 1))))

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
    (inst bt (make-ea :dword :base object
                      :disp (- (* vector-data-offset n-word-bytes)
                               other-pointer-lowtag))
             index)
    (inst sbb result result)
    (inst and result (fixnumize 1))))

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
           (inst mov result (make-ea-for-vector-data object :index ecx))
           (move ecx index)
           ;; We used to mask ECX for all values of ELEMENT-PER-WORD,
           ;; but since Intel's documentation says that the chip will
           ;; mask shift and rotate counts by 31 automatically, we can
           ;; safely move the masking operation under the protection of
           ;; this UNLESS in the bit-vector case.  --njf, 2006-07-14
           ,@(unless (= elements-per-word n-word-bits)
               `((inst and ecx ,(1- elements-per-word))
                 (inst shl ecx ,(1- (integer-length bits)))))
           (inst shr result :cl)
           (inst and result ,(1- (ash 1 bits)))))
       (define-vop (,(symbolicate 'data-vector-ref-with-offset/ type "-C"))
         (:translate data-vector-ref-with-offset)
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg)))
         (:arg-types ,type (:constant index) (:constant (integer 0 0)))
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
         (:args (object :scs (descriptor-reg) :to (:argument 2))
                (index :scs (unsigned-reg) :target ecx)
                (value :scs (unsigned-reg immediate) :target result))
         (:info offset)
         (:arg-types ,type positive-fixnum (:constant (integer 0 0))
                     positive-fixnum)
         (:results (result :scs (unsigned-reg)))
         (:result-types positive-fixnum)
         (:temporary (:sc unsigned-reg) word-index)
         (:temporary (:sc unsigned-reg) old)
         (:temporary (:sc unsigned-reg :offset ecx-offset :from (:argument 1)) ecx)
         (:generator 25
           (aver (zerop offset))
           (move word-index index)
           (inst shr word-index ,bit-shift)
           (inst mov old (make-ea-for-vector-data object :index word-index))
           (move ecx index)
           ;; We used to mask ECX for all values of ELEMENT-PER-WORD,
           ;; but since Intel's documentation says that the chip will
           ;; mask shift and rotate counts by 31 automatically, we can
           ;; safely move the masking operation under the protection of
           ;; this UNLESS in the bit-vector case.  --njf, 2006-07-14
           ,@(unless (= elements-per-word n-word-bits)
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
           (inst mov (make-ea-for-vector-data object :index word-index)
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
         (:arg-types ,type (:constant index) (:constant (integer 0 0))
                     positive-fixnum)
         (:info index offset)
         (:results (result :scs (unsigned-reg)))
         (:result-types positive-fixnum)
         (:temporary (:sc unsigned-reg :to (:result 0)) old)
         (:generator 20
           (aver (zerop offset))
           (multiple-value-bind (word extra) (floor index ,elements-per-word)
             (loadw old object (+ word vector-data-offset) other-pointer-lowtag)
             (sc-case value
               (immediate
                (let* ((value (tn-value value))
                       (mask ,(1- (ash 1 bits)))
                       (shift (* extra ,bits)))
                  (unless (= value mask)
                    (inst and old (ldb (byte n-word-bits 0)
                                       (lognot (ash mask shift)))))
                  (unless (zerop value)
                    (inst or old (ash value shift)))))
               (unsigned-reg
                (let ((shift (* extra ,bits)))
                  (unless (zerop shift)
                    (inst ror old shift))
                  (inst and old (lognot ,(1- (ash 1 bits))))
                  (inst or old value)
                  (unless (zerop shift)
                    (inst rol old shift)))))
             (storew old object (+ word vector-data-offset) other-pointer-lowtag)
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
  (sc-case index
    (immediate
     (make-ea :dword :base object
              :disp (- (+ (* vector-data-offset n-word-bytes)
                          (* element-size (+ offset (tn-value index)))
                          complex-offset)
                       other-pointer-lowtag)))
    (t
     (make-ea :dword :base object :index index :scale scale
              :disp (- (+ (* vector-data-offset n-word-bytes)
                          (* element-size offset)
                          complex-offset)
                       other-pointer-lowtag)))))

(define-vop (data-vector-ref-with-offset/simple-array-single-float)
  (:note "inline array access")
  (:translate data-vector-ref-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg immediate)))
  (:info offset)
  (:arg-types simple-array-single-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag
                                                4 vector-data-offset)))
  (:results (value :scs (single-reg)))
  (:result-types single-float)
  (:generator 5
   (with-empty-tn@fp-top(value)
     (inst fld (make-ea-for-float-ref object index offset 4)))))

(define-vop (data-vector-set-with-offset/simple-array-single-float)
  (:note "inline array store")
  (:translate data-vector-set-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg immediate))
         (value :scs (single-reg) :target result))
  (:info offset)
  (:arg-types simple-array-single-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag
                                                4 vector-data-offset))
              single-float)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:generator 5
    (cond ((zerop (tn-offset value))
           ;; Value is in ST0.
           (inst fst (make-ea-for-float-ref object index offset 4))
           (unless (zerop (tn-offset result))
             ;; Value is in ST0 but not result.
             (inst fst result)))
          (t
           ;; Value is not in ST0.
           (inst fxch value)
           (inst fst (make-ea-for-float-ref object index offset 4))
           (cond ((zerop (tn-offset result))
                  ;; The result is in ST0.
                  (inst fst value))
                 (t
                  ;; Neither value or result are in ST0
                  (unless (location= value result)
                    (inst fst result))
                  (inst fxch value)))))))

(define-vop (data-vector-ref-with-offset/simple-array-double-float)
  (:note "inline array access")
  (:translate data-vector-ref-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg immediate)))
  (:info offset)
  (:arg-types simple-array-double-float
              tagged-num
              (:constant (constant-displacement other-pointer-lowtag
                                                8 vector-data-offset)))
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:generator 7
   (with-empty-tn@fp-top(value)
     (inst fldd (make-ea-for-float-ref object index offset 8 :scale 2)))))

(define-vop (data-vector-set-with-offset/simple-array-double-float)
  (:note "inline array store")
  (:translate data-vector-set-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg immediate))
         (value :scs (double-reg) :target result))
  (:info offset)
  (:arg-types simple-array-double-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag
                                                8 vector-data-offset))
              double-float)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:generator 20
    (cond ((zerop (tn-offset value))
           ;; Value is in ST0.
           (inst fstd (make-ea-for-float-ref object index offset 8 :scale 2))
           (unless (zerop (tn-offset result))
                   ;; Value is in ST0 but not result.
                   (inst fstd result)))
          (t
           ;; Value is not in ST0.
           (inst fxch value)
           (inst fstd (make-ea-for-float-ref object index offset 8 :scale 2))
           (cond ((zerop (tn-offset result))
                  ;; The result is in ST0.
                  (inst fstd value))
                 (t
                  ;; Neither value or result are in ST0
                  (unless (location= value result)
                          (inst fstd result))
                  (inst fxch value)))))))

;;; complex float variants

(define-vop (data-vector-ref-with-offset/simple-array-complex-single-float)
  (:note "inline array access")
  (:translate data-vector-ref-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg immediate)))
  (:info offset)
  (:arg-types simple-array-complex-single-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag
                                                8 vector-data-offset)))
  (:results (value :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 5
    (let ((real-tn (complex-single-reg-real-tn value)))
      (with-empty-tn@fp-top (real-tn)
        (inst fld (make-ea-for-float-ref object index offset 8 :scale 2))))
    (let ((imag-tn (complex-single-reg-imag-tn value)))
      (with-empty-tn@fp-top (imag-tn)
        ;; FIXME
        (inst fld (make-ea-for-float-ref object index offset 8
                                         :scale 2 :complex-offset 4))))))

(define-vop (data-vector-set-with-offset/simple-array-complex-single-float)
  (:note "inline array store")
  (:translate data-vector-set-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg immediate))
         (value :scs (complex-single-reg) :target result))
  (:info offset)
  (:arg-types simple-array-complex-single-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag
                                                8 vector-data-offset))
              complex-single-float)
  (:results (result :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 5
    (let ((value-real (complex-single-reg-real-tn value))
          (result-real (complex-single-reg-real-tn result)))
      (cond ((zerop (tn-offset value-real))
             ;; Value is in ST0.
             (inst fst (make-ea-for-float-ref object index offset 8 :scale 2))
             (unless (zerop (tn-offset result-real))
               ;; Value is in ST0 but not result.
               (inst fst result-real)))
            (t
             ;; Value is not in ST0.
             (inst fxch value-real)
             (inst fst (make-ea-for-float-ref object index offset 8 :scale 2))
             (cond ((zerop (tn-offset result-real))
                    ;; The result is in ST0.
                    (inst fst value-real))
                   (t
                    ;; Neither value or result are in ST0
                    (unless (location= value-real result-real)
                      (inst fst result-real))
                    (inst fxch value-real))))))
    (let ((value-imag (complex-single-reg-imag-tn value))
          (result-imag (complex-single-reg-imag-tn result)))
      (inst fxch value-imag)
      (inst fst (make-ea-for-float-ref object index offset 8
                                       :scale 2 :complex-offset 4))
      (unless (location= value-imag result-imag)
        (inst fst result-imag))
      (inst fxch value-imag))))

(define-vop (data-vector-ref-with-offset/simple-array-complex-double-float)
  (:note "inline array access")
  (:translate data-vector-ref-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg immediate)))
  (:info offset)
  (:arg-types simple-array-complex-double-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag
                                                16 vector-data-offset)))
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 7
    (let ((real-tn (complex-double-reg-real-tn value)))
      (with-empty-tn@fp-top (real-tn)
        (inst fldd (make-ea-for-float-ref object index offset 16 :scale 4)))
    (let ((imag-tn (complex-double-reg-imag-tn value)))
      (with-empty-tn@fp-top (imag-tn)
        (inst fldd (make-ea-for-float-ref object index offset 16
                                          :scale 4 :complex-offset 8)))))))

(define-vop (data-vector-set-with-offset/simple-array-complex-double-float)
  (:note "inline array store")
  (:translate data-vector-set-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg immediate))
         (value :scs (complex-double-reg) :target result))
  (:info offset)
  (:arg-types simple-array-complex-double-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag
                                                16 vector-data-offset))
              complex-double-float)
  (:results (result :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 20
    (let ((value-real (complex-double-reg-real-tn value))
          (result-real (complex-double-reg-real-tn result)))
      (cond ((zerop (tn-offset value-real))
             ;; Value is in ST0.
             (inst fstd (make-ea-for-float-ref object index offset 16
                                               :scale 4))
             (unless (zerop (tn-offset result-real))
               ;; Value is in ST0 but not result.
               (inst fstd result-real)))
            (t
             ;; Value is not in ST0.
             (inst fxch value-real)
             (inst fstd (make-ea-for-float-ref object index offset 16
                                               :scale 4))
             (cond ((zerop (tn-offset result-real))
                    ;; The result is in ST0.
                    (inst fstd value-real))
                   (t
                    ;; Neither value or result are in ST0
                    (unless (location= value-real result-real)
                      (inst fstd result-real))
                    (inst fxch value-real))))))
    (let ((value-imag (complex-double-reg-imag-tn value))
          (result-imag (complex-double-reg-imag-tn result)))
      (inst fxch value-imag)
      (inst fstd (make-ea-for-float-ref object index offset 16
                                        :scale 4 :complex-offset 8))
      (unless (location= value-imag result-imag)
        (inst fstd result-imag))
      (inst fxch value-imag))))


;;; {un,}signed-byte-8, simple-base-string

(macrolet ((define-data-vector-frobs (ptype element-type ref-inst
                                            8-bit-tns-p &rest scs)
  `(progn
    (define-vop (,(symbolicate "DATA-VECTOR-REF-WITH-OFFSET/" ptype))
      (:translate data-vector-ref-with-offset)
      (:policy :fast-safe)
      (:args (object :scs (descriptor-reg))
             (index :scs (signed-reg immediate)))
      (:info offset)
      (:arg-types ,ptype tagged-num
                  (:constant (constant-displacement other-pointer-lowtag
                                                    1 vector-data-offset)))
      (:results (value :scs ,scs))
      (:result-types ,element-type)
      (:generator 5
        (sc-case index
          (immediate
           (inst ,ref-inst value (make-ea-for-vector-data
                                  object :size :byte
                                  :offset (+ (tn-value index) offset))))
          (t
           (inst ,ref-inst value
                 (make-ea-for-vector-data object :size :byte
                                          :index index :offset offset))))))
    (define-vop (,(symbolicate "DATA-VECTOR-SET-WITH-OFFSET/" ptype))
      (:translate data-vector-set-with-offset)
      (:policy :fast-safe)
      (:args (object :scs (descriptor-reg) :to (:eval 0))
             (index :scs (signed-reg immediate) :to (:eval 0))
             (value :scs ,scs ,@(unless 8-bit-tns-p
                                  '(:target eax))))
      (:info offset)
      (:arg-types ,ptype tagged-num
                  (:constant (constant-displacement other-pointer-lowtag
                                                    1 vector-data-offset))
                  ,element-type)
      ,@(unless 8-bit-tns-p
         '((:temporary (:sc unsigned-reg :offset eax-offset :target result
                        :from (:argument 2) :to (:result 0))
            eax)))
      (:results (result :scs ,scs))
      (:result-types ,element-type)
      (:generator 5
        ,@(unless 8-bit-tns-p
           '((move eax value)))
        (sc-case index
          (immediate
           (inst mov (make-ea-for-vector-data
                      object :size :byte :offset (+ (tn-value index) offset))
                 ,(if 8-bit-tns-p
                      'value
                      'al-tn)))
          (t
           (inst mov (make-ea-for-vector-data object :size :byte
                                              :index index :offset offset)
                 ,(if 8-bit-tns-p
                      'value
                      'al-tn))))
        (move result ,(if 8-bit-tns-p
                          'value
                          'eax)))))))
  (define-data-vector-frobs simple-array-unsigned-byte-7 positive-fixnum
    movzx nil unsigned-reg signed-reg)
  (define-data-vector-frobs simple-array-unsigned-byte-8 positive-fixnum
    movzx nil unsigned-reg signed-reg)
  (define-data-vector-frobs simple-array-signed-byte-8 tagged-num
    movsx nil signed-reg)
  (define-data-vector-frobs simple-base-string character
                            #!+sb-unicode movzx #!-sb-unicode mov
                            #!+sb-unicode nil #!-sb-unicode t character-reg))

;;; {un,}signed-byte-16
(macrolet ((define-data-vector-frobs (ptype element-type ref-inst &rest scs)
    `(progn
      (define-vop (,(symbolicate "DATA-VECTOR-REF-WITH-OFFSET/" ptype))
        (:translate data-vector-ref-with-offset)
        (:policy :fast-safe)
        (:args (object :scs (descriptor-reg))
               (index :scs (signed-reg immediate)))
        (:info offset)
        (:arg-types ,ptype tagged-num
                    (:constant (constant-displacement other-pointer-lowtag
                                                      2 vector-data-offset)))
        (:results (value :scs ,scs))
        (:result-types ,element-type)
        (:generator 5
          (sc-case index
            (immediate
             (inst ,ref-inst value
                   (make-ea-for-vector-data object :size :word
                                            :offset (+ (tn-value index) offset))))
            (t
             (inst ,ref-inst value
                   (make-ea-for-vector-data object :size :word
                                            :index index :offset offset))))))
      (define-vop (,(symbolicate "DATA-VECTOR-SET-WITH-OFFSET/" ptype))
        (:translate data-vector-set-with-offset)
        (:policy :fast-safe)
        (:args (object :scs (descriptor-reg) :to (:eval 0))
               (index :scs (signed-reg immediate) :to (:eval 0))
               (value :scs ,scs :target eax))
        (:info offset)
        (:arg-types ,ptype tagged-num
                    (:constant (constant-displacement other-pointer-lowtag
                                                      2 vector-data-offset))
                    ,element-type)
        (:temporary (:sc unsigned-reg :offset eax-offset :target result
                         :from (:argument 2) :to (:result 0))
                    eax)
        (:results (result :scs ,scs))
        (:result-types ,element-type)
        (:generator 5
          (move eax value)
          (sc-case index
            (immediate
             (inst mov (make-ea-for-vector-data
                        object :size :word :offset (+ (tn-value index) offset))
                   ax-tn))
            (t
             (inst mov (make-ea-for-vector-data object :size :word
                                                :index index :offset offset)
                   ax-tn)))
          (move result eax))))))
  (define-data-vector-frobs simple-array-unsigned-byte-15 positive-fixnum
    movzx unsigned-reg signed-reg)
  (define-data-vector-frobs simple-array-unsigned-byte-16 positive-fixnum
    movzx unsigned-reg signed-reg)
  (define-data-vector-frobs simple-array-signed-byte-16 tagged-num
    movsx signed-reg))


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
    (inst xadd (make-ea :dword :base array
                        :scale 1 :index index
                        :disp (- (* vector-data-offset n-word-bytes)
                                 other-pointer-lowtag))
          diff :lock)
    (move result diff)))
