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
	  (make-ea :qword :base rank
		   :disp (+ (* (1+ array-dimensions-offset) n-word-bytes)
			    lowtag-mask)))
    (inst and bytes (lognot lowtag-mask))
    (inst lea header (make-ea :qword :base rank
			      :disp (fixnumize (1- array-dimensions-offset))))
    (inst shl header n-widetag-bits)
    (inst or  header type)
    (inst shr header (1- n-lowtag-bits))
    (pseudo-atomic
     (allocation result bytes node)
     (inst lea result (make-ea :qword :base result :disp other-pointer-lowtag))
     (storew header result 0 other-pointer-lowtag))))

;;;; additional accessors and setters for the array header
(define-full-reffer %array-dimension *
  array-dimensions-offset other-pointer-lowtag
  (any-reg) positive-fixnum sb!kernel:%array-dimension)

(define-full-setter %set-array-dimension *
  array-dimensions-offset other-pointer-lowtag
  (any-reg) positive-fixnum sb!kernel:%set-array-dimension)

(define-vop (array-rank-vop)
  (:translate sb!kernel:%array-rank)
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
	 (bound :scs (any-reg descriptor-reg))
	 (index :scs (any-reg descriptor-reg) :target result))
;  (:arg-types * positive-fixnum tagged-num)
  (:results (result :scs (any-reg descriptor-reg)))
 ; (:result-types positive-fixnum)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 5
    (let ((error (generate-error-code vop invalid-array-index-error
				      array bound index))
	  (index (if (sc-is index immediate)
		   (fixnumize (tn-value index))
		   index)))
      (inst cmp bound index)
      ;; We use below-or-equal even though it's an unsigned test,
      ;; because negative indexes appear as large unsigned numbers.
      ;; Therefore, we get the <0 and >=bound test all rolled into one.
      (inst jmp :be error)
      (unless (and (tn-p index) (location= result index))
	(inst mov result index)))))

;;;; accessors/setters

;;; variants built on top of WORD-INDEX-REF, etc. I.e., those vectors
;;; whose elements are represented in integer registers and are built
;;; out of 8, 16, or 32 bit elements.
(macrolet ((def-full-data-vector-frobs (type element-type &rest scs)
	     `(progn
		(define-full-reffer ,(symbolicate "DATA-VECTOR-REF/" type)
		  ,type vector-data-offset other-pointer-lowtag ,scs
		  ,element-type data-vector-ref)
		(define-full-setter ,(symbolicate "DATA-VECTOR-SET/" type)
		  ,type vector-data-offset other-pointer-lowtag ,scs
		  ,element-type data-vector-set)))
	   )
  (def-full-data-vector-frobs simple-vector * descriptor-reg any-reg)
  (def-full-data-vector-frobs simple-array-unsigned-byte-64 unsigned-num
    unsigned-reg)
  (def-full-data-vector-frobs simple-array-signed-byte-61 tagged-num any-reg)
  (def-full-data-vector-frobs simple-array-unsigned-byte-60
      positive-fixnum any-reg)
  (def-full-data-vector-frobs simple-array-signed-byte-64
      signed-num signed-reg)
  (def-full-data-vector-frobs simple-array-unsigned-byte-63 unsigned-num
    unsigned-reg))

;;;; integer vectors whose elements are smaller than a byte, i.e.,
;;;; bit, 2-bit, and 4-bit vectors

(macrolet ((def-small-data-vector-frobs (type bits)
	     (let* ((elements-per-word (floor n-word-bits bits))
		    (bit-shift (1- (integer-length elements-per-word))))
    `(progn
       (define-vop (,(symbolicate 'data-vector-ref/ type))
	 (:note "inline array access")
	 (:translate data-vector-ref)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg))
		(index :scs (unsigned-reg)))
	 (:arg-types ,type positive-fixnum)
	 (:results (result :scs (unsigned-reg) :from (:argument 0)))
	 (:result-types positive-fixnum)
	 (:temporary (:sc unsigned-reg :offset ecx-offset) ecx)
	 (:generator 20
	   (move ecx index)
	   (inst shr ecx ,bit-shift)
	   (inst mov result
		 (make-ea :qword :base object :index ecx :scale n-word-bytes
			  :disp (- (* vector-data-offset n-word-bytes)
				   other-pointer-lowtag)))
	   (move ecx index)
	   (inst and ecx ,(1- elements-per-word))
	   ,@(unless (= bits 1)
	       `((inst shl ecx ,(1- (integer-length bits)))))
	   (inst shr result :cl)
	   (inst and result ,(1- (ash 1 bits)))))
       (define-vop (,(symbolicate 'data-vector-ref-c/ type))
	 (:translate data-vector-ref)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg)))
	 (:arg-types ,type (:constant index))
	 (:info index)
	 (:results (result :scs (unsigned-reg)))
	 (:result-types positive-fixnum)
	 (:generator 15
	   (multiple-value-bind (word extra) (floor index ,elements-per-word)
	     (loadw result object (+ word vector-data-offset)
		    other-pointer-lowtag)
	     (unless (zerop extra)
	       (inst shr result (* extra ,bits)))
	     (unless (= extra ,(1- elements-per-word))
	       (inst and result ,(1- (ash 1 bits)))))))
       (define-vop (,(symbolicate 'data-vector-set/ type))
	 (:note "inline array store")
	 (:translate data-vector-set)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg) :target ptr)
		(index :scs (unsigned-reg) :target ecx)
		(value :scs (unsigned-reg immediate) :target result))
	 (:arg-types ,type positive-fixnum positive-fixnum)
	 (:results (result :scs (unsigned-reg)))
	 (:result-types positive-fixnum)
	 (:temporary (:sc unsigned-reg) word-index)
	 (:temporary (:sc unsigned-reg :from (:argument 0)) ptr old)
	 (:temporary (:sc unsigned-reg :offset ecx-offset :from (:argument 1))
		     ecx)
	 (:generator 25
	   (move word-index index)
	   (inst shr word-index ,bit-shift)
	   (inst lea ptr
		 (make-ea :qword :base object :index word-index 
			  :scale n-word-bytes
			  :disp (- (* vector-data-offset n-word-bytes)
				   other-pointer-lowtag)))
	   (loadw old ptr)
	   (move ecx index)
	   (inst and ecx ,(1- elements-per-word))
	   ,@(unless (= bits 1)
	       `((inst shl ecx ,(1- (integer-length bits)))))
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
	   (storew old ptr)
	   (sc-case value
	     (immediate
	      (inst mov result (tn-value value)))
	     (unsigned-reg
	      (move result value)))))
       (define-vop (,(symbolicate 'data-vector-set-c/ type))
	 (:translate data-vector-set)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg))
		(value :scs (unsigned-reg immediate) :target result))
	 (:arg-types ,type (:constant index) positive-fixnum)
	 (:info index)
	 (:results (result :scs (unsigned-reg)))
	 (:result-types positive-fixnum)
	 (:temporary (:sc unsigned-reg :to (:result 0)) old)
	 (:generator 20
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
		    (inst and old (lognot (ash mask shift))))
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

(define-vop (data-vector-ref/simple-array-single-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg)))
  (:arg-types simple-array-single-float positive-fixnum)
  (:results (value :scs (single-reg)))
  (:result-types single-float)
  (:generator 5
   (inst movss value (make-ea :dword :base object :index index :scale 1
			      :disp (- (* vector-data-offset
					  n-word-bytes)
				       other-pointer-lowtag)))))

(define-vop (data-vector-ref-c/simple-array-single-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types simple-array-single-float (:constant (signed-byte 61)))
  (:results (value :scs (single-reg)))
  (:result-types single-float)
  (:generator 4
   (inst movss value (make-ea :dword :base object
			      :disp (- (+ (* vector-data-offset
					     n-word-bytes)
					  (* 4 index))
				       other-pointer-lowtag)))))

(define-vop (data-vector-set/simple-array-single-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg))
	 (value :scs (single-reg) :target result))
  (:arg-types simple-array-single-float positive-fixnum single-float)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:generator 5
   (inst movss (make-ea :dword :base object :index index :scale 1
			:disp (- (* vector-data-offset
				    n-word-bytes)
				 other-pointer-lowtag))
	 value)
   (unless (location= result value)
     (inst movss result value))))

(define-vop (data-vector-set-c/simple-array-single-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (value :scs (single-reg) :target result))
  (:info index)
  (:arg-types simple-array-single-float (:constant (signed-byte 29))
	      single-float)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:generator 4
   (inst movss (make-ea :dword :base object
			:disp (- (+ (* vector-data-offset
				       n-word-bytes)
				    (* 4 index))
				 other-pointer-lowtag))
	 value)
   (unless (location= result value)
     (inst movss result value))))

(define-vop (data-vector-ref/simple-array-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg)))
  (:arg-types simple-array-double-float positive-fixnum)
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:generator 7
   (inst movsd value (make-ea :dword :base object :index index :scale 2
			      :disp (- (* vector-data-offset
					  n-word-bytes)
				       other-pointer-lowtag)))))

(define-vop (data-vector-ref-c/simple-array-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types simple-array-double-float (:constant (signed-byte 29)))
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:generator 6
   (inst movsd value (make-ea :dword :base object
			      :disp (- (+ (* vector-data-offset
					     n-word-bytes)
					  (* 8 index))
				       other-pointer-lowtag)))))

(define-vop (data-vector-set/simple-array-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg))
	 (value :scs (double-reg) :target result))
  (:arg-types simple-array-double-float positive-fixnum double-float)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:generator 20
   (inst movsd (make-ea :dword :base object :index index :scale 2
			       :disp (- (* vector-data-offset
					   n-word-bytes)
					other-pointer-lowtag))
	 value)
   (unless (location= result value)
     (inst movsd result value))))

(define-vop (data-vector-set-c/simple-array-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (value :scs (double-reg) :target result))
  (:info index)
  (:arg-types simple-array-double-float (:constant (signed-byte 61))
	      double-float)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:generator 19
   (inst movsd (make-ea :dword :base object
		       :disp (- (+ (* vector-data-offset
				      n-word-bytes)
				   (* 8 index))
				other-pointer-lowtag))
	 value)
   (unless (location= result value)
     (inst movsd result value))))


;;; complex float variants XXX completely broken

(define-vop (data-vector-ref/simple-array-complex-single-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg)))
  (:arg-types simple-array-complex-single-float positive-fixnum)
  (:results (value :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 5
    (let ((real-tn (complex-single-reg-real-tn value)))
      (with-empty-tn@fp-top (real-tn)
	(inst fld (make-ea :dword :base object :index index :scale 2
			   :disp (- (* vector-data-offset
				       n-word-bytes)
				    other-pointer-lowtag)))))
    (let ((imag-tn (complex-single-reg-imag-tn value)))
      (with-empty-tn@fp-top (imag-tn)
	(inst fld (make-ea :dword :base object :index index :scale 2
			   :disp (- (* (1+ vector-data-offset)
				       n-word-bytes)
				    other-pointer-lowtag)))))))

(define-vop (data-vector-ref-c/simple-array-complex-single-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types simple-array-complex-single-float (:constant (signed-byte 29)))
  (:results (value :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 4
    (let ((real-tn (complex-single-reg-real-tn value)))
      (with-empty-tn@fp-top (real-tn)
	(inst fld (make-ea :dword :base object
			   :disp (- (+ (* vector-data-offset
					  n-word-bytes)
				       (* 8 index))
				    other-pointer-lowtag)))))
    (let ((imag-tn (complex-single-reg-imag-tn value)))
      (with-empty-tn@fp-top (imag-tn)
	(inst fld (make-ea :dword :base object
			   :disp (- (+ (* vector-data-offset
					  n-word-bytes)
				       (* 8 index) 4)
				    other-pointer-lowtag)))))))

(define-vop (data-vector-set/simple-array-complex-single-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg))
	 (value :scs (complex-single-reg) :target result))
  (:arg-types simple-array-complex-single-float positive-fixnum
	      complex-single-float)
  (:results (result :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 5
    (let ((value-real (complex-single-reg-real-tn value))
	  (result-real (complex-single-reg-real-tn result)))
      (cond ((zerop (tn-offset value-real))
	     ;; Value is in ST0.
	     (inst fst (make-ea :dword :base object :index index :scale 2
				:disp (- (* vector-data-offset
					    n-word-bytes)
					 other-pointer-lowtag)))
	     (unless (zerop (tn-offset result-real))
	       ;; Value is in ST0 but not result.
	       (inst fst result-real)))
	    (t
	     ;; Value is not in ST0.
	     (inst fxch value-real)
	     (inst fst (make-ea :dword :base object :index index :scale 2
				:disp (- (* vector-data-offset
					    n-word-bytes)
					 other-pointer-lowtag)))
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
      (inst fst (make-ea :dword :base object :index index :scale 2
			 :disp (- (+ (* vector-data-offset
					n-word-bytes)
				     4)
				  other-pointer-lowtag)))
      (unless (location= value-imag result-imag)
	(inst fst result-imag))
      (inst fxch value-imag))))

(define-vop (data-vector-set-c/simple-array-complex-single-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (value :scs (complex-single-reg) :target result))
  (:info index)
  (:arg-types simple-array-complex-single-float (:constant (signed-byte 61))
	      complex-single-float)
  (:results (result :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 4
    (let ((value-real (complex-single-reg-real-tn value))
	  (result-real (complex-single-reg-real-tn result)))
      (cond ((zerop (tn-offset value-real))
	     ;; Value is in ST0.
	     (inst fst (make-ea :dword :base object
				:disp (- (+ (* vector-data-offset
					       n-word-bytes)
					    (* 8 index))
					 other-pointer-lowtag)))
	     (unless (zerop (tn-offset result-real))
	       ;; Value is in ST0 but not result.
	       (inst fst result-real)))
	    (t
	     ;; Value is not in ST0.
	     (inst fxch value-real)
	     (inst fst (make-ea :dword :base object
				:disp (- (+ (* vector-data-offset
					       n-word-bytes)
					    (* 8 index))
					 other-pointer-lowtag)))
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
      (inst fst (make-ea :dword :base object
			 :disp (- (+ (* vector-data-offset
					n-word-bytes)
				     (* 8 index) 4)
				  other-pointer-lowtag)))
      (unless (location= value-imag result-imag)
	(inst fst result-imag))
      (inst fxch value-imag))))


(define-vop (data-vector-ref/simple-array-complex-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg)))
  (:arg-types simple-array-complex-double-float positive-fixnum)
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 7
    (let ((real-tn (complex-double-reg-real-tn value)))
      (with-empty-tn@fp-top (real-tn)
	(inst fldd (make-ea :dword :base object :index index :scale 4
			    :disp (- (* vector-data-offset
					n-word-bytes)
				     other-pointer-lowtag)))))
    (let ((imag-tn (complex-double-reg-imag-tn value)))
      (with-empty-tn@fp-top (imag-tn)
	(inst fldd (make-ea :dword :base object :index index :scale 4
			    :disp (- (+ (* vector-data-offset
					   n-word-bytes)
					8)
				     other-pointer-lowtag)))))))

(define-vop (data-vector-ref-c/simple-array-complex-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types simple-array-complex-double-float (:constant (signed-byte 29)))
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 6
    (let ((real-tn (complex-double-reg-real-tn value)))
      (with-empty-tn@fp-top (real-tn)
	(inst fldd (make-ea :dword :base object
			    :disp (- (+ (* vector-data-offset
					   n-word-bytes)
					(* 16 index))
				     other-pointer-lowtag)))))
    (let ((imag-tn (complex-double-reg-imag-tn value)))
      (with-empty-tn@fp-top (imag-tn)
	(inst fldd (make-ea :dword :base object
			    :disp (- (+ (* vector-data-offset
					   n-word-bytes)
					(* 16 index) 8)
				     other-pointer-lowtag)))))))

(define-vop (data-vector-set/simple-array-complex-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg))
	 (value :scs (complex-double-reg) :target result))
  (:arg-types simple-array-complex-double-float positive-fixnum
	      complex-double-float)
  (:results (result :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 20
    (let ((value-real (complex-double-reg-real-tn value))
	  (result-real (complex-double-reg-real-tn result)))
      (cond ((zerop (tn-offset value-real))
	     ;; Value is in ST0.
	     (inst fstd (make-ea :dword :base object :index index :scale 4
				 :disp (- (* vector-data-offset
					     n-word-bytes)
					  other-pointer-lowtag)))
	     (unless (zerop (tn-offset result-real))
	       ;; Value is in ST0 but not result.
	       (inst fstd result-real)))
	    (t
	     ;; Value is not in ST0.
	     (inst fxch value-real)
	     (inst fstd (make-ea :dword :base object :index index :scale 4
				 :disp (- (* vector-data-offset
					     n-word-bytes)
					  other-pointer-lowtag)))
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
      (inst fstd (make-ea :dword :base object :index index :scale 4
			  :disp (- (+ (* vector-data-offset
					 n-word-bytes)
				      8)
				   other-pointer-lowtag)))
      (unless (location= value-imag result-imag)
	(inst fstd result-imag))
      (inst fxch value-imag))))

(define-vop (data-vector-set-c/simple-array-complex-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (value :scs (complex-double-reg) :target result))
  (:info index)
  (:arg-types simple-array-complex-double-float (:constant (signed-byte 61))
	      complex-double-float)
  (:results (result :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 19
    (let ((value-real (complex-double-reg-real-tn value))
	  (result-real (complex-double-reg-real-tn result)))
      (cond ((zerop (tn-offset value-real))
	     ;; Value is in ST0.
	     (inst fstd (make-ea :dword :base object
				 :disp (- (+ (* vector-data-offset
						n-word-bytes)
					     (* 16 index))
					  other-pointer-lowtag)))
	     (unless (zerop (tn-offset result-real))
	       ;; Value is in ST0 but not result.
	       (inst fstd result-real)))
	    (t
	     ;; Value is not in ST0.
	     (inst fxch value-real)
	     (inst fstd (make-ea :dword :base object
				 :disp (- (+ (* vector-data-offset
						n-word-bytes)
					     (* 16 index))
					  other-pointer-lowtag)))
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
      (inst fstd (make-ea :dword :base object
			  :disp (- (+ (* vector-data-offset
					 n-word-bytes)
				      (* 16 index) 8)
				   other-pointer-lowtag)))
      (unless (location= value-imag result-imag)
	(inst fstd result-imag))
      (inst fxch value-imag))))






;;; unsigned-byte-8
(macrolet ((define-data-vector-frobs (ptype)
  `(progn
    (define-vop (,(symbolicate "DATA-VECTOR-REF/" ptype))
      (:translate data-vector-ref)
      (:policy :fast-safe)
      (:args (object :scs (descriptor-reg))
             (index :scs (unsigned-reg)))
      (:arg-types ,ptype positive-fixnum)
      (:results (value :scs (unsigned-reg signed-reg)))
      (:result-types positive-fixnum)
      (:generator 5
	(inst movzx value
	      (make-ea :byte :base object :index index :scale 1
		       :disp (- (* vector-data-offset n-word-bytes)
				other-pointer-lowtag)))))
    (define-vop (,(symbolicate "DATA-VECTOR-REF-C/" ptype))
      (:translate data-vector-ref)
      (:policy :fast-safe)
      (:args (object :scs (descriptor-reg)))
      (:info index)
      (:arg-types ,ptype (:constant (signed-byte 61)))
      (:results (value :scs (unsigned-reg signed-reg)))
      (:result-types positive-fixnum)
      (:generator 4
	(inst movzx value
	      (make-ea :byte :base object
		       :disp (- (+ (* vector-data-offset n-word-bytes) index)
				other-pointer-lowtag)))))
    (define-vop (,(symbolicate "DATA-VECTOR-SET/" ptype))
      (:translate data-vector-set)
      (:policy :fast-safe)
      (:args (object :scs (descriptor-reg) :to (:eval 0))
	     (index :scs (unsigned-reg) :to (:eval 0))
	     (value :scs (unsigned-reg signed-reg) :target eax))
      (:arg-types ,ptype positive-fixnum positive-fixnum)
      (:temporary (:sc unsigned-reg :offset eax-offset :target result
		       :from (:argument 2) :to (:result 0))
		  eax)
      (:results (result :scs (unsigned-reg signed-reg)))
      (:result-types positive-fixnum)
      (:generator 5
	(move eax value)
	(inst mov (make-ea :byte :base object :index index :scale 1
			   :disp (- (* vector-data-offset n-word-bytes)
				    other-pointer-lowtag))
	      al-tn)
	(move result eax)))
    (define-vop (,(symbolicate "DATA-VECTOR-SET-C/" ptype))
      (:translate data-vector-set)
      (:policy :fast-safe)
      (:args (object :scs (descriptor-reg) :to (:eval 0))
	     (value :scs (unsigned-reg signed-reg) :target eax))
      (:info index)
      (:arg-types ,ptype (:constant (signed-byte 61))
		  positive-fixnum)
      (:temporary (:sc unsigned-reg :offset eax-offset :target result
		       :from (:argument 1) :to (:result 0))
		  eax)
      (:results (result :scs (unsigned-reg signed-reg)))
      (:result-types positive-fixnum)
      (:generator 4
	(move eax value)
	(inst mov (make-ea :byte :base object
			   :disp (- (+ (* vector-data-offset n-word-bytes) index)
				    other-pointer-lowtag))
	      al-tn)
	(move result eax))))))
  (define-data-vector-frobs simple-array-unsigned-byte-7)
  (define-data-vector-frobs simple-array-unsigned-byte-8))

;;; unsigned-byte-16
(macrolet ((define-data-vector-frobs (ptype)
    `(progn
      (define-vop (,(symbolicate "DATA-VECTOR-REF/" ptype))
	(:translate data-vector-ref)
	(:policy :fast-safe)
	(:args (object :scs (descriptor-reg))
	       (index :scs (unsigned-reg)))
	(:arg-types ,ptype positive-fixnum)
	(:results (value :scs (unsigned-reg signed-reg)))
	(:result-types positive-fixnum)
	(:generator 5
	  (inst movzx value
		(make-ea :word :base object :index index :scale 2
			 :disp (- (* vector-data-offset n-word-bytes)
				  other-pointer-lowtag)))))
      (define-vop (,(symbolicate "DATA-VECTOR-REF-C/" ptype))
	(:translate data-vector-ref)
	(:policy :fast-safe)
	(:args (object :scs (descriptor-reg)))
	(:info index)
	(:arg-types ,ptype (:constant (signed-byte 29)))
	(:results (value :scs (unsigned-reg signed-reg)))
	(:result-types positive-fixnum)
	(:generator 4
	  (inst movzx value
		(make-ea :word :base object
			 :disp (- (+ (* vector-data-offset n-word-bytes) (* 2 index))
				  other-pointer-lowtag)))))
      (define-vop (,(symbolicate "DATA-VECTOR-SET/" ptype))
	(:translate data-vector-set)
	(:policy :fast-safe)
	(:args (object :scs (descriptor-reg) :to (:eval 0))
	       (index :scs (unsigned-reg) :to (:eval 0))
	       (value :scs (unsigned-reg signed-reg) :target eax))
	(:arg-types ,ptype positive-fixnum positive-fixnum)
	(:temporary (:sc unsigned-reg :offset eax-offset :target result
			 :from (:argument 2) :to (:result 0))
		    eax)
	(:results (result :scs (unsigned-reg signed-reg)))
	(:result-types positive-fixnum)
	(:generator 5
	  (move eax value)
	  (inst mov (make-ea :word :base object :index index :scale 2
			     :disp (- (* vector-data-offset n-word-bytes)
				      other-pointer-lowtag))
		ax-tn)
	  (move result eax)))

      (define-vop (,(symbolicate "DATA-VECTOR-SET-C/" ptype))
	(:translate data-vector-set)
	(:policy :fast-safe)
	(:args (object :scs (descriptor-reg) :to (:eval 0))
	       (value :scs (unsigned-reg signed-reg) :target eax))
	(:info index)
	(:arg-types ,ptype (:constant (signed-byte 29))
		    positive-fixnum)
	(:temporary (:sc unsigned-reg :offset eax-offset :target result
			 :from (:argument 1) :to (:result 0))
		    eax)
	(:results (result :scs (unsigned-reg signed-reg)))
	(:result-types positive-fixnum)
	(:generator 4
	  (move eax value)
	  (inst mov (make-ea :word :base object
			     :disp (- (+ (* vector-data-offset n-word-bytes)
					 (* 2 index))
				      other-pointer-lowtag))
		ax-tn)
	  (move result eax))))))
  (define-data-vector-frobs simple-array-unsigned-byte-15)
  (define-data-vector-frobs simple-array-unsigned-byte-16))

(macrolet ((define-data-vector-frobs (ptype)
    `(progn
      (define-vop (,(symbolicate "DATA-VECTOR-REF/" ptype))
	(:translate data-vector-ref)
	(:policy :fast-safe)
	(:args (object :scs (descriptor-reg))
	       (index :scs (unsigned-reg)))
	(:arg-types ,ptype positive-fixnum)
	(:results (value :scs (unsigned-reg signed-reg)))
	(:result-types positive-fixnum)
	(:generator 5
	  (inst movzxd value
		(make-ea :dword :base object :index index :scale 4
			 :disp (- (* vector-data-offset n-word-bytes)
				  other-pointer-lowtag)))))
      (define-vop (,(symbolicate "DATA-VECTOR-REF-C/" ptype))
	(:translate data-vector-ref)
	(:policy :fast-safe)
	(:args (object :scs (descriptor-reg)))
	(:info index)
	(:arg-types ,ptype (:constant (signed-byte 61)))
	(:results (value :scs (unsigned-reg signed-reg)))
	(:result-types positive-fixnum)
	(:generator 4
	  (inst movzxd value
		(make-ea :dword :base object
			 :disp (- (+ (* vector-data-offset n-word-bytes)
				     (* 4 index))
				  other-pointer-lowtag)))))
      (define-vop (,(symbolicate "DATA-VECTOR-SET/" ptype))
	(:translate data-vector-set)
	(:policy :fast-safe)
	(:args (object :scs (descriptor-reg) :to (:eval 0))
	       (index :scs (unsigned-reg) :to (:eval 0))
	       (value :scs (unsigned-reg signed-reg) :target rax))
	(:arg-types ,ptype positive-fixnum positive-fixnum)
	(:temporary (:sc unsigned-reg :offset rax-offset :target result
			 :from (:argument 2) :to (:result 0))
		    rax)
	(:results (result :scs (unsigned-reg signed-reg)))
	(:result-types positive-fixnum)
	(:generator 5
	  (move rax value)
	  (inst mov (make-ea :dword :base object :index index :scale 4
				:disp (- (* vector-data-offset n-word-bytes)
					 other-pointer-lowtag))
		eax-tn)
	  (move result rax)))

      (define-vop (,(symbolicate "DATA-VECTOR-SET-C/" ptype))
	(:translate data-vector-set)
	(:policy :fast-safe)
	(:args (object :scs (descriptor-reg) :to (:eval 0))
	       (value :scs (unsigned-reg signed-reg) :target rax))
	(:info index)
	(:arg-types ,ptype (:constant (signed-byte 61))
		    positive-fixnum)
	(:temporary (:sc unsigned-reg :offset rax-offset :target result
			 :from (:argument 1) :to (:result 0))
		    rax)
	(:results (result :scs (unsigned-reg signed-reg)))
	(:result-types positive-fixnum)
	(:generator 4
	  (move rax value)
	  (inst mov (make-ea :dword :base object
			     :disp (- (+ (* vector-data-offset n-word-bytes)
					 (* 4 index))
				      other-pointer-lowtag))
		eax-tn)
	  (move result rax))))))
  (define-data-vector-frobs simple-array-unsigned-byte-32)
  (define-data-vector-frobs simple-array-unsigned-byte-31))

;;; simple-string

(define-vop (data-vector-ref/simple-base-string)
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (unsigned-reg)))
  (:arg-types simple-base-string positive-fixnum)
  (:results (value :scs (base-char-reg)))
  (:result-types base-char)
  (:generator 5
    (inst mov value
	  (make-ea :byte :base object :index index :scale 1
		   :disp (- (* vector-data-offset n-word-bytes)
			    other-pointer-lowtag)))))

(define-vop (data-vector-ref-c/simple-base-string)
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types simple-base-string (:constant (signed-byte 61)))
  (:results (value :scs (base-char-reg)))
  (:result-types base-char)
  (:generator 4
    (inst mov value
	  (make-ea :byte :base object
		   :disp (- (+ (* vector-data-offset n-word-bytes) index)
			    other-pointer-lowtag)))))

(define-vop (data-vector-set/simple-base-string)
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:eval 0))
	 (index :scs (unsigned-reg) :to (:eval 0))
	 (value :scs (base-char-reg) :target result))
  (:arg-types simple-base-string positive-fixnum base-char)
  (:results (result :scs (base-char-reg)))
  (:result-types base-char)
  (:generator 5
    (inst mov (make-ea :byte :base object :index index :scale 1
		       :disp (- (* vector-data-offset n-word-bytes)
				other-pointer-lowtag))
	  value)
    (move result value)))

(define-vop (data-vector-set/simple-base-string-c)
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:eval 0))
	 (value :scs (base-char-reg)))
  (:info index)
  (:arg-types simple-base-string (:constant (signed-byte 61)) base-char)
  (:results (result :scs (base-char-reg)))
  (:result-types base-char)
  (:generator 4
   (inst mov (make-ea :byte :base object
		      :disp (- (+ (* vector-data-offset n-word-bytes) index)
			       other-pointer-lowtag))
	 value)
   (move result value)))

;;; signed-byte-8

(define-vop (data-vector-ref/simple-array-signed-byte-8)
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (unsigned-reg)))
  (:arg-types simple-array-signed-byte-8 positive-fixnum)
  (:results (value :scs (signed-reg)))
  (:result-types tagged-num)
  (:generator 5
    (inst movsx value
	  (make-ea :byte :base object :index index :scale 1
		   :disp (- (* vector-data-offset n-word-bytes)
			    other-pointer-lowtag)))))

(define-vop (data-vector-ref-c/simple-array-signed-byte-8)
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types simple-array-signed-byte-8 (:constant (signed-byte 61)))
  (:results (value :scs (signed-reg)))
  (:result-types tagged-num)
  (:generator 4
    (inst movsx value
	  (make-ea :byte :base object
		   :disp (- (+ (* vector-data-offset n-word-bytes) index)
			    other-pointer-lowtag)))))

(define-vop (data-vector-set/simple-array-signed-byte-8)
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:eval 0))
	 (index :scs (unsigned-reg) :to (:eval 0))
	 (value :scs (signed-reg) :target eax))
  (:arg-types simple-array-signed-byte-8 positive-fixnum tagged-num)
  (:temporary (:sc unsigned-reg :offset eax-offset :target result
		   :from (:argument 2) :to (:result 0))
	      eax)
  (:results (result :scs (signed-reg)))
  (:result-types tagged-num)
  (:generator 5
    (move eax value)
    (inst mov (make-ea :byte :base object :index index :scale 1
		       :disp (- (* vector-data-offset n-word-bytes)
				other-pointer-lowtag))
	  al-tn)
    (move result eax)))

(define-vop (data-vector-set-c/simple-array-signed-byte-8)
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:eval 0))
	 (value :scs (signed-reg) :target eax))
  (:info index)
  (:arg-types simple-array-signed-byte-8 (:constant (signed-byte 61))
	      tagged-num)
  (:temporary (:sc unsigned-reg :offset eax-offset :target result
		   :from (:argument 1) :to (:result 0))
	      eax)
  (:results (result :scs (signed-reg)))
  (:result-types tagged-num)
  (:generator 4
    (move eax value)
    (inst mov (make-ea :byte :base object
		       :disp (- (+ (* vector-data-offset n-word-bytes) index)
				other-pointer-lowtag))
	  al-tn)
    (move result eax)))

;;; signed-byte-16

(define-vop (data-vector-ref/simple-array-signed-byte-16)
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (unsigned-reg)))
  (:arg-types simple-array-signed-byte-16 positive-fixnum)
  (:results (value :scs (signed-reg)))
  (:result-types tagged-num)
  (:generator 5
    (inst movsx value
	  (make-ea :word :base object :index index :scale 2
		   :disp (- (* vector-data-offset n-word-bytes)
			    other-pointer-lowtag)))))

(define-vop (data-vector-ref-c/simple-array-signed-byte-16)
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types simple-array-signed-byte-16 (:constant (signed-byte 61)))
  (:results (value :scs (signed-reg)))
  (:result-types tagged-num)
  (:generator 4
    (inst movsx value
	  (make-ea :word :base object
		   :disp (- (+ (* vector-data-offset n-word-bytes)
			       (* 2 index))
			    other-pointer-lowtag)))))

(define-vop (data-vector-set/simple-array-signed-byte-16)
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:eval 0))
	 (index :scs (unsigned-reg) :to (:eval 0))
	 (value :scs (signed-reg) :target eax))
  (:arg-types simple-array-signed-byte-16 positive-fixnum tagged-num)
  (:temporary (:sc signed-reg :offset eax-offset :target result
		   :from (:argument 2) :to (:result 0))
	      eax)
  (:results (result :scs (signed-reg)))
  (:result-types tagged-num)
  (:generator 5
    (move eax value)
    (inst mov (make-ea :word :base object :index index :scale 2
		       :disp (- (* vector-data-offset n-word-bytes)
				other-pointer-lowtag))
	  ax-tn)
    (move result eax)))

(define-vop (data-vector-set-c/simple-array-signed-byte-16)
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:eval 0))
	 (value :scs (signed-reg) :target eax))
  (:info index)
  (:arg-types simple-array-signed-byte-16 (:constant (signed-byte 61)) tagged-num)
  (:temporary (:sc signed-reg :offset eax-offset :target result
		   :from (:argument 1) :to (:result 0))
	      eax)
  (:results (result :scs (signed-reg)))
  (:result-types tagged-num)
  (:generator 4
    (move eax value)
    (inst mov
	  (make-ea :word :base object
		   :disp (- (+ (* vector-data-offset n-word-bytes)
			       (* 2 index))
			    other-pointer-lowtag))
	  ax-tn)
    (move result eax)))


(define-vop (data-vector-ref/simple-array-signed-byte-32)
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (unsigned-reg)))
  (:arg-types simple-array-signed-byte-32 positive-fixnum)
  (:results (value :scs (signed-reg)))
  (:result-types tagged-num)
  (:generator 5
    (inst movsxd value
	  (make-ea :dword :base object :index index :scale 4
		   :disp (- (* vector-data-offset n-word-bytes)
			    other-pointer-lowtag)))))

(define-vop (data-vector-ref-c/simple-array-signed-byte-32)
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types simple-array-signed-byte-32 (:constant (signed-byte 61)))
  (:results (value :scs (signed-reg)))
  (:result-types tagged-num)
  (:generator 4
    (inst movsxd value
	  (make-ea :dword :base object
		   :disp (- (+ (* vector-data-offset n-word-bytes)
			       (* 4 index))
			    other-pointer-lowtag)))))

(define-vop (data-vector-set/simple-array-signed-byte-32)
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:eval 0))
	 (index :scs (unsigned-reg) :to (:eval 0))
	 (value :scs (signed-reg) :target eax))
  (:arg-types simple-array-signed-byte-32 positive-fixnum tagged-num)
  (:temporary (:sc signed-reg :offset eax-offset :target result
		   :from (:argument 2) :to (:result 0))
	      eax)
  (:results (result :scs (signed-reg)))
  (:result-types tagged-num)
  (:generator 5
    (move eax value)
    (inst mov (make-ea :dword :base object :index index :scale 4
		       :disp (- (* vector-data-offset n-word-bytes)
				other-pointer-lowtag))
	  eax-tn)
    (move result eax)))

(define-vop (data-vector-set-c/simple-array-signed-byte-32)
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:eval 0))
	 (value :scs (signed-reg) :target eax))
  (:info index)
  (:arg-types simple-array-signed-byte-32 (:constant (signed-byte 61)) tagged-num)
  (:temporary (:sc signed-reg :offset eax-offset :target result
		   :from (:argument 1) :to (:result 0))
	      eax)
  (:results (result :scs (signed-reg)))
  (:result-types tagged-num)
  (:generator 4
    (move eax value)
    (inst mov
	  (make-ea :dword :base object
		   :disp (- (+ (* vector-data-offset n-word-bytes)
			       (* 4 index))
			    other-pointer-lowtag))
	  rax-tn)
    (move result eax)))

;;; These VOPs are used for implementing float slots in structures (whose raw
;;; data is an unsigned-64 vector).
(define-vop (raw-ref-single data-vector-ref/simple-array-single-float)
  (:translate %raw-ref-single)
  (:arg-types sb!c::raw-vector positive-fixnum))
(define-vop (raw-ref-single-c data-vector-ref-c/simple-array-single-float)
  (:translate %raw-ref-single)
  (:arg-types sb!c::raw-vector (:constant (signed-byte 61))))
(define-vop (raw-set-single data-vector-set/simple-array-single-float)
  (:translate %raw-set-single)
  (:arg-types sb!c::raw-vector positive-fixnum single-float))
(define-vop (raw-set-single-c data-vector-set-c/simple-array-single-float)
  (:translate %raw-set-single)
  (:arg-types sb!c::raw-vector (:constant (signed-byte 61)) single-float))
(define-vop (raw-ref-double data-vector-ref/simple-array-double-float)
  (:translate %raw-ref-double)
  (:arg-types sb!c::raw-vector positive-fixnum))
(define-vop (raw-ref-double-c data-vector-ref-c/simple-array-double-float)
  (:translate %raw-ref-double)
  (:arg-types sb!c::raw-vector (:constant (signed-byte 61))))
(define-vop (raw-set-double data-vector-set/simple-array-double-float)
  (:translate %raw-set-double)
  (:arg-types sb!c::raw-vector positive-fixnum double-float))
(define-vop (raw-set-double-c data-vector-set-c/simple-array-double-float)
  (:translate %raw-set-double)
  (:arg-types sb!c::raw-vector (:constant (signed-byte 61)) double-float))


;;;; complex-float raw structure slot accessors

(define-vop (raw-ref-complex-single
	     data-vector-ref/simple-array-complex-single-float)
  (:translate %raw-ref-complex-single)
  (:arg-types sb!c::raw-vector positive-fixnum))
(define-vop (raw-ref-complex-single-c
	     data-vector-ref-c/simple-array-complex-single-float)
  (:translate %raw-ref-complex-single)
  (:arg-types sb!c::raw-vector (:constant (signed-byte 61))))
(define-vop (raw-set-complex-single
	     data-vector-set/simple-array-complex-single-float)
  (:translate %raw-set-complex-single)
  (:arg-types sb!c::raw-vector positive-fixnum complex-single-float))
(define-vop (raw-set-complex-single-c
	     data-vector-set-c/simple-array-complex-single-float)
  (:translate %raw-set-complex-single)
  (:arg-types sb!c::raw-vector (:constant (signed-byte 61))
	      complex-single-float))
(define-vop (raw-ref-complex-double
	     data-vector-ref/simple-array-complex-double-float)
  (:translate %raw-ref-complex-double)
  (:arg-types sb!c::raw-vector positive-fixnum))
(define-vop (raw-ref-complex-double-c
	     data-vector-ref-c/simple-array-complex-double-float)
  (:translate %raw-ref-complex-double)
  (:arg-types sb!c::raw-vector (:constant (signed-byte 61))))
(define-vop (raw-set-complex-double
	     data-vector-set/simple-array-complex-double-float)
  (:translate %raw-set-complex-double)
  (:arg-types sb!c::raw-vector positive-fixnum complex-double-float))
(define-vop (raw-set-complex-double-c
	     data-vector-set-c/simple-array-complex-double-float)
  (:translate %raw-set-complex-double)
  (:arg-types sb!c::raw-vector (:constant (signed-byte 61))
	      complex-double-float))


;;; These vops are useful for accessing the bits of a vector
;;; irrespective of what type of vector it is.
(define-full-reffer raw-bits * 0 other-pointer-lowtag (unsigned-reg)
  unsigned-num %raw-bits)
(define-full-setter set-raw-bits * 0 other-pointer-lowtag (unsigned-reg)
  unsigned-num %set-raw-bits)

;;;; miscellaneous array VOPs

(define-vop (get-vector-subtype get-header-data))
(define-vop (set-vector-subtype set-header-data))
