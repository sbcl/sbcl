;;;; type testing and checking VOPs for the x86 VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; test generation utilities

(eval-when (:compile-toplevel :execute)

(defparameter *immediate-types*
  (list unbound-marker-type base-char-type))

(defparameter *fun-header-types*
  (list funcallable-instance-header-type
	simple-fun-header-type
	closure-fun-header-type
	closure-header-type))

(defun canonicalize-headers (headers)
  (collect ((results))
    (let ((start nil)
	  (prev nil)
	  (delta (- other-immediate-1-lowtag other-immediate-0-lowtag)))
      (flet ((emit-test ()
	       (results (if (= start prev)
			    start
			    (cons start prev)))))
	(dolist (header (sort headers #'<))
	  (cond ((null start)
		 (setf start header)
		 (setf prev header))
		((= header (+ prev delta))
		 (setf prev header))
		(t
		 (emit-test)
		 (setf start header)
		 (setf prev header))))
	(emit-test)))
    (results)))

) ; EVAL-WHEN

(macrolet ((test-type (value target not-p &rest type-codes)
  ;; Determine what interesting combinations we need to test for.
  (let* ((type-codes (mapcar #'eval type-codes))
	 (fixnump (and (member even-fixnum-lowtag type-codes)
		       (member odd-fixnum-lowtag type-codes)
		       t))
	 (lowtags (remove lowtag-limit type-codes :test #'<))
	 (extended (remove lowtag-limit type-codes :test #'>))
	 (immediates (intersection extended *immediate-types* :test #'eql))
	 (headers (set-difference extended *immediate-types* :test #'eql))
	 (function-p (if (intersection headers *fun-header-types*)
			 (if (subsetp headers *fun-header-types*)
			     t
			     (error "can't test for mix of function subtypes ~
				     and normal header types"))
			 nil)))
    (unless type-codes
      (error "At least one type must be supplied for TEST-TYPE."))
    (cond
     (fixnump
      (when (remove-if #'(lambda (x)
			   (or (= x even-fixnum-lowtag)
			       (= x odd-fixnum-lowtag)))
		       lowtags)
	(error "can't mix fixnum testing with other lowtags"))
      (when function-p
	(error "can't mix fixnum testing with function subtype testing"))
      (when immediates
	(error "can't mix fixnum testing with other immediates"))
      (if headers
	  `(%test-fixnum-and-headers ,value ,target ,not-p
				     ',(canonicalize-headers headers))
	  `(%test-fixnum ,value ,target ,not-p)))
     (immediates
      (when headers
	(error "can't mix testing of immediates with testing of headers"))
      (when lowtags
	(error "can't mix testing of immediates with testing of lowtags"))
      (when (cdr immediates)
	(error "can't test multiple immediates at the same time"))
      `(%test-immediate ,value ,target ,not-p ,(car immediates)))
     (lowtags
      (when (cdr lowtags)
	(error "can't test multiple lowtags at the same time"))
      (if headers
	  `(%test-lowtag-and-headers
	    ,value ,target ,not-p ,(car lowtags)
	    ,function-p ',(canonicalize-headers headers))
	  `(%test-lowtag ,value ,target ,not-p ,(car lowtags))))
     (headers
      `(%test-headers ,value ,target ,not-p ,function-p
		      ',(canonicalize-headers headers)))
     (t
      (error "nothing to test?"))))))

;;; Emit the most compact form of the test immediate instruction,
;;; using an 8 bit test when the immediate is only 8 bits and the
;;; value is one of the four low registers (eax, ebx, ecx, edx) or the
;;; control stack.
(defun generate-fixnum-test (value)
  (let ((offset (tn-offset value)))
    (cond ((and (sc-is value any-reg descriptor-reg)
		(or (= offset eax-offset) (= offset ebx-offset)
		    (= offset ecx-offset) (= offset edx-offset)))
	   (inst test (make-random-tn :kind :normal
				      :sc (sc-or-lose 'byte-reg)
				      :offset offset)
		 3))
	  ((sc-is value control-stack)
	   (inst test (make-ea :byte :base ebp-tn
			       :disp (- (* (1+ offset) sb!vm:word-bytes)))
		 3))
	  (t
	   (inst test value 3)))))

(defun %test-fixnum (value target not-p)
  (generate-fixnum-test value)
  (inst jmp (if not-p :nz :z) target))

(defun %test-fixnum-and-headers (value target not-p headers)
  (let ((drop-through (gen-label)))
    (generate-fixnum-test value)
    (inst jmp :z (if not-p drop-through target))
    (%test-headers value target not-p nil headers drop-through)))

(defun %test-immediate (value target not-p immediate)
  ;; Code a single instruction byte test if possible.
  (let ((offset (tn-offset value)))
    (cond ((and (sc-is value any-reg descriptor-reg)
		(or (= offset eax-offset) (= offset ebx-offset)
		    (= offset ecx-offset) (= offset edx-offset)))
	   (inst cmp (make-random-tn :kind :normal
				     :sc (sc-or-lose 'byte-reg)
				     :offset offset)
		 immediate))
	  (t
	   (move eax-tn value)
	   (inst cmp al-tn immediate))))
  (inst jmp (if not-p :ne :e) target))

(defun %test-lowtag (value target not-p lowtag &optional al-loaded)
  (unless al-loaded
    (move eax-tn value)
    (inst and al-tn lowtag-mask))
  (inst cmp al-tn lowtag)
  (inst jmp (if not-p :ne :e) target))

(defun %test-lowtag-and-headers (value target not-p lowtag function-p headers)
  (let ((drop-through (gen-label)))
    (%test-lowtag value (if not-p drop-through target) nil lowtag)
    (%test-headers value target not-p function-p headers drop-through t)))


(defun %test-headers (value target not-p function-p headers
			    &optional (drop-through (gen-label)) al-loaded)
  (let ((lowtag (if function-p fun-pointer-lowtag other-pointer-lowtag)))
    (multiple-value-bind (equal less-or-equal when-true when-false)
	;; EQUAL and LESS-OR-EQUAL are the conditions for branching to TARGET.
	;; WHEN-TRUE and WHEN-FALSE are the labels to branch to when we know
	;; it's true and when we know it's false respectively.
	(if not-p
	    (values :ne :a drop-through target)
	    (values :e :na target drop-through))
      (%test-lowtag value when-false t lowtag al-loaded)
      (inst mov al-tn (make-ea :byte :base value :disp (- lowtag)))
      (do ((remaining headers (cdr remaining)))
	  ((null remaining))
	(let ((header (car remaining))
	      (last (null (cdr remaining))))
	  (cond
	   ((atom header)
	    (inst cmp al-tn header)
	    (if last
		(inst jmp equal target)
		(inst jmp :e when-true)))
	   (t
	     (let ((start (car header))
		   (end (cdr header)))
	       (unless (= start bignum-type)
		 (inst cmp al-tn start)
		 (inst jmp :b when-false)) ; was :l
	       (inst cmp al-tn end)
	       (if last
		   (inst jmp less-or-equal target)
		   (inst jmp :be when-true))))))) ; was :le
      (emit-label drop-through))))

;; pw -- based on RISC version. Not sure extra hair is needed yet.
;; difference is that this one uses SUB which overwrites operand
;; both cmp and sub take 2 cycles so maybe its a wash
#+nil
(defun %test-headers (value target not-p function-p headers
			    &optional (drop-through (gen-label)) al-loaded)
  (let ((lowtag (if function-p fun-pointer-lowtag other-pointer-lowtag)))
    (multiple-value-bind (equal less-or-equal when-true when-false)
	;; EQUAL and LESS-OR-EQUAL are the conditions for branching to TARGET.
	;; WHEN-TRUE and WHEN-FALSE are the labels to branch to when we know
	;; it's true and when we know it's false respectively.
	(if not-p
	    (values :ne :a drop-through target)
	    (values :e :na target drop-through))
      (%test-lowtag value when-false t lowtag al-loaded)
      (inst mov al-tn (make-ea :byte :base value :disp (- lowtag)))
      (let ((delta 0))
	(do ((remaining headers (cdr remaining)))
	    ((null remaining))
	  (let ((header (car remaining))
		(last (null (cdr remaining))))
	    (cond
	      ((atom header)
	       (inst sub al-tn (- header delta))
	       (setf delta header)
	       (if last
		   (inst jmp equal target)
		   (inst jmp :e when-true)))
	      (t
	       (let ((start (car header))
		     (end (cdr header)))
		 (unless (= start bignum-type)
		   (inst sub al-tn (- start delta))
		   (setf delta start)
		   (inst jmp :l when-false))
		 (inst sub al-tn (- end delta))
		 (setf delta end)
		 (if last
		     (inst jmp less-or-equal target)
		     (inst jmp :le when-true))))))))
      (emit-label drop-through))))

;;;; type checking and testing

(define-vop (check-type)
  (:args (value :target result :scs (any-reg descriptor-reg)))
  (:results (result :scs (any-reg descriptor-reg)))
  (:temporary (:sc unsigned-reg :offset eax-offset :to (:result 0)) eax)
  (:ignore eax)
  (:vop-var vop)
  (:save-p :compute-only))

(define-vop (type-predicate)
  (:args (value :scs (any-reg descriptor-reg)))
  (:temporary (:sc unsigned-reg :offset eax-offset) eax)
  (:ignore eax)
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe))

;;; simpler VOP that don't need a temporary register
(define-vop (simple-check-type)
  (:args (value :target result :scs (any-reg descriptor-reg)))
  (:results (result :scs (any-reg descriptor-reg)
		    :load-if (not (and (sc-is value any-reg descriptor-reg)
				       (sc-is result control-stack)))))
  (:vop-var vop)
  (:save-p :compute-only))

(define-vop (simple-type-predicate)
  (:args (value :scs (any-reg descriptor-reg control-stack)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe))

(eval-when (:compile-toplevel :execute)

(defun cost-to-test-types (type-codes)
  (+ (* 2 (length type-codes))
     (if (> (apply #'max type-codes) lowtag-limit) 7 2)))

); EVAL-WHEN

;;; FIXME: DEF-TYPE-VOPS and DEF-SIMPLE-TYPE-VOPS are only used in
;;; this file, so they should be in the EVAL-WHEN above, or otherwise
;;; tweaked so that they don't appear in the target system.

(defmacro def-type-vops (pred-name check-name ptype error-code
				   &rest type-codes)
  (let ((cost (cost-to-test-types (mapcar #'eval type-codes))))
    `(progn
       ,@(when pred-name
	   `((define-vop (,pred-name type-predicate)
	       (:translate ,pred-name)
	       (:generator ,cost
		 (test-type value target not-p ,@type-codes)))))
       ,@(when check-name
	   `((define-vop (,check-name check-type)
	       (:generator ,cost
		 (let ((err-lab
			(generate-error-code vop ,error-code value)))
		   (test-type value err-lab t ,@type-codes)
		   (move result value))))))
       ,@(when ptype
	   `((primitive-type-vop ,check-name (:check) ,ptype))))))

(defmacro def-simple-type-vops (pred-name check-name ptype error-code
					  &rest type-codes)
  (let ((cost (cost-to-test-types (mapcar #'eval type-codes))))
    `(progn
       ,@(when pred-name
	   `((define-vop (,pred-name simple-type-predicate)
	       (:translate ,pred-name)
	       (:generator ,cost
		 (test-type value target not-p ,@type-codes)))))
       ,@(when check-name
	   `((define-vop (,check-name simple-check-type)
	       (:generator ,cost
		 (let ((err-lab
			(generate-error-code vop ,error-code value)))
		   (test-type value err-lab t ,@type-codes)
		   (move result value))))))
       ,@(when ptype
	   `((primitive-type-vop ,check-name (:check) ,ptype))))))

(def-simple-type-vops fixnump check-fixnum fixnum object-not-fixnum-error
  even-fixnum-lowtag odd-fixnum-lowtag)

(def-type-vops functionp check-function function
  object-not-function-error fun-pointer-lowtag)

(def-type-vops listp check-list list object-not-list-error
  list-pointer-lowtag)

(def-type-vops %instancep check-instance instance object-not-instance-error
  instance-pointer-lowtag)

(def-type-vops bignump check-bignum bignum
  object-not-bignum-error bignum-type)

(def-type-vops ratiop check-ratio ratio
  object-not-ratio-error ratio-type)

(def-type-vops complexp check-complex complex object-not-complex-error
  complex-type complex-single-float-type complex-double-float-type
  #!+long-float complex-long-float-type)

(def-type-vops complex-rational-p check-complex-rational nil
  object-not-complex-rational-error complex-type)

(def-type-vops complex-float-p check-complex-float nil
  object-not-complex-float-error
  complex-single-float-type complex-double-float-type
  #!+long-float complex-long-float-type)

(def-type-vops complex-single-float-p check-complex-single-float
  complex-single-float object-not-complex-single-float-error
  complex-single-float-type)

(def-type-vops complex-double-float-p check-complex-double-float
  complex-double-float object-not-complex-double-float-error
  complex-double-float-type)

#!+long-float
(def-type-vops complex-long-float-p check-complex-long-float
  complex-long-float object-not-complex-long-float-error
  complex-long-float-type)

(def-type-vops single-float-p check-single-float single-float
  object-not-single-float-error single-float-type)

(def-type-vops double-float-p check-double-float double-float
  object-not-double-float-error double-float-type)

#!+long-float
(def-type-vops long-float-p check-long-float long-float
  object-not-long-float-error long-float-type)

(def-type-vops simple-string-p check-simple-string simple-string
  object-not-simple-string-error simple-string-type)

(def-type-vops simple-bit-vector-p check-simple-bit-vector simple-bit-vector
  object-not-simple-bit-vector-error simple-bit-vector-type)

(def-type-vops simple-vector-p check-simple-vector simple-vector
  object-not-simple-vector-error simple-vector-type)

(def-type-vops simple-array-unsigned-byte-2-p
  check-simple-array-unsigned-byte-2
  simple-array-unsigned-byte-2
  object-not-simple-array-unsigned-byte-2-error
  simple-array-unsigned-byte-2-type)

(def-type-vops simple-array-unsigned-byte-4-p
  check-simple-array-unsigned-byte-4
  simple-array-unsigned-byte-4
  object-not-simple-array-unsigned-byte-4-error
  simple-array-unsigned-byte-4-type)

(def-type-vops simple-array-unsigned-byte-8-p
  check-simple-array-unsigned-byte-8
  simple-array-unsigned-byte-8
  object-not-simple-array-unsigned-byte-8-error
  simple-array-unsigned-byte-8-type)

(def-type-vops simple-array-unsigned-byte-16-p
  check-simple-array-unsigned-byte-16
  simple-array-unsigned-byte-16
  object-not-simple-array-unsigned-byte-16-error
  simple-array-unsigned-byte-16-type)

(def-type-vops simple-array-unsigned-byte-32-p
  check-simple-array-unsigned-byte-32
  simple-array-unsigned-byte-32
  object-not-simple-array-unsigned-byte-32-error
  simple-array-unsigned-byte-32-type)

(def-type-vops simple-array-signed-byte-8-p
  check-simple-array-signed-byte-8
  simple-array-signed-byte-8
  object-not-simple-array-signed-byte-8-error
  simple-array-signed-byte-8-type)

(def-type-vops simple-array-signed-byte-16-p
  check-simple-array-signed-byte-16
  simple-array-signed-byte-16
  object-not-simple-array-signed-byte-16-error
  simple-array-signed-byte-16-type)

(def-type-vops simple-array-signed-byte-30-p
  check-simple-array-signed-byte-30
  simple-array-signed-byte-30
  object-not-simple-array-signed-byte-30-error
  simple-array-signed-byte-30-type)

(def-type-vops simple-array-signed-byte-32-p
  check-simple-array-signed-byte-32
  simple-array-signed-byte-32
  object-not-simple-array-signed-byte-32-error
  simple-array-signed-byte-32-type)

(def-type-vops simple-array-single-float-p check-simple-array-single-float
  simple-array-single-float object-not-simple-array-single-float-error
  simple-array-single-float-type)

(def-type-vops simple-array-double-float-p check-simple-array-double-float
  simple-array-double-float object-not-simple-array-double-float-error
  simple-array-double-float-type)

#!+long-float
(def-type-vops simple-array-long-float-p check-simple-array-long-float
  simple-array-long-float object-not-simple-array-long-float-error
  simple-array-long-float-type)

(def-type-vops simple-array-complex-single-float-p
  check-simple-array-complex-single-float
  simple-array-complex-single-float
  object-not-simple-array-complex-single-float-error
  simple-array-complex-single-float-type)

(def-type-vops simple-array-complex-double-float-p
  check-simple-array-complex-double-float
  simple-array-complex-double-float
  object-not-simple-array-complex-double-float-error
  simple-array-complex-double-float-type)

#!+long-float
(def-type-vops simple-array-complex-long-float-p
  check-simple-array-complex-long-float
  simple-array-complex-long-float
  object-not-simple-array-complex-long-float-error
  simple-array-complex-long-float-type)

(def-type-vops base-char-p check-base-char base-char
  object-not-base-char-error base-char-type)

(def-type-vops system-area-pointer-p check-system-area-pointer
  system-area-pointer object-not-sap-error sap-type)

(def-type-vops weak-pointer-p check-weak-pointer weak-pointer
  object-not-weak-pointer-error weak-pointer-type)

(def-type-vops code-component-p nil nil nil
  code-header-type)

(def-type-vops lra-p nil nil nil
  return-pc-header-type)

(def-type-vops fdefn-p nil nil nil
  fdefn-type)

(def-type-vops funcallable-instance-p nil nil nil
  funcallable-instance-header-type)

(def-type-vops array-header-p nil nil nil
  simple-array-type complex-string-type complex-bit-vector-type
  complex-vector-type complex-array-type)

(def-type-vops stringp check-string nil object-not-string-error
  simple-string-type complex-string-type)

(def-type-vops bit-vector-p check-bit-vector nil object-not-bit-vector-error
  simple-bit-vector-type complex-bit-vector-type)

(def-type-vops vectorp check-vector nil object-not-vector-error
  simple-string-type simple-bit-vector-type simple-vector-type
  simple-array-unsigned-byte-2-type simple-array-unsigned-byte-4-type
  simple-array-unsigned-byte-8-type simple-array-unsigned-byte-16-type
  simple-array-unsigned-byte-32-type
  simple-array-signed-byte-8-type simple-array-signed-byte-16-type
  simple-array-signed-byte-30-type simple-array-signed-byte-32-type
  simple-array-single-float-type simple-array-double-float-type
  #!+long-float simple-array-long-float-type
  simple-array-complex-single-float-type
  simple-array-complex-double-float-type
  #!+long-float simple-array-complex-long-float-type
  complex-string-type complex-bit-vector-type complex-vector-type)

;;; Note that this "type VOP" is sort of an oddball; it doesn't so
;;; much test for a Lisp-level type as just expose a low-level type
;;; code at the Lisp level. It is used as a building block to help us
;;; to express things like the test for (TYPEP FOO '(VECTOR T))
;;; efficiently in Lisp code, but it doesn't correspond to any type
;;; expression which would actually occur in reasonable application
;;; code. (Common Lisp doesn't have any natural way of expressing this
;;; type.) Thus, there's no point in building up the full machinery of
;;; associated backend type predicates and so forth as we do for
;;; ordinary type VOPs.
(def-type-vops complex-vector-p check-complex-vector nil object-not-complex-vector-error
  complex-vector-type)

(def-type-vops simple-array-p check-simple-array nil object-not-simple-array-error
  simple-array-type simple-string-type simple-bit-vector-type
  simple-vector-type simple-array-unsigned-byte-2-type
  simple-array-unsigned-byte-4-type simple-array-unsigned-byte-8-type
  simple-array-unsigned-byte-16-type simple-array-unsigned-byte-32-type
  simple-array-signed-byte-8-type simple-array-signed-byte-16-type
  simple-array-signed-byte-30-type simple-array-signed-byte-32-type
  simple-array-single-float-type simple-array-double-float-type
  #!+long-float simple-array-long-float-type
  simple-array-complex-single-float-type
  simple-array-complex-double-float-type
  #!+long-float simple-array-complex-long-float-type)

(def-type-vops arrayp check-array nil object-not-array-error
  simple-array-type simple-string-type simple-bit-vector-type
  simple-vector-type simple-array-unsigned-byte-2-type
  simple-array-unsigned-byte-4-type simple-array-unsigned-byte-8-type
  simple-array-unsigned-byte-16-type simple-array-unsigned-byte-32-type
  simple-array-signed-byte-8-type simple-array-signed-byte-16-type
  simple-array-signed-byte-30-type simple-array-signed-byte-32-type
  simple-array-single-float-type simple-array-double-float-type
  #!+long-float simple-array-long-float-type
  simple-array-complex-single-float-type
  simple-array-complex-double-float-type
  #!+long-float simple-array-complex-long-float-type
  complex-string-type complex-bit-vector-type complex-vector-type
  complex-array-type)

(def-type-vops numberp check-number nil object-not-number-error
  even-fixnum-lowtag odd-fixnum-lowtag bignum-type ratio-type
  single-float-type double-float-type #!+long-float long-float-type complex-type
  complex-single-float-type complex-double-float-type
  #!+long-float complex-long-float-type)

(def-type-vops rationalp check-rational nil object-not-rational-error
  even-fixnum-lowtag odd-fixnum-lowtag ratio-type bignum-type)

(def-type-vops integerp check-integer nil object-not-integer-error
  even-fixnum-lowtag odd-fixnum-lowtag bignum-type)

(def-type-vops floatp check-float nil object-not-float-error
  single-float-type double-float-type #!+long-float long-float-type)

(def-type-vops realp check-real nil object-not-real-error
  even-fixnum-lowtag odd-fixnum-lowtag ratio-type bignum-type
  single-float-type double-float-type #!+long-float long-float-type)

;;;; other integer ranges

;;; A (SIGNED-BYTE 32) can be represented with either fixnum or a bignum with
;;; exactly one digit.

(define-vop (signed-byte-32-p type-predicate)
  (:translate signed-byte-32-p)
  (:generator 45
    (multiple-value-bind (yep nope)
	(if not-p
	    (values not-target target)
	    (values target not-target))
      (generate-fixnum-test value)
      (inst jmp :e yep)
      (move eax-tn value)
      (inst and al-tn lowtag-mask)
      (inst cmp al-tn other-pointer-lowtag)
      (inst jmp :ne nope)
      (loadw eax-tn value 0 other-pointer-lowtag)
      (inst cmp eax-tn (+ (ash 1 type-bits) bignum-type))
      (inst jmp (if not-p :ne :e) target))
    NOT-TARGET))

(define-vop (check-signed-byte-32 check-type)
  (:generator 45
    (let ((nope (generate-error-code vop
				     object-not-signed-byte-32-error
				     value)))
      (generate-fixnum-test value)
      (inst jmp :e yep)
      (move eax-tn value)
      (inst and al-tn lowtag-mask)
      (inst cmp al-tn other-pointer-lowtag)
      (inst jmp :ne nope)
      (loadw eax-tn value 0 other-pointer-lowtag)
      (inst cmp eax-tn (+ (ash 1 type-bits) bignum-type))
      (inst jmp :ne nope))
    YEP
    (move result value)))

;;; An (unsigned-byte 32) can be represented with either a positive
;;; fixnum, a bignum with exactly one positive digit, or a bignum with
;;; exactly two digits and the second digit all zeros.
(define-vop (unsigned-byte-32-p type-predicate)
  (:translate unsigned-byte-32-p)
  (:generator 45
    (let ((not-target (gen-label))
	  (single-word (gen-label))
	  (fixnum (gen-label)))
      (multiple-value-bind (yep nope)
	  (if not-p
	      (values not-target target)
	      (values target not-target))
	;; Is it a fixnum?
	(generate-fixnum-test value)
	(move eax-tn value)
	(inst jmp :e fixnum)

	;; If not, is it an other pointer?
	(inst and al-tn lowtag-mask)
	(inst cmp al-tn other-pointer-lowtag)
	(inst jmp :ne nope)
	;; Get the header.
	(loadw eax-tn value 0 other-pointer-lowtag)
	;; Is it one?
	(inst cmp eax-tn (+ (ash 1 type-bits) bignum-type))
	(inst jmp :e single-word)
	;; If it's other than two, we can't be an (unsigned-byte 32)
	(inst cmp eax-tn (+ (ash 2 type-bits) bignum-type))
	(inst jmp :ne nope)
	;; Get the second digit.
	(loadw eax-tn value (1+ bignum-digits-offset) other-pointer-lowtag)
	;; All zeros, its an (unsigned-byte 32).
	(inst or eax-tn eax-tn)
	(inst jmp :z yep)
	(inst jmp nope)
	
	(emit-label single-word)
	;; Get the single digit.
	(loadw eax-tn value bignum-digits-offset other-pointer-lowtag)

	;; positive implies (unsigned-byte 32).
	(emit-label fixnum)
	(inst or eax-tn eax-tn)
	(inst jmp (if not-p :s :ns) target)

	(emit-label not-target)))))

(define-vop (check-unsigned-byte-32 check-type)
  (:generator 45
    (let ((nope
	   (generate-error-code vop object-not-unsigned-byte-32-error value))
	  (yep (gen-label))
	  (fixnum (gen-label))
	  (single-word (gen-label)))

      ;; Is it a fixnum?
      (generate-fixnum-test value)
      (move eax-tn value)
      (inst jmp :e fixnum)

      ;; If not, is it an other pointer?
      (inst and al-tn lowtag-mask)
      (inst cmp al-tn other-pointer-lowtag)
      (inst jmp :ne nope)
      ;; Get the header.
      (loadw eax-tn value 0 other-pointer-lowtag)
      ;; Is it one?
      (inst cmp eax-tn (+ (ash 1 type-bits) bignum-type))
      (inst jmp :e single-word)
      ;; If it's other than two, we can't be an (unsigned-byte 32)
      (inst cmp eax-tn (+ (ash 2 type-bits) bignum-type))
      (inst jmp :ne nope)
      ;; Get the second digit.
      (loadw eax-tn value (1+ bignum-digits-offset) other-pointer-lowtag)
      ;; All zeros, its an (unsigned-byte 32).
      (inst or eax-tn eax-tn)
      (inst jmp :z yep)
      (inst jmp nope)
	
      (emit-label single-word)
      ;; Get the single digit.
      (loadw eax-tn value bignum-digits-offset other-pointer-lowtag)

      ;; positive implies (unsigned-byte 32).
      (emit-label fixnum)
      (inst or eax-tn eax-tn)
      (inst jmp :s nope)

      (emit-label yep)
      (move result value))))

;;;; list/symbol types
;;;
;;; symbolp (or symbol (eq nil))
;;; consp (and list (not (eq nil)))

(define-vop (symbolp type-predicate)
  (:translate symbolp)
  (:generator 12
    (let ((is-symbol-label (if not-p drop-thru target)))
      (inst cmp value nil-value)
      (inst jmp :e is-symbol-label)
      (test-type value target not-p symbol-header-type))
    DROP-THRU))

(define-vop (check-symbol check-type)
  (:generator 12
    (let ((error (generate-error-code vop object-not-symbol-error value)))
      (inst cmp value nil-value)
      (inst jmp :e drop-thru)
      (test-type value error t symbol-header-type))
    DROP-THRU
    (move result value)))

(define-vop (consp type-predicate)
  (:translate consp)
  (:generator 8
    (let ((is-not-cons-label (if not-p target drop-thru)))
      (inst cmp value nil-value)
      (inst jmp :e is-not-cons-label)
      (test-type value target not-p list-pointer-lowtag))
    DROP-THRU))

(define-vop (check-cons check-type)
  (:generator 8
    (let ((error (generate-error-code vop object-not-cons-error value)))
      (inst cmp value nil-value)
      (inst jmp :e error)
      (test-type value error t list-pointer-lowtag)
      (move result value))))

) ; MACROLET
