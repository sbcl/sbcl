(in-package "SB!VM")



;;;; Test generation utilities.

(eval-when (:compile-toplevel :execute)

(defparameter *immediate-types*
  (list unbound-marker-widetag base-char-widetag))

(defparameter *fun-header-widetags*
  (list funcallable-instance-header-widetag
	simple-fun-header-widetag
	closure-fun-header-widetag
	closure-header-widetag))

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

); eval-when (compile eval)

(macrolet ((test-type (value temp target not-p &rest type-codes)
  ;; Determine what interesting combinations we need to test for.
  (let* ((type-codes (mapcar #'eval type-codes))
	 (fixnump (and (member even-fixnum-lowtag type-codes)
		       (member odd-fixnum-lowtag type-codes)
		       t))
	 (lowtags (remove lowtag-limit type-codes :test #'<))
	 (extended (remove lowtag-limit type-codes :test #'>))
	 (immediates (intersection extended *immediate-types* :test #'eql))
	 (headers (set-difference extended *immediate-types* :test #'eql))
	 (function-p (if (intersection headers *fun-header-widetags*)
			 (if (subsetp headers *fun-header-widetags*)
			     t
			     (error "Can't test for mix of function subtypes ~
				     and normal header types."))
			 nil)))
    (unless type-codes
      (error "Must supply at least on type for test-type."))
    (cond
     (fixnump
      (when (remove-if #'(lambda (x)
			   (or (= x even-fixnum-lowtag)
			       (= x odd-fixnum-lowtag)))
		       lowtags)
	(error "Can't mix fixnum testing with other lowtags."))
      (when function-p
	(error "Can't mix fixnum testing with function subtype testing."))
      (when immediates
	(error "Can't mix fixnum testing with other immediates."))
      (if headers
	  `(%test-fixnum-and-headers ,value ,temp ,target ,not-p
				     ',(canonicalize-headers headers))
	  `(%test-fixnum ,value ,temp ,target ,not-p)))
     (immediates
      (when headers
	(error "Can't mix testing of immediates with testing of headers."))
      (when lowtags
	(error "Can't mix testing of immediates with testing of lowtags."))
      (when (cdr immediates)
	(error "Can't test multiple immediates at the same time."))
      `(%test-immediate ,value ,temp ,target ,not-p ,(car immediates)))
     (lowtags
      (when (cdr lowtags)
	(error "Can't test multiple lowtags at the same time."))
      (if headers
	  `(%test-lowtag-and-headers
	    ,value ,temp ,target ,not-p ,(car lowtags)
	    ,function-p ',(canonicalize-headers headers))
	  `(%test-lowtag ,value ,temp ,target ,not-p ,(car lowtags))))
     (headers
      `(%test-headers ,value ,temp ,target ,not-p ,function-p
		      ',(canonicalize-headers headers)))
     (t
      (error "Nothing to test?"))))))


(defun %test-fixnum (value temp target not-p)
  (declare (ignore temp))
  (assemble ()
    (inst extru value 31 2 zero-tn (if not-p := :<>))
    (inst b target :nullify t)))

(defun %test-fixnum-and-headers (value temp target not-p headers)
  (let ((drop-through (gen-label)))
    (assemble ()
      (inst extru value 31 2 zero-tn :<>)
      (inst b (if not-p drop-through target) :nullify t))
    (%test-headers value temp target not-p nil headers drop-through)))

(defun %test-immediate (value temp target not-p immediate)
  (assemble ()
    (inst extru value 31 8 temp)
    (inst bci := not-p immediate temp target)))

(defun %test-lowtag (value temp target not-p lowtag &optional temp-loaded)
  (assemble ()
    (unless temp-loaded
      (inst extru value 31 3 temp))
    (inst bci := not-p lowtag temp target)))

(defun %test-lowtag-and-headers (value temp target not-p lowtag
				       function-p headers)
  (let ((drop-through (gen-label)))
    (%test-lowtag value temp (if not-p drop-through target) nil lowtag)
    (%test-headers value temp target not-p function-p headers drop-through t)))

(defun %test-headers (value temp target not-p function-p headers
			    &optional (drop-through (gen-label)) temp-loaded)
  (let ((lowtag (if function-p fun-pointer-lowtag other-pointer-lowtag)))
    (multiple-value-bind
	(equal greater-or-equal when-true when-false)
	;; EQUAL and GREATER-OR-EQUAL are the conditions for branching to
	;; TARGET.  WHEN-TRUE and WHEN-FALSE are the labels to branch to when
	;; we know it's true and when we know it's false respectively.
	(if not-p
	    (values :<> :< drop-through target)
	    (values := :>= target drop-through))
      (assemble ()
	(%test-lowtag value temp when-false t lowtag temp-loaded)
	(inst ldb (- 3 lowtag) value temp)
	(do ((remaining headers (cdr remaining)))
	    ((null remaining))
	  (let ((header (car remaining))
		(last (null (cdr remaining))))
	    (cond
	     ((atom header)
	      (if last
		  (inst bci equal nil header temp target)
		  (inst bci := nil header temp when-true)))
	     (t
	      (let ((start (car header))
		    (end (cdr header)))
		(unless (= start bignum-widetag)
		  (inst bci :> nil start temp when-false))
		(if last
		    (inst bci greater-or-equal nil end temp target)
		    (inst bci :>= nil end temp when-true)))))))
	(emit-label drop-through)))))


;;;; Type checking and testing:

(define-vop (check-type)
  (:args (value :target result :scs (any-reg descriptor-reg)))
  (:results (result :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg) :to (:result 0)) temp)
  (:vop-var vop)
  (:save-p :compute-only))

(define-vop (type-predicate)
  (:args (value :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe))

(eval-when (:compile-toplevel :execute)

(defun cost-to-test-types (type-codes)
  (+ (* 2 (length type-codes))
     (if (> (apply #'max type-codes) lowtag-limit) 7 2)))

) ; EVAL-WHEN

(defmacro def-type-vops (pred-name check-name ptype error-code
				   &rest type-codes)
  (let ((cost (cost-to-test-types (mapcar #'eval type-codes))))
    `(progn
       ,@(when pred-name
	   `((define-vop (,pred-name type-predicate)
	       (:translate ,pred-name)
	       (:generator ,cost
		 (test-type value temp target not-p ,@type-codes)))))
       ,@(when check-name
	   `((define-vop (,check-name check-type)
	       (:generator ,cost
		 (let ((err-lab
			(generate-error-code vop ,error-code value)))
		   (test-type value temp err-lab t ,@type-codes)
		   (move value result))))))
       ,@(when ptype
	   `((primitive-type-vop ,check-name (:check) ,ptype))))))

(def-type-vops fixnump check-fixnum fixnum object-not-fixnum-error
  even-fixnum-lowtag odd-fixnum-lowtag)

(def-type-vops functionp check-function function
  object-not-fun-error fun-pointer-lowtag)

(def-type-vops listp check-list list object-not-list-error
  list-pointer-lowtag)

(def-type-vops %instancep check-instance instance object-not-instance-error
  instance-pointer-lowtag)

(def-type-vops bignump check-bignum bignum
  object-not-bignum-error bignum-widetag)

(def-type-vops ratiop check-ratio ratio
  object-not-ratio-error ratio-widetag)

(def-type-vops complexp check-complex complex object-not-complex-error
  complex-widetag complex-single-float-widetag complex-double-float-widetag)

(def-type-vops complex-rational-p check-complex-rational nil
  object-not-complex-rational-error complex-widetag)

(def-type-vops complex-float-p check-complex-float nil
  object-not-complex-float-error
  complex-single-float-widetag complex-double-float-widetag)

(def-type-vops complex-single-float-p check-complex-single-float
  complex-single-float object-not-complex-single-float-error
  complex-single-float-widetag)

(def-type-vops complex-double-float-p check-complex-double-float
  complex-double-float object-not-complex-double-float-error
  complex-double-float-widetag)

(def-type-vops single-float-p check-single-float single-float
  object-not-single-float-error single-float-widetag)

(def-type-vops double-float-p check-double-float double-float
  object-not-double-float-error double-float-widetag)

(def-type-vops simple-string-p check-simple-string simple-string
  object-not-simple-string-error simple-string-widetag)

(def-type-vops simple-bit-vector-p check-simple-bit-vector simple-bit-vector
  object-not-simple-bit-vector-error simple-bit-vector-widetag)

(def-type-vops simple-vector-p check-simple-vector simple-vector
  object-not-simple-vector-error simple-vector-widetag)

(def-type-vops simple-array-unsigned-byte-2-p
  check-simple-array-unsigned-byte-2
  simple-array-unsigned-byte-2
  object-not-simple-array-unsigned-byte-2-error
  simple-array-unsigned-byte-2-widetag)

(def-type-vops simple-array-unsigned-byte-4-p
  check-simple-array-unsigned-byte-4
  simple-array-unsigned-byte-4
  object-not-simple-array-unsigned-byte-4-error
  simple-array-unsigned-byte-4-widetag)

(def-type-vops simple-array-unsigned-byte-8-p
  check-simple-array-unsigned-byte-8
  simple-array-unsigned-byte-8
  object-not-simple-array-unsigned-byte-8-error
  simple-array-unsigned-byte-8-widetag)

(def-type-vops simple-array-unsigned-byte-16-p
  check-simple-array-unsigned-byte-16
  simple-array-unsigned-byte-16
  object-not-simple-array-unsigned-byte-16-error
  simple-array-unsigned-byte-16-widetag)

(def-type-vops simple-array-unsigned-byte-32-p
  check-simple-array-unsigned-byte-32
  simple-array-unsigned-byte-32
  object-not-simple-array-unsigned-byte-32-error
  simple-array-unsigned-byte-32-widetag)

(def-type-vops simple-array-signed-byte-8-p
  check-simple-array-signed-byte-8
  simple-array-signed-byte-8
  object-not-simple-array-signed-byte-8-error
  simple-array-signed-byte-8-widetag)

(def-type-vops simple-array-signed-byte-16-p
  check-simple-array-signed-byte-16
  simple-array-signed-byte-16
  object-not-simple-array-signed-byte-16-error
  simple-array-signed-byte-16-widetag)

(def-type-vops simple-array-signed-byte-30-p
  check-simple-array-signed-byte-30
  simple-array-signed-byte-30
  object-not-simple-array-signed-byte-30-error
  simple-array-signed-byte-30-widetag)

(def-type-vops simple-array-signed-byte-32-p
  check-simple-array-signed-byte-32
  simple-array-signed-byte-32
  object-not-simple-array-signed-byte-32-error
  simple-array-signed-byte-32-widetag)

(def-type-vops simple-array-single-float-p check-simple-array-single-float
  simple-array-single-float object-not-simple-array-single-float-error
  simple-array-single-float-widetag)

(def-type-vops simple-array-double-float-p check-simple-array-double-float
  simple-array-double-float object-not-simple-array-double-float-error
  simple-array-double-float-widetag)

(def-type-vops simple-array-complex-single-float-p
  check-simple-array-complex-single-float
  simple-array-complex-single-float
  object-not-simple-array-complex-single-float-error
  simple-array-complex-single-float-widetag)

(def-type-vops simple-array-complex-double-float-p
  check-simple-array-complex-double-float
  simple-array-complex-double-float
  object-not-simple-array-complex-double-float-error
  simple-array-complex-double-float-widetag)

(def-type-vops base-char-p check-base-char base-char
  object-not-base-char-error base-char-widetag)

(def-type-vops system-area-pointer-p check-system-area-pointer
  system-area-pointer object-not-sap-error sap-widetag)

(def-type-vops weak-pointer-p check-weak-pointer weak-pointer
  object-not-weak-pointer-error weak-pointer-widetag)

#|
(def-type-vops scavenger-hook-p nil nil nil
  0)
|#

(def-type-vops code-component-p nil nil nil
  code-header-widetag)

(def-type-vops lra-p nil nil nil
  return-pc-header-widetag)

(def-type-vops fdefn-p nil nil nil
  fdefn-widetag)

(def-type-vops funcallable-instance-p nil nil nil
  funcallable-instance-header-widetag)

(def-type-vops array-header-p nil nil nil
  simple-array-widetag complex-string-widetag complex-bit-vector-widetag
  complex-vector-widetag complex-array-widetag)

#+nil
(def-type-vops nil check-function-or-symbol nil
  object-not-function-or-symbol-error
  fun-pointer-lowtag symbol-header-widetag)

(def-type-vops stringp check-string nil object-not-string-error
  simple-string-widetag complex-string-widetag)

(def-type-vops bit-vector-p check-bit-vector nil object-not-bit-vector-error
  simple-bit-vector-widetag complex-bit-vector-widetag)

(def-type-vops vectorp check-vector nil object-not-vector-error
  simple-string-widetag simple-bit-vector-widetag simple-vector-widetag
  simple-array-unsigned-byte-2-widetag simple-array-unsigned-byte-4-widetag
  simple-array-unsigned-byte-8-widetag simple-array-unsigned-byte-16-widetag
  simple-array-unsigned-byte-32-widetag
  simple-array-signed-byte-8-widetag simple-array-signed-byte-16-widetag
  simple-array-signed-byte-30-widetag simple-array-signed-byte-32-widetag
  simple-array-single-float-widetag simple-array-double-float-widetag
  simple-array-complex-single-float-widetag
  simple-array-complex-double-float-widetag
  complex-string-widetag complex-bit-vector-widetag complex-vector-widetag)

(def-type-vops complex-vector-p check-complex-vector nil object-not-complex-vector-error
  complex-vector-widetag)

(def-type-vops simple-array-p check-simple-array nil object-not-simple-array-error
  simple-array-widetag simple-string-widetag simple-bit-vector-widetag
  simple-vector-widetag simple-array-unsigned-byte-2-widetag
  simple-array-unsigned-byte-4-widetag simple-array-unsigned-byte-8-widetag
  simple-array-unsigned-byte-16-widetag simple-array-unsigned-byte-32-widetag
  simple-array-signed-byte-8-widetag simple-array-signed-byte-16-widetag
  simple-array-signed-byte-30-widetag simple-array-signed-byte-32-widetag
  simple-array-single-float-widetag simple-array-double-float-widetag
  simple-array-complex-single-float-widetag
  simple-array-complex-double-float-widetag)

(def-type-vops arrayp check-array nil object-not-array-error
  simple-array-widetag simple-string-widetag simple-bit-vector-widetag
  simple-vector-widetag simple-array-unsigned-byte-2-widetag
  simple-array-unsigned-byte-4-widetag simple-array-unsigned-byte-8-widetag
  simple-array-unsigned-byte-16-widetag simple-array-unsigned-byte-32-widetag
  simple-array-signed-byte-8-widetag simple-array-signed-byte-16-widetag
  simple-array-signed-byte-30-widetag simple-array-signed-byte-32-widetag
  simple-array-single-float-widetag simple-array-double-float-widetag
  simple-array-complex-single-float-widetag
  simple-array-complex-double-float-widetag
  complex-string-widetag complex-bit-vector-widetag complex-vector-widetag
  complex-array-widetag)

(def-type-vops numberp check-number nil object-not-number-error
  even-fixnum-lowtag odd-fixnum-lowtag bignum-widetag ratio-widetag
  single-float-widetag double-float-widetag complex-widetag
  complex-single-float-widetag complex-double-float-widetag)

(def-type-vops rationalp check-rational nil object-not-rational-error
  even-fixnum-lowtag odd-fixnum-lowtag ratio-widetag bignum-widetag)

(def-type-vops integerp check-integer nil object-not-integer-error
  even-fixnum-lowtag odd-fixnum-lowtag bignum-widetag)

(def-type-vops floatp check-float nil object-not-float-error
  single-float-widetag double-float-widetag)

(def-type-vops realp check-real nil object-not-real-error
  even-fixnum-lowtag odd-fixnum-lowtag ratio-widetag bignum-widetag
  single-float-widetag double-float-widetag)


;;;; Other integer ranges.

;;; A (signed-byte 32) can be represented with either fixnum or a bignum with
;;; exactly one digit.

(defun signed-byte-32-test (value temp not-p target not-target)
  (multiple-value-bind
      (yep nope)
      (if not-p
	  (values not-target target)
	  (values target not-target))
    (assemble ()
      (inst extru value 31 2 zero-tn :<>)
      (inst b yep :nullify t)
      (inst extru value 31 3 temp)
      (inst bci :<> nil other-pointer-lowtag temp nope)
      (loadw temp value 0 other-pointer-lowtag)
      (inst bci := not-p (+ (ash 1 n-widetag-bits) bignum-widetag) temp target)))
  (values))

(define-vop (signed-byte-32-p type-predicate)
  (:translate signed-byte-32-p)
  (:generator 45
    (signed-byte-32-test value temp not-p target not-target)
    NOT-TARGET))

(define-vop (check-signed-byte-32 check-type)
  (:generator 45
    (let ((loose (generate-error-code vop object-not-signed-byte-32-error
				      value)))
      (signed-byte-32-test value temp t loose okay))
    OKAY
    (move value result)))

;;; An (unsigned-byte 32) can be represented with either a positive fixnum, a
;;; bignum with exactly one positive digit, or a bignum with exactly two digits
;;; and the second digit all zeros.

(defun unsigned-byte-32-test (value temp not-p target not-target)
  (let ((nope (if not-p target not-target)))
    (assemble ()
      ;; Is it a fixnum?
      (inst extru value 31 2 zero-tn :<>)
      (inst b fixnum)
      (inst move value temp)

      ;; If not, is it an other pointer?
      (inst extru value 31 3 temp)
      (inst bci :<> nil other-pointer-lowtag temp nope)
      ;; Get the header.
      (loadw temp value 0 other-pointer-lowtag)
      ;; Is it one?
      (inst bci := nil (+ (ash 1 n-widetag-bits) bignum-widetag) temp single-word)
      ;; If it's other than two, we can't be an (unsigned-byte 32)
      (inst bci :<> nil (+ (ash 2 n-widetag-bits) bignum-widetag) temp nope)
      ;; Get the second digit.
      (loadw temp value (1+ bignum-digits-offset) other-pointer-lowtag)
      ;; All zeros, its an (unsigned-byte 32).
      (inst comb (if not-p := :<>) temp zero-tn not-target :nullify t)
      (inst b target :nullify t)
	
      SINGLE-WORD
      ;; Get the single digit.
      (loadw temp value bignum-digits-offset other-pointer-lowtag)

      ;; positive implies (unsigned-byte 32).
      FIXNUM
      (inst bc :>= not-p temp zero-tn target)))
  (values))

(define-vop (unsigned-byte-32-p type-predicate)
  (:translate unsigned-byte-32-p)
  (:generator 45
    (unsigned-byte-32-test value temp not-p target not-target)
    NOT-TARGET))

(define-vop (check-unsigned-byte-32 check-type)
  (:generator 45
    (let ((loose (generate-error-code vop object-not-unsigned-byte-32-error
				      value)))
      (unsigned-byte-32-test value temp t loose okay))
    OKAY
    (move value result)))


;;;; List/symbol types:
;;; 
;;; symbolp (or symbol (eq nil))
;;; consp (and list (not (eq nil)))

(define-vop (symbolp type-predicate)
  (:translate symbolp)
  (:generator 12
    (inst bc := nil value null-tn (if not-p drop-thru target))
    (test-type value temp target not-p symbol-header-widetag)
    DROP-THRU))

(define-vop (check-symbol check-type)
  (:generator 12
    (inst comb := value null-tn drop-thru)
    (let ((error (generate-error-code vop object-not-symbol-error value)))
      (test-type value temp error t symbol-header-widetag))
    DROP-THRU
    (move value result)))
  
(define-vop (consp type-predicate)
  (:translate consp)
  (:generator 8
    (inst bc := nil value null-tn (if not-p target drop-thru))
    (test-type value temp target not-p list-pointer-lowtag)
    DROP-THRU))

(define-vop (check-cons check-type)
  (:generator 8
    (let ((error (generate-error-code vop object-not-cons-error value)))
      (inst bc := nil value null-tn error)
      (test-type value temp error t list-pointer-lowtag))
    (move value result)))

) ; MACROLET