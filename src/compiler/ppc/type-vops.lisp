;;;; type testing and checking VOPs for the PPC VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

(defun %test-fixnum (value target not-p &key temp)
  (assemble ()
    ;; FIXME: again, this 3 should be FIXNUM-MASK
    (inst andi. temp value 3)
    (inst b? (if not-p :ne :eq) target)))

(defun %test-fixnum-and-headers (value target not-p headers &key temp)
  (let ((drop-through (gen-label)))
    (assemble ()
      (inst andi. temp value 3)
      (inst beq (if not-p drop-through target)))
    (%test-headers value target not-p nil headers
		   :drop-through drop-through :temp temp)))

(defun %test-immediate (value target not-p immediate &key temp)
  (assemble ()
    (inst andi. temp value widetag-mask)
    (inst cmpwi temp immediate)
    (inst b? (if not-p :ne :eq) target)))

(defun %test-lowtag (value target not-p lowtag &key temp)
  (assemble ()
    (inst andi. temp value lowtag-mask)
    (inst cmpwi temp lowtag)
    (inst b? (if not-p :ne :eq) target)))

(defun %test-lowtag-and-headers (value target not-p lowtag function-p headers
                                 &key temp)
  (let ((drop-through (gen-label)))
    (%test-lowtag value (if not-p drop-through target) not-p lowtag
                  :temp temp)
    (%test-headers value target not-p function-p headers
                   :temp temp :drop-through drop-through)))

(defun %test-headers (value target not-p function-p headers
		      &key temp (drop-through (gen-label)))
    (let ((lowtag (if function-p fun-pointer-lowtag other-pointer-lowtag)))
    (multiple-value-bind (when-true when-false)
        (if not-p
            (values drop-through target)
            (values target drop-through))
      (assemble ()
        (%test-lowtag value when-false t lowtag :temp temp)
        (load-type temp value (- lowtag))
        (do ((remaining headers (cdr remaining)))
            ((null remaining))
          (let ((header (car remaining))
                (last (null (cdr remaining))))
            (cond
              ((atom header)
	       (inst cmpwi temp header)
               (if last
                   (inst b? (if not-p :ne :eq) target)
                   (inst beq when-true)))
              (t
               (let ((start (car header))
                     (end (cdr header)))
                 (unless (= start bignum-widetag)
                   (inst cmpwi temp start)
                   (inst blt when-false))
                 (inst cmpwi temp end)
                 (if last
                     (inst b? (if not-p :gt :le) target)
                     (inst ble when-true)))))))
        (emit-label drop-through)))))

;;; Simple type checking and testing:
(define-vop (check-type)
  (:args (value :target result :scs (any-reg descriptor-reg)))
  (:results (result :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:vop-var vop)
  (:save-p :compute-only))

(define-vop (type-predicate)
  (:args (value :scs (any-reg descriptor-reg)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:temporary (:scs (non-descriptor-reg)) temp))

(defun cost-to-test-types (type-codes)
  (+ (* 2 (length type-codes))
     (if (> (apply #'max type-codes) lowtag-limit) 7 2)))
  
(defmacro !define-type-vops (pred-name check-name ptype error-code
			     (&rest type-codes)
			     ;; KLUDGE: ideally, the compiler could
			     ;; derive that it can use the sneaky trap
			     ;; twice mechanism itself.  However, one
			     ;; thing at a time...
			     &key mask &allow-other-keys)
  (let ((cost (cost-to-test-types (mapcar #'eval type-codes))))
    `(progn
       ,@(when pred-name
  	   `((define-vop (,pred-name type-predicate)
	       (:translate ,pred-name)
	       (:generator ,cost
		 (test-type value target not-p (,@type-codes) :temp temp)))))
       ,@(when check-name
	   `((define-vop (,check-name check-type)
	       (:generator ,cost
		 ,@(if mask
		       `((inst andi. temp value ,mask)
			 (inst twi 0 value (error-number-or-lose ',error-code))
			 (inst twi :ne temp ,@(if ;; KLUDGE: At
					          ;; present, MASK is
					          ;; 3 or LOWTAG-MASK
					          (eql mask 3)
						  ;; KLUDGE
						  `(0)
						  type-codes))
			 (move result value))
		       `((let ((err-lab
				(generate-error-code vop ,error-code value)))
			   (test-type value err-lab t (,@type-codes) :temp temp)
			   (move result value))))))))
       ,@(when ptype
	   `((primitive-type-vop ,check-name (:check) ,ptype))))))

#|
  (def-type-vops fixnump nil nil object-not-fixnum-error
		 sb!vm:even-fixnum-lowtag sb!vm:odd-fixnum-lowtag)
  (define-vop (check-fixnum check-type)
      (:generator 3
		  (inst andi. temp value 3)
		  (inst twi 0 value (error-number-or-lose 'object-not-fixnum-error))
		  (inst twi :ne temp 0)
		  (move result value)))
  (primitive-type-vop check-fixnum (:check) fixnum)
  (def-type-vops functionp nil nil
		 object-not-fun-error sb!vm:fun-pointer-lowtag)
  
  (define-vop (check-fun check-type)
      (:generator 3
		  (inst andi. temp value 7)
		  (inst twi 0 value (error-number-or-lose 'object-not-fun-error))
		  (inst twi :ne temp sb!vm:fun-pointer-lowtag)
		  (move result value)))
  (primitive-type-vop check-fun (:check) function)
  
  (def-type-vops listp nil nil
		 object-not-list-error sb!vm:list-pointer-lowtag)
  (define-vop (check-list check-type)
      (:generator 3
		  (inst andi. temp value 7)
		  (inst twi 0 value (error-number-or-lose 'object-not-list-error))
		  (inst twi :ne temp sb!vm:list-pointer-lowtag)
		  (move result value)))
  (primitive-type-vop check-list (:check) list)
  
  (def-type-vops %instancep nil nil
		 object-not-instance-error sb!vm:instance-pointer-lowtag)
  (define-vop (check-instance check-type)
      (:generator 3
		  (inst andi. temp value 7)
		  (inst twi 0 value (error-number-or-lose 'object-not-instance-error))
		  (inst twi :ne temp sb!vm:instance-pointer-lowtag)
		  (move result value)))
  (primitive-type-vop check-instance (:check) instance)
  
  
  (def-type-vops bignump check-bignum bignum
		 object-not-bignum-error sb!vm:bignum-widetag)
  
  (def-type-vops ratiop check-ratio ratio
		 object-not-ratio-error sb!vm:ratio-widetag)
  
  (def-type-vops complexp check-complex complex
		 object-not-complex-error sb!vm:complex-widetag
		 complex-single-float-widetag complex-double-float-widetag)
  
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
  object-not-single-float-error sb!vm:single-float-widetag)

(def-type-vops double-float-p check-double-float double-float
  object-not-double-float-error sb!vm:double-float-widetag)

(def-type-vops simple-string-p check-simple-string simple-string
  object-not-simple-string-error sb!vm:simple-string-widetag)

(def-type-vops simple-bit-vector-p check-simple-bit-vector simple-bit-vector
  object-not-simple-bit-vector-error simple-bit-vector-widetag)

(def-type-vops simple-vector-p check-simple-vector simple-vector
  object-not-simple-vector-error sb!vm:simple-vector-widetag)

(def-type-vops simple-array-unsigned-byte-2-p
  check-simple-array-unsigned-byte-2
  simple-array-unsigned-byte-2
  object-not-simple-array-unsigned-byte-2-error
  sb!vm:simple-array-unsigned-byte-2-widetag)

(def-type-vops simple-array-unsigned-byte-4-p
  check-simple-array-unsigned-byte-4
  simple-array-unsigned-byte-4
  object-not-simple-array-unsigned-byte-4-error
  sb!vm:simple-array-unsigned-byte-4-widetag)

(def-type-vops simple-array-unsigned-byte-8-p
  check-simple-array-unsigned-byte-8
  simple-array-unsigned-byte-8
  object-not-simple-array-unsigned-byte-8-error
  sb!vm:simple-array-unsigned-byte-8-widetag)

(def-type-vops simple-array-unsigned-byte-16-p
  check-simple-array-unsigned-byte-16
  simple-array-unsigned-byte-16
  object-not-simple-array-unsigned-byte-16-error
  sb!vm:simple-array-unsigned-byte-16-widetag)

(def-type-vops simple-array-unsigned-byte-32-p
  check-simple-array-unsigned-byte-32
  simple-array-unsigned-byte-32
  object-not-simple-array-unsigned-byte-32-error
  sb!vm:simple-array-unsigned-byte-32-widetag)

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
  sb!vm:simple-array-single-float-widetag)

(def-type-vops simple-array-double-float-p check-simple-array-double-float
  simple-array-double-float object-not-simple-array-double-float-error
  sb!vm:simple-array-double-float-widetag)

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
  object-not-base-char-error sb!vm:base-char-widetag)

(def-type-vops system-area-pointer-p check-system-area-pointer
  system-area-pointer object-not-sap-error sb!vm:sap-widetag)

(def-type-vops weak-pointer-p check-weak-pointer weak-pointer
  object-not-weak-pointer-error sb!vm:weak-pointer-widetag)

(def-type-vops code-component-p nil nil nil
  sb!vm:code-header-widetag)

(def-type-vops lra-p nil nil nil
  sb!vm:return-pc-header-widetag)

(def-type-vops fdefn-p nil nil nil
  sb!vm:fdefn-widetag)

(def-type-vops funcallable-instance-p nil nil nil
  sb!vm:funcallable-instance-header-widetag)

(def-type-vops array-header-p nil nil nil
  sb!vm:simple-array-widetag sb!vm:complex-string-widetag sb!vm:complex-bit-vector-widetag
  sb!vm:complex-vector-widetag sb!vm:complex-array-widetag)

(def-type-vops nil check-function-or-symbol nil object-not-function-or-symbol-error
  sb!vm:fun-pointer-lowtag sb!vm:symbol-header-widetag)

(def-type-vops stringp check-string nil object-not-string-error
  sb!vm:simple-string-widetag sb!vm:complex-string-widetag)

(def-type-vops complex-vector-p check-complex-vector nil
 object-not-complex-vector-error complex-vector-widetag)

(def-type-vops bit-vector-p check-bit-vector nil object-not-bit-vector-error
  sb!vm:simple-bit-vector-widetag sb!vm:complex-bit-vector-widetag)

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
  sb!vm:even-fixnum-lowtag sb!vm:odd-fixnum-lowtag sb!vm:ratio-widetag sb!vm:bignum-widetag)

(def-type-vops integerp check-integer nil object-not-integer-error
  sb!vm:even-fixnum-lowtag sb!vm:odd-fixnum-lowtag sb!vm:bignum-widetag)

(def-type-vops floatp check-float nil object-not-float-error
  sb!vm:single-float-widetag sb!vm:double-float-widetag)

(def-type-vops realp check-real nil object-not-real-error
  sb!vm:even-fixnum-lowtag sb!vm:odd-fixnum-lowtag sb!vm:ratio-widetag sb!vm:bignum-widetag
  sb!vm:single-float-widetag sb!vm:double-float-widetag))
|#

;;;; Other integer ranges.

;;; A (signed-byte 32) can be represented with either fixnum or a bignum with
;;; exactly one digit.

(define-vop (signed-byte-32-p type-predicate)
  (:translate signed-byte-32-p)
  (:generator 45
    (let ((not-target (gen-label)))
      (multiple-value-bind
	  (yep nope)
	  (if not-p
	      (values not-target target)
	      (values target not-target))
	(inst andi. temp value #x3)
	(inst beq yep)
	(test-type value nope t (other-pointer-lowtag) :temp temp)
	(loadw temp value 0 other-pointer-lowtag)
	(inst cmpwi temp (+ (ash 1 n-widetag-bits)
			  bignum-widetag))
	(inst b? (if not-p :ne :eq) target)
	(emit-label not-target)))))

(define-vop (check-signed-byte-32 check-type)
  (:generator 45
    (let ((nope (generate-error-code vop object-not-signed-byte-32-error value))
	  (yep (gen-label)))
      (inst andi. temp value #x3)
      (inst beq yep)
      (test-type value nope t (other-pointer-lowtag) :temp temp)
      (loadw temp value 0 other-pointer-lowtag)
      (inst cmpwi temp (+ (ash 1 n-widetag-bits) bignum-widetag))
      (inst bne nope)
      (emit-label yep)
      (move result value))))


;;; An (unsigned-byte 32) can be represented with either a positive fixnum, a
;;; bignum with exactly one positive digit, or a bignum with exactly two digits
;;; and the second digit all zeros.

(define-vop (unsigned-byte-32-p type-predicate)
  (:translate unsigned-byte-32-p)
  (:generator 45
    (let ((not-target (gen-label))
	  (single-word (gen-label))
 	  (fixnum (gen-label)))
      (multiple-value-bind
	  (yep nope)
	  (if not-p
	      (values not-target target)
	      (values target not-target))
	;; Is it a fixnum?
	(inst andi. temp value #x3)
        (inst cmpwi :cr1 value 0)
        (inst beq fixnum)

	;; If not, is it an other pointer?
	(test-type value nope t (other-pointer-lowtag) :temp temp)
	;; Get the header.
	(loadw temp value 0 other-pointer-lowtag)
	;; Is it one?
	(inst cmpwi temp (+ (ash 1 n-widetag-bits) bignum-widetag))
	(inst beq single-word)
	;; If it's other than two, we can't be an (unsigned-byte 32)
	(inst cmpwi temp (+ (ash 2 n-widetag-bits) bignum-widetag))
	(inst bne nope)
	;; Get the second digit.
	(loadw temp value (1+ bignum-digits-offset) other-pointer-lowtag)
	;; All zeros, its an (unsigned-byte 32).
	(inst cmpwi temp 0)
	(inst beq yep)
	;; Otherwise, it isn't.
	(inst b nope)
	
	(emit-label single-word)
	;; Get the single digit.
	(loadw temp value bignum-digits-offset other-pointer-lowtag)
	(inst cmpwi :cr1 temp 0)

	;; positive implies (unsigned-byte 32).
	(emit-label fixnum)
	(inst b?  :cr1 (if not-p :lt :ge) target)

	(emit-label not-target)))))	  

(define-vop (check-unsigned-byte-32 check-type)
  (:generator 45
    (let ((nope
	   (generate-error-code vop object-not-unsigned-byte-32-error value))
	  (yep (gen-label))
	  (fixnum (gen-label))
	  (single-word (gen-label)))
      ;; Is it a fixnum?
      (inst andi. temp value #x3)
      (inst cmpwi :cr1 value 0)
      (inst beq fixnum)

      ;; If not, is it an other pointer?
      (test-type value nope t (other-pointer-lowtag) :temp temp)
      ;; Get the number of digits.
      (loadw temp value 0 other-pointer-lowtag)
      ;; Is it one?
      (inst cmpwi temp (+ (ash 1 n-widetag-bits) bignum-widetag))
      (inst beq single-word)
      ;; If it's other than two, we can't be an (unsigned-byte 32)
      (inst cmpwi temp (+ (ash 2 n-widetag-bits) bignum-widetag))
      (inst bne nope)
      ;; Get the second digit.
      (loadw temp value (1+ bignum-digits-offset) other-pointer-lowtag)
      ;; All zeros, its an (unsigned-byte 32).
      (inst cmpwi temp 0)
      (inst beq yep)
      ;; Otherwise, it isn't.
      (inst b nope)
      
      (emit-label single-word)
      ;; Get the single digit.
      (loadw temp value bignum-digits-offset other-pointer-lowtag)
      ;; positive implies (unsigned-byte 32).
      (inst cmpwi :cr1 temp 0)
      
      (emit-label fixnum)
      (inst blt :cr1 nope)
      
      (emit-label yep)
      (move result value))))




;;;; List/symbol types:
;;; 
;;; symbolp (or symbol (eq nil))
;;; consp (and list (not (eq nil)))

(define-vop (symbolp type-predicate)
  (:translate symbolp)
  (:generator 12
    (let* ((drop-thru (gen-label))
	   (is-symbol-label (if not-p drop-thru target)))
      (inst cmpw value null-tn)
      (inst beq is-symbol-label)
      (test-type value target not-p (symbol-header-widetag) :temp temp)
      (emit-label drop-thru))))

(define-vop (check-symbol check-type)
  (:generator 12
    (let ((drop-thru (gen-label))
	  (error (generate-error-code vop object-not-symbol-error value)))
      (inst cmpw value null-tn)
      (inst beq drop-thru)
      (test-type value error t (symbol-header-widetag) :temp temp)
      (emit-label drop-thru)
      (move result value))))
  
(define-vop (consp type-predicate)
  (:translate consp)
  (:generator 8
    (let* ((drop-thru (gen-label))
	   (is-not-cons-label (if not-p target drop-thru)))
      (inst cmpw value null-tn)
      (inst beq is-not-cons-label)
      (test-type value target not-p (list-pointer-lowtag) :temp temp)
      (emit-label drop-thru))))

(define-vop (check-cons check-type)
  (:generator 8
    (let ((error (generate-error-code vop object-not-cons-error value)))
      (inst cmpw value null-tn)
      (inst beq error)
      (test-type value error t (list-pointer-lowtag) :temp temp)
      (move result value))))

