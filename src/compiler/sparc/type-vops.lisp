;;;; type testing and checking VOPs for the Sparc VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; Simple type checking and testing:
;;;
;;; These types are represented by a single type code, so are easily
;;; open-coded as a mask and compare.
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

;;; moved to macros. FIXME.
;;;(defun cost-to-test-types (type-codes)
;;;  (+ (* 2 (length type-codes))
;;;     (if (> (apply #'max type-codes) lowtag-limit) 7 2)))
;;;
;;;(defparameter immediate-types
;;;  (list base-char-type unbound-marker-type))
;;;
;;;(defparameter function-header-types
;;;  (list funcallable-instance-header-type
;;;        byte-code-function-type byte-code-closure-type
;;;        function-header-type closure-function-header-type
;;;        closure-header-type))
;;;
;; FIXME: there's a canonicalize-headers in alpha/ and x86/

(defmacro def-type-vops (pred-name check-name ptype error-code
			 &rest type-codes)
  ;;; FIXME: #+sb-xc-host?
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
		   (move result value))))))
       ,@(when ptype
	   `((primitive-type-vop ,check-name (:check) ,ptype))))))

;;; This is a direct translation of the code in CMUCL
;;; compiler/sparc/macros.lisp. Don't blame me if it doesn't work.

;;; moved test-type back to macros.lisp, as other bits of code use it
;;; too. FIXME.




  
;; Don't use this because it uses the deprecated taddcctv instruction.
#+ignore
(progn
  (def-type-vops fixnump nil nil nil even-fixnum-lowtag odd-fixnum-lowtag)
  (define-vop (check-fixnum check-type)
      (:ignore temp)
    (:generator 1
		(inst taddcctv result value zero-tn)))
  (primitive-type-vop check-fixnum (:check) fixnum))
  
;; This avoids the taddcctv instruction
(def-type-vops fixnump check-fixnum fixnum object-not-fixnum-error
	       even-fixnum-lowtag odd-fixnum-lowtag)
(def-type-vops functionp check-fun function
	       object-not-fun-error fun-pointer-lowtag)
  
  ;; The following encode the error type and register in the trap
  ;; instruction, however this breaks on the later sparc Ultra.
  #+ignore
  (progn
    (def-type-vops listp nil nil nil list-pointer-lowtag)
    (define-vop (check-list check-type)
	(:generator 3
		    (inst and temp value lowtag-mask)
		    (inst cmp temp list-pointer-lowtag)
		    (inst t :ne (logior (ash (tn-offset value) 8) object-not-list-trap))
		    (move result value)))
    (primitive-type-vop check-list (:check) list)
    
    (def-type-vops %instancep nil nil nil instance-pointer-lowtag)
    (define-vop (check-instance check-type)
	(:generator 3
		    (inst and temp value lowtag-mask)
		    (inst cmp temp instance-pointer-lowtag)
		    (inst t :ne (logior (ash (tn-offset value) 8) object-not-instance-trap))
		    (move result value)))
    (primitive-type-vop check-instance (:check) instance))

  ;; These avoid the trap instruction.
  (def-type-vops listp check-list list object-not-list-error
  list-pointer-lowtag)
  (def-type-vops %instancep check-instance instance object-not-instance-error
  instance-pointer-lowtag)
      
  (def-type-vops bignump check-bignum bignum
  object-not-bignum-error bignum-widetag)
      
  (def-type-vops ratiop check-ratio ratio
  object-not-ratio-error ratio-widetag)
      
  (def-type-vops complexp check-complex complex object-not-complex-error
  complex-widetag complex-single-float-widetag
  complex-double-float-widetag #!+long-float complex-long-float-widetag)

  (def-type-vops complex-rational-p check-complex-rational nil
  object-not-complex-rational-error complex-widetag)

  (def-type-vops complex-float-p check-complex-float nil
  object-not-complex-float-error
  complex-single-float-widetag complex-double-float-widetag
  #!+long-float complex-long-float-widetag)

  (def-type-vops complex-single-float-p check-complex-single-float
  complex-single-float object-not-complex-single-float-error
  complex-single-float-widetag)

  (def-type-vops complex-double-float-p check-complex-double-float
  complex-double-float object-not-complex-double-float-error
  complex-double-float-widetag)

  #!+long-float
  (def-type-vops complex-long-float-p check-complex-long-float
  complex-long-float object-not-complex-long-float-error
  complex-long-float-widetag)

  (def-type-vops single-float-p check-single-float single-float
  object-not-single-float-error single-float-widetag)

  (def-type-vops double-float-p check-double-float double-float
  object-not-double-float-error double-float-widetag)

  #!+long-float
  (def-type-vops long-float-p check-long-float long-float
  object-not-long-float-error long-float-widetag)

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

  #!+long-float
  (def-type-vops simple-array-long-float-p check-simple-array-long-float
  simple-array-long-float object-not-simple-array-long-float-error
  simple-array-long-float-widetag)
      
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
      
  #!+long-float
  (def-type-vops simple-array-complex-long-float-p
  check-simple-array-complex-long-float
  simple-array-complex-long-float
  object-not-simple-array-complex-long-float-error
  simple-array-complex-long-float-widetag)

  (def-type-vops base-char-p check-base-char base-char
  object-not-base-char-error base-char-widetag)
      
  (def-type-vops system-area-pointer-p check-system-area-pointer
  system-area-pointer object-not-sap-error sap-widetag)
      
  (def-type-vops weak-pointer-p check-weak-pointer weak-pointer
  object-not-weak-pointer-error weak-pointer-widetag)
  ;; FIXME
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

  ;; This appears to have disappeared. FIXME -- CSR
  (def-type-vops nil check-fun-or-symbol nil object-not-fun-or-symbol-error
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
  #!+long-float simple-array-long-float-widetag
  simple-array-complex-single-float-widetag
  simple-array-complex-double-float-widetag
  #!+long-float simple-array-complex-long-float-widetag
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
  #!+long-float simple-array-long-float-widetag
  simple-array-complex-single-float-widetag
  simple-array-complex-double-float-widetag
  #!+long-float simple-array-complex-long-float-widetag)
      
  (def-type-vops arrayp check-array nil object-not-array-error
  simple-array-widetag simple-string-widetag simple-bit-vector-widetag
  simple-vector-widetag simple-array-unsigned-byte-2-widetag
  simple-array-unsigned-byte-4-widetag simple-array-unsigned-byte-8-widetag
  simple-array-unsigned-byte-16-widetag simple-array-unsigned-byte-32-widetag
  simple-array-signed-byte-8-widetag simple-array-signed-byte-16-widetag
  simple-array-signed-byte-30-widetag simple-array-signed-byte-32-widetag
  simple-array-single-float-widetag simple-array-double-float-widetag
  #!+long-float simple-array-long-float-widetag
  simple-array-complex-single-float-widetag
  simple-array-complex-double-float-widetag
  #!+long-float simple-array-complex-long-float-widetag
  complex-string-widetag complex-bit-vector-widetag complex-vector-widetag
  complex-array-widetag)
      
  (def-type-vops numberp check-number nil object-not-number-error
  even-fixnum-lowtag odd-fixnum-lowtag bignum-widetag ratio-widetag
  single-float-widetag double-float-widetag #!+long-float long-float-widetag
  complex-widetag complex-single-float-widetag complex-double-float-widetag
  #!+long-float complex-long-float-widetag)
      
  (def-type-vops rationalp check-rational nil object-not-rational-error
  even-fixnum-lowtag odd-fixnum-lowtag ratio-widetag bignum-widetag)
      
  (def-type-vops integerp check-integer nil object-not-integer-error
  even-fixnum-lowtag odd-fixnum-lowtag bignum-widetag)
      
  (def-type-vops floatp check-float nil object-not-float-error
  single-float-widetag double-float-widetag #!+long-float long-float-widetag)
      
  (def-type-vops realp check-real nil object-not-real-error
  even-fixnum-lowtag odd-fixnum-lowtag ratio-widetag bignum-widetag
  single-float-widetag double-float-widetag #!+long-float long-float-widetag)

  
;;;; Other integer ranges.

  ;; A (signed-byte 32) can be represented with either fixnum or a
  ;; bignum with exactly one digit.

  (define-vop (signed-byte-32-p type-predicate)
  (:translate signed-byte-32-p)
  (:generator 45
	      (let ((not-target (gen-label)))
		(multiple-value-bind
		      (yep nope)
		    (if not-p
			(values not-target target)
			(values target not-target))
		  (inst andcc zero-tn value #x3)
		  (inst b :eq yep)
		  (test-type value temp nope t other-pointer-lowtag)
		  (loadw temp value 0 other-pointer-lowtag)
		  (inst cmp temp (+ (ash 1 n-widetag-bits)
				    bignum-widetag))
		  (inst b (if not-p :ne :eq) target)
		  (inst nop)
		  (emit-label not-target)))))

  (define-vop (check-signed-byte-32 check-type)
  (:generator 45
	      (let ((nope (generate-error-code vop object-not-signed-byte-32-error value))
		    (yep (gen-label)))
		(inst andcc temp value #x3)
		(inst b :eq yep)
		(test-type value temp nope t other-pointer-lowtag)
		(loadw temp value 0 other-pointer-lowtag)
		(inst cmp temp (+ (ash 1 n-widetag-bits) bignum-widetag))
		(inst b :ne nope)
		(inst nop)
		(emit-label yep)
		(move result value))))


  ;; An (unsigned-byte 32) can be represented with either a
  ;; positive fixnum, a bignum with exactly one positive digit, or
  ;; a bignum with exactly two digits and the second digit all
  ;; zeros.

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
		  (inst andcc temp value #x3)
		  (inst b :eq fixnum)
		  (inst cmp value)

		  ;; If not, is it an other pointer?
		  (test-type value temp nope t other-pointer-lowtag)
		  ;; Get the header.
		  (loadw temp value 0 other-pointer-lowtag)
		  ;; Is it one?
		  (inst cmp temp (+ (ash 1 n-widetag-bits) bignum-widetag))
		  (inst b :eq single-word)
		  ;; If it's other than two, we can't be an
		  ;; (unsigned-byte 32)
		  (inst cmp temp (+ (ash 2 n-widetag-bits) bignum-widetag))
		  (inst b :ne nope)
		  ;; Get the second digit.
		  (loadw temp value (1+ bignum-digits-offset) other-pointer-lowtag)
		  ;; All zeros, its an (unsigned-byte 32).
		  (inst cmp temp)
		  (inst b :eq yep)
		  (inst nop)
		  ;; Otherwise, it isn't.
		  (inst b nope)
		  (inst nop)
			
		  (emit-label single-word)
		  ;; Get the single digit.
		  (loadw temp value bignum-digits-offset other-pointer-lowtag)
		  (inst cmp temp)
			
		  ;; positive implies (unsigned-byte 32).
		  (emit-label fixnum)
		  (inst b (if not-p :lt :ge) target)
		  (inst nop)
			
		  (emit-label not-target)))))	  

  (define-vop (check-unsigned-byte-32 check-type)
  (:generator 45
	      (let ((nope
		     (generate-error-code vop object-not-unsigned-byte-32-error value))
		    (yep (gen-label))
		    (fixnum (gen-label))
		    (single-word (gen-label)))
		;; Is it a fixnum?
		(inst andcc temp value #x3)
		(inst b :eq fixnum)
		(inst cmp value)
			
		;; If not, is it an other pointer?
		(test-type value temp nope t other-pointer-lowtag)
		;; Get the number of digits.
		(loadw temp value 0 other-pointer-lowtag)
		;; Is it one?
		(inst cmp temp (+ (ash 1 n-widetag-bits) bignum-widetag))
		(inst b :eq single-word)
		;; If it's other than two, we can't be an (unsigned-byte 32)
		(inst cmp temp (+ (ash 2 n-widetag-bits) bignum-widetag))
		(inst b :ne nope)
		;; Get the second digit.
		(loadw temp value (1+ bignum-digits-offset) other-pointer-lowtag)
		;; All zeros, its an (unsigned-byte 32).
		(inst cmp temp)
		(inst b :eq yep)
		;; Otherwise, it isn't.
		(inst b :ne nope)
		(inst nop)
			
		(emit-label single-word)
		;; Get the single digit.
		(loadw temp value bignum-digits-offset other-pointer-lowtag)
		;; positive implies (unsigned-byte 32).
		(inst cmp temp)
			
		(emit-label fixnum)
		(inst b :lt nope)
		(inst nop)
			
		(emit-label yep)
		(move result value))))


  
;;;; List/symbol types:

  ;; symbolp (or symbol (eq nil))
  ;; consp (and list (not (eq nil)))
      
  (define-vop (symbolp type-predicate)
  (:translate symbolp)
  (:generator 12
	      (let* ((drop-thru (gen-label))
		     (is-symbol-label (if not-p drop-thru target)))
		(inst cmp value null-tn)
		(inst b :eq is-symbol-label)
		(test-type value temp target not-p symbol-header-widetag)
		(emit-label drop-thru))))
      
  (define-vop (check-symbol check-type)
  (:generator 12
	      (let ((drop-thru (gen-label))
		    (error (generate-error-code vop object-not-symbol-error value)))
		(inst cmp value null-tn)
		(inst b :eq drop-thru)
		(test-type value temp error t symbol-header-widetag)
		(emit-label drop-thru)
		(move result value))))
      
  (define-vop (consp type-predicate)
  (:translate consp)
  (:generator 8
	      (let* ((drop-thru (gen-label))
		     (is-not-cons-label (if not-p target drop-thru)))
		(inst cmp value null-tn)
		(inst b :eq is-not-cons-label)
		(test-type value temp target not-p list-pointer-lowtag)
		(emit-label drop-thru))))
      
  (define-vop (check-cons check-type)
  (:generator 8
	      (let ((error (generate-error-code vop object-not-cons-error value)))
		(inst cmp value null-tn)
		(inst b :eq error)
		(test-type value temp error t list-pointer-lowtag)
		(move result value))))
