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

;;; Emit the most compact form of the test immediate instruction,
;;; using an 8 bit test when the immediate is only 8 bits and the
;;; value is one of the four low registers (eax, ebx, ecx, edx) or the
;;; control stack.
(defun generate-fixnum-test (value)
  "zero flag set if VALUE is fixnum"
  (let ((offset (tn-offset value)))
    (cond ((and (sc-is value any-reg descriptor-reg)
		(or (= offset eax-offset) (= offset ebx-offset)
		    (= offset ecx-offset) (= offset edx-offset)))
	   (inst test (make-random-tn :kind :normal
				      :sc (sc-or-lose 'byte-reg)
				      :offset offset)
		 7))
	  ((sc-is value control-stack)
	   (inst test (make-ea :byte :base rbp-tn
			       :disp (- (* (1+ offset) n-word-bytes)))
		 7))
	  (t
	   (inst test value 7)))))

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
		(or (= offset rax-offset) (= offset rbx-offset)
		    (= offset rcx-offset) (= offset rdx-offset)))
	   (inst cmp (make-random-tn :kind :normal
				     :sc (sc-or-lose 'byte-reg)
				     :offset offset)
		 immediate))
	  (t
	   (move rax-tn value)
	   (inst cmp al-tn immediate))))
  (inst jmp (if not-p :ne :e) target))

(defun %test-lowtag (value target not-p lowtag &optional al-loaded)
  (unless al-loaded
    (move rax-tn value)
    (inst and al-tn lowtag-mask))
  (inst cmp al-tn lowtag)
  (inst jmp (if not-p :ne :e) target))

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
	       (unless (= start bignum-widetag)
		 (inst cmp al-tn start)
		 (inst jmp :b when-false)) ; was :l
	       (inst cmp al-tn end)
	       (if last
		   (inst jmp less-or-equal target)
		   (inst jmp :be when-true))))))) ; was :le
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

(defun cost-to-test-types (type-codes)
  (+ (* 2 (length type-codes))
     (if (> (apply #'max type-codes) lowtag-limit) 7 2)))

(defmacro !define-type-vops (pred-name check-name ptype error-code
			     (&rest type-codes)
			     &key (variant nil variant-p) &allow-other-keys)
  ;; KLUDGE: UGH. Why do we need this eval? Can't we put this in the
  ;; expansion?
  (let* ((cost (cost-to-test-types (mapcar #'eval type-codes)))
	 (prefix (if variant-p
		     (concatenate 'string (string variant) "-")
		     "")))
    `(progn
       ,@(when pred-name
	   `((define-vop (,pred-name ,(intern (concatenate 'string prefix "TYPE-PREDICATE")))
	       (:translate ,pred-name)
	       (:generator ,cost
		 (test-type value target not-p (,@type-codes))))))
       ,@(when check-name
	   `((define-vop (,check-name ,(intern (concatenate 'string prefix "CHECK-TYPE")))
	       (:generator ,cost
		 (let ((err-lab
			(generate-error-code vop ,error-code value)))
		   (test-type value err-lab t (,@type-codes))
		   (move result value))))))
       ,@(when ptype
	   `((primitive-type-vop ,check-name (:check) ,ptype))))))

;;;; other integer ranges

(define-vop (fixnump/unsigned-byte-64 simple-type-predicate)
  (:args (value :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:translate fixnump)
  (:temporary (:sc unsigned-reg) tmp)
  (:generator 5
    (inst mov tmp value)
    (inst shr tmp 61)
    (inst jmp (if not-p :nz :z) target)))

(define-vop (signed-byte-32-p type-predicate)
  (:translate signed-byte-32-p)
  (:generator 7
    ;; (and (fixnum) (or (no bits set >31) (all bits set >31))
    (move rax-tn value)
    (inst test rax-tn 7)
    (inst jmp :ne (if not-p target not-target))
    (inst sar rax-tn (+ 32 3 -1))    
    (inst jmp (if not-p :nz :z) target)
    (inst cmp rax-tn -1)
    (inst jmp (if not-p :ne :eq) target)
    NOT-TARGET))

(define-vop (check-signed-byte-32 check-type)
  (:generator 8
    (let ((nope (generate-error-code vop
				     object-not-signed-byte-32-error
				     value))
	  (ok (gen-label)))
      (move rax-tn value)
      (inst test rax-tn 7)
      (inst jmp :ne nope)
      (inst sar rax-tn (+ 32 3 -1))      
      (inst jmp :z ok)
      (inst cmp rax-tn -1)
      (inst jmp :ne nope)
      (emit-label OK)
      (move result value))))


(define-vop (unsigned-byte-32-p type-predicate)
  (:translate unsigned-byte-32-p)
  (:generator 7
    ;; (and (fixnum) (no bits set >31))
    (move rax-tn value)
    (inst test rax-tn 7)
    (inst jmp :ne (if not-p target not-target))
    (inst shr rax-tn (+ 32 sb!vm::n-fixnum-tag-bits))
    (inst jmp (if not-p :nz :z) target)
    NOT-TARGET))

(define-vop (check-unsigned-byte-32 check-type)
  (:generator 8
    (let ((nope
	   (generate-error-code vop object-not-unsigned-byte-32-error value)))
      (move rax-tn value)
      (inst test rax-tn 7)
      (inst jmp :ne nope)
      (inst shr rax-tn (+ 32 sb!vm::n-fixnum-tag-bits))
      (inst jmp :nz nope)
      (move result value))))

;;; An (unsigned-byte 64) can be represented with either a positive
;;; fixnum, a bignum with exactly one positive digit, or a bignum with
;;; exactly two digits and the second digit all zeros.
(define-vop (unsigned-byte-64-p type-predicate)
  (:translate unsigned-byte-64-p)
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
	(inst cmp eax-tn (+ (ash 1 n-widetag-bits) bignum-widetag))
	(inst jmp :e single-word)
	;; If it's other than two, we can't be an (unsigned-byte 64)
	(inst cmp eax-tn (+ (ash 2 n-widetag-bits) bignum-widetag))
	(inst jmp :ne nope)
	;; Get the second digit.
	(loadw eax-tn value (1+ bignum-digits-offset) other-pointer-lowtag)
	;; All zeros, its an (unsigned-byte 64).
	(inst or eax-tn eax-tn)
	(inst jmp :z yep)
	(inst jmp nope)
	
	(emit-label single-word)
	;; Get the single digit.
	(loadw eax-tn value bignum-digits-offset other-pointer-lowtag)

	;; positive implies (unsigned-byte 64).
	(emit-label fixnum)
	(inst or eax-tn eax-tn)
	(inst jmp (if not-p :s :ns) target)

	(emit-label not-target)))))

(define-vop (check-unsigned-byte-64 check-type)
  (:generator 45
    (let ((nope
	   (generate-error-code vop object-not-unsigned-byte-64-error value))
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
      (inst cmp eax-tn (+ (ash 1 n-widetag-bits) bignum-widetag))
      (inst jmp :e single-word)
      ;; If it's other than two, we can't be an (unsigned-byte 64)
      (inst cmp eax-tn (+ (ash 2 n-widetag-bits) bignum-widetag))
      (inst jmp :ne nope)
      ;; Get the second digit.
      (loadw eax-tn value (1+ bignum-digits-offset) other-pointer-lowtag)
      ;; All zeros, its an (unsigned-byte 64).
      (inst or eax-tn eax-tn)
      (inst jmp :z yep)
      (inst jmp nope)
	
      (emit-label single-word)
      ;; Get the single digit.
      (loadw eax-tn value bignum-digits-offset other-pointer-lowtag)

      ;; positive implies (unsigned-byte 64).
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
      (test-type value target not-p (symbol-header-widetag)))
    DROP-THRU))

(define-vop (check-symbol check-type)
  (:generator 12
    (let ((error (generate-error-code vop object-not-symbol-error value)))
      (inst cmp value nil-value)
      (inst jmp :e drop-thru)
      (test-type value error t (symbol-header-widetag)))
    DROP-THRU
    (move result value)))

(define-vop (consp type-predicate)
  (:translate consp)
  (:generator 8
    (let ((is-not-cons-label (if not-p target drop-thru)))
      (inst cmp value nil-value)
      (inst jmp :e is-not-cons-label)
      (test-type value target not-p (list-pointer-lowtag)))
    DROP-THRU))

(define-vop (check-cons check-type)
  (:generator 8
    (let ((error (generate-error-code vop object-not-cons-error value)))
      (inst cmp value nil-value)
      (inst jmp :e error)
      (test-type value error t (list-pointer-lowtag))
      (move result value))))
