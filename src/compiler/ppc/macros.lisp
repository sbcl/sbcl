;;; 

(in-package "SB!VM")


;;; Instruction-like macros.

(defmacro move (dst src)
  "Move SRC into DST unless they are location=."
  (once-only ((n-dst dst)
	      (n-src src))
    `(unless (location= ,n-dst ,n-src)
       (inst mr ,n-dst ,n-src))))

(macrolet
    ((frob (op inst shift)
       `(defmacro ,op (object base &optional (offset 0) (lowtag 0))
	  `(inst ,',inst ,object ,base (- (ash ,offset ,,shift) ,lowtag)))))
  (frob loadw lwz word-shift)
  (frob storew stw word-shift))

(defmacro load-symbol (reg symbol)
  `(inst addi ,reg null-tn (static-symbol-offset ,symbol)))

(macrolet
    ((frob (slot)
       (let ((loader (intern (concatenate 'simple-string
					  "LOAD-SYMBOL-"
					  (string slot))))
	     (storer (intern (concatenate 'simple-string
					  "STORE-SYMBOL-"
					  (string slot))))
	     (offset (intern (concatenate 'simple-string
					  "SYMBOL-"
					  (string slot)
					  "-SLOT")
			     (find-package "SB!VM"))))
	 `(progn
	    (defmacro ,loader (reg symbol)
	      `(inst lwz ,reg null-tn
		     (+ (static-symbol-offset ',symbol)
			(ash ,',offset word-shift)
			(- other-pointer-lowtag))))
	    (defmacro ,storer (reg symbol)
	      `(inst stw ,reg null-tn
		     (+ (static-symbol-offset ',symbol)
			(ash ,',offset word-shift)
			(- other-pointer-lowtag))))))))
  (frob value)
  (frob function))

(defmacro load-type (target source &optional (offset 0))
  "Loads the type bits of a pointer into target independent of
  byte-ordering issues."
  (once-only ((n-target target)
	      (n-source source)
	      (n-offset offset))
    (ecase *backend-byte-order*
      (:little-endian
       `(inst lbz ,n-target ,n-source ,n-offset))
      (:big-endian
       `(inst lbz ,n-target ,n-source (+ ,n-offset 3))))))

;;; Macros to handle the fact that we cannot use the machine native call and
;;; return instructions. 

(defmacro lisp-jump (function lip)
  "Jump to the lisp function FUNCTION.  LIP is an interior-reg temporary."
  `(progn
    ;; something is deeply bogus.  look at this
    ;; (loadw ,lip ,function sb!vm:function-code-offset sb!vm:function-pointer-type)
    (inst addi ,lip ,function (- (* n-word-bytes sb!vm:simple-fun-code-offset) sb!vm:fun-pointer-lowtag))
    (inst mtctr ,lip)
    (move code-tn ,function)
    (inst bctr)))

(defmacro lisp-return (return-pc lip &key (offset 0) (frob-code t))
  "Return to RETURN-PC."
  `(progn
     (inst addi ,lip ,return-pc (- (* (1+ ,offset) n-word-bytes) other-pointer-lowtag))
     (inst mtlr ,lip)
     ,@(if frob-code
         `((move code-tn ,return-pc)))
     (inst blr)))

(defmacro emit-return-pc (label)
  "Emit a return-pc header word.  LABEL is the label to use for this return-pc."
  `(progn
     (align n-lowtag-bits)
     (emit-label ,label)
     (inst lra-header-word)))



;;;; Stack TN's

;;; Load-Stack-TN, Store-Stack-TN  --  Interface
;;;
;;;    Move a stack TN to a register and vice-versa.
;;;
(defmacro load-stack-tn (reg stack)
  `(let ((reg ,reg)
	 (stack ,stack))
     (let ((offset (tn-offset stack)))
       (sc-case stack
	 ((control-stack)
	  (loadw reg cfp-tn offset))))))

(defmacro store-stack-tn (stack reg)
  `(let ((stack ,stack)
	 (reg ,reg))
     (let ((offset (tn-offset stack)))
       (sc-case stack
	 ((control-stack)
	  (storew reg cfp-tn offset))))))


;;; MAYBE-LOAD-STACK-TN  --  Interface
;;;
(defmacro maybe-load-stack-tn (reg reg-or-stack)
  "Move the TN Reg-Or-Stack into Reg if it isn't already there."
  (once-only ((n-reg reg)
	      (n-stack reg-or-stack))
    `(sc-case ,n-reg
       ((any-reg descriptor-reg)
	(sc-case ,n-stack
	  ((any-reg descriptor-reg)
	   (move ,n-reg ,n-stack))
	  ((control-stack)
	   (loadw ,n-reg cfp-tn (tn-offset ,n-stack))))))))


;;;; Storage allocation:

(defmacro with-fixed-allocation ((result-tn flag-tn temp-tn type-code size)
				 &body body)
  "Do stuff to allocate an other-pointer object of fixed Size with a single
  word header having the specified Type-Code.  The result is placed in
  Result-TN, and Temp-TN is a non-descriptor temp (which may be randomly used
  by the body.)  The body is placed inside the PSEUDO-ATOMIC, and presumably
  initializes the object."
  (once-only ((result-tn result-tn) (temp-tn temp-tn) (flag-tn flag-tn)
	      (type-code type-code) (size size))
    `(pseudo-atomic (,flag-tn :extra (pad-data-block ,size))
       (inst ori ,result-tn alloc-tn other-pointer-lowtag)
       (inst lr ,temp-tn (logior (ash (1- ,size) n-widetag-bits) ,type-code))
       (storew ,temp-tn ,result-tn 0 other-pointer-lowtag)
       ,@body)))


;;;; Type testing noise.

;;; GEN-RANGE-TEST -- internal
;;;
;;; Generate code that branches to TARGET iff REG contains one of VALUES.
;;; If NOT-P is true, invert the test.  Jumping to NOT-TARGET is the same
;;; as falling out the bottom.
;;; 
(defun gen-range-test (reg target not-target not-p min seperation max values)
  (let ((tests nil)
	(start nil)
	(end nil)
	(insts nil))
    (multiple-value-bind (equal less-or-equal greater-or-equal label)
			 (if not-p
			     (values :ne :gt :lt not-target)
			     (values :eq :le :ge target))
      (flet ((emit-test ()
	       (if (= start end)
		   (push start tests)
		   (push (cons start end) tests))))
	(dolist (value values)
	  (cond ((< value min)
		 (error "~S is less than the specified minimum of ~S"
			value min))
		((> value max)
		 (error "~S is greater than the specified maximum of ~S"
			value max))
		((not (zerop (rem (- value min) seperation)))
		 (error "~S isn't an even multiple of ~S from ~S"
			value seperation min))
		((null start)
		 (setf start value))
		((> value (+ end seperation))
		 (emit-test)
		 (setf start value)))
	  (setf end value))
	(emit-test))
      (macrolet ((inst (name &rest args)
		       `(push (list 'inst ',name ,@args) insts)))
	(do ((remaining (nreverse tests) (cdr remaining)))
	    ((null remaining))
	  (let ((test (car remaining))
		(last (null (cdr remaining))))
	    (if (atom test)
		(progn
		  (inst cmpwi reg test)
		  (if last
		      (inst b? equal target)
		      (inst beq label)))
		(let ((start (car test))
		      (end (cdr test)))
		  (cond ((and (= start min) (= end max))
			 (warn "The values ~S cover the entire range from ~
			 ~S to ~S [step ~S]."
			       values min max seperation)
			 (push `(unless ,not-p (inst b ,target)) insts))
			((= start min)
			 (inst cmpwi reg end)
			 (if last
			     (inst b? less-or-equal target)
			     (inst ble label)))
			((= end max)
			 (inst cmpwi reg start)
			 (if last
			     (inst b? greater-or-equal target)
			     (inst bge label)))
			(t
			 (inst cmpwi reg start)
			 (inst blt (if not-p target not-target))
			 (inst cmpwi reg end)
			 (if last
			     (inst b? less-or-equal target)
			     (inst ble label))))))))))
    (nreverse insts)))

(defun gen-other-immediate-test (reg target not-target not-p values)
  (gen-range-test reg target not-target not-p
		  (+ other-immediate-0-lowtag lowtag-limit)
		  (- other-immediate-1-lowtag other-immediate-0-lowtag)
		  (ash 1 n-widetag-bits)
		  values))


(defun test-type-aux (reg temp target not-target not-p lowtags immed hdrs
			  function-p)
  (let* ((fixnump (and (member even-fixnum-lowtag lowtags :test #'eql)
		       (member odd-fixnum-lowtag lowtags :test #'eql)))
	 (lowtags (sort (if fixnump
			    (delete even-fixnum-lowtag
				    (remove odd-fixnum-lowtag lowtags
					    :test #'eql)
				    :test #'eql)
			    (copy-list lowtags))
			#'<))
	 (lowtag (if function-p
		     sb!vm:fun-pointer-lowtag
		     sb!vm:other-pointer-lowtag))
	 (hdrs (sort (copy-list hdrs) #'<))
	 (immed (sort (copy-list immed) #'<)))
    (append
     (when immed
       `((inst andi. ,temp ,reg widetag-mask)
	 ,@(if (or fixnump lowtags hdrs)
	       (let ((fall-through (gensym)))
		 `((let (,fall-through (gen-label))
		     ,@(gen-other-immediate-test
			temp (if not-p not-target target)
			fall-through nil immed)
		     (emit-label ,fall-through))))
	       (gen-other-immediate-test temp target not-target not-p immed))))
     (when fixnump
       `((inst andi. ,temp ,reg 3)
	 ,(if (or lowtags hdrs)
	      `(inst beq ,(if not-p not-target target))
	      `(inst b? ,(if not-p :ne :eq) ,target))))
     (when (or lowtags hdrs)
       `((inst andi. ,temp ,reg lowtag-mask)))
     (when lowtags
       (if hdrs
	   (let ((fall-through (gensym)))
	     `((let ((,fall-through (gen-label)))
		 ,@(gen-range-test temp (if not-p not-target target)
				   fall-through nil
				   0 1 (1- lowtag-limit) lowtags)
		 (emit-label ,fall-through))))
	   (gen-range-test temp target not-target not-p 0 1
			   (1- lowtag-limit) lowtags)))
     (when hdrs
       `((inst cmpwi ,temp ,lowtag)
	 (inst bne ,(if not-p target not-target))
	 (load-type ,temp ,reg (- ,lowtag))
	 ,@(gen-other-immediate-test temp target not-target not-p hdrs))))))

(defparameter immediate-types
  (list base-char-widetag unbound-marker-widetag))

(defparameter function-subtypes
  (list funcallable-instance-header-widetag
	simple-fun-header-widetag closure-fun-header-widetag
	closure-header-widetag))

(defmacro test-type (register temp target not-p &rest type-codes)
  (let* ((type-codes (mapcar #'eval type-codes))
	 (lowtags (remove lowtag-limit type-codes :test #'<))
	 (extended (remove lowtag-limit type-codes :test #'>))
	 (immediates (intersection extended immediate-types :test #'eql))
	 (headers (set-difference extended immediate-types :test #'eql))
	 (function-p nil))
    (unless type-codes
      (error "Must supply at least on type for test-type."))
    (when (and headers (member other-pointer-lowtag lowtags))
      (warn "OTHER-POINTER-LOWTAG supersedes the use of ~S" headers)
      (setf headers nil))
    (when (and immediates
	       (or (member other-immediate-0-lowtag lowtags)
		   (member other-immediate-1-lowtag lowtags)))
      (warn "OTHER-IMMEDIATE-n-LOWTAG supersedes the use of ~S" immediates)
      (setf immediates nil))
    (when (intersection headers function-subtypes)
      (unless (subsetp headers function-subtypes)
	(error "Can't test for mix of function subtypes and normal ~
		header types."))
      (setq function-p t))
      
    (let ((n-reg (gensym))
	  (n-temp (gensym))
	  (n-target (gensym))
	  (not-target (gensym)))
      `(let ((,n-reg ,register)
	     (,n-temp ,temp)
	     (,n-target ,target)
	     (,not-target (gen-label)))
	 (declare (ignorable ,n-temp))
	 ,@(if (constantp not-p)
	       (test-type-aux n-reg n-temp n-target not-target
			      (eval not-p) lowtags immediates headers
			      function-p)
	       `((cond (,not-p
			,@(test-type-aux n-reg n-temp n-target not-target t
					 lowtags immediates headers
					 function-p))
		       (t
			,@(test-type-aux n-reg n-temp n-target not-target nil
					 lowtags immediates headers
					 function-p)))))
	 (emit-label ,not-target)))))


;;;; Error Code

(defvar *adjustable-vectors* nil)

(defmacro with-adjustable-vector ((var) &rest body)
  `(let ((,var (or (pop *adjustable-vectors*)
		   (make-array 16
			       :element-type '(unsigned-byte 8)
			       :fill-pointer 0
			       :adjustable t))))
     (setf (fill-pointer ,var) 0)
     (unwind-protect
	 (progn
	   ,@body)
       (push ,var *adjustable-vectors*))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun emit-error-break (vop kind code values)
    (let ((vector (gensym)))
      `((let ((vop ,vop))
	  (when vop
	    (note-this-location vop :internal-error)))
	(inst unimp ,kind)
	(with-adjustable-vector (,vector)
	  (write-var-integer (error-number-or-lose ',code) ,vector)
	  ,@(mapcar #'(lambda (tn)
			`(let ((tn ,tn))
			   (write-var-integer (make-sc-offset (sc-number
							       (tn-sc tn))
							      (tn-offset tn))
					      ,vector)))
		    values)
	  (inst byte (length ,vector))
	  (dotimes (i (length ,vector))
	    (inst byte (aref ,vector i))))
	(align word-shift)))))

(defmacro error-call (vop error-code &rest values)
  "Cause an error.  ERROR-CODE is the error to cause."
  (cons 'progn
	(emit-error-break vop error-trap error-code values)))


(defmacro cerror-call (vop label error-code &rest values)
  "Cause a continuable error.  If the error is continued, execution resumes at
  LABEL."
  `(progn
     ,@(emit-error-break vop cerror-trap error-code values)
     (inst b ,label)))

(defmacro generate-error-code (vop error-code &rest values)
  "Generate-Error-Code Error-code Value*
  Emit code for an error with the specified Error-Code and context Values."
  `(assemble (*elsewhere*)
     (let ((start-lab (gen-label)))
       (emit-label start-lab)
       (error-call ,vop ,error-code ,@values)
       start-lab)))

(defmacro generate-cerror-code (vop error-code &rest values)
  "Generate-CError-Code Error-code Value*
  Emit code for a continuable error with the specified Error-Code and
  context Values.  If the error is continued, execution resumes after
  the GENERATE-CERROR-CODE form."
  (let ((continue (gensym "CONTINUE-LABEL-"))
	(error (gensym "ERROR-LABEL-")))
    `(let ((,continue (gen-label)))
       (emit-label ,continue)
       (assemble (*elsewhere*)
	 (let ((,error (gen-label)))
	   (emit-label ,error)
	   (cerror-call ,vop ,continue ,error-code ,@values)
	   ,error)))))



;;; PSEUDO-ATOMIC -- Handy macro for making sequences look atomic.
;;;
;;; flag-tn must be wired to NL3. If a deferred interrupt happens
;;; while we have the low bits of alloc-tn set, we add a "large"
;;; constant to flag-tn.  On exit, we add flag-tn to alloc-tn
;;; which (a) aligns alloc-tn again and (b) makes alloc-tn go
;;; negative.  We then trap if alloc-tn's negative (handling the
;;; deferred interrupt) and using flag-tn - minus the large constant -
;;; to correct alloc-tn.
(defmacro pseudo-atomic ((flag-tn &key (extra 0)) &rest forms)
  (let ((n-extra (gensym)))
    `(let ((,n-extra ,extra))
       (without-scheduling ()
	;; Extra debugging stuff:
	#+debug
	(progn
	  (inst andi. ,flag-tn alloc-tn 7)
	  (inst twi :ne ,flag-tn 0))
	(inst lr ,flag-tn (- ,n-extra 4))
	(inst addi alloc-tn alloc-tn 4))
      ,@forms
      (without-scheduling ()
       (inst add alloc-tn alloc-tn ,flag-tn)
       (inst twi :lt alloc-tn 0))
      #+debug
      (progn
	(inst andi. ,flag-tn alloc-tn 7)
	(inst twi :ne ,flag-tn 0)))))



