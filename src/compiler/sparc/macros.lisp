;;;; various useful macros for generating Sparc code

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;; Instruction-like macros.

(defmacro move (dst src)
  "Move SRC into DST unless they are location=."
  (once-only ((n-dst dst)
	      (n-src src))
    `(unless (location= ,n-dst ,n-src)
       (inst move ,n-dst ,n-src))))

(macrolet
    ((frob (op inst shift)
       `(defmacro ,op (object base &optional (offset 0) (lowtag 0))
	  `(inst ,',inst ,object ,base (- (ash ,offset ,,shift) ,lowtag)))))
  (frob loadw ld word-shift)
  (frob storew st word-shift))

(defmacro load-symbol (reg symbol)
  `(inst add ,reg null-tn (static-symbol-offset ,symbol)))

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
	      `(inst ld ,reg null-tn
		     (+ (static-symbol-offset ',symbol)
			(ash ,',offset word-shift)
			(- other-pointer-lowtag))))
	    (defmacro ,storer (reg symbol)
	      `(inst st ,reg null-tn
		     (+ (static-symbol-offset ',symbol)
			(ash ,',offset word-shift)
			(- other-pointer-lowtag))))))))
  (frob value)
  (frob function))

(defmacro load-type (target source &optional (offset 0))
  #!+sb-doc
  "Loads the type bits of a pointer into target independent of
  byte-ordering issues."
  (once-only ((n-target target)
	      (n-source source)
	      (n-offset offset))
    ;; FIXME: although I don't understand entirely, I'm going to do
    ;; what whn does in x86/macros.lisp -- Christophe
    (ecase *backend-byte-order*
      (:little-endian
       `(inst ldub ,n-target ,n-source ,n-offset))
      (:big-endian
       `(inst ldub ,n-target ,n-source (+ ,n-offset 3))))))

;;; Macros to handle the fact that we cannot use the machine native call and
;;; return instructions. 

(defmacro lisp-jump (fun)
  "Jump to the lisp function FUNCTION.  LIP is an interior-reg temporary."
  `(progn
     (inst j ,fun
	   (- (ash simple-fun-code-offset word-shift) fun-pointer-lowtag))
     (move code-tn ,fun)))

(defmacro lisp-return (return-pc &key (offset 0) (frob-code t))
  "Return to RETURN-PC."
  `(progn
     (inst j ,return-pc
	   (- (* (1+ ,offset) n-word-bytes) other-pointer-lowtag))
     ,(if frob-code
	  `(move code-tn ,return-pc)
	  '(inst nop))))

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

(defmacro with-fixed-allocation ((result-tn temp-tn type-code size)
				 &body body)
  "Do stuff to allocate an other-pointer object of fixed Size with a single
  word header having the specified Type-Code.  The result is placed in
  Result-TN, and Temp-TN is a non-descriptor temp (which may be randomly used
  by the body.)  The body is placed inside the PSEUDO-ATOMIC, and presumably
  initializes the object."
  (once-only ((result-tn result-tn) (temp-tn temp-tn)
	      (type-code type-code) (size size))
    `(pseudo-atomic (:extra (pad-data-block ,size))
       (inst or ,result-tn alloc-tn other-pointer-lowtag)
       (inst li ,temp-tn (logior (ash (1- ,size) n-widetag-bits) ,type-code))
       (storew ,temp-tn ,result-tn 0 other-pointer-lowtag)
       ,@body)))


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
     (inst b ,label)
     ,@(emit-error-break vop cerror-trap error-code values)))

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



;;; a handy macro for making sequences look atomic
(defmacro pseudo-atomic ((&key (extra 0)) &rest forms)
  (let ((n-extra (gensym)))
    `(let ((,n-extra ,extra))
       ;; Set the pseudo-atomic flag.
       (without-scheduling ()
	 (inst add alloc-tn 4))
       ,@forms
       ;; Reset the pseudo-atomic flag.
       (without-scheduling ()
	 #+nil (inst taddcctv alloc-tn (- ,n-extra 4))
	;; Remove the pseudo-atomic flag.
	(inst add alloc-tn (- ,n-extra 4))
	;; Check to see if pseudo-atomic interrupted flag is set (bit 0 = 1).
	(inst andcc zero-tn alloc-tn 3)
	;; The C code needs to process this correctly and fixup alloc-tn.
	(inst t :ne pseudo-atomic-trap)))))

;;; FIXME: test typing macros. Should(?) be in type-vops.lisp, except
;;; that they're also used in subprim.lisp

(defun cost-to-test-types (type-codes)
  (+ (* 2 (length type-codes))
     (if (> (apply #'max type-codes) lowtag-limit) 7 2)))

(defparameter *immediate-types*
  (list base-char-widetag unbound-marker-widetag))

(defparameter *fun-header-widetags*
  (list funcallable-instance-header-widetag
	simple-fun-header-widetag
	closure-fun-header-widetag
	closure-header-widetag))

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
		  (inst cmp reg test)
		  (if last
		      (inst b equal target)
		      (inst b :eq label)))
		(let ((start (car test))
		      (end (cdr test)))
		  (cond ((and (= start min) (= end max))
			 (warn "The values ~S cover the entire range from ~
			 ~S to ~S [step ~S]."
			       values min max seperation)
			 (push `(unless ,not-p (inst b ,target)) insts))
			((= start min)
			 (inst cmp reg end)
			 (if last
			     (inst b less-or-equal target)
			     (inst b :le label)))
			((= end max)
			 (inst cmp reg start)
			 (if last
			     (inst b greater-or-equal target)
			     (inst b :ge label)))
			(t
			 (inst cmp reg start)
			 (inst b :lt (if not-p target not-target))
			 (inst cmp reg end)
			 (if last
			     (inst b less-or-equal target)
			     (inst b :le label))))))))))
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
		     fun-pointer-lowtag
		     other-pointer-lowtag))
	 (hdrs (sort (copy-list hdrs) #'<))
	 (immed (sort (copy-list immed) #'<)))
    (append
     (when immed
       `((inst and ,temp ,reg widetag-mask)
	 ,@(if (or fixnump lowtags hdrs)
	       (let ((fall-through (gensym)))
		 `((let (,fall-through (gen-label))
		     ,@(gen-other-immediate-test
			temp (if not-p not-target target)
			fall-through nil immed)
		     (emit-label ,fall-through))))
	       (gen-other-immediate-test temp target not-target not-p immed))))
     (when fixnump
       `((inst andcc zero-tn ,reg fixnum-tag-mask)
	 ,(if (or lowtags hdrs)
	      `(if (member :sparc-v9 *backend-subfeatures*)
   		   (inst b :eq ,(if not-p not-target target) ,(if not-p :pn :pt))
		   (inst b :eq ,(if not-p not-target target)))
	      `(if (member :sparc-v9 *backend-subfeatures*)
		   (inst b ,(if not-p :ne :eq) ,target ,(if not-p :pn :pt))
		   (inst b ,(if not-p :ne :eq) ,target)))))
     (when (or lowtags hdrs)
       `((inst and ,temp ,reg lowtag-mask)))
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
       `((inst cmp ,temp ,lowtag)
	 (if (member :sparc-v9 *backend-subfeatures*)
	     (inst b :ne ,(if not-p target not-target) ,(if not-p :pn :pt))
	     (inst b :ne ,(if not-p target not-target)))
	 (inst nop)
	 (load-type ,temp ,reg (- ,lowtag))
	 ,@(gen-other-immediate-test temp target not-target not-p hdrs))))))

(defmacro test-type (register temp target not-p &rest type-codes)
  (let* ((type-codes (mapcar #'eval type-codes))
	 (lowtags (remove lowtag-limit type-codes :test #'<))
	 (extended (remove lowtag-limit type-codes :test #'>))
	 (immediates (intersection extended *immediate-types* :test #'eql))
	 (headers (set-difference extended *immediate-types* :test #'eql))
	 (function-p nil))
    (unless type-codes
      (error "Must supply at least on type for test-type."))
    (when (and headers (member other-pointer-lowtag lowtags))
      (warn "OTHER-POINTER-TYPE supersedes the use of ~S" headers)
      (setf headers nil))
    (when (and immediates
	       (or (member other-immediate-0-lowtag lowtags)
		   (member other-immediate-1-lowtag lowtags)))
      (warn "OTHER-IMMEDIATE-n-TYPE supersedes the use of ~S" immediates)
      (setf immediates nil))
    (when (intersection headers *fun-header-widetags*)
      (unless (subsetp headers *fun-header-widetags*)
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
	(inst nop)
	(emit-label ,not-target)))))
