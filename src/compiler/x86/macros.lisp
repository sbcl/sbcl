;;;; a bunch of handy macros for the x86

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;; We can load/store into fp registers through the top of stack
;;; %st(0) (fr0 here). Loads imply a push to an empty register which
;;; then changes all the reg numbers. These macros help manage that.

;;; Use this when we don't have to load anything. It preserves old tos
;;; value, but probably destroys tn with operation.
(defmacro with-tn@fp-top((tn) &body body)
  `(progn
    (unless (zerop (tn-offset ,tn))
      (inst fxch ,tn))
    ,@body
    (unless (zerop (tn-offset ,tn))
      (inst fxch ,tn))))

;;; Use this to prepare for load of new value from memory. This
;;; changes the register numbering so the next instruction had better
;;; be a FP load from memory; a register load from another register
;;; will probably be loading the wrong register!
(defmacro with-empty-tn@fp-top((tn) &body body)
  `(progn
    (inst fstp ,tn)
    ,@body
    (unless (zerop (tn-offset ,tn))
      (inst fxch ,tn))))		; save into new dest and restore st(0)

;;;; instruction-like macros

(defmacro move (dst src)
  #!+sb-doc
  "Move SRC into DST unless they are location=."
  (once-only ((n-dst dst)
	      (n-src src))
    `(unless (location= ,n-dst ,n-src)
       (inst mov ,n-dst ,n-src))))

(defmacro make-ea-for-object-slot (ptr slot lowtag)
  `(make-ea :dword :base ,ptr :disp (- (* ,slot n-word-bytes) ,lowtag)))

(defmacro loadw (value ptr &optional (slot 0) (lowtag 0))
  `(inst mov ,value (make-ea-for-object-slot ,ptr ,slot ,lowtag)))

(defmacro storew (value ptr &optional (slot 0) (lowtag 0))
  (once-only ((value value))
    `(inst mov (make-ea-for-object-slot ,ptr ,slot ,lowtag) ,value)))

(defmacro pushw (ptr &optional (slot 0) (lowtag 0))
  `(inst push (make-ea-for-object-slot ,ptr ,slot ,lowtag)))

(defmacro popw (ptr &optional (slot 0) (lowtag 0))
  `(inst pop (make-ea-for-object-slot ,ptr ,slot ,lowtag)))

;;;; macros to generate useful values

(defmacro load-symbol (reg symbol)
  `(inst mov ,reg (+ nil-value (static-symbol-offset ,symbol))))

(defmacro load-symbol-value (reg symbol)
  `(inst mov ,reg
	 (make-ea :dword
		  :disp (+ nil-value
			   (static-symbol-offset ',symbol)
			   (ash symbol-value-slot word-shift)
			   (- other-pointer-lowtag)))))

(defmacro store-symbol-value (reg symbol)
  `(inst mov
	 (make-ea :dword
		  :disp (+ nil-value
			   (static-symbol-offset ',symbol)
			   (ash symbol-value-slot word-shift)
			   (- other-pointer-lowtag)))
	 ,reg))

#!+sb-thread
(defmacro load-tl-symbol-value (reg symbol)
  `(progn
    (inst mov ,reg
     (make-ea :dword
      :disp (+ nil-value
	       (static-symbol-offset ',symbol)
	       (ash symbol-tls-index-slot word-shift)
	       (- other-pointer-lowtag))))
    (inst fs-segment-prefix)
    (inst mov ,reg (make-ea :dword :scale 1 :index ,reg))))
#!-sb-thread
(defmacro load-tl-symbol-value (reg symbol) `(load-symbol-value ,reg ,symbol))

#!+sb-thread
(defmacro store-tl-symbol-value (reg symbol temp)
  `(progn
    (inst mov ,temp
     (make-ea :dword
      :disp (+ nil-value
	       (static-symbol-offset ',symbol)
	       (ash symbol-tls-index-slot word-shift)
	       (- other-pointer-lowtag))))
    (inst fs-segment-prefix)
    (inst mov (make-ea :dword :scale 1 :index ,temp) ,reg)))
#!-sb-thread
(defmacro store-tl-symbol-value (reg symbol temp)
  (declare (ignore temp))
  `(store-symbol-value ,reg ,symbol))
  
(defmacro load-type (target source &optional (offset 0))
  #!+sb-doc
  "Loads the type bits of a pointer into target independent of
   byte-ordering issues."
  (once-only ((n-target target)
	      (n-source source)
	      (n-offset offset))
    (ecase *backend-byte-order*
      (:little-endian
       `(inst mov ,n-target
	      (make-ea :byte :base ,n-source :disp ,n-offset)))
      (:big-endian
       `(inst mov ,n-target
	      (make-ea :byte :base ,n-source :disp (+ ,n-offset 3)))))))

;;;; allocation helpers

;;; All allocation is done by calls to assembler routines that
;;; eventually invoke the C alloc() function.  Once upon a time
;;; (before threads) allocation within an alloc_region could also be
;;; done inline, with the aid of two C symbols storing the current
;;; allocation region boundaries; however, C symbols are global.

;;; C calls for allocation don't /seem/ to make an awful lot of
;;; difference to speed.  Guessing from historical context, it looks
;;; like inline allocation was introduced before pseudo-atomic, at
;;; which time all calls to alloc() would have needed a syscall to
;;; mask signals for the duration.  Now we have pseudoatomic there's
;;; no need for that overhead.  Still, inline alloc would be a neat
;;; addition someday

(defun allocation-dynamic-extent (alloc-tn size)
  (inst sub esp-tn size)
  ;; FIXME: SIZE _should_ be double-word aligned (suggested but
  ;; unfortunately not enforced by PAD-DATA-BLOCK and
  ;; WITH-FIXED-ALLOCATION), so that ESP is always divisible by 8 (for
  ;; 32-bit lispobjs).  In that case, this AND instruction is
  ;; unneccessary and could be removed.  If not, explain why.  -- CSR,
  ;; 2004-03-30
  (inst and esp-tn #.(ldb (byte 32 0) (lognot lowtag-mask)))
  (aver (not (location= alloc-tn esp-tn)))
  (inst mov alloc-tn esp-tn)
  (values))

(defun allocation-notinline (alloc-tn size)
  (let* ((alloc-tn-offset (tn-offset alloc-tn))
	 ;; C call to allocate via dispatch routines. Each
	 ;; destination has a special entry point. The size may be a
	 ;; register or a constant.
	 (tn-text (ecase alloc-tn-offset
		    (#.eax-offset "eax")
		    (#.ecx-offset "ecx")
		    (#.edx-offset "edx")
		    (#.ebx-offset "ebx")
		    (#.esi-offset "esi")
		    (#.edi-offset "edi")))
	 (size-text (case size (8 "8_") (16 "16_") (t ""))))
    (unless (or (eql size 8) (eql size 16))
      (unless (and (tn-p size) (location= alloc-tn size))
	(inst mov alloc-tn size)))
    (inst call (make-fixup (extern-alien-name 
			    (concatenate 'string
					 "alloc_" size-text
					 "to_" tn-text))
			   :foreign))))

(defun allocation-inline (alloc-tn size)
  (let ((ok (gen-label))
	(free-pointer
	 (make-ea :dword :disp 
		  #!+sb-thread (* n-word-bytes thread-alloc-region-slot)
		  #!-sb-thread (make-fixup (extern-alien-name "boxed_region")
					    :foreign)
		  :scale 1)) ; thread->alloc_region.free_pointer
	(end-addr 
	 (make-ea :dword :disp
		  #!+sb-thread (* n-word-bytes (1+ thread-alloc-region-slot))
		  #!-sb-thread (make-fixup (extern-alien-name "boxed_region")
					   :foreign 4)
		  :scale 1)))	; thread->alloc_region.end_addr
    (unless (and (tn-p size) (location= alloc-tn size))
      (inst mov alloc-tn size))
    #!+sb-thread (inst fs-segment-prefix)
    (inst add alloc-tn free-pointer)
    #!+sb-thread (inst fs-segment-prefix)
    (inst cmp alloc-tn end-addr)
    (inst jmp :be OK)
    (let ((dst (ecase (tn-offset alloc-tn)
		 (#.eax-offset "alloc_overflow_eax")
		 (#.ecx-offset "alloc_overflow_ecx")
		 (#.edx-offset "alloc_overflow_edx")
		 (#.ebx-offset "alloc_overflow_ebx")
		 (#.esi-offset "alloc_overflow_esi")
		 (#.edi-offset "alloc_overflow_edi"))))
      (inst call (make-fixup (extern-alien-name dst) :foreign)))
    (emit-label ok)
    #!+sb-thread (inst fs-segment-prefix)
    (inst xchg free-pointer alloc-tn))
  (values))


;;; Emit code to allocate an object with a size in bytes given by
;;; SIZE.  The size may be an integer or a TN. If Inline is a VOP
;;; node-var then it is used to make an appropriate speed vs size
;;; decision.

;;; Allocation should only be used inside a pseudo-atomic section, which
;;; should also cover subsequent initialization of the object.

;;; (FIXME: so why aren't we asserting this?)

(defun allocation (alloc-tn size &optional inline dynamic-extent)
  (cond
    (dynamic-extent (allocation-dynamic-extent alloc-tn size))
    ((or (null inline) (policy inline (>= speed space)))
     (allocation-inline alloc-tn size))
    (t (allocation-notinline alloc-tn size)))
  (values))

;;; Allocate an other-pointer object of fixed SIZE with a single word
;;; header having the specified WIDETAG value. The result is placed in
;;; RESULT-TN.
(defmacro with-fixed-allocation ((result-tn widetag size &optional inline)
				 &rest forms)
  `(pseudo-atomic
    (allocation ,result-tn (pad-data-block ,size) ,inline)
    (storew (logior (ash (1- ,size) n-widetag-bits) ,widetag)
	    ,result-tn)
    (inst lea ,result-tn
     (make-ea :byte :base ,result-tn :disp other-pointer-lowtag))
    ,@forms))

;;;; error code
(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
  (defun emit-error-break (vop kind code values)
    (let ((vector (gensym)))
      `((inst int 3)				; i386 breakpoint instruction
	;; The return PC points here; note the location for the debugger.
	(let ((vop ,vop))
  	  (when vop
		(note-this-location vop :internal-error)))
	(inst byte ,kind)			; eg trap_Xyyy
	(with-adjustable-vector (,vector)	; interr arguments
	  (write-var-integer (error-number-or-lose ',code) ,vector)
	  ,@(mapcar (lambda (tn)
		      `(let ((tn ,tn))
			 ;; classic CMU CL comment:
			 ;;   zzzzz jrd here. tn-offset is zero for constant
			 ;;   tns.
			 (write-var-integer (make-sc-offset (sc-number
							     (tn-sc tn))
							    (or (tn-offset tn)
								0))
					    ,vector)))
		    values)
	  (inst byte (length ,vector))
	  (dotimes (i (length ,vector))
	    (inst byte (aref ,vector i))))))))

(defmacro error-call (vop error-code &rest values)
  #!+sb-doc
  "Cause an error. ERROR-CODE is the error to cause."
  (cons 'progn
	(emit-error-break vop error-trap error-code values)))

(defmacro generate-error-code (vop error-code &rest values)
  #!+sb-doc
  "Generate-Error-Code Error-code Value*
  Emit code for an error with the specified Error-Code and context Values."
  `(assemble (*elsewhere*)
     (let ((start-lab (gen-label)))
       (emit-label start-lab)
       (error-call ,vop ,error-code ,@values)
       start-lab)))


;;;; PSEUDO-ATOMIC

;;; This is used to wrap operations which leave untagged memory lying
;;; around.  It's an operation which the AOP weenies would describe as
;;; having "cross-cutting concerns", meaning it appears all over the
;;; place and there's no logical single place to attach documentation.
;;; grep (mostly in src/runtime) is your friend 

;;; FIXME: *PSEUDO-ATOMIC-FOO* could be made into *PSEUDO-ATOMIC-BITS*,
;;; set with a single operation and cleared with SHR *PSEUDO-ATOMIC-BITS*,-2;
;;; the ATOMIC bit is bit 0, the INTERRUPTED bit is bit 1, and you check
;;; the C flag after the shift to see whether you were interrupted.
;;;
;;; KLUDGE: since the stack on the x86 is treated conservatively, it
;;; does not matter whether a signal occurs during construction of a
;;; dynamic-extent object, as the half-finished construction of the
;;; object will not cause any difficulty.  We can therefore elide 
(defvar *dynamic-extent* nil)

#!+sb-thread
(defmacro pseudo-atomic (&rest forms)
  (with-unique-names (label)
    `(if *dynamic-extent* ; I will burn in hell
         (progn ,@forms)
         (let ((,label (gen-label)))
	   (inst fs-segment-prefix)
	   (inst mov (make-ea :byte 
			      :disp (* 4 thread-pseudo-atomic-interrupted-slot)) 0)
	   (inst fs-segment-prefix)
	   (inst mov (make-ea :byte :disp (* 4 thread-pseudo-atomic-atomic-slot)) 1)
	   ,@forms
	   (inst fs-segment-prefix)
	   (inst mov (make-ea :byte :disp (* 4 thread-pseudo-atomic-atomic-slot)) 0)
	   (inst fs-segment-prefix)
	   (inst cmp (make-ea :byte
			      :disp (* 4 thread-pseudo-atomic-interrupted-slot)) 0)
	   (inst jmp :eq ,label)
	   ;; if PAI was set, interrupts were disabled at the same
	   ;; time using the process signal mask.
	   (inst break pending-interrupt-trap)
	   (emit-label ,label)))))

#!-sb-thread
(defmacro pseudo-atomic (&rest forms)
  (with-unique-names (label)
    `(if *dynamic-extent*
         (progn ,@forms)
         (let ((,label (gen-label)))
	   ;; FIXME: The MAKE-EA noise should become a MACROLET macro
	   ;; or something. (perhaps SVLB, for static variable low
	   ;; byte)
	   (inst mov (make-ea :byte :disp (+ nil-value
					     (static-symbol-offset
					      '*pseudo-atomic-interrupted*)
					     (ash symbol-value-slot word-shift)
					     ;; FIXME: Use mask, not minus, to
					     ;; take out type bits.
					     (- other-pointer-lowtag)))
		 0)
	   (inst mov (make-ea :byte :disp (+ nil-value
					     (static-symbol-offset
					      '*pseudo-atomic-atomic*)
					     (ash symbol-value-slot word-shift)
					     (- other-pointer-lowtag)))
		 (fixnumize 1))
	   ,@forms
	   (inst mov (make-ea :byte :disp (+ nil-value
					     (static-symbol-offset
					      '*pseudo-atomic-atomic*)
					     (ash symbol-value-slot word-shift)
					     (- other-pointer-lowtag)))
		 0)
	   ;; KLUDGE: Is there any requirement for interrupts to be
	   ;; handled in order? It seems as though an interrupt coming
	   ;; in at this point will be executed before any pending
	   ;; interrupts.  Or do incoming interrupts check to see
	   ;; whether any interrupts are pending? I wish I could find
	   ;; the documentation for pseudo-atomics.. -- WHN 19991130
	   (inst cmp (make-ea :byte
			      :disp (+ nil-value
				       (static-symbol-offset
					'*pseudo-atomic-interrupted*)
				       (ash symbol-value-slot word-shift)
				       (- other-pointer-lowtag)))
		 0)
	   (inst jmp :eq ,label)
	   ;; if PAI was set, interrupts were disabled at the same
	   ;; time using the process signal mask.
	   (inst break pending-interrupt-trap)
	   (emit-label ,label)))))

;;;; indexed references

(defmacro define-full-reffer (name type offset lowtag scs el-type &optional translate)
  `(progn
     (define-vop (,name)
       ,@(when translate
	   `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
	      (index :scs (any-reg)))
       (:arg-types ,type tagged-num)
       (:results (value :scs ,scs))
       (:result-types ,el-type)
       (:generator 3			; pw was 5
	 (inst mov value (make-ea :dword :base object :index index
				  :disp (- (* ,offset n-word-bytes)
					   ,lowtag)))))
     (define-vop (,(symbolicate name "-C"))
       ,@(when translate
	   `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg)))
       (:info index)
       (:arg-types ,type (:constant (signed-byte 30)))
       (:results (value :scs ,scs))
       (:result-types ,el-type)
       (:generator 2			; pw was 5
	 (inst mov value (make-ea :dword :base object
				  :disp (- (* (+ ,offset index) n-word-bytes)
					   ,lowtag)))))))

(defmacro define-full-setter (name type offset lowtag scs el-type &optional translate)
  `(progn
     (define-vop (,name)
       ,@(when translate
	   `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
	      (index :scs (any-reg))
	      (value :scs ,scs :target result))
       (:arg-types ,type tagged-num ,el-type)
       (:results (result :scs ,scs))
       (:result-types ,el-type)
       (:generator 4			; was 5
	 (inst mov (make-ea :dword :base object :index index
			    :disp (- (* ,offset n-word-bytes) ,lowtag))
	       value)
	 (move result value)))
     (define-vop (,(symbolicate name "-C"))
       ,@(when translate
	   `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
	      (value :scs ,scs :target result))
       (:info index)
       (:arg-types ,type (:constant (signed-byte 30)) ,el-type)
       (:results (result :scs ,scs))
       (:result-types ,el-type)
       (:generator 3			; was 5
	 (inst mov (make-ea :dword :base object
			    :disp (- (* (+ ,offset index) n-word-bytes)
				     ,lowtag))
	       value)
	 (move result value)))))

;;; helper for alien stuff.
(defmacro with-pinned-objects ((&rest objects) &body body)
  "Arrange with the garbage collector that the pages occupied by
OBJECTS will not be moved in memory for the duration of BODY.
Useful for e.g. foreign calls where another thread may trigger
garbage collection"
  `(multiple-value-prog1
       (progn
	 ,@(loop for p in objects 
		 collect `(push-word-on-c-stack
			   (int-sap (sb!kernel:get-lisp-obj-address ,p))))
	 ,@body)
     ;; If the body returned normally, we should restore the stack pointer
     ;; for the benefit of any following code in the same function.  If
     ;; there's a non-local exit in the body, sp is garbage anyway and
     ;; will get set appropriately from {a, the} frame pointer before it's
     ;; next needed
     (pop-words-from-c-stack ,(length objects))))
