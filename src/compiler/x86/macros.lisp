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

;;; Two allocation approaches are implemented. A call into C can be
;;; used, and in that case special care can be taken to disable
;;; interrupts. Alternatively with gencgc inline allocation is possible
;;; although it isn't interrupt safe.

;;; For GENCGC it is possible to inline object allocation, to permit
;;; this set the following variable to True.
;;;
;;; FIXME: The comment above says that this isn't interrupt safe. Is that
;;; right? If so, do we want to do this? And surely we don't want to do this by
;;; default? How much time does it save to do this? Is it any different in the
;;; current CMU CL version instead of the one that I grabbed in 1998?
;;; (Later observation: In order to be interrupt safe, it'd probably
;;; have to use PSEUDO-ATOMIC, so it's probably not -- yuck. Try benchmarks
;;; with and without inline allocation, and unless the inline allocation
;;; wins by a whole lot, it's not likely to be worth messing with. If
;;; we want to hack up memory allocation for performance, effort spent
;;; on DYNAMIC-EXTENT would probably give a better payoff.)
(defvar *maybe-use-inline-allocation* t)

;;; Emit code to allocate an object with a size in bytes given by
;;; Size. The size may be an integer of a TN. If Inline is a VOP
;;; node-var then it is used to make an appropriate speed vs size
;;; decision.
;;;
;;; FIXME: We call into C.. except when inline allocation is enabled..?
;;;
;;; FIXME: Also, calls to
;;; ALLOCATION are always wrapped with PSEUDO-ATOMIC -- why? Is it to
;;; make sure that no GC happens between the time of allocation and the
;;; time that the allocated memory has its tag bits set correctly?
;;; If so, then ALLOCATION itself might as well set the PSEUDO-ATOMIC
;;; bits, so that the caller need only clear them. Check whether it's
;;; true that every ALLOCATION is surrounded by PSEUDO-ATOMIC, and
;;; that every PSEUDO-ATOMIC contains a single ALLOCATION, which is
;;; its first instruction. If so, the connection should probably be
;;; formalized, in documentation and in macro definition,
;;; with the macro becoming e.g. PSEUDO-ATOMIC-ALLOCATION.
(defun allocation (alloc-tn size &optional inline)
  (flet ((load-size (dst-tn size)
	   (unless (and (tn-p size) (location= alloc-tn size))
	     (inst mov dst-tn size))))
    (let ((alloc-tn-offset (tn-offset alloc-tn)))
	  ;; C call to allocate via dispatch routines. Each
	  ;; destination has a special entry point. The size may be a
	  ;; register or a constant.
	  (ecase alloc-tn-offset
	    (#.eax-offset
	     (case size
	       (8 (inst call (make-fixup (extern-alien-name "alloc_8_to_eax")
					 :foreign)))
	       (16 (inst call (make-fixup (extern-alien-name "alloc_16_to_eax")
					  :foreign)))
	       (t
		(load-size eax-tn size)
		(inst call (make-fixup (extern-alien-name "alloc_to_eax")
				       :foreign)))))
	    (#.ecx-offset
	     (case size
	       (8 (inst call (make-fixup (extern-alien-name "alloc_8_to_ecx")
					 :foreign)))
	       (16 (inst call (make-fixup (extern-alien-name "alloc_16_to_ecx")
					  :foreign)))
	       (t
		(load-size ecx-tn size)
		(inst call (make-fixup (extern-alien-name "alloc_to_ecx")
				       :foreign)))))
	    (#.edx-offset
	     (case size
	       (8 (inst call (make-fixup (extern-alien-name "alloc_8_to_edx")
					 :foreign)))
	       (16 (inst call (make-fixup (extern-alien-name "alloc_16_to_edx")
					  :foreign)))
	       (t
		(load-size edx-tn size)
		(inst call (make-fixup (extern-alien-name "alloc_to_edx")
				       :foreign)))))
	    (#.ebx-offset
	     (case size
	       (8 (inst call (make-fixup (extern-alien-name "alloc_8_to_ebx")
					 :foreign)))
	       (16 (inst call (make-fixup (extern-alien-name "alloc_16_to_ebx")
					  :foreign)))
	       (t
		(load-size ebx-tn size)
		(inst call (make-fixup (extern-alien-name "alloc_to_ebx")
				       :foreign)))))
	    (#.esi-offset
	     (case size
	       (8 (inst call (make-fixup (extern-alien-name "alloc_8_to_esi")
					 :foreign)))
	       (16 (inst call (make-fixup (extern-alien-name "alloc_16_to_esi")
					  :foreign)))
	       (t
		(load-size esi-tn size)
		(inst call (make-fixup (extern-alien-name "alloc_to_esi")
				       :foreign)))))
	    (#.edi-offset
	     (case size
	       (8 (inst call (make-fixup (extern-alien-name "alloc_8_to_edi")
					 :foreign)))
	       (16 (inst call (make-fixup (extern-alien-name "alloc_16_to_edi")
					  :foreign)))
	       (t
		(load-size edi-tn size)
		(inst call (make-fixup (extern-alien-name "alloc_to_edi")
				   :foreign))))))))
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

;;; FIXME: *PSEUDO-ATOMIC-FOO* could be made into *PSEUDO-ATOMIC-BITS*,
;;; set with a single operation and cleared with SHR *PSEUDO-ATOMIC-BITS*,-2;
;;; the ATOMIC bit is bit 0, the INTERRUPTED bit is bit 1, and you check
;;; the C flag after the shift to see whether you were interrupted.

;;; FIXME: It appears that PSEUDO-ATOMIC is used to wrap operations which leave
;;; untagged memory lying around, but some documentation would be nice.
#!+sb-thread
(defmacro pseudo-atomic (&rest forms)
  (with-unique-names (label)
    `(let ((,label (gen-label)))
      (inst fs-segment-prefix)
      (inst mov (make-ea :byte :disp (* 4 thread-pseudo-atomic-atomic-slot)) 1)
      (inst fs-segment-prefix)
      (inst mov (make-ea :byte 
		 :disp (* 4 thread-pseudo-atomic-interrupted-slot)) 0) 
      ,@forms
      (inst fs-segment-prefix)
      (inst mov (make-ea :byte :disp (* 4 thread-pseudo-atomic-atomic-slot)) 0)
      (inst fs-segment-prefix)
      (inst cmp (make-ea :byte
		 :disp (* 4 thread-pseudo-atomic-interrupted-slot)) 0)
      (inst jmp :eq ,label)
      ;; if PAI was set, interrupts were disabled at the same time
      ;; using the process signal mask.  
      (inst break pending-interrupt-trap)
      (emit-label ,label))))

#!-sb-thread
(defmacro pseudo-atomic (&rest forms)
  (with-unique-names (label)
    `(let ((,label (gen-label)))
      ;; FIXME: The MAKE-EA noise should become a MACROLET macro or
      ;; something. (perhaps SVLB, for static variable low byte)
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
      ;; in at this point will be executed before any pending interrupts.
      ;; Or do incoming interrupts check to see whether any interrupts
      ;; are pending? I wish I could find the documentation for
      ;; pseudo-atomics.. -- WHN 19991130
      (inst cmp (make-ea :byte
                 :disp (+ nil-value
                          (static-symbol-offset
                           '*pseudo-atomic-interrupted*)
                          (ash symbol-value-slot word-shift)
                          (- other-pointer-lowtag)))
       0)
      (inst jmp :eq ,label)
      ;; if PAI was set, interrupts were disabled at the same time
      ;; using the process signal mask.  
      (inst break pending-interrupt-trap)
      (emit-label ,label))))



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

