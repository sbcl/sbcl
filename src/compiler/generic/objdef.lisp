;;;; machine-independent aspects of the object representation

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

(file-comment
  "$Header$")

;;;; the primitive objects themselves

(define-primitive-object (cons :lowtag list-pointer-type
			       :alloc-trans cons)
  (car :ref-trans car :set-trans sb!c::%rplaca :init :arg)
  (cdr :ref-trans cdr :set-trans sb!c::%rplacd :init :arg))

(define-primitive-object (instance :lowtag instance-pointer-type
				   :header instance-header-type
				   :alloc-trans %make-instance)
  (slots :rest-p t))

(define-primitive-object (bignum :lowtag other-pointer-type
				 :header bignum-type
				 :alloc-trans sb!bignum::%allocate-bignum)
  (digits :rest-p t :c-type #!-alpha "long" #!+alpha "u32"))

(define-primitive-object (ratio :type ratio
				:lowtag other-pointer-type
				:header ratio-type
				:alloc-trans %make-ratio)
  (numerator :type integer
	     :ref-known (flushable movable)
	     :ref-trans %numerator
	     :init :arg)
  (denominator :type integer
	       :ref-known (flushable movable)
	       :ref-trans %denominator
	       :init :arg))

(define-primitive-object (single-float :lowtag other-pointer-type
				       :header single-float-type)
  (value :c-type "float"))

(define-primitive-object (double-float :lowtag other-pointer-type
				       :header double-float-type)
  (filler)
  (value :c-type "double" :length 2))

#!+long-float
(define-primitive-object (long-float :lowtag other-pointer-type
				     :header long-float-type)
  #!+sparc (filler)
  (value :c-type "long double" :length #!+x86 3 #!+sparc 4))

(define-primitive-object (complex :type complex
				  :lowtag other-pointer-type
				  :header complex-type
				  :alloc-trans %make-complex)
  (real :type real
	:ref-known (flushable movable)
	:ref-trans %realpart
	:init :arg)
  (imag :type real
	:ref-known (flushable movable)
	:ref-trans %imagpart
	:init :arg))

(define-primitive-object (array :lowtag other-pointer-type
				:header t)
  (fill-pointer :type index
		:ref-trans %array-fill-pointer
		:ref-known (flushable foldable)
		:set-trans (setf %array-fill-pointer)
		:set-known (unsafe))
  (fill-pointer-p :type (member t nil)
		  :ref-trans %array-fill-pointer-p
		  :ref-known (flushable foldable)
		  :set-trans (setf %array-fill-pointer-p)
		  :set-known (unsafe))
  (elements :type index
	    :ref-trans %array-available-elements
	    :ref-known (flushable foldable)
	    :set-trans (setf %array-available-elements)
	    :set-known (unsafe))
  (data :type array
	:ref-trans %array-data-vector
	:ref-known (flushable foldable)
	:set-trans (setf %array-data-vector)
	:set-known (unsafe))
  (displacement :type (or index null)
		:ref-trans %array-displacement
		:ref-known (flushable foldable)
		:set-trans (setf %array-displacement)
		:set-known (unsafe))
  (displaced-p :type (member t nil)
	       :ref-trans %array-displaced-p
	       :ref-known (flushable foldable)
	       :set-trans (setf %array-displaced-p)
	       :set-known (unsafe))
  (dimensions :rest-p t))

(define-primitive-object (vector :type vector
				 :lowtag other-pointer-type
				 :header t)
  (length :ref-trans sb!c::vector-length
	  :type index)
  (data :rest-p t :c-type #!-alpha "unsigned long" #!+alpha "u32"))

(define-primitive-object (code :type code-component
			       :lowtag other-pointer-type
			       :header t)
  (code-size :type index
	     :ref-known (flushable movable)
	     :ref-trans %code-code-size)
  (entry-points :type (or function null)
		:ref-known (flushable)
		:ref-trans %code-entry-points
		:set-known (unsafe)
		:set-trans (setf %code-entry-points))
  (debug-info :type t
	      :ref-known (flushable)
	      :ref-trans %code-debug-info
	      :set-known (unsafe)
	      :set-trans (setf %code-debug-info))
  (trace-table-offset)
  (constants :rest-p t))

(define-primitive-object (fdefn :type fdefn
				:lowtag other-pointer-type
				:header fdefn-type)
  (name :ref-trans fdefn-name)
  (function :type (or function null) :ref-trans fdefn-function)
  (raw-addr :c-type #!-alpha "char *" #!+alpha "u32"))

(define-primitive-object (function :type function
				   :lowtag function-pointer-type
				   :header function-header-type)
  #!-gengc (self :ref-trans %function-self :set-trans (setf %function-self))
  #!+gengc (entry-point :c-type "char *")
  (next :type (or function null)
	:ref-known (flushable)
	:ref-trans %function-next
	:set-known (unsafe)
	:set-trans (setf %function-next))
  (name :ref-known (flushable)
	:ref-trans %function-name
	:set-known (unsafe)
	:set-trans (setf %function-name))
  (arglist :ref-known (flushable)
	   :ref-trans %function-arglist
	   :set-known (unsafe)
	   :set-trans (setf %function-arglist))
  (type :ref-known (flushable)
	:ref-trans %function-type
	:set-known (unsafe)
	:set-trans (setf %function-type))
  (code :rest-p t :c-type "unsigned char"))

#!-gengc
(define-primitive-object (return-pc :lowtag other-pointer-type :header t)
  (return-point :c-type "unsigned char" :rest-p t))

(define-primitive-object (closure :lowtag function-pointer-type
				  :header closure-header-type)
  #!-gengc (function :init :arg :ref-trans %closure-function)
  #!+gengc (entry-point :c-type "char *")
  (info :rest-p t))

(define-primitive-object (funcallable-instance
			  :lowtag function-pointer-type
			  :header funcallable-instance-header-type
			  :alloc-trans %make-funcallable-instance)
  #!-gengc
  (function
   :ref-known (flushable) :ref-trans %funcallable-instance-function
   :set-known (unsafe) :set-trans (setf %funcallable-instance-function))
  #!+gengc (entry-point :c-type "char *")
  (lexenv :ref-known (flushable) :ref-trans %funcallable-instance-lexenv
	  :set-known (unsafe) :set-trans (setf %funcallable-instance-lexenv))
  (layout :init :arg
	  :ref-known (flushable) :ref-trans %funcallable-instance-layout
	  :set-known (unsafe) :set-trans (setf %funcallable-instance-layout))
  (info :rest-p t))

(define-primitive-object (value-cell :lowtag other-pointer-type
				     :header value-cell-header-type
				     :alloc-trans make-value-cell)
  (value :set-trans value-cell-set
	 :set-known (unsafe)
	 :ref-trans value-cell-ref
	 :ref-known (flushable)
	 :init :arg))

#!+alpha
(define-primitive-object (sap :lowtag other-pointer-type
			      :header sap-type)
  (padding)
  (pointer :c-type "char *" :length 2))

#!-alpha
(define-primitive-object (sap :lowtag other-pointer-type
			      :header sap-type)
  (pointer :c-type "char *"))


(define-primitive-object (weak-pointer :type weak-pointer
				       :lowtag other-pointer-type
				       :header weak-pointer-type
				       :alloc-trans make-weak-pointer)
  (value :ref-trans sb!c::%weak-pointer-value :ref-known (flushable)
	 :init :arg)
  (broken :type (member t nil)
	  :ref-trans sb!c::%weak-pointer-broken :ref-known (flushable)
	  :init :null)
  (next :c-type #!-alpha "struct weak_pointer *" #!+alpha "u32"))

;;;; other non-heap data blocks

(define-primitive-object (binding)
  value
  symbol)

(define-primitive-object (unwind-block)
  (current-uwp :c-type #!-alpha "struct unwind_block *" #!+alpha "u32")
  (current-cont :c-type #!-alpha "lispobj *" #!+alpha "u32")
  #!-x86 current-code
  entry-pc)

(define-primitive-object (catch-block)
  (current-uwp :c-type #!-alpha "struct unwind_block *" #!+alpha "u32")
  (current-cont :c-type #!-alpha "lispobj *" #!+alpha "u32")
  #!-x86 current-code
  entry-pc
  tag
  (previous-catch :c-type #!-alpha "struct catch_block *" #!+alpha "u32")
  size)

;;; (For an explanation of this, see the comments at the definition of
;;; KLUDGE-NONDETERMINISTIC-CATCH-BLOCK-SIZE.)
(assert (= sb!vm::kludge-nondeterministic-catch-block-size catch-block-size))

#!+gengc
(define-primitive-object (mutator)
  ;; Holds the lisp thread structure, if any.
  (thread)
  ;; Signal control magic.
  (foreign-fn-call-active :c-type "boolean")
  (interrupts-disabled-count :c-type "int")
  (interrupt-pending :c-type "boolean")
  (pending-signal :c-type "int")
  (pending-code :c-type "int")
  (pending-mask :c-type "int")
  (gc-pending :c-type "boolean")
  ;; Stacks.
  (control-stack-base :c-type "lispobj *")
  (control-stack-pointer :c-type "lispobj *")
  (control-stack-end :c-type "lispobj *")
  (control-frame-pointer :c-type "lispobj *")
  (current-unwind-protect :c-type "struct unwind_block *")
  (current-catch-block :c-type "struct catch_block *")
  (binding-stack-base :c-type "struct binding *")
  (binding-stack-pointer :c-type "struct binding *")
  (binding-stack-end :c-type "struct binding *")
  (number-stack-base :c-type "char *")
  (number-stack-pointer :c-type "char *")
  (number-stack-end :c-type "char *")
  (eval-stack)
  (eval-stack-top)
  ;; Allocation stuff.
  (nursery-start :c-type "lispobj *")
  (nursery-fill-pointer :c-type "lispobj *")
  (nursery-end :c-type "lispobj *")
  (storebuf-start :c-type "lispobj **")
  (storebuf-fill-pointer :c-type "lispobj **")
  (storebuf-end :c-type "lispobj **")
  (words-consed :c-type "unsigned long"))


;;;; symbols

#!+gengc
(defknown %make-symbol (index simple-string) symbol
  (flushable movable))

#+gengc
(defknown symbol-hash (symbol) index
  (flushable movable))

#+x86
(defknown symbol-hash (symbol) (integer 0 #.*target-most-positive-fixnum*)
  (flushable movable))

(define-primitive-object (symbol :lowtag other-pointer-type
				 :header symbol-header-type
				 #!-x86 :alloc-trans
				 #!-(or gengc x86) make-symbol
				 #!+gengc %make-symbol)
  (value :set-trans %set-symbol-value
	 :init :unbound)
  #!-(or gengc x86) unused
  #!+gengc (hash :init :arg)
  #!+x86 (hash)
  (plist :ref-trans symbol-plist
	 :set-trans %set-symbol-plist
	 :init :null)
  (name :ref-trans symbol-name :init :arg)
  (package :ref-trans symbol-package
	   :set-trans %set-symbol-package
	   :init :null))

(define-primitive-object (complex-single-float
			  :lowtag other-pointer-type
			  :header complex-single-float-type)
  (real :c-type "float")
  (imag :c-type "float"))

(define-primitive-object (complex-double-float
			  :lowtag other-pointer-type
			  :header complex-double-float-type)
  (filler)
  (real :c-type "double" :length 2)
  (imag :c-type "double" :length 2))

#!+long-float
(define-primitive-object (complex-long-float
			  :lowtag other-pointer-type
			  :header complex-long-float-type)
  #!+sparc (filler)
  (real :c-type "long double" :length #!+x86 3 #!+sparc 4)
  (imag :c-type "long double" :length #!+x86 3 #!+sparc 4))

