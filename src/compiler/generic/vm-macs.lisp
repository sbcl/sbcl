;;;; some macros and constants that are object-format-specific or are
;;;; used for defining the object format

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; other miscellaneous stuff

;;; This returns a form that returns a dual-word aligned number of bytes when
;;; given a number of words.
;;;
;;; FIXME: should be a function
;;; FIXME: should be called PAD-DATA-BLOCK-SIZE
(defmacro pad-data-block (words)
  `(logandc2 (+ (ash ,words word-shift) lowtag-mask) lowtag-mask))

;;;; primitive object definition stuff

(defun remove-keywords (options keywords)
  (cond ((null options) nil)
	((member (car options) keywords)
	 (remove-keywords (cddr options) keywords))
	(t
	 (list* (car options) (cadr options)
		(remove-keywords (cddr options) keywords)))))

(def!struct (prim-object-slot
	     (:constructor make-slot (name docs rest-p offset length options))
	     (:make-load-form-fun just-dump-it-normally)
	     (:conc-name slot-))
  (name nil :type symbol)
  (docs nil :type (or null simple-string))
  (rest-p nil :type (member t nil))
  (offset 0 :type fixnum)
  (length 1 :type fixnum)
  (options nil :type list))

(def!struct (primitive-object (:make-load-form-fun just-dump-it-normally))
  (name nil :type symbol)
  (header nil :type symbol)
  (lowtag nil :type symbol)
  (options nil :type list)
  (slots nil :type list)
  (size 0 :type fixnum)
  (variable-length nil :type (member t nil)))

(defvar *primitive-objects* nil)

(defun %define-primitive-object (primobj)
  (let ((name (primitive-object-name primobj)))
    (setf *primitive-objects*
	  (cons primobj
		(remove name *primitive-objects*
			:key #'primitive-object-name :test #'eq)))
    name))

(defmacro define-primitive-object
	  ((name &key header lowtag alloc-trans (type t))
	   &rest slot-specs)
  (collect ((slots) (exports) (constants) (forms) (inits))
    (let ((offset (if header 1 0))
	  (variable-length nil))
      (dolist (spec slot-specs)
	(when variable-length
	  (error "No more slots can follow a :rest-p slot."))
	(destructuring-bind
	    (slot-name &rest options
		       &key docs rest-p (length (if rest-p 0 1))
		       ((:type slot-type) t) init
		       (ref-known nil ref-known-p) ref-trans
		       (set-known nil set-known-p) set-trans
		       &allow-other-keys)
	    (if (atom spec) (list spec) spec)
	  (slots (make-slot slot-name docs rest-p offset length
			    (remove-keywords options
					     '(:docs :rest-p :length))))
	  (let ((offset-sym (symbolicate name "-" slot-name
					 (if rest-p "-OFFSET" "-SLOT"))))
	    (constants `(defconstant ,offset-sym ,offset
			  ,@(when docs (list docs))))
	    (exports offset-sym))
	  (when ref-trans
	    (when ref-known-p
	      (forms `(defknown ,ref-trans (,type) ,slot-type ,ref-known)))
	    (forms `(def-reffer ,ref-trans ,offset ,lowtag)))
	  (when set-trans
	    (when set-known-p
	      (forms `(defknown ,set-trans
				,(if (listp set-trans)
				     (list slot-type type)
				     (list type slot-type))
				,slot-type
			,set-known)))
	    (forms `(def-setter ,set-trans ,offset ,lowtag)))
	  (when init
	    (inits (cons init offset)))
	  (when rest-p
	    (setf variable-length t))
	  (incf offset length)))
      (unless variable-length
	(let ((size (symbolicate name "-SIZE")))
	  (constants `(defconstant ,size ,offset))
	  (exports size)))
      (when alloc-trans
	(forms `(def-alloc ,alloc-trans ,offset ,variable-length ,header
			   ,lowtag ',(inits))))
      `(progn
	 (eval-when (:compile-toplevel :load-toplevel :execute)
	   (%define-primitive-object
	    ',(make-primitive-object :name name
				     :header header
				     :lowtag lowtag
				     :slots (slots)
				     :size offset
				     :variable-length variable-length))
	   ,@(constants))
	 ,@(forms)))))

;;;; stuff for defining reffers and setters

(in-package "SB!C")

(defun %def-reffer (name offset lowtag)
  (let ((info (function-info-or-lose name)))
    (setf (function-info-ir2-convert info)
	  #'(lambda (node block)
	      (ir2-convert-reffer node block name offset lowtag))))
  name)

(defmacro def-reffer (name offset lowtag)
  `(%def-reffer ',name ,offset ,lowtag))

(defun %def-setter (name offset lowtag)
  (let ((info (function-info-or-lose name)))
    (setf (function-info-ir2-convert info)
	  (if (listp name)
	      #'(lambda (node block)
		  (ir2-convert-setfer node block name offset lowtag))
	      #'(lambda (node block)
		  (ir2-convert-setter node block name offset lowtag)))))
  name)

(defmacro def-setter (name offset lowtag)
  `(%def-setter ',name ,offset ,lowtag))

(defun %def-alloc (name words variable-length header lowtag inits)
  (let ((info (function-info-or-lose name)))
    (setf (function-info-ir2-convert info)
	  (if variable-length
	      #'(lambda (node block)
		  (ir2-convert-variable-allocation node block name words header
						   lowtag inits))
	      #'(lambda (node block)
		  (ir2-convert-fixed-allocation node block name words header
						lowtag inits)))))
  name)

(defmacro def-alloc (name words variable-length header lowtag inits)
  `(%def-alloc ',name ,words ,variable-length ,header ,lowtag ,inits))

;;;; some general constant definitions

;;; FIXME: SC-NUMBER-LIMIT should probably be exported from SB!C
;;; or SB!VM so that we don't need to do this extra IN-PACKAGE.
(in-package "SB!C")

;;; the maximum number of SCs in any implementation
(defconstant sc-number-limit 32)
