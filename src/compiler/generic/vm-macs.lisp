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
	     (:constructor make-slot (name docs rest-p offset options))
	     (:make-load-form-fun just-dump-it-normally)
	     (:conc-name slot-))
  (name nil :type symbol)
  (docs nil :type (or null simple-string))
  (rest-p nil :type (member t nil))
  (offset 0 :type fixnum)
  (options nil :type list))

(def!struct (primitive-object (:make-load-form-fun just-dump-it-normally))
  (name nil :type symbol)
  (widetag nil :type symbol)
  (lowtag nil :type symbol)
  (options nil :type list)
  (slots nil :type list)
  (size 0 :type fixnum)
  (variable-length-p nil :type (member t nil)))

(defvar *primitive-objects* nil)

(defun %define-primitive-object (primobj)
  (let ((name (primitive-object-name primobj)))
    (setf *primitive-objects*
	  (cons primobj
		(remove name *primitive-objects*
			:key #'primitive-object-name :test #'eq)))
    name))

(defmacro define-primitive-object
	  ((name &key lowtag widetag alloc-trans (type t))
	   &rest slot-specs)
  (collect ((slots) (exports) (constants) (forms) (inits))
    (let ((offset (if widetag 1 0))
	  (variable-length-p nil))
      (dolist (spec slot-specs)
	(when variable-length-p
	  (error "No more slots can follow a :rest-p slot."))
	(destructuring-bind
	    (slot-name &rest options
		       &key docs rest-p (length (if rest-p 0 1))
		       ((:type slot-type) t) init
		       (ref-known nil ref-known-p) ref-trans
		       (set-known nil set-known-p) set-trans
		       &allow-other-keys)
	    (if (atom spec) (list spec) spec)
	  (slots (make-slot slot-name docs rest-p offset
			    (remove-keywords options
					     '(:docs :rest-p :length))))
	  (let ((offset-sym (symbolicate name "-" slot-name
					 (if rest-p "-OFFSET" "-SLOT"))))
	    (constants `(def!constant ,offset-sym ,offset
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
	    (setf variable-length-p t))
	  (incf offset length)))
      (unless variable-length-p
	(let ((size (symbolicate name "-SIZE")))
	  (constants `(def!constant ,size ,offset))
	  (exports size)))
      (when alloc-trans
	(forms `(def-alloc ,alloc-trans ,offset ,variable-length-p ,widetag
			   ,lowtag ',(inits))))
      `(progn
	 (eval-when (:compile-toplevel :load-toplevel :execute)
	   (%define-primitive-object
	    ',(make-primitive-object :name name
				     :widetag widetag
				     :lowtag lowtag
				     :slots (slots)
				     :size offset
				     :variable-length-p variable-length-p))
	   ,@(constants))
	 ,@(forms)))))

;;;; stuff for defining reffers and setters

(in-package "SB!C")

(defmacro def-reffer (name offset lowtag)
  `(%def-reffer ',name ,offset ,lowtag))
(defmacro def-setter (name offset lowtag)
  `(%def-setter ',name ,offset ,lowtag))
(defmacro def-alloc (name words variable-length-p header lowtag inits)
  `(%def-alloc ',name ,words ,variable-length-p ,header ,lowtag ,inits))
;;; KLUDGE: The %DEF-FOO functions used to implement the macros here
;;; are defined later in another file, since they use structure slot
;;; setters defined later, and we can't have physical forward
;;; references to structure slot setters because ANSI in its wisdom
;;; allows the xc host CL to implement structure slot setters as SETF
;;; expanders instead of SETF functions. -- WHN 2002-02-09

;;;; some general constant definitions

;;; FIXME: SC-NUMBER-LIMIT should probably be exported from SB!C
;;; or SB!VM so that we don't need to do this extra IN-PACKAGE.
(in-package "SB!C")

;;; the maximum number of SCs in any implementation
(def!constant sc-number-limit 32)

;;; Modular functions

;;; For a documentation, see CUT-TO-WIDTH.

(defstruct modular-class
  ;; hash: name -> { :GOOD | optimizer | ({modular-fun-info}*)}
  (funs (make-hash-table :test 'eq))
  ;; hash: modular-variant -> (prototype width)
  ;;
  ;; FIXME: Reimplement with generic function names of kind
  ;; (MODULAR-VERSION prototype width)
  (versions (make-hash-table :test 'eq))
  ;; list of increasing widths
  (widths nil))
(defvar *unsigned-modular-class* (make-modular-class))
(defvar *signed-modular-class* (make-modular-class))
(defun find-modular-class (kind)
  (ecase kind
    (:unsigned *unsigned-modular-class*)
    (:signed *signed-modular-class*)))

(defstruct modular-fun-info
  (name (missing-arg) :type symbol)
  (width (missing-arg) :type (integer 0))
  (lambda-list (missing-arg) :type list)
  (prototype (missing-arg) :type symbol))

(defun find-modular-version (fun-name class width)
  (let ((infos (gethash fun-name (modular-class-funs (find-modular-class class)))))
    (if (listp infos)
        (find-if (lambda (item-width) (>= item-width width))
                 infos
                 :key #'modular-fun-info-width)
        infos)))

;;; Return (VALUES prototype-name width)
(defun modular-version-info (name class)
  (values-list (gethash name (modular-class-versions (find-modular-class class)))))

(defun %define-modular-fun (name lambda-list prototype class width)
  (let* ((class (find-modular-class class))
         (funs (modular-class-funs class))
         (versions (modular-class-versions class))
         (infos (the list (gethash prototype funs)))
         (info (find-if (lambda (item-width) (= item-width width))
                        infos
                        :key #'modular-fun-info-width)))
    (if info
        (unless (and (eq name (modular-fun-info-name info))
                     (= (length lambda-list)
                        (length (modular-fun-info-lambda-list info))))
          (setf (modular-fun-info-name info) name)
          (style-warn "Redefining modular version ~S of ~S for width ~S."
                      name prototype width))
        (setf (gethash prototype funs)
              (merge 'list
                     (list (make-modular-fun-info :name name
                                                  :width width
                                                  :lambda-list lambda-list
                                                  :prototype prototype))
                     infos
                     #'< :key #'modular-fun-info-width)
              (gethash name versions)
              (list prototype width)))
    (setf (modular-class-widths class)
          (merge 'list (list width) (modular-class-widths class) #'<))))

(defmacro define-modular-fun (name lambda-list prototype class width)
  (check-type name symbol)
  (check-type prototype symbol)
  (check-type class (member :unsigned :signed))
  (check-type width unsigned-byte)
  (dolist (arg lambda-list)
    (when (member arg lambda-list-keywords)
      (error "Lambda list keyword ~S is not supported for ~
              modular function lambda lists." arg)))
  `(progn
     (%define-modular-fun ',name ',lambda-list ',prototype ',class ,width)
     (defknown ,name ,(mapcar (constantly 'integer) lambda-list)
               (,(ecase class
                   (:unsigned 'unsigned-byte)
                   (:signed 'signed-byte))
                 ,width)
               (foldable flushable movable)
               :derive-type (make-modular-fun-type-deriver
                             ',prototype ',class ,width))))

(defun %define-good-modular-fun (name class)
  (setf (gethash name (modular-class-funs (find-modular-class class))) :good)
  name)

(defmacro define-good-modular-fun (name class)
  (check-type name symbol)
  (check-type class (member :unsigned :signed))
  `(%define-good-modular-fun ',name ',class))

(defmacro define-modular-fun-optimizer
    (name ((&rest lambda-list) class &key (width (gensym "WIDTH")))
     &body body)
  (check-type name symbol)
  (check-type class (member :unsigned :signed))
  (dolist (arg lambda-list)
    (when (member arg lambda-list-keywords)
      (error "Lambda list keyword ~S is not supported for ~
              modular function lambda lists." arg)))
  (with-unique-names (call args)
    `(setf (gethash ',name (modular-class-funs (find-modular-class ',class)))
           (lambda (,call ,width)
             (declare (type basic-combination ,call)
                      (type (integer 0) width))
             (let ((,args (basic-combination-args ,call)))
               (when (= (length ,args) ,(length lambda-list))
                 (destructuring-bind ,lambda-list ,args
                   (declare (type lvar ,@lambda-list))
                   ,@body)))))))
