;;;; the part of the Alien implementation which is needed at
;;;; cross-compilation time

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!ALIEN")

;;;; utility functions

(defun align-offset (offset alignment)
  (let ((extra (rem offset alignment)))
    (if (zerop extra) offset (+ offset (- alignment extra)))))

(defun guess-alignment (bits)
  (cond ((null bits) nil)
	#!-x86 ((> bits 32) 64)
	((> bits 16) 32)
	((> bits 8) 16)
	((> bits 1) 8)
	(t 1)))

;;;; ALIEN-TYPE-INFO stuff

(eval-when (:compile-toplevel :execute :load-toplevel)

(defstruct alien-type-class
  (name nil :type symbol)
  (include nil :type (or null alien-type-class))
  (unparse nil :type (or null function))
  (type= nil :type (or null function))
  (lisp-rep nil :type (or null function))
  (alien-rep nil :type (or null function))
  (extract-gen nil :type (or null function))
  (deposit-gen nil :type (or null function))
  (naturalize-gen nil :type (or null function))
  (deport-gen nil :type (or null function))
  ;; Cast?
  (arg-tn nil :type (or null function))
  (result-tn nil :type (or null function))
  (subtypep nil :type (or null function)))
(def!method print-object ((type-class alien-type-class) stream)
  (print-unreadable-object (type-class stream :type t)
    (prin1 (alien-type-class-name type-class) stream)))

(defun alien-type-class-or-lose (name)
  (or (gethash name *alien-type-classes*)
      (error "no alien type class ~S" name)))

(defun create-alien-type-class-if-necessary (name include)
  (let ((old (gethash name *alien-type-classes*))
	(include (and include (alien-type-class-or-lose include))))
    (if old
	(setf (alien-type-class-include old) include)
	(setf (gethash name *alien-type-classes*)
	      (make-alien-type-class :name name :include include)))))

(defparameter *method-slot-alist*
  '((:unparse . alien-type-class-unparse)
    (:type= . alien-type-class-type=)
    (:subtypep . alien-type-class-subtypep)
    (:lisp-rep . alien-type-class-lisp-rep)
    (:alien-rep . alien-type-class-alien-rep)
    (:extract-gen . alien-type-class-extract-gen)
    (:deposit-gen . alien-type-class-deposit-gen)
    (:naturalize-gen . alien-type-class-naturalize-gen)
    (:deport-gen . alien-type-class-deport-gen)
    ;; cast?
    (:arg-tn . alien-type-class-arg-tn)
    (:result-tn . alien-type-class-result-tn)))

(defun method-slot (method)
  (cdr (or (assoc method *method-slot-alist*)
	   (error "no method ~S" method))))

) ; EVAL-WHEN

;;; We define a keyword "BOA" constructor so that we can reference the
;;; slot names in init forms.
(def!macro def-alien-type-class ((name &key include include-args) &rest slots)
  (let ((defstruct-name
	 (intern (concatenate 'string "ALIEN-" (symbol-name name) "-TYPE"))))
    (multiple-value-bind (include include-defstruct overrides)
	(etypecase include
	  (null
	   (values nil 'alien-type nil))
	  (symbol
	   (values
	    include
	    (intern (concatenate 'string
				 "ALIEN-" (symbol-name include) "-TYPE"))
	    nil))
	  (list
	   (values
	    (car include)
	    (intern (concatenate 'string
				 "ALIEN-" (symbol-name (car include)) "-TYPE"))
	    (cdr include))))
      `(progn
	 (eval-when (:compile-toplevel :load-toplevel :execute)
	   (create-alien-type-class-if-necessary ',name ',(or include 'root)))
	 (def!struct (,defstruct-name
			(:include ,include-defstruct
				  (:class ',name)
				  ,@overrides)
			(:constructor
			 ,(intern (concatenate 'string "MAKE-"
					       (string defstruct-name)))
			 (&key class bits alignment
			       ,@(mapcar #'(lambda (x)
					     (if (atom x) x (car x)))
					 slots)
			       ,@include-args)))
	   ,@slots)))))

(def!macro def-alien-type-method ((class method) lambda-list &rest body)
  (let ((defun-name (intern (concatenate 'string
					 (symbol-name class)
					 "-"
					 (symbol-name method)
					 "-METHOD"))))
    `(progn
       (defun ,defun-name ,lambda-list
	 ,@body)
       (setf (,(method-slot method) (alien-type-class-or-lose ',class))
	     #',defun-name))))

(def!macro invoke-alien-type-method (method type &rest args)
  (let ((slot (method-slot method)))
    (once-only ((type type))
      `(funcall (do ((class (alien-type-class-or-lose (alien-type-class ,type))
			    (alien-type-class-include class)))
		    ((null class)
		     (error "method ~S not defined for ~S"
			    ',method (alien-type-class ,type)))
		  (let ((fn (,slot class)))
		    (when fn
		      (return fn))))
		,type ,@args))))

;;;; type parsing and unparsing

;;; CMU CL used COMPILER-LET to bind *AUXILIARY-TYPE-DEFINITIONS*, and
;;; COMPILER-LET is no longer supported by ANSI or SBCL. Instead, we
;;; follow the suggestion in CLTL2 of using SYMBOL-MACROLET to achieve
;;; a similar effect.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun auxiliary-type-definitions (env)
    (multiple-value-bind (result expanded-p)
	(sb!xc:macroexpand '&auxiliary-type-definitions& env)
      (if expanded-p
	  result
	  ;; This is like having the global symbol-macro definition be
	  ;; NIL, but global symbol-macros make me vaguely queasy, so
	  ;; I do it this way instead.
	  nil))))

;;; Process stuff in a new scope.
(def!macro with-auxiliary-alien-types (env &body body)
  ``(symbol-macrolet ((&auxiliary-type-definitions&
		       ,(append *new-auxiliary-types*
				(auxiliary-type-definitions ,env))))
      ,(let ((*new-auxiliary-types* nil))
	 ,@body)))

;;; FIXME: Now that *NEW-AUXILIARY-TYPES* is born initialized to NIL,
;;; we no longer need to make a distinction between this and
;;; %PARSE-ALIEN-TYPE.
(defun parse-alien-type (type env)
  (declare (type (or sb!kernel:lexenv null) env))
  #!+sb-doc
  "Parse the list structure TYPE as an alien type specifier and return
   the resultant ALIEN-TYPE structure."
  (%parse-alien-type type env))

(defun %parse-alien-type (type env)
  (declare (type (or sb!kernel:lexenv null) env))
  (if (consp type)
      (let ((translator (info :alien-type :translator (car type))))
	(unless translator
	  (error "unknown alien type: ~S" type))
	(funcall translator type env))
      (case (info :alien-type :kind type)
	(:primitive
	 (let ((translator (info :alien-type :translator type)))
	   (unless translator
	     (error "no translator for primitive alien type ~S" type))
	   (funcall translator (list type) env)))
	(:defined
	 (or (info :alien-type :definition type)
	     (error "no definition for alien type ~S" type)))
	(:unknown
	 (error "unknown alien type: ~S" type)))))

(defun auxiliary-alien-type (kind name env)
  (declare (type (or sb!kernel:lexenv null) env))
  (flet ((aux-defn-matches (x)
	   (and (eq (first x) kind) (eq (second x) name))))
    (let ((in-auxiliaries
	   (or (find-if #'aux-defn-matches *new-auxiliary-types*)
	       (find-if #'aux-defn-matches (auxiliary-type-definitions env)))))
      (if in-auxiliaries
	  (values (third in-auxiliaries) t)
	  (ecase kind
	    (:struct
	     (info :alien-type :struct name))
	    (:union
	     (info :alien-type :union name))
	    (:enum
	     (info :alien-type :enum name)))))))

(defun (setf auxiliary-alien-type) (new-value kind name env)
  (declare (type (or sb!kernel:lexenv null) env))
  (flet ((aux-defn-matches (x)
	   (and (eq (first x) kind) (eq (second x) name))))
    (when (find-if #'aux-defn-matches *new-auxiliary-types*)
      (error "attempt to multiply define ~A ~S" kind name))
    (when (find-if #'aux-defn-matches (auxiliary-type-definitions env))
      (error "attempt to shadow definition of ~A ~S" kind name)))
  (push (list kind name new-value) *new-auxiliary-types*)
  new-value)

(defun verify-local-auxiliaries-okay ()
  (dolist (info *new-auxiliary-types*)
    (destructuring-bind (kind name defn) info
      (declare (ignore defn))
      (when (ecase kind
	      (:struct
	       (info :alien-type :struct name))
	      (:union
	       (info :alien-type :union name))
	      (:enum
	       (info :alien-type :enum name)))
	(error "attempt to shadow definition of ~A ~S" kind name)))))

(defun unparse-alien-type (type)
  #!+sb-doc
  "Convert the alien-type structure TYPE back into a list specification of
   the type."
  (declare (type alien-type type))
  (let ((*record-types-already-unparsed* nil))
    (%unparse-alien-type type)))

;;; Does all the work of UNPARSE-ALIEN-TYPE. It's separate because we
;;; need to recurse inside the binding of
;;; *RECORD-TYPES-ALREADY-UNPARSED*.
(defun %unparse-alien-type (type)
  (invoke-alien-type-method :unparse type))

;;;; alien type defining stuff

(def!macro def-alien-type-translator (name lambda-list &body body)
  (let ((whole (gensym "WHOLE"))
	(env (gensym "ENV"))
	(defun-name (symbolicate "ALIEN-" name "-TYPE-TRANSLATOR")))
    (multiple-value-bind (body decls docs)
	(sb!kernel:parse-defmacro lambda-list whole body name
				  'def-alien-type-translator
				  :environment env)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (defun ,defun-name (,whole ,env)
	   (declare (ignorable ,env))
	   ,@decls
	   (block ,name
	     ,body))
	 (%def-alien-type-translator ',name #',defun-name ,docs)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %def-alien-type-translator (name translator docs)
    (declare (ignore docs))
    (setf (info :alien-type :kind name) :primitive)
    (setf (info :alien-type :translator name) translator)
    (clear-info :alien-type :definition name)
    #+nil
    (setf (fdocumentation name 'alien-type) docs)
    name))

(def!macro def-alien-type (name type &environment env)
  #!+sb-doc
  "Define the alien type NAME to be equivalent to TYPE. Name may be NIL for
   STRUCT and UNION types, in which case the name is taken from the type
   specifier."
  (with-auxiliary-alien-types env
    (let ((alien-type (parse-alien-type type env)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 ,@(when *new-auxiliary-types*
	     `((%def-auxiliary-alien-types ',*new-auxiliary-types*)))
	 ,@(when name
	     `((%def-alien-type ',name ',alien-type)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %def-auxiliary-alien-types (types)
    (dolist (info types)
      (destructuring-bind (kind name defn) info
	(macrolet ((frob (kind)
			 `(let ((old (info :alien-type ,kind name)))
			    (unless (or (null old) (alien-type-= old defn))
			      (warn
			       "redefining ~A ~S to be:~%  ~S,~%was:~%  ~S"
			       kind name defn old))
			    (setf (info :alien-type ,kind name) defn))))
	  (ecase kind
	    (:struct (frob :struct))
	    (:union (frob :union))
	    (:enum (frob :enum)))))))
  (defun %def-alien-type (name new)
    (ecase (info :alien-type :kind name)
      (:primitive
       (error "~S is a built-in alien type." name))
      (:defined
       (let ((old (info :alien-type :definition name)))
	 (unless (or (null old) (alien-type-= new old))
	   (warn "redefining ~S to be:~%  ~S,~%was~%  ~S"
		 name
		 (unparse-alien-type new)
		 (unparse-alien-type old)))))
      (:unknown))
    (setf (info :alien-type :definition name) new)
    (setf (info :alien-type :kind name) :defined)
    name))

;;;; the root alien type

(eval-when (:compile-toplevel :load-toplevel :execute)
  (create-alien-type-class-if-necessary 'root nil))

(def!struct (alien-type
	     (:make-load-form-fun sb!kernel:just-dump-it-normally)
	     (:constructor make-alien-type (&key class bits alignment)))
  (class 'root :type symbol)
  (bits nil :type (or null unsigned-byte))
  (alignment (guess-alignment bits) :type (or null unsigned-byte)))
(def!method print-object ((type alien-type) stream)
  (print-unreadable-object (type stream :type t)
    (prin1 (unparse-alien-type type) stream)))

;;;; the SAP type

(def-alien-type-class (system-area-pointer))

(def-alien-type-translator system-area-pointer ()
  (make-alien-system-area-pointer-type
   :bits #!-alpha sb!vm:word-bits #!+alpha 64))

(def-alien-type-method (system-area-pointer :unparse) (type)
  (declare (ignore type))
  'system-area-pointer)

(def-alien-type-method (system-area-pointer :lisp-rep) (type)
  (declare (ignore type))
  'system-area-pointer)

(def-alien-type-method (system-area-pointer :alien-rep) (type)
  (declare (ignore type))
  'system-area-pointer)

(def-alien-type-method (system-area-pointer :naturalize-gen) (type alien)
  (declare (ignore type))
  alien)

(def-alien-type-method (system-area-pointer :deport-gen) (type object)
  (declare (ignore type))
  (/noshow "doing alien type method SYSTEM-AREA-POINTER :DEPORT-GEN" object)
  object)

(def-alien-type-method (system-area-pointer :extract-gen) (type sap offset)
  (declare (ignore type))
  `(sap-ref-sap ,sap (/ ,offset sb!vm:byte-bits)))

;;;; the ALIEN-VALUE type

(def-alien-type-class (alien-value :include system-area-pointer))

(def-alien-type-method (alien-value :lisp-rep) (type)
  (declare (ignore type))
  nil)

(def-alien-type-method (alien-value :naturalize-gen) (type alien)
  `(%sap-alien ,alien ',type))

(def-alien-type-method (alien-value :deport-gen) (type value)
  (declare (ignore type))
  (/noshow "doing alien type method ALIEN-VALUE :DEPORT-GEN" value)
  `(alien-sap ,value))

;;; HEAP-ALIEN-INFO -- defstruct.
;;;
;;; Information describing a heap-allocated alien.
(def!struct (heap-alien-info
	     (:make-load-form-fun sb!kernel:just-dump-it-normally))
  ;; The type of this alien.
  (type (required-argument) :type alien-type)
  ;; The form to evaluate to produce the SAP pointing to where in the heap
  ;; it is.
  (sap-form (required-argument)))
(def!method print-object ((info heap-alien-info) stream)
  (print-unreadable-object (info stream :type t)
    (funcall (formatter "~S ~S")
	     stream
	     (heap-alien-info-sap-form info)
	     (unparse-alien-type (heap-alien-info-type info)))))

;;;; Interfaces to the different methods

(defun alien-type-= (type1 type2)
  #!+sb-doc
  "Return T iff TYPE1 and TYPE2 describe equivalent alien types."
  (or (eq type1 type2)
      (and (eq (alien-type-class type1)
	       (alien-type-class type2))
	   (invoke-alien-type-method :type= type1 type2))))

(defun alien-subtype-p (type1 type2)
  #!+sb-doc
  "Return T iff the alien type TYPE1 is a subtype of TYPE2. Currently, the
   only supported subtype relationships are is that any pointer type is a
   subtype of (* t), and any array type first dimension will match
   (array <eltype> nil ...). Otherwise, the two types have to be
   ALIEN-TYPE-=."
  (or (eq type1 type2)
      (invoke-alien-type-method :subtypep type1 type2)))

(defun compute-naturalize-lambda (type)
  `(lambda (alien ignore)
     (declare (ignore ignore))
     ,(invoke-alien-type-method :naturalize-gen type 'alien)))

(defun compute-deport-lambda (type)
  (declare (type alien-type type))
  (/noshow "entering COMPUTE-DEPORT-LAMBDA" type)
  (multiple-value-bind (form value-type)
      (invoke-alien-type-method :deport-gen type 'value)
    `(lambda (value ignore)
       (declare (type ,(or value-type
			   (compute-lisp-rep-type type)
			   `(alien ,type))
		      value)
		(ignore ignore))
       ,form)))

(defun compute-extract-lambda (type)
  `(lambda (sap offset ignore)
     (declare (type system-area-pointer sap)
	      (type unsigned-byte offset)
	      (ignore ignore))
     (naturalize ,(invoke-alien-type-method :extract-gen type 'sap 'offset)
		 ',type)))

(defun compute-deposit-lambda (type)
  (declare (type alien-type type))
  `(lambda (sap offset ignore value)
     (declare (type system-area-pointer sap)
	      (type unsigned-byte offset)
	      (ignore ignore))
     (let ((value (deport value ',type)))
       ,(invoke-alien-type-method :deposit-gen type 'sap 'offset 'value)
       ;; Note: the reason we don't just return the pre-deported value
       ;; is because that would inhibit any (deport (naturalize ...))
       ;; optimizations that might have otherwise happen. Re-naturalizing
       ;; the value might cause extra consing, but is flushable, so probably
       ;; results in better code.
       (naturalize value ',type))))

(defun compute-lisp-rep-type (type)
  (invoke-alien-type-method :lisp-rep type))

(defun compute-alien-rep-type (type)
  (invoke-alien-type-method :alien-rep type))

;;;; default methods

(def-alien-type-method (root :unparse) (type)
  `(<unknown-alien-type> ,(type-of type)))

(def-alien-type-method (root :type=) (type1 type2)
  (declare (ignore type1 type2))
  t)

(def-alien-type-method (root :subtypep) (type1 type2)
  (alien-type-= type1 type2))

(def-alien-type-method (root :lisp-rep) (type)
  (declare (ignore type))
  nil)

(def-alien-type-method (root :alien-rep) (type)
  (declare (ignore type))
  '*)

(def-alien-type-method (root :naturalize-gen) (type alien)
  (declare (ignore alien))
  (error "cannot represent ~S typed aliens" type))

(def-alien-type-method (root :deport-gen) (type object)
  (declare (ignore object))
  (error "cannot represent ~S typed aliens" type))

(def-alien-type-method (root :extract-gen) (type sap offset)
  (declare (ignore sap offset))
  (error "cannot represent ~S typed aliens" type))

(def-alien-type-method (root :deposit-gen) (type sap offset value)
  `(setf ,(invoke-alien-type-method :extract-gen type sap offset) ,value))

(def-alien-type-method (root :arg-tn) (type state)
  (declare (ignore state))
  (error "Aliens of type ~S cannot be passed as arguments to CALL-OUT."
	 (unparse-alien-type type)))

(def-alien-type-method (root :result-tn) (type state)
  (declare (ignore state))
  (error "Aliens of type ~S cannot be returned from CALL-OUT."
	 (unparse-alien-type type)))

;;;; the INTEGER type

(def-alien-type-class (integer)
  (signed t :type (member t nil)))

(def-alien-type-translator signed (&optional (bits sb!vm:word-bits))
  (make-alien-integer-type :bits bits))

(def-alien-type-translator integer (&optional (bits sb!vm:word-bits))
  (make-alien-integer-type :bits bits))

(def-alien-type-translator unsigned (&optional (bits sb!vm:word-bits))
  (make-alien-integer-type :bits bits :signed nil))

(def-alien-type-method (integer :unparse) (type)
  (list (if (alien-integer-type-signed type) 'signed 'unsigned)
	(alien-integer-type-bits type)))

(def-alien-type-method (integer :type=) (type1 type2)
  (and (eq (alien-integer-type-signed type1)
	   (alien-integer-type-signed type2))
       (= (alien-integer-type-bits type1)
	  (alien-integer-type-bits type2))))

(def-alien-type-method (integer :lisp-rep) (type)
  (list (if (alien-integer-type-signed type) 'signed-byte 'unsigned-byte)
	(alien-integer-type-bits type)))

(def-alien-type-method (integer :alien-rep) (type)
  (list (if (alien-integer-type-signed type) 'signed-byte 'unsigned-byte)
	(alien-integer-type-bits type)))

(def-alien-type-method (integer :naturalize-gen) (type alien)
  (declare (ignore type))
  alien)

(def-alien-type-method (integer :deport-gen) (type value)
  (declare (ignore type))
  value)

(def-alien-type-method (integer :extract-gen) (type sap offset)
  (declare (type alien-integer-type type))
  (let ((ref-fun
	 (if (alien-integer-type-signed type)
	  (case (alien-integer-type-bits type)
	    (8 'signed-sap-ref-8)
	    (16 'signed-sap-ref-16)
	    (32 'signed-sap-ref-32)
	    #!+alpha (64 'signed-sap-ref-64))
	  (case (alien-integer-type-bits type)
	    (8 'sap-ref-8)
	    (16 'sap-ref-16)
	    (32 'sap-ref-32)
	    #!+alpha (64 'sap-ref-64)))))
    (if ref-fun
	`(,ref-fun ,sap (/ ,offset sb!vm:byte-bits))
	(error "cannot extract ~D bit integers"
	       (alien-integer-type-bits type)))))

;;;; the BOOLEAN type

(def-alien-type-class (boolean :include integer :include-args (signed)))

;;; FIXME: Check to make sure that we aren't attaching user-readable
;;; stuff to CL:BOOLEAN in any way which impairs ANSI compliance.
(def-alien-type-translator boolean (&optional (bits sb!vm:word-bits))
  (make-alien-boolean-type :bits bits :signed nil))

(def-alien-type-method (boolean :unparse) (type)
  `(boolean ,(alien-boolean-type-bits type)))

(def-alien-type-method (boolean :lisp-rep) (type)
  (declare (ignore type))
  `(member t nil))

(def-alien-type-method (boolean :naturalize-gen) (type alien)
  (declare (ignore type))
  `(not (zerop ,alien)))

(def-alien-type-method (boolean :deport-gen) (type value)
  (declare (ignore type))
  `(if ,value 1 0))

;;;; the ENUM type

(def-alien-type-class (enum :include (integer (:bits 32))
			    :include-args (signed))
  name		; name of this enum (if any)
  from		; alist from keywords to integers.
  to		; alist or vector from integers to keywords.
  kind		; Kind of from mapping, :vector or :alist.
  offset)	; Offset to add to value for :vector from mapping.

(def-alien-type-translator enum (&whole
				 type name
				 &rest mappings
				 &environment env)
  (cond (mappings
	 (let ((result (parse-enum name mappings)))
	   (when name
	     (multiple-value-bind (old old-p)
		 (auxiliary-alien-type :enum name env)
	       (when old-p
		 (unless (alien-type-= result old)
		   (warn "redefining alien enum ~S" name))))
	     (setf (auxiliary-alien-type :enum name env) result))
	   result))
	(name
	 (multiple-value-bind (result found)
	     (auxiliary-alien-type :enum name env)
	   (unless found
	     (error "unknown enum type: ~S" name))
	   result))
	(t
	 (error "empty enum type: ~S" type))))

(defun parse-enum (name elements)
  (when (null elements)
    (error "An enumeration must contain at least one element."))
  (let ((min nil)
	(max nil)
	(from-alist ())
	(prev -1))
    (declare (list from-alist))
    (dolist (el elements)
      (multiple-value-bind (sym val)
	  (if (listp el)
	      (values (first el) (second el))
	      (values el (1+ prev)))
	(setf prev val)
	(unless (keywordp sym)
	  (error "The enumeration element ~S is not a keyword." sym))
	(unless (integerp val)
	  (error "The element value ~S is not an integer." val))
	(unless (and max (> max val)) (setq max val))
	(unless (and min (< min val)) (setq min val))
	(when (rassoc val from-alist)
	  (error "The element value ~S is used more than once." val))
	(when (assoc sym from-alist :test #'eq)
	  (error "The enumeration element ~S is used more than once." sym))
	(push (cons sym val) from-alist)))
    (let* ((signed (minusp min))
	   (min-bits (if signed
			 (1+ (max (integer-length min)
				  (integer-length max)))
			 (integer-length max))))
      (when (> min-bits 32)
	(error "can't represent enums needing more than 32 bits"))
      (setf from-alist (sort from-alist #'< :key #'cdr))
      (cond
       ;; If range is at least 20% dense, use vector mapping. Crossover
       ;; point solely on basis of space would be 25%. Vector mapping
       ;; is always faster, so give the benefit of the doubt.
       ((< 0.2 (/ (float (length from-alist)) (float (- max min))))
	;; If offset is small and ignorable, ignore it to save time.
	(when (< 0 min 10) (setq min 0))
	(let ((to (make-array (1+ (- max min)))))
	  (dolist (el from-alist)
	    (setf (svref to (- (cdr el) min)) (car el)))
	  (make-alien-enum-type :name name :signed signed
				:from from-alist :to to :kind
				:vector :offset (- min))))
       (t
	(make-alien-enum-type :name name :signed signed
			      :from from-alist
			      :to (mapcar #'(lambda (x) (cons (cdr x) (car x)))
					  from-alist)
			      :kind :alist))))))

(def-alien-type-method (enum :unparse) (type)
  `(enum ,(alien-enum-type-name type)
	 ,@(let ((prev -1))
	     (mapcar #'(lambda (mapping)
			 (let ((sym (car mapping))
			       (value (cdr mapping)))
			   (prog1
			       (if (= (1+ prev) value)
				   sym
				   `(,sym ,value))
			     (setf prev value))))
		     (alien-enum-type-from type)))))

(def-alien-type-method (enum :type=) (type1 type2)
  (and (eq (alien-enum-type-name type1)
	   (alien-enum-type-name type2))
       (equal (alien-enum-type-from type1)
	      (alien-enum-type-from type2))))

(def-alien-type-method (enum :lisp-rep) (type)
  `(member ,@(mapcar #'car (alien-enum-type-from type))))

(def-alien-type-method (enum :naturalize-gen) (type alien)
  (ecase (alien-enum-type-kind type)
    (:vector
     `(svref ',(alien-enum-type-to type)
	     (+ ,alien ,(alien-enum-type-offset type))))
    (:alist
     `(ecase ,alien
	,@(mapcar #'(lambda (mapping)
		      `(,(car mapping) ,(cdr mapping)))
		  (alien-enum-type-to type))))))

(def-alien-type-method (enum :deport-gen) (type value)
  `(ecase ,value
     ,@(mapcar #'(lambda (mapping)
		   `(,(car mapping) ,(cdr mapping)))
	       (alien-enum-type-from type))))

;;;; the FLOAT types

(def-alien-type-class (float)
  (type (required-argument) :type symbol))

(def-alien-type-method (float :unparse) (type)
  (alien-float-type-type type))

(def-alien-type-method (float :lisp-rep) (type)
  (alien-float-type-type type))

(def-alien-type-method (float :alien-rep) (type)
  (alien-float-type-type type))

(def-alien-type-method (float :naturalize-gen) (type alien)
  (declare (ignore type))
  alien)

(def-alien-type-method (float :deport-gen) (type value)
  (declare (ignore type))
  value)

(def-alien-type-class (single-float :include (float (:bits 32))
				    :include-args (type)))

(def-alien-type-translator single-float ()
  (make-alien-single-float-type :type 'single-float))

(def-alien-type-method (single-float :extract-gen) (type sap offset)
  (declare (ignore type))
  `(sap-ref-single ,sap (/ ,offset sb!vm:byte-bits)))

(def-alien-type-class (double-float :include (float (:bits 64))
				    :include-args (type)))

(def-alien-type-translator double-float ()
  (make-alien-double-float-type :type 'double-float))

(def-alien-type-method (double-float :extract-gen) (type sap offset)
  (declare (ignore type))
  `(sap-ref-double ,sap (/ ,offset sb!vm:byte-bits)))

#!+long-float
(def-alien-type-class (long-float :include (float (:bits #!+x86 96
							  #!+sparc 128))
				  :include-args (type)))

#!+long-float
(def-alien-type-translator long-float ()
  (make-alien-long-float-type :type 'long-float))

#!+long-float
(def-alien-type-method (long-float :extract-gen) (type sap offset)
  (declare (ignore type))
  `(sap-ref-long ,sap (/ ,offset sb!vm:byte-bits)))

;;;; the POINTER type

(def-alien-type-class (pointer :include (alien-value (:bits
						      #!-alpha sb!vm:word-bits
						      #!+alpha 64)))
  (to nil :type (or alien-type null)))

(def-alien-type-translator * (to &environment env)
  (make-alien-pointer-type :to (if (eq to t) nil (parse-alien-type to env))))

(def-alien-type-method (pointer :unparse) (type)
  (let ((to (alien-pointer-type-to type)))
    `(* ,(if to
	     (%unparse-alien-type to)
	     t))))

(def-alien-type-method (pointer :type=) (type1 type2)
  (let ((to1 (alien-pointer-type-to type1))
	(to2 (alien-pointer-type-to type2)))
    (if to1
	(if to2
	    (alien-type-= to1 to2)
	    nil)
	(null to2))))

(def-alien-type-method (pointer :subtypep) (type1 type2)
  (and (alien-pointer-type-p type2)
       (let ((to1 (alien-pointer-type-to type1))
	     (to2 (alien-pointer-type-to type2)))
	 (if to1
	     (if to2
		 (alien-subtype-p to1 to2)
		 t)
	     (null to2)))))

(def-alien-type-method (pointer :deport-gen) (type value)
  (/noshow "doing alien type method POINTER :DEPORT-GEN" type value)
  (values
   ;; FIXME: old version, highlighted a bug in xc optimization
   `(etypecase ,value
      (null
       (int-sap 0))
      (system-area-pointer
       ,value)
      ((alien ,type)
       (alien-sap ,value)))
   ;; new version, works around bug in xc optimization
   #+nil
   `(etypecase ,value
      (system-area-pointer
       ,value)
      ((alien ,type)
       (alien-sap ,value))
      (null
       (int-sap 0)))
   `(or null system-area-pointer (alien ,type))))

;;;; the MEM-BLOCK type

(def-alien-type-class (mem-block :include alien-value))

(def-alien-type-method (mem-block :extract-gen) (type sap offset)
  (declare (ignore type))
  `(sap+ ,sap (/ ,offset sb!vm:byte-bits)))

(def-alien-type-method (mem-block :deposit-gen) (type sap offset value)
  (let ((bits (alien-mem-block-type-bits type)))
    (unless bits
      (error "can't deposit aliens of type ~S (unknown size)" type))
    `(sb!kernel:system-area-copy ,value 0 ,sap ,offset ',bits)))

;;;; the ARRAY type

(def-alien-type-class (array :include mem-block)
  (element-type (required-argument) :type alien-type)
  (dimensions (required-argument) :type list))

(def-alien-type-translator array (ele-type &rest dims &environment env)
  (when dims
    (unless (typep (first dims) '(or index null))
      (error "The first dimension is not a non-negative fixnum or NIL: ~S"
	     (first dims)))
    (let ((loser (find-if-not #'(lambda (x) (typep x 'index))
			      (rest dims))))
      (when loser
	(error "A dimension is not a non-negative fixnum: ~S" loser))))
	
  (let ((type (parse-alien-type ele-type env)))
    (make-alien-array-type
     :element-type type
     :dimensions dims
     :alignment (alien-type-alignment type)
     :bits (if (and (alien-type-bits type)
		    (every #'integerp dims))
	       (* (align-offset (alien-type-bits type)
				(alien-type-alignment type))
		  (reduce #'* dims))))))

(def-alien-type-method (array :unparse) (type)
  `(array ,(%unparse-alien-type (alien-array-type-element-type type))
	  ,@(alien-array-type-dimensions type)))

(def-alien-type-method (array :type=) (type1 type2)
  (and (equal (alien-array-type-dimensions type1)
	      (alien-array-type-dimensions type2))
       (alien-type-= (alien-array-type-element-type type1)
		     (alien-array-type-element-type type2))))

(def-alien-type-method (array :subtypep) (type1 type2)
  (and (alien-array-type-p type2)
       (let ((dim1 (alien-array-type-dimensions type1))
	     (dim2 (alien-array-type-dimensions type2)))
	 (and (= (length dim1) (length dim2))
	      (or (and dim2
		       (null (car dim2))
		       (equal (cdr dim1) (cdr dim2)))
		  (equal dim1 dim2))
	      (alien-subtype-p (alien-array-type-element-type type1)
			       (alien-array-type-element-type type2))))))

;;;; the RECORD type

(def!struct (alien-record-field
	     (:make-load-form-fun sb!kernel:just-dump-it-normally))
  (name (required-argument) :type symbol)
  (type (required-argument) :type alien-type)
  (bits nil :type (or unsigned-byte null))
  (offset 0 :type unsigned-byte))
(def!method print-object ((field alien-record-field) stream)
  (print-unreadable-object (field stream :type t)
    (format stream
	    "~S ~S~@[:~D~]"
	    (alien-record-field-type field)
	    (alien-record-field-name field)
	    (alien-record-field-bits field))))

(def-alien-type-class (record :include mem-block)
  (kind :struct :type (member :struct :union))
  (name nil :type (or symbol null))
  (fields nil :type list))

(def-alien-type-translator struct (name &rest fields &environment env)
  (parse-alien-record-type :struct name fields env))

(def-alien-type-translator union (name &rest fields &environment env)
  (parse-alien-record-type :union name fields env))

(defun parse-alien-record-type (kind name fields env)
  (declare (type (or sb!kernel:lexenv null) env))
  (cond (fields
	 (let* ((old (and name (auxiliary-alien-type kind name env)))
		(old-fields (and old (alien-record-type-fields old))))
	   (cond (old-fields
		  ;; KLUDGE: We can't easily compare the new fields
		  ;; against the old fields, since the old fields have
		  ;; already been parsed into an internal
		  ;; representation, so we just punt, assuming that
		  ;; they're consistent. -- WHN 200000505
		  #|
		  (unless (equal fields old-fields)
		    ;; FIXME: Perhaps this should be a warning, and we
		    ;; should overwrite the old definition and proceed?
		    (error "mismatch in fields for ~S~%  old ~S~%  new ~S"
			   name old-fields fields))
                  |#
		  old)
		 (t
		  (let ((new (make-alien-record-type :name name
						     :kind kind)))
		    (when name
		      (setf (auxiliary-alien-type kind name env) new))
		    (parse-alien-record-fields new fields env)
		    new)))))
	(name
	 (or (auxiliary-alien-type kind name env)
	     (setf (auxiliary-alien-type kind name env)
		   (make-alien-record-type :name name :kind kind))))
	(t
	 (make-alien-record-type :kind kind))))

;;; This is used by PARSE-ALIEN-TYPE to parse the fields of struct and
;;; union types. RESULT holds the record type we are paring the fields
;;; of, and FIELDS is the list of field specifications.
(defun parse-alien-record-fields (result fields env)
  (declare (type alien-record-type result)
	   (type list fields))
  (let ((total-bits 0)
	(overall-alignment 1)
	(parsed-fields nil))
    (dolist (field fields)
      (destructuring-bind (var type &optional bits) field
	(declare (ignore bits))
	(let* ((field-type (parse-alien-type type env))
	       (bits (alien-type-bits field-type))
	       (alignment (alien-type-alignment field-type))
	       (parsed-field
		(make-alien-record-field :type field-type
					 :name var)))
	  (push parsed-field parsed-fields)
	  (when (null bits)
	    (error "unknown size: ~S" (unparse-alien-type field-type)))
	  (when (null alignment)
	    (error "unknown alignment: ~S" (unparse-alien-type field-type)))
	  (setf overall-alignment (max overall-alignment alignment))
	  (ecase (alien-record-type-kind result)
	    (:struct
	     (let ((offset (align-offset total-bits alignment)))
	       (setf (alien-record-field-offset parsed-field) offset)
	       (setf total-bits (+ offset bits))))
	    (:union
	     (setf total-bits (max total-bits bits)))))))
    (let ((new (nreverse parsed-fields)))
      (setf (alien-record-type-fields result) new))
    (setf (alien-record-type-alignment result) overall-alignment)
    (setf (alien-record-type-bits result)
	  (align-offset total-bits overall-alignment))))

(def-alien-type-method (record :unparse) (type)
  `(,(case (alien-record-type-kind type)
       (:struct 'struct)
       (:union 'union)
       (t '???))
    ,(alien-record-type-name type)
    ,@(unless (member type *record-types-already-unparsed* :test #'eq)
	(push type *record-types-already-unparsed*)
	(mapcar #'(lambda (field)
		    `(,(alien-record-field-name field)
		      ,(%unparse-alien-type (alien-record-field-type field))
		      ,@(if (alien-record-field-bits field)
			    (list (alien-record-field-bits field)))))
		(alien-record-type-fields type)))))

;;; Test the record fields. The depth is limiting in case of cyclic
;;; pointers.
(defun record-fields-match (fields1 fields2 depth)
  (declare (type list fields1 fields2)
	   (type (mod 64) depth))
  (labels ((record-type-= (type1 type2 depth)
	     (and (eq (alien-record-type-name type1)
		      (alien-record-type-name type2))
		  (eq (alien-record-type-kind type1)
		      (alien-record-type-kind type2))
		  (= (length (alien-record-type-fields type1))
		     (length (alien-record-type-fields type2)))
		  (record-fields-match (alien-record-type-fields type1)
				       (alien-record-type-fields type2)
				       (1+ depth))))
	   (pointer-type-= (type1 type2 depth)
	     (let ((to1 (alien-pointer-type-to type1))
		   (to2 (alien-pointer-type-to type2)))
	       (if to1
		   (if to2
		       (type-= to1 to2 (1+ depth))
		       nil)
		   (null to2))))
	   (type-= (type1 type2 depth)
	     (cond ((and (alien-pointer-type-p type1)
			 (alien-pointer-type-p type2))
		    (or (> depth 10)
			(pointer-type-= type1 type2 depth)))
		   ((and (alien-record-type-p type1)
			 (alien-record-type-p type2))
		    (record-type-= type1 type2 depth))
		   (t
		    (alien-type-= type1 type2)))))
    (do ((fields1-rem fields1 (rest fields1-rem))
	 (fields2-rem fields2 (rest fields2-rem)))
	((or (eq fields1-rem fields2-rem)
	     (endp fields1-rem) (endp fields2-rem))
	 (eq fields1-rem fields2-rem))
      (let ((field1 (first fields1-rem))
	    (field2 (first fields2-rem)))
	(declare (type alien-record-field field1 field2))
	(unless (and (eq (alien-record-field-name field1)
			 (alien-record-field-name field2))
		     (eql (alien-record-field-bits field1)
			  (alien-record-field-bits field2))
		     (eql (alien-record-field-offset field1)
			  (alien-record-field-offset field2))
		     (let ((field1 (alien-record-field-type field1))
			   (field2 (alien-record-field-type field2)))
		       (type-= field1 field2 (1+ depth))))
	  (return nil))))))

(def-alien-type-method (record :type=) (type1 type2)
  (and (eq (alien-record-type-name type1)
	   (alien-record-type-name type2))
       (eq (alien-record-type-kind type1)
	   (alien-record-type-kind type2))
       (= (length (alien-record-type-fields type1))
	  (length (alien-record-type-fields type2)))
       (record-fields-match (alien-record-type-fields type1)
			    (alien-record-type-fields type2) 0)))

;;;; the FUNCTION and VALUES types

(defvar *values-type-okay* nil)

(def-alien-type-class (function :include mem-block)
  (result-type (required-argument) :type alien-type)
  (arg-types (required-argument) :type list)
  (stub nil :type (or null function)))

(def-alien-type-translator function (result-type &rest arg-types
						 &environment env)
  (make-alien-function-type
   :result-type (let ((*values-type-okay* t))
		  (parse-alien-type result-type env))
   :arg-types (mapcar (lambda (arg-type) (parse-alien-type arg-type env))
		      arg-types)))

(def-alien-type-method (function :unparse) (type)
  `(function ,(%unparse-alien-type (alien-function-type-result-type type))
	     ,@(mapcar #'%unparse-alien-type
		       (alien-function-type-arg-types type))))

(def-alien-type-method (function :type=) (type1 type2)
  (and (alien-type-= (alien-function-type-result-type type1)
		     (alien-function-type-result-type type2))
       (= (length (alien-function-type-arg-types type1))
	  (length (alien-function-type-arg-types type2)))
       (every #'alien-type-=
	      (alien-function-type-arg-types type1)
	      (alien-function-type-arg-types type2))))

(def-alien-type-class (values)
  (values (required-argument) :type list))

(def-alien-type-translator values (&rest values &environment env)
  (unless *values-type-okay*
    (error "cannot use values types here"))
  (let ((*values-type-okay* nil))
    (make-alien-values-type
     :values (mapcar (lambda (alien-type) (parse-alien-type alien-type env))
		     values))))

(def-alien-type-method (values :unparse) (type)
  `(values ,@(mapcar #'%unparse-alien-type
		     (alien-values-type-values type))))

(def-alien-type-method (values :type=) (type1 type2)
  (and (= (length (alien-values-type-values type1))
	  (length (alien-values-type-values type2)))
       (every #'alien-type-=
	      (alien-values-type-values type1)
	      (alien-values-type-values type2))))

;;;; a structure definition needed both in the target and in the
;;;; cross-compilation host

;;; information about local aliens. The WITH-ALIEN macro builds one of
;;; these structures and LOCAL-ALIEN and friends communicate
;;; information about how that local alien is represented.
(def!struct (local-alien-info
	     (:make-load-form-fun sb!kernel:just-dump-it-normally)
	     (:constructor make-local-alien-info
			   (&key type force-to-memory-p)))
  ;; the type of the local alien
  (type (required-argument) :type alien-type)
  ;; T if this local alien must be forced into memory. Using the ADDR macro
  ;; on a local alien will set this.
  (force-to-memory-p (or (alien-array-type-p type) (alien-record-type-p type))
		     :type (member t nil)))
(def!method print-object ((info local-alien-info) stream)
  (print-unreadable-object (info stream :type t)
    (format stream
	    "~:[~;(forced to stack) ~]~S"
	    (local-alien-info-force-to-memory-p info)
	    (unparse-alien-type (local-alien-info-type info)))))

;;;; the ADDR macro

(sb!kernel:defmacro-mundanely addr (expr &environment env)
  #!+sb-doc
  "Return an Alien pointer to the data addressed by Expr, which must be a call
   to SLOT or DEREF, or a reference to an Alien variable."
  (let ((form (sb!xc:macroexpand expr env)))
    (or (typecase form
	  (cons
	   (case (car form)
	     (slot
	      (cons '%slot-addr (cdr form)))
	     (deref
	      (cons '%deref-addr (cdr form)))
	     (%heap-alien
	      (cons '%heap-alien-addr (cdr form)))
	     (local-alien
	      (let ((info (let ((info-arg (second form)))
			    (and (consp info-arg)
				 (eq (car info-arg) 'quote)
				 (second info-arg)))))
		(unless (local-alien-info-p info)
		  (error "Something is wrong, LOCAL-ALIEN-INFO not found: ~S"
			 form))
		(setf (local-alien-info-force-to-memory-p info) t))
	      (cons '%local-alien-addr (cdr form)))))
	  (symbol
	   (let ((kind (info :variable :kind form)))
	     (when (eq kind :alien)
	       `(%heap-alien-addr ',(info :variable :alien-info form))))))
	(error "~S is not a valid L-value." form))))
