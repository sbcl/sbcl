;;;; permutation vectors

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is derived from software originally released by Xerox
;;;; Corporation. Copyright and release statements follow. Later modifications
;;;; to the software are in the public domain and are provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for more
;;;; information.

;;;; copyright information from original PCL sources:
;;;;
;;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;;; All rights reserved.
;;;;
;;;; Use and copying of this software and preparation of derivative works based
;;;; upon this software are permitted. Any distribution of this software or
;;;; derivative works must comply with all applicable United States export
;;;; control laws.
;;;;
;;;; This software is made available AS IS, and Xerox Corporation makes no
;;;; warranty about the software, its performance or its conformity to any
;;;; specification.

(in-package "SB-PCL")

(defmacro instance-slot-index (wrapper slot-name)
  `(let ((pos 0))
     (declare (fixnum pos))
     (block loop
       (dolist (sn (wrapper-instance-slots-layout ,wrapper))
	 (when (eq ,slot-name sn) (return-from loop pos))
	 (incf pos)))))

(defun pv-cache-limit-fn (nlines)
  (default-limit-fn nlines))

(defstruct (pv-table (:predicate pv-tablep)
		     (:constructor make-pv-table-internal
				   (slot-name-lists call-list))
		     (:copier nil))
  (cache nil :type (or cache null))
  (pv-size 0 :type fixnum)
  (slot-name-lists nil :type list)
  (call-list nil :type list))

#-sb-fluid (declaim (sb-ext:freeze-type pv-table))

(defvar *initial-pv-table* (make-pv-table-internal nil nil))

; help new slot-value-using-class methods affect fast iv access
(defvar *all-pv-table-list* nil)

(defun make-pv-table (&key slot-name-lists call-list)
  (let ((pv-table (make-pv-table-internal slot-name-lists call-list)))
    (push pv-table *all-pv-table-list*)
    pv-table))

(defun make-pv-table-type-declaration (var)
  `(type pv-table ,var))

(defvar *slot-name-lists-inner* (make-hash-table :test 'equal))
(defvar *slot-name-lists-outer* (make-hash-table :test 'equal))

;;; Entries in this are lists of (table . pv-offset-list).
(defvar *pv-key-to-pv-table-table* (make-hash-table :test 'equal))

(defun intern-pv-table (&key slot-name-lists call-list)
  (let ((new-p nil))
    (flet ((inner (x)
	     (or (gethash x *slot-name-lists-inner*)
		 (setf (gethash x *slot-name-lists-inner*) (copy-list x))))
	   (outer (x)
	     (or (gethash x *slot-name-lists-outer*)
		 (setf (gethash x *slot-name-lists-outer*)
		       (let ((snl (copy-list (cdr x)))
			     (cl (car x)))
			 (setq new-p t)
			 (make-pv-table :slot-name-lists snl
					:call-list cl))))))
    (let ((pv-table (outer (mapcar #'inner (cons call-list slot-name-lists)))))
      (when new-p
	(let ((pv-index 1))
	  (dolist (slot-name-list slot-name-lists)
	    (dolist (slot-name (cdr slot-name-list))
	      (note-pv-table-reference slot-name pv-index pv-table)
	      (incf pv-index)))
	  (dolist (gf-call call-list)
	    (note-pv-table-reference gf-call pv-index pv-table)
	    (incf pv-index))
	  (setf (pv-table-pv-size pv-table) pv-index)))
      pv-table))))

(defun note-pv-table-reference (ref pv-offset pv-table)
  (let ((entry (gethash ref *pv-key-to-pv-table-table*)))
    (when (listp entry)
      (let ((table-entry (assq pv-table entry)))
	(when (and (null table-entry)
		   (> (length entry) 8))
	  (let ((new-table-table (make-hash-table :size 16 :test 'eq)))
	    (dolist (table-entry entry)
	      (setf (gethash (car table-entry) new-table-table)
		    (cdr table-entry)))
	    (setf (gethash ref *pv-key-to-pv-table-table*) new-table-table)))
	(when (listp entry)
	  (if (null table-entry)
	      (let ((new (cons pv-table pv-offset)))
		(if (consp entry)
		    (push new (cdr entry))
		    (setf (gethash ref *pv-key-to-pv-table-table*)
			  (list new))))
	      (push pv-offset (cdr table-entry)))
	  (return-from note-pv-table-reference nil))))
    (let ((list (gethash pv-table entry)))
      (if (consp list)
	  (push pv-offset (cdr list))
	  (setf (gethash pv-table entry) (list pv-offset)))))
  nil)

(defun map-pv-table-references-of (ref function)
  (let ((entry (gethash ref *pv-key-to-pv-table-table*)))
    (if (listp entry)
	(dolist (table+pv-offset-list entry)
	  (funcall function
		   (car table+pv-offset-list)
		   (cdr table+pv-offset-list)))
	(maphash function entry)))
  ref)

(defvar *pvs* (make-hash-table :test 'equal))

(defun optimize-slot-value-by-class-p (class slot-name type)
  (or (not (eq *boot-state* 'complete))
      (let ((slotd (find-slot-definition class slot-name)))
	(and slotd
	     (slot-accessor-std-p slotd type)))))

(defun compute-pv-slot (slot-name wrapper class class-slots class-slot-p-cell)
  (if (symbolp slot-name)
      (when (optimize-slot-value-by-class-p class slot-name 'all)
	(or (instance-slot-index wrapper slot-name)
	    (let ((cell (assq slot-name class-slots)))
	      (when cell
		(setf (car class-slot-p-cell) t)
		cell))))
      (when (consp slot-name)
	(dolist (type '(reader writer) nil)
	  (when (eq (car slot-name) type)
	    (return
	      (let* ((gf-name (cadr slot-name))
		     (gf (gdefinition gf-name))
		     (location (when (eq *boot-state* 'complete)
				 (accessor-values1 gf type class))))
		(when (consp location)
		  (setf (car class-slot-p-cell) t))
		location)))))))

(defun compute-pv (slot-name-lists wrappers)
  (unless (listp wrappers) (setq wrappers (list wrappers)))
  (let* ((not-simple-p-cell (list nil))
	 (elements
          (let ((elements nil))
            (dolist (slot-names slot-name-lists)
	      (when slot-names
		(let* ((wrapper     (pop wrappers))
		       (std-p (typep wrapper 'wrapper))
		       (class       (wrapper-class* wrapper))
		       (class-slots (and std-p (wrapper-class-slots wrapper))))
		  (dolist (slot-name (cdr slot-names))
                    ;; Original PCL code had this idiom.  why not:
                    ;;
                    ;; (WHEN STD-P
                    ;;   (PUSH ...)) ?
                    (push (when std-p
                            (compute-pv-slot slot-name wrapper class
                                             class-slots not-simple-p-cell))
                          elements)))))
            (nreverse elements))))
    (if (car not-simple-p-cell)
	(make-permutation-vector (cons t elements))
	(or (gethash elements *pvs*)
	    (setf (gethash elements *pvs*)
		  (make-permutation-vector (cons nil elements)))))))

(defun compute-calls (call-list wrappers)
  (declare (ignore call-list wrappers))
  #||
  (map 'vector
       (lambda (call)
	 (compute-emf-from-wrappers call wrappers))
       call-list)
  ||#
  '#())

#|| ; Need to finish this, then write the maintenance functions.
(defun compute-emf-from-wrappers (call wrappers)
  (when call
    (destructuring-bind (gf-name nreq restp arg-info) call
      (if (eq gf-name 'make-instance)
	  (error "should not get here") ; there is another mechanism for this.
	  (lambda (&rest args)
	    (if (not (eq *boot-state* 'complete))
		(apply (gdefinition gf-name) args)
		(let* ((gf (gdefinition gf-name))
		       (arg-info (arg-info-reader gf))
		       (classes '?)
		       (types '?)
		       (emf (cache-miss-values-internal gf arg-info
							wrappers classes types
							'caching)))
		  (update-all-pv-tables call wrappers emf)
		  (invoke-emf emf args))))))))
||#

(defun make-permutation-vector (indexes)
  (make-array (length indexes) :initial-contents indexes))

(defun pv-table-lookup (pv-table pv-wrappers)
  (let* ((slot-name-lists (pv-table-slot-name-lists pv-table))
	 (call-list (pv-table-call-list pv-table))
	 (cache (or (pv-table-cache pv-table)
		    (setf (pv-table-cache pv-table)
			  (get-cache (- (length slot-name-lists)
					(count nil slot-name-lists))
				     t
				     #'pv-cache-limit-fn
				     2)))))
    (or (probe-cache cache pv-wrappers)
	(let* ((pv (compute-pv slot-name-lists pv-wrappers))
	       (calls (compute-calls call-list pv-wrappers))
	       (pv-cell (cons pv calls))
	       (new-cache (fill-cache cache pv-wrappers pv-cell)))
	  (unless (eq new-cache cache)
	    (setf (pv-table-cache pv-table) new-cache))
	  pv-cell))))

(defun make-pv-type-declaration (var)
  `(type simple-vector ,var))

(defvar *empty-pv* #())

(defmacro pvref (pv index)
  `(svref ,pv ,index))

(defmacro copy-pv (pv)
  `(copy-seq ,pv))

(defun make-calls-type-declaration (var)
  `(type simple-vector ,var))

(defmacro callsref (calls index)
  `(svref ,calls ,index))

(defvar *pv-table-cache-update-info* nil)

(defun update-pv-table-cache-info (class)
  (let ((slot-names-for-pv-table-update nil)
	(new-icui nil))
    (dolist (icu *pv-table-cache-update-info*)
      (if (eq (car icu) class)
	  (pushnew (cdr icu) slot-names-for-pv-table-update)
	  (push icu new-icui)))
    (setq *pv-table-cache-update-info* new-icui)
    (when slot-names-for-pv-table-update
      (update-all-pv-table-caches class slot-names-for-pv-table-update))))

(defun update-all-pv-table-caches (class slot-names)
  (let* ((cwrapper (class-wrapper class))
	 (std-p (typep cwrapper 'wrapper))
	 (class-slots (and std-p (wrapper-class-slots cwrapper)))
	 (class-slot-p-cell (list nil))
	 (new-values (mapcar (lambda (slot-name)
			       (cons slot-name
				     (when std-p
				       (compute-pv-slot
					slot-name cwrapper class
					class-slots class-slot-p-cell))))
			     slot-names))
	 (pv-tables nil))
    (dolist (slot-name slot-names)
      (map-pv-table-references-of
       slot-name
       (lambda (pv-table pv-offset-list)
	 (declare (ignore pv-offset-list))
	 (pushnew pv-table pv-tables))))
    (dolist (pv-table pv-tables)
      (let* ((cache (pv-table-cache pv-table))
	     (slot-name-lists (pv-table-slot-name-lists pv-table))
	     (pv-size (pv-table-pv-size pv-table))
	     (pv-map (make-array pv-size :initial-element nil)))
	(let ((map-index 1) (param-index 0))
	  (dolist (slot-name-list slot-name-lists)
	    (dolist (slot-name (cdr slot-name-list))
	      (let ((a (assoc slot-name new-values)))
		(setf (svref pv-map map-index)
		      (and a (cons param-index (cdr a)))))
	      (incf map-index))
	    (incf param-index)))
	(when cache
	  (map-cache (lambda (wrappers pv-cell)
		       (setf (car pv-cell)
			     (update-slots-in-pv wrappers (car pv-cell)
						 cwrapper pv-size pv-map)))
		     cache))))))

(defun update-slots-in-pv (wrappers pv cwrapper pv-size pv-map)
  (if (not (if (atom wrappers)
	       (eq cwrapper wrappers)
	       (dolist (wrapper wrappers nil)
		 (when (eq wrapper cwrapper)
		   (return t)))))
      pv
      (let* ((old-intern-p (listp (pvref pv 0)))
	     (new-pv (if old-intern-p
			 (copy-pv pv)
			 pv))
	     (new-intern-p t))
	(if (atom wrappers)
	    (dotimes-fixnum (i pv-size)
	      (when (consp (let ((map (svref pv-map i)))
			     (if map
				 (setf (pvref new-pv i) (cdr map))
				 (pvref new-pv i))))
		(setq new-intern-p nil)))
	    (let ((param 0))
	      (dolist (wrapper wrappers)
		(when (eq wrapper cwrapper)
		  (dotimes-fixnum (i pv-size)
		    (when (consp (let ((map (svref pv-map i)))
				   (if (and map (= (car map) param))
				       (setf (pvref new-pv i) (cdr map))
				       (pvref new-pv i))))
		      (setq new-intern-p nil))))
		(incf param))))
	(when new-intern-p
	  (setq new-pv (let ((list-pv (coerce pv 'list)))
			 (or (gethash (cdr list-pv) *pvs*)
			     (setf (gethash (cdr list-pv) *pvs*)
				   (if old-intern-p
				       new-pv
				       (make-permutation-vector list-pv)))))))
	new-pv)))

(defun maybe-expand-accessor-form (form required-parameters slots env)
  (let* ((fname (car form))
	 #||(len (length form))||#
	 (gf (if (symbolp fname)
		 (unencapsulated-fdefinition fname)
		 (gdefinition fname))))
    (macrolet ((maybe-optimize-reader ()
		 `(let ((parameter
			 (can-optimize-access1 (cadr form)
					       required-parameters env)))
		   (when parameter
		     (optimize-reader slots parameter gf-name form))))
	       (maybe-optimize-writer ()
		 `(let ((parameter
			 (can-optimize-access1 (caddr form)
					       required-parameters env)))
		   (when parameter
		     (optimize-writer slots parameter gf-name form)))))
      (unless (and (consp (cadr form))
		   (eq 'instance-accessor-parameter (caadr form)))
	(when (and (eq *boot-state* 'complete)
		   (generic-function-p gf))
	  (let ((methods (generic-function-methods gf)))
	    (when methods
	      (let* ((gf-name (generic-function-name gf))
		     (arg-info (gf-arg-info gf))
		     (metatypes (arg-info-metatypes arg-info))
		     (nreq (length metatypes))
		     (applyp (arg-info-applyp arg-info)))
		(when (null applyp)
		  (cond ((= nreq 1)
			 (when (some #'standard-reader-method-p methods)
			   (maybe-optimize-reader)))
			((and (= nreq 2)
			      (consp gf-name)
			      (eq (car gf-name) 'setf))
			 (when (some #'standard-writer-method-p methods)
			   (maybe-optimize-writer)))))))))))))

(defun optimize-generic-function-call (form
				       required-parameters
				       env
				       slots
				       calls)
  (declare (ignore required-parameters env slots calls))
  (or ; (optimize-reader ...)?
      form))

(defun can-optimize-access (form required-parameters env)
  (let ((type (ecase (car form)
		(slot-value 'reader)
		(set-slot-value 'writer)
		(slot-boundp 'boundp)))
	(var (cadr form))
	(slot-name (eval (caddr form)))) ; known to be constant
    (can-optimize-access1 var required-parameters env type slot-name)))

;;; FIXME: This looks like an internal helper function for
;;; CAN-OPTIMIZE-ACCESS, and it is used that way, but it's also called
;;; bare from several places in the code. Perhaps the two functions
;;; should be renamed CAN-OPTIMIZE-ACCESS-FOR-FORM and
;;; CAN-OPTIMIZE-ACCESS-FOR-VAR. If so, I'd just as soon use keyword
;;; args instead of optional ones, too.
(defun can-optimize-access1 (var required-parameters env
			     &optional type slot-name)
  (when (and (consp var) (eq 'the (car var)))
    ;; FIXME: We should assert list of length 3 here. Or maybe we
    ;; should just define EXTRACT-THE, replace the whole
    ;;   (WHEN ..)
    ;; form with
    ;;   (AWHEN (EXTRACT-THE VAR)
    ;;     (SETF VAR IT))
    ;; and then use EXTRACT-THE similarly to clean up the other tests
    ;; against 'THE scattered through the PCL code.
    (setq var (caddr var)))
  (when (symbolp var)
    (let* ((rebound? (caddr (var-declaration '%variable-rebinding var env)))
	   (parameter-or-nil (car (memq (or rebound? var)
					required-parameters))))
      (when parameter-or-nil
	(let* ((class-name (caddr (var-declaration '%class
						   parameter-or-nil
						   env)))
	       (class (find-class class-name nil)))
	  (when (or (not (eq *boot-state* 'complete))
		    (and class (not (class-finalized-p class))))
	    (setq class nil))
	  (when (and class-name (not (eq class-name t)))
	    (when (or (null type)
		      (not (and class
				(memq *the-class-structure-object*
				      (class-precedence-list class))))
		      (optimize-slot-value-by-class-p class slot-name type))
	      (cons parameter-or-nil (or class class-name)))))))))

(defun optimize-slot-value (slots sparameter form)
  (if sparameter
      (destructuring-bind (ignore1 ignore2 slot-name-form) form
	(declare (ignore ignore1 ignore2))
	(let ((slot-name (eval slot-name-form)))
	  (optimize-instance-access slots :read sparameter slot-name nil)))
      `(accessor-slot-value ,@(cdr form))))

(defun optimize-set-slot-value (slots sparameter form)
  (if sparameter
      (destructuring-bind (ignore1 ignore2 slot-name-form new-value) form
	(declare (ignore ignore1 ignore2))
	(let ((slot-name (eval slot-name-form)))
	  (optimize-instance-access slots
				    :write
				    sparameter
				    slot-name
				    new-value)))
      `(accessor-set-slot-value ,@(cdr form))))

(defun optimize-slot-boundp (slots sparameter form)
  (if sparameter
      (destructuring-bind
	  ;; FIXME: In CMU CL ca. 19991205, this binding list had a
	  ;; fourth element in it, NEW-VALUE. It's hard to see how
	  ;; that could possibly be right, since SLOT-BOUNDP has no
	  ;; NEW-VALUE. Since it was causing a failure in building PCL
	  ;; for SBCL, so I changed it to match the definition of
	  ;; SLOT-BOUNDP (and also to match the list used in the
	  ;; similar OPTIMIZE-SLOT-VALUE, above). However, I'm weirded
	  ;; out by this, since this is old code which has worked for
	  ;; ages to build PCL for CMU CL, so it's hard to see why it
	  ;; should need a patch like this in order to build PCL for
	  ;; SBCL. I'd like to return to this and find a test case
	  ;; which exercises this function both in CMU CL, to see
	  ;; whether it's really a previously-unexercised bug or
	  ;; whether I've misunderstood something (and, presumably,
	  ;; patched it wrong).
	  (slot-boundp-symbol instance slot-name-form)
	  form
	(declare (ignore slot-boundp-symbol instance))
	(let ((slot-name (eval slot-name-form)))
	  (optimize-instance-access slots
				    :boundp
				    sparameter
				    slot-name
				    nil)))
      `(accessor-slot-boundp ,@(cdr form))))

(defun optimize-reader (slots sparameter gf-name form)
  (if sparameter
      (optimize-accessor-call slots :read sparameter gf-name nil)
      form))

(defun optimize-writer (slots sparameter gf-name form)
  (if sparameter
      (destructuring-bind (ignore1 ignore2 new-value) form
	(declare (ignore ignore1 ignore2))
	(optimize-accessor-call slots :write sparameter gf-name new-value))
      form))

;;; The SLOTS argument is an alist, the CAR of each entry is the name
;;; of a required parameter to the function. The alist is in order, so
;;; the position of an entry in the alist corresponds to the
;;; argument's position in the lambda list.
(defun optimize-instance-access (slots
				 read/write
				 sparameter
				 slot-name
				 new-value)
  (let ((class (if (consp sparameter) (cdr sparameter) *the-class-t*))
	(parameter (if (consp sparameter) (car sparameter) sparameter)))
    (if (and (eq *boot-state* 'complete)
	     (classp class)
	     (memq *the-class-structure-object* (class-precedence-list class)))
	(let ((slotd (find-slot-definition class slot-name)))
	  (ecase read/write
	    (:read
	     `(,(slot-definition-defstruct-accessor-symbol slotd) ,parameter))
	    (:write
	     `(setf (,(slot-definition-defstruct-accessor-symbol slotd)
		     ,parameter)
		    ,new-value))
	    (:boundp
	     t)))
	(let* ((parameter-entry (assq parameter slots))
	       (slot-entry      (assq slot-name (cdr parameter-entry)))
	       (position (posq parameter-entry slots))
	       (pv-offset-form (list 'pv-offset ''.PV-OFFSET.)))
	  (unless parameter-entry
	    (bug "slot optimization bewilderment: O-I-A"))
	  (unless slot-entry
	    (setq slot-entry (list slot-name))
	    (push slot-entry (cdr parameter-entry)))
	  (push pv-offset-form (cdr slot-entry))
	  (ecase read/write
	    (:read
	     `(instance-read ,pv-offset-form ,parameter ,position
			     ',slot-name ',class))
	    (:write
	     `(let ((.new-value. ,new-value))
		(instance-write ,pv-offset-form ,parameter ,position
				',slot-name ',class .new-value.)))
	    (:boundp
	     `(instance-boundp ,pv-offset-form ,parameter ,position
			       ',slot-name ',class)))))))

(defun optimize-accessor-call (slots read/write sparameter gf-name new-value)
  (let* ((class (if (consp sparameter) (cdr sparameter) *the-class-t*))
	 (parameter (if (consp sparameter) (car sparameter) sparameter))
	 (parameter-entry (assq parameter slots))
	 (name (case read/write
		 (:read `(reader ,gf-name))
		 (:write `(writer ,gf-name))))
	 (slot-entry      (assoc name (cdr parameter-entry) :test #'equal))
	 (position (posq parameter-entry slots))
	 (pv-offset-form (list 'pv-offset ''.PV-OFFSET.)))
    (unless parameter-entry
      (error "slot optimization bewilderment: O-A-C"))
    (unless slot-entry
      (setq slot-entry (list name))
      (push slot-entry (cdr parameter-entry)))
    (push pv-offset-form (cdr slot-entry))
    (ecase read/write
      (:read
       `(instance-reader ,pv-offset-form ,parameter ,position ,gf-name ',class))
      (:write
       `(let ((.new-value. ,new-value))
	  (instance-writer ,pv-offset-form ,parameter ,position ,gf-name ',class
			   .new-value.))))))

(defvar *unspecific-arg* '..unspecific-arg..)

(defun optimize-gf-call-internal (form slots env)
  (when (and (consp form)
	     (eq (car form) 'the))
    (setq form (caddr form)))
  (or (and (symbolp form)
	   (let* ((rebound? (caddr (var-declaration '%variable-rebinding
						    form
						    env)))
		  (parameter-or-nil (car (assq (or rebound? form) slots))))
	     (when parameter-or-nil
	       (let* ((class-name (caddr (var-declaration 'class
							  parameter-or-nil
							  env))))
		 (when (and class-name (not (eq class-name t)))
		   (position parameter-or-nil slots :key #'car))))))
      (if (constantp form)
	  (let ((form (eval form)))
	    (if (symbolp form)
		form
		*unspecific-arg*))
	  *unspecific-arg*)))

(defun optimize-gf-call (slots calls gf-call-form nreq restp env)
  (unless (eq (car gf-call-form) 'make-instance) ; XXX needs more work
    (let* ((args (cdr gf-call-form))
	   (all-args-p (eq (car gf-call-form) 'make-instance))
	   (non-required-args (nthcdr nreq args))
	   (required-args (ldiff args non-required-args))
	   (call-spec (list (car gf-call-form) nreq restp
			    (mapcar (lambda (form)
				      (optimize-gf-call-internal form slots env))
				    (if all-args-p
					args
					required-args))))
	   (call-entry (assoc call-spec calls :test #'equal))
	   (pv-offset-form (list 'pv-offset ''.PV-OFFSET.)))
      (unless (some #'integerp
		    (let ((spec-args (cdr call-spec)))
		      (if all-args-p
			  (ldiff spec-args (nthcdr nreq spec-args))
			  spec-args)))
	(return-from optimize-gf-call nil))
      (unless call-entry
	(setq call-entry (list call-spec))
	(push call-entry (cdr calls)))
      (push pv-offset-form (cdr call-entry))
      (if (eq (car call-spec) 'make-instance)
	  `(funcall (pv-ref .pv. ,pv-offset-form) ,@(cdr gf-call-form))
	  `(let ((.emf. (pv-ref .pv. ,pv-offset-form)))
	    (invoke-effective-method-function .emf. ,restp
	     ,@required-args ,@(when restp `((list ,@non-required-args)))))))))

(define-walker-template pv-offset) ; These forms get munged by mutate slots.
(defmacro pv-offset (arg) arg)
(define-walker-template instance-accessor-parameter)
(defmacro instance-accessor-parameter (x) x)

;;; It is safe for these two functions to be wrong. They just try to
;;; guess what the most likely case will be.
(defun generate-fast-class-slot-access-p (class-form slot-name-form)
  (let ((class (and (constantp class-form) (eval class-form)))
	(slot-name (and (constantp slot-name-form) (eval slot-name-form))))
    (and (eq *boot-state* 'complete)
	 (standard-class-p class)
	 (not (eq class *the-class-t*)) ; shouldn't happen, though.
	 (let ((slotd (find-slot-definition class slot-name)))
	   (and slotd (eq :class (slot-definition-allocation slotd)))))))

(defun skip-fast-slot-access-p (class-form slot-name-form type)
  (let ((class (and (constantp class-form) (eval class-form)))
	(slot-name (and (constantp slot-name-form) (eval slot-name-form))))
    (and (eq *boot-state* 'complete)
	 (standard-class-p class)
	 (not (eq class *the-class-t*)) ; shouldn't happen, though.
	 (let ((slotd (find-slot-definition class slot-name)))
	   (and slotd (skip-optimize-slot-value-by-class-p class
							   slot-name
							   type))))))

(defun skip-optimize-slot-value-by-class-p (class slot-name type)
  (let ((slotd (find-slot-definition class slot-name)))
    (and slotd
	 (eq *boot-state* 'complete)
	 (not (slot-accessor-std-p slotd type)))))

(defmacro instance-read-internal (pv slots pv-offset default &optional type)
  (unless (member type '(nil :instance :class :default))
    (error "illegal type argument to ~S: ~S" 'instance-read-internal type))
  (if (eq type :default)
      default
      (let* ((index (gensym))
	     (value index))
	`(locally (declare #.*optimize-speed*)
	  (let ((,index (pvref ,pv ,pv-offset)))
	    (setq ,value (typecase ,index
			   ,@(when (or (null type) (eq type :instance))
			       `((fixnum (clos-slots-ref ,slots ,index))))
			   ,@(when (or (null type) (eq type :class))
			       `((cons (cdr ,index))))
			   (t +slot-unbound+)))
	    (if (eq ,value +slot-unbound+)
		,default
		,value))))))

(defmacro instance-read (pv-offset parameter position slot-name class)
  (if (skip-fast-slot-access-p class slot-name 'reader)
      `(accessor-slot-value ,parameter ,slot-name)
      `(instance-read-internal .pv. ,(slot-vector-symbol position)
	,pv-offset (accessor-slot-value ,parameter ,slot-name)
	,(if (generate-fast-class-slot-access-p class slot-name)
	     :class :instance))))

(defmacro instance-reader (pv-offset parameter position gf-name class)
  (declare (ignore class))
  `(instance-read-internal .pv. ,(slot-vector-symbol position)
    ,pv-offset
    (,gf-name (instance-accessor-parameter ,parameter))
    :instance))

(defmacro instance-write-internal (pv slots pv-offset new-value default
				      &optional type)
  (unless (member type '(nil :instance :class :default))
    (error "illegal type argument to ~S: ~S" 'instance-write-internal type))
  (if (eq type :default)
      default
      (let* ((index (gensym)))
	`(locally (declare #.*optimize-speed*)
	  (let ((,index (pvref ,pv ,pv-offset)))
	    (typecase ,index
	      ,@(when (or (null type) (eq type :instance))
                      `((fixnum (setf (clos-slots-ref ,slots ,index)
				      ,new-value))))
	      ,@(when (or (null type) (eq type :class))
		  `((cons (setf (cdr ,index) ,new-value))))
	      (t ,default)))))))

(defmacro instance-write (pv-offset
			  parameter
			  position
			  slot-name
			  class
			  new-value)
  (if (skip-fast-slot-access-p class slot-name 'writer)
      `(accessor-set-slot-value ,parameter ,slot-name ,new-value)
      `(instance-write-internal .pv. ,(slot-vector-symbol position)
	,pv-offset ,new-value
	(accessor-set-slot-value ,parameter ,slot-name ,new-value)
	,(if (generate-fast-class-slot-access-p class slot-name)
	     :class :instance))))

(defmacro instance-writer (pv-offset
			   parameter
			   position
			   gf-name
			   class
			   new-value)
  (declare (ignore class))
  `(instance-write-internal .pv. ,(slot-vector-symbol position)
    ,pv-offset ,new-value
    (,(if (consp gf-name)
	  (get-setf-fun-name gf-name)
	  gf-name)
     (instance-accessor-parameter ,parameter)
     ,new-value)
    :instance))

(defmacro instance-boundp-internal (pv slots pv-offset default
				       &optional type)
  (unless (member type '(nil :instance :class :default))
    (error "illegal type argument to ~S: ~S" 'instance-boundp-internal type))
  (if (eq type :default)
      default
      (let* ((index (gensym)))
	`(locally (declare #.*optimize-speed*)
	  (let ((,index (pvref ,pv ,pv-offset)))
	    (typecase ,index
	      ,@(when (or (null type) (eq type :instance))
		  `((fixnum (not (and ,slots
                                      (eq (clos-slots-ref ,slots ,index)
                                          +slot-unbound+))))))
	      ,@(when (or (null type) (eq type :class))
		  `((cons (not (eq (cdr ,index) +slot-unbound+)))))
	      (t ,default)))))))

(defmacro instance-boundp (pv-offset parameter position slot-name class)
  (if (skip-fast-slot-access-p class slot-name 'boundp)
      `(accessor-slot-boundp ,parameter ,slot-name)
      `(instance-boundp-internal .pv. ,(slot-vector-symbol position)
	,pv-offset (accessor-slot-boundp ,parameter ,slot-name)
	,(if (generate-fast-class-slot-access-p class slot-name)
	     :class :instance))))

;;; This magic function has quite a job to do indeed.
;;;
;;; The careful reader will recall that <slots> contains all of the
;;; optimized slot access forms produced by OPTIMIZE-INSTANCE-ACCESS.
;;; Each of these is a call to either INSTANCE-READ or INSTANCE-WRITE.
;;;
;;; At the time these calls were produced, the first argument was
;;; specified as the symbol .PV-OFFSET.; what we have to do now is
;;; convert those pv-offset arguments into the actual number that is
;;; the correct offset into the pv.
;;;
;;; But first, oh but first, we sort <slots> a bit so that for each
;;; argument we have the slots in alphabetical order. This
;;; canonicalizes the PV-TABLE's a bit and will hopefully lead to
;;; having fewer PV's floating around. Even if the gain is only
;;; modest, it costs nothing.
(defun slot-name-lists-from-slots (slots calls)
  (multiple-value-bind (slots calls) (mutate-slots-and-calls slots calls)
    (let* ((slot-name-lists
	    (mapcar (lambda (parameter-entry)
		      (cons nil (mapcar #'car (cdr parameter-entry))))
		    slots))
	   (call-list
	    (mapcar #'car calls)))
      (dolist (call call-list)
	(dolist (arg (cdr call))
	  (when (integerp arg)
	    (setf (car (nth arg slot-name-lists)) t))))
      (setq slot-name-lists (mapcar (lambda (r+snl)
				      (when (or (car r+snl) (cdr r+snl))
					r+snl))
				    slot-name-lists))
      (let ((cvt (apply #'vector
			(let ((i -1))
			  (mapcar (lambda (r+snl)
				    (when r+snl (incf i)))
				  slot-name-lists)))))
	(setq call-list (mapcar (lambda (call)
				  (cons (car call)
					(mapcar (lambda (arg)
						  (if (integerp arg)
						      (svref cvt arg)
						      arg))
						(cdr call))))
				call-list)))
      (values slot-name-lists call-list))))

(defun mutate-slots-and-calls (slots calls)
  (let ((sorted-slots (sort-slots slots))
	(sorted-calls (sort-calls (cdr calls)))
	(pv-offset 0))  ; index 0 is for info
    (dolist (parameter-entry sorted-slots)
      (dolist (slot-entry (cdr parameter-entry))
	(incf pv-offset)	
	(dolist (form (cdr slot-entry))
	  (setf (cadr form) pv-offset))))
    (dolist (call-entry sorted-calls)
      (incf pv-offset)
      (dolist (form (cdr call-entry))
	(setf (cadr form) pv-offset)))
    (values sorted-slots sorted-calls)))

(defun symbol-pkg-name (sym)
  (let ((pkg (symbol-package sym)))
    (if pkg (package-name pkg) "")))

;;; FIXME: Because of the existence of UNINTERN and RENAME-PACKAGE,
;;; the part of this ordering which is based on SYMBOL-PKG-NAME is not
;;; stable. This ordering is only used in to
;;; SLOT-NAME-LISTS-FROM-SLOTS, where it serves to "canonicalize the
;;; PV-TABLE's a bit and will hopefully lead to having fewer PV's
;;; floating around", so it sounds as though the instability won't
;;; actually lead to bugs, just small inefficiency. But still, it
;;; would be better to reimplement this function as a comparison based
;;; on SYMBOL-HASH:
;;;   * stable comparison
;;;   * smaller code (here, and in being able to discard SYMBOL-PKG-NAME)
;;;   * faster code.
(defun symbol-lessp (a b)
  (if (eq (symbol-package a)
	  (symbol-package b))
      (string-lessp (symbol-name a)
		    (symbol-name b))
      (string-lessp (symbol-pkg-name a)
		    (symbol-pkg-name b))))

(defun symbol-or-cons-lessp (a b)
  (etypecase a
    (symbol (etypecase b
	      (symbol (symbol-lessp a b))
	      (cons t)))
    (cons   (etypecase b
	      (symbol nil)
	      (cons (if (eq (car a) (car b))
			(symbol-or-cons-lessp (cdr a) (cdr b))
			(symbol-or-cons-lessp (car a) (car b))))))))

(defun sort-slots (slots)
  (mapcar (lambda (parameter-entry)
	    (cons (car parameter-entry)
		  (sort (cdr parameter-entry)	;slot entries
			#'symbol-or-cons-lessp
			:key #'car)))
	  slots))

(defun sort-calls (calls)
  (sort calls #'symbol-or-cons-lessp :key #'car))

;;;; This needs to work in terms of metatypes and also needs to work
;;;; for automatically generated reader and writer functions.
;;;; Automatically generated reader and writer functions use this
;;;; stuff too.

(defmacro pv-binding ((required-parameters slot-name-lists pv-table-symbol)
		      &body body)
  (let (slot-vars pv-parameters)
    (loop for slots in slot-name-lists
          for required-parameter in required-parameters
          for i from 0
          do (when slots
               (push required-parameter pv-parameters)
               (push (slot-vector-symbol i) slot-vars)))
    `(pv-binding1 (.pv. .calls. ,pv-table-symbol
                   ,(nreverse pv-parameters) ,(nreverse slot-vars))
       ,@body)))

(defmacro pv-binding1 ((pv calls pv-table-symbol pv-parameters slot-vars)
		       &body body)
  `(pv-env (,pv ,calls ,pv-table-symbol ,pv-parameters)
     (let (,@(mapcar (lambda (slot-var p) `(,slot-var (get-slots-or-nil ,p)))
		     slot-vars pv-parameters))
	,@body)))

;;; This gets used only when the default MAKE-METHOD-LAMBDA is
;;; overridden.
(defmacro pv-env ((pv calls pv-table-symbol pv-parameters)
		  &rest forms)
  `(let* ((.pv-table. ,pv-table-symbol)
	  (.pv-cell. (pv-table-lookup-pv-args .pv-table. ,@pv-parameters))
	  (,pv (car .pv-cell.))
	  (,calls (cdr .pv-cell.)))
     (declare ,(make-pv-type-declaration pv))
     (declare ,(make-calls-type-declaration calls))
     ,@(when (symbolp pv-table-symbol)
	 `((declare (special ,pv-table-symbol))))
     ,pv ,calls
     ,@forms))

(defvar *non-var-declarations*
  ;; FIXME: VALUES was in this list, conditionalized with #+CMU, but I
  ;; don't *think* CMU CL had, or SBCL has, VALUES declarations. If
  ;; SBCL doesn't have 'em, VALUES should probably be removed from
  ;; this list.
  '(values
    %method-name
    %method-lambda-list
    optimize
    ftype
    inline
    notinline))

(defvar *var-declarations-with-arg*
  '(%class
    type))

(defvar *var-declarations-without-arg*
  '(ignore
    ignorable special dynamic-extent
    ;; FIXME: Possibly this entire list and variable could go away.
    ;; If not, certainly we should remove all these built-in typenames
    ;; from the list, and replace them with a test for "is it a type
    ;; name?" (CLTL1 allowed only built-in type names as declarations,
    ;; but ANSI CL allows any type name as a declaration.)
    array atom base-char bignum bit bit-vector character compiled-function
    complex cons double-float extended-char
    fixnum float function hash-table integer
    keyword list long-float nil null number package pathname random-state ratio
    rational readtable sequence short-float signed-byte simple-array
    simple-bit-vector simple-string simple-vector single-float standard-char
    stream string symbol t unsigned-byte vector))

(defun split-declarations (body args calls-next-method-p)
  (let ((inner-decls nil)
	(outer-decls nil)
	decl)
    (loop (when (null body) (return nil))
	  (setq decl (car body))
	  (unless (and (consp decl)
		       (eq (car decl) 'declare))
	    (return nil))
	  (dolist (form (cdr decl))
	    (when (consp form)
	      (let ((declaration-name (car form)))
		(if (member declaration-name *non-var-declarations*)
		    (push `(declare ,form) outer-decls)
		    (let ((arg-p
			   (member declaration-name
				   *var-declarations-with-arg*))
			  (non-arg-p
			   (member declaration-name
				   *var-declarations-without-arg*))
			  (dname (list (pop form)))
			  (inners nil) (outers nil))
		      (unless (or arg-p non-arg-p)
			;; FIXME: This warning, and perhaps the
			;; various *VAR-DECLARATIONS-FOO* and/or
			;; *NON-VAR-DECLARATIONS* variables,
			;; could probably go away now that we're not
			;; trying to be portable between different
			;; CLTL1 hosts the way PCL was. (Note that to
			;; do this right, we need to be able to handle
			;; user-defined (DECLAIM (DECLARATION FOO))
			;; stuff.)
			(warn "The declaration ~S is not understood by ~S.~@
			       Please put ~S on one of the lists ~S,~%~S, or~%~S.~@
			(Assuming it is a variable declaration without argument)."
			      declaration-name 'split-declarations
			      declaration-name
			      '*non-var-declarations*
			      '*var-declarations-with-arg*
			      '*var-declarations-without-arg*)
			(push declaration-name *var-declarations-without-arg*))
		      (when arg-p
			(setq dname (append dname (list (pop form)))))
		      (case (car dname)
			(%class (push `(declare (,@dname ,@form)) inner-decls))
			(t
			 (dolist (var form)
			   (if (member var args)
			       ;; Quietly remove IGNORE declarations
			       ;; on args when a next-method is
			       ;; involved, to prevent compiler
			       ;; warnings about ignored args being
			       ;; read.
			       (unless (and calls-next-method-p
					    (eq (car dname) 'ignore))
				 (push var outers))
			       (push var inners)))
			 (when outers
			   (push `(declare (,@dname ,@outers)) outer-decls))
			 (when inners
			   (push
			    `(declare (,@dname ,@inners))
			    inner-decls)))))))))
	  (setq body (cdr body)))
    (values outer-decls inner-decls body)))

;;; Pull a name out of the %METHOD-NAME declaration in the function
;;; body given, or return NIL if no %METHOD-NAME declaration is found.
(defun body-method-name (body)
  (multiple-value-bind (real-body declarations documentation)
      (parse-body body nil)
    (declare (ignore documentation real-body))
    (let ((name-decl (get-declaration '%method-name declarations)))
      (and name-decl
	   (destructuring-bind (name) name-decl
	     name)))))

;;; Convert a lambda expression containing a SB-PCL::%METHOD-NAME
;;; declaration (which is a naming style internal to PCL) into an
;;; SB-INT:NAMED-LAMBDA expression (which is a naming style used
;;; throughout SBCL, understood by the main compiler); or if there's
;;; no SB-PCL::%METHOD-NAME declaration, then just return the original
;;; lambda expression.
(defun name-method-lambda (method-lambda)
  (let ((method-name (body-method-name (cddr method-lambda))))
    (if method-name
	`(named-lambda ,method-name ,(rest method-lambda))
	method-lambda)))

(defun make-method-initargs-form-internal (method-lambda initargs env)
  (declare (ignore env))
  (let (method-lambda-args
	lmf ; becomes body of function
	lmf-params)
    (if (not (and (= 3 (length method-lambda))
		  (= 2 (length (setq method-lambda-args (cadr method-lambda))))
		  (consp (setq lmf (third method-lambda)))
		  (eq 'simple-lexical-method-functions (car lmf))
		  (eq (car method-lambda-args)
		      (cadr (setq lmf-params (cadr lmf))))
		  (eq (cadr method-lambda-args)
		      (caddr lmf-params))))
	`(list* :function ,(name-method-lambda method-lambda)
		',initargs)
	(let* ((lambda-list (car lmf-params))
	       (nreq 0)
	       (restp nil)
	       (args nil))
	  (dolist (arg lambda-list)
	    (when (member arg '(&optional &rest &key))
	      (setq restp t)
	      (return nil))
	    (when (eq arg '&aux)
	      (return nil))
	    (incf nreq)
	    (push arg args))
	  (setq args (nreverse args))
	  (setf (getf (getf initargs :plist) :arg-info) (cons nreq restp))
	  (make-method-initargs-form-internal1
	   initargs (cddr lmf) args lmf-params restp)))))

(defun make-method-initargs-form-internal1
    (initargs body req-args lmf-params restp)
  (multiple-value-bind (outer-decls inner-decls body-sans-decls)
      (split-declarations
       body req-args (getf (cdr lmf-params) :call-next-method-p))
    (let* ((rest-arg (when restp '.rest-arg.))
	   (args+rest-arg (if restp
			      (append req-args (list rest-arg))
			      req-args)))
      `(list*
	:fast-function
	(,(if (body-method-name body) 'named-lambda 'lambda)
	 ,@(when (body-method-name body)
	     (list (body-method-name body))) ; function name
	 (.pv-cell. .next-method-call. ,@args+rest-arg) ; function args
	 ;; body of the function
	 (declare (ignorable .pv-cell. .next-method-call.))
	 ,@outer-decls
	 (macrolet ((pv-env ((pv calls pv-table-symbol pv-parameters)
			     &rest forms)
			    (declare (ignore pv-table-symbol
					     pv-parameters))
			    `(let ((,pv (car .pv-cell.))
				   (,calls (cdr .pv-cell.)))
			       (declare ,(make-pv-type-declaration pv)
					,(make-calls-type-declaration calls))
			       ,pv ,calls
			       ,@forms)))
	   (fast-lexical-method-functions
	    (,(car lmf-params) .next-method-call. ,req-args ,rest-arg
	     ,@(cdddr lmf-params))
	    ,@inner-decls
	    ,@body-sans-decls)))
	',initargs))))

;;; Use arrays and hash tables and the fngen stuff to make this much
;;; better. It doesn't really matter, though, because a function
;;; returned by this will get called only when the user explicitly
;;; funcalls a result of method-function. BUT, this is needed to make
;;; early methods work.
(defun method-function-from-fast-function (fmf)
  (declare (type function fmf))
  (let* ((method-function nil) (pv-table nil)
	 (arg-info (method-function-get fmf :arg-info))
	 (nreq (car arg-info))
	 (restp (cdr arg-info)))
    (setq method-function
	  (lambda (method-args next-methods)
	    (unless pv-table
	      (setq pv-table (method-function-pv-table fmf)))
	    (let* ((pv-cell (when pv-table
			      (get-method-function-pv-cell
			       method-function method-args pv-table)))
		   (nm (car next-methods))
		   (nms (cdr next-methods))
		   (nmc (when nm
			  (make-method-call
			   :function (if (std-instance-p nm)
					 (method-function nm)
					 nm)
			   :call-method-args (list nms)))))
	      (if restp
		  (let* ((rest (nthcdr nreq method-args))
			 (args (ldiff method-args rest)))
		    (apply fmf pv-cell nmc (nconc args (list rest))))
		  (apply fmf pv-cell nmc method-args)))))
    (let* ((fname (method-function-get fmf :name))
	   (name `(,(or (get (car fname) 'method-sym)
			(setf (get (car fname) 'method-sym)
			      (let ((str (symbol-name (car fname))))
				(if (string= "FAST-" str :end2 5)
				    (intern (subseq str 5) *pcl-package*)
				    (car fname)))))
		    ,@(cdr fname))))
      (set-fun-name method-function name))
    (setf (method-function-get method-function :fast-function) fmf)
    method-function))

(defun get-method-function-pv-cell (method-function
				    method-args
				    &optional pv-table)
  (let ((pv-table (or pv-table (method-function-pv-table method-function))))
    (when pv-table
      (let ((pv-wrappers (pv-wrappers-from-all-args pv-table method-args)))
	(when pv-wrappers
	  (pv-table-lookup pv-table pv-wrappers))))))

(defun pv-table-lookup-pv-args (pv-table &rest pv-parameters)
  (pv-table-lookup pv-table (pv-wrappers-from-pv-args pv-parameters)))

(defun pv-wrappers-from-pv-args (&rest args)
  (let (wrappers)
    (dolist (arg args (if (cdr wrappers) (nreverse wrappers) (car wrappers)))
      (let ((wrapper (wrapper-of arg)))
	(push (if (invalid-wrapper-p wrapper)
		  (check-wrapper-validity wrapper)
		  wrapper)
	      wrappers)))))

(defun pv-wrappers-from-all-args (pv-table args)
  (loop for snl in (pv-table-slot-name-lists pv-table) and arg in args
        when snl
          collect (wrapper-of arg) into wrappers
        finally (return (if (cdr wrappers) wrappers (car wrappers)))))

;;; Return the subset of WRAPPERS which is used in the cache
;;; of PV-TABLE.
(defun pv-wrappers-from-all-wrappers (pv-table wrappers)
  (loop for snl in (pv-table-slot-name-lists pv-table) and w in wrappers
        when snl
          collect w into result
        finally (return (if (cdr result) result (car result)))))
