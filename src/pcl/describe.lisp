;;;; that part of the DESCRIBE mechanism which is based on code from
;;;; PCL

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

(defmethod slots-to-inspect ((class slot-class) (object slot-object))
  (class-slots class))

(defmethod describe-object ((object slot-object) stream)

  (let* ((class (class-of object))
	 (slotds (slots-to-inspect class object))
	 (max-slot-name-length 0)
	 (instance-slotds ())
	 (class-slotds ())
	 (other-slotds ()))

    (flet ((adjust-slot-name-length (name)
	     (setq max-slot-name-length
		   (max max-slot-name-length
			(length (the string (symbol-name name))))))
	   (describe-slot (name value &optional (allocation () alloc-p))
	     (if alloc-p
		 (format stream
			 "~% ~A ~S ~VT  ~S"
			 name allocation (+ max-slot-name-length 7) value)
		 (format stream
			 "~% ~A~VT  ~S"
			 name max-slot-name-length value))))

      ;; Figure out a good width for the slot-name column.
      (dolist (slotd slotds)
	(adjust-slot-name-length (slot-definition-name slotd))
	(case (slot-definition-allocation slotd)
	  (:instance (push slotd instance-slotds))
	  (:class  (push slotd class-slotds))
	  (otherwise (push slotd other-slotds))))
      (setq max-slot-name-length  (min (+ max-slot-name-length 3) 30))
      (format stream "~%~@<~S ~_is an instance of class ~S.~:>" object class)

      ;; Now that we know the width, we can print.
      (when instance-slotds
	(format stream "~%The following slots have :INSTANCE allocation:")
	(dolist (slotd (nreverse instance-slotds))
	  (describe-slot
	   (slot-definition-name slotd)
	   (slot-value-or-default object
				  (slot-definition-name slotd)))))
      (when class-slotds
	(format stream "~%The following slots have :CLASS allocation:")
	(dolist (slotd (nreverse class-slotds))
	  (describe-slot
	   (slot-definition-name slotd)
	   (slot-value-or-default object
				  (slot-definition-name slotd)))))
      (when other-slotds
	(format stream "~%The following slots have allocation as shown:")
	(dolist (slotd (nreverse other-slotds))
	  (describe-slot
	   (slot-definition-name slotd)
	   (slot-value-or-default object
				  (slot-definition-name slotd))
	   (slot-definition-allocation slotd)))))))

(defvar *describe-metaobjects-as-objects-p* nil)

(defmethod describe-object ((fun standard-generic-function) stream)
  (format stream "~A is a generic function.~%" fun)
  (format stream "Its arguments are:~%  ~S~%"
	  (generic-function-pretty-arglist fun))
  (format stream "Its methods are:")
  (dolist (method (generic-function-methods fun))
    (format stream "~2%    ~{~S ~}~:S =>~%"
	    (method-qualifiers method)
	    (unparse-specializers method))
    (describe-object (or (method-fast-function method)
			 (method-function method))
		     stream))
  (when *describe-metaobjects-as-objects-p*
    (call-next-method)))

(defmethod describe-object ((class class) stream)
  (flet ((pretty-class (c) (or (class-name c) c)))
    (macrolet ((ft (string &rest args) `(format stream ,string ,@args)))
      (ft "~&~S is a class, it is an instance of ~S.~%"
	  class (pretty-class (class-of class)))
      (let ((name (class-name class)))
	(if name
	    (if (eq class (find-class name nil))
		(ft "Its proper name is ~S.~%" name)
		(ft "Its name is ~S, but this is not a proper name.~%" name))
	    (ft "It has no name (the name is NIL).~%")))
      (ft "The direct superclasses are: ~:S, and the direct~%~
	   subclasses are: ~:S. The class precedence list is:~%~S~%~
	   There are ~D methods specialized for this class."
	  (mapcar #'pretty-class (class-direct-superclasses class))
	  (mapcar #'pretty-class (class-direct-subclasses class))
	  (mapcar #'pretty-class (class-precedence-list class))
	  (length (specializer-direct-methods class)))))
  (when *describe-metaobjects-as-objects-p*
    (call-next-method)))

(defmethod describe-object ((package package) stream)
  (pprint-logical-block (stream nil)
    (format stream "~&~S is a ~S." package (type-of package))
    (format stream
	    "~@[~&It has nicknames ~{~:_~S~^ ~}~]"
	    (package-nicknames package))
    (let* ((internal (sb-impl::package-internal-symbols package))
	   (internal-count (- (sb-impl::package-hashtable-size internal)
			      (sb-impl::package-hashtable-free internal)))
	   (external (sb-impl::package-external-symbols package))
	   (external-count (- (sb-impl::package-hashtable-size external)
			      (sb-impl::package-hashtable-free external))))
      (format stream
	      "~&It has ~S internal and ~S external symbols."
	      internal-count external-count))
    (format stream
	    "~@[~&It uses ~{~:_~S~^ ~}~]"
	    (package-use-list package))
    (format stream
	    "~@[~&It is used by ~{~:_~S~^ ~}~]"
	    (package-used-by-list package))))
