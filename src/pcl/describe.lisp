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

  (fresh-line stream)
  
  (let* ((class (class-of object))
	 (slotds (slots-to-inspect class object))
	 (max-slot-name-length 0)
	 (instance-slotds ())
	 (class-slotds ())
	 (other-slotds ()))

    (format stream "~&~@<~S ~_is an instance of class ~S.~:>" object class)

    ;; Figure out a good width for the slot-name column.
    (flet ((adjust-slot-name-length (name)
	     (setq max-slot-name-length
		   (max max-slot-name-length
			(length (the string (symbol-name name)))))))
      (dolist (slotd slotds)
	(adjust-slot-name-length (slot-definition-name slotd))
	(case (slot-definition-allocation slotd)
	  (:instance (push slotd instance-slotds))
	  (:class  (push slotd class-slotds))
	  (otherwise (push slotd other-slotds))))
      (setq max-slot-name-length  (min (+ max-slot-name-length 3) 30)))

    ;; Now that we know the width, we can print.
    (flet ((describe-slot (name value &optional (allocation () alloc-p))
	     (if alloc-p
		 (format stream
			 "~& ~A ~S ~VT  ~S"
			 name allocation (+ max-slot-name-length 7) value)
		 (format stream
			 "~& ~A~VT  ~S"
			 name max-slot-name-length value))))
      (when instance-slotds
	(format stream "~&The following slots have :INSTANCE allocation:")
	(dolist (slotd (nreverse instance-slotds))
	  (describe-slot
	   (slot-definition-name slotd)
	   (slot-value-or-default object
				  (slot-definition-name slotd)))))
      (when class-slotds
	(format stream "~&The following slots have :CLASS allocation:")
	(dolist (slotd (nreverse class-slotds))
	  (describe-slot
	   (slot-definition-name slotd)
	   (slot-value-or-default object
				  (slot-definition-name slotd)))))
      (when other-slotds
	(format stream "~&The following slots have allocation as shown:")
	(dolist (slotd (nreverse other-slotds))
	  (describe-slot
	   (slot-definition-name slotd)
	   (slot-value-or-default object
				  (slot-definition-name slotd))
	   (slot-definition-allocation slotd))))))

  (terpri stream))

(defmethod describe-object ((fun standard-generic-function) stream)
  (format stream "~&~A is a generic function." fun)
  (when (documentation fun t)
    (format stream "~&  Function documentation: ~A" (documentation fun t)))
  (format stream "~&Its lambda-list is:~&  ~S"
	  (generic-function-pretty-arglist fun))
  (let ((methods (generic-function-methods fun)))
    (if (null methods)
	(format stream "~&It has no methods.~%")
	(let ((gf-name (generic-function-name fun)))
	  (format stream "~&Its methods are:")
	  (dolist (method methods)
	    (format stream "~&  (~A ~{~S ~}~:S)~%"
		    gf-name
		    (method-qualifiers method)
		    (unparse-specializers method))
	    (when (documentation method t)
	      (format stream "~&    Method documentation: ~A"
		      (documentation method t))))))))

(defmethod describe-object ((class class) stream)
  (flet ((pretty-class (c) (or (class-name c) c)))
    (macrolet ((ft (string &rest args) `(format stream ,string ,@args)))
      (ft "~&~@<~S is a class. It is an instance of ~S.~:@>"
	  class (pretty-class (class-of class)))
      (let ((name (class-name class)))
	(if name
	    (if (eq class (find-class name nil))
		(ft "~&~@<Its proper name is ~S.~@:>" name)
		(ft "~&~@<Its name is ~S, but this is not a proper name.~@:>"
		    name))
	    (ft "~&~@<It has no name (the name is NIL).~@:>")))
      (ft "~&~@<The direct superclasses are: ~:S, and the direct ~
	   subclasses are: ~:S.~I~_The class is ~:[not ~;~]finalized~
           ~:[. ~;; its class precedence list is:~2I~_~:*~S.~]~I~_~
	   There ~[are~;is~:;are~] ~:*~S method~:P specialized for ~
           this class.~:@>~%"
	  (mapcar #'pretty-class (class-direct-superclasses class))
	  (mapcar #'pretty-class (class-direct-subclasses class))
	  (class-finalized-p class)
	  (mapcar #'pretty-class (cpl-or-nil class))
	  (length (specializer-direct-methods class))))))

(defmethod describe-object ((package package) stream)
  (format stream "~&~S is a ~S." package (type-of package))
  (format stream
	  "~@[~&~@<It has nicknames ~2I~{~:_~S~^ ~}~:>~]"
	  (package-nicknames package))
  (let* ((internal (package-internal-symbols package))
	 (internal-count (- (package-hashtable-size internal)
			    (package-hashtable-free internal)))
	 (external (package-external-symbols package))
	 (external-count (- (package-hashtable-size external)
			    (package-hashtable-free external))))
    (format stream
	    "~&It has ~S internal and ~S external symbols."
	    internal-count external-count))
  (flet (;; Turn a list of packages into something a human likes
	 ;; to read.
	 (humanize (package-list)
	   (sort (mapcar #'package-name package-list) #'string<)))
    (format stream
	    "~@[~&~@<It uses packages named ~2I~{~:_~S~^ ~}~:>~]"
	    (humanize (package-use-list package)))
    (format stream
	    "~@[~&~@<It is used by packages named ~2I~{~:_~S~^ ~}~:>~]"
	    (humanize (package-used-by-list package))))
  (terpri stream))
