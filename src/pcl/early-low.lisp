;;;; some code pulled out of CMU CL's low.lisp to solve build order problems,
;;;; and some other stuff that just plain needs to be done early

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

(/show "starting early-low.lisp")

;;; FIXME: The PCL package is internal and is used by code in potential
;;; bottlenecks. Access to it might be faster through #.(find-package "SB-PCL")
;;; than through *PCL-PACKAGE*. And since it's internal, no one should be
;;; doing things like deleting and recreating it in a running target Lisp.
;;; So perhaps we should replace it uses of *PCL-PACKAGE* with uses of
;;; (PCL-PACKAGE), and make PCL-PACKAGE a macro which expands into
;;; the SB-PCL package itself. Maybe we should even use this trick for
;;; COMMON-LISP and KEYWORD, too. (And the definition of PCL-PACKAGE etc.
;;; could be made less viciously brittle when SB-FLUID.)
;;; (Or perhaps just define a macro
;;;   (DEFMACRO PKG (NAME)
;;;     #-SB-FLUID (FIND-PACKAGE NAME)
;;;     #+SB-FLUID `(FIND-PACKAGE ,NAME))
;;; and use that to replace all three variables.)
(defvar *pcl-package*		     (find-package "SB-PCL"))
(defvar *slot-accessor-name-package* (find-package "SB-SLOT-ACCESSOR-NAME"))

;;; This excludes structure types created with the :TYPE option to
;;; DEFSTRUCT. It also doesn't try to deal with types created by
;;; hairy DEFTYPEs, e.g.
;;;   (DEFTYPE CACHE-STRUCTURE (SIZE)
;;;     (IF (> SIZE 11) 'BIG-CS 'SMALL-CS)).
;;; KLUDGE: In fact, it doesn't seem to deal with DEFTYPEs at all. Perhaps
;;; it needs a more mnemonic name. -- WHN 19991204
(defun structure-type-p (type)
  (and (symbolp type)
       (let ((class  (cl:find-class type nil)))
	 (and class
	      (typep (sb-kernel:layout-info (sb-kernel:class-layout class))
		     'sb-kernel:defstruct-description)))))

(/show "finished with early-low.lisp")

;;;; instrumented versions of OUTPUT-OBJECT and friends
;;;; 
;;;; REMOVEME once condition system problem is fixed 

(in-package :sb-impl)

(defun check-for-c (object &optional assign) ; nee CHECK-FOR-CIRCULARITY
  (cond ((null *print-circle*)
	 ;; Don't bother, nobody cares.
	 nil)
	((null *circularity-hash-table*)
	 :initiate)
	((null *circularity-counter*)
	 (ecase (gethash object *circularity-hash-table*)
	   ((nil)
	    ;; First encounter.
	    (setf (gethash object *circularity-hash-table*) t)
	    ;; We need to keep looking.
	    nil)
	   ((t)
	    ;; Second encounter.
	    (setf (gethash object *circularity-hash-table*) 0)
	    ;; It's a circular reference.
	    t)
	   (0
	    ;; It's a circular reference.
	    t)))
	(t
	 (let ((value (gethash object *circularity-hash-table*)))
	   (case value
	     ((nil t)
	      ;; If NIL, we found an object that wasn't there the first time
	      ;; around. If T, exactly one occurance of this object appears.
	      ;; Either way, just print the thing without any special
	      ;; processing. Note: you might argue that finding a new object
	      ;; means that something is broken, but this can happen. If
	      ;; someone uses the ~@<...~:> format directive, it conses a
	      ;; new list each time though FORMAT (i.e. the &REST list), so
	      ;; we will have different cdrs.
	      nil)
	     (0
	      (if assign
		  (let ((value (incf *circularity-counter*)))
		    ;; First occurance of this object. Set the counter.
		    (setf (gethash object *circularity-hash-table*) value)
		    value)
		  t))
	     (t
	      ;; Second or later occurance.
	      (- value)))))))

(defun handle-c (marker stream) ; nee HANDLE-CIRCULARITY
  (case marker
    (:initiate
     ;; Someone forgot to initiate circularity detection.
     (let ((*print-circle* nil))
       (error "trying to use CHECK-FOR-C when ~
	       circularity checking isn't initiated")))
    ((t)
     ;; It's a second (or later) reference to the object while we are
     ;; just looking. So don't bother groveling it again.
     nil)
    (t
     (write-char #\# stream)
     (let ((*print-base* 10) (*print-radix* nil))
       (cond ((minusp marker)
	      (output-integer (- marker) stream)
	      (write-char #\# stream)
	      nil)
	     (t
	      (output-integer marker stream)
	      (write-char #\= stream)
	      t))))))

(defun o-o (object stream) ; nee OUTPUT-OBJECT
  (/show "entering O-O")
  (labels ((print-it (stream)
	     (/show "entering PRINT-IT" *print-pretty*)
	     (if *print-pretty*
		 (if *pretty-printer*
		     (funcall *pretty-printer* object stream)
		     (let ((*print-pretty* nil))
		       (output-ugly-object object stream)))
		 (output-ugly-object object stream))
	     (/show "leaving PRINT-IT"))
	   (check-it (stream)
	     (/show "entering CHECK-IT")
	     (let ((marker (check-for-c object t)))
	       (case marker
		 (:initiate
		  (/show "MARKER=:INITIATE case")
		  (let ((*circularity-hash-table*
			 (make-hash-table :test 'eq)))
		    (check-it (make-broadcast-stream))
		    (let ((*circularity-counter* 0))
		      (check-it stream))))
		 ((nil)
		  (/show "MARKER=NIL case")
		  (print-it stream))
		 (t
		  (/show "MARKER fall-through-to-default case")
		  (when (handle-c marker stream)
		    (print-it stream)))))
	     (/show "leaving CHECK-IT")))
    (prog1
	(cond ((or (not *print-circle*)
		   (numberp object)
		   (characterp object)
		   (and (symbolp object)
			(symbol-package object)))
	       (/show "no-circularity-checking case")
	       ;; If it's a number, character, or interned symbol, we
	       ;; don't want to check for circularity/sharing.
	       (print-it stream))
	      ((or *circularity-hash-table*
		   (consp object)
		   (typep object 'instance)
		   (typep object '(array t *)))
	       ;; If we have already started circularity detection, this
	       ;; object might be a shared reference. If we have not, then
	       ;; if it is a cons, an instance, or an array of element
	       ;; type T it might contain a circular reference to itself
	       ;; or multiple shared references.
	       (/show "CHECK-IT case")
	       (check-it stream))
	      (t
	       (/show "don't-CHECK-IT case")
	       (print-it stream)))
      (/show "leaving O-O"))))

(defun o-u-o (object stream) ; nee OUTPUT-UGLY-OBJECT
  (/show "entering O-U-O")
  (/show (typep object 'fixnum))
  (/show (typep object 'list))
  (/show (typep object 'instance))
  (/show (typep object 'function))
  (typecase object
    ;; KLUDGE: The TYPECASE approach here is non-ANSI; the ANSI definition of
    ;; PRINT-OBJECT says it provides printing and we're supposed to provide
    ;; PRINT-OBJECT methods covering all classes. We deviate from this
    ;; by using PRINT-OBJECT only when we print instance values. However,
    ;; ANSI makes it hard to tell that we're deviating from this:
    ;;   (1) ANSI specifies that the user isn't supposed to call PRINT-OBJECT
    ;;       directly.
    ;;   (2) ANSI (section 11.1.2.1.2) says it's undefined to define
    ;;       a method on an external symbol in the CL package which is
    ;;       applicable to arg lists containing only direct instances of
    ;;       standardized classes.
    ;; Thus, in order for the user to detect our sleaziness, he has to do
    ;; something relatively obscure like
    ;;   (1) actually use tools like FIND-METHOD to look for PRINT-OBJECT
    ;;       methods, or
    ;;   (2) define a PRINT-OBJECT method which is specialized on the stream
    ;;       value (e.g. a Gray stream object).
    ;; As long as no one comes up with a non-obscure way of detecting this
    ;; sleaziness, fixing this nonconformity will probably have a low
    ;; priority. -- WHN 20000121
    (fixnum
     (/show "FIXNUM case")
     (output-integer object stream))
    (list
     (/show "LIST case")
     (if (null object)
	 (output-symbol object stream)
	 (output-list object stream)))
    (instance
     (/show "INSTANCE case")
     (print-object object stream))
    (function
     (/show "FUNCTION case")
     (unless (and (funcallable-instance-p object)
		  (printed-as-funcallable-standard-class object stream))
       (output-function object stream)))
    (symbol
     (output-symbol object stream))
    (number
     (etypecase object
       (integer
	(output-integer object stream))
       (float
	(output-float object stream))
       (ratio
	(output-ratio object stream))
       (ratio
	(output-ratio object stream))
       (complex
	(output-complex object stream))))
    (character
     (output-character object stream))
    (vector
     (output-vector object stream))
    (array
     (output-array object stream))
    (system-area-pointer
     (output-sap object stream))
    (weak-pointer
     (output-weak-pointer object stream))
    (lra
     (output-lra object stream))
    (code-component
     (output-code-component object stream))
    (fdefn
     (output-fdefn object stream))
    (t
     (output-random object stream)))
  (/show "leaving O-U-O"))

(in-package "SB-PRETTY")

;;; nee PPRINT-DISPATCH
(defun pprint-d (object &optional (table *print-pprint-dispatch*))
  (declare (type (or pprint-dispatch-table null) table))
  (/show "entering PPRINT-D")
  (let* ((table (or table *initial-pprint-dispatch*))
	 (cons-entry
	  (and (consp object)
	       (gethash (car object)
			(pprint-dispatch-table-cons-entries table))))
	 (entry
	  (dolist (entry (pprint-dispatch-table-entries table) cons-entry)
	    (when (and cons-entry
		       (entry< entry cons-entry))
	      (return cons-entry))
	    (when (funcall (pprint-dispatch-entry-test-fn entry) object)
	      (return entry)))))
    (/show "got TABLE, CONS-ENTRY, and ENTRY")
    (/show (not (null cons-entry)) (not (null entry)))
    (when entry
      (/show (pprint-dispatch-entry-function entry)))
    (if entry
	(values (pprint-dispatch-entry-function entry) t)
	(values #'(lambda (stream object)
		    (output-ugly-object object stream))
		nil))))