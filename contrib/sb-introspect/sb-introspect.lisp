;;; This is here as a discussion point, not yet a supported interface.  If
;;; you would like to use the functions here, or you would like other
;;; functions to be here, join the debate on sbcl-devel

;;; For the avoidance of doubt, the exported interface is the
;;; proposed supported interface.

(defpackage :sb-introspect
  (:use "CL")
  (:export "FUNCTION-ARGLIST" "VALID-FUNCTION-NAME-P"
	   "FIND-DEFINITION-SOURCE"
	   "DEFINITION-SOURCE" "DEFINITION-SOURCE-PATHNAME"
	   "DEFINITION-NOT-FOUND" "DEFINITION-NAME"
	   "DEFINITION-SOURCE-FORM-NUMBER" ; unsure.  character offset instead?
	   ))
(in-package :sb-introspect)


(defun valid-function-name-p (name)
  "True if NAME denotes a function name that can be passed to MACRO-FUNCTION or FDEFINITION "
  (and (sb-int:valid-function-name-p name) t))

(defun function-arglist (function)
  "Describe the lambda list for the function designator FUNCTION.
Works for macros, simple functions and generic functions"
  (cond ((valid-function-name-p function) 
         (function-arglist
	  (or (macro-function function) (fdefinition function))))
        ((typep function 'generic-function)
         (sb-pcl::generic-function-pretty-arglist function))
        (t
         (sb-impl::%simple-fun-arglist function))))

;;; Considering whether to throw this or something like it when a definition
;;; is unforthcoming.  Presently we do something undefined (NIL or random
;;; error)
(define-condition definition-not-found (error)
  ((name :initarg :name :reader definition-name))
  (:report (lambda (c s)
	     (format s "No definition for ~S known" (definition-name c)))))

;;; find-definition-source returns a definition-source object, with accessors
;;; as per export list.  Might not be a struct.
(defstruct definition-source pathname form-number)


;;; the intention is that everything we're able to query the source
;;; location for, we should be able to do it through this gf
(defgeneric find-definition-source (thing))

;;; breaks on structure accessors, probably other closures as well
(defmethod find-definition-source ((o function))
  (let* ((name (sb-vm::%simple-fun-name o))
	 (debug-info
	  (sb-kernel:%code-debug-info (sb-kernel:fun-code-header o)))
	 (debug-source (car (sb-c::compiled-debug-info-source debug-info))))
    ;; FIXME why only the first debug-source?  can there be >1?
    (sb-int:aver (not (cdr (sb-c::compiled-debug-info-source debug-info))))
    (make-definition-source
     :pathname
     (and (eql (sb-c::debug-source-from debug-source) :file)
	  (parse-namestring (sb-c::debug-source-name debug-source)))
     :form-number
     (loop for debug-fun across (sb-c::compiled-debug-info-fun-map debug-info)
	   when (and (sb-c::debug-fun-p debug-fun)
		     (eql (sb-c::compiled-debug-fun-name debug-fun) name))
	   return (sb-c::compiled-debug-fun-tlf-number debug-fun)))))

(defmethod find-definition-source ((o method))
  (find-definition-source (or (sb-pcl::method-fast-function o)
			      (sb-pcl:method-function o))))

(defmethod find-definition-source (name)
  (and (valid-function-name-p name)
       (find-definition-source (or (macro-function name) (fdefinition name)))))

