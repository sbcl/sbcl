;;; This is here as a discussion point, not yet a supported interface.  If
;;; you would like to use the functions here, or you would like other
;;; functions to be here, join the debate on navel@metacircles.com.
;;; List info at http://lists.metacircles.com/cgi-bin/mailman/listinfo/navel

;;; For the avoidance of doubt, the exported interface is the proposed
;;; supported interface.  Anything else is internal, though you're
;;; welcome to argue a case for exporting it.

;;; If you steal the code from this file to cut and paste into your
;;; own project, there will be much wailing and gnashing of teeth.
;;; Your teeth.  If need be, we'll kick them for you.  This is a
;;; contrib, we're allowed to look in internals.  You're an
;;; application programmer, and are not.

;;; TODO
;;; 1) structs don't have within-file location info.  problem for the
;;;   structure itself, accessors and the predicate
;;; 2) what should find-definition-source on a symbol return?  there may be
;;;   several definitions (class, function, etc)
;;; 3) error handling.  Signal random errors, or handle and resignal 'our'
;;;   error, or return NIL?
;;; 4) FIXMEs
;;; 5) would be nice to have some interface to the compiler that lets us
;;;   fake the filename and position, for use with C-M-x

(declaim (optimize (debug 1)))

(defpackage :sb-introspect
  (:use "CL")
  (:export "FUNCTION-ARGLIST" "VALID-FUNCTION-NAME-P"
	   "FIND-DEFINITION-SOURCE"
	   "DEFINITION-SOURCE" "DEFINITION-SOURCE-PATHNAME"
	   "DEFINITION-NOT-FOUND" "DEFINITION-NAME"
	   "DEFINITION-SOURCE-FORM-PATH"
	   "DEFINITION-SOURCE-CHARACTER-OFFSET"
	   ))
(in-package :sb-introspect)


(defun valid-function-name-p (name)
  "True if NAME denotes a function name that can be passed to MACRO-FUNCTION or FDEFINITION "
  (and (sb-int:valid-function-name-p name) t))

(defun function-arglist (function)
  "Describe the lambda list for the function designator FUNCTION.
Works for macros, simple functions and generic functions.  Signals error
if not found"
  (cond ((valid-function-name-p function) 
         (function-arglist
	  (or (macro-function function) (fdefinition function))))
        ((typep function 'generic-function)
         (sb-pcl::generic-function-pretty-arglist function))
	(t (sb-impl::%simple-fun-arglist
	    (sb-impl::%closure-fun function)))))

(defgeneric find-definition-source (thing)
  (:documentation "Find the source location that defines THING.
Returns a DEFINITION-SOURCE object"))

;;; This is an opaque object with accessors as per export list.
;;; Might not be a struct.

(defstruct definition-source
  pathname				; source file, not fasl
  form-path
  character-offset
  )

;;; This is kludgey.  We expect these functions (the underlying functions,
;;; not the closures) to be in static space and so not move ever.
;;; FIXME It's also possibly wrong: not all structures use these vanilla
;;; accessors, e.g. when the :type option is used
(defvar *struct-slotplace-reader*
  (sb-vm::%simple-fun-self #'definition-source-pathname))
(defvar *struct-slotplace-writer*
  (sb-vm::%simple-fun-self #'(setf definition-source-pathname)))
(defvar *struct-predicate*
  (sb-vm::%simple-fun-self #'definition-source-p))

;; Internal-only, don't call this directly
(defun find-function-definition-source (o)
  (let* ((debug-info
	  (sb-kernel:%code-debug-info
	   (sb-kernel:fun-code-header(sb-kernel::%closure-fun o))))
	 (debug-source
	  (car (sb-c::compiled-debug-info-source debug-info)))
	 (debug-fun (elt (sb-c::compiled-debug-info-fun-map debug-info) 0))
	 (tlf (and debug-fun (sb-c::compiled-debug-fun-tlf-number debug-fun))))
    ;; HAZARDOUS ASSUMPTION: in CMUCL it's possible to get >1 debug-source
    ;; for a debug-info (one per file).  In SBCL the function that builds
    ;; debug-sources always produces singleton lists
    (sb-int:aver (not (cdr (sb-c::compiled-debug-info-source debug-info))))
    (make-definition-source
     :pathname
     (and (eql (sb-c::debug-source-from debug-source) :file)
	  (parse-namestring (sb-c::debug-source-name debug-source)))
     ;; we don't have a real sexp path, annoyingly.  Fake one from the
     ;; top-level form number
     :character-offset
     (and tlf
	  (elt (sb-c::debug-source-start-positions debug-source) tlf))
     :form-path (and tlf (list tlf)))))

(defmethod find-definition-source ((o function))
  (cond
    ((struct-accessor-p o)
     (find-definition-source (struct-accessor-structure-class o)))
    ((struct-predicate-p o)
     (find-definition-source (struct-predicate-structure-class o)))
    (t (find-function-definition-source o))))

(defmethod find-definition-source ((o method))
  (find-definition-source (or (sb-pcl::method-fast-function o)
			      (sb-pcl:method-function o))))

(defmethod find-definition-source (name)
  (and (valid-function-name-p name)
       (find-definition-source (or (macro-function name) (fdefinition name)))))

;; these are internal functions, and probably incomplete
(defun struct-accessor-p (function)
  (let ((self (sb-vm::%simple-fun-self function)))
    ;; FIXME there are other kinds of struct accessor.  Fill out this list
    (member self (list *struct-slotplace-reader*
		       *struct-slotplace-writer*))))

(defun struct-predicate-p (function)
  (let ((self (sb-vm::%simple-fun-self function)))
    ;; FIXME there may be other structure predicate functions
    (member self (list *struct-predicate*))))

;; FIXME need one for constructor too, perhaps

(defun struct-accessor-structure-class (function)
  (let ((self (sb-vm::%simple-fun-self function)))
    (cond
      ((member self (list *struct-slotplace-reader* *struct-slotplace-writer*))
       (find-class
	(sb-kernel::classoid-name
	 (sb-kernel::layout-classoid
	  (sb-kernel:%closure-index-ref function 1)))))
      )))

(defun struct-predicate-structure-class (function)
  (let ((self (sb-vm::%simple-fun-self function)))
    (cond
      ((member self (list *struct-predicate*))
       (find-class
	(sb-kernel::classoid-name
	 (sb-kernel::layout-classoid
	  (sb-kernel:%closure-index-ref function 0)))))
      )))

(defmethod find-definition-source ((o structure-class))
  ;; FIXME we don't get form-number from this, which is a shame
  (let ((constructor
	 (sb-kernel::structure-classoid-constructor
	  (sb-kernel:classoid-cell-classoid
	   (sb-int:info :type :classoid (class-name o))))))
    (find-definition-source constructor)))

(provide 'sb-introspect)
