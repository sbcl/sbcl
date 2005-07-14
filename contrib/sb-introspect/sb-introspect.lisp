;;; introspection library

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

(defpackage :sb-introspect
  (:use "CL")
  (:export "FUNCTION-ARGLIST"
           "VALID-FUNCTION-NAME-P"
           "FIND-DEFINITION-SOURCE"
           "DEFINITION-SOURCE"
           "DEFINITION-SOURCE-PATHNAME"
           "DEFINITION-SOURCE-FORM-PATH"
           "DEFINITION-SOURCE-CHARACTER-OFFSET"
           "DEFINITION-SOURCE-FILE-WRITE-DATE"
           "DEFINITION-SOURCE-PLIST"
           "DEFINITION-NOT-FOUND" "DEFINITION-NAME"
           "FIND-FUNCTION-CALLEES"
           "FIND-FUNCTION-CALLERS"))

(in-package :sb-introspect)

;;;; Internal interface for SBCL debug info

;;; Here are some tutorial-style type definitions to help understand
;;; the internal SBCL debugging data structures we're using. The
;;; commentary is based on CMUCL's debug internals manual.
;;;
(deftype debug-info ()
  "Structure containing all the debug information related to a function.
Function objects reference debug-infos which in turn reference
debug-sources and so on."
  'sb-c::compiled-debug-info)

(deftype debug-source ()
  "Debug sources describe where to find source code.
For example, the debug source for a function compiled from a file will
include the pathname of the file and the position of the definition."
  'sb-c::debug-source)

(deftype debug-function ()
  "Debug function represent static compile-time information about a function."
  'sb-c::compiled-debug-fun)

(declaim (ftype (function (function) debug-info) function-debug-info))
(defun function-debug-info (function)
  (let* ((function-object (sb-kernel::%closure-fun function))
         (function-header (sb-kernel:fun-code-header function-object)))
    (sb-kernel:%code-debug-info function-header)))

(declaim (ftype (function (function) debug-source) function-debug-source))
(defun function-debug-source (function)
  (debug-info-source (function-debug-info function)))

(declaim (ftype (function (debug-info) debug-source) debug-info-source))
(defun debug-info-source (debug-info)
  (sb-c::debug-info-source debug-info))

(declaim (ftype (function (debug-info) debug-function) debug-info-debug-function))
(defun debug-info-debug-function (debug-info)
  (elt (sb-c::compiled-debug-info-fun-map debug-info) 0))

(defun valid-function-name-p (name)
  "True if NAME denotes a function name that can be passed to MACRO-FUNCTION or FDEFINITION "
  (and (sb-int:valid-function-name-p name) t))

;;;; Finding definitions

(defstruct definition-source
  ;; Pathname of the source file that the definition was compiled from.
  ;; This is null if the definition was not compiled from a file.
  (pathname nil :type (or null pathname))
  ;; Source-path of the definition within the file.
  ;; This may be incomplete depending on the debug level at which the
  ;; source was compiled.
  (form-path '() :type list)
  ;; Character offset of the top-level-form containing the definition.
  ;; This corresponds to the first element of form-path.
  (character-offset nil :type (or null integer))
  ;; File-write-date of the source file when compiled.
  ;; Null if not compiled from a file.
  (file-write-date nil :type (or null integer))
  ;; plist from WITH-COMPILATION-UNIT
  (plist nil))

(defun find-definition-source (object)
  (etypecase object
    (method
     (find-definition-source (or (sb-pcl::method-fast-function object)
                                 (sb-pcl:method-function object))))
    (function
     (cond ((struct-accessor-p object)
            (find-definition-source (struct-accessor-structure-class object)))
           ((struct-predicate-p object)
            (find-definition-source (struct-predicate-structure-class object)))
           (t (find-function-definition-source object))))
    (structure-class
     (let ((constructor
            (sb-kernel::structure-classoid-constructor
             (sb-kernel:classoid-cell-classoid
              (sb-int:info :type :classoid (class-name object))))))
       (find-definition-source constructor)))
    (t
     (if (valid-function-name-p object)
         (find-definition-source (or (macro-function object)
                                     (fdefinition object)))))))

(defun find-function-definition-source (function)
  (let* ((debug-info (function-debug-info function))
         (debug-source (debug-info-source debug-info))
         (debug-fun (debug-info-debug-function debug-info))
         (tlf (if debug-fun (sb-c::compiled-debug-fun-tlf-number debug-fun))))
    (make-definition-source
     :pathname
     (if (eql (sb-c::debug-source-from debug-source) :file)
         (parse-namestring (sb-c::debug-source-name debug-source)))
     :character-offset
     (if tlf
         (elt (sb-c::debug-source-start-positions debug-source) tlf))
     ;; Unfortunately there is no proper source path available in the
     ;; debug-source. FIXME: We could use sb-di:code-locations to get
     ;; a full source path. -luke (12/Mar/2005)
     :form-path (if tlf (list tlf))
     :file-write-date (sb-c::debug-source-created debug-source)
     :plist (sb-c::debug-source-plist debug-source))))

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

(defun struct-accessor-p (function)
  (let ((self (sb-vm::%simple-fun-self function)))
    ;; FIXME there are other kinds of struct accessor.  Fill out this list
    (member self (list *struct-slotplace-reader*
                       *struct-slotplace-writer*))))

(defun struct-predicate-p (function)
  (let ((self (sb-vm::%simple-fun-self function)))
    ;; FIXME there may be other structure predicate functions
    (member self (list *struct-predicate*))))

;;; FIXME: maybe this should be renamed as FUNCTION-LAMBDA-LIST?
(defun function-arglist (function)
  "Describe the lambda list for the function designator FUNCTION.
Works for special-operators, macros, simple functions and generic
functions.  Signals error if not found"
  (cond ((valid-function-name-p function)
         (function-arglist
          (or (macro-function function) (fdefinition function))))
        ((typep function 'generic-function)
         (sb-pcl::generic-function-pretty-arglist function))
        (t (sb-impl::%simple-fun-arglist
            (sb-impl::%closure-fun function)))))

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

;;;; find callers/callees, liberated from Helmut Eller's code in SLIME

;;; This interface is trmendously experimental.

;;; For the moment I'm taking the view that FDEFN is an internal
;;; object (one out of one CMUCL developer surveyed didn't know what
;;; they were for), so these routines deal in FUNCTIONs

;;; Find callers and callees by looking at the constant pool of
;;; compiled code objects.  We assume every fdefn object in the
;;; constant pool corresponds to a call to that function.  A better
;;; strategy would be to use the disassembler to find actual
;;; call-sites.

(defun find-function-callees (function)
  "Return functions called by FUNCTION."
  (let ((callees '()))
    (map-code-constants
     (sb-kernel:fun-code-header function)
     (lambda (obj)
       (when (sb-kernel:fdefn-p obj)
         (push (sb-kernel:fdefn-fun obj)
               callees))))
    callees))


(defun find-function-callers (function &optional (spaces '(:read-only :static
                                                           :dynamic)))
  "Return functions which call FUNCTION, by searching SPACES for code objects"
  (let ((referrers '()))
    (map-caller-code-components
     function
     spaces
     (lambda (code)
       (let ((entry (sb-kernel:%code-entry-points  code)))
         (cond ((not entry)
                (push (princ-to-string code) referrers))
               (t
                (loop for e = entry then (sb-kernel::%simple-fun-next e)
                      while e
                      do (pushnew e referrers)))))))
    referrers))

(declaim (inline map-code-constants))
(defun map-code-constants (code fn)
  "Call FN for each constant in CODE's constant pool."
  (check-type code sb-kernel:code-component)
  (loop for i from sb-vm:code-constants-offset below
        (sb-kernel:get-header-data code)
        do (funcall fn (sb-kernel:code-header-ref code i))))

(declaim (inline map-allocated-code-components))
(defun map-allocated-code-components (spaces fn)
  "Call FN for each allocated code component in one of SPACES.  FN
receives the object and its size as arguments.  SPACES should be a
list of the symbols :dynamic, :static, or :read-only."
  (dolist (space spaces)
    (sb-vm::map-allocated-objects
     (lambda (obj header size)
       (when (= sb-vm:code-header-widetag header)
         (funcall fn obj size)))
     space)))

(declaim (inline map-caller-code-components))
(defun map-caller-code-components (function spaces fn)
  "Call FN for each code component with a fdefn for FUNCTION in its
constant pool."
  (let ((function (coerce function 'function)))
    (map-allocated-code-components
     spaces
     (lambda (obj size)
       (declare (ignore size))
       (map-code-constants
        obj
        (lambda (constant)
          (when (and (sb-kernel:fdefn-p constant)
                     (eq (sb-kernel:fdefn-fun constant)
                         function))
            (funcall fn obj))))))))

(provide 'sb-introspect)
