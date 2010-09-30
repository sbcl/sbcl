;;; introspection library

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;; For the avoidance of doubt, the exported interface is the supported
;;; interface. Anything else is internal, though you're welcome to argue a
;;; case for exporting it.

;;; If you steal the code from this file to cut and paste into your
;;; own project, there will be much wailing and gnashing of teeth.
;;; Your teeth.  If need be, we'll kick them for you.  This is a
;;; contrib, we're allowed to look in internals.  You're an
;;; application programmer, and are not.

;;; TODO
;;; 1) structs don't have within-file location info.  problem for the
;;;   structure itself, accessors and the predicate
;;; 3) error handling.  Signal random errors, or handle and resignal 'our'
;;;   error, or return NIL?
;;; 4) FIXMEs

(defpackage :sb-introspect
  (:use "CL")
  (:export "ALLOCATION-INFORMATION"
           "FUNCTION-ARGLIST"
           "FUNCTION-LAMBDA-LIST"
           "FUNCTION-TYPE"
           "DEFTYPE-LAMBDA-LIST"
           "VALID-FUNCTION-NAME-P"
           "FIND-DEFINITION-SOURCE"
           "FIND-DEFINITION-SOURCES-BY-NAME"
           "DEFINITION-SOURCE"
           "DEFINITION-SOURCE-PATHNAME"
           "DEFINITION-SOURCE-FORM-PATH"
           "DEFINITION-SOURCE-CHARACTER-OFFSET"
           "DEFINITION-SOURCE-FILE-WRITE-DATE"
           "DEFINITION-SOURCE-PLIST"
           "DEFINITION-NOT-FOUND" "DEFINITION-NAME"
           "FIND-FUNCTION-CALLEES"
           "FIND-FUNCTION-CALLERS"
           "WHO-BINDS"
           "WHO-CALLS"
           "WHO-REFERENCES"
           "WHO-SETS"
           "WHO-MACROEXPANDS"
           "WHO-SPECIALIZES-DIRECTLY"
           "WHO-SPECIALIZES-GENERALLY"))

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
  (let* ((function-object (sb-kernel::%fun-fun function))
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
  "True if NAME denotes a valid function name, ie. one that can be passed to
FBOUNDP."
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
  (plist nil)
  ;; Any extra metadata that the caller might be interested in. For
  ;; example the specializers of the method whose definition-source this
  ;; is.
  (description nil :type list))

(defun find-definition-sources-by-name (name type)
  "Returns a list of DEFINITION-SOURCEs for the objects of type TYPE
defined with name NAME. NAME may be a symbol or a extended function
name. Type can currently be one of the following:

   (Public)
   :CLASS
   :COMPILER-MACRO
   :CONDITION
   :CONSTANT
   :FUNCTION
   :GENERIC-FUNCTION
   :MACRO
   :METHOD
   :METHOD-COMBINATION
   :PACKAGE
   :SETF-EXPANDER
   :STRUCTURE
   :SYMBOL-MACRO
   :TYPE
   :VARIABLE

   (Internal)
   :OPTIMIZER
   :SOURCE-TRANSFORM
   :TRANSFORM
   :VOP

If an unsupported TYPE is requested, the function will return NIL.
"
  (flet ((listify (x)
           (if (listp x)
               x
               (list x)))
         (get-class (name)
           (and (symbolp name)
                (find-class name nil)))
         (real-fdefinition (name)
           ;; for getting the real function object, even if the
           ;; function is being profiled
           (let ((profile-info (gethash name sb-profile::*profiled-fun-name->info*)))
             (if profile-info
                 (sb-profile::profile-info-encapsulated-fun profile-info)
                 (fdefinition name)))))
    (listify
     (case type
       ((:variable)
        (when (and (symbolp name)
                   (eq (sb-int:info :variable :kind name) :special))
          (translate-source-location (sb-int:info :source-location type name))))
       ((:constant)
        (when (and (symbolp name)
                   (eq (sb-int:info :variable :kind name) :constant))
          (translate-source-location (sb-int:info :source-location type name))))
       ((:symbol-macro)
        (when (and (symbolp name)
                   (eq (sb-int:info :variable :kind name) :macro))
          (translate-source-location (sb-int:info :source-location type name))))
       ((:macro)
        (when (and (symbolp name)
                   (macro-function name))
          (find-definition-source (macro-function name))))
       ((:compiler-macro)
        (when (compiler-macro-function name)
          (find-definition-source (compiler-macro-function name))))
       ((:function :generic-function)
        (when (and (fboundp name)
                   (or (not (symbolp name))
                       (not (macro-function name))))
          (let ((fun (real-fdefinition name)))
            (when (eq (not (typep fun 'generic-function))
                      (not (eq type :generic-function)))
              (find-definition-source fun)))))
       ((:type)
        ;; Source locations for types are saved separately when the expander
        ;; is a closure without a good source-location.
        (let ((loc (sb-int:info :type :source-location name)))
          (if loc
              (translate-source-location loc)
              (let ((expander-fun (sb-int:info :type :expander name)))
                (when expander-fun
                  (find-definition-source expander-fun))))))
       ((:method)
        (when (fboundp name)
          (let ((fun (real-fdefinition name)))
           (when (typep fun 'generic-function)
             (loop for method in (sb-mop::generic-function-methods
                                  fun)
                for source = (find-definition-source method)
                when source collect source)))))
       ((:setf-expander)
        (when (and (consp name)
                   (eq (car name) 'setf))
          (setf name (cadr name)))
        (let ((expander (or (sb-int:info :setf :inverse name)
                            (sb-int:info :setf :expander name))))
          (when expander
            (sb-introspect:find-definition-source (if (symbolp expander)
                                                      (symbol-function expander)
                                                      expander)))))
       ((:structure)
        (let ((class (get-class name)))
          (if class
              (when (typep class 'sb-pcl::structure-class)
                (find-definition-source class))
              (when (sb-int:info :typed-structure :info name)
                (translate-source-location
                 (sb-int:info :source-location :typed-structure name))))))
       ((:condition :class)
        (let ((class (get-class name)))
          (when (and class
                     (not (typep class 'sb-pcl::structure-class)))
            (when (eq (not (typep class 'sb-pcl::condition-class))
                      (not (eq type :condition)))
              (find-definition-source class)))))
       ((:method-combination)
        (let ((combination-fun
               (find-method #'sb-mop:find-method-combination
                            nil
                            (list (find-class 'generic-function)
                                  (list 'eql name)
                                  t)
                            nil)))
          (when combination-fun
            (find-definition-source combination-fun))))
       ((:package)
        (when (symbolp name)
          (let ((package (find-package name)))
            (when package
              (find-definition-source package)))))
       ;; TRANSFORM and OPTIMIZER handling from swank-sbcl
       ((:transform)
        (when (symbolp name)
          (let ((fun-info (sb-int:info :function :info name)))
            (when fun-info
              (loop for xform in (sb-c::fun-info-transforms fun-info)
                    for source = (find-definition-source
                                  (sb-c::transform-function xform))
                    for typespec = (sb-kernel:type-specifier
                                    (sb-c::transform-type xform))
                    for note = (sb-c::transform-note xform)
                    do (setf (definition-source-description source)
                             (if (consp typespec)
                                 (list (second typespec) note)
                                 (list note)))
                    collect source)))))
       ((:optimizer)
        (when (symbolp name)
          (let ((fun-info (sb-int:info :function :info name)))
            (when fun-info
              (let ((otypes '((sb-c::fun-info-derive-type . sb-c:derive-type)
                              (sb-c::fun-info-ltn-annotate . sb-c:ltn-annotate)
                              (sb-c::fun-info-ltn-annotate . sb-c:ltn-annotate)
                              (sb-c::fun-info-optimizer . sb-c:optimizer))))
                (loop for (reader . name) in otypes
                      for fn = (funcall reader fun-info)
                      when fn collect
                      (let ((source (find-definition-source fn)))
                        (setf (definition-source-description source)
                              (list name))
                        source)))))))
       ((:vop)
        (when (symbolp name)
          (let ((fun-info (sb-int:info :function :info name)))
            (when fun-info
              (loop for vop in (sb-c::fun-info-templates fun-info)
                    for source = (find-definition-source
                                  (sb-c::vop-info-generator-function vop))
                    do (setf (definition-source-description source)
                             (list (sb-c::template-name vop)
                                   (sb-c::template-note vop)))
                    collect source)))))
       ((:source-transform)
        (when (symbolp name)
          (let ((transform-fun (sb-int:info :function :source-transform name)))
            (when transform-fun
              (sb-introspect:find-definition-source transform-fun)))))
       (t
        nil)))))

(defun find-definition-source (object)
  (typecase object
    ((or sb-pcl::condition-class sb-pcl::structure-class)
     (let ((classoid (sb-impl::find-classoid (class-name object))))
       (when classoid
         (let ((layout (sb-impl::classoid-layout classoid)))
           (when layout
             (translate-source-location
              (sb-kernel::layout-source-location layout)))))))
    (method-combination
     (car
      (find-definition-sources-by-name
       (sb-pcl::method-combination-type-name object) :method-combination)))
    (package
     (translate-source-location (sb-impl::package-source-location object)))
    (class
     (translate-source-location (sb-pcl::definition-source object)))
    ;; Use the PCL definition location information instead of the function
    ;; debug-info for methods and generic functions. Sometimes the
    ;; debug-info would point into PCL internals instead of the proper
    ;; location.
    (generic-function
     (let ((source (translate-source-location
                    (sb-pcl::definition-source object))))
       (when source
         (setf (definition-source-description source)
               (list (sb-mop:generic-function-lambda-list object))))
       source))
    (method
     (let ((source (translate-source-location
                    (sb-pcl::definition-source object))))
       (when source
         (setf (definition-source-description source)
               (append (method-qualifiers object)
                       (if (sb-mop:method-generic-function object)
                           (sb-pcl::unparse-specializers
                            (sb-mop:method-generic-function object)
                            (sb-mop:method-specializers object))
                           (sb-mop:method-specializers object)))))
       source))
    #+sb-eval
    (sb-eval:interpreted-function
     (let ((source (translate-source-location
                    (sb-eval:interpreted-function-source-location object))))
       source))
    (function
     (cond ((struct-accessor-p object)
            (find-definition-source
             (struct-accessor-structure-class object)))
           ((struct-predicate-p object)
            (find-definition-source
             (struct-predicate-structure-class object)))
           (t
            (find-function-definition-source object))))
    ((or condition standard-object structure-object)
     (find-definition-source (class-of object)))
    (t
     (error "Don't know how to retrieve source location for a ~S"
            (type-of object)))))

(defun find-function-definition-source (function)
  (let* ((debug-info (function-debug-info function))
         (debug-source (debug-info-source debug-info))
         (debug-fun (debug-info-debug-function debug-info))
         (tlf (if debug-fun (sb-c::compiled-debug-fun-tlf-number debug-fun))))
    (make-definition-source
     :pathname
     ;; KLUDGE: at the moment, we don't record the correct toplevel
     ;; form number for forms processed by EVAL (including EVAL-WHEN
     ;; :COMPILE-TOPLEVEL).  Until that's fixed, don't return a
     ;; DEFINITION-SOURCE with a pathname.  (When that's fixed, take
     ;; out the (not (debug-source-form ...)) test.
     (if (and (sb-c::debug-source-namestring debug-source)
              (not (sb-c::debug-source-form debug-source)))
         (parse-namestring (sb-c::debug-source-namestring debug-source)))
     :character-offset
     (if tlf
         (elt (sb-c::debug-source-start-positions debug-source) tlf))
     ;; Unfortunately there is no proper source path available in the
     ;; debug-source. FIXME: We could use sb-di:code-locations to get
     ;; a full source path. -luke (12/Mar/2005)
     :form-path (if tlf (list tlf))
     :file-write-date (sb-c::debug-source-created debug-source)
     :plist (sb-c::debug-source-plist debug-source))))

(defun translate-source-location (location)
  (if location
      (make-definition-source
       :pathname (let ((n (sb-c:definition-source-location-namestring location)))
                   (when n
                     (parse-namestring n)))
       :form-path
       (let ((number (sb-c:definition-source-location-toplevel-form-number
                         location)))
         (when number
           (list number)))
       :plist (sb-c:definition-source-location-plist location))
      (make-definition-source)))

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

(defun function-arglist (function)
  "Deprecated alias for FUNCTION-LAMBDA-LIST."
  (function-lambda-list function))

(define-compiler-macro function-arglist (function)
  (sb-int:deprecation-warning 'function-arglist 'function-lambda-list)
  `(function-lambda-list ,function))

(defun function-lambda-list (function)
  "Describe the lambda list for the extended function designator FUNCTION.
Works for special-operators, macros, simple functions, interpreted functions,
and generic functions. Signals an error if FUNCTION is not a valid extended
function designator."
  (cond ((valid-function-name-p function)
         (function-lambda-list (or (and (symbolp function)
                                        (macro-function function))
                                   (fdefinition function))))
        ((typep function 'generic-function)
         (sb-pcl::generic-function-pretty-arglist function))
        #+sb-eval
        ((typep function 'sb-eval:interpreted-function)
         (sb-eval:interpreted-function-lambda-list function))
        (t
         (sb-kernel:%simple-fun-arglist (sb-kernel:%fun-fun function)))))

(defun deftype-lambda-list (typespec-operator)
  "Returns the lambda list of TYPESPEC-OPERATOR as first return
value, and a flag whether the arglist could be found as second
value."
  (check-type typespec-operator symbol)
  (case (sb-int:info :type :kind typespec-operator)
    (:defined
     (sb-int:info :type :lambda-list typespec-operator))
    (:primitive
     (let ((translator-fun (sb-int:info :type :translator typespec-operator)))
       (if translator-fun
           (values (sb-kernel:%fun-lambda-list translator-fun) t)
           ;; Some builtin types (e.g. STRING) do not have a
           ;; translator, but they were actually defined via DEFTYPE
           ;; in src/code/deftypes-for-target.lisp.
           (sb-int:info :type :lambda-list typespec-operator))))
    (t (values nil nil))))

(defun function-type (function-designator)
  "Returns the ftype of FUNCTION-DESIGNATOR, or NIL."
  (flet ((ftype-of (function-designator)
           (sb-kernel:type-specifier
            (sb-int:info :function :type function-designator))))
    (etypecase function-designator
      (symbol
       (when (and (fboundp function-designator)
                  (not (macro-function function-designator))
                  (not (special-operator-p function-designator)))
         (ftype-of function-designator)))
      (cons
       (when (and (sb-int:legal-fun-name-p function-designator)
                  (fboundp function-designator))
         (ftype-of function-designator)))
      (generic-function
       (function-type (sb-pcl:generic-function-name function-designator)))
      (function
       ;; Give declared type in globaldb priority over derived type
       ;; because it contains more accurate information e.g. for
       ;; struct-accessors.
       (let ((type (function-type (sb-kernel:%fun-name
                                   (sb-impl::%fun-fun function-designator)))))
         (if type
             type
             (sb-impl::%fun-type function-designator)))))))

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
     space
     t)))

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

;;; XREF facility

(defun get-simple-fun (functoid)
  (etypecase functoid
    (sb-kernel::fdefn
     (get-simple-fun (sb-vm::fdefn-fun functoid)))
    ((or null sb-impl::funcallable-instance)
     nil)
    (function
     (sb-kernel::%fun-fun functoid))))

(defun collect-xref (kind-index wanted-name)
  (let ((ret nil))
    (dolist (env sb-c::*info-environment* ret)
      ;; Loop through the infodb ...
      (sb-c::do-info (env :class class :type type :name info-name
                          :value value)
        ;; ... looking for function or macro definitions
        (when (and (eql class :function)
                   (or (eql type :macro-function)
                       (eql type :definition)))
          ;; Get a simple-fun for the definition, and an xref array
          ;; from the table if available.
          (let* ((simple-fun (get-simple-fun value))
                 (xrefs (when simple-fun
                          (sb-kernel:%simple-fun-xrefs simple-fun)))
                 (array (when xrefs
                          (aref xrefs kind-index))))
            ;; Loop through the name/path xref entries in the table
            (loop for i from 0 below (length array) by 2
                  for xref-name = (aref array i)
                  for xref-path = (aref array (1+ i))
                  do (when (eql xref-name wanted-name)
                       (let ((source-location
                              (find-function-definition-source simple-fun)))
                         ;; Use the more accurate source path from
                         ;; the xref entry.
                         (setf (definition-source-form-path source-location)
                               xref-path)
                         (push (cons info-name source-location)
                               ret))))))))))

(defun who-calls (function-name)
  "Use the xref facility to search for source locations where the
global function named FUNCTION-NAME is called. Returns a list of
function name, definition-source pairs."
  (collect-xref #.(position :calls sb-c::*xref-kinds*) function-name))

(defun who-binds (symbol)
  "Use the xref facility to search for source locations where the
special variable SYMBOL is rebound. Returns a list of function name,
definition-source pairs."
  (collect-xref #.(position :binds sb-c::*xref-kinds*) symbol))

(defun who-references (symbol)
  "Use the xref facility to search for source locations where the
special variable or constant SYMBOL is read. Returns a list of function
name, definition-source pairs."
  (collect-xref #.(position :references sb-c::*xref-kinds*) symbol))

(defun who-sets (symbol)
  "Use the xref facility to search for source locations where the
special variable SYMBOL is written to. Returns a list of function name,
definition-source pairs."
  (collect-xref #.(position :sets sb-c::*xref-kinds*) symbol))

(defun who-macroexpands (macro-name)
  "Use the xref facility to search for source locations where the
macro MACRO-NAME is expanded. Returns a list of function name,
definition-source pairs."
  (collect-xref #.(position :macroexpands sb-c::*xref-kinds*) macro-name))

(defun who-specializes-directly (class-designator)
  "Search for source locations of methods directly specializing on
CLASS-DESIGNATOR. Returns an alist of method name, definition-source
pairs.

A method matches the criterion either if it specializes on the same
class as CLASS-DESIGNATOR designates (this includes CLASS-EQ
specializers), or if it eql-specializes on an instance of the
designated class.

Experimental.
"
  (let ((class (canonicalize-class-designator class-designator)))
    (unless class
      (return-from who-specializes-directly nil))
    (let ((result (collect-specializing-methods
                   #'(lambda (specl)
                       ;; Does SPECL specialize on CLASS directly?
                       (typecase specl
                         (sb-pcl::class-eq-specializer
                          (eq (sb-pcl::specializer-object specl) class))
                         (sb-pcl::eql-specializer
                          (let ((obj (sb-mop:eql-specializer-object specl)))
                            (eq (class-of obj) class)))
                         ((not sb-pcl::standard-specializer)
                          nil)
                         (t
                          (eq specl class)))))))
      (map-into result #'(lambda (m)
                           (cons `(method ,(method-generic-function-name m))
                                 (find-definition-source m)))
                result))))

(defun who-specializes-generally (class-designator)
  "Search for source locations of methods specializing on
CLASS-DESIGNATOR, or a subclass of it. Returns an alist of method
name, definition-source pairs.

A method matches the criterion either if it specializes on the
designated class itself or a subclass of it (this includes CLASS-EQ
specializers), or if it eql-specializes on an instance of the
designated class or a subclass of it.

Experimental.
"
  (let ((class (canonicalize-class-designator class-designator)))
    (unless class
      (return-from who-specializes-generally nil))
    (let ((result (collect-specializing-methods
                   #'(lambda (specl)
                       ;; Does SPECL specialize on CLASS or a subclass
                       ;; of it?
                       (typecase specl
                         (sb-pcl::class-eq-specializer
                          (subtypep (sb-pcl::specializer-object specl) class))
                         (sb-pcl::eql-specializer
                          (typep (sb-mop:eql-specializer-object specl) class))
                         ((not sb-pcl::standard-specializer)
                          nil)
                         (t
                          (subtypep specl class)))))))
      (map-into result #'(lambda (m)
                           (cons `(method ,(method-generic-function-name m))
                                 (find-definition-source m)))
                result))))

(defun canonicalize-class-designator (class-designator)
  (typecase class-designator
    (symbol (find-class class-designator nil))
    (class  class-designator)
    (t nil)))

(defun method-generic-function-name (method)
  (sb-mop:generic-function-name (sb-mop:method-generic-function method)))

(defun collect-specializing-methods (predicate)
  (let ((result '()))
    (sb-pcl::map-specializers
     #'(lambda (specl)
         (when (funcall predicate specl)
           (let ((methods (sb-mop:specializer-direct-methods specl)))
             (setf result (append methods result))))))
    (delete-duplicates result)))


;;;; ALLOCATION INTROSPECTION

(defun allocation-information (object)
  #+sb-doc
  "Returns information about the allocation of OBJECT. Primary return value
indicates the general type of allocation: :IMMEDIATE, :HEAP, :STACK,
or :FOREIGN.

Possible secondary return value provides additional information about the
allocation.

For :HEAP objects the secondary value is a plist:

  :SPACE
    Inficates the heap segment the object is allocated in.

  :GENERATION
    Is the current generation of the object: 0 for nursery, 6 for pseudo-static
    generation loaded from core. (GENCGC and :SPACE :DYNAMIC only.)

  :LARGE
    Indicates a \"large\" object subject to non-copying
    promotion. (GENCGC and :SPACE :DYNAMIC only.)

  :BOXED
    Indicates that the object is allocated in a boxed region. Unboxed
    allocation is used for eg. specialized arrays after they have survived one
    collection. (GENCGC and :SPACE :DYNAMIC only.)

  :PINNED
    Indicates that the page(s) on which the object resides are kept live due
    to conservative references. Note that object may reside on a pinned page
    even if :PINNED in NIL if the GC has not had the need to mark the the page
    as pinned. (GENCGC and :SPACE :DYNAMIC only.)

  :WRITE-PROTECTED
    Indicates that the page on which the object starts is write-protected,
    which indicates for :BOXED objects that it hasn't been written to since
    the last GC of its generation. (GENCGC and :SPACE :DYNAMIC only.)

  :PAGE
    The index of the page the object resides on. (GENGC and :SPACE :DYNAMIC
    only.)

For :STACK objects secondary value is the thread on whose stack the object is
allocated.

Expected use-cases include introspection to gain insight into allocation and
GC behaviour and restricting memoization to heap-allocated arguments.

Experimental: interface subject to change."
  ;; FIXME: Would be nice to provide the size of the object as well, though
  ;; maybe that should be a separate function, and something like MAP-PARTS
  ;; for mapping over parts of arbitrary objects so users can get "deep sizes"
  ;; as well if they want to.
  ;;
  ;; FIXME: For the memoization use-case possibly we should also provide a
  ;; simpler HEAP-ALLOCATED-P, since that doesn't require disabling the GC
  ;; scanning threads for negative answers? Similarly, STACK-ALLOCATED-P for
  ;; checking if an object has been stack-allocated by a given thread for
  ;; testing purposes might not come amiss.
  (if (typep object '(or fixnum character))
      (values :immediate nil)
      (let ((plist
             (sb-sys:without-gcing
               ;; Disable GC so the object cannot move to another page while
               ;; we have the address.
               (let* ((addr (sb-kernel:get-lisp-obj-address object))
                      (space
                       (cond ((< sb-vm:read-only-space-start addr
                                 (* sb-vm:*read-only-space-free-pointer*
                                    sb-vm:n-word-bytes))
                              :read-only)
                             ((< sb-vm:static-space-start addr
                                 (* sb-vm:*static-space-free-pointer*
                                    sb-vm:n-word-bytes))
                              :static)
                             ((< (sb-kernel:current-dynamic-space-start) addr
                                 (sb-sys:sap-int (sb-kernel:dynamic-space-free-pointer)))
                              :dynamic))))
                 (when space
                   #+gencgc
                   (if (eq :dynamic space)
                       (let ((index (sb-vm::find-page-index addr)))
                         (symbol-macrolet ((page (sb-alien:deref sb-vm::page-table index)))
                           (let ((flags (sb-alien:slot page 'sb-vm::flags)))
                             (list :space space
                                   :generation (sb-alien:slot page 'sb-vm::gen)
                                   :write-protected (logbitp 0 flags)
                                   :boxed (logbitp 2 flags)
                                   :pinned (logbitp 5 flags)
                                   :large (logbitp 6 flags)
                                   :page index))))
                       (list :space space))
                   #-gencgc
                   (list :space space))))))
        (cond (plist
               (values :heap plist))
              (t
               (let ((sap (sb-sys:int-sap (sb-kernel:get-lisp-obj-address object))))
                 ;; FIXME: Check other stacks as well.
                 #+sb-thread
                 (dolist (thread (sb-thread:list-all-threads))
                   (let ((c-start (sb-di::descriptor-sap
                                   (sb-thread::%symbol-value-in-thread
                                    'sb-vm:*control-stack-start*
                                    thread)))
                         (c-end (sb-di::descriptor-sap
                                 (sb-thread::%symbol-value-in-thread
                                  'sb-vm:*control-stack-end*
                                  thread))))
                     (when (and c-start c-end)
                       (when (and (sb-sys:sap<= c-start sap)
                                  (sb-sys:sap< sap c-end))
                         (return-from allocation-information
                           (values :stack thread))))))
                 #-sb-thread
                 (when (sb-vm:control-stack-pointer-valid-p sap nil)
                   (return-from allocation-information
                     (values :stack sb-thread::*current-thread*))))
               :foreign)))))

