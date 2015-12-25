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
;;;   structure itself, accessors, the copier and the predicate
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
           "DEFINITION-SOURCE-FORM-NUMBER"
           "DEFINITION-SOURCE-CHARACTER-OFFSET"
           "DEFINITION-SOURCE-FILE-WRITE-DATE"
           "DEFINITION-SOURCE-PLIST"
           "FIND-FUNCTION-CALLEES"
           "FIND-FUNCTION-CALLERS"
           "MAP-ROOT"
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

(declaim (ftype (function (t debug-info) debug-function) debug-info-debug-function))
(defun debug-info-debug-function (function debug-info)
  (let ((map (sb-c::compiled-debug-info-fun-map debug-info))
        (name (sb-kernel:%simple-fun-name (sb-kernel:%fun-fun function))))
    (or
     (find-if
      (lambda (x)
        (and
         (sb-c::compiled-debug-fun-p x)
         (eq (sb-c::compiled-debug-fun-name x) name)))
      map)
     (elt map 0))))

(defun valid-function-name-p (name)
  "True if NAME denotes a valid function name, ie. one that can be passed to
FBOUNDP."
  (and (sb-int:valid-function-name-p name) t))

;;;; Utilities for code

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

;;;; Finding definitions

(defstruct definition-source
  ;; Pathname of the source file that the definition was compiled from.
  ;; This is null if the definition was not compiled from a file.
  (pathname nil :type (or null pathname))
  ;; Source-path of the definition within the file.
  ;; This may be incomplete depending on the debug level at which the
  ;; source was compiled.
  (form-path '() :type list)
  ;; Depth first number of the form.
  ;; FORM-PATH above usually contains just the top-level form number,
  ;; ideally the proper form path could be dervied from the
  ;; form-number and the tlf-number, but it's a bit complicated and
  ;; Slime already knows how to deal with form numbers, so delegate
  ;; that job to Slime.
  (form-number nil :type (or null unsigned-byte))
  ;; Character offset of the top-level-form containing the definition.
  ;; This corresponds to the first element of form-path.
  (character-offset nil :type (or null unsigned-byte))
  ;; File-write-date of the source file when compiled.
  ;; Null if not compiled from a file.
  (file-write-date nil :type (or null unsigned-byte))
  ;; plist from WITH-COMPILATION-UNIT
  (plist nil)
  ;; Any extra metadata that the caller might be interested in. For
  ;; example the specializers of the method whose definition-source this
  ;; is.
  (description nil :type list))

(defun vop-sources-from-fun-templates (name)
  (let ((fun-info (sb-int:info :function :info name)))
    (when fun-info
      (loop for vop in (sb-c::fun-info-templates fun-info)
            for source = (find-definition-source
                          (sb-c::vop-info-generator-function vop))
            do (setf (definition-source-description source)
                     (if (sb-c::template-note vop)
                         (list (sb-c::template-name vop)
                               (sb-c::template-note vop))
                         (list (sb-c::template-name vop))))
            collect source))))

(defun find-vop-source (name)
  (let* ((templates (vop-sources-from-fun-templates name))
         (vop (gethash name sb-c::*backend-template-names*))
         (generator (when vop
                      (sb-c::vop-info-generator-function vop)))
         (source (when generator
                   (find-definition-source generator))))
    (cond
      (source
       (setf (definition-source-description source)
             (list name))
       (cons source templates))
      (t
       templates))))

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
   :ALIEN-TYPE
   :VARIABLE
   :DECLARATION

   (Internal)
   :OPTIMIZER
   :SOURCE-TRANSFORM
   :TRANSFORM
   :VOP
   :IR1-CONVERT

If an unsupported TYPE is requested, the function will return NIL.
"
  (flet ((get-class (name)
           (and (symbolp name)
                (find-class name nil)))
         (real-fdefinition (name)
           ;; for getting the real function object, even if the
           ;; function is being profiled
           (let ((profile-info (gethash name sb-profile::*profiled-fun-name->info*)))
             (if profile-info
                 (sb-profile::profile-info-encapsulated-fun profile-info)
                 (fdefinition name)))))
    (sb-int:ensure-list
     (case type
       ((:variable)
        (when (and (symbolp name)
                   (member (sb-int:info :variable :kind name)
                           '(:global :special :alien)))
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
       (:ir1-convert
        (let ((converter (sb-int:info :function :ir1-convert name)))
          (and converter
           (find-definition-source converter))))
       ((:function :generic-function)
        (when (and (fboundp name)
                   (or (consp name)
                       (and
                        (not (macro-function name))
                        (not (special-operator-p name)))))
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
                (when (functionp expander-fun)
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
            (find-definition-source
             (cond ((symbolp expander) (symbol-function expander))
                   ((listp expander) (cdr expander))
                   (t expander))))))
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
        (let ((fun-info (and (symbolp name)
                             (sb-int:info :function :info name))))
          (when fun-info
            (let ((otypes '((sb-c:fun-info-derive-type . sb-c:derive-type)
                            (sb-c:fun-info-ltn-annotate . sb-c:ltn-annotate)
                            (sb-c:fun-info-optimizer . sb-c:optimizer)
                            (sb-c:fun-info-ir2-convert . sb-c:ir2-convert)
                            (sb-c::fun-info-stack-allocate-result
                             . sb-c::stack-allocate-result))))
              (loop for (reader . name) in otypes
                    for fn = (funcall reader fun-info)
                    when fn collect
                    (let ((source (find-definition-source fn)))
                      (setf (definition-source-description source)
                            (list name))
                      source))))))
       (:vop
        (let ((loc (sb-int:info :source-location type name)))
          (if loc
              (translate-source-location loc)
              (find-vop-source name))))
       (:alien-type
        (let ((loc (sb-int:info :source-location type name)))
          (and loc
               (translate-source-location loc))))
       ((:source-transform)
        (let* ((transform-fun
                (or (sb-int:info :function :source-transform name)
                    (and (typep name '(cons (eql setf) (cons symbol null)))
                         (sb-int:info :function :source-transform
                                      (second name)))))
               ;; A cons for the :source-transform is essentially the same
               ;; info that was formerly in :structure-accessor.
               (accessor (and (consp transform-fun) (cdr transform-fun))))
          ;; Structure accessors have source transforms, but the
          ;; returned locations will neither show the actual place
          ;; where it's defined, nor is really interesting.
          (when (and transform-fun
                     (not accessor))
            (find-definition-source transform-fun))))
       (:declaration
        (let ((locations (sb-int:info :source-location :declaration name)))
          (loop for (kind loc) on locations by #'cddr
                when loc
                collect (let ((loc (translate-source-location loc)))
                          (setf (definition-source-description loc)
                                ;; Copy list to ensure that user code
                                ;; cannot mutate the original.
                                (copy-list (sb-int:ensure-list kind)))
                          loc))))
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
    ((or class sb-mop:slot-definition)
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
    #+sb-fasteval
    (sb-interpreter:interpreted-function
     (translate-source-location (sb-interpreter:fun-source-location object)))
    (function
     (find-function-definition-source object))
    ((or condition standard-object structure-object)
     (find-definition-source (class-of object)))
    (t
     (error "Don't know how to retrieve source location for a ~S"
            (type-of object)))))

(defun find-function-definition-source (function)
  (let* ((debug-info (function-debug-info function))
         (debug-source (debug-info-source debug-info))
         (debug-fun (debug-info-debug-function function debug-info))
         (tlf (if debug-fun (sb-c::compiled-debug-fun-tlf-number debug-fun))))
    (make-definition-source
     :pathname
     (when (stringp (sb-c::debug-source-namestring debug-source))
       (parse-namestring (sb-c::debug-source-namestring debug-source)))
     :character-offset
     (if tlf
         (elt (sb-c::debug-source-start-positions debug-source) tlf))
     :form-path (if tlf (list tlf))
     :form-number (sb-c::compiled-debug-fun-form-number debug-fun)
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
       :form-number (sb-c:definition-source-location-form-number
                     location)
       :plist (sb-c:definition-source-location-plist location))
      (make-definition-source)))

(sb-int:define-deprecated-function :late "1.0.24.5" function-arglist function-lambda-list
    (function)
  (function-lambda-list function))

(defun function-lambda-list (function)
  "Describe the lambda list for the extended function designator FUNCTION.
Works for special-operators, macros, simple functions, interpreted functions,
and generic functions. Signals an error if FUNCTION is not a valid extended
function designator."
  (cond ((and (symbolp function) (special-operator-p function))
         (function-lambda-list (sb-int:info :function :ir1-convert function)))
        ((valid-function-name-p function)
         (function-lambda-list (or (and (symbolp function)
                                        (macro-function function))
                                   (fdefinition function))))
        ((typep function 'generic-function)
         (sb-pcl::generic-function-pretty-arglist function))
        (t
         (sb-kernel:%fun-lambda-list function))))

(defun deftype-lambda-list (typespec-operator)
  "Returns the lambda list of TYPESPEC-OPERATOR as first return
value, and a flag whether the arglist could be found as second
value."
  (check-type typespec-operator symbol)
  ;; Don't return a lambda-list for combinators AND,OR,NOT.
  (let* ((f (and (sb-int:info :type :kind typespec-operator)
                 (sb-int:info :type :expander typespec-operator)))
         (f (if (listp f) (car f) f)))
    (if (functionp f)
        (values (sb-kernel:%fun-lambda-list f) t)
        (values nil nil))))

(defun function-type (function-designator)
  "Returns the ftype of FUNCTION-DESIGNATOR, or NIL."
  (flet ((ftype-of (function-designator)
           (sb-kernel:type-specifier
            (sb-int:proclaimed-ftype function-designator))))
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

;;;; find callers/callees, liberated from Helmut Eller's code in SLIME

;;; This interface is tremendously experimental.

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
  (declare (sb-kernel:simple-fun function))
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

;;; XREF facility

(defun get-simple-fun (functoid)
  (etypecase functoid
    (sb-kernel:fdefn
     (get-simple-fun (sb-kernel:fdefn-fun functoid)))
    ((or null sb-kernel:funcallable-instance)
     nil)
    (sb-kernel:closure
     ;; FIXME: should use ENCAPSULATION-INFO instead of hardwiring an index.
     (let ((fun (sb-kernel:%closure-fun functoid)))
       (if (and (eq (sb-kernel:%fun-name fun) 'sb-impl::encapsulation)
                (plusp (sb-kernel:get-closure-length functoid))
                (typep (sb-kernel:%closure-index-ref functoid 0) 'sb-impl::encapsulation-info))
           (get-simple-fun
            (sb-impl::encapsulation-info-definition
             (sb-kernel:%closure-index-ref functoid 0)))
           fun)))
    (function
     (sb-kernel:%fun-fun functoid))))

;; Call FUNCTION with two args, NAME and VALUE, for each value that is
;; either the FDEFINITION or MACRO-FUNCTION of some global name.
;;
(defun call-with-each-global-functoid (function)
  (sb-c::call-with-each-globaldb-name
   (lambda (name)
     ;; In general it might be unsafe to call INFO with a NAME that is not
     ;; valid for the kind of info being retrieved, as when the defaulting
     ;; function tries to perform a sanity-check. But here it's safe.
     (let ((functoid (or (sb-int:info :function :macro-function name)
                         (sb-int:info :function :definition name))))
             (if functoid
                 (funcall function name functoid))))))

(defun collect-xref (kind-index wanted-name)
  (let ((ret nil))
    (call-with-each-global-functoid
     (lambda (info-name value)
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
                  do (when (equal xref-name wanted-name)
                       (let ((source-location
                              (find-function-definition-source simple-fun)))
                         ;; Use the more accurate source path from
                         ;; the xref entry.
                         (setf (definition-source-form-path source-location)
                               xref-path)
                         (push (cons info-name source-location)
                               ret)))))))
    ret))

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
    Indicates the heap segment the object is allocated in.

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
  (if (typep object '(or fixnum character
                      #.(if (= sb-vm:n-word-bits 64) 'single-float (values))))
      (values :immediate nil)
      (let ((plist
             (sb-sys:without-gcing
               ;; Disable GC so the object cannot move to another page while
               ;; we have the address.
               (let* ((addr (sb-kernel:get-lisp-obj-address object))
                      (space
                       (cond ((< sb-vm:read-only-space-start addr
                                 (ash sb-vm:*read-only-space-free-pointer*
                                      sb-vm:n-fixnum-tag-bits))
                              :read-only)
                             ((< sb-vm:static-space-start addr
                                 (ash sb-vm:*static-space-free-pointer*
                                      sb-vm:n-fixnum-tag-bits))
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

(defun map-root (function object &key simple (ext t))
  "Call FUNCTION with all non-immediate objects pointed to by OBJECT.
Returns OBJECT.

If SIMPLE is true (default is NIL), elides those pointers that are not
notionally part of certain built-in objects, but backpointers to a
conceptual parent: eg. elides the pointer from a SYMBOL to the
corresponding PACKAGE.

If EXT is true (default is T), includes some pointers that are not
actually contained in the object, but found in certain well-known
indirect containers: FDEFINITIONs, EQL specializers, classes, and
thread-local symbol values in other threads fall into this category.

NOTE: calling MAP-ROOT with a THREAD does not currently map over
conservative roots from the thread registers and interrupt contexts.

Experimental: interface subject to change."
  (let ((fun (coerce function 'function))
        (seen (sb-int:alloc-xset)))
    (flet ((call (part)
             (when (and (member (sb-kernel:lowtag-of part)
                                `(,sb-vm:instance-pointer-lowtag
                                  ,sb-vm:list-pointer-lowtag
                                  ,sb-vm:fun-pointer-lowtag
                                  ,sb-vm:other-pointer-lowtag))
                        (not (sb-int:xset-member-p part seen)))
               (sb-int:add-to-xset part seen)
               (funcall fun part))))
      (when ext
        (let ((table sb-pcl::*eql-specializer-table*))
          (call (sb-int:with-locked-system-table (table)
                  (gethash object table)))))
      (etypecase object
        ((or bignum float sb-sys:system-area-pointer fixnum))
        (sb-ext:weak-pointer
         (call (sb-ext:weak-pointer-value object)))
        (cons
         (call (car object))
         (call (cdr object))
         (when (and ext (ignore-errors (fboundp object)))
           (call (fdefinition object))))
        (ratio
         (call (numerator object))
         (call (denominator object)))
        (complex
         (call (realpart object))
         (call (realpart object)))
        (sb-vm::instance
         (call (sb-kernel:%instance-layout object))
         (sb-kernel:do-instance-tagged-slot (i object)
           (call (sb-kernel:%instance-ref object i)))
         #+sb-thread
         (when (typep object 'sb-thread:thread)
           (cond ((eq object sb-thread:*current-thread*)
                  (dolist (value (sb-thread::%thread-local-references))
                    (call value))
                  (sb-vm::map-stack-references #'call))
                 (t
                  ;; KLUDGE: INTERRUPT-THREAD is Not Nice (tm), but
                  ;; the alternative would be stopping the world...
                  #+sb-thread
                  (let ((sem (sb-thread:make-semaphore))
                        (refs nil))
                    (handler-case
                        (progn
                          (sb-thread:interrupt-thread
                           object
                           (lambda ()
                             (setf refs (sb-thread::%thread-local-references))
                             (sb-vm::map-stack-references (lambda (x) (push x refs)))
                             (sb-thread:signal-semaphore sem)))
                          (sb-thread:wait-on-semaphore sem))
                      (sb-thread:interrupt-thread-error ()))
                    (mapc #'call refs))))))
        (array
         (if (simple-vector-p object)
             (dotimes (i (length object))
               (call (aref object i)))
             (when (sb-kernel:array-header-p object)
               (call (sb-kernel::%array-data-vector object))
               (call (sb-kernel::%array-displaced-p object))
               (unless simple
                 (call (sb-kernel::%array-displaced-from object))))))
        (sb-kernel:code-component
         (call (sb-kernel:%code-entry-points object))
         (call (sb-kernel:%code-debug-info object))
         (loop for i from sb-vm:code-constants-offset
               below (sb-kernel:get-header-data object)
               do (call (sb-kernel:code-header-ref object i))))
        (sb-kernel:fdefn
         (call (sb-kernel:fdefn-name object))
         (call (sb-kernel:fdefn-fun object)))
        (sb-kernel:simple-fun
         (unless simple
           (call (sb-kernel:%simple-fun-next object)))
         (call (sb-kernel:fun-code-header object))
         (call (sb-kernel:%simple-fun-name object))
         (call (sb-kernel:%simple-fun-arglist object))
         (call (sb-kernel:%simple-fun-type object))
         (call (sb-kernel:%simple-fun-info object)))
        (sb-kernel:closure
         (call (sb-kernel:%closure-fun object))
         (sb-kernel:do-closure-values (x object)
           (call x)))
        (sb-kernel:funcallable-instance
         (call (sb-kernel:%funcallable-instance-function object))
         (loop for i from 1 below (- (1+ (sb-kernel:get-closure-length object))
                                     sb-vm::funcallable-instance-info-offset)
               do (call (sb-kernel:%funcallable-instance-info object i))))
        (symbol
         (when ext
           (dolist (thread (sb-thread:list-all-threads))
             (call (sb-thread:symbol-value-in-thread object thread nil))))
         (handler-case
             ;; We don't have GLOBAL-BOUNDP, and there's no ERRORP arg.
             (call (sb-ext:symbol-global-value object))
           (unbound-variable ()))
         ;; These first two are probably unnecessary.
         ;; The functoid values, if present, are in SYMBOL-INFO
         ;; which is traversed whether or not EXT was true.
         ;; But should we traverse SYMBOL-INFO?
         ;; I don't know what is expected of this interface.
         (when (and ext (ignore-errors (fboundp object)))
           (call (fdefinition object))
           (call (macro-function object))
           (let ((class (find-class object nil)))
             (when class (call class))))
         (call (symbol-plist object)) ; perhaps SB-KERNEL:SYMBOL-INFO instead?
         (call (symbol-name object))
         (unless simple
           (call (symbol-package object))))
        (sb-kernel::random-class
         (case (sb-kernel:widetag-of object)
           (#.sb-vm::value-cell-header-widetag
            (call (sb-kernel::value-cell-ref object)))
           (t
            (warn "~&MAP-ROOT: Unknown widetag ~S: ~S~%"
                  (sb-kernel:widetag-of object) object)))))))
  object)
