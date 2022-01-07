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
  (:use "CL" "SB-KERNEL" "SB-INT")
  (:import-from "SB-VM" "PRIMITIVE-OBJECT-SIZE")
  (:shadow "VALID-FUNCTION-NAME-P")
  (:export "ALLOCATION-INFORMATION"
           "FUNCTION-ARGLIST"
           "FUNCTION-LAMBDA-LIST"
           "FUNCTION-TYPE"
           "METHOD-COMBINATION-LAMBDA-LIST"
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (system-package-p *package*) t))

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

(declaim (ftype (sfunction (function) debug-info) function-debug-info))
(defun function-debug-info (function)
  (let* ((function-object (%fun-fun function))
         (function-header (fun-code-header function-object)))
    (%code-debug-info function-header)))

(declaim (ftype (sfunction (function) debug-source) function-debug-source))
(defun function-debug-source (function)
  (debug-info-source (function-debug-info function)))

(declaim (ftype (sfunction (debug-info) debug-source) debug-info-source))
(defun debug-info-source (debug-info)
  (sb-c::debug-info-source debug-info))

(declaim (ftype (sfunction (t debug-info) debug-function) debug-info-debug-function))
(defun debug-info-debug-function (function debug-info)
  (sb-di::compiled-debug-fun-from-pc debug-info
                                     (sb-di::function-start-pc-offset function)))

(defun valid-function-name-p (name)
  "True if NAME denotes a valid function name, ie. one that can be passed to
FBOUNDP."
  (and (sb-int:valid-function-name-p name) t))

;;;; Utilities for code

(declaim (inline map-code-constants))
(defun map-code-constants (code fn)
  "Call FN for each constant in CODE's constant pool."
  (check-type code code-component)
  (loop for i from sb-vm:code-constants-offset below (code-header-words code)
     do (funcall fn (code-header-ref code i))))

(declaim (inline map-allocated-code-components))
(defun map-allocated-code-components (spaces fn)
  "Call FN for each allocated code component in one of SPACES.  FN
receives the object and its size as arguments.  SPACES should be a
list of the symbols :dynamic, :static, :read-only, or :immobile on
#+immobile-space"
  (apply #'sb-vm:map-allocated-objects
     (lambda (obj header size)
       (when (= sb-vm:code-header-widetag header)
         (funcall fn obj size)))
     spaces))

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
          (when (and (fdefn-p constant)
                     (eq (fdefn-fun constant) function))
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

(defun vops-translating-fun (name)
  (let ((fun-info (info :function :info name)))
    (when fun-info
      (sb-c::fun-info-templates fun-info))))

(defun find-vop-source (name)
  (let* ((vop (gethash name sb-c::*backend-parsed-vops*))
         (translating (vops-translating-fun name))
         (vops (if vop
                   (cons vop (remove vop translating))
                   translating)))
    (loop for vop in vops
          for vop-parse = (if (typep vop 'sb-c::vop-parse)
                              vop
                              (gethash (sb-c::vop-info-name vop)
                                       sb-c::*backend-parsed-vops*))
          for name = (sb-c::vop-parse-name vop-parse)
          for loc = (sb-c::vop-parse-source-location vop-parse)
          when loc
          collect (let ((source (translate-source-location loc)))
                    (setf (definition-source-description source)
                          (if (sb-c::vop-parse-note vop-parse)
                              (list name (sb-c::vop-parse-note vop-parse))
                              (list name)))
                    source))))

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
    (ensure-list
     (case type
       ((:variable)
        (when (and (symbolp name)
                   (member (info :variable :kind name)
                           '(:global :special :alien)))
          (translate-source-location (info :source-location type name))))
       ((:constant)
        (when (and (symbolp name)
                   (eq (info :variable :kind name) :constant))
          (translate-source-location (info :source-location type name))))
       ((:symbol-macro)
        (when (and (symbolp name)
                   (eq (info :variable :kind name) :macro))
          (translate-source-location (info :source-location type name))))
       ((:macro)
        (when (and (symbolp name)
                   (macro-function name))
          (find-definition-source (macro-function name))))
       ((:compiler-macro)
        (when (compiler-macro-function name)
          (find-definition-source (compiler-macro-function name))))
       (:ir1-convert
        (let ((converter (info :function :ir1-convert name)))
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
        (let ((loc (info :type :source-location name)))
          (if loc
              (translate-source-location loc)
              (let ((expander-fun (info :type :expander name)))
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
        (let ((expander (info :setf :expander name)))
          (cond ((typep expander '(cons symbol))
                 (translate-source-location (cddr expander)))
                (expander
                 (find-definition-source
                  (if (listp expander) (cdr expander) expander))))))
       ((:structure)
        (let ((class (get-class name)))
          (if class
              (when (typep class 'sb-pcl::structure-class)
                (find-definition-source class))
              (when (info :typed-structure :info name)
                (translate-source-location
                 (info :source-location :typed-structure name))))))
       ((:condition :class)
        (let ((class (get-class name)))
          (when (and class
                     (not (typep class 'sb-pcl::structure-class)))
            (when (eq (not (typep class 'sb-pcl::condition-class))
                      (not (eq type :condition)))
              (find-definition-source class)))))
       ((:method-combination)
        (let ((info (gethash name sb-pcl::**method-combinations**)))
          (when info
            (translate-source-location
             (sb-pcl::method-combination-info-source-location info)))))
       ((:package)
        (when (symbolp name)
          (let ((package (find-package name)))
            (when package
              (find-definition-source package)))))
       ;; TRANSFORM and OPTIMIZER handling from swank-sbcl
       ((:transform)
        (let ((fun-info (info :function :info name)))
          (when fun-info
            (loop for xform in (sb-c::fun-info-transforms fun-info)
                  for source = (find-definition-source
                                (sb-c::transform-function xform))
                  for typespec = (type-specifier
                                  (sb-c::transform-type xform))
                  for note = (sb-c::transform-note xform)
                  do (setf (definition-source-description source)
                           (if (consp typespec)
                               (list (second typespec) note)
                               (list note)))
                  collect source))))
       ((:optimizer)
        (let ((fun-info (and (symbolp name)
                             (info :function :info name))))
          (when fun-info
            (let ((otypes '((sb-c:fun-info-derive-type . sb-c:derive-type)
                            (sb-c:fun-info-ltn-annotate . sb-c:ltn-annotate)
                            (sb-c:fun-info-optimizer . sb-c:optimizer)
                            (sb-c:fun-info-ir2-convert . sb-c:ir2-convert)
                            (sb-c::fun-info-ir2-hook . sb-c::ir2-hook)
                            (sb-c::fun-info-stack-allocate-result
                             . sb-c::stack-allocate-result)
                            (sb-c::fun-info-constraint-propagate
                             . sb-c::constraint-propagate)
                            (sb-c::fun-info-constraint-propagate-if
                             . sb-c::constraint-propagate-if)
                            (sb-c::fun-info-call-type-deriver
                             . sb-c::call-type-deriver))))
              (loop for (reader . name) in otypes
                    for fn = (funcall reader fun-info)
                    when fn collect
                    (let ((source (find-definition-source fn)))
                      (setf (definition-source-description source)
                            (list name))
                      source))))))
       (:vop
        (find-vop-source name))
       (:alien-type
        (let ((loc (info :source-location type name)))
          (and loc
               (translate-source-location loc))))
       ((:source-transform)
        (let* ((transform-fun
                (or (info :function :source-transform name)
                    (and (typep name '(cons (eql setf) (cons symbol null)))
                         (info :function :source-transform
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
        (let ((locations (info :source-location :declaration name)))
          (loop for (kind loc) on locations by #'cddr
                when loc
                collect (let ((loc (translate-source-location loc)))
                          (setf (definition-source-description loc)
                                ;; Copy list to ensure that user code
                                ;; cannot mutate the original.
                                (copy-list (ensure-list kind)))
                          loc))))
       (t
        nil)))))

(defun find-definition-source (object)
  (typecase object
    ((or sb-pcl::condition-class sb-pcl::structure-class)
     (let ((classoid (sb-pcl::class-classoid object)))
       (when classoid
         (translate-source-location
          (sb-kernel::classoid-source-location classoid)))))
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
    (interpreted-function
     #+sb-eval
     (let ((source (translate-source-location
                    (sb-eval:interpreted-function-source-location object))))
       source)
     #+sb-fasteval
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
         (tlf (sb-c::compiled-debug-info-tlf-number debug-info)))
    (make-definition-source
     :pathname
     (when (stringp (debug-source-namestring debug-source))
       (parse-namestring (debug-source-namestring debug-source)))
     :character-offset
     (sb-c::compiled-debug-info-char-offset debug-info)
     :form-path (if tlf (list tlf))
     :form-number (sb-c::compiled-debug-fun-form-number debug-fun)
     :file-write-date (debug-source-created debug-source)
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

(define-deprecated-function :late "1.0.24.5" function-arglist function-lambda-list
    (function)
  (function-lambda-list function))

(defun function-lambda-list (function)
  "Return the lambda list for the extended function designator FUNCTION.
Works for special-operators, macros, simple functions, interpreted functions,
and generic functions. Signals an error if FUNCTION is not a valid extended
function designator.

If the function does not have a lambda list (compiled with debug 0),
then two values are returned: (values nil t)"
  (cond ((and (symbolp function) (special-operator-p function))
         (function-lambda-list (info :function :ir1-convert function)))
        ((valid-function-name-p function)
         (function-lambda-list (or (and (symbolp function)
                                        (macro-function function))
                                   (fdefinition function))))
        ((typep function 'generic-function)
         (sb-pcl::generic-function-pretty-arglist function))
        (t
         (let ((raw-result (%fun-lambda-list function)))
           (if (eq raw-result :unknown)
               (values nil t)
               (values raw-result nil))))))

(defun deftype-lambda-list (typespec-operator)
  "Returns the lambda list of TYPESPEC-OPERATOR as first return
value, and a flag whether the arglist could be found as second
value."
  (check-type typespec-operator symbol)
  ;; Don't return a lambda-list for combinators AND,OR,NOT.
  (let* ((f (and (info :type :kind typespec-operator)
                 (info :type :expander typespec-operator)))
         (f (if (listp f) (car f) f)))
    (if (functionp f)
        (values (%fun-lambda-list f) t)
        (values nil nil))))

(defun method-combination-lambda-list (method-combination)
  "Return the lambda-list of METHOD-COMBINATION designator.
METHOD-COMBINATION can be a method combination object,
or a method combination name."
  (let* ((name (etypecase method-combination
                 (symbol method-combination)
                 (method-combination
                  (sb-pcl::method-combination-type-name method-combination))))
         (info (or (gethash name sb-pcl::**method-combinations**)
                   (error "~S: no such method combination." name))))
    (sb-pcl::method-combination-info-lambda-list info)))

(defun function-type (function-designator)
  "Returns the ftype of FUNCTION-DESIGNATOR, or NIL."
  (etypecase function-designator
    ((or symbol cons)
     ;; XXX: why require FBOUNDP? Would it be wrong to always report the proclaimed type?
     (when (and (legal-fun-name-p function-designator) ; guarding FBOUNDP against error
                (fboundp function-designator)
                (eq (info :function :kind function-designator) :function))
       (type-specifier (global-ftype function-designator))))
    (function
     (let ((name (%fun-name function-designator)))
       (if (and (legal-fun-name-p name)
                (fboundp name)
                ;; It seems inappropriate to report the global ftype if this
                ;; function is not the current binding of the global name,
                (eq (fdefinition name) function-designator))
           ;; Give declared type in globaldb priority over derived type
           ;; because it contains more accurate information e.g. for
           ;; struct-accessors.
           (function-type name)
           (sb-impl::%fun-ftype function-designator))))))

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
  (declare (simple-fun function))
  (let ((callees '()))
    (map-code-constants
     (fun-code-header function)
     (lambda (obj)
       (when (fdefn-p obj)
         (push (fdefn-fun obj) callees))))
    callees))

(defun find-function-callers (function &optional (spaces '(:read-only :static
                                                           :dynamic
                                                           #+immobile-code :immobile)))
  "Return functions which call FUNCTION, by searching SPACES for code objects"
  (let ((referrers '()))
    (map-caller-code-components
     function
     spaces
     (lambda (code)
       (dotimes (i (code-n-entries code))
         (pushnew (%code-entry-point code i) referrers))))
    referrers))

;;; XREF facility

(defun collect-xref (wanted-kind wanted-name)
  (let ((result '()))
    (sb-c:map-simple-funs
     (lambda (name fun)
       (binding* ((xrefs (%simple-fun-xrefs fun) :exit-if-null))
         (sb-c:map-packed-xref-data
          (lambda (xref-kind xref-name xref-form-number)
            (when (and (eq xref-kind wanted-kind)
                       (equal xref-name wanted-name))
              (let ((source-location (find-function-definition-source fun)))
                ;; Use the more accurate source path from the xref
                ;; entry.
                (setf (definition-source-form-number source-location)
                      xref-form-number)
                (let ((name (cond ((sb-c::transform-p name)
                                   (append (%fun-name fun)
                                           (let* ((type (sb-c::transform-type name))
                                                  (type-spec (type-specifier type)))
                                             (and (sb-kernel:fun-type-p type)
                                                  (list (second type-spec))))))
                                  ((sb-c::vop-info-p name)
                                   (list 'sb-c:define-vop
                                         (sb-c::vop-info-name name)))
                                  (t
                                   name))))
                  (push (cons name source-location) result)))))
          xrefs))))
    result))

(defun who-calls (function-name)
  "Use the xref facility to search for source locations where the
global function named FUNCTION-NAME is called. Returns a list of
function name, definition-source pairs."
  (collect-xref :calls function-name))

(defun who-binds (symbol)
  "Use the xref facility to search for source locations where the
special variable SYMBOL is rebound. Returns a list of function name,
definition-source pairs."
  (collect-xref :binds symbol))

(defun who-references (symbol)
  "Use the xref facility to search for source locations where the
special variable or constant SYMBOL is read. Returns a list of function
name, definition-source pairs."
  (collect-xref :references symbol))

(defun who-sets (symbol)
  "Use the xref facility to search for source locations where the
special variable SYMBOL is written to. Returns a list of function name,
definition-source pairs."
  (collect-xref :sets symbol))

(defun who-macroexpands (macro-name)
  "Use the xref facility to search for source locations where the
macro MACRO-NAME is expanded. Returns a list of function name,
definition-source pairs."
  (collect-xref :macroexpands macro-name))

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
  (if (not (sb-vm:is-lisp-pointer (get-lisp-obj-address object)))
      (values :immediate nil)
      (let ((plist
             (sb-sys:with-pinned-objects (object)
               (let ((space (sb-ext:heap-allocated-p object)))
                 (when space
                   #+gencgc
                   (if (eq :dynamic space)
                       (symbol-macrolet ((page (sb-alien:deref sb-vm::page-table index)))
                         ;; No wonder #+big-endian failed introspection tests-
                         ;; bits are packed in the opposite order. And thankfully,
                         ;; this fix seems not to depend on whether the numbering
                         ;; scheme is MSB 0 or LSB 0, afaict.
                         (let* ((wp (page-protected-p object))
                                (index (sb-vm:find-page-index
                                        (get-lisp-obj-address object)))
                                (flags (sb-alien:slot page 'sb-vm::flags))
                                .
                                #+big-endian
                                ((type      (ldb (byte 5 3) flags))
                                 (dontmove  (logbitp 0 flags)))
                                #+little-endian
                                ((type      (ldb (byte 5 0) flags))
                                 (dontmove  (logbitp 7 flags))))
                           (list :space space
                                 :generation (sb-alien:slot page 'sb-vm::gen)
                                 :write-protected wp
                                 :boxed (logbitp 0 type)
                                 :pinned dontmove
                                 :large (logbitp 4 type)
                                 :page index)))
                       (list :space space))
                   #-gencgc
                   (list :space space))))))
        (cond (plist
               (values :heap plist))
              (t
               #+sb-thread
               (let ((thread (sb-ext:stack-allocated-p object t)))
                 (when thread
                   (return-from allocation-information
                     (values :stack thread))))
               #-sb-thread
               (when (sb-vm:control-stack-pointer-valid-p
                      (sb-sys:int-sap (get-lisp-obj-address object)) nil)
                 (return-from allocation-information
                   (values :stack sb-thread::*current-thread*)))
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
  (when (typep object '(or bignum float sb-sys:system-area-pointer
                           fixnum character))
    (return-from map-root object))
  (let ((fun (coerce function 'function))
        (seen (alloc-xset)))
    (flet ((call (part)
             (when (and (sb-vm:is-lisp-pointer (get-lisp-obj-address part))
                        (not (xset-member-p part seen)))
                 (add-to-xset part seen)
                 (funcall fun part))))
      (when ext
        (multiple-value-bind (value foundp)
            (let ((table sb-pcl::*eql-specializer-table*))
              (with-system-mutex ((hash-table-lock table))
                (gethash object table)))
          (when foundp (call value))))
      (sb-vm:do-referenced-object (object call)
        (cons
         :extend
         (when (and ext (ignore-errors (fboundp object)))
           (call (fdefinition object))))
        (instance
         :extend
         #+sb-thread
         (when (typep object 'sb-thread:thread)
           (cond ((eq object sb-thread:*current-thread*)
                  (dolist (value (sb-thread::%thread-local-references))
                    (call value))
                  (sb-vm::map-stack-references #'call))
                 (t
                  ;; KLUDGE: INTERRUPT-THREAD is Not Nice (tm), but
                  ;; the alternative would be stopping the world...
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
                    ;; This is whacky - the other thread signals our condition var,
                    ;; *then* we call the funarg on objects that may no longer
                    ;; satisfy VALID-LISP-POINTER-P.
                    ;; And incidentally, we miss any references from TLS indices
                    ;; that map onto the 'struct thread', which is just as well
                    ;; since they're either fixnums or dynamic-extent objects.
                    (mapc #'call refs))))))
        ((satisfies array-header-p)
         :override
         ;; The default implementation always scans %array-displaced-from
         (call (%array-data object))
         (call (%array-displaced-p object))
         (unless simple
           (call (%array-displaced-from object))))
        (code-component
         :extend
         (loop for i below (code-n-entries object)
               do (call (%code-entry-point object i))))
        (function ; excluding CLOSURE and FUNCALLABLE-INSTANCE
         :override
         (unless simple
           (call (fun-code-header object)))
         (call (%simple-fun-name object))
         (call (%simple-fun-arglist object))
         (call (%simple-fun-source object))
         (call (%simple-fun-info object)))
        (symbol
         ;; We use :override here because (apparently) the intent is
         ;; to avoid calling FUNCTION on the SYMBOL-PACKAGE
         ;; when SIMPLE is NIL (the default). And we skip SYMBOL-EXTRA for
         ;; the same reason that we don't call FUNCTION on SYMBOL-INFO
         ;; (logically it's "system" data, not for user consumption).
         ;; Frankly this entire function is a confusing mishmash that is not
         ;; accurate for computing a true graph of objects starting from a
         ;; certain point, given all the special cases that it implements.
         :override
         (when ext
           (dolist (thread (sb-thread:list-all-threads))
             (call (sb-thread:symbol-value-in-thread object thread nil))))
         (call (sb-sys:%primitive sb-c:fast-symbol-global-value object))
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
        (t
         :extend
         (case (widetag-of object)
           (#.sb-vm:value-cell-widetag
            (call (value-cell-ref object)))
           (t
            (warn "~&MAP-ROOT: Unknown widetag ~S: ~S~%"
                  (widetag-of object) object)))))))
  object)

(defun object-size (object)
  (+ (primitive-object-size object)
     (typecase object
       (sb-mop:funcallable-standard-object
        (primitive-object-size (sb-pcl::fsc-instance-slots object)))
       (standard-object
        (primitive-object-size (sb-pcl::std-instance-slots object)))
       (t 0))))

;;; Print a distribution of object sizes in SPACE.
;;; There are two bins for cons-sized objects: conses and anything else,
;;; the latter including SAPs, value cells, 0-length simple-vectors,
;;; and a bunch of other things.
(defun object-size-histogram (&optional
                              (space :dynamic)
                              (size-bins ; objects whose size in words is <= this
                               `#(2 4 6 8 10 16 20 24 32 64 128 256 512 1024
                                  2048 4096 8192 16384 32768 131072 524288
                                  ,(ash 1 20) ,(ash 1 21) ,(ash 1 23))))
  (declare (simple-vector size-bins))
  (let* ((n-bins (+ (length size-bins) 2))
         (counts (make-array n-bins :initial-element 0))
         (size-totals (make-array n-bins :initial-element 0)))
    (sb-vm:map-allocated-objects
     (lambda (obj type size)
       (declare (ignore type))
       (cond ((consp obj)
              (incf (aref counts 0)))
             (t
              (let* ((words (ash size (- sb-vm:word-shift)))
                     (bin
                      (let ((i (position words size-bins :test #'<=)))
                        (if i (1+ i) (1- n-bins)))))
                (incf (aref counts bin))
                (incf (aref size-totals bin) words)))))
     space)
    (format t "     Freq     Tot Words~% =========    =========~%")
    (dotimes (i n-bins)
      (format t " ~9d  ~11d  ~a~%"
              (aref counts i)
              (if (eql i 0) ; cons bin
                  (* 2 (aref counts i))
                  (aref size-totals i))
              (cond ((zerop i) "cons")
                    ((eql i (1- n-bins))
                     (format nil " > ~D" (aref size-bins (- n-bins 3))))
                    (t
                     (let ((this-bin-size (aref size-bins (1- i)))
                           (prev-bin-size (when (>= i 2) (aref size-bins (- i 2)))))
                       (format nil "~:[<=~;=~] ~D"
                               (or (not prev-bin-size)
                                   (= this-bin-size (+ prev-bin-size 2)))
                               this-bin-size))))))))

(defun largest-objects (&key (threshold #+gencgc sb-vm:gencgc-page-bytes
                                        #-gencgc sb-c:+backend-page-bytes+)
                             (sort :size))
  (declare (type (member :address :size) sort))
  (flet ((show-obj (obj)
           #-gencgc
           (format t "~10x ~7x ~s~%"
                     (get-lisp-obj-address obj)
                     (primitive-object-size obj)
                     (type-of obj))
           #+gencgc
           (let* ((gen (generation-of obj))
                  (page (sb-vm::find-page-index (sb-kernel:get-lisp-obj-address obj)))
                  (flags (if (>= page 0)
                             (sb-alien:slot (sb-alien:deref sb-vm:page-table page)
                                            'sb-vm::flags))))
             (format t "~10x ~7x ~a ~:[        ~;~:*~8b~] ~s~%"
                     (get-lisp-obj-address obj)
                     (primitive-object-size obj)
                     (if gen gen #\?)
                     flags
                     (type-of obj)))))
    (case sort
     (:address
      (sb-vm:map-allocated-objects
       (lambda (obj widetag size)
         (declare (ignore widetag))
         (when (>= size threshold)
           (show-obj obj)))
       :all))
     (:size
      (let (list)
         (sb-vm:map-allocated-objects
          (lambda (obj widetag size)
            (declare (ignore widetag))
            (when (>= size threshold)
              (push obj list)))
          :all)
         (mapc #'show-obj
               (stable-sort list #'> :key #'primitive-object-size))
         nil)))))
