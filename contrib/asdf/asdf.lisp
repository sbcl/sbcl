;;; This is ASDF 3.3.1
(eval-when (:compile-toplevel :load-toplevel :execute) (require :uiop))
;;;; -------------------------------------------------------------------------
;;;; Handle upgrade as forward- and backward-compatibly as possible
;; See https://bugs.launchpad.net/asdf/+bug/485687

(uiop/package:define-package :asdf/upgrade
  (:recycle :asdf/upgrade :asdf)
  (:use :uiop/common-lisp :uiop)
  (:export
   #:asdf-version #:*previous-asdf-versions* #:*asdf-version*
   #:asdf-message #:*verbose-out*
   #:upgrading-p #:when-upgrading #:upgrade-asdf #:defparameter*
   #:*post-upgrade-cleanup-hook* #:cleanup-upgraded-asdf
   ;; There will be no symbol left behind!
   #:with-asdf-deprecation
   #:intern*)
  (:import-from :uiop/package #:intern* #:find-symbol*))
(in-package :asdf/upgrade)

;;; Special magic to detect if this is an upgrade

(with-upgradability ()
  (defun asdf-version ()
    "Exported interface to the version of ASDF currently installed. A string.
You can compare this string with e.g.: (ASDF:VERSION-SATISFIES (ASDF:ASDF-VERSION) \"3.4.5.67\")."
    (when (find-package :asdf)
      (or (symbol-value (find-symbol (string :*asdf-version*) :asdf))
          (let* ((revsym (find-symbol (string :*asdf-revision*) :asdf))
                 (rev (and revsym (boundp revsym) (symbol-value revsym))))
            (etypecase rev
              (string rev)
              (cons (format nil "~{~D~^.~}" rev))
              (null "1.0"))))))
  ;; This (private) variable contains a list of versions of previously loaded variants of ASDF,
  ;; from which ASDF was upgraded.
  ;; Important: define *p-a-v* /before/ *a-v* so that they initialize correctly.
  (defvar *previous-asdf-versions*
    (let ((previous (asdf-version)))
      (when previous
        ;; Punt on upgrade from ASDF1 or ASDF2, by renaming (or deleting) the package.
        (when (version< previous "2.27") ;; 2.27 is the first to have the :asdf3 feature.
          (let ((away (format nil "~A-~A" :asdf previous)))
            (rename-package :asdf away)
            (when *load-verbose*
              (format t "~&; Renamed old ~A package away to ~A~%" :asdf away))))
        (list previous))))
  ;; This public variable will be bound shortly to the currently loaded version of ASDF.
  (defvar *asdf-version* nil)
  ;; We need to clear systems from versions older than the one in this (private) parameter.
  ;; The latest incompatible defclass is 2.32.13 renaming a slot in component,
  ;; or 3.2.0.2 for CCL (incompatibly changing some superclasses).
  ;; the latest incompatible gf change is in 3.1.7.20 (see redefined-functions below).
  (defparameter *oldest-forward-compatible-asdf-version* "3.2.0.2")
  ;; Semi-private variable: a designator for a stream on which to output ASDF progress messages
  (defvar *verbose-out* nil)
  ;; Private function by which ASDF outputs progress messages and warning messages:
  (defun asdf-message (format-string &rest format-args)
    (when *verbose-out* (apply 'format *verbose-out* format-string format-args)))
  ;; Private hook for functions to run after ASDF has upgraded itself from an older variant:
  (defvar *post-upgrade-cleanup-hook* ())
  ;; Private function to detect whether the current upgrade counts as an incompatible
  ;; data schema upgrade implying the need to drop data.
  (defun upgrading-p (&optional (oldest-compatible-version *oldest-forward-compatible-asdf-version*))
    (and *previous-asdf-versions*
         (version< (first *previous-asdf-versions*) oldest-compatible-version)))
  ;; Private variant of defparameter that works in presence of incompatible upgrades:
  ;; behaves like defvar in a compatible upgrade (e.g. reloading system after simple code change),
  ;; but behaves like defparameter if in presence of an incompatible upgrade.
  (defmacro defparameter* (var value &optional docstring (version *oldest-forward-compatible-asdf-version*))
    (let* ((name (string-trim "*" var))
           (valfun (intern (format nil "%~A-~A-~A" :compute name :value))))
      `(progn
         (defun ,valfun () ,value)
         (defvar ,var (,valfun) ,@(ensure-list docstring))
         (when (upgrading-p ,version)
           (setf ,var (,valfun))))))
  ;; Private macro to declare sections of code that are only compiled and run when upgrading.
  ;; The use of eval portably ensures that the code will not have adverse compile-time side-effects,
  ;; whereas the use of handler-bind portably ensures that it will not issue warnings when it runs.
  (defmacro when-upgrading ((&key (version *oldest-forward-compatible-asdf-version*)
                               (upgrading-p `(upgrading-p ,version)) when) &body body)
    "A wrapper macro for code that should only be run when upgrading a
previously-loaded version of ASDF."
    `(with-upgradability ()
       (when (and ,upgrading-p ,@(when when `(,when)))
         (handler-bind ((style-warning #'muffle-warning))
           (eval '(progn ,@body))))))
  ;; Only now can we safely update the version.
  (let* (;; For bug reporting sanity, please always bump this version when you modify this file.
         ;; Please also modify asdf.asd to reflect this change. make bump-version v=3.4.5.67.8
         ;; can help you do these changes in synch (look at the source for documentation).
         ;; Relying on its automation, the version is now redundantly present on top of asdf.lisp.
         ;; "3.4" would be the general branch for major version 3, minor version 4.
         ;; "3.4.5" would be an official release in the 3.4 branch.
         ;; "3.4.5.67" would be a development version in the official branch, on top of 3.4.5.
         ;; "3.4.5.0.8" would be your eighth local modification of official release 3.4.5
         ;; "3.4.5.67.8" would be your eighth local modification of development version 3.4.5.67
         (asdf-version "3.3.1")
         (existing-version (asdf-version)))
    (setf *asdf-version* asdf-version)
    (when (and existing-version (not (equal asdf-version existing-version)))
      (push existing-version *previous-asdf-versions*)
      (when (or *verbose-out* *load-verbose*)
        (format (or *verbose-out* *trace-output*)
                (compatfmt "~&~@<; ~@;Upgrading ASDF ~@[from version ~A ~]to version ~A~@:>~%")
                existing-version asdf-version)))))

;;; Upon upgrade, specially frob some functions and classes that are being incompatibly redefined
(when-upgrading ()
  (let* ((previous-version (first *previous-asdf-versions*))
         (redefined-functions ;; List of functions that changes incompatibly since 2.27:
          ;; gf signature changed (should NOT happen), defun that became a generic function,
          ;; method removed that will mess up with new ones (especially :around :before :after,
          ;; more specific or call-next-method'ed method) and/or semantics otherwise modified. Oops.
          ;; NB: it's too late to do anything about functions in UIOP!
          ;; If you introduce some critical incompatibility there, you must change the function name.
          ;; Note that we don't need do anything about functions that changed incompatibly
          ;; from ASDF 2.26 or earlier: we wholly punt on the entire ASDF package in such an upgrade.
          ;; Also note that we don't include the defgeneric=>defun, because they are
          ;; done directly with defun* and need not trigger a punt on data.
          ;; See discussion at https://gitlab.common-lisp.net/asdf/asdf/merge_requests/36
          `(,@(when (version<= previous-version "3.1.2") '(#:component-depends-on #:input-files)) ;; crucial methods *removed* before 3.1.2
            ,@(when (version<= previous-version "3.1.7.20") '(#:find-component))))
         (redefined-classes
          ;; redefining the classes causes interim circularities
          ;; with the old ASDF during upgrade, and many implementations bork
          #-clozure ()
          #+clozure
          '((#:compile-concatenated-source-op (#:operation) ())
            (#:compile-bundle-op (#:operation) ())
            (#:concatenate-source-op (#:operation) ())
            (#:dll-op (#:operation) ())
            (#:lib-op (#:operation) ())
            (#:monolithic-compile-bundle-op (#:operation) ())
            (#:monolithic-concatenate-source-op (#:operation) ()))))
    (loop :for name :in redefined-functions
      :for sym = (find-symbol* name :asdf nil)
      :do (when sym (fmakunbound sym)))
    (labels ((asym (x) (multiple-value-bind (s p)
                           (if (consp x) (values (car x) (cadr x)) (values x :asdf))
                         (find-symbol* s p nil)))
             (asyms (l) (mapcar #'asym l)))
      (loop* :for (name superclasses slots) :in redefined-classes
             :for sym = (find-symbol* name :asdf nil)
             :when (and sym (find-class sym))
             :do (eval `(defclass ,sym ,(asyms superclasses) ,(asyms slots)))))))


;;; Self-upgrade functions
(with-upgradability ()
  ;; This private function is called at the end of asdf/footer and ensures that,
  ;; *if* this loading of ASDF was an upgrade, then all registered cleanup functions will be called.
  (defun cleanup-upgraded-asdf (&optional (old-version (first *previous-asdf-versions*)))
    (let ((new-version (asdf-version)))
      (unless (equal old-version new-version)
        (push new-version *previous-asdf-versions*)
        (when old-version
          (if (version<= new-version old-version)
              (error (compatfmt "~&~@<; ~@;Downgraded ASDF from version ~A to version ~A~@:>~%")
                     old-version new-version)
              (asdf-message (compatfmt "~&~@<; ~@;Upgraded ASDF from version ~A to version ~A~@:>~%")
                            old-version new-version))
          ;; In case the previous version was too old to be forward-compatible, clear systems.
          ;; TODO: if needed, we may have to define a separate hook to run
          ;; in case of forward-compatible upgrade.
          ;; Or to move the tests forward-compatibility test inside each hook function?
          (unless (version<= *oldest-forward-compatible-asdf-version* old-version)
            (call-functions (reverse *post-upgrade-cleanup-hook*)))
          t))))

  (defun upgrade-asdf ()
    "Try to upgrade of ASDF. If a different version was used, return T.
   We need do that before we operate on anything that may possibly depend on ASDF."
    (let ((*load-print* nil)
          (*compile-print* nil))
      (handler-bind (((or style-warning) #'muffle-warning))
        (symbol-call :asdf :load-system :asdf :verbose nil))))

  (defmacro with-asdf-deprecation ((&rest keys &key &allow-other-keys) &body body)
    `(with-upgradability ()
       (with-deprecation ((version-deprecation *asdf-version* ,@keys))
         ,@body))))
;;;; -------------------------------------------------------------------------
;;;; Session

(uiop/package:define-package :asdf/session
  (:recycle :asdf/session :asdf/cache :asdf/component
            :asdf/action :asdf/find-system :asdf/plan :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade)
  (:export
   #:get-file-stamp #:compute-file-stamp #:register-file-stamp
   #:asdf-cache #:set-asdf-cache-entry #:unset-asdf-cache-entry #:consult-asdf-cache
   #:do-asdf-cache #:normalize-namestring
   #:call-with-asdf-session #:with-asdf-session
   #:*asdf-session* #:*asdf-session-class* #:session #:toplevel-asdf-session
   #:session-cache #:forcing #:asdf-upgraded-p
   #:visited-actions #:visiting-action-set #:visiting-action-list
   #:total-action-count #:planned-action-count #:planned-output-action-count
   #:clear-configuration-and-retry #:retry
   #:operate-level
   ;; conditions
   #:system-definition-error ;; top level, moved here because this is the earliest place for it.
   #:formatted-system-definition-error #:format-control #:format-arguments #:sysdef-error))
(in-package :asdf/session)


(with-upgradability ()
  ;; The session variable.
  ;; NIL when outside a session.
  (defvar *asdf-session* nil)
  (defparameter* *asdf-session-class* 'session
    "The default class for sessions")

  (defclass session ()
    (;; The ASDF session cache is used to memoize some computations.
     ;; It is instrumental in achieving:
     ;; * Consistency in the view of the world relied on by ASDF within a given session.
     ;;   Inconsistencies in file stamps, system definitions, etc., could cause infinite loops
     ;;   (a.k.a. stack overflows) and other erratic behavior.
     ;; * Speed and reliability of ASDF, with fewer side-effects from access to the filesystem, and
     ;;   no expensive recomputations of transitive dependencies for input-files or output-files.
     ;; * Testability of ASDF with the ability to fake timestamps without actually touching files.
     (ancestor
      :initform nil :initarg :ancestor :reader session-ancestor
      :documentation "Top level session that this is part of")
     (session-cache
      :initform (make-hash-table :test 'equal) :initarg :session-cache :reader session-cache
      :documentation "Memoize expensive computations")
     (operate-level
      :initform 0 :initarg :operate-level :accessor session-operate-level
      :documentation "Number of nested calls to operate we're under (for toplevel session only)")
     ;; shouldn't the below be superseded by the session-wide caching of action-status
     ;; for (load-op "asdf") ?
     (asdf-upgraded-p
      :initform nil :initarg :asdf-upgraded-p :accessor asdf-upgraded-p
      :documentation "Was ASDF already upgraded in this session - only valid for toplevel-asdf-session.")
     (forcing
      :initform nil :initarg :forcing :accessor forcing
      :documentation "Forcing parameters for the session")
     ;; Table that to actions already visited while walking the dependencies associates status
     (visited-actions :initform (make-hash-table :test 'equal) :accessor visited-actions)
     ;; Actions that depend on those being currently walked through, to detect circularities
     (visiting-action-set ;; as a set
      :initform (make-hash-table :test 'equal) :accessor visiting-action-set)
     (visiting-action-list :initform () :accessor visiting-action-list) ;; as a list
     ;; Counts of total actions in plan
     (total-action-count :initform 0 :accessor total-action-count)
     ;; Count of actions that need to be performed
     (planned-action-count :initform 0 :accessor planned-action-count)
     ;; Count of actions that need to be performed that have a non-empty list of output-files.
     (planned-output-action-count :initform 0 :accessor planned-output-action-count))
    (:documentation "An ASDF session with a cache to memoize some computations"))

  (defun toplevel-asdf-session ()
    (when *asdf-session* (or (session-ancestor *asdf-session*) *asdf-session*)))

  (defun operate-level ()
    (session-operate-level (toplevel-asdf-session)))

  (defun (setf operate-level) (new-level)
    (setf (session-operate-level (toplevel-asdf-session)) new-level))

  (defun asdf-cache ()
    (session-cache *asdf-session*))

  ;; Set a session cache entry for KEY to a list of values VALUE-LIST, when inside a session.
  ;; Return those values.
  (defun set-asdf-cache-entry (key value-list)
    (values-list (if *asdf-session*
                     (setf (gethash key (asdf-cache)) value-list)
                     value-list)))

  ;; Unset the session cache entry for KEY, when inside a session.
  (defun unset-asdf-cache-entry (key)
    (when *asdf-session*
      (remhash key (session-cache *asdf-session*))))

  ;; Consult the session cache entry for KEY if present and in a session;
  ;; if not present, compute it by calling the THUNK,
  ;; and set the session cache entry accordingly, if in a session.
  ;; Return the values from the cache and/or the thunk computation.
  (defun consult-asdf-cache (key &optional thunk)
    (if *asdf-session*
        (multiple-value-bind (results foundp) (gethash key (session-cache *asdf-session*))
          (if foundp
              (values-list results)
              (set-asdf-cache-entry key (multiple-value-list (call-function thunk)))))
        (call-function thunk)))

  ;; Syntactic sugar for consult-asdf-cache
  (defmacro do-asdf-cache (key &body body)
    `(consult-asdf-cache ,key #'(lambda () ,@body)))

  ;; Compute inside a ASDF session with a cache.
  ;; First, make sure an ASDF session is underway, by binding the session cache variable
  ;; to a new hash-table if it's currently null (or even if it isn't, if OVERRIDE is true).
  ;; Second, if a new session was started, establish restarts for retrying the overall computation.
  ;; Finally, consult the cache if a KEY was specified with the THUNK as a fallback when the cache
  ;; entry isn't found, or just call the THUNK if no KEY was specified.
  (defun call-with-asdf-session (thunk &key override key override-cache override-forcing)
    (let ((fun (if key #'(lambda () (consult-asdf-cache key thunk)) thunk)))
      (if (and (not override) *asdf-session*)
          (funcall fun)
          (loop
            (restart-case
                (let ((*asdf-session*
                       (apply 'make-instance *asdf-session-class*
                              (when *asdf-session*
                                `(:ancestor ,(toplevel-asdf-session)
                                  ,@(unless override-forcing
                                      `(:forcing ,(forcing *asdf-session*)))
                                  ,@(unless override-cache
                                      `(:session-cache ,(session-cache *asdf-session*))))))))
                  (return (funcall fun)))
              (retry ()
                :report (lambda (s)
                          (format s (compatfmt "~@<Retry ASDF operation.~@:>"))))
              (clear-configuration-and-retry ()
                :report (lambda (s)
                          (format s (compatfmt "~@<Retry ASDF operation after resetting the configuration.~@:>")))
                (clrhash (session-cache *asdf-session*))
                (clear-configuration)))))))

  ;; Syntactic sugar for call-with-asdf-session
  (defmacro with-asdf-session ((&key key override override-cache override-forcing) &body body)
    `(call-with-asdf-session
      #'(lambda () ,@body)
      :override ,override :key ,key
      :override-cache ,override-cache :override-forcing ,override-forcing))


  ;;; Define specific accessor for file (date) stamp.

  ;; Normalize a namestring for use as a key in the session cache.
  (defun normalize-namestring (pathname)
    (let ((resolved (resolve-symlinks*
                     (ensure-absolute-pathname
                      (physicalize-pathname pathname)
                      'get-pathname-defaults))))
      (with-pathname-defaults () (namestring resolved))))

  ;; Compute the file stamp for a normalized namestring
  (defun compute-file-stamp (normalized-namestring)
    (with-pathname-defaults ()
      (or (safe-file-write-date normalized-namestring) t)))

  ;; Override the time STAMP associated to a given FILE in the session cache.
  ;; If no STAMP is specified, recompute a new one from the filesystem.
  (defun register-file-stamp (file &optional (stamp nil stampp))
    (let* ((namestring (normalize-namestring file))
           (stamp (if stampp stamp (compute-file-stamp namestring))))
      (set-asdf-cache-entry `(get-file-stamp ,namestring) (list stamp))))

  ;; Get or compute a memoized stamp for given FILE from the session cache.
  (defun get-file-stamp (file)
    (when file
      (let ((namestring (normalize-namestring file)))
        (do-asdf-cache `(get-file-stamp ,namestring) (compute-file-stamp namestring)))))


  ;;; Conditions

  (define-condition system-definition-error (error) ()
    ;; [this use of :report should be redundant, but unfortunately it's not.
    ;; cmucl's lisp::output-instance prefers the kernel:slot-class-print-function
    ;; over print-object; this is always conditions::%print-condition for
    ;; condition objects, which in turn does inheritance of :report options at
    ;; run-time.  fortunately, inheritance means we only need this kludge here in
    ;; order to fix all conditions that build on it.  -- rgr, 28-Jul-02.]
    #+cmucl (:report print-object))

  (define-condition formatted-system-definition-error (system-definition-error)
    ((format-control :initarg :format-control :reader format-control)
     (format-arguments :initarg :format-arguments :reader format-arguments))
    (:report (lambda (c s)
               (apply 'format s (format-control c) (format-arguments c)))))

  (defun sysdef-error (format &rest arguments)
    (error 'formatted-system-definition-error :format-control
           format :format-arguments arguments)))
;;;; -------------------------------------------------------------------------
;;;; Components

(uiop/package:define-package :asdf/component
  (:recycle :asdf/component :asdf/find-component :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/session)
  (:export
   #:component #:component-find-path
   #:find-component ;; methods defined in find-component
   #:component-name #:component-pathname #:component-relative-pathname
   #:component-parent #:component-system #:component-parent-pathname
   #:child-component #:parent-component #:module
   #:file-component
   #:source-file #:c-source-file #:java-source-file
   #:static-file #:doc-file #:html-file
   #:file-type
   #:source-file-type #:source-file-explicit-type ;; backward-compatibility
   #:component-in-order-to #:component-sideway-dependencies
   #:component-if-feature #:around-compile-hook
   #:component-description #:component-long-description
   #:component-version #:version-satisfies
   #:component-inline-methods ;; backward-compatibility only. DO NOT USE!
   #:component-operation-times ;; For internal use only.
   ;; portable ASDF encoding and implementation-specific external-format
   #:component-external-format #:component-encoding
   #:component-children-by-name #:component-children #:compute-children-by-name
   #:component-build-operation
   #:module-default-component-class
   #:module-components ;; backward-compatibility. DO NOT USE.
   #:sub-components

   ;; conditions
   #:duplicate-names

   ;; Internals we'd like to share with the ASDF package, especially for upgrade purposes
   #:name #:version #:description #:long-description #:author #:maintainer #:licence
   #:components-by-name #:components #:children #:children-by-name
   #:default-component-class #:source-file
   #:defsystem-depends-on ; This symbol retained for backward compatibility.
   #:sideway-dependencies #:if-feature #:in-order-to #:inline-methods
   #:relative-pathname #:absolute-pathname #:operation-times #:around-compile
   #:%encoding #:properties #:component-properties #:parent))
(in-package :asdf/component)

(with-upgradability ()
  (defgeneric component-name (component)
    (:documentation "Name of the COMPONENT, unique relative to its parent"))
  (defgeneric component-system (component)
    (:documentation "Top-level system containing the COMPONENT"))
  (defgeneric component-pathname (component)
    (:documentation "Pathname of the COMPONENT if any, or NIL."))
  (defgeneric component-relative-pathname (component)
    ;; in ASDF4, rename that to component-specified-pathname ?
    (:documentation "Specified pathname of the COMPONENT,
intended to be merged with the pathname of that component's parent if any, using merged-pathnames*.
Despite the function's name, the return value can be an absolute pathname, in which case the merge
will leave it unmodified."))
  (defgeneric component-external-format (component)
    (:documentation "The external-format of the COMPONENT.
By default, deduced from the COMPONENT-ENCODING."))
  (defgeneric component-encoding (component)
    (:documentation "The encoding of the COMPONENT. By default, only :utf-8 is supported.
Use asdf-encodings to support more encodings."))
  (defgeneric version-satisfies (component version)
    (:documentation "Check whether a COMPONENT satisfies the constraint of being at least as recent
as the specified VERSION, which must be a string of dot-separated natural numbers, or NIL."))
  (defgeneric component-version (component)
    (:documentation "Return the version of a COMPONENT, which must be a string of dot-separated
natural numbers, or NIL."))
  (defgeneric (setf component-version) (new-version component)
    (:documentation "Updates the version of a COMPONENT, which must be a string of dot-separated
natural numbers, or NIL."))
  (defgeneric component-parent (component)
    (:documentation "The parent of a child COMPONENT,
or NIL for top-level components (a.k.a. systems)"))
  ;; NIL is a designator for the absence of a component, in which case the parent is also absent.
  (defmethod component-parent ((component null)) nil)

  ;; Deprecated: Backward compatible way of computing the FILE-TYPE of a component.
  ;; TODO: find users, have them stop using that, remove it for ASDF4.
  (defgeneric source-file-type (component system)
    (:documentation "DEPRECATED. Use the FILE-TYPE of a COMPONENT instead."))

  (define-condition duplicate-names (system-definition-error)
    ((name :initarg :name :reader duplicate-names-name))
    (:report (lambda (c s)
               (format s (compatfmt "~@<Error while defining system: multiple components are given same name ~S~@:>")
                       (duplicate-names-name c))))))


(with-upgradability ()
  (defclass component ()
    ((name :accessor component-name :initarg :name :type string :documentation
           "Component name: designator for a string composed of portable pathname characters")
     ;; We might want to constrain version with
     ;; :type (and string (satisfies parse-version))
     ;; but we cannot until we fix all systems that don't use it correctly!
     (version :accessor component-version :initarg :version :initform nil)
     (description :accessor component-description :initarg :description :initform nil)
     (long-description :accessor component-long-description :initarg :long-description :initform nil)
     (sideway-dependencies :accessor component-sideway-dependencies :initform nil)
     (if-feature :accessor component-if-feature :initform nil :initarg :if-feature)
     ;; In the ASDF object model, dependencies exist between *actions*,
     ;; where an action is a pair of an operation and a component.
     ;; Dependencies are represented as alists of operations
     ;; to a list where each entry is a pair of an operation and a list of component specifiers.
     ;; Up until ASDF 2.26.9, there used to be two kinds of dependencies:
     ;; in-order-to and do-first, each stored in its own slot. Now there is only in-order-to.
     ;; in-order-to used to represent things that modify the filesystem (such as compiling a fasl)
     ;; and do-first things that modify the current image (such as loading a fasl).
     ;; These are now unified because we now correctly propagate timestamps between dependencies.
     ;; Happily, no one seems to have used do-first too much (especially since until ASDF 2.017,
     ;; anything you specified was overridden by ASDF itself anyway), but the name in-order-to remains.
     ;; The names are bad, but they have been the official API since Dan Barlow's ASDF 1.52!
     ;; LispWorks's defsystem has caused-by and requires for in-order-to and do-first respectively.
     ;; Maybe rename the slots in ASDF? But that's not very backward-compatible.
     ;; See our ASDF 2 paper for more complete explanations.
     (in-order-to :initform nil :initarg :in-order-to
                  :accessor component-in-order-to)
     ;; Methods defined using the "inline" style inside a defsystem form:
     ;; we store them here so we can delete them when the system is re-evaluated.
     (inline-methods :accessor component-inline-methods :initform nil)
     ;; ASDF4: rename it from relative-pathname to specified-pathname. It need not be relative.
     ;; There is no initform and no direct accessor for this specified pathname,
     ;; so we only access the information through appropriate methods, after it has been processed.
     ;; Unhappily, some braindead systems directly access the slot. Make them stop before ASDF4.
     (relative-pathname :initarg :pathname)
     ;; The absolute-pathname is computed based on relative-pathname and parent pathname.
     ;; The slot is but a cache used by component-pathname.
     (absolute-pathname)
     (operation-times :initform (make-hash-table)
                      :accessor component-operation-times)
     (around-compile :initarg :around-compile)
     ;; Properties are for backward-compatibility with ASDF2 only. DO NOT USE!
     (properties :accessor component-properties :initarg :properties
                 :initform nil)
     (%encoding :accessor %component-encoding :initform nil :initarg :encoding)
     ;; For backward-compatibility, this slot is part of component rather than of child-component. ASDF4: stop it.
     (parent :initarg :parent :initform nil :reader component-parent)
     (build-operation
      :initarg :build-operation :initform nil :reader component-build-operation)
     ;; Cache for ADDITIONAL-INPUT-FILES function.
     (additional-input-files :accessor %additional-input-files :initform nil))
    (:documentation "Base class for all components of a build"))

  (defgeneric find-component (base path &key registered)
    (:documentation "Find a component by resolving the PATH starting from BASE parent.
If REGISTERED is true, only search currently registered systems."))

  (defun component-find-path (component)
    "Return a path from a root system to the COMPONENT.
The return value is a list of component NAMES; a list of strings."
    (check-type component (or null component))
    (reverse
     (loop :for c = component :then (component-parent c)
           :while c :collect (component-name c))))

  (defmethod print-object ((c component) stream)
    (print-unreadable-object (c stream :type t :identity nil)
      (format stream "~{~S~^ ~}" (component-find-path c))))

  (defmethod component-system ((component component))
    (if-let (system (component-parent component))
      (component-system system)
      component)))


;;;; Component hierarchy within a system
;; The tree typically but not necessarily follows the filesystem hierarchy.
(with-upgradability ()
  (defclass child-component (component) ()
    (:documentation "A CHILD-COMPONENT is a COMPONENT that may be part of
a PARENT-COMPONENT."))

  (defclass file-component (child-component)
    ((type :accessor file-type :initarg :type)) ; no default
    (:documentation "a COMPONENT that represents a file"))
  (defclass source-file (file-component)
    ((type :accessor source-file-explicit-type ;; backward-compatibility
           :initform nil))) ;; NB: many systems have come to rely on this default.
  (defclass c-source-file (source-file)
    ((type :initform "c")))
  (defclass java-source-file (source-file)
    ((type :initform "java")))
  (defclass static-file (source-file)
    ((type :initform nil))
    (:documentation "Component for a file to be included as is in the build output"))
  (defclass doc-file (static-file) ())
  (defclass html-file (doc-file)
    ((type :initform "html")))

  (defclass parent-component (component)
    ((children
      :initform nil
      :initarg :components
      :reader module-components ; backward-compatibility
      :accessor component-children)
     (children-by-name
      :reader module-components-by-name ; backward-compatibility
      :accessor component-children-by-name)
     (default-component-class
      :initform nil
      :initarg :default-component-class
      :accessor module-default-component-class))
  (:documentation "A PARENT-COMPONENT is a component that may have children.")))

(with-upgradability ()
  ;; (Private) Function that given a PARENT component,
  ;; the list of children of which has been initialized,
  ;; compute the hash-table in slot children-by-name that allows to retrieve its children by name.
  ;; If ONLY-IF-NEEDED-P is defined, skip any (re)computation if the slot is already populated.
  (defun compute-children-by-name (parent &key only-if-needed-p)
    (unless (and only-if-needed-p (slot-boundp parent 'children-by-name))
      (let ((hash (make-hash-table :test 'equal)))
        (setf (component-children-by-name parent) hash)
        (loop :for c :in (component-children parent)
              :for name = (component-name c)
              :for previous = (gethash name hash)
              :do (when previous (error 'duplicate-names :name name))
                  (setf (gethash name hash) c))
        hash))))

(with-upgradability ()
  (defclass module (child-component parent-component)
    (#+clisp (components)) ;; backward compatibility during upgrade only
    (:documentation "A module is a intermediate component with both a parent and children,
typically but not necessarily representing the files in a subdirectory of the build source.")))


;;;; component pathnames
(with-upgradability ()
  (defgeneric component-parent-pathname (component)
    (:documentation "The pathname of the COMPONENT's parent, if any, or NIL"))
  (defmethod component-parent-pathname (component)
    (component-pathname (component-parent component)))

  ;; The default method for component-pathname tries to extract a cached precomputed
  ;; absolute-pathname from the relevant slot, and if not, computes it by merging the
  ;; component-relative-pathname (which should be component-specified-pathname, it can be absolute)
  ;; with the directory of the component-parent-pathname.
  (defmethod component-pathname ((component component))
    (if (slot-boundp component 'absolute-pathname)
        (slot-value component 'absolute-pathname)
        (let ((pathname
                (merge-pathnames*
                 (component-relative-pathname component)
                 (pathname-directory-pathname (component-parent-pathname component)))))
          (unless (or (null pathname) (absolute-pathname-p pathname))
            (error (compatfmt "~@<Invalid relative pathname ~S for component ~S~@:>")
                   pathname (component-find-path component)))
          (setf (slot-value component 'absolute-pathname) pathname)
          pathname)))

  ;; Default method for component-relative-pathname:
  ;; combine the contents of slot relative-pathname (from specified initarg :pathname)
  ;; with the appropriate source-file-type, which defaults to the file-type of the component.
  (defmethod component-relative-pathname ((component component))
    ;; SOURCE-FILE-TYPE below is strictly for backward-compatibility with ASDF1.
    ;; We ought to be able to extract this from the component alone with FILE-TYPE.
    ;; TODO: track who uses it in Quicklisp, and have them not use it anymore;
    ;; maybe issue a WARNING (then eventually CERROR) if the two methods diverge?
    (parse-unix-namestring
     (or (and (slot-boundp component 'relative-pathname)
              (slot-value component 'relative-pathname))
         (component-name component))
     :want-relative t
     :type (source-file-type component (component-system component))
     :defaults (component-parent-pathname component)))

  (defmethod source-file-type ((component parent-component) (system parent-component))
    :directory)

  (defmethod source-file-type ((component file-component) (system parent-component))
    (file-type component)))


;;;; Encodings
(with-upgradability ()
  (defmethod component-encoding ((c component))
    (or (loop :for x = c :then (component-parent x)
              :while x :thereis (%component-encoding x))
        (detect-encoding (component-pathname c))))

  (defmethod component-external-format ((c component))
    (encoding-external-format (component-encoding c))))


;;;; around-compile-hook
(with-upgradability ()
  (defgeneric around-compile-hook (component)
    (:documentation "An optional hook function that will be called with one argument, a thunk.
The hook function must call the thunk, that will compile code from the component, and may or may not
also evaluate the compiled results. The hook function may establish dynamic variable bindings around
this compilation, or check its results, etc."))
  (defmethod around-compile-hook ((c component))
    (cond
      ((slot-boundp c 'around-compile)
       (slot-value c 'around-compile))
      ((component-parent c)
       (around-compile-hook (component-parent c))))))


;;;; version-satisfies
(with-upgradability ()
  ;; short-circuit testing of null version specifications.
  ;; this is an all-pass, without warning
  (defmethod version-satisfies :around ((c t) (version null))
    t)
  (defmethod version-satisfies ((c component) version)
    (unless (and version (slot-boundp c 'version) (component-version c))
      (when version
        (warn "Requested version ~S but ~S has no version" version c))
      (return-from version-satisfies nil))
    (version-satisfies (component-version c) version))

  (defmethod version-satisfies ((cver string) version)
    (version<= version cver)))


;;; all sub-components (of a given type)
(with-upgradability ()
  (defun sub-components (component &key (type t))
    "Compute the transitive sub-components of given COMPONENT that are of given TYPE"
    (while-collecting (c)
      (labels ((recurse (x)
                 (when (if-let (it (component-if-feature x)) (featurep it) t)
                   (when (typep x type)
                     (c x))
                   (when (typep x 'parent-component)
                     (map () #'recurse (component-children x))))))
        (recurse component)))))

;;;; -------------------------------------------------------------------------
;;;; Operations

(uiop/package:define-package :asdf/operation
  (:recycle :asdf/operation :asdf/action :asdf) ;; asdf/action for FEATURE pre 2.31.5.
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/session)
  (:export
   #:operation
   #:*operations* #:make-operation #:find-operation
   #:feature)) ;; TODO: stop exporting the deprecated FEATURE feature.
(in-package :asdf/operation)

;;; Operation Classes
(when-upgrading (:version "2.27" :when (find-class 'operation nil))
  ;; override any obsolete shared-initialize method when upgrading from ASDF2.
  (defmethod shared-initialize :after ((o operation) (slot-names t) &key)
    (values)))

(with-upgradability ()
  (defclass operation ()
    ()
    (:documentation "The base class for all ASDF operations.

ASDF does NOT and never did distinguish between multiple operations of the same class.
Therefore, all slots of all operations MUST have :allocation :class and no initargs. No exceptions.
"))

  (defvar *in-make-operation* nil)

  (defun check-operation-constructor ()
    "Enforce that OPERATION instances must be created with MAKE-OPERATION."
    (unless *in-make-operation*
      (sysdef-error "OPERATION instances must only be created through MAKE-OPERATION.")))

  (defmethod print-object ((o operation) stream)
    (print-unreadable-object (o stream :type t :identity nil)))

  ;;; Override previous methods (from 3.1.7 and earlier) and add proper error checking.
  #-genera ;; Genera adds its own system initargs, e.g. clos-internals:storage-area 8
  (defmethod initialize-instance :after ((o operation) &rest initargs &key &allow-other-keys)
    (unless (null initargs)
      (parameter-error "~S does not accept initargs" 'operation))))


;;; make-operation, find-operation

(with-upgradability ()
  ;; A table to memoize instances of a given operation. There shall be only one.
  (defparameter* *operations* (make-hash-table :test 'equal))

  ;; A memoizing way of creating instances of operation.
  (defun make-operation (operation-class)
    "This function creates and memoizes an instance of OPERATION-CLASS.
All operation instances MUST be created through this function.

Use of INITARGS is not supported at this time."
    (let ((class (coerce-class operation-class
                               :package :asdf/interface :super 'operation :error 'sysdef-error))
          (*in-make-operation* t))
      (ensure-gethash class *operations* `(make-instance ,class))))

  ;; This function is mostly for backward and forward compatibility:
  ;; operations used to preserve the operation-original-initargs of the context,
  ;; and may in the future preserve some operation-canonical-initargs.
  ;; Still, the treatment of NIL as a disabling context is useful in some cases.
  (defgeneric find-operation (context spec)
    (:documentation "Find an operation by resolving the SPEC in the CONTEXT"))
  (defmethod find-operation ((context t) (spec operation))
    spec)
  (defmethod find-operation ((context t) (spec symbol))
    (when spec ;; NIL designates itself, i.e. absence of operation
      (make-operation spec))) ;; TODO: preserve the (operation-canonical-initargs context)
  (defmethod find-operation ((context t) (spec string))
    (make-operation spec))) ;; TODO: preserve the (operation-canonical-initargs context)

;;;; -------------------------------------------------------------------------
;;;; Systems

(uiop/package:define-package :asdf/system
  (:recycle :asdf :asdf/system :asdf/find-system)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/session :asdf/component)
  (:export
   #:system #:proto-system #:undefined-system #:reset-system-class
   #:system-source-file #:system-source-directory #:system-relative-pathname
   #:system-description #:system-long-description
   #:system-author #:system-maintainer #:system-licence #:system-license
   #:definition-dependency-list #:definition-dependency-set #:system-defsystem-depends-on
   #:system-depends-on #:system-weakly-depends-on
   #:component-build-pathname #:build-pathname
   #:component-entry-point #:entry-point
   #:homepage #:system-homepage
   #:bug-tracker #:system-bug-tracker
   #:mailto #:system-mailto
   #:long-name #:system-long-name
   #:source-control #:system-source-control
   #:coerce-name #:primary-system-name #:primary-system-p #:coerce-filename
   #:find-system #:builtin-system-p)) ;; forward-reference, defined in find-system
(in-package :asdf/system)

(with-upgradability ()
  ;; The method is actually defined in asdf/find-system,
  ;; but we declare the function here to avoid a forward reference.
  (defgeneric find-system (system &optional error-p)
    (:documentation "Given a system designator, find the actual corresponding system object.
If no system is found, then signal an error if ERROR-P is true (the default), or else return NIL.
A system designator is usually a string (conventionally all lowercase) or a symbol, designating
the same system as its downcased name; it can also be a system object (designating itself)."))
  (defgeneric system-source-file (system)
    (:documentation "Return the source file in which system is defined."))
  ;; This is bad design, but was the easiest kluge I found to let the user specify that
  ;; some special actions create outputs at locations controled by the user that are not affected
  ;; by the usual output-translations.
  ;; TODO: Fix operate to stop passing flags to operation (which in the current design shouldn't
  ;; have any flags, since the stamp cache, etc., can't distinguish them), and instead insert
  ;; *there* the ability of specifying special output paths, not in the system definition.
  (defgeneric component-build-pathname (component)
    (:documentation "The COMPONENT-BUILD-PATHNAME, when defined and not null, specifies the
output pathname for the action using the COMPONENT-BUILD-OPERATION.

NB: This interface is subject to change. Please contact ASDF maintainers if you use it."))

  ;; TODO: Should this have been made a SYSTEM-ENTRY-POINT instead?
  (defgeneric component-entry-point (component)
    (:documentation "The COMPONENT-ENTRY-POINT, when defined, specifies what function to call
(with no argument) when running an image dumped from the COMPONENT.

NB: This interface is subject to change. Please contact ASDF maintainers if you use it."))
  (defmethod component-entry-point ((c component))
    nil))


;;;; The system class

(with-upgradability ()
  (defclass proto-system () ; slots to keep when resetting a system
    ;; To preserve identity for all objects, we'd need keep the components slots
    ;; but also to modify parse-component-form to reset the recycled objects.
    ((name)
     (source-file)
     ;; These two slots contains the *inferred* dependencies of define-op,
     ;; from loading the .asd file, as list and as set.
     (definition-dependency-list
         :initform nil :accessor definition-dependency-list)
     (definition-dependency-set
         :initform (list-to-hash-set nil) :accessor definition-dependency-set))
    (:documentation "PROTO-SYSTEM defines the elements of identity that are preserved when
a SYSTEM is redefined and its class is modified."))

  (defclass system (module proto-system)
    ;; Backward-compatibility: inherit from module. ASDF4: only inherit from parent-component.
    (;; {,long-}description is now inherited from component, but we add the legacy accessors
     (description :accessor system-description)
     (long-description :accessor system-long-description)
     (author :accessor system-author :initarg :author :initform nil)
     (maintainer :accessor system-maintainer :initarg :maintainer :initform nil)
     (licence :accessor system-licence :initarg :licence
              :accessor system-license :initarg :license :initform nil)
     (homepage :accessor system-homepage :initarg :homepage :initform nil)
     (bug-tracker :accessor system-bug-tracker :initarg :bug-tracker :initform nil)
     (mailto :accessor system-mailto :initarg :mailto :initform nil)
     (long-name :accessor system-long-name :initarg :long-name :initform nil)
     ;; Conventions for this slot aren't clear yet as of ASDF 2.27, but whenever they are, they will be enforced.
     ;; I'm introducing the slot before the conventions are set for maximum compatibility.
     (source-control :accessor system-source-control :initarg :source-control :initform nil)
     (builtin-system-p :accessor builtin-system-p :initform nil :initarg :builtin-system-p)
     (build-pathname
      :initform nil :initarg :build-pathname :accessor component-build-pathname)
     (entry-point
      :initform nil :initarg :entry-point :accessor component-entry-point)
     (source-file :initform nil :initarg :source-file :accessor system-source-file)
     ;; This slot contains the *declared* defsystem-depends-on dependencies
     (defsystem-depends-on :reader system-defsystem-depends-on :initarg :defsystem-depends-on
                           :initform nil)
     ;; these two are specially set in parse-component-form, so have no :INITARGs.
     (depends-on :reader system-depends-on :initform nil)
     (weakly-depends-on :reader system-weakly-depends-on :initform nil))
    (:documentation "SYSTEM is the base class for top-level components that users may request
ASDF to build."))

  (defclass undefined-system (system) ()
    (:documentation "System that was not defined yet."))

  (defun reset-system-class (system new-class &rest keys &key &allow-other-keys)
    "Erase any data from a SYSTEM except its basic identity, then reinitialize it
based on supplied KEYS."
    (change-class (change-class system 'proto-system) new-class)
    (apply 'reinitialize-instance system keys)))


;;; Canonicalizing system names

(with-upgradability ()
  (defun coerce-name (name)
    "Given a designator for a component NAME, return the name as a string.
The designator can be a COMPONENT (designing its name; note that a SYSTEM is a component),
a SYMBOL (designing its name, downcased), or a STRING (designing itself)."
    (typecase name
      (component (component-name name))
      (symbol (string-downcase name))
      (string name)
      (t (sysdef-error (compatfmt "~@<Invalid component designator: ~3i~_~A~@:>") name))))

  (defun primary-system-name (system-designator)
    "Given a system designator NAME, return the name of the corresponding primary system,
after which the .asd file is named. That's the first component when dividing the name
as a string by / slashes. A component designates its system."
    (etypecase system-designator
      (string (if-let (p (position #\/ system-designator))
                (subseq system-designator 0 p) system-designator))
      (symbol (primary-system-name (coerce-name system-designator)))
      (component (primary-system-name (coerce-name (component-system system-designator))))))

  (defun primary-system-p (system)
    "Given a system designator SYSTEM, return T if it designates a primary system, or else NIL.
Also return NIL if system is neither a SYSTEM nor a string designating one."
    (typecase system
      (string (not (find #\/ system)))
      (system (primary-system-p (coerce-name system)))))

  (defun coerce-filename (name)
    "Coerce a system designator NAME into a string suitable as a filename component.
The (current) transformation is to replace characters /:\\ each by --,
the former being forbidden in a filename component.
NB: The onus is unhappily on the user to avoid clashes."
    (frob-substrings (coerce-name name) '("/" ":" "\\") "--")))


;;;; Pathnames

(with-upgradability ()
  ;; Resolve a system designator to a system before extracting its system-source-file
  (defmethod system-source-file ((system-name string))
    (system-source-file (find-system system-name)))
  (defmethod system-source-file ((system-name symbol))
    (when system-name
      (system-source-file (find-system system-name))))

  (defun system-source-directory (system-designator)
    "Return a pathname object corresponding to the directory
in which the system specification (.asd file) is located."
    (pathname-directory-pathname (system-source-file system-designator)))

  (defun* (system-relative-pathname) (system name &key type)
    "Given a SYSTEM, and a (Unix-style relative path) NAME of a file (or directory) of given TYPE,
return the absolute pathname of a corresponding file under that system's source code pathname."
    (subpathname (system-source-directory system) name :type type))

  (defmethod component-pathname ((system system))
    "Given a SYSTEM, and a (Unix-style relative path) NAME of a file (or directory) of given TYPE,
return the absolute pathname of a corresponding file under that system's source code pathname."
    (let ((pathname (or (call-next-method) (system-source-directory system))))
      (unless (and (slot-boundp system 'relative-pathname) ;; backward-compatibility with ASDF1-age
                   (slot-value system 'relative-pathname)) ;; systems that directly access this slot.
        (setf (slot-value system 'relative-pathname) pathname))
      pathname))

  ;; The default method of component-relative-pathname for a system:
  ;; if a pathname was specified in the .asd file, it must be relative to the .asd file
  ;; (actually, to its truename* if *resolve-symlinks* it true, the default).
  ;; The method will return an *absolute* pathname, once again showing that the historical name
  ;; component-relative-pathname is misleading and should have been component-specified-pathname.
  (defmethod component-relative-pathname ((system system))
    (parse-unix-namestring
     (and (slot-boundp system 'relative-pathname)
          (slot-value system 'relative-pathname))
     :want-relative t
     :type :directory
     :ensure-absolute t
     :defaults (system-source-directory system)))

  ;; A system has no parent; if some method wants to make a path "relative to its parent",
  ;; it will instead be relative to the system itself.
  (defmethod component-parent-pathname ((system system))
    (system-source-directory system))

  ;; Most components don't have a specified component-build-pathname, and therefore
  ;; no magic redirection of their output that disregards the output-translations.
  (defmethod component-build-pathname ((c component))
    nil))

;;;; -------------------------------------------------------------------------
;;;; Finding systems

(uiop/package:define-package :asdf/system-registry
  (:recycle :asdf/system-registry :asdf/find-system :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade
    :asdf/session :asdf/component :asdf/system)
  (:export
   #:remove-entry-from-registry #:coerce-entry-to-directory
   #:registered-system #:register-system
   #:registered-systems* #:registered-systems
   #:clear-system #:map-systems
   #:*system-definition-search-functions* #:search-for-system-definition
   #:*central-registry* #:probe-asd #:sysdef-central-registry-search
   #:contrib-sysdef-search #:sysdef-find-asdf ;; backward compatibility symbols, functions removed
   #:sysdef-preloaded-system-search #:register-preloaded-system #:*preloaded-systems*
   #:find-system-if-being-defined #:mark-component-preloaded ;; forward references to asdf/find-system
   #:sysdef-immutable-system-search #:register-immutable-system #:*immutable-systems*
   #:*registered-systems* #:clear-registered-systems
   ;; defined in source-registry, but specially mentioned here:
   #:sysdef-source-registry-search))
(in-package :asdf/system-registry)

(with-upgradability ()
  ;;; Registry of Defined Systems

  (defvar *registered-systems* (make-hash-table :test 'equal)
    "This is a hash table whose keys are strings -- the names of systems --
and whose values are systems.
A system is referred to as \"registered\" if it is present in this table.")

  (defun registered-system (name)
    "Return a system of given NAME that was registered already,
if such a system exists.  NAME is a system designator, to be
normalized by COERCE-NAME. The value returned is a system object,
or NIL if not found."
    (gethash (coerce-name name) *registered-systems*))

  (defun registered-systems* ()
    "Return a list containing every registered system (as a system object)."
    (loop :for registered :being :the :hash-values :of *registered-systems*
          :collect registered))

  (defun registered-systems ()
    "Return a list of the names of every registered system."
    (mapcar 'coerce-name (registered-systems*)))

  (defun register-system (system)
    "Given a SYSTEM object, register it."
    (check-type system system)
    (let ((name (component-name system)))
      (check-type name string)
      (asdf-message (compatfmt "~&~@<; ~@;Registering system ~3i~_~A~@:>~%") name)
      (setf (gethash name *registered-systems*) system)))

  (defun map-systems (fn)
    "Apply FN to each defined system.

FN should be a function of one argument. It will be
called with an object of type asdf:system."
    (loop :for registered :being :the :hash-values :of *registered-systems*
          :do (funcall fn registered)))


  ;;; Preloaded systems: in the image even if you can't find source files backing them.

  (defvar *preloaded-systems* (make-hash-table :test 'equal)
    "Registration table for preloaded systems.")

  (declaim (ftype (function (t) t) mark-component-preloaded)) ; defined in asdf/find-system

  (defun make-preloaded-system (name keys)
    "Make a preloaded system of given NAME with build information from KEYS"
    (let ((system (apply 'make-instance (getf keys :class 'system)
                         :name name :source-file (getf keys :source-file)
                         (remove-plist-keys '(:class :name :source-file) keys))))
      (mark-component-preloaded system)
      system))

  (defun sysdef-preloaded-system-search (requested)
    "If REQUESTED names a system registered as preloaded, return a new system
with its registration information."
    (let ((name (coerce-name requested)))
      (multiple-value-bind (keys foundp) (gethash name *preloaded-systems*)
        (when foundp
          (make-preloaded-system name keys)))))

  (defun ensure-preloaded-system-registered (name)
    "If there isn't a registered _defined_ system of given NAME,
and a there is a registered _preloaded_ system of given NAME,
then define and register said preloaded system."
    (if-let (system (and (not (registered-system name)) (sysdef-preloaded-system-search name)))
      (register-system system)))

  (defun register-preloaded-system (system-name &rest keys &key (version t) &allow-other-keys)
    "Register a system as being preloaded. If the system has not been loaded from the filesystem
yet, or if its build information is later cleared with CLEAR-SYSTEM, a dummy system will be
registered without backing filesystem information, based on KEYS (e.g. to provide a VERSION).
If VERSION is the default T, and a system was already loaded, then its version will be preserved."
    (let ((name (coerce-name system-name)))
      (when (eql version t)
        (if-let (system (registered-system name))
          (setf (getf keys :version) (component-version system))))
      (setf (gethash name *preloaded-systems*) keys)
      (ensure-preloaded-system-registered system-name)))


  ;;; Immutable systems: in the image and can't be reloaded from source.

  (defvar *immutable-systems* nil
    "A hash-set (equal hash-table mapping keys to T) of systems that are immutable,
i.e. already loaded in memory and not to be refreshed from the filesystem.
They will be treated specially by find-system, and passed as :force-not argument to make-plan.

For instance, to can deliver an image with many systems precompiled, that *will not* check the
filesystem for them every time a user loads an extension, what more risk a problematic upgrade
 or catastrophic downgrade, before you dump an image, you may use:
   (map () 'asdf:register-immutable-system (asdf:already-loaded-systems))

Note that direct access to this variable from outside ASDF is not supported.
Please call REGISTER-IMMUTABLE-SYSTEM to add new immutable systems, and
contact maintainers if you need a stable API to do more than that.")

  (defun sysdef-immutable-system-search (requested)
    (let ((name (coerce-name requested)))
      (when (and *immutable-systems* (gethash name *immutable-systems*))
        (or (registered-system requested)
            (error 'formatted-system-definition-error
                   :format-control "Requested system ~A registered as an immutable-system, ~
but not even registered as defined"
                   :format-arguments (list name))))))

  (defun register-immutable-system (system-name &rest keys)
    "Register SYSTEM-NAME as preloaded and immutable.
It will automatically be considered as passed to FORCE-NOT in a plan."
    (let ((system-name (coerce-name system-name)))
      (apply 'register-preloaded-system system-name keys)
      (unless *immutable-systems*
        (setf *immutable-systems* (list-to-hash-set nil)))
      (setf (gethash system-name *immutable-systems*) t)))


  ;;; Making systems undefined.

  (defun clear-system (system)
    "Clear the entry for a SYSTEM in the database of systems previously defined.
However if the system was registered as PRELOADED (which it is if it is IMMUTABLE),
then a new system with the same name will be defined and registered in its place
from which build details will have been cleared.
Note that this does NOT in any way cause any of the code of the system to be unloaded.
Returns T if system was or is now undefined, NIL if a new preloaded system was redefined."
    ;; There is no "unload" operation in Common Lisp, and
    ;; a general such operation cannot be portably written,
    ;; considering how much CL relies on side-effects to global data structures.
    (let ((name (coerce-name system)))
      (remhash name *registered-systems*)
      (unset-asdf-cache-entry `(find-system ,name))
      (not (ensure-preloaded-system-registered name))))

  (defun clear-registered-systems ()
    "Clear all currently registered defined systems.
Preloaded systems (including immutable ones) will be reset, other systems will be de-registered."
    (map () 'clear-system (registered-systems)))


  ;;; Searching for system definitions

  ;; For the sake of keeping things reasonably neat, we adopt a convention that
  ;; only symbols are to be pushed to this list (rather than e.g. function objects),
  ;; which makes upgrade easier. Also, the name of these symbols shall start with SYSDEF-
  (defvar *system-definition-search-functions* '()
    "A list that controls the ways that ASDF looks for system definitions.
It contains symbols to be funcalled in order, with a requested system name as argument,
until one returns a non-NIL result (if any), which must then be a fully initialized system object
with that name.")

  ;; Initialize and/or upgrade the *system-definition-search-functions*
  ;; so it doesn't contain obsolete symbols, and does contain the current ones.
  (defun cleanup-system-definition-search-functions ()
    (setf *system-definition-search-functions*
          (append
           ;; Remove known-incompatible sysdef functions from old versions of asdf.
           ;; Order matters, so we can't just use set-difference.
           (let ((obsolete
                  '(contrib-sysdef-search sysdef-find-asdf sysdef-preloaded-system-search)))
             (remove-if #'(lambda (x) (member x obsolete)) *system-definition-search-functions*))
           ;; Tuck our defaults at the end of the list if they were absent.
           ;; This is imperfect, in case they were removed on purpose,
           ;; but then it will be the responsibility of whoever removes these symmbols
           ;; to upgrade asdf before he does such a thing rather than after.
           (remove-if #'(lambda (x) (member x *system-definition-search-functions*))
                      '(sysdef-central-registry-search
                        sysdef-source-registry-search)))))
  (cleanup-system-definition-search-functions)

  ;; This (private) function does the search for a system definition using *s-d-s-f*;
  ;; it is to be called by locate-system.
  (defun search-for-system-definition (system)
    ;; Search for valid definitions of the system available in the current session.
    ;; Previous definitions as registered in *registered-systems* MUST NOT be considered;
    ;; they will be reconciled by locate-system then find-system.
    ;; There are two special treatments: first, specially search for objects being defined
    ;; in the current session, to avoid definition races between several files;
    ;; second, specially search for immutable systems, so they cannot be redefined.
    ;; Finally, use the search functions specified in *system-definition-search-functions*.
    (let ((name (coerce-name system)))
      (flet ((try (f) (if-let ((x (funcall f name))) (return-from search-for-system-definition x))))
        (try 'find-system-if-being-defined)
        (try 'sysdef-immutable-system-search)
        (map () #'try *system-definition-search-functions*))))


  ;;; The legacy way of finding a system: the *central-registry*

  ;; This variable contains a list of directories to be lazily searched for the requested asd
  ;; by sysdef-central-registry-search.
  (defvar *central-registry* nil
    "A list of 'system directory designators' ASDF uses to find systems.

A 'system directory designator' is a pathname or an expression
which evaluates to a pathname. For example:

    (setf asdf:*central-registry*
          (list '*default-pathname-defaults*
                #p\"/home/me/cl/systems/\"
                #p\"/usr/share/common-lisp/systems/\"))

This variable is for backward compatibility.
Going forward, we recommend new users should be using the source-registry.")

  ;; Function to look for an asd file of given NAME under a directory provided by DEFAULTS.
  ;; Return the truename of that file if it is found and TRUENAME is true.
  ;; Return NIL if the file is not found.
  ;; On Windows, follow shortcuts to .asd files.
  (defun probe-asd (name defaults &key truename)
    (block nil
      (when (directory-pathname-p defaults)
        (if-let (file (probe-file*
                       (ensure-absolute-pathname
                        (parse-unix-namestring name :type "asd")
                        #'(lambda () (ensure-absolute-pathname defaults 'get-pathname-defaults nil))
                        nil)
                       :truename truename))
          (return file))
        #-(or clisp genera) ; clisp doesn't need it, plain genera doesn't have read-sequence(!)
        (os-cond
         ((os-windows-p)
          (when (physical-pathname-p defaults)
            (let ((shortcut
                    (make-pathname
                     :defaults defaults :case :local
                     :name (strcat name ".asd")
                     :type "lnk")))
              (when (probe-file* shortcut)
                (ensure-pathname (parse-windows-shortcut shortcut) :namestring :native)))))))))

  ;; Function to push onto *s-d-s-f* to use the *central-registry*
  (defun sysdef-central-registry-search (system)
    (let ((name (primary-system-name system))
          (to-remove nil)
          (to-replace nil))
      (block nil
        (unwind-protect
             (dolist (dir *central-registry*)
               (let ((defaults (eval dir))
                     directorized)
                 (when defaults
                   (cond ((directory-pathname-p defaults)
                          (let* ((file (probe-asd name defaults :truename *resolve-symlinks*)))
                            (when file
                              (return file))))
                         (t
                          (restart-case
                              (let* ((*print-circle* nil)
                                     (message
                                       (format nil
                                               (compatfmt "~@<While searching for system ~S: ~3i~_~S evaluated to ~S which is not an absolute directory.~@:>")
                                               system dir defaults)))
                                (error message))
                            (remove-entry-from-registry ()
                              :report "Remove entry from *central-registry* and continue"
                              (push dir to-remove))
                            (coerce-entry-to-directory ()
                              :test (lambda (c) (declare (ignore c))
                                      (and (not (directory-pathname-p defaults))
                                           (directory-pathname-p
                                            (setf directorized
                                                  (ensure-directory-pathname defaults)))))
                              :report (lambda (s)
                                        (format s (compatfmt "~@<Coerce entry to ~a, replace ~a and continue.~@:>")
                                                directorized dir))
                              (push (cons dir directorized) to-replace))))))))
          ;; cleanup
          (dolist (dir to-remove)
            (setf *central-registry* (remove dir *central-registry*)))
          (dolist (pair to-replace)
            (let* ((current (car pair))
                   (new (cdr pair))
                   (position (position current *central-registry*)))
              (setf *central-registry*
                    (append (subseq *central-registry* 0 position)
                            (list new)
                            (subseq *central-registry* (1+ position)))))))))))

;;;; -------------------------------------------------------------------------
;;;; Actions

(uiop/package:define-package :asdf/action
  (:nicknames :asdf-action)
  (:recycle :asdf/action :asdf/plan :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/session :asdf/component :asdf/operation)
  (:import-from :asdf/operation #:check-operation-constructor)
  (:import-from :asdf/component #:%additional-input-files)
  (:export
   #:action #:define-convenience-action-methods
   #:action-description #:format-action
   #:downward-operation #:upward-operation #:sideway-operation #:selfward-operation
   #:non-propagating-operation
   #:component-depends-on
   #:input-files #:output-files #:output-file #:operation-done-p
   #:action-operation #:action-component #:make-action
   #:component-operation-time #:mark-operation-done #:compute-action-stamp
   #:perform #:perform-with-restarts #:retry #:accept
   #:action-path #:find-action
   #:operation-definition-warning #:operation-definition-error ;; condition
   #:action-valid-p
   #:circular-dependency #:circular-dependency-actions
   #:call-while-visiting-action #:while-visiting-action
   #:additional-input-files))
(in-package :asdf/action)

(eval-when (#-lispworks :compile-toplevel :load-toplevel :execute) ;; LispWorks issues spurious warning

  (deftype action ()
    "A pair of operation and component uniquely identifies a node in the dependency graph
of steps to be performed while building a system."
    '(cons operation component))

  (deftype operation-designator ()
    "An operation designates itself. NIL designates a context-dependent current operation,
and a class-name or class designates the canonical instance of the designated class."
    '(or operation null symbol class)))

;;; these are pseudo accessors -- let us abstract away the CONS cell representation of plan
;;; actions.
(with-upgradability ()
  (defun make-action (operation component)
    (cons operation component))
  (defun action-operation (action)
    (car action))
  (defun action-component (action)
    (cdr action)))

;;;; Reified representation for storage or debugging. Note: an action is identified by its class.
(with-upgradability ()
  (defun action-path (action)
    "A readable data structure that identifies the action."
    (when action
      (let ((o (action-operation action))
            (c (action-component action)))
        (cons (type-of o) (component-find-path c)))))
  (defun find-action (path)
    "Reconstitute an action from its action-path"
    (destructuring-bind (o . c) path (make-action (make-operation o) (find-component () c)))))

;;;; Convenience methods
(with-upgradability ()
  ;; A macro that defines convenience methods for a generic function (gf) that
  ;; dispatches on operation and component.  The convenience methods allow users
  ;; to call the gf with operation and/or component designators, that the
  ;; methods will resolve into actual operation and component objects, so that
  ;; the users can interact using readable designators, but developers only have
  ;; to write methods that handle operation and component objects.
  ;; FUNCTION is the generic function name
  ;; FORMALS is its list of arguments, which must include OPERATION and COMPONENT.
  ;; IF-NO-OPERATION is a form (defaults to NIL) describing what to do if no operation is found.
  ;; IF-NO-COMPONENT is a form (defaults to NIL) describing what to do if no component is found.
  (defmacro define-convenience-action-methods
      (function formals &key if-no-operation if-no-component)
    (let* ((rest (gensym "REST"))
           (found (gensym "FOUND"))
           (keyp (equal (last formals) '(&key)))
           (formals-no-key (if keyp (butlast formals) formals))
           (len (length formals-no-key))
           (operation 'operation)
           (component 'component)
           (opix (position operation formals))
           (coix (position component formals))
           (prefix (subseq formals 0 opix))
           (suffix (subseq formals (1+ coix) len))
           (more-args (when keyp `(&rest ,rest &key &allow-other-keys))))
      (assert (and (integerp opix) (integerp coix) (= coix (1+ opix))))
      (flet ((next-method (o c)
               (if keyp
                   `(apply ',function ,@prefix ,o ,c ,@suffix ,rest)
                   `(,function ,@prefix ,o ,c ,@suffix))))
        `(progn
           (defmethod ,function (,@prefix (,operation string) ,component ,@suffix ,@more-args)
             (declare (notinline ,function))
             (let ((,component (find-component () ,component))) ;; do it first, for defsystem-depends-on
               ,(next-method `(safe-read-from-string ,operation :package :asdf/interface) component)))
           (defmethod ,function (,@prefix (,operation symbol) ,component ,@suffix ,@more-args)
             (declare (notinline ,function))
             (if ,operation
                 ,(next-method
                   `(make-operation ,operation)
                   `(or (find-component () ,component) ,if-no-component))
                 ,if-no-operation))
           (defmethod ,function (,@prefix (,operation operation) ,component ,@suffix ,@more-args)
             (declare (notinline ,function))
             (if (typep ,component 'component)
                 (error "No defined method for ~S on ~/asdf-action:format-action/"
                        ',function (make-action ,operation ,component))
                 (if-let (,found (find-component () ,component))
                    ,(next-method operation found)
                    ,if-no-component))))))))


;;;; Self-description
(with-upgradability ()
  (defgeneric action-description (operation component)
    (:documentation "returns a phrase that describes performing this operation
on this component, e.g. \"loading /a/b/c\".
You can put together sentences using this phrase."))
  (defmethod action-description (operation component)
    (format nil (compatfmt "~@<~A on ~A~@:>")
            operation component))

  (defun format-action (stream action &optional colon-p at-sign-p)
    "FORMAT helper to display an action's action-description.
Use it in FORMAT control strings as ~/asdf-action:format-action/"
    (assert (null colon-p)) (assert (null at-sign-p))
    (destructuring-bind (operation . component) action
      (princ (action-description operation component) stream))))


;;;; Detection of circular dependencies
(with-upgradability ()
  (defun (action-valid-p) (operation component)
    "Is this action valid to include amongst dependencies?"
    ;; If either the operation or component was resolved to nil, the action is invalid.
    ;; :if-feature will invalidate actions on components for which the features don't apply.
    (and operation component
         (if-let (it (component-if-feature component)) (featurep it) t)))

  (define-condition circular-dependency (system-definition-error)
    ((actions :initarg :actions :reader circular-dependency-actions))
    (:report (lambda (c s)
               (format s (compatfmt "~@<Circular dependency: ~3i~_~S~@:>")
                       (circular-dependency-actions c)))))

  (defun call-while-visiting-action (operation component fun)
    "Detect circular dependencies"
    (with-asdf-session ()
      (with-accessors ((action-set visiting-action-set)
                       (action-list visiting-action-list)) *asdf-session*
        (let ((action (cons operation component)))
          (when (gethash action action-set)
            (error 'circular-dependency :actions
                   (member action (reverse action-list) :test 'equal)))
          (setf (gethash action action-set) t)
          (push action action-list)
          (unwind-protect
               (funcall fun)
            (pop action-list)
            (setf (gethash action action-set) nil))))))

  ;; Syntactic sugar for call-while-visiting-action
  (defmacro while-visiting-action ((o c) &body body)
    `(call-while-visiting-action ,o ,c #'(lambda () ,@body))))


;;;; Dependencies
(with-upgradability ()
  (defgeneric component-depends-on (operation component) ;; ASDF4: rename to component-dependencies
    (:documentation
     "Returns a list of dependencies needed by the component to perform
    the operation.  A dependency has one of the following forms:

      (<operation> <component>*), where <operation> is an operation designator
        with respect to FIND-OPERATION in the context of the OPERATION argument,
        and each <component> is a component designator with respect to
        FIND-COMPONENT in the context of the COMPONENT argument,
        and means that the component depends on
        <operation> having been performed on each <component>;

        [Note: an <operation> is an operation designator -- it can be either an
        operation name or an operation object.  Similarly, a <component> may be
        a component name or a component object.  Also note that, the degenerate
        case of (<operation>) is a no-op.]

    Methods specialized on subclasses of existing component types
    should usually append the results of CALL-NEXT-METHOD to the list."))
  (define-convenience-action-methods component-depends-on (operation component))

  (defmethod component-depends-on :around ((o operation) (c component))
    (do-asdf-cache `(component-depends-on ,o ,c)
      (call-next-method))))


;;;; upward-operation, downward-operation, sideway-operation, selfward-operation
;; These together handle actions that propagate along the component hierarchy or operation universe.
(with-upgradability ()
  (defclass downward-operation (operation)
    ((downward-operation
      :initform nil :reader downward-operation
      :type operation-designator :allocation :class))
    (:documentation "A DOWNWARD-OPERATION's dependencies propagate down the component hierarchy.
I.e., if O is a DOWNWARD-OPERATION and its DOWNWARD-OPERATION slot designates operation D, then
the action (O . M) of O on module M will depends on each of (D . C) for each child C of module M.
The default value for slot DOWNWARD-OPERATION is NIL, which designates the operation O itself.
E.g. in order for a MODULE to be loaded with LOAD-OP (resp. compiled with COMPILE-OP), all the
children of the MODULE must have been loaded with LOAD-OP (resp. compiled with COMPILE-OP."))
  (defun downward-operation-depends-on (o c)
    `((,(or (downward-operation o) o) ,@(component-children c))))
  (defmethod component-depends-on ((o downward-operation) (c parent-component))
    `(,@(downward-operation-depends-on o c) ,@(call-next-method)))

  (defclass upward-operation (operation)
    ((upward-operation
      :initform nil :reader upward-operation
      :type operation-designator :allocation :class))
    (:documentation "An UPWARD-OPERATION has dependencies that propagate up the component hierarchy.
I.e., if O is an instance of UPWARD-OPERATION, and its UPWARD-OPERATION slot designates operation U,
then the action (O . C) of O on a component C that has the parent P will depends on (U . P).
The default value for slot UPWARD-OPERATION is NIL, which designates the operation O itself.
E.g. in order for a COMPONENT to be prepared for loading or compiling with PREPARE-OP, its PARENT
must first be prepared for loading or compiling with PREPARE-OP."))
  ;; For backward-compatibility reasons, a system inherits from module and is a child-component
  ;; so we must guard against this case. ASDF4: remove that.
  (defun upward-operation-depends-on (o c)
    (if-let (p (component-parent c)) `((,(or (upward-operation o) o) ,p))))
  (defmethod component-depends-on ((o upward-operation) (c child-component))
    `(,@(upward-operation-depends-on o c) ,@(call-next-method)))

  (defclass sideway-operation (operation)
    ((sideway-operation
      :initform nil :reader sideway-operation
      :type operation-designator :allocation :class))
    (:documentation "A SIDEWAY-OPERATION has dependencies that propagate \"sideway\" to siblings
that a component depends on. I.e. if O is a SIDEWAY-OPERATION, and its SIDEWAY-OPERATION slot
designates operation S (where NIL designates O itself), then the action (O . C) of O on component C
depends on each of (S . D) where D is a declared dependency of C.
E.g. in order for a COMPONENT to be prepared for loading or compiling with PREPARE-OP,
each of its declared dependencies must first be loaded as by LOAD-OP."))
  (defun sideway-operation-depends-on (o c)
    `((,(or (sideway-operation o) o) ,@(component-sideway-dependencies c))))
  (defmethod component-depends-on ((o sideway-operation) (c component))
    `(,@(sideway-operation-depends-on o c) ,@(call-next-method)))

  (defclass selfward-operation (operation)
    ((selfward-operation
      ;; NB: no :initform -- if an operation depends on others, it must explicitly specify which
      :type (or operation-designator list) :reader selfward-operation :allocation :class))
    (:documentation "A SELFWARD-OPERATION depends on another operation on the same component.
I.e., if O is a SELFWARD-OPERATION, and its SELFWARD-OPERATION designates a list of operations L,
then the action (O . C) of O on component C depends on each (S . C) for S in L.
E.g. before a component may be loaded by LOAD-OP, it must have been compiled by COMPILE-OP.
A operation-designator designates a singleton list of the designated operation;
a list of operation-designators designates the list of designated operations;
NIL is not a valid operation designator in that context.  Note that any dependency
ordering between the operations in a list of SELFWARD-OPERATION should be specified separately
in the respective operation's COMPONENT-DEPENDS-ON methods so that they be scheduled properly."))
  (defun selfward-operation-depends-on (o c)
    (loop :for op :in (ensure-list (selfward-operation o)) :collect `(,op ,c)))
  (defmethod component-depends-on ((o selfward-operation) (c component))
    `(,@(selfward-operation-depends-on o c) ,@(call-next-method)))

  (defclass non-propagating-operation (operation)
    ()
    (:documentation "A NON-PROPAGATING-OPERATION is an operation that propagates
no dependencies whatsoever.  It is supplied in order that the programmer be able
to specify that s/he is intentionally specifying an operation which invokes no
dependencies.")))


;;;---------------------------------------------------------------------------
;;; Help programmers catch obsolete OPERATION subclasses
;;;---------------------------------------------------------------------------
(with-upgradability ()
  (define-condition operation-definition-warning (simple-warning)
    ()
    (:documentation "Warning condition related to definition of obsolete OPERATION objects."))

  (define-condition operation-definition-error (simple-error)
    ()
    (:documentation "Error condition related to definition of incorrect OPERATION objects."))

  (defmethod initialize-instance :before ((o operation) &key)
    (check-operation-constructor)
    (unless (typep o '(or downward-operation upward-operation sideway-operation
                          selfward-operation non-propagating-operation))
      (warn 'operation-definition-warning
            :format-control
            "No dependency propagating scheme specified for operation class ~S.
The class needs to be updated for ASDF 3.1 and specify appropriate propagation mixins."
            :format-arguments (list (type-of o)))))

  (defmethod initialize-instance :before ((o non-propagating-operation) &key)
    (when (typep o '(or downward-operation upward-operation sideway-operation selfward-operation))
      (error 'operation-definition-error
             :format-control
             "Inconsistent class: ~S
  NON-PROPAGATING-OPERATION is incompatible with propagating operation classes as superclasses."
             :format-arguments
             (list (type-of o)))))

  (defun backward-compatible-depends-on (o c)
    "DEPRECATED: all subclasses of OPERATION used in ASDF should inherit from one of
 DOWNWARD-OPERATION UPWARD-OPERATION SIDEWAY-OPERATION SELFWARD-OPERATION NON-PROPAGATING-OPERATION.
 The function BACKWARD-COMPATIBLE-DEPENDS-ON temporarily provides ASDF2 behaviour for those that
 don't. In the future this functionality will be removed, and the default will be no propagation."
    (uiop/version::notify-deprecated-function
     (version-deprecation *asdf-version* :style-warning "3.2")
     `(backward-compatible-depends-on :for-operation ,o))
    `(,@(sideway-operation-depends-on o c)
      ,@(when (typep c 'parent-component) (downward-operation-depends-on o c))))

  (defmethod component-depends-on ((o operation) (c component))
    `(;; Normal behavior, to allow user-specified in-order-to dependencies
      ,@(cdr (assoc (type-of o) (component-in-order-to c)))
        ;; For backward-compatibility with ASDF2, any operation that doesn't specify propagation
        ;; or non-propagation through an appropriate mixin will be downward and sideway.
        ,@(unless (typep o '(or downward-operation upward-operation sideway-operation
                             selfward-operation non-propagating-operation))
            (backward-compatible-depends-on o c))))

  (defmethod downward-operation ((o operation)) nil)
  (defmethod sideway-operation ((o operation)) nil))


;;;---------------------------------------------------------------------------
;;; End of OPERATION class checking
;;;---------------------------------------------------------------------------


;;;; Inputs, Outputs, and invisible dependencies
(with-upgradability ()
  (defgeneric output-files (operation component)
    (:documentation "Methods for this function return two values: a list of output files
corresponding to this action, and a boolean indicating if they have already been subjected
to relevant output translations and should not be further translated.

Methods on PERFORM *must* call this function to determine where their outputs are to be located.
They may rely on the order of the files to discriminate between outputs.
"))
  (defgeneric input-files (operation component)
    (:documentation "A list of input files corresponding to this action.

Methods on PERFORM *must* call this function to determine where their inputs are located.
They may rely on the order of the files to discriminate between inputs.
"))
  (defgeneric operation-done-p (operation component)
    (:documentation "Returns a boolean which is NIL if the action must be performed (again)."))
  (define-convenience-action-methods output-files (operation component))
  (define-convenience-action-methods input-files (operation component))
  (define-convenience-action-methods operation-done-p (operation component))

  (defmethod operation-done-p ((o operation) (c component))
    t)

  ;; Translate output files, unless asked not to. Memoize the result.
  (defmethod output-files :around ((operation t) (component t))
    (do-asdf-cache `(output-files ,operation ,component)
      (values
       (multiple-value-bind (pathnames fixedp) (call-next-method)
         ;; 1- Make sure we have absolute pathnames
         (let* ((directory (pathname-directory-pathname
                            (component-pathname (find-component () component))))
                (absolute-pathnames
                  (loop
                    :for pathname :in pathnames
                    :collect (ensure-absolute-pathname pathname directory))))
           ;; 2- Translate those pathnames as required
           (if fixedp
               absolute-pathnames
               (mapcar *output-translation-function* absolute-pathnames))))
       t)))
  (defmethod output-files ((o operation) (c component))
    nil)
  (defun output-file (operation component)
    "The unique output file of performing OPERATION on COMPONENT"
    (let ((files (output-files operation component)))
      (assert (length=n-p files 1))
      (first files)))

  (defgeneric additional-input-files (operation component)
    (:documentation "Additional input files for the operation on this
    component.  These are files that are inferred, rather than
    explicitly specified, and these are typically NOT files that
    undergo operations directly.  Instead, they are files that it is
    important for ASDF to know about in order to compute operation times,etc."))
  (define-convenience-action-methods additional-input-files (operation component))
  (defmethod additional-input-files ((op operation) (comp component))
      (cdr (assoc op (%additional-input-files comp))))

  ;; Memoize input files.
  (defmethod input-files :around (operation component)
    (do-asdf-cache `(input-files ,operation ,component)
      ;; get the additional input files, if any
      (append (call-next-method)
              ;; must come after the first, for other code that
              ;; assumes the first will be the "key" file
              (additional-input-files operation component))))

  ;; By default an action has no input-files.
  (defmethod input-files ((o operation) (c component))
    nil)

  ;; An action with a selfward-operation by default gets its input-files from the output-files of
  ;; the actions using selfward-operations it depends on (and the same component),
  ;; or if there are none, on the component-pathname of the component if it's a file
  ;; -- and then on the results of the next-method.
  (defmethod input-files ((o selfward-operation) (c component))
    `(,@(or (loop :for dep-o :in (ensure-list (selfward-operation o))
                  :append (or (output-files dep-o c) (input-files dep-o c)))
            (if-let ((pathname (component-pathname c)))
              (and (file-pathname-p pathname) (list pathname))))
      ,@(call-next-method))))


;;;; Done performing
(with-upgradability ()
  ;; ASDF4: hide it behind plan-action-stamp
  (defgeneric component-operation-time (operation component)
    (:documentation "Return the timestamp for when an action was last performed"))
  (defgeneric (setf component-operation-time) (time operation component)
    (:documentation "Update the timestamp for when an action was last performed"))
  (define-convenience-action-methods component-operation-time (operation component))

  ;; ASDF4: hide it behind (setf plan-action-stamp)
  (defgeneric mark-operation-done (operation component)
    (:documentation "Mark a action as having been just done.

Updates the action's COMPONENT-OPERATION-TIME to match the COMPUTE-ACTION-STAMP
using the JUST-DONE flag."))
  (defgeneric compute-action-stamp (plan- operation component &key just-done)
    ;; NB: using plan- rather than plan above allows clisp to upgrade from 2.26(!)
    (:documentation "Has this action been successfully done already,
and at what known timestamp has it been done at or will it be done at?
* PLAN is a plan object modelling future effects of actions,
  or NIL to denote what actually happened.
* OPERATION and COMPONENT denote the action.
Takes keyword JUST-DONE:
* JUST-DONE is a boolean that is true if the action was just successfully performed,
  at which point we want compute the actual stamp and warn if files are missing;
  otherwise we are making plans, anticipating the effects of the action.
Returns two values:
* a STAMP saying when it was done or will be done,
  or T if the action involves files that need to be recomputed.
* a boolean DONE-P that indicates whether the action has actually been done,
  and both its output-files and its in-image side-effects are up to date."))

  (defmethod component-operation-time ((o operation) (c component))
    (gethash o (component-operation-times c)))

  (defmethod (setf component-operation-time) (stamp (o operation) (c component))
    (assert stamp () "invalid null stamp for ~A" (action-description o c))
    (setf (gethash o (component-operation-times c)) stamp))

  (defmethod mark-operation-done ((o operation) (c component))
    (let ((stamp (compute-action-stamp nil o c :just-done t)))
      (assert stamp () "Failed to compute a stamp for completed action ~A" (action-description o c))1
      (setf (component-operation-time o c) stamp))))


;;;; Perform
(with-upgradability ()
  (defgeneric perform (operation component)
    (:documentation "PERFORM an action, consuming its input-files and building its output-files"))
  (define-convenience-action-methods perform (operation component))

  (defmethod perform :around ((o operation) (c component))
    (while-visiting-action (o c) (call-next-method)))
  (defmethod perform :before ((o operation) (c component))
    (ensure-all-directories-exist (output-files o c)))
  (defmethod perform :after ((o operation) (c component))
    (mark-operation-done o c))
  (defmethod perform ((o operation) (c parent-component))
    nil)
  (defmethod perform ((o operation) (c source-file))
    ;; For backward compatibility, don't error on operations that don't specify propagation.
    (when (typep o '(or downward-operation upward-operation sideway-operation
                     selfward-operation non-propagating-operation))
      (sysdef-error
       (compatfmt "~@<Required method ~S not implemented for ~/asdf-action:format-action/~@:>")
       'perform (make-action o c))))

  ;; The restarts of the perform-with-restarts variant matter in an interactive context.
  ;; The retry strategies of p-w-r itself, and/or the background workers of a multiprocess build
  ;; may call perform directly rather than call p-w-r.
  (defgeneric perform-with-restarts (operation component)
    (:documentation "PERFORM an action in a context where suitable restarts are in place."))
  (defmethod perform-with-restarts (operation component)
    (perform operation component))
  (defmethod perform-with-restarts :around (operation component)
    (loop
      (restart-case
          (return (call-next-method))
        (retry ()
          :report
          (lambda (s)
            (format s (compatfmt "~@<Retry ~A.~@:>")
                    (action-description operation component))))
        (accept ()
          :report
          (lambda (s)
            (format s (compatfmt "~@<Continue, treating ~A as having been successful.~@:>")
                    (action-description operation component)))
          (mark-operation-done operation component)
          (return))))))
;;;; -------------------------------------------------------------------------
;;;; Actions to build Common Lisp software

(uiop/package:define-package :asdf/lisp-action
  (:recycle :asdf/lisp-action :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/session
   :asdf/component :asdf/system :asdf/operation :asdf/action)
  (:export
   #:try-recompiling
   #:cl-source-file #:cl-source-file.cl #:cl-source-file.lsp
   #:basic-load-op #:basic-compile-op
   #:load-op #:prepare-op #:compile-op #:test-op #:load-source-op #:prepare-source-op
   #:call-with-around-compile-hook
   #:perform-lisp-compilation #:perform-lisp-load-fasl #:perform-lisp-load-source
   #:lisp-compilation-output-files))
(in-package :asdf/lisp-action)


;;;; Component classes
(with-upgradability ()
  (defclass cl-source-file (source-file)
    ((type :initform "lisp"))
    (:documentation "Component class for a Common Lisp source file (using type \"lisp\")"))
  (defclass cl-source-file.cl (cl-source-file)
    ((type :initform "cl"))
    (:documentation "Component class for a Common Lisp source file using type \"cl\""))
  (defclass cl-source-file.lsp (cl-source-file)
    ((type :initform "lsp"))
    (:documentation "Component class for a Common Lisp source file using type \"lsp\"")))


;;;; Operation classes
(with-upgradability ()
  (defclass basic-load-op (operation) ()
    (:documentation "Base class for operations that apply the load-time effects of a file"))
  (defclass basic-compile-op (operation) ()
    (:documentation "Base class for operations that apply the compile-time effects of a file")))


;;; Our default operations: loading into the current lisp image
(with-upgradability ()
  (defclass prepare-op (upward-operation sideway-operation)
    ((sideway-operation :initform 'load-op :allocation :class))
    (:documentation "Load the dependencies for the COMPILE-OP or LOAD-OP of a given COMPONENT."))
  (defclass load-op (basic-load-op downward-operation selfward-operation)
    ;; NB: even though compile-op depends on prepare-op it is not needed-in-image-p,
    ;; so we need to directly depend on prepare-op for its side-effects in the current image.
    ((selfward-operation :initform '(prepare-op compile-op) :allocation :class))
    (:documentation "Operation for loading the compiled FASL for a Lisp file"))
  (defclass compile-op (basic-compile-op downward-operation selfward-operation)
    ((selfward-operation :initform 'prepare-op :allocation :class))
    (:documentation "Operation for compiling a Lisp file to a FASL"))


  (defclass prepare-source-op (upward-operation sideway-operation)
    ((sideway-operation :initform 'load-source-op :allocation :class))
    (:documentation "Operation for loading the dependencies of a Lisp file as source."))
  (defclass load-source-op (basic-load-op downward-operation selfward-operation)
    ((selfward-operation :initform 'prepare-source-op :allocation :class))
    (:documentation "Operation for loading a Lisp file as source."))

  (defclass test-op (selfward-operation)
    ((selfward-operation :initform 'load-op :allocation :class))
    (:documentation "Operation for running the tests for system.
If the tests fail, an error will be signaled.")))


;;;; Methods for prepare-op, compile-op and load-op

;;; prepare-op
(with-upgradability ()
  (defmethod action-description ((o prepare-op) (c component))
    (format nil (compatfmt "~@<loading dependencies of ~3i~_~A~@:>") c))
  (defmethod perform ((o prepare-op) (c component))
    nil)
  (defmethod input-files ((o prepare-op) (s system))
    (if-let (it (system-source-file s)) (list it))))

;;; compile-op
(with-upgradability ()
  (defmethod action-description ((o compile-op) (c component))
    (format nil (compatfmt "~@<compiling ~3i~_~A~@:>") c))
  (defmethod action-description ((o compile-op) (c parent-component))
    (format nil (compatfmt "~@<completing compilation for ~3i~_~A~@:>") c))
  (defgeneric call-with-around-compile-hook (component thunk)
    (:documentation "A method to be called around the PERFORM'ing of actions that apply the
compile-time side-effects of file (i.e., COMPILE-OP or LOAD-SOURCE-OP). This method can be used
to setup readtables and other variables that control reading, macroexpanding, and compiling, etc.
Note that it will NOT be called around the performing of LOAD-OP."))
  (defmethod call-with-around-compile-hook ((c component) function)
    (call-around-hook (around-compile-hook c) function))
  (defun perform-lisp-compilation (o c)
    "Perform the compilation of the Lisp file associated to the specified action (O . C)."
    (let (;; Before 2.26.53, that was unfortunately component-pathname. Now,
          ;; we consult input-files, the first of which should be the one to compile-file
          (input-file (first (input-files o c)))
          ;; On some implementations, there are more than one output-file,
          ;; but the first one should always be the primary fasl that gets loaded.
          (outputs (output-files o c)))
      (multiple-value-bind (output warnings-p failure-p)
          (destructuring-bind
              (output-file
               &optional
                 #+(or clasp ecl mkcl) object-file
                 #+clisp lib-file
                 warnings-file &rest rest) outputs
            ;; Allow for extra outputs that are not of type warnings-file
            ;; The way we do it is kludgy. In ASDF4, output-files shall not be positional.
            (declare (ignore rest))
            (when warnings-file
              (unless (equal (pathname-type warnings-file) (warnings-file-type))
                (setf warnings-file nil)))
            (call-with-around-compile-hook
             c #'(lambda (&rest flags)
                   (apply 'compile-file* input-file
                          :output-file output-file
                          :external-format (component-external-format c)
                          :warnings-file warnings-file
                          (append
                           #+clisp (list :lib-file lib-file)
                           #+(or clasp ecl mkcl) (list :object-file object-file)
                           flags)))))
        (check-lisp-compile-results output warnings-p failure-p
                                    "~/asdf-action::format-action/" (list (cons o c))))))
  (defun report-file-p (f)
    "Is F a build report file containing, e.g., warnings to check?"
    (equalp (pathname-type f) "build-report"))
  (defun perform-lisp-warnings-check (o c)
    "Check the warnings associated with the dependencies of an action."
    (let* ((expected-warnings-files (remove-if-not #'warnings-file-p (input-files o c)))
           (actual-warnings-files (loop :for w :in expected-warnings-files
                                        :when (get-file-stamp w)
                                          :collect w
                                        :else :do (warn "Missing warnings file ~S while ~A"
                                                        w (action-description o c)))))
      (check-deferred-warnings actual-warnings-files)
      (let* ((output (output-files o c))
             (report (find-if #'report-file-p output)))
        (when report
          (with-open-file (s report :direction :output :if-exists :supersede)
            (format s ":success~%"))))))
  (defmethod perform ((o compile-op) (c cl-source-file))
    (perform-lisp-compilation o c))
  (defun lisp-compilation-output-files (o c)
    "Compute the output-files for compiling the Lisp file for the specified action (O . C),
an OPERATION and a COMPONENT."
    (let* ((i (first (input-files o c)))
           (f (compile-file-pathname
               i #+clasp :output-type #+ecl :type #+(or clasp ecl) :fasl
               #+mkcl :fasl-p #+mkcl t)))
      `(,f ;; the fasl is the primary output, in first position
        #+clasp
        ,@(unless nil ;; was (use-ecl-byte-compiler-p)
            `(,(compile-file-pathname i :output-type :object)))
        #+clisp
        ,@`(,(make-pathname :type "lib" :defaults f))
        #+ecl
        ,@(unless (use-ecl-byte-compiler-p)
            `(,(compile-file-pathname i :type :object)))
        #+mkcl
        ,(compile-file-pathname i :fasl-p nil) ;; object file
        ,@(when (and *warnings-file-type* (not (builtin-system-p (component-system c))))
            `(,(make-pathname :type *warnings-file-type* :defaults f))))))
  (defmethod output-files ((o compile-op) (c cl-source-file))
    (lisp-compilation-output-files o c))
  (defmethod perform ((o compile-op) (c static-file))
    nil)

  ;; Performing compile-op on a system will check the deferred warnings for the system
  (defmethod perform ((o compile-op) (c system))
    (when (and *warnings-file-type* (not (builtin-system-p c)))
      (perform-lisp-warnings-check o c)))
  (defmethod input-files ((o compile-op) (c system))
    (when (and *warnings-file-type* (not (builtin-system-p c)))
      ;; The most correct way to do it would be to use:
      ;; (collect-dependencies o c :other-systems nil :keep-operation 'compile-op :keep-component 'cl-source-file)
      ;; but it's expensive and we don't care too much about file order or ASDF extensions.
      (loop :for sub :in (sub-components c :type 'cl-source-file)
            :nconc (remove-if-not 'warnings-file-p (output-files o sub)))))
  (defmethod output-files ((o compile-op) (c system))
    (when (and *warnings-file-type* (not (builtin-system-p c)))
      (if-let ((pathname (component-pathname c)))
        (list (subpathname pathname (coerce-filename c) :type "build-report"))))))

;;; load-op
(with-upgradability ()
  (defmethod action-description ((o load-op) (c cl-source-file))
    (format nil (compatfmt "~@<loading FASL for ~3i~_~A~@:>") c))
  (defmethod action-description ((o load-op) (c parent-component))
    (format nil (compatfmt "~@<completing load for ~3i~_~A~@:>") c))
  (defmethod action-description ((o load-op) (c component))
    (format nil (compatfmt "~@<loading ~3i~_~A~@:>") c))
  (defmethod perform-with-restarts ((o load-op) (c cl-source-file))
    (loop
      (restart-case
          (return (call-next-method))
        (try-recompiling ()
          :report (lambda (s)
                    (format s "Recompile ~a and try loading it again"
                            (component-name c)))
          (perform (find-operation o 'compile-op) c)))))
  (defun perform-lisp-load-fasl (o c)
    "Perform the loading of a FASL associated to specified action (O . C),
an OPERATION and a COMPONENT."
    (if-let (fasl (first (input-files o c)))
      (load* fasl)))
  (defmethod perform ((o load-op) (c cl-source-file))
    (perform-lisp-load-fasl o c))
  (defmethod perform ((o load-op) (c static-file))
    nil))


;;;; prepare-source-op, load-source-op

;;; prepare-source-op
(with-upgradability ()
  (defmethod action-description ((o prepare-source-op) (c component))
    (format nil (compatfmt "~@<loading source for dependencies of ~3i~_~A~@:>") c))
  (defmethod input-files ((o prepare-source-op) (s system))
    (if-let (it (system-source-file s)) (list it)))
  (defmethod perform ((o prepare-source-op) (c component))
    nil))

;;; load-source-op
(with-upgradability ()
  (defmethod action-description ((o load-source-op) (c component))
    (format nil (compatfmt "~@<Loading source of ~3i~_~A~@:>") c))
  (defmethod action-description ((o load-source-op) (c parent-component))
    (format nil (compatfmt "~@<Loaded source of ~3i~_~A~@:>") c))
  (defun perform-lisp-load-source (o c)
    "Perform the loading of a Lisp file as associated to specified action (O . C)"
    (call-with-around-compile-hook
     c #'(lambda ()
           (load* (first (input-files o c))
                  :external-format (component-external-format c)))))

  (defmethod perform ((o load-source-op) (c cl-source-file))
    (perform-lisp-load-source o c))
  (defmethod perform ((o load-source-op) (c static-file))
    nil))


;;;; test-op
(with-upgradability ()
  (defmethod perform ((o test-op) (c component))
    nil)
  (defmethod operation-done-p ((o test-op) (c system))
    "Testing a system is _never_ done."
    nil))
;;;; -------------------------------------------------------------------------
;;;; Finding components

(uiop/package:define-package :asdf/find-component
  (:recycle :asdf/find-component :asdf/find-system :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/session
   :asdf/component :asdf/system :asdf/system-registry)
  (:export
   #:find-component
   #:resolve-dependency-name #:resolve-dependency-spec
   #:resolve-dependency-combination
   ;; Conditions
   #:missing-component #:missing-requires #:missing-parent #:missing-component-of-version #:retry
   #:missing-dependency #:missing-dependency-of-version
   #:missing-requires #:missing-parent
   #:missing-required-by #:missing-version))
(in-package :asdf/find-component)

;;;; Missing component conditions

(with-upgradability ()
  (define-condition missing-component (system-definition-error)
    ((requires :initform "(unnamed)" :reader missing-requires :initarg :requires)
     (parent :initform nil :reader missing-parent :initarg :parent)))

  (define-condition missing-component-of-version (missing-component)
    ((version :initform nil :reader missing-version :initarg :version)))

  (define-condition missing-dependency (missing-component)
    ((required-by :initarg :required-by :reader missing-required-by)))

  (defmethod print-object ((c missing-dependency) s)
    (format s (compatfmt "~@<~A, required by ~A~@:>")
            (call-next-method c nil) (missing-required-by c)))

  (define-condition missing-dependency-of-version (missing-dependency
                                                   missing-component-of-version)
    ())

  (defmethod print-object ((c missing-component) s)
    (format s (compatfmt "~@<Component ~S not found~@[ in ~A~]~@:>")
            (missing-requires c)
            (when (missing-parent c)
              (coerce-name (missing-parent c)))))

  (defmethod print-object ((c missing-component-of-version) s)
    (format s (compatfmt "~@<Component ~S does not match version ~A~@[ in ~A~]~@:>")
            (missing-requires c)
            (missing-version c)
            (when (missing-parent c)
              (coerce-name (missing-parent c))))))


;;;; Finding components

(with-upgradability ()
  (defgeneric resolve-dependency-combination (component combinator arguments)
    (:documentation "Return a component satisfying the dependency specification (COMBINATOR . ARGUMENTS)
in the context of COMPONENT"))

  ;; Methods for find-component

  ;; If the base component is a string, resolve it as a system, then if not nil follow the path.
  (defmethod find-component ((base string) path &key registered)
    (if-let ((s (if registered
                    (registered-system base)
                    (find-system base nil))))
      (find-component s path :registered registered)))

  ;; If the base component is a symbol, coerce it to a name if not nil, and resolve that.
  ;; If nil, use the path as base if not nil, or else return nil.
  (defmethod find-component ((base symbol) path &key registered)
    (cond
      (base (find-component (coerce-name base) path :registered registered))
      (path (find-component path nil :registered registered))
      (t    nil)))

  ;; If the base component is a cons cell, resolve its car, and add its cdr to the path.
  (defmethod find-component ((base cons) path &key registered)
    (find-component (car base) (cons (cdr base) path) :registered registered))

  ;; If the base component is a parent-component and the path a string, find the named child.
  (defmethod find-component ((parent parent-component) (name string) &key registered)
    (declare (ignorable registered))
    (compute-children-by-name parent :only-if-needed-p t)
    (values (gethash name (component-children-by-name parent))))

  ;; If the path is a symbol, coerce it to a name if non-nil, or else just return the base.
  (defmethod find-component (base (name symbol) &key registered)
    (if name
        (find-component base (coerce-name name) :registered registered)
        base))

  ;; If the path is a cons, first resolve its car as path, then its cdr.
  (defmethod find-component ((c component) (name cons) &key registered)
    (find-component (find-component c (car name) :registered registered)
                    (cdr name) :registered registered))

  ;; If the path is a component, return it, disregarding the base.
  (defmethod find-component ((base t) (actual component) &key registered)
    (declare (ignorable registered))
    actual)

  ;; Resolve dependency NAME in the context of a COMPONENT, with given optional VERSION constraint.
  ;; This (private) function is used below by RESOLVE-DEPENDENCY-SPEC and by the :VERSION spec.
  (defun resolve-dependency-name (component name &optional version)
    (loop
      (restart-case
          (return
            (let ((comp (find-component (component-parent component) name)))
              (unless comp
                (error 'missing-dependency
                       :required-by component
                       :requires name))
              (when version
                (unless (version-satisfies comp version)
                  (error 'missing-dependency-of-version
                         :required-by component
                         :version version
                         :requires name)))
              comp))
        (retry ()
          :report (lambda (s)
                    (format s (compatfmt "~@<Retry loading ~3i~_~A.~@:>") name))
          :test
          (lambda (c)
            (or (null c)
                (and (typep c 'missing-dependency)
                     (eq (missing-required-by c) component)
                     (equal (missing-requires c) name))))
          (unless (component-parent component)
            (let ((name (coerce-name name)))
              (unset-asdf-cache-entry `(find-system ,name))))))))

  ;; Resolve dependency specification DEP-SPEC in the context of COMPONENT.
  ;; This is notably used by MAP-DIRECT-DEPENDENCIES to process the results of COMPONENT-DEPENDS-ON
  ;; and by PARSE-DEFSYSTEM to process DEFSYSTEM-DEPENDS-ON.
  (defun resolve-dependency-spec (component dep-spec)
    (let ((component (find-component () component)))
      (if (atom dep-spec)
          (resolve-dependency-name component dep-spec)
          (resolve-dependency-combination component (car dep-spec) (cdr dep-spec)))))

  ;; Methods for RESOLVE-DEPENDENCY-COMBINATION to parse lists as dependency specifications.
  (defmethod resolve-dependency-combination (component combinator arguments)
    (parameter-error (compatfmt "~@<In ~S, bad dependency ~S for ~S~@:>")
                     'resolve-dependency-combination (cons combinator arguments) component))

  (defmethod resolve-dependency-combination (component (combinator (eql :feature)) arguments)
    (when (featurep (first arguments))
      (resolve-dependency-spec component (second arguments))))

  (defmethod resolve-dependency-combination (component (combinator (eql :version)) arguments)
    (resolve-dependency-name component (first arguments) (second arguments)))) ;; See lp#527788

;;;; -------------------------------------------------------------------------
;;;; Forcing

(uiop/package:define-package :asdf/forcing
  (:recycle :asdf/forcing :asdf/plan :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/session
        :asdf/component :asdf/operation :asdf/system :asdf/system-registry)
  (:export
   #:forcing #:make-forcing #:forced #:forced-not #:performable-p
   #:normalize-forced-systems #:normalize-forced-not-systems
   #:action-forced-p #:action-forced-not-p))
(in-package :asdf/forcing)

;;;; Forcing
(with-upgradability ()
  (defclass forcing ()
    (;; Can plans using this forcing be PERFORMed? A plan that has different force and force-not
     ;; settings than the session can only be used for read-only queries that do not cause the
     ;; status of any action to be raised.
     (performable-p :initform nil :initarg :performable-p :reader performable-p)
     ;; Parameters
     (parameters :initform nil :initarg :parameters :reader parameters)
     ;; Table of systems specified via :force arguments
     (forced :initarg :forced :reader forced)
     ;; Table of systems specified via :force-not argument (and/or immutable)
     (forced-not :initarg :forced-not :reader forced-not)))

  (defgeneric action-forced-p (forcing operation component)
    (:documentation "Is this action forced to happen in this plan?"))
  (defgeneric action-forced-not-p (forcing operation component)
    (:documentation "Is this action forced to not happen in this plan?
Takes precedence over action-forced-p."))

  (defun normalize-forced-systems (force system)
    "Given a SYSTEM on which operate is called and the specified FORCE argument,
extract a hash-set of systems that are forced, or a predicate on system names,
or NIL if none are forced, or :ALL if all are."
    (etypecase force
      ((or (member nil :all) hash-table function) force)
      (cons (list-to-hash-set (mapcar #'coerce-name force)))
      ((eql t) (when system (list-to-hash-set (list (coerce-name system)))))))

  (defun normalize-forced-not-systems (force-not system)
    "Given a SYSTEM on which operate is called, the specified FORCE-NOT argument,
and the set of IMMUTABLE systems, extract a hash-set of systems that are effectively forced-not,
or predicate on system names, or NIL if none are forced, or :ALL if all are."
    (let ((requested
            (etypecase force-not
              ((or (member nil :all) hash-table function) force-not)
              (cons (list-to-hash-set (mapcar #'coerce-name force-not)))
              ((eql t) (if system (let ((name (coerce-name system)))
                                    #'(lambda (x) (not (equal x name))))
                           :all)))))
      (if (and *immutable-systems* requested)
          #'(lambda (x) (or (call-function requested x)
                            (call-function *immutable-systems* x)))
          (or *immutable-systems* requested))))

  ;; TODO: shouldn't we be looking up the primary system name, rather than the system name?
  (defun action-override-p (forcing operation component override-accessor)
    "Given a plan, an action, and a function that given the plan accesses a set of overrides,
i.e. force or force-not, see if the override applies to the current action."
    (declare (ignore operation))
    (call-function (funcall override-accessor forcing)
                   (coerce-name (component-system (find-component () component)))))

  (defmethod action-forced-p (forcing operation component)
    (and
     ;; Did the user ask us to re-perform the action?
     (action-override-p forcing operation component 'forced)
     ;; You really can't force a builtin system and :all doesn't apply to it.
     (not (builtin-system-p (component-system component)))))

  (defmethod action-forced-not-p (forcing operation component)
    ;; Did the user ask us to not re-perform the action?
    ;; NB: force-not takes precedence over force, as it should
    (action-override-p forcing operation component 'forced-not))

  ;; Null forcing means no forcing either way
  (defmethod action-forced-p ((forcing null) (operation operation) (component component))
    nil)
  (defmethod action-forced-not-p ((forcing null) (operation operation) (component component))
    nil)

  (defun or-function (fun1 fun2)
    (cond
      ((or (null fun2) (eq fun1 :all)) fun1)
      ((or (null fun1) (eq fun2 :all)) fun2)
      (t #'(lambda (x) (or (call-function fun1 x) (call-function fun2 x))))))

  (defun make-forcing (&key performable-p system
                         (force nil force-p) (force-not nil force-not-p) &allow-other-keys)
    (let* ((session-forcing (when *asdf-session* (forcing *asdf-session*)))
           (system (and system (coerce-name system)))
           (forced (normalize-forced-systems force system))
           (forced-not (normalize-forced-not-systems force-not system))
           (parameters `(,@(when force `(:force ,force))
                         ,@(when force-not `(:force-not ,force-not))
                         ,@(when (or (eq force t) (eq force-not t)) `(:system ,system))
                         ,@(when performable-p `(:performable-p t))))
           forcing)
      (cond
        ((not session-forcing)
         (setf forcing (make-instance 'forcing
                                      :performable-p performable-p :parameters parameters
                                      :forced forced :forced-not forced-not))
         (when (and performable-p *asdf-session*)
           (setf (forcing *asdf-session*) forcing)))
        (performable-p
         (when (and (not (equal parameters (parameters session-forcing)))
                    (or force-p force-not-p))
           (parameter-error "~*~S and ~S arguments not allowed in a nested call to ~3:*~S ~
unless identically to toplevel"
                            (find-symbol* :operate :asdf) :force :force-not))
         (setf forcing session-forcing))
        (t
         (setf forcing (make-instance 'forcing
                           ;; Combine force and force-not with values from the toplevel-plan
                           :parameters `(,@parameters :on-top-of ,(parameters session-forcing))
                           :forced (or-function (forced session-forcing) forced)
                           :forced-not (or-function (forced-not session-forcing) forced-not)))))
      forcing))

  (defmethod print-object ((forcing forcing) stream)
    (print-unreadable-object (forcing stream :type t)
      (format stream "~{~S~^ ~}" (parameters forcing))))

  ;; During upgrade, the *asdf-session* may legitimately be NIL, so we must handle that case.
  (defmethod forcing ((x null))
    (if-let (session (toplevel-asdf-session))
      (forcing session)
      (make-forcing :performable-p t)))

  ;; When performing a plan that is a list of actions, use the toplevel asdf sesssion forcing.
  (defmethod forcing ((x cons)) (forcing (toplevel-asdf-session))))
;;;; -------------------------------------------------------------------------
;;;; Plan

(uiop/package:define-package :asdf/plan
  ;; asdf/action below is needed for required-components, traverse-action and traverse-sub-actions
  ;; that used to live there before 3.2.0.
  (:recycle :asdf/plan :asdf/action :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/session
        :asdf/component :asdf/operation :asdf/action :asdf/lisp-action
        :asdf/system :asdf/system-registry :asdf/find-component :asdf/forcing)
  (:export
   #:plan #:plan-traversal #:sequential-plan #:*plan-class*
   #:action-status #:status-stamp #:status-index #:status-done-p #:status-keep-p #:status-need-p
   #:action-already-done-p
   #:+status-good+ #:+status-todo+ #:+status-void+
   #:system-out-of-date #:action-up-to-date-p
   #:circular-dependency #:circular-dependency-actions
   #:needed-in-image-p
   #:map-direct-dependencies #:reduce-direct-dependencies #:direct-dependencies
   #:compute-action-stamp #:traverse-action #:record-dependency
   #:make-plan #:plan-actions #:plan-actions-r #:perform-plan #:mark-as-done
   #:required-components #:filtered-sequential-plan
   #:plan-component-type #:plan-keep-operation #:plan-keep-component))
(in-package :asdf/plan)

;;;; Generic plan traversal class
(with-upgradability ()
  (defclass plan () ()
    (:documentation "Base class for a plan based on which ASDF can build a system"))
  (defclass plan-traversal (plan)
    (;; The forcing parameters for this plan. Also indicates whether the plan is performable,
     ;; in which case the forcing is the same as for the entire session.
     (forcing :initform (forcing (toplevel-asdf-session)) :initarg :forcing :reader forcing))
    (:documentation "Base class for plans that simply traverse dependencies"))
  ;; Sequential plans (the default)
  (defclass sequential-plan (plan-traversal)
    ((actions-r :initform nil :accessor plan-actions-r))
    (:documentation "Simplest, default plan class, accumulating a sequence of actions"))

  (defgeneric plan-actions (plan)
    (:documentation "Extract from a plan a list of actions to perform in sequence"))
  (defmethod plan-actions ((plan list))
    plan)
  (defmethod plan-actions ((plan sequential-plan))
    (reverse (plan-actions-r plan)))

  (defgeneric record-dependency (plan operation component)
    (:documentation "Record an action as a dependency in the current plan"))

  ;; No need to record a dependency to build a full graph, just accumulate nodes in order.
  (defmethod record-dependency ((plan sequential-plan) (o operation) (c component))
    (values)))

(when-upgrading (:version "3.3.0")
  (defmethod initialize-instance :after ((plan plan-traversal) &key &allow-other-keys)))


;;;; Planned action status
(with-upgradability ()
  (defclass action-status ()
    ((bits
      :type fixnum :initarg :bits :reader status-bits
      :documentation "bitmap describing the status of the action.")
     (stamp
      :type (or integer boolean) :initarg :stamp :reader status-stamp
      :documentation "STAMP associated with the ACTION if it has been completed already in some
previous session or image, T if it was done and builtin the image, or NIL if it needs to be done.")
     (level
      :type fixnum :initarg :level :initform 0 :reader status-level
      :documentation "the highest (operate-level) at which the action was needed")
     (index
      :type (or integer null) :initarg :index :initform nil :reader status-index
      :documentation "INDEX associated with the ACTION in the current session,
or NIL if no the status is considered outside of a specific plan."))
    (:documentation "Status of an action in a plan"))

  ;; STAMP   KEEP-P DONE-P NEED-P     symbol bitmap  previously   currently
  ;; not-nil   T      T      T     =>  GOOD     7    up-to-date   done (e.g. file previously loaded)
  ;; not-nil   T      T     NIL    =>  HERE     6    up-to-date   unplanned yet done
  ;; not-nil   T     NIL     T     =>  REDO     5    up-to-date   planned (e.g. file to load)
  ;; not-nil   T     NIL    NIL    =>  SKIP     4    up-to-date   unplanned (e.g. file compiled)
  ;; not-nil  NIL     T      T     =>  DONE     3    out-of-date  done
  ;; not-nil  NIL     T     NIL    =>  WHAT     2    out-of-date  unplanned yet done(?)
  ;;  NIL     NIL    NIL     T     =>  TODO     1    out-of-date  planned
  ;;  NIL     NIL    NIL    NIL    =>  VOID     0    out-of-date  unplanned
  ;;
  ;; Note that a VOID status cannot happen as part of a transitive dependency of a wanted node
  ;; while traversing a node with TRAVERSE-ACTION; it can only happen while checking whether an
  ;; action is up-to-date with ACTION-UP-TO-DATE-P.
  ;;
  ;; When calling TRAVERSE-ACTION, the +need-bit+ is set,
  ;; unless the action is up-to-date and not needed-in-image (HERE, SKIP).
  ;; When PERFORMing an action, the +done-bit+ is set.
  ;; When the +need-bit+ is set but not the +done-bit+, the level slot indicates which level of
  ;; OPERATE it was last marked needed for; if it happens to be needed at a higher-level, then
  ;; its urgency (and that of its transitive dependencies) must be escalated so that it will be
  ;; done before the end of this level of operate.
  ;;
  ;; Also, when no ACTION-STATUS is associated to an action yet, NIL serves as a bottom value.
  ;;
  (defparameter +keep-bit+ 4)
  (defparameter +done-bit+ 2)
  (defparameter +need-bit+ 1)
  (defparameter +good-bits+ 7)
  (defparameter +todo-bits+ 1)
  (defparameter +void-bits+ 0)

  (defparameter +status-good+
    (make-instance 'action-status :bits +good-bits+ :stamp t))
  (defparameter +status-todo+
    (make-instance 'action-status :bits +todo-bits+ :stamp nil))
  (defparameter +status-void+
    (make-instance 'action-status :bits +void-bits+ :stamp nil)))

(with-upgradability ()
  (defun make-action-status (&key bits stamp (level 0) index)
    (check-type bits (integer 0 7))
    (check-type stamp (or integer boolean))
    (check-type level (integer 0 #.most-positive-fixnum))
    (check-type index (or integer null))
    (assert (eq (null stamp) (zerop (logand bits #.(logior +keep-bit+ +done-bit+)))) ()
            "Bad action-status :bits ~S :stamp ~S" bits stamp)
    (block nil
      (when (and (null index) (zerop level))
        (case bits
          (#.+void-bits+ (return +status-void+))
          (#.+todo-bits+ (return +status-todo+))
          (#.+good-bits+ (when (eq stamp t) (return +status-good+)))))
      (make-instance 'action-status :bits bits :stamp stamp :level level :index index)))

  (defun status-keep-p (status)
    (plusp (logand (status-bits status) #.+keep-bit+)))
  (defun status-done-p (status)
    (plusp (logand (status-bits status) #.+done-bit+)))
  (defun status-need-p (status)
    (plusp (logand (status-bits status) #.+need-bit+)))

  (defun merge-action-status (status1 status2) ;; status-and
    "Return the earliest status later than both status1 and status2"
    (make-action-status
     :bits (logand (status-bits status1) (status-bits status2))
     :stamp (latest-timestamp (status-stamp status1) (status-stamp status2))
     :level (min (status-level status1) (status-level status2))
     :index (or (status-index status1) (status-index status2))))

  (defun mark-status-needed (status &optional (level (operate-level))) ;; limited status-or
    "Return the same status but with the need bit set, for the given level"
    (if (and (status-need-p status)
             (>= (status-level status) level))
        status
        (make-action-status
         :bits (logior (status-bits status) +need-bit+)
         :level (max level (status-level status))
         :stamp (status-stamp status)
         :index (status-index status))))

  (defmethod print-object ((status action-status) stream)
    (print-unreadable-object (status stream :type t)
      (with-slots (bits stamp level index) status
        (format stream "~{~S~^ ~}" `(:bits ,bits :stamp ,stamp :level ,level :index ,index)))))

  (defgeneric action-status (plan operation component)
    (:documentation "Returns the ACTION-STATUS associated to the action of OPERATION on COMPONENT
in the PLAN, or NIL if the action wasn't visited yet as part of the PLAN."))

  (defgeneric (setf action-status) (new-status plan operation component)
    (:documentation "Sets the ACTION-STATUS associated to
the action of OPERATION on COMPONENT in the PLAN"))

  (defmethod action-status ((plan null) (o operation) (c component))
    (multiple-value-bind (stamp done-p) (component-operation-time o c)
      (if done-p
          (make-action-status :bits #.+keep-bit+ :stamp stamp)
          +status-void+)))

  (defmethod (setf action-status) (new-status (plan null) (o operation) (c component))
    (let ((times (component-operation-times c)))
      (if (status-done-p new-status)
          (setf (gethash o times) (status-stamp new-status))
          (remhash o times)))
    new-status)

  ;; Handle FORCED-NOT: it makes an action return its current timestamp as status
  (defmethod action-status ((p plan) (o operation) (c component))
    ;; TODO: should we instead test something like:
    ;; (action-forced-not-p plan operation (primary-system component))
    (or (gethash (make-action o c) (visited-actions *asdf-session*))
        (when (action-forced-not-p (forcing p) o c)
          (let ((status (action-status nil o c)))
            (setf (gethash (make-action o c) (visited-actions *asdf-session*))
                  (make-action-status
                   :bits +good-bits+
                   :stamp (or (and status (status-stamp status)) t)
                   :index (incf (total-action-count *asdf-session*))))))))

  (defmethod (setf action-status) (new-status (p plan) (o operation) (c component))
    (setf (gethash (make-action o c) (visited-actions *asdf-session*)) new-status))

  (defmethod (setf action-status) :after
      (new-status (p sequential-plan) (o operation) (c component))
    (unless (status-done-p new-status)
      (push (make-action o c) (plan-actions-r p)))))


;;;; Is the action needed in this image?
(with-upgradability ()
  (defgeneric needed-in-image-p (operation component)
    (:documentation "Is the action of OPERATION on COMPONENT needed in the current image
to be meaningful, or could it just as well have been done in another Lisp image?"))

  (defmethod needed-in-image-p ((o operation) (c component))
    ;; We presume that actions that modify the filesystem don't need be run
    ;; in the current image if they have already been done in another,
    ;; and can be run in another process (e.g. a fork),
    ;; whereas those that don't are meant to side-effect the current image and can't.
    (not (output-files o c))))


;;;; Visiting dependencies of an action and computing action stamps
(with-upgradability ()
  (defun* (map-direct-dependencies) (operation component fun)
    "Call FUN on all the valid dependencies of the given action in the given plan"
    (loop* :for (dep-o-spec . dep-c-specs) :in (component-depends-on operation component)
      :for dep-o = (find-operation operation dep-o-spec)
      :when dep-o
      :do (loop :for dep-c-spec :in dep-c-specs
            :for dep-c = (and dep-c-spec (resolve-dependency-spec component dep-c-spec))
            :when (action-valid-p dep-o dep-c)
            :do (funcall fun dep-o dep-c))))

  (defun* (reduce-direct-dependencies) (operation component combinator seed)
    "Reduce the direct dependencies to a value computed by iteratively calling COMBINATOR
for each dependency action on the dependency's operation and component and an accumulator
initialized with SEED."
    (map-direct-dependencies
     operation component
     #'(lambda (dep-o dep-c) (setf seed (funcall combinator dep-o dep-c seed))))
    seed)

  (defun* (direct-dependencies) (operation component)
    "Compute a list of the direct dependencies of the action within the plan"
    (reverse (reduce-direct-dependencies operation component #'acons nil)))

  ;; In a distant future, get-file-stamp, component-operation-time and latest-stamp
  ;; shall also be parametrized by the plan, or by a second model object,
  ;; so they need not refer to the state of the filesystem,
  ;; and the stamps could be cryptographic checksums rather than timestamps.
  ;; Such a change remarkably would only affect COMPUTE-ACTION-STAMP.

  (defmethod compute-action-stamp (plan (o operation) (c component) &key just-done)
    ;; Given an action, figure out at what time in the past it has been done,
    ;; or if it has just been done, return the time that it has.
    ;; Returns two values:
    ;; 1- the TIMESTAMP of the action if it has already been done and is up to date,
    ;;   or NIL is either hasn't been done or is out of date.
    ;;   (An ASDF extension could use a cryptographic digest instead.)
    ;; 2- the DONE-IN-IMAGE-P boolean flag that is T if the action has already been done
    ;;   in the current image, or NIL if it hasn't.
    ;; Note that if e.g. LOAD-OP only depends on up-to-date files, but
    ;; hasn't been done in the current image yet, then it can have a non-NIL timestamp,
    ;; yet a NIL done-in-image-p flag: we can predict what timestamp it will have once loaded,
    ;; i.e. that of the input-files.
    ;; If just-done is NIL, these values return are the notional fields of
    ;; a KEEP, REDO or TODO status (VOID is possible, but probably an error).
    ;; If just-done is T, they are the notional fields of DONE status
    ;; (or, if something went wrong, TODO).
    (nest
     (block ())
     (let* ((dep-status ; collect timestamp from dependencies (or T if forced or out-of-date)
             (reduce-direct-dependencies
              o c
              #'(lambda (do dc status)
                  ;; out-of-date dependency: don't bother looking further
                  (let ((action-status (action-status plan do dc)))
                    (cond
                      ((and action-status (or (status-keep-p action-status)
                                              (and just-done (status-stamp action-status))))
                       (merge-action-status action-status status))
                      (just-done
                       ;; It's OK to lose some ASDF action stamps during self-upgrade
                       (unless (equal "asdf" (primary-system-name dc))
                         (warn "Computing just-done stamp in plan ~S for action ~S, but dependency ~S wasn't done yet!"
                               plan
                               (action-path (make-action o c))
                               (action-path (make-action do dc))))
                       status)
                      (t
                       (return (values nil nil))))))
              +status-good+))
            (dep-stamp (status-stamp dep-status))))
     (let* (;; collect timestamps from inputs, and exit early if any is missing
            (in-files (input-files o c))
            (in-stamps (mapcar #'get-file-stamp in-files))
            (missing-in (loop :for f :in in-files :for s :in in-stamps :unless s :collect f))
            (latest-in (timestamps-latest (cons dep-stamp in-stamps))))
       (when (and missing-in (not just-done)) (return (values nil nil))))
     (let* (;; collect timestamps from outputs, and exit early if any is missing
            (out-files (remove-if 'null (output-files o c)))
            (out-stamps (mapcar (if just-done 'register-file-stamp 'get-file-stamp) out-files))
            (missing-out (loop :for f :in out-files :for s :in out-stamps :unless s :collect f))
            (earliest-out (timestamps-earliest out-stamps)))
       (when (and missing-out (not just-done)) (return (values nil nil))))
     (let (;; Time stamps from the files at hand, and whether any is missing
           (all-present (not (or missing-in missing-out)))
           ;; Has any input changed since we last generated the files?
           ;; Note that we use timestamp<= instead of timestamp< to play nice with generated files.
           ;; Any race condition is intrinsic to the limited timestamp resolution.
           (up-to-date-p (timestamp<= latest-in earliest-out))
           ;; If everything is up to date, the latest of inputs and outputs is our stamp
           (done-stamp (timestamps-latest (cons latest-in out-stamps))))
       ;; Warn if some files are missing:
       ;; either our model is wrong or some other process is messing with our files.
       (when (and just-done (not all-present))
         ;; Shouldn't that be an error instead?
         (warn "~A completed without ~:[~*~;~*its input file~:p~2:*~{ ~S~}~*~]~
                ~:[~; or ~]~:[~*~;~*its output file~:p~2:*~{ ~S~}~*~]"
               (action-description o c)
               missing-in (length missing-in) (and missing-in missing-out)
               missing-out (length missing-out))))
     (let (;; There are three kinds of actions:
           (out-op (and out-files t)) ; those that create files on the filesystem
           ;;(image-op (and in-files (null out-files))) ; those that load stuff into the image
           ;;(null-op (and (null out-files) (null in-files))) ; placeholders that do nothing
           ))
     (if (or just-done ;; The done-stamp is valid: if we're just done, or
             (and all-present ;; if all filesystem effects are up-to-date
                  up-to-date-p
                  (operation-done-p o c) ;; and there's no invalidating reason.
                  (not (action-forced-p (forcing (or plan *asdf-session*)) o c))))
         (values done-stamp ;; return the hard-earned timestamp
                 (or just-done
                     out-op ;; A file-creating op is done when all files are up to date.
                     ;; An image-effecting operation is done when
                     (and (status-done-p dep-status) ;; all the dependencies were done, and
                          (multiple-value-bind (perform-stamp perform-done-p)
                              (component-operation-time o c)
                            (and perform-done-p ;; the op was actually run,
                                 (equal perform-stamp done-stamp)))))) ;; with a matching stamp.
         ;; done-stamp invalid: return a timestamp in an indefinite future, action not done yet
         (values nil nil)))))


;;;; The four different actual traversals:
;; * TRAVERSE-ACTION o c T: Ensure all dependencies are either up-to-date in-image, or planned
;; * TRAVERSE-ACTION o c NIL: Ensure all dependencies are up-to-date or planned, in-image or not
;; * ACTION-UP-TO-DATE-P: Check whether some (defsystem-depends-on ?) dependencies are up to date
;; * COLLECT-ACTION-DEPENDENCIES: Get the dependencies (filtered), don't change any status
(with-upgradability ()

  ;; Compute the action status for a newly visited action.
  (defun compute-action-status (plan operation component need-p)
    (multiple-value-bind (stamp done-p)
        (compute-action-stamp plan operation component)
      (assert (or stamp (not done-p)))
      (make-action-status
       :bits (logior (if stamp #.+keep-bit+ 0)
                     (if done-p #.+done-bit+ 0)
                     (if need-p #.+need-bit+ 0))
       :stamp stamp
       :level (operate-level)
       :index (incf (total-action-count *asdf-session*)))))

  ;; TRAVERSE-ACTION, in the context of a given PLAN object that accumulates dependency data,
  ;; visits the action defined by its OPERATION and COMPONENT arguments,
  ;; and all its transitive dependencies (unless already visited),
  ;; in the context of the action being (or not) NEEDED-IN-IMAGE-P,
  ;; i.e. needs to be done in the current image vs merely have been done in a previous image.
  ;;
  ;; TRAVERSE-ACTION updates the VISITED-ACTIONS entries for the action and for all its
  ;; transitive dependencies (that haven't been sufficiently visited so far).
  ;; It does not return any usable value.
  ;;
  ;; Note that for an XCVB-like plan with one-image-per-file-outputting-action,
  ;; the below method would be insufficient, since it assumes a single image
  ;; to traverse each node at most twice; non-niip actions would be traversed only once,
  ;; but niip nodes could be traversed once per image, i.e. once plus once per non-niip action.

  (defun traverse-action (plan operation component needed-in-image-p)
    (block nil
      (unless (action-valid-p operation component) (return))
      ;; Record the dependency. This hook is needed by POIU, which tracks a full dependency graph,
      ;; instead of just a dependency order as in vanilla ASDF.
      ;; TODO: It is also needed to detect OPERATE-in-PERFORM.
      (record-dependency plan operation component)
      (while-visiting-action (operation component) ; maintain context, handle circularity.
        ;; needed-in-image distinguishes b/w things that must happen in the
        ;; current image and those things that simply need to have been done in a previous one.
        (let* ((aniip (needed-in-image-p operation component)) ; action-specific needed-in-image
               ;; effective niip: meaningful for the action and required by the plan as traversed
               (eniip (and aniip needed-in-image-p))
               ;; status: have we traversed that action previously, and if so what was its status?
               (status (action-status plan operation component))
               (level (operate-level)))
          (when (and status
                     (or (status-done-p status) ;; all done
                         (and (status-need-p status) (<= level (status-level status))) ;; already visited
                         (and (status-keep-p status) (not eniip)))) ;; up-to-date and not eniip
            (return)) ; Already visited with sufficient need-in-image level!
          (labels ((visit-action (niip) ; We may visit the action twice, once with niip NIL, then T
                     (map-direct-dependencies ; recursively traverse dependencies
                      operation component #'(lambda (o c) (traverse-action plan o c niip)))
                     ;; AFTER dependencies have been traversed, compute action stamp
                     (let* ((status (if status
                                        (mark-status-needed status level)
                                        (compute-action-status plan operation component t)))
                            (out-of-date-p (not (status-keep-p status)))
                            (to-perform-p (or out-of-date-p (and niip (not (status-done-p status))))))
                       (cond ; it needs be done if it's out of date or needed in image but absent
                         ((and out-of-date-p (not niip)) ; if we need to do it,
                          (visit-action t)) ; then we need to do it *in the (current) image*!
                         (t
                          (setf (action-status plan operation component) status)
                          (when (status-done-p status)
                            (setf (component-operation-time operation component)
                                  (status-stamp status)))
                          (when to-perform-p ; if it needs to be added to the plan, count it
                            (incf (planned-action-count *asdf-session*))
                            (unless aniip ; if it's output-producing, count it
                              (incf (planned-output-action-count *asdf-session*)))))))))
            (visit-action eniip)))))) ; visit the action

  ;; NB: This is not an error, not a warning, but a normal expected condition,
  ;; to be to signaled by FIND-SYSTEM when it detects an out-of-date system,
  ;; *before* it tries to replace it with a new definition.
  (define-condition system-out-of-date (condition)
    ((name :initarg :name :reader component-name))
    (:documentation "condition signaled when a system is detected as being out of date")
    (:report (lambda (c s)
               (format s "system ~A is out of date" (component-name c)))))

  (defun action-up-to-date-p (plan operation component)
    "Check whether an action was up-to-date at the beginning of the session.
Update the VISITED-ACTIONS table with the known status, but don't add anything to the PLAN."
    (block nil
      (unless (action-valid-p operation component) (return t))
      (while-visiting-action (operation component) ; maintain context, handle circularity.
        ;; Do NOT record the dependency: it might be out of date.
        (let ((status (or (action-status plan operation component)
                          (setf (action-status plan operation component)
                                (let ((dependencies-up-to-date-p
                                       (handler-case
                                           (block nil
                                             (map-direct-dependencies
                                              operation component
                                              #'(lambda (o c)
                                                  (unless (action-up-to-date-p plan o c)
                                                    (return nil))))
                                             t)
                                         (system-out-of-date () nil))))
                                  (if dependencies-up-to-date-p
                                      (compute-action-status plan operation component nil)
                                      +status-void+))))))
          (and (status-keep-p status) (status-stamp status)))))))


;;;; Incidental traversals

;;; Making a FILTERED-SEQUENTIAL-PLAN can be used to, e.g., all of the source
;;; files required by a bundling operation.
(with-upgradability ()
  (defclass filtered-sequential-plan (sequential-plan)
    ((component-type :initform t :initarg :component-type :reader plan-component-type)
     (keep-operation :initform t :initarg :keep-operation :reader plan-keep-operation)
     (keep-component :initform t :initarg :keep-component :reader plan-keep-component))
    (:documentation "A variant of SEQUENTIAL-PLAN that only records a subset of actions."))

  (defmethod initialize-instance :after ((plan filtered-sequential-plan)
                                         &key system other-systems)
    ;; Ignore force and force-not, rely on other-systems:
    ;; force traversal of what we're interested in, i.e. current system or also others;
    ;; force-not traversal of what we're not interested in, i.e. other systems unless other-systems.
    (setf (slot-value plan 'forcing)
          (make-forcing :system system :force :all :force-not (if other-systems nil t))))

  (defmethod plan-actions ((plan filtered-sequential-plan))
    (with-slots (keep-operation keep-component) plan
      (loop :for action :in (call-next-method)
        :as o = (action-operation action)
        :as c = (action-component action)
        :when (and (typep o keep-operation) (typep c keep-component))
        :collect (make-action o c))))

  (defun collect-action-dependencies (plan operation component)
    (when (action-valid-p operation component)
      (while-visiting-action (operation component) ; maintain context, handle circularity.
        (let ((action (make-action operation component)))
          (unless (nth-value 1 (gethash action (visited-actions *asdf-session*)))
            (setf (gethash action (visited-actions *asdf-session*)) nil)
            (when (and (typep component (plan-component-type plan))
                       (not (action-forced-not-p (forcing plan) operation component)))
              (map-direct-dependencies operation component
                                       #'(lambda (o c) (collect-action-dependencies plan o c)))
              (push action (plan-actions-r plan))))))))

  (defgeneric collect-dependencies (operation component &key &allow-other-keys)
    (:documentation "Given an action, build a plan for all of its dependencies."))
  (define-convenience-action-methods collect-dependencies (operation component &key))
  (defmethod collect-dependencies ((operation operation) (component component)
                                   &rest keys &key &allow-other-keys)
    (let ((plan (apply 'make-instance 'filtered-sequential-plan
                       :system (component-system component) keys)))
      (loop :for action :in (direct-dependencies operation component)
        :do (collect-action-dependencies plan (action-operation action) (action-component action)))
      (plan-actions plan)))

  (defun* (required-components) (system &rest keys &key (goal-operation 'load-op) &allow-other-keys)
    "Given a SYSTEM and a GOAL-OPERATION (default LOAD-OP), traverse the dependencies and
return a list of the components involved in building the desired action."
    (with-asdf-session (:override t)
      (remove-duplicates
       (mapcar 'action-component
               (apply 'collect-dependencies goal-operation system
                      (remove-plist-key :goal-operation keys)))
       :from-end t))))


;;;; High-level interface: make-plan, perform-plan
(with-upgradability ()
  (defgeneric make-plan (plan-class operation component &key &allow-other-keys)
    (:documentation "Generate and return a plan for performing OPERATION on COMPONENT."))
  (define-convenience-action-methods make-plan (plan-class operation component &key))

  (defgeneric mark-as-done (plan-class operation component)
    (:documentation "Mark an action as done in a plan, after performing it."))
  (define-convenience-action-methods mark-as-done (plan-class operation component))

  (defgeneric perform-plan (plan &key)
    (:documentation "Actually perform a plan and build the requested actions"))

  (defparameter* *plan-class* 'sequential-plan
    "The default plan class to use when building with ASDF")

  (defmethod make-plan (plan-class (o operation) (c component) &rest keys &key &allow-other-keys)
    (with-asdf-session ()
      (let ((plan (apply 'make-instance (or plan-class *plan-class*) keys)))
        (traverse-action plan o c t)
        plan)))

  (defmethod perform-plan :around ((plan t) &key)
    (assert (performable-p (forcing plan)) () "plan not performable")
    (let ((*package* *package*)
          (*readtable* *readtable*))
      (with-compilation-unit () ;; backward-compatibility.
        (call-next-method))))   ;; Going forward, see deferred-warning support in lisp-build.

  (defun action-already-done-p (plan operation component)
    (if-let (status (action-status plan operation component))
      (status-done-p status)))

  (defmethod perform-plan ((plan t) &key)
    (loop :for action :in (plan-actions plan)
      :as o = (action-operation action)
      :as c = (action-component action) :do
      (unless (action-already-done-p plan o c)
        (perform-with-restarts o c)
        (mark-as-done plan o c))))

  (defmethod mark-as-done ((plan plan) (o operation) (c component))
    (let ((plan-status (action-status plan o c))
          (perform-status (action-status nil o c)))
      (assert (and (status-stamp perform-status) (status-keep-p perform-status)) ()
              "Just performed ~A but failed to mark it done" (action-description o c))
      (setf (action-status plan o c)
            (make-action-status
             :bits (logior (status-bits plan-status) +done-bit+)
             :stamp (status-stamp perform-status)
             :level (status-level plan-status)
             :index (status-index plan-status))))))
;;;; -------------------------------------------------------------------------
;;;; Invoking Operations

(uiop/package:define-package :asdf/operate
  (:recycle :asdf/operate :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/session
        :asdf/component :asdf/system :asdf/system-registry :asdf/find-component
        :asdf/operation :asdf/action :asdf/lisp-action :asdf/forcing :asdf/plan)
  (:export
   #:operate #:oos #:build-op #:make
   #:load-system #:load-systems #:load-systems*
   #:compile-system #:test-system #:require-system #:module-provide-asdf
   #:component-loaded-p #:already-loaded-systems
   #:recursive-operate))
(in-package :asdf/operate)

(with-upgradability ()
  (defgeneric operate (operation component &key)
    (:documentation
     "Operate does mainly four things for the user:

1. Resolves the OPERATION designator into an operation object.
   OPERATION is typically a symbol denoting an operation class, instantiated with MAKE-OPERATION.
2. Resolves the COMPONENT designator into a component object.
   COMPONENT is typically a string or symbol naming a system, loaded from disk using FIND-SYSTEM.
3. It then calls MAKE-PLAN with the operation and system as arguments.
4. Finally calls PERFORM-PLAN on the resulting plan to actually build the system.

The entire computation is wrapped in WITH-COMPILATION-UNIT and error handling code.
If a VERSION argument is supplied, then operate also ensures that the system found satisfies it
using the VERSION-SATISFIES method.
If a PLAN-CLASS argument is supplied, that class is used for the plan.
If a PLAN-OPTIONS argument is supplied, the options are passed to the plan.

The :FORCE or :FORCE-NOT argument to OPERATE can be:
  T to force the inside of the specified system to be rebuilt (resp. not),
    without recursively forcing the other systems we depend on.
  :ALL to force all systems including other systems we depend on to be rebuilt (resp. not).
  (SYSTEM1 SYSTEM2 ... SYSTEMN) to force systems named in a given list
:FORCE-NOT has precedence over :FORCE; builtin systems cannot be forced.

For backward compatibility, all keyword arguments are passed to MAKE-OPERATION
when instantiating a new operation, that will in turn be inherited by new operations.
But do NOT depend on it, for this is deprecated behavior."))

  (define-convenience-action-methods operate (operation component &key)
    :if-no-component (error 'missing-component :requires component))

  ;; This method ensures that an ASDF upgrade is attempted as the very first thing,
  ;; with suitable state preservation in case in case it actually happens,
  ;; and that a few suitable dynamic bindings are established.
  (defmethod operate :around (operation component &rest keys
                              &key verbose
                                (on-warnings *compile-file-warnings-behaviour*)
                                (on-failure *compile-file-failure-behaviour*))
    (nest
     (with-asdf-session ())
     (let* ((operation-remaker ;; how to remake the operation after ASDF was upgraded (if it was)
             (etypecase operation
               (operation (let ((name (type-of operation)))
                            #'(lambda () (make-operation name))))
               ((or symbol string) (constantly operation))))
            (component-path (typecase component ;; to remake the component after ASDF upgrade
                              (component (component-find-path component))
                              (t component)))
            (system-name (labels ((first-name (x)
                                    (etypecase x
                                      ((or string symbol) x) ; NB: includes the NIL case.
                                      (cons (or (first-name (car x)) (first-name (cdr x)))))))
                           (coerce-name (first-name component-path)))))
       (apply 'make-forcing :performable-p t :system system-name keys)
       ;; Before we operate on any system, make sure ASDF is up-to-date,
       ;; for if an upgrade is ever attempted at any later time, there may be BIG trouble.
       (unless (asdf-upgraded-p (toplevel-asdf-session))
         (setf (asdf-upgraded-p (toplevel-asdf-session)) t)
         (when (upgrade-asdf)
           ;; If we were upgraded, restart OPERATE the hardest of ways, for
           ;; its function may have been redefined.
           (return-from operate
             (with-asdf-session (:override t :override-cache t)
               (apply 'operate (funcall operation-remaker) component-path keys))))))
      ;; Setup proper bindings around any operate call.
     (let* ((*verbose-out* (and verbose *standard-output*))
            (*compile-file-warnings-behaviour* on-warnings)
            (*compile-file-failure-behaviour* on-failure)))
     (unwind-protect
          (progn
            (incf (operate-level))
            (call-next-method))
       (decf (operate-level)))))

  (defmethod operate :before ((operation operation) (component component)
                              &key version)
    (unless (version-satisfies component version)
      (error 'missing-component-of-version :requires component :version version))
    (record-dependency nil operation component))

  (defmethod operate ((operation operation) (component component)
                      &key plan-class plan-options)
    (let ((plan (apply 'make-plan plan-class operation component
                       :forcing (forcing *asdf-session*) plan-options)))
      (perform-plan plan)
      (values operation plan)))

  (defun oos (operation component &rest args &key &allow-other-keys)
    (apply 'operate operation component args))

  (setf (documentation 'oos 'function)
        (format nil "Short for _operate on system_ and an alias for the OPERATE function.~%~%~a"
                (documentation 'operate 'function)))

  (define-condition recursive-operate (warning)
    ((operation :initarg :operation :reader condition-operation)
     (component :initarg :component :reader condition-component)
     (action :initarg :action :reader condition-action))
    (:report (lambda (c s)
               (format s (compatfmt "~@<Deprecated recursive use of (~S '~S '~S) while visiting ~S ~
- please use proper dependencies instead~@:>")
                       'operate
                       (type-of (condition-operation c))
                       (component-find-path (condition-component c))
                       (action-path (condition-action c)))))))

;;;; Common operations
(when-upgrading ()
  (defmethod component-depends-on ((o prepare-op) (s system))
    (call-next-method)))
(with-upgradability ()
  (defclass build-op (non-propagating-operation) ()
    (:documentation "Since ASDF3, BUILD-OP is the recommended 'master' operation,
to operate by default on a system or component, via the function BUILD.
Its meaning is configurable via the :BUILD-OPERATION option of a component.
which typically specifies the name of a specific operation to which to delegate the build,
as a symbol or as a string later read as a symbol (after loading the defsystem-depends-on);
if NIL is specified (the default), BUILD-OP falls back to LOAD-OP,
that will load the system in the current image."))
  (defmethod component-depends-on ((o build-op) (c component))
    `((,(or (component-build-operation c) 'load-op) ,c)
      ,@(call-next-method)))

  (defun make (system &rest keys)
    "The recommended way to interact with ASDF3.1 is via (ASDF:MAKE :FOO).
It will build system FOO using the operation BUILD-OP,
the meaning of which is configurable by the system, and
defaults to LOAD-OP, to load it in current image."
    (apply 'operate 'build-op system keys)
    t)

  (defun load-system (system &rest keys &key force force-not verbose version &allow-other-keys)
    "Shorthand for `(operate 'asdf:load-op system)`. See OPERATE for details."
    (declare (ignore force force-not verbose version))
    (apply 'operate 'load-op system keys)
    t)

  (defun load-systems* (systems &rest keys)
    "Loading multiple systems at once."
    (dolist (s systems) (apply 'load-system s keys)))

  (defun load-systems (&rest systems)
    "Loading multiple systems at once."
    (load-systems* systems))

  (defun compile-system (system &rest args &key force force-not verbose version &allow-other-keys)
    "Shorthand for `(asdf:operate 'asdf:compile-op system)`. See OPERATE for details."
    (declare (ignore force force-not verbose version))
    (apply 'operate 'compile-op system args)
    t)

  (defun test-system (system &rest args &key force force-not verbose version &allow-other-keys)
    "Shorthand for `(asdf:operate 'asdf:test-op system)`. See OPERATE for details."
    (declare (ignore force force-not verbose version))
    (apply 'operate 'test-op system args)
    t))

;;;;; Define the function REQUIRE-SYSTEM, that, similarly to REQUIRE,
;; only tries to load its specified target if it's not loaded yet.
(with-upgradability ()
  (defun component-loaded-p (component)
    "Has the given COMPONENT been successfully loaded in the current image (yet)?
Note that this returns true even if the component is not up to date."
    (if-let ((component (find-component component () :registered t)))
      (nth-value 1 (component-operation-time (make-operation 'load-op) component))))

  (defun already-loaded-systems ()
    "return a list of the names of the systems that have been successfully loaded so far"
    (mapcar 'coerce-name (remove-if-not 'component-loaded-p (registered-systems*)))))


;;;; Define the class REQUIRE-SYSTEM, to be hooked into CL:REQUIRE when possible,
;; i.e. for ABCL, CLISP, ClozureCL, CMUCL, ECL, MKCL and SBCL
;; Note that despite the two being homonyms, the _function_ require-system
;; and the _class_ require-system are quite distinct entities, fulfilling independent purposes.
(with-upgradability ()
  (defvar *modules-being-required* nil)

  (defclass require-system (system)
    ((module :initarg :module :initform nil :accessor required-module))
    (:documentation "A SYSTEM subclass whose processing is handled by
the implementation's REQUIRE rather than by internal ASDF mechanisms."))

  (defmethod perform ((o compile-op) (c require-system))
    nil)

  (defmethod perform ((o load-op) (s require-system))
    (let* ((module (or (required-module s) (coerce-name s)))
           (*modules-being-required* (cons module *modules-being-required*)))
      (assert (null (component-children s)))
      (require module)))

  (defmethod resolve-dependency-combination (component (combinator (eql :require)) arguments)
    (unless (and (length=n-p arguments 1)
                 (typep (car arguments) '(or string (and symbol (not null)))))
      (parameter-error (compatfmt "~@<In ~S, bad dependency ~S for ~S. ~S takes one argument, a string or non-null symbol~@:>")
                       'resolve-dependency-combination
                       (cons combinator arguments) component combinator))
    ;; :require must be prepared for some implementations providing modules using ASDF,
    ;; as SBCL used to do, and others may might do. Thus, the system provided in the end
    ;; would be a downcased name as per module-provide-asdf above. For the same reason,
    ;; we cannot assume that the system in the end will be of type require-system,
    ;; but must check whether we can use find-system and short-circuit cl:require.
    ;; Otherwise, calling cl:require could result in nasty reentrant calls between
    ;; cl:require and asdf:operate that could potentially blow up the stack,
    ;; all the while defeating the consistency of the dependency graph.
    (let* ((module (car arguments)) ;; NB: we already checked that it was not null
           ;; CMUCL, MKCL, SBCL like their module names to be all upcase.
           (module-name (string module))
           (system-name (string-downcase module))
           (system (find-system system-name nil)))
      (or system (let ((system (make-instance 'require-system :name system-name :module module-name)))
                   (register-system system)
                   system))))

  (defun module-provide-asdf (name)
    ;; We must use string-downcase, because modules are traditionally specified as symbols,
    ;; that implementations traditionally normalize as uppercase, for which we seek a system
    ;; with a name that is traditionally in lowercase. Case is lost along the way. That's fine.
    ;; We could make complex, non-portable rules to try to preserve case, and just documenting
    ;; them would be a hell that it would be a disservice to inflict on users.
    (let ((module-name (string name))
          (system-name (string-downcase name)))
      (unless (member module-name *modules-being-required* :test 'equal)
        (let ((*modules-being-required* (cons module-name *modules-being-required*))
              #+sbcl (sb-impl::*requiring* (remove module-name sb-impl::*requiring* :test 'equal)))
          (handler-bind
              (((or style-warning recursive-operate) #'muffle-warning)
               (missing-component (constantly nil))
               (fatal-condition
                #'(lambda (e)
                    (format *error-output* (compatfmt "~@<ASDF could not load ~(~A~) because ~A.~@:>~%")
                            name e))))
            (let ((*verbose-out* (make-broadcast-stream)))
              (let ((system (find-system system-name nil)))
                (when system
                  ;; Do not use require-system after all, use load-system:
                  ;; on the one hand, REQUIRE already uses *MODULES* not to load something twice,
                  ;; on the other hand, REQUIRE-SYSTEM uses FORCE-NOT which may conflict with
                  ;; the toplevel session forcing settings.
                  (load-system system :verbose nil)
                  t)))))))))


;;;; Some upgrade magic
(with-upgradability ()
  (defun restart-upgraded-asdf ()
    ;; If we're in the middle of something, restart it.
    (let ((systems-being-defined
           (when *asdf-session*
             (prog1
                 (loop :for k :being :the hash-keys :of (asdf-cache)
                   :when (eq (first k) 'find-system) :collect (second k))
               (clrhash (asdf-cache))))))
      ;; Regardless, clear defined systems, since they might be invalid
      ;; after an incompatible ASDF upgrade.
      (clear-registered-systems)
      ;; The configuration also may have to be upgraded.
      (upgrade-configuration)
      ;; If we were in the middle of an operation, be sure to restore the system being defined.
      (dolist (s systems-being-defined) (find-system s nil))))
  (register-hook-function '*post-upgrade-cleanup-hook* 'restart-upgraded-asdf))
;;;; -------------------------------------------------------------------------
;;;; Finding systems

(uiop/package:define-package :asdf/find-system
  (:recycle :asdf/find-system :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade
        :asdf/session :asdf/component :asdf/system :asdf/operation :asdf/action :asdf/lisp-action
        :asdf/find-component :asdf/system-registry :asdf/plan :asdf/operate)
  (:import-from #:asdf/component #:%additional-input-files)
  (:export
   #:find-system #:locate-system #:load-asd #:define-op
   #:load-system-definition-error #:error-name #:error-pathname #:error-condition))
(in-package :asdf/find-system)

(with-upgradability ()
  (define-condition load-system-definition-error (system-definition-error)
    ((name :initarg :name :reader error-name)
     (pathname :initarg :pathname :reader error-pathname)
     (condition :initarg :condition :reader error-condition))
    (:report (lambda (c s)
               (format s (compatfmt "~@<Error while trying to load definition for system ~A from pathname ~A: ~3i~_~A~@:>")
                       (error-name c) (error-pathname c) (error-condition c)))))


  ;;; Methods for find-system

  ;; Reject NIL as a system designator.
  (defmethod find-system ((name null) &optional (error-p t))
    (when error-p
      (sysdef-error (compatfmt "~@<NIL is not a valid system name~@:>"))))

  ;; Default method for find-system: resolve the argument using COERCE-NAME.
  (defmethod find-system (name &optional (error-p t))
    (find-system (coerce-name name) error-p))

  (defun find-system-if-being-defined (name)
    ;; This function finds systems being defined *in the current ASDF session*, as embodied by
    ;; its session cache, even before they are fully defined and registered in *registered-systems*.
    ;; The purpose of this function is to prevent races between two files that might otherwise
    ;; try overwrite each other's system objects, resulting in infinite loops and stack overflow.
    ;; This function explicitly MUST NOT find definitions merely registered in previous sessions.
    ;; NB: this function depends on a corresponding side-effect in parse-defsystem;
    ;; the precise protocol between the two functions may change in the future (or not).
    (first (gethash `(find-system ,(coerce-name name)) (asdf-cache))))

  (defclass define-op (non-propagating-operation) ()
    (:documentation "An operation to record dependencies on loading a .asd file."))

  (defmethod record-dependency ((plan null) (operation t) (component t))
    (unless (or (typep operation 'define-op)
                (and (typep operation 'load-op)
                     (typep component 'system)
                     (equal "asdf" (coerce-name component))))
      (if-let ((action (first (visiting-action-list *asdf-session*))))
        (let ((parent-operation (action-operation action))
              (parent-component (action-component action)))
          (cond
            ((and (typep parent-operation 'define-op)
                  (typep parent-component 'system))
             (let ((action (cons operation component)))
               (unless (gethash action (definition-dependency-set parent-component))
                 (push (cons operation component) (definition-dependency-list parent-component))
                 (setf (gethash action (definition-dependency-set parent-component)) t))))
            (t
             (warn 'recursive-operate
                   :operation operation :component component :action action)))))))

  (defmethod component-depends-on ((o define-op) (s system))
    `(;;NB: 1- ,@(system-defsystem-depends-on s)) ; Should be already included in the below.
      ;; 2- We don't call-next-method to avoid other methods
      ,@(loop* :for (o . c) :in (definition-dependency-list s) :collect (list o c))))

  (defmethod component-depends-on ((o operation) (s system))
    `(,@(when (and (not (typep o 'define-op))
                   (or (system-source-file s) (definition-dependency-list s)))
              `((define-op ,(primary-system-name s))))
      ,@(call-next-method)))

  (defmethod perform ((o operation) (c undefined-system))
    (sysdef-error "Trying to use undefined or incompletely defined system ~A" (coerce-name c)))

  ;; TODO: could this file be refactored so that locate-system is merely
  ;; the cache-priming call to input-files here?
  (defmethod input-files ((o define-op) (s system))
    (assert (equal (coerce-name s) (primary-system-name s)))
    (if-let ((asd (system-source-file s))) (list asd)))

  (defmethod perform ((o define-op) (s system))
    (assert (equal (coerce-name s) (primary-system-name s)))
    (nest
     (if-let ((pathname (first (input-files o s)))))
     (let ((readtable *readtable*) ;; save outer syntax tables. TODO: proper syntax-control
           (print-pprint-dispatch *print-pprint-dispatch*)))
     (with-standard-io-syntax)
     (let ((*print-readably* nil)
           ;; Note that our backward-compatible *readtable* is
           ;; a global readtable that gets globally side-effected. Ouch.
           ;; Same for the *print-pprint-dispatch* table.
           ;; We should do something about that for ASDF3 if possible, or else ASDF4.
           (*readtable* readtable) ;; restore inside syntax table
           (*print-pprint-dispatch* print-pprint-dispatch)
           (*package* (find-package :asdf-user))
           (*default-pathname-defaults*
            ;; resolve logical-pathnames so they won't wreak havoc in parsing namestrings.
            (pathname-directory-pathname (physicalize-pathname pathname)))))
     (handler-bind
         (((and error (not missing-component))
           #'(lambda (condition)
               (error 'load-system-definition-error
                      :name (coerce-name s) :pathname pathname :condition condition))))
       (asdf-message (compatfmt "~&~@<; ~@;Loading system definition~@[ for ~A~] from ~A~@:>~%")
                     (coerce-name s) pathname)
       ;; dependencies will depend on what's loaded via definition-dependency-list
       (unset-asdf-cache-entry `(component-depends-on ,o ,s))
       (unset-asdf-cache-entry `(input-files ,o ,s)))
     (load* pathname :external-format (encoding-external-format (detect-encoding pathname)))))

  (defun load-asd (pathname &key name)
    "Load system definitions from PATHNAME.
NAME if supplied is the name of a system expected to be defined in that file.

Do NOT try to load a .asd file directly with CL:LOAD. Always use ASDF:LOAD-ASD."
    (with-asdf-session ()
      ;; TODO: use OPERATE, so we consult the cache and only load once per session.
      (flet ((do-it (o c) (operate o c)))
        (let ((primary-name (primary-system-name (or name (pathname-name pathname))))
              (operation (make-operation 'define-op)))
          (if-let (system (registered-system primary-name))
            (progn
              ;; We already determine this to be obsolete ---
              ;; or should we move some tests from find-system to check for up-to-date-ness here?
              (setf (component-operation-time operation system) t
                    (definition-dependency-list system) nil
                    (definition-dependency-set system) (list-to-hash-set nil))
              (do-it operation system))
            (let ((system (make-instance 'undefined-system
                                         :name primary-name :source-file pathname)))
              (register-system system)
              (unwind-protect (do-it operation system)
                (when (typep system 'undefined-system)
                  (clear-system system)))))))))

  (defvar *old-asdf-systems* (make-hash-table :test 'equal))

  ;; (Private) function to check that a system that was found isn't an asdf downgrade.
  ;; Returns T if everything went right, NIL if the system was an ASDF of the same or older version,
  ;; that shall not be loaded. Also issue a warning if it was a strictly older version of ASDF.
  (defun check-not-old-asdf-system (name pathname)
    (or (not (member name '("asdf" "uiop") :test 'equal))
        (null pathname)
        (let* ((asdfp (equal name "asdf")) ;; otherwise, it's uiop
               (version-pathname
                (subpathname pathname "version" :type (if asdfp "lisp-expr" "lisp")))
               (version (and (probe-file* version-pathname :truename nil)
                             (read-file-form version-pathname :at (if asdfp '(0) '(2 2 2)))))
               (old-version (asdf-version)))
          (cond
            ;; Don't load UIOP of the exact same version: we already loaded it as part of ASDF.
            ((and (equal old-version version) (equal name "uiop")) nil)
            ((version<= old-version version) t) ;; newer or same version: Good!
            (t ;; old version: bad
             (ensure-gethash
              (list (namestring pathname) version) *old-asdf-systems*
              #'(lambda ()
                  (let ((old-pathname (system-source-file (registered-system "asdf"))))
                    (if asdfp
                        (warn "~@<~
        You are using ASDF version ~A ~:[(probably from (require \"asdf\") ~
        or loaded by quicklisp)~;from ~:*~S~] and have an older version of ASDF ~
        ~:[(and older than 2.27 at that)~;~:*~A~] registered at ~S. ~
        Having an ASDF installed and registered is the normal way of configuring ASDF to upgrade itself, ~
        and having an old version registered is a configuration error. ~
        ASDF will ignore this configured system rather than downgrade itself. ~
        In the future, you may want to either: ~
        (a) upgrade this configured ASDF to a newer version, ~
        (b) install a newer ASDF and register it in front of the former in your configuration, or ~
        (c) uninstall or unregister this and any other old version of ASDF from your configuration. ~
        Note that the older ASDF might be registered implicitly through configuration inherited ~
        from your system installation, in which case you might have to specify ~
        :ignore-inherited-configuration in your in your ~~/.config/common-lisp/source-registry.conf ~
        or other source-registry configuration file, environment variable or lisp parameter. ~
        Indeed, a likely offender is an obsolete version of the cl-asdf debian or ubuntu package, ~
        that you might want to upgrade (if a recent enough version is available) ~
        or else remove altogether (since most implementations ship with a recent asdf); ~
        if you lack the system administration rights to upgrade or remove this package, ~
        then you might indeed want to either install and register a more recent version, ~
        or use :ignore-inherited-configuration to avoid registering the old one. ~
        Please consult ASDF documentation and/or experts.~@:>~%"
                              old-version old-pathname version pathname)
                        ;; NB: for UIOP, don't warn, just ignore.
                        (warn "ASDF ~A (from ~A), UIOP ~A (from ~A)"
                              old-version old-pathname version pathname)
                        ))))
             nil))))) ;; only issue the warning the first time, but always return nil

  (defun locate-system (name)
    "Given a system NAME designator, try to locate where to load the system from.
Returns five values: FOUNDP FOUND-SYSTEM PATHNAME PREVIOUS PREVIOUS-TIME
FOUNDP is true when a system was found,
either a new unregistered one or a previously registered one.
FOUND-SYSTEM when not null is a SYSTEM object that may be REGISTER-SYSTEM'ed.
PATHNAME when not null is a path from which to load the system,
either associated with FOUND-SYSTEM, or with the PREVIOUS system.
PREVIOUS when not null is a previously loaded SYSTEM object of same name.
PREVIOUS-TIME when not null is the time at which the PREVIOUS system was loaded."
    (with-asdf-session () ;; NB: We don't cache the results. We once used to, but it wasn't useful,
      ;; and keeping a negative cache was a bug (see lp#1335323), which required
      ;; explicit invalidation in clear-system and find-system (when unsucccessful).
      (let* ((name (coerce-name name))
             (previous (registered-system name)) ; load from disk if absent or newer on disk
             (primary (registered-system (primary-system-name name)))
             (previous-time (and previous primary (component-operation-time 'define-op primary)))
             (found (search-for-system-definition name))
             (found-system (and (typep found 'system) found))
             (pathname (ensure-pathname
                        (or (and (typep found '(or pathname string)) (pathname found))
                            (system-source-file found-system)
                            (system-source-file previous))
                        :want-absolute t :resolve-symlinks *resolve-symlinks*))
             (foundp (and (or found-system pathname previous) t)))
        (check-type found (or null pathname system))
        (unless (check-not-old-asdf-system name pathname)
          (check-type previous system) ;; asdf is preloaded, so there should be a previous one.
          (setf found-system nil pathname nil))
        (values foundp found-system pathname previous previous-time))))

  ;; Main method for find-system: first, make sure the computation is memoized in a session cache.
  ;; Unless the system is immutable, use locate-system to find the primary system;
  ;; reconcile the finding (if any) with any previous definition (in a previous session,
  ;; preloaded, with a previous configuration, or before filesystem changes), and
  ;; load a found .asd if appropriate. Finally, update registration table and return results.

  (defun definition-dependencies-up-to-date-p (system)
    (check-type system system)
    (assert (primary-system-p system))
    (handler-case
        (loop :with plan = (make-instance *plan-class*)
          :for action :in (definition-dependency-list system)
          :always (action-up-to-date-p
                   plan (action-operation action) (action-component action))
          :finally
          (let ((o (make-operation 'define-op)))
            (multiple-value-bind (stamp done-p)
                (compute-action-stamp plan o system)
              (return (and (timestamp<= stamp (component-operation-time o system))
                           done-p)))))
      (system-out-of-date () nil)))

  (defmethod find-system ((name string) &optional (error-p t))
    (nest
     (with-asdf-session (:key `(find-system ,name)))
     (let ((name-primary-p (primary-system-p name)))
       (unless name-primary-p (find-system (primary-system-name name) nil)))
     (or (and *immutable-systems* (gethash name *immutable-systems*) (registered-system name)))
     (multiple-value-bind (foundp found-system pathname previous previous-time)
         (locate-system name)
       (assert (eq foundp (and (or found-system pathname previous) t))))
     (let ((previous-pathname (system-source-file previous))
           (system (or previous found-system)))
       (when (and found-system (not previous))
         (register-system found-system))
       (when (and system pathname)
         (setf (system-source-file system) pathname))
       (if-let ((stamp (get-file-stamp pathname)))
         (let ((up-to-date-p
                (and previous
                     (or (pathname-equal pathname previous-pathname)
                         (and pathname previous-pathname
                              (pathname-equal
                               (physicalize-pathname pathname)
                               (physicalize-pathname previous-pathname))))
                     (timestamp<= stamp previous-time)
                     ;; TODO: check that all dependencies are up-to-date.
                     ;; This necessitates traversing them without triggering
                     ;; the adding of nodes to the plan.
                     (or (not name-primary-p)
                         (definition-dependencies-up-to-date-p previous)))))
           (unless up-to-date-p
             (restart-case
                 (signal 'system-out-of-date :name name)
               (continue () :report "continue"))
             (load-asd pathname :name name)))))
     ;; Try again after having loaded from disk if needed
     (or (registered-system name)
         (when error-p (error 'missing-component :requires name)))))

  ;; Resolved forward reference for asdf/system-registry.
  (defun mark-component-preloaded (component)
    "Mark a component as preloaded."
    (let ((component (find-component component nil :registered t)))
      ;; Recurse to children, so asdf/plan will hopefully be happy.
      (map () 'mark-component-preloaded (component-children component))
      ;; Mark the timestamps of the common lisp-action operations as 0.
      (let ((cot (component-operation-times component)))
        (dolist (o `(,@(when (primary-system-p component) '(define-op))
                       prepare-op compile-op load-op))
          (setf (gethash (make-operation o) cot) 0))))))
;;;; -------------------------------------------------------------------------
;;;; Defsystem

(uiop/package:define-package :asdf/parse-defsystem
  (:recycle :asdf/parse-defsystem :asdf/defsystem :asdf)
  (:nicknames :asdf/defsystem) ;; previous name, to be compatible with, in case anyone cares
  (:use :uiop/common-lisp :asdf/driver :asdf/upgrade
   :asdf/session :asdf/component :asdf/system :asdf/system-registry
   :asdf/find-component :asdf/action :asdf/lisp-action :asdf/operate)
  (:import-from :asdf/system #:depends-on #:weakly-depends-on)
  ;; these needed for record-additional-system-input-file
  (:import-from :asdf/operation #:make-operation)
  (:import-from :asdf/component #:%additional-input-files)
  (:import-from :asdf/find-system #:define-op)
  (:export
   #:defsystem #:register-system-definition
   #:class-for-type #:*default-component-class*
   #:determine-system-directory #:parse-component-form
   #:non-toplevel-system #:non-system-system #:bad-system-name
   #:sysdef-error-component #:check-component-input
   #:explain))
(in-package :asdf/parse-defsystem)

;;; Pathname
(with-upgradability ()
  (defun determine-system-directory (pathname)
    ;; The defsystem macro calls this function to determine the pathname of a system as follows:
    ;; 1. If the pathname argument is an pathname object (NOT a namestring),
    ;;    that is already an absolute pathname, return it.
    ;; 2. Otherwise, the directory containing the LOAD-PATHNAME
    ;;    is considered (as deduced from e.g. *LOAD-PATHNAME*), and
    ;;    if it is indeed available and an absolute pathname, then
    ;;    the PATHNAME argument is normalized to a relative pathname
    ;;    as per PARSE-UNIX-NAMESTRING (with ENSURE-DIRECTORY T)
    ;;    and merged into that DIRECTORY as per SUBPATHNAME.
    ;;    Note: avoid *COMPILE-FILE-PATHNAME* because the .asd is loaded as source,
    ;;    but may be from within the EVAL-WHEN of a file compilation.
    ;; If no absolute pathname was found, we return NIL.
    (check-type pathname (or null string pathname))
    (pathname-directory-pathname
     (resolve-symlinks*
      (ensure-absolute-pathname
       (parse-unix-namestring pathname :type :directory)
       #'(lambda () (ensure-absolute-pathname
                     (load-pathname) 'get-pathname-defaults nil))
       nil)))))


;;; Component class
(with-upgradability ()
  ;; What :file gets interpreted as, unless overridden by a :default-component-class
  (defvar *default-component-class* 'cl-source-file)

  (defun class-for-type (parent type)
      (or (coerce-class type :package :asdf/interface :super 'component :error nil)
          (and (eq type :file)
               (coerce-class
                (or (loop :for p = parent :then (component-parent p) :while p
                      :thereis (module-default-component-class p))
                    *default-component-class*)
                :package :asdf/interface :super 'component :error nil))
          (sysdef-error "don't recognize component type ~S" type))))


;;; Check inputs
(with-upgradability ()
  (define-condition non-system-system (system-definition-error)
    ((name :initarg :name :reader non-system-system-name)
     (class-name :initarg :class-name :reader non-system-system-class-name))
    (:report (lambda (c s)
               (format s (compatfmt "~@<Error while defining system ~S: class ~S isn't a subclass of ~S~@:>")
                       (non-system-system-name c) (non-system-system-class-name c) 'system))))

  (define-condition non-toplevel-system (system-definition-error)
    ((parent :initarg :parent :reader non-toplevel-system-parent)
     (name :initarg :name :reader non-toplevel-system-name))
    (:report (lambda (c s)
               (format s (compatfmt "~@<Error while defining system: component ~S claims to have a system ~S as a child~@:>")
                       (non-toplevel-system-parent c) (non-toplevel-system-name c)))))

  (define-condition bad-system-name (warning)
    ((name :initarg :name :reader component-name)
     (source-file :initarg :source-file :reader system-source-file))
    (:report (lambda (c s)
               (let* ((file (system-source-file c))
                      (name (component-name c))
                      (asd (pathname-name file)))
                 (format s (compatfmt "~@<System definition file ~S contains definition for system ~S. ~
Please only define ~S and secondary systems with a name starting with ~S (e.g. ~S) in that file.~@:>")
                       file name asd (strcat asd "/") (strcat asd "/test"))))))

  (defun sysdef-error-component (msg type name value)
    (sysdef-error (strcat msg (compatfmt "~&~@<The value specified for ~(~A~) ~A is ~S~@:>"))
                  type name value))

  (defun check-component-input (type name weakly-depends-on
                                depends-on components)
    "A partial test of the values of a component."
    (unless (listp depends-on)
      (sysdef-error-component ":depends-on must be a list."
                              type name depends-on))
    (unless (listp weakly-depends-on)
      (sysdef-error-component ":weakly-depends-on must be a list."
                              type name weakly-depends-on))
    (unless (listp components)
      (sysdef-error-component ":components must be NIL or a list of components."
                              type name components)))


  (defun record-additional-system-input-file (pathname component parent)
    (let* ((record-on (if parent
                          (loop :with retval
                                :for par = parent :then (component-parent par)
                                :while par
                                :do (setf retval par)
                                :finally (return retval))
                          component))
           (comp (if (typep record-on 'component)
                     record-on
                     ;; at this point there will be no parent for RECORD-ON
                     (find-component record-on nil)))
           (op (make-operation 'define-op))
           (cell (or (assoc op (%additional-input-files comp))
                       (let ((new-cell (list op)))
                         (push new-cell (%additional-input-files comp))
                         new-cell))))
      (pushnew pathname (cdr cell) :test 'pathname-equal)
      (values)))

  ;; Given a form used as :version specification, in the context of a system definition
  ;; in a file at PATHNAME, for given COMPONENT with given PARENT, normalize the form
  ;; to an acceptable ASDF-format version.
  (defun* (normalize-version) (form &key pathname component parent)
    (labels ((invalid (&optional (continuation "using NIL instead"))
               (warn (compatfmt "~@<Invalid :version specifier ~S~@[ for component ~S~]~@[ in ~S~]~@[ from file ~S~]~@[, ~A~]~@:>")
                     form component parent pathname continuation))
             (invalid-parse (control &rest args)
               (unless (if-let (target (find-component parent component)) (builtin-system-p target))
                 (apply 'warn control args)
                 (invalid))))
      (if-let (v (typecase form
                   ((or string null) form)
                   (real
                    (invalid "Substituting a string")
                    (format nil "~D" form)) ;; 1.0 becomes "1.0"
                   (cons
                    (case (first form)
                      ((:read-file-form)
                       (destructuring-bind (subpath &key (at 0)) (rest form)
                         (let ((path (subpathname pathname subpath)))
                           (record-additional-system-input-file path component parent)
                           (safe-read-file-form path
                                                :at at :package :asdf-user))))
                      ((:read-file-line)
                       (destructuring-bind (subpath &key (at 0)) (rest form)
                         (let ((path (subpathname pathname subpath)))
                           (record-additional-system-input-file path component parent)
                           (safe-read-file-line (subpathname pathname subpath)
                                                :at at))))
                      (otherwise
                       (invalid))))
                   (t
                    (invalid))))
        (if-let (pv (parse-version v #'invalid-parse))
          (unparse-version pv)
          (invalid))))))


;;; "inline methods"
(with-upgradability ()
  (defparameter* +asdf-methods+
    '(perform-with-restarts perform explain output-files operation-done-p))

  (defun %remove-component-inline-methods (component)
    (dolist (name +asdf-methods+)
      (map ()
           ;; this is inefficient as most of the stored
           ;; methods will not be for this particular gf
           ;; But this is hardly performance-critical
           #'(lambda (m)
               (remove-method (symbol-function name) m))
           (component-inline-methods component)))
    (component-inline-methods component) nil)

  (defun %define-component-inline-methods (ret rest)
    (loop* :for (key value) :on rest :by #'cddr
           :for name = (and (keywordp key) (find key +asdf-methods+ :test 'string=))
           :when name :do
           (destructuring-bind (op &rest body) value
             (loop :for arg = (pop body)
                   :while (atom arg)
                   :collect arg :into qualifiers
                   :finally
                      (destructuring-bind (o c) arg
                        (pushnew
                         (eval `(defmethod ,name ,@qualifiers ((,o ,op) (,c (eql ,ret))) ,@body))
                         (component-inline-methods ret)))))))

  (defun %refresh-component-inline-methods (component rest)
    ;; clear methods, then add the new ones
    (%remove-component-inline-methods component)
    (%define-component-inline-methods component rest)))


;;; Main parsing function
(with-upgradability ()
  (defun parse-dependency-def (dd)
    (if (listp dd)
        (case (first dd)
          (:feature
           (unless (= (length dd) 3)
             (sysdef-error "Ill-formed feature dependency: ~s" dd))
           (let ((embedded (parse-dependency-def (third dd))))
             `(:feature ,(second dd) ,embedded)))
          (feature
           (sysdef-error "`feature' has been removed from the dependency spec language of ASDF. Use :feature instead in ~s." dd))
          (:require
           (unless (= (length dd) 2)
             (sysdef-error "Ill-formed require dependency: ~s" dd))
           dd)
          (:version
           (unless (= (length dd) 3)
             (sysdef-error "Ill-formed version dependency: ~s" dd))
           `(:version ,(coerce-name (second dd)) ,(third dd)))
          (otherwise (sysdef-error "Ill-formed dependency: ~s" dd)))
      (coerce-name dd)))

  (defun parse-dependency-defs (dd-list)
    "Parse the dependency defs in DD-LIST into canonical form by translating all
system names contained using COERCE-NAME. Return the result."
    (mapcar 'parse-dependency-def dd-list))

  (defun* (parse-component-form) (parent options &key previous-serial-component)
    (destructuring-bind
        (type name &rest rest &key
                                (builtin-system-p () bspp)
                                ;; the following list of keywords is reproduced below in the
                                ;; remove-plist-keys form.  important to keep them in sync
                                components pathname perform explain output-files operation-done-p
                                weakly-depends-on depends-on serial
                                do-first if-component-dep-fails version
                                ;; list ends
         &allow-other-keys) options
      (declare (ignore perform explain output-files operation-done-p builtin-system-p))
      (check-component-input type name weakly-depends-on depends-on components)
      (when (and parent
                 (find-component parent name)
                 (not ;; ignore the same object when rereading the defsystem
                  (typep (find-component parent name)
                         (class-for-type parent type))))
        (error 'duplicate-names :name name))
      (when do-first (error "DO-FIRST is not supported anymore as of ASDF 3"))
      (let* ((name (coerce-name name))
             (args `(:name ,name
                     :pathname ,pathname
                     ,@(when parent `(:parent ,parent))
                     ,@(remove-plist-keys
                        '(:components :pathname :if-component-dep-fails :version
                          :perform :explain :output-files :operation-done-p
                          :weakly-depends-on :depends-on :serial)
                        rest)))
             (component (find-component parent name))
             (class (class-for-type parent type)))
        (when (and parent (subtypep class 'system))
          (error 'non-toplevel-system :parent parent :name name))
        (if component ; preserve identity
            (apply 'reinitialize-instance component args)
            (setf component (apply 'make-instance class args)))
        (component-pathname component) ; eagerly compute the absolute pathname
        (when (typep component 'system)
          ;; cache information for introspection
          (setf (slot-value component 'depends-on)
                (parse-dependency-defs depends-on)
                (slot-value component 'weakly-depends-on)
                ;; these must be a list of systems, cannot be features or versioned systems
                (mapcar 'coerce-name weakly-depends-on)))
        (let ((sysfile (system-source-file (component-system component)))) ;; requires the previous
          (when (and (typep component 'system) (not bspp))
            (setf (builtin-system-p component) (lisp-implementation-pathname-p sysfile)))
          (setf version (normalize-version version :component name :parent parent :pathname sysfile)))
        ;; Don't use the accessor: kluge to avoid upgrade issue on CCL 1.8.
        ;; A better fix is required.
        (setf (slot-value component 'version) version)
        (when (typep component 'parent-component)
          (setf (component-children component)
                (loop
                  :with previous-component = nil
                  :for c-form :in components
                  :for c = (parse-component-form component c-form
                                                 :previous-serial-component previous-component)
                  :for name = (component-name c)
                  :collect c
                  :when serial :do (setf previous-component name)))
          (compute-children-by-name component))
        (when previous-serial-component
          (push previous-serial-component depends-on))
        (when weakly-depends-on
          ;; ASDF4: deprecate this feature and remove it.
          (appendf depends-on
                   (remove-if (complement #'(lambda (x) (find-system x nil))) weakly-depends-on)))
        ;; Used by POIU. ASDF4: rename to component-depends-on?
        (setf (component-sideway-dependencies component) depends-on)
        (%refresh-component-inline-methods component rest)
        (when if-component-dep-fails
          (error "The system definition for ~S uses deprecated ~
            ASDF option :IF-COMPONENT-DEP-FAILS. ~
            Starting with ASDF 3, please use :IF-FEATURE instead"
           (coerce-name (component-system component))))
        component)))

  (defun register-system-definition
      (name &rest options &key pathname (class 'system) (source-file () sfp)
                            defsystem-depends-on &allow-other-keys)
    ;; The system must be registered before we parse the body,
    ;; otherwise we recur when trying to find an existing system
    ;; of the same name to reuse options (e.g. pathname) from.
    ;; To avoid infinite recursion in cases where you defsystem a system
    ;; that is registered to a different location to find-system,
    ;; we also need to remember it in the asdf-cache.
    (nest
     (with-asdf-session ())
     (let* ((name (coerce-name name))
            (source-file (if sfp source-file (resolve-symlinks* (load-pathname))))))
     (flet ((fix-case (x) (if (logical-pathname-p source-file) (string-downcase x) x))))
     (let* ((asd-name (and source-file
                           (equal "asd" (fix-case (pathname-type source-file)))
                           (fix-case (pathname-name source-file))))
            (primary-name (primary-system-name name)))
       (when (and asd-name (not (equal asd-name primary-name)))
         (warn (make-condition 'bad-system-name :source-file source-file :name name))))
     (let* (;; NB: handle defsystem-depends-on BEFORE to create the system object,
            ;; so that in case it fails, there is no incomplete object polluting the build.
            (checked-defsystem-depends-on
             (let* ((dep-forms (parse-dependency-defs defsystem-depends-on))
                    (deps (loop :for spec :in dep-forms
                            :when (resolve-dependency-spec nil spec)
                            :collect :it)))
               (load-systems* deps)
               dep-forms))
            (system (or (find-system-if-being-defined name)
                        (if-let (registered (registered-system name))
                          (reset-system-class registered 'undefined-system
                                              :name name :source-file source-file)
                          (register-system (make-instance 'undefined-system
                                                          :name name :source-file source-file)))))
            (component-options
             (append
              (remove-plist-keys '(:defsystem-depends-on :class) options)
              ;; cache defsystem-depends-on in canonical form
              (when checked-defsystem-depends-on
                `(:defsystem-depends-on ,checked-defsystem-depends-on))))
            (directory (determine-system-directory pathname)))
       ;; This works hand in hand with asdf/find-system:find-system-if-being-defined:
       (set-asdf-cache-entry `(find-system ,name) (list system)))
     ;; We change-class AFTER we loaded the defsystem-depends-on
     ;; since the class might be defined as part of those.
     (let ((class (class-for-type nil class)))
       (unless (subtypep class 'system)
         (error 'non-system-system :name name :class-name (class-name class)))
       (unless (eq (type-of system) class)
         (reset-system-class system class)))
     (parse-component-form nil (list* :module name :pathname directory component-options))))

  (defmacro defsystem (name &body options)
    `(apply 'register-system-definition ',name ',options)))
;;;; -------------------------------------------------------------------------
;;;; ASDF-Bundle

(uiop/package:define-package :asdf/bundle
  (:recycle :asdf/bundle :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade
   :asdf/component :asdf/system :asdf/operation
   :asdf/find-component ;; used by ECL
   :asdf/action :asdf/lisp-action :asdf/plan :asdf/operate :asdf/parse-defsystem)
  (:export
   #:bundle-op #:bundle-type #:program-system
   #:bundle-system #:bundle-pathname-type #:direct-dependency-files
   #:monolithic-op #:monolithic-bundle-op #:operation-monolithic-p
   #:basic-compile-bundle-op #:prepare-bundle-op
   #:compile-bundle-op #:load-bundle-op #:monolithic-compile-bundle-op #:monolithic-load-bundle-op
   #:lib-op #:monolithic-lib-op
   #:dll-op #:monolithic-dll-op
   #:deliver-asd-op #:monolithic-deliver-asd-op
   #:program-op #:image-op #:compiled-file #:precompiled-system #:prebuilt-system
   #:user-system-p #:user-system #:trivial-system-p
   #:prologue-code #:epilogue-code #:static-library))
(in-package :asdf/bundle)

(with-upgradability ()
  (defclass bundle-op (operation)
    ;; NB: use of instance-allocated slots for operations is DEPRECATED
    ;; and only supported in a temporary fashion for backward compatibility.
    ;; Supported replacement: Define slots on program-system instead.
    ((bundle-type :initform :no-output-file :reader bundle-type :allocation :class))
    (:documentation "base class for operations that bundle outputs from multiple components"))

  (defclass monolithic-op (operation) ()
    (:documentation "A MONOLITHIC operation operates on a system *and all of its
dependencies*.  So, for example, a monolithic concatenate operation will
concatenate together a system's components and all of its dependencies, but a
simple concatenate operation will concatenate only the components of the system
itself."))

  (defclass monolithic-bundle-op (bundle-op monolithic-op)
    ;; Old style way of specifying prologue and epilogue on ECL: in the monolithic operation.
    ;; DEPRECATED. Supported replacement: Define slots on program-system instead.
    ((prologue-code :initform nil :accessor prologue-code)
     (epilogue-code :initform nil :accessor epilogue-code))
    (:documentation "operations that are both monolithic-op and bundle-op"))

  (defclass program-system (system)
    ;; New style (ASDF3.1) way of specifying prologue and epilogue on ECL: in the system
    ((prologue-code :initform nil :initarg :prologue-code :reader prologue-code)
     (epilogue-code :initform nil :initarg :epilogue-code :reader epilogue-code)
     (no-uiop :initform nil :initarg :no-uiop :reader no-uiop)
     (prefix-lisp-object-files :initarg :prefix-lisp-object-files
                               :initform nil :accessor prefix-lisp-object-files)
     (postfix-lisp-object-files :initarg :postfix-lisp-object-files
                                :initform nil :accessor postfix-lisp-object-files)
     (extra-object-files :initarg :extra-object-files
                         :initform nil :accessor extra-object-files)
     (extra-build-args :initarg :extra-build-args
                       :initform nil :accessor extra-build-args)))

  (defmethod prologue-code ((x system)) nil)
  (defmethod epilogue-code ((x system)) nil)
  (defmethod no-uiop ((x system)) nil)
  (defmethod prefix-lisp-object-files ((x system)) nil)
  (defmethod postfix-lisp-object-files ((x system)) nil)
  (defmethod extra-object-files ((x system)) nil)
  (defmethod extra-build-args ((x system)) nil)

  (defclass link-op (bundle-op) ()
    (:documentation "Abstract operation for linking files together"))

  (defclass gather-operation (bundle-op)
    ((gather-operation :initform nil :allocation :class :reader gather-operation)
     (gather-type :initform :no-output-file :allocation :class :reader gather-type))
    (:documentation "Abstract operation for gathering many input files from a system"))

  (defun operation-monolithic-p (op)
    (typep op 'monolithic-op))

  ;; Dependencies of a gather-op are the actions of the dependent operation
  ;; for all the (sorted) required components for loading the system.
  ;; Monolithic operations typically use lib-op as the dependent operation,
  ;; and all system-level dependencies as required components.
  ;; Non-monolithic operations typically use compile-op as the dependent operation,
  ;; and all transitive sub-components as required components (excluding other systems).
  (defmethod component-depends-on ((o gather-operation) (s system))
    (let* ((mono (operation-monolithic-p o))
           (go (make-operation (or (gather-operation o) 'compile-op)))
           (bundle-p (typep go 'bundle-op))
           ;; In a non-mono operation, don't recurse to other systems.
           ;; In a mono operation gathering bundles, don't recurse inside systems.
           (component-type (if mono (if bundle-p 'system t) '(not system)))
           ;; In the end, only keep system bundles or non-system bundles, depending.
           (keep-component (if bundle-p 'system '(not system)))
           (deps
            ;; Required-components only looks at the dependencies of an action, excluding the action
            ;; itself, so it may be safely used by an action recursing on its dependencies (which
            ;; may or may not be an overdesigned API, since in practice we never use it that way).
            ;; Therefore, if we use :goal-operation 'load-op :keep-operation 'load-op, which looks
            ;; cleaner, we will miss the load-op on the requested system itself, which doesn't
            ;; matter for a regular system, but matters, a lot, for a package-inferred-system.
            ;; Using load-op as the goal operation and basic-compile-op as the keep-operation works
            ;; for our needs of gathering all the files we want to include in a bundle.
            ;; Note that we use basic-compile-op rather than compile-op so it will still work on
            ;; systems that would somehow load dependencies with load-bundle-op.
            (required-components
             s :other-systems mono :component-type component-type :keep-component keep-component
             :goal-operation 'load-op :keep-operation 'basic-compile-op)))
      `((,go ,@deps) ,@(call-next-method))))

  ;; Create a single fasl for the entire library
  (defclass basic-compile-bundle-op (bundle-op basic-compile-op)
    ((gather-type :initform #-(or clasp ecl mkcl) :fasl #+(or clasp ecl mkcl) :object
                  :allocation :class)
     (bundle-type :initform :fasb :allocation :class))
    (:documentation "Base class for compiling into a bundle"))

  ;; Analog to prepare-op, for load-bundle-op and compile-bundle-op
  (defclass prepare-bundle-op (sideway-operation)
    ((sideway-operation
      :initform #+(or clasp ecl mkcl) 'load-bundle-op #-(or clasp ecl mkcl) 'load-op
      :allocation :class))
    (:documentation "Operation class for loading the bundles of a system's dependencies"))

  (defclass lib-op (link-op gather-operation non-propagating-operation)
    ((gather-type :initform :object :allocation :class)
     (bundle-type :initform :lib :allocation :class))
    (:documentation "Compile the system and produce a linkable static library (.a/.lib)
for all the linkable object files associated with the system. Compare with DLL-OP.

On most implementations, these object files only include extensions to the runtime
written in C or another language with a compiler producing linkable object files.
On CLASP, ECL, MKCL, these object files _also_ include the contents of Lisp files
themselves. In any case, this operation will produce what you need to further build
a static runtime for your system, or a dynamic library to load in an existing runtime."))

  ;; What works: on ECL, CLASP(?), MKCL, we link the many .o files from the system into the .so;
  ;; on other implementations, we combine (usually concatenate) the .fasl files into one.
  (defclass compile-bundle-op (basic-compile-bundle-op selfward-operation gather-operation
                                                       #+(or clasp ecl mkcl) link-op)
    ((selfward-operation :initform '(prepare-bundle-op) :allocation :class))
    (:documentation "This operator is an alternative to COMPILE-OP. Build a system
and all of its dependencies, but build only a single (\"monolithic\") FASL, instead
of one per source file, which may be more resource efficient.  That monolithic
FASL should be loaded with LOAD-BUNDLE-OP, rather than LOAD-OP."))

  (defclass load-bundle-op (basic-load-op selfward-operation)
    ((selfward-operation :initform '(prepare-bundle-op compile-bundle-op) :allocation :class))
    (:documentation "This operator is an alternative to LOAD-OP. Build a system
and all of its dependencies, using COMPILE-BUNDLE-OP. The difference with
respect to LOAD-OP is that it builds only a single FASL, which may be
faster and more resource efficient."))

  ;; NB: since the monolithic-op's can't be sideway-operation's,
  ;; if we wanted lib-op, dll-op, deliver-asd-op to be sideway-operation's,
  ;; we'd have to have the monolithic-op not inherit from the main op,
  ;; but instead inherit from a basic-FOO-op as with basic-compile-bundle-op above.

  (defclass dll-op (link-op gather-operation non-propagating-operation)
    ((gather-type :initform :object :allocation :class)
     (bundle-type :initform :dll :allocation :class))
    (:documentation "Compile the system and produce a dynamic loadable library (.so/.dll)
for all the linkable object files associated with the system. Compare with LIB-OP."))

  (defclass deliver-asd-op (basic-compile-op selfward-operation)
    ((selfward-operation
      ;; TODO: implement link-op on all implementations, and make that
      ;; '(compile-bundle-op lib-op #-(or clasp ecl mkcl) dll-op)
      :initform '(compile-bundle-op #+(or clasp ecl mkcl) lib-op)
      :allocation :class))
    (:documentation "produce an asd file for delivering the system as a single fasl"))


  (defclass monolithic-deliver-asd-op (deliver-asd-op monolithic-bundle-op)
    ((selfward-operation
      ;; TODO: implement link-op on all implementations, and make that
      ;; '(monolithic-compile-bundle-op monolithic-lib-op #-(or clasp ecl mkcl) monolithic-dll-op)
      :initform '(monolithic-compile-bundle-op #+(or clasp ecl mkcl) monolithic-lib-op)
      :allocation :class))
    (:documentation "produce fasl and asd files for combined system and dependencies."))

  (defclass monolithic-compile-bundle-op
      (basic-compile-bundle-op monolithic-bundle-op
       #+(or clasp ecl mkcl) link-op gather-operation non-propagating-operation)
    ()
    (:documentation "Create a single fasl for the system and its dependencies."))

  (defclass monolithic-load-bundle-op (load-bundle-op monolithic-bundle-op)
    ((selfward-operation :initform 'monolithic-compile-bundle-op :allocation :class))
    (:documentation "Load a single fasl for the system and its dependencies."))

  (defclass monolithic-lib-op (lib-op monolithic-bundle-op non-propagating-operation)
    ((gather-type :initform :object :allocation :class))
    (:documentation "Compile the system and produce a linkable static library (.a/.lib)
for all the linkable object files associated with the system or its dependencies. See LIB-OP."))

  (defclass monolithic-dll-op (dll-op monolithic-bundle-op non-propagating-operation)
    ((gather-type :initform :object :allocation :class))
    (:documentation "Compile the system and produce a dynamic loadable library (.so/.dll)
for all the linkable object files associated with the system or its dependencies. See LIB-OP"))

  (defclass image-op (monolithic-bundle-op selfward-operation
                      #+(or clasp ecl mkcl) link-op #+(or clasp ecl mkcl) gather-operation)
    ((bundle-type :initform :image :allocation :class)
     (gather-operation :initform 'lib-op :allocation :class)
     #+(or clasp ecl mkcl) (gather-type :initform :static-library :allocation :class)
     (selfward-operation :initform '(#-(or clasp ecl mkcl) load-op) :allocation :class))
    (:documentation "create an image file from the system and its dependencies"))

  (defclass program-op (image-op)
    ((bundle-type :initform :program :allocation :class))
    (:documentation "create an executable file from the system and its dependencies"))

  ;; From the ASDF-internal bundle-type identifier, get a filesystem-usable pathname type.
  (defun bundle-pathname-type (bundle-type)
    (etypecase bundle-type
      ((or null string) ;; pass through nil or string literal
       bundle-type)
      ((eql :no-output-file) ;; marker for a bundle-type that has NO output file
       (error "No output file, therefore no pathname type"))
      ((eql :fasl) ;; the type of a fasl
       (compile-file-type)) ; on image-based platforms, used as input and output
      ((eql :fasb) ;; the type of a fasl
       #-(or clasp ecl mkcl) (compile-file-type) ; on image-based platforms, used as input and output
       #+(or clasp ecl mkcl) "fasb") ; on C-linking platforms, only used as output for system bundles
      ((member :image)
       #+allegro "dxl"
       #+(and clisp os-windows) "exe"
       #-(or allegro (and clisp os-windows)) "image")
      ;; NB: on CLASP and ECL these implementations, we better agree with
      ;; (compile-file-type :type bundle-type))
      ((eql :object) ;; the type of a linkable object file
       (os-cond ((os-unix-p) "o")
                ((os-windows-p) (if (featurep '(:or :mingw32 :mingw64)) "o" "obj"))))
      ((member :lib :static-library) ;; the type of a linkable library
       (os-cond ((os-unix-p) "a")
                ((os-windows-p) (if (featurep '(:or :mingw32 :mingw64)) "a" "lib"))))
      ((member :dll :shared-library) ;; the type of a shared library
       (os-cond ((os-macosx-p) "dylib") ((os-unix-p) "so") ((os-windows-p) "dll")))
      ((eql :program) ;; the type of an executable program
       (os-cond ((os-unix-p) nil) ((os-windows-p) "exe")))))

  ;; Compute the output-files for a given bundle action
  (defun bundle-output-files (o c)
    (let ((bundle-type (bundle-type o)))
      (unless (or (eq bundle-type :no-output-file) ;; NIL already means something regarding type.
                  (and (null (input-files o c)) (not (member bundle-type '(:image :program)))))
        (let ((name (or (component-build-pathname c)
                        (let ((suffix
                               (unless (typep o 'program-op)
                                 ;; "." is no good separator for Logical Pathnames, so we use "--"
                                 (if (operation-monolithic-p o)
                                     "--all-systems"
                                     ;; These use a different type .fasb or .a instead of .fasl
                                     #-(or clasp ecl mkcl) "--system"))))
                          (format nil "~A~@[~A~]" (component-name c) suffix))))
              (type (bundle-pathname-type bundle-type)))
          (values (list (subpathname (component-pathname c) name :type type))
                  (eq (class-of o) (coerce-class (component-build-operation c)
                                                 :package :asdf/interface
                                                 :super 'operation
                                                 :error nil)))))))

  (defmethod output-files ((o bundle-op) (c system))
    (bundle-output-files o c))

  #-(or clasp ecl mkcl)
  (progn
    (defmethod perform ((o image-op) (c system))
      (dump-image (output-file o c) :executable (typep o 'program-op)))
    (defmethod perform :before ((o program-op) (c system))
      (setf *image-entry-point* (ensure-function (component-entry-point c)))))

  (defclass compiled-file (file-component)
    ((type :initform #-(or clasp ecl mkcl) (compile-file-type) #+(or clasp ecl mkcl) "fasb"))
    (:documentation "Class for a file that is already compiled,
e.g. as part of the implementation, of an outer build system that calls into ASDF,
or of opaque libraries shipped along the source code."))

  (defclass precompiled-system (system)
    ((build-pathname :initarg :fasb :initarg :fasl))
    (:documentation "Class For a system that is delivered as a precompiled fasl"))

  (defclass prebuilt-system (system)
    ((build-pathname :initarg :static-library :initarg :lib
                     :accessor prebuilt-system-static-library))
    (:documentation "Class for a system delivered with a linkable static library (.a/.lib)")))


;;;
;;; BUNDLE-OP
;;;
;;; This operation takes all components from one or more systems and
;;; creates a single output file, which may be
;;; a FASL, a statically linked library, a shared library, etc.
;;; The different targets are defined by specialization.
;;;
(when-upgrading (:version "3.2.0")
  ;; Cancel any previously defined method
  (defmethod initialize-instance :after ((instance bundle-op) &rest initargs &key &allow-other-keys)
    (declare (ignore initargs))))

(with-upgradability ()
  (defgeneric trivial-system-p (component))

  (defun user-system-p (s)
    (and (typep s 'system)
         (not (builtin-system-p s))
         (not (trivial-system-p s)))))

(eval-when (#-lispworks :compile-toplevel :load-toplevel :execute)
  (deftype user-system () '(and system (satisfies user-system-p))))

;;;
;;; First we handle monolithic bundles.
;;; These are standalone systems which contain everything,
;;; including other ASDF systems required by the current one.
;;; A PROGRAM is always monolithic.
;;;
;;; MONOLITHIC SHARED LIBRARIES, PROGRAMS, FASL
;;;
(with-upgradability ()
  (defun direct-dependency-files (o c &key (test 'identity) (key 'output-files) &allow-other-keys)
    ;; This function selects output files from direct dependencies;
    ;; your component-depends-on method must gather the correct dependencies in the correct order.
    (while-collecting (collect)
      (map-direct-dependencies
       o c #'(lambda (sub-o sub-c)
               (loop :for f :in (funcall key sub-o sub-c)
                 :when (funcall test f) :do (collect f))))))

  (defun pathname-type-equal-function (type)
    #'(lambda (p) (equalp (pathname-type p) type)))

  (defmethod input-files ((o gather-operation) (c system))
    (unless (eq (bundle-type o) :no-output-file)
      (direct-dependency-files
       o c :key 'output-files
           :test (pathname-type-equal-function (bundle-pathname-type (gather-type o))))))

  ;; Find the operation that produces a given bundle-type
  (defun select-bundle-operation (type &optional monolithic)
    (ecase type
      ((:dll :shared-library)
       (if monolithic 'monolithic-dll-op 'dll-op))
      ((:lib :static-library)
       (if monolithic 'monolithic-lib-op 'lib-op))
      ((:fasb)
       (if monolithic 'monolithic-compile-bundle-op 'compile-bundle-op))
      ((:image)
       'image-op)
      ((:program)
       'program-op))))

;;;
;;; LOAD-BUNDLE-OP
;;;
;;; This is like ASDF's LOAD-OP, but using bundle fasl files.
;;;
(with-upgradability ()
  (defmethod component-depends-on ((o load-bundle-op) (c system))
    `((,o ,@(component-sideway-dependencies c))
      (,(if (user-system-p c) 'compile-bundle-op 'load-op) ,c)
      ,@(call-next-method)))

  (defmethod input-files ((o load-bundle-op) (c system))
    (when (user-system-p c)
      (output-files (find-operation o 'compile-bundle-op) c)))

  (defmethod perform ((o load-bundle-op) (c system))
    (when (input-files o c)
      (perform-lisp-load-fasl o c)))

  (defmethod mark-operation-done :after ((o load-bundle-op) (c system))
    (mark-operation-done (find-operation o 'load-op) c)))

;;;
;;; PRECOMPILED FILES
;;;
;;; This component can be used to distribute ASDF systems in precompiled form.
;;; Only useful when the dependencies have also been precompiled.
;;;
(with-upgradability ()
  (defmethod trivial-system-p ((s system))
    (every #'(lambda (c) (typep c 'compiled-file)) (component-children s)))

  (defmethod input-files ((o operation) (c compiled-file))
    (list (component-pathname c)))
  (defmethod perform ((o load-op) (c compiled-file))
    (perform-lisp-load-fasl o c))
  (defmethod perform ((o load-source-op) (c compiled-file))
    (perform (find-operation o 'load-op) c))
  (defmethod perform ((o operation) (c compiled-file))
    nil))

;;;
;;; Pre-built systems
;;;
(with-upgradability ()
  (defmethod trivial-system-p ((s prebuilt-system))
    t)

  (defmethod perform ((o link-op) (c prebuilt-system))
    nil)

  (defmethod perform ((o basic-compile-bundle-op) (c prebuilt-system))
    nil)

  (defmethod perform ((o lib-op) (c prebuilt-system))
    nil)

  (defmethod perform ((o dll-op) (c prebuilt-system))
    nil)

  (defmethod component-depends-on ((o gather-operation) (c prebuilt-system))
    nil)

  (defmethod output-files ((o lib-op) (c prebuilt-system))
    (values (list (prebuilt-system-static-library c)) t)))


;;;
;;; PREBUILT SYSTEM CREATOR
;;;
(with-upgradability ()
  (defmethod output-files ((o deliver-asd-op) (s system))
    (list (make-pathname :name (component-name s) :type "asd"
                         :defaults (component-pathname s))))

  (defmethod perform ((o deliver-asd-op) (s system))
    (let* ((inputs (input-files o s))
           (fasl (first inputs))
           (library (second inputs))
           (asd (first (output-files o s)))
           (name (if (and fasl asd) (pathname-name asd) (return-from perform)))
           (version (component-version s))
           (dependencies
             (if (operation-monolithic-p o)
                 ;; We want only dependencies, and we use basic-load-op rather than load-op so that
                 ;; this will keep working on systems that load dependencies with load-bundle-op
                 (remove-if-not 'builtin-system-p
                                (required-components s :component-type 'system
                                                       :keep-operation 'basic-load-op))
                 (while-collecting (x) ;; resolve the sideway-dependencies of s
                   (map-direct-dependencies
                    'load-op s
                    #'(lambda (o c)
                        (when (and (typep o 'load-op) (typep c 'system))
                          (x c)))))))
           (depends-on (mapcar 'coerce-name dependencies)))
      (when (pathname-equal asd (system-source-file s))
        (cerror "overwrite the asd file"
                "~/asdf-action:format-action/ is going to overwrite the system definition file ~S ~
which is probably not what you want; you probably need to tweak your output translations."
                (cons o s) asd))
      (with-open-file (s asd :direction :output :if-exists :supersede
                             :if-does-not-exist :create)
        (format s ";;; Prebuilt~:[~; monolithic~] ASDF definition for system ~A~%"
                (operation-monolithic-p o) name)
        (format s ";;; Built for ~A ~A on a ~A/~A ~A~%"
                (lisp-implementation-type)
                (lisp-implementation-version)
                (software-type)
                (machine-type)
                (software-version))
        (let ((*package* (find-package :asdf-user)))
          (pprint `(defsystem ,name
                     :class prebuilt-system
                     :version ,version
                     :depends-on ,depends-on
                     :components ((:compiled-file ,(pathname-name fasl)))
                     ,@(when library `(:lib ,(file-namestring library))))
                  s)
          (terpri s)))))

  #-(or clasp ecl mkcl)
  (defmethod perform ((o basic-compile-bundle-op) (c system))
    (let* ((input-files (input-files o c))
           (fasl-files (remove (compile-file-type) input-files :key #'pathname-type :test-not #'equalp))
           (non-fasl-files (remove (compile-file-type) input-files :key #'pathname-type :test #'equalp))
           (output-files (output-files o c))
           (output-file (first output-files)))
      (assert (eq (not input-files) (not output-files)))
      (when input-files
        (when non-fasl-files
          (error "On ~A, asdf/bundle can only bundle FASL files, but these were also produced: ~S"
                 (implementation-type) non-fasl-files))
        (when (or (prologue-code c) (epilogue-code c))
          (error "prologue-code and epilogue-code are not supported on ~A"
                 (implementation-type)))
        (with-staging-pathname (output-file)
          (combine-fasls fasl-files output-file)))))

  (defmethod input-files ((o load-op) (s precompiled-system))
    (bundle-output-files (find-operation o 'compile-bundle-op) s))

  (defmethod perform ((o load-op) (s precompiled-system))
    (perform-lisp-load-fasl o s))

  (defmethod component-depends-on ((o load-bundle-op) (s precompiled-system))
    `((load-op ,s) ,@(call-next-method))))

#| ;; Example use:
(asdf:defsystem :precompiled-asdf-utils :class asdf::precompiled-system :fasl (asdf:apply-output-translations (asdf:system-relative-pathname :asdf-utils "asdf-utils.system.fasl")))
(asdf:load-system :precompiled-asdf-utils)
|#

#+(or clasp ecl mkcl)
(with-upgradability ()
  (defun system-module-pathname (module)
    (let ((name (coerce-name module)))
      (some
       'file-exists-p
       (list
        #+clasp (compile-file-pathname (make-pathname :name name :defaults "sys:") :output-type :object)
        #+ecl (compile-file-pathname (make-pathname :name name :defaults "sys:") :type :lib)
        #+ecl (compile-file-pathname (make-pathname :name (strcat "lib" name) :defaults "sys:") :type :lib)
        #+ecl (compile-file-pathname (make-pathname :name name :defaults "sys:") :type :object)
        #+mkcl (make-pathname :name name :type (bundle-pathname-type :lib) :defaults #p"sys:")
        #+mkcl (make-pathname :name name :type (bundle-pathname-type :lib) :defaults #p"sys:contrib;")))))

  (defun make-prebuilt-system (name &optional (pathname (system-module-pathname name)))
    "Creates a prebuilt-system if PATHNAME isn't NIL."
    (when pathname
      (make-instance 'prebuilt-system
                     :name (coerce-name name)
                     :static-library (resolve-symlinks* pathname))))

  (defun linkable-system (x)
    (or (if-let (s (find-system x))
          (and (output-files 'lib-op s) s))
        (if-let (p (system-module-pathname (coerce-name x)))
          (make-prebuilt-system x p))))

  (defmethod component-depends-on :around ((o image-op) (c system))
    (let* ((next (call-next-method))
           (deps (make-hash-table :test 'equal))
           (linkable (loop* :for (do . dcs) :in next :collect
                       (cons do
                             (loop :for dc :in dcs
                               :for dep = (and dc (resolve-dependency-spec c dc))
                               :when dep
                               :do (setf (gethash (coerce-name (component-system dep)) deps) t)
                               :collect (or (and (typep dep 'system) (linkable-system dep)) dep))))))
        `((lib-op
           ,@(unless (no-uiop c)
               (list (linkable-system "cmp")
                     (unless (or (and (gethash "uiop" deps) (linkable-system "uiop"))
                                 (and (gethash "asdf" deps) (linkable-system "asdf")))
                       (or (linkable-system "uiop")
                           (linkable-system "asdf")
                           "asdf")))))
          ,@linkable)))

  (defmethod perform ((o link-op) (c system))
    (let* ((object-files (input-files o c))
           (output (output-files o c))
           (bundle (first output))
           (programp (typep o 'program-op))
           (kind (bundle-type o)))
      (when output
        (apply 'create-image
               bundle (append
                       (when programp (prefix-lisp-object-files c))
                       object-files
                       (when programp (postfix-lisp-object-files c)))
               :kind kind
               :prologue-code (when programp (prologue-code c))
               :epilogue-code (when programp (epilogue-code c))
               :build-args (when programp (extra-build-args c))
               :extra-object-files (when programp (extra-object-files c))
               :no-uiop (no-uiop c)
               (when programp `(:entry-point ,(component-entry-point c))))))))
;;;; -------------------------------------------------------------------------
;;;; Concatenate-source

(uiop/package:define-package :asdf/concatenate-source
  (:recycle :asdf/concatenate-source :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade
   :asdf/component :asdf/operation
   :asdf/system
   :asdf/action :asdf/lisp-action :asdf/plan :asdf/bundle)
  (:export
   #:concatenate-source-op
   #:load-concatenated-source-op
   #:compile-concatenated-source-op
   #:load-compiled-concatenated-source-op
   #:monolithic-concatenate-source-op
   #:monolithic-load-concatenated-source-op
   #:monolithic-compile-concatenated-source-op
   #:monolithic-load-compiled-concatenated-source-op))
(in-package :asdf/concatenate-source)

;;;
;;; Concatenate sources
;;;
(with-upgradability ()
  ;; Base classes for both regular and monolithic concatenate-source operations
  (defclass basic-concatenate-source-op (bundle-op)
    ((bundle-type :initform "lisp" :allocation :class)))
  (defclass basic-load-concatenated-source-op (basic-load-op selfward-operation) ())
  (defclass basic-compile-concatenated-source-op (basic-compile-op selfward-operation) ())
  (defclass basic-load-compiled-concatenated-source-op (basic-load-op selfward-operation) ())

  ;; Regular concatenate-source operations
  (defclass concatenate-source-op (basic-concatenate-source-op non-propagating-operation) ()
    (:documentation "Operation to concatenate all sources in a system into a single file"))
  (defclass load-concatenated-source-op (basic-load-concatenated-source-op)
    ((selfward-operation :initform '(prepare-op concatenate-source-op) :allocation :class))
    (:documentation "Operation to load the result of concatenate-source-op as source"))
  (defclass compile-concatenated-source-op (basic-compile-concatenated-source-op)
    ((selfward-operation :initform '(prepare-op concatenate-source-op) :allocation :class))
    (:documentation "Operation to compile the result of concatenate-source-op"))
  (defclass load-compiled-concatenated-source-op (basic-load-compiled-concatenated-source-op)
    ((selfward-operation :initform '(prepare-op compile-concatenated-source-op) :allocation :class))
    (:documentation "Operation to load the result of compile-concatenated-source-op"))

  (defclass monolithic-concatenate-source-op
      (basic-concatenate-source-op monolithic-bundle-op non-propagating-operation) ()
    (:documentation "Operation to concatenate all sources in a system and its dependencies
into a single file"))
  (defclass monolithic-load-concatenated-source-op (basic-load-concatenated-source-op)
    ((selfward-operation :initform 'monolithic-concatenate-source-op :allocation :class))
    (:documentation "Operation to load the result of monolithic-concatenate-source-op as source"))
  (defclass monolithic-compile-concatenated-source-op (basic-compile-concatenated-source-op)
    ((selfward-operation :initform 'monolithic-concatenate-source-op :allocation :class))
    (:documentation "Operation to compile the result of monolithic-concatenate-source-op"))
  (defclass monolithic-load-compiled-concatenated-source-op
      (basic-load-compiled-concatenated-source-op)
    ((selfward-operation :initform 'monolithic-compile-concatenated-source-op :allocation :class))
    (:documentation "Operation to load the result of monolithic-compile-concatenated-source-op"))

  (defmethod input-files ((operation basic-concatenate-source-op) (s system))
    (loop :with encoding = (or (component-encoding s) *default-encoding*)
          :with other-encodings = '()
          :with around-compile = (around-compile-hook s)
          :with other-around-compile = '()
          :for c :in (required-components  ;; see note about similar call to required-components
                      s :goal-operation 'load-op ;;  in bundle.lisp
                        :keep-operation 'basic-compile-op
                        :other-systems (operation-monolithic-p operation))
          :append
          (when (typep c 'cl-source-file)
            (let ((e (component-encoding c)))
              (unless (equal e encoding)
                (let ((a (assoc e other-encodings)))
                  (if a (push (component-find-path c) (cdr a))
                      (push (list a (component-find-path c)) other-encodings)))))
            (unless (equal around-compile (around-compile-hook c))
              (push (component-find-path c) other-around-compile))
            (input-files (make-operation 'compile-op) c)) :into inputs
          :finally
             (when other-encodings
               (warn "~S uses encoding ~A but has sources that use these encodings:~{ ~A~}"
                     operation encoding
                     (mapcar #'(lambda (x) (cons (car x) (list (reverse (cdr x)))))
                             other-encodings)))
             (when other-around-compile
               (warn "~S uses around-compile hook ~A but has sources that use these hooks: ~A"
                     operation around-compile other-around-compile))
             (return inputs)))
  (defmethod output-files ((o basic-compile-concatenated-source-op) (s system))
    (lisp-compilation-output-files o s))

  (defmethod perform ((o basic-concatenate-source-op) (s system))
    (let* ((ins (input-files o s))
           (out (output-file o s))
           (tmp (tmpize-pathname out)))
      (concatenate-files ins tmp)
      (rename-file-overwriting-target tmp out)))
  (defmethod perform ((o basic-load-concatenated-source-op) (s system))
    (perform-lisp-load-source o s))
  (defmethod perform ((o basic-compile-concatenated-source-op) (s system))
    (perform-lisp-compilation o s))
  (defmethod perform ((o basic-load-compiled-concatenated-source-op) (s system))
    (perform-lisp-load-fasl o s)))

;;;; -------------------------------------------------------------------------
;;;; Package systems in the style of quick-build or faslpath

(uiop:define-package :asdf/package-inferred-system
  (:recycle :asdf/package-inferred-system :asdf/package-system :asdf)
  (:use :uiop/common-lisp :uiop
        :asdf/upgrade :asdf/session
        :asdf/component :asdf/system :asdf/system-registry :asdf/lisp-action
        :asdf/parse-defsystem)
  (:export
   #:package-inferred-system #:sysdef-package-inferred-system-search
   #:package-system ;; backward compatibility only. To be removed.
   #:register-system-packages
   #:*defpackage-forms* #:*package-inferred-systems* #:package-inferred-system-missing-package-error))
(in-package :asdf/package-inferred-system)

(with-upgradability ()
  ;; The names of the recognized defpackage forms.
  (defparameter *defpackage-forms* '(defpackage define-package))

  (defun initial-package-inferred-systems-table ()
    ;; Mark all existing packages are preloaded.
    (let ((h (make-hash-table :test 'equal)))
      (dolist (p (list-all-packages))
        (dolist (n (package-names p))
          (setf (gethash n h) t)))
      h))

  ;; Mapping from package names to systems that provide them.
  (defvar *package-inferred-systems* (initial-package-inferred-systems-table))

  (defclass package-inferred-system (system)
    ()
    (:documentation "Class for primary systems for which secondary systems are automatically
in the one-file, one-file, one-system style: system names are mapped to files under the primary
system's system-source-directory, dependencies are inferred from the first defpackage form in
every such file"))

  ;; DEPRECATED. For backward compatibility only. To be removed in an upcoming release:
  (defclass package-system (package-inferred-system) ())

  ;; Is a given form recognizable as a defpackage form?
  (defun defpackage-form-p (form)
    (and (consp form)
         (member (car form) *defpackage-forms*)))

  ;; Find the first defpackage form in a stream, if any
  (defun stream-defpackage-form (stream)
    (loop :for form = (read stream nil nil) :while form
          :when (defpackage-form-p form) :return form))

  (defun file-defpackage-form (file)
    "Return the first DEFPACKAGE form in FILE."
    (with-input-file (f file)
      (stream-defpackage-form f)))

  (define-condition package-inferred-system-missing-package-error (system-definition-error)
    ((system :initarg :system :reader error-system)
     (pathname :initarg :pathname :reader error-pathname))
    (:report (lambda (c s)
               (format s (compatfmt "~@<No package form found while ~
                                     trying to define package-inferred-system ~A from file ~A~>")
                       (error-system c) (error-pathname c)))))

  (defun package-dependencies (defpackage-form)
    "Return a list of packages depended on by the package
defined in DEFPACKAGE-FORM.  A package is depended upon if
the DEFPACKAGE-FORM uses it or imports a symbol from it."
    (assert (defpackage-form-p defpackage-form))
    (remove-duplicates
     (while-collecting (dep)
       (loop* :for (option . arguments) :in (cddr defpackage-form) :do
              (ecase option
                ((:use :mix :reexport :use-reexport :mix-reexport)
                 (dolist (p arguments) (dep (string p))))
                ((:import-from :shadowing-import-from)
                 (dep (string (first arguments))))
                ((:nicknames :documentation :shadow :export :intern :unintern :recycle)))))
     :from-end t :test 'equal))

  (defun package-designator-name (package)
    "Normalize a package designator to a string"
    (etypecase package
      (package (package-name package))
      (string package)
      (symbol (string package))))

  (defun register-system-packages (system packages)
    "Register SYSTEM as providing PACKAGES."
    (let ((name (or (eq system t) (coerce-name system))))
      (dolist (p (ensure-list packages))
        (setf (gethash (package-designator-name p) *package-inferred-systems*) name))))

  (defun package-name-system (package-name)
    "Return the name of the SYSTEM providing PACKAGE-NAME, if such exists,
otherwise return a default system name computed from PACKAGE-NAME."
    (check-type package-name string)
    (or (gethash package-name *package-inferred-systems*)
        (string-downcase package-name)))

  ;; Given a file in package-inferred-system style, find its dependencies
  (defun package-inferred-system-file-dependencies (file &optional system)
    (if-let (defpackage-form (file-defpackage-form file))
      (remove t (mapcar 'package-name-system (package-dependencies defpackage-form)))
      (error 'package-inferred-system-missing-package-error :system system :pathname file)))

  ;; Given package-inferred-system object, check whether its specification matches
  ;; the provided parameters
  (defun same-package-inferred-system-p (system name directory subpath around-compile dependencies)
    (and (eq (type-of system) 'package-inferred-system)
         (equal (component-name system) name)
         (pathname-equal directory (component-pathname system))
         (equal dependencies (component-sideway-dependencies system))
         (equal around-compile (around-compile-hook system))
         (let ((children (component-children system)))
           (and (length=n-p children 1)
                (let ((child (first children)))
                  (and (eq (type-of child) 'cl-source-file)
                       (equal (component-name child) "lisp")
                       (and (slot-boundp child 'relative-pathname)
                            (equal (slot-value child 'relative-pathname) subpath))))))))

  ;; sysdef search function to push into *system-definition-search-functions*
  (defun sysdef-package-inferred-system-search (system)
    (let ((primary (primary-system-name system)))
      (unless (equal primary system)
        (let ((top (find-system primary nil)))
          (when (typep top 'package-inferred-system)
            (if-let (dir (component-pathname top))
              (let* ((sub (subseq system (1+ (length primary))))
                     (f (probe-file* (subpathname dir sub :type "lisp")
                                     :truename *resolve-symlinks*)))
                (when (file-pathname-p f)
                  (let ((dependencies (package-inferred-system-file-dependencies f system))
                        (previous (registered-system system))
                        (around-compile (around-compile-hook top)))
                    (if (same-package-inferred-system-p previous system dir sub around-compile dependencies)
                        previous
                        (eval `(defsystem ,system
                                 :class package-inferred-system
                                 :source-file nil
                                 :pathname ,dir
                                 :depends-on ,dependencies
                                 :around-compile ,around-compile
                                 :components ((cl-source-file "lisp" :pathname ,sub)))))))))))))))

(with-upgradability ()
  (pushnew 'sysdef-package-inferred-system-search *system-definition-search-functions*)
  (setf *system-definition-search-functions*
        (remove (find-symbol* :sysdef-package-system-search :asdf/package-system nil)
                *system-definition-search-functions*)))
;;;; ---------------------------------------------------------------------------
;;;; asdf-output-translations

(uiop/package:define-package :asdf/output-translations
  (:recycle :asdf/output-translations :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade)
  (:export
   #:*output-translations* #:*output-translations-parameter*
   #:invalid-output-translation
   #:output-translations #:output-translations-initialized-p
   #:initialize-output-translations #:clear-output-translations
   #:disable-output-translations #:ensure-output-translations
   #:apply-output-translations
   #:validate-output-translations-directive #:validate-output-translations-form
   #:validate-output-translations-file #:validate-output-translations-directory
   #:parse-output-translations-string #:wrapping-output-translations
   #:user-output-translations-pathname #:system-output-translations-pathname
   #:user-output-translations-directory-pathname #:system-output-translations-directory-pathname
   #:environment-output-translations #:process-output-translations
   #:compute-output-translations
   #+abcl #:translate-jar-pathname
   ))
(in-package :asdf/output-translations)

;; (setf output-translations) between 2.27 and 3.0.3 was using a defsetf macro
;; for the sake of obsolete versions of GCL 2.6. Make sure it doesn't come to haunt us.
(when-upgrading (:version "3.1.2") (fmakunbound '(setf output-translations)))

(with-upgradability ()
  (define-condition invalid-output-translation (invalid-configuration warning)
    ((format :initform (compatfmt "~@<Invalid asdf output-translation ~S~@[ in ~S~]~@{ ~@?~}~@:>"))))

  (defvar *output-translations* ()
    "Either NIL (for uninitialized), or a list of one element,
said element itself being a sorted list of mappings.
Each mapping is a pair of a source pathname and destination pathname,
and the order is by decreasing length of namestring of the source pathname.")

  (defun output-translations ()
    "Return the configured output-translations, if any"
    (car *output-translations*))

  ;; Set the output-translations, by sorting the provided new-value.
  (defun set-output-translations (new-value)
    (setf *output-translations*
          (list
           (stable-sort (copy-list new-value) #'>
                        :key #'(lambda (x)
                                 (etypecase (car x)
                                   ((eql t) -1)
                                   (pathname
                                    (let ((directory
                                           (normalize-pathname-directory-component
                                            (pathname-directory (car x)))))
                                      (if (listp directory) (length directory) 0))))))))
    new-value)
  (defun (setf output-translations) (new-value) (set-output-translations new-value))

  (defun output-translations-initialized-p ()
    "Have the output-translations been initialized yet?"
    (and *output-translations* t))

  (defun clear-output-translations ()
    "Undoes any initialization of the output translations."
    (setf *output-translations* '())
    (values))
  (register-clear-configuration-hook 'clear-output-translations)


  ;;; Validation of the configuration directives...

  (defun validate-output-translations-directive (directive)
    (or (member directive '(:enable-user-cache :disable-cache nil))
        (and (consp directive)
             (or (and (length=n-p directive 2)
                      (or (and (eq (first directive) :include)
                               (typep (second directive) '(or string pathname null)))
                          (and (location-designator-p (first directive))
                               (or (location-designator-p (second directive))
                                   (location-function-p (second directive))))))
                 (and (length=n-p directive 1)
                      (location-designator-p (first directive)))))))

  (defun validate-output-translations-form (form &key location)
    (validate-configuration-form
     form
     :output-translations
     'validate-output-translations-directive
     :location location :invalid-form-reporter 'invalid-output-translation))

  (defun validate-output-translations-file (file)
    (validate-configuration-file
     file 'validate-output-translations-form :description "output translations"))

  (defun validate-output-translations-directory (directory)
    (validate-configuration-directory
     directory :output-translations 'validate-output-translations-directive
               :invalid-form-reporter 'invalid-output-translation))


  ;;; Parse the ASDF_OUTPUT_TRANSLATIONS environment variable and/or some file contents
  (defun parse-output-translations-string (string &key location)
    (cond
      ((or (null string) (equal string ""))
       '(:output-translations :inherit-configuration))
      ((not (stringp string))
       (error (compatfmt "~@<Environment string isn't: ~3i~_~S~@:>") string))
      ((eql (char string 0) #\")
       (parse-output-translations-string (read-from-string string) :location location))
      ((eql (char string 0) #\()
       (validate-output-translations-form (read-from-string string) :location location))
      (t
       (loop
         :with inherit = nil
         :with directives = ()
         :with start = 0
         :with end = (length string)
         :with source = nil
         :with separator = (inter-directory-separator)
         :for i = (or (position separator string :start start) end) :do
           (let ((s (subseq string start i)))
             (cond
               (source
                (push (list source (if (equal "" s) nil s)) directives)
                (setf source nil))
               ((equal "" s)
                (when inherit
                  (error (compatfmt "~@<Only one inherited configuration allowed: ~3i~_~S~@:>")
                         string))
                (setf inherit t)
                (push :inherit-configuration directives))
               (t
                (setf source s)))
             (setf start (1+ i))
             (when (> start end)
               (when source
                 (error (compatfmt "~@<Uneven number of components in source to destination mapping: ~3i~_~S~@:>")
                        string))
               (unless inherit
                 (push :ignore-inherited-configuration directives))
               (return `(:output-translations ,@(nreverse directives)))))))))


  ;; The default sources of configuration for output-translations
  (defparameter* *default-output-translations*
    '(environment-output-translations
      user-output-translations-pathname
      user-output-translations-directory-pathname
      system-output-translations-pathname
      system-output-translations-directory-pathname))

  ;; Compulsory implementation-dependent wrapping for the translations:
  ;; handle implementation-provided systems.
  (defun wrapping-output-translations ()
    `(:output-translations
    ;; Some implementations have precompiled ASDF systems,
    ;; so we must disable translations for implementation paths.
      #+(or clasp #|clozure|# ecl mkcl sbcl)
      ,@(let ((h (resolve-symlinks* (lisp-implementation-directory))))
          (when h `(((,h ,*wild-path*) ()))))
      #+mkcl (,(translate-logical-pathname "CONTRIB:") ())
      ;; All-import, here is where we want user stuff to be:
      :inherit-configuration
      ;; These are for convenience, and can be overridden by the user:
      #+abcl (#p"/___jar___file___root___/**/*.*" (:user-cache #p"**/*.*"))
      #+abcl (#p"jar:file:/**/*.jar!/**/*.*" (:function translate-jar-pathname))
      ;; We enable the user cache by default, and here is the place we do:
      :enable-user-cache))

  ;; Relative pathnames of output-translations configuration to XDG configuration directory
  (defparameter *output-translations-file* (parse-unix-namestring "common-lisp/asdf-output-translations.conf"))
  (defparameter *output-translations-directory* (parse-unix-namestring "common-lisp/asdf-output-translations.conf.d/"))

  ;; Locating various configuration pathnames, depending on input or output intent.
  (defun user-output-translations-pathname (&key (direction :input))
    (xdg-config-pathname *output-translations-file* direction))
  (defun system-output-translations-pathname (&key (direction :input))
    (find-preferred-file (system-config-pathnames *output-translations-file*)
                         :direction direction))
  (defun user-output-translations-directory-pathname (&key (direction :input))
    (xdg-config-pathname *output-translations-directory* direction))
  (defun system-output-translations-directory-pathname (&key (direction :input))
    (find-preferred-file (system-config-pathnames *output-translations-directory*)
                         :direction direction))
  (defun environment-output-translations ()
    (getenv "ASDF_OUTPUT_TRANSLATIONS"))


  ;;; Processing the configuration.

  (defgeneric process-output-translations (spec &key inherit collect))

  (defun inherit-output-translations (inherit &key collect)
    (when inherit
      (process-output-translations (first inherit) :collect collect :inherit (rest inherit))))

  (defun* (process-output-translations-directive) (directive &key inherit collect)
    (if (atom directive)
        (ecase directive
          ((:enable-user-cache)
           (process-output-translations-directive '(t :user-cache) :collect collect))
          ((:disable-cache)
           (process-output-translations-directive '(t t) :collect collect))
          ((:inherit-configuration)
           (inherit-output-translations inherit :collect collect))
          ((:ignore-inherited-configuration :ignore-invalid-entries nil)
           nil))
        (let ((src (first directive))
              (dst (second directive)))
          (if (eq src :include)
              (when dst
                (process-output-translations (pathname dst) :inherit nil :collect collect))
              (when src
                (let ((trusrc (or (eql src t)
                                  (let ((loc (resolve-location src :ensure-directory t :wilden t)))
                                    (if (absolute-pathname-p loc) (resolve-symlinks* loc) loc)))))
                  (cond
                    ((location-function-p dst)
                     (funcall collect
                              (list trusrc (ensure-function (second dst)))))
                    ((typep dst 'boolean)
                     (funcall collect (list trusrc t)))
                    (t
                     (let* ((trudst (resolve-location dst :ensure-directory t :wilden t)))
                       (funcall collect (list trudst t))
                       (funcall collect (list trusrc trudst)))))))))))

  (defmethod process-output-translations ((x symbol) &key
                                                       (inherit *default-output-translations*)
                                                       collect)
    (process-output-translations (funcall x) :inherit inherit :collect collect))
  (defmethod process-output-translations ((pathname pathname) &key inherit collect)
    (cond
      ((directory-pathname-p pathname)
       (process-output-translations (validate-output-translations-directory pathname)
                                    :inherit inherit :collect collect))
      ((probe-file* pathname :truename *resolve-symlinks*)
       (process-output-translations (validate-output-translations-file pathname)
                                    :inherit inherit :collect collect))
      (t
       (inherit-output-translations inherit :collect collect))))
  (defmethod process-output-translations ((string string) &key inherit collect)
    (process-output-translations (parse-output-translations-string string)
                                 :inherit inherit :collect collect))
  (defmethod process-output-translations ((x null) &key inherit collect)
    (inherit-output-translations inherit :collect collect))
  (defmethod process-output-translations ((form cons) &key inherit collect)
    (dolist (directive (cdr (validate-output-translations-form form)))
      (process-output-translations-directive directive :inherit inherit :collect collect)))


  ;;; Top-level entry-points to configure output-translations

  (defun compute-output-translations (&optional parameter)
    "read the configuration, return it"
    (remove-duplicates
     (while-collecting (c)
       (inherit-output-translations
        `(wrapping-output-translations ,parameter ,@*default-output-translations*) :collect #'c))
     :test 'equal :from-end t))

  ;; Saving the user-provided parameter to output-translations, if any,
  ;; so we can recompute the translations after code upgrade.
  (defvar *output-translations-parameter* nil)

  ;; Main entry-point for users.
  (defun initialize-output-translations (&optional (parameter *output-translations-parameter*))
    "read the configuration, initialize the internal configuration variable,
return the configuration"
    (setf *output-translations-parameter* parameter
          (output-translations) (compute-output-translations parameter)))

  (defun disable-output-translations ()
    "Initialize output translations in a way that maps every file to itself,
effectively disabling the output translation facility."
    (initialize-output-translations
     '(:output-translations :disable-cache :ignore-inherited-configuration)))

  ;; checks an initial variable to see whether the state is initialized
  ;; or cleared. In the former case, return current configuration; in
  ;; the latter, initialize.  ASDF will call this function at the start
  ;; of (asdf:find-system).
  (defun ensure-output-translations ()
    (if (output-translations-initialized-p)
        (output-translations)
        (initialize-output-translations)))


  ;; Top-level entry-point to _use_ output-translations
  (defun* (apply-output-translations) (path)
    (etypecase path
      (logical-pathname
       path)
      ((or pathname string)
       (ensure-output-translations)
       (loop* :with p = (resolve-symlinks* path)
              :for (source destination) :in (car *output-translations*)
              :for root = (when (or (eq source t)
                                    (and (pathnamep source)
                                         (not (absolute-pathname-p source))))
                            (pathname-root p))
              :for absolute-source = (cond
                                       ((eq source t) (wilden root))
                                       (root (merge-pathnames* source root))
                                       (t source))
              :when (or (eq source t) (pathname-match-p p absolute-source))
              :return (translate-pathname* p absolute-source destination root source)
              :finally (return p)))))


  ;; Hook into uiop's output-translation mechanism
  #-cormanlisp
  (setf *output-translation-function* 'apply-output-translations)


  ;;; Implementation-dependent hacks
  #+abcl ;; ABCL: make it possible to use systems provided in the ABCL jar.
  (defun translate-jar-pathname (source wildcard)
    (declare (ignore wildcard))
    (flet ((normalize-device (pathname)
             (if (find :windows *features*)
                 pathname
                 (make-pathname :defaults pathname :device :unspecific))))
      (let* ((jar
               (pathname (first (pathname-device source))))
             (target-root-directory-namestring
               (format nil "/___jar___file___root___/~@[~A/~]"
                       (and (find :windows *features*)
                            (pathname-device jar))))
             (relative-source
               (relativize-pathname-directory source))
             (relative-jar
               (relativize-pathname-directory (ensure-directory-pathname jar)))
             (target-root-directory
               (normalize-device
                (pathname-directory-pathname
                 (parse-namestring target-root-directory-namestring))))
             (target-root
               (merge-pathnames* relative-jar target-root-directory))
             (target
               (merge-pathnames* relative-source target-root)))
        (normalize-device (apply-output-translations target))))))

;;;; -----------------------------------------------------------------
;;;; Source Registry Configuration, by Francois-Rene Rideau
;;;; See the Manual and https://bugs.launchpad.net/asdf/+bug/485918

(uiop/package:define-package :asdf/source-registry
  ;; NB: asdf/find-system allows upgrade from <=3.2.1 that have initialize-source-registry there
  (:recycle :asdf/source-registry :asdf/find-system :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/system :asdf/system-registry)
  (:export
   #:*source-registry-parameter* #:*default-source-registries*
   #:invalid-source-registry
   #:source-registry-initialized-p
   #:initialize-source-registry #:clear-source-registry #:*source-registry*
   #:ensure-source-registry #:*source-registry-parameter*
   #:*default-source-registry-exclusions* #:*source-registry-exclusions*
   #:*wild-asd* #:directory-asd-files #:register-asd-directory
   #:*recurse-beyond-asds* #:collect-asds-in-directory #:collect-sub*directories-asd-files
   #:validate-source-registry-directive #:validate-source-registry-form
   #:validate-source-registry-file #:validate-source-registry-directory
   #:parse-source-registry-string #:wrapping-source-registry
   #:default-user-source-registry #:default-system-source-registry
   #:user-source-registry #:system-source-registry
   #:user-source-registry-directory #:system-source-registry-directory
   #:environment-source-registry #:process-source-registry #:inherit-source-registry
   #:compute-source-registry #:flatten-source-registry
   #:sysdef-source-registry-search))
(in-package :asdf/source-registry)

(with-upgradability ()
  (define-condition invalid-source-registry (invalid-configuration warning)
    ((format :initform (compatfmt "~@<Invalid source registry ~S~@[ in ~S~]~@{ ~@?~}~@:>"))))

  ;; Default list of directories under which the source-registry tree search won't recurse
  (defvar *default-source-registry-exclusions*
    '(;;-- Using ack 1.2 exclusions
      ".bzr" ".cdv"
      ;; "~.dep" "~.dot" "~.nib" "~.plst" ; we don't support ack wildcards
      ".git" ".hg" ".pc" ".svn" "CVS" "RCS" "SCCS" "_darcs"
      "_sgbak" "autom4te.cache" "cover_db" "_build"
      ;;-- debian often builds stuff under the debian directory... BAD.
      "debian"))

  ;; Actual list of directories under which the source-registry tree search won't recurse
  (defvar *source-registry-exclusions* *default-source-registry-exclusions*)

  ;; The state of the source-registry after search in configured locations
  (defvar *source-registry* nil
    "Either NIL (for uninitialized), or an equal hash-table, mapping
system names to pathnames of .asd files")

  ;; Saving the user-provided parameter to the source-registry, if any,
  ;; so we can recompute the source-registry after code upgrade.
  (defvar *source-registry-parameter* nil)

  (defun source-registry-initialized-p ()
    (typep *source-registry* 'hash-table))

  (defun clear-source-registry ()
    "Undoes any initialization of the source registry."
    (setf *source-registry* nil)
    (values))
  (register-clear-configuration-hook 'clear-source-registry)

  (defparameter *wild-asd*
    (make-pathname :directory nil :name *wild* :type "asd" :version :newest))

  (defun directory-asd-files (directory)
    (directory-files directory *wild-asd*))

  (defun collect-asds-in-directory (directory collect)
    (let ((asds (directory-asd-files directory)))
      (map () collect asds)
      asds))

  (defvar *recurse-beyond-asds* t
    "Should :tree entries of the source-registry recurse in subdirectories
after having found a .asd file? True by default.")

  ;; When walking down a filesystem tree, if in a directory there is a .cl-source-registry.cache,
  ;; read its contents instead of further recursively querying the filesystem.
  (defun process-source-registry-cache (directory collect)
    (let ((cache (ignore-errors
                  (safe-read-file-form (subpathname directory ".cl-source-registry.cache")))))
      (when (and (listp cache) (eq :source-registry-cache (first cache)))
        (loop :for s :in (rest cache) :do (funcall collect (subpathname directory s)))
        t)))

  (defun collect-sub*directories-asd-files
      (directory &key (exclude *default-source-registry-exclusions*) collect
                   (recurse-beyond-asds *recurse-beyond-asds*) ignore-cache)
    (let ((visited (make-hash-table :test 'equalp)))
      (flet ((collectp (dir)
               (unless (and (not ignore-cache) (process-source-registry-cache directory collect))
                 (let ((asds (collect-asds-in-directory dir collect)))
                   (or recurse-beyond-asds (not asds)))))
             (recursep (x)                    ; x will be a directory pathname
               (and
                (not (member (car (last (pathname-directory x))) exclude :test #'equal))
                (flet ((pathname-key (x)
                         (namestring (truename* x))))
                  (let ((visitedp (gethash (pathname-key x) visited)))
                    (if visitedp nil
                        (setf (gethash (pathname-key x) visited) t)))))))
      (collect-sub*directories directory #'collectp #'recursep (constantly nil)))))


  ;;; Validate the configuration forms

  (defun validate-source-registry-directive (directive)
    (or (member directive '(:default-registry))
        (and (consp directive)
             (let ((rest (rest directive)))
               (case (first directive)
                 ((:include :directory :tree)
                  (and (length=n-p rest 1)
                       (location-designator-p (first rest))))
                 ((:exclude :also-exclude)
                  (every #'stringp rest))
                 ((:default-registry)
                  (null rest)))))))

  (defun validate-source-registry-form (form &key location)
    (validate-configuration-form
     form :source-registry 'validate-source-registry-directive
          :location location :invalid-form-reporter 'invalid-source-registry))

  (defun validate-source-registry-file (file)
    (validate-configuration-file
     file 'validate-source-registry-form :description "a source registry"))

  (defun validate-source-registry-directory (directory)
    (validate-configuration-directory
     directory :source-registry 'validate-source-registry-directive
               :invalid-form-reporter 'invalid-source-registry))


  ;;; Parse the configuration string

  (defun parse-source-registry-string (string &key location)
    (cond
      ((or (null string) (equal string ""))
       '(:source-registry :inherit-configuration))
      ((not (stringp string))
       (error (compatfmt "~@<Environment string isn't: ~3i~_~S~@:>") string))
      ((find (char string 0) "\"(")
       (validate-source-registry-form (read-from-string string) :location location))
      (t
       (loop
         :with inherit = nil
         :with directives = ()
         :with start = 0
         :with end = (length string)
         :with separator = (inter-directory-separator)
         :for pos = (position separator string :start start) :do
           (let ((s (subseq string start (or pos end))))
             (flet ((check (dir)
                      (unless (absolute-pathname-p dir)
                        (error (compatfmt "~@<source-registry string must specify absolute pathnames: ~3i~_~S~@:>") string))
                      dir))
               (cond
                 ((equal "" s) ; empty element: inherit
                  (when inherit
                    (error (compatfmt "~@<Only one inherited configuration allowed: ~3i~_~S~@:>")
                           string))
                  (setf inherit t)
                  (push ':inherit-configuration directives))
                 ((string-suffix-p s "//") ;; TODO: allow for doubling of separator even outside Unix?
                  (push `(:tree ,(check (subseq s 0 (- (length s) 2)))) directives))
                 (t
                  (push `(:directory ,(check s)) directives))))
             (cond
               (pos
                (setf start (1+ pos)))
               (t
                (unless inherit
                  (push '(:ignore-inherited-configuration) directives))
                (return `(:source-registry ,@(nreverse directives))))))))))

  (defun register-asd-directory (directory &key recurse exclude collect)
    (if (not recurse)
        (collect-asds-in-directory directory collect)
        (collect-sub*directories-asd-files
         directory :exclude exclude :collect collect)))

  (defparameter* *default-source-registries*
    '(environment-source-registry
      user-source-registry
      user-source-registry-directory
      default-user-source-registry
      system-source-registry
      system-source-registry-directory
      default-system-source-registry)
    "List of default source registries" "3.1.0.102")

  (defparameter *source-registry-file* (parse-unix-namestring "common-lisp/source-registry.conf"))
  (defparameter *source-registry-directory* (parse-unix-namestring "common-lisp/source-registry.conf.d/"))

  (defun wrapping-source-registry ()
    `(:source-registry
      #+(or clasp ecl sbcl) (:tree ,(resolve-symlinks* (lisp-implementation-directory)))
      :inherit-configuration
      #+mkcl (:tree ,(translate-logical-pathname "SYS:"))
      #+cmucl (:tree #p"modules:")
      #+scl (:tree #p"file://modules/")))
  (defun default-user-source-registry ()
    `(:source-registry
      (:tree (:home "common-lisp/"))
      #+sbcl (:directory (:home ".sbcl/systems/"))
      (:directory ,(xdg-data-home "common-lisp/systems/"))
      (:tree ,(xdg-data-home "common-lisp/source/"))
      :inherit-configuration))
  (defun default-system-source-registry ()
    `(:source-registry
      ,@(loop :for dir :in (xdg-data-dirs "common-lisp/")
              :collect `(:directory (,dir "systems/"))
              :collect `(:tree (,dir "source/")))
      :inherit-configuration))
  (defun user-source-registry (&key (direction :input))
    (xdg-config-pathname *source-registry-file* direction))
  (defun system-source-registry (&key (direction :input))
    (find-preferred-file (system-config-pathnames *source-registry-file*)
                         :direction direction))
  (defun user-source-registry-directory (&key (direction :input))
    (xdg-config-pathname *source-registry-directory* direction))
  (defun system-source-registry-directory (&key (direction :input))
    (find-preferred-file (system-config-pathnames *source-registry-directory*)
                         :direction direction))
  (defun environment-source-registry ()
    (getenv "CL_SOURCE_REGISTRY"))


  ;;; Process the source-registry configuration

  (defgeneric process-source-registry (spec &key inherit register))

  (defun* (inherit-source-registry) (inherit &key register)
    (when inherit
      (process-source-registry (first inherit) :register register :inherit (rest inherit))))

  (defun* (process-source-registry-directive) (directive &key inherit register)
    (destructuring-bind (kw &rest rest) (if (consp directive) directive (list directive))
      (ecase kw
        ((:include)
         (destructuring-bind (pathname) rest
           (process-source-registry (resolve-location pathname) :inherit nil :register register)))
        ((:directory)
         (destructuring-bind (pathname) rest
           (when pathname
             (funcall register (resolve-location pathname :ensure-directory t)))))
        ((:tree)
         (destructuring-bind (pathname) rest
           (when pathname
             (funcall register (resolve-location pathname :ensure-directory t)
                      :recurse t :exclude *source-registry-exclusions*))))
        ((:exclude)
         (setf *source-registry-exclusions* rest))
        ((:also-exclude)
         (appendf *source-registry-exclusions* rest))
        ((:default-registry)
         (inherit-source-registry
          '(default-user-source-registry default-system-source-registry) :register register))
        ((:inherit-configuration)
         (inherit-source-registry inherit :register register))
        ((:ignore-inherited-configuration)
         nil)))
    nil)

  (defmethod process-source-registry ((x symbol) &key inherit register)
    (process-source-registry (funcall x) :inherit inherit :register register))
  (defmethod process-source-registry ((pathname pathname) &key inherit register)
    (cond
      ((directory-pathname-p pathname)
       (let ((*here-directory* (resolve-symlinks* pathname)))
         (process-source-registry (validate-source-registry-directory pathname)
                                  :inherit inherit :register register)))
      ((probe-file* pathname :truename *resolve-symlinks*)
       (let ((*here-directory* (pathname-directory-pathname pathname)))
         (process-source-registry (validate-source-registry-file pathname)
                                  :inherit inherit :register register)))
      (t
       (inherit-source-registry inherit :register register))))
  (defmethod process-source-registry ((string string) &key inherit register)
    (process-source-registry (parse-source-registry-string string)
                             :inherit inherit :register register))
  (defmethod process-source-registry ((x null) &key inherit register)
    (inherit-source-registry inherit :register register))
  (defmethod process-source-registry ((form cons) &key inherit register)
    (let ((*source-registry-exclusions* *default-source-registry-exclusions*))
      (dolist (directive (cdr (validate-source-registry-form form)))
        (process-source-registry-directive directive :inherit inherit :register register))))


  ;; Flatten the user-provided configuration into an ordered list of directories and trees
  (defun flatten-source-registry (&optional (parameter *source-registry-parameter*))
    (remove-duplicates
     (while-collecting (collect)
       (with-pathname-defaults () ;; be location-independent
         (inherit-source-registry
          `(wrapping-source-registry
            ,parameter
            ,@*default-source-registries*)
          :register #'(lambda (directory &key recurse exclude)
                        (collect (list directory :recurse recurse :exclude exclude))))))
     :test 'equal :from-end t))

  ;; MAYBE: move this utility function to uiop/pathname and export it?
  (defun pathname-directory-depth (p)
    (length (normalize-pathname-directory-component (pathname-directory p))))

  (defun preferred-source-path-p (x y)
    "Return T iff X is to be preferred over Y as a source path"
    (let ((lx (pathname-directory-depth x))
          (ly (pathname-directory-depth y)))
      (or (< lx ly)
          (and (= lx ly)
               (string< (namestring x)
                        (namestring y))))))

  ;; Will read the configuration and initialize all internal variables.
  (defun compute-source-registry (&optional (parameter *source-registry-parameter*)
                                    (registry *source-registry*))
    (dolist (entry (flatten-source-registry parameter))
      (destructuring-bind (directory &key recurse exclude) entry
        (let* ((h (make-hash-table :test 'equal))) ; table to detect duplicates
          (register-asd-directory
           directory :recurse recurse :exclude exclude :collect
           #'(lambda (asd)
               (let* ((name (pathname-name asd))
                      (name (if (typep asd 'logical-pathname)
                                ;; logical pathnames are upper-case,
                                ;; at least in the CLHS and on SBCL,
                                ;; yet (coerce-name :foo) is lower-case.
                                ;; won't work well with (load-system "Foo")
                                ;; instead of (load-system 'foo)
                                (string-downcase name)
                                name)))
                 (unless (gethash name registry) ; already shadowed by something else
                   (if-let (old (gethash name h))
                     ;; If the name appears multiple times,
                     ;; prefer the one with the shallowest directory,
                     ;; or if they have same depth, compare unix-namestring with string<
                     (multiple-value-bind (better worse)
                         (if (preferred-source-path-p asd old)
                             (progn (setf (gethash name h) asd) (values asd old))
                             (values old asd))
                       (when *verbose-out*
                         (warn (compatfmt "~@<In source-registry entry ~A~@[/~*~] ~
                                              found several entries for ~A - picking ~S over ~S~:>")
                               directory recurse name better worse)))
                     (setf (gethash name h) asd))))))
          (maphash #'(lambda (k v) (setf (gethash k registry) v)) h))))
    (values))

  (defun initialize-source-registry (&optional (parameter *source-registry-parameter*))
    ;; Record the parameter used to configure the registry
    (setf *source-registry-parameter* parameter)
    ;; Clear the previous registry database:
    (setf *source-registry* (make-hash-table :test 'equal))
    ;; Do it!
    (compute-source-registry parameter))

  ;; Checks an initial variable to see whether the state is initialized
  ;; or cleared. In the former case, return current configuration; in
  ;; the latter, initialize.  ASDF will call this function at the start
  ;; of (asdf:find-system) to make sure the source registry is initialized.
  ;; However, it will do so *without* a parameter, at which point it
  ;; will be too late to provide a parameter to this function, though
  ;; you may override the configuration explicitly by calling
  ;; initialize-source-registry directly with your parameter.
  (defun ensure-source-registry (&optional parameter)
    (unless (source-registry-initialized-p)
      (initialize-source-registry parameter))
    (values))

  (defun sysdef-source-registry-search (system)
    (ensure-source-registry)
    (values (gethash (primary-system-name system) *source-registry*))))


;;;; -------------------------------------------------------------------------
;;; Internal hacks for backward-compatibility

(uiop/package:define-package :asdf/backward-internals
  (:recycle :asdf/backward-internals :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/find-system)
  (:export #:load-sysdef))
(in-package :asdf/backward-internals)

(with-asdf-deprecation (:style-warning "3.2" :warning "3.4")
  (defun load-sysdef (name pathname)
    (declare (ignore name pathname))
    ;; Needed for backward compatibility with swank-asdf from SLIME 2015-12-01 or older.
    (error "Use asdf:load-asd instead of asdf::load-sysdef")))
;;;; -------------------------------------------------------------------------
;;; Backward-compatible interfaces

(uiop/package:define-package :asdf/backward-interface
  (:recycle :asdf/backward-interface :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/session
   :asdf/component :asdf/system :asdf/system-registry :asdf/operation :asdf/action
   :asdf/lisp-action :asdf/plan :asdf/operate
   :asdf/find-system :asdf/parse-defsystem :asdf/output-translations :asdf/bundle)
  (:export
   #:*asdf-verbose*
   #:operation-error #:compile-error #:compile-failed #:compile-warned
   #:error-component #:error-operation #:traverse
   #:component-load-dependencies
   #:enable-asdf-binary-locations-compatibility
   #:operation-on-failure #:operation-on-warnings #:on-failure #:on-warnings
   #:component-property
   #:run-shell-command
   #:system-definition-pathname #:system-registered-p #:require-system
   #:explain
   #+ecl #:make-build))
(in-package :asdf/backward-interface)

;; NB: the warning status of these functions may have to be distinguished later,
;; as some get removed faster than the others in client code.
(with-asdf-deprecation (:style-warning "3.2" :warning "3.4")

  ;; These conditions from ASDF 1 and 2 are used by many packages in Quicklisp;
  ;; but ASDF3 replaced them with somewhat different variants of uiop:compile-condition
  ;; that do not involve ASDF actions.
  ;; TODO: find the offenders and stop them.
  (progn
    (define-condition operation-error (error) ;; Bad, backward-compatible name
      ;; Used by SBCL, cffi-tests, clsql-mysql, clsql-uffi, qt, elephant, uffi-tests, sb-grovel
      ((component :reader error-component :initarg :component)
       (operation :reader error-operation :initarg :operation))
      (:report (lambda (c s)
                 (format s (compatfmt "~@<~A while invoking ~A on ~A~@:>")
                         (type-of c) (error-operation c) (error-component c)))))
    (define-condition compile-error (operation-error) ())
    (define-condition compile-failed (compile-error) ())
    (define-condition compile-warned (compile-error) ()))

  ;; In Quicklisp 2015-05, still used by lisp-executable, staple, repl-utilities, cffi
  (defun component-load-dependencies (component) ;; from ASDF 2.000 to 2.26
    "DEPRECATED. Please use COMPONENT-SIDEWAY-DEPENDENCIES instead; or better,
define your operations with proper use of SIDEWAY-OPERATION, SELFWARD-OPERATION,
or define methods on PREPARE-OP, etc."
    ;; Old deprecated name for the same thing. Please update your software.
    (component-sideway-dependencies component))

  ;; These old interfaces from ASDF1 have never been very meaningful
  ;; but are still used in obscure places.
  ;; In Quicklisp 2015-05, still used by cl-protobufs and clx.
  (defgeneric operation-on-warnings (operation)
    (:documentation "DEPRECATED. Please use UIOP:*COMPILE-FILE-WARNINGS-BEHAVIOUR* instead."))
  (defgeneric operation-on-failure (operation)
    (:documentation "DEPRECATED. Please use UIOP:*COMPILE-FILE-FAILURE-BEHAVIOUR* instead."))
  (defgeneric (setf operation-on-warnings) (x operation)
    (:documentation "DEPRECATED. Please SETF UIOP:*COMPILE-FILE-WARNINGS-BEHAVIOUR* instead."))
  (defgeneric (setf operation-on-failure) (x operation)
    (:documentation "DEPRECATED. Please SETF UIOP:*COMPILE-FILE-FAILURE-BEHAVIOUR* instead."))
  (progn
    (defmethod operation-on-warnings ((o operation))
      *compile-file-warnings-behaviour*)
    (defmethod operation-on-failure ((o operation))
      *compile-file-failure-behaviour*)
    (defmethod (setf operation-on-warnings) (x (o operation))
      (setf *compile-file-warnings-behaviour* x))
    (defmethod (setf operation-on-failure) (x (o operation))
      (setf *compile-file-failure-behaviour* x)))

  ;; Quicklisp 2015-05: Still used by SLIME's swank-asdf (!), common-lisp-stat,
  ;; js-parser, osicat, babel, staple, weblocks, cl-png, plain-odbc, autoproject,
  ;; cl-blapack, com.informatimago, cells-gtk3, asdf-dependency-grovel,
  ;; cl-glfw, cffi, jwacs, montezuma
  (defun system-definition-pathname (x)
    ;; As of 2.014.8, we mean to make this function obsolete,
    ;; but that won't happen until all clients have been updated.
    "DEPRECATED. This function used to expose ASDF internals with subtle
differences with respect to user expectations, that have been refactored
away since. We recommend you use ASDF:SYSTEM-SOURCE-FILE instead for a
mostly compatible replacement that we're supporting, or even
ASDF:SYSTEM-SOURCE-DIRECTORY or ASDF:SYSTEM-RELATIVE-PATHNAME
if that's whay you mean." ;;)
    (system-source-file x))

  ;; TRAVERSE is the function used to compute a plan in ASDF 1 and 2.
  ;; It was never officially exposed but some people still used it.
  (defgeneric traverse (operation component &key &allow-other-keys)
    (:documentation
     "DEPRECATED. Use MAKE-PLAN and PLAN-ACTIONS, or REQUIRED-COMPONENTS,
or some other supported interface instead.

Generate and return a plan for performing OPERATION on COMPONENT.

The plan returned is a list of dotted-pairs. Each pair is the CONS
of ASDF operation object and a COMPONENT object. The pairs will be
processed in order by OPERATE."))
  (progn
    (define-convenience-action-methods traverse (operation component &key)))
  (defmethod traverse ((o operation) (c component) &rest keys &key plan-class &allow-other-keys)
    (plan-actions (apply 'make-plan plan-class o c keys)))


  ;; ASDF-Binary-Locations compatibility
  ;; This remains supported for legacy user, but not recommended for new users.
  ;; We suspect there are no more legacy users in 2016.
  (defun enable-asdf-binary-locations-compatibility
      (&key
         (centralize-lisp-binaries nil)
         (default-toplevel-directory
             ;; Use ".cache/common-lisp/" instead ???
             (subpathname (user-homedir-pathname) ".fasls/"))
         (include-per-user-information nil)
         (map-all-source-files (or #+(or clasp clisp ecl mkcl) t nil))
         (source-to-target-mappings nil)
         (file-types `(,(compile-file-type)
                        "build-report"
                        #+clasp (compile-file-type :output-type :object)
                        #+ecl (compile-file-type :type :object)
                        #+mkcl (compile-file-type :fasl-p nil)
                        #+clisp "lib" #+sbcl "cfasl"
                        #+sbcl "sbcl-warnings" #+clozure "ccl-warnings")))
    "DEPRECATED. Use asdf-output-translations instead."
    #+(or clasp clisp ecl mkcl)
    (when (null map-all-source-files)
      (error "asdf:enable-asdf-binary-locations-compatibility doesn't support :map-all-source-files nil on CLISP, ECL and MKCL"))
    (let* ((patterns (if map-all-source-files (list *wild-file*)
                         (loop :for type :in file-types
                           :collect (make-pathname :type type :defaults *wild-file*))))
           (destination-directory
            (if centralize-lisp-binaries
                `(,default-toplevel-directory
                     ,@(when include-per-user-information
                             (cdr (pathname-directory (user-homedir-pathname))))
                     :implementation ,*wild-inferiors*)
                `(:root ,*wild-inferiors* :implementation))))
      (initialize-output-translations
       `(:output-translations
         ,@source-to-target-mappings
         #+abcl (#p"jar:file:/**/*.jar!/**/*.*" (:function translate-jar-pathname))
         #+abcl (#p"/___jar___file___root___/**/*.*" (,@destination-directory))
         ,@(loop :for pattern :in patterns
             :collect `((:root ,*wild-inferiors* ,pattern)
                        (,@destination-directory ,pattern)))
         (t t)
         :ignore-inherited-configuration))))
  (progn
    (defmethod operate :before (operation-class system &rest args &key &allow-other-keys)
      (declare (ignore operation-class system args))
      (when (find-symbol* '#:output-files-for-system-and-operation :asdf nil)
        (error "ASDF 2 is not compatible with ASDF-BINARY-LOCATIONS, which you are using.
ASDF 2 now achieves the same purpose with its builtin ASDF-OUTPUT-TRANSLATIONS,
which should be easier to configure. Please stop using ASDF-BINARY-LOCATIONS,
and instead use ASDF-OUTPUT-TRANSLATIONS. See the ASDF manual for details.
In case you insist on preserving your previous A-B-L configuration, but
do not know how to achieve the same effect with A-O-T, you may use function
ASDF:ENABLE-ASDF-BINARY-LOCATIONS-COMPATIBILITY as documented in the manual;
call that function where you would otherwise have loaded and configured A-B-L."))))


  ;; run-shell-command from ASDF 2, lightly fixed from ASDF 1, copied from MK-DEFSYSTEM. Die!
  (defun run-shell-command (control-string &rest args)
    "PLEASE DO NOT USE. This function is not just DEPRECATED, but also dysfunctional.
Please use UIOP:RUN-PROGRAM instead."
    #-(and ecl os-windows)
    (let ((command (apply 'format nil control-string args)))
      (asdf-message "; $ ~A~%" command)
      (let ((exit-code
             (ignore-errors
               (nth-value 2 (run-program command :force-shell t :ignore-error-status t
                                         :output *verbose-out*)))))
        (typecase exit-code
          ((integer 0 255) exit-code)
          (t 255))))
    #+(and ecl os-windows)
    (not-implemented-error "run-shell-command" "for ECL on Windows."))

  ;; HOW do we get rid of variables??? With a symbol-macro that issues a warning?
  ;; In Quicklisp 2015-05, cl-protobufs still uses it, but that should be fixed in next version.
  (progn
    (defvar *asdf-verbose* nil)) ;; backward-compatibility with ASDF2 only. Unused.

  ;; Do NOT use in new code. NOT SUPPORTED.
  ;; NB: When this goes away, remove the slot PROPERTY in COMPONENT.
  ;; In Quicklisp 2014-05, it's still used by yaclml, amazon-ecs, blackthorn-engine, cl-tidy.
  ;; See TODO for further cleanups required before to get rid of it.
  (defgeneric component-property (component property))
  (defgeneric (setf component-property) (new-value component property))

  (defmethod component-property ((c component) property)
    (cdr (assoc property (slot-value c 'properties) :test #'equal)))

  (defmethod (setf component-property) (new-value (c component) property)
    (let ((a (assoc property (slot-value c 'properties) :test #'equal)))
      (if a
          (setf (cdr a) new-value)
          (setf (slot-value c 'properties)
                (acons property new-value (slot-value c 'properties)))))
    new-value)


  ;; This method survives from ASDF 1, but really it is superseded by action-description.
  (defgeneric explain (operation component)
    (:documentation "Display a message describing an action.

DEPRECATED. Use ASDF:ACTION-DESCRIPTION and/or ASDF::FORMAT-ACTION instead."))
  (progn
    (define-convenience-action-methods explain (operation component)))
  (defmethod explain ((o operation) (c component))
    (asdf-message (compatfmt "~&~@<; ~@;~A~:>~%") (action-description o c))))

(with-asdf-deprecation (:style-warning "3.3")
  (defun system-registered-p (name)
    "DEPRECATED. Return a generalized boolean that is true if a system of given NAME was registered already.
NAME is a system designator, to be normalized by COERCE-NAME.
The value returned if true is a pair of a timestamp and a system object."
    (if-let (system (registered-system name))
      (cons (if-let (primary-system (registered-system (primary-system-name name)))
              (component-operation-time 'define-op primary-system))
            system)))

  (defun require-system (system &rest keys &key &allow-other-keys)
    "Ensure the specified SYSTEM is loaded, passing the KEYS to OPERATE, but do not update the
system or its dependencies if it has already been loaded."
    (declare (ignore keys))
    (unless (component-loaded-p system)
      (load-system system))))

;;; This function is for backward compatibility with ECL only.
#+ecl
(with-asdf-deprecation (:style-warning "3.2" :warning "9999")
  (defun make-build (system &rest args
                     &key (monolithic nil) (type :fasl) (move-here nil move-here-p)
                       prologue-code epilogue-code no-uiop
                       prefix-lisp-object-files postfix-lisp-object-files extra-object-files
                       &allow-other-keys)
    (let* ((operation (asdf/bundle::select-bundle-operation type monolithic))
           (move-here-path (if (and move-here
                                    (typep move-here '(or pathname string)))
                               (ensure-pathname move-here :namestring :lisp :ensure-directory t)
                               (system-relative-pathname system "asdf-output/")))
           (extra-build-args (remove-plist-keys
                              '(:monolithic :type :move-here
                                :prologue-code :epilogue-code :no-uiop
                                :prefix-lisp-object-files :postfix-lisp-object-files
                                :extra-object-files)
                              args))
           (build-system (if (subtypep operation 'image-op)
                             (eval `(defsystem "asdf.make-build"
                                      :class program-system
                                      :source-file nil
                                      :pathname ,(system-source-directory system)
                                      :build-operation ,operation
                                      :build-pathname ,(subpathname move-here-path
                                                                    (file-namestring (first (output-files operation system))))
                                      :depends-on (,(coerce-name system))
                                      :prologue-code ,prologue-code
                                      :epilogue-code ,epilogue-code
                                      :no-uiop ,no-uiop
                                      :prefix-lisp-object-files ,prefix-lisp-object-files
                                      :postfix-lisp-object-files ,postfix-lisp-object-files
                                      :extra-object-files ,extra-object-files
                                      :extra-build-args ,extra-build-args))
                             system))
           (files (output-files operation build-system)))
      (operate operation build-system)
      (if (or move-here
              (and (null move-here-p) (member operation '(program-op image-op))))
          (loop :with dest-path = (resolve-symlinks* (ensure-directories-exist move-here-path))
            :for f :in files
            :for new-f = (make-pathname :name (pathname-name f)
                                        :type (pathname-type f)
                                        :defaults dest-path)
            :do (rename-file-overwriting-target f new-f)
            :collect new-f)
          files))))
;;;; ---------------------------------------------------------------------------
;;;; Handle ASDF package upgrade, including implementation-dependent magic.

(uiop/package:define-package :asdf/interface
  (:nicknames :asdf :asdf-utilities)
  (:recycle :asdf/interface :asdf)
  (:unintern
   #:loaded-systems ; makes for annoying SLIME completion
   #:output-files-for-system-and-operation) ; ASDF-BINARY-LOCATION function we use to detect ABL
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/session
   :asdf/component :asdf/system :asdf/system-registry :asdf/find-component
   :asdf/operation :asdf/action :asdf/lisp-action
   :asdf/output-translations :asdf/source-registry
   :asdf/forcing :asdf/plan :asdf/operate :asdf/find-system :asdf/parse-defsystem
   :asdf/bundle :asdf/concatenate-source
   :asdf/backward-internals :asdf/backward-interface :asdf/package-inferred-system)
  ;; Note: (1) we are NOT automatically reexporting everything from previous packages.
  ;; (2) we only reexport UIOP functionality when backward-compatibility requires it.
  (:export
   #:defsystem #:find-system #:load-asd #:locate-system #:coerce-name #:primary-system-name
   #:oos #:operate #:make-plan #:perform-plan #:sequential-plan
   #:system-definition-pathname
   #:search-for-system-definition #:find-component #:component-find-path
   #:compile-system #:load-system #:load-systems #:load-systems*
   #:require-system #:test-system #:clear-system
   #:operation #:make-operation #:find-operation
   #:upward-operation #:downward-operation #:sideway-operation #:selfward-operation
                      #:non-propagating-operation
   #:build-op #:make
   #:load-op #:prepare-op #:compile-op
   #:prepare-source-op #:load-source-op #:test-op #:define-op
   #:feature #:version #:version-satisfies #:upgrade-asdf
   #:implementation-identifier #:implementation-type #:hostname
   #:component-depends-on ; backward-compatible name rather than action-depends-on
   #:input-files #:additional-input-files
   #:output-files #:output-file #:perform #:perform-with-restarts
   #:operation-done-p #:explain #:action-description #:component-sideway-dependencies
   #:needed-in-image-p
   #:bundle-op #:monolithic-bundle-op #:precompiled-system #:compiled-file #:bundle-system
   #:program-system
   #:basic-compile-bundle-op #:prepare-bundle-op
   #:compile-bundle-op #:load-bundle-op #:monolithic-compile-bundle-op #:monolithic-load-bundle-op
   #:lib-op #:dll-op #:deliver-asd-op #:program-op #:image-op
   #:monolithic-lib-op #:monolithic-dll-op #:monolithic-deliver-asd-op
   #:concatenate-source-op
   #:load-concatenated-source-op
   #:compile-concatenated-source-op
   #:load-compiled-concatenated-source-op
   #:monolithic-concatenate-source-op
   #:monolithic-load-concatenated-source-op
   #:monolithic-compile-concatenated-source-op
   #:monolithic-load-compiled-concatenated-source-op
   #:operation-monolithic-p
   #:required-components
   #:component-loaded-p
   #:component #:parent-component #:child-component #:system #:module
   #:file-component #:source-file #:c-source-file #:java-source-file
   #:cl-source-file #:cl-source-file.cl #:cl-source-file.lsp
   #:static-file #:doc-file #:html-file
   #:file-type #:source-file-type
   #:register-preloaded-system #:sysdef-preloaded-system-search
   #:register-immutable-system #:sysdef-immutable-system-search
   #:package-inferred-system #:register-system-packages
   #:component-children
   #:component-children-by-name
   #:component-pathname
   #:component-relative-pathname
   #:component-name
   #:component-version
   #:component-parent
   #:component-system
   #:component-encoding
   #:component-external-format
   #:system-description
   #:system-long-description
   #:system-author
   #:system-maintainer
   #:system-license
   #:system-licence
   #:system-source-file
   #:system-source-directory
   #:system-relative-pathname
   #:system-homepage
   #:system-mailto
   #:system-bug-tracker
   #:system-long-name
   #:system-source-control
   #:map-systems
   #:system-defsystem-depends-on
   #:system-depends-on
   #:system-weakly-depends-on
   #:*system-definition-search-functions*   ; variables
   #:*central-registry*
   #:*compile-file-warnings-behaviour*
   #:*compile-file-failure-behaviour*
   #:*resolve-symlinks*
   #:*verbose-out*
   #:asdf-version
   #:compile-condition #:compile-file-error #:compile-warned-error #:compile-failed-error
   #:compile-warned-warning #:compile-failed-warning
   #:error-name
   #:error-pathname
   #:load-system-definition-error
   #:error-component #:error-operation
   #:system-definition-error
   #:missing-component
   #:missing-component-of-version
   #:missing-dependency
   #:missing-dependency-of-version
   #:circular-dependency        ; errors
   #:duplicate-names #:non-toplevel-system #:non-system-system #:bad-system-name #:system-out-of-date
   #:package-inferred-system-missing-package-error
   #:operation-definition-warning #:operation-definition-error
   #:try-recompiling ; restarts
   #:retry
   #:accept
   #:coerce-entry-to-directory
   #:remove-entry-from-registry
   #:clear-configuration-and-retry
   #:*encoding-detection-hook*
   #:*encoding-external-format-hook*
   #:*default-encoding*
   #:*utf-8-external-format*
   #:clear-configuration
   #:*output-translations-parameter*
   #:initialize-output-translations
   #:disable-output-translations
   #:clear-output-translations
   #:ensure-output-translations
   #:apply-output-translations
   #:compile-file*
   #:compile-file-pathname*
   #:*warnings-file-type* #:enable-deferred-warnings-check #:disable-deferred-warnings-check
   #:enable-asdf-binary-locations-compatibility
   #:*default-source-registries*
   #:*source-registry-parameter*
   #:initialize-source-registry
   #:compute-source-registry
   #:clear-source-registry
   #:ensure-source-registry
   #:process-source-registry
   #:registered-system #:registered-systems #:already-loaded-systems
   #:resolve-location
   #:asdf-message
   #:*user-cache*
   #:user-output-translations-pathname
   #:system-output-translations-pathname
   #:user-output-translations-directory-pathname
   #:system-output-translations-directory-pathname
   #:user-source-registry
   #:system-source-registry
   #:user-source-registry-directory
   #:system-source-registry-directory

   ;; The symbols below are all DEPRECATED, do not use. To be removed in a further release.
   #:*asdf-verbose* #:run-shell-command
   #:component-load-dependencies #:system-registered-p #:package-system
   #+ecl #:make-build
   #:operation-on-warnings #:operation-on-failure #:operation-error
   #:compile-failed #:compile-warned #:compile-error
   #:module-components #:component-property #:traverse))
;;;; ---------------------------------------------------------------------------
;;;; ASDF-USER, where the action happens.

(uiop/package:define-package :asdf/user
  (:nicknames :asdf-user)
  ;; NB: releases before 3.1.2 this :use'd only uiop/package instead of uiop below.
  ;; They also :use'd uiop/common-lisp, that reexports common-lisp and is not included in uiop.
  ;; ASDF3 releases from 2.27 to 2.31 called uiop asdf-driver and asdf/foo uiop/foo.
  ;; ASDF1 and ASDF2 releases (2.26 and earlier) create a temporary package
  ;; that only :use's :cl and :asdf
  (:use :uiop/common-lisp :uiop :asdf/interface))
;;;; -----------------------------------------------------------------------
;;;; ASDF Footer: last words and cleanup

(uiop/package:define-package :asdf/footer
  (:recycle :asdf/footer :asdf)
  (:use :uiop/common-lisp :uiop
        :asdf/system ;; used by ECL
        :asdf/upgrade :asdf/system-registry :asdf/operate :asdf/bundle)
  ;; Happily, all those implementations all have the same module-provider hook interface.
  #+(or abcl clasp cmucl clozure ecl mkcl sbcl)
  (:import-from #+abcl :sys #+(or clasp cmucl ecl) :ext #+clozure :ccl #+mkcl :mk-ext #+sbcl sb-ext
                #:*module-provider-functions*
                #+ecl #:*load-hooks*)
  #+(or clasp mkcl) (:import-from :si #:*load-hooks*))

(in-package :asdf/footer)

;;;; Register ASDF itself and all its subsystems as preloaded.
(with-upgradability ()
  (dolist (s '("asdf" "uiop" "asdf-package-system"))
    ;; Don't bother with these system names, no one relies on them anymore:
    ;; "asdf-utils" "asdf-bundle" "asdf-driver" "asdf-defsystem"
    (register-preloaded-system s :version *asdf-version*)))


;;;; Hook ASDF into the implementation's REQUIRE and other entry points.
#+(or abcl clasp clisp clozure cmucl ecl mkcl sbcl)
(with-upgradability ()
  ;; Hook into CL:REQUIRE.
  #-clisp (pushnew 'module-provide-asdf *module-provider-functions*)
  #+clisp (if-let (x (find-symbol* '#:*module-provider-functions* :custom nil))
            (eval `(pushnew 'module-provide-asdf ,x)))

  #+(or clasp ecl mkcl)
  (progn
    (pushnew '("fasb" . si::load-binary) *load-hooks* :test 'equal :key 'car)

    #+os-windows
    (unless (assoc "asd" *load-hooks* :test 'equal)
      (appendf *load-hooks* '(("asd" . si::load-source))))

    ;; Wrap module provider functions in an idempotent, upgrade friendly way
    (defvar *wrapped-module-provider* (make-hash-table))
    (setf (gethash 'module-provide-asdf *wrapped-module-provider*) 'module-provide-asdf)
    (defun wrap-module-provider (provider name)
      (let ((results (multiple-value-list (funcall provider name))))
        (when (first results) (register-preloaded-system (coerce-name name)))
        (values-list results)))
    (defun wrap-module-provider-function (provider)
      (ensure-gethash provider *wrapped-module-provider*
                      (constantly
                       #'(lambda (module-name)
                           (wrap-module-provider provider module-name)))))
    (setf *module-provider-functions*
          (mapcar #'wrap-module-provider-function *module-provider-functions*))))

#+cmucl ;; Hook into the CMUCL herald.
(with-upgradability ()
  (defun herald-asdf (stream)
    (format stream "    ASDF ~A" (asdf-version)))
  (setf (getf ext:*herald-items* :asdf) '(herald-asdf)))


;;;; Done!
(with-upgradability ()
  #+allegro ;; restore *w-o-n-r-c* setting as saved in uiop/common-lisp
  (when (boundp 'excl:*warn-on-nested-reader-conditionals*)
    (setf excl:*warn-on-nested-reader-conditionals* uiop/common-lisp::*acl-warn-save*))

  ;; Advertise the features we provide.
  (dolist (f '(:asdf :asdf2 :asdf3 :asdf3.1 :asdf3.2 :asdf3.3)) (pushnew f *features*))

  ;; Provide both lowercase and uppercase, to satisfy more people, especially LispWorks users.
  (provide "asdf") (provide "ASDF")

  ;; Finally, call a function that will cleanup in case this is an upgrade of an older ASDF.
  (cleanup-upgraded-asdf))

(when *load-verbose*
  (asdf-message ";; ASDF, version ~a~%" (asdf-version)))
